-- |
-- Module      : Network.SocketA.POSIX.TCPIP
-- Description : Integrate TCP\/IP socket I\/O with GHC
-- Copyright   : (c) axionbuster, 2025
-- License     : BSD-3-Clause
--
-- This module provides high-level TCP/IP networking primitives for Linux systems,
-- integrated with GHC's event manager. It offers a simpler interface
-- compared to the low-level "Sock" module, with non-blocking operations that work
-- with GHC's IO manager.
--
-- All sockets are created in non-blocking mode and use asynchronous I/O.
-- The module provides common TCP/IP operations like accept, connect, send, and
-- receive with proper integration into GHC's runtime system.
module Network.SocketA.POSIX.TCPIP
  ( -- * Types
    S.Socket,
    S.AddrFamily (..),
    S.SocketType (..),
    S.Protocol (..),
    S.AddrInfo_ (..),
    S.AddrInfo,
    S.ShutdownHow (..),
    S.GetAddrInfoError (..),
    S.AddressLen,

    -- * Constants and Patterns
#if defined(linux_HOST_OS)
    pattern S.SOCK_NONBLOCK,
    pattern S.SOCK_CLOEXEC,
#endif
    pattern S.SOCK_STREAM,
    pattern S.AF_INET,
    pattern S.AF_INET6,
    pattern S.IPPROTO_TCP,
    pattern S.SHUT_RD,
    pattern S.SHUT_WR,
    pattern S.SHUT_RDWR,
    S.addrinfo0,

    -- * Functions
    S.socket,
    S.getaddrinfo,
    close,
    S.bind,
    listen,
    accept,
    S.shutdown,
    connect,
    recv,
    recvall,
    recvAll,
    send,
    sendall,
    sendAll,
    S.addrpair,
    S.addrpair_,
    S.withaddrpair,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Internal (ByteString, createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Function
import Foreign.C.Error
import Foreign.Ptr
import GHC.Event
import Network.SocketA.POSIX.Sock qualified as S
import System.Posix.Types

-- here Fd is a newtype over a CInt; distinct from FD type from GHC.IO.FD

-- assuming a threaded runtime, get the system event manager
evman :: IO EventManager
evman =
  getSystemEventManager >>= \case
    Just e -> pure e
    Nothing -> error "evman: no event manager; use threaded runtime"

-- register an event
-- - here IOCallback :: FdKey -> Event -> IO ()
-- - for FdKey, only the keyFd :: !Fd field is exposed; we can't make
-- our own FdKey
-- - Event is a bit set: 0 = nothing; read, write, (close)
regfd :: IOCallback -> Event -> Lifetime -> Fd -> IO FdKey
regfd c e l f = do
  m <- evman
  registerFd m c f e l

-- unregister an event
unregfd :: FdKey -> IO ()
unregfd f = evman >>= \m -> unregisterFd m f

-- close responsibly (race-safety)
closefd :: (Fd -> IO ()) -> Fd -> IO ()
closefd a b = evman >>= \m -> closeFd m a b

-- annoying because of newtyping
sk2fd :: S.Socket -> Fd
sk2fd = Fd . S.unsocket

fd2sk :: Fd -> S.Socket
fd2sk (Fd f) = S.Socket f

-- | close a socket
close :: S.Socket -> IO ()
-- we need to unregister any interests, so we use closefd
close = closefd (S.close . fd2sk) . sk2fd

-- if successful, unwrap; otherwise, rethrow the exception
unwrap :: (Exception a) => Either a b -> IO b
unwrap = either throwIO pure

-- | mark a socket as passive with an automatically determined queue length
listen :: S.Socket -> IO ()
listen = (`S.listen` 4096)

-- perform a one-shot async operation on a socket
--
-- flow:
--
-- 1. register socket FD with event manager for specified event
-- 2. when event fires, execute AIO action in masked context
-- 3. cleanup registration regardless of success/failure
--
-- guarantees:
-- - registration cleaned up via bracket
-- - exceptions propagated to caller
-- - callback executes exactly once
-- - no memory/FD leaks on exceptions
--
-- __note__: socket must be non-blocking
async1 :: Event -> S.AIO a -> S.Socket -> IO a
async1 e f h = do
  -- synopsis.
  --  1. register an interest in the socket's file descriptor
  --  2. wait [w] for the event to occur (-> takeMVar)
  --     (done asynchronously by GHC's event manager)
  --  3. process the event if successful, or rethrow the exception
  --     (-> unwrap)
  --     (then deregister the interest)
  w <- newEmptyMVar
  let g _ _ = mask_ do
        catch
          -- uninterruptible putMVar
          -- (this is the only place that can put to the MVar)
          do S.unaio f >>= putMVar w . Right
          -- wormhole the exception to the calling thread
          \(x :: SomeException) -> putMVar w (Left x)
  -- no need to apply mask_ here since 'bracket' takes care of the cleanup
  bracket
    do regfd g e OneShot (sk2fd h)
    do unregfd
    do \_ -> takeMVar w >>= unwrap

-- | accept a connection from a bound, listening socket
accept :: S.Socket -> IO S.Socket
accept s = async1 evtRead (S.accept s) s

-- | connect a socket to a remote address
connect :: S.Socket -> S.AddressLen -> IO ()
connect s a = do
  -- code works like 'accept', but need a special workflow...
  --  1. call 'connect', but it will almost certainly fail immediately with
  --     EINPROGRESS. can be caught by (<|>) instance of AIO
  --  2. once called back by event mgr, check for status using 'connecterror'
  --  3. on no error, clean up & return; otherwise, throw error
  w <- newEmptyMVar
  let g _ _ = mask_ do
        catch
          do
            S.connecterror s >>= \case
              Nothing -> putMVar w do Right ()
              Just e -> putMVar w $ Left $ toException do
                errnoToIOError
                  "connect"
                  e
                  Nothing -- 'Handle' data structure
                  Nothing -- file name
          \(x :: SomeException) -> putMVar w (Left x)
  bracket
    ( mask_ do
        S.unaio (S.connect s a <|> pure ())
          *> regfd g evtWrite OneShot (sk2fd s)
    )
    do unregfd
    do \_ -> takeMVar w >>= unwrap

-- | recv some data from a socket
--
-- on EOF, returns an empty bytestring
recv :: S.Socket -> Int -> IO ByteString
recv _ a | a < 0 = error $ "recv: invalid length: " ++ show a
recv s a = createAndTrim a \p ->
  let q = castPtr p
      b = fromIntegral a
      f = fromIntegral <$> S.recv s q b S.recvflags0
   in async1 evtRead f s

-- | send some data to a socket
send :: S.Socket -> ByteString -> IO Int
send s b = unsafeUseAsCStringLen b \(p, fromIntegral -> a) ->
  let q = castPtr p
      f = fromIntegral <$> S.send s q a S.sendflags0
   in async1 evtWrite f s

-- | recv at most @n@ bytes from a socket, where the returned length
-- is only less than @n@ if EOF is reached
recvall, recvAll :: S.Socket -> Int -> IO ByteString
recvall _ a | a < 0 = error $ "recvall: invalid length: " ++ show a
recvall s a =
  B.concat <$> do
    a & fix \r i -> do
      b <- recv s i
      if B.null b
        then pure []
        else do
          let t = i - B.length b
          if t > 0
            then (b :) <$> r t
            else pure [b]
recvAll = recvall
{-# INLINE recvAll #-}

-- | attempt to send the 'ByteString', throwing an exception if the
-- entire bytestring could not be sent
sendall, sendAll :: S.Socket -> ByteString -> IO ()
sendall s b =
  let a = B.length b
      f i = when (i < a) do j <- send s b; f (i + j)
   in f 0
sendAll = sendall
{-# INLINE sendAll #-}
