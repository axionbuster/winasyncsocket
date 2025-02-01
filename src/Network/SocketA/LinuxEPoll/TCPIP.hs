module Network.SocketA.LinuxEPoll.TCPIP
  ( -- * Types
    S.Socket,
    S.AddrFamily (..),
    S.SocketType (..),
    S.Protocol (..),
    S.AddrInfo_ (..),
    S.AddrInfo,
    S.ShutdownHow (..),

    -- * Constants and Patterns
    pattern S.SOCK_NONBLOCK,
    pattern S.SOCK_CLOEXEC,
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
    S.bindfirst,
    S.bindfirst2,
    S.listen,
    accept,
    S.sockaddrin,
    S.shutdown,
    connect,
    recv,
    recvall,
    recvAll,
    send,
    sendall,
    sendAll,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Internal (ByteString, createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Function
import Foreign.Ptr
import GHC.Event
import Network.SocketA.LinuxEPoll.Sock qualified as S
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

-- a highly repetitive pattern for async operations
-- (oneshot, one event, one socket)
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
  let g _ _ =
        catch
          do S.unaio f >>= putMVar w . Right
          \(x :: SomeException) -> putMVar w (Left x)
  bracket
    do regfd g e OneShot (sk2fd h)
    do unregfd
    do \_ -> takeMVar w >>= unwrap

-- | accept a connection from a bound, listening socket
accept :: S.Socket -> IO S.Socket
accept s = async1 evtRead (S.accept s) s

-- | connect a socket to a remote address
connect :: S.Socket -> S.SockAddr -> IO ()
connect s a = async1 evtWrite (S.connect s a) s

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
