-- |
-- Module: TCPIP
-- Description: Integrate TCP\/IP socket I\/O with GHC (Windows)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module integrates asynchronous TCP\/IP primitives with RTS.
module TCPIP
  ( -- * Types
    AddrInfo (..),
    Socket (..),
    SocketError (..),
    UnknownError (..),
    ShutdownHow (..),

    -- * Operations
    socket,
    getaddrinfo,
    bind,
    listen,
    accept,
    connect,
    recvbuf,
    recvBuf,
    sendbuf,
    sendBuf,
    sendall,
    sendAll,
    recv,
    send,
    shutdown,
    close,

    -- * Patterns
    pattern SD_RECEIVE,
    pattern SD_SEND,
    pattern SD_BOTH,
  )
where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Functor
import Data.IORef
import Foreign hiding (void)
import GHC.Event.Windows
import Sock
  ( ADDRINFOW (..),
    AddrInfo (..),
    ShutdownHow (..),
    Socket (..),
    SocketError (..),
    close,
    sksk,
    pattern SD_BOTH,
    pattern SD_RECEIVE,
    pattern SD_SEND,
  )
import Sock qualified as S
import System.IO.Unsafe
import System.Win32.Types

-- global virtual function table for certain socket extension methods.
-- this requirement is imposed by Windows Sockets API
globalvtable :: IORef S.VTABLE
globalvtable = unsafePerformIO do
  -- these steps are performed once in the lifecycle of the program
  S.startup -- set up Winsock 2
  bracket -- load virtual function table
    do S.socket S.AF_INET6 S.SOCK_STREAM S.IPPROTO_TCP
    do S.closesocket
    do S.loadvt >=> newIORef
{-# NOINLINE globalvtable #-}

-- | create a new managed socket
socket :: IO Socket
socket = S.socket S.AF_INET6 S.SOCK_STREAM S.IPPROTO_TCP >>= S.managesocket

-- turn an unmanaged socket into a HANDLE
handleu :: S.SOCKET -> HANDLE
handleu (S.SOCKET s) = wordPtrToPtr s

-- | IOCP (I\/O Completion Ports) error, uninformative
data UnknownError = UnknownError
  deriving (Show, Eq)

-- | \"unknown IOCP error\"
instance Exception UnknownError where
  displayException _ = "unknown IOCP error"

-- throw an IOCP error
throwunknown :: IO a
throwunknown = throwIO UnknownError

iofail :: (Integral b) => b -> IO (IOResult a)
iofail = pure . IOFailed . Just . fromIntegral

-- abbreviation of highly repetitive code
overlapped ::
  -- label
  String ->
  -- HANDLE (socket)
  HANDLE ->
  -- call extension method with given vtable and LPOVERLAPPED
  (S.VTABLE -> LPOVERLAPPED -> IO a) ->
  -- do this with the number of bytes transferred when done successfully
  (DWORD -> IO t) ->
  -- if no error, polish up returned value from withOverlapped
  (t -> IO b) ->
  -- value returned from withOverlapped
  IO b
overlapped l a b c d =
  withOverlapped l a 0 b' c' >>= \case
    IOSuccess q -> d q
    IOFailed e -> throw1 e
  where
    b' o = do
      v <- readIORef globalvtable
      catch (b v o $> CbPending) do pure . CbError . fromIntegral . getskerr
    c' 0 x = IOSuccess <$> c x
    c' e _ = iofail e

-- IOResult has a IOFailed :: Just Int -> IOResult x constructor.
-- we take the Just Int part and throw it
throw1 :: Maybe Int -> IO a
throw1 (Just e) = throwIO $ SocketError $ fromIntegral e
throw1 Nothing = throwunknown

-- | accept a new connection
accept :: Socket -> IO Socket
accept (sksk -> l) = do
  a <- socket
  overlapped
    do "accept"
    do handleu l
    do \v -> S.acceptex v l (sksk a)
    do const $ S.finishaccept l (sksk a)
    do const (pure a)

-- get the ai_addrlen from an AddrInfo pointer
getailen :: AddrInfo -> IO Int
getailen (AddrInfo a) = withForeignPtr a do
  fmap (fromIntegral . S.ai_addrlen) . peek

-- | connect a socket to an address. the socket must be bound using
-- 'bind' first
connect :: Socket -> AddrInfo -> IO ()
connect (sksk -> l) a =
  overlapped
    do "connect"
    do handleu l
    -- connectex' requires the socket to be bound first
    do \v o -> getailen a >>= \b -> S.connectex' v l a b o
    do const (pure ())
    do const (pure ())

-- | receive a buffer; corresponds to @recvBuf@ from package "network"
recvbuf :: Socket -> Ptr a -> Int -> IO Int
recvbuf (sksk -> l) b c =
  overlapped
    do "recvbuf"
    do handleu l
    -- recvbuf, sendbuf: not using one of the extension methods
    do const $ S.recv l $ S.WSABUF (fromIntegral c) (castPtr b)
    do pure
    do pure . fromIntegral

-- | compatibility with "network". see 'recvbuf'
recvBuf :: Socket -> Ptr a -> Int -> IO Int
recvBuf = recvbuf
{-# INLINE recvBuf #-}

-- | send a buffer; corresponds to @sendBuf@ from package "network"
sendbuf :: Socket -> Ptr a -> Int -> IO Int
sendbuf (sksk -> l) b c =
  overlapped
    do "sendbuf"
    do handleu l
    do const $ S.send l $ S.WSABUF (fromIntegral c) (castPtr b)
    do pure
    do pure . fromIntegral

-- | compatibility with "network". see 'sendbuf'
sendBuf :: Socket -> Ptr a -> Int -> IO Int
sendBuf = sendbuf
{-# INLINE sendBuf #-}

-- TODO: Sock also has recvmany and sendmany for vectored I/O

-- | receive up to a certain number of bytes
recv :: Socket -> Int -> IO ByteString
recv s a = createAndTrim a \p -> recvbuf s p a

-- | attempt to send the 'ByteString' and measure how many bytes were sent
send :: Socket -> ByteString -> IO Int
send s a = unsafeUseAsCStringLen a $ uncurry $ sendbuf s

-- | shut down a socket
shutdown :: Socket -> ShutdownHow -> IO ()
shutdown (sksk -> s) = S.shutdown s

-- | bind a socket to an address
bind :: Socket -> AddrInfo -> IO ()
bind (sksk -> s) = S.bind s

-- | mark the socket as /listening/ for new connections
listen :: Socket -> IO ()
listen (sksk -> s) = S.listen s

-- | resolve the address given by the host (IP) and service (port)
--
-- protocol: TCP\/IP, IPv4 or IPv6 (dual mode)
getaddrinfo :: String -> String -> IO AddrInfo
getaddrinfo node service = S.getaddrinfo node service $ Just do
  let ai = S.ADDRINFOW0
   in ai
        { ai_family = S.unaddrfamily S.AF_INET6,
          ai_socktype = S.unsockettype S.SOCK_STREAM,
          ai_protocol = S.unprotocol S.IPPROTO_TCP
        }

-- | attempt to send all of the given 'ByteString'
sendall, sendAll :: Socket -> ByteString -> IO ()
sendall s b = send s b >>= \c -> when (c < B.length b) $ sendall s (B.drop c b)
sendAll = sendall
{-# INLINE sendAll #-}
