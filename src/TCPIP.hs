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
    S.AddrFlag (..),
    S.AddrFamily (..),
    S.Protocol (..),
    ADDRINFOW (..),
    Socket (..),
    SocketError (..),
    UnknownError (..),
    ShutdownHow (..),
    S.SockoptLevel (..),
    S.SockoptName (..),
    S.SocketType (..),
    S.SockAddr (..),
    S.SockAddrIn (..),
    S.InAddr (..),
    S.In6Addr (..),
    S.SockAddrIn6 (..),
    S.SockAddrIn6Old (..),

    -- * Operations
    startup,
    socket,
    S.getaddrinfo,
    setsockopt_dword,
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
    addrinfow0,
    S.sockaddr0,
    S.sockaddrin0,
    S.in6addr0,
    S.sockaddrin60,
    S.sockaddrin6old0,

    -- * Patterns
    pattern S.ADDRINFOW0,
    pattern S.INADDR_ANY,
    pattern SD_RECEIVE,
    pattern SD_SEND,
    pattern SD_BOTH,
    pattern S.AF_INET,
    pattern S.AF_INET6,
    pattern S.SOCK_STREAM,
    pattern S.IPPROTO_TCP,
    pattern S.AI_PASSIVE,
    pattern S.SOL_SOCKET,
    pattern S.IPPROTO_IPV6,
    pattern S.SO_REUSEADDR,
    pattern S.IPV6_V6ONLY,
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
    \s -> do
      -- enable dual mode for IPv6 so we can accept
      -- both IPv4 and IPv6 connections
      S.setsockopt_dword s S.IPPROTO_IPV6 S.IPV6_V6ONLY 0
      S.loadvt s >>= newIORef
{-# NOINLINE globalvtable #-}

-- | set up Winsock 2
startup :: IO ()
startup = void $ readIORef globalvtable

-- | create a new managed socket with given address family, socket type,
-- and protocol
socket :: S.AddrFamily -> S.SocketType -> S.Protocol -> IO Socket
socket a b c = mask_ do
  s <- S.socket a b c
  t <- S.managesocket s
  associateHandle' (handleu s)
  pure t

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
  -- allocate a new socket with the same protocol info
  i <- S.getprotocolinfo l
  a <- socket i.iAddressFamily i.iSocketType i.iProtocol
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

-- | connect an unbound socket to an address
connect :: Socket -> AddrInfo -> IO ()
connect (sksk -> l) a = do
  -- binding l is a Microsoft requirement (using ConnectEx extension)
  S.bind
    l
    S.sockaddrin0
      { S.sin_family = fromIntegral . S.unaddrfamily $ S.AF_INET,
        S.sin_addr = S.INADDR_ANY,
        S.sin_port = 0
      }
  overlapped
    do "connect"
    do handleu l
    -- connectex' requires the socket to be bound first
    do \v o -> getailen a >>= \b -> S.connectex' v l a b o
    do const (pure ())
    do const (pure ())

-- | receive a buffer; corresponds to @recvBuf@ from package "network"
recvbuf, recvBuf :: Socket -> Ptr a -> Int -> IO Int
recvbuf (sksk -> l) b c =
  overlapped
    do "recvbuf"
    do handleu l
    -- recvbuf, sendbuf: not using one of the extension methods
    do const $ S.recv l $ S.WSABUF (fromIntegral c) (castPtr b)
    do pure
    do pure . fromIntegral
recvBuf = recvbuf
{-# INLINE recvBuf #-}

-- | send a buffer; corresponds to @sendBuf@ from package "network"
sendbuf, sendBuf :: Socket -> Ptr a -> Int -> IO Int
sendbuf (sksk -> l) b c =
  overlapped
    do "sendbuf"
    do handleu l
    do const $ S.send l $ S.WSABUF (fromIntegral c) (castPtr b)
    do pure
    do pure . fromIntegral
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
bind :: Socket -> S.SockAddrIn -> IO ()
bind (sksk -> s) = S.bind s

-- | mark the socket as /listening/ for new connections
listen :: Socket -> IO ()
listen (sksk -> s) = S.listen s

-- | set a socket option (@DWORD@)
setsockopt_dword :: Socket -> S.SockoptLevel -> S.SockoptName -> DWORD -> IO ()
setsockopt_dword (sksk -> s) = S.setsockopt_dword s

-- | term equal to 'S.ADDRINFOW0' (useful for record updates)
addrinfow0 :: ADDRINFOW
addrinfow0 = S.ADDRINFOW0

-- | attempt to send all of the given 'ByteString'
sendall, sendAll :: Socket -> ByteString -> IO ()
sendall s b = send s b >>= \c -> when (c < B.length b) $ sendall s (B.drop c b)
sendAll = sendall
{-# INLINE sendAll #-}
