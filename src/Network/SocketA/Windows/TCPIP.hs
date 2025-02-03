-- |
-- Module: TCPIP
-- Description: Integrate TCP\/IP socket I\/O with GHC (Windows)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module integrates asynchronous TCP\/IP primitives with RTS.
module Network.SocketA.Windows.TCPIP
  ( -- * Types
    AddrInfo (..),
    S.AddrFlag (..),
    S.AddrFamily (..),
    S.Protocol (..),
    ADDRINFOW (..),
    S.SOCKET (..),
    S.Socket, -- type alias of S.SOCKET
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
    S.AddressLen (..),

    -- * Operations
    startup,
    socket,
    S.getaddrinfo,
    S.withaddrlen,
    setsockopt_dword,
    S.bind,
    S.listen,
    accept,
    connect,
    recvbuf,
    recvBuf,
    recvall,
    recvAll,
    sendbuf,
    sendBuf,
    sendall,
    sendAll,
    recv,
    send,
    S.shutdown,
    S.close,
    addrinfow0,
    addrinfo0,
    S.sockaddr0,
    S.sockaddrin0,
    S.in6addr0,
    S.sockaddrin60,
    S.sockaddrin6old0,
    S.sockaddrin,

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
import Data.Function
import Data.IORef
import Foreign hiding (void)
import GHC.Event.Windows
import GHC.Event.Windows.FFI
import Network.SocketA.Windows.Sock
  ( ADDRINFOW (..),
    AddrInfo (..),
    ShutdownHow (..),
    SocketError (..),
    pattern SD_BOTH,
    pattern SD_RECEIVE,
    pattern SD_SEND,
  )
import Network.SocketA.Windows.Sock qualified as S
import System.IO.Unsafe
import System.Win32.Types

-- | a zero address info
addrinfo0 :: S.ADDRINFOW
addrinfo0 = addrinfow0

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

-- | create a new socket with given address family, socket type, and protocol
socket :: S.AddrFamily -> S.SocketType -> S.Protocol -> IO S.SOCKET
socket af ty proto = mask_ do
  s <- S.socket af ty proto
  -- hook it up with GHC's IO manager
  associateHandle' (handleu s)
  pure s

-- turn a socket into a HANDLE
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
  (S.VTABLE -> LPOVERLAPPED -> IO S.AsyncStatus) ->
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
      catch
        do
          b v o >>= \case
            S.OK -> CbDone <$> getOverlappedResult a o False -- no wait
            S.PENDING -> pure CbPending
        do pure . CbError . fromIntegral . getskerr
    c' 0 x = IOSuccess <$> c x
    c' e _ = iofail e

-- IOResult has a IOFailed :: Just Int -> IOResult x constructor.
-- we take the Just Int part and throw it
throw1 :: Maybe Int -> IO a
throw1 (Just e) = throwIO $ SocketError $ fromIntegral e
throw1 Nothing = throwunknown

-- | accept a new connection
accept :: S.SOCKET -> IO S.SOCKET
accept l = do
  i <- S.getprotocolinfo l
  a <- socket i.iAddressFamily i.iSocketType i.iProtocol
  overlapped
    do "accept"
    do handleu l
    do \v -> S.acceptex v l a
    do const $ S.finishaccept l a
    do const (pure a)

-- | connect an unbound socket to an address
connect :: S.SOCKET -> S.AddressLen -> IO ()
connect l a = do
  -- binding l is a Microsoft requirement (using ConnectEx extension)
  alloca \b -> do
    poke b $
      S.sockaddrin0
        { S.sin_family = fromIntegral . S.unaddrfamily $ S.AF_INET,
          S.sin_addr = S.INADDR_ANY,
          S.sin_port = 0
        }
    S.bind
      l
      ( S.AddressLen
          (castPtr b)
          (fromIntegral $ sizeOf (S.sockaddrin0 :: S.SockAddrIn))
      )
  overlapped
    do "connect"
    do handleu l
    do \v -> S.connectex v l a
    do const (pure ())
    do const (pure ())

-- | receive a buffer; corresponds to @recvBuf@ from package "network"
recvbuf, recvBuf :: S.SOCKET -> Ptr a -> Int -> IO Int
recvbuf l b c =
  overlapped
    do "recvbuf"
    do handleu l
    -- recvbuf, sendbuf: not using one of the extension methods
    do
      \_ o -> do
        S.recv l (S.WSABUF (fromIntegral c) (castPtr b)) o
        pure S.PENDING
    do pure
    do pure . fromIntegral
recvBuf = recvbuf
{-# INLINE recvBuf #-}

-- | send a buffer; corresponds to @sendBuf@ from package "network"
sendbuf, sendBuf :: S.SOCKET -> Ptr a -> Int -> IO Int
sendbuf l b c =
  overlapped
    do "sendbuf"
    do handleu l
    do
      \_ o -> do
        S.send l (S.WSABUF (fromIntegral c) (castPtr b)) o
        pure S.PENDING
    do pure
    do pure . fromIntegral
sendBuf = sendbuf
{-# INLINE sendBuf #-}

-- TODO: Sock also has recvmany and sendmany for vectored I/O

-- | receive up to a certain number of bytes
recv :: S.SOCKET -> Int -> IO ByteString
recv s a = createAndTrim a \p -> recvbuf s p a

-- | attempt to send the 'ByteString' and measure how many bytes were sent
send :: S.SOCKET -> ByteString -> IO Int
send s a = unsafeUseAsCStringLen a $ uncurry $ sendbuf s

-- | set a socket option (@DWORD@)
setsockopt_dword ::
  S.SOCKET ->
  S.SockoptLevel ->
  S.SockoptName ->
  DWORD ->
  IO ()
setsockopt_dword s = S.setsockopt_dword s

-- | term equal to 'S.ADDRINFOW0' (useful for record updates)
addrinfow0 :: ADDRINFOW
addrinfow0 = S.ADDRINFOW0

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

-- | attempt to send all of the given 'ByteString'
sendall, sendAll :: S.SOCKET -> ByteString -> IO ()
sendall s b = send s b >>= \c -> when (c < B.length b) $ sendall s (B.drop c b)
sendAll = sendall
{-# INLINE sendAll #-}
