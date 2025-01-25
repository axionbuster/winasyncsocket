{- |
Module      : Sock
Description : Socket operations and constants for Windows Sockets 2
Copyright   : (c) axionbuster, 2025
License     : BSD-3-Clause

This module provides low-level bindings to the Windows Sockets 2 API (Winsock2).
It includes socket operations, constants, and data structures needed for network
programming on Windows platforms.

Portions of this code are interpreted from Tamar Christina's code.

Unicode operations are used wherever possible.
-}
module Sock
  ( -- * Types
    SOCKET(..)
  , Socket(..)
  , VTABLE(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , ADDRINFOW(..)
  , SockAddr
  , AddrInfo(..)
  , SocketError(..)
  , ShutdownHow(..)
  , WSABUF(..)
  , LPWSABUF
    -- * Patterns
  , pattern ADDRINFOW0
  , pattern AF_INET
  , pattern AF_INET6
  , pattern SOCK_STREAM
  , pattern IPPROTO_TCP
  , pattern IPPROTO_UDP
  , pattern Success
  , pattern WouldBlock
  , pattern NotSupported
  , pattern ConnectionReset
  , pattern Pending
  , pattern INVALID_SOCKET
  , pattern SOCKET_ERROR
  , pattern SD_RECEIVE
  , pattern SD_SEND
  , pattern SD_BOTH
    -- * Operations
  , startup
  , socket
  , bind
  , listen
  , getaddrinfo
  , loadvt
  , shutdown
  , closesocket
  , acceptex
  , finishaccept
  , sockaddr
  , connectex
  , connectex'
  , recv
  , recvmany
  , send
  , sendmany
  -- * Managed sockets
  , managesocket
  , close
  ) where

-- frustratingly, formatter 'ormolu' doesn't work.

import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types
import GHC.Event.Windows
import System.IO.Unsafe
import System.Mem.Weak
import System.Win32.Types

-- oh, did you know, there are two distinct Unicode-enabling macros
-- that do slightly different things? the recommendation is always enable
-- both when either one is enabled.

#ifndef UNICODE
#define UNICODE
#endif

#ifndef _UNICODE
#define _UNICODE
#endif

#define WIN32_LEAN_AND_MEAN

#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#include <mswsock.h>

#include "ax.h"

-- | raw socket error as returned by WSAGetLastError()
newtype SocketError = SocketError { getskerr :: CInt }
  deriving (Eq, Show, Storable)

foreign import capi unsafe "winbase.h LocalFree"
  localfree :: LPVOID -> IO ()

foreign import capi unsafe "winbase.h FormatMessage"
  -- the type of the 5th argument [out lpBuffer; LPTSTR*] depends on a flag in
  -- the 1st argument [in dwFlags; DWORD]: if it contains
  -- FORMAT_MESSAGE_ALLOCATE_BUFFER, then lpBuffer has type LPTSTR *, and
  -- it will point to the OS-allocated string to be freed by using LocalFree.
  -- otherwise, lpBuffer has type LPTSTR, and we need to allocate it ourselves.
  formatmessage :: DWORD -> LPVOID -> DWORD -> DWORD -> Ptr LPTSTR -> DWORD ->
                   LPVOID -> IO DWORD

foreign import capi unsafe "winnt.h MAKELANGID"
  makelangid :: DWORD -> DWORD -> DWORD

-- | string fetched from the Windows API. yes, we make a /syscall/.
instance Exception SocketError where
  displayException (SocketError s) = "Socket error: " ++ show m
    where
      m = unsafeDupablePerformIO do
        mask_ do
          alloca \b ->
            formatmessage
              do #{const FORMAT_MESSAGE_ALLOCATE_BUFFER}
                  .|. #{const FORMAT_MESSAGE_FROM_SYSTEM}
              do nullPtr
              do fromIntegral s
              do makelangid #{const LANG_NEUTRAL} #{const SUBLANG_NEUTRAL}
              do b
              do 101 -- minimum buffer size; some random prime because why not.
              do nullPtr -- va_list * [in, optional]
              >>= \case
                -- 0: failure, resort to showing number
                0 -> pure $ "<Winsock2 error code " ++ show s ++ ">"
                _ -> do
                  o <- peek b >>= peekTString
                  localfree (castPtr b)
                  pure o

-- | no error
pattern Success :: SocketError
pattern Success = SocketError 0

-- | operation is not permitted because it would block
pattern WouldBlock :: SocketError
pattern WouldBlock = SocketError #{const WSAEWOULDBLOCK}

-- | operation is not supported
pattern NotSupported :: SocketError
pattern NotSupported = SocketError #{const WSAVERNOTSUPPORTED}

-- | connection has been reset
pattern ConnectionReset :: SocketError 
pattern ConnectionReset = SocketError #{const WSAECONNRESET}

-- | not an error; operation will complete in the background
pattern Pending :: SocketError
pattern Pending = SocketError #{const ERROR_IO_PENDING}

throwsk :: SocketError -> IO a
throwsk = throwIO

-- | there is a socket error to be inspected using a call to WSAGetLastError().
-- note: all exported functions already call WSAGetLastError to report the
-- actual error code, so user code should not expect to get this error
pattern SOCKET_ERROR :: CInt
pattern SOCKET_ERROR = #{const SOCKET_ERROR}

-- | raw socket type
newtype SOCKET = SOCKET { unsocket :: WordPtr }
  deriving (Eq, Show, Storable)

-- | a placeholder socket number
pattern INVALID_SOCKET :: SOCKET
pattern INVALID_SOCKET = SOCKET #{const INVALID_SOCKET}

-- raw operations

data WSADATA
type LPWSADATA = Ptr WSADATA

foreign import capi "winsock2.h WSAStartup"
  wsastartup :: WORD -> LPWSADATA -> IO SocketError

-- | start up Windows Sockets v2.2. check version for 2.2.
startup :: IO ()
startup =
  allocaBytes #{size WSADATA} \d ->
  wsastartup 0x0202 d >>= \case
    Success -> do
      (w :: WORD) <- #{peek WSADATA, wVersion} d
      unless (w == 0x0202) do throwsk NotSupported
      pure ()
    e -> throwsk e

-- | the @ADDRINFOW@ structure from \<ws2def.h\>
data ADDRINFOW = ADDRINFOW
  { ai_flags :: CInt
  , ai_family :: CInt
  , ai_socktype :: CInt
  , ai_protocol :: CInt
  , ai_addrlen :: CSize
  , ai_canonname :: LPWSTR
  , ai_addr :: Ptr SockAddr
  , ai_next :: Ptr ADDRINFOW
  } deriving (Eq, Show)

instance Storable ADDRINFOW where
  sizeOf _ = #{size ADDRINFOW}
  alignment _ = #{alignment ADDRINFOW}
  peek p = do
    flags <- #{peek ADDRINFOW, ai_flags} p
    family <- #{peek ADDRINFOW, ai_family} p
    socktype <- #{peek ADDRINFOW, ai_socktype} p
    protocol <- #{peek ADDRINFOW, ai_protocol} p
    addrlen <- #{peek ADDRINFOW, ai_addrlen} p
    canonname <- #{peek ADDRINFOW, ai_canonname} p
    addr <- #{peek ADDRINFOW, ai_addr} p
    next <- #{peek ADDRINFOW, ai_next} p
    pure $ ADDRINFOW flags family socktype protocol addrlen canonname addr next
  poke p (ADDRINFOW flags family socktype protocol addrlen canonname addr next) = do
    #{poke ADDRINFOW, ai_flags} p flags
    #{poke ADDRINFOW, ai_family} p family
    #{poke ADDRINFOW, ai_socktype} p socktype
    #{poke ADDRINFOW, ai_protocol} p protocol
    #{poke ADDRINFOW, ai_addrlen} p addrlen
    #{poke ADDRINFOW, ai_canonname} p canonname
    #{poke ADDRINFOW, ai_addr} p addr
    #{poke ADDRINFOW, ai_next} p next

pattern NULL :: Ptr a
pattern NULL <- (const nullPtr -> _)
  where NULL = nullPtr

-- | a zero 'ADDRINFOW'
pattern ADDRINFOW0 :: ADDRINFOW
pattern ADDRINFOW0 = ADDRINFOW 0 0 0 0 0 NULL NULL NULL

-- | a 'ForeignPtr' wrapper over 'ADDRINFOW'. it frees the 'ADDRINFOW' using
-- the correct function
newtype AddrInfo = AddrInfo (ForeignPtr ADDRINFOW)
  deriving (Eq, Show)

foreign import capi "ws2tcpip.h GetAddrInfoW"
  -- 3rd [in] pointer is marked "const": "const ADDRINFOW *"
  getaddrinfow :: LPCWSTR -> LPCWSTR -> Ptr ADDRINFOW -> Ptr (Ptr ADDRINFOW)
                  -> IO CInt

foreign import capi "ws2tcpip.h &FreeAddrInfoW"
  freeaddrinfow :: FinalizerPtr ADDRINFOW

foreign import capi "winsock2.h WSAGetLastError"
  wsagetlasterror :: IO SocketError

-- if 0, go. otherwise, throw a socket error.
ok :: IO a -> CInt -> IO a
ok a 0 = a
ok a _ = wsagetlasterror >>= \case
  Pending -> a
  e -> throwsk e

-- | get the address info for the given node, service, and hints
getaddrinfo :: String -> String -> Maybe ADDRINFOW -> IO AddrInfo
getaddrinfo node service (Just hints) =
  withCWString node \n ->
  withCWString service \s ->
  alloca \h -> do
    poke h hints
    alloca \a ->
      getaddrinfow n s h a >>= ok do
        peek a >>= fmap AddrInfo . newForeignPtr freeaddrinfow
getaddrinfo node service Nothing =
  withCWString node \n ->
  withCWString service \s ->
  alloca \a ->
  getaddrinfow n s nullPtr a >>= ok do
    peek a >>= fmap AddrInfo . newForeignPtr freeaddrinfow

type GROUP = #{type GROUP}

data WSAPROTOCOL_INFOW

foreign import capi unsafe "winsock2.h WSASocketW"
  wsasocketw :: CInt -> CInt -> CInt -> Ptr (WSAPROTOCOL_INFOW)
               -> GROUP -> DWORD -> IO SOCKET

-- | address family. prefix: @AF_@
newtype AddrFamily = AddrFamily { unaddrfamily :: CInt }
  deriving (Show, Eq)

-- | Internet Protocol (IP) version 4
pattern AF_INET :: AddrFamily
pattern AF_INET = AddrFamily #{const AF_INET}

-- | on Windows Vista and later, AF_INET6 works in dual-mode IPv4 and IPv6.
-- it can communicate with both types of addresses. IPv4 addresses get mapped
-- to IPv6 addresses, using a compatibility encoding. it does not mean
-- that the protocol gets updated, though
pattern AF_INET6 :: AddrFamily
pattern AF_INET6 = AddrFamily #{const AF_INET6}

-- | socket type. prefix: @SOCK_@
newtype SocketType = SocketType { unsockettype :: CInt }
  deriving (Show, Eq)

-- | reliable byte stream; TCP
pattern SOCK_STREAM :: SocketType
pattern SOCK_STREAM = SocketType #{const SOCK_STREAM}

-- | protocol type. prefix: @IPPROTO_@
newtype Protocol = Protocol { unprotocol :: CInt }

pattern IPPROTO_TCP, IPPROTO_UDP :: Protocol
-- | TCP
pattern IPPROTO_TCP = Protocol #{const IPPROTO_TCP}
-- | UDP
pattern IPPROTO_UDP = Protocol #{const IPPROTO_UDP}

-- | open an overlapping (non-blocking) socket
socket :: AddrFamily -> SocketType -> Protocol -> IO SOCKET
socket af ty proto =
  let info = nullPtr
      grp = 0
      flags = #{const WSA_FLAG_OVERLAPPED}
   in wsasocketw (unaddrfamily af) (unsockettype ty)
                 (unprotocol proto) info grp flags >>= \case
        INVALID_SOCKET -> wsagetlasterror >>= throwsk
        s -> pure s

-- | a socket address (opaque)
data SockAddr

foreign import capi unsafe "winsock2.h bind"
  c_bind :: SOCKET -> Ptr SockAddr -> CInt -> IO CInt

-- | bind a socket to an address
bind :: SOCKET -> AddrInfo -> IO ()
bind s (AddrInfo ai) =
  withForeignPtr ai \pai -> do
    sa <- #{peek ADDRINFOW, ai_addr} pai
    c_bind s sa #{size struct sockaddr} >>= ok (pure ())

foreign import capi unsafe "winsock2.h listen"
  c_listen :: SOCKET -> CInt -> IO CInt

-- | allow a bound socket to listen for connections (TCP)
listen :: SOCKET -> IO ()
listen s = c_listen s 0 >>= ok (pure ())

-- | a virtual table for socket extensions
data VTABLE = VTABLE
  { sx_acceptex :: LPVOID
  , sx_connectex :: LPVOID
  , sx_disconnectex :: LPVOID
  , sx_getacceptexsockaddrs :: LPVOID
  , sx_transmitfile :: LPVOID
  , sx_transmitpackets :: LPVOID
  , sx_wsarecvmsg :: LPVOID
  , sx_wsasendmsg :: LPVOID
  }

foreign import capi unsafe "winsock2.h WSAIoctl"
  wsaioctl :: SOCKET -> DWORD -> LPVOID -> DWORD -> Ptr LPVOID ->
              DWORD -> LPDWORD -> LPVOID ->
              LPVOID -> IO CInt

data GUID = GUID
  { guid_Data1 :: #{type DWORD}  -- 4 bytes
  , guid_Data2 :: #{type WORD}   -- 2 bytes
  , guid_Data3 :: #{type WORD}   -- 2 bytes
  , guid_Data4 :: [#{type BYTE}] -- 8 bytes
  } deriving (Show, Eq)

instance Storable GUID where
  sizeOf _ = #{size GUID}
  alignment _ = #{alignment GUID}
  peek p = GUID
    <$> #{peek GUID, Data1} p
    <*> #{peek GUID, Data2} p
    <*> #{peek GUID, Data3} p
    <*> peekArray 8 (#{ptr GUID, Data4} p)
  poke p (GUID d1 d2 d3 d4) = do
    #{poke GUID, Data1} p d1
    #{poke GUID, Data2} p d2
    #{poke GUID, Data3} p d3
    pokeArray (#{ptr GUID, Data4} p) d4

newtype HSGUIDENUM = HSGUIDENUM CInt

#{enum HSGUIDENUM, HSGUIDENUM, HS_ACCEPTEX, HS_CONNECTEX,
  HS_DISCONNECTEX, HS_GETACCEPTEXSOCKADDRS, HS_TRANSMITFILE,
  HS_TRANSMITPACKETS, HS_WSARECVMSG, HS_WSASENDMSG}

foreign import capi "ax.h hs_getguid"
  -- this is a highly unsafe function: it invokes undefined behavior
  -- of the HSGUIDENUM is not one of the constants above.
  hs_getguid :: Ptr GUID -> HSGUIDENUM -> IO ()

-- load a dynamically loaded function
loadfunc :: SOCKET -> Ptr LPVOID -> Ptr GUID -> IO ()
loadfunc s f g =
  alloca \outsizptr ->
  wsaioctl
    s
    #{const SIO_GET_EXTENSION_FUNCTION_POINTER}
    do castPtr g
    #{size GUID}
    f
    #{size LPVOID}
    outsizptr
    nullPtr
    nullPtr
    >>= ok do pure ()

-- | load the socket extension functions
loadvt :: SOCKET -> IO VTABLE
loadvt s = do
  let l h = alloca \g -> do
        hs_getguid g h
        alloca \pp -> do
          loadfunc s pp g
          peek pp
  VTABLE
    <$> l hsAcceptex
    <*> l hsConnectex
    <*> l hsDisconnectex
    <*> l hsGetacceptexsockaddrs
    <*> l hsTransmitfile
    <*> l hsTransmitpackets
    <*> l hsWsarecvmsg
    <*> l hsWsasendmsg

-- | what gets disabled when a socket is 'shutdown'. prefix is @SD_@
newtype ShutdownHow = ShutdownHow CInt

pattern SD_RECEIVE, SD_SEND, SD_BOTH :: ShutdownHow
-- | disable 'recv'
pattern SD_RECEIVE = ShutdownHow #{const SD_RECEIVE}
-- | disable 'send'
pattern SD_SEND = ShutdownHow #{const SD_SEND}
-- | disable both 'recv' and 'send'
pattern SD_BOTH = ShutdownHow #{const SD_BOTH}

foreign import capi "winsock2.h shutdown"
  c_shutdown :: SOCKET -> ShutdownHow -> IO CInt

-- | disable reception, transmission, or both. see also: 'closesocket'.
--
-- this does not close the socket.
shutdown :: SOCKET -> ShutdownHow -> IO ()
shutdown s h = c_shutdown s h >>= ok do pure ()

foreign import capi "winsock2.h closesocket"
  c_closesocket :: SOCKET -> IO CInt

-- | close a socket
--
-- warnings from Microsoft (not all):
--
-- - depending on the linger structure, may or may not block.
-- - cannot assume all I/O operations will be ended when it returns.
-- - system may immediately reuse socket number.
closesocket :: SOCKET -> IO ()
closesocket = c_closesocket >=> ok do pure ()

type AcceptEx = SOCKET -> SOCKET -> LPVOID ->
                DWORD -> DWORD -> DWORD -> LPDWORD -> LPOVERLAPPED ->
                IO CBool

foreign import ccall "dynamic"
  vt_acceptex :: FunPtr AcceptEx -> AcceptEx

-- like 'ok', but for CBool
ok' :: IO a -> CBool -> IO a
ok' a = \case
  1 -> a
  0 -> wsagetlasterror >>= \case
        Pending -> a
        e -> throwsk e
  x -> error $ "ok': CBool returned something other than 1 or 0" ++ show x

-- | accept a connection from the listening socket; set up accepting socket
acceptex :: VTABLE -> SOCKET -> SOCKET -> LPOVERLAPPED -> IO ()
acceptex vt lis acc ol =
  let ax = vt_acceptex $ castPtrToFunPtr vt.sx_acceptex
      sz = fromIntegral @Int @DWORD $ 16 + #{size SOCKADDR_IN}
   in allocaBytes (3 * fromIntegral sz) \o ->
      alloca \b ->
      ax lis acc o 0 sz sz b ol >>= ok' do pure ()

foreign import capi "ax.h hs_finishaccept"
  hs_finishaccept :: SOCKET -> SOCKET -> IO CInt

-- | finish accepting a socket
finishaccept :: SOCKET -> SOCKET -> IO ()
finishaccept lis acc = hs_finishaccept lis acc >>= ok do pure ()

-- | get the socket address for use with 'connectex'
sockaddr :: AddrInfo -> IO (Ptr SockAddr)
sockaddr (AddrInfo ai) =
  withForeignPtr ai \a ->
  ai_addr <$> peek a

type ConnectEx = SOCKET -> Ptr SockAddr -> CInt -> LPVOID ->
                 DWORD -> LPDWORD -> LPOVERLAPPED -> IO CBool

foreign import ccall "dynamic"
  vt_connectex :: FunPtr ConnectEx -> ConnectEx

-- | given a bound socket, connect to a server
connectex :: VTABLE -> SOCKET -> Ptr SockAddr -> Int -> LPOVERLAPPED -> IO ()
connectex vt s a al ol =
  let cx = vt_connectex $ castPtrToFunPtr vt.sx_connectex
   in cx s a (fromIntegral al) nullPtr 0 nullPtr ol >>= ok' do pure ()

-- | run 'connectex' on an 'AddrInfo' using 'sockaddr' to extract the
-- @'Ptr' 'SockAddr'@
connectex' :: VTABLE -> SOCKET -> AddrInfo -> Int -> LPOVERLAPPED -> IO ()
connectex' vt s a al ol = sockaddr a >>= \b -> connectex vt s b al ol

foreign import capi "winsock2.h WSARecv"
  wsarecv :: SOCKET -> LPWSABUF -> DWORD -> LPDWORD -> LPDWORD ->
             LPOVERLAPPED -> LPVOID -> IO CInt

-- | structure identical to @WSABUF@ from \<ws2def.h\>
data WSABUF = WSABUF
  { wb_len :: ULONG
  , wb_buf :: Ptr CChar
  }

instance Storable WSABUF where
  sizeOf _ = #{size WSABUF}
  alignment _ = #{alignment WSABUF}
  peek p = liftA2 WSABUF
    do #{peek WSABUF, len} p
    do #{peek WSABUF, buf} p
  poke p (WSABUF l b) = do
    #{poke WSABUF, len} p l
    #{poke WSABUF, buf} p b

-- | a pointer to 'WSABUF'
type LPWSABUF = Ptr WSABUF

-- | receive into a buffer. see: 'recvmany', 'send'
recv :: SOCKET -> WSABUF -> LPOVERLAPPED -> IO ()
recv s b o =
  alloca \u -> do
    poke u b
    alloca \flags -> do
      poke flags 0
      -- 1. expects an array of buffers. here we give a length of 1.
      -- array is copied before wsarecv returns.
      -- 2. since lpCompletionRoutine is NULL, *lpOverlapped
      -- will be signaled once this routine completes.
      wsarecv s u 1 nullPtr flags o nullPtr >>= ok do pure ()

-- | receive multiple buffers. see: 'recv', 'sendmany'
recvmany :: SOCKET -> [WSABUF] -> LPOVERLAPPED -> IO ()
recvmany _ [] _ = pure ()
recvmany s bs o =
  withArrayLen bs \(fromIntegral -> lu) u ->
    alloca \flags -> do
      poke flags 0
      wsarecv s u lu nullPtr flags o nullPtr >>= ok do pure ()

foreign import capi "winsock2.h WSASend"
  wsasend :: SOCKET -> LPWSABUF -> DWORD -> LPDWORD -> DWORD ->
             LPOVERLAPPED -> LPVOID -> IO CInt

-- | send a buffer. see: 'sendmany', 'recv'
send :: SOCKET -> WSABUF -> LPOVERLAPPED -> IO ()
send s b o =
  alloca \u -> do
    poke u b
    wsasend s u 1 nullPtr 0 o nullPtr >>= ok do pure ()

-- | send multiple buffers. see: 'send', 'recvmany'
sendmany :: SOCKET -> [WSABUF] -> LPOVERLAPPED -> IO ()
sendmany _ [] _ = pure ()
sendmany s bs o =
  withArrayLen bs \(fromIntegral -> lu) u ->
  wsasend s u lu nullPtr 0 o nullPtr >>= ok do pure ()

-- | a managed 'SOCKET'. see: 'managesocket', 'close'
data Socket = Socket
  { -- | extract the underlying 'SOCKET'
    sksk :: SOCKET
  , skok :: IORef Bool
  }

-- | manage a socket so it will be closed when it goes out of scope.
-- uses 'closesocket' without 'shutdown'
managesocket :: SOCKET -> IO Socket
managesocket sksk = do
  skok <- newIORef True
  let sock = Socket {..}
  addFinalizer sock do
    readIORef skok >>= \case
      True -> atomicWriteIORef skok False *> closesocket sksk
      _ -> pure ()
  pure sock

-- | close a managed socket
close :: Socket -> IO ()
close (Socket s o) =
  readIORef o >>= \case
    True -> writeIORef o False *> closesocket s
    _ -> pure ()
