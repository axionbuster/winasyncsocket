{- |
Module      : Sock
Description : Socket operations and constants for Windows Sockets 2
Copyright   : (c) axionbuster, 2025
License     : BSD-3-Clause

This module provides low-level bindings to the Windows Sockets 2 API (Winsock2).
It includes socket operations, constants, and data structures needed for network
programming on Windows platforms.
-}
module Sock 
  ( -- * Types
    SOCKET
  , VTABLE(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , AddrInfo
  , SocketError(..)
  , ShutdownHow(..)
    -- * Type Patterns
  , pattern AF_INET
  , pattern AF_INET6
  , pattern SOCK_STREAM
  , pattern IPPROTO_TCP
  , pattern IPPROTO_UDP
  , pattern Success
  , pattern WouldBlock
  , pattern NotSupported
  , pattern INVALID_SOCKET
  , pattern SOCKET_ERROR
  , pattern SD_RECEIVE
  , pattern SD_SEND
  , pattern SD_BOTH
    -- * Functions
  , startup
  , socket
  , bind
  , listen
  , getaddrinfo
  , loadvt
  , shutdown
  , closesocket
  ) where

-- frustratingly, formatter 'ormolu' doesn't work.

import Control.Exception
import Control.Monad
import Data.Either
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe
import GHC.Event.Windows
import System.Win32.Types

-- i don't know if there's any way to fix the inclusion order warning:
--  #warning Please include winsock2.h before windows.h
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#include <mswsock.h>

#include "ax.h"

-- | raw socket error as returned by WSAGetLastError()
newtype SocketError = SocketError CInt
  deriving (Eq, Show, Storable)

instance Exception SocketError where
  -- terrible, but works for now...
  displayException (SocketError s) = "Socket error: " ++ show s

pattern Success :: SocketError
pattern Success = SocketError 0

pattern WouldBlock :: SocketError
pattern WouldBlock = SocketError #{const WSAEWOULDBLOCK}

pattern NotSupported :: SocketError
pattern NotSupported = SocketError #{const WSAVERNOTSUPPORTED}

pattern ConnectionReset :: SocketError 
pattern ConnectionReset = SocketError #{const WSAECONNRESET}

throwsk :: SocketError -> IO a
throwsk = throwIO

pattern SOCKET_ERROR :: CInt
pattern SOCKET_ERROR = #{const SOCKET_ERROR}

-- | raw socket type
newtype SOCKET = SOCKET { unsocket :: WordPtr }
  deriving (Eq, Show, Storable)

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

data ADDRINFOW = ADDRINFOW
  { ai_flags :: CInt
  , ai_family :: CInt
  , ai_socktype :: CInt
  , ai_protocol :: CInt
  , ai_addrlen :: CSize
  , ai_canonname :: LPWSTR
  , ai_addr :: Ptr ()
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

ok :: IO a -> CInt -> IO a
ok a 0 = a
ok _ _ = wsagetlasterror >>= throwsk

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

newtype AddrFamily = AddrFamily { unaddrfamily :: CInt }
  deriving (Show, Eq)

pattern AF_INET :: AddrFamily
pattern AF_INET = AddrFamily #{const AF_INET}

-- | on Windows Vista and later, AF_INET6 works in dual-mode IPv4 and IPv6.
-- it can communicate with both types of addresses. IPv4 addresses get mapped
-- to IPv6 addresses, using a compatibility encoding. it does not mean
-- that the protocol gets updated, though
pattern AF_INET6 :: AddrFamily
pattern AF_INET6 = AddrFamily #{const AF_INET6}

newtype SocketType = SocketType { unsockettype :: CInt }
  deriving (Show, Eq)

pattern SOCK_STREAM :: SocketType
pattern SOCK_STREAM = SocketType #{const SOCK_STREAM}

newtype Protocol = Protocol { unprotocol :: CInt }

pattern IPPROTO_TCP, IPPROTO_UDP :: Protocol
pattern IPPROTO_TCP = Protocol #{const IPPROTO_TCP}
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

-- not opaque, but is highly complicated
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

newtype HSGUIDENUM = HSGUIDENUM { ungenum :: CInt }

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

-- | what gets disabled when a socket is 'shutdown'
newtype ShutdownHow = ShutdownHow CInt

pattern SD_RECEIVE, SD_SEND, SD_BOTH :: ShutdownHow
pattern SD_RECEIVE = ShutdownHow #{const SD_RECEIVE}
pattern SD_SEND = ShutdownHow #{const SD_SEND}
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
