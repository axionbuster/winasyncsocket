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
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , AddrInfo
  , SocketError(..)
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
    -- * Functions
  , startup
  , socket
  , bind
  , listen
  , getaddrinfo
  ) where

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

-- yeah, they have slightly different contents, but we need both

#include <winsock2.h>
#include <winsock.h>
#include <ws2tcpip.h>
#include <mswsock.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINCALL stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINCALL ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

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

foreign import WINCALL "WSAStartup"
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

foreign import WINCALL "GetAddrInfoW"
  -- 3rd [in] pointer is marked "const": "const ADDRINFOW *"
  getaddrinfow :: LPCWSTR -> LPCWSTR -> Ptr ADDRINFOW -> Ptr (Ptr ADDRINFOW)
                  -> IO CInt

foreign import WINCALL "&FreeAddrInfoW"
  freeaddrinfow :: FinalizerPtr ADDRINFOW

foreign import WINCALL "WSAGetLastError"
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

foreign import WINCALL unsafe "WSASocketW"
  wsasocketw :: CInt -> CInt -> CInt -> Ptr (WSAPROTOCOL_INFOW)
               -> GROUP -> DWORD -> IO SOCKET

newtype AddrFamily = AddrFamily { unaddrfamily :: CInt }
  deriving (Show, Eq)

pattern AF_INET :: AddrFamily
pattern AF_INET = AddrFamily #{const AF_INET}

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

foreign import WINCALL unsafe "bind"
  c_bind :: SOCKET -> Ptr SockAddr -> CInt -> IO CInt

-- | bind a socket to an address
bind :: SOCKET -> AddrInfo -> IO ()
bind s (AddrInfo ai) =
  withForeignPtr ai \pai -> do
    sa <- #{peek ADDRINFOW, ai_addr} pai
    c_bind s sa #{size struct sockaddr} >>= ok (pure ())

foreign import WINCALL unsafe "listen"
  c_listen :: SOCKET -> CInt -> IO CInt

-- | allow a bound socket to listen for connections (TCP)
listen :: SOCKET -> IO ()
listen s = c_listen s 0 >>= ok (pure ())

-- | 'SOCKET' but with some function pointers attached
data SocketEx = SocketEx
  { sx_socket :: SOCKET
  , sx_acceptex :: AcceptEx
  }

data WSAOVERLAPPED

type LPWSAOVERLAPPED = Ptr WSAOVERLAPPED

type LPWSAOVERLAPPED_COMPLETION_ROUTINE =
  FunPtr (DWORD -> DWORD -> LPWSAOVERLAPPED -> DWORD -> IO ())

type AcceptEx =
  FunPtr (SOCKET -> SOCKET -> LPVOID -> DWORD -> DWORD ->
          DWORD -> LPDWORD -> LPOVERLAPPED -> IO CBool)

foreign import WINCALL unsafe "WSAIoctl"
  wsaioctl :: SOCKET -> DWORD -> LPVOID -> DWORD -> LPVOID ->
              DWORD -> LPDWORD -> LPWSAOVERLAPPED ->
              LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO CInt

loadax :: SOCKET -> WSAOVERLAPPED ->
          LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO SocketEx
loadax s o k =
  alloca \outsizptr ->
  alloca \guid -> do
    poke guid #{const WSAID_ACCEPTEX}
    allocaBytes #{size LPVOID} \ax ->
      wsaioctl
        s
        #{const SIO_GET_EXTENSION_FUNCTION_POINTER}
        guid
        #{size GUID}
        ax
        #{size LPVOID}
        outsizptr
        o
        k >>= ok (SocketEx s <$> peek ax)
