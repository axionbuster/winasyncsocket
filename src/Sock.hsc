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
  , AddrFlag(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , ADDRINFOW(..)
  , SockAddr
  , SockoptLevel(..)
  , SockoptName(..)
  , AddrInfo(..)
  , SocketError(..)
  , ShutdownHow(..)
  , WSABUF(..)
  , LPWSABUF
    -- * Patterns
  , pattern ADDRINFOW0
  , pattern AI_zero
  , pattern AI_PASSIVE
  , pattern AI_V4MAPPED
  , pattern AI_ALL
  , pattern AF_zero
  , pattern AF_INET
  , pattern AF_INET6
  , pattern SOCK_zero
  , pattern SOCK_STREAM
  , pattern IPPROTO_zero
  , pattern IPPROTO_TCP
  , pattern IPPROTO_UDP
  , pattern SOL_SOCKET
  , pattern IPPROTO_IPV6
  , pattern SO_REUSEADDR
  , pattern IPV6_V6ONLY
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
  , setsockopt_dword
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

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Data.List (unsnoc)
import Debug.Trace
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
  deriving newtype (Eq, Storable)
  deriving stock (Show)

foreign import capi unsafe "winbase.h LocalFree"
  localfree :: LPVOID -> IO ()

foreign import capi unsafe "winbase.h FormatMessageW"
  -- the type of the 5th argument [out lpBuffer; LPWSTR*] depends on a flag in
  -- the 1st argument [in dwFlags; DWORD]: if it contains
  -- FORMAT_MESSAGE_ALLOCATE_BUFFER, then lpBuffer has type LPWSTR *, and
  -- it will point to the OS-allocated string to be freed by using LocalFree.
  -- otherwise, lpBuffer has type LPTSTR, and we need to allocate it ourselves.
  formatmessagew :: DWORD -> LPVOID -> DWORD -> DWORD -> Ptr LPWSTR -> DWORD ->
                    LPVOID -> IO DWORD

foreign import capi unsafe "winnt.h MAKELANGID"
  makelangid :: DWORD -> DWORD -> DWORD

-- | 'displayException': we make a /syscall/ to fetch the message
instance Exception SocketError where
  displayException (SocketError s) = "Socket error: " ++ z m
    where
      -- z: remove trailing newline
      z r = case unsnoc r of
        Nothing -> ""
        Just (rr, '\n') -> rr
        _ -> r
      m = unsafeDupablePerformIO do
        mask_ do
          alloca \b ->
            formatmessagew
              do #{const FORMAT_MESSAGE_ALLOCATE_BUFFER}
                  .|. #{const FORMAT_MESSAGE_FROM_SYSTEM}
              do nullPtr
              do fromIntegral s
              do makelangid #{const LANG_NEUTRAL} #{const SUBLANG_NEUTRAL}
              do b
              do 0
              do nullPtr -- va_list * [in, optional]
              >>= \case
                -- 0: failure, resort to showing number
                0 -> pure $ "<Winsock2 error code " ++ show s ++ ">"
                n -> do
                  u <- peek b
                  peekCWStringLen (u, fromIntegral n) <* localfree (castPtr u)

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
  deriving newtype (Eq, Storable)
  deriving stock (Show)

-- | a placeholder socket number
pattern INVALID_SOCKET :: SOCKET
pattern INVALID_SOCKET = SOCKET #{const INVALID_SOCKET}

-- raw operations

data WSADATA
type LPWSADATA = Ptr WSADATA

foreign import capi unsafe "winsock2.h WSAStartup"
  wsastartup :: WORD -> LPWSADATA -> IO SocketError

foreign import capi unsafe "winsock2.h WSASetLastError"
  wsasetlasterror :: SocketError -> IO ()

foreign import capi unsafe "winsock2.h WSACleanup"
  wsacleanup :: IO CInt

-- | start up Windows Sockets v2.2. check version for 2.2.
startup :: IO ()
startup =
  allocaBytes #{size WSADATA} \d ->
  wsastartup 0x0202 d >>= \case
    Success -> do
      (w :: WORD) <- #{peek WSADATA, wVersion} d
      unless (w == 0x0202) do
        wsasetlasterror NotSupported
        void wsacleanup -- we can't do anything about an error here
        throwsk NotSupported
      pure ()
    e -> throwsk e

-- | a socket option level. prefix: many. see Microsoft's documentation.
newtype SockoptLevel = SockoptLevel { unsockoptlevel :: CInt }
  deriving newtype (Eq, Storable)
  deriving stock (Show)

-- | general socket options
pattern SOL_SOCKET :: SockoptLevel
pattern SOL_SOCKET = SockoptLevel #{const SOL_SOCKET}

-- | IPv6 socket options
pattern IPPROTO_IPV6 :: SockoptLevel
pattern IPPROTO_IPV6 = SockoptLevel #{const IPPROTO_IPV6}

-- | socket options
newtype SockoptName = SockoptName { unsockoptname :: CInt }
  deriving newtype (Eq, Storable)
  deriving stock (Show)

-- | reuse of the local address and port
pattern SO_REUSEADDR :: SockoptName
pattern SO_REUSEADDR = SockoptName #{const SO_REUSEADDR}

-- | enable or disable the dual-mode socket option for IPv6 sockets
--
-- use 'setsockopt_dword' with either 1 or 0 to set this option
pattern IPV6_V6ONLY :: SockoptName
pattern IPV6_V6ONLY = SockoptName #{const IPV6_V6ONLY}

foreign import capi unsafe "winsock2.h setsockopt"
  c_setsockopt :: SOCKET -> SockoptLevel ->
                  SockoptName -> Ptr CChar -> CInt -> IO CInt

-- methinks it's too dangerous to make a general setsockopt that
-- works with all Storable types. it's better to have a separate
-- function for each type of value. or maybe do a typeclass
-- or type family. idk.

-- | set a socket option with a DWORD value
setsockopt_dword :: SOCKET -> SockoptLevel -> SockoptName -> DWORD -> IO ()
setsockopt_dword s l n v =
  alloca \p -> do
    poke p v
    mask_ do c_setsockopt s l n (castPtr p) #{size DWORD} >>= ok (pure ())

-- | the @ADDRINFOW@ structure from \<ws2def.h\>
data ADDRINFOW = ADDRINFOW
  { ai_flags :: AddrFlag
  , ai_family :: AddrFamily
  , ai_socktype :: SocketType
  , ai_protocol :: Protocol
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
pattern ADDRINFOW0 =
  ADDRINFOW AI_zero AF_zero SOCK_zero IPPROTO_zero 0 NULL NULL NULL

-- | a 'ForeignPtr' wrapper over 'ADDRINFOW'. it frees the 'ADDRINFOW' using
-- the correct function
newtype AddrInfo = AddrInfo (ForeignPtr ADDRINFOW)
  deriving (Eq, Show)

foreign import capi unsafe "ws2tcpip.h GetAddrInfoW"
  -- 3rd [in] pointer is marked "const": "const ADDRINFOW *"
  getaddrinfow :: LPCWSTR -> LPCWSTR -> Ptr ADDRINFOW -> Ptr (Ptr ADDRINFOW)
                  -> IO CInt

foreign import capi unsafe "ws2tcpip.h &FreeAddrInfoW"
  freeaddrinfow :: FinalizerPtr ADDRINFOW

foreign import capi unsafe "winsock2.h WSAGetLastError"
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
    alloca \a -> mask_ do
      getaddrinfow n s h a >>= ok do
        peek a >>= fmap AddrInfo . newForeignPtr freeaddrinfow
getaddrinfo node service Nothing =
  withCWString node \n ->
  withCWString service \s ->
  alloca \a -> mask_ do
    getaddrinfow n s nullPtr a >>= ok do
      peek a >>= fmap AddrInfo . newForeignPtr freeaddrinfow

type GROUP = #{type GROUP}

data WSAPROTOCOL_INFOW

foreign import capi unsafe "winsock2.h WSASocketW"
  wsasocketw :: CInt -> CInt -> CInt -> Ptr (WSAPROTOCOL_INFOW)
               -> GROUP -> DWORD -> IO SOCKET

-- | address info flag. prefix: @AI_@.
newtype AddrFlag = AddrFlag { unaddrflag :: CInt }
  deriving (Show, Eq)
  deriving (Storable, Bits, FiniteBits) via (CInt)

-- | address family. prefix: @AF_@
newtype AddrFamily = AddrFamily { unaddrfamily :: CInt }
  deriving (Show, Eq)
  deriving (Storable, Bits, FiniteBits) via (CInt)

-- | socket type. prefix: @SOCK_@
newtype SocketType = SocketType { unsockettype :: CInt }
  deriving (Show, Eq)
  deriving (Storable, Bits, FiniteBits) via (CInt)

-- | protocol type. prefix: @IPPROTO_@
newtype Protocol = Protocol { unprotocol :: CInt }
  deriving (Show, Eq)
  deriving (Storable, Bits, FiniteBits) via (CInt)

-- | zero 'AddrFlag'
pattern AI_zero :: AddrFlag
pattern AI_zero = AddrFlag 0

-- | allow wildcard addresses
pattern AI_PASSIVE, AI_V4MAPPED, AI_ALL :: AddrFlag
pattern AI_PASSIVE = AddrFlag #{const AI_PASSIVE}
pattern AI_V4MAPPED = AddrFlag #{const AI_V4MAPPED}
pattern AI_ALL = AddrFlag #{const AI_ALL}

-- | zero 'AddrFamily'
pattern AF_zero :: AddrFamily
pattern AF_zero = AddrFamily 0

-- | Internet Protocol (IP) version 4
pattern AF_INET :: AddrFamily
pattern AF_INET = AddrFamily #{const AF_INET}

-- | on Windows Vista and later, AF_INET6 works in dual-mode IPv4 and IPv6.
-- it can communicate with both types of addresses. IPv4 addresses get mapped
-- to IPv6 addresses, using a compatibility encoding. it does not mean
-- that the protocol gets updated, though
pattern AF_INET6 :: AddrFamily
pattern AF_INET6 = AddrFamily #{const AF_INET6}

-- | zero 'SocketType'
pattern SOCK_zero :: SocketType
pattern SOCK_zero = SocketType 0

-- | reliable byte stream; TCP
pattern SOCK_STREAM :: SocketType
pattern SOCK_STREAM = SocketType #{const SOCK_STREAM}

-- | zero 'Protocol'
pattern IPPROTO_zero :: Protocol
pattern IPPROTO_zero = Protocol 0

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
   in mask_ do
        wsasocketw (unaddrfamily af) (unsockettype ty)
                   (unprotocol proto) info grp flags >>= \case
          INVALID_SOCKET -> wsagetlasterror >>= throwsk
          s -> pure s

-- | a socket address (opaque)
data SockAddr

foreign import capi unsafe "winsock2.h bind"
  c_bind :: SOCKET -> Ptr SockAddr -> CInt -> IO CInt

-- | bind a socket to an address
bind :: SOCKET -> AddrInfo -> IO ()
bind s (AddrInfo ai) = mask_ do
  withForeignPtr ai \pai -> do
    sa <- #{peek ADDRINFOW, ai_addr} pai
    le <- #{peek ADDRINFOW, ai_addrlen} pai
    c_bind s sa le >>= ok (pure ())

foreign import capi unsafe "winsock2.h listen"
  c_listen :: SOCKET -> CInt -> IO CInt

-- | allow a bound socket to listen for connections (TCP)
listen :: SOCKET -> IO ()
listen s = mask_ do c_listen s 128 >>= ok (pure ())

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

foreign import capi unsafe "ax.h hs_getguid"
  -- this is a highly unsafe function: it invokes undefined behavior
  -- of the HSGUIDENUM is not one of the constants above.
  hs_getguid :: Ptr GUID -> HSGUIDENUM -> IO ()

-- load a dynamically loaded function
loadfunc :: SOCKET -> Ptr LPVOID -> Ptr GUID -> IO ()
loadfunc s f g =
  alloca \outsizptr ->
  mask_ do
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
        alloca \pp -> mask_ do
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

foreign import capi unsafe "winsock2.h shutdown"
  c_shutdown :: SOCKET -> ShutdownHow -> IO CInt

-- | disable reception, transmission, or both. see also: 'closesocket'.
--
-- this does not close the socket.
shutdown :: SOCKET -> ShutdownHow -> IO ()
shutdown s h = mask_ do c_shutdown s h >>= ok (pure ())

foreign import capi unsafe "winsock2.h closesocket"
  c_closesocket :: SOCKET -> IO CInt

-- | close a socket
--
-- warnings from Microsoft (not all):
--
-- - depending on the linger structure, may or may not block.
-- - cannot assume all I/O operations will be ended when it returns.
-- - system may immediately reuse socket number.
closesocket :: SOCKET -> IO ()
closesocket s = mask_ do c_closesocket s >>= ok (pure ())

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
      mask_ do
        ax lis acc o 0 sz sz b ol >>= ok' do pure ()

foreign import capi unsafe "ax.h hs_finishaccept"
  hs_finishaccept :: SOCKET -> SOCKET -> IO CInt

-- | finish accepting a socket
finishaccept :: SOCKET -> SOCKET -> IO ()
finishaccept lis acc = mask_ do hs_finishaccept lis acc >>= ok (pure ())

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
   in mask_ do
        cx s a (fromIntegral al) nullPtr 0 nullPtr ol >>= ok' do pure ()

-- | run 'connectex' on an 'AddrInfo' using 'sockaddr' to extract the
-- @'Ptr' 'SockAddr'@
connectex' :: VTABLE -> SOCKET -> AddrInfo -> Int -> LPOVERLAPPED -> IO ()
connectex' vt s a al ol = sockaddr a >>= \b -> connectex vt s b al ol

foreign import capi unsafe "winsock2.h WSARecv"
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
    alloca \flags -> mask_ do
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
    alloca \flags -> mask_ do
      poke flags 0
      wsarecv s u lu nullPtr flags o nullPtr >>= ok do pure ()

foreign import capi unsafe "winsock2.h WSASend"
  wsasend :: SOCKET -> LPWSABUF -> DWORD -> LPDWORD -> DWORD ->
             LPOVERLAPPED -> LPVOID -> IO CInt

-- | send a buffer. see: 'sendmany', 'recv'
send :: SOCKET -> WSABUF -> LPOVERLAPPED -> IO ()
send s b o =
  alloca \u -> mask_ do
    poke u b
    wsasend s u 1 nullPtr 0 o nullPtr >>= ok do pure ()

-- | send multiple buffers. see: 'send', 'recvmany'
sendmany :: SOCKET -> [WSABUF] -> LPOVERLAPPED -> IO ()
sendmany _ [] _ = pure ()
sendmany s bs o =
  withArrayLen bs \(fromIntegral -> lu) u -> mask_ do
    wsasend s u lu nullPtr 0 o nullPtr >>= ok do pure ()

-- | a managed 'SOCKET'. see: 'managesocket', 'close'
data Socket = Socket
  { -- | extract the underlying 'SOCKET' so we can use raw API.
    -- be careful when using an unmanaged socket
    sksk :: SOCKET
    -- mutex for finalization. if locked, it's either closing or has been closed
  , skok :: MVar ()
  }

instance Show Socket where
  show = show . sksk

-- | manage a socket so it will be closed when it goes out of scope.
-- uses 'closesocket' without 'shutdown'
managesocket :: SOCKET -> IO Socket
managesocket sksk = do
  skok <- newMVar ()
  let x = Socket {..}
  addFinalizer x do
    -- strictly speaking, on exception, there's no point in unlocking
    -- the mutex, but i'm just gonna do it
    mask_ do
      catch
        do tryTakeMVar skok >>= maybe (pure ()) do
             const do closesocket sksk
        do -- if finalizer fails, yell in the console
           \(e :: SocketError) -> putMVar skok () *>
             traceIO do displayException e
  pure x

-- | close a managed socket
close :: Socket -> IO ()
close Socket {..} =
  (tryTakeMVar skok >>= maybe (pure ()) \_ -> closesocket sksk)
    `onException` putMVar skok ()
