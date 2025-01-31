{- |
Module      : Network.SocketA.LinuxEPoll.Sock
Description : Socket operations and types for Linux/epoll
Copyright   : (c) axionbuster, 2025
License     : BSD-3-Clause

This module provides low-level bindings to POSIX/Linux socket APIs. It includes
socket operations, constants, and data structures needed for network programming
on Linux platforms.

Note that this module specifically targets Linux systems and makes use of Linux-specific
features like epoll and accept4() for better performance.

All sockets are created in non-blocking mode as this is required for use with epoll.
-}
module Network.SocketA.LinuxEPoll.Sock
  ( -- * Types
    Socket(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , AddrFlag(..)
  , GetAddrInfoError(..)
  , SockAddr(..)
  , InAddr(..)
  , AddrInfo
  , AddrInfo_(..)
  , ShutdownHow(..)
    -- * Patterns
  , pattern SOCK_NONBLOCK
  , pattern SOCK_CLOEXEC
  , pattern SOCK_STREAM
  , pattern AF_INET
  , pattern AF_INET6
  , pattern SHUT_RD
  , pattern SHUT_WR
  , pattern SHUT_RDWR
    -- * Operations
  , socket
  , close
  , shutdown
  , bind
  , bindfirst
  , bindfirst2
  , listen
  , accept
  , connect
  , getaddrinfo
  , recv
  , send
    -- * Constants and helpers
  , recvflags0
  , sendflags0
  , addrfamily0
  , sockettype0
  , protocol0
  , addrflag0
  , addrinfo0
  , peekin
  , pokesa
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Data
import Data.Functor
import Data.Void
import Foreign hiding (void)
import Foreign.C.ConstPtr
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

#include <netdb.h>
#include <sys/socket.h>

-- | A socket handle for Linux\/POSIX systems, wrapping a file descriptor.
-- All sockets are created in non-blocking mode by default.
newtype Socket = Socket { unsocket :: CInt }
  deriving newtype (Eq, Storable)
  deriving stock (Show)
-- | Socket address family\/domain identifier (AF_*)
newtype AddrFamily = AddrFamily { undomain :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | Socket type identifier (SOCK_*)
newtype SocketType = SocketType { unsockettype :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | Protocol identifier (IPPROTO_*)
newtype Protocol = Protocol { unprotocol :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | Address information flags (AI_*)
newtype AddrFlag = AddrFlag { unaddrflag :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

-- | zero address family
addrfamily0 :: AddrFamily
addrfamily0 = AddrFamily 0

-- | zero socket type
sockettype0 :: SocketType
sockettype0 = SocketType 0

-- | zero protocol
protocol0 :: Protocol
protocol0 = Protocol 0

-- | zero address flag
addrflag0 :: AddrFlag
addrflag0 = AddrFlag 0

-- result; negative 1 is error
newtype RN1 = RN1 { unrn1 :: CInt }
  deriving newtype (Show, Eq, Num)

-- perform the action unless the number is -1. if it
-- is, inspect errno immediately and then throw
okn1 :: String -> (RN1 -> IO a) -> RN1 -> IO a
okn1 m _ (-1) = throwErrno m
okn1 _ a n = a n

-- okn1, but return ()
okn1_ :: String -> RN1 -> IO ()
okn1_ m = okn1 m (const (pure ()))

-- is the error blocking?
blocking :: IO Bool
blocking = getErrno <&> \case
  e | e == eWOULDBLOCK || e == eAGAIN -> True
  _ -> False

foreign import capi unsafe "sys/socket.h socket"
  c_socket :: AddrFamily -> SocketType -> Protocol -> IO RN1

-- | Create a new non-blocking socket.
-- 
-- The socket is always created in non-blocking mode as required for epoll.
-- May throw 'GetAddrInfoError' on failure.
socket :: AddrFamily -> SocketType -> Protocol -> IO Socket
socket d s p =
  c_socket d (s .|. SOCK_NONBLOCK) p
    >>= okn1 "socket" (pure . Socket . unrn1)

pattern SOCK_NONBLOCK, SOCK_CLOEXEC, SOCK_STREAM :: SocketType
-- | non-blocking socket flag
pattern SOCK_NONBLOCK = SocketType #{const SOCK_NONBLOCK}
-- | close-on-exec socket flag
pattern SOCK_CLOEXEC = SocketType #{const SOCK_CLOEXEC}
-- | stream socket
pattern SOCK_STREAM = SocketType #{const SOCK_STREAM}

pattern AF_INET, AF_INET6 :: AddrFamily
-- | IPv4
pattern AF_INET = AddrFamily #{const AF_INET}
-- | IPv6
pattern AF_INET6 = AddrFamily #{const AF_INET}

newtype GetAddrInfoError = GetAddrInfoError { ungetaddrinfoerror :: CInt }
  deriving newtype (Eq)
  deriving stock (Show, Typeable)

-- | calls @gai_strerror@ to find the error message; if referred to errno,
-- finds the message for errno
instance Exception GetAddrInfoError where
  displayException f@(GetAddrInfoError e) = unsafePerformIO
    case e of
      #{const EAI_SYSTEM} -> pure "EAI_SYSTEM"
      _ -> do
        let ConstPtr a = c_gai_strerror f
        peekCString a

-- | type of 'ai_addrlen'
type Socklen = #{type socklen_t}

-- | Socket address structure for IPv4.
-- This implementation is opaque and managed through 'pokesa' and 'peekin'.
data SockAddr
  = SockAddrIn
    { sin_family :: SaFamilyT
    , sin_port :: InPortT
    , sin_addr :: InAddr
    }
  deriving (Eq, Show)

-- | @struct in_addr@
newtype InAddr = InAddr { s_addr :: InAddrT }
  deriving newtype (Eq, Storable)
  deriving stock (Show)

-- | @in_addr_t@
type InAddrT = #{type in_addr_t}

-- | - member 'peek' will throw an error when called
-- - 'sizeOf' is that of @struct sockaddr_storage@; beware when doing
-- array operations
instance Storable SockAddr where
  sizeOf _ = #{size struct sockaddr_storage}
  alignment _ = #{alignment struct sockaddr_storage}
  peek = error "peek SockAddr called"
  poke = pokesa

-- | type of 'sin_family'
type SaFamilyT = #{type sa_family_t}

-- | type of 'sin_port'
type InPortT = #{type in_port_t}

-- | peek a @struct sockaddr_in@ data structure
peekin :: Ptr SockAddr -> IO SockAddr
peekin p =
  SockAddrIn
    <$> #{peek struct sockaddr_in, sin_family} p
    <*> #{peek struct sockaddr_in, sin_port} p
    <*> #{peek struct sockaddr_in, sin_addr} p

-- | poke a 'SockAddr'
pokesa :: Ptr SockAddr -> SockAddr -> IO ()
pokesa p SockAddrIn {..} = do
  #{poke struct sockaddr_in, sin_family} p sin_family
  #{poke struct sockaddr_in, sin_port} p sin_port
  #{poke struct sockaddr_in, sin_addr} p sin_addr

-- | address information
--
-- contrast this with @ADDRINFOW@ (Win32); the locations of 'ai_addr' and
-- 'ai_canonname' are swapped.
data AddrInfo_ = AddrInfo_
  { ai_flags :: AddrFlag
  , ai_family :: AddrFamily
  , ai_socktype :: SocketType
  , ai_protocol :: Protocol
  , ai_addrlen :: Socklen
  , ai_addr :: Ptr SockAddr
  , ai_canonname :: CString
  , ai_next :: Ptr AddrInfo_
  } deriving (Eq, Show)

instance Storable AddrInfo_ where
  sizeOf _ = #{size struct addrinfo}
  alignment _ = #{alignment struct addrinfo}
  peek p = do
    flags <- peekByteOff p #{offset struct addrinfo, ai_flags}
    family <- peekByteOff p #{offset struct addrinfo, ai_family}
    socktype <- peekByteOff p #{offset struct addrinfo, ai_socktype}
    protocol <- peekByteOff p #{offset struct addrinfo, ai_protocol}
    addrlen <- peekByteOff p #{offset struct addrinfo, ai_addrlen}
    addr <- peekByteOff p #{offset struct addrinfo, ai_addr}
    canonname <- peekByteOff p #{offset struct addrinfo, ai_canonname}
    next <- peekByteOff p #{offset struct addrinfo, ai_next}
    pure AddrInfo_
      { ai_flags = AddrFlag flags
      , ai_family = AddrFamily family
      , ai_socktype = SocketType socktype
      , ai_protocol = Protocol protocol
      , ai_addrlen = addrlen
      , ai_addr = addr
      , ai_canonname = canonname
      , ai_next = next
      }
  poke p AddrInfo_ {..} = do
    pokeByteOff p #{offset struct addrinfo, ai_flags} (unaddrflag ai_flags)
    pokeByteOff p #{offset struct addrinfo, ai_family} (undomain ai_family)
    pokeByteOff p #{offset struct addrinfo, ai_socktype} (unsockettype ai_socktype)
    pokeByteOff p #{offset struct addrinfo, ai_protocol} (unprotocol ai_protocol)
    pokeByteOff p #{offset struct addrinfo, ai_addrlen} ai_addrlen
    pokeByteOff p #{offset struct addrinfo, ai_addr} ai_addr
    pokeByteOff p #{offset struct addrinfo, ai_canonname} ai_canonname
    pokeByteOff p #{offset struct addrinfo, ai_next} ai_next

-- | the zero value for 'AddrInfo_'
addrinfo0 :: AddrInfo_
addrinfo0 = AddrInfo_
  { ai_flags = addrflag0
  , ai_family = addrfamily0
  , ai_socktype = sockettype0
  , ai_protocol = protocol0
  , ai_addrlen = 0
  , ai_addr = nullPtr
  , ai_canonname = nullPtr
  , ai_next = nullPtr
  }

foreign import capi unsafe "netdb.h getaddrinfo"
  c_getaddrinfo :: ConstPtr CChar -> ConstPtr CChar -> ConstPtr AddrInfo_ ->
                   Ptr (Ptr AddrInfo_) -> IO GetAddrInfoError

foreign import capi unsafe "netdb.h &freeaddrinfo"
  c_freeaddrinfo :: FunPtr (Ptr AddrInfo_ -> IO ())

foreign import capi unsafe "netdb.h gai_strerror"
  c_gai_strerror :: GetAddrInfoError -> ConstPtr CChar

-- | a managed 'AddrInfo_' list (head only); never NULL
type AddrInfo = ForeignPtr AddrInfo_

-- | Lookup network addresses. This is a high-level wrapper around
-- the @getaddrinfo(3)@ system call.
--
-- if it fails, it throws a 'GetAddrInfoError' or 'IOError'
getaddrinfo :: String -> String -> Maybe AddrInfo_ -> IO (ForeignPtr AddrInfo_)
getaddrinfo node service hints =
  withCString node \(ConstPtr -> n) ->
  withCString service \(ConstPtr -> s) ->
  alloca \h -> do
    i <- case hints of
      Just j -> poke h j $> h
      Nothing -> pure nullPtr
    alloca \r -> mask_ do
      c_getaddrinfo n s (ConstPtr i) r >>= \case
          GetAddrInfoError 0 -> peek r >>= newForeignPtr c_freeaddrinfo
          GetAddrInfoError (#{const EAI_SYSTEM}) -> throwErrno "getaddrinfo"
          e -> throwIO e

foreign import capi unsafe "unistd.h close"
  c_close :: Socket -> IO RN1

foreign import capi unsafe "sys/socket.h bind"
  c_bind :: Socket -> ConstPtr SockAddr -> Socklen -> IO RN1

-- | Close a socket, releasing all associated system resources.
-- 
-- This operation is immediate and cannot be undone.
close :: Socket -> IO ()
close = c_close >=> okn1_ "close"

foreign import capi unsafe "sys/socket.h shutdown"
  c_shutdown :: Socket -> ShutdownHow -> IO RN1

-- | Partially or fully shut down a socket connection.
--
-- Unlike 'close', this does not release system resources.
-- Use 'SHUT_RD', 'SHUT_WR', or 'SHUT_RDWR' to specify which operations to disable.
shutdown :: Socket -> ShutdownHow -> IO ()
shutdown s h = c_shutdown s h >>= okn1_ "shutdown"

-- | how a socket will be shut down in 'shutdown'
newtype ShutdownHow = ShutdownHow CInt
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

pattern SHUT_RD, SHUT_WR, SHUT_RDWR :: ShutdownHow
-- | disallow further reading
pattern SHUT_RD = ShutdownHow #{const SHUT_RD}
-- | disallow further writing
pattern SHUT_WR = ShutdownHow #{const SHUT_WR}
-- | disallow further reading and writing
pattern SHUT_RDWR = ShutdownHow #{const SHUT_RDWR}

-- | Bind a socket to a local address.
--
-- Required before 'listen' for server sockets.
bind :: Socket -> SockAddr -> IO ()
bind s a =
  alloca \a' -> do
    poke a' a
    c_bind s (ConstPtr a') #{size struct sockaddr_storage} >>= okn1_ "bind"

-- | Try to bind to addresses in sequence until one succeeds.
--
-- Takes a linked list of addresses and attempts to bind until success.
-- Throws an error if all attempts fail.
bindfirst :: Socket -> AddrInfo -> IO ()
bindfirst s l =
  withForeignPtr l \h ->
  peek h >>= fix \r h' ->
  c_bind s (ConstPtr h'.ai_addr) h'.ai_addrlen >>= \case
    0 -> pure ()
    _ -> case h'.ai_next of
      p | p == nullPtr -> throwErrno "bindfirst"
      p -> peek p >>= r

-- | Create and bind a socket to the first successful address.
--
-- Combines socket creation and binding in one operation.
-- Returns the newly created and bound socket.
bindfirst2 :: AddrInfo -> IO Socket
bindfirst2 l =
  withForeignPtr l \h ->
  peek h >>= fix \r h' ->
  c_socket h'.ai_family h'.ai_socktype h'.ai_protocol >>= \case
    (-1) -> case h'.ai_next of
      p | p == nullPtr -> throwErrno "bindfirst2 (socket)"
      p -> peek p >>= r
    (Socket . unrn1 -> s) -> mask \restore ->
      c_bind s (ConstPtr h'.ai_addr) h'.ai_addrlen >>= \case
        0 -> restore $ pure s
        _ ->
          c_close s >>= \case
            0 ->
              restore case h'.ai_next of
                p | p == nullPtr -> throwErrno "bindfirst2 (bind)"
                p -> peek p >>= r
            _ -> restore do
              -- if closing a socket fails, stop iterating;
              -- something serious might be going on
              throwErrno "bindfirst2 (bind; close)"

foreign import capi unsafe "sys/socket.h listen"
  c_listen :: Socket -> CInt -> IO RN1

-- | Mark a socket as passive for accepting incoming connections.
--
-- Must be called before using 'accept' on a socket.
listen ::
  Socket ->  -- ^ The socket to mark as passive
  Int ->     -- ^ Maximum length of the pending connections queue
  IO ()
listen s (fromIntegral -> i) = c_listen s i >>= okn1_ "listen"

-- now we begin to have nonblocking async calls...

-- ok unless negative 1; on negative 1, check for blocking. if blocking,
-- throw empty; otherwise, return
okn1asy :: String -> (RN1 -> IO a) -> RN1 -> IO a
okn1asy n _ (-1) = blocking >>= \case
  True -> empty
  False -> throwErrno n
okn1asy _ a e = a e

-- accept4 is Linux API; saves a call to fcntl to mark the socket nonblocking
foreign import capi unsafe "sys/socket.h accept4"
  -- 2nd, 3rd: restrict, nullable
  -- only accepted flags are SOCK_NONBLOCK and SOCK_CLOEXEC
  c_accept4 :: Socket -> Ptr SockAddr -> Ptr Socklen -> SocketType -> IO RN1

-- | Accept a new connection on a listening socket.
--
-- Returns the newly created connected socket.
-- Throws 'empty' if the operation would block.
accept :: Socket -> SockAddr -> IO Socket
accept s a =
  alloca \sa -> do
    poke sa a
    alloca \sl -> do
      poke sl (fromIntegral . sizeOf $ a)
      -- FIXME: must retrieve from underlying socket
      let st = SOCK_NONBLOCK .|. SOCK_CLOEXEC
      mask_ do
        c_accept4 s sa sl st >>= okn1asy "accept" do
          pure . Socket . unrn1

foreign import capi unsafe "sys/socket.h connect"
  c_connect :: Socket -> ConstPtr SockAddr -> Socklen -> IO RN1

-- | Connect a socket to a remote address.
--
-- For non-blocking sockets, may throw 'empty' before the connection completes.
-- The caller should wait for writability to detect connection completion.
connect :: Socket -> SockAddr -> IO ()
connect s a =
  alloca \sa -> do
    poke sa a
    c_connect s (ConstPtr sa) (fromIntegral . sizeOf $ a) >>= \case
      -1 -> blocking >>= \case
        True -> empty
        False -> throwErrno "connect"
      _ -> pure ()

-- | @ssize_t@; not defined in "Foreign.C.Types" for some reason
type CSsize = #{type ssize_t}

-- | flags for use in 'recv'
newtype RecvFlags = RecvFlags CInt
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

-- | zero recv flags
recvflags0 :: RecvFlags
recvflags0 = RecvFlags 0

foreign import capi unsafe "sys/socket.h recv"
  c_recv :: Socket -> Ptr Void -> CSize -> RecvFlags -> IO CSsize

-- | Receive data from a socket into a buffer.
--
-- Returns number of bytes read.
-- Throws 'empty' if it would block.
-- Returns 0 on end-of-file for stream sockets.
recv :: Socket -> Ptr Void -> CSize -> RecvFlags -> IO CSsize
recv s b l f = c_recv s b l f >>= \case
  -1 -> blocking >>= \case
    True -> empty
    False -> throwErrno "recv"
  n -> pure n

-- | flags for use in 'send'
newtype SendFlags = SendFlags CInt
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

-- | zero send flags
sendflags0 :: SendFlags
sendflags0 = SendFlags 0

foreign import capi unsafe "sys/socket.h send"
  c_send :: Socket -> Ptr Void -> CSize -> SendFlags -> IO CSsize

-- | Send data from a buffer through a socket.
--
-- Returns number of bytes sent.
-- Throws 'empty' if operation would block.
send :: Socket -> Ptr Void -> CSize -> SendFlags -> IO CSsize
send s b l f = c_send s b l f >>= \case
  -1 -> blocking >>= \case
    True -> empty
    False -> throwErrno "send"
  n -> pure n
