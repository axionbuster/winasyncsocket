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
  , SocketError(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , AddrFlag(..)
  , GetAddrInfoError(..)
  , SockAddr(..)
  , InAddr(..)
  , AddrInfo
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

-- | A socket handle on Linux\/POSIX systems. This is a thin wrapper around
-- a file descriptor (represented as 'CInt').
newtype Socket = Socket { unsocket :: CInt }
  deriving newtype (Eq, Storable)
  deriving stock (Show)
-- | Socket error type, wrapping the error number returned by socket operations.
newtype SocketError = SocketError { getskerr :: CInt }
  deriving newtype (Eq, Storable)
  deriving stock (Show)

-- | socket address family\/domain
newtype AddrFamily = AddrFamily { undomain :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | socket type\/configuration
newtype SocketType = SocketType { unsockettype :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | socket protocol
newtype Protocol = Protocol { unprotocol :: CInt }
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)
-- | address flag
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

-- | Create a non-blocking socket. The non-blocking flag will be
-- unconditionally set because it is required for epoll operation.
--
-- This is the primary way to create a new socket.
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
pattern AF_INET6 = AddrFamily #{const AF_INET6}

newtype GetAddrInfoError = GetAddrInfoError { ungetaddrinfoerror :: CInt }
  deriving newtype (Eq)
  deriving stock (Show, Typeable)

foreign import capi unsafe "fe.h hs_strerror_r1"
  hs_strerror_r1 :: CInt -> CString -> CSize -> IO CInt

-- calls @strerror_r@ to find the error message
errno2string :: Errno -> IO String
errno2string (Errno e) =
  allocaBytes 256 \buf ->
    hs_strerror_r1 e buf 256 >>= \case
      0 -> peekCString buf
      _ -> pure "errno2string: strerror_r failed"

-- | calls @gai_strerror@ to find the error message; if referred to errno,
-- finds the message for errno
instance Exception GetAddrInfoError where
  displayException f@(GetAddrInfoError e) = unsafePerformIO
    case e of
      #{const EAI_SYSTEM} -> errno2string =<< getErrno
      _ -> do
        let ConstPtr a = c_gai_strerror f
        peekCString a

-- | type of 'ai_addrlen'
type Socklen = #{type socklen_t}

-- | socket address
--
-- unlike on Windows, this data structure is opaque, and not exposed to users
--
-- it also does not /truly/ implement 'Storable'
-- because it uses a disjunctive type
-- that's only known by the programmer. see 'pokesa', 'peekin'
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
-- if it fails, it throws a 'GetAddrInfoError', not a 'SocketError'
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
          e -> throwIO e

foreign import capi unsafe "unistd.h close"
  c_close :: Socket -> IO RN1

foreign import capi unsafe "sys/socket.h bind"
  c_bind :: Socket -> ConstPtr SockAddr -> Socklen -> IO RN1

-- | Close a socket file descriptor. This releases system resources
-- associated with the socket.
close :: Socket -> IO ()
close = c_close >=> okn1_ "close"

foreign import capi unsafe "sys/socket.h shutdown"
  c_shutdown :: Socket -> ShutdownHow -> IO RN1

-- | Shut down part of a full-duplex connection.
-- Unlike 'close', this does not release socket resources.
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
bind :: Socket -> SockAddr -> IO ()
bind s a =
  alloca \a' -> do
    poke a' a
    c_bind s (ConstPtr a') #{size struct sockaddr_storage} >>= okn1_ "bind"

-- | Try to bind to addresses in sequence until one succeeds.
-- Throws an error if none succeed.
bindfirst :: Socket -> AddrInfo -> IO ()
bindfirst s l =
  withForeignPtr l \h ->
  peek h >>= fix \r h' ->
  c_bind s (ConstPtr h'.ai_addr) h'.ai_addrlen >>= \case
    0 -> pure ()
    _ -> case h'.ai_next of
      p | p == nullPtr -> throwErrno "bindfirst"
      p -> peek p >>= r

-- | Create a socket and bind it to the first successful address.
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

-- | Mark a socket as passive, indicating it will be used to accept
-- incoming connection requests using 'accept'.
listen ::
  Socket ->  -- ^ The socket to mark as passive
  Int ->     -- ^ Maximum length of the pending connections queue
  IO ()
listen s (fromIntegral -> i) = c_listen s i >>= okn1_ "listen"

-- now we begin to have nonblocking async calls...

-- ok unless negative 1; on negative 1, check for blocking. if blocking,
-- return empty; otherwise, return something
okn1asy :: (Alternative f) =>
  String -> (RN1 -> IO (f a)) -> RN1 -> IO (f a)
okn1asy n _ (-1) = blocking >>= \case
  True -> pure empty
  False -> throwErrno n
okn1asy _ a e = a e

-- accept4 is Linux API; saves a call to fcntl to mark the socket nonblocking
foreign import capi unsafe "sys/socket.h accept4"
  -- 2nd, 3rd: restrict, nullable
  -- only accepted flags are SOCK_NONBLOCK and SOCK_CLOEXEC
  c_accept4 :: Socket -> Ptr SockAddr -> Ptr Socklen -> SocketType -> IO RN1

-- | Accept a new connection on a listening socket.
-- Returns 'Nothing' if would block, 'Just' 'Socket' if successful.
accept :: Socket -> SockAddr -> IO (Maybe Socket)
accept s a =
  alloca \sa -> do
    poke sa a
    alloca \sl -> do
      poke sl (fromIntegral . sizeOf $ a)
      -- FIXME: must retrieve from underlying socket
      let st = SOCK_NONBLOCK .|. SOCK_CLOEXEC
      mask_ do
        c_accept4 s sa sl st >>= okn1asy "accept" do
          pure . Just . Socket . unrn1

foreign import capi unsafe "sys/socket.h connect"
  c_connect :: Socket -> ConstPtr SockAddr -> Socklen -> IO RN1

-- | Connect a socket to a remote address.
-- May throw 'empty' before connection is complete due to non-blocking nature.
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
-- Returns 'Nothing' if would block, 'Just' @bytes_read@ if successful.
recv :: Socket -> Ptr Void -> CSize -> RecvFlags -> IO (Maybe CSsize)
recv s b l f = c_recv s b l f >>= \case
  -1 -> blocking >>= \case
    True -> pure Nothing
    False -> throwErrno "recv"
  n -> pure (Just n)

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
-- Returns 'Nothing' if would block, 'Just' @bytes_sent@ if successful.
send :: Socket -> Ptr Void -> CSize -> SendFlags -> IO (Maybe CSsize)
send s b l f = c_send s b l f >>= \case
  -1 -> blocking >>= \case
    True -> pure Nothing
    False -> throwErrno "send"
  n -> pure (Just n)
