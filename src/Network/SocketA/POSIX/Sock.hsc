{- |
Module      : Network.SocketA.POSIX.Sock
Description : Socket operations and types for POSIX systems
Copyright   : (c) axionbuster, 2025
License     : BSD-3-Clause

This module provides low-level bindings to POSIX socket APIs. It includes
socket operations, constants and data structures needed for network programming.

All sockets are created in non-blocking mode as this is required for
this library.
-}
module Network.SocketA.POSIX.Sock
  ( -- * Types
    Socket(..)
  , AddrFamily(..)
  , SocketType(..)
  , Protocol(..)
  , AddrFlag(..)
  , GetAddrInfoError(..)
  , SockAddr
  , AddrLen(..)
  , AddrInfo
  , AddrInfo_(..)
  , ShutdownHow(..)
  , AIO(..)
    -- * Patterns
#if defined(linux_HOST_OS)
  , pattern SOCK_NONBLOCK
  , pattern SOCK_CLOEXEC
#endif
  , pattern SOCK_STREAM
  , pattern AF_INET
  , pattern AF_INET6
  , pattern IPPROTO_TCP
  , pattern SHUT_RD
  , pattern SHUT_WR
  , pattern SHUT_RDWR
  , pattern INVALID_SOCKET
  , pattern SOCKET_ERROR
  , pattern Success
  , pattern NotSupported
  , pattern ConnectionReset
  , pattern InProgress
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
  , connecterror
  , getaddrinfo
  , recv
  , send
  , setnonblock
    -- * Constants and helpers
  , recvflags0
  , sendflags0
  , addrfamily0
  , sockettype0
  , protocol0
  , addrflag0
  , addrinfo0
  , addrpair
  , addrpair_
  , withaddrlen
  -- * 'AIO' helpers
  , underaio
  , aalloca
  , apoke
  , apeek
  , amask_
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Coerce
import Data.Data
import Data.Functor
import Data.Void
import Foreign hiding (void)
import Foreign.C.ConstPtr
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe
import System.Posix.Types

#include <fcntl.h>
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

-- | invalid socket handle
pattern INVALID_SOCKET :: Socket
pattern INVALID_SOCKET = Socket (-1)

-- result; negative 1 is error
newtype RN1 = RN1 { unrn1 :: CInt }
  deriving newtype (Show, Eq, Num)

-- | no error
pattern Success :: Errno
pattern Success = Errno 0

-- | check error by calling
--
--  - 'getErrno' on POSIX
--  - 'WSAGetLastError' on Windows
--
-- note: all exported functions already call WSAGetLastError to report the
-- actual error code, so user code should not expect to get this error
pattern SOCKET_ERROR :: Errno
pattern SOCKET_ERROR = Errno (-1)

-- | operation is not supported
pattern NotSupported :: Errno
pattern NotSupported = Errno #{const ENOTSUP}

-- | connection has been reset
pattern ConnectionReset :: Errno
pattern ConnectionReset = Errno #{const ECONNRESET}

-- | not an error; operation will complete in the background (POSIX)
pattern InProgress :: Errno
pattern InProgress = Errno #{const EINPROGRESS}

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
-- The socket is always created in non-blocking mode.
socket :: AddrFamily -> SocketType -> Protocol -> IO Socket
socket d s p = mask_ do
  t <- c_socket d s p >>= okn1 "socket" (pure . Socket . unrn1)
  setnonblock t $> t

#if defined(linux_HOST_OS)
pattern SOCK_NONBLOCK, SOCK_CLOEXEC :: SocketType
-- | non-blocking socket flag
pattern SOCK_NONBLOCK = SocketType #{const SOCK_NONBLOCK}
-- | close-on-exec socket flag
pattern SOCK_CLOEXEC = SocketType #{const SOCK_CLOEXEC}
#endif

pattern SOCK_STREAM :: SocketType
-- | stream socket
pattern SOCK_STREAM = SocketType #{const SOCK_STREAM}

pattern AF_INET, AF_INET6 :: AddrFamily
-- | IPv4
pattern AF_INET = AddrFamily #{const AF_INET}
-- | IPv6
pattern AF_INET6 = AddrFamily #{const AF_INET}

pattern IPPROTO_TCP :: Protocol
-- | TCP
pattern IPPROTO_TCP = Protocol #{const IPPROTO_TCP}

-- | error returned from a call to @getaddrinfo@
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

-- | a pointer to an address along with its length
--
-- __SAFETY__: lifetime is tied to the parent 'AddrInfo_' or 'AddrInfo',
-- but it's not tracked. refer to 'withaddrlen' for safe usage
data AddrLen = AddrLen { aaddr :: Ptr SockAddr, alen :: Socklen }
  deriving stock (Show)

-- | opaque pointer to a @struct sockaddr@ (one of many variants thereof)
data SockAddr
  deriving stock (Show, Read)

-- | type of 'ai_addrlen'
type Socklen = #{type socklen_t}

-- | address information
--
-- the Haskell representation orders the fields in WinSock order; the
-- 'Storable' instance will take care of reordering them for POSIX
data AddrInfo_ = AddrInfo_
  { ai_flags :: AddrFlag
  , ai_family :: AddrFamily
  , ai_socktype :: SocketType
  , ai_protocol :: Protocol
  , ai_addrlen :: Socklen
  , ai_canonname :: CString
  , ai_addr :: Ptr SockAddr
  , ai_next :: Ptr AddrInfo_
  } deriving (Eq, Show)

instance Storable AddrInfo_ where
  sizeOf _ = #{size struct addrinfo}
  alignment _ = #{alignment struct addrinfo}
  -- the 'ai_canonname' field actually follows the 'ai_addr' field
  -- in POSIX; this is different from the way it's done in Windows Sockets
  peek p = do
    flags <- #{peek struct addrinfo, ai_flags} p
    family <- #{peek struct addrinfo, ai_family} p
    socktype <- #{peek struct addrinfo, ai_socktype} p
    protocol <- #{peek struct addrinfo, ai_protocol} p
    addrlen <- #{peek struct addrinfo, ai_addrlen} p
    addr <- #{peek struct addrinfo, ai_addr} p
    canonname <- #{peek struct addrinfo, ai_canonname} p
    next <- #{peek struct addrinfo, ai_next} p
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
    #{poke struct addrinfo, ai_flags} p (unaddrflag ai_flags)
    #{poke struct addrinfo, ai_family} p (undomain ai_family)
    #{poke struct addrinfo, ai_socktype} p (unsockettype ai_socktype)
    #{poke struct addrinfo, ai_protocol} p (unprotocol ai_protocol)
    #{poke struct addrinfo, ai_addrlen} p ai_addrlen
    #{poke struct addrinfo, ai_addr} p ai_addr
    #{poke struct addrinfo, ai_canonname} p ai_canonname
    #{poke struct addrinfo, ai_next} p ai_next

-- | the zero value for 'AddrInfo_'
addrinfo0 :: AddrInfo_
addrinfo0 = AddrInfo_
  { ai_flags = addrflag0
  , ai_family = addrfamily0
  , ai_socktype = sockettype0
  , ai_protocol = protocol0
  , ai_addrlen = 0
  , ai_canonname = nullPtr
  , ai_addr = nullPtr
  , ai_next = nullPtr
  }

-- | extract the 'SockAddr' from an 'AddrInfo_' (assuming it's valid)
--
-- __SAFETY__: highly unsafe; lifetime is tied to the parent 'AddrInfo_',
-- but it's not tracked
addrpair_ :: AddrInfo_ -> AddrLen
addrpair_ AddrInfo_ {..} = AddrLen ai_addr ai_addrlen
{-# INLINE addrpair_ #-}

-- | extract the 'SockAddr' from an 'AddrInfo' (assuming it's valid)
--
-- __SAFETY__: highly unsafe; lifetime is tied to the parent 'AddrInfo',
-- but it's not tracked. use 'withaddrlen' instead if possible
addrpair :: AddrInfo -> IO AddrLen
addrpair a = withForeignPtr a (peek . castPtr) <&> addrpair_
{-# INLINE addrpair #-}

-- | do something with an the address\/length pair from an 'AddrInfo'
withaddrlen :: AddrInfo -> (AddrLen -> IO a) -> IO a
withaddrlen a = (addrpair a >>=)

foreign import capi unsafe "netdb.h getaddrinfo"
  c_getaddrinfo :: ConstPtr CChar -> ConstPtr CChar -> ConstPtr AddrInfo_ ->
                   Ptr (Ptr AddrInfo_) -> IO GetAddrInfoError

foreign import capi unsafe "netdb.h &freeaddrinfo"
  c_freeaddrinfo :: FunPtr (Ptr AddrInfo_ -> IO ())

foreign import capi unsafe "netdb.h gai_strerror"
  c_gai_strerror :: GetAddrInfoError -> ConstPtr CChar

-- | a managed 'AddrInfo_' list (head only); never NULL
--
-- to get the address for use with 'bind', use 'sockaddrin'
type AddrInfo = ForeignPtr AddrInfo_

-- | Lookup network addresses. This is a high-level wrapper around
-- the @getaddrinfo(3)@ system call.
--
-- if it fails, it throws a 'GetAddrInfoError' or 'IOError'
--
-- you can use 'addrinfo0' as a default 'AddrInfo_' value, and
-- use 'sockaddrin' to extract the 'SockAddr' from the returned 'AddrInfo'
getaddrinfo :: String -> String -> Maybe AddrInfo_ -> IO AddrInfo
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
bind :: Socket -> AddrLen -> IO ()
bind s a = mask_ do
  c_bind s (ConstPtr a.aaddr) a.alen >>= okn1_ "bind"

-- | Try to bind to addresses in sequence until one succeeds.
--
-- Takes a linked list of addresses and attempts to bind until success.
-- Throws an error if all attempts fail.
bindfirst :: Socket -> AddrInfo -> IO ()
bindfirst s l =
  withForeignPtr l \h ->
  peek h >>= fix \r h' ->
  mask \restore ->
    c_bind s (ConstPtr h'.ai_addr) h'.ai_addrlen >>= \case
      0 -> pure ()
      _ -> case h'.ai_next of
        p | p == nullPtr -> throwErrno "bindfirst"
        p -> peek p >>= restore . r

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
listen s (fromIntegral -> i) = mask_ do c_listen s i >>= okn1_ "listen"

foreign import capi unsafe "sys/socket.h getsockopt"
  c_getsockopt :: Socket -> CInt -> CInt -> Ptr CInt -> Ptr Socklen -> IO RN1

-- internal: get an integer option from a socket
getsockopt_int :: Socket -> CInt -> CInt -> IO CInt
getsockopt_int s l o = alloca \v -> alloca \l' -> mask_ do
  poke l' #{size int}
  c_getsockopt s l o v l' >>= okn1_ "getsockopt_int"
  peek v

-- | check if a 'connect' call has succeeded
--
-- * 'Just': the connection has failed with the given error
-- * 'Nothing': the connection has succeeded
connecterror :: Socket -> IO (Maybe Errno)
connecterror s =
  getsockopt_int s #{const SOL_SOCKET} #{const SO_ERROR} <&> \case
    0 -> Nothing
    e -> Just (Errno e)

foreign import capi unsafe "fcntl.h fcntl"
  c_fcntlv :: Socket -> CInt -> IO RN1

foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl1i :: Socket -> CInt -> CInt -> IO RN1

-- | using @fcntl@, mark a socket as non-blocking
setnonblock :: Socket -> IO ()
setnonblock s = mask_ do
  f <- c_fcntlv s #{const F_GETFL} >>= okn1 "setnonblock (get)" (pure . unrn1)
  c_fcntl1i s #{const F_SETFL} (f .|. #{const O_NONBLOCK}) >>=
    okn1_ "setnonblock (set)"

-- now we begin to have nonblocking async calls...

-- | IO, but with an overloaded 'Alternative' instance so we can use
-- 'empty' to signal blocking
--
-- can be considered package-internal
newtype AIO a = AIO { unaio :: IO a }
  deriving newtype (Functor, Applicative, Monad)

-- | poor man\'s @withRunInIO@
underaio :: ((forall x. AIO x -> IO x) -> IO y) -> AIO y
underaio f = AIO $ f unaio
{-# INLINE underaio #-}

-- | 'empty' exception for 'AIO'
data AX = AX
  deriving stock (Typeable)
  deriving anyclass (Exception)

-- | \"unhandled blocking\" exception message
instance Show AX where
  show _ = "internal error: unhandled blocking"

-- | 'IO'\'s 'Alternative' instance has a critical flaw of throwing a regular
-- 'IOError' for 'empty' and catch any 'IOError' regardless of whether it\'s
-- from 'empty' or other sources; we fix that here
instance Alternative AIO where
  empty = AIO $ throwIO AX
  !a <|> b = AIO $ catch (unaio a) \AX -> unaio b
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

-- | regular 'IO'
instance MonadFail AIO where
  fail = AIO . fail
  {-# INLINE fail #-}

-- | lifted 'alloca'
aalloca :: (Storable a) => (Ptr a -> AIO b) -> AIO b
aalloca a = underaio \u -> alloca (u . a)
{-# INLINE aalloca #-}

-- | lifted 'poke'
apoke :: (Storable a) => Ptr a -> a -> AIO ()
apoke = (AIO .) . poke
{-# INLINE apoke #-}

-- | lifted 'peek'
apeek :: (Storable a) => Ptr a -> AIO a
apeek = AIO . peek
{-# INLINE apeek #-}

-- | lifted 'mask_'
amask_ :: AIO a -> AIO a
amask_ a = underaio \u -> mask_ (u a)
{-# INLINE amask_ #-}

asetnonblock :: Socket -> AIO ()
asetnonblock = coerce setnonblock
{-# INLINE asetnonblock #-}

athrowerrno :: String -> AIO a
athrowerrno = AIO . throwErrno
{-# INLINE athrowerrno #-}

ablocking :: AIO Bool
ablocking = AIO blocking
{-# INLINE ablocking #-}

ageterrno :: AIO Errno
ageterrno = AIO getErrno
{-# INLINE ageterrno #-}

-- ok unless negative 1; on negative 1, check for blocking. if blocking,
-- throw empty; otherwise, return
okn1asy :: String -> (RN1 -> AIO a) -> RN1 -> AIO a
okn1asy n _ (-1) = AIO blocking >>= \case
  True -> empty
  False -> athrowerrno n
okn1asy _ a e = a e

foreign import capi unsafe "sys/socket.h accept"
  -- 2nd, 3rd: restrict, nullable
  c_accept :: Socket -> Ptr SockAddr -> Ptr Socklen -> AIO RN1

-- | Accept a new connection on a listening socket.
--
-- Returns the newly created connected socket and the peer address.
--
-- Throws 'empty' if the operation would block.
accept :: Socket -> AIO Socket
accept s =
  amask_ do
    c_accept s nullPtr nullPtr >>= okn1asy "accept" \(unrn1 -> t) -> do
      let u = Socket t
      asetnonblock u
      pure u

foreign import capi unsafe "sys/socket.h connect"
  c_connect :: Socket -> ConstPtr SockAddr -> Socklen -> AIO RN1

-- | Connect a socket to a remote address.
--
-- For non-blocking sockets, may throw 'empty' before the connection completes.
-- The caller should wait for writability to detect connection completion.
connect :: Socket -> AddrLen -> AIO ()
connect s a =
  amask_ do
    c_connect s (ConstPtr a.aaddr) a.alen >>= \case
      -1 ->
        -- for 'connect' on TCP/IP, the blocking error is EINPROGRESS
        ageterrno >>= \case
          e | e == eINPROGRESS -> empty
          _ ->
            -- EAGAIN could be returned for UNIX domain sockets
            ablocking >>= \case
              True -> empty
              False -> athrowerrno "connect"
      _ -> pure ()

-- | flags for use in 'recv'
newtype RecvFlags = RecvFlags CInt
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

-- | zero recv flags
recvflags0 :: RecvFlags
recvflags0 = RecvFlags 0

foreign import capi unsafe "sys/socket.h recv"
  c_recv :: Socket -> Ptr Void -> CSize -> RecvFlags -> AIO CSsize

-- common, send and recv
sendrecv :: String -> (Socket -> Ptr Void -> CSize -> a -> AIO CSsize)
         -> Socket -> Ptr Void -> CSize -> a -> AIO CSsize
sendrecv a g s b l f = amask_ do
  g s b l f >>= \case
    -1 -> ablocking >>= \case
      True -> empty
      False -> athrowerrno a
    n -> pure n
{-# INLINE sendrecv #-}

-- | Receive data from a socket into a buffer.
--
-- Returns number of bytes read.
-- Throws 'empty' if it would block.
-- Returns 0 on end-of-file for stream sockets.
recv :: Socket -> Ptr Void -> CSize -> RecvFlags -> AIO CSsize
recv = sendrecv "recv" c_recv

-- | flags for use in 'send'
newtype SendFlags = SendFlags CInt
  deriving newtype (Eq, Storable, Bits, FiniteBits)
  deriving stock (Show)

-- | zero send flags
sendflags0 :: SendFlags
sendflags0 = SendFlags 0

foreign import capi unsafe "sys/socket.h send"
  c_send :: Socket -> Ptr Void -> CSize -> SendFlags -> AIO CSsize

-- | Send data from a buffer through a socket.
--
-- Returns number of bytes sent.
-- Throws 'empty' if operation would block.
send :: Socket -> Ptr Void -> CSize -> SendFlags -> AIO CSsize
send = sendrecv "send" c_send
