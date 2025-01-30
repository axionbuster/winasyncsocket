module Network.SocketA.LinuxEPoll.Sock where

import Control.Exception
import Data.Bits
import Data.Data
import Foreign hiding (void)
import Foreign.C.ConstPtr
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

#include <netdb.h>
#include <sys/socket.h>

-- | socket (POSIX/Linux)
newtype Socket = Socket { unsocket :: CInt }
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

addrfamily0 :: AddrFamily
addrfamily0 = AddrFamily 0

sockettype0 :: SocketType
sockettype0 = SocketType 0

protocol0 :: Protocol
protocol0 = Protocol 0

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

foreign import capi unsafe "sys/socket.h socket"
  c_socket :: AddrFamily -> SocketType -> Protocol -> IO RN1

-- | create a non-blocking socket. the non-blocking flag will be
-- unconditionally set on because, otherwise, this library cannot
-- function.
socket :: AddrFamily -> SocketType -> Protocol -> IO Socket
socket d s p =
  c_socket d (s .|. SOCK_NONBLOCK) p
    >>= okn1 "socket" (pure . Socket . unrn1)

pattern SOCK_NONBLOCK, SOCK_CLOEXEC, SOCK_STREAM :: SocketType
-- | non-blocking socket flag
pattern SOCK_NONBLOCK = SocketType #{const SOCK_NONBLOCK}
pattern SOCK_CLOEXEC = SocketType #{const SOCK_CLOEXEC}
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

-- | calls @strerror_r@ to find the error message
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
data SockAddr deriving (Eq, Show)

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
  , ai_next :: Ptr SockAddr
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
                   Ptr AddrInfo_ -> GetAddrInfoError

foreign import capi unsafe "netdb.h &freeaddrinfo"
  c_freeaddrinfo :: FunPtr (Ptr AddrInfo_ -> IO ())

foreign import capi unsafe "netdb.h gai_strerror"
  c_gai_strerror :: GetAddrInfoError -> ConstPtr CChar
