module Network.SocketA (Networking (..)) where

#if defined(mingw32_HOST_OS)
import Network.SocketA.Windows.TCPIP qualified as S
#else
import Network.SocketA.POSIX.TCPIP qualified as S
#endif

import Data.ByteString (ByteString)

data Platform = Platform

class Networking (p :: Platform) where
  type AddrFamily p
  type AddrInfo p
  type AddrInfo_ p
  type AddressLen p
  type Error p
  type Protocol p
  type ShutdownHow p
  type Socket p
  type SocketType p

  startup :: IO ()
  socket :: AddrFamily p -> SocketType p -> Protocol p -> IO (Socket p)
  bind :: Socket p -> AddressLen p -> IO ()
  listen :: Socket p -> IO ()
  accept :: Socket p -> IO (Socket p)
  connect :: Socket p -> AddressLen p -> IO ()
  recv :: Socket p -> Int -> IO ByteString
  recvall :: Socket p -> Int -> IO ByteString
  send :: Socket p -> ByteString -> IO Int
  sendall :: Socket p -> ByteString -> IO ()
  shutdown :: Socket p -> ShutdownHow p -> IO ()
  close :: Socket p -> IO ()
  getaddrinfo :: String -> String -> Maybe (AddrInfo_ p) -> IO (AddrInfo p)
  withaddrpair :: AddrInfo p -> (AddressLen p -> IO a) -> IO a

#if defined(mingw32_HOST_OS)
instance Networking 'Platform where
  type AddrFamily 'Platform = S.AddrFamily
  type AddrInfo 'Platform = S.AddrInfo
  type AddrInfo_ 'Platform = S.ADDRINFOW
  type AddressLen 'Platform = S.AddressLen
  type Error 'Platform = S.SocketError
  type Protocol 'Platform = S.Protocol
  type ShutdownHow 'Platform = S.ShutdownHow
  type Socket 'Platform = S.Socket
  type SocketType 'Platform = S.SocketType

  startup = S.startup
  socket = S.socket
  bind = S.bind
  listen = S.listen
  accept = S.accept
  connect = S.connect
  recv = S.recv
  recvall = S.recv
  send = S.send
  sendall = S.sendall
  shutdown = S.shutdown
  close = S.close
  getaddrinfo = S.getaddrinfo
  withaddrpair = S.withaddrpair
#else
#endif
