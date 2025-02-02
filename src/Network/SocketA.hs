module Network.SocketA where

import Data.ByteString (ByteString)

class Networking p where
  type AddrFamily p
  type AddrInfo p
  type AddrInfo_ p
  type AddressLen p
  type Error p
  type Protocol p
  type ShutdownHow p
  type Socket p
  type SocketType p

  socket :: AddrFamily p -> SocketType p -> Protocol p -> IO (Socket p)
  bind :: Socket p -> AddressLen p -> IO ()
  listen :: Socket p -> IO ()
  accept :: Socket p -> IO (Socket p)
  connect :: Socket p -> AddressLen p -> IO ()
  recv :: Socket p -> Int -> IO ByteString
  recvall :: Socket p -> Int -> IO ByteString
  send :: Socket p -> ByteString -> IO Int
  sendall :: Socket p -> ByteString -> IO ()
  shutdown :: Socket p -> IO ()
  close :: Socket p -> IO ()
  getaddrinfo :: String -> String -> Maybe (AddrInfo_ p) -> IO (AddrInfo p)
