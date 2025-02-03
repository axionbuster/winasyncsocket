{-|
Module      : Network.SocketA
Description : Cross-platform asynchronous socket programming interface
Copyright   : (c) axionbuster, 2025
License     : BSD-3-Clause

This module provides a unified interface for asynchronous socket programming
across Windows and POSIX platforms. It abstracts over platform-specific
differences through the 'Networking' typeclass.

The interface supports common TCP\/IP operations like accepting connections,
sending and receiving data, with proper integration into each platform's
event notification system (IOCP on Windows, epoll\/kqueue on POSIX).

All operations are non-blocking by default and work with the platform's
native async I\/O mechanisms.
-}
module Network.SocketA (Platform (..), Networking (..)) where

#if defined(mingw32_HOST_OS)
import Network.SocketA.Windows.TCPIP qualified as S
#else
import Control.Exception
import Network.SocketA.POSIX.TCPIP qualified as S
#endif

import Data.ByteString (ByteString)

-- | Platform type tag for selecting the appropriate networking implementation
data Platform = P

-- | Typeclass providing a unified interface for asynchronous socket operations.
-- 
-- This abstracts over platform differences between Windows and POSIX systems,
-- allowing portable networking code to be written.
class Networking (p :: Platform) where
  -- | Socket address family (domain) type for the platform
  type AddrFamily p
  -- | Address info type for name resolution results
  type AddrInfo p
  -- | Raw address info structure type
  type AddrInfo_ p
  -- | Address and length pair type
  type AddressLen p
  -- | Platform-specific error type
  type Error p
  -- | Protocol identifier type
  type Protocol p
  -- | Shutdown operation specifier type
  type ShutdownHow p
  -- | Socket handle type
  type Socket p
  -- | Socket type specifier
  type SocketType p

  -- | Initialize the networking subsystem (Windows only, no-op on POSIX)
  --
  -- It's safe to call this function multiple times, as it will only initialize
  -- the subsystem once.
  startup :: IO ()
  -- | Create a new non-blocking socket
  socket :: AddrFamily p -> SocketType p -> Protocol p -> IO (Socket p)
  -- | Bind a socket to a local address
  bind :: Socket p -> AddressLen p -> IO ()
  -- | Mark a socket as passive and ready to accept connections
  listen :: Socket p -> IO ()
  -- | Accept a new connection on a listening socket
  accept :: Socket p -> IO (Socket p)
  -- | Connect to a remote address
  connect :: Socket p -> AddressLen p -> IO ()
  -- | Receive some data from a socket
  recv :: Socket p -> Int -> IO ByteString
  -- | Receive exactly the requested number of bytes unless EOF is reached
  recvall :: Socket p -> Int -> IO ByteString
  -- | Send some data to a socket, returning bytes sent
  send :: Socket p -> ByteString -> IO Int
  -- | Send entire ByteString to a socket
  sendall :: Socket p -> ByteString -> IO ()
  -- | Shut down part of a full-duplex connection
  shutdown :: Socket p -> ShutdownHow p -> IO ()
  -- | Close a socket and free associated resources
  close :: Socket p -> IO ()
  -- | Resolve network addresses
  getaddrinfo :: String -> String -> Maybe (AddrInfo_ p) -> IO (AddrInfo p)
  -- | Safely work with address/length pairs from AddrInfo
  withaddrpair :: AddrInfo p -> (AddressLen p -> IO a) -> IO a
  -- | Safely work with a new socket, closing it automatically when done
  --
  -- This function works as though it were implemented using 'bracket' as:
  --
  -- @
  -- withsocket af st pr f = bracket (socket af st pr) close f
  -- @
  withsocket :: AddrFamily p -> SocketType p -> Protocol p ->
                (Socket p -> IO a) -> IO a
  withsocket af st pr = bracket (socket @p af st pr) (close @p)

#if defined(mingw32_HOST_OS)
instance Networking 'P where
  type AddrFamily 'P = S.AddrFamily
  type AddrInfo 'P = S.AddrInfo
  type AddrInfo_ 'P = S.ADDRINFOW
  type AddressLen 'P = S.AddressLen
  type Error 'P = S.SocketError
  type Protocol 'P = S.Protocol
  type ShutdownHow 'P = S.ShutdownHow
  type Socket 'P = S.Socket
  type SocketType 'P = S.SocketType

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
instance Networking 'P where
  type AddrFamily 'P = S.AddrFamily
  type AddrInfo 'P = S.AddrInfo
  type AddrInfo_ 'P = S.AddrInfo_
  type AddressLen 'P = S.AddressLen
  type Error 'P = IOError
  type Protocol 'P = S.Protocol
  type ShutdownHow 'P = S.ShutdownHow
  type Socket 'P = S.Socket
  type SocketType 'P = S.SocketType

  startup = pure ()
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
  getaddrinfo a b c = catch (S.getaddrinfo a b c)
    \(e :: S.GetAddrInfoError) -> throwIO $ userError (displayException e)
  withaddrpair = S.withaddrpair
#endif
