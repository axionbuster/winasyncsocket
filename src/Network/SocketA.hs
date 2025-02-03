-- |
-- Module      : Network.SocketA
-- Description : Cross-platform asynchronous socket programming interface
-- Copyright   : (c) axionbuster, 2025
-- License     : BSD-3-Clause
--
-- This module provides a unified interface for asynchronous socket programming
-- across Windows and POSIX platforms through direct exports of
-- platform-specific implementations.
module Network.SocketA
  ( -- * Types
    AddrFamily,
    AddrInfo,
    AddrInfo_,
    AddressLen,
    SocketError,
    Protocol,
    ShutdownHow,
    Socket,
    SocketType,

    -- * Operations
    startup,
    socket,
    bind,
    listen,
    accept,
    connect,
    recv,
    recvall,
    send,
    sendall,
    shutdown,
    close,
    getaddrinfo,
    withaddrpair,
    withsocket,
    catchsocket,
  )
where

import Control.Exception
#if defined(mingw32_HOST_OS)
import Network.SocketA.Windows.TCPIP
#else
import Network.SocketA.POSIX.TCPIP hiding (getaddrinfo)
import Network.SocketA.POSIX.TCPIP qualified as S
#endif

-- | Safely work with a new socket, closing it automatically when done
withsocket :: AddrFamily -> SocketType -> Protocol -> (Socket -> IO a) -> IO a
withsocket af st pr = bracket (socket af st pr) close

#if defined(mingw32_HOST_OS)
-- | A node in the address information list
type AddrInfo_ = ADDRINFOW

#else
-- | Socket exception type
type SocketError = IOError

-- | (For POSIX systems, this is a no-op)
startup :: IO ()
startup = pure ()
{-# INLINE startup #-}

-- so, because it throws a different exception, we need to catch it and
-- rethrow it as a user error

-- | Get address information for a given host and port
getaddrinfo :: String -> String -> Maybe AddrInfo_ -> IO AddrInfo
getaddrinfo a b c =
  catch
    do S.getaddrinfo a b c
    \(e :: GetAddrInfoError) -> throwIO $ userError (displayException e)
#endif

-- | Catch socket exceptions
catchsocket :: IO a -> (SocketError -> IO a) -> IO a
catchsocket a b = catch a \(e :: SocketError) -> b e
