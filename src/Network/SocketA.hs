-- different platforms have different opaqueness levels
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

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
    AddrFamily (..),
    AddrInfo (..),
    AddrInfo_ (..),
#if defined(mingw32_HOST_OS)
    ADDRINFOW (..),
#endif
    AddressLen (..),
    SocketError (..),
    Protocol (..),
    ShutdownHow (..),
    Socket,
    SocketType (..),

    -- * Constants and Patterns
#if defined(linux_HOST_OS)
    pattern SOCK_NONBLOCK,
    pattern SOCK_CLOEXEC,
#endif
    pattern SOCK_STREAM,
    pattern AF_INET,
    pattern AF_INET6,
    pattern IPPROTO_TCP,
    pattern SHUT_RD,
    pattern SHUT_WR,
    pattern SHUT_RDWR,
    pattern SD_RECEIVE,
    pattern SD_SEND,
    pattern SD_BOTH,

    -- * Operations
    startup,
    addrinfo0,
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

-- Windows and POSIX use different terms for the same thing (shutdown how)

pattern SHUT_RD, SHUT_WR, SHUT_RDWR :: ShutdownHow

-- | Shutdown the receive channel; alias for 'SD_RECEIVE'
pattern SHUT_RD = SD_RECEIVE

-- | Shutdown the send channel; alias for 'SD_SEND'
pattern SHUT_WR = SD_SEND

-- | Shutdown both channels; alias for 'SD_BOTH'
pattern SHUT_RDWR = SD_BOTH

#else
-- | Socket exception type
type SocketError = IOError

pattern SD_RECEIVE, SD_SEND, SD_BOTH :: ShutdownHow

-- | Shutdown the receive channel; alias for 'SHUT_RD'
pattern SD_RECEIVE = SHUT_RD

-- | Shutdown the send channel; alias for 'SHUT_WR'
pattern SD_SEND = SHUT_WR

-- | Shutdown both channels; alias for 'SHUT_RDWR'
pattern SD_BOTH = SHUT_RDWR

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
--
-- - Windows: 'SocketError', which is its own type
-- - POSIX: 'IOError', which is a type alias for 'IOException'
catchsocket :: IO a -> (SocketError -> IO a) -> IO a
catchsocket a b = catch a \(e :: SocketError) -> b e
