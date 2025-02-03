-- different platforms have different opaqueness levels
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

-- |
-- Module      : Network.SocketA.Types
-- Description : Types and constants for cross-platform socket programming
-- Copyright   : (c) axionbuster, 2025
-- License     : BSD-3-Clause
--
-- This module provides common types and constants used in socket programming
-- across Windows and POSIX platforms. It handles the platform-specific 
-- differences in types and naming conventions.
module Network.SocketA.Types
  ( -- * Types
    AddrFamily (..),
    AddrInfo,
    AddrInfo_ (..),
#if defined(mingw32_HOST_OS)
    ADDRINFOW (..),
    SOCKET,
#endif
    AddrLen,
    SocketError,
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
    pattern INVALID_SOCKET,
  )
where

#if defined(mingw32_HOST_OS)
import Network.SocketA.Windows.TCPIP

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
import Network.SocketA.POSIX.TCPIP

-- | Socket exception type; in POSIX systems, this is an alias for 'IOError'
-- but in Windows, it is its own type
--
-- Currently, you are not allowed to inspect the error code in either
-- platform, so you should not rely on the error code for error handling.
-- Socket errors need to be treated as completely opaque, for now.
type SocketError = IOError

pattern SD_RECEIVE, SD_SEND, SD_BOTH :: ShutdownHow

-- | Shutdown the receive channel; alias for 'SHUT_RD'
pattern SD_RECEIVE = SHUT_RD

-- | Shutdown the send channel; alias for 'SHUT_WR'
pattern SD_SEND = SHUT_WR

-- | Shutdown both channels; alias for 'SHUT_RDWR'
pattern SD_BOTH = SHUT_RDWR
#endif
