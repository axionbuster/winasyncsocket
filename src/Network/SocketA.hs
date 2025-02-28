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
  ( -- * Types, Constants, and Patterns
    module Network.SocketA.Types,

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
    withaddrlen,
    withsocket,
    catchsocket,
    handlesocket,
  )
where

import Control.Exception
import Network.SocketA.Types
#if defined(mingw32_HOST_OS)
import Network.SocketA.Windows.TCPIP
#else
import Network.SocketA.POSIX.TCPIP hiding (getaddrinfo)
import Network.SocketA.POSIX.TCPIP qualified as S
#endif
-- | Safely work with a new socket, closing it automatically when done
withsocket :: AddrFamily -> SocketType -> Protocol -> (Socket -> IO a) -> IO a
withsocket af st pr = bracket (socket af st pr) close

#if !defined(mingw32_HOST_OS)
-- | Initialize Windows Sockets on Windows; no-op on POSIX
--
-- It's OK to call this function multiple times
--
-- Socket operations will fail if this function is not called on Windows
--
-- It is recommended to call this function before any other socket operations
-- for cross-platform compatibility
startup :: IO ()
startup = pure ()
{-# INLINE startup #-}

-- so, because getaddrinfo throws its own kind of exception, we need to
-- catch it and rethrow it as IOError (POSIX only)
-- (on Windows, getaddrinfo throws a SocketError, so we don't need to do this)

-- | Get address information for a given node, service, and hints
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

-- | Handle socket exceptions
--
-- - Windows: 'SocketError', which is its own type
-- - POSIX: 'IOError', which is a type alias for 'IOException'
handlesocket :: (SocketError -> IO a) -> IO a -> IO a
handlesocket = flip catchsocket
