-- |
-- Module      : Network.SocketA.Unlift
-- Description : Lifted and unlifted socket operations for monadic contexts
-- Copyright   : (c) axionbuster, 2025
-- License     : BSD-3-Clause
--
-- This module provides lifted versions of basic socket operations for use in
-- 'MonadIO' contexts, and unlifted versions of bracketed operations for use
-- in 'MonadUnliftIO' contexts.
module Network.SocketA.Unlift
  ( -- * Operations
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
    withaddrlen,
    withsocket,
    catchsocket,
    handlesocket,
  )
where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.ByteString (ByteString)
import Network.SocketA qualified as S
import Network.SocketA.Types

-- | Lifted version of 'Network.SocketA.startup'
startup :: (MonadIO m) => m ()
startup = liftIO S.startup
{-# INLINE startup #-}

-- | Lifted version of 'Network.SocketA.socket'
socket :: (MonadIO m) => AddrFamily -> SocketType -> Protocol -> m Socket
socket = ((liftIO .) .) . S.socket
{-# INLINE socket #-}

-- | Lifted version of 'Network.SocketA.bind'
bind :: (MonadIO m) => Socket -> AddrLen -> m ()
bind = (liftIO .) . S.bind
{-# INLINE bind #-}

-- | Lifted version of 'Network.SocketA.listen'
listen :: (MonadIO m) => Socket -> m ()
listen = liftIO . S.listen
{-# INLINE listen #-}

-- | Lifted version of 'Network.SocketA.accept'
accept :: (MonadIO m) => Socket -> m Socket
accept = liftIO . S.accept
{-# INLINE accept #-}

-- | Lifted version of 'Network.SocketA.connect'
connect :: (MonadIO m) => Socket -> AddrLen -> m ()
connect = (liftIO .) . S.connect
{-# INLINE connect #-}

-- | Lifted version of 'Network.SocketA.recv'
recv :: (MonadIO m) => Socket -> Int -> m ByteString
recv = (liftIO .) . S.recv
{-# INLINE recv #-}

-- | Lifted version of 'Network.SocketA.recvall'
recvall :: (MonadIO m) => Socket -> Int -> m ByteString
recvall = (liftIO .) . S.recvall
{-# INLINE recvall #-}

-- | Lifted version of 'Network.SocketA.send'
send :: (MonadIO m) => Socket -> ByteString -> m Int
send = (liftIO .) . S.send
{-# INLINE send #-}

-- | Lifted version of 'Network.SocketA.sendall'
sendall :: (MonadIO m) => Socket -> ByteString -> m ()
sendall = (liftIO .) . S.sendall
{-# INLINE sendall #-}

-- | Lifted version of 'Network.SocketA.shutdown'
shutdown :: (MonadIO m) => Socket -> ShutdownHow -> m ()
shutdown = (liftIO .) . S.shutdown
{-# INLINE shutdown #-}

-- | Lifted version of 'Network.SocketA.close'
close :: (MonadIO m) => Socket -> m ()
close = liftIO . S.close
{-# INLINE close #-}

-- | Lifted version of 'Network.SocketA.getaddrinfo'
getaddrinfo :: (MonadIO m) => String -> String -> Maybe AddrInfo_ -> m AddrInfo
getaddrinfo = ((liftIO .) .) . S.getaddrinfo
{-# INLINE getaddrinfo #-}

-- | Unlifted version of 'Network.SocketA.withaddrlen'
withaddrlen :: (MonadUnliftIO m) => AddrInfo -> (AddrLen -> m a) -> m a
withaddrlen ai f = withRunInIO \run -> S.withaddrlen ai (run . f)
{-# INLINE withaddrlen #-}

-- | Unlifted version of 'Network.SocketA.withsocket'
withsocket ::
  (MonadUnliftIO m) =>
  AddrFamily ->
  SocketType ->
  Protocol ->
  (Socket -> m a) ->
  m a
withsocket af st pr f = withRunInIO \run -> S.withsocket af st pr (run . f)
{-# INLINE withsocket #-}

-- | Unlifted version of 'Network.SocketA.catchsocket'
catchsocket :: (MonadUnliftIO m) => m a -> (SocketError -> m a) -> m a
catchsocket a f = withRunInIO \run -> S.catchsocket (run a) (run . f)
{-# INLINE catchsocket #-}

-- | Unlifted version of 'Network.SocketA.handlesocket'
handlesocket :: (MonadUnliftIO m) => (SocketError -> m a) -> m a -> m a
handlesocket f a = withRunInIO \run -> S.handlesocket (run . f) (run a)
{-# INLINE handlesocket #-}
