module Network.SocketA.LinuxEPoll.TCPIP
  ( -- * Types
    S.Socket,
    S.AddrFamily,
    S.SocketType,
    S.Protocol,
    S.AddrInfo_,
    S.AddrInfo,

    -- * Functions
    usingepoll,
    S.socket,
    S.getaddrinfo,
    close,
    S.bind,
    S.bindfirst,
    S.bindfirst2,
    S.listen,
    accept,
  )
where

import Control.Concurrent
import Control.Exception
import GHC.Event
import Network.SocketA.LinuxEPoll.Sock qualified as S
import System.Posix.Types

-- | check if epoll is available
usingepoll :: Bool
usingepoll = S.epollavailable
{-# INLINE usingepoll #-}

-- here Fd is a newtype over a CInt; distinct from FD type from GHC.IO.FD

-- assuming a threaded runtime, get the system event manager
evman :: IO EventManager
evman =
  getSystemEventManager >>= \case
    Just e -> pure e
    Nothing -> error "evman: no event manager; use threaded runtime"

-- register an event
-- - here IOCallback :: FdKey -> Event -> IO ()
-- - for FdKey, only the keyFd :: !Fd field is exposed; we can't make
-- our own FdKey
-- - Event is a bit set: 0 = nothing; read, write, (close)
regfd :: IOCallback -> Event -> Lifetime -> Fd -> IO FdKey
regfd c e l f = do
  m <- evman
  registerFd m c f e l

-- unregister an event
unregfd :: FdKey -> IO ()
unregfd f = evman >>= \m -> unregisterFd m f

-- close responsibly (race-safety)
closefd :: (Fd -> IO ()) -> Fd -> IO ()
closefd a b = evman >>= \m -> closeFd m a b

-- annoying because of newtyping
sk2fd :: S.Socket -> Fd
sk2fd = Fd . S.unsocket

fd2sk :: Fd -> S.Socket
fd2sk (Fd f) = S.Socket f

-- | close a socket
close :: S.Socket -> IO ()
-- we need to unregister any interests, so we use closefd
close = closefd (S.close . fd2sk) . sk2fd

-- TODO: shutdown

right :: (Exception a) => Either a b -> IO b
right = either throwIO pure

-- | accept a connection from a bound, listening socket
accept :: S.Socket -> S.SockAddr -> IO (S.Socket, S.SockAddr)
accept s a = do
  w <- newEmptyMVar
  let f _ _ = mask_ do
        catch
          do S.unaio (S.accept s a) >>= putMVar w . Right
          \(x :: SomeException) -> putMVar w (Left x)
  bracket
    do regfd f evtRead OneShot (sk2fd s)
    do unregfd
    do \_ -> takeMVar w >>= right
