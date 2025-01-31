module Network.SocketA.LinuxEPoll.TCPIP (
  -- * Types
  S.Socket,
  S.AddrFamily,
  S.SocketType,
  S.Protocol,
  S.AddrInfo_,
  S.AddrInfo,
  -- * Functions
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
import GHC.Event
import System.Posix.Types
import Network.SocketA.LinuxEPoll.Sock qualified as S

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

-- | accept a connection from a bound, listening socket
accept :: S.Socket -> S.SockAddr -> IO S.Socket
accept s a = do
  w <- newEmptyMVar
  let f h _ = g h
      g h = S.accept (fd2sk h.keyFd) a >>= putMVar w
  e <- regfd f evtRead OneShot (sk2fd s) -- evtRead = EPOLLIN
  takeMVar w <* unregfd e
