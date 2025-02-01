module Network.SocketA.LinuxEPoll.TCPIP
  ( -- * Types
    S.Socket,
    S.AddrFamily (..),
    S.SocketType (..),
    S.Protocol (..),
    S.AddrInfo_ (..),
    S.AddrInfo,

    -- * Constants and Patterns
    pattern S.SOCK_NONBLOCK,
    pattern S.SOCK_CLOEXEC,
    pattern S.SOCK_STREAM,
    pattern S.AF_INET,
    pattern S.AF_INET6,
    pattern S.IPPROTO_TCP,
    pattern S.SHUT_RD,
    pattern S.SHUT_WR,
    pattern S.SHUT_RDWR,
    S.addrinfo0,

    -- * Functions
    S.socket,
    S.getaddrinfo,
    close,
    S.bind,
    S.bindfirst,
    S.bindfirst2,
    S.listen,
    accept,
    S.sockaddrin,
  )
where

import Control.Concurrent
import Control.Exception
import GHC.Event
import Network.SocketA.LinuxEPoll.Sock qualified as S
import System.Posix.Types
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

-- if successful, unwrap; otherwise, rethrow the exception
unwrap :: (Exception a) => Either a b -> IO b
unwrap = either throwIO pure

-- | accept a connection from a bound, listening socket
accept :: S.Socket -> IO S.Socket
accept s = do
  -- synopsis.
  --  1. register an interest in the socket's file descriptor for reading
  --     (-> accept)
  --  2. wait [w] for the event to occur (-> takeMVar)
  --     (done asynchronously by GHC's event manager)
  --  3. process the event if successful, or rethrow the exception
  --     (-> unwrap)
  --     (then deregister the interest)
  w <- newEmptyMVar
  let f _ _ =
        catch
          do S.unaio (S.accept s) >>= putMVar w . Right
          \(x :: SomeException) -> putMVar w (Left x)
  bracket
    do regfd f evtRead OneShot (sk2fd s)
    do unregfd -- unnecessary since OneShot, but good practice
    do \_ -> takeMVar w >>= unwrap
