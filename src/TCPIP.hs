-- |
-- Module: TCPIP
-- Description: Integrate TCP\/IP socket I\/O with GHC
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
module TCPIP where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.IORef
import Foreign hiding (void)
import GHC.Event.Windows
import GHC.Stack
import Sock (AddrInfo (..), Socket (..), SocketError (..), sksk)
import Sock qualified as S
import System.IO.Unsafe
import System.Win32.Types

-- global virtual function table for certain socket extension methods.
-- this requirement is imposed by Windows Sockets API
globalvtable :: IORef S.VTABLE
globalvtable = unsafePerformIO do
  -- these steps are performed once in the lifecycle of the program
  S.startup -- set up Winsock 2
  bracket -- load virtual function table
    do S.socket S.AF_INET6 S.SOCK_STREAM S.IPPROTO_TCP
    do S.closesocket
    do S.loadvt >=> newIORef
{-# NOINLINE globalvtable #-}

-- | create a new managed socket
socket :: IO Socket
socket = S.socket S.AF_INET6 S.SOCK_STREAM S.IPPROTO_TCP >>= S.managesocket

-- turn an unmanaged socket into a HANDLE
handleu :: S.SOCKET -> HANDLE
handleu (S.SOCKET s) = wordPtrToPtr s

-- extract a HANDLE from a managed socket
handlem :: Socket -> HANDLE
handlem = handleu . sksk

throwunknown :: IO a
throwunknown = fail "unknown IOCP error"

iofail :: (Integral b) => b -> IO (IOResult a)
iofail = pure . IOFailed . Just . fromIntegral

-- abbreviation of highly repetitive code
overlapped ::
  -- label
  String ->
  -- HANDLE (socket)
  HANDLE ->
  -- call extension method with given vtable and LPOVERLAPPED
  (S.VTABLE -> LPOVERLAPPED -> IO a) ->
  -- do this when it returns from RTS
  IO t ->
  -- if no error, polish up returned value from withOverlapped
  (t -> IO b) ->
  -- value returned from withOverlapped
  IO b
overlapped l a b c d =
  withOverlapped l a 0 b' c' >>= \case
    IOSuccess q -> d q
    IOFailed e -> throw1 e
  where
    b' o = do
      v <- readIORef globalvtable
      catch (b v o $> CbPending) do pure . CbError . fromIntegral . getskerr
    c' 0 _ = IOSuccess <$> c
    c' e _ = iofail e

-- IOResult has a IOFailed :: Just Int -> IOResult x constructor.
-- we take the Just Int part and throw it
throw1 :: (HasCallStack) => Maybe Int -> IO a
throw1 (Just e) = throwIO $ SocketError $ fromIntegral e
throw1 Nothing = throwunknown

-- | accept a new connection
accept :: Socket -> IO Socket
accept (sksk -> l) = do
  a <- socket
  overlapped
    do "accept"
    do handleu l
    do \v -> S.acceptex v l (sksk a)
    do S.finishaccept l (sksk a)
    do const (pure a)

-- get the ai_addrlen from an AddrInfo pointer
getailen :: AddrInfo -> IO Int
getailen (AddrInfo a) = withForeignPtr a do
  fmap (fromIntegral . S.ai_addrlen) . peek

-- | connect a socket to an address
connect :: Socket -> AddrInfo -> IO ()
connect (sksk -> l) a =
  overlapped
    do "connect"
    do handleu l
    do \v o -> getailen a >>= \b -> S.connectex' v l a b o
    do pure ()
    do const (pure ())
