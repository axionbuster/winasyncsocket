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
import Foreign.Ptr
import GHC.Event.Windows
import Sock (Socket (..), SocketError (..), sksk)
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

-- IOResult has a IOFailed :: Just Int -> IOResult x constructor.
-- we take the Just Int part and throw it
throw1 :: Maybe Int -> IO a
throw1 (Just e) = throwIO $ SocketError $ fromIntegral e
throw1 Nothing = throwunknown

-- | accept a new connection
accept :: Socket -> IO Socket
accept (sksk -> l) = do
  a <- socket
  v <- readIORef globalvtable
  let start :: LPOVERLAPPED -> IO (CbResult Int)
      start o = catch
        do S.acceptex v l (sksk a) o $> CbPending
        do pure . CbError . fromIntegral . getskerr
      complete :: DWORD -> DWORD -> IO (IOResult Socket)
      complete 0 _ = S.finishaccept l (sksk a) $> IOSuccess a
      complete e _ = pure $ IOFailed $ Just $ fromIntegral e
  -- withOverlapped provides the OVERLAPPED structure to the OS;
  -- keeps a callback table; queues the event to the event loop as well as
  -- the OS and suspends the Haskell thread until it returns
  withOverlapped
    do "accept"
    do handleu l
    do 0 -- Offset/OffsetHigh
    do start
    do complete
    >>= \case
      IOSuccess _ -> pure a -- payload contains a
      IOFailed e -> throw1 e
