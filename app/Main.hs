-- | demonstration server/client
module Main (main) where

import Control.Exception
import Control.Monad
import Data.ByteString.Char8 qualified as C
import System.Environment
import System.IO
import TCPIP

server :: IO ()
server = do
  hPutStrLn stderr "Starting server on localhost:50123"
  addr <- getaddrinfo "127.0.0.1" "50123"
  bracket socket close \sock -> do
    bind sock addr
    listen sock
    forever $ bracket (accept sock) close \c -> do
      hPutStrLn stderr "Client connected"
      forever do
        msg <- recv c 1024
        when (C.null msg) $ error "Client disconnected"
        void $ send c msg
        hPutStrLn stderr $ "Echoed: " ++ show msg

client :: IO ()
client = do
  hPutStrLn stderr "Connecting to localhost:50123"
  addr <- getaddrinfo "127.0.0.1" "50123"
  bracket socket close \sock -> do
    bind sock =<< getaddrinfo "0.0.0.0" "0"
    connect sock addr
    hPutStrLn stderr "Connected. Type messages to send (Ctrl+C to exit)"
    forever do
      line <- C.getLine
      void $ send sock line
      resp <- recv sock 1024
      C.putStrLn resp

main :: IO ()
main = do
  startup
  args <- getArgs
  catch
    case args of
      ["--server"] -> server
      ["--client"] -> client
      _ -> do
        hPutStrLn stderr "Usage: winsocktest2 --server|--client"
        error "Invalid arguments"
    do
      \(e :: SomeException) ->
        putStrLn $ "error: " ++ displayException e
