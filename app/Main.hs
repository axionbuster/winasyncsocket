-- | demonstration server/client
module Main (main) where

import Control.Exception
import Control.Monad
import Data.ByteString.Char8 qualified as C
import Debug.Trace
import System.Environment
import System.IO
import TCPIP

server :: IO ()
server = do
  hPutStrLn stderr "Starting server on localhost:50123"
  addr <- getaddrinfo "::" "50123"
  bracket socket close \sock -> do
    traceIO "binding socket to addr"
    bind sock addr
    traceIO "listening on socket"
    listen sock
    traceIO "now accepting clients..."
    forever $ bracket (accept sock) close \c -> do
      hPutStrLn stderr "client connected"
      forever do
        msg <- recv c 1024
        when (C.null msg) $ error "client disconnected"
        sendall c msg
        hPutStrLn stderr $ "echoed: " ++ show msg

client :: IO ()
client = do
  hPutStrLn stderr "connecting to localhost:50123"
  addr <- getaddrinfo "::1" "50123"
  bracket socket close \sock -> do
    traceIO "binding socket to addr"
    bind sock addr
    traceIO "connecting to server"
    connect sock addr
    hPutStrLn stderr "connected. type messages to send (Ctrl+C to exit)"
    forever do
      line <- C.getLine
      sendall sock line
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
