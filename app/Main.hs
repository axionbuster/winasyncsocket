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
  addr <- getaddrinfo "::" "50123" $ Just $ addrinfow0
    { ai_socktype = SOCK_STREAM,
      ai_protocol = IPPROTO_TCP,
      ai_family = AF_INET6,
      ai_flags = AI_PASSIVE
    }
  let mksocket = do
        s <- socket
        setsockopt_dword s SOL_SOCKET SO_REUSEADDR 1
        pure s
  bracket mksocket close \sock -> do
    traceIO "disabling IPV6_V6ONLY"
    setsockopt_dword sock IPPROTO_IPV6 IPV6_V6ONLY 0
    traceIO "binding socket to addr"
    bind sock addr
    traceIO "listening on socket"
    listen sock
    traceIO "now accepting client.."
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
  zero <- getaddrinfo "::" "0" $ Just $ addrinfow0
    { ai_socktype = SOCK_STREAM,
      ai_protocol = IPPROTO_TCP,
      ai_flags = AI_PASSIVE
    }
  let ai1 = addrinfow0
        { ai_socktype = SOCK_STREAM,
          ai_protocol = IPPROTO_TCP
        }
      mksocket = socket' ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "::1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    traceIO "binding socket to zero"
    bind sock zero
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
