module Main (main) where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Function
import Debug.Trace
import Network.SocketA.POSIX.TCPIP
import System.Environment
import System.IO

withColor :: String -> String -> String
withColor color text = color ++ text ++ "\ESC[0m"

server :: IO ()
server = do
  hPutStrLn stderr "starting server on localhost:50123 (POSIX)"
  let ai1 =
        addrinfo0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    withaddrpair addr do bind sock
    listen sock
    traceIO "listening for connections..."
    forever do
      bracket
        do accept sock
        do \c -> traceIO (withColor "\ESC[31m" "bye!") *> close c
        do
          \c -> do
            traceIO (withColor "\ESC[32m" "client connected")
            handle (\(e :: IOException) -> traceShowM e) do
              -- Echo loop
              fix \loop -> do
                traceIO "waiting for message"
                msg <- recv c 10 -- make small so i can test that it works
                traceIO "message received"
                when (B.length msg > 0) do
                  traceIO "sending message back"
                  sendall c msg
                  traceIO "message sent"
                  loop

client :: IO ()
client = do
  let ai1 =
        addrinfo0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    traceIO "connecting to server (POSIX)"
    withaddrpair addr do connect sock
    hPutStrLn stderr "connected. type messages to send (Ctrl+C to exit)"
    forever do
      line <- C.getLine
      sendall sock line
      resp <- recvall sock $ B.length line
      C.putStrLn resp

main :: IO ()
main = do
  args <- getArgs
  catch
    case args of
      ["--server"] -> server
      ["--client"] -> client
      _ -> do
        hPutStrLn stderr "Usage: ... --server|--client"
        error "Invalid arguments"
    do
      \(e :: SomeException) ->
        putStrLn $ "error: " ++ displayException e
