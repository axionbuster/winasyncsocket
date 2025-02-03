module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Debug.Trace
import Network.SocketA
import System.Environment
import System.IO

withColor :: String -> String -> String
withColor color text = color ++ "\ESC[0m" ++ text ++ "\ESC[0m"

theaddr, theport :: String
theaddr = "127.0.0.1"
theport = "40001"

server :: IO ()
server = do
  let ai1 =
        addrinfo0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol

  traceIO $ "starting server on " ++ theaddr ++ ":" ++ theport
  addr <- getaddrinfo theaddr theport $ Just ai1
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
            handle (\(e :: IOException) -> hPrint stderr e) do
              fix \loop -> do
                traceIO "waiting for message"
                msg <- recv c 1024
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
  addr <- getaddrinfo theaddr theport $ Just ai1
  bracket mksocket close \sock -> do
    traceIO "connecting to server"
    withaddrpair addr do connect sock
    traceIO "connected. type messages to send (Ctrl+C to exit)"
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
        traceIO "Usage: winasyncsocket-exe --server|--client"
        error "Invalid arguments"
    \(e :: SomeException) ->
      putStrLn $ "error: " ++ displayException e
