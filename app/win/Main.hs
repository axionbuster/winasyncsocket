-- | demonstration server/client
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.ByteString.Char8 qualified as C
import Debug.Trace
import Network.SocketA.Windows.TCPIP
import System.Environment
import System.IO

withColor :: String -> String -> String
withColor color text = color ++ text ++ "\ESC[0m"

server :: IO ()
server = do
  hPutStrLn stderr "starting server on localhost:50123"
  let ai1 =
        addrinfow0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    withaddrpair addr do bind sock
    listen sock
    hPutStrLn stderr "listening for connections..."
    forever do
      bracket
        do accept sock
        do \c -> traceIO (withColor "\ESC[31m" "bye!") *> close c
        do
          \c -> do
            hPutStrLn stderr (withColor "\ESC[32m" "client connected")
            handle (\(e :: IOException) -> hPrint stderr e) do
              -- Echo loop
              fix \loop -> do
                traceIO "waiting for message"
                msg <- recv c 1024
                traceIO "message received"
                if C.length msg > 0
                  then do
                    traceIO "sending message back"
                    sendall c msg
                    traceIO "message sent"
                    loop
                  else pure ()

client :: IO ()
client = do
  hPutStrLn stderr "connecting to localhost:50123"
  let ai1 =
        addrinfow0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    traceIO "connecting to server"
    withaddrpair addr do connect sock
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
