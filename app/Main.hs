-- | demonstration server/client
module Main (main) where

import Control.Exception
import Control.Monad
import Data.ByteString.Char8 qualified as C
import Debug.Trace
import System.Environment
import System.IO
import TCPIP

client :: IO ()
client = do
  hPutStrLn stderr "connecting to localhost:50123"
  let ai1 =
        addrinfow0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP
          }
      ain =
        sockaddrin0
          { sin_family = fromIntegral . unaddrfamily $ AF_INET,
            sin_port = 0,
            sin_addr = INADDR_ANY
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    traceIO "binding accepting socket"
    bind sock ain
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
      ["--server"] -> error "server not implemented"
      ["--client"] -> client
      _ -> do
        hPutStrLn stderr "Usage: winsocktest2 --server|--client"
        error "Invalid arguments"
    do
      \(e :: SomeException) ->
        putStrLn $ "error: " ++ displayException e
