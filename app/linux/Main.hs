module Main (main) where

import Control.Exception
import Control.Monad
import Debug.Trace
import Network.SocketA.LinuxEPoll.TCPIP
import System.IO

withColor :: String -> String -> String
withColor color text = color ++ text ++ "\ESC[0m"

server :: IO ()
server = do
  hPutStrLn stderr "starting server on localhost:50123"
  let ai1 =
        addrinfo0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket ai1.ai_family ai1.ai_socktype ai1.ai_protocol
  addr <- getaddrinfo "127.0.0.1" "50123" $ Just ai1
  bracket mksocket close \sock -> do
    sockaddrin addr >>= bind sock
    listen sock 0
    traceIO "listening for connections..."
    forever do
      bracket
        do accept sock
        do \c -> traceIO (withColor "\ESC[31m" "bye!") *> close c
        do
          \_ -> do
            traceIO (withColor "\ESC[32m" "client connected")

-- handle (\(e :: IOException) -> hPrint stderr e) do
--   -- Echo loop
--   fix \loop -> do
--     traceIO "waiting for message"
--     msg <- recv c 1024
--     traceIO "message received"
--     if C.length msg > 0
--       then do
--         traceIO "sending message back"
--         sendall c msg
--         traceIO "message sent"
--         loop
--       else pure ()

main :: IO ()
main = server
