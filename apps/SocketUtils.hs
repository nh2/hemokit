module SocketUtils
  ( tcpServerFromChan
  , makeTCPServerChan
  , makeTCPServer
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           Network.Simple.TCP


-- | A TCP server that serves ByteStrings from a Chan.
tcpServerFromChan :: Chan BSL.ByteString -> (Socket, SockAddr) -> IO ()
tcpServerFromChan chan = \(sock, _remoteAddr) -> do
  c <- dupChan chan
  forever (readChan c >>= send sock . BSL.toStrict)


-- | Creates and starts (forking) a ByteString-serving TCP server.
-- Returns a Chan from which the server will read.
makeTCPServerChan :: String -> Int -> IO (Chan BSL.ByteString)
makeTCPServerChan host port = do
  chan <- newChan
  _ <- forkIO $ withSocketsDo $ serve (Host host) (show port) (tcpServerFromChan chan)
  return chan


-- | Creates and starts (forking) a ByteString-serving TCP server.
-- Returns a function that serves the contents.
makeTCPServer :: String -> Int -> IO (BSL.ByteString -> IO ())
makeTCPServer host port = do
  chan <- makeTCPServerChan host port
  return (writeChan chan)
