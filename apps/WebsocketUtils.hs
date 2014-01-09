module WebsocketUtils
  ( wsServerFromChan
  , makeWSServerChan
  , makeWSServer
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           Network.WebSockets


-- | A websocket server that serves ByteStrings from a Chan.
wsServerFromChan :: Chan BSL.ByteString -> PendingConnection -> IO ()
wsServerFromChan chan = \req -> do
  conn <- acceptRequest req
  c <- dupChan chan
  forever (readChan c >>= sendTextData conn)


-- | Creates and starts (forking) a ByteString-serving websocket server.
-- Returns a Chan from which the server will read.
makeWSServerChan :: String -> Int -> IO (Chan BSL.ByteString)
makeWSServerChan host port = do
  chan <- newChan
  _ <- forkIO $ runServer host port (wsServerFromChan chan)
  return chan


-- | Creates and starts (forking) a ByteString-serving websocket server.
-- Returns a function that serves the contents.
makeWSServer :: String -> Int -> IO (BSL.ByteString -> IO ())
makeWSServer host port = do
  chan <- makeWSServerChan host port
  return (writeChan chan)
