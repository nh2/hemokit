module Hemokit.Conduit where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson (ToJSON (..), encode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit
import qualified Network.Simple.TCP as TCP
import qualified Network.WebSockets as WS

import           Hemokit

import           Hemokit.Internal.Utils (untilNothing)


rawSource :: (MonadIO m) => EmotivDevice -> Source m EmotivRawData
rawSource dev = void $ untilNothing (liftIO (readEmotivRaw dev)) yield

parsePackets :: (MonadIO m) => EmotivDevice -> Conduit EmotivRawData m (EmotivState, EmotivPacket)
parsePackets dev = awaitForever (\raw -> liftIO (updateEmotivState dev raw) >>= yield)


-- * Convenience

emotivStates :: (MonadIO m) => EmotivDevice -> Source m EmotivState
emotivStates dev = rawSource dev $= mapOutput fst (parsePackets dev)

emotivPackets :: (MonadIO m) => EmotivDevice -> Source m EmotivPacket
emotivPackets dev = rawSource dev $= mapOutput snd (parsePackets dev)


-- * JSON

-- TODO check if we really need this since it doesn't do any monadic thing
jsonConduit :: (Monad m, ToJSON i) => Conduit i m ByteString
jsonConduit = awaitForever (yield . encode)


-- * TCP sockets

tcpSink :: (MonadIO m) => String -> Int -> Sink ByteString m ()
tcpSink host port = do
  chan <- liftIO $ newChan

  -- Server loop: Send what comes in via the chan; Nothing shuts down
  let jsonTCPServerFromChan :: (TCP.Socket, TCP.SockAddr)  -> IO ()
      jsonTCPServerFromChan = \(sock, _remoteAddr) -> do
        void $ untilNothing (readChan chan) (TCP.send sock . BSL.toStrict)

  -- Fork off Websocket server
  _ <- liftIO $ forkIO $ TCP.withSocketsDo $ TCP.serve (TCP.Host host) (show port) jsonTCPServerFromChan

  -- Send messages to server via the chan
  void $ awaitForever (liftIO . writeChan chan . Just)

  -- Tell server to shut down
  liftIO $ writeChan chan Nothing


-- * Websockets

websocketSink :: (MonadIO m) => String -> Int -> Sink ByteString m ()
websocketSink host port = do
  chan <- liftIO $ newChan

  -- Server loop: Send what comes in via the chan; Nothing shuts down
  let jsonWSServerFromChan :: WS.PendingConnection  -> IO ()
      jsonWSServerFromChan = \req -> do
        conn <- WS.acceptRequest req
        void $ untilNothing (readChan chan) (WS.sendTextData conn)

  -- Fork off Websocket server
  _ <- liftIO $ forkIO $ WS.runServer host port jsonWSServerFromChan

  -- Send messages to server via the chan
  void $ awaitForever (liftIO . writeChan chan . Just)

  -- Tell server to shut down
  liftIO $ writeChan chan Nothing
