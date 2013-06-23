module Hemokit.Conduit where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson (ToJSON (..), encode)
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Network.WebSockets as WS

import           Hemokit

import           Hemokit.Internal.Utils (untilNothing)


rawSource :: (MonadIO m) => EmotivDevice -> Source m EmotivRawData
rawSource dev = void $ untilNothing (liftIO (readEmotivRaw dev)) yield

parsePackets :: (MonadIO m) => EmotivDevice -> Conduit EmotivRawData m (EmotivState, EmotivPacket)
parsePackets dev = awaitForever (liftIO . updateEmotivState dev)


-- * Convenience

emotivStates :: (MonadIO m) => EmotivDevice -> Source m EmotivState
emotivStates dev = rawSource dev $= mapOutput fst (parsePackets dev)

emotivPackets :: (MonadIO m) => EmotivDevice -> Source m EmotivPacket
emotivPackets dev = rawSource dev $= mapOutput snd (parsePackets dev)


-- * JSON

jsonConduit :: (Monad m, ToJSON i) => Conduit i m ByteString
jsonConduit = awaitForever (return . encode)


-- * Websockets

websocketSink :: (MonadIO m, ToJSON a) => String -> Int -> Sink a m ()
websocketSink host port = do
  chan <- liftIO $ newChan

  -- Server loop: Send what comes in via the chan; Nothing shuts down
  let jsonWSServerFromChan :: WS.Request -> WS.WebSockets WS.Hybi10 ()
      jsonWSServerFromChan = \req -> do
        WS.acceptRequest req
        void $ untilNothing (liftIO (readChan chan)) (WS.sendTextData . encode)

  -- Fork off Websocket server
  _ <- liftIO $ forkIO $ WS.runServer host port jsonWSServerFromChan

  -- Send messages to server via the chan
  void $ awaitForever (liftIO . writeChan chan . Just)

  -- Tell server to shut down
  liftIO $ writeChan chan Nothing
