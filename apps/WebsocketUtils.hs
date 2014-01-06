{-# LANGUAGE ExistentialQuantification #-}

module WebsocketUtils
  ( jsonWSServerFromChan
  , makeJsonWSServer
  , makeJsonOrShowWSServer
  , JsonShowable (..)
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson (ToJSON(..), encode)
import           Network.WebSockets


-- | A websocket server that serves JSON from a Chan.
jsonWSServerFromChan :: (ToJSON a) => Chan a -> PendingConnection -> IO ()
jsonWSServerFromChan chan = \req -> do
  conn <- acceptRequest req
  c <- dupChan chan
  forever (readChan c >>= sendTextData conn . encode)


-- | Creates and starts (forking) a JSON-serving websocket server.
-- Returns a Chan from which the server will read.
makeJsonWSServer :: (ToJSON a) => String -> Int -> IO (Chan a)
makeJsonWSServer host port = do
  chan <- newChan
  _ <- forkIO $ runServer host port (jsonWSServerFromChan chan)
  return chan


-- | Creates a server that can serve things either as text or as JSON.
-- If `json` is true, inputs to the returned function will be sent as JSON,
-- otherwise they will be `show`n and then JSON-formatted as a String.
makeJsonOrShowWSServer :: (Show a, ToJSON a) => String -> Int -> Bool -> IO (a -> IO ())
makeJsonOrShowWSServer host port json =
  if json then do chan <- makeJsonWSServer host port
                  return (writeChan chan)
          else do chan <- makeJsonWSServer host port
                  return (writeChan chan . show)


-- | Something that can be shown and formatted as JSON.
data JsonShowable = forall a . (Show a, ToJSON a) => JsonShowable a

instance Show JsonShowable   where show (JsonShowable x) = show x
instance ToJSON JsonShowable where toJSON (JsonShowable x) = toJSON x
