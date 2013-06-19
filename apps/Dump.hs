{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Monad
import           Data.Aeson (ToJSON (..), encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Base64 as Base64
import           Data.List
import           Data.List.Split (splitOn)
import           Options.Applicative hiding (action)
import           System.IO
import           Text.Read
import           Text.Show.Pretty

import           Hemokit
import           Hemokit.Start

import           WebsocketUtils (makeJsonOrShowWSServer, JsonShowable (..))


-- | Arguments for the EEG dump application.
data DumpArgs = DumpArgs
  { emotivArgs  :: EmotivArgs
  , packetsOnly :: Bool -- ^ Only print hardware-sent information, not cumulative state.
  , listDevices :: Bool -- ^ Do not do anything, print available devices.
  , json        :: Bool -- ^ Whether to format the output as JSON.
  , serve       :: Maybe (String, Int) -- ^ Serve via websockets on host:port.
  }

-- | Parser for `DumpArgs`.
dumpArgsParser :: Parser DumpArgs
dumpArgsParser = DumpArgs
  <$> emotivArgsParser
  <*> switch
      ( long "packets-only"
        <> help "Dump packets as sent from the device instead of cumulative state" )
  <*> switch
      ( long "list"
        <> help "Show all available Emotiv devices and exit" )
  <*> switch
      ( long "json"
        <> help "Format output as JSON" )
  <*> (optional . nullOption)
      ( long "serve" <> metavar "HOST:PORT"
        <> eitherReader parseHostPort
        <> help ("Serve output via websockets on this address, e.g. 127.0.0.1:1234 " ++
                 "(port 1234, only localhost) or 0.0.0.0:1234 (port 1234, all interfaces)") )
  where
    -- TODO https://github.com/pcapriotti/optparse-applicative/issues/48
    eitherReader str2either = reader (either fail return . str2either)


-- | Parses host and port from a string like "0.0.0.0:1234".
parseHostPort :: String -> Either String (String, Int)
parseHostPort hostPort = case readMaybe portStr of
  Nothing -> Left $ show portStr ++ " is not a valid port number"
  Just p  -> Right (host, p)
  where
    (host, portStr) = splitLast ":" hostPort

    splitLast :: String -> String -> (String, String)
    splitLast sep s = let sp = splitOn sep s -- splitOn never returns []
                       in (intercalate sep (init sp), last sp)


main :: IO ()
main = do
  DumpArgs{ emotivArgs
          , packetsOnly
          , listDevices
          , json
          , serve
          } <- execParser $ info (helper <*> dumpArgsParser)
                                 (progDesc "Dumps Emotiv data")

  if listDevices -- Only list devices
    then getEmotivDevices >>= putStrLn . ("Available devices:\n" ++) . ppShow
    else do

      e'device <- getEmotivDeviceFromArgs emotivArgs

      -- Do we have a device?
      case e'device of
        Left err     -> error err
        Right device -> do

          -- Print to stdout or serve via websockets? Show the datatype or format via JSON?
          -- `output` accepts anything that's JSON-formattable and showable (wrapped in JsonShowable).
          output <- case serve of
            Nothing           -> return (putStrLn . if json then unpack . encode else show)
            Just (host, port) -> makeJsonOrShowWSServer host port json

          -- Packet loop
          forever $ do
            (state, packet) <- readEmotiv device

            -- Output accumulative state or device-sent packet?
            if packetsOnly then output (JsonShowable packet)
                           else output (JsonShowable state)
            hFlush stdout


-- * JSON instances

instance ToJSON EmotivPacket
instance ToJSON EmotivState

instance ToJSON EmotivRawData where
  toJSON = toJSON . Base64.encode . emotivRawDataBytes

instance ToJSON Sensor where
  toJSON = toJSON . show
