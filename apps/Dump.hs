{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Data.Aeson (ToJSON (..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.ByteString.Lazy.Builder.ASCII as ASCIIBuilder
import           Data.Function (fix)
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Time.Clock
import           Data.Monoid
import qualified Data.Vector as V
import           Options.Applicative hiding (action)
import           System.IO
import           Text.Read
import           Text.Show.Pretty

import           Hemokit
import           Hemokit.Start

import           Hemokit.Internal.Utils (withJustM, textBase64)
import           SocketUtils (makeTCPServer)
import           WebsocketUtils (makeWSServer)


-- | Arguments for the EEG dump application.
data DumpArgs = DumpArgs
  { emotivArgs  :: EmotivArgs
  , mode        :: DumpMode -- ^ What to dump.
  , realtime    :: Bool     -- ^ In case fromFile is used, throttle to 128 Hz.
  , listDevices :: Bool     -- ^ Do not do anything, print available devices.
  , format      :: OutputFormat      -- ^ How to print the output.
  , serve       :: Maybe ServeMethod -- ^ Serve via TCP or websockets on host:port.
  }

-- | Whether to dump raw data, hardware-sent packages, cumulative states,
-- or measurements of device-computer latency.
data DumpMode = Raw | Packets | State | Measure deriving (Eq, Ord, Show)

-- | In what format to print the output.
-- `Default` is raw bytes to stdout for `Raw` mode and `show` for everything else.
data OutputFormat = Default | Json | Spaced | SensorBytes deriving (Eq, Ord, Show)

-- | Whether to serve via plain TCP or Websockets (hostname and port).
data ServeMethod
  = TCP       String Int
  | Websocket String Int

-- | Parser for `DumpArgs`.
dumpArgsParser :: Parser DumpArgs
dumpArgsParser = DumpArgs
  <$> emotivArgsParser
  <*> option (eitherReader parseDumpMode)
      ( long "mode"
        <> value State
        <> help "What to dump. Can be 'raw', 'packets', 'state' or 'measure'" )
  <*> switch
      ( long "realtime"
        <> help "In case --from-file is used, throttle data to 128 Hz like on real device" )
  <*> switch
      ( long "list"
        <> help "Show all available Emotiv devices and exit" )
  <*> option (eitherReader parseOutputFormat)
      ( long "format"
        <> value Default
        <> help "Format output as Haskell value, JSON or space-separated" )
  <*> (optional . option (eitherReader parseHostPort))
      ( long "serve" <> metavar "HOST:PORT"
        <> help ("Serve output via a TCP server, e.g. 127.0.0.1:1234 " ++
                 "(port 1234, only localhost) or 0.0.0.0:1234 (all interfaces). " ++
                 "Use 'ws://' before the host to serve via websockets") )


-- | `DumpMode` command line parser.
parseDumpMode :: Monad m => String -> m DumpMode
parseDumpMode s = case s of
  "raw"     -> return Raw
  "packets" -> return Packets
  "state"   -> return State
  "measure" -> return Measure
  _         -> fail "Mode is not valid. Must be 'raw', 'packets', or 'state'."

-- | `OutputFormat` command line parser.
parseOutputFormat :: Monad m => String -> m OutputFormat
parseOutputFormat s = case s of
  "default"     -> return Default
  "json"        -> return Json
  "spaced"      -> return Spaced
  "sensorbytes" -> return SensorBytes
  _             -> fail "Format is not valid. Must be 'default', 'json', 'spaced' or 'sensorbytes'."


-- | Parses host and port from a string like "0.0.0.0:1234".
parseHostPort :: String -> Either String ServeMethod
parseHostPort hostPortWs = case readMaybe portStr of
  Nothing -> Left $ show portStr ++ " is not a valid port number"
  Just p  -> Right $ if ws then Websocket host p
                           else TCP host p
  where
    (ws, hostPort) = case stripPrefix "ws://" hostPortWs of
                       Just rest -> (True,  rest)
                       Nothing   -> (False, hostPortWs)

    (host, portStr) = splitLast ":" hostPort

    splitLast :: String -> String -> (String, String)
    splitLast sep s = let sp = splitOn sep s -- splitOn never returns []
                       in (intercalate sep (init sp), last sp)


-- | Space-separates all values in the order
-- `[counter] [battery] [gyroX] [gyroY] [sensors..] [qualities..]`.
whitespaceFormat :: EmotivState -> BSL.ByteString
whitespaceFormat EmotivState{ counter, battery, gyroX, gyroY, sensors, qualities }
  = Builder.toLazyByteString . mconcat
    . intersperse (Builder.char8 ' ') . map ASCIIBuilder.intDec $ ints
  where
    ints = [ counter, battery, gyroX, gyroY ] ++ V.toList sensors ++ V.toList qualities


-- | Formats all sensor values as 16 bit ints (big endian) into a `ByteString`.
sensorBytesFormat :: EmotivState -> BSL.ByteString
sensorBytesFormat EmotivState{ sensors }
  = Builder.toLazyByteString . V.foldl1' (<>)
    . V.map (Builder.word16BE . fromIntegral) $ sensors


main :: IO ()
main = do
  DumpArgs{ emotivArgs
          , mode
          , realtime
          , listDevices
          , format
          , serve
          } <- parseArgs "Dumps Emotiv data" dumpArgsParser

  -- Catch invalid mode/format combinations immediately
  -- (so that we don't block first and error afterwards, see `formatOutput`).
  when (format `elem` [Spaced, SensorBytes] && mode /= State) $
    error $ "cannot space-format in " ++ show mode ++ " mode"

  if listDevices -- Only list devices
    then getEmotivDevices >>= putStrLn . ("Available devices:\n" ++) . ppShow
    else do

      e'device <- getEmotivDeviceFromArgs emotivArgs

      -- Do we have a device?
      case e'device of
        Left err     -> error err
        Right device -> do

          let formatOutput x = case format of
                Default     -> BSL8.pack (show x)
                Json        -> encode x
                Spaced      -> error "hemokit-dump BUG: formatOutput/spaced not caught early"
                SensorBytes -> error "hemokit-dump BUG: formatOutput/sensorbytes not caught early"

          -- Print to stdout or serve via websockets? Show the datatype or format via JSON?
          -- `output` accepts anything that's JSON-formattable and showable (wrapped in JsonShowable).
          output <- case serve of
            -- TODO use Data.ByteString.Lazy.UTF8.fromString instead of BSL8 to prevent unicode errors
            Nothing        -> return (\x -> BSL8.putStrLn x >> hFlush stdout)
            Just (Websocket host port) -> do sendFn <- makeWSServer host port
                                             return sendFn
            Just (TCP host port)       -> do sendFn <- makeTCPServer host port
                                             return sendFn

          -- For --mode measure: See how long a 0-128 cycle takes
          timeRef <- newIORef =<< getCurrentTime
          countRef <- newIORef (0 :: Int)

          -- Packet loop
          fix $ \loop -> do

            timeBefore <- getCurrentTime

            -- Output accumulative state, device-sent packet, or raw data?

            moreInput <- case mode of
              Packets -> readEmotiv device `withJustM` \(_, packet) ->
                           output $ formatOutput packet

              State   -> readEmotiv device `withJustM` \(state, _) ->
                           case format of
                             Spaced      -> output $ whitespaceFormat state
                             SensorBytes -> output $ sensorBytesFormat state
                             _           -> output $ formatOutput state

              Raw     -> readEmotivRaw device `withJustM` \rawBytes -> do
                           case format of
                             Default -> -- raw data stdout; flush so that consuming apps immediately get it
                                        BS.putStr (emotivRawDataBytes rawBytes) >> hFlush stdout

                             _       -> -- use EmotivRawData newtype for base64 encoding
                                        output $ formatOutput rawBytes

              Measure -> readEmotivRaw device `withJustM` \_ -> do
                           -- When a full cycle is done, print how long it took.
                           count <- readIORef countRef
                           modifyIORef' countRef (+1)
                           when (count == 128) $ do
                             cycleTime <- diffUTCTime <$> getCurrentTime <*> readIORef timeRef
                             output . formatOutput $ toDouble cycleTime
                             writeIORef countRef 0
                             writeIORef timeRef =<< getCurrentTime
                           where
                             toDouble x = fromRational (toRational x) :: Double


            -- When realtime is on, throttle the reading to 1/129 (a real
            -- device's frequency). But take into the account the time that
            -- we have spent reading from the device.
            when realtime $ do
              timeTaken <- (`diffUTCTime` timeBefore) <$> getCurrentTime
              let delayUs = 1000000 `div` 129 - round (timeTaken * 1000000)
              when (delayUs > 0) $ threadDelay delayUs

            when moreInput loop


-- * JSON instances

instance ToJSON EmotivPacket
instance ToJSON EmotivState

instance ToJSON EmotivRawData where
  toJSON = toJSON . textBase64 . emotivRawDataBytes

instance ToJSON Sensor where
  toJSON = toJSON . show
