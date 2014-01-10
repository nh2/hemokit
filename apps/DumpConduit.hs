{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON (..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.ByteString.Lazy.Builder.ASCII as Builder
import qualified Data.ByteString.Base64 as Base64
import           Data.Conduit
import qualified Data.Conduit.List as CL
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
import           Hemokit.Conduit
import           Hemokit.Start


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
data OutputFormat = Default | Json | Spaced deriving (Eq, Ord, Show)

-- | Whether to serve via plain TCP or Websockets (hostname and port).
data ServeMethod
  = TCP       String Int
  | Websocket String Int

-- | Parser for `DumpArgs`.
dumpArgsParser :: Parser DumpArgs
dumpArgsParser = DumpArgs
  <$> emotivArgsParser
  <*> nullOption
      ( long "mode"
        <> reader parseDumpMode <> value State
        <> help "What to dump. Can be 'raw', 'packets', 'state' or 'measure'" )
  <*> switch
      ( long "realtime"
        <> help "In case --from-file is used, throttle data to 128 Hz like on real device" )
  <*> switch
      ( long "list"
        <> help "Show all available Emotiv devices and exit" )
  <*> nullOption
      ( long "format"
        <> reader parseOutputFormat <> value Default
        <> help "Format output as Haskell value, JSON or space-separated" )
  <*> (optional . nullOption)
      ( long "serve" <> metavar "HOST:PORT"
        <> eitherReader parseHostPort
        <> help ("Serve output via a TCP server, e.g. 127.0.0.1:1234 " ++
                 "(port 1234, only localhost) or 0.0.0.0:1234 (all interfaces). " ++
                 "Use 'ws://' before the host to serve via websockets") )
  where
    -- TODO https://github.com/pcapriotti/optparse-applicative/issues/48
    eitherReader str2either = reader (either fail return . str2either)


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
  "default"-> return Default
  "json"   -> return Json
  "spaced" -> return Spaced
  _        -> fail "Format is not valid. Must be 'default', 'json', or 'spaced'."


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

whitespaceFormat :: EmotivState -> BSL.ByteString
whitespaceFormat EmotivState{ counter, battery, gyroX, gyroY, sensors, qualities }
  = Builder.toLazyByteString . mconcat
    . intersperse (Builder.char8 ' ') . map Builder.intDec $ ints
  where
    ints = [ counter, battery, gyroX, gyroY ] ++ V.toList sensors ++ V.toList qualities


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
  when (format == Spaced && mode /= State) $
    error $ "cannot space-format in " ++ show mode ++ " mode"

  if listDevices -- Only list devices
    then getEmotivDevices >>= putStrLn . ("Available devices:\n" ++) . ppShow
    else do

      e'device <- getEmotivDeviceFromArgs emotivArgs

      -- Do we have a device?
      case e'device of
        Left err     -> error err
        Right device -> do

          let -- Show the datatype or format via JSON?
              formatConduit :: (ToJSON i, Show i) => Conduit i IO BSL.ByteString
              formatConduit = case format of
                Default -> CL.map (BSL8.pack . show)
                Json    -> CL.map encode
                Spaced  -> error "hemokit-dump BUG: formatOutput/spaced not caught early"

              -- Print to stdout or serve via websockets?
              outputSink = case serve of
                Nothing                    -> CL.mapM_ BSL8.putStrLn
                Just (Websocket host port) -> websocketSink host port
                Just (TCP host port)       -> tcpSink host port

              -- Prints raw bytes to stdout
              rawBytesSink = CL.mapM_ (putStrBsFlush . emotivRawDataBytes)

              throttled = if realtime then ($= throttle) else id

          -- Output accumulative state, device-sent packet, or raw data?
          case mode of
            Packets -> throttled (emotivPackets device) $$ formatConduit =$ outputSink

            State   -> throttled (emotivStates  device) $$ case format of
                         Spaced -> CL.map whitespaceFormat =$ outputSink
                         _      -> formatConduit =$ outputSink

            Raw     -> throttled (rawSource     device) $$ case format of
                         Default -> rawBytesSink
                         _       -> formatConduit =$ outputSink -- use EmotivRawData newtype for base64 encoding

            Measure -> throttled (rawSource     device) $= measureConduit $$ formatConduit =$ outputSink

  where
    putStrBsFlush bs = BS.putStr bs >> hFlush stdout

    measureConduit = do
      -- For --mode measure: See how long a 0-128 cycle takes
      timeRef  <- liftIO $ newIORef =<< getCurrentTime
      countRef <- liftIO $ newIORef (0 :: Int)

      let yieldCyleTimes = do
            -- When a full cycle is done, print how long it took.
            count <- liftIO $ readIORef countRef
                              <* modifyIORef' countRef (+1)
            when (count == 128) $ do
              cycleTime <- liftIO $ diffUTCTime <$> getCurrentTime <*> readIORef timeRef
              yield $ toDoule cycleTime
              liftIO $ do writeIORef countRef 0
                          writeIORef timeRef =<< getCurrentTime
            where
              toDoule x = fromRational (toRational x) :: Double

      awaitForever (const yieldCyleTimes)


-- When realtime is on, throttle the reading to 1/129 (a real
-- device's frequency). But take into the account the time that
-- we have spent reading from the device.
throttle :: (MonadIO m) => Conduit i m i
throttle = fix $ \loop -> do

  timeBefore <- liftIO getCurrentTime
  m'x <- await

  case m'x of
    Nothing -> return ()
    Just x -> do
      timeTaken <- liftIO $ (`diffUTCTime` timeBefore) <$> getCurrentTime
      let delayUs = 1000000 `div` 129 - round (timeTaken * 1000000)
      when (delayUs > 0) $ liftIO $ threadDelay delayUs
      yield x
      loop


-- * JSON instances

instance ToJSON EmotivPacket
instance ToJSON EmotivState

instance ToJSON EmotivRawData where
  toJSON = toJSON . Base64.encode . emotivRawDataBytes

instance ToJSON Sensor where
  toJSON = toJSON . show
