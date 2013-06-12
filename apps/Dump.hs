{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           System.IO
import           Text.Show.Pretty (ppShow)

import           Hemokit hiding (serial)


data DumpArgs = DumpArgs
  { model       :: EmotivModel
  , serial      :: Maybe SerialNumber
  , packetsOnly :: Bool
  , fromFile    :: Maybe FilePath
  -- , bla         :: FilePath
  } deriving (Eq, Show)

parseModel :: Monad m => String -> m EmotivModel
parseModel s = case s of
  "consumer"  -> return Consumer
  "developer" -> return Developer
  _           -> fail "Model is not valid. Must be 'consumer' or 'developer'."


dumpArgs :: Parser DumpArgs
dumpArgs = DumpArgs
  <$> nullOption
      ( long "model" <> metavar "MODEL"
        <> reader parseModel <> value Consumer
        <> help "Consumer or Developer model" )
  <*> (optional . nullOption)
      ( long "serial" <> metavar "SERIALNUMBER"
        <> maybeReader makeSerialNumberFromString "Serial number of has invalid format"
        <> help "The serial to use. If no --from-file is given, this will select the device" )
  <*> switch
      ( long "packets-only"
        <> help "Dump packets as sent from the device instead of cumulative state" )
  <*> (optional . strOption)
      ( long "from-file" <> metavar "PATH"
        <> help "The file path to read from (e.g. /dev/hidraw0 or myfile.dump)" )
  -- <*> strOption
  --     ( long "from-filea" <> metavar "PATH"
  --       <> help "The file path to read from (e.g. /dev/hidraw0 or myfile.dump)" )
  where
    maybeReader mbFn msg = reader $ maybe (fail msg) pure . mbFn


main :: IO ()
main = do
  DumpArgs{ model, serial, packetsOnly, fromFile
          } <- execParser $ info (helper <*> dumpArgs) (progDesc "Dumps Emotiv data")

  device <- case fromFile of
    -- Use device file / file handle
    Just f | Just s <- serial -> openEmotivDeviceFile model s f
           | otherwise        -> error "A serial number must be provided when using --from-file"

    -- Use HIDAPI
    Nothing -> do
      devices <- getEmotivDevices

      putStrLn $ "Available devices:\n" ++ ppShow devices

      case devices of
        [] -> error "No devices found."
        _  -> openEmotivDevice model $ case serial of
          Just s -> fromMaybe (error $ "No device with serial " ++ show s)
                              (find ((Just s ==) . deviceInfoSerial) devices)
          _      -> last devices

  forever $ do
    (state, packet) <- readEmotiv device
    if packetsOnly then print packet
                   else print state
    hFlush stdout
