{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad
import           Options.Applicative hiding (action)
import           System.IO
import           Text.Show.Pretty

import           Hemokit
import           Hemokit.Start


data DumpArgs = DumpArgs
  { emotivArgs  :: EmotivArgs
  , packetsOnly :: Bool -- ^ Only print hardware-sent information, not cumulative state.
  , listDevices :: Bool -- ^ Do not do anything, print available devices.
  }

dumpArgsParser :: Parser DumpArgs
dumpArgsParser = DumpArgs
  <$> emotivArgsParser
  <*> switch
      ( long "packets-only"
        <> help "Dump packets as sent from the device instead of cumulative state" )
  <*> switch
      ( long "list"
        <> help "Show all available Emotiv devices and exit" )


main :: IO ()
main = do
  DumpArgs{ emotivArgs
          , packetsOnly
          , listDevices
          } <- execParser $ info (helper <*> dumpArgsParser)
                                 (progDesc "Dumps Emotiv data")

  if listDevices -- Only list devices
    then getEmotivDevices >>= putStrLn . ("Available devices:\n" ++) . ppShow
    else do
      e'device <- getEmotivDeviceFromArgs emotivArgs

      case e'device of
        Left err     -> error err
        Right device -> forever $ do

          (state, packet) <- readEmotiv device
          if packetsOnly then print packet
                         else print state
          hFlush stdout
