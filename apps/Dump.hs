module Main where

import           Control.Monad
import           System.IO
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do

  devices <- getEmotivDevices

  putStrLn $ "Available devices:\n" ++ ppShow devices

  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice (last devices)

  forever $ do
    emotivPacket <- readEmotivPacket device
    print emotivPacket
    hFlush stdout
