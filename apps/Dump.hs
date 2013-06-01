module Main where

import           Control.Monad
import           System.IO
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do

  devices <- getEmotivDevices
  when (length devices == 0) (error "No devices found.")

  device <- openEmotivDevice $ primaryDevice devices

  forever $ do
    emotivPacket <- readEmotivPacket device
    print emotivPacket
    hFlush stdout
