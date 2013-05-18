module Main where

import           Control.Monad
import           System.IO
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do

  devices <- getEmotivDevices

  putStrLn $ "AvailableDevices:\n" ++ ppShow devices

  device <- openEmotivDevice $ case devices of d:_ -> d
                                               []  -> error "no Epoc devices found"

  forever $ do
    emotivPacket <- readEmotivPacket device
    print emotivPacket
    hFlush stdout
