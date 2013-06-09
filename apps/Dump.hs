module Main where

import           Control.Monad
import           System.IO
import           Text.Show.Pretty (ppShow)
import           System.Environment

import           Hemokit


main :: IO ()
main = do
  args <- getArgs
  let packetsOnly = "--packets" `elem` args

  devices <- getEmotivDevices

  putStrLn $ "Available devices:\n" ++ ppShow devices

  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice (last devices)

  forever $ do
    (state, packet) <- readEmotiv device
    if packetsOnly then print packet
                   else print state
    hFlush stdout
