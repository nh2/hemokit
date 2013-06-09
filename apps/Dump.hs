module Main where

import           Control.Monad
import           System.IO
import           System.Environment
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do
  args <- getArgs
  let packetsOnly = "--packets" `elem` args
  let model       = if "--developer" `elem` args then Developer else Consumer

  devices <- getEmotivDevices

  putStrLn $ "Available devices:\n" ++ ppShow devices

  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice model (last devices)

  forever $ do
    (state, packet) <- readEmotiv device
    if packetsOnly then print packet
                   else print state
    hFlush stdout
