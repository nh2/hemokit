module Main where

import           Control.Monad
import           Data.List
import qualified Data.Vector as V
import           Text.Show.Pretty (ppShow)

import           Hemokit

main :: IO ()
main = do

  devices <- getEmotivDevices
  when (length devices == 0) (error "No devices found.")

  device <- openEmotivDevice $ primaryDevice devices

  forever $ do
    emotivPacket <- readEmotivPacket device
    putStrLn $ intercalate "\t" $ map show $ V.toList $ sensors emotivPacket
