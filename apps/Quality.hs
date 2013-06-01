module Main where

import           Control.Monad
import qualified Data.Vector.Mutable as VM
import           System.IO
import           Text.Show.Pretty (ppShow)

import           Hemokit


main :: IO ()
main = do

  devices <- getEmotivDevices
  when (length devices == 0) (error "No devices found.")

  device <- openEmotivDevice $ primaryDevice devices

  qualities <- VM.replicate 14 0

  forever $ do
    emotivPacket <- readEmotivPacket device
    case quality emotivPacket of
    	Just ( s, q ) -> do
    		VM.write qualities (fromEnum s) q
    		forM_ [minBound :: Sensor .. maxBound] $ \s' -> do
    			q' <- VM.read qualities (fromEnum s')
    			putStrLn (show s' ++ ":\t" ++ show q')
    	Nothing -> return ()
