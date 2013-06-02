module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Complex
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import           Numeric.FFT.Vector.Unnormalized
import           System.Environment

import           Hemokit


packets :: EmotivDevice -> Source IO EmotivState
packets d = forever (liftIO (readEmotiv d) >>= yield . fst)

buffer :: Monad m => Int -> Conduit a m [ a ]
buffer n = forever (CL.take n >>= yield)

printAll :: Sink [V.Vector Double] IO ()
printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map showFFT tds))


-- Convert a length M list of length N vectors into a length N list of length M vectors.
transposeV :: Int -> [ V.Vector a ] -> [ V.Vector a ]
transposeV n vs = [ V.fromList (map (V.! i) vs) | i <- [ 0 .. n - 1 ] ]

showFFT :: V.Vector Double -> String
showFFT ms = map (toChar . (/(V.maximum ms))) $ V.toList ms
    where
      toChar m
        | m < 0.25 = ' '
        | m < 0.5  = '.'
        | m < 0.75 = 'o'
        | otherwise = '#'

norm :: V.Vector Double -> V.Vector Double
norm v = V.map (subtract avg) v
  where
    avg = V.sum v / fromIntegral (V.length v)


main :: IO ()
main = do
  args <- getArgs
  let model = if "--developer" `elem` args then Developer else Consumer

  devices <- getEmotivDevices
  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice model (last devices)

  let size = 256
  let fft = plan dftR2C size

  let sensorData = mapOutput (V.map fromIntegral . sensors) (packets device)
  let fftConduit = mapOutput (map (V.map magnitude . execute fft . norm) . transposeV 14) (buffer size)

  sensorData $= fftConduit $$ printAll
