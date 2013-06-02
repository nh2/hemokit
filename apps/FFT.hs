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

printAll :: Show a => Sink a IO ()
printAll = awaitForever (liftIO . print)


-- Convert a length M list of length N vectors into a length N list of length M vectors.
transposeV :: Int -> [ V.Vector a ] -> [ V.Vector a ]
transposeV n vs = [ V.fromList (map (V.! i) vs) | i <- [ 0 .. n - 1 ] ]


main :: IO ()
main = do
  args <- getArgs
  let model = if "--developer" `elem` args then Developer else Consumer

  devices <- getEmotivDevices
  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice model (last devices)

  let size = 16
  let fft = plan dftR2C size

  let sensorData = mapOutput (V.map fromIntegral . sensors) (packets device)
  let fftConduit = mapOutput (map (V.map magnitude . execute fft) . transposeV 14) (buffer size)

  sensorData $= fftConduit $$ printAll
