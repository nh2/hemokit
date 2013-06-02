module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Complex
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Numeric.FFT.Vector.Unnormalized
import qualified Data.Vector as V
import           Text.Show.Pretty (ppShow)

import           Hemokit

packets :: EmotivDevice -> Source IO EmotivPacket
packets d = forever (liftIO (readEmotivPacket d) >>= yield)

buffer :: Monad m => Int -> Conduit a m [ a ]
buffer n = forever (CL.take n >>= yield)

-- Convert a length M list of length N vectors into a length N list of length M vectors.
transposeV :: Int -> [ V.Vector a ] -> [ V.Vector a ]
transposeV n vs = [ V.fromList (map (V.! i) vs) | i <- [ 0 .. n - 1 ] ]

printAll :: Show a => Sink a IO ()
printAll = awaitForever (liftIO . print)

main :: IO ()
main = do

  ds <- getEmotivDevices
  when (length ds == 0) (error "No devices found.")

  d <- openEmotivDevice $ primaryDevice ds

  let size = 16
  let fft = plan dftR2C size

  let sensorData = mapOutput (V.map fromIntegral . sensors) (packets d)
  let fftConduit = mapOutput (map (V.map magnitude . execute fft) . transposeV 14) (buffer size)

  sensorData $= fftConduit $$ printAll
