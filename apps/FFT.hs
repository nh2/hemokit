module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Complex
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List
import qualified Data.Vector as V
import           Numeric.FFT.Vector.Unnormalized
import           System.Environment
import           Text.Printf

import           Hemokit


packets :: EmotivDevice -> Source IO EmotivState
packets d = forever (liftIO (readEmotiv d) >>= yield . fst)

buffer :: Monad m => Int -> Conduit a m [ a ]
buffer n = forever (CL.take n >>= yield)

printAll :: Sink [V.Vector Double] IO ()
-- printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map showFFT tds))
printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map graphFFT tds))
-- printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map graphFFT [last tds]))


-- | Converts a length M list of length N vectors into a length N list of length M vectors.
-- Example: [ v1a v1b v1c ]      [ v1a, v2a ]
--          [ v2a v2b v2c ]  ->  [ v1b, v2b ]
--                               [ v1c, v2c ]
transposeV :: Int -> [ V.Vector a ] -> [ V.Vector a ]
transposeV n vs = [ V.fromList (map (V.! i) vs) | i <- [ 0 .. n - 1 ] ]

showFFT :: V.Vector Double -> String
showFFT ms = unwords . V.toList . V.map (formatNumber . maxed) $ ms
    where
      formatNumber n = printf "%.3f" n
      -- formatNumber n = printf "%2.0f" n

      -- simple      = id
      -- distributed = (/ V.sum ms)
      maxed       = (/ V.maximum ms)


graphFFT :: V.Vector Double -> String
graphFFT ms = (unlines . transpose . V.toList . V.map (formatNumber . maxed) $ ms) ++ showFFT ms
    where
      maxed = (/ V.maximum ms)
      formatNumber n = replicate space ' ' ++ replicate filled '|'
        where
          chars  = 40
          filled = floor (n * fromIntegral chars)
          space  = chars - filled


toChar :: Double -> Char
toChar m
  | m < 0.25  = ' '
  | m < 0.5   = '.'
  | m < 0.75  = 'o'
  | otherwise = '#'

-- | Reduces a data series by its average.
-- Example: 4 5 4 3 -> 0 1 0 -1
ground :: V.Vector Double -> V.Vector Double
ground v = V.map (subtract avg) v
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
  let fftConduit = mapOutput (map (V.map magnitude . execute fft . ground) . transposeV 14) (buffer size)

  sensorData $= fftConduit $$ printAll
