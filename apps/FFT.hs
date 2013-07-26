{-# LANGUAGE LambdaCase, DeriveDataTypeable #-}

module Main where

import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Complex
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.FFT.Vector.Unnormalized
import           System.IO
import           Text.Printf

import           Hemokit
import           Hemokit.Start

import           Hemokit.Internal.Utils (untilNothing)

import           Data.IORef
import           Control.Concurrent.Async

import           Learning


-- TODO envelope to 0 on the sides?
rollingFFTConduit :: (Monad m) => Int -> ConduitM (Vector Double) [[Double]] m ()
rollingFFTConduit size = mapOutput (map (V.toList . V.map magnitude . execute fft . ground) . transposeV 14) (rollingBuffer size)
  where
    fft = plan dftR2C size


packets :: EmotivDevice -> Source IO EmotivState
packets d = void $ untilNothing (liftIO (readEmotiv d)) (yield . fst)

buffer :: Monad m => Int -> Conduit a m [ a ]
buffer n = forever (CL.take n >>= yield)


-- | Rolls a buffer of size n over the input, always taking one element in,
-- throwing an old one out.
-- Only starts returning buffers once the buffer is filled.
--
-- Implemented using a Difference List.
-- This allows fast skipping of buffers, e.g. for using only every 5th one.
rollingBuffer :: (Monad m) => Int -> Conduit a m [ a ]
rollingBuffer 0 = return ()
rollingBuffer n | n < 0     = error "rollingBuffer: negative buffer size"
                | otherwise = fillup 0 id
  where
    -- Consume until buffer is filled with n elements.
    fillup have front
      | have < n  = await >>= maybe (return ()) (\x -> fillup (have+1) (front . (x:)))
      | otherwise = roll front
    -- Then keep kicking one element out, taking a new element in, yielding the buffer each time.
    roll front = do yield (front [])
                    await >>= maybe (return ()) (\x -> roll (tail . front . (x:)))


printAll :: Sink [V.Vector Double] IO ()
-- printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map showFFT tds))
-- printAll = awaitForever $ \tds -> liftIO $ putStrLn (unlines (map graphFFT tds))
printAll = do
  liftIO $ hSetBuffering stdout (BlockBuffering (Just 8000))
  awaitForever $ \tds -> liftIO $ putStrLn (unlines (map graphFFT [last tds])) >> hFlush stdout -- >> threadDelay 1000000


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
-- graphFFT ms = (unlines . transpose . V.toList . V.map (formatNumber . maxed) $ ms) ++ showFFT ms
graphFFT ms = (unlines . transpose . V.toList . V.map (formatNumber . maxed) $ ms)
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
-- This is useful to bring a signal moving around at some level "to the ground".
-- Example: 4 5 4 3 -> 0 1 0 -1
ground :: V.Vector Double -> V.Vector Double
ground v = V.map (subtract avg) v
  where
    avg = V.sum v / fromIntegral (V.length v)


main :: IO ()
main = do
  m'device <- getEmotivDeviceFromArgs =<< parseArgs "FFT on Emotiv data" emotivArgsParser

  case m'device of
    Left err -> error err
    Right device -> do

      let sensorData = mapOutput (V.map fromIntegral . sensors) (packets device)

      -- Note that on startup, nothing will happen for some time.
      -- This is when we buffer up packets for the first FFT.
      putStrLn "Waiting for FFT to fill..."

      -- sensorData $= rollingFFTConduit 256 $$ printAll
      -- sensorData $= rollingFFTConduit 64 $$ printAll

      -- trainingData <- sensorData $= rollingFFTConduit 64 $= keyboardSideConduit $$ CL.consume
      -- mapM_ (\(ffts, side) -> print (head ffts, side)) trainingData

      sensorData $$ rollingFFTConduit 64 =$ do
        -- Get first few values as training data
        taggedSensorVals <- CL.isolate 160 =$ keyboardSideConduit =$ CL.consume

        let trainingData :: [(Side, [Double])]
            trainingData = map (second sensorValsToFeatures) taggedSensorVals

            -- Clean input data (remove feature if all equal for any label)
            clean             = makeBadFeatureFilter trainingData
            cleanTrainingData = map (second clean) trainingData

            -- Train classifier
            classifier       = trainBayes' cleanTrainingData
            cleanAndClassify = probabilitiesBayes' classifier . clean

        -- Print how many features were used
        printFeaturesUsed trainingData cleanTrainingData

        liftIO $ putStrLn "Classifying..."

        -- Classify remaining data
        CL.mapM_ (print . cleanAndClassify . sensorValsToFeatures)
  where
    sensorValsToFeatures = concat

    printFeaturesUsed trainingData cleanTrainingData = liftIO $ do
      let nFeatures      = length . snd . head $ trainingData
          nCleanFeatures = length . snd . head $ cleanTrainingData
      putStrLn . unlines $ [ "Features: " ++ show nFeatures
                           , "Clean features: " ++ show nCleanFeatures ]


data Side = L | R | None deriving (Enum, Eq, Ord, Show, Read)


keyboardSideConduit :: (MonadIO m) => Conduit i m (Side, i)
keyboardSideConduit = do
  liftIO $ hSetBuffering stdin NoBuffering -- immediate getChar reads

  sideRef <- liftIO $ newIORef None

  keyboardThread <- liftIO . async $ forever $ getChar >>= \case
      '1' -> putStrLn "\nRecording as LEFT"  >> writeIORef sideRef L
      '2' -> putStrLn "\nRecording as NONE"  >> writeIORef sideRef None
      '3' -> putStrLn "\nRecording as RIGHT" >> writeIORef sideRef R
      _   -> return ()

  awaitForever $ \i -> do
    side <- liftIO $ readIORef sideRef
    liftIO $ print side
    yield (side, i)

  liftIO $ cancel keyboardThread


newtype SideExample = SideExample ([V.Vector Double], Side)
                    deriving (Eq, Ord, Read, Show)


