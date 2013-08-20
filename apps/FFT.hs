{-# LANGUAGE LambdaCase, DeriveDataTypeable, NamedFieldPuns #-}

module Main where

import           Control.Arrow (second)
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Complex
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.FFT.Vector.Unnormalized
import           Options.Applicative hiding (action)
import           System.IO
import           Text.Printf
import           Text.Read

import           Hemokit
import           Hemokit.Start
import           Hemokit.Conduit

import           Hemokit.Internal.Utils (untilNothing)

import           Data.IORef
import           Control.Concurrent.Async

import qualified Learning as L


data Mode = Print | Graph | Learning | WebGraph
          deriving (Eq, Ord, Show)

-- | Arguments for the FFT application.
data FFTArgs = FFTArgs
  { emotivArgs  :: EmotivArgs
  , mode        :: Mode
  , fftSize     :: Int
  , serve       :: Maybe (String, Int) -- ^ Serve via websockets on host:port.
  }

-- | Parser for `FFTArgs`.
fftArgsParser :: Parser FFTArgs
fftArgsParser = FFTArgs
  <$> emotivArgsParser
  <*> nullOption
      ( long "mode"
        <> reader parseMode <> value Graph
        <> help "What to dump. Can be 'print', 'graph', 'webgraph' or 'learning''" )
  <*> nullOption
      ( long "fft-size" <> short 's' <> metavar "N"
        <> reader parseFFTSize <> value 64
        <> help "Size of the FFT window. Must be a positive power of 2" )
  <*> (optional . nullOption)
      ( long "serve" <> metavar "HOST:PORT"
        <> eitherReader parseHostPort
        <> help ("Serve output via websockets, e.g. 127.0.0.1:1234 " ++
                 "(port 1234, only localhost) or 0.0.0.0:1234 (all interfaces)") )


-- | `Mode` command line parser.
parseMode :: Monad m => String -> m Mode
parseMode s = case s of
  "print"    -> return Print
  "graph"    -> return Graph
  "learning" -> return Learning
  "webgraph" -> return WebGraph
  _          -> fail "Mode is not valid."


parseFFTSize :: Monad m => String -> m Int
parseFFTSize s = case readMaybe s of
  Just n | isPowerOf2 n -> return n
  Just _                -> fail "FFT size must be a power of 2"
  Nothing               -> fail "FFT size must be a number"
  where
    isPowerOf2 x = x `elem` (takeWhile (<= x) $ iterate (*2) 1)


-- | Parses host and port from a string like "0.0.0.0:1234".
parseHostPort :: String -> Either String (String, Int)
parseHostPort hostPort = case readMaybe portStr of
  Nothing -> Left $ show portStr ++ " is not a valid port number"
  Just p  -> Right (host, p)
  where
    (host, portStr) = splitLast ":" hostPort

    splitLast :: String -> String -> (String, String)
    splitLast sep s = let sp = splitOn sep s -- splitOn never returns []
                       in (intercalate sep (init sp), last sp)


-- TODO check whether we really need `ground`
rollingFFTConduit :: (Monad m) => Int -> ConduitM (Vector Double) [V.Vector Double] m ()
-- rollingFFTConduit size = mapOutput (map (V.map magnitude . execute fft . window . ground) . transposeV 14) (rollingBuffer size)
rollingFFTConduit size = mapOutput (map (V.map magnitude . execute fft . window) . transposeV 14) (rollingBuffer size)
  where
    fft = plan dftR2C size
    window = V.zipWith (*) hammingWindow

    hammingWindow :: Vector Double
    hammingWindow = V.generate size $ \i ->
        0.5 * (1 - cos (2 * pi * fromIntegral i / (fromIntegral size - 1)))


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
printAll = CL.mapM_ $ \tds -> liftIO $ putStrLn (unlines (map showFFT tds))

graphFirstSensor :: Sink [V.Vector Double] IO ()
-- graphFirstSensor = CL.mapM_ $ \tds -> liftIO $ putStrLn (unlines (map graphFFT tds))
graphFirstSensor = do
  liftIO $ hSetBuffering stdout (BlockBuffering (Just 8000))
  CL.mapM_ $ \tds -> liftIO $ putStrLn (unlines (map graphFFT [last tds])) >> hFlush stdout -- >> threadDelay 1000000


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
  FFTArgs{ emotivArgs
          , mode
          , fftSize
          , serve
          } <- parseArgs "FFT on Emotiv data" fftArgsParser

  m'device <- getEmotivDeviceFromArgs emotivArgs

  case m'device of
    Left err -> error err
    Right device -> do

      let sensorData = mapOutput (V.map fromIntegral . sensors) (packets device)

      -- Note that on startup, nothing will happen for some time.
      -- This is when we buffer up packets for the first FFT.
      putStrLn "Waiting for FFT to fill..."

      let fftConduit = sensorData $= rollingFFTConduit fftSize

      case mode of
        Print    -> fftConduit $$ printAll
        Graph    -> fftConduit $$ graphFirstSensor
        Learning -> fftConduit $$ learningSink
        -- WebGraph -> fftConduit $= CL.mapM (\x -> threadDelay 1000000 >> return x) $$ case serve of
        WebGraph -> fftConduit $$ case serve of
                      Just (host, port) -> websocketSink host port
                      Nothing           -> error "no --serve option given"


learningSink :: (MonadIO m) => Sink [V.Vector Double] m ()
learningSink = do
  -- Get first few values as training data
  taggedSensorVals <- CL.isolate 160 =$ keyboardSideConduit =$ CL.consume

  let trainingData :: [(Side, [Double])]
      trainingData = map (second sensorValsToFeatures) taggedSensorVals

      -- Clean input data (remove feature if all equal for any label)
      clean             = L.makeBadFeatureFilter trainingData
      cleanTrainingData = map (second clean) trainingData

      -- Train classifier
      classifier       = L.trainBayes' cleanTrainingData
      cleanAndClassify = L.probabilitiesBayes' classifier . clean

  -- Print how many features were used
  printFeaturesUsed trainingData cleanTrainingData

  liftIO $ putStrLn "Classifying..."

  -- Classify remaining data
  CL.mapM_ (liftIO . print . cleanAndClassify . sensorValsToFeatures)

  where
    sensorValsToFeatures = concat . map V.toList

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


