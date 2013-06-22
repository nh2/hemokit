{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main
import           Control.Monad.Identity
import           Data.Conduit
import qualified Data.Conduit.List as CL



main :: IO ()
main = defaultMain
  [ bench "rollingBuffer"          $ nf (benchBuf rollingBuffer 1000)               [1..10000::Int]
  , bench "rollingBuffer DL"       $ nf (benchBuf rollingBufferDL 1000)             [1..10000::Int]
  , bench "last rollingBuffer"     $ nf (last . benchBuf rollingBuffer 1000)        [1..10000::Int]
  , bench "last rollingBuffer DL"  $ nf (last . benchBuf rollingBufferDL 1000)      [1..10000::Int]
  , bench "5th rollingBuffer"      $ nf (every5th . benchBuf rollingBuffer 1000)    [1..10000::Int]
  , bench "5th rollingBuffer DL"   $ nf (every5th . benchBuf rollingBufferDL 1000)  [1..10000::Int]
  ]
  where
    benchBuf bufFn bufSize inputList = runIdentity (CL.sourceList inputList $= bufFn bufSize $$ CL.consume)


every5th :: [a] -> [a]
every5th (x:_:_:_:_:xs) = x : every5th xs
every5th _              = []


-- Implemented with lists + reverse.
rollingBuffer :: (Monad m) => Int -> Conduit a m [ a ]
rollingBuffer 0 = return ()
rollingBuffer n = fillup 0 []
  where
    -- Consume until buffer is filled with n elements.
    fillup have buf
      | have < n  = await >>= maybe (return ()) (\x -> fillup (have+1) (x:buf))
      | otherwise = roll buf
    -- Then keep kicking one element out, taking a new element in, yielding the buffer each time.
    roll buf = do yield (reverse buf)
                  await >>= maybe (return ()) (\x -> roll (x : init buf))


-- Implemented with a Difference List.
rollingBufferDL :: (Monad m) => Int -> Conduit a m [ a ]
rollingBufferDL 0 = return ()
rollingBufferDL n = fillup 0 id
  where
    -- Consume until buffer is filled with n elements.
    fillup have front
      | have < n  = await >>= maybe (return ()) (\x -> fillup (have+1) (front . (x:)))
      | otherwise = roll front
    -- Then keep kicking one element out, taking a new element in, yielding the buffer each time.
    roll front = do yield (front [])
                    await >>= maybe (return ()) (\x -> roll (tail . front . (x:)))
