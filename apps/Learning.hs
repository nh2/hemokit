{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Learning where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HLearn.Algebra hiding (Map) -- TODO report
import HLearn.Models.Distributions


-- TODO generalize Double
-- TODO Enum requirement, then use IntMap?

type FeatureIndex = Int

data BayesClassifier label = BayesClassifier -- label must be Ord,
  { distsByLabelAndFeature :: [(label, Int, [Normal Double])]
    -- ^ invariants:
    -- * nonempty
    -- * ordered by label
    -- * all distribution lists have same size featuresSize
  , featuresSize           :: Int
  } deriving (Eq, Show)


check :: [(r, [a])] -> Either String ()
check cases
  | null cases                   = Left "trainBayes: cannot train on empty cases list"
  | any hasDifferentLength cases = Left "trainBayes: feature vectors do not have same size"
  | otherwise                    = return ()
  where
    len                       = length $ snd (head cases)
    hasDifferentLength (_, l) = length l /= len


trainBayes' :: forall r . (Ord r, Show r) => [(r, [Double])] -> BayesClassifier r
trainBayes' = either error id . trainBayes


-- TODO error types instead of String
--      - in case of variance undefined, also offer the value that all things had

trainBayes :: forall r . (Ord r, Show r) => [(r, [Double])] -> Either String (BayesClassifier r)
trainBayes cases = do -- Either monad

  check cases

  dists <- forM usedLabels $ \label ->
             (label, counts label, ) <$> mapM ( distFor . (label, ) ) [0 .. len-1]

  return $ BayesClassifier dists len
  where
    usedLabels = Set.toAscList . Set.fromList $ map fst cases -- ordered
    len        = length $ snd (head cases)

    m = train <$> inputsByLabelAndFeature :: Map (r, FeatureIndex) (Normal Double)

    inputsByLabelAndFeature :: Map (r, FeatureIndex) [Double] -- [Double] guaranteed to be non-empty
    inputsByLabelAndFeature = Map.fromListWith (++)
                                [ ((label, fi), [val]) | (label, features) <- cases
                                                       , (fi, val) <- zip [0..] features ]

    distFor lf = case Map.lookup lf m of
      Nothing                  -> error $ "trainBayes BUG: Missing (label x feature): " ++ show lf
      Just dist
        | variance dist == 0.0 -> Left $ "variance undefined (only one feature value?) for (label x feature): " ++ show lf
        | otherwise            -> Right dist

    countsMap :: Map r Int
    countsMap = Map.fromListWith (\ !n !o -> n + o) [ (label, 1) | (label, _) <- cases ]

    counts :: r -> Int
    counts l = fromMaybe (error $ "trainBayes BUG: Missing label count for " ++ show l)
                         (Map.lookup l countsMap)


probabilitiesBayes' :: (Ord r) => BayesClassifier r -> [Double] -> [(r, Double)]
probabilitiesBayes' c features = either error id $ probabilitiesBayes c features


-- TODO check why my output here is different from HLearn
--      (only by a different relative scale thoug). Does it normalize?
probabilitiesBayes :: (Ord r) => BayesClassifier r -> [Double] -> Either String [(r, Double)]
probabilitiesBayes (BayesClassifier dists len) features
  | length features /= len = Left "classifyBayes: input feature vector not of same size as training ones"
  | otherwise              = Right $ map (second (/ normalizingConstant)) classProbs -- normalize
                                     -- TODO comment that this still works even with the log
  where
    -- Naive Bayes: simply multiply probabilities (== sum the logs to not exceed FP arithmetic)
    --              posterior(label) = P(label in nSamples) * p(feature_1 | label)
    --                                                      * p(feature_2 | label)
   --                                                       * ...
    classProbs = [ (label
                   , sum . map log $
                       (labelCount ./. nSamples):[ pdf d val | (val, d) <- zip features featureDists ]
                   )
                 | (label, labelCount, featureDists) <- dists ]

    -- Dividing by the sum of all class probabilities will make them sum up to 1 again.
    normalizingConstant = sum (map snd classProbs) -- TODO check sum

    nSamples = sum $ map (\(_, c, _) -> c) dists
    a ./. b = fromIntegral a / fromIntegral b



classifyBayes' :: (Ord r) => BayesClassifier r -> [Double] -> r
classifyBayes' c features = either error id $ classifyBayes c features


classifyBayes :: (Ord r) => BayesClassifier r -> [Double] -> Either String r
classifyBayes c features = fst . maximumBy (compare `on` snd) <$> probabilitiesBayes c features


-- * Input helpers


-- | Some features (buckets in the FFT) might always have the same values (be "bad").
--
-- We have to ignore these buckets for training (and later classification)
-- because they would have 0 variance (not allowed for normal distribution).
--
-- This function returns a function that drops bad features from a feature vector.
--
-- (This has to be done *per* label: a feature is bad already if it has all
-- the same values for *some* label.)
makeBadFeatureFilter :: (Ord r, Eq a) => [(r, [a])] -> ([a] -> [a])
makeBadFeatureFilter trainingData = \features ->
  if length features /= length featureMask
    then error $ "makeBadFeatureFilter CALLER ERROR: different (length features, length featureMask): "
                 ++ show (length features, length featureMask)
    else [ f | (f, True) <- zip features featureMask ]
  where
    featuresPerLabel = map snd $ partitionOnKey trainingData
    -- TODO length check for transpose
    featureMaskPerLabel = map (map notAllEquals . transpose) featuresPerLabel
    featureMask = foldl' (zipWith (&&)) (repeat True) featureMaskPerLabel -- AND all masks


notAllEquals :: (Eq a) => [a] -> Bool
notAllEquals l = case group l of _:_:_ -> True -- at least 2 groups
                                 _     -> False

partitionOnKey :: (Ord k) => [(k, a)] -> [(k, [a])]
partitionOnKey l = Map.toAscList $ Map.fromListWith (++) [ (k, [v]) | (k, v) <- l ]
