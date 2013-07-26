{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Learning where

import Control.Applicative
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


check :: [([a], b)] -> Either String ()
check cases
  | null cases                   = Left "trainBayes: cannot train on empty cases list"
  | any hasDifferentLength cases = Left "trainBayes: feature vectors do not have same size"
  | otherwise                    = return ()
  where
    len                       = length $ fst (head cases)
    hasDifferentLength (l, _) = length l /= len


trainBayes' :: forall r . (Ord r, Show r) => [([Double], r)] -> BayesClassifier r
trainBayes' = either error id . trainBayes


-- TODO error types instead of String

trainBayes :: forall r . (Ord r, Show r) => [([Double], r)] -> Either String (BayesClassifier r)
trainBayes cases = do -- Either monad

  check cases

  dists <- forM usedLabels $ \label ->
             (label, counts label, ) <$> mapM ( distFor . (label, ) ) [0 .. len-1]

  return $ BayesClassifier dists len
  where
    usedLabels = Set.toList . Set.fromList $ map snd cases -- ordered
    len        = length $ fst (head cases)

    m = train <$> inputsByLabelAndFeature :: Map (r, FeatureIndex) (Normal Double)

    inputsByLabelAndFeature :: Map (r, FeatureIndex) [Double] -- [Double] guaranteed to be non-empty
    inputsByLabelAndFeature = Map.fromListWith (++)
                                [ ((label, fi), [val]) | (features, label) <- cases
                                                       , (fi, val) <- zip [0..] features ]

    distFor lf = case Map.lookup lf m of
      Nothing                  -> error $ "trainBayes BUG: Missing (label x feature): " ++ show lf
      Just dist
        | variance dist == 0.0 -> Left $ "variance undefined (only one feature value?) for (label x feature): " ++ show lf
        | otherwise            -> Right dist

    countsMap :: Map r Int
    countsMap = Map.fromListWith (\ !n !o -> n + o) [ (label, 1) | (_, label) <- cases ]

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
  | otherwise              = Right
      [ (label, sum . map log $ -- naive Bayes: simply multiply probabilities (== sum the logs to not exceed FP arithmetic)
                  (labelCount ./. nSamples):[ pdf d val | (val, d) <- zip features featureDists ]
        )
      | (label, labelCount, featureDists) <- dists ]
  where
    nSamples = sum (map (\(_, c, _) -> c) dists)
    a ./. b = fromIntegral a / fromIntegral b



classifyBayes' :: (Ord r) => BayesClassifier r -> [Double] -> r
classifyBayes' c features = either error id $ classifyBayes c features


classifyBayes :: (Ord r) => BayesClassifier r -> [Double] -> Either String r
classifyBayes c features = fst . maximumBy (compare `on` snd) <$> probabilitiesBayes c features
