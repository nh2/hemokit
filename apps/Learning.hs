{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Learning where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HLearn.Algebra hiding (Map) -- TODO report
import HLearn.Models.Distributions


type FeatureIndex = Int

data BayesClassifier label = BayesClassifier -- label must be Ord,
  { distsByLabelAndFeature :: [(label, [Normal Double])]
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
             (label, ) <$> mapM ( distFor . (label, ) ) [0 .. len-1]

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
      Nothing                   -> Left $ "trainBayes: Missing (label x feature): " ++ show lf
      Just dist
        | variance dist == 0.0 -> Left $ "variance undefined (only one feature value?) for (label x feature): " ++ show lf
        | otherwise            -> Right dist


classifyBayes' :: (Ord r) => BayesClassifier r -> [Double] -> [(r, Double)]
classifyBayes' classifier features = either error id $ classifyBayes classifier features


classifyBayes :: (Ord r) => BayesClassifier r -> [Double] -> Either String [(r, Double)]
classifyBayes (BayesClassifier dists len) features
  | length features /= len = Left "classifyBayes: input feature vector not of same size as training ones"
  | otherwise              = Right [ (label, product -- naive Bayes: simply multiply probabilities
                                               [ pdf d val | (val, d) <- zip features featureDists ]
                                     )
                                   | (label, featureDists) <- dists ]
