{-# LANGUAGE ScopedTypeVariables #-}

module Learning where

import Control.Applicative
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import HLearn.Algebra hiding (Map) -- TODO report
import HLearn.Models.Distributions


type FeatureIndex = Int

data BayesClassifier label = BayesClassifier -- label must be Ord, Enum
  { distsByLabelAndFeature :: [(label, [Normal Double])]
    -- ^ invariants:
    -- * nonempty
    -- * ordered by label
    -- * all distribution lists have same size featuresSize
  , featuresSize           :: Int
  } deriving (Eq, Show)


check :: [([a], b)] -> ()
check cases
  | null cases                   = error "trainBayes: cannot train on empty cases list"
  | any hasDifferentLength cases = error "trainBayes: feature vectors do not have same size"
  | otherwise                    = ()
  where
    len                       = length $ fst (head cases)
    hasDifferentLength (l, _) = length l /= len


trainBayes :: forall r . (Ord r, Enum r, Show r) => [([Double], r)] -> BayesClassifier r
trainBayes cases | () <- check cases = BayesClassifier dists len
  where
    usedLabels = Set.toList . Set.fromList $ map snd cases -- ordered

    dists = [  (label, [ distFor (label, fi) | fi <- [0 .. len-1] ])  | label <- usedLabels ]

    m = train <$> inputsByLabelAndFeature :: Map (r, Int) (Normal Double)

    inputsByLabelAndFeature :: Map (r, Int) [Double]
    inputsByLabelAndFeature = Map.fromListWith (++)
                                [ ((label, fi), [val]) | (features, label) <- cases
                                                       , (fi, val) <- zip [0..] features ]

    len = length $ fst (head cases)
    distFor k = fromMaybe (error $ "trainBayes: Missing (label x feature): " ++ show k)
                          (Map.lookup k m)


classifyBayes :: (Ord r, Enum r) => BayesClassifier r -> [Double] -> [(r, Double)]
classifyBayes (BayesClassifier dists len) features
  | length features /= len = error "classifyBayes: input feature vector not of same size as training ones"
  | otherwise              = [ (label, pdf d val) | (label, featureDists) <- dists
                                                  , (val, d) <- zip features featureDists ]
