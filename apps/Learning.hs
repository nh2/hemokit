{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE PolyKinds #-}

module Learning where

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Bayes
import HLearn.Models.Classifiers.Common

-- import GHC.TypeLits


data FftCase r = FftCase
  { result :: r
  , ffts   :: [Double]
  } deriving (Read, Show, Eq, Ord)

toFftCase :: ([Double], r) -> FftCase r
toFftCase (vals, res) = FftCase res vals


instance Labeled (FftCase r) where
  type Label      (FftCase r) = r
  type Attributes (FftCase r) = FftCase r -- TODO change to [Double]
  getLabel = result
  getAttributes p = p

-- type Double462 = '[Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double]
type Double231 = '[Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double]
type Double10 = '[Double,Double,Double,Double,Double,Double,Double,Double,Double,Double]

instance Trainable (FftCase r) where
  -- Generated by "[" ++ (intercalate "," $ replicate 231 "Double") ++ "]
  -- type GetHList (FftCase r) = HList (r ': Replicate 231 Double) -- needs UndecidableInstances
  type GetHList (FftCase r) = HList '[r,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double]
  getHList FftCase{ result, ffts } = result ::: list2hlist ffts



data TH_result = TH_result
data TH_ffts   = TH_ffts

instance TypeLens TH_result where type TypeLensIndex TH_result = Nat1Box Zero
instance TypeLens TH_ffts   where type TypeLensIndex TH_ffts   = Nat1Box (Succ Zero)



type BayesClassifier r = Bayes TH_result
                               (Multivariate (FftCase r)
                                 '[ MultiCategorical   '[r]
                                  , Independent Normal Double10
                                  ]
                                  Double
                               )


trainFFT :: (Ord r, Enum r) => [([Double], r)] -> BayesClassifier r
trainFFT = train . map toFftCase

arbitraryResult :: (Enum r) => r
arbitraryResult = toEnum 0

classifyFFT :: (Ord r, Enum r) => BayesClassifier r -> [Double] -> r
classifyFFT classifier c = classify classifier (getAttributes (FftCase arbitraryResult c))

probabilitiesFFT :: (Ord r, Enum r) => BayesClassifier r -> [Double] -> [(r, Double)]
probabilitiesFFT classifier c = dist2list $ probabilityClassify classifier (getAttributes (FftCase arbitraryResult c))

