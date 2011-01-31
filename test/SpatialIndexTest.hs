{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SpatialIndexTest (tests, isSpatialIndex) where

import Control.Applicative (Applicative, pure)
import Data.Monoid (Monoid, mappend)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.SpatialIndex


tests :: Test
tests = testGroup "SpacialIndex tests"
 [ isSpatialIndex (undefined :: [Integer]) "[Integer]"
 , isSpatialIndex (undefined :: [Double]) "[Double]"
 ]

isSpatialIndex :: ( Applicative i, Monoid (i e)
                  , SpatialIndex (i e) e q, e ~ q
                  , Arbitrary (i e), Show (i e)
                  , Eq e, Arbitrary e, Show e)
                 => i e -> String -> Test
isSpatialIndex _type groupName =
    testGroup (groupName ++ " is a valid spatial index")
        [ makeTest "recall" prop_recall
        ]
  where
    makeTest name f = testProperty name $ f _type

prop_recall :: (SpatialIndex (i e) e q, e ~ q, Eq e,
                Applicative i, Monoid (i e))
            => i e -> i e -> e -> Bool
prop_recall _ i a = nearest i' a == a
    where
      i' = mappend i $ pure a

