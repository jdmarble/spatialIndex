{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SpatialIndexTest (tests, isSpatialIndex) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.SpatialIndex

tests :: Test
tests = testGroup "SpacialIndex tests"
 [ isSpatialIndex (undefined :: [Integer]) "[Integer]"
 , isSpatialIndex (undefined :: [Double]) "[Double]"
 ]

isSpatialIndex :: ( SpatialIndex i, a ~ Element i
                  , Arbitrary i, Show i
                  , Eq a, Arbitrary a, Show a)
                 => i -> String -> Test
isSpatialIndex _type groupName = 
    testGroup (groupName ++ " is a valid spatial index")
        [ makeTest "recall" prop_recall
        ]
  where
    makeTest name f = testProperty name $ f _type

prop_recall :: (SpatialIndex i, a ~ Element i, Eq a) => i -> i -> a -> Bool
prop_recall _ i a = nearest i' a == a
    where
      i' = insert a i

