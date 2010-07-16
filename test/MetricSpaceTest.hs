{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetricSpaceTest (tests, isMetricSpace) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.MetricSpace

tests :: Test
tests = testGroup "MetricSpace tests"
    [ isMetricSpace (undefined :: Integer) "Integer"
    , isMetricSpace (undefined :: Double) "Double"
    ]

isMetricSpace ::
    forall a. (Epsilon (Metric a), MetricSpace a, Arbitrary a, Show a)
    => a -> String -> Test
isMetricSpace _type groupName = testGroup (groupName ++ " is a metric space")
        [ makeTest "non-negative" prop_non_negative
        , makeTest "indiscernible" prop_indiscernible
        , makeTest "symmetry" prop_symmetry
        , makeTest "triangle inequality" prop_triangle_inequality
        ]
  where
    makeTest name f = testProperty name $ f _type

prop_non_negative :: MetricSpace a => a -> a -> a -> Bool
prop_non_negative _ x y = distance x y >= 0

prop_indiscernible :: MetricSpace a => a -> a -> Bool
prop_indiscernible _ x = distance x x == 0

prop_symmetry :: MetricSpace a => a -> a -> a -> Bool
prop_symmetry _ x y = (distance x y) == (distance y x)

prop_triangle_inequality :: (MetricSpace a, Epsilon (Metric a))
                         => a -> a -> a -> a -> Bool
prop_triangle_inequality _ x y z =
    (distance x z) <= ((distance x y) + (distance y z) + epsilon)


class Epsilon a where
    epsilon :: a

instance Epsilon Integer where
    epsilon = 0

instance Epsilon Float where
    epsilon = 0.00001

instance Epsilon Double where
    epsilon = 0.000000001
