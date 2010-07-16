{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MetricSpace (MetricSpace (..)) where

class (Real (Metric a), Eq a) => MetricSpace a where
    type Metric a
    distance :: a -> a -> Metric a


instance MetricSpace Integer where
    type Metric Integer = Integer
    distance = absDiff

instance MetricSpace Float where
    type Metric Float = Float
    distance = absDiff

instance MetricSpace Double where
    type Metric Double = Double
    distance = absDiff

absDiff :: Num n => n -> n -> n
absDiff a b = abs (a - b)
