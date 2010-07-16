{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SpatialIndex (SpatialIndex (..)) where

import Data.List.Extras.Argmax (argmin)

import Data.MetricSpace


class SpatialIndex i where
    type Element i
    type MetricReal i
    singleton :: Element i -> (Element i -> Element i -> MetricReal i) -> i
    insert :: Element i -> i -> i
    nearest :: i -> Element i -> Element i

instance MetricSpace a => SpatialIndex [a] where
    type Element [a] = a
    type MetricReal [a] = Metric a
    singleton a _ = [a]
    insert = (:)
    nearest as q = argmin (distance q) as
    
instance Ord b => SpatialIndex ([a], a -> a -> b) where
    type Element ([a], a -> a -> b) = a
    type MetricReal ([a], a -> a -> b) = b
    singleton a f = ([a], f)
    insert a (as, dist) = (a:as, dist)
    nearest (as, dist) q = argmin (dist q) as
