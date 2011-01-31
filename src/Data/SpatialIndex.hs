{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SpatialIndex (SpatialIndex (..)) where

import Data.List.Extras.Argmax (argmin)

import Data.MetricSpace


class SpatialIndex i where
    type Element i
    type Query i
    insert :: Element i -> i -> i
    nearest :: i -> Query i -> Element i

instance MetricSpace a => SpatialIndex [a] where
    type Element [a] = a
    type Query [a] = a
    insert = (:)
    nearest as q = argmin (distance q) as

instance Ord c => SpatialIndex (b -> a -> c, [a]) where
    type Element (b -> a -> c, [a]) = a
    type Query (b -> a -> c, [a]) = b
    insert a (dist, as) = (dist, a:as)
    nearest (_, []) _ = error "nearest: empty SpatialIndex"
    nearest (dist, as) q = argmin (dist q) as
