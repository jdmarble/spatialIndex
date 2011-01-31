{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.SpatialIndex (SpatialIndex (..)) where

import Data.Foldable (Foldable, toList)
import Data.List.Extras.Argmax (argmin)

import Data.MetricSpace


class SpatialIndex i e q where
    nearest :: i -> q -> e

instance (Foldable t, MetricSpace e) => SpatialIndex (t e) e e where
    nearest te q = argmin (distance q) $ toList te

instance (Foldable t, Ord o) => SpatialIndex (q -> e -> o, t e) e q where
    nearest (dist, te) q = argmin (dist q) $ toList te

instance (Foldable t, Ord o) => SpatialIndex (t (q -> o, e)) e q where
    nearest te q = snd . argmin (($ q) . fst) $ toList te
