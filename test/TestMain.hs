module Main where

import Test.Framework

import qualified MetricSpaceTest
import qualified SpatialIndexTest


main :: IO ()
main = defaultMain 
       [ MetricSpaceTest.tests
       , SpatialIndexTest.tests       
       ]


