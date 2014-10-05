{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.QuadTree

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)

instance Arbitrary a => Arbitrary (QuadTree a) where
  arbitrary = do
    NonNegative x <- arbitrary
    NonNegative y <- arbitrary
    base <- arbitrary
    return (makeTree (x,y) base)

prop_simple :: QuadTree a -> Bool
prop_simple qt = True

return [] -- Template Haskell splice. See QuickCheck hackage docs.
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  allClear <- runTests
  if allClear
    then exitSuccess
    else exitFailure
