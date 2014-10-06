{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.QuadTree.Internal

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Lens (set)

-- QTs constructed via repetitive setting
instance (Eq a, Arbitrary a) => Arbitrary (QuadTree a) where
  arbitrary = do
    Positive x <- arbitrary
    Positive y <- arbitrary
    base <- arbitrary
    let baseTree = makeTree (x,y) base

    indices <- listOf $ generateIndex baseTree
    values <- infiniteListOf arbitrary
    let setList = zip indices values

    return $ foldr setAt baseTree setList

    where setAt (index, value) = set (atLocation index) value

-- Generates a random valid location index for a quadtree
generateIndex :: QuadTree a -> Gen Location
generateIndex qt = do
  x <- choose (0, treeLength qt - 1)
  y <- choose (0, treeWidth qt - 1)
  return (x,y)


---- TESTS

prop_saneTrees :: QuadTree Bool -> Bool
prop_saneTrees = go <$> treeDepth <*> wrappedTree
  where go :: Int -> Quadrant a -> Bool
        go _ (Leaf _) = True
        go 0 _        = False
        go n (Node a b c d) = and [go (n - 1) a,
                                   go (n - 1) b,
                                   go (n - 1) c,
                                   go (n - 1) d]

return [] -- Template Haskell splice. See QuickCheck hackage docs.
runTests :: IO Bool
runTests = $verboseCheckAll

main :: IO ()
main = do
  allClear <- runTests
  if allClear
    then exitSuccess
    else exitFailure
