{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.QuadTree.Internal

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)

import Control.Lens (set)
import Control.Monad (replicateM)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

{- Structure

The QuadTree type has two structural invariants/constraints:

   1. The internal raw tree must not be deeper than its
   declared depth.

   2. No branch node can have four leaves that are identical.
   These need to be fused into a single leaf node by the algorithms.

We will acknowledge and manage these invariants by constructing
two separate Arbitrary generators for QuadTrees:

  1. The first generator will construct QuadTrees strictly using the
  exposed API (makeTree and setLocation). We'll use this to test if
  the invariant is consistently maintained across the subset of QuadTrees
  that the user can construct.

  2. The second generator will generate QuadTrees ex nihilo that obey
  the invariants. We'll use this for our primary testing purposes, since
  it can theoretically generate valid non-user-constructable trees and
  because it can generate large complex trees far far more efficiently.

-}

---- The API-constructable QuadTree generator

newtype APITree a = Constructed (QuadTree a)

instance Show a => Show (APITree a) where
  show (Constructed qt) = show qt

instance (Eq a, Arbitrary a) => Arbitrary (APITree a) where
  arbitrary = do
    Positive len <- arbitrary
    Positive wid  <- arbitrary
    baseValue <- arbitrary
    let baseTree = makeTree (len, wid) baseValue

    indices <- listOf $ generateIndex baseTree
    values <- infiniteListOf arbitrary
    let setList = zip indices values

    return . Constructed $ foldr setAt baseTree setList

    where setAt (index, value) = set (atLocation index) value

-- Generates a random valid location index for a quadtree
generateIndex :: QuadTree a -> Gen Location
generateIndex qt = do
  x <- choose (0, treeLength qt - 1)
  y <- choose (0, treeWidth qt - 1)
  return (x,y)


---- Ex-nihilo QuadTree generator

instance (Eq a, Arbitrary a) => Arbitrary (QuadTree a) where
  arbitrary = do
    Positive len <- arbitrary
    Positive wid <- arbitrary
    let depth = smallestDepth (len, wid)
    tree <- generateQuadrant depth

    return Wrapper { treeLength = len,
                     treeWidth = wid,
                     treeDepth = depth,
                     wrappedTree = tree }

generateQuadrant :: (Eq a, Arbitrary a) => Int -> Gen (Quadrant a)
generateQuadrant 0 = generateLeaf
generateQuadrant n = oneof [generateLeaf, generateNode (n - 1)]

generateLeaf :: Arbitrary a => Gen (Quadrant a)
generateLeaf = Leaf <$> arbitrary

generateNode :: (Eq a, Arbitrary a) => Int -> Gen (Quadrant a)
generateNode n = do
  [a,b,c,d] <- replicateM 4 (generateQuadrant n) `suchThat` (not . equalLeaves)
  return (Node a b c d)
    where equalLeaves :: Eq a => [Quadrant a] -> Bool
          equalLeaves [Leaf a, Leaf b, Leaf c, Leaf d] = allEqual [a,b,c,d]
          equalLeaves _ = False

---- APITree structural tests

-- We use Bools here since they're the most trivial Eq type.
-- A QuadTree constructed with Bool insertions is the fastest way
-- to build/fuse up a complex set of nodes at various heights.

-- Inner tree representation cannot be deeper than defined depth
prop_APITreeDepth :: APITree Bool -> Bool
prop_APITreeDepth (Constructed qt) = go (treeDepth qt) (wrappedTree qt)
  where go :: Int -> Quadrant a -> Bool
        go _ (Leaf _) = True
        go 0 _        = False
        go n (Node a b c d) = and $ fmap (go (n - 1)) [a,b,c,d]

-- Inner tree representation cannot have branches holding four equal leaves
prop_APITreeInequality :: APITree Bool -> Bool
prop_APITreeInequality (Constructed qt) = go $ wrappedTree qt
  where go :: Eq a => Quadrant a -> Bool
        go (Leaf _)            = True
        go (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
          | allEqual [a,b,c,d] = False
        go (Node a b c d) = and $ fmap go [a,b,c,d]


---- Ex Nihilo QuadTree tests

-- For completeness, we'll test the structural requirements here as well.
-- The requirements are baked into the generator, but this lets us test
-- that generator.

-- Inner tree representation cannot be deeper than defined depth
prop_treeDepth :: QuadTree Bool -> Bool
prop_treeDepth = go <$> treeDepth <*> wrappedTree
  where go :: Int -> Quadrant a -> Bool
        go _ (Leaf _) = True
        go 0 _        = False
        go n (Node a b c d) = and $ fmap (go (n - 1)) [a,b,c,d]

-- Inner tree representation cannot have branches holding four equal leaves
prop_treeInequality :: QuadTree Bool -> Bool
prop_treeInequality = go . wrappedTree
  where go :: Eq a => Quadrant a -> Bool
        go (Leaf _)            = True
        go (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
          | allEqual [a,b,c,d] = False
        go (Node a b c d)      = and $ fmap go [a,b,c,d]

{- Functor Laws:

  fmap id  ==  id
  fmap (f . g)  ==  fmap f . fmap g -}

-- prop_fmapIdempotance :: Arbitrary a => 

---- Collate and run tests:

return [] -- Template Haskell splice. See QuickCheck hackage docs.
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  allClear <- runTests
  if allClear
    then exitSuccess
    else exitFailure

--------- Manual repl test fragments:

-- x' :: QuadTree Int
-- x' = Wrapper { treeLength = 6
--             , treeWidth = 5
--             , treeDepth = 3
--             , wrappedTree = y' }

-- y' :: Quadrant Int
-- y' = Node (Leaf 0)
--           (Node (Leaf 2)
--                 (Leaf 3)
--                 (Leaf 4)
--                 (Leaf 5))
--           (Leaf 1)
--           (Leaf 9)

-- basic :: QuadTree Int
-- basic = Wrapper {treeLength = 4, treeWidth = 5, treeDepth = 3,
--                  wrappedTree = Node (Leaf 0)
--                                     (Leaf 1)
--                                     (Leaf 2)
--                                     (Leaf 3)}

-- x5 = set (atLocation (2,3)) 1 (makeTree (5,7) 0)
-- x6 = set (atLocation (2,3)) 1 (makeTree (6,7) 0)
-- p n = printTree (head . show) n

-- test = set (atLocation (0,0)) 'd' $
--        set (atLocation (5,5)) 'c' $
--        set (atLocation (3,2)) 'b' $
--        set (atLocation (2,4)) 'a' $
--        makeTree (6,6) '.'
