{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.QuadTree.Internal

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Modifiers (Positive(..), NonNegative(..))
import Test.QuickCheck.Gen (Gen, choose, oneof, suchThat,
                            listOf, infiniteListOf)
import Test.QuickCheck.Property (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import Text.Show.Functions ()
import System.Exit (exitSuccess, exitFailure)

import Control.Lens.Type (Lens')
import Control.Lens.Setter (set)
import Control.Lens.Getter (view)
import Control.Monad (replicateM)
import Data.Functor ((<$>))
import Data.Composition ((.:))

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
    Positive wid <- arbitrary
    baseValue    <- arbitrary
    let baseTree = makeTree (len, wid) baseValue

    indices <- listOf $ generateIndexOf baseTree
    values  <- infiniteListOf arbitrary
    let setList = zip indices values

    return . Constructed $ foldr setAt baseTree setList
      where setAt (index, value) qt = setLocation index qt value

-- Generates a random valid location index for a quadtree
generateIndexOf :: QuadTree a -> Gen Location
generateIndexOf qt = do
  x <- choose (0, treeLength qt - 1)
  y <- choose (0, treeWidth qt  - 1)
  return (x,y)


---- Ex-nihilo QuadTree generator

newtype GenTree a = Generated (QuadTree a)

instance Show a => Show (GenTree a) where
  show (Generated qt) = show qt

instance (Eq a, Arbitrary a) => Arbitrary (GenTree a) where
  arbitrary = do
    Positive len <- arbitrary
    Positive wid <- arbitrary
    let depth = smallestDepth (len, wid)
    tree <- generateQuadrant depth

    return . Generated $ Wrapper { treeLength = len,
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
          equalLeaves _                                = False


-- Ex-nihilo Quadrant generator

instance (Eq a, Arbitrary a) => Arbitrary (Quadrant a) where
  arbitrary = do
    NonNegative depth <- arbitrary
    generateQuadrant depth

---- General index generator

-- Ideally, we'd be able to generate random dimensionally valid lenses as
-- part of the arguments to property functions that take quadtrees.
-- But we'd need dependent types for that, so we're just going to generate
-- independent random lenses and only test the ones that would work with
-- the tree.

newtype Index = MkIndex (Int, Int)

instance Arbitrary Index where
  arbitrary = do
    NonNegative x <- arbitrary
    NonNegative y <- arbitrary
    return $ MkIndex (x,y)

instance Show Index where
  show (MkIndex index) = show index


---- APITree structural tests

-- We use Bools here since they're the most trivial Eq type.
-- A QuadTree constructed with Bool insertions is the fastest way
-- to build/fuse up a complex set of nodes at various heights.

-- Inner tree representation cannot be deeper than defined depth
prop_APITreeDepth :: APITree Bool -> Bool
prop_APITreeDepth (Constructed qt) = go (treeDepth qt) (wrappedTree qt)
  where go :: Int -> Quadrant a -> Bool
        go _ (Leaf _)       = True
        go 0 _              = False
        go n (Node a b c d) = and $ fmap (go (n - 1)) [a,b,c,d]

-- Inner tree representation cannot have branches holding four equal leaves
prop_APITreeInequality :: APITree Bool -> Bool
prop_APITreeInequality (Constructed qt) = go $ wrappedTree qt
  where go :: Eq a => Quadrant a -> Bool
        go (Leaf _)            = True
        go (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
          | allEqual [a,b,c,d] = False
        go (Node a b c d)      = and $ fmap go [a,b,c,d]


---- Ex Nihilo QuadTree tests

-- For completeness, we'll test the structural requirements here as well.
-- The requirements are baked into the generator, but this lets us test
-- that generator.

-- Inner tree representation cannot be deeper than defined depth
prop_treeDepth :: GenTree Bool -> Bool
prop_treeDepth (Generated qt) = go (treeDepth qt) (wrappedTree qt)
  where go :: Int -> Quadrant a -> Bool
        go _ (Leaf _)       = True
        go 0 _              = False
        go n (Node a b c d) = and $ fmap (go (n - 1)) [a,b,c,d]

-- Inner tree representation cannot have branches holding four equal leaves
prop_treeInequality :: GenTree Bool -> Bool
prop_treeInequality (Generated qt) = go $ wrappedTree qt
  where go :: Eq a => Quadrant a -> Bool
        go (Leaf _)            = True
        go (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
          | allEqual [a,b,c,d] = False
        go (Node a b c d)      = and $ fmap go [a,b,c,d]

{- Functor laws

  fmap id = id
  fmap (f . g) = fmap f . fmap g -}

prop_functor1 :: Eq a => GenTree a -> Bool
prop_functor1 (Generated qt)     = fmap id qt == qt

prop_functor2 :: Eq c => GenTree a -> (b -> c) -> (a -> b) -> Bool
prop_functor2 (Generated qt) f g = fmap (f . g) qt == (fmap f . fmap g) qt

{- Lens laws

  view l (set l b a)  = b
  set l (view l a) a  = a
  set l c (set l b a) = set l c a -}

prop_lens1 :: Eq a => GenTree a -> a -> Index -> Property
prop_lens1 (Generated a) b (MkIndex location) =
  location `validIndexOf` a  ==>  view l (set l b a) == b
  where l :: Eq a => Lens' (QuadTree a) a
        l = atLocation location

prop_lens2 :: Eq a => GenTree a -> Index -> Property
prop_lens2 (Generated a) (MkIndex location) =
  location `validIndexOf` a  ==>  set l (view l a) a == a
  where l :: Eq a => Lens' (QuadTree a) a
        l = atLocation location

prop_lens3 :: Eq a => GenTree a -> a -> a -> Index -> Property
prop_lens3 (Generated a) b c (MkIndex location) =
  location `validIndexOf` a  ==>  set l c (set l b a) == set l c a
  where l :: Eq a => Lens' (QuadTree a) a
        l = atLocation location


validIndexOf :: Location -> QuadTree a -> Bool
validIndexOf = not .: outOfBounds


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
