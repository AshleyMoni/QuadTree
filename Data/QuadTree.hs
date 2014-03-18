
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Safe #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : QuadTree
Description : Region quadtrees with Lens support.
Copyright   : (c) Ashley Moni, 2014
License     : GPL-3
Maintainer  : Ashley Moni <y30099923@gmail.com>
Stability   : Experimental
Portability : Rank 2 Types, Scoped Type Variables

The purpose of this module is to provide discrete region quadtrees
that can be used as simple functional alternatives to 2D arrays,
with lens support.

@
test = set ('atLocation' (0,0)) \'d\' $
       set ('atLocation' (5,5)) \'c\' $
       set ('atLocation' (3,2)) \'b\' $
       set ('atLocation' (2,4)) \'a\' $
       'makeTree' (6,6) \'.\'
@

>>> printTree id test
d.....
......
...b..
......
..a...
.....c
-}

module Data.QuadTree (
  -- * Data Type & Constructor
  QuadTree, makeTree,
  -- * Index access
  -- $locations
  Location, getLocation, setLocation, atLocation,
  -- * Functor
  fuseTree, tmap,
  -- * Foldable
  -- $foldables
  filterTree, sortTreeBy,
  -- ** Tiles
  -- $tiles
  Region, Tile,
  -- ** Tile functions
  -- $tileuse
  tile, expand, foldTiles,
  filterTiles, sortTilesBy,
  -- * Printers
  showTree, printTree,
  -- * Miscellaneous helpers
  outOfBounds, treeDimensions, regionArea, inRegion ) where

import Control.Lens.Type (Lens')
import Control.Lens.Lens (lens)
import Control.Lens.Setter (set)

import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Composition ((.:))

-- Foldable:
import Data.Foldable (Foldable, foldr)
import Prelude hiding (foldr)

---- Structures:

-- |Tuple corresponds to (X, Y) co-ordinates.

type Location = (Int, Int)

-- |The eponymous data type.
--
-- 'QuadTree' is itself a wrapper around an internal tree structure
-- along with spatial metadata about the boundaries and depth of the
-- 2D area it maps to.

data QuadTree a = Wrapper { wrappedTree :: Quadrant a
                          , treeLength :: Int
                          , treeWidth  :: Int
                          , treeDepth :: Int }
  deriving (Show, Read)

instance Functor QuadTree where
  fmap fn = onQuads $ fmap fn

instance Foldable QuadTree where
  foldr = foldTree

--

data Quadrant a = Leaf a
                | Node (Quadrant a)
                       (Quadrant a)
                       (Quadrant a)
                       (Quadrant a)
  deriving (Show, Read)

instance Functor Quadrant where
  fmap fn (Leaf x)       = Leaf (fn x)
  fmap fn (Node a b c d) = Node (fmap fn a)
                                (fmap fn b)
                                (fmap fn c)
                                (fmap fn d)

---- Index access:

-- $locations
-- This provides an array-style interface to the 'QuadTree', albeit
-- with an O(log n) lookup and insertion speed. This is both faster
-- and slower than an actual array (O(1) lookup and O(n) insertion
-- respectively).
--
-- The user can imagine a two dimensional grid that can be modified
-- or queried via co-ordinate pair indices.


-- |Lens for accessing and manipulating data at a specific
-- location.
--
-- This is simply 'getLocation' and 'setLocation' wrapped into a lens.
atLocation :: Eq a => Location -> Lens' (QuadTree a) a
atLocation index = lens (getLocation index) (setLocation index)

-- |Getter for the value at a given location for a 'QuadTree'.
getLocation :: Location -> QuadTree a -> a
getLocation index tree
  | outOfBounds tree index =
      error "Location index out of QuadTree bounds."
  | otherwise =
      go (offsetIndex tree index) (treeDepth tree) (wrappedTree tree)
  where
    go :: Location -> Int -> Quadrant a -> a
    go _ _ (Leaf x) = x
    go _ 0 _        = error "Wrapped tree is deeper than tree depth."
    go (x,y) n (Node a b c d) =
      go (x `mod` mid, y `mod` mid) (n - 1) node
      where mid = 2 ^ (n - 1)
            node | y < mid   = if x < mid then a
                                          else b
                 | otherwise = if x < mid then c
                                          else d

-- |Setter for the value at a given location for a 'QuadTree'.
--
-- This automatically compresses the 'QuadTree' nodes if possible with
-- the new value.
setLocation :: forall a. Eq a => Location -> QuadTree a -> a -> QuadTree a
setLocation index tree new
  | outOfBounds tree index =
      error "Location index out of QuadTree bounds."
  | otherwise =
      onQuads (go (offsetIndex tree index) (treeDepth tree)) tree
  where
    go :: Eq a => Location -> Int -> Quadrant a -> Quadrant a
    go (x,y) n (Leaf old)
      | old == new  = Leaf old
      |   n == 0    = Leaf new
      | otherwise   = go (x,y) n (Node l l l l)
      where l = Leaf old
    go _     0 _    = error "Wrapped tree is deeper than tree depth."
    go (x,y) n (Node a b c d) = fusedNode
      where fusedNode = fuse newNode
            newNode
              | y < mid   = if x < mid then Node (recurse a) b c d
                                       else Node a (recurse b) c d
              | otherwise = if x < mid then Node a b (recurse c) d
                                       else Node a b c (recurse d)
            recurse = go (x `mod` mid, y `mod` mid) (n - 1)
            mid = 2 ^ (n - 1)

---- Helpers:

-- |Checks if a 'Location' is outside the boundaries of a 'QuadTree'.

outOfBounds :: QuadTree a -> Location -> Bool
outOfBounds tree (x,y) = x < 0 || y < 0
                         || x >= treeLength tree
                         || y >= treeWidth  tree

-- |Dimensions of a 'QuadTree', as an Int pair.

treeDimensions :: QuadTree a
               -> (Int, Int) -- ^ (Length, Width)
treeDimensions tree = (treeLength tree, treeWidth tree)

offsetIndex :: QuadTree a -> Location -> Location
offsetIndex tree (x,y) = (x + xOffset, y + yOffset)
  where (xOffset, yOffset) = offsets tree

offsets :: QuadTree a -> (Int, Int)
offsets tree = (xOffset, yOffset)
  where xOffset = (dimension - treeLength tree) `div` 2
        yOffset = (dimension - treeWidth  tree) `div` 2
        dimension = 2 ^ treeDepth tree

fuse :: Eq a => Quadrant a -> Quadrant a
fuse (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
  | a == b && b == c && c == d = Leaf a
fuse oldNode                   = oldNode

---- Functor:

onQuads :: (Quadrant a -> Quadrant b) -> QuadTree a -> QuadTree b
onQuads fn tree = tree {wrappedTree = fn (wrappedTree tree)}

-- |Cleanup function for use after any 'Control.Monad.fmap'.
--
-- When elements of a 'QuadTree' are modified by 'setLocation' (or 
-- the 'atLocation' lens), it automatically compresses identical
-- adjacent nodes into larger ones. This keeps the 'QuadTree' from
-- bloating over constant use.
--
-- 'Control.Monad.fmap' does not do this. If you wish to treat the
-- 'QuadTree' as a 'Control.Monad.Functor', you should compose this
-- function after to collapse it down to its minimum size.
--
-- Example:
-- @
-- 'fuseTree' $ 'Control.Monad.fmap' fn tree
-- @
-- This particular example is reified in the function below.

fuseTree :: Eq a => QuadTree a -> QuadTree a
fuseTree = onQuads fuseQuads

fuseQuads :: Eq a => Quadrant a -> Quadrant a
fuseQuads (Node a b c d) = fuse $ Node (fuseQuads a)
                                       (fuseQuads b)
                                       (fuseQuads c)
                                       (fuseQuads d)
fuseQuads leaf           = leaf

-- |tmap is simply 'Control.Monad.fmap' with 'fuseTree' applied after.
--
-- prop> tmap fn tree == fuseTree $ fmap fn tree
tmap :: Eq b => (a -> b) -> QuadTree a -> QuadTree b
tmap = fuseTree .: fmap

---- Foldable:

-- $foldables
-- 'QuadTree's can be folded just like lists. If you simply replace
-- the "Prelude" fold functions with "Data.Foldable" ones...
--
-- @
-- import "Data.Foldable"
-- import "Prelude" hiding (foldr, foldl, any, sum, find...)
-- @
--
-- ... Then you can directly call then on 'QuadTree's without
-- qualification. No list functionality will be lost since the
-- "Data.Foldable" functions also work exactly like the "Prelude"
-- folds for list processing.
--
-- In addition you also get some extras like 'Data.Foldable.toList'.


-- $tiles
-- Directly folding a 'QuadTree' will expand it into a sequence of
-- elements that are then folded over. For some types of operations
-- this can be incredibly inefficient; it may be faster to simply
-- manipulate a sequence of leaves and then later decompose the
-- results into a list of elements.
--
-- For these operations, we can use 'Tile's. 'Tile's are simply
-- blocks of elements, represented by a tuple of the leaf data and
-- some information on the spatial location and dimensions of the
-- block.


-- $tileuse
-- The bread and butter method of manipulating 'Tile's is to first
-- decompose a 'QuadTree' with 'tile', process the intermediate
-- representation, and then decompose it into a final list of elements
-- with 'expand'.
--
-- @
-- 'expand' . fn . 'tile' $ tree
-- @

-- |Rectangular area, represented by a tuple of four Ints.
--
-- They correspond to (X floor, Y floor, X ceiling, Y ceiling).
--
-- The co-ordinates are inclusive of all the rows and columns in all
-- four Ints.
--
-- prop> regionArea (x, y, x, y) == 1

type Region = (Int, Int, Int, Int)

-- |Each 'Tile' is a tuple of an element from a 'QuadTree' and the
-- 'Region' it subtends.

type Tile a = (a, Region)

foldTree :: (a -> b -> b) -> b -> QuadTree a -> b
foldTree fn z = foldr fn z . expand . tile

-- |Takes a list of 'Tile's and then decomposes them into a list of
-- all their elements, properly weighted by 'Tile' size.

expand :: [Tile a] -> [a]
expand = concatMap decompose
  where decompose :: Tile a -> [a]
        decompose (a, r) = replicate (regionArea r) a

-- |Returns a list of 'Tile's. The block equivalent of
-- 'Data.Foldable.toList'.

tile :: QuadTree a -> [Tile a]
tile = foldTiles (:) []

-- |Decomposes a 'QuadTree' into its constituent 'Tile's, before
-- folding a 'Tile' consuming function over all of them.

foldTiles :: forall a b. (Tile a -> b -> b) -> b -> QuadTree a -> b
foldTiles fn z tree = go (treeRegion tree) (wrappedTree tree) z
  where go :: Region -> Quadrant a -> b -> b
        go r (Leaf a) = fn (a, normalizedIntersection)
          where normalizedIntersection =
                  (interXl - xOffset, interYt - yOffset,
                   interXr - xOffset, interYb - yOffset)
                (xOffset, yOffset)     = offsets tree
                (interXl, interYt, interXr, interYb) = 
                  regionIntersection (boundaries tree) r
        go (xl, yt, xr, yb) (Node a b c d) =
          go (xl,       yt,       midx, midy) a .
          go (midx + 1, yt,       xr,   midy) b .
          go (xl,       midy + 1, midx, yb)   c .
          go (midx + 1, midy + 1, xr,   yb)   d
          where midx = (xr + xl) `div` 2
                midy = (yt + yb) `div` 2

treeRegion :: QuadTree a -> Region
treeRegion tree = (0, 0, limit, limit)
  where limit = (2 ^ treeDepth tree) - 1

boundaries :: QuadTree a -> Region
boundaries tree = (left, top, right, bottom)
  where (left,  top)    = offsetIndex tree (0,0)
        (right, bottom) = offsetIndex tree (treeLength tree - 1,
                                            treeWidth  tree - 1)

regionIntersection :: Region -> Region -> Region
regionIntersection (xl , yt , xr , yb )
                   (xl', yt', xr', yb') =
  (max xl xl', max yt yt',
   min xr xr', min yb yb')

-- |Simple helper function that lets you calculate the area of a
-- 'Region', usually for 'Data.List.replicate' purposes.

regionArea :: Region -> Int
regionArea (xl,yt,xr,yb) = (xr + 1 - xl) * (yb + 1 - yt)

-- |Does the region contain this location?

inRegion :: Location -> Region -> Bool
inRegion (x,y) (xl,yt,xr,yb) = xl <= x && x <= xr &&
                               yt <= y && y <= yb

---- Foldable extras:

-- |'Data.List.filter's a list of the 'QuadTree' 's elements.

filterTree :: (a -> Bool) -> QuadTree a -> [a]
filterTree fn = expand . filterTiles fn . tile

-- |'Data.List.sortBy's a list of the 'QuadTree' 's elements.

sortTreeBy :: (a -> a -> Ordering) -> QuadTree a -> [a]
sortTreeBy fn = expand . sortTilesBy fn . tile

-- |'Data.List.filter's a list of the 'Tile's of a 'QuadTree'.

filterTiles :: (a -> Bool) -> [Tile a] -> [Tile a]
filterTiles _  [] = []
filterTiles fn ((a,r) : rs)
  | fn a      = (a,r) : filterTiles fn rs
  | otherwise =         filterTiles fn rs

-- |'Data.List.sortBy's a list of the 'Tile's of a 'QuadTree'.

sortTilesBy :: (a -> a -> Ordering) -> [Tile a] -> [Tile a]
sortTilesBy fn = sortBy (fn `on` fst)

---- Constructor:

-- |Constructor that generates a 'QuadTree' of the given dimensions,
-- with all cells filled with a default value.

makeTree :: (Int, Int) -- ^ (Length, Width)
                  -> a -- ^ Initial element to fill
                  -> QuadTree a
makeTree (x,y) a
  | x <= 0 || y <= 0 = error "Invalid dimensions for tree."
  | otherwise = Wrapper { wrappedTree = Leaf a
                        , treeLength = x
                        , treeWidth  = y
                        , treeDepth = fst . fromJust $
                            find ((>= max x y) . snd) $
                              zip [0..] (iterate (*2) 1) }


---- Sample Printers:

-- |Generates a newline delimited string representing a 'QuadTree' as
-- a 2D block of characters.
--
-- Note that despite the word 'show' in the function name, this does
-- not 'Text.show' the 'QuadTree'. It pretty prints it. The name
-- is simply a mnemonic for its @'QuadTree' -> String@ behaviour.

showTree :: (a -> Char) -- ^ Function to generate characters for each
                        -- 'QuadTree' element.
         -> QuadTree a -> String
showTree printer tree = breakString (treeLength tree) string
  where string   = map printer grid
        grid = [getLocation (x,y) tree |
                y <- [0 .. treeWidth  tree - 1],
                x <- [0 .. treeLength tree - 1]]
        breakString :: Int -> String -> String
        breakString _ [] = []
        breakString n xs = a ++ "\n" ++ breakString n b
          where (a,b) = splitAt n xs

-- |As 'showTree' above, but also prints it.

printTree :: (a -> Char) -- ^ Function to generate characters for each
                         -- 'QuadTree' element.
          -> QuadTree a -> IO ()
printTree = putStr .: showTree


--------- Test:

x' :: QuadTree Int
x' = Wrapper { treeLength = 6
            , treeWidth = 5
            , treeDepth = 3
            , wrappedTree = y' }

y' :: Quadrant Int
y' = Node (Leaf 0)
          (Node (Leaf 2)
                (Leaf 3)
                (Leaf 4)
                (Leaf 5))
          (Leaf 1)
          (Leaf 9)

basic :: QuadTree Int
basic = Wrapper {treeLength = 4, treeWidth = 5, treeDepth = 3,
                 wrappedTree = Node (Leaf 0)
                                    (Leaf 1)
                                    (Leaf 2)
                                    (Leaf 3)}

x5 = set (atLocation (2,3)) 1 (makeTree (5,7) 0)
x6 = set (atLocation (2,3)) 1 (makeTree (6,7) 0)
p n = printTree (head . show) n

test = set (atLocation (0,0)) 'd' $
       set (atLocation (5,5)) 'c' $
       set (atLocation (3,2)) 'b' $
       set (atLocation (2,4)) 'a' $
       makeTree (6,6) '.'
