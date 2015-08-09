{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Safe #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Data.QuadTree
Description : Region quadtrees with lens support.
Copyright   : (c) Ashley Moni, 2015
License     : BSD3
Maintainer  : Ashley Moni <ashley.moni1@gmail.com>
Stability   : Stable

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
  Location, atLocation, getLocation, setLocation, mapLocation,
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

import Data.QuadTree.Internal


-- $locations
-- This provides an array-style interface to the 'QuadTree', albeit
-- with an O(log n) lookup and insertion speed. This is both faster
-- and slower than an actual array (O(1) lookup and O(n) insertion
-- respectively).
--
-- The user can imagine a two dimensional grid that can be modified
-- or queried via co-ordinate pair indices.


-- $foldables
-- 'QuadTree's can be folded just like lists. If you simply replace
-- the "Prelude" fold functions with "Data.Foldable" ones...
--
-- @
-- import "Data.Foldable"
-- import "Prelude" hiding (foldr, foldl, any, sum, find...)
-- @
--
-- ... Then you can directly call them on 'QuadTree's without
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

