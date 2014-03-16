{-# LANGUAGE Safe #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuadTree ( QuadTree, makeTree
                , atLocation
                , outOfBounds, fuseTree
                , expand, tile, foldTiles
                , filterTree, sortTreeBy, filterTiles, sortTilesBy
                , showTree, printTree ) where

import Control.Lens.Type (Lens')
import Control.Lens.Lens (lens)
-- import Control.Lens.Setter (set)

import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Composition ((.:))

-- Foldable:

import Data.Foldable (Foldable, foldr)
import Prelude hiding (foldr)

---- Structures:

type Location = (Int, Int)

--

data QuadTree a = Wrapper { wrappedQuad :: Quadrant a
                          , zoneLength :: Int
                          , zoneWidth  :: Int
                          , zoneDepth :: Int }
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

---- Lens:

atLocation :: Eq a => Location -> Lens' (QuadTree a) a
atLocation index = lens (getLocation index) (setLocation index)

getLocation :: Location -> QuadTree a -> a
getLocation index zone
  | outOfBounds zone index =
      error "Location index out of QuadTree bounds."
  | otherwise =
      go (offsetIndex zone index) (zoneDepth zone) (wrappedQuad zone)
  where
    go :: Location -> Int -> Quadrant a -> a
    go _ _ (Leaf x) = x
    go _ 0 _        = error "Wrapped tree is deeper than zone depth."
    go (x,y) n (Node a b c d) =
      go (x `mod` mid, y `mod` mid) (n - 1) node
      where mid = 2 ^ (n - 1)
            node | y < mid   = if x < mid then a
                                          else b
                 | otherwise = if x < mid then c
                                          else d

setLocation :: forall a. Eq a => Location -> QuadTree a -> a -> QuadTree a
setLocation index zone new
  | outOfBounds zone index =
      error "Location index out of QuadTree bounds."
  | otherwise =
      onQuads (go (offsetIndex zone index) (zoneDepth zone)) zone
  where
    go :: Eq a => Location -> Int -> Quadrant a -> Quadrant a
    go (x,y) n (Leaf old)
      | old == new  = Leaf old
      |   n == 0    = Leaf new
      | otherwise   = go (x,y) n (Node l l l l)
      where l = Leaf old
    go _     0 _    = error "Wrapped tree is deeper than zone depth."
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

outOfBounds :: QuadTree a -> Location -> Bool
outOfBounds zone (x,y) = x < 0 || y < 0
                         || x >= zoneLength zone
                         || y >= zoneWidth  zone

offsetIndex :: QuadTree a -> Location -> Location
offsetIndex zone (x,y) = (x + xOffset, y + yOffset)
  where xOffset = (dimension - zoneLength zone) `div` 2
        yOffset = (dimension - zoneWidth  zone) `div` 2
        dimension = 2 ^ zoneDepth zone

fuse :: Eq a => Quadrant a -> Quadrant a
fuse (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
  | a == b && b == c && c == d = Leaf a
fuse oldNode                   = oldNode

---- Functor:

onQuads :: (Quadrant a -> Quadrant b) -> QuadTree a -> QuadTree b
onQuads fn zone = zone {wrappedQuad = fn (wrappedQuad zone)}

{- fuseTree is an exported helper function for treating QuadTrees as
   functors. It won't properly garbage collect equivalent nodes under
   a simple fmap, so a cleanup function is needed to compress it back
   down to its proper size. -}

fuseTree :: Eq a => QuadTree a -> QuadTree a
fuseTree = onQuads fuseQuad

fuseQuad :: Eq a => Quadrant a -> Quadrant a
fuseQuad (Node a b c d) = fuse $ Node (fuseQuad a)
                                      (fuseQuad b)
                                      (fuseQuad c)
                                      (fuseQuad d)
fuseQuad leaf           = leaf

---- Foldable:

--   Region = (floorX, floorY, ceilX, ceilY)
type Region = (Int,    Int,    Int,   Int)
type Tile a = (a, Region)

foldTree :: (a -> b -> b) -> b -> QuadTree a -> b
foldTree fn z = foldr fn z . expand . tile

expand :: [Tile a] -> [a]
expand = concatMap decompose
  where decompose :: Tile a -> [a]
        decompose (a, r) = replicate (regionArea r) a

tile :: QuadTree a -> [Tile a]
tile = foldTiles (:) []

foldTiles :: forall a b. (Tile a -> b -> b) -> b -> QuadTree a -> b
foldTiles fn z zone = go (zoneRegion zone) (wrappedQuad zone) z
  where go :: Region -> Quadrant a -> b -> b
        go r (Leaf a) = fn (a, intersection)
          where intersection = regionIntersection (boundaries zone) r
        go (xl, yt, xr, yb) (Node a b c d) =
          go (xl,       yt,       midx, midy) a .
          go (midx + 1, yt,       xr,   midy) b .
          go (xl,       midy + 1, midx, yb)   c .
          go (midx + 1, midy + 1, xr,   yb)   d
          where midx = (xr + xl) `div` 2
                midy = (yt + yb) `div` 2

zoneRegion :: QuadTree a -> Region
zoneRegion zone = (0, 0, limit, limit)
  where limit = (2 ^ zoneDepth zone) - 1

boundaries :: QuadTree a -> Region
boundaries zone = (left, top, right, bottom)
  where (left,  top)    = offsetIndex zone (0,0)
        (right, bottom) = offsetIndex zone (zoneLength zone - 1,
                                            zoneWidth  zone - 1)

regionIntersection :: Region -> Region -> Region
regionIntersection (xl , yt , xr , yb )
                   (xl', yt', xr', yb') =
  (max xl xl', max yt yt',
   min xr xr', min yb yb')

regionArea :: Region -> Int
regionArea (xl,yt,xr,yb) = (xr + 1 - xl) * (yb + 1 - yt)

---- Foldable extras:

filterTree :: (a -> Bool) -> QuadTree a -> [a]
filterTree fn = expand . filterTiles fn . tile

sortTreeBy :: (a -> a -> Ordering) -> QuadTree a -> [a]
sortTreeBy fn = expand . sortTilesBy fn . tile

filterTiles :: (a -> Bool) -> [Tile a] -> [Tile a]
filterTiles _  [] = []
filterTiles fn ((a,r) : rs)
  | fn a      = (a,r) : filterTiles fn rs
  | otherwise =         filterTiles fn rs

sortTilesBy :: (a -> a -> Ordering) -> [Tile a] -> [Tile a]
sortTilesBy fn = sortBy (fn `on` fst)

---- Constructor:

makeTree :: (Int, Int) -> a -> QuadTree a
makeTree (x,y) a
  | x <= 0 || y <= 0 = error "Invalid dimensions for zone."
  | otherwise = Wrapper { wrappedQuad = Leaf a
                        , zoneLength = x
                        , zoneWidth  = y
                        , zoneDepth = fst . fromJust $
                            find ((>= max x y) . snd) $
                              zip [0..] (iterate (*2) 1) }


---- Sample Printers:

showTree :: (a -> Char) -> QuadTree a -> String
showTree printer zone = breakString (zoneLength zone) string
  where string   = map printer grid
        grid = [getLocation (x,y) zone |
                y <- [0 .. zoneWidth  zone - 1],
                x <- [0 .. zoneLength zone - 1]]
        breakString :: Int -> String -> String
        breakString _ [] = []
        breakString n xs = a ++ "\n" ++ breakString n b
          where (a,b) = splitAt n xs

printTree :: (a -> Char) -> QuadTree a -> IO ()
printTree = putStr .: showTree


--------- Test:

-- x' :: QuadTree Int
-- x' = Wrapper { zoneLength = 6
--             , zoneWidth = 5
--             , zoneDepth = 3
--             , wrappedQuad = y' }

-- y' :: Quadrant Int
-- y' = Node (Leaf 0)
--           (Node (Leaf 2)
--                 (Leaf 3)
--                 (Leaf 4)
--                 (Leaf 5))
--           (Leaf 1)
--           (Leaf 9)

-- basic :: QuadTree Int
-- basic = Wrapper {zoneLength = 4, zoneWidth = 5, zoneDepth = 3,
--                  wrappedQuad = Node (Leaf 0)
--                                     (Leaf 1)
--                                     (Leaf 2)
--                                     (Leaf 3)}

-- -- x5 = set (atLocation (2,3)) 1 (makeTree (5,7) 0)
-- -- x6 = set (atLocation (2,3)) 1 (makeTree (6,7) 0)
-- p n = printTree (head . show) n

-- x1 = set (atLocation (5,5)) 5 $
--      set (atLocation (3,2)) 2 $
--      set (atLocation (2,4)) 1 $
--      (makeTree (6,6) 0)
