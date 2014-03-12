{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuadTree ( makeZone
                , atLocation
                , outOfBounds, fuseZone
                , showZone, printZone ) where

import Control.Lens (Lens', lens)

import Data.List (find)
import Data.Maybe (fromJust)

-- Foldable:

import Data.Foldable (Foldable, foldr)
import Prelude hiding (foldr)

---- Structures:

type Location = (Int, Int)

--

data QuadZone a = Wrapper { wrappedTree :: QuadTree a
                          , zoneLength :: Int
                          , zoneWidth  :: Int
                          , zoneDepth :: Int }

instance Functor QuadZone where
  fmap fn = onTree $ fmap fn

instance Foldable QuadZone where
  foldr = foldZone

instance Show a => Show (QuadZone a) where
  show zone = "<" ++ dimensions ++
              ":" ++ show (wrappedTree zone) ++ ">"
    where dimensions = show l ++ "x" ++ show w
          l = zoneLength zone
          w = zoneWidth  zone

--

data QuadTree a = Leaf a
                | Node (QuadTree a)
                       (QuadTree a)
                       (QuadTree a)
                       (QuadTree a)

instance Functor QuadTree where
  fmap fn (Leaf x)       = Leaf (fn x)
  fmap fn (Node a b c d) = Node (fmap fn a)
                                (fmap fn b)
                                (fmap fn c)
                                (fmap fn d)

instance Show a => Show (QuadTree a) where
  show (Leaf x)       = show x
  show (Node a b c d) = "{a=" ++ show a ++
                        " b=" ++ show b ++
                        " c=" ++ show c ++
                        " d=" ++ show d ++ "}"

---- Lens:

atLocation :: Eq a => Location -> Lens' (QuadZone a) a
atLocation index = lens (getLocation index) (setLocation index)

getLocation :: Location -> QuadZone a -> a
getLocation index zone
  | outOfBounds zone index =
      error "Location index out of QuadZone bounds."
  | otherwise =
      go (offsetIndex zone index) (zoneDepth zone) (wrappedTree zone)
  where
    go :: Location -> Int -> QuadTree a -> a
    go _ _ (Leaf x) = x
    go _ 0 _        = error "Wrapped tree is deeper than zone depth."
    go (x,y) n (Node a b c d) =
      go (x `mod` mid, y `mod` mid) (n - 1) node
      where mid = 2 ^ (n - 1)
            node
              | y < mid   = if x < mid then a
                                       else b
              | otherwise = if x < mid then c
                                       else d

setLocation :: forall a. Eq a => Location -> QuadZone a -> a -> QuadZone a
setLocation index zone new
  | outOfBounds zone index =
      error "Location index out of QuadZone bounds."
  | otherwise =
      zone {wrappedTree = go (offsetIndex zone index)
                             (zoneDepth zone) (wrappedTree zone)}
  where
    go :: Eq a => Location -> Int -> QuadTree a -> QuadTree a
    go (x,y) n (Leaf old)
      | old == new  = Leaf old
      | n == 0      = Leaf new
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

outOfBounds :: QuadZone a -> Location -> Bool
outOfBounds zone (x,y) = x < 0 || y < 0
                         || x >= zoneLength zone
                         || y >= zoneWidth  zone

offsetIndex :: QuadZone a -> Location -> Location
offsetIndex zone (x,y) = (x + xOffset, y + yOffset)
  where dimension = 2 ^ zoneDepth zone
        xOffset = (dimension - zoneLength zone) `div` 2
        yOffset = (dimension - zoneWidth  zone) `div` 2

fuse :: Eq a => QuadTree a -> QuadTree a
fuse (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
  | a == b && b == c && c == d = Leaf a
fuse oldNode                   = oldNode

---- Functor:

onTree :: (QuadTree a -> QuadTree b) -> QuadZone a -> QuadZone b
onTree fn zone = zone {wrappedTree = fn (wrappedTree zone)}

{- fuseZone is an exported helper function for treating QuadZones as
   functors. It won't properly garbage collect equivalent nodes under
   a simple fmap, so a cleanup function is needed to compress it
   back down to its proper size. -}

fuseZone :: Eq a => QuadZone a -> QuadZone a
fuseZone = onTree fuseTree

fuseTree :: Eq a => QuadTree a -> QuadTree a
fuseTree (Node a b c d) = fuse $ Node (fuseTree a)
                                      (fuseTree b)
                                      (fuseTree c)
                                      (fuseTree d)
fuseTree leaf           = leaf

---- Foldable:

--   Region = (floorX, floorY, ceilX, ceilY)
type Region = (Int,    Int,    Int,   Int)

foldZone :: (a -> b -> b) -> b -> QuadZone a -> b
foldZone fn z zone = foldr fn z expandedList
  where expandedList = concat $
                       map (\(a, r) -> replicate (regionArea r) a) $
                       regionList zone

regionList :: QuadZone a -> [(a, Region)]
regionList zone = go (zoneRegion zone) (wrappedTree zone) []
  where go :: Region -> QuadTree a -> [(a, Region)] -> [(a, Region)]
        go r (Leaf a) z = (a, intersection) : z
          where intersection = regionIntersection (boundaries zone) r
        go (xl, yt, xr, yb) (Node a b c d) z =
          go (xl,       yt,       midx, midy) a $
          go (midx + 1, yt,       xr,   midy) b $
          go (xl,       midy + 1, midx, yb)   c $
          go (midx + 1, midy + 1, xr,   yb)   d z
          where midx = (xr + xl) `div` 2
                midy = (yt + yb) `div` 2

zoneRegion :: QuadZone a -> Region
zoneRegion zone = (0, 0, limit, limit)
  where limit = (2 ^ zoneDepth zone) - 1

boundaries :: QuadZone a -> Region
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

---- Constructor:

makeZone :: (Int, Int) -> a -> QuadZone a
makeZone (x,y) a
  | x <= 0 || y <= 0 = error "Invalid dimensions for zone."
  | otherwise = Wrapper { wrappedTree = Leaf a
                        , zoneLength = x
                        , zoneWidth  = y
                        , zoneDepth = fst . fromJust $
                            find ((>= (max x y)) . snd) $
                              zip [0..] (iterate (*2) 1) }


---- Sample Printers:

showZone :: (a -> Char) -> QuadZone a -> String
showZone printer zone = breakString (zoneLength zone) string
  where string   = map printer grid
        grid = [getLocation (x,y) zone |
                y <- [0 .. pred $ zoneWidth  zone],
                x <- [0 .. pred $ zoneLength zone]]
        breakString :: Int -> String -> String
        breakString _ [] = []
        breakString n xs = a ++ "\n" ++ breakString n b
          where (a,b) = splitAt n xs

printZone :: (a -> Char) -> QuadZone a -> IO ()
printZone = ((.).(.)) putStr showZone


--------- Test:

-- x' :: QuadZone Int
-- x' = Wrapper { zoneLength = 6
--             , zoneWidth = 5
--             , zoneDepth = 3
--             , wrappedTree = y' }

-- y' :: QuadTree Int
-- y' = Node (Leaf 0)
--           (Node (Leaf 2)
--                 (Leaf 3)
--                 (Leaf 4)
--                 (Leaf 5))
--           (Leaf 1)
--           (Leaf 9)

-- basic :: QuadZone Int
-- basic = Wrapper {zoneLength = 4, zoneWidth = 5, zoneDepth = 3,
--                  wrappedTree = Node (Leaf 0)
--                                     (Leaf 1)
--                                     (Leaf 2)
--                                     (Leaf 3)}

-- x5 = set (atLocation (2,3)) 1 (makeZone (5,7) 0)
-- x6 = set (atLocation (2,3)) 1 (makeZone (6,7) 0)
-- p n = printZone (head . show) n

-- x1 = set (atLocation (5,5)) 5 $
--      set (atLocation (3,2)) 2 $
--      set (atLocation (2,4)) 1 $
--      (makeZone (6,6) 0)
