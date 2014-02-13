{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuadTree ( makeZone
                , atLocation
                , outOfBounds
                , showZone, printZone ) where

import Control.Lens (Lens', lens)
import Data.List (find)
import Data.Maybe (fromJust)

---- Structures:

type Location = (Int, Int)

--

data QuadZone a = Wrapper { wrappedTree :: QuadTree a
                          , zoneLength :: Int
                          , zoneWidth  :: Int
                          , zoneDepth :: Int }

instance Functor QuadZone where
  fmap fn zone = zone { wrappedTree = fmap fn (wrappedTree zone) }

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
  show (Node a b c d) = "{" ++ show a ++
                        " " ++ show b ++
                        " " ++ show c ++
                        " " ++ show d ++ "}"

---- Lens:

atLocation :: Eq a => Location -> Lens' (QuadZone a) a
atLocation index = lens (getLocation index) (setLocation index)

getLocation :: Location -> QuadZone a -> a
getLocation index zone
  | outOfBounds zone index =
      error "Location index out of QuadZone bounds."
  | otherwise =
      go (balanceIndex zone index) (zoneDepth zone) (wrappedTree zone)
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
      zone {wrappedTree = go (balanceIndex zone index)
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
      where fusedNode =
              case newNode of
                Node (Leaf a') (Leaf b') (Leaf c') (Leaf d')
                  | a' == b' && b' == c' && c' == d' -> Leaf a'
                _ -> newNode
            newNode
              | y < mid   = if x < mid then Node (recurse a) b c d
                                       else Node a (recurse b) c d
              | otherwise = if x < mid then Node a b (recurse c) d
                                       else Node a b c (recurse d)
            recurse = go (x `mod` mid, y `mod` mid) (n - 1)
            mid = 2 ^ (n - 1)

-- Helpers:

outOfBounds :: QuadZone a -> Location -> Bool
outOfBounds zone (x,y) = x < 0 || y < 0
                         || x >= zoneLength zone
                         || y >= zoneWidth  zone

balanceIndex :: QuadZone a -> Location -> Location
balanceIndex zone (x,y) = (x + xOffset, y + yOffset)
  where dimension = 2 ^ zoneDepth zone
        xOffset = (dimension - zoneLength zone) `div` 2
        yOffset = (dimension - zoneWidth  zone) `div` 2

-- Constructor:

makeZone :: (Int, Int) -> a -> QuadZone a
makeZone (x,y) a
  | x <= 0 || y <= 0 = error "Invalid dimensions for zone."
  | otherwise = Wrapper { wrappedTree = Leaf a
                        , zoneLength = x
                        , zoneWidth  = y
                        , zoneDepth = fst . fromJust $
                            find ((>= (max x y)) . snd) $
                              zip [0..] (iterate (*2) 1) }


-- Sample Printers:

showZone :: (a -> Char) -> QuadZone a -> String
showZone printer zone = breakString (zoneWidth zone) string
  where string   = map printer grid
        grid = [getLocation (x,y) zone |
                x <- [0 .. pred $ zoneLength zone],
                y <- [0 .. pred $ zoneWidth  zone]]
        breakString :: Int -> String -> String
        breakString _ [] = []
        breakString n xs = a ++ "\n" ++ breakString n b
          where (a,b) = splitAt n xs

printZone :: (a -> Char) -> QuadZone a -> IO ()
printZone = ((.).(.)) putStr showZone


--------- Test:

-- x :: QuadZone Int
-- x = Wrapper { zoneLength = 4
--             , zoneWidth = 4
--             , zoneDepth = 2
--             , wrappedTree = y }

-- y :: QuadTree Int
-- y = Node (Leaf 1)
--          (Node (Leaf 1)
--                (Leaf 0)
--                (Leaf 0)
--                (Leaf 0))
--          (Leaf 1)
--          (Leaf 1)

-- x5 = set (atLocation (2,3)) 1 (makeZone (5,7) 0)
-- x6 = set (atLocation (2,3)) 1 (makeZone (6,7) 0)
-- p n = printZone (head . show) n
