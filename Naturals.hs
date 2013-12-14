module Naturals
( Nat,
  toNat,
  fromNat ) where

import Data.Function ( on )

-- New type:

newtype Nat = MakeNat Int

toNat :: Int -> Nat
toNat n | n < 0     = error "Natural number cannot be negative."
        | otherwise = MakeNat n

fromNat :: Nat -> Int
fromNat (MakeNat n) = n

-- Helper functions:

onNats :: (Int -> Int -> a) -> Nat -> Nat -> a
onNats = (`on` fromNat)

mapNats :: (Int -> Int -> Int) -> Nat -> Nat -> Nat
mapNats f x y = MakeNat (onNats f x y)

-- Typeclasses:

instance Show Nat where
  show (MakeNat n) = show n

instance Num Nat where
  (+) = mapNats (+)
  (*) = mapNats (*)
  x - y | y > x     = error "Non-natural result of subtraction."
        | otherwise = mapNats (-) x y

  abs = id

  signum 0 = 0
  signum _ = 1

  fromInteger = toNat . fromIntegral

  negate 0 = 0
  negate _ = error "Cannot negate non-zero natural."

instance Eq Nat where
  (==)    = onNats (==)

instance Ord Nat where
  compare = onNats compare
  (<)     = onNats (<)
  (>=)    = onNats (>=)
  (>)     = onNats (>)
  (<=)    = onNats (<=)
  max     = mapNats max
  min     = mapNats min

instance Real Nat where
  toRational = toRational . fromNat

instance Bounded Nat where
  minBound = 0
  maxBound = MakeNat maxBound

instance Enum Nat where
  succ (MakeNat n) | n == maxBound = error "Machine word upper bound reached."
                   | otherwise     = MakeNat (succ n)

  pred (MakeNat 0) = error "No naturals below 0."
  pred (MakeNat n) = MakeNat (pred n)

  toEnum   = toNat
  fromEnum = fromNat

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | y >= x    = maxBound
                | otherwise = minBound

instance Integral Nat where
  quot = mapNats quot
  rem  = mapNats rem
  div  = quot
  mod  = rem
  
  quotRem n d = (MakeNat x, MakeNat y)
    where (x,y) = onNats quotRem n d
  divMod = quotRem

  toInteger = toInteger . fromNat
