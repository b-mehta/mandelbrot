module Lib (compute)
  where

{-# SPECIALISE compute :: Int -> Double -> Double -> Maybe (Int, Double) #-}
compute :: (Fractional a, Ord a) => Int -> a -> a -> Maybe (Int, a)
compute m x y
  | bulbCheck x y = Nothing
  | otherwise     = compute' m x y

compute' :: (Num a, Ord a) => Int -> a -> a -> Maybe (Int, a)
compute' n c1 c2 = go (0,0) 0
  where go (x,y) count
          | x*x + y*y > 256 = Just (count, x*x + y*y)
          | count >= n      = Nothing
          | otherwise       = go (x*x - y*y + c1, 2*x*y + c2) (count + 1)

bulbCheck :: (Fractional a, Ord a) => a -> a -> Bool
bulbCheck x y = q * (q + (x - 1/4)) < y^2 / 4 || (x+1)^2 + y^2 < 1/16
  where q = (x - 1/4)^2 + y^2
