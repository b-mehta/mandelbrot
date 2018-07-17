module Lib
  where

import Data.List            (findIndex)
import Data.Complex         (Complex, magnitude, realPart, imagPart)

newtype Cutoff = Cutoff Int

compute' :: RealFloat a => Cutoff -> Complex a -> Maybe Int
compute' (Cutoff n) = findIndex ((> 2) . magnitude) . take n . tail . flip iterate 0 . func

compute :: RealFloat a => Cutoff -> Complex a -> Maybe Int
compute m z
  | bulbCheck z = Nothing
  | otherwise   = compute' m z

-- |Determines whether the point is inside either of the two major bulbs
bulbCheck :: RealFloat a => Complex a -> Bool
bulbCheck z = q * (q + (realPart z - 1/4)) < (imagPart z)^2 / 4 || magnitude (z + 1) < 1/4
  where q = (magnitude (z - 1/4))^2

func :: Num a => a -> a -> a
func c z = z^2 + c
