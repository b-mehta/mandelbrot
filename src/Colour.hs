module Colour (makePixel) where

import Data.Word                    (Word8)
import Codec.Picture                (PixelRGB8(..))

makePixel :: Real a => Maybe (Int, a) -> PixelRGB8
makePixel Nothing  = PixelRGB8 0 0 0
makePixel (Just (n,zn)) = PixelRGB8 (toPix a) (toPix b) (toPix c)
  where (a,b,c) = getVal $ snd . properFraction $! log (1 + (fromIntegral n + 1 - nu) / 80)
        nu = logBase 2 (logBase 2 (realToFrac zn))

toPix :: Double -> Word8
toPix t = floor (t * 255)

getVal :: Double -> (Double, Double, Double)
getVal x
  | t < 1     = interp (0,0,0) (0,0,1) t
  | t < 2     = interp (0,0,1) (1,1,1) (t-1)
  | t < 3     = interp (1,1,1) (1,0.5,0) (t-2)
  | otherwise = interp (1,0.5,0) (0,0,0) (t-3)
  where t = x * 4

interp :: Num a => (a, a, a) -> (a, a, a) -> a -> (a, a, a)
interp (x1,y1,z1) (x2,y2,z2) t = ((1-t) * x1 + t * x2,(1-t) * y1 + t * y2,(1-t) * z1 + t * z2)
