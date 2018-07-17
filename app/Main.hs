module Main where

import Codec.Picture                (generateImage, writePng, PixelRGB8(..), Image, Pixel8)
import Lib                          (compute, Cutoff(..))
import Data.Complex                 (Complex(..))

makeImage :: (Complex Double, Complex Double) -> (Int,Int) -> Cutoff -> Image PixelRGB8
makeImage (x1 :+ y1, x2 :+ y2) (xNum, yNum) m = generateImage pixelRenderer xNum yNum
  where pixelRenderer x y = makePixel $ compute m $ (linearScale (x1,x2) xNum x :+ linearScale (y2,y1) yNum y)

makePixel :: Maybe Int -> PixelRGB8
makePixel Nothing  = PixelRGB8 0 0 0
makePixel (Just n) = PixelRGB8 0 t t
  where t = fromIntegral $ fold n

fold :: Int -> Int
fold n
  | m < 256   = m
  | otherwise = 512 - m
  where m = (7 * n) `mod` 512

linearScale :: Fractional a => (a, a) -> Int -> Int -> a
linearScale (l,r) steps i = (fromIntegral i) / (fromIntegral steps) * (r - l) + l

type Range a = (a,a)

wholeScale, neckScale :: Range (Complex Double)
wholeScale = ((-2) :+ (-1.5), 1 :+ 1.5)
neckScale = ((-0.9) :+ (-0.3), (-0.6) :+ 0)

largeImg, medImg, miniImg :: Range Int
largeImg = (5000,5000)
medImg = (1000,1000)
miniImg = (50,50)

main :: IO ()
main = writePng "mandelbrot.png" $! (makeImage neckScale largeImg (Cutoff 1000) :: Image PixelRGB8)
