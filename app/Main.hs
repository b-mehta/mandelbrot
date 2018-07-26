module Main where

import Codec.Picture                (withImage, writePng, PixelRGB8(..), Image)
import Lib                          (compute, Cutoff(..))
import Colour                       (makePixel)
import System.Console.ANSI
import Text.Printf
import System.Environment
import Control.Monad (when)

makeImage :: (Double,Double) -> Double -> Int -> Cutoff -> IO (Image PixelRGB8)
makeImage (c1,c2) l num m =
  let step = num `div` 1000
      pixelRenderer x y = do when (x == 0 && y `mod` step == 0) $ do
                               cursorUpLine 1 >> clearLine
                               putStrLn (printf "Rendering: %4.1f%%" ((fromIntegral $ y * 1000 `div` num) / 10 :: Float))
                             return $ makePixel $ compute m (linearScale c1 l num x) (linearScale c2 (-l) num y)
   in do 
     putStrLn ""
     img <- withImage num num pixelRenderer 
     cursorUpLine 1 >> clearLine
     return img

linearScale :: Fractional a => a -> a -> Int -> Int -> a
linearScale c width steps i = (fromIntegral i) / (fromIntegral steps) * width + (c - width/2)

egPoint, egPoint2, egPoint3 :: (Double,Double)
egPoint =  (-0.73258263759
           ,-0.24114713638)
egPoint2 = (-1.47904448099716
           , 0.0107524846974)
egPoint3 = (-1.258489538063
           , 0.382359645113)

run :: [String] -> IO ()
run [x,y,z,a,b] = writePng "test.png" =<< makeImage (read x, read y) (read z) (read a) (Cutoff $ read b)
run _ = errorWithoutStackTrace "invalid inputs given"

main :: IO ()
main = getArgs >>= run
