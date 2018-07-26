module GenImages where

import Lib                          (compute)
import Colour                       (makePixel)

import Codec.Picture                (withImage, generateImage, PixelRGB8(..), Image)
import System.Console.ANSI          (cursorUpLine, clearLine)
import Text.Printf                  (printf)
import Control.Monad                (when)

data Setting = Setting
  { xCoord :: Double 
  , yCoord :: Double
  , zoom   :: Double 
  , width  :: Int 
  , cutoff :: Int
  }

makeImageProgress :: Setting -> IO (Image PixelRGB8)
makeImageProgress (Setting c1 c2 l num m) =
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

makeImage :: Setting -> Image PixelRGB8
makeImage (Setting c1 c2 l num m) =
  let pixelRenderer x y = makePixel $ compute m (linearScale c1 l num x) (linearScale c2 (-l) num y)
   in generateImage pixelRenderer num num

linearScale :: Fractional a => a -> a -> Int -> Int -> a
linearScale c w steps i = (fromIntegral i) / (fromIntegral steps) * w + (c - w/2)
