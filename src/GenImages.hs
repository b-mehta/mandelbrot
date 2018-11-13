module GenImages (Setting(..), makeImg, makeImage, makeImageProgress, Precision) where

import Lib                          (compute)
import Colour                       (makePixel)

import Codec.Picture                (withImage, generateImage, PixelRGB8(..), Image)
import System.Console.ANSI          (cursorUpLine, clearLine)
import Text.Printf                  (printf)
import Control.Monad                (when)

data Setting = Setting
  { xCoord :: Precision
  , yCoord :: Precision
  , zoom   :: Precision
  , width  :: Int
  , cutoff :: Int
  }

type Precision = Double

mkPixel :: Setting -> Int -> Int -> PixelRGB8
mkPixel (Setting c1 c2 l num m) x y = makePixel (compute m x' y')
  where x' = linearScale c1 l num x
        y' = linearScale c2 (-l) num y

makeImageProgress :: Setting -> IO (Image PixelRGB8)
makeImageProgress c =
  let num = width c
      step = (num * num) `div` 1000
      pixelRenderer x y = do when (x == 0 && (num * y) `mod` step == 0) $ do
                               cursorUpLine 1 >> clearLine
                               putStrLn (printf "Rendering: %4.1f%%" (fromIntegral (y * 1000 `div` num) / 10 :: Float))
                             return $ mkPixel c x y
   in do
     putStrLn ""
     img <- withImage num num pixelRenderer
     cursorUpLine 1 >> clearLine
     return img

makeImage :: Setting -> Image PixelRGB8
makeImage c = generateImage (mkPixel c) (width c) (width c)

makeImg :: Setting -> IO (Image PixelRGB8)
makeImg c
  | width c >= 32 = makeImageProgress c
  | otherwise     = return $ makeImage c

linearScale :: Fractional a => a -> a -> Int -> Int -> a
linearScale c w steps i = fromIntegral i / fromIntegral steps * w + (c - w/2)
