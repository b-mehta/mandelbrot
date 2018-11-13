{-# LANGUAGE PartialTypeSignatures, ParallelListComp #-}
import Codec.Picture.Types
import Codec.FFmpeg.Juicy
import Codec.FFmpeg
import Data.List

import GenImages (makeImage, Setting(..), Precision)

x,y,lim :: Precision
x = -0.54899998583216
y = 0.60400300965798
lim = 0.0000000000003

mkSettings :: Int -> [Setting]
mkSettings w = zipWith5 Setting (repeat x) (repeat y) zooms (repeat w) (repeat 10000)

zooms :: [Precision]
zooms = takeWhile (> lim) $ iterate (*0.99) 3

size :: Int
size = 2048

main :: IO ()
main = do
  let images = makeImage <$> mkSettings size
  print (length images)
  imageWrite size size "out.mp4" images

fps :: Int
fps = 30

imageWrite :: JuicyPixelFormat p => Int -> Int -> FilePath -> [Image p] -> IO ()
imageWrite w h out xs = do
  f <- imageWriter (EncodingParams (fromIntegral w) (fromIntegral h) fps Nothing Nothing "" Nothing) out
  write f xs

write :: (Maybe x -> IO ()) -> [x] -> IO ()
write f xs = foldMap (f . Just) xs >> f Nothing
