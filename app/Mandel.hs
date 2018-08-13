import GenImages (makeImg, Setting(..))

import Codec.Picture                (writePng)
import Data.Number.Fixed
import Options.Applicative

egPoint, egPoint2, egPoint3, egPoint4 :: (Double,Double)
egPoint =  (-0.73258263759
           ,-0.24114713638)
egPoint2 = (-1.47904448099716
           , 0.0107524846974)
egPoint3 = (-1.258489538063
           , 0.382359645113)
egPoint4 = (-0.743643887037159
           , 0.131825904205313)

data Config = Config Setting FilePath

run :: Config -> IO ()
run (Config s file) = makeImg s >>= writePng file

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Create a picture of the mandelbrot set with the complement coloured."
     <> header "mandelbrot - generate images of the mandelbrot set" )

settings :: Parser Setting
settings = Setting
          <$> option auto (short 'x' <> metavar "X-COORD")
          <*> option auto (short 'y' <> metavar "Y-COORD")
          <*> option auto (short 'l' <> metavar "WIDTH")
          <*> option auto (short 'w' <> value 2000 <> metavar "PIXEL WIDTH")
          <*> option auto (short 'n' <> value 1000 <> metavar "MAX_ITERATION")

config :: Parser Config
config = Config
        <$> settings
        <*> strOption
         (  long "output" <> short 'o'
         <> value "test.png"
         <> metavar "FILE")
