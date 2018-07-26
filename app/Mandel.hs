import GenImages

import Codec.Picture                (writePng)
import System.Environment           (getArgs)
import Options.Applicative

egPoint, egPoint2, egPoint3 :: (Double,Double)
egPoint =  (-0.73258263759
           ,-0.24114713638)
egPoint2 = (-1.47904448099716
           , 0.0107524846974)
egPoint3 = (-1.258489538063
           , 0.382359645113)

run :: [String] -> IO ()
run [x,y,z,a,b,c] = writePng c =<< makeImageProgress (Setting (read x) (read y) (read z) (read a) (read b))
run _ = errorWithoutStackTrace "invalid inputs given"

main :: IO ()
main = getArgs >>= run
