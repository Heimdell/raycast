
import Pile
import Point3D

import Data.List (foldl')

main = print $ foldl' (*) one $ map (\a -> rotate (rad a) (Point 1 2 a)) [1.. 10000000]
