
module Point3D where

import Data.List (unfoldr)

data Point a = Point a a a
  deriving stock (Eq, Ord, Show)

scalar :: (a -> a -> a) -> Point a -> Point a -> Point a
scalar (?) (Point a b c) (Point x y z) = Point (a ? x) (b ? y) (c ? z)

instance Num a => Num (Point a) where
  (+) = scalar (+)
  (-) = scalar (-)

  (*)         = error "Point.(*): what?"
  abs         = error "Point.abs: what?"
  signum      = error "Point.signum: what?"
  fromInteger = error "Point.fromIntegral: what?"

raycast :: Point Int -> Point Float -> [Point Int]
raycast start = scanl (+) start . ray

ray :: Point Float -> [Point Int]
ray pt = filter nonZero $ unfoldr (Just . step) (Point 0 0 0)
  where
    dv = normalize pt

    step acc =
      ( Point (truncate x) (truncate y) (truncate z)
      , Point (wrap     x) (wrap     y) (wrap     z)
      )
      where
        Point x y z = dv + acc

    nonZero (Point 0 0 0) = False
    nonZero  _            = True

    wrap x
      | x >   1   = x - 1
      | x < (-1)  = x + 1
      | otherwise = x

normalize :: Point Float -> Point Float
normalize pt@(Point x y z) = Point (x / len) (y / len) (z / len)
  where
    len = vlen pt

vlen :: Point Float -> Float
vlen (Point x y z) = sqrt (x * x + y * y + z * z)
