
module Pile where

import Data.Ord (comparing)
import Data.List (maximumBy, intercalate)

import Data.Vector.Primitive qualified as V

import Point3D

data Angle = A Float
  deriving stock (Eq, Ord, Show)

newtype Matrix = Matrix { mRaw :: V.Vector Float }
  deriving stock (Eq, Ord)

instance Show Matrix where
  show (Matrix v'') =
    let
      v       = V.toList v''
      v'      = fmap approx v
      strs    = fmap show v'
      longest = maximumBy (comparing length) strs
      strs'   = fmap (lpad $ length longest) strs
    in
      unlines $ flip map [0.. 3] \i ->
        ("( " <>) . (<> ")") $ intercalate "| " $ flip map [0.. 3] \j ->
          strs' !! (i * 4 + j) <> " "

    where
      lpad n s = replicate (max 0 (n - length s)) ' ' <> s

      approx f = float (int (1000 * f)) / 1000

instance Num Matrix where
  Matrix a + Matrix b = Matrix $ V.zipWith (+) a b
  (*)         = mult
  negate      = error "Matrix.negate: what?"
  abs         = error "Matrix.abs: what?"
  signum      = error "Matrix.signum: what?"
  fromInteger = scale' . fromIntegral

instance Fractional Matrix where
  recip = inverse
  fromRational = scale' . fromRational

make :: (Int -> Int -> Float) -> Matrix
make gen = Matrix $ V.generate 16 \i -> gen (div i 4) (mod i 4)

matrix :: [Float] -> Matrix
matrix = Matrix . V.fromList

one :: Matrix
one = scale' 1

move :: Point Float -> Matrix
move (Point x y z) = matrix
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 1, 0
  , x, y, z, 1
  ]

scale :: Point Float -> Matrix
scale (Point x y z) = matrix
  [ x, 0, 0, 0
  , 0, y, 0, 0
  , 0, 0, z, 0
  , 0, 0, 0, 1
  ]

scale' :: Float -> Matrix
scale' x = matrix
  [ x, 0, 0, 0
  , 0, x, 0, 0
  , 0, 0, x, 0
  , 0, 0, 0, 1
  ]

rotate :: Angle -> Point Float -> Matrix
rotate (A angle) pt = matrix
  [ a00, a01, a02, 0
  , a10, a11, a12, 0
  , a20, a21, a22, 0
  , 0,   0,   0,   1
  ]
  where
    Point x y z = normalize pt

    c  = cos angle
    s  = sin angle
    c1 = 1 - c

    xs = x * s
    ys = y * s
    zs = z * s

    xyc1 = x * y * c1
    yzc1 = y * z * c1
    zxc1 = z * x * c1

    a00 = x * x * c1 + c
    a11 = y * y * c1 + c
    a22 = z * z * c1 + c

    a01 = xyc1 + zs
    a20 = zxc1 + ys
    a12 = yzc1 + xs

    a02 = zxc1 - ys
    a10 = xyc1 - zs
    a21 = yzc1 - xs

rad :: Float -> Angle
rad angle = A $ angle / 180 * pi

{-# INLINE (!) #-}
(!) :: Matrix -> (Int, Int) -> Float
Matrix v ! (i, j) = v V.! (i * 4 + j)

float :: Int -> Float
float = fromIntegral

int :: Float -> Int
int = round

vec :: V.Prim a => [a] -> V.Vector a
vec = V.fromList

row :: Int -> Matrix -> V.Vector Float
row i m = V.generate 4 \j -> m ! (i, j)

column :: Int -> Matrix -> V.Vector Float
column j m = V.generate 4 \i -> m ! (i, j)

transpose :: Matrix -> Matrix
transpose m = make \i j -> m ! (j, i)

dot :: V.Vector Float -> V.Vector Float -> Float
dot = (V.sum .) . V.zipWith (*)

apply :: Matrix -> V.Vector Float -> V.Vector Float
apply m v =
  let
    m' = transpose m
  in
    flip V.map (vec [0.. 3]) \i -> dot v (row i m')

mult :: Matrix -> Matrix -> Matrix
mult a b =
  make \i j -> 0
    + m V.! (16 * i + 4 * 0 + j)
    + m V.! (16 * i + 4 * 1 + j)
    + m V.! (16 * i + 4 * 2 + j)
    + m V.! (16 * i + 4 * 3 + j)

  where
    m = V.generate 64 \index ->
      let
        i = div index 16
        k = div index 4 `mod` 4
        j = mod index 4
      in
        a ! (i, k) * b ! (k, j)

data View = View
  { left, right, bottom, top, near, far :: Float
  }

frustum :: View -> Matrix
frustum (View left right bottom top znear zfar) =
  let
    x = 2 * znear / (right - left)
    y = 2 * znear / (top   - bottom)

    a = (right + left)   / (right - left)
    b = (top   + bottom) / (top   - bottom)

    c = -(zfar + znear)   / (zfar - znear)
    d = -2 * zfar * znear / (zfar - znear)
  in matrix
    [ x, 0, 0,  0
    , 0, y, 0,  0
    , a, b, c, -1
    , 0, 0, d,  0
    ]

ortho :: View -> Matrix
ortho (View l r b t n f) =
  let
    ai = 2/rsl
    bi = 2/tsb
    ci = -2/fsn
    di = -(r+l)/rsl
    ei = -(t+b)/tsb
    fi = -(f+n)/fsn
  in matrix
    [ ai, 0,  0,  0
    , 0,  bi, 0,  0
    , 0,  0,  ci, 0
    , di, ei, fi, 1
    ]
  where
    rsl = r - l
    tsb = t - b
    fsn = f - n

inverse :: Matrix -> Matrix
inverse (Matrix m) =
  matrix
    [ m00, m01, m02, m03
    , m10, m11, m12, m13
    , m20, m21, m22, m23
    , m30, m31, m32, m33
    ]
  where
    [  i00, i01, i02, i03
     , i10, i11, i12, i13
     , i20, i21, i22, i23
     , i30, i31, i32, i33
     ] = V.toList m

    s0 = i00 * i11 - i10 * i01
    s1 = i00 * i12 - i10 * i02
    s2 = i00 * i13 - i10 * i03
    s3 = i01 * i12 - i11 * i02
    s4 = i01 * i13 - i11 * i03
    s5 = i02 * i13 - i12 * i03

    c5 = i22 * i33 - i32 * i23
    c4 = i21 * i33 - i31 * i23
    c3 = i21 * i32 - i31 * i22
    c2 = i20 * i33 - i30 * i23
    c1 = i20 * i32 - i30 * i22
    c0 = i20 * i31 - i30 * i21

    invdet = 1 / (s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0)

    m00 = ( i11 * c5 - i12 * c4 + i13 * c3) * invdet
    m01 = (-i01 * c5 + i02 * c4 - i03 * c3) * invdet
    m02 = ( i31 * s5 - i32 * s4 + i33 * s3) * invdet
    m03 = (-i21 * s5 + i22 * s4 - i23 * s3) * invdet

    m10 = (-i10 * c5 + i12 * c2 - i13 * c1) * invdet
    m11 = ( i00 * c5 - i02 * c2 + i03 * c1) * invdet
    m12 = (-i30 * s5 + i32 * s2 - i33 * s1) * invdet
    m13 = ( i20 * s5 - i22 * s2 + i23 * s1) * invdet

    m20 = ( i10 * c4 - i11 * c2 + i13 * c0) * invdet
    m21 = (-i00 * c4 + i01 * c2 - i03 * c0) * invdet
    m22 = ( i30 * s4 - i31 * s2 + i33 * s0) * invdet
    m23 = (-i20 * s4 + i21 * s2 - i23 * s0) * invdet

    m30 = (-i10 * c3 + i11 * c1 - i12 * c0) * invdet
    m31 = ( i00 * c3 - i01 * c1 + i02 * c0) * invdet
    m32 = (-i30 * s3 + i31 * s1 - i32 * s0) * invdet
    m33 = ( i20 * s3 - i21 * s1 + i22 * s0) * invdet
