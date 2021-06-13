
import Control.Lens

import Data.List (foldl')
import Data.Vector.Primitive qualified as V
import Data.Map qualified as Map

import Graphics.Gloss.Raster.Field hiding (Point)

import Matrix
import Metric
import Point3D

main = doMain (640, 480) w 60 (Point 25 25 25)
  where
    w = Volume $ Map.fromList
      [ (Point x y z, rgb 0 0 1)
      | x <- [0.. 10]
      , y <- [0.. 10]
      , z <- [0.. 10]
      ]


doMain (w, h) s fov c = do
  animateField
    (InWindow "cursed sands" (w, h) (100, 100))
    (1, 1)
    \t (dx, dy)
      -> traceRay 0 s
      $  take 50
      $  raycast c
      $  rotate2 dx dy
          `apply2pt` Point (-1) (-1) (-1)
  where
    rotate2 a b
      = rotate (rad (fov * a)) (Point 0 1 0)
      * rotate (rad (fov * (float h / float w) * b)) (Point 0 0 1)

apply2pt :: Matrix -> Point Float -> Point Float
apply2pt m (Point x y z) =
  let [x', y', z', _] = V.toList $ apply m $ vec [x, y, z, 1]
  in Point x' y' z'

traceRay :: Metric s => Int -> s Color -> [Point Int] -> Color
traceRay z _ []         = rgb 0 0 0
traceRay z s (pt : pts) = case s^.elemAt pt of
  Just c  -> c
  Nothing -> traceRay z s pts