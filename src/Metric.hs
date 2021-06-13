
module Metric where

import Control.Lens

import Data.Default
import Data.Map qualified as Map
import Data.Map (Map)

import Point3D

class Metric s where
  elemAt :: Point Int -> Lens' (s c) (Maybe c)

data Volume c = Volume
  { _raw  :: Map (Point Int) c
  }

makeLenses ''Volume

instance Metric Volume where
  elemAt p = raw . at p