module Camera where

import Geometry
import Linear


class Camera c where
  -- | Based on the camera parameters, generate a ray starting at a given
  -- pixel location.
  -- The pixel coordinates should be in the range (0, 0) -> (1, 1) where
  -- these points represent the corners of the image.
  generateRay :: c -> V2 Double -> Line


data OrthographicCamera = OrthographicCamera {
  center    :: V3 Double
, direction :: V3 Double
, upAxis    :: V3 Double
, size      :: Double
} deriving (Eq, Show)


instance Camera OrthographicCamera where
  generateRay _cam (V2 _x _y) = undefined
