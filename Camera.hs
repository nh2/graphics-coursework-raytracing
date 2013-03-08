{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}

-- | This module describes how to generate rays based on the camera type.
module Camera where

import Geometry
import Linear
import Scene


class IsCamera c where
  -- | Based on the camera parameters, generate a ray starting at a given
  -- pixel location.
  -- The pixel coordinates should be in the range (0, 0) -> (1, 1) where
  -- these points represent the corners of the image.
  generateRay :: c -> V2 Double -> Line


-- | How to generate rays for an orthographic camera.
instance IsCamera OrthographicCamera where
  generateRay cam (V2 _x _y) = undefined -- TODO implement
