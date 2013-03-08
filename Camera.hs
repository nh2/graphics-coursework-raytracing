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
  generateRay OrthographicCamera { center = c, direction, upAxis, size }
              (V2 x y)
    = Line { start     = c + (x - 0.5) * size *^ horizAxis + (y - 0.5) * size *^ upAxis'
           , direction = normalize direction }
    where
      horizAxis = normalize $ direction `cross` upAxis -- horizontal axis in the camera plane
      upAxis' = normalize $ horizAxis `cross` direction -- corrected up axis
