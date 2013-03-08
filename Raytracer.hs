{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains the ray-tracer logic.
module Raytracer where

import           Control.Applicative
import           Linear hiding (trace)
import           Data.Function (on)
import           Data.List (minimumBy)
import           Data.Word (Word8)

import           Camera
import           Geometry
import           Scene


-- | A color pixel, 8-bit per color.
type RGB8 = V3 Word8


-- | Ray-traces a scene into a list of RGB pixels.
raytrace :: Scene -> (Int, Int) -> [RGB8]
raytrace Scene { sceneCam = cam
               , sceneBg = Background bgColF
               , sceneObjs } (size_x, size_y)
  = [ trace (generateRay cam coord) | coord <- floatingPixelCoords size_x size_y ]
  where
    trace ray@Line { start } = case [ (i, obj) | obj <- sceneObjs
                                               , i <- ray `intersect` obj ] of
      [] -> bgCol
      is -> colorOf $ snd $ minimumBy closestToCamera is
      where
        closestToCamera = compare `on` (qd start . fst)

    colorOf Sphere { sphereMaterial = Material rgb } = toRGB8 rgb
    toRGB8 v = round <$> 255 *^ v
    bgCol = toRGB8 bgColF


-- | Ray-traces a scene into a list of greyscale values (1 per pixel).
raytraceDepth :: Scene -> (Int, Int) -> (Int, Int) -> [Double]
raytraceDepth Scene { sceneCam = cam
                    , sceneObjs } (size_x, size_y) (nearDepth, farDepth)
  = [ trace (generateRay cam coord) | coord <- floatingPixelCoords size_x size_y ]
  where
    trace ray@Line { start } = case [ d | obj <- sceneObjs
                                        , i <- ray `intersect` obj
                                        , let d = distance start i
                                        , d < fi farDepth
                                        ] of
      [] -> 0.0
      ds -> clipNear $ minimum ds

    -- Clip everything that is closer than the near plane to 1.0.
    clipNear :: Double -> Double
    clipNear x
      | x < fi nearDepth = 1.0
      | otherwise       = 1.0 - (x - fi nearDepth) / depthRange

    depthRange = fi $ farDepth - nearDepth



-- Count down y as camera coordinates start from bottom left.
floatingPixelCoords :: Int -> Int -> [V2 Double]
floatingPixelCoords size_x size_y =
  [ V2 (fi x / fi size_x)
       (fi y / fi size_y) | y <- [size_y-1, size_y-2 .. 0]
                          , x <- [0 .. size_x-1] ]


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
