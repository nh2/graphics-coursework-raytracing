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
  = [ trace (generateRay cam coord) | coord <- floatingPixelCoords ]
  where
    fi = fromIntegral
    bgCol = toRGB8 bgColF

    floatingPixelCoords = [ V2 (fi x / fi size_x)
                               (fi y / fi size_y) | x <- [0..size_x-1]
                                                  , y <- [0..size_y-1] ]

    trace ray@Line { start } = case [ (i, obj) | obj <- sceneObjs
                                               , i <- ray `intersect` obj ] of
      [] -> bgCol
      is -> colorOf $ snd $ minimumBy closestToCamera is
      where
        closestToCamera = compare `on` (qd start . fst)

    colorOf Sphere { sphereMaterial = Material rgb } = toRGB8 rgb
    toRGB8 v = round <$> 255 *^ v
