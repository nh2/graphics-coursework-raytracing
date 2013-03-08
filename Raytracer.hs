{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains the ray-tracer logic.
module Raytracer where

import           Control.Applicative
import           Linear
import           Data.Word (Word8)

import           Camera
import           Geometry
import           Scene


-- | A color pixel, 8-bit per color.
type RGB8 = V3 Word8


-- | Ray-traces a scene into a list of RGB pixels.
raytrace :: Scene -> (Int, Int) -> [RGB8]
raytrace _scene (_width, _height) = undefined -- TODO implement
