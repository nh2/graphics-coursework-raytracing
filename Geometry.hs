{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module deals with intersections between geometric objects,
-- for example line-sphere-intersections.
module Geometry where

import           Linear

import           Scene


-- | A line in 3D space with a start point and a direction vector.
data Line = Line {
  start     :: V3 Double
, direction :: V3 Double
} deriving (Eq, Show)


-- | An intersection point.
type Hit = V3 Double


class Intersect a b where
  -- | Intersect two objects a and b.
  -- Returns the list of intersections points in 3D space.
  intersect :: a -> b -> [Hit]


-- How to intersect lines with spheres.
instance Intersect Line Sphere where
  -- From http://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
  intersect (Line o l) (Sphere c r _) | s < 0      = []
                                      | nearZero s = [head hits]
                                      | otherwise  = hits
    where
      ds   = (-l `dot` (o - c)) +- sqrt s
      s    = (l `dot` (o - c))^two - quadrance (o - c) + r^two
      hits = [ o + d *^ l | d <- ds ]

      two = 2 :: Int
      a +- b = [a + b, a - b]


instance Intersect Sphere Line where
  intersect = flip intersect
