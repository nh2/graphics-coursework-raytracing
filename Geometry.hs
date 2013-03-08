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
  intersect (Line _o _l) (Sphere _c _r _) = undefined -- TODO implement


instance Intersect Sphere Line where
  intersect = flip intersect
