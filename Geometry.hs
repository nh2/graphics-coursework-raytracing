{-# LANGUAGE MultiParamTypeClasses #-}

module Geometry where

import           Linear


data Sphere = Sphere {
  center :: V3 Double
, radius :: Double
} deriving (Eq, Show)


data Line = Line {
  start     :: V3 Double
, direction :: V3 Double
} deriving (Eq, Show)


type Hit = V3 Double

class Intersect a b where
  intersect :: a -> b -> [Hit]


instance Intersect Line Sphere where
  intersect (Line _o _l) (Sphere _c _r) = undefined


instance Intersect Sphere Line where
  intersect = flip intersect
