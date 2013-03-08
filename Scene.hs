-- | Contains all important objects of a scene:
--
-- * Sphere
-- * Material
-- * Camera
-- * Scene object containing all above
--
-- Can be parsed from a scenen file with the "SceneParser" module.
module Scene where

import           Linear


data Scene = Scene
  { sceneCam  :: OrthographicCamera
  , sceneBg   :: Background
  , sceneObjs :: [Sphere]
  } deriving (Eq, Show)

data OrthographicCamera = OrthographicCamera
  { center    :: V3 Double
  , direction :: V3 Double
  , upAxis    :: V3 Double
  , size      :: Double
  } deriving (Eq, Show)

newtype Background = Background RGB
                   deriving (Eq, Show)

data Sphere = Sphere
  { sphereCenter   :: V3 Double
  , sphereRadius   :: Double
  , sphereMaterial :: Material
  } deriving (Eq, Show)

data Material = Material
  { materialColor :: RGB
  } deriving (Eq, Show)

-- | A floating-point color value from a scene file.
type RGB = V3 Double
