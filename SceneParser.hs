{-# LANGUAGE FlexibleContexts #-}

module SceneParser (
  Scene (..)
, Material (..)
, Camera (..)
, Sphere (..)
, parseScene
-- * Internals
, parseGroup
, parseOrthographicCamera
) where

import           Control.Applicative
import           Control.Monad
import           Linear
import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Number (sign, decimal, floating2)


type Vec3d = V3 Double
type RGB   = V3 Double

newtype Background = Background RGB
                   deriving (Eq, Show)

data Material = Material { materialColor :: RGB
                         } deriving (Eq, Show)

data Camera = Camera { camCenter :: Vec3d
                     , camDir    :: Vec3d
                     , camUp     :: Vec3d
                     , camSize   :: Double
                     } deriving (Eq, Show)

data Sphere = Sphere { sphereCenter   :: Vec3d
                     , sphereRadius   :: Double
                     , sphereMaterial :: Material
                     } deriving (Eq, Show)

data Scene = Scene { sceneCam  :: Camera
                   , sceneBg   :: Background
                   , sceneObjs :: [Sphere]
                   } deriving (Eq, Show)


parseScene :: FilePath -> String -> Either String Scene
parseScene filename txt = either (Left . show) Right (parse sceneParser filename txt)


float :: Parser Double
float = sign <*> floating2 True


ws :: Parser ()
ws = spaces


parseVec3 :: Parser (V3 Double)
parseVec3 = V3 <$> (float <* ws) <*> (float <* ws) <*> float


parseCurly :: String -> Parser a -> Parser a
parseCurly name parseContent =
  string name *> ws *> char '{' *> ws *> parseContent <* ws <* char '}'


parseOrthographicCamera :: Parser Camera
parseOrthographicCamera = parseCurly "OrthographicCamera" $
  Camera <$> (string "center"    *> ws *> parseVec3 <* ws)
         <*> (string "direction" *> ws *> parseVec3 <* ws)
         <*> (string "up"        *> ws *> parseVec3 <* ws)
         <*> (string "size"      *> ws *> float          )

parseBackground :: Parser Background
parseBackground = parseCurly "Background" $
  Background <$> (string "color" *> ws *> parseVec3)


parseMaterial :: Parser Material
parseMaterial = parseCurly "Material" $
  Material <$> (string "diffuseColor" *> ws *> parseVec3)


parseSphere :: Material -> Parser Sphere
parseSphere mat = parseCurly "Sphere" $
  Sphere <$> (string "center" *> ws *> parseVec3 <* ws)
         <*> (string "radius" *> ws *> float          )
         <*> pure mat


parseGroup :: Parser [Sphere]
parseGroup = parseCurly "Group" $ do
  string "num_objects" >> ws
  n <- decimal
  ws
  spheres <- concat <$> ((`sepEndBy1` ws) $ do
    mat <- parseMaterial
    ws
    parseSphere mat `sepEndBy1` ws)

  when (length spheres /= n) $ fail $
    "declared 'num_objects " ++ show n ++ "', but got " ++ show (length spheres) ++ " objects"

  return spheres


sceneParser :: Parser Scene
sceneParser = Scene <$> (ws *> parseOrthographicCamera <* ws)
                    <*> (      parseBackground         <* ws)
                    <*> (      parseGroup              <* ws)
