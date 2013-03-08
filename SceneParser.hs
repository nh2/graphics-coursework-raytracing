{-# LANGUAGE FlexibleContexts #-}

-- | A parser for scene configuration files.
--
-- Use `parseScene` to parse a scene from a file.
module SceneParser (
  parseScene
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

import           Scene


-- | Parses a scene file into a @Scene@ datatype.
--
-- Takes a filename (for nice parser error messages) and the
-- file contents to parse.
--
-- Returns @Left errorMessage@ on failure.
parseScene :: FilePath -> String -> Either String Scene
parseScene filename txt = either (Left . show) Right (parse sceneParser filename txt)


-- | Parses a signed floating point number.
float :: Parser Double
float = sign <*> floating2 True


-- | Eats whitespace.
ws :: Parser ()
ws = spaces


-- | Parses a vector of 3 floating point numbers.
parseVec3 :: Parser (V3 Double)
parseVec3 = V3 <$> (float <* ws) <*> (float <* ws) <*> float


-- | Parses a section with a section name and contents between
-- curly braces.
parseCurly :: String -> Parser a -> Parser a
parseCurly name parseContent =
  string name *> ws *> char '{' *> ws *> parseContent <* ws <* char '}'


-- | Parses an orthographic camera.
parseOrthographicCamera :: Parser OrthographicCamera
parseOrthographicCamera = parseCurly "OrthographicCamera" $
  OrthographicCamera <$> (string "center"    *> ws *> parseVec3 <* ws)
                     <*> (string "direction" *> ws *> parseVec3 <* ws)
                     <*> (string "up"        *> ws *> parseVec3 <* ws)
                     <*> (string "size"      *> ws *> float          )

-- | Parses a background color.
parseBackground :: Parser Background
parseBackground = parseCurly "Background" $
  Background <$> (string "color" *> ws *> parseVec3)


-- | Parses a material (color).
parseMaterial :: Parser Material
parseMaterial = parseCurly "Material" $
  Material <$> (string "diffuseColor" *> ws *> parseVec3)


-- | Parses a sphere.
parseSphere :: Material -> Parser Sphere
parseSphere mat = parseCurly "Sphere" $
  Sphere <$> (string "center" *> ws *> parseVec3 <* ws)
         <*> (string "radius" *> ws *> float          )
         <*> pure mat


-- | Parses a group of spheres and their materials.
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


-- Parses a full scene consisting of:
--
-- * a camera
-- * a background
-- * a group consisting of spheres and their materials
sceneParser :: Parser Scene
sceneParser = Scene <$> (ws *> parseOrthographicCamera <* ws)
                    <*> (      parseBackground         <* ws)
                    <*> (      parseGroup              <* ws)
