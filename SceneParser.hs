{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module SceneParser (Scene(Scene), parseScene) where

import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Data.Vec
import Control.Applicative

type GFloat = Double
type Vec3d = GFloat :. (GFloat :. (GFloat :. ()))
mkVec3d x y z = x :. y :. z :. ()
type RGBF = GFloat :. (GFloat :. (GFloat :. ()))

data MatColor = MatColor RGBF
            deriving (Show, Eq)

data Camera = Camera {camCenter :: Vec3d, camDir :: Vec3d,
                     camUp :: Vec3d, camSize :: GFloat }
            deriving (Show, Eq)

data Scene = Scene {sceneCam :: Camera, sceneBg :: RGBF,
                    sceneObjs :: [Sphere]}
            deriving (Show, Eq)

data Sphere = Sphere {sphCenter :: Vec3d, shpRadius :: GFloat,
                      sphMaterial :: MatColor}
            deriving (Show, Eq)

parseScene :: String -> Maybe Scene
parseScene txt
  =case parse (Scene <$> parseCam <*> parseBG <*> parseGroup) "" txt of
         Left _ -> Nothing
         Right s -> Just s

sceneFile :: GenParser Char st [[String]]
sceneFile = undefined

lexer = PT.makeTokenParser emptyDef
float = (char '-' *> ((negate .regardless) <$> num)) P.<|>
                       regardless  <$> num
         where
          num = PT.naturalOrFloat lexer
          regardless :: Either Integer Double -> Double
          regardless (Left x) = fromIntegral x
          regardless (Right x) = x

parse3dVec = mkVec3d <$> float <*> float <*> float

parseCurly name parseContent
  = do
   {spaces ; string name ; spaces ; char '{' ; spaces ;
    cont <- parseContent; spaces ; char '}'; spaces; return cont}

parseCam
  = parseCurly "OrthographicCamera"  ((Camera)
  <$> (string "center " *> parse3dVec)
  <*> (spaces *> string "direction "*> parse3dVec)
  <*> (spaces *> string "up " *> parse3dVec)
  <*> (spaces *> string "size " *> float))

parseBG = parseCurly "Background" (string "color " *> parse3dVec)

data GrpItem = Mat MatColor | Obj Sphere
parseGroup = parseCurly "Group"
            (do {string "num_objects "; float; spaces;
             fstMat <- parseMat; parseObjs fstMat})

parseObjs curMat
  = (do {m <- parseMat; parseObjs m})
    P.<|>(do {s <- parseSphere curMat; rest <- parseObjs curMat ; return (s : rest)})
    P.<|> do{ return []}

parseMat = parseCurly "Material" (string "diffuseColor " *> parse3dVec)
parseSphere mat = parseCurly "Sphere" (Sphere
                  <$> (string "center " *> parse3dVec)
                  <*> (string "radius " *> float))
                  <*> (pure (MatColor mat))

camSample :: String
camSample = "OrthographicCamera { \n center 10 5 10 \n direction -1 -0.5 -1 \n up 0 0.5 1 \n size 5}";
vecSample = "crap { 1.0 2.0 3}"
bgSample = "Background { \n color 1.2 0.1 0.2 \n }"

objSample =  "Group { \n num_objects 5 \n Material { diffuseColor 1 0 0 } \n Sphere { \n center 0 0 0 \n radius 1 \n } \n Material { diffuseColor 0 1 0 } \n Sphere { \ncenter 1 1 1 \n radius 0.75} }"

