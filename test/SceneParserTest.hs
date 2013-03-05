import           Control.Applicative
import           Control.Monad
import           Linear
import           Text.Parsec

import SceneParser


camSample, vecSample, bgSample, objSample :: String
camSample = "OrthographicCamera { \n center 10 5 10 \n direction -1 -0.5 -1 \n up 0 0.5 1 \n size 5}";
vecSample = "crap { 1.0 2.0 3}"
bgSample = "Background { \n color 1.2 0.1 0.2 \n }"

objSample =  "Group { \n num_objects 2 \n Material { diffuseColor 1 0 0 } \n Sphere { \n center 0 0 0 \n radius 1 \n } \n Material { diffuseColor 0 1 0 } \n Sphere { \ncenter 1 1 1 \n radius 0.75} }"

sceneSample = unlines [camSample, bgSample, objSample]

check parser sample res = case parse parser "" sample of
  Right x -> x == res
  Left _  -> False

test = and
  [ check parseGroup objSample $
      [Sphere {sphereCenter = V3 0.0 0.0 0.0, sphereRadius = 1.0, sphereMaterial = Material {materialColor = V3 1.0 0.0 0.0}},Sphere {sphereCenter = V3 1.0 1.0 1.0, sphereRadius = 0.75, sphereMaterial = Material {materialColor = V3 0.0 1.0 0.0}}]
  , check parseOrthographicCamera camSample $
      Camera {camCenter = V3 10.0 5.0 10.0, camDir = V3 (-1.0) (-0.5) (-1.0), camUp = V3 0.0 0.5 1.0, camSize = 5.0}
  ]

testFiles = forM_ [1..5] $ \i -> do
  let filename = "scenes/scene" ++ show i ++ ".txt"
  print =<< parseScene filename <$> readFile filename

