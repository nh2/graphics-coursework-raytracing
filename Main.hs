{-# LANGUAGE NamedFieldPuns #-}

-- | The main module of the raytracer program.
--
-- Parses command line arguments and scene files,
-- then invokes the raytracer and writes the result into a PPM file.
module Main where

import           Codec.PPM.Binary (writePPM)
import           Control.Applicative
import           Linear
import           Options.Applicative

import Raytracer
import SceneParser


-- * Command line argument parsing

-- | Data type repesenting command line arguments.
data Args = Args {
  scenePath :: FilePath
, size      :: (Int, Int)
, output    :: FilePath
, depth     :: ((Int, Int), FilePath)
} deriving (Eq, Show)


-- | The command line argument parser. Example:
--
-- >raycast -input scene1.txt -size 200 200 -output scene1.ppm -depth 13 16 depth1.ppm
argsParser :: Parser Args
argsParser = Args <$> strOption (long "input" <> metavar "SCENE_DESCRIPTION_FILE")
                  <*> ((,) <$> option (long "size" <> metavar "WIDTH")
                           <*> argument auto (metavar "HEIGHT"))
                  <*> strOption (long "output" <> metavar "TEXTURE_OUTPUT_FILE")
                  <*> ((,) <$> ((,) <$> option (long "depth" <> metavar "MIN")
                                    <*> argument auto (metavar "MAX"))
                           <*> argument str (metavar "DEPTH_OUTPUT_FILE"))


-- | The entry point to the raytracer program.
main :: IO ()
main = do

  -- Parse arguments
  Args { scenePath
       , size = (sx, sy)
       , output
       , depth = ((near, far), depthPath)
       } <- execParser (info (helper <*> argsParser) fullDesc)

  -- Parse scene
  parsedScene <- parseScene scenePath <$> readFile scenePath

  let outputSize = (fromIntegral sx, fromIntegral sy)

  case parsedScene of
    Left e      -> putStrLn e -- scene parsing unsuccessful
    Right scene -> do
      -- Raytrace image
      let pixels = raytrace scene (sx, sy)

      -- Write image to PPM file
      writePPM output outputSize [ (r,g,b) | V3 r g b <- pixels ]

      -- Calculate depth image
      let depth = raytraceDepth scene (sx, sy) (near, far)
          -- Scale depth float into 0-255 range
          s d   = fromIntegral (min 255 (floor (d * 255)) :: Int)

      -- Write depth image
      writePPM depthPath outputSize [ (s d, s d, s d) | d <- depth ]
