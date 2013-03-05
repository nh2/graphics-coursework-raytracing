module Main where

import           Control.Applicative as Args
import           Options.Applicative

import SceneParser
import Geometry


-- * Command line argument parsing

data Args = Args {
  input  :: FilePath
, size   :: (Int, Int)
, output :: FilePath
, depth  :: ((Int, Int), FilePath)
} deriving (Eq, Show)


argsParser :: Parser Args
argsParser = Args <$> strOption (long "input" <> metavar "SCENE_DESCRIPTION_FILE")
                  <*> ((,) <$> option (long "size" <> metavar "WIDTH")
                           <*> argument auto (metavar "HEIGHT"))
                  <*> strOption (long "output" <> metavar "TEXTURE_OUTPUT_FILE")
                  <*> ((,) <$> ((,) <$> option (long "depth" <> metavar "MIN")
                                    <*> argument auto (metavar "MAX"))
                           <*> argument auto (metavar "DEPTH_OUTPUT_FILE"))


main :: IO ()
main = do

  -- Parse arguments
  args <- execParser (info (helper <*> argsParser) fullDesc)

  print args


-- raycast -input scene4.txt -size 200 200 -output scene4.ppm -depth 13 16 depth4.ppm
