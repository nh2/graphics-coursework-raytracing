module Main where

import           Control.Applicative
import           Control.Monad
import           Test.Hspec
import           Test.HUnit

import SceneParser


assertRight :: Either String a -> Assertion
assertRight = either assertFailure (const $ return ())


main :: IO ()
main = hspec $ do

  describe "Scene parser" $ do

    describe "parses all given scene files" $ do

      forM_ [1..5] $ \i -> do
        let filename = "scenes/scene" ++ show i ++ ".txt"

        it filename $ do
          assertRight =<< parseScene filename <$> readFile filename
