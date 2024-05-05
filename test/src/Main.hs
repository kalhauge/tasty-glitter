{-# LANGUAGE BlockArguments #-}

module Main where

import Test.Tasty
import Test.Tasty.Glitter
import Test.Tasty.Options

import Control.Monad (when)
import Data.Data (Proxy (Proxy))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (replaceExtension, takeExtension)
import System.Process.Typed

import qualified Control.GitSpec

main :: IO ()
main =
  defaultMainWithIngredients ingr (testGroup "tasty-glitter" [test, Control.GitSpec.test])
 where
  ingr =
    includingOptions [Option (Proxy :: Proxy GlitterConfig)]
      : defaultIngredients

test :: TestTree
test =
  testGroup
    "Main"
    [ testGlitter "source" ["src/", "test/src"] noop \files ->
        [ testEachChangedFile "should format" files \file -> do
            checkAndCapture $ proc "fourmolu" ["-m", "check", file]
        ]
    , testGlitter
        "to dot"
        ["expected/graphs"]
        do
          createDirectoryIfMissing True "expected/graphs"
          writeFile
            "expected/graphs/simple.dot"
            "digraph G { A -> B -> C; C -> B }"
          writeFile
            "expected/graphs/reversed.dot"
            "digraph G { C -> B -> A; B -> C }"
        \files ->
          [ testEachChangedFile "should produce pdf" files \file -> do
              when (takeExtension file == ".dot") do
                let args = ["-Tpdf", file, "-o", replaceExtension file ".pdf"]
                checkAndCapture $ proc "dot" args
          , testShouldNotHaveChangedDiff "should not have changed" files
          ]
    ]
