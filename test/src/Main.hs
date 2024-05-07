{-# LANGUAGE BlockArguments #-}

module Main where

import Test.Tasty
import Test.Tasty.Expected
import Test.Tasty.Glitter

import System.FilePath (replaceExtension, (</>))
import System.Process.Typed

-- import Test.Hspec.Expectations.Pretty

import qualified Control.GitSpec
import Data.Function
import System.IO (IOMode (..), hFlush, hPutStrLn, withFile)

main :: IO ()
main = defaultMain (testGroup "tasty-glitter" [test, Control.GitSpec.test])

test :: TestTree
test =
  testGroup
    "Main"
    [ testGroup
        "source"
        [ assayEach "should format" \file -> do
            proc "fourmolu" ["-m", "check", file]
              `shouldExitWith` ExitSuccess
        ]
        & filterGlitter (extensionIs ".hs")
    , testGroup
        "USAGE.md"
        [assayShouldBeStaged]
        & withPopulatedFile "USAGE.md" \fp -> withFile fp WriteMode \h -> do
          hPutStrLn h "This file shows how to use the tasty-glitter-test test-suite:"
          hPutStrLn h "```shell"
          hPutStrLn h "$> cabal run tasty-glitter-test -- --help"
          hFlush h
          proc "cabal" ["run", "tasty-glitter-test", "--", "--help"]
            & setStdout (useHandleOpen h)
            & runProcess_
          hPutStrLn h "```"
    , testGroup
        "dot graphs"
        [ assayEach "should produce pdf" \file -> do
            let args = ["-Tpdf", file, "-o", replaceExtension file ".pdf"]
            proc "dot" args `shouldExitWith` ExitSuccess
        , assayShouldBeStaged
        ]
        & filterGlitter (extensionIs ".dot")
        & withPopulatedDirectory "expected/graphs" ["*.pdf"] \dir -> do
          writeFile
            (dir </> "simple.dot")
            "digraph G { A -> B -> C; C -> B }"
          writeFile
            (dir </> "reversed.dot")
            "digraph G { C -> B -> A; B -> C }"
    ]
