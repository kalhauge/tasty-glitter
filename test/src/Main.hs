{-# LANGUAGE BlockArguments #-}

module Main where

import Test.Tasty
import Test.Tasty.Glitter

import System.Process.Typed

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ glitter "source" ["src/", "test/src"] (pure ()) \files ->
        [ testEachChangedFile "should format" files \file -> do
            checkAndCapture $ proc "fourmolu" ["-m", "check", file]
        ]
    ]
