{-# LANGUAGE BlockArguments #-}

module Control.GitSpec (test) where

import Control.Git (gitChanges)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Directory.Internal.Prelude (tryIOError)
import Test.Hspec.Expectations.Pretty
import Test.Tasty
import Test.Tasty.Expected

test :: TestTree
test =
  testGroup
    "Control.Git"
    [ it "find unadded folder" do
        _ <- tryIOError (removeDirectoryRecursive "test/git/unadded")
        createDirectoryIfMissing True "test/git/unadded"
        writeFile "test/git/unadded/test1.txt" "test1"
        writeFile "test/git/unadded/test2.txt" "test2"
        gitChanges ["test/git/unadded"]
          `shouldReturn` [ "test/git/unadded/test1.txt"
                         , "test/git/unadded/test2.txt"
                         ]
        removeDirectoryRecursive "test/git/unadded"
    , it "changes in added file" do
        createDirectoryIfMissing True "test/git/"

        writeFile "test/git/added.txt" "same as always"
        gitChanges ["test/git/added.txt"]
          `shouldReturn` []

        writeFile "test/git/added.txt" "a change"
        gitChanges ["test/git/added.txt"]
          `shouldReturn` ["test/git/added.txt"]

        writeFile "test/git/added.txt" "same as always"
    ]
