{-# LANGUAGE BlockArguments #-}

module Control.GitSpec (test) where

import Control.Git (gitChanges)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Directory.Internal.Prelude (tryIOError)
import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test =
  testGroup
    "Control.Git"
    [ testCase "find unadded folder" do
        _ <- tryIOError (removeDirectoryRecursive "test/git/unadded")
        createDirectoryIfMissing True "test/git/unadded"
        writeFile "test/git/unadded/test1.txt" "test1"
        writeFile "test/git/unadded/test2.txt" "test2"
        changes <- gitChanges ["test/git/unadded"]
        changes
          @?= [ "test/git/unadded/test1.txt"
              , "test/git/unadded/test2.txt"
              ]
        removeDirectoryRecursive "test/git/unadded"
    , testCase "changes in added file" do
        createDirectoryIfMissing True "test/git/"

        writeFile "test/git/added.txt" "same as always"
        changes <- gitChanges ["test/git/added.txt"]
        changes @?= []

        writeFile "test/git/added.txt" "a change"
        changes2 <- gitChanges ["test/git/added.txt"]
        changes2 @?= ["test/git/added.txt"]

        writeFile "test/git/added.txt" "same as always"
    ]
