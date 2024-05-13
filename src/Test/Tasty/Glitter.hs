{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Glitter (
  -- * GlitterFile
  GlitterFile,

  -- * Assaying
  assay,
  assayEach,
  assayShouldBeStaged,

  -- * Modifiers
  filterGlitter,
  withPopulatedFile,
  withPopulatedDirectory,

  -- * Advanced Modifiers
  modifyGlitterWithCleanup,

  -- * Option
  GlitterConfig (..),

  -- * Helpers
  extensionIs,
) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers

import Test.Tasty.Expected

import Control.Exception
import Control.Git (gitChanges, gitDiffFiles, gitLsFiles)
import Control.Monad
import Data.Data (Proxy (..), Typeable)
import Data.Tagged
import GHC.Stack (HasCallStack)
import System.Directory
import System.FilePath
import Test.HUnit (assertFailure)
import qualified Test.HUnit.Lang as HUnit

{- | A glitter file, is a file that exist and potentially should
be persisted in the repository, if it passes all it's tests.
-}
type GlitterFile = FilePath

data GlitterConfig = GlitterConfig
  { glitterfiles :: !(IO [GlitterFile])
  -- ^ The list files, that glitter, but might not be gold.
  , sifter :: !([FilePath] -> IO [GlitterFile])
  -- ^ The detector that given a list of folders and files,
  -- findes a list of files that glitters.
  }

instance IsOption GlitterConfig where
  defaultValue = GlitterConfig (gitChanges []) (remainEmpty gitChanges)
  optionName = pure "changed-files"
  optionHelp = pure "choose the strategy for finding changed files."
  showDefaultValue = const (Just "unstaged")
  parseValue = \case
    "unstaged" -> Just defaultValue
    "all" -> Just (GlitterConfig (gitLsFiles []) (remainEmpty gitLsFiles))
    _ -> Nothing

remainEmpty :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [FilePath]
remainEmpty _ [] = pure []
remainEmpty fn fs = fn fs

-- | A test that sees if a list of interesting files are good.
data GlitterTest t = (IsTest t) =>
  GlitterTest
  { fileCheck :: [FilePath] -> t
  }
  deriving (Typeable)

instance forall t. (IsTest t) => IsTest (GlitterTest t) where
  testOptions =
    pure
      ( Option (Proxy :: Proxy GlitterConfig)
          : unTagged (testOptions @t)
      )
  run options GlitterTest{fileCheck} prog = do
    try (glitterfiles (lookupOption options)) >>= \case
      Right fp ->
        run options (fileCheck fp) prog
      Left (HUnit.HUnitFailure mbloc message) ->
        pure $ testFailed (prependLocation mbloc (HUnit.formatFailureReason message))

-- | Determine if glitter files are gold.
assay :: TestName -> ([FilePath] -> Expectation) -> TestTree
assay name t = singleTest name (GlitterTest (Expected . t))

{- | Determine if the glitter files are gold, one by one.
Uses the @ExpectedSteps@ function also display which file is worked
on now.
-}
assayEach :: TestName -> (FilePath -> Expectation) -> TestTree
assayEach name test =
  singleTest name . GlitterTest $ \files -> ExpectedSteps \step -> do
    forM_ files \file -> do
      step file
      test file

{- | Make sure that all glitter files are staged.
This is usefull if the test-suite makes changes to the files.
-}
assayShouldBeStaged :: (HasCallStack) => TestTree
assayShouldBeStaged = assay "should have been staged" \files -> do
  case files of
    [] -> pure ()
    _otherwise ->
      gitDiffFiles files >>= \case
        Nothing -> do
          changes <- gitChanges files
          unless (null changes) do
            assertFailure ("Following files are untracked:\n" <> unlines changes)
        Just err -> assertFailure err

-- | The base glitter combinator.
modifyGlitterWithCleanup
  :: ([GlitterFile] -> IO [FilePath])
  -- ^ An action that given a current the changed files,
  -- does an action on the repoistory and return a list of files
  -- and folders to check for
  -> ([GlitterFile] -> [FilePath] -> IO ())
  -- ^ A cleanup operation, to revert possible changes.
  -- First argument is the change files outside and the second is the files inside this combinator.
  -> TestTree
  -- ^ The test tree, to update the changed files of.
  -> TestTree
modifyGlitterWithCleanup action clean tree =
  askOption \cfg@(GlitterConfig{glitterfiles, sifter}) ->
    withResource
      (glitterfiles >>= (action >=> sifter))
      (\fp -> do fp' <- glitterfiles; clean fp' fp)
      \files -> localOption (cfg{glitterfiles = files}) tree

-- | Filter out any file that should not be checked.
filterGlitter
  :: (GlitterFile -> Bool) -> TestTree -> TestTree
filterGlitter fn =
  adjustOption
    ( \cfg@(GlitterConfig{glitterfiles}) ->
        cfg{glitterfiles = filter fn <$> glitterfiles}
    )

{- | Check if the glitter file has the following extension:
>>> extensionIs ".jpg" "example.jpg"
True
-}
extensionIs :: String -> GlitterFile -> Bool
extensionIs ext fp = takeExtension fp == ext

-- | Populate a directory with an action, and search the directory for changed files.
withPopulatedDirectory :: FilePath -> [String] -> (FilePath -> IO ()) -> TestTree -> TestTree
withPopulatedDirectory dir ignore action =
  modifyGlitterWithCleanup
    ( \_ -> do
        createDirectoryIfMissing True dir
        unless (null ignore) do
          writeFile (dir </> ".gitignore") (unlines ignore)
        action dir
        pure [dir]
    )
    (const . const $ pure ())

{- | Populate a file with an action and change the glitter files to be this file,
if it has changed.
-}
withPopulatedFile :: FilePath -> (FilePath -> IO ()) -> TestTree -> TestTree
withPopulatedFile fp action =
  modifyGlitterWithCleanup
    ( \_ -> do
        createDirectoryIfMissing True (takeDirectory fp)
        action fp
        pure [fp]
    )
    (const . const $ pure ())
