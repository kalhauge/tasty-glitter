{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Glitter (
  testGlitter,

  -- * Option
  GlitterConfig,

  -- * Utils
  testEachChangedFile,
  testShouldNotHaveChangedDiff,
  noop,

  -- ** Subprocesses
  check,
  checkAndCapture,
) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers

import Test.Tasty.HUnit

import Control.Git (gitChanges, gitDiffFiles, gitLsFiles)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import System.Exit
import System.Process.Typed

newtype GlitterConfig = GlitterConfig
  { detector :: [FilePath] -> IO [FilePath]
  }

instance IsOption GlitterConfig where
  defaultValue = GlitterConfig gitChanges
  optionName = pure "changed-files"
  optionHelp = pure "choose the strategy for finding changed files."
  showDefaultValue = const (Just "unstaged")
  parseValue = \case
    "unstaged" -> Just defaultValue
    "all" -> Just (GlitterConfig gitLsFiles)
    _ -> Nothing

newtype GlitterTest = GlitterTest
  { files :: IO [FilePath]
  }

instance IsTest GlitterTest where
  testOptions = pure []
  run _ GlitterTest{files} _ = do
    fs <- files
    case fs of
      [] -> pure $ testPassed ""
      fs' -> pure $ testFailed (unlines fs')

-- | A glitter test.
testGlitter
  :: TestName
  -- ^ The name of the test
  -> [FilePath]
  -- ^ The files or folders to check for changes
  -> IO a
  -- ^ An action that generate or changes the files or folders (or does nothing)
  -> (ChangedFiles -> [TestTree])
  -- ^ Given an resource of changed tests, create a test-tree.
  -> TestTree
testGlitter name fp gen tests =
  askOption \(GlitterConfig{detector}) ->
    withResource (gen >> detector fp) (const (pure ())) $
      testGroup name . tests

type ChangedFiles = IO [FilePath]

-- | Test each file in succession.
testEachChangedFile :: (HasCallStack) => TestName -> ChangedFiles -> (FilePath -> Assertion) -> TestTree
testEachChangedFile tn files cs = testCaseSteps tn \step -> do
  files >>= mapM_ \f -> step f >> cs f

{- | Test that the files are all staged.
Output a diff if they are not.
-}
testShouldNotHaveChangedDiff :: (HasCallStack) => TestName -> ChangedFiles -> TestTree
testShouldNotHaveChangedDiff tn files = testCase tn do
  files >>= \case
    [] -> pure ()
    files' -> do
      gitDiffFiles files' >>= \case
        Nothing -> pure ()
        Just err -> assertFailure err

-- | Don't do anything
noop :: IO ()
noop = pure ()

-- | Capture the stdout of a program
checkAndCapture :: (HasCallStack) => ProcessConfig stdin stdout stderr -> Assertion
checkAndCapture config = do
  (err, bs) <- readProcessInterleaved config
  case err of
    ExitSuccess -> pure ()
    ExitFailure f ->
      assertFailure
        ( show config
            ++ "failed with "
            ++ show f
            ++ ":\n"
            ++ LazyText.unpack (LazyText.decodeUtf8 bs)
        )

-- | Capture the stdout of a program
check :: (HasCallStack) => ProcessConfig stdin stdout stderr -> Assertion
check config = do
  err <- runProcess config
  case err of
    ExitSuccess -> pure ()
    ExitFailure f ->
      assertFailure
        ( show config
            ++ "failed with "
            ++ show f
        )
