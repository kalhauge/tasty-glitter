{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Glitter (
  glitter,

  -- * Utils
  testEachChangedFile,
  testShouldNotHaveChanged,
  noop,

  -- ** Subprocesses
  check,
  checkAndCapture,
) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers

import Test.Tasty.HUnit

import Control.Git (gitChanges, gitLsFiles)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import System.Exit
import System.Process.Typed

newtype GlitterConfig = GlitterConfig
  { detector :: [FilePath] -> IO [FilePath]
  }

instance IsOption GlitterConfig where
  defaultValue = GlitterConfig gitChanges
  optionName = pure "glitter"
  optionHelp = pure "glitter help"
  showDefaultValue = const (Just "git")
  parseValue = \case
    "git" -> Just defaultValue
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
glitter
  :: TestName
  -- ^ The name of the test
  -> [FilePath]
  -- ^ The files or folders to check for changes
  -> IO a
  -- ^ An action that generate or changes the files or folders (or does nothing)
  -> (ChangedFiles -> [TestTree])
  -- ^ Given an resource of changed tests, create a test-tree.
  -> TestTree
glitter name fp gen tests =
  askOption \(GlitterConfig{detector}) ->
    withResource (gen >> detector fp) (const (pure ())) $
      testGroup name . tests

type ChangedFiles = IO [FilePath]

-- | Test each file in succession.
testEachChangedFile :: TestName -> ChangedFiles -> (FilePath -> Assertion) -> TestTree
testEachChangedFile tn files cs = testCaseSteps tn \step -> do
  files >>= mapM_ \f -> step f >> cs f

-- | Test that the files have not changed
testShouldNotHaveChanged :: TestName -> ChangedFiles -> TestTree
testShouldNotHaveChanged tn files = testCase tn do
  files >>= \case
    [] -> pure ()
    rs -> assertFailure (unlines rs)

-- | Don't do anything
noop :: IO ()
noop = pure ()

-- | Capture the stdout of a program
checkAndCapture :: ProcessConfig stdin stdout stderr -> Assertion
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
check :: ProcessConfig stdin stdout stderr -> Assertion
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
