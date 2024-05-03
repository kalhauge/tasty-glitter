{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Glitter (
  glitter,
) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers

import Control.Git (gitChanges, gitLsFiles)

newtype GlitterConfig = GlitterConfig
  { detector :: FilePath -> IO [FilePath]
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
      fs' -> pure $ testFailed ("changed files:\n" ++ unlines fs')

-- | A glitter test.
glitter
  :: TestName
  -- ^ The name of the test
  -> FilePath
  -- ^ A file or folder to check for changes
  -> IO a
  -- ^ An action that generate the file or folder
  -> (IO [FilePath] -> [TestTree])
  -- ^ Given an resource of changed tests, create a test-tree.
  -> TestTree
glitter name fp gen tests =
  askOption \(GlitterConfig{detector}) ->
    withResource (gen >> detector fp) (const (pure ())) \files ->
      testGroup
        name
        ( tests files
            ++ [ singleTest "should not have changed" (GlitterTest files)
               ]
        )
