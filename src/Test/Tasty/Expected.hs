{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | A temporary part of the library. Might move to its own package.
module Test.Tasty.Expected (
  -- * Expectations
  Expectation,
  it,
  inStepsIt,

  -- * Helpers
  shouldExitWith,

  -- * Advanced Stuff
  Expected (..),
  ExpectedSteps (..),
) where

import Control.Exception
import Control.Monad
import Data.Data (Typeable)
import Data.IORef
import Data.List
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import GHC.Exception
import System.Process.Typed (ExitCode, ProcessConfig, readProcessInterleaved)
import qualified Test.HUnit.Lang as HUnit
import Test.Tasty.Providers
import Test.Tasty.Runners
import Text.Printf

type Expectation = HUnit.Assertion

newtype Expected = Expected HUnit.Assertion
  deriving (Typeable)

instance IsTest Expected where
  testOptions = pure []
  run _ (Expected test) _ = do
    hunitResult <- try test
    return $
      case hunitResult of
        Right () -> testPassed ""
        Left (HUnit.HUnitFailure mbloc message) ->
          testFailed $ prependLocation mbloc (HUnit.formatFailureReason message)

newtype ExpectedSteps = ExpectedSteps ((String -> IO ()) -> HUnit.Assertion)
  deriving (Typeable)

-- | Borrowed from [here](https://hackage.haskell.org/package/tasty-hunit-0.10.1/docs/src/Test.Tasty.HUnit.Steps.html#testCaseSteps)
instance IsTest ExpectedSteps where
  run _ (ExpectedSteps assertionFn) _ = do
    ref <- newIORef []

    let
      stepFn :: String -> IO ()
      stepFn msg = do
        tme <- getTime
        atomicModifyIORef ref (\l -> ((tme, msg) : l, ()))

    hunitResult <-
      (Right <$> assertionFn stepFn)
        `catch` \(SomeException ex) -> return $ Left (displayException ex)

    endTime <- getTime

    maxMsgLength <- foldl' max 0 . map (length . snd) <$> readIORef ref

    let msgFormat = "%-" ++ show (min maxMsgLength 62) ++ "s (%.02fs)"

    msgs <-
      snd
        . foldl'
          ( \(lastTime, acc) (curTime, msg) ->
              let !duration = lastTime - curTime
                  !msg' = if duration >= 0.01 then printf msgFormat msg duration else msg
               in (curTime, msg' : acc)
          )
          (endTime, [])
        <$> readIORef ref

    return $
      case hunitResult of
        Right{} -> testPassed (unlines msgs)
        Left errMsg ->
          testFailed $
            if null msgs
              then errMsg
              else
                unlines $
                  msgs ++ map ("  " ++) (lines errMsg)

  testOptions = return []

prependLocation :: Maybe SrcLoc -> String -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ s

-- | Create a new expectation
it :: TestName -> Expectation -> TestTree
it name test = singleTest name (Expected test)

-- | Create a new expectation
inStepsIt :: TestName -> ((String -> IO ()) -> Expectation) -> TestTree
inStepsIt name test = singleTest name (ExpectedSteps test)

shouldExitWith :: ProcessConfig stdin stdout stderr -> ExitCode -> Expectation
shouldExitWith pcfg expect = do
  (ec, bytstr) <- readProcessInterleaved pcfg
  when (ec /= expect) do
    HUnit.assertFailure (LazyText.unpack $ LazyText.decodeUtf8 bytstr)
