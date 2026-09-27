-- | A minimal test harness, so the test suite needs no Hackage packages.
module Test.Harness
  ( Test
  , test
  , group
  , assertEqual
  , runTests
  ) where

import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.Exit (exitFailure)

-- | A named test, or a named group of tests.
data Test
  = Case String (Either String ())
  | Group String [Test]

test :: String -> Either String () -> Test
test = Case

group :: String -> [Test] -> Test
group = Group

-- | 'Right ()' on equality, otherwise a message showing both values.
assertEqual :: (Eq a, Show a) => a -> a -> Either String ()
assertEqual expected actual
  | expected == actual = Right ()
  | otherwise = Left ("expected: " <> show expected <> "\n     got: " <> show actual)

-- | Run all tests, print failures, and exit non-zero if any failed.
runTests :: [Test] -> IO ()
runTests tests = do
  failures <- newIORef (0 :: Int)
  total <- newIORef (0 :: Int)
  mapM_ (go failures total "") tests
  failed <- readIORef failures
  count <- readIORef total
  putStrLn (show (count - failed) <> "/" <> show count <> " tests passed")
  unless (failed == 0) exitFailure
  where
    go :: IORef Int -> IORef Int -> String -> Test -> IO ()
    go failures total prefix = \case
      Group name ts -> mapM_ (go failures total (prefix <> name <> " / ")) ts
      Case name result -> do
        modifyIORef' total (+ 1)
        case result of
          Right () -> pure ()
          Left msg -> do
            modifyIORef' failures (+ 1)
            putStrLn ("FAIL " <> prefix <> name <> "\n     " <> msg)
