module Main (main) where

import Him.App qualified as App
import System.Environment (getArgs)

-- | @him [FILE...]@
main :: IO ()
main = getArgs >>= App.run
