{-# LANGUAGE OverloadedStrings #-}

-- | Times Him.Diff on a large file with a few edits (what the git signs
-- recompute after an edit). Build and run:
--   stack exec -- ghc -O1 -package him bench/DiffBench.hs -o /tmp/diffbench && /tmp/diffbench
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Him.Diff (diffLines)
import Text.Printf (printf)

main :: IO ()
main = do
  let old = [T.pack ("line " <> show i <> " lorem ipsum dolor sit amet") | i <- [1 .. 200000 :: Int]]
      edit k f ls = take k ls <> f (drop k ls)
      cases =
        [ ("one changed line in the middle", edit 100000 (\(_ : r) -> "changed" : r) old)
        , ("a line added at the top", "new" : old)
        , ("10 edits spread out", foldr (\k -> edit k (\(_ : r) -> "x" : r)) old [0, 20000 .. 180000])
        , ("identical", old)
        ]
  _ <- evaluate (length old)
  forM_ cases $ \(name, new) -> do
    _ <- evaluate (length new)
    t0 <- getMonotonicTime
    n <- evaluate (length (diffLines old new))
    t1 <- getMonotonicTime
    printf "%-32s %d hunks in %.1f ms\n" (name :: String) n ((t1 - t0) * 1000)
