{-# LANGUAGE OverloadedStrings #-}

-- | Times the tree-sitter provider: a full parse and highlighting a
-- screenful (and the whole file). Build and run:
--   stack exec -- ghc -O1 -package him bench/HighlightBench.hs -o /tmp/hlbench
--   /tmp/hlbench LANGUAGE FILE
module Main (main) where

import Control.Exception (evaluate)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import Him.Buffer qualified as B
import Him.Language (langName, languages)
import Him.Syntax
import Him.Syntax.TreeSitter (treeSitter)
import System.Environment (getArgs)
import Text.Printf (printf)

timed :: String -> IO a -> IO a
timed label act = do
  t0 <- getMonotonicTime
  a <- act
  t1 <- getMonotonicTime
  printf "  %-28s %.1f ms\n" label ((t1 - t0) * 1000)
  pure a

main :: IO ()
main = do
  [name, file] <- getArgs
  text <- TIO.readFile file
  let lang = fromMaybe (error "language") (find ((== T.pack name) . langName) languages)
      buffer = B.fromText text
      n = B.lineCount buffer
  printf "%s: %d lines\n" file n
  Just session <- timed "load grammar + query" (spStart treeSitter lang)
  timed "full parse" (ssUpdate session 1 buffer [])
  let middle = n `div` 2
  spans <- timed "highlight 60 lines (middle)" (ssHighlight session middle (middle + 60) >>= \s -> evaluate (IntMap.size s) >> pure s)
  printf "  (%d spans)\n" (sum (map length (IntMap.elems spans)))
  timed "highlight 260 lines (window)" (ssHighlight session middle (middle + 260) >>= evaluate . IntMap.size)
  timed "highlight whole file" (ssHighlight session 0 n >>= evaluate . IntMap.size)
  timed "parse again (edit)" (ssUpdate session 2 (B.fromText (T.cons ' ' text)) [])
  pure ()
