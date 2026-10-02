{-# LANGUAGE OverloadedStrings #-}
-- | Times the file picker's pieces: walking a tree (listFiles) and
-- filtering (matches). Build and run:
--   stack exec -- ghc -O1 -package him bench/PickerBench.hs -o /tmp/pickerbench
--   /tmp/pickerbench DIR [DIR...]
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Him.FileTree (listFiles)
import Him.Picker (PickTarget (..), Picker (..), newPicker, pickerItem, setQuery)
import System.Environment (getArgs)
import Text.Printf (printf)

timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getMonotonicTime
  a <- act
  t1 <- getMonotonicTime
  pure (a, (t1 - t0) * 1000)

main :: IO ()
main = do
  dirs <- getArgs
  forM_ dirs $ \dir -> do
    (files, ms) <- timed (listFiles 1000000 dir >>= \fs -> length fs `seq` pure fs)
    printf "listFiles %s: %d files in %.1f ms\n" dir (length files) ms
    (picker, mi) <- timed (let p = newPicker "files" [pickerItem (T.pack f) (PickFile f) "" | f <- files] in evaluate (length (pkItems p)) >> pure p)
    printf "  building %d items: %.1f ms\n" (length (pkItems picker)) mi
    forM_ ["s", "src", "term", "helixterm", "zzzz"] $ \q -> do
      (p', mq) <- timed (let p' = setQuery (T.pack q) picker in evaluate (length (pkMatches p') + pkMatchCount p') >> pure p')
      printf "  query %-10s -> %6d matches, %4d shown, in %.1f ms\n" q (pkMatchCount p') (length (pkMatches p')) mq
