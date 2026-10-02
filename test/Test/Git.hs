-- | Diffs, git state and git in a temporary repository.
module Test.Git
  ( diffTests'
  , gitStateTests
  , gitTests
  ) where

import Control.Monad (when)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.File (loadDocument)
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectoryIfMissing, doesPathExist, getTemporaryDirectory, removeDirectoryRecursive)
import Him.Diff
import Him.GitState
import Him.Actions.Git (gitHousekeeping)
import Data.IntMap.Strict qualified as IntMap
import Him.Process (ProcessResult (..), runProcess)
import Him.Key
import Data.Text qualified as T
import Test.Harness
import Test.Util

diffTests' :: [Test]
diffTests' =
  [ test "identical texts have no hunks" (assertEqual [] (diffLines ["a", "b"] ["a", "b"]))
  , test "an added line" (assertEqual [Hunk 1 0 1 1] (diffLines ["a", "c"] ["a", "b", "c"]))
  , test "a removed line" (assertEqual [Hunk 1 1 1 0] (diffLines ["a", "b", "c"] ["a", "c"]))
  , test "a changed line" (assertEqual [Hunk 1 1 1 1] (diffLines ["a", "b", "c"] ["a", "x", "c"]))
  , test "separate hunks" (assertEqual [Hunk 0 1 0 1, Hunk 3 0 3 1] (diffLines ["a", "b", "c"] ["x", "b", "c", "d"]))
  , test "kinds" (assertEqual [Added, Removed, Changed] (map hunkKind [Hunk 1 0 1 2, Hunk 1 2 1 0, Hunk 0 1 0 1]))
  , test "mapLine shifts lines after a hunk" $
      -- A line inserted before old line 1; old line 2 replaced by new line 3.
      assertEqual [0, 2, 3, 4] (map (mapLine [Hunk 1 0 1 1, Hunk 2 1 3 1]) [0, 1, 2, 3])
  , test "random edits: hunks rebuild the new text and are minimal" (mapM_ diffModel (take 400 (chunks' (randoms 29))))
  , test "a huge rewrite falls back to one hunk" $
      let old = [T.pack (show i) | i <- [1 .. 3000 :: Int]]
          new = [T.pack ("x" <> show i) | i <- [1 .. 3000 :: Int]]
       in assertEqual [Hunk 0 3000 0 3000] (diffLines old new)
  ]
  where
    chunks' xs = let (a, b) = splitAt 30 xs in a : chunks' b
    diffModel rs = case rs of
      (r1 : r2 : more) ->
        let alphabet = ["a", "b", "c", "d"]
            old = [alphabet !! (r `mod` 4) | r <- take (r1 `mod` 12) more]
            new = [alphabet !! (r `div` 5 `mod` 4) | r <- take (r2 `mod` 12) (drop 12 more)]
            hs = diffLines old new
            changed = sum [hOldCount h + hNewCount h | h <- hs]
            optimal = length old + length new - 2 * lcs old new
         in if applyHunks hs old new /= new
              then Left ("does not rebuild: " <> show (old, new, hs))
              else if changed /= optimal then Left ("not minimal: " <> show (old, new, hs)) else Right ()
      _ -> Right ()
    -- Longest common subsequence, by the textbook dynamic program.
    lcs xs ys = last (foldl step (replicate (length ys + 1) 0) xs)
      where
        step prev x = scanl (\left (y, diag, up) -> if x == y then diag + 1 else max left up) 0 (zip3 ys prev (drop 1 prev))

gitStateTests :: [Test]
gitStateTests =
  [ test "stage all lines of a hunk" $
      assertEqual ["a", "X", "c"] (apply ["a", "b", "c"] ["a", "X", "c"] (const True))
  , test "stage nothing" (assertEqual ["a", "b", "c"] (apply ["a", "b", "c"] ["a", "X", "c"] (const False)))
  , test "changed lines are paired, so one line can be staged" $
      assertEqual ["X", "b", "c"] (apply ["a", "b", "c"] ["X", "Y", "c"] (== 0))
  , test "extra added lines are staged when selected" $
      assertEqual ["a", "new2", "b"] (apply ["a", "b"] ["a", "new1", "new2", "b"] (== 2))
  , test "a removal is staged through the line above it" $
      assertEqual (["a", "c"], ["a", "b", "c"]) (apply ["a", "b", "c"] ["a", "c"] (== 0), apply ["a", "b", "c"] ["a", "c"] (== 1))
  , test "reverting = applying the unselected changes" $
      let old = ["a", "b", "c", "d"]
          new = ["a", "B", "c", "D"]
       in assertEqual ["a", "b", "c", "D"] (apply old new (/= 1))
  , test "signs: added, changed, removed; unstaged wins" $
      let t = GitTracking (GitBase "" "" "" [] True [] True) [Hunk 0 0 0 1, Hunk 2 1 3 0] [Hunk 0 1 0 1, Hunk 4 1 5 1] 0 False False
       in assertEqual
            [(0, Sign SignAdded False), (2, Sign SignRemoved False), (5, Sign SignChanged True)]
            (IntMap.toList (gitSigns t 0 10))
  ]
  where
    apply old new = applySelected old new (diffLines old new)

-- | Git end to end in a temporary repository.
gitTests :: IO [Test]
gitTests = do
  config <- either (fail . T.unpack) pure defaultConfig
  dir <- getTemporaryDirectory
  let repo = dir <> "/him-test-git"
      file = repo <> "/f.txt"
      g args = runProcess "git" args (Just repo) ""
      run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      stagedDiff = either (const "") (TE.decodeUtf8 . prStdout) <$> g ["diff", "--cached", "--no-color", "-U0"]
  exists <- doesPathExist repo
  when exists (removeDirectoryRecursive repo)
  createDirectoryIfMissing True repo
  _ <- g ["init", "-q"]
  _ <- g ["config", "user.email", "t@t"]
  _ <- g ["config", "user.name", "t"]
  writeFile file "one\ntwo\nthree\n"
  _ <- g ["add", "f.txt"]
  _ <- g ["commit", "-q", "-m", "init"]
  doc <- either (fail . T.unpack) pure =<< loadDocument file
  opened <- settle config =<< execStateT gitHousekeeping (newEditor (24, 80) doc)
  -- Change line 2, add a line after line 3.
  edited <- settle config =<< keys "j e c T W O esc g e o f o u r esc" opened
  let hunksOf ed = (gtUnstaged <$> tracking (docGit (edDoc ed)), gtStaged <$> tracking (docGit (edDoc ed)))
  -- Stage only line 2 (the change), not the added line.
  staged <- settle config =<< keys "g g j x space g s" edited
  cached <- stagedDiff
  unstaged <- settle config =<< keys "g g j x space g u" staged
  cachedAfter <- stagedDiff
  reset <- settle config =<< keys "g g j x space g r" edited
  outside <- settle config =<< execStateT gitHousekeeping (newEditor (24, 80) (newDocument (Just "/") (buf "")))
  removeDirectoryRecursive repo
  pure
    [ test "a tracked file gets its git base" (assertEqual (Just "f.txt") (gbPath . gtBase <$> tracking (docGit (edDoc opened))))
    , test "edits show as unstaged hunks" (assertEqual (Just [Hunk 1 1 1 1, Hunk 3 0 3 1], Just []) (hunksOf edited))
    , test "staging the selected line writes only it to the index" $
        assertEqual True ("-two\n+TWO\n" `T.isInfixOf` cached && not ("+four" `T.isInfixOf` cached))
    , test "after staging, the change is staged and the rest unstaged" (assertEqual (Just [Hunk 3 0 3 1], Just [Hunk 1 1 1 1]) (hunksOf staged))
    , test "unstaging the line empties the index diff" (assertEqual ("", Just [Hunk 1 1 1 1, Hunk 3 0 3 1]) (cachedAfter, fst (hunksOf unstaged)))
    , test "reset puts the index version of the selected line back" (assertEqual ["one", "two", "three", "four"] (B.toLines (docBuffer (edDoc reset))))
    , test "outside a repository there is no git state" (assertEqual GitOutside (docGit (edDoc outside)))
    ]
