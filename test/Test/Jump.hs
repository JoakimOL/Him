-- | The jumplist (ADR-47): the pure list, and @C-o@ / @tab@ / @C-s@ /
-- @space j@ through the default keys.
module Test.Jump
  ( jumpTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM, toList)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Jumplist
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.Picker (Picker (..), PickerItem (..))
import Him.Position (Pos (..))
import Him.Selection (Range (..), point, primary, rangeHead, single)
import Him.Session (handleEvent)
import Test.Harness
import Test.Util

jumpTests :: IO [Test]
jumpTests = do
  config <- either (fail . show) pure defaultConfig
  let start = newEditor (20, 80) (newDocument Nothing (buf (T.intercalate "\n" ["line " <> T.pack (show i) | i <- [1 .. 50 :: Int]])))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      lineAfter :: Text -> IO Int
      lineAfter ks = cursorLine <$> typeKeys ks start
  backAndForth <- mapM lineAfter ["g e C-o", "g e C-o tab", "g e C-o C-i", "5 g g g e C-o C-o", "5 g g g e C-o C-o tab"]
  saved <- lineAfter "j j C-s g g C-o"
  followed <- lineAfter "5 g g g e g g i x ret esc C-o C-o"
  searched <- lineAfter "/ l i n e space 7 ret C-o"
  listed <- typeKeys "5 g g g e space j" start
  deleted <- typeKeys "del" listed
  picked <- typeKeys "ret" listed
  deletedMarked <- typeKeys "tab tab del" listed
  other <- typeKeys "space b del" start
  pure
    [ test "push drops the jumps after the current one and repeats" $
        assertEqual ([j 0, j 3], 2) (shape (push (j 3) (Jumplist (Seq.fromList [j 0, j 1, j 2]) 1)))
    , test "push does not repeat the last jump" (assertEqual ([j 0], 1) (shape (push (j 0) (push (j 0) emptyJumplist))))
    , test "push keeps at most 30" $
        assertEqual (capacity, j 11) (let jl = foldl (flip push) emptyJumplist (map j [1 .. 40]) in (Seq.length (jlJumps jl), Seq.index (jlJumps jl) 0))
    , test "backward pushes where the cursor is, forward comes back" $
        let Just (to, jl) = backward 1 (j 9) (fromList' [j 0, j 1])
         in assertEqual (j 1, ([j 0, j 1, j 9], 1), Just (j 9)) (to, shape jl, fst <$> forward 1 jl)
    , test "backward skips a jump to where the cursor is" $
        assertEqual (Just (j 0)) (fst <$> backward 1 (j 1) (fromList' [j 0, j 1]))
    , test "backward when full" $
        let full = fromList' (map j [1 .. 30])
         in assertEqual (Just (j 30, 28)) ((\(to, jl) -> (to, jlCurrent jl)) <$> backward 1 (j 99) full)
    , test "nothing before the first jump" (assertEqual Nothing (fst <$> backward 1 (j 0) emptyJumplist))
    , test "remove moves the current entry with it" $
        assertEqual ([j 0, j 2], 1) (shape (remove 1 (Jumplist (Seq.fromList [j 0, j 1, j 2]) 2)))
    , test "positions follow a change" $
        assertEqual [Pos 0 1, Pos 1 0, Pos 4 0, Pos 3 1]
          [rangeHead (primary (mapThroughChange (Pos 1 0, Pos 1 2, "ab\ncd\n") (single (point p)))) | p <- [Pos 0 1, Pos 1 1, Pos 2 0, Pos 1 3]]
    , test "C-o goes back, tab and C-i forward again" (assertEqual [0, 49, 49, 0, 4] backAndForth)
    , test "C-s saves the selection" (assertEqual 2 saved)
    , test "jumps follow edits above them" (assertEqual 5 followed)
    , test "a search is a jump" (assertEqual 0 searched)
    , test "space j lists the jumps, newest first" $
        assertEqual (Just ["[scratch]:5", "[scratch]:1"], Picking) (map piLabel . pkMatches <$> edPicker listed, edMode listed)
    , test "del removes the selected entry" $
        assertEqual (Just ["[scratch]:1"], [0]) (map piLabel . pkMatches <$> edPicker deleted, jumpLines deleted)
    , test "tab marks entries and del removes them all" $
        assertEqual (Just [], []) (map piLabel . pkMatches <$> edPicker deletedMarked, jumpLines deletedMarked)
    , test "ret jumps to it, and where the cursor was is pushed" $
        assertEqual (4, Nothing, [0, 4, 49]) (cursorLine picked, edPicker picked, jumpLines picked)
    , test "pickers without a second action say so" $
        assertEqual (Just "this picker has no second action") (statusText other)
    ]
  where
    j l = Jump 1 (single (Range (Pos l 0) (Pos l 0) Nothing))
    shape jl = (toList (jlJumps jl), jlCurrent jl)
    fromList' js = Jumplist (Seq.fromList js) (length js)
    cursorLine e = posLine (rangeHead (primary (docSelection (edDoc e))))
    jumpLines e = [posLine (rangeHead (primary (jumpSelection x))) | jl <- IntMap.elems (edJumps e), x <- toList (jlJumps jl)]
    statusText e = (\(Status _ t) -> t) <$> edStatus e
