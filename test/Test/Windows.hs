-- | Splits: the layout tree, and windows through the keys.
module Test.Windows
  ( windowTests
  ) where

import Data.Foldable (foldlM)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Control.Monad.Trans.State.Strict (execStateT)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Key (parseKeys)
import Him.Position (Pos (..))
import Him.Render (render)
import Him.Render.Theme (defaultTheme)
import Him.Selection (primary, rangeHead)
import Him.Session (handleEvent)
import Him.Window
import Test.Harness
import Test.Util

windowTests :: IO [Test]
windowTests = do
  config <- either (fail . show) pure defaultConfig
  let start t = newEditor (12, 60) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      text = T.intercalate "\n" [T.pack (show i) | i <- [1 .. 40 :: Int]]
      cursorLine e = posLine (rangeHead (primary (docSelection (edDoc e))))
  vsplit <- typeKeys "C-w v" (start text)
  -- Move in the new window; the old one keeps its place.
  moved <- typeKeys "C-w v 9 j" (start text)
  back <- typeKeys "C-w h" moved
  -- Each window keeps its own buffer.
  scratchRight <- typeKeys "C-w n v i x esc" (start "file")
  scratchLeft <- typeKeys "C-w h" scratchRight
  closed <- typeKeys ": q ret" vsplit
  quitLast <- typeKeys ": q ret" closed
  only <- typeKeys "C-w s C-w v C-w o" (start text)
  stacked <- typeKeys "space w s space w k" (start text)
  swapped <- typeKeys "C-w v C-w H" (start text)
  paged <- typeKeys "C-w s C-f" (start text)
  let frame = render defaultTheme Nothing vsplit
  pure
    [ test "a split goes beside the window, or splits it across" $
        assertEqual
          [Split Beside [Leaf 1, Leaf 2, Leaf 3], Split Beside [Leaf 1, Split Stacked [Leaf 2, Leaf 3]]]
          [insertBeside Beside 2 3 (Split Beside [Leaf 1, Leaf 2]), insertBeside Stacked 2 3 (Split Beside [Leaf 1, Leaf 2])]
    , test "removing a window collapses its split; the last one stays" $
        assertEqual [Leaf 1, Leaf 1] [removeWindow 2 (Split Beside [Leaf 1, Leaf 2]), removeWindow 1 (Leaf 1)]
    , test "boxes share the space, with a border between windows side by side" $
        assertEqual
          [(1, Box 0 0 10 40), (2, Box 0 41 5 39), (3, Box 5 41 5 39)]
          (boxes (Box 0 0 10 80) (Split Beside [Leaf 1, Split Stacked [Leaf 2, Leaf 3]]))
    , test "neighbours are found on each side" $
        let placed = boxes (Box 0 0 10 80) (Split Beside [Leaf 1, Split Stacked [Leaf 2, Leaf 3]])
         in assertEqual [Just 2, Just 1, Just 3, Just 2, Nothing] [neighbour SideRight 1 placed, neighbour SideLeft 3 placed, neighbour SideDown 2 placed, neighbour SideUp 3 placed, neighbour SideUp 2 placed]
    , test "C-w v splits side by side and focuses the new window" $
        assertEqual (Split Beside [Leaf 0, Leaf 2], 2, [0]) (edLayout vsplit, edFocus vsplit, IntMap.keys (edWindows vsplit))
    , test "windows keep their own cursor and view" $
        assertEqual (9, 0) (cursorLine moved, cursorLine back)
    , test "windows can show different buffers" $
        assertEqual ("x", "file") (B.toText (docBuffer (edDoc scratchRight)), B.toText (docBuffer (edDoc scratchLeft)))
    , test ":q closes the window, and quits with the last one" $
        assertEqual (Leaf 0, False, True) (edLayout closed, edQuit closed, edQuit quitLast)
    , test "C-w o keeps only the focused window" $
        assertEqual (1, True) (length (leaves (edLayout only)), IntMap.null (edWindows only))
    , test "space w works like C-w" (assertEqual (Split Stacked [Leaf 0, Leaf 2], 0) (edLayout stacked, edFocus stacked))
    , test "C-w H swaps with the window to the left" (assertEqual (Split Beside [Leaf 2, Leaf 0], 2) (edLayout swapped, edFocus swapped))
    , test "both windows are drawn, with a border and a status line each" $
        assertEqual (True, 2) ("│" `T.isInfixOf` rowText frame 0, length (T.breakOnAll "1:1" (rowText frame 10)))
    , test "paging moves by the window's height" $
        -- 12 rows: the command line, then windows of 6 and 5 rows (4 of text).
        assertEqual 4 (cursorLine paged)
    ]
