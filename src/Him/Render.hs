-- | Rendering the editor state into a 'Frame'.
--
-- The screen is split into regions ('Layout'), and each region is drawn by a
-- component of type @Theme -> Editor -> Rect -> Frame -> Frame@. To add a UI
-- element, give it a region here and write a component in @Him.Render.*@.
module Him.Render
  ( render
  , Layout (..)
  , layout
  , ensureCursorVisible
  ) where

import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.CommandLine
import Him.Render.Frame
import Him.Render.StatusLine
import Him.Render.TextArea
import Him.Render.Theme
import Him.Selection (primary, rangeHead)
import Him.Terminal.Ansi (CursorShape (..))
import Him.View (scrollToCursor)

data Layout = Layout
  { layoutText :: Rect
  , layoutStatus :: Rect
  , layoutCommand :: Rect
  }

layout :: (Int, Int) -> Layout
layout (rows, cols) =
  Layout
    { layoutText = Rect 0 0 (max 0 (rows - 2)) cols
    , layoutStatus = Rect (rows - 2) 0 1 cols
    , layoutCommand = Rect (rows - 1) 0 1 cols
    }

scrolloff :: Int
scrolloff = 3

-- | Scroll the view so the primary cursor is on screen.
ensureCursorVisible :: Editor -> Editor
ensureCursorVisible ed = ed {edView = scrollToCursor (rectHeight r, rectWidth r) scrolloff cursor (edView ed)}
  where
    r = layoutText (layout (edSize ed))
    cursor = (posLine (rangeHead (primary (docSelection (edDoc ed)))), cursorDisplayCol ed)

render :: Theme -> Editor -> Frame
render theme ed =
  frame {frameCursor = cursor, frameCursorShape = shape}
  where
    (rows, cols) = edSize ed
    Layout textR statusR cmdR = layout (edSize ed)
    frame =
      drawCommandLine theme ed cmdR
        . drawStatusLine theme ed statusR
        . drawTextArea theme ed textR
        $ blankFrame rows cols
    cursor = case edMode ed of
      CmdLine -> Just (commandLineCursor ed cmdR)
      _ -> cursorPosition ed textR
    shape = case edMode ed of
      Insert -> CursorBar
      CmdLine -> CursorBar
      _ -> CursorBlock
