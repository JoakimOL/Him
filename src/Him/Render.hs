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
import Him.Render.Gutter
import Him.Render.Completion
import Him.Render.Info
import Him.Render.Picker
import Him.Render.StatusLine
import Him.Render.TextArea
import Him.Render.Theme
import Him.Selection (primary, rangeHead)
import Him.Terminal.Ansi (CursorShape (..))
import Him.View (View (..), scrollToCursor)

data Layout = Layout
  { layoutGutter :: Rect
  , layoutText :: Rect
  , layoutStatus :: Rect
  , layoutCommand :: Rect
  }

layout :: Editor -> Layout
layout ed =
  Layout
    { layoutGutter = Rect 0 0 textRows gutter
    , layoutText = Rect 0 gutter textRows (max 1 (cols - gutter))
    , layoutStatus = Rect (rows - 2) 0 1 cols
    , layoutCommand = Rect (rows - 1) 0 1 cols
    }
  where
    (rows, cols) = edSize ed
    textRows = max 0 (rows - 2)
    -- Drop the gutter on very narrow terminals.
    gutter = if cols > 20 then gutterWidth ed else 0

scrolloff :: Int
scrolloff = 3

-- | Scroll the view so the primary cursor is on screen.
ensureCursorVisible :: Editor -> Editor
ensureCursorVisible ed = ed {edView = scrollToCursor (rectHeight r, rectWidth r) scrolloff cursor (edView ed)}
  where
    r = layoutText (layout ed)
    cursor = (posLine (rangeHead (primary (docSelection (edDoc ed)))), cursorDisplayCol ed)

-- | Render the editor. The previous frame, if given, lets unchanged rows be
-- reused (see "Him.Render.TextArea").
render :: Theme -> Maybe Frame -> Editor -> Frame
render theme prev ed =
  frame {frameCursor = cursor, frameCursorShape = shape, frameScroll = Just scroll}
  where
    (rows, cols) = edSize ed
    Layout gutterR textR statusR cmdR = layout ed
    -- Gutter and text area are full-width rows that move with the view.
    scroll = ScrollInfo (rectRow textR) (rectHeight textR) (viewTop (edView ed))
    frame =
      drawCommandLine theme ed cmdR
        . drawStatusLine theme ed statusR
        . drawPicker theme ed overlay
        . drawInfo theme ed overlay (cursorPosition ed textR)
        . drawCompletion theme ed overlay (cursorPosition ed textR)
        . drawTextArea theme prev ed textR
        . drawGutter theme ed gutterR
        $ blankFrame rows cols
    -- Popups cover the text area and the gutter.
    overlay = Rect 0 0 (rectHeight textR) cols
    cursor = case edMode ed of
      CmdLine -> Just (commandLineCursor ed cmdR)
      Picking -> pickerCursor ed overlay
      _ -> cursorPosition ed textR
    shape = case edMode ed of
      Insert -> CursorBar
      CmdLine -> CursorBar
      Picking -> CursorBar
      _ -> CursorBlock
