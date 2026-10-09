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
import Him.Editor (Editor (..), reviewFor, windowBoxes, windowEditor)
import Him.Review (displayRows, rowOfLine)
import Him.Buffer (lineCount)
import Him.Window (Box (..))
import Him.Mode (Mode (..))
import Him.Options (CursorKind (..), Options (..), cursorKindFor)
import Him.Position (Pos (..))
import Him.Render.Canvas
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

-- | Where one window's parts go: the gutter and text area, and its status
-- line below them.
data Layout = Layout
  { layoutWindow :: Int
  , layoutGutter :: Rect
  , layoutText :: Rect
  , layoutStatus :: Rect
  }

-- | Every window's layout, from its box ('windowBoxes').
windowLayouts :: Editor -> [Layout]
windowLayouts ed = [layoutIn w box (if w == edFocus ed then ed else windowEditor ed w) | (w, box) <- windowBoxes ed]

layoutIn :: Int -> Box -> Editor -> Layout
layoutIn w (Box row col height width) ed =
  Layout
    { layoutWindow = w
    , layoutGutter = Rect row col textRows gutter
    , layoutText = Rect row (col + gutter) textRows (max 1 (width - gutter))
    , layoutStatus = Rect (row + textRows) col 1 width
    }
  where
    textRows = max 0 (height - 1)
    -- Drop the gutter in very narrow windows.
    gutter = if width > 20 then gutterWidth ed else 0

-- | The focused window's layout.
layout :: Editor -> Layout
layout ed = case [l | l <- windowLayouts ed, layoutWindow l == edFocus ed] of
  l : _ -> l
  [] -> layoutIn (edFocus ed) (Box 0 0 (max 0 (fst (edSize ed) - 1)) (snd (edSize ed))) ed

-- | Scroll the view so the primary cursor is on screen.
ensureCursorVisible :: Editor -> Editor
ensureCursorVisible ed = ed {edView = reviewed (scrollToCursor (height, rectWidth r) scrolloff cursor (edView ed))}
  where
    r = layoutText (layout ed)
    height = rectHeight r
    scrolloff = optScrolloff (edOptions ed)
    line = posLine (rangeHead (primary (docSelection (edDoc ed))))
    cursor = (line, cursorDisplayCol ed)
    -- A review's extra rows (ADR change-review) take screen rows too: scroll further
    -- until the cursor's line is drawn above the margin.
    reviewed v = case reviewFor ed (docId (edDoc ed)) of
      Nothing -> v
      Just rv -> go (100 :: Int) v
        where
          go 0 view = view
          go n view =
            case rowOfLine (displayRows (Just rv) (lineCount (docBuffer (edDoc ed))) (viewTop view) height) line of
              Just row | row < max 1 (height - scrolloff) || viewTop view >= line -> view
              _ -> go (n - 1) view {viewTop = viewTop view + 1}

-- | Render the editor. The previous frame, if given, lets unchanged rows be
-- reused (see "Him.Render.TextArea").
render :: Theme -> Maybe Frame -> Editor -> Frame
render theme prev ed =
  frame {frameCursor = cursor, frameCursorShape = shape, frameScroll = scroll, frameColors = (themeForeground theme, themeBackground theme)}
  where
    (rows, cols) = edSize ed
    focused = layout ed
    textR = layoutText focused
    cmdR = Rect (rows - 1) 0 1 cols
    -- The terminal can scroll the focused window's rows only when it spans
    -- the whole width (scrolling moves whole rows).
    scroll
      | rectCol (layoutGutter focused) == 0 && rectCol (layoutStatus focused) == 0 && rectWidth (layoutStatus focused) == cols =
          Just (ScrollInfo (rectRow textR) (rectHeight textR) (viewTop (edView ed)))
      | otherwise = Nothing
    frame =
      drawCommandLine theme ed cmdR
        . drawCanvas theme ed overlay
        . drawPicker theme ed overlay
        . drawInfo theme ed overlay (cursorPosition ed textR)
        . drawCompletion theme ed overlay (cursorPosition ed textR)
        . drawBorders
        $ foldl' drawWindow (blankFrame rows cols) (windowLayouts ed)
    drawWindow f l =
      let isFocused = layoutWindow l == edFocus ed
          wed = if isFocused then ed else windowEditor ed (layoutWindow l)
       in drawStatusLine theme isFocused wed (layoutStatus l)
            . drawTextArea theme isFocused prev wed (layoutText l)
            . drawGutter theme wed (layoutGutter l)
            $ f
    -- A column between windows side by side.
    drawBorders f =
      foldl'
        (\fr (_, Box row col height width) -> if col + width < cols then foldl' (\acc r -> putText r (col + width) (themeWindow theme) "│" acc) fr [row .. row + height - 1] else fr)
        f
        (windowBoxes ed)
    -- Popups cover the windows (not the command line and the status row
    -- above it).
    overlay = Rect 0 0 (max 0 (rows - 2)) cols
    cursor = case edMode ed of
      -- A canvas has no cursor (ADR plugin-canvas).
      _ | Just _ <- edCanvas ed -> Nothing
      CmdLine -> Just (commandLineCursor ed cmdR)
      Picking -> pickerCursor ed overlay
      _ -> cursorPosition ed textR
    shape = case cursorKindFor (edOptions ed) (edMode ed) of
      CursorKindBlock -> CursorBlock
      CursorKindBar -> CursorBar
      CursorKindUnderline -> CursorUnderline
