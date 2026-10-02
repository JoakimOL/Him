-- | The main text area: buffer lines, selections, and the cursor position.
module Him.Render.TextArea
  ( drawTextArea
  , cursorPosition
  , cursorDisplayCol
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Buffer (lineAt, lineCount)
import Him.Document (DirEntry (..), DocKind (..), Document (..))
import Him.Syntax (SyntaxInfo (..))
import Him.Syntax.Span (LineSpan (..))
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Lsp.Protocol (Severity (..))
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnostics)
import Him.Terminal.Ansi (Style (..), packStyle, unpackStyle)
import Him.Selection
import Him.TextWidth (displayCol, glyphs, isWide, layoutLine)
import Him.View (View (..))

-- | Draws the visible lines. Rows whose 'RowKey' is the same as in the
-- previous frame are copied from it instead of being laid out again.
drawTextArea :: Theme -> Maybe Frame -> Editor -> Rect -> Frame -> Frame
drawTextArea theme prev ed rect frame0 = foldl' drawRow frame0 [0 .. rectHeight rect - 1]
  where
    prevFrame = case prev of
      Just p | frameRows p == frameRows frame0 && frameCols p == frameCols frame0 -> Just p
      _ -> Nothing
    -- Where a line was on the previous screen: rows are reused by line, so
    -- scrolling does not invalidate them.
    prevRowOf screenRow = case prevFrame >>= frameScroll of
      Just si -> screenRow + (top - siTop si)
      Nothing -> screenRow
    doc = edDoc ed
    buf = docBuffer doc
    View top left = edView ed
    sel = docSelection doc
    prim = primary sel
    -- In insert mode the cursor is a bar between characters, so a collapsed
    -- range is not shown as a one-character selection.
    -- Only ranges touching the visible lines, filtered once per frame (there
    -- may be thousands of ranges after @% s@).
    bottom = top + rectHeight rect
    onScreen = [r | r <- ranges sel, posLine (rangeEnd r) >= top, posLine (rangeStart r) < bottom]
    shown = [(rangeStart r, rangeEnd r) | r <- onScreen, edMode ed /= Insert || not (isCollapsed r)]
    secondaryHeads = [rangeHead r | r <- onScreen, r /= prim]
    -- Packed once per frame, not per cell.
    textStyle = packStyle (themeText theme)
    dirStyle = packStyle (themeDirectory theme)
    headerStyle = packStyle (themeDirectoryHeader theme)
    -- Lines of a directory listing are coloured by what they are: 0 plain
    -- text, 1 the header, 2 a directory. (Part of the row key, so a cached
    -- row is never reused with the wrong colour.)
    lineClass line = case docKind doc of
      DirectoryDoc entries
        | line == 0 -> 1
        | (e : _) <- drop (line - 1) entries, deIsDir e -> 2
      _ -> 0 :: Int
    diagnosticsByLine =
      IntMap.fromListWith (<>) [(sdLine sd, [(sdStart sd, sdEnd sd, sdSeverity sd)]) | sd <- shownDiagnostics (edLsp ed) (docLsp doc) buf, sdLine sd >= top, sdLine sd < bottom]
    sevRank = \case
      SevError -> 0
      SevWarning -> 1
      SevInfo -> 2
      SevHint -> 3 :: Int
    cursorStyle = packStyle (themeCursor theme)
    selectionStyle = packStyle (themeSelection theme)

    drawRow f r
      | line >= lineCount buf = putText screenRow (rectCol rect) (themeTilde theme) "~" f
      | Just p <- prevFrame
      , IntMap.lookup (prevRowOf screenRow) (frameRowKeys p) == Just key =
          remember (copyCells (prevRowOf screenRow) screenRow (rectCol rect) (rectWidth rect) p f)
      | otherwise = remember (putCells screenRow (rectCol rect) visible f)
      where
        key = RowKey line text spans cursors left (rectCol rect) (rectWidth rect) cls syntax [(a, b, sevRank sev) | (a, b, sev) <- underlines]
        underlines = IntMap.findWithDefault [] line diagnosticsByLine
        -- Diagnostics are underlined, in their severity's colour.
        underlineAt i = case [sev | (a, b, sev) <- underlines, a <= i, i < b] of
          sev : _ -> Just sev
          [] -> Nothing
        syntax = IntMap.findWithDefault [] line (siSpans (docSyntax doc))
        -- The syntax style of each highlighted span, under the selection.
        syntaxStyles = [(lsStart sp, lsEnd sp, packStyle st) | sp <- syntax, Just st <- [scopeStyle theme (lsScope sp)]]
        syntaxAt i = case [st | (a, b, st) <- syntaxStyles, a <= i, i < b] of
          st : _ -> st
          [] -> base
        baseAt i = case underlineAt i of
          Nothing -> syntaxAt i
          Just sev -> packStyle ((unpackStyle (syntaxAt i)) {styleUnderline = True, styleFg = styleFg (themeDiagnostic theme sev)})
        cls = lineClass line
        base = case cls of
          1 -> headerStyle
          2 -> dirStyle
          _ -> textStyle
        remember fr = fr {frameRowKeys = IntMap.insert screenRow key (frameRowKeys fr)}
        line = top + r
        screenRow = rectRow rect + r
        text = lineAt line buf
        len = T.length text
        -- Selected column intervals and secondary cursors on this line,
        -- computed once per row rather than per character.
        spans = [(colOn s0 0, colOn e0 len) | (s0, e0) <- shown, posLine s0 <= line, line <= posLine e0]
        colOn (Pos l c) dflt = if l == line then c else dflt
        cursors = [c | Pos l c <- secondaryHeads, l == line]
        styleAt i
          | i `elem` cursors = Just cursorStyle
          | any (\(a, b) -> a <= i && i <= b) spans = Just selectionStyle
          | otherwise = Nothing
        -- The cells of the whole line from display column 0, then the part
        -- inside the horizontal scroll window.
        -- Fast path: printable ASCII is one cell per character (no tabs,
        -- wide or control characters), so no layout is needed.
        plain = T.all (\ch -> ch >= ' ' && ch < '\DEL') text
        lineCells
          | plain = zipWith (\i ch -> Cell ch (fromMaybe (baseAt i) (styleAt i))) [0 ..] (T.unpack text) <> lineEndCell
          | otherwise = concatMap charCells (layoutLine text) <> lineEndCell
        visible
          | plain = take (rectWidth rect) (drop left lineCells)
          | otherwise = fixEdges (take (rectWidth rect) (drop left lineCells))
        charCells (i, _, w, c) =
          let style = fromMaybe (baseAt i) (styleAt i)
           in if isWide c
                then [Cell c style, Cell continuation style]
                else map (`Cell` style) (glyphs c w)
        -- The line end only shows when it is selected (or a cursor).
        lineEndCell = maybe [] (\st -> [Cell ' ' st]) (styleAt len)
        -- A wide character cut by the left or right edge becomes a blank, so
        -- the terminal never draws half of it.
        fixEdges cs = case cs of
          (Cell c st : rest) | c == continuation -> fixRight (Cell ' ' st : rest)
          _ -> fixRight cs
        fixRight cs = case reverse cs of
          (Cell c st : rest) | isWide c -> reverse (Cell ' ' st : rest)
          _ -> cs

-- | Display column of the primary cursor.
cursorDisplayCol :: Editor -> Int
cursorDisplayCol ed = displayCol (lineAt l buf) c
  where
    buf = docBuffer (edDoc ed)
    Pos l c = rangeHead (primary (docSelection (edDoc ed)))

-- | Screen position of the primary cursor, if it is inside the area.
cursorPosition :: Editor -> Rect -> Maybe (Int, Int)
cursorPosition ed rect
  | row >= 0 && row < rectHeight rect && col >= 0 && col < rectWidth rect =
      Just (rectRow rect + row, rectCol rect + col)
  | otherwise = Nothing
  where
    View top left = edView ed
    row = posLine (rangeHead (primary (docSelection (edDoc ed)))) - top
    col = cursorDisplayCol ed - left
