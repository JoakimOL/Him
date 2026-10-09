-- | The main text area: buffer lines, selections, and the cursor position.
module Him.Render.TextArea
  ( drawTextArea
  , cursorPosition
  , cursorDisplayCol
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Him.Buffer (endPos, lineAt, lineCount)
import Him.Chat (ChatMark (..), ChatState (..), ChatStatus (..))
import Him.Chat.Transcript (Inline (..), inlineSpans)
import Him.Document (DirEntry (..), DocKind (..), Document (..))
import Him.Syntax (SyntaxInfo (..))
import Him.Syntax.Span (LineSpan (..))
import Him.Editor (Editor (..), reviewFor)
import Him.Review (DisplayRow (..), addedLines, displayRows, rowOfLine)
import Him.Diff (Hunk (..))
import Him.Mode (Mode (..))
import Him.Options (Options (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Lsp.Protocol (Severity (..))
import Him.GitState (SignKind (..))
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnosticsIn)
import Him.Terminal.Ansi (Style (..), defaultStyle, packStyle, patchStyle, unpackStyle)
import Him.Selection
import Him.TextWidth (displayCol, glyphs, isWide, layoutLine)
import Him.View (View (..))
import Him.PluginUI (Annotation (..), Highlight (..), annotationsIn, highlightsIn)

-- | Draws the visible lines. Rows whose 'RowKey' is the same as in the
-- previous frame are copied from it instead of being laid out again.
drawTextArea :: Theme -> Bool -> Maybe Frame -> Editor -> Rect -> Frame -> Frame
drawTextArea theme focused prev ed rect frame0 = foldl' drawDisplayRow frame0 (zip [0 ..] rows)
  where
    -- A document under review also shows each proposed change's header
    -- and removed lines (ADR change-review).
    review = reviewFor ed (docId doc)
    rows = displayRows review (lineCount buf) top (rectHeight rect)
    drawDisplayRow f (r, row) = case row of
      LineRow l -> drawRow f r l
      EmptyRow -> putText (rectRow rect + r) (rectCol rect) (themeTilde theme) "~" f
      HeaderRow n total h ->
        band (themeReviewHeader theme) r (" ✎ " <> T.pack (show n) <> "/" <> T.pack (show total) <> "  −" <> T.pack (show (hOldCount h)) <> " +" <> T.pack (show (hNewCount h)) <> "  ·  space c a keep  ·  space c d discard  ·  space c n next") f
      RemovedRow t -> band (themeRemoved theme) r (T.drop left (T.replace "\t" (T.replicate tabWidth " ") t)) f
    band st r t = putText (rectRow rect + r) (rectCol rect) st (T.take (rectWidth rect) t <> T.replicate (rectWidth rect - T.length t) " ")
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
    tabWidth = optTabWidth (edOptions ed)
    View top left = edView ed
    sel = docSelection doc
    prim = primary sel
    -- In insert mode the cursor is a bar between characters, so a collapsed
    -- range is not shown as a one-character selection.
    -- Only ranges touching the visible lines, filtered once per frame (there
    -- may be thousands of ranges after @% s@).
    bottom = top + rectHeight rect
    onScreen = [r | r <- ranges sel, posLine (rangeEnd r) >= top, posLine (rangeStart r) < bottom]
    shown = [(rangeStart r, rangeEnd r, r == prim) | r <- onScreen, edMode ed /= Insert || not (isCollapsed r)]
    -- An unfocused window has no terminal cursor: its primary is drawn too.
    secondaryHeads = [rangeHead r | r <- onScreen, r /= prim || not focused]
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
      -- The chat (ADR chat-panel): the input box (empty, empty while the model
      -- works, or with a message), and the transcript's lines by mark.
      ChatDoc cs
        | line >= posLine (csInput cs) ->
            if csInput cs /= endPos buf then 32 else if csStatus cs == ChatWaiting then 31 else 30
        | Just m <- IntMap.lookup line (csMarks cs) -> 10 + fromEnum m
        | otherwise -> 4
      _ | any (\(a, b) -> a <= line && line < b) pending -> 3
      _ -> 0 :: Int
    -- Lines a proposed change adds (ADR change-review).
    pending = addedLines review
    pendingStyle = packStyle (themeText theme `patchStyle` themeHighlight theme)
    -- The chat's lines.
    dim = themeText theme `patchStyle` themePopupDetail theme
    -- A style's colour and modifiers over the text's background.
    bold st = themeText theme `patchStyle` st {styleBg = styleBg (themeText theme), styleBold = True}
    chatStyle = \case
      MarkWelcome -> dim
      MarkUser -> bold (themeDirectoryHeader theme)
      MarkUserText -> themeText theme
      MarkContext -> dim
      MarkClaude -> bold (themeDirectory theme)
      MarkTool -> dim
      MarkChange -> themeText theme `patchStyle` (themeGitSign theme SignAdded False) {styleBg = styleBg (themeText theme)}
      MarkReview -> themeReviewHeader theme
      MarkNote -> dim `patchStyle` defaultStyle {styleItalic = True}
      MarkError -> themeText theme `patchStyle` themeError theme
      MarkFence -> themeCodeBlock theme `patchStyle` themePopupDetail theme
      MarkCode -> themeCodeBlock theme
    chatStyles = [(10 + fromEnum m, packStyle (chatStyle m)) | m <- [minBound .. maxBound]]
    inputStyle = packStyle (themeChatInput theme)
    placeholderStyle = packStyle (themeChatInput theme `patchStyle` themePopupDetail theme)
    -- Rows drawn across the whole width (a box: code, the input, the
    -- review summary), and what an empty input says.
    banded c = c `elem` [10 + fromEnum MarkReview, 10 + fromEnum MarkFence, 10 + fromEnum MarkCode, 30, 31, 32]
    placeholder = \case
      30 -> "Ask about your code  (ret send · A-ret new line · up earlier)"
      31 -> "Claude is working…  (C-c stops it)"
      _ -> ""
    -- Markdown in the chat's prose.
    inlineStyle = \case
      InlineCode -> themeInlineCode theme
      InlineBold -> defaultStyle {styleBold = True}
      InlineHeading -> bold (themeDirectoryHeader theme)
    annotationsByLine = annotationsIn (docId doc) top bottom (edPluginUI ed)
    highlightsByLine = highlightsIn (docId doc) top bottom (edPluginUI ed)
    diagnosticsByLine =
      IntMap.fromListWith (<>) [(sdLine sd, [(sdStart sd, sdEnd sd, sdSeverity sd)]) | sd <- shownDiagnosticsIn (edLsp ed) (docLsp doc) buf top bottom]
    sevRank = \case
      SevError -> 0
      SevWarning -> 1
      SevInfo -> 2
      SevHint -> 3 :: Int
    -- Styles are laid over each other (Helix's patching): syntax over the
    -- text, then a diagnostic's underline, the selection, a cursor.
    over st = packStyle . (`patchStyle` st) . unpackStyle

    drawRow f r line
      | Just p <- prevFrame
      , Map.lookup (prevRowOf screenRow, rectCol rect) (frameRowKeys p) == Just key =
          remember (copyCells (prevRowOf screenRow) screenRow (rectCol rect) (rectWidth rect) p f)
      | otherwise = remember (putCells screenRow (rectCol rect) visible f)
      where
        key = RowKey line text spans cursors left (rectCol rect) (rectWidth rect) cls syntax [(a, b, sevRank sev) | (a, b, sev) <- underlines] annotations highlights
        highlights = IntMap.findWithDefault [] line highlightsByLine
        annotations = IntMap.findWithDefault [] line annotationsByLine
        underlines = IntMap.findWithDefault [] line diagnosticsByLine
        -- Diagnostics are underlined (as the theme says).
        underlineAt i = case [sev | (a, b, sev) <- underlines, a <= i, i < b] of
          sev : _ -> Just sev
          [] -> Nothing
        syntax = IntMap.findWithDefault [] line (siSpans (docSyntax doc))
        -- The syntax style of each highlighted span, under the selection;
        -- plugins' highlights go over the syntax's.
        syntaxStyles =
          [(hlFrom h, hlTo h, over (faceStyle theme (hlFace h)) base) | h <- highlights]
            <> [(lsStart sp, lsEnd sp, over st base) | sp <- syntax, Just st <- [scopeStyle theme (lsScope sp)]]
            <> [(a, b, over (inlineStyle k) base) | cls == 4, (a, b, k) <- inlineSpans text]
        syntaxAt i = case [st | (a, b, st) <- syntaxStyles, a <= i, i < b] of
          st : _ -> st
          [] -> base
        baseAt i = case underlineAt i of
          Nothing -> syntaxAt i
          Just sev -> over (themeDiagnosticText theme sev) (syntaxAt i)
        cls = lineClass line
        base = case cls of
          1 -> headerStyle
          2 -> dirStyle
          3 -> pendingStyle
          c | c >= 30 -> inputStyle
          c | Just st <- lookup c chatStyles -> st
          _ -> textStyle
        remember fr = fr {frameRowKeys = Map.insert (screenRow, rectCol rect) key (frameRowKeys fr)}
        screenRow = rectRow rect + r
        text = lineAt line buf
        len = T.length text
        -- Selected column intervals and secondary cursors on this line,
        -- computed once per row rather than per character.
        spans = [(colOn s0 0, colOn e0 len, isPrim) | (s0, e0, isPrim) <- shown, posLine s0 <= line, line <= posLine e0]
        colOn (Pos l c) dflt = if l == line then c else dflt
        cursors = [c | Pos l c <- secondaryHeads, l == line]
        styleAt i
          | i `elem` cursors = Just (themeCursor theme)
          | (_, _, isPrim) : _ <- filter (\(a, b, _) -> a <= i && i <= b) spans =
              Just (if isPrim then themeSelectionPrimary theme else themeSelection theme)
          | otherwise = Nothing
        styled i = maybe (baseAt i) (`over` baseAt i) (styleAt i)
        -- The cells of the whole line from display column 0, then the part
        -- inside the horizontal scroll window.
        -- Fast path: printable ASCII is one cell per character (no tabs,
        -- wide or control characters), so no layout is needed.
        plain = T.all (\ch -> ch >= ' ' && ch < '\DEL') text
        lineCells
          | plain = zipWith (\i ch -> Cell ch (styled i)) [0 ..] (T.unpack text) <> lineEndCell
          | otherwise = concatMap charCells (layoutLine tabWidth text) <> lineEndCell
        visible = annotate . fillRow $
          if plain
            then take (rectWidth rect) (drop left lineCells)
            else fixEdges (take (rectWidth rect) (drop left lineCells))
        -- A banded row is filled to the edge (after an empty input's
        -- placeholder).
        fillRow cs
          | banded cls =
              let extra = [Cell ch placeholderStyle | ch <- T.unpack (placeholder cls)]
                  row = take (rectWidth rect) (cs <> extra)
               in row <> replicate (rectWidth rect - length row) (Cell ' ' base)
          | otherwise = cs
        charCells (i, _, w, c) =
          let style = styled i
           in if isWide c
                then [Cell c style, Cell continuation style]
                else map (`Cell` style) (glyphs c w)
        -- Plugins' annotations follow the line's end, a space apart, as far
        -- as the row goes (ADR plugin-building-blocks).
        annotate cs
          | null annotations || banded cls = cs
          | otherwise =
              cs
                <> take
                  (rectWidth rect - length cs)
                  (concat [Cell ' ' base : annotationCells a | a <- annotations])
        annotationCells a =
          let st = over (faceStyle theme (anFace a)) base
           in concat [if isWide c then [Cell c st, Cell continuation st] else [Cell (if c < ' ' then ' ' else c) st] | c <- T.unpack (anText a)]
        -- The line end only shows when it is selected (or a cursor).
        lineEndCell = maybe [] (\st -> [Cell ' ' (over st base)]) (styleAt len)
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
cursorDisplayCol ed = displayCol (optTabWidth (edOptions ed)) (lineAt l buf) c
  where
    buf = docBuffer (edDoc ed)
    Pos l c = rangeHead (primary (docSelection (edDoc ed)))

-- | Screen position of the primary cursor, if it is inside the area.
cursorPosition :: Editor -> Rect -> Maybe (Int, Int)
cursorPosition ed rect = case rowOfLine rows line of
  Just row | col >= 0 && col < rectWidth rect -> Just (rectRow rect + row, rectCol rect + col)
  _ -> Nothing
  where
    View top left = edView ed
    doc = edDoc ed
    line = posLine (rangeHead (primary (docSelection doc)))
    -- The rows on screen (a review's extra rows included, ADR change-review).
    rows = displayRows (reviewFor ed (docId doc)) (lineCount (docBuffer doc)) top (rectHeight rect)
    col = cursorDisplayCol ed - left
