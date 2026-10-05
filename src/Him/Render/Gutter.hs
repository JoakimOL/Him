-- | The gutter left of the text area: a sign lane (git changes,
-- diagnostics; only while a plugin draws there) and line numbers.
module Him.Render.Gutter
  ( drawGutter
  , gutterWidth
  ) where

import Data.Text qualified as T
import Him.Buffer (lineCount)
import Data.IntMap.Strict qualified as IntMap
import Him.GitState (SignKind (..))
import Him.PluginUI (GutterSign (..), signsIn)
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnosticsIn)
import Him.Document (DocKind (..), Document (..))
import Him.Chat (ChatMark (..), ChatState (..))
import Him.Editor (Editor (..), reviewFor)
import Him.Review (DisplayRow (..), addedLines, displayRows)
import Him.Options (LineNumbers (..), Options (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Terminal.Ansi (Style (..))
import Him.Selection (primary, rangeHead)
import Him.View (View (..))

-- | Width of the gutter: a column for signs (when a plugin draws them), the
-- digits of the largest line number (at least three; none when line
-- numbers are off), and one column of padding.
gutterWidth :: Editor -> Int
gutterWidth ed = case docKind (edDoc ed) of
  -- The chat has no line numbers: a column for its bars (ADR-45).
  ChatDoc _ -> 1
  _ -> signLane ed + numbers + 1
  where
    numbers = case optLineNumbers (edOptions ed) of
      LineNumbersOff -> 0
      _ -> max 3 (length (show (lineCount (docBuffer (edDoc ed)))))

signLane :: Editor -> Int
signLane ed = if edSignLane ed then 1 else 0

drawGutter :: Theme -> Editor -> Rect -> Frame -> Frame
drawGutter theme ed rect frame0 = case docKind (edDoc ed) of
  ChatDoc cs -> drawChatBars theme ed cs rect frame0
  _ -> foldl' drawDisplayRow frame0 (zip [0 ..] rows)
  where
    -- The same rows as the text area: a review's removed lines get a minus,
    -- the lines it adds a plus (ADR-43).
    review = reviewFor ed (docId doc)
    rows = displayRows review (lineCount (docBuffer doc)) top (rectHeight rect)
    added = addedLines review
    drawDisplayRow f (r, row) = case row of
      LineRow l -> drawRow f r l
      RemovedRow _ -> sign r "-" (themeGitSign theme SignRemoved False) f
      _ -> f
    sign r t st f = if lane == 0 then f else putText (rectRow rect + r) (rectCol rect) st t f
    doc = edDoc ed
    top = viewTop (edView ed)
    current = posLine (rangeHead (primary (docSelection doc)))
    lane = signLane ed
    digits = rectWidth rect - 1 - lane
    -- Plugins' signs (git's among them, ADR-50).
    signs = signsIn (docId doc) top (top + rectHeight rect - 1) (edPluginUI ed)
    -- Diagnostics win over git signs: the most severe on each line.
    diagnostics =
      IntMap.fromListWith min [(sdLine sd, sdSeverity sd) | sd <- shownDiagnosticsIn (edLsp ed) (docLsp doc) (docBuffer doc) top (top + rectHeight rect)]
    drawRow f r line =
      putText (rectRow rect + r) (rectCol rect + lane) style label $
        if lane == 0 then f else putText (rectRow rect + r) (rectCol rect) signStyle signText f
      where
        style = if line == current then themeGutterCurrent theme else themeGutter theme
        -- Relative numbers count from the cursor's line, which shows its own.
        number = case optLineNumbers (edOptions ed) of
          LineNumbersRelative | line /= current -> abs (line - current)
          _ -> line + 1
        label = if digits <= 0 then "" else T.justifyRight digits ' ' (T.pack (show number)) <> " "
        (signText, signStyle) = case (IntMap.lookup line diagnostics, IntMap.lookup line signs) of
          _ | any (\(a, b) -> a <= line && line < b) added -> ("+", themeGitSign theme SignAdded False)
          (Just sev, _) -> ("●", themeDiagnostic theme sev)
          (_, Just gs) -> (T.take 1 (gsText gs), faceStyle theme (gsFace gs))
          _ -> (" ", themeGutter theme)

-- | The chat's gutter: a bar beside your messages, the code blocks, the
-- changes to review, and the input.
drawChatBars :: Theme -> Editor -> ChatState -> Rect -> Frame -> Frame
drawChatBars theme ed cs rect frame0 = foldl' bar frame0 [0 .. rectHeight rect - 1]
  where
    top = viewTop (edView ed)
    count = lineCount (docBuffer (edDoc ed))
    bar f r =
      let line = top + r
          st
            | line >= count = Nothing
            | line >= posLine (csInput cs) = Just (themeChatInput theme)
            | otherwise = case IntMap.lookup line (csMarks cs) of
                Just m
                  | m `elem` [MarkUser, MarkUserText, MarkContext] -> Just (themeDirectoryHeader theme)
                  | m `elem` [MarkFence, MarkCode] -> Just (themeCodeBlock theme)
                  | m == MarkReview -> Just (themeReviewHeader theme)
                _ -> Nothing
       in case st of
            Just s
              | m <- IntMap.lookup line (csMarks cs)
              , m `elem` map Just [MarkUser, MarkUserText, MarkContext] ->
                  putText (rectRow rect + r) (rectCol rect) s {styleBg = styleBg (themeText theme)} "▌" f
              | otherwise -> putText (rectRow rect + r) (rectCol rect) s " " f
            Nothing -> f
