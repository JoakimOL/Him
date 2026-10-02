-- | Keeping the current document's highlighting current (ADR-26): find a
-- provider for its language, and ask for spans of the lines around the
-- view whenever the text changes or the view leaves the lines covered.
-- Which provider does the work is invisible here ("Him.Syntax").
module Him.Actions.Syntax
  ( syntaxHousekeeping
  , applySyntaxResult
  , highlightMargin
  ) where

import Control.Monad.Trans.State.Strict (get, modify')
import Him.Buffer qualified as Buffer
import Him.EditorM
import Him.Document (DocKind (..), Document (..))
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Editor
import Him.Language (detectLanguage, langName, languages)
import Him.Repl (ReplState (..))
import Data.List (find)
import Him.Syntax
import Him.View (View (..))
import Him.Window (Window (..))
import Data.IntMap.Strict qualified as IntMap

-- | Lines highlighted above and below the visible ones, so scrolling a
-- little needs no new request.
highlightMargin :: Int
highlightMargin = 100

-- | After every event (like 'Him.Actions.Git.gitHousekeeping'). At most
-- one highlight job per document is in flight; when the text moved on
-- meanwhile, the next round asks again.
syntaxHousekeeping :: EditorM ()
syntaxHousekeeping = do
  ed <- get
  let d = edDoc ed
      si = docSyntax d
      setSyntax s = modifyDoc (\doc -> doc {docSyntax = s})
      (top, bottom) = visibleLines ed
  case siStatus si of
    SyntaxUnknown -> case (docKind d, docPath d) of
      (TextDoc, Just path)
        | Just language <- detectLanguage languages path (Buffer.lineAt 0 (docBuffer d)) -> do
            setSyntax si {siStatus = SyntaxStarting, siLanguage = Just (langName language)}
            request (StartJob (SyntaxStart (docId d) language))
      -- A REPL's transcript is highlighted as its language.
      (ReplDoc rs, _)
        | Just language <- find ((== rsLanguage rs) . langName) languages -> do
            setSyntax si {siStatus = SyntaxStarting, siLanguage = Just (langName language)}
            request (StartJob (SyntaxStart (docId d) language))
      _ -> setSyntax si {siStatus = SyntaxNone}
    SyntaxActive _
      | not (siPending si)
      , siVersion si /= docVersion d || top < siFrom si || bottom > siTo si -> do
          let from = max 0 (top - highlightMargin)
              to = bottom + highlightMargin
          setSyntax si {siPending = True}
          request (StartJob (Highlight (docId d) (docVersion d) (docBuffer d) from to))
    _ -> pure ()

-- | The lines of the current document that windows show: the focused
-- window's, and those of other windows on the same document when they are
-- near enough to highlight in one go (ADR-37).
visibleLines :: Editor -> (Int, Int)
visibleLines ed
  | bottom - top <= 2000 = (top, bottom)
  | otherwise = own
  where
    height = focusedTextHeight ed
    own = (viewTop (edView ed), viewTop (edView ed) + height)
    others = [(viewTop v, viewTop v + height) | (_, w) <- IntMap.toList (edWindows ed), winDoc w == docId (edDoc ed), let v = winView w]
    top = minimum (map fst (own : others))
    bottom = maximum (map snd (own : others))

-- | A syntax job reported back. Spans for an older version are still
-- shown (they are at most one burst of typing behind) until newer ones
-- arrive.
applySyntaxResult :: JobResult -> EditorM ()
applySyntaxResult = \case
  SyntaxStarted doc started -> modify' $ modifyDocument doc $ \d ->
    let si = docSyntax d
     in d {docSyntax = si {siStatus = maybe SyntaxNone SyntaxActive started, siPending = False, siVersion = -1}}
  Highlighted doc version from to spans -> modify' $ modifyDocument doc $ \d ->
    let si = docSyntax d
     in d
          { docSyntax =
              if version >= siVersion si
                then si {siSpans = spans, siVersion = version, siFrom = from, siTo = to, siPending = False}
                else si {siPending = False}
          }
  _ -> pure ()
