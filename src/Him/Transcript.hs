-- | Transcripts, the text of REPL and chat buffers (ADR repl, ADR ai-chat):
-- output goes in just before what is being typed, so typing is never
-- interrupted; @ret@ takes what was typed as the next input. Output is not
-- an edit: it is not undone, and it does not make the buffer dirty.
module Him.Transcript
  ( replState
  , insertOutput
  , takeInput
  , cleanOutput
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Document (DocKind (..), Document (..), inputPos, setInputPos)
import Him.Position (Pos (..))
import Him.Repl (ReplState (..))
import Him.Selection (Range (..), mapRanges, point, single)

replState :: Document -> Maybe ReplState
replState d = case docKind d of
  ReplDoc rs -> Just rs
  _ -> Nothing

-- | Text from the REPL (or code sent to it, shown as if typed), put before
-- the input. Cursors at or after that point move along with the input.
insertOutput :: Text -> Document -> Document
insertOutput t d = case inputPos d of
  Just input | not (T.null t) ->
    let at = Buffer.clampPos (docBuffer d) input
        (buf, end) = Buffer.insertText at t (docBuffer d)
        shift p
          | p < at = p
          | posLine p == posLine at = Pos (posLine end) (posCol end + posCol p - posCol at)
          | otherwise = Pos (posLine p + posLine end - posLine at) (posCol p)
     in setInputPos end
          d
            { docBuffer = buf
            , docSelection = mapRanges (\r -> r {rangeAnchor = shift (rangeAnchor r), rangeHead = shift (rangeHead r)}) (docSelection d)
            , docVersion = docVersion d + 1
            }
  _ -> d

-- | What was typed after the last output, and the buffer with it closed
-- by a line break (the next output and input go after it, the cursor too).
takeInput :: Document -> Maybe (Text, Document)
takeInput d = case inputPos d of
  Nothing -> Nothing
  Just from ->
    let buf0 = docBuffer d
        input = Buffer.textRange from (Buffer.endPos buf0) buf0
        (buf, end) = Buffer.insertText (Buffer.endPos buf0) "\n" buf0
     in Just
          ( input
          , setInputPos end
              d
                { docBuffer = buf
                , docSelection = single (point end)
                , docVersion = docVersion d + 1
                }
          )

-- | Output as the buffer shows it: no terminal escape sequences (colours,
-- cursor movement), no carriage returns.
cleanOutput :: Text -> Text
cleanOutput = T.pack . go . T.unpack
  where
    go = \case
      [] -> []
      '\ESC' : '[' : rest -> go (drop 1 (dropWhile (not . final) rest))
      '\ESC' : ']' : rest -> go (dropOsc rest)
      '\ESC' : _ : rest -> go rest
      '\r' : rest -> go rest
      c : rest -> c : go rest
    final c = isAsciiLower c || isAsciiUpper c || c == '@' || c == '~'
    -- An OSC sequence ends with BEL or ESC \.
    dropOsc = \case
      [] -> []
      '\a' : rest -> rest
      '\ESC' : '\\' : rest -> rest
      _ : rest -> dropOsc rest
