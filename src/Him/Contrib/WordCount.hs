-- | A contrib plugin (ADR-51): the number of words in the buffer, in the
-- status line. An example of an event handler, state and a segment.
module Him.Contrib.WordCount
  ( wordCount
  , countWords
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Him.Plugin

wordCount :: PluginSpec (IntMap Int)
wordCount =
  (pluginSpec "wordcount" "The number of words in the buffer, in the status line" IntMap.empty)
    { psDefaultOn = False
    , psOptions = [("max-lines", "longer buffers are not counted, as counting follows every change (default 10000)")]
    , psOnEvent = \case
        BufferOpened i -> recount i
        BufferChanged i _ -> recount i
        BufferClosed i -> modifyState (IntMap.delete i) >> showCounts
        _ -> pure ()
    }

-- | Count a buffer's words again (text buffers only, up to max-lines).
recount :: BufferId -> PluginM (IntMap Int) ()
recount i = do
  limit <- optionInt "max-lines" 10000
  bufferInfo i >>= \case
    Just b | biKind b == TextBuffer && biLineCount b <= limit -> do
      n <- maybe 0 countWords <$> bufferText i
      modifyState (IntMap.insert i n)
    _ -> modifyState (IntMap.delete i)
  showCounts

showCounts :: PluginM (IntMap Int) ()
showCounts = do
  counts <- getState
  setSegments [(segment (label n)) {segDoc = Just i, segFace = Just (face "comment")} | (i, n) <- IntMap.toList counts]
  where
    label n = T.pack (show n) <> (if n == 1 then " word" else " words")

countWords :: Text -> Int
countWords = length . T.words
