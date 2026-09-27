-- | Entry point of the editor. Grows into the main event loop
-- (read event -> resolve key -> run command -> render) in later milestones.
module Him.App
  ( run
  ) where

import Control.Concurrent (newChan, readChan, writeChan)
import Data.ByteString.Builder (Builder, byteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Him.Event (Event (..))
import Him.Key (KeyCode (..), plain, showKey)
import Him.Log (logMsg)
import Him.Terminal.Ansi
import Him.Terminal.Input (startInputReader)
import Him.Terminal.Output (writeOutput)
import Him.Terminal.Raw (withRawTerminal)
import Him.Terminal.Size (getWindowSize, onResize)

-- | Run the editor, optionally opening the given file.
run :: Maybe FilePath -> IO ()
run file = do
  logMsg ("starting, file = " <> show file)
  withRawTerminal $ do
    events <- newChan
    size <- fromMaybe (24, 80) <$> getWindowSize
    onResize (writeChan events . uncurry EvResize)
    startInputReader events
    let loop sz lastKey = do
          writeOutput (drawDemo sz lastKey)
          readChan events >>= \case
            EvResize rows cols -> loop (rows, cols) lastKey
            EvKey k
              | k == plain (KChar 'q') -> pure ()
              | otherwise -> loop sz (showKey k)
    loop size "(none)"

-- | Temporary screen for milestones 3-4: tildes, a centred welcome message,
-- and the last decoded key in the bottom row.
drawDemo :: (Int, Int) -> Text -> Builder
drawDemo (rows, cols) lastKey =
  hideCursor
    <> clearScreen
    <> foldMap (\r -> moveCursor r 0 <> text "~") [0 .. rows - 2]
    <> centred (rows `div` 3) "him - a modal editor"
    <> centred (rows `div` 3 + 1) "press keys to see how they decode, q to quit"
    <> moveCursor (rows - 1) 0
    <> sgr defaultStyle {styleReverse = True}
    <> text (T.justifyLeft cols ' ' (" last key: " <> lastKey <> "   size: " <> T.pack (show cols <> "x" <> show rows)))
    <> sgr defaultStyle
    <> moveCursor 0 0
    <> showCursor
  where
    text = byteString . encodeUtf8
    centred row msg = moveCursor row (max 0 ((cols - T.length msg) `div` 2)) <> text msg
