-- | Helpers shared by the tests.
module Test.Util
  ( runNoIO
  , squeeze
  , firstText
  , buf
  , randoms
  , settle
  , testRuntime
  , settleUntil
  , settleWith
  , rowText
  , lookupRow
  , themeFile
  , runMotion
  , runEdit
  , fakeProvider
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Text (Text)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config (Config (..))
import Him.Edit
import Him.Editor
import Him.Event (Event (..))
import System.IO.Unsafe (unsafePerformIO)
import Him.EditorM (EditorM)
import Him.Runtime (Runtime, newRuntime)
import Him.Runtime qualified as Runtime
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import System.Timeout (timeout)
import Him.Syntax
import Him.Actions.Lsp (lspFlush)
import GHC.Clock (getMonotonicTime)
import Him.Language (langName)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Him.Motion
import Him.Position (Pos (..))
import Him.Selection
import Him.Render.Frame (Cell (..), Frame (..))
import Him.Theme (ThemeFile, parseThemeFile)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T

-- | Run an editor action that does no IO (for pure tests).
runNoIO :: EditorM () -> Editor -> Editor
runNoIO act ed = unsafePerformIO (execStateT act ed)

-- | Collapse runs of spaces in the details (they are padded into columns).
squeeze :: [(Text, Text)] -> [(Text, Text)]
squeeze = map (fmap (T.unwords . T.words))

firstText :: [Text] -> Text
firstText = \case
  x : _ -> x
  [] -> ""

buf :: Text -> B.Buffer
buf = B.fromText

-- | A tiny deterministic pseudo-random generator (no QuickCheck: boot
-- libraries only).
randoms :: Int -> [Int]
randoms = drop 1 . iterate (\x -> (x * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (62 :: Int)))

-- | Like the main loop: start the jobs the editor asked for on a real
-- runtime and handle their results as events, until nothing arrives for a
-- while (results may ask for more jobs).
settle :: Config -> Editor -> IO Editor
settle config ed0 = do
  rt <- testRuntime config
  settleWith config rt ed0

-- | A runtime and its event channel, to share between 'settleWith' calls
-- (state such as highlighters lives in the runtime, as in the editor).
testRuntime :: Config -> IO (Runtime, TChan Event)
testRuntime config = do
  events <- newTChanIO
  runtime <- newRuntime config (atomically . writeTChan events)
  pure (runtime, events)

-- | Handle job results until the editor satisfies a condition, or a
-- timeout (milliseconds) passes. For answers that take a while (language
-- servers).
settleUntil :: Config -> (Runtime, TChan Event) -> Int -> (Editor -> Bool) -> Editor -> IO Editor
settleUntil config (runtime, events) ms done ed0 = do
  deadline <- (+ fromIntegral ms / 1000) <$> getMonotonicTime
  let loop ed
        | done ed = pure ed
        | otherwise = do
            mapM_ (Runtime.perform runtime) (edEffects ed)
            now <- getMonotonicTime
            next <- timeout (max 1 (round ((deadline - now) * 1000000))) (atomically (readTChan events))
            case next of
              Nothing -> pure ed {edEffects = []}
              Just ev -> do
                ed' <- execStateT (handleEvent config ev) ed {edEffects = []}
                -- Like the main loop: send the text before the next frame.
                execStateT lspFlush ed' >>= loop
  loop ed0

settleWith :: Config -> (Runtime, TChan Event) -> Editor -> IO Editor
settleWith config (runtime, events) ed0 = do
  let loop ed = do
        mapM_ (Runtime.perform runtime) (edEffects ed)
        next <- timeout 300000 (atomically (readTChan events))
        case next of
          Nothing -> pure ed {edEffects = []}
          Just ev -> execStateT (handleEvent config ev) ed {edEffects = []} >>= loop
  loop ed0

-- | The characters of a frame's row.
rowText :: Frame -> Int -> Text
rowText f r = T.pack [c | Cell c _ <- maybe [] toList (lookupRow f r)]

lookupRow :: Frame -> Int -> Maybe (Seq.Seq Cell)
lookupRow f r = case drop r (toList (frameCells f)) of
  (row : _) -> Just row
  [] -> Nothing

-- | A theme file the test knows to be valid.
themeFile :: Text -> ThemeFile
themeFile = either (error . T.unpack) id . parseThemeFile

-- | Apply a motion to a collapsed range at a position; returns (anchor, head).
runMotion :: Motion -> Text -> Pos -> (Pos, Pos)
runMotion m t p = let r = m (buf t) (point p) in (rangeAnchor r, rangeHead r)

runEdit :: Edit -> Text -> Range -> (Text, Pos)
runEdit e t r = let (b', r') = e (buf t) r in (B.toText b', rangeHead r')

-- | A provider for the tests: highlights the word "let" in Haskell files,
-- through the same interface tree-sitter uses.
fakeProvider :: SyntaxProvider
fakeProvider = SyntaxProvider "fake" $ \language ->
  if langName language /= "haskell"
    then pure Nothing
    else do
      current <- newIORef B.empty
      pure . Just $
        SyntaxSession
          { ssUpdate = \_ b _ -> writeIORef current b
          , ssHighlight = \from to -> do
              b <- readIORef current
              pure $
                IntMap.fromList
                  [ (l, [LineSpan i (i + 3) "keyword" | i <- occurrences "let" (B.lineAt l b)])
                  | l <- [from .. min to (B.lineCount b - 1)]
                  ]
          , ssClose = pure ()
          }
  where
    occurrences needle hay = [T.length before | (before, _) <- T.breakOnAll needle hay]
