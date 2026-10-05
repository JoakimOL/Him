-- | What plugins build on (ADR-50): events, their processes, status line
-- segments, gutter signs and annotations.
module Test.PluginApi
  ( pluginApiTests
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.App (handleEvent)
import Him.Session (housekeeping)
import Him.Config (Config (..), Plugin (..), plugin)
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.EditorM (request)
import Him.Effect (Effect (..))
import Him.Event qualified as Ev
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.PluginEvent
import Him.PluginUI
import Him.Render (render)
import Him.Render.Theme (defaultTheme)
import Test.Harness
import Test.Util

pluginApiTests :: IO [Test]
pluginApiTests = do
  config0 <- either (fail . T.unpack) pure defaultConfig
  -- A plugin that writes down every event, and starts a process when the
  -- first document opens.
  seen <- newIORef []
  let recorder =
        (plugin "test" "records events")
          { plEvent = \ev -> do
              liftIO (modifyIORef' seen (<> [ev]))
              case ev of
                BufferOpened 1 -> request (ProcessStart "test:p" "printf" ["a\\nb"] Nothing)
                _ -> pure ()
          }
      config = config0 {cfgPlugins = cfgPlugins config0 <> [recorder]}
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (Ev.EvKey k)) e) ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      start = newEditor (10, 40) (newDocument Nothing (buf "hello"))
  -- As the editor starts: housekeeping before the first key.
  _ <- settle config =<< typeKeys "i x esc" =<< execStateT (housekeeping config) start
  events <- readIORef seen
  pure
    [ test "signs: the higher priority wins where they overlap; only the asked lines" $
        let ui n spans = (n, emptyPluginUI {puSigns = IntMap.singleton 1 spans})
            uis = Map.fromList [ui "a" [SignSpan 0 3 (sign "a" 1)], ui "b" [SignSpan 2 6 (sign "b" 2)]]
         in assertEqual [(1, "a"), (2, "b"), (3, "b")] (IntMap.toList (gsText <$> signsIn 1 1 3 uis))
    , test "segments: for this document or every one, best first" $
        let uis = Map.singleton "a" emptyPluginUI {puSegments = [(segment "x") {segDoc = Just 2}, (segment "y") {segPriority = 1}, segment "z"]}
         in assertEqual (["y", "z"], ["y", "x", "z"]) (map segText (segmentsFor 1 uis), map segText (segmentsFor 2 uis))
    , test "annotations by line, in the range" $
        let uis = Map.singleton "a" emptyPluginUI {puAnnotations = IntMap.singleton 1 [Annotation 0 "zero" (face "comment"), Annotation 5 "five" (face "comment")]}
         in assertEqual [0] (IntMap.keys (annotationsIn 1 0 3 uis))
    , test "closed documents' signs and segments are forgotten" $
        let uis = Map.singleton "a" emptyPluginUI {puSigns = IntMap.fromList [(1, []), (2, [])], puSegments = [(segment "x") {segDoc = Just 2}]}
         in assertEqual (Map.singleton "a" emptyPluginUI {puSigns = IntMap.singleton 1 []}) (dropDocuments (IntSet.singleton 1) uis)
    , test "events: opened and entered at first, then changes, saves, closes, modes" $
        let d v s i = (newDocument Nothing (buf "")) {docId = i, docVersion = v, docSaves = s}
            (first, s1) = detectEvents [d 0 0 1] 1 Normal unseen
            (second, s2) = detectEvents [d 1 0 1, d 0 0 2] 2 Insert s1
            (third, _) = detectEvents [d 1 1 1] 1 Insert s2
         in assertEqual
              ([BufferOpened 1, BufferEntered 1], [BufferOpened 2, BufferChanged 1 1, BufferEntered 2, ModeChanged Normal Insert], [BufferClosed 2, BufferSaved 1, BufferEntered 1])
              (first, second, third)
    , test "a plugin hears what happens, and its process's lines" $
        assertEqual
          [ BufferOpened 1
          , BufferEntered 1
          , ModeChanged Normal Insert
          , BufferChanged 1 1
          , ModeChanged Insert Normal
          , ProcessOutput "p" "a"
          , ProcessOutput "p" "b"
          , ProcessExited "p" 0
          ]
          events
    , test "a segment, a sign and an annotation are drawn" $
        let ed0 = newEditor (6, 40) (newDocument Nothing (buf "hello\nworld"))
            ui =
              emptyPluginUI
                { puSegments = [segment "SEG"]
                , puSigns = IntMap.singleton 1 [SignSpan 1 2 (sign "*" 1)]
                , puAnnotations = IntMap.singleton 1 [Annotation 0 "note" (face "comment")]
                }
            ed = ed0 {edPluginUI = Map.singleton "t" ui, edSignLane = True}
            rows = [rowText (render defaultTheme Nothing ed) r | r <- [0 .. 5]]
         in assertEqual (True, True, True) (any (T.isInfixOf "hello note") rows, any (T.isPrefixOf "*") rows, any (T.isInfixOf "SEG") rows)
    ]
  where
    sign t p = GutterSign t (face "diff.plus") p
