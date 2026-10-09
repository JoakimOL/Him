-- | What plugins show (ADR plugin-building-blocks, ADR plugin-canvas): status line segments,
-- gutter signs, annotations at the end of lines, highlights in a buffer's text,
-- a buffer's own keys, and a canvas (a box in the middle of the screen the
-- plugin fills cell by cell). A plugin sets them as data; the core
-- draws them, so rendering stays pure and plugins cannot draw over each
-- other. Pure.
module Him.PluginUI
  ( PluginUI (..)
  , emptyPluginUI
  , Face (..)
  , face
  , Segment (..)
  , Side (..)
  , segment
  , GutterSign (..)
  , SignSpan (..)
  , Annotation (..)
  , Highlight (..)
  , Canvas (..)
  , canvas
  , OpenCanvas (..)
  , segmentsFor
  , signsIn
  , annotationsIn
  , highlightsIn
  , keymapOf
  , dropDocuments
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)

-- | How something looks: a theme scope (@diff.plus@, @error@, @comment@,
-- @ui.text.focus@; a missing scope falls back to its parent, as in
-- Helix themes). With the flag, a scope the theme lacks is drawn as its
-- parent, dimmed (git's staged signs: @diff.plus.staged@).
data Face = Face
  { faceScope :: !Text
  , faceDimFallback :: !Bool
  }
  deriving stock (Eq, Show)

face :: Text -> Face
face scope = Face scope False

data Side = SideLeft | SideRight
  deriving stock (Eq, Show)

-- | A part of the status line: left ones follow the file name, right ones
-- come before the cursor position. When the line is too narrow, the
-- lowest priority goes first.
data Segment = Segment
  { segSide :: !Side
  , segText :: !Text
  , segFace :: !(Maybe Face)
  -- ^ 'Nothing': the status line's own colours.
  , segPriority :: !Int
  , segDoc :: !(Maybe Int)
  -- ^ Only in windows showing this document; 'Nothing': in every window.
  }
  deriving stock (Eq, Show)

-- | A right-side segment for every window, of priority 0.
segment :: Text -> Segment
segment t = Segment SideRight t Nothing 0 Nothing

-- | One cell in the gutter's sign lane. Where signs overlap, the higher
-- priority wins (diagnostics win over all of them).
data GutterSign = GutterSign
  { gsText :: !Text
  , gsFace :: !Face
  , gsPriority :: !Int
  }
  deriving stock (Eq, Show)

-- | A sign on the lines @[ssFrom, ssTo)@.
data SignSpan = SignSpan
  { ssFrom :: !Int
  , ssTo :: !Int
  , ssSign :: !GutterSign
  }
  deriving stock (Eq, Show)

-- | Text shown after the end of a line (virtual text: it is not in the
-- buffer).
data Annotation = Annotation
  { anLine :: !Int
  , anText :: !Text
  , anFace :: !Face
  }
  deriving stock (Eq, Show)

-- | Characters @[hlFrom, hlTo)@ of a line drawn in a face, over the
-- syntax highlighting (a plugin's own colouring of a buffer, e.g. a git
-- status buffer's sections and diff lines).
data Highlight = Highlight
  { hlLine :: !Int
  , hlFrom :: !Int
  , hlTo :: !Int
  , hlFace :: !Face
  }
  deriving stock (Eq, Show)

-- | A box in the middle of the screen that a plugin draws cell by cell
-- (a game, a dashboard). While it is open it has the keys: those its
-- keymap binds run their actions, the others reach the plugin as
-- 'Him.PluginEvent.CanvasKey'; an unbound @esc@ closes it.
data Canvas = Canvas
  { canvasTitle :: !Text
  , canvasWidth :: !Int
  , canvasHeight :: !Int
  -- ^ The inside, in cells (the border is extra). It shrinks to fit the
  -- screen.
  , canvasRows :: ![[(Text, Face)]]
  -- ^ Top to bottom, each row a run of texts in faces (one cell per
  -- character); what is missing is blank.
  , canvasKeymap :: !(Maybe Text)
  -- ^ The plugin's keymap to use while it is open (its name in
  -- 'Him.Plugin.psKeymaps').
  }
  deriving stock (Eq, Show)

-- | An empty canvas of a title and an inside size.
canvas :: Text -> Int -> Int -> Canvas
canvas title w h = Canvas title w h [] Nothing

-- | The canvas on screen: its plugin and name, and what it shows. Its
-- keymap is the full name (@plugin:keymap@).
data OpenCanvas = OpenCanvas
  { ocOwner :: !Text
  , ocName :: !Text
  , ocCanvas :: !Canvas
  }
  deriving stock (Eq, Show)

-- | One plugin's part of the screen.
data PluginUI = PluginUI
  { puSegments :: ![Segment]
  , puSigns :: !(IntMap [SignSpan])
  -- ^ By document id.
  , puAnnotations :: !(IntMap [Annotation])
  -- ^ By document id.
  , puHighlights :: !(IntMap (IntMap [Highlight]))
  -- ^ By document id, then line.
  , puKeymaps :: !(IntMap Text)
  -- ^ The keymap (full name, @plugin:keymap@) over normal mode's in a
  -- document, by document id.
  }
  deriving stock (Eq, Show)

emptyPluginUI :: PluginUI
emptyPluginUI = PluginUI [] IntMap.empty IntMap.empty IntMap.empty IntMap.empty

-- | The segments for a window showing a document, best first.
segmentsFor :: Int -> Map Text PluginUI -> [Segment]
segmentsFor doc uis =
  sortOn (Down . segPriority) [s | ui <- Map.elems uis, s <- puSegments ui, maybe True (== doc) (segDoc s)]

-- | The sign on each of a document's lines in @[from, to]@ (the highest
-- priority where several plugins' signs overlap).
signsIn :: Int -> Int -> Int -> Map Text PluginUI -> IntMap GutterSign
signsIn doc from to uis =
  IntMap.fromListWith
    (\a b -> if gsPriority a >= gsPriority b then a else b)
    [ (l, ssSign s)
    | ui <- Map.elems uis
    , s <- IntMap.findWithDefault [] doc (puSigns ui)
    , ssTo s > from
    , ssFrom s <= to
    , l <- [max from (ssFrom s) .. min to (ssTo s - 1)]
    ]

-- | The annotations on a document's lines in @[from, to]@, by line, in
-- plugin order.
annotationsIn :: Int -> Int -> Int -> Map Text PluginUI -> IntMap [Annotation]
annotationsIn doc from to uis =
  IntMap.fromListWith
    (flip (<>))
    [ (anLine a, [a])
    | ui <- Map.elems uis
    , a <- IntMap.findWithDefault [] doc (puAnnotations ui)
    , anLine a >= from
    , anLine a <= to
    ]

-- | The highlights on a document's lines in @[from, to]@, by line; an
-- earlier plugin's win where they overlap.
highlightsIn :: Int -> Int -> Int -> Map Text PluginUI -> IntMap [Highlight]
highlightsIn doc from to uis =
  IntMap.unionsWith
    (<>)
    [ fst (IntMap.split (to + 1) (snd (IntMap.split (from - 1) byLine)))
    | ui <- Map.elems uis
    , Just byLine <- [IntMap.lookup doc (puHighlights ui)]
    ]

-- | The keymap a plugin set for a document, if any (the first plugin's).
keymapOf :: Int -> Map Text PluginUI -> Maybe Text
keymapOf doc uis = case [k | ui <- Map.elems uis, Just k <- [IntMap.lookup doc (puKeymaps ui)]] of
  k : _ -> Just k
  [] -> Nothing

-- | Forget what the plugins show for documents that are no longer open.
dropDocuments :: IntSet -> Map Text PluginUI -> Map Text PluginUI
dropDocuments open = Map.map $ \ui ->
  ui
    { puSigns = IntMap.restrictKeys (puSigns ui) open
    , puAnnotations = IntMap.restrictKeys (puAnnotations ui) open
    , puHighlights = IntMap.restrictKeys (puHighlights ui) open
    , puKeymaps = IntMap.restrictKeys (puKeymaps ui) open
    , puSegments = [s | s <- puSegments ui, maybe True (`IntSet.member` open) (segDoc s)]
    }
