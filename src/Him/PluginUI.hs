-- | What plugins show (ADR-50): status line segments, gutter signs and
-- annotations at the end of lines. A plugin sets them as data; the core
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
  , segmentsFor
  , signsIn
  , annotationsIn
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

-- | One plugin's part of the screen.
data PluginUI = PluginUI
  { puSegments :: ![Segment]
  , puSigns :: !(IntMap [SignSpan])
  -- ^ By document id.
  , puAnnotations :: !(IntMap [Annotation])
  -- ^ By document id.
  }
  deriving stock (Eq, Show)

emptyPluginUI :: PluginUI
emptyPluginUI = PluginUI [] IntMap.empty IntMap.empty

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

-- | Forget what the plugins show for documents that are no longer open.
dropDocuments :: IntSet -> Map Text PluginUI -> Map Text PluginUI
dropDocuments open = Map.map $ \ui ->
  ui
    { puSigns = IntMap.restrictKeys (puSigns ui) open
    , puAnnotations = IntMap.restrictKeys (puAnnotations ui) open
    , puSegments = [s | s <- puSegments ui, maybe True (`IntSet.member` open) (segDoc s)]
    }
