-- | Effects: what an action asks for beyond changing the editor state
-- (ADR-23). Actions only queue them ('Him.Command.request'), so they stay
-- state changes that tests can inspect; the main loop carries them out.
--
-- Some are handled right after the key with the config at hand
-- ('RunAction', 'OpenPalette'); jobs run on background threads
-- ("Him.Runtime") and report back as 'JobResult' events.
module Him.Effect
  ( Effect (..)
  , Job (..)
  , JobKey (..)
  , jobKey
  , JobResult (..)
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Him.Invocation (Invocation)
import Him.Picker (PickerItem)

data Effect
  = -- | Run another action, e.g. the one chosen in the command palette.
    RunAction !Invocation
  | -- | Open the command palette (it lists the configured actions and keys).
    OpenPalette
  | -- | Start a background job; one already running with the same key is
    -- cancelled first.
    StartJob !Job
  | CancelJob !JobKey
  deriving stock (Eq, Show)

-- | Background work. Results carry the generation (and query) they were
-- started for, so an answer that arrives too late is recognised and
-- dropped.
data Job
  = -- | List the files below a directory for the picker of this generation.
    ScanFiles !Int !FilePath
  | -- | Rank a large picker's items for a query.
    FilterPicker !Int !Text !(Seq PickerItem)
  deriving stock (Eq, Show)

data JobKey = ScanJob | FilterJob
  deriving stock (Eq, Ord, Show)

jobKey :: Job -> JobKey
jobKey = \case
  ScanFiles {} -> ScanJob
  FilterPicker {} -> FilterJob

data JobResult
  = FilesFound !Int ![FilePath]
  | ScanFinished !Int
  | -- | Generation, query, best matches, total number of matches.
    PickerFiltered !Int !Text ![PickerItem] !Int
  deriving stock (Eq, Show)
