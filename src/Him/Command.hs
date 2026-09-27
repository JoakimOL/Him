-- | Commands: named editor actions. Everything a key can do is a command,
-- so keys are bound to command /names/ (see "Him.Keymap") and new
-- functionality is added by adding commands to the registry.
module Him.Command
  ( EditorM
  , Command (..)
  , Registry
  , mkRegistry
    -- * Helpers for writing commands
  , getDoc
  , modifyDoc
  , setMode
  , info
  , failWith
  , edit
  , motion
  , quit
  ) where

import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Document (Document (..))
import Him.Edit (Edit)
import Him.Editor
import Him.Mode (Mode (..))
import Him.Motion (Motion, Movement (..), applyMotion)
import Him.Selection (mapRanges, modifyPrimary, primary)

-- | Commands run with access to the editor state and IO (for files etc.).
-- Keep the actual logic in pure modules ("Him.Motion", "Him.Edit") where
-- possible, and use these commands as thin wrappers.
type EditorM = StateT Editor IO

data Command = Command
  { cmdName :: !Text
  -- ^ snake_case, used in keymaps.
  , cmdDoc :: !Text
  , cmdRun :: EditorM ()
  }

type Registry = Map Text Command

mkRegistry :: [Command] -> Registry
mkRegistry cs = Map.fromList [(cmdName c, c) | c <- cs]

getDoc :: EditorM Document
getDoc = gets edDoc

modifyDoc :: (Document -> Document) -> EditorM ()
modifyDoc f = modify' (\e -> e {edDoc = f (edDoc e)})

setMode :: Mode -> EditorM ()
setMode m = modify' (\e -> e {edMode = m})

info :: Text -> EditorM ()
info t = modify' (\e -> e {edStatus = Just (Status Info t)})

failWith :: Text -> EditorM ()
failWith t = modify' (\e -> e {edStatus = Just (Status Error t)})

quit :: EditorM ()
quit = modify' (\e -> e {edQuit = True})

-- | Apply a pure edit to the primary range and mark the document modified.
-- (Multi-range edits need position mapping between ranges; they come with
-- multiple-selection support.)
edit :: Edit -> EditorM ()
edit f = modifyDoc $ \d ->
  let (buf, r) = f (docBuffer d) (primary (docSelection d))
   in d {docBuffer = buf, docSelection = modifyPrimary (const r) (docSelection d), docDirty = True}

-- | Apply a motion to every range. In select mode the ranges are extended.
motion :: Motion -> EditorM ()
motion m = do
  mode <- gets edMode
  let movement = if mode == Select then Extend else Move
  modifyDoc $ \d ->
    d {docSelection = mapRanges (applyMotion movement m (docBuffer d)) (docSelection d)}
