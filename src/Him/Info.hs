-- | The info box: what can be typed next (see 'InfoBox').
--
-- After a prefix key (@g@, @space@) it lists the keys that may follow and
-- what they do, like Helix's autoinfo. On the @:@ line it lists the
-- commands matching the typed name, or the candidates of the last @tab@.
-- It is recomputed after every key from the editor state and the config,
-- so it never goes stale.
module Him.Info
  ( refreshInfo
  , keyInfo
  , exInfo
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Him.Action
import Him.Config (Config (..))
import Him.Editor
import Him.Ex (ExCommand (..))
import Him.Key (showKey, showKeys)
import Him.Keymap (children, lookupPrefix)
import Him.Mode (Mode (..))

refreshInfo :: Config -> Editor -> Editor
refreshInfo config ed = ed {edInfo = box}
  where
    box = case (edPending ed, edMode ed, edPrompt ed) of
      -- Keys that are typed when the chord does not go on ("j j" in
      -- insert mode) are typing, not a menu to show.
      (ks@(_ : _), mode, _) | all (isJust . cfgFallback config mode) ks -> Nothing
      (_ : _, _, _) -> keyInfo config ed
      ([], CmdLine, ExPrompt) -> exInfo (cfgExCommands config) ed
      _ -> Nothing

-- | The keys that can follow the pending ones.
keyInfo :: Config -> Editor -> Maybe InfoBox
keyInfo config ed = do
  keymap <- Map.lookup (keymapMode ed) (cfgKeymaps config)
  sub <- lookupPrefix keymap pending
  pure
    InfoBox
      { infoTitle = fromMaybe (showKeys pending) (Map.lookup pending names)
      , infoRows = [(showKey k, describe k b) | (k, b) <- children sub]
      , infoPlace = BottomRight
      }
  where
    pending = edPending ed
    names = cfgPrefixNames config
    describe k = \case
      Just bound -> boundDoc (boundInvocation bound)
      Nothing -> "+" <> fromMaybe "more" (Map.lookup (pending <> [k]) names)
    boundDoc inv = case lookupAction (invAction inv) (cfgActions config) of
      Just a
        | null (invArgs inv) -> actDoc a
        | otherwise -> actDoc a <> " (" <> T.unwords (invArgs inv) <> ")"
      Nothing -> renderInvocation inv

-- | While the command name is typed: the commands it could be. After it:
-- the command's description, or the candidates of the last @tab@.
exInfo :: [ExCommand] -> Editor -> Maybe InfoBox
exInfo table ed
  | not (null (edCompletions ed)) =
      Just (InfoBox "complete" [(c, "") | c <- edCompletions ed] BottomLeft)
  | T.null rest = case matching of
      [] -> Nothing
      cs -> Just (InfoBox "commands" (map row cs) BottomLeft)
  | otherwise = case [c | c <- table, name `elem` exNames c] of
      c : _ -> Just (InfoBox "command" [row c] BottomLeft)
      [] -> Nothing
  where
    (name, rest) = T.break (== ' ') (edCmdLine ed)
    matching = [c | c <- table, any (name `T.isPrefixOf`) (exNames c)]
    row c = (T.intercalate ", " (exNames c), exDoc c)
