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
  , registerRows
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
      _ | Just w <- edAwait ed, w `elem` [AwaitRegister, AwaitInsertRegister] ->
            Just (InfoBox "registers" (registerRows ed <> specials) BottomRight Nothing)
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
      , infoSelected = Nothing
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
  | Just cc <- edCompletions ed =
      Just (InfoBox "complete" [(c, "") | c <- ccShown cc] BottomLeft (ccSelected cc))
  | T.null rest = case matching of
      [] -> Nothing
      cs -> Just (InfoBox "commands" (map row cs) BottomLeft Nothing)
  | otherwise = case [c | c <- table, name `elem` exNames c] of
      c : _ -> Just (InfoBox "command" [row c] BottomLeft Nothing)
      [] -> Nothing
  where
    (name, rest) = T.break (== ' ') (edCmdLine ed)
    matching = [c | c <- table, any (name `T.isPrefixOf`) (exNames c)]
    row c = (T.intercalate ", " (exNames c), exDoc c)

-- | The registers that hold something, @\"@ first, each with the start of
-- its text (a register yanked from several ranges says how many).
registerRows :: Editor -> [(T.Text, T.Text)]
registerRows ed = [(T.singleton c, preview vs) | (c, vs) <- order (Map.toList (edRegisters ed))]
  where
    order rs = [r | r@(c, _) <- rs, c == '"'] <> [r | r@(c, _) <- rs, c /= '"']
    preview vs =
      let n = length vs
          count = if n > 1 then "[" <> T.pack (show n) <> "] " else ""
          flat = T.concatMap visible (T.intercalate " " vs)
       in count <> if T.length flat > width then T.take (width - 1) flat <> "…" else flat
    visible = \case
      '\n' -> "⏎"
      '\t' -> "→"
      c -> T.singleton c
    width = 50

-- | The registers that need no yank first, listed after @\"@.
specials :: [(T.Text, T.Text)]
specials = [("+", "system clipboard"), ("*", "primary selection"), ("_", "discard (black hole)")]
