-- | Registers: where yanks go and pastes come from. @\"@ is the default,
-- @\" a@ picks another for the next command (any character; it is
-- created when yanked to), @_@ discards, @/@ holds the last search, and
-- @+@ / @*@ are the system clipboard and primary selection
-- ("Him.Clipboard"). @:registers@ lists them, @:clear-register@ forgets
-- them.
module Him.Actions.Register
  ( actions
  , exCommands
  , yank
  , selectedRegister
  , useRegister
  , awaitedRegister
  , clipboardSet
  , clipboardGet
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Clipboard (ClipboardKind (..), ClipboardProvider (..), chooseProvider, joinValues)
import Him.Document (Document (..))
import Him.Edit (Edit, insertAtHead, pasteAfter, pasteBefore, replaceWith, selectionText)
import Him.Editor (Await (..), Editor (..), InfoBox (..), InfoPlace (..))
import Him.EditorM
import Him.Effect (Effect (..), RegisterUse (..))
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Info (registerRows)
import Him.Mode (Mode (..))
import Him.Options (Options (..))
import Him.Selection (rangeCount, ranges)

actions :: [Action]
actions =
  [ simple "select_register" GClipboard "Use a register for the next command (the next key names it: a-z, + clipboard, * primary, _ none…)" $
      modify' (\e -> e {edAwait = Just AwaitRegister})
  , simple "insert_register" GClipboard "Insert a register's text (the next key names it)" $
      modify' (\e -> e {edAwait = Just AwaitInsertRegister})
  , simple "yank" GClipboard "Copy the selection into the register" (yankReporting =<< selectedRegister)
  , simple "paste_after" GClipboard "Paste after each selection" (useRegister UsePasteAfter =<< selectedRegister)
  , simple "paste_before" GClipboard "Paste before each selection" (useRegister UsePasteBefore =<< selectedRegister)
  , simple "replace_with_yanked" GClipboard "Replace each selection with the register's text" (useRegister UseReplace =<< selectedRegister)
  , simple "replace_with_clipboard" GClipboard "Replace each selection with the system clipboard (+)" (useRegister UseReplace '+')
  , simple "yank_to_clipboard" GClipboard "Copy the selection to the system clipboard (+)" (yankReporting '+')
  , simple "paste_clipboard_after" GClipboard "Paste the system clipboard after each selection" (useRegister UsePasteAfter '+')
  , simple "paste_clipboard_before" GClipboard "Paste the system clipboard before each selection" (useRegister UsePasteBefore '+')
  , simple "show_registers" GClipboard "List the registers and what they hold" showRegisters
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["registers", "reg"] "List the registers and what they hold" NoArgs $ \_ -> showRegisters
  , ExCommand ["clear-register", "clear-registers"] "Forget registers (e.g. :clear-register a +); without a name, all of them" NoArgs $ \case
      [] -> do
        modify' (\e -> e {edRegisters = Map.empty})
        info "cleared every register"
      names -> do
        let cs = concatMap T.unpack names
        modify' (\e -> e {edRegisters = foldr Map.delete (edRegisters e) cs})
        info ("cleared " <> T.unwords [T.singleton c | c <- cs])
  ]

-- | The register chosen with @\"@, else the default.
selectedRegister :: EditorM Char
selectedRegister = gets (fromMaybe defaultRegister . edSelectedRegister)

defaultRegister :: Char
defaultRegister = '"'

-- | The key after @\"@ (the register for the next command) or after
-- insert mode's @C-r@ (the register to insert).
awaitedRegister :: Await -> Char -> EditorM ()
awaitedRegister waiting c = case waiting of
  AwaitInsertRegister -> useRegister UseInsert c
  _ -> modify' (\e -> e {edSelectedRegister = Just c})

-- | Copy every range into a register, one value per range.
yank :: Char -> EditorM ()
yank c = do
  d <- getDoc
  writeRegister c (map (selectionText (docBuffer d)) (ranges (docSelection d)))

yankReporting :: Char -> EditorM ()
yankReporting c = do
  yank c
  vs <- getRegister c
  let to = if c == defaultRegister then "" else " to " <> T.singleton c
  info $ case vs of
    _ | c == '_' -> "discarded (register _)"
    [v] -> "yanked " <> T.pack (show (T.length v)) <> " characters" <> to
    _ -> "yanked " <> T.pack (show (length vs)) <> " selections" <> to

-- | @_@ keeps nothing; @+@ and @*@ also go to the system clipboard.
writeRegister :: Char -> [Text] -> EditorM ()
writeRegister c vs
  | c == '_' = pure ()
  | isClipboard c = setRegister c vs >> request (ClipboardSet c vs)
  | otherwise = setRegister c vs

isClipboard :: Char -> Bool
isClipboard c = c == '+' || c == '*'

-- | Use a register's values; the clipboard's are read first (the main loop
-- does that, 'clipboardGet').
useRegister :: RegisterUse -> Char -> EditorM ()
useRegister use c
  | isClipboard c = request (ClipboardGet c use)
  | otherwise = applyUse use c

applyUse :: RegisterUse -> Char -> EditorM ()
applyUse use c = do
  vs <- getRegister c
  case use of
    UseShowRegisters -> popupRegisters
    UseRefresh -> pure ()
    _ | null vs -> failWith ("register " <> T.singleton c <> " is empty")
    UsePasteAfter -> eachValue pasteAfter vs
    UsePasteBefore -> eachValue pasteBefore vs
    UseReplace -> eachValue replaceWith vs >> setMode Normal
    UseInsert -> eachValue insertAtHead vs

-- | With as many values as ranges, each range gets its own; otherwise
-- every range gets all of them, joined.
eachValue :: (Text -> Edit) -> [Text] -> EditorM ()
eachValue at vs = do
  n <- rangeCount . docSelection <$> getDoc
  let value i
        | length vs == n = vs !! i
        | otherwise = T.concat vs
  editEach (at . value)

-- | @:registers@: the clipboard registers listed are read again first.
showRegisters :: EditorM ()
showRegisters = do
  regs <- gets edRegisters
  case filter (`Map.member` regs) "+*" of
    [] -> popupRegisters
    cs -> do
      forM_ (init cs) (\c -> request (ClipboardGet c UseRefresh))
      request (ClipboardGet (last cs) UseShowRegisters)

popupRegisters :: EditorM ()
popupRegisters =
  gets registerRows >>= \case
    [] -> info "no registers yet (y yanks into \", \" a y into a)"
    rows -> modify' (\e -> e {edPopup = Just (InfoBox "registers" rows BottomRight Nothing)})

-- | Copy to the clipboard (the 'ClipboardSet' effect).
clipboardSet :: [ClipboardProvider] -> Char -> [Text] -> EditorM ()
clipboardSet providers c vs =
  withProvider providers $ \p ->
    liftIO (cbSet p (kindOf c) (joinValues vs)) >>= \case
      Right () -> pure ()
      Left err -> failWith ("clipboard (" <> cbName p <> "): " <> err)

-- | Read the clipboard into its register, then use it (the 'ClipboardGet'
-- effect). When it holds what was copied from here, the register keeps
-- one value per range. When it cannot be read, the register's last value
-- is used.
clipboardGet :: [ClipboardProvider] -> Char -> RegisterUse -> EditorM ()
clipboardGet providers c use = do
  name <- gets (optClipboardProvider . edOptions)
  provider <- liftIO (chooseProvider providers name)
  result <- case provider of
    Nothing -> pure (Left (noProvider name))
    Just p -> liftIO (cbGet p (kindOf c))
  cached <- getRegister c
  case result of
    Right t -> do
      when (joinValues cached /= t) $ setRegister c [t]
      applyUse use c
    Left err
      | null cached, use `elem` [UsePasteAfter, UsePasteBefore, UseReplace, UseInsert] -> failWith ("clipboard: " <> err)
      | otherwise -> applyUse use c

withProvider :: [ClipboardProvider] -> (ClipboardProvider -> EditorM ()) -> EditorM ()
withProvider providers k = do
  name <- gets (optClipboardProvider . edOptions)
  liftIO (chooseProvider providers name) >>= \case
    Just p -> k p
    Nothing -> failWith (noProvider name <> "; the register keeps it inside him")

noProvider :: Text -> Text
noProvider = \case
  "auto" -> "no clipboard found (wl-clipboard, xclip, xsel, pbcopy or tmux)"
  name -> "no clipboard provider named " <> name

kindOf :: Char -> ClipboardKind
kindOf c = if c == '*' then PrimarySelection else SystemClipboard
