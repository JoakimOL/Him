-- | Search actions: @/@ and @?@ prompts with an incremental preview, @n@ and
-- @N@ to repeat, @*@ to search for the selection.
module Him.Commands.Search
  ( actions
  , startSearch
  , executeSearch
  , executeSelect
  , cancelSearch
  , refreshSearchPreview
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Command
import Him.Document (Document (..))
import Him.Edit (selectionText)
import Him.Editor
import Him.Mode (Mode (..))
import Him.Search
import Him.Selection

actions :: [Action]
actions =
  [ simple "search_forward" GSearch "Search forward (/)" (startSearch Forward)
  , simple "search_backward" GSearch "Search backward (?)" (startSearch Backward)
  , simple "search_next" GSearch "Select the next match of the last search" (repeatSearch Forward)
  , simple "search_prev" GSearch "Select the previous match of the last search" (repeatSearch Backward)
  , simple "search_selection" GSearch "Use the selection as the search pattern" $ do
      d <- getDoc
      let t = selectionText (docBuffer d) (primary (docSelection d))
      if T.any (== '\n') t
        then failWith "cannot search for text spanning lines"
        else do
          setSearchRegister t
          info ("search: " <> t)
  , simple "select_matches" GSelection "Select the matches of a pattern inside the selection (s)" $ do
      sel <- docSelection <$> getDoc
      modify' (\e -> e {edCmdLine = "", edPrompt = SelectPrompt sel, edPreviewPending = False})
      setMode CmdLine
  , action "search_text" GSearch "Search forward for the given text" (text "pattern") $ \pattern -> do
      setSearchRegister pattern
      repeatSearch Forward
  ]

searchRegister :: Char
searchRegister = '/'

setSearchRegister :: Text -> EditorM ()
setSearchRegister t = setRegister searchRegister [t]

lastSearch :: EditorM (Maybe Text)
lastSearch = listToMaybe <$> getRegister searchRegister

startSearch :: Direction -> EditorM ()
startSearch dir = do
  sel <- docSelection <$> getDoc
  modify' (\e -> e {edCmdLine = "", edPrompt = SearchPrompt dir sel, edPreviewPending = False})
  setMode CmdLine

-- | Enter on the search prompt. An empty pattern repeats the last search.
executeSearch :: Direction -> Selection -> Text -> EditorM ()
executeSearch dir origin typed = do
  pattern <-
    if T.null typed
      then fromMaybe "" <$> lastSearch
      else pure typed
  modify' (\e -> e {edPreviewPending = False})
  setSelection origin
  case compileNeedle pattern of
    Nothing -> failWith "no search pattern"
    Just needle -> do
      setSearchRegister pattern
      jump dir needle origin

-- | Enter on the @s@ prompt.
executeSelect :: Selection -> Text -> EditorM ()
executeSelect origin typed = do
  modify' (\e -> e {edPreviewPending = False})
  setSelection origin
  d <- getDoc
  case compileNeedle typed of
    Nothing -> failWith "no pattern"
    Just needle -> case selectMatches needle (docBuffer d) origin of
      Nothing -> failWith ("no matches: " <> typed)
      Just sel -> setSelection sel

cancelSearch :: Selection -> EditorM ()
cancelSearch origin = do
  modify' (\e -> e {edPreviewPending = False})
  setSelection origin

repeatSearch :: Direction -> EditorM ()
repeatSearch dir =
  lastSearch >>= \case
    Nothing -> failWith "no previous search (use / first)"
    Just pattern -> case compileNeedle pattern of
      Nothing -> failWith "no previous search (use / first)"
      Just needle -> getDoc >>= jump dir needle . docSelection

-- | Select the next match in a direction, searching from a selection.
jump :: Direction -> Needle -> Selection -> EditorM ()
jump dir needle sel = do
  d <- getDoc
  mode <- gets edMode
  case findMatch dir needle (docBuffer d) (rangeStart (primary sel)) of
    Nothing -> failWith ("pattern not found: " <> needleText needle)
    Just m -> do
      setSelection (selectMatch mode m sel)
      if matchWrapped m then info "search wrapped around" else pure ()

-- | The match becomes the primary range (in select mode it extends it).
selectMatch :: Mode -> Match -> Selection -> Selection
selectMatch mode m = modifyPrimary $ \r ->
  Range (if mode == Select then rangeAnchor r else matchStart m) (matchEnd m) Nothing

setSelection :: Selection -> EditorM ()
setSelection sel = modifyDoc (\d -> d {docSelection = sel})

-- | Incremental search: show the first match of the text typed so far,
-- searching from where the search started. Called once before rendering,
-- so a burst of typed keys searches once.
refreshSearchPreview :: Editor -> Editor
refreshSearchPreview ed = case (edPreviewPending ed, edMode ed, edPrompt ed) of
  (True, CmdLine, SearchPrompt dir origin) -> preview origin $ \needle buf -> do
    m <- findMatch dir needle buf (rangeStart (primary origin))
    pure (selectMatch Normal m origin)
  (True, CmdLine, SelectPrompt origin) -> preview origin $ \needle buf ->
    selectMatches needle buf origin
  _ -> ed {edPreviewPending = False}
  where
    preview origin f =
      let doc = edDoc ed
          shown = compileNeedle (edCmdLine ed) >>= \needle -> f needle (docBuffer doc)
       in ed
            { edPreviewPending = False
            , edDoc = doc {docSelection = fromMaybe origin shown}
            }
