-- | Splits (ADR window-splits): actions on windows, with Helix's names and keys
-- (@C-w@ or @space w@, then @v s w h j k l q o H J K L n@), and the @:@
-- commands @:vsplit@, @:hsplit@ and their @-new@ forms.
module Him.Actions.Window
  ( actions
  , exCommands
  , windowBindings
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Actions.File (openFile)
import Him.Buffer qualified as Buffer
import Him.Document (newDocument)
import Him.Editor
import Him.EditorM
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Window (Axis (..), Side (..), leaves, neighbour)

actions :: [Action]
actions =
  [ simple "vsplit" GWindows "Split the window: the same file side by side" (split Beside)
  , simple "hsplit" GWindows "Split the window: the same file above and below" (split Stacked)
  , simple "vsplit_new" GWindows "Split the window side by side, with a new scratch buffer" (splitNew Beside)
  , simple "hsplit_new" GWindows "Split the window above and below, with a new scratch buffer" (splitNew Stacked)
  , simple "rotate_view" GWindows "Focus the next window" (rotate 1)
  , simple "rotate_view_reverse" GWindows "Focus the previous window" (rotate (-1))
  , simple "jump_view_left" GWindows "Focus the window to the left" (jump SideLeft)
  , simple "jump_view_right" GWindows "Focus the window to the right" (jump SideRight)
  , simple "jump_view_up" GWindows "Focus the window above" (jump SideUp)
  , simple "jump_view_down" GWindows "Focus the window below" (jump SideDown)
  , simple "swap_view_left" GWindows "Swap the window with the one to the left" (modify' (swapWindow SideLeft))
  , simple "swap_view_right" GWindows "Swap the window with the one to the right" (modify' (swapWindow SideRight))
  , simple "swap_view_up" GWindows "Swap the window with the one above" (modify' (swapWindow SideUp))
  , simple "swap_view_down" GWindows "Swap the window with the one below" (modify' (swapWindow SideDown))
  , simple "wclose" GWindows "Close the window (not the last one)" $
      gets closeWindow >>= maybe (failWith "this is the only window (:q quits)") (modify' . const)
  , simple "wonly" GWindows "Close every other window" (modify' onlyWindow)
  ]

-- | Helix's window keys, after @C-w@ and after @space w@.
windowBindings :: [(Text, Text)]
windowBindings =
  [ (prefix <> " " <> k, a)
  | prefix <- ["C-w", "space w"]
  , (k, a) <-
      [ ("v", "vsplit")
      , ("C-v", "vsplit")
      , ("s", "hsplit")
      , ("C-s", "hsplit")
      , ("w", "rotate_view")
      , ("C-w", "rotate_view")
      , ("h", "jump_view_left")
      , ("C-h", "jump_view_left")
      , ("left", "jump_view_left")
      , ("j", "jump_view_down")
      , ("C-j", "jump_view_down")
      , ("down", "jump_view_down")
      , ("k", "jump_view_up")
      , ("C-k", "jump_view_up")
      , ("up", "jump_view_up")
      , ("l", "jump_view_right")
      , ("C-l", "jump_view_right")
      , ("right", "jump_view_right")
      , ("H", "swap_view_left")
      , ("J", "swap_view_down")
      , ("K", "swap_view_up")
      , ("L", "swap_view_right")
      , ("q", "wclose")
      , ("C-q", "wclose")
      , ("o", "wonly")
      , ("C-o", "wonly")
      , ("n v", "vsplit_new")
      , ("n s", "hsplit_new")
      ]
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["vsplit", "vs"] "Split the window side by side, showing the file(s) given (or the same one)" PathArgs (splitOpen Beside)
  , ExCommand ["hsplit", "hs"] "Split the window above and below, showing the file(s) given (or the same one)" PathArgs (splitOpen Stacked)
  , ExCommand ["vsplit-new", "vnew"] "Split the window side by side, with a new scratch buffer" NoArgs $ \_ -> splitNew Beside
  , ExCommand ["hsplit-new", "hnew"] "Split the window above and below, with a new scratch buffer" NoArgs $ \_ -> splitNew Stacked
  ]

split :: Axis -> EditorM ()
split axis = modify' (splitWindow axis)

-- | Split, and open a new scratch buffer in the new window.
splitNew :: Axis -> EditorM ()
splitNew axis = modify' (openBuffer (newDocument Nothing Buffer.empty) . splitWindow axis)

-- | One split per file (Helix's @:vsplit a b@); none given: the same file.
splitOpen :: Axis -> [Text] -> EditorM ()
splitOpen axis = \case
  [] -> split axis
  paths -> mapM_ (\p -> split axis >> openFile (T.unpack p)) paths

-- | Focus the window @n@ steps along the layout order, wrapping around.
rotate :: Int -> EditorM ()
rotate n = do
  ed <- get
  let ws = leaves (edLayout ed)
  case lookup (edFocus ed) (zip ws [0 ..]) of
    Just i -> modify' (focusWindow (ws !! ((i + n) `mod` length ws)))
    Nothing -> pure ()

jump :: Side -> EditorM ()
jump side = do
  ed <- get
  case neighbour side (edFocus ed) (windowBoxes ed) of
    Just w -> modify' (focusWindow w)
    Nothing -> pure ()
