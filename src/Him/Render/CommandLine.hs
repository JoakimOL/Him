-- | The bottom row: the @:@ command being typed, or a status message.
module Him.Render.CommandLine
  ( drawCommandLine
  , commandLineCursor
  ) where

import Data.Text qualified as T
import Him.Editor
import Him.Mode (Mode (..))
import Him.Search (Direction (..))
import Him.Render.Frame
import Him.Render.Theme

drawCommandLine :: Theme -> Editor -> Rect -> Frame -> Frame
drawCommandLine theme ed rect = case (edMode ed, edStatus ed) of
  (CmdLine, _) -> putText (rectRow rect) (rectCol rect) (themeText theme) (promptLabel ed <> edCmdLine ed)
  (_, Just (Status Info msg)) -> putText (rectRow rect) (rectCol rect) (themeInfo theme) msg
  (_, Just (Status Error msg)) -> putText (rectRow rect) (rectCol rect) (themeError theme) msg
  _ -> id

promptLabel :: Editor -> T.Text
promptLabel ed = case edPrompt ed of
  ExPrompt -> ":"
  SearchPrompt Forward _ -> "/"
  SearchPrompt Backward _ -> "?"
  SelectPrompt _ -> "select:"

commandLineCursor :: Editor -> Rect -> (Int, Int)
commandLineCursor ed rect =
  (rectRow rect, rectCol rect + min (rectWidth rect - 1) (1 + T.length (edCmdLine ed)))
