-- | The bottom row: the @:@ command being typed, or a status message.
module Him.Render.CommandLine
  ( drawCommandLine
  , commandLineCursor
  ) where

import Data.List (sortOn)
import Data.Text qualified as T
import Him.Document (Document (..))
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnostics)
import Him.Position (Pos (..))
import Him.Selection (primary, rangeHead)
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
  -- Otherwise the diagnostic on the cursor's line, if any.
  _ -> case diagnosticsHere ed of
    sd : _ -> putText (rectRow rect) (rectCol rect) (themeDiagnostic theme (sdSeverity sd)) (T.take (rectWidth rect) (T.unwords (T.lines (sdMessage sd))))
    [] -> id

promptLabel :: Editor -> T.Text
promptLabel ed = case edPrompt ed of
  ExPrompt -> ":"
  SearchPrompt Forward _ -> "/"
  SearchPrompt Backward _ -> "?"
  SelectPrompt _ -> "select:"
  FilePrompt act -> fileActionLabel act
  RenamePrompt _ -> "rename to: "

commandLineCursor :: Editor -> Rect -> (Int, Int)
commandLineCursor ed rect =
  (rectRow rect, rectCol rect + min (rectWidth rect - 1) (T.length (promptLabel ed) + T.length (edCmdLine ed)))

-- | What the command line shows for a file operation.
fileActionLabel :: FileAction -> T.Text
fileActionLabel = \case
  NewFile _ -> "new file: "
  NewDirectory _ -> "new directory: "
  RenameEntry _ old -> "rename " <> T.pack old <> " to: "
  DeleteEntries _ [name] -> "delete " <> T.pack name <> "? [y/N] "
  DeleteEntries _ names -> "delete " <> T.pack (show (length names)) <> " entries? [y/N] "

-- | The diagnostics on the cursor's line, most severe first.
diagnosticsHere :: Editor -> [ShownDiagnostic]
diagnosticsHere ed =
  let d = edDoc ed
      Pos l _ = rangeHead (primary (docSelection d))
   in sortOn sdSeverity [sd | sd <- shownDiagnostics (edLsp ed) (docLsp d) (docBuffer d), sdLine sd == l]
