-- | What the model may do in the chat (ADR-41): read files, list the
-- project's files, and propose edits. Reading happens at once; an edit is
-- applied to the file's buffer as a pending edit ("Him.Chat") that the
-- user approves or denies. Pure: the tool definitions, checking a call's
-- input, and making and undoing an edit on a document.
module Him.Chat.Tools
  ( chatTools
  , systemPrompt
  , ToolRequest (..)
  , parseToolCall
  , proposeEdit
  , proposeWrite
  , revertEdit
  , editSummary
  , toolResult
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Chat
import Him.Document (Document (..), clampSelection, replaceBuffer)
import Him.Json hiding (path)

-- | The tools, in the Messages API's shape.
chatTools :: [Value]
chatTools =
  [ tool "read_file" "Read a file of the project (as the editor has it, with unsaved changes)." [("path", "Path relative to the project root")]
  , tool "list_files" "List the project's files (honouring .gitignore)." []
  , tool
      "edit_file"
      "Replace one occurrence of old_text in a file with new_text. old_text must occur exactly once; include enough surrounding lines to make it unique. The user sees the change in the editor and approves or rejects it."
      [("path", "Path relative to the project root"), ("old_text", "The exact text to replace"), ("new_text", "The replacement")]
  , tool
      "write_file"
      "Create a file, or replace a file's whole content. The user approves or rejects it in the editor."
      [("path", "Path relative to the project root"), ("content", "The full content of the file")]
  ]
  where
    tool name desc params =
      object
        [ ("name", JString name)
        , ("description", JString desc)
        , ( "input_schema"
          , object
              [ ("type", JString "object")
              , ("properties", object [(p, object [("type", JString "string"), ("description", JString d)]) | (p, d) <- params])
              , ("required", JArray [JString p | (p, _) <- params])
              ]
          )
        ]

-- | The system prompt: where the model is and how its edits are handled.
systemPrompt :: FilePath -> Text
systemPrompt root =
  T.unlines
    [ "You are an assistant inside him, a modal terminal text editor. The user chats with you in a split beside their code."
    , "The project root is " <> T.pack root <> "; paths are relative to it."
    , "Use read_file and list_files to look at the code. To change code, use edit_file (preferred, for focused changes) or write_file (new files, whole rewrites)."
    , "Each edit is shown to the user in the editor, and they approve or reject it before you continue; if they reject one, ask or propose something else."
    , "Keep replies short: they are read in a narrow window. Use plain text; code in fenced blocks."
    ]

-- | A tool call with its input checked (the input streamed without
-- server-side validation, so it is checked here before anything runs).
data ToolRequest
  = ReadFile !FilePath
  | ListFiles
  | EditFile !FilePath !Text !Text
  | WriteFile !FilePath !Text
  deriving stock (Eq, Show)

parseToolCall :: ToolCall -> Either Text ToolRequest
parseToolCall call = case tcInput call of
  Left raw -> Left (renderText (object [("INVALID_JSON", JString raw)]))
  Right input -> case tcName call of
    "read_file" -> ReadFile <$> str "path" input
    "list_files" -> Right ListFiles
    "edit_file" -> EditFile <$> str "path" input <*> field "old_text" input <*> field "new_text" input
    "write_file" -> WriteFile <$> str "path" input <*> field "content" input
    other -> Left ("unknown tool " <> other)
  where
    field k v = maybe (Left ("missing string field " <> k)) Right (key k v >>= asText)
    str k v = T.unpack <$> field k v
    renderText = decodeUtf8Lenient . renderJson

-- | Apply an @edit_file@ to a document as a pending edit (an undoable
-- change of whole lines), or say why it cannot be applied.
proposeEdit :: Int -> Text -> FilePath -> Text -> Text -> Document -> Either Text (PendingEdit, Document)
proposeEdit number toolId path old new d
  | T.null old = Left "old_text is empty"
  | otherwise = case T.breakOnAll old text of
      [] -> Left ("old_text was not found in " <> T.pack path)
      [(before, _)] ->
        let startLine = T.count "\n" before
            endLine = startLine + T.count "\n" old
            oldLines = [Buffer.lineAt l buf | l <- [startLine .. endLine]]
            lineStart = T.takeWhileEnd (/= '\n') before
            lineEnd = T.drop (T.length lineStart + T.length old) (T.intercalate "\n" oldLines)
            newLines = T.splitOn "\n" (lineStart <> new <> lineEnd)
         in Right (apply startLine oldLines newLines)
      _ -> Left ("old_text occurs more than once in " <> T.pack path <> "; include more context")
  where
    buf = docBuffer d
    text = Buffer.toText buf
    apply l oldLines newLines =
      let buf' = Buffer.replaceLines l (l + length oldLines) newLines buf
       in ( PendingEdit number toolId path (docId d) l oldLines newLines Undecided
          , replaceBuffer buf' (clampSelection buf' (docSelection d)) d
          )

-- | Apply a @write_file@: the whole content replaced.
proposeWrite :: Int -> Text -> FilePath -> Text -> Document -> (PendingEdit, Document)
proposeWrite number toolId path content d =
  let buf = docBuffer d
      oldLines = Buffer.toLines buf
      newLines = T.splitOn "\n" (fromMaybe content (T.stripSuffix "\n" content))
      buf' = Buffer.replaceLines 0 (length oldLines) newLines buf
   in (PendingEdit number toolId path (docId d) 0 oldLines newLines Undecided, replaceBuffer buf' (clampSelection buf' (docSelection d)) d)

-- | Put the old lines back (a denied edit), as an undoable change.
revertEdit :: PendingEdit -> Document -> Document
revertEdit pe d =
  let buf = docBuffer d
      buf' = Buffer.replaceLines (peLine pe) (peLine pe + length (peNew pe)) (peOld pe) buf
   in replaceBuffer buf' (clampSelection buf' (docSelection d)) d

-- | How an edit is shown in the chat: a heading and its lines as a diff.
editSummary :: PendingEdit -> Text
editSummary pe =
  T.unlines $
    [ "[edit #" <> T.pack (show (peNumber pe)) <> " " <> T.pack (pePath pe) <> ":" <> T.pack (show (peLine pe + 1)) <> ", -" <> T.pack (show (length (peOld pe))) <> " +" <> T.pack (show (length (peNew pe))) <> " lines]"
    ]
      <> take 12 (["- " <> l | l <- peOld pe] <> ["+ " <> l | l <- peNew pe])
      <> ["  …" | length (peOld pe) + length (peNew pe) > 12]

-- | A tool result for the history.
toolResult :: Text -> Bool -> Text -> Value
toolResult toolId isError content =
  object ([("type", JString "tool_result"), ("tool_use_id", JString toolId), ("content", JString content)] <> [("is_error", JBool True) | isError])
