-- | What the model may do in the chat (ADR ai-chat): read files, list the
-- project's files, and propose edits. Reading happens at once; an edit is
-- applied to the file's buffer as a pending edit ("Him.Chat") that the
-- user approves or denies. Pure: the tool definitions, checking a call's
-- input, and making and undoing an edit on a document.
module Him.Chat.Tools
  ( chatTools
  , systemPrompt
  , ToolRequest (..)
  , parseToolCall
  , editBuffer
  , writeBuffer
  , proposedResult
  , toolResult
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Chat
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

-- | The buffer after an @edit_file@: one exact occurrence of the old text
-- replaced, or why not.
editBuffer :: FilePath -> Text -> Text -> Buffer.Buffer -> Either Text Buffer.Buffer
editBuffer path old new buf
  | T.null old = Left "old_text is empty"
  | otherwise = case T.breakOnAll old text of
      [] -> Left ("old_text was not found in " <> T.pack path)
      [(before, _)] -> Right (Buffer.fromText (before <> new <> T.drop (T.length before + T.length old) text))
      _ -> Left ("old_text occurs more than once in " <> T.pack path <> "; include more context")
  where
    text = Buffer.toText buf

-- | The buffer after a @write_file@.
writeBuffer :: Text -> Buffer.Buffer
writeBuffer content = Buffer.fromText (fromMaybe content (T.stripSuffix "\n" content))

-- | What the model is told when it proposes a change: it may go on as if
-- the change were made; the user reviews them after the turn.
proposedResult :: FilePath -> Text
proposedResult path =
  "Proposed as a change to " <> T.pack path <> ". The user reviews proposed changes after your turn; until approved, the file on disk is unchanged. Go on as if it were applied (read_file shows it)."

-- | A tool result for the history.
toolResult :: Text -> Bool -> Text -> Value
toolResult toolId isError content =
  object ([("type", JString "tool_result"), ("tool_use_id", JString toolId), ("content", JString content)] <> [("is_error", JBool True) | isError])
