-- | The AI chat (ADR ai-chat): one interface for chat providers, and the state
-- of a chat buffer. Pure. The provider for the Claude API is
-- "Him.Chat.Anthropic"; the tools the model may call and the edits it
-- proposes are "Him.Chat.Tools"; the plugin is "Him.Actions.Chat".
module Him.Chat
  ( -- * Providers
    ChatProvider (..)
  , ChatSession (..)
  , ChatRequest (..)
  , ChatEvent (..)
  , ToolCall (..)
  , ChatConfig (..)
  , defaultChatConfig
    -- * A chat buffer
  , ChatState (..)
  , ChatStatus (..)
  , ChatMark (..)
  , Review (..)
  , newChatState
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Him.Diff (Hunk)
import Him.Json (Value)
import Him.Position (Pos (..))

-- | Something that answers a conversation: the Claude API, Claude Code, or
-- a fake in the tests. Each chat buffer gets its own session ('cpStart'),
-- like a highlighter (ADR syntax-providers), so a provider can keep a process or a
-- conversation of its own.
data ChatProvider = ChatProvider
  { cpName :: Text
  , cpStart :: IO ChatSession
  }

-- | A conversation with a provider.
data ChatSession = ChatSession
  { sessSend :: ChatConfig -> ChatRequest -> (ChatEvent -> IO ()) -> IO (IO ())
  -- ^ Start a turn and return at once; events arrive through the callback,
  -- ending with 'ChatFinished' or 'ChatFailed'. The returned action cancels
  -- the turn.
  , sessAnswer :: Text -> Bool -> Text -> IO ()
  -- ^ Answer a 'ChatToolCall' (call id, is it an error, the result), for a
  -- provider whose model waits for tools during the turn.
  , sessClose :: IO ()
  }

-- | What is sent: a system prompt, the conversation so far (messages in the
-- Messages API's shape, appended to, never edited), and the tools.
data ChatRequest = ChatRequest
  { crSystem :: !Text
  , crMessages :: ![Value]
  , crTools :: ![Value]
  }
  deriving stock (Eq, Show)

data ChatEvent
  = -- | Visible text, as it is generated.
    ChatText !Text
  | -- | The reply is complete: why it stopped, the assistant message to
    -- append to the history (as the provider returned it, thinking blocks
    -- included), and the tools it asks to call (answered with the next
    -- request).
    ChatFinished !Text !Value ![ToolCall]
  | -- | A tool the model waits for now, in the middle of the turn (Claude
    -- Code through MCP); answered with 'sessAnswer'.
    ChatToolCall !ToolCall
  | ChatFailed !Text
  | -- | Something the model did that the editor did not do for it (Claude
    -- Code's own search tools), to show.
    ChatActivity !Text
  deriving stock (Eq, Show)

-- | A tool call: its id, the tool, and the input (or the raw text when it
-- was not valid JSON).
data ToolCall = ToolCall
  { tcId :: !Text
  , tcName :: !Text
  , tcInput :: !(Either Text Value)
  }
  deriving stock (Eq, Show)

-- | @[chat]@ in the config file.
data ChatConfig = ChatConfig
  { ccProvider :: !Text
  , ccModel :: !Text
  , ccEffort :: !Text
  , ccMaxTokens :: !Int
  }
  deriving stock (Eq, Show)

defaultChatConfig :: ChatConfig
defaultChatConfig = ChatConfig "claude-code" "claude-opus-5-5" "high" 64000

data ChatStatus
  = ChatIdle
  | -- | A turn is running.
    ChatWaiting
  deriving stock (Eq, Show)

-- | A document with changes the model proposed (ADR change-review): its text before
-- the first of them (the base, which is also what is on disk for an
-- approved state), and the proposed changes, the diff from the base to the
-- buffer. Approving a change applies it to the base and writes the base;
-- denying puts the base's lines back in the buffer. Edits by hand in
-- between simply become part of the diff.
data Review = Review
  { rvDoc :: !Int
  , rvPath :: !FilePath
  , rvBase :: ![Text]
  , rvHunks :: ![Hunk]
  -- ^ Base to buffer, for the buffer's version 'rvVersion'.
  , rvVersion :: !Int
  }
  deriving stock (Eq, Show)

-- | What a line of the transcript is, for how it is drawn
-- ("Him.Chat.Transcript"). The model's prose has no mark.
data ChatMark
  = MarkWelcome
  | -- | "You", above a message.
    MarkUser
  | MarkUserText
  | -- | What the editor told the model with the message (the file).
    MarkContext
  | -- | "Claude", above an answer.
    MarkClaude
  | -- | Something the model did: read a file, searched.
    MarkTool
  | -- | A change it proposed.
    MarkChange
  | -- | The changes waiting for review, at the end of a turn.
    MarkReview
  | MarkNote
  | MarkError
  | -- | A code block's fence (@```@) and its lines.
    MarkFence
  | MarkCode
  deriving stock (Eq, Show, Enum, Bounded)

-- | A chat buffer: the transcript, then the prompt and what is typed after
-- 'csInput' (the next message).
data ChatState = ChatState
  { csInput :: !Pos
  , csStatus :: !ChatStatus
  , csHistory :: ![Value]
  -- ^ The conversation, append-only.
  , csReviews :: ![Review]
  -- ^ Documents with proposed changes.
  , csDecisions :: ![Text]
  -- ^ What the user decided since the last message (told to the model
  -- with the next one).
  , csMarks :: !(IntMap ChatMark)
  -- ^ The transcript's lines that are not the model's prose.
  , csFence :: !Bool
  -- ^ The transcript's last line is inside a code block.
  , csSent :: ![Text]
  -- ^ The messages sent, the last first (recalled with up and down).
  , csRecall :: !Int
  -- ^ Which of them is in the input (-1: none).
  }
  deriving stock (Eq, Show)

newChatState :: ChatState
newChatState = ChatState (Pos 0 0) ChatIdle [] [] [] IntMap.empty False [] (-1)
