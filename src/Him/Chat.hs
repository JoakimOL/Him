-- | The AI chat (ADR-41): one interface for chat providers, and the state
-- of a chat buffer. Pure. The provider for the Claude API is
-- "Him.Chat.Anthropic"; the tools the model may call and the edits it
-- proposes are "Him.Chat.Tools"; the plugin is "Him.Actions.Chat".
module Him.Chat
  ( -- * Providers
    ChatProvider (..)
  , ChatRequest (..)
  , ChatEvent (..)
  , ToolCall (..)
  , ChatConfig (..)
  , defaultChatConfig
    -- * A chat buffer
  , ChatState (..)
  , ChatStatus (..)
  , PendingEdit (..)
  , EditDecision (..)
  , newChatState
  ) where

import Data.Text (Text)
import Him.Json (Value)
import Him.Position (Pos (..))

-- | Something that answers a conversation: the Claude API, or a fake in
-- the tests. 'cpSend' starts a request and returns at once; events arrive
-- through the callback, ending with 'ChatFinished' or 'ChatFailed'. The
-- returned action cancels the request.
data ChatProvider = ChatProvider
  { cpName :: Text
  , cpSend :: ChatConfig -> ChatRequest -> (ChatEvent -> IO ()) -> IO (IO ())
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
    -- included), and the tools it asks to call.
    ChatFinished !Text !Value ![ToolCall]
  | ChatFailed !Text
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
defaultChatConfig = ChatConfig "anthropic" "claude-opus-5-5" "high" 64000

data ChatStatus
  = ChatIdle
  | -- | A request is running.
    ChatWaiting
  | -- | Edits wait for the user's decision before the conversation goes on.
    ChatDeciding
  deriving stock (Eq, Show)

-- | What the user decided about an edit.
data EditDecision = Undecided | Approved | Denied
  deriving stock (Eq, Show)

-- | An edit the model proposed, applied to the file's buffer until the
-- user approves (keep and save) or denies (put the old lines back). It
-- replaces lines @[peLine, peLine + length peOld)@ by 'peNew'.
data PendingEdit = PendingEdit
  { peNumber :: !Int
  -- ^ Shown in the chat (#1, #2, …).
  , peToolId :: !Text
  , pePath :: !FilePath
  , peDoc :: !Int
  , peLine :: !Int
  , peOld :: ![Text]
  , peNew :: ![Text]
  , peDecision :: !EditDecision
  }
  deriving stock (Eq, Show)

-- | A chat buffer: the transcript is the document's text, what is typed
-- after 'csInput' is the next message.
data ChatState = ChatState
  { csInput :: !Pos
  , csStatus :: !ChatStatus
  , csHistory :: ![Value]
  -- ^ The conversation, append-only.
  , csEdits :: ![PendingEdit]
  -- ^ The current turn's edits.
  , csResults :: ![(Text, Maybe Value)]
  -- ^ The current turn's tool results by call id, in the calls' order; an
  -- edit's is there once it is decided.
  , csNextEdit :: !Int
  }
  deriving stock (Eq, Show)

newChatState :: ChatState
newChatState = ChatState (Pos 0 0) ChatIdle [] [] [] 1
