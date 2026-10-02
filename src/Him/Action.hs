-- | Actions: the user-facing features keys are bound to (see ADR-17 in
-- docs/PLAN.md).
--
-- An action has a stable snake_case name, a group, a doc string, and a list
-- of typed parameters. A binding names an action and gives it arguments as
-- text, e.g. @move_line_down 5@ or @insert_text "-- "@. Binding validates the
-- arguments once, when the keymap is built, and produces a ready-to-run
-- 'Bound' action, so a bad binding (from the defaults or, later, from a
-- config file) is reported at startup instead of when the key is pressed.
module Him.Action
  ( -- * Actions
    Action (..)
  , ActionGroup (..)
  , groupName
  , action
  , simple
    -- * Parameters
  , ArgSpec
  , Param (..)
  , ParamType (..)
  , int
  , text
  , choice
  , optional
    -- * Registry
  , ActionRegistry
  , mkActionRegistry
  , lookupAction
  , registryActions
    -- * Invocations
  , Invocation (..)
  , parseInvocation
  , renderInvocation
  , Bound (..)
  , bindInvocation
  , bindText
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Him.Command (EditorM)
import Him.Invocation

-- | Groups order and label actions in documentation and help. They are not
-- part of an action's name, so moving an action between groups never
-- breaks a binding.
data ActionGroup
  = GMovement
  | GSelection
  | GModes
  | GEditing
  | GClipboard
  | GHistory
  | GSearch
  | GBuffers
  | GPrompt
  | GMisc
  deriving stock (Eq, Ord, Show, Enum, Bounded)

groupName :: ActionGroup -> Text
groupName = \case
  GMovement -> "movement"
  GSelection -> "selection"
  GModes -> "modes"
  GEditing -> "editing"
  GClipboard -> "clipboard"
  GHistory -> "history"
  GSearch -> "search"
  GBuffers -> "buffers"
  GPrompt -> "prompt"
  GMisc -> "misc"

data Action = Action
  { actName :: !Text
  -- ^ snake_case and stable: bindings (and config files) refer to it.
  , actGroup :: !ActionGroup
  , actDoc :: !Text
  , actParams :: [Param]
  , actBind :: [Text] -> Either Text (EditorM ())
  -- ^ Check the arguments and produce the action to run.
  }

-- | An action with parameters, described by an 'ArgSpec'.
action :: Text -> ActionGroup -> Text -> ArgSpec a -> (a -> EditorM ()) -> Action
action name grp doc spec run =
  Action
    { actName = name
    , actGroup = grp
    , actDoc = doc
    , actParams = specParams spec
    , actBind = \args -> case specParse spec args of
        Left e -> Left (name <> ": " <> e)
        Right (a, []) -> Right (run a)
        Right (_, extra) ->
          Left (name <> ": unexpected argument " <> T.unwords (map quoteArg extra))
    }

-- | An action without parameters.
simple :: Text -> ActionGroup -> Text -> EditorM () -> Action
simple name grp doc run = action name grp doc (pure ()) (const run)

-- * Parameters

data ParamType
  = PInt
  | PText
  | -- | One of a fixed set of words.
    PChoice [Text]
  deriving stock (Eq, Show)

data Param = Param
  { paramName :: !Text
  , paramType :: !ParamType
  , paramDefault :: !(Maybe Text)
  -- ^ Shown in help; a parameter with a default may be left out.
  }
  deriving stock (Eq, Show)

-- | Positional parameters: each one takes the next argument. The
-- 'Applicative' instance combines them in order, and the spec describes
-- itself ('actParams'), so help text and a config parser can list them.
data ArgSpec a = ArgSpec
  { specParams :: [Param]
  , specParse :: [Text] -> Either Text (a, [Text])
  }

instance Functor ArgSpec where
  fmap f (ArgSpec ps p) = ArgSpec ps (fmap (\(a, rest) -> (f a, rest)) . p)

instance Applicative ArgSpec where
  pure a = ArgSpec [] (\args -> Right (a, args))
  ArgSpec pf f <*> ArgSpec pa a =
    ArgSpec (pf <> pa) $ \args -> do
      (g, rest) <- f args
      (x, rest') <- a rest
      pure (g x, rest')

param :: Param -> (Text -> Either Text a) -> ArgSpec a
param p conv = ArgSpec [p] $ \case
  (arg : rest) -> (,rest) <$> conv arg
  [] -> Left ("missing argument <" <> paramName p <> ">")

int :: Text -> ArgSpec Int
int name = param (Param name PInt Nothing) $ \t -> case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Right n
  _ -> Left ("<" <> name <> "> must be a number, got " <> quoteArg t)

text :: Text -> ArgSpec Text
text name = param (Param name PText Nothing) Right

-- | One of the given words, mapped to a value.
choice :: Text -> [(Text, a)] -> ArgSpec a
choice name opts = param (Param name (PChoice (map fst opts)) Nothing) $ \t ->
  maybe
    (Left ("<" <> name <> "> must be one of " <> T.intercalate ", " (map fst opts) <> ", got " <> quoteArg t))
    Right
    (lookup t opts)

-- | Make the last parameter of a spec optional, with a default. (Only
-- trailing parameters should be optional, since arguments are positional.)
optional :: Text -> a -> ArgSpec a -> ArgSpec a
optional shown def (ArgSpec ps p) =
  ArgSpec (map (\q -> q {paramDefault = Just shown}) ps) $ \case
    [] -> Right (def, [])
    args -> p args

-- * Registry

newtype ActionRegistry = ActionRegistry (Map Text Action)

-- | Build the registry. Two actions with the same name are an error.
mkActionRegistry :: [Action] -> Either Text ActionRegistry
mkActionRegistry acts = case Map.keys (Map.filter (> (1 :: Int)) counts) of
  [] -> Right (ActionRegistry (Map.fromList [(actName a, a) | a <- acts]))
  dups -> Left ("duplicate action names: " <> T.intercalate ", " dups)
  where
    counts = Map.fromListWith (+) [(actName a, 1) | a <- acts]

lookupAction :: Text -> ActionRegistry -> Maybe Action
lookupAction name (ActionRegistry m) = Map.lookup name m

-- | All actions, by group, then name.
registryActions :: ActionRegistry -> [Action]
registryActions (ActionRegistry m) =
  map snd (Map.toAscList (Map.fromList [((actGroup a, actName a), a) | a <- Map.elems m]))

-- | A validated binding, ready to run.
data Bound = Bound
  { boundInvocation :: !Invocation
  , boundRun :: EditorM ()
  , boundCounted :: Maybe (Int -> EditorM ())
  -- ^ How to run it with a count typed before the key (@5 j@). Present when
  -- the binding gives no arguments and the action's first parameter is an
  -- integer named @count@; other bindings ignore a count.
  }

bindInvocation :: ActionRegistry -> Invocation -> Either Text Bound
bindInvocation reg inv = case lookupAction (invAction inv) reg of
  Nothing -> Left ("unknown action: " <> invAction inv)
  Just a -> do
    run <- actBind a (invArgs inv)
    let counted = case (invArgs inv, actParams a) of
          ([], Param "count" PInt _ : _) ->
            Just (\n -> either (const run) id (actBind a [T.pack (show n)]))
          _ -> Nothing
    pure (Bound inv run counted)

-- | Parse and bind a binding's text.
bindText :: ActionRegistry -> Text -> Either Text Bound
bindText reg t = parseInvocation t >>= bindInvocation reg
