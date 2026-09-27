-- | Keys as the editor sees them, independent of terminal byte encodings.
--
-- Convention (same as Helix): a shifted letter is just the upper-case
-- character, e.g. @W@ is @KChar 'W'@ without 'Shift'. 'Shift' only appears on
-- non-character keys such as @S-tab@ or @S-left@.
module Him.Key
  ( Key (..)
  , KeyCode (..)
  , Modifier (..)
  , plain
  , withMod
  , ctrl
  , alt
  , showKey
  , showKeys
  , parseKey
  , parseKeys
  ) where

import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data KeyCode
  = KChar Char
  | KEnter
  | KEsc
  | KBackspace
  | KTab
  | KUp
  | KDown
  | KLeft
  | KRight
  | KHome
  | KEnd
  | KPageUp
  | KPageDown
  | KInsert
  | KDelete
  | KF Int
  deriving stock (Eq, Ord, Show)

data Modifier = Ctrl | Alt | Shift
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Key = Key
  { keyCode :: !KeyCode
  , keyMods :: !(Set Modifier)
  }
  deriving stock (Eq, Ord, Show)

plain :: KeyCode -> Key
plain code = Key code Set.empty

withMod :: Modifier -> Key -> Key
withMod m (Key code mods) = Key code (Set.insert m mods)

ctrl :: Char -> Key
ctrl = withMod Ctrl . plain . KChar

alt :: Char -> Key
alt = withMod Alt . plain . KChar

-- | Names for keys that are not a single printable character.
namedKeys :: [(Text, KeyCode)]
namedKeys =
  [ ("ret", KEnter)
  , ("esc", KEsc)
  , ("backspace", KBackspace)
  , ("tab", KTab)
  , ("up", KUp)
  , ("down", KDown)
  , ("left", KLeft)
  , ("right", KRight)
  , ("home", KHome)
  , ("end", KEnd)
  , ("pageup", KPageUp)
  , ("pagedown", KPageDown)
  , ("ins", KInsert)
  , ("del", KDelete)
  , ("space", KChar ' ')
  , ("minus", KChar '-')
  ]

modPrefixes :: [(Text, Modifier)]
modPrefixes = [("C-", Ctrl), ("A-", Alt), ("S-", Shift)]

-- | Human-readable key, in the syntax 'parseKey' accepts: @C-s@, @A-x@, @ret@.
showKey :: Key -> Text
showKey (Key code mods) = foldMap prefix [Ctrl, Alt, Shift] <> name
  where
    prefix m
      | m `Set.member` mods = maybe "" fst (find ((== m) . snd) modPrefixes)
      | otherwise = ""
    name = case code of
      KF n -> "F" <> T.pack (show n)
      KChar c | c /= ' ' && c /= '-' -> T.singleton c
      _ -> maybe "?" fst (find ((== code) . snd) namedKeys)

showKeys :: [Key] -> Text
showKeys = T.unwords . map showKey

-- | Parse one key: optional modifier prefixes, then a character or a name.
-- Examples: @"a"@, @"C-s"@, @"A-S-left"@, @"ret"@, @"F5"@.
parseKey :: Text -> Maybe Key
parseKey = go Set.empty
  where
    go mods t
      | Just (m, rest) <- stripModifier t = go (Set.insert m mods) rest
      | otherwise = (`Key` mods) <$> parseCode t
    stripModifier t
      | T.length t <= 1 = Nothing
      | otherwise = case [(m, rest) | (p, m) <- modPrefixes, Just rest <- [T.stripPrefix p t]] of
          (hit : _) | not (T.null (snd hit)) -> Just hit
          _ -> Nothing
    parseCode t
      | Just code <- lookup t namedKeys = Just code
      | Just n <- T.stripPrefix "F" t
      , not (T.null n)
      , T.all (`elem` ['0' .. '9']) n =
          Just (KF (read (T.unpack n)))
      | [c] <- T.unpack t = Just (KChar c)
      | otherwise = Nothing

-- | Parse a space-separated key sequence, e.g. @"g g"@ or @"space f"@.
parseKeys :: Text -> Maybe [Key]
parseKeys = traverse parseKey . T.words
