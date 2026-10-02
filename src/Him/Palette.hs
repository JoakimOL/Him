-- | The command palette (@space ?@): every action, with its parameters,
-- the keys bound to it, and its description, as picker items. Built from
-- the config, so rebound keys show up as they are.
module Him.Palette
  ( paletteItems
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Config (Config (..))
import Him.Key (showKeys)
import Him.Keymap (keymapBindings)
import Him.Mode (Mode (..))
import Him.Picker (PickTarget (..), PickerItem, pickerItem)

-- | Items for the palette opened in a mode: keys of that mode are listed;
-- an action without one shows the keys of the first other mode that has
-- some (e.g. @insert: tab@).
paletteItems :: Config -> Mode -> [PickerItem]
paletteItems config mode =
  [ pickerItem (actName a <> signature a) (PickAction (actName a) (needsArgs a)) (detail ks a)
  | (a, ks) <- withKeys
  ]
  where
    withKeys = [(a, keysFor a) | a <- registryActions (cfgActions config)]
    -- Keys are padded to a common width (capped) so the descriptions line up.
    keyWidth = min 18 (maximum (0 : [T.length ks | (_, ks) <- withKeys]))
    needsArgs a = any (isNothing . paramDefault) (actParams a)
    signature a = T.concat [" " <> shape p | p <- actParams a]
    shape p = case paramDefault p of
      Nothing -> "<" <> paramName p <> ">"
      Just _ -> "[" <> paramName p <> "]"
    keysIn m = Map.fromListWith (flip (<>)) $ do
      km <- maybe [] pure (Map.lookup m (cfgKeymaps config))
      (ks, b) <- keymapBindings km
      let inv = boundInvocation b
          args = if null (invArgs inv) then "" else " (" <> T.unwords (invArgs inv) <> ")"
      pure (invAction inv, [showKeys ks <> args])
    byMode = Map.fromList [(m, keysIn m) | m <- [minBound .. maxBound]]
    keysFor a =
      case [(m, ks) | m <- mode : filter (/= mode) [minBound .. maxBound], Just ks <- [Map.lookup m byMode >>= Map.lookup (actName a)]] of
        (m, ks) : _ -> (if m == mode then "" else modeName m <> ": ") <> T.intercalate ", " ks
        [] -> ""
    detail ks a = T.justifyLeft keyWidth ' ' ks <> "  " <> actDoc a

modeName :: Mode -> Text
modeName = T.toLower . T.pack . show
