-- | The system clipboard behind the @+@ and @*@ registers. Like the
-- syntax and chat providers, it is one interface ('ClipboardProvider')
-- with backends chosen by name (@[editor] clipboard-provider@); the
-- editor never knows which one it talks to. @auto@ takes the first
-- available, in Helix's order: macOS's pasteboard, Wayland
-- (@wl-copy@ / @wl-paste@), X11 (@xclip@, then @xsel@), tmux, and last
-- the terminal itself (OSC 52), which can copy but not paste.
module Him.Clipboard
  ( ClipboardKind (..)
  , ClipboardProvider (..)
  , systemProviders
  , chooseProvider
  , joinValues
  , base64
  ) where

import Control.Exception (IOException, try)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Word (Word8)
import Him.Process (ProcessResult (..), runProcess)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), hClose, hFlush, stdout, withFile)
import System.Info (os)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

-- | The @+@ register is the clipboard, @*@ the primary selection (X11 and
-- Wayland; elsewhere both are the clipboard).
data ClipboardKind = SystemClipboard | PrimarySelection
  deriving stock (Eq, Show)

data ClipboardProvider = ClipboardProvider
  { cbName :: Text
  , cbAvailable :: IO Bool
  -- ^ For @auto@: can it work here (its programs found, its display set)?
  , cbGet :: ClipboardKind -> IO (Either Text Text)
  , cbSet :: ClipboardKind -> Text -> IO (Either Text ())
  }

-- | The built-in providers, in the order @auto@ tries them. @none@ is
-- never chosen by @auto@: the registers then live in the editor only.
systemProviders :: [ClipboardProvider]
systemProviders =
  [ commands "pasteboard" (pure (os == "darwin")) (const ("pbpaste", [])) (const ("pbcopy", []))
  , commands
      "wayland"
      (env "WAYLAND_DISPLAY" `andAlso` programs ["wl-copy", "wl-paste"])
      (\k -> ("wl-paste", ["--no-newline"] <> primary k ["--primary"]))
      (\k -> ("wl-copy", ["--type", "text/plain"] <> primary k ["--primary"]))
  , commands
      "x-clip"
      (env "DISPLAY" `andAlso` programs ["xclip"])
      (\k -> ("xclip", ["-o", "-selection", selection k]))
      (\k -> ("xclip", ["-i", "-selection", selection k]))
  , commands
      "x-sel"
      (env "DISPLAY" `andAlso` programs ["xsel"])
      (\k -> ("xsel", ["-o", xselFlag k]))
      (\k -> ("xsel", ["-i", xselFlag k]))
  , commands "tmux" (env "TMUX" `andAlso` programs ["tmux"]) (const ("tmux", ["save-buffer", "-"])) (const ("tmux", ["load-buffer", "-w", "-"]))
  , termcode
  , ClipboardProvider "none" (pure False) (const (pure (Left "no clipboard (editor.clipboard-provider = \"none\")"))) (\_ _ -> pure (Right ()))
  ]
  where
    primary k flags = if k == PrimarySelection then flags else []
    selection k = if k == PrimarySelection then "primary" else "clipboard"
    xselFlag k = if k == PrimarySelection then "-p" else "-b"
    env name = maybe False (not . null) <$> lookupEnv name
    programs ps = all isJust <$> traverse findExecutable ps
    andAlso a b = a >>= \ok -> if ok then b else pure False

-- | The provider a setting names; @auto@ is the first available one, and
-- 'Nothing' when none is.
chooseProvider :: [ClipboardProvider] -> Text -> IO (Maybe ClipboardProvider)
chooseProvider providers = \case
  "auto" -> firstAvailable providers
  name -> pure (find ((== name) . cbName) providers)
  where
    firstAvailable [] = pure Nothing
    firstAvailable (p : ps) = cbAvailable p >>= \ok -> if ok then pure (Just p) else firstAvailable ps

-- | A provider of a program that prints the clipboard and one that reads
-- the new contents from stdin.
commands :: Text -> IO Bool -> (ClipboardKind -> (FilePath, [String])) -> (ClipboardKind -> (FilePath, [String])) -> ClipboardProvider
commands name available getter setter =
  ClipboardProvider
    { cbName = name
    , cbAvailable = available
    , cbGet = \k -> do
        let (cmd, args) = getter k
        timeout limit (runProcess cmd args Nothing BS.empty) >>= \case
          Nothing -> pure (Left (T.pack cmd <> " did not answer"))
          Just (Left err) -> pure (Left err)
          Just (Right r)
            | prExit r == ExitSuccess -> pure (Right (TE.decodeUtf8With TE.lenientDecode (prStdout r)))
            | otherwise -> pure (Left (T.pack cmd <> ": " <> T.strip (TE.decodeUtf8With TE.lenientDecode (prStderr r))))
    , cbSet = \k t -> do
        let (cmd, args) = setter k
        setWith cmd args t
    }
  where
    limit = 2000000

-- | Copy by giving a program the text. Its output goes nowhere: @wl-copy@
-- and @xclip@ stay in the background to serve the clipboard, and would keep
-- a captured pipe open forever.
setWith :: FilePath -> [String] -> Text -> IO (Either Text ())
setWith cmd args t =
  fmap (either (Left . T.pack . show @IOException) id) . try $
    withFile "/dev/null" ReadWriteMode $ \null' -> do
      (Just hin, _, _, ph) <- createProcess (proc cmd args) {std_in = CreatePipe, std_out = UseHandle null', std_err = UseHandle null'}
      _ <- try @IOException (BS.hPut hin (TE.encodeUtf8 t))
      _ <- try @IOException (hClose hin)
      timeout 2000000 (waitForProcess ph) >>= \case
        Just ExitSuccess -> pure (Right ())
        Just (ExitFailure n) -> pure (Left (T.pack cmd <> " failed (exit " <> T.pack (show n) <> ")"))
        Nothing -> Left (T.pack cmd <> " did not finish") <$ terminateProcess ph

-- | The terminal's clipboard (OSC 52): works over ssh, but it cannot be
-- read, so a paste uses what the editor copied last.
termcode :: ClipboardProvider
termcode =
  ClipboardProvider
    { cbName = "termcode"
    , cbAvailable = pure True
    , cbGet = const (pure (Left "the terminal's clipboard (OSC 52) cannot be read"))
    , cbSet = \k t -> do
        let target = if k == PrimarySelection then "p" else "c"
        BS.hPut stdout (BC.pack ("\ESC]52;" <> target <> ";") <> base64 (TE.encodeUtf8 t) <> BC.pack "\a")
        hFlush stdout
        pure (Right ())
    }

-- | A register's values as one clipboard text: one per line, unless a value
-- ends its line already.
joinValues :: [Text] -> Text
joinValues = \case
  [] -> ""
  [v] -> v
  v : vs -> v <> (if "\n" `T.isSuffixOf` v then "" else "\n") <> joinValues vs

-- | Standard base64 with padding.
base64 :: BS.ByteString -> BS.ByteString
base64 bs = fst (BS.unfoldrN (4 * ((BS.length bs + 2) `div` 3)) step 0)
  where
    n = BS.length bs
    byte i = if i < n then fromIntegral (BS.index bs i) else 0 :: Int
    alphabet = BC.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    step :: Int -> Maybe (Word8, Int)
    step j =
      let g = j `div` 4
          i = 3 * g
          triple = (byte i `shiftL` 16) .|. (byte (i + 1) `shiftL` 8) .|. byte (i + 2)
          k = j `mod` 4
          used = n - i -- bytes of this group that exist
          c
            | k >= 2 && used < k = '='
            | otherwise = BC.index alphabet ((triple `shiftR` (18 - 6 * k)) .&. 63)
       in Just (fromIntegral (fromEnum c), j + 1)
