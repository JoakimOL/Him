-- | A contrib plugin (ADR plugin-api): the files opened lately, remembered
-- between runs, in a picker (@space o@). @ret@ opens the chosen files
-- (@tab@ marks several), @del@ forgets them. An example of a picker with
-- a secondary action, settings and a state file.
module Him.Contrib.RecentFiles
  ( recentFiles
  ) where

import Control.Exception (IOException, try)
import Data.List (nub)
import Data.Text qualified as T
import Him.Plugin
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute)
import System.FilePath (makeRelative)

-- | The files, latest first ('Nothing' until read from the state file).
type Recent = Maybe [FilePath]

recentFiles :: PluginSpec Recent
recentFiles =
  (pluginSpec "recent-files" "Files opened lately, remembered between runs (space o)" Nothing)
    { psDefaultOn = False
    , psOptions = [("max", "how many files to remember (default 100)")]
    , psActions =
        [ action "recent_files" "Pick from the files opened lately" pick
        , action "recent_files_forget" "Forget the chosen files (del in the recent files picker)" forget
        ]
    , psCommands = [command ["recent"] "Pick from the files opened lately" NoArgs (const pick)]
    , psBindings = [(Normal, "space o", "recent_files")]
    , psOnEvent = \case
        BufferEntered i -> bufferInfo i >>= mapM_ remember . (>>= biPath)
        _ -> pure ()
    }

-- | The list, read from the state file the first time.
load :: PluginM Recent [FilePath]
load =
  getState >>= \case
    Just files -> pure files
    Nothing -> do
      file <- stateFile "files"
      files <- liftIO (either (const []) lines <$> try @IOException (readFile' file))
      putState (Just files)
      pure files
  where
    -- Read it all before writing to it again.
    readFile' f = readFile f >>= \s -> length s `seq` pure s

save :: [FilePath] -> PluginM Recent ()
save files = do
  putState (Just files)
  file <- stateFile "files"
  liftIO (either (const ()) id <$> try @IOException (writeFile file (unlines files)))

remember :: FilePath -> PluginM Recent ()
remember path = do
  absolute <- liftIO (makeAbsolute path)
  limit <- optionInt "max" 100
  files <- load
  if take 1 files == [absolute] then pure () else save (take limit (nub (absolute : files)))

pick :: PluginM Recent ()
pick = do
  current <- biPath <$> currentBuffer
  here <- traverse (liftIO . makeAbsolute) current
  files <- filter ((/= here) . Just) <$> load
  cwd <- liftIO getCurrentDirectory
  home <- liftIO getHomeDirectory
  let shown f
        | rel <- makeRelative cwd f, rel /= f = rel
        | rel <- makeRelative home f, rel /= f = "~/" <> rel
        | otherwise = f
  forgetKey <- keyFor Picking "picker_secondary"
  if null files
    then notify "no recent files yet"
    else
      openPicker
        PickerSpec
          { pickerTitle = "recent files (" <> forgetKey <> " forgets)"
          , pickerItems = [Item (T.pack (shown f)) "" (TargetFile f) | f <- files]
          , pickerPrimary = "picker_open"
          , pickerSecondary = Just "recent_files_forget"
          }

forget :: PluginM Recent ()
forget = do
  gone <- (\items -> [f | Item _ _ (TargetFile f) <- items]) <$> chosenItems
  files <- load
  save (filter (`notElem` gone) files)
  closePicker
  pick
