-- | A contrib plugin (ADR plugin-canvas): a git status buffer in the manner of
-- magit (@space g g@, @:magit@). It lists the branch, the untracked
-- files, the unstaged and the staged changes, coloured with highlights;
-- its own keys work in it: @s@ / @u@ stage and unstage the file, hunk or
-- section under the cursor (with lines of hunks selected, only those
-- lines), @tab@ shows a file's hunks, @ret@ opens the
-- file there, @c@ commits (@:magit-commit message@), @g r@ refreshes, @q@
-- closes it. An example of a buffer with highlights, a keymap and
-- processes.
module Him.Contrib.Magit
  ( magit
    -- * The pure parts (for the tests)
  , Section (..)
  , Change (..)
  , FileDiff (..)
  , Hunk (..)
  , Row (..)
  , parseStatus
  , parseDiff
  , layout
  , hunkPatch
  , linesPatch
  ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.Plugin
import System.FilePath (takeDirectory, (</>))

data Section = Untracked | Unstaged | Staged
  deriving stock (Eq, Ord, Show)

-- | A file in a section, with git's letter for what happened to it
-- (@M@, @A@, @D@, @R@, …; @?@ untracked, @U@ unmerged).
data Change = Change
  { chSection :: !Section
  , chStatus :: !Char
  , chPath :: !FilePath
  }
  deriving stock (Eq, Show)

data Hunk = Hunk
  { hkHeader :: !Text
  -- ^ @\@\@ -a,b +c,d \@\@ …@
  , hkLines :: ![Text]
  }
  deriving stock (Eq, Show)

-- | One file's part of a diff: the lines before the first hunk
-- (@diff --git@ … @+++@), then the hunks.
data FileDiff = FileDiff
  { fdPath :: !FilePath
  , fdHeader :: ![Text]
  , fdHunks :: ![Hunk]
  }
  deriving stock (Eq, Show)

-- | What a line of the buffer shows, for the keys acting on it.
data Row
  = RowHeader !Section
  | RowFile !Section !FilePath
  | -- | A hunk's header (by its index in the file's diff).
    RowHunk !Section !FilePath !Int
  | -- | A line inside a hunk (its index there).
    RowHunkLine !Section !FilePath !Int !Int
  | RowNone
  deriving stock (Eq, Show)

data St = St
  { stBuffer :: !(Maybe BufferId)
  , stShow :: !Bool
  -- ^ Show the buffer once the status is in (the user asked for it).
  , stRoot :: !(Maybe FilePath)
  , stOutput :: !(Map Text [Text])
  -- ^ Each running git's output so far, last line first.
  , stRunning :: !Int
  , stBranch :: !Text
  , stChanges :: ![Change]
  , stDiffs :: !(Map (Section, FilePath) FileDiff)
  , stExpanded :: !(Set (Section, FilePath))
  , stRows :: !(IntMap Row)
  }

initial :: St
initial = St Nothing False Nothing Map.empty 0 "" [] Map.empty Set.empty IntMap.empty

magit :: PluginSpec St
magit =
  (pluginSpec "magit" "A magit-like git status buffer: stage, unstage, commit (space g g)" initial)
    { psDefaultOn = False
    , psActions =
        [ action "magit" "Show the git status buffer" (modifyState (\s -> s {stShow = True}) >> refresh)
        , action "magit_refresh" "Run git status again" refresh
        , action "magit_stage" "Stage the selected lines of a diff, or the file, hunk or section under the cursor" (orLines False (atCursor stage))
        , action "magit_unstage" "Unstage the selected lines of a diff, or the file, hunk or section under the cursor" (orLines True (atCursor unstage))
        , action "magit_toggle" "Show or hide the hunks of the file under the cursor" (atCursor toggle)
        , action "magit_open" "Open the file under the cursor, at the hunk's line" (atCursor open)
        , action "magit_commit" "Commit the staged changes (asks for the message)" (runAction "command_mode_with \"magit-commit \"")
        , action "magit_quit" "Close the git status buffer" (getState >>= mapM_ closeBuffer . stBuffer)
        ]
    , psCommands =
        [ command ["magit"] "Show the git status buffer" NoArgs (const (modifyState (\s -> s {stShow = True}) >> refresh))
        , command ["magit-commit"] "Commit the staged changes with this message" NoArgs commit
        ]
    , psBindings = [(Normal, "space g g", "magit")]
    , psKeymaps =
        [ ( "status"
          ,
            [ ("s", "magit_stage")
            , ("u", "magit_unstage")
            , ("tab", "magit_toggle")
            , ("ret", "magit_open")
            , ("c", "magit_commit")
            , ("g r", "magit_refresh")
            , ("q", "magit_quit")
            ]
          )
        ]
    , psOnEvent = onEvent
    }

onEvent :: Event -> PluginM St ()
onEvent = \case
  BufferClosed i -> modifyState (\s -> if stBuffer s == Just i then s {stBuffer = Nothing} else s)
  -- A save may change the status; follow it while the buffer is open.
  BufferSaved _ -> getState >>= \s -> when (stBuffer s /= Nothing) refresh
  ProcessOutput name line -> modifyState (\s -> s {stOutput = Map.insertWith (<>) name [line] (stOutput s)})
  ProcessExited "root" code -> do
    out <- output "root"
    case reverse out of
      root : _ | code == 0 -> do
        let dir = T.unpack root
        modifyState (\s -> s {stRoot = Just dir, stRunning = 3})
        spawn "status" "git" ["status", "--porcelain=v1", "-b", "-z", "--untracked-files=all"] (Just dir)
        spawn "diff" "git" ["diff", "--no-color", "--no-ext-diff"] (Just dir)
        spawn "cached" "git" ["diff", "--cached", "--no-color", "--no-ext-diff"] (Just dir)
      _ -> modifyState (\s -> s {stShow = False}) >> warn "magit: not in a git repository"
  ProcessExited name _
    | name `elem` ["status", "diff", "cached"] -> do
        modifyState (\s -> s {stRunning = stRunning s - 1})
        running <- stRunning <$> getState
        when (running == 0) $ do
          status <- output "status"
          unstaged <- output "diff"
          staged <- output "cached"
          let (branch, changes) = parseStatus (T.intercalate "\n" (reverse status))
              diffs = Map.fromList ([((Unstaged, fdPath d), d) | d <- parseDiff (reverse unstaged)] <> [((Staged, fdPath d), d) | d <- parseDiff (reverse staged)])
          modifyState (\s -> s {stBranch = branch, stChanges = changes, stDiffs = diffs})
          draw
  ProcessExited "op" code -> do
    out <- output "op"
    if code == 0
      then mapM_ notify (take 1 (reverse out))
      else warn ("git: " <> T.intercalate " " (take 2 (reverse out)))
    refresh
  _ -> pure ()

-- | A process's output, last line first; forgotten after.
output :: Text -> PluginM St [Text]
output name = do
  s <- getState
  putState s {stOutput = Map.delete name (stOutput s)}
  pure (Map.findWithDefault [] name (stOutput s))

-- | Find the repository (from the current file's directory, else where
-- the buffer was made), then ask git for the status and the diffs.
refresh :: PluginM St ()
refresh = do
  s <- getState
  current <- currentBuffer
  let dir = case biPath current of
        Just path -> Just (takeDirectory path)
        Nothing -> stRoot s
  spawn "root" "git" ["rev-parse", "--show-toplevel"] dir

-- | Show the status in the buffer (made the first time).
draw :: PluginM St ()
draw = do
  s <- getState
  help <- T.intercalate " · " <$> traverse (\(inv, what) -> (<> (" " <> what)) <$> keyInKeymap "status" inv) [("magit_stage", "stage"), ("magit_unstage", "unstage"), ("magit_toggle", "hunks"), ("magit_open", "open"), ("magit_commit", "commit"), ("magit_refresh", "refresh"), ("magit_quit", "close")]
  let (texts, highlights, rows) = layout help (stBranch s) (stChanges s) (stDiffs s) (stExpanded s)
      body = T.intercalate "\n" texts
  existing <- maybe (pure Nothing) bufferInfo (stBuffer s)
  case existing of
    Just b -> do
      setScratchText (biId b) body
      when (stShow s) (focusBuffer (biId b))
    Nothing
      | stShow s -> do
          b <- openScratch "magit" body
          modifyState (\st -> st {stBuffer = Just b})
      | otherwise -> pure ()
  getState >>= \st -> case stBuffer st of
    Just b -> do
      setHighlights b highlights
      setBufferKeymap b (Just "status")
      putState st {stRows = rows, stShow = False}
    Nothing -> pure ()

-- | Run something on the row under the cursor, in the status buffer.
atCursor :: (Row -> PluginM St ()) -> PluginM St ()
atCursor f = do
  s <- getState
  current <- currentBuffer
  Pos line _ <- cursor
  if Just (biId current) == stBuffer s
    then f (IntMap.findWithDefault RowNone line (stRows s))
    else warn "not in the git status buffer"

stage :: Row -> PluginM St ()
stage = \case
  RowHeader Untracked -> getState >>= \s -> git ("add" : "--" : [chPath c | c <- stChanges s, chSection c == Untracked])
  RowHeader Unstaged -> git ["add", "-u"]
  RowFile Untracked path -> git ["add", "--", path]
  RowFile Unstaged path -> git ["add", "--", path]
  RowHunk Unstaged path k -> applyHunk False path k
  RowHunkLine Unstaged path k _ -> applyHunk False path k
  RowNone -> pure ()
  _ -> notify "already staged"

unstage :: Row -> PluginM St ()
unstage = \case
  RowHeader Staged -> git ["restore", "--staged", "--", "."]
  RowFile Staged path -> git ["restore", "--staged", "--", path]
  RowHunk Staged path k -> applyHunk True path k
  RowHunkLine Staged path k _ -> applyHunk True path k
  RowNone -> pure ()
  _ -> notify "not staged"

-- | With a selection (not just the cursor) over lines of shown hunks:
-- stage (or unstage) only the changed lines it covers, in every range.
-- Otherwise the given action.
orLines :: Bool -> PluginM St () -> PluginM St ()
orLines reverse' otherwise' = do
  s <- getState
  current <- currentBuffer
  ranges' <- selections
  let section = if reverse' then Staged else Unstaged
      spanned = [l | (a, h) <- ranges', a /= h, l <- [min (posLine a) (posLine h) .. max (posLine a) (posLine h)]]
      chosen = Map.fromListWith (<>) [(path, [(k, i)]) | l <- spanned, Just (RowHunkLine sec path k i) <- [IntMap.lookup l (stRows s)], sec == section]
      patches = [p | (path, picked) <- Map.toList chosen, Just d <- [Map.lookup (section, path) (stDiffs s)], Just p <- [linesPatch reverse' d (Set.fromList picked)]]
  if Just (biId current) /= stBuffer s || Map.null chosen
    then otherwise'
    else
      if null patches
        then notify "no changed lines selected"
        else do
          mapM_ (\(a, _) -> setCursor a) (take 1 ranges')
          applyPatch reverse' (T.concat patches)

-- | Run git in the repository; the status follows when it is done.
git :: [String] -> PluginM St ()
git args = getState >>= \s -> spawn "op" "git" args (stRoot s)

-- | Stage one hunk of the unstaged diff, or unstage one of the staged
-- diff (@git apply --cached@, reversed), through a patch file.
applyHunk :: Bool -> FilePath -> Int -> PluginM St ()
applyHunk reverse' path k = do
  s <- getState
  let section = if reverse' then Staged else Unstaged
  case Map.lookup (section, path) (stDiffs s) >>= \d -> hunkPatch d k of
    Nothing -> warn "no such hunk"
    Just patch -> applyPatch reverse' patch

-- | Apply a patch to the index (reversed: take it out), through a file.
applyPatch :: Bool -> Text -> PluginM St ()
applyPatch reverse' patch = do
  file <- stateFile "hunk.patch"
  written <- liftIO (try @IOException (TIO.writeFile file patch))
  case written of
    Left e -> warn (T.pack (show e))
    Right () -> git (["apply", "--cached"] <> ["--reverse" | reverse'] <> [file])

toggle :: Row -> PluginM St ()
toggle = \case
  RowFile section path | section /= Untracked -> flipFile section path
  RowHunk section path _ -> flipFile section path >> backTo section path
  RowHunkLine section path _ _ -> flipFile section path >> backTo section path
  RowFile Untracked _ -> notify "an untracked file has no diff"
  _ -> pure ()
  where
    flipFile section path = do
      modifyState (\s -> s {stExpanded = (if Set.member (section, path) (stExpanded s) then Set.delete else Set.insert) (section, path) (stExpanded s)})
      draw
    -- Hiding a file's hunks from inside one: the cursor goes to the file.
    backTo section path = do
      rows <- stRows <$> getState
      case [l | (l, RowFile s p) <- IntMap.toList rows, s == section, p == path] of
        l : _ -> setCursor (Pos l 0)
        [] -> pure ()

open :: Row -> PluginM St ()
open row = do
  s <- getState
  let at path line = case stRoot s of
        Just root -> openFile (root </> path) >> setCursor (Pos line 0)
        Nothing -> pure ()
      hunkLine section path k = maybe 0 (\h -> newStart (hkHeader h)) (hunkOf section path k)
      hunkOf section path k = Map.lookup (section, path) (stDiffs s) >>= \d -> lookupIndex k (fdHunks d)
  case row of
    RowFile _ path -> at path 0
    RowHunk section path k -> at path (hunkLine section path k)
    RowHunkLine section path k i ->
      -- Lines the hunk removes are not in the file: count the others.
      let before = maybe 0 (length . filter (not . T.isPrefixOf "-") . take i . hkLines) (hunkOf section path k)
       in at path (hunkLine section path k + max 0 (before - 1))
    _ -> pure ()
  where
    lookupIndex k xs = case drop k xs of
      x : _ | k >= 0 -> Just x
      _ -> Nothing

commit :: [Text] -> PluginM St ()
commit args
  | null args = warn "usage: :magit-commit <message>"
  | otherwise = git ["commit", "-m", T.unpack (T.unwords args)]

-- * Pure

-- | @git status --porcelain=v1 -b -z@: the branch line, then the files
-- in their sections (a file can be both unstaged and staged).
parseStatus :: Text -> (Text, [Change])
parseStatus t = case T.splitOn "\0" t of
  first : rest | Just branch <- T.stripPrefix "## " first -> (branchName branch, entries rest)
  rest -> ("", entries rest)
  where
    branchName b = case T.stripPrefix "No commits yet on " b of
      Just name -> name <> " (no commits yet)"
      Nothing -> T.takeWhile (/= ' ') (fst (T.breakOn "..." b))
    entries = \case
      e : more | T.length e >= 4 -> do
        let x = T.index e 0
            y = T.index e 1
            path = T.unpack (T.drop 3 e)
            -- A rename's old path follows as a field of its own.
            more' = if x `elem` ['R', 'C'] then drop 1 more else more
            unmerged = x == 'U' || y == 'U' || (x, y) `elem` [('A', 'A'), ('D', 'D')]
            found
              | x == '?' = [Change Untracked '?' path]
              | unmerged = [Change Unstaged 'U' path]
              | otherwise = [Change Staged x path | x /= ' '] <> [Change Unstaged y path | y /= ' ']
        found <> entries more'
      _ : more -> entries more
      [] -> []

-- | A @git diff@ (lines) split into files and hunks.
parseDiff :: [Text] -> [FileDiff]
parseDiff = go
  where
    go ls = case dropWhile (not . T.isPrefixOf "diff --git ") ls of
      [] -> []
      start : more ->
        let (body, rest) = break (T.isPrefixOf "diff --git ") more
            (header, hunkLines) = break (T.isPrefixOf "@@") body
         in FileDiff (pathOf start header) (start : header) (hunks hunkLines) : go rest
    hunks = \case
      h : more ->
        let (body, rest) = break (T.isPrefixOf "@@") more
         in Hunk h body : hunks rest
      [] -> []
    -- The new name (@+++ b/…@), or the old one for a deleted file.
    pathOf start header = case ([p | l <- header, Just p <- [T.stripPrefix "+++ b/" l]], [p | l <- header, Just p <- [T.stripPrefix "--- a/" l]]) of
      (p : _, _) -> T.unpack p
      (_, p : _) -> T.unpack p
      -- No +++ / --- lines (a binary file): @diff --git a/x b/x@.
      _ -> let (before, after) = T.breakOnEnd " b/" start in T.unpack (if T.null before then start else after)

-- | The patch of one hunk of a file's diff ('Nothing': no such hunk).
hunkPatch :: FileDiff -> Int -> Maybe Text
hunkPatch d k = case drop k (fdHunks d) of
  h : _ | k >= 0 -> Just (T.unlines (fdHeader d <> [hkHeader h] <> hkLines h))
  _ -> Nothing

-- | A patch of some lines of a file's diff: the chosen (hunk, line)
-- pairs (lines counted from 1 within the hunk). Changes not chosen are
-- left out: an added line is dropped, a removed one becomes context. For
-- unstaging (reversed, applied with @--reverse@) it is the other way
-- round. 'Nothing' when no chosen line is a change.
linesPatch :: Bool -> FileDiff -> Set (Int, Int) -> Maybe Text
linesPatch reverse' d chosen
  | null hunks' = Nothing
  | otherwise = Just (T.unlines (fdHeader d <> concat (shift 0 hunks')))
  where
    hunks' = [(h, kept) | (k, h) <- zip [0 ..] (fdHunks d), let kept = keep k (hkLines h), any isChange [l | (l, True) <- kept]]
    isChange l = "+" `T.isPrefixOf` l || "-" `T.isPrefixOf` l
    -- Each line, and whether it is a chosen change.
    keep k ls = go Nothing (zip [1 ..] ls)
      where
        go _ [] = []
        go previous ((i, l) : rest)
          -- "\ No newline at end of file" goes with the line before it.
          | "\\" `T.isPrefixOf` l = [(l, False) | previous == Just True] <> go previous rest
          | otherwise = case T.uncons l of
              Just (c, body)
                | c == dropped && not chosenHere -> go (Just False) rest
                | c == contexted && not chosenHere -> (" " <> body, False) : go (Just True) rest
              _ -> (l, chosenHere && isChange l) : go (Just True) rest
          where
            chosenHere = Set.member (k, i) chosen
    (dropped, contexted) = if reverse' then ('-', '+') else ('+', '-')
    -- New headers: the counts of what is left, and the new side's start
    -- moved by the hunks before.
    shift _ [] = []
    shift delta ((h, kept) : rest) =
      let ls = map fst kept
          count p = length (filter p ls)
          old = count (\l -> not ("+" `T.isPrefixOf` l || "\\" `T.isPrefixOf` l))
          new = count (\l -> not ("-" `T.isPrefixOf` l || "\\" `T.isPrefixOf` l))
          start = oldStart (hkHeader h)
          rest' = T.drop 2 (snd (T.breakOn "@@" (T.drop 2 (hkHeader h))))
          header = "@@ -" <> range start old <> " +" <> range (start + delta) new <> " @@" <> rest'
       in (header : ls) : shift (delta + new - old) rest
    range start n = T.pack (show start) <> "," <> T.pack (show n)

-- | The first line (from 1, as in the header) of a hunk in the old file.
oldStart :: Text -> Int
oldStart header = case T.takeWhile isDigit (T.drop 1 (snd (T.breakOn "-" header))) of
  digits | not (T.null digits) -> read (T.unpack digits)
  _ -> 0

-- | The first line (from 0) of a hunk in the new file: @+c@ of
-- @\@\@ -a,b +c,d \@\@@.
newStart :: Text -> Int
newStart header = case T.breakOn "+" (T.drop 2 header) of
  (_, rest) | Just digits <- nonEmpty (T.takeWhile isDigit (T.drop 1 rest)) -> max 0 (read (T.unpack digits) - 1)
  _ -> 0
  where
    nonEmpty x = if T.null x then Nothing else Just x

-- | The buffer: its lines, their highlights, and what each line is. The
-- help line (the keys, as bound) comes last.
layout :: Text -> Text -> [Change] -> Map (Section, FilePath) FileDiff -> Set (Section, FilePath) -> ([Text], [Highlight], IntMap Row)
layout help branch changes diffs expanded = (map fst3 lines', concat [map (\(a, b, f) -> Highlight l a b f) hs | (l, (_, hs, _)) <- zip [0 ..] lines'], rows)
  where
    rows = IntMap.fromList [(l, r) | (l, (_, _, r)) <- zip [0 ..] lines', r /= RowNone]
    fst3 (a, _, _) = a
    lines' =
      [("Head:     " <> branch, [(0, 5, face "keyword"), (10, 10 + T.length branch, face "string")], RowNone)]
        <> concatMap section [Untracked, Unstaged, Staged]
        <> (if null changes then [blank, ("Nothing to commit, working tree clean", [], RowNone)] else [])
        <> [blank, (help, [(0, T.length help, face "comment")], RowNone)]
    blank = ("", [], RowNone)
    section sec = case [c | c <- changes, chSection c == sec] of
      [] -> []
      cs ->
        let title = sectionTitle sec
            count = " (" <> T.pack (show (length cs)) <> ")"
         in [blank, (title <> count, [(0, T.length title, face "markup.heading"), (T.length title, T.length title + T.length count, face "comment")], RowHeader sec)]
              <> concatMap (file sec) cs
    file sec c =
      let (label, labelFace) = describe (chStatus c)
          path = T.pack (chPath c)
          shown = if sec == Untracked then "  " <> path else "  " <> T.justifyLeft 11 ' ' label <> path
          hl = if sec == Untracked then [] else [(2, 2 + T.length label, face labelFace)]
          open' = Set.member (sec, chPath c) expanded
          hunkRows = case Map.lookup (sec, chPath c) diffs of
            Just d | open' -> concat [hunk sec (chPath c) k h | (k, h) <- zip [0 ..] (fdHunks d)]
            _ -> []
       in (shown, hl, RowFile sec (chPath c)) : hunkRows
    hunk sec path k h =
      (hkHeader h, [(0, T.length (hkHeader h), face "diff.delta")], RowHunk sec path k)
        : [(l, lineFace l, RowHunkLine sec path k i) | (i, l) <- zip [1 ..] (hkLines h)]
    lineFace l
      | "+" `T.isPrefixOf` l = [(0, T.length l, face "diff.plus")]
      | "-" `T.isPrefixOf` l = [(0, T.length l, face "diff.minus")]
      | otherwise = []
    sectionTitle = \case
      Untracked -> "Untracked files"
      Unstaged -> "Unstaged changes"
      Staged -> "Staged changes"
    describe = \case
      'M' -> ("modified", "diff.delta")
      'A' -> ("new file", "diff.plus")
      'D' -> ("deleted", "diff.minus")
      'R' -> ("renamed", "diff.delta.moved")
      'C' -> ("copied", "diff.plus")
      'T' -> ("typechange", "diff.delta")
      'U' -> ("unmerged", "error")
      _ -> ("changed", "diff.delta")
