-- | Git in the editor (ADR-25): keeping each document's git state current,
-- moving between changes, and staging, unstaging or resetting the selected
-- lines.
module Him.Actions.Git
  ( gitPlugin
  , actions
  , gitHousekeeping
  , applyGitResult
  , markGitReload
  , gitSignSpans
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Him.Action
import Him.Buffer qualified as Buffer
import Him.EditorM
import Him.Actions.Jump (jumping)
import Him.Diff
import Him.Document (DocKind (..), Document (..), replaceBuffer)
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Editor
import Him.GitState
import Him.Position (Pos (..))
import Him.Selection
import Data.Map.Strict qualified as Map
import Him.Config (Plugin (..), plugin)
import Him.Key (KeyCode (..), plain)
import Him.Mode (Mode (..))
import Data.IntMap.Strict qualified as IntMap
import Him.PluginUI (Face (..), GutterSign (..), PluginUI (..), Segment (..), Side (..), SignSpan (..), face)

-- | Git signs in the gutter, change navigation and line staging (ADR-25),
-- as a plugin (ADR-35).
gitPlugin :: Plugin
gitPlugin =
  (plugin "git" "Signs for changed lines, ] g / [ g, staging selected lines (space g)")
    { plActions = actions
    , plBindings =
        Map.fromList
          [ ( Normal
            ,
              [ ("space g s", "git_stage_selection")
              , ("space g u", "git_unstage_selection")
              , ("space g S", "git_stage_file")
              , ("space g U", "git_unstage_file")
              , ("space g r", "git_reset_selection")
              , ("] g", "goto_next_change")
              , ("[ g", "goto_prev_change")
              ]
            )
          ]
    , plPrefixNames = [([plain (KChar ' '), plain (KChar 'g')], "git")]
    , plSigns = True
    , plHousekeeping = gitHousekeeping
    , plJobResult = applyGitResult
    , -- Every document is looked up again; signs come back as answers do.
      plEnable = modify' (mapDocuments (\d -> d {docGit = GitUnknown}))
    , plDisable = modify' (mapDocuments (\d -> d {docGit = GitUnknown}))
    }

actions :: [Action]
actions =
  [ simple "goto_next_change" GMovement "Go to the next git change" (jumpChange True)
  , simple "goto_prev_change" GMovement "Go to the previous git change" (jumpChange False)
  , simple "git_stage_selection" GGit "Stage the changes on the selected lines" $
      withGit $ \d t -> do
        let current = Buffer.toLines (docBuffer d)
            base = gtBase t
            hunks = diffLines (gbIndex base) current
            selected = selectedLines d
        writeIndex d base (Just (applySelected (gbIndex base) current hunks (`IntSet.member` selected)))
  , simple "git_unstage_selection" GGit "Unstage the changes on the selected lines" $
      withGit $ \d t -> do
        let current = Buffer.toLines (docBuffer d)
            base = gtBase t
            unstaged = diffLines (gbIndex base) current
            staged = diffLines (gbHead base) (gbIndex base)
            selected = selectedLines d
            -- Index lines are selected through the buffer lines they show as.
            chosen i = not (IntSet.member (mapLine unstaged i) selected)
        writeIndex d base (Just (applySelected (gbHead base) (gbIndex base) staged chosen))
  , simple "git_stage_file" GGit "Stage the whole file as shown" $
      withGit $ \d t -> writeIndex d (gtBase t) (Just (Buffer.toLines (docBuffer d)))
  , simple "git_unstage_file" GGit "Unstage the whole file" $
      withGit $ \d t ->
        let base = gtBase t
         in writeIndex d base (if gbInHead base then Just (gbHead base) else Nothing)
  , simple "git_reset_selection" GGit "Undo the unstaged changes on the selected lines" $
      withGit $ \d t -> do
        let current = Buffer.toLines (docBuffer d)
            index = gbIndex (gtBase t)
            selected = selectedLines d
            restored = applySelected index current (diffLines index current) (not . (`IntSet.member` selected))
        if restored == current
          then info "nothing to reset here"
          else replaceLines restored
  ]
  where
    withGit k = do
      d <- getDoc
      case docGit d of
        GitTracked t -> k d t
        GitOutside -> failWith "not in a git repository"
        _ -> failWith "git status is still loading"
    writeIndex d base new = do
      request (StartJob (GitWriteIndex (docId d) base (docLineEnding d) new))
      info "staging…"

-- | Lines covered by any range.
selectedLines :: Document -> IntSet.IntSet
selectedLines d =
  IntSet.fromList [l | r <- ranges (docSelection d), l <- [posLine (rangeStart r) .. posLine (rangeEnd r)]]

-- | Replace the buffer's lines as one undoable change, keeping the cursor
-- on its line.
replaceLines :: [T.Text] -> EditorM ()
replaceLines new = modifyDoc $ \d ->
  let buf = Buffer.fromLines new
      line = min (length new - 1) (posLine (rangeHead (primary (docSelection d))))
   in replaceBuffer buf (single (point (Pos (max 0 line) 0))) d

jumpChange :: Bool -> EditorM ()
jumpChange forward = jumping $ do
  d <- getDoc
  case tracking (docGit d) of
    Nothing -> failWith "no git changes"
    Just t -> case changeStarts t of
      [] -> info "no changes"
      starts -> do
        let line = posLine (rangeHead (primary (docSelection d)))
            target
              | forward = case filter (> line) starts of
                  l : _ -> l
                  [] -> head' starts
              | otherwise = case filter (< line) (reverse starts) of
                  l : _ -> l
                  [] -> last starts
        modifyDoc (\doc -> doc {docSelection = single (point (Pos target 0))})
  where
    head' = \case
      l : _ -> l
      [] -> 0

-- | After every event: make sure the current document's git state is
-- loaded and its hunks are for its current version. One diff runs at a
-- time per document; when it answers for an older version, the next round
-- asks again, so a burst of edits costs one diff at a time.
gitHousekeeping :: EditorM ()
gitHousekeeping = do
  modify' syncUI
  d <- getDoc
  case (docGit d, docKind d, docPath d) of
    (GitUnknown, TextDoc, Just path) -> do
      modifyDoc (\doc -> doc {docGit = GitLoading})
      request (StartJob (GitLoad (docId d) path))
    (GitUnknown, _, _) -> modifyDoc (\doc -> doc {docGit = GitOutside})
    (GitTracked t, _, Just path)
      | gtReload t -> do
          setTracking t {gtReload = False}
          request (StartJob (GitLoad (docId d) path))
      | gtVersion t /= docVersion d && gtRequested t /= docVersion d -> do
          setTracking t {gtRequested = docVersion d}
          request (StartJob (GitDiff (docId d) (docVersion d) (gtBase t) (docBuffer d)))
    _ -> pure ()
  where
    setTracking t = modifyDoc (\doc -> doc {docGit = GitTracked t})

-- | Show each document's changes as signs, and its branch in the status
-- line (ADR-50); only when they changed since they were last shown.
syncUI :: Editor -> Editor
syncUI ed
  | fmap (\ui -> (puSigns ui, puSegments ui)) (Map.lookup "git" (edPluginUI ed)) == Just (signs, segments) = ed
  | otherwise = modifyPluginUI "git" (\ui -> ui {puSigns = signs, puSegments = segments}) ed
  where
    tracked = [(d, t) | d <- allDocuments ed, Just t <- [tracking (docGit d)]]
    signs = IntMap.fromList [(docId d, spans) | (d, t) <- tracked, let spans = gitSignSpans t, not (null spans)]
    segments =
      [ Segment SideRight branch Nothing 5 (Just (docId d))
      | (d, t) <- tracked
      , let branch = gbBranch (gtBase t)
      , not (T.null branch)
      ]

-- | The signs of a document's changes: unstaged ones over staged ones,
-- which are dimmer. A removal is marked on the line above it (or line 0).
gitSignSpans :: GitTracking -> [SignSpan]
gitSignSpans t = spans False (gtUnstaged t) <> spans True (gtStaged t)
  where
    spans staged hunks = [span' staged h | h <- hunks]
    span' staged h = case hunkKind h of
      Added -> SignSpan (hNewStart h) (hNewStart h + hNewCount h) (sign staged "▎" "diff.plus")
      Changed -> SignSpan (hNewStart h) (hNewStart h + hNewCount h) (sign staged "▎" "diff.delta")
      Removed -> let l = max 0 (hNewStart h - 1) in SignSpan l (l + 1) (sign staged "▁" "diff.minus")
    sign staged glyph scope
      | staged = GutterSign glyph (Face (scope <> ".staged") True) 10
      | otherwise = GutterSign glyph (face (scope <> ".gutter")) 11

-- | A git job reported back.
applyGitResult :: JobResult -> EditorM ()
applyGitResult = \case
  GitLoaded doc Nothing -> modify' (modifyDocument doc (\d -> d {docGit = GitOutside}))
  GitLoaded doc (Just base) -> modify' $ modifyDocument doc $ \d ->
    d
      { docGit = GitTracked $ case docGit d of
          -- Keep the old hunks on screen until the new diff arrives.
          GitTracked t -> t {gtBase = base, gtVersion = -1, gtRequested = -1}
          _ -> GitTracking base [] [] (-1) (-1) False
      }
  GitDiffed doc version unstaged staged -> modify' $ modifyDocument doc $ \d -> case docGit d of
    GitTracked t
      | docVersion d == version -> d {docGit = GitTracked t {gtUnstaged = unstaged, gtStaged = staged, gtVersion = version}}
      | otherwise -> d
    _ -> d
  GitWritten doc result -> do
    modify' (modifyDocument doc markGitReload)
    current <- gets (docId . edDoc)
    if current /= doc
      then pure ()
      else either (\e -> failWith ("git: " <> e)) (const (info "staged changes updated")) result
  _ -> pure ()

-- | Look the file up in git again (after a save or staging), keeping the
-- current signs until the answer comes.
markGitReload :: Document -> Document
markGitReload d = d {docGit = reload (docGit d)}
  where
    reload = \case
      GitTracked t -> GitTracked t {gtReload = True}
      _ -> GitUnknown
