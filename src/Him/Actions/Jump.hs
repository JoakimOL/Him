-- | The jumplist in the editor (ADR-47): @C-o@ / @C-i@ walk it, @C-s@
-- saves the selection, @space j@ lists it. Actions that jump wrap their
-- work in 'jumping', which remembers where the cursor was. The list
-- itself is "Him.Jumplist".
module Him.Actions.Jump
  ( actions
  , jumping
  , syncJumps
  , jumplistPicker
  , deleteJump
  , goToEntry
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find, findIndex)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Him.Action
import Him.Buffer qualified as Buffer
import Him.Document (Document (..), clampSelection, displayName)
import Him.Editor
import Him.EditorM
import Him.Jumplist
import Him.Mode (Mode (..))
import Him.Picker (PickTarget (..), Picker (..), newPicker, pickerItem)
import Him.Position (Pos (..))
import Him.Selection (primary, rangeHead)

actions :: [Action]
actions =
  [ action "jump_backward" GMovement "Go back to where the cursor was before the last jump" (optional "1" 1 (int "count")) $ \n -> do
      -- Going back pushes where the cursor is (Him.Jumplist.backward).
      gets (jumpDoc . here) >>= \doc -> modify' (track doc . syncJumps)
      ed <- get
      case backward n (here ed) (jumplist ed) of
        Just (j, jl) -> setJumplist jl >> goTo j
        Nothing -> failWith "no earlier jump"
  , action "jump_forward" GMovement "Go forward again in the jumplist" (optional "1" 1 (int "count")) $ \n -> do
      modify' syncJumps
      gets (forward n . jumplist) >>= \case
        Just (j, jl) -> setJumplist jl >> goTo j
        Nothing -> failWith "no later jump"
  , simple "save_selection" GSelection "Save the selection to the jumplist" $ do
      gets here >>= pushJump
      info "selection saved to the jumplist"
  , simple "jumplist_picker" GBuffers "List the jumplist: ret jumps, del removes an entry" $ do
      modify' syncJumps
      p <- gets jumplistPicker
      modify' (\e -> e {edPicker = Just p, edMode = Picking})
  ]

-- | Run something that may jump: if it moved the cursor (or went to
-- another document), the place before is pushed onto the jumplist.
jumping :: EditorM a -> EditorM a
jumping act = do
  before <- gets here
  r <- act
  after <- gets here
  if after /= before then pushJump before else pure ()
  pure r

-- | Where the cursor is in the focused window.
here :: Editor -> Jump
here ed = Jump (docId (edDoc ed)) (docSelection (edDoc ed))

jumplist :: Editor -> Jumplist
jumplist ed = IntMap.findWithDefault emptyJumplist (edFocus ed) (edJumps ed)

setJumplist :: Jumplist -> EditorM ()
setJumplist jl = modify' (\e -> e {edJumps = IntMap.insert (edFocus e) jl (edJumps e)})

pushJump :: Jump -> EditorM ()
pushJump j = modify' $ \e ->
  let e' = track (jumpDoc j) (syncJumps e)
   in e' {edJumps = IntMap.insert (edFocus e') (push j (jumplist e')) (edJumps e')}

-- | Start following a document's edits (its jumps' positions refer to its
-- text as it is now).
track :: Int -> Editor -> Editor
track doc e = case find ((== doc) . docId) (allDocuments e) of
  Just d | IntMap.notMember doc (edJumpTexts e) -> e {edJumpTexts = IntMap.insert doc (docVersion d, docBuffer d) (edJumpTexts e)}
  _ -> e

-- | Show a place: its document (if still open) and the selection there.
goTo :: Jump -> EditorM ()
goTo j =
  gets (findIndex ((== jumpDoc j) . docId . bufDoc) . fst . buffers) >>= \case
    Nothing -> failWith "that buffer is closed"
    Just i -> do
      modify' (gotoBuffer i)
      modifyDoc (\d -> d {docSelection = clampSelection (docBuffer d) (jumpSelection j)})

-- | Go to the jumplist's entry at an index (picked in @space j@), as a
-- jump of its own, so the list is not cut short.
goToEntry :: Int -> EditorM ()
goToEntry i = gets (Seq.lookup i . jlJumps . jumplist) >>= maybe (pure ()) (jumping . goTo)

-- | Remove the entry at an index from the focused window's jumplist.
deleteJump :: Int -> EditorM ()
deleteJump i = gets (remove i . jumplist) >>= setJumplist

-- | The @space j@ picker: the focused window's jumps, newest first.
jumplistPicker :: Editor -> Picker
jumplistPicker ed = newPicker "jumplist (del removes)" items
  where
    docs = IntMap.fromList [(docId d, d) | d <- allDocuments ed]
    items =
      [ pickerItem (displayName d <> ":" <> T.pack (show (line + 1))) (PickJump i) (T.strip (T.take 300 (Buffer.lineAt line (docBuffer d))))
      | (i, j) <- reverse (zip [0 ..] (toList (jlJumps (jumplist ed))))
      , Just d <- [IntMap.lookup (jumpDoc j) docs]
      , let line = min (Buffer.lineCount (docBuffer d) - 1) (posLine (rangeHead (primary (jumpSelection j))))
      ]

-- | Keep the jumps in step with the documents (after every event, and
-- before the list is used): positions follow edits made since they were
-- recorded, jumps into closed documents and lists of closed windows go.
syncJumps :: Editor -> Editor
syncJumps ed0 = tidy (IntMap.foldlWithKey' one ed0 (edJumpTexts ed0))
  where
    one ed doc (version, old) = case find ((== doc) . docId) (allDocuments ed) of
      Nothing -> ed {edJumps = IntMap.map (mapJumps (\j -> if jumpDoc j == doc then Nothing else Just j)) (edJumps ed)}
      Just d
        | docVersion d == version -> ed
        | otherwise ->
            let new = docBuffer d
                move = maybe id mapThroughChange (Buffer.changeBetween old new)
                f j = Just (if jumpDoc j == doc then j {jumpSelection = clampSelection new (move (jumpSelection j))} else j)
             in ed {edJumps = IntMap.map (mapJumps f) (edJumps ed), edJumpTexts = IntMap.insert doc (docVersion d, new) (edJumpTexts ed)}
    tidy ed =
      let windows = IntSet.fromList (edFocus ed : IntMap.keys (edWindows ed))
          jumps = IntMap.filterWithKey (\w _ -> IntSet.member w windows) (edJumps ed)
          used = IntSet.fromList [jumpDoc j | jl <- IntMap.elems jumps, j <- toList (jlJumps jl)]
       in ed {edJumps = jumps, edJumpTexts = IntMap.filterWithKey (\d _ -> IntSet.member d used) (edJumpTexts ed)}
