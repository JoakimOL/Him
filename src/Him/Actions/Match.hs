-- | Match mode (@m@, ADR match-mode), as in Helix: @m m@ goes to the matching
-- bracket; @m s@ / @m r@ / @m d@ add, replace and delete the pair around
-- each selection; @m i@ / @m a@ select inside / around a text object
-- ("Him.TextObject"). The commands that need a character wait for the
-- next key ('awaitedMatchKey').
module Him.Actions.Match
  ( actions
  , awaitedMatchKey
  ) where

import Control.Monad.Trans.State.Strict (modify')
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Action
import Him.Buffer (Buffer, deleteRange, insertText)
import Him.Document (Document (..))
import Him.Edit (Edit)
import Him.Editor (Await (..), Editor (..))
import Him.EditorM
import Him.Position (Pos (..))
import Him.Selection
import Him.TextObject

actions :: [Action]
actions =
  [ simple "match_brackets" GMovement "Go to the bracket matching the one under the cursor" $
      motion (\b r -> maybe r point (matchingBracket b (rangeHead r)))
  , simple "surround_add" GEditing "Surround each selection with a pair (the next key: ( [ { < or a quote…)" (await AwaitSurround)
  , simple "surround_delete" GEditing "Delete the pair around each selection (the next key names it; m: the closest)" (await AwaitDeleteSurround)
  , simple "surround_replace" GEditing "Replace the pair around each selection (the next two keys: old, new)" (await AwaitReplaceSurround)
  , simple "select_textobject_inner" GSelection "Select inside a text object (next key: w W p m ( [ { < \" ' `)" (await (AwaitObject True))
  , simple "select_textobject_around" GSelection "Select around a text object (next key: w W p m ( [ { < \" ' `)" (await (AwaitObject False))
  ]

await :: Await -> EditorM ()
await a = modify' (\e -> e {edAwait = Just a})

-- | The character a match-mode command waited for.
awaitedMatchKey :: Await -> Char -> EditorM ()
awaitedMatchKey waiting ch = case waiting of
  AwaitSurround -> edit (surround (pairFor ch))
  AwaitDeleteSurround -> edit (deleteSurround ch)
  AwaitReplaceSurround -> await (AwaitReplaceSurroundWith ch)
  AwaitReplaceSurroundWith old -> edit (replaceSurround old (pairFor ch))
  AwaitObject inside -> modifyDoc $ \d ->
    d {docSelection = mapRanges (\r -> fromMaybe r (textObject inside ch (docBuffer d) (rangeHead r))) (docSelection d)}
  AwaitFind {} -> pure ()
  AwaitReplaceChar -> pure ()
  AwaitRegister -> pure ()
  AwaitInsertRegister -> pure ()

-- | Put a pair around a range; the range then covers the pair too.
surround :: (Char, Char) -> Edit
surround (open, close) b r =
  let Pos sl sc = rangeStart r
      Pos el ec = rangeEnd r
      (b1, _) = insertText (Pos el (ec + 1)) (T.singleton close) b
      (b2, _) = insertText (Pos sl sc) (T.singleton open) b1
      closeAt = if sl == el then Pos el (ec + 2) else Pos el (ec + 1)
   in (b2, Range (Pos sl sc) closeAt Nothing)

-- | The pair around a range (the closest one for @m@).
pairAround :: Char -> Buffer -> Range -> Maybe (Pos, Pos)
pairAround ch b r = do
  Range o cl _ <- textObject False (if ch == 'm' then 'm' else ch) b (rangeStart r)
  if cl >= rangeEnd r then Just (o, cl) else Nothing

deleteSurround :: Char -> Edit
deleteSurround ch b r = case pairAround ch b r of
  Nothing -> (b, r)
  Just (o, cl) ->
    let b' = deleteAt o (deleteAt cl b)
        -- Positions after the opening character on its line move back one.
        shift p@(Pos l c) = if l == posLine o && c > posCol o then Pos l (c - 1) else p
     in (b', r {rangeAnchor = shift (rangeAnchor r), rangeHead = shift (rangeHead r)})

replaceSurround :: Char -> (Char, Char) -> Edit
replaceSurround old (open, close) b r = case pairAround old b r of
  Nothing -> (b, r)
  Just (o, cl) -> (replaceAt o open (replaceAt cl close b), r)

deleteAt :: Pos -> Buffer -> Buffer
deleteAt p@(Pos l c) = deleteRange p (Pos l (c + 1))

replaceAt :: Pos -> Char -> Buffer -> Buffer
replaceAt p ch = fst . insertText p (T.singleton ch) . deleteAt p
