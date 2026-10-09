-- | Match mode (@m@) and @I@ / @A@, through the default keys.
module Test.Match
  ( matchTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Key (parseKeys)
import Him.Position (Pos (..))
import Him.Selection (primary, rangeEnd, rangeStart)
import Him.Session (handleEvent)
import Test.Harness
import Test.Util

matchTests :: IO [Test]
matchTests = do
  config <- either (fail . show) pure defaultConfig
  let start t = newEditor (20, 80) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      selected :: Text -> Text -> IO (Pos, Pos)
      selected t ks = (\e -> let r = primary (docSelection (edDoc e)) in (rangeStart r, rangeEnd r)) <$> typeKeys ks (start t)
      textAfter :: Text -> Text -> IO Text
      textAfter t ks = B.toText . docBuffer . edDoc <$> typeKeys ks (start t)
      at = Pos 0
  innerWord <- selected "foo bar" "f b m i w"
  aroundWord <- selected "foo bar" "m a w"
  innerParen <- selected "(a b)" "l m i ("
  aroundParen <- selected "(a b)" "l m a )"
  nested <- selected "(a (b) c)" "f b m i ("
  nestedAround <- selected "(a (b) c)" "f b m a ("
  quoted <- selected "say \"hi there\" now" "f h m i \""
  closest <- selected "f(x, [1, 2])" "f 1 m i m"
  paragraph <- selected "a\nb\n\nc" "m i p"
  matchFwd <- selected "(a (b) c)" "m m"
  matchBack <- selected "(a (b) c)" "f ) m m"
  added <- textAfter "foo bar" "m i w m s ("
  addedQuotes <- textAfter "a\na" "% s a ret m s \""
  deleted <- textAfter "(foo) bar" "l m d ("
  deletedClosest <- textAfter "[foo] bar" "l m d m"
  replaced <- textAfter "(foo)" "l m r ( ["
  -- Letters for pairs that are hard to type: b () B {} r [] c <> q ``.
  letterInner <- mapM (\(t, k) -> selected t ("l m i " <> k)) [("(ab)", "b"), ("{ab}", "B"), ("[ab]", "r"), ("<ab>", "c"), ("`ab`", "q")]
  letterSurround <- textAfter "foo" "m i w m s B"
  letterReplace <- textAfter "(foo)" "l m r b r"
  letterDelete <- textAfter "<foo>" "l m d c"
  insertStart <- textAfter "  foo" "I x esc"
  insertEnd <- textAfter "  foo" "A x esc"
  pure
    [ test "m i w selects the word, m a w with the blank after it" (assertEqual ((at 4, at 6), (at 0, at 3)) (innerWord, aroundWord))
    , test "m i ( / m a ) select inside / around brackets" (assertEqual ((at 1, at 3), (at 0, at 4)) (innerParen, aroundParen))
    , test "the innermost pair around the cursor counts" (assertEqual ((at 4, at 4), (at 3, at 5)) (nested, nestedAround))
    , test "m i \" selects inside quotes" (assertEqual (at 5, at 12) quoted)
    , test "m i m selects inside the closest pair" (assertEqual (at 6, at 9) closest)
    , test "m i p selects the paragraph" (assertEqual (at 0, Pos 1 1) paragraph)
    , test "m m goes to the matching bracket, both ways" (assertEqual ((at 8, at 8), (at 3, at 3)) (matchFwd, matchBack))
    , test "m s ( surrounds the selection" (assertEqual "(foo) bar" added)
    , test "b B r c q name () {} [] <> `` in m i" (assertEqual (replicate 5 (at 1, at 2)) letterInner)
    , test "the letters work in m s, m r and m d" (assertEqual ("{foo}", "[foo]", "foo") (letterSurround, letterReplace, letterDelete))
    , test "m s surrounds every selection" (assertEqual "\"a\"\n\"a\"" addedQuotes)
    , test "m d ( deletes the pair around the cursor" (assertEqual "foo bar" deleted)
    , test "m d m deletes the closest pair" (assertEqual "foo bar" deletedClosest)
    , test "m r ( [ replaces the pair" (assertEqual "[foo]" replaced)
    , test "I inserts after the indentation, A at the end of the line" (assertEqual ("  xfoo", "  foox") (insertStart, insertEnd))
    ]
