-- | The text core: buffers and the rope, motions, edits, search, undo, selections, widths, the view, regexes.
module Test.Text
  ( bufferTests
  , ropeModelTests
  , changeTests
  , findCharTests
  , searchTests
  , motionTests
  , editTests
  , historyTests
  , multiSelectionTests
  , widthTests
  , viewTests
  , regexTests
  ) where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.Buffer qualified as B
import Him.Edit
import Him.Search (Direction (..), Match (..), compileNeedle, findMatch, selectMatches)
import Him.History qualified as H
import Him.Regex
import Him.Motion
import Him.Position (Pos (..))
import Him.Selection
import Him.View (View (..), scrollToCursor)
import Him.TextWidth (charIndexAtCol, charWidth, displayCol, glyphs)
import Data.Text qualified as T
import Test.Harness
import Test.Util

bufferTests :: [Test]
bufferTests =
  [ test "empty has one line" (assertEqual 1 (B.lineCount B.empty))
  , test "fromText/toText round trip" (assertEqual "a\nb\n" (B.toText (buf "a\nb\n")))
  , test "insert in line" (assertEqual ("abXc", Pos 0 3) (ins (Pos 0 2) "X" "abc"))
  , test "insert newline" (assertEqual ("ab\nc", Pos 1 0) (ins (Pos 0 2) "\n" "abc"))
  , test "insert multi-line" (assertEqual ("aX\nYZ\nYb", Pos 2 1) (ins (Pos 0 1) "X\nYZ\nY" "ab"))
  , test "delete within line" (assertEqual "ac" (B.toText (B.deleteRange (Pos 0 1) (Pos 0 2) (buf "abc"))))
  , test "delete across lines" (assertEqual "ad" (B.toText (B.deleteRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd"))))
  , test "textRange across lines" (assertEqual "b\nc" (B.textRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd")))
  , test "nextPos crosses line end" (assertEqual (Pos 1 0) (B.nextPos (buf "a\nb") (Pos 0 1)))
  , test "nextPos stops at end" (assertEqual (Pos 1 1) (B.nextPos (buf "a\nb") (Pos 1 1)))
  , test "prevPos crosses line start" (assertEqual (Pos 0 1) (B.prevPos (buf "a\nb") (Pos 1 0)))
  , test "charAt line end is newline" (assertEqual (Just '\n') (B.charAt (Pos 0 1) (buf "a\nb")))
  , test "charAt end of file" (assertEqual Nothing (B.charAt (Pos 1 1) (buf "a\nb")))
  ]
  where
    ins :: Pos -> Text -> Text -> (Text, Pos)
    ins p t s = let (b', p') = B.insertText p t (buf s) in (B.toText b', p')

ropeModelTests :: [Test]
ropeModelTests =
  [ test "fromRegions joins regions" (assertEqual ["a", "b", "c", "d"] (B.toLines (B.fromRegions [(False, "a\nb"), (False, "c\nd")])))
  , test "CR regions strip \\r" (assertEqual ["a", "b"] (B.toLines (B.fromRegions [(True, "a\r\nb\r")])))
  , test "2000 random edits match the model" (runModel 2000 1)
  , test "edits on a single line" (runModel 500 7)
  ]
  where
    start = B.fromRegions [(False, T.intercalate "\n" [T.pack ("line " <> show i) | i <- [n .. n + 49 :: Int]]) | n <- [0, 50 .. 450]]
    model0 = B.toLines start
    runModel steps seed = go steps (randoms seed) start model0
    go :: Int -> [Int] -> B.Buffer -> [Text] -> Either String ()
    go 0 _ b m = check b m
    go n (r1 : r2 : r3 : r4 : rs) b m = case check b m of
      Left e -> Left ("before step " <> show n <> ": " <> e)
      Right () ->
        let lineN = length m
            l1 = r1 `mod` lineN
            c1 = r2 `mod` (T.length (m !! l1) + 1)
            p1 = Pos l1 c1
         in if even r3
              then
                let t = ["x", "\n", "ab\ncd", "\n\n", "é"] !! (r4 `mod` 5)
                    (b', _) = B.insertText p1 t b
                 in go (n - 1) rs b' (modelInsert m p1 t)
              else
                let l2 = min (lineN - 1) (l1 + r4 `mod` 3)
                    c2 = (r4 `div` 3) `mod` (T.length (m !! l2) + 1)
                    p2 = max p1 (Pos l2 c2)
                 in go (n - 1) rs (B.deleteRange p1 p2 b) (modelDelete m p1 p2)
    go _ _ b m = check b m
    check b m
      | B.toLines b /= m = Left ("lines differ: " <> show (take 3 (B.toLines b)) <> " vs " <> show (take 3 m))
      | B.lineCount b /= length m = Left "lineCount differs"
      | [B.lineAt i b | i <- [0 .. length m - 1]] /= m = Left "lineAt differs"
      | B.linesDownFrom (length m - 1) b /= reverse m = Left "linesDownFrom differs"
      | otherwise = Right ()
    modelText = T.intercalate "\n"
    offsetOf m (Pos l c) = sum [T.length x + 1 | x <- take l m] + c
    modelInsert m p t = let whole = modelText m; (a, z) = T.splitAt (offsetOf m p) whole in T.splitOn "\n" (a <> t <> z)
    modelDelete m p q = let whole = modelText m in T.splitOn "\n" (T.take (offsetOf m p) whole <> T.drop (offsetOf m q) whole)

changeTests :: [Test]
changeTests =
  [ test "equal texts have no change" (assertEqual Nothing (B.changeBetween (buf "a\nb") (buf "a\nb")))
  , test "a typed character" (assertEqual (Just (Pos 1 2, Pos 1 2, "X")) (B.changeBetween (buf "ab\ncd\nef") (buf "ab\ncdX\nef")))
  , test "a line added at the end" (assertEqual (Just (Pos 1 1, Pos 1 1, "\nc")) (B.changeBetween (buf "a\nb") (buf "a\nb\nc")))
  , test "the last line removed" (assertEqual (Just (Pos 0 1, Pos 1 1, "")) (B.changeBetween (buf "a\nb") (buf "a")))
  , test "random edits: the change rebuilds the new text" (mapM_ changeModel (take 500 (chunks (randoms 31))))
  , test "edits in a large loaded file are found" $
      let big = B.fromRegions [(False, T.intercalate "\n" [T.pack (show i) | i <- [n .. n + 999 :: Int]]) | n <- [0, 1000 .. 9000]]
          (edited, _) = B.insertText (Pos 5500 1) "X" big
       in assertEqual (Just (Pos 5500 1, Pos 5500 1, "X")) (B.changeBetween big edited)
  ]
  where
    chunks xs = let (a, z) = splitAt 6 xs in a : chunks z
    alphabet = ["a", "bc", "", "def"]
    changeModel rs = case rs of
      (r1 : r2 : r3 : r4 : r5 : _) ->
        let old = T.intercalate "\n" [alphabet !! ((r1 `div` (4 ^ i)) `mod` 4) | i <- [0 .. r2 `mod` 6 :: Int]]
            cut = r3 `mod` (T.length old + 1)
            len = r4 `mod` 4
            ins = ["", "x", "\n", "y\nz\n", "\n\n"] !! (r5 `mod` 5)
            new = T.take cut old <> ins <> T.drop (cut + len) old
         in case B.changeBetween (buf old) (buf new) of
              Nothing -> if old == new then Right () else Left ("missed: " <> show (old, new))
              Just (s0, e0, t) ->
                let rebuilt = T.take (offsetOf old s0) old <> t <> T.drop (offsetOf old e0) old
                 in if rebuilt == new then Right () else Left (show (old, new, s0, e0, t))
      _ -> Right ()
    offsetOf t (Pos l c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" t)] + c

findCharTests :: [Test]
findCharTests =
  [ test "f selects to the character" (assertEqual (r 0 0 0 3) (find' True False 'd' 1 "abcdef" 0 0))
  , test "a count finds a later one" (assertEqual (r 0 0 0 3) (find' True False 'a' 2 "xaxaxa" 0 0))
  , test "t stops before it" (assertEqual (r 0 0 0 2) (find' True True 'd' 1 "abcdef" 0 0))
  , test "a repeated t skips a target right next to the cursor" (assertEqual (r 0 0 0 0, r 0 0 0 2) (find' True True 'b' 1 "abcbx" 0 0, findChar True True True 'b' 1 (buf "abcbx") (r 0 0 0 0)))
  , test "F and T go back" (assertEqual (r 0 5 0 1, r 0 5 0 2) (find' False False 'b' 1 "abcdef" 0 5, find' False True 'b' 1 "abcdef" 0 5))
  , test "the search crosses lines" (assertEqual (r 0 1 1 1) (find' True False 'y' 1 "ab\nxy" 0 1))
  , test "a line break can be found" (assertEqual (r 0 0 0 2) (find' True False '\n' 1 "ab\nxy" 0 0))
  , test "not found leaves the selection" (assertEqual (r 0 1 0 1) (find' True False 'z' 1 "abc" 0 1))
  ]
  where
    r a b c d = Range (Pos a b) (Pos c d) Nothing
    find' fwd till ch n t l c = findChar False fwd till ch n (buf t) (r l c l c)

searchTests :: [Test]
searchTests =
  [ test "empty and multi-line patterns are rejected" (assertEqual (Nothing, Nothing) (compileNeedle True "", compileNeedle True "a\nb"))
  , test "smart case: lower-case pattern ignores case" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "smart case: upper-case pattern is exact" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC Abc" "ABC" (Pos 0 0)))
  , test "match end is inclusive" (assertEqual (Just (Pos 0 6)) (matchEnd <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "wraps around" (assertEqual (Just (Match (Pos 0 0) (Pos 0 1) True)) (findIn "ab ab" "ab" (Pos 0 3)))
  , test "multi-byte columns" (assertEqual (Just (Pos 0 3)) (matchStart <$> findIn "漢字 x" "x" (Pos 0 0)))
  , test "1500 random searches match the naive search" (randomSearches 1500)
  ]
  where
    findIn t p pos = compileNeedle True p >>= \n -> findMatch True Forward n (buf t) pos
    alphabet = "aAbB é漢\t" :: String
    randomSearches :: Int -> Either String ()
    randomSearches count = go count (randoms 99)
      where
        go 0 _ = Right ()
        go k (r1 : r2 : r3 : r4 : r5 : r6 : rs) =
          let (b, rs') = randomBuffer r1 rs
              ls = B.toLines b
              needle = T.pack [alphabet !! (x `mod` length alphabet) | x <- take (1 + r2 `mod` 3) rs']
              l = r3 `mod` length ls
              pos = Pos l (r4 `mod` (T.length (ls !! l) + 1))
              dir = if even r5 then Forward else Backward
              expected = naive dir needle ls pos
              actual = (\n -> matchStart <$> findMatch True dir n b pos) =<< compileNeedle True needle
           in if expected == actual
                then go (k - 1) (drop 6 rs')
                else Left (show (dir, needle, pos, ls) <> ": expected " <> show expected <> ", got " <> show actual <> show r6)
        go _ _ = Right ()
    -- Several regions (blocks) plus a few edits, so searches cross block
    -- boundaries and edited single-line blocks.
    randomBuffer r rs =
      let lineOf xs = T.pack [alphabet !! (x `mod` length alphabet) | x <- xs]
          rows = [lineOf (take (x `mod` 7) (drop (i * 7) rs)) | (i, x) <- zip [0 .. 11] (drop 100 rs)]
          regions = [(False, T.intercalate "\n" chunk) | chunk <- chunksOf (1 + r `mod` 4) rows]
          b0 = B.fromRegions regions
          b1 = fst (B.insertText (Pos (r `mod` B.lineCount b0) 0) (lineOf (take 3 (drop 200 rs))) b0)
       in (b1, drop 300 rs)
    chunksOf n xs = case splitAt n xs of
      (a, []) -> [a]
      (a, z) -> a : chunksOf n z
    -- All match starts in document order, overlapping matches included.
    naive dir needle ls pos =
      let fold = not (T.any isUpperAscii needle)
          norm = if fold then T.map lowerAscii else id
          n = norm needle
          starts = [Pos li c | (li, line) <- zip [0 ..] ls, let ln = norm line, c <- [0 .. T.length ln - T.length n], n `T.isPrefixOf` T.drop c ln]
       in case dir of
            Forward -> case filter (> pos) starts of
              (p : _) -> Just p
              [] -> case starts of
                (p : _) -> Just p
                [] -> Nothing
            Backward -> case reverse (filter (< pos) starts) of
              (p : _) -> Just p
              [] -> case reverse starts of
                (p : _) -> Just p
                [] -> Nothing
    isUpperAscii c = c >= 'A' && c <= 'Z'
    lowerAscii c = if isUpperAscii c then toEnum (fromEnum c + 32) else c

motionTests :: [Test]
motionTests =
  [ test "h wraps to previous line end" (assertEqual (Pos 0 2, Pos 0 2) (runMotion charLeft "ab\ncd" (Pos 1 0)))
  , test "j keeps desired column" $
      let b = buf "abcd\na\nabcd"
          r1 = lineDown b (point (Pos 0 3))
          r2 = lineDown b r1
       in assertEqual (Pos 1 1, Pos 2 3) (rangeHead r1, rangeHead r2)
  , test "w selects word and trailing blanks" (assertEqual (Pos 0 0, Pos 0 5) (runMotion nextWordStart "hello world" (Pos 0 0)))
  , test "w from end of word selects next word" (assertEqual (Pos 0 6, Pos 0 10) (runMotion nextWordStart "hello world" (Pos 0 5)))
  , test "w skips to next line" (assertEqual (Pos 1 0, Pos 1 2) (runMotion nextWordStart "ab\ncde" (Pos 0 1)))
  , test "w treats punctuation as its own word" (assertEqual (Pos 0 0, Pos 0 2) (runMotion nextWordStart "foo.bar" (Pos 0 0)))
  , test "e selects to end of word" (assertEqual (Pos 0 0, Pos 0 4) (runMotion nextWordEnd "hello world" (Pos 0 0)))
  , test "e from end of word goes to next word end" (assertEqual (Pos 0 5, Pos 0 10) (runMotion nextWordEnd "hello world" (Pos 0 4)))
  , test "b selects back to word start" (assertEqual (Pos 0 8, Pos 0 6) (runMotion prevWordStart "hello world" (Pos 0 8)))
  , test "b from word start goes to previous word" (assertEqual (Pos 0 5, Pos 0 0) (runMotion prevWordStart "hello world" (Pos 0 6)))
  , test "b at file start stays" (assertEqual (Pos 0 0, Pos 0 0) (runMotion prevWordStart "hello" (Pos 0 0)))
  , test "w at end of file stays" (assertEqual (Pos 0 5, Pos 0 5) (runMotion nextWordStart "hello" (Pos 0 5)))
  , test "x selects the line with its newline" (assertEqual (Pos 1 0, Pos 1 2) (runMotion selectLine "ab\ncd\nef" (Pos 1 1)))
  , test "x twice extends a line" $
      let b = buf "ab\ncd\nef"
          r = selectLine b (selectLine b (point (Pos 0 1)))
       in assertEqual (Pos 0 0, Pos 1 2) (rangeAnchor r, rangeHead r)
  , test "extend keeps the anchor" $
      let r = applyMotion Extend charRight (buf "abc") (Range (Pos 0 0) (Pos 0 1) Nothing)
       in assertEqual (Pos 0 0, Pos 0 2) (rangeAnchor r, rangeHead r)
  ]

editTests :: [Test]
editTests =
  [ test "backspace joins lines" (assertEqual ("abcd", Pos 0 2) (runEdit deleteBackward "ab\ncd" (point (Pos 1 0))))
  , test "backspace at start does nothing" (assertEqual ("ab", Pos 0 0) (runEdit deleteBackward "ab" (point (Pos 0 0))))
  , test "newline keeps indentation" (assertEqual ("  ab\n  c", Pos 1 2) (runEdit insertNewline "  abc" (point (Pos 0 4))))
  , test "delete selection is inclusive" (assertEqual ("ad", Pos 0 1) (runEdit deleteSelection "abcd" (Range (Pos 0 1) (Pos 0 2) Nothing)))
  , test "delete a line with its newline" (assertEqual ("ab\nef", Pos 1 0) (runEdit deleteSelection "ab\ncd\nef" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "delete the last line removes it" (assertEqual ("ab", Pos 0 0) (runEdit deleteSelection "ab\ncd" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "open line below" (assertEqual ("  ab\n  \ncd", Pos 1 2) (runEdit openLineBelow "  ab\ncd" (point (Pos 0 1))))
  ]

historyTests :: [Test]
historyTests =
  [ test "nothing to undo" (assertEqual Nothing (fst <$> H.undo (snap "a") H.emptyHistory))
  , test "only the first state of a change is kept" $
      let h = H.commit (H.beginChange (snap "b") (H.beginChange (snap "a") H.emptyHistory))
       in assertEqual (Just (snap "a")) (fst <$> H.undo (snap "c") h)
  , test "undo then redo returns to the current state" $
      let h = H.commit (H.beginChange (snap "a") H.emptyHistory)
       in assertEqual (Just (snap "b")) (H.undo (snap "b") h >>= \(s', h') -> fst <$> H.redo s' h')
  , test "a new change clears redo" $
      let h1 = H.commit (H.beginChange (snap "a") H.emptyHistory)
          h2 = maybe H.emptyHistory snd (H.undo (snap "b") h1)
          h3 = H.commit (H.beginChange (snap "a") h2)
       in assertEqual Nothing (fst <$> H.redo (snap "x") h3)
  ]
  where
    snap t = H.Snapshot (buf t) (single (point (Pos 0 0)))

data CursorEdit = Insert' | Backspace' | Delete'

multiSelectionTests :: [Test]
multiSelectionTests =
  [ test "fromRanges sorts and keeps the primary" $
      assertEqual (Just ([r 0 4 0 5, r 1 0 1 0], 1)) (shape <$> fromRanges [r 1 0 1 0, r 0 4 0 5] 0)
  , test "fromRanges merges overlaps" $
      assertEqual (Just ([r 0 0 0 6], 0)) (shape <$> fromRanges [r 0 0 0 3, r 0 2 0 6] 1)
  , test "fromRanges merges equal cursors" $
      assertEqual (Just ([r 0 2 0 2], 0)) (shape <$> fromRanges [r 0 2 0 2, r 0 2 0 2] 1)
  , test "remove and rotate the primary" $
      let sel = sel_ [r 0 0 0 0, r 1 0 1 0, r 2 0 2 0] 1
       in assertEqual ([r 0 0 0 0, r 2 0 2 0], 1, 2, 0) (ranges (removePrimary sel), primaryIndex (removePrimary sel), primaryIndex (rotatePrimary 1 sel), primaryIndex (rotatePrimary (-1) sel))
  , test "inserting at several cursors" $
      let sel = sel_ [r 0 1 0 1, r 1 0 1 0, r 0 3 0 3] 0
          (b, sel') = applyEdits (const (insertAtHead "X")) (buf "abcd\nef") sel
       in assertEqual ("aXbcXd\nXef", [r 0 2 0 2, r 0 5 0 5, r 1 1 1 1]) (B.toText b, ranges sel')
  , test "newlines at several cursors" $
      let sel = sel_ [r 0 1 0 1, r 0 2 0 2] 0
          (b, sel') = applyEdits (const insertNewline) (buf "abc") sel
       in assertEqual ("a\nb\nc", [r 1 0 1 0, r 2 0 2 0]) (B.toText b, ranges sel')
  , test "deleting several selections" $
      let sel = sel_ [r 0 0 0 1, r 0 4 0 5, r 1 1 1 2] 0
          (b, _) = applyEdits (const deleteSelection) (buf "one two\nthree") sel
       in assertEqual "e o\ntee" (B.toText b)
  , test "random multi-cursor inserts match a model" (multiModel 300 11 Insert')
  , test "random multi-cursor backspaces match a model" (multiModel 300 13 Backspace')
  , test "random multi-cursor forward deletes match a model" (multiModel 300 19 Delete')
  , test "random multi-range deletes match a model" (mapM_ deleteModel (take 400 (chunks (randoms 17))))
  , test "deleting a line and the last line" $
      let (b, _) = applyEdits (const deleteSelection) (buf "a\nb\nc") (sel_ [r 1 0 1 1, r 2 0 2 1] 0)
       in assertEqual "a" (B.toText b)
  , test "deleteForward at adjacent cursors" $
      let (b, sel') = applyEdits (const deleteForward) (buf "abcd") (sel_ [r 0 1 0 1, r 0 2 0 2] 0)
       in assertEqual ("ad", [r 0 1 0 1]) (B.toText b, ranges sel')
  , test "copy_selection_on_next_line skips short lines" $
      let sel = copySelectionBelow (buf "abcd\nx\nabcd") (single (r 0 2 0 3))
       in assertEqual ([r 0 2 0 3, r 2 2 2 3], 1) (shape sel)
  , test "split on newlines drops the line breaks" $
      assertEqual ([r 0 1 0 2, r 1 0 1 2, r 3 0 3 1], 0) (shape (splitOnNewlines (buf "abc\ndef\n\nghi") (single (r 0 1 3 1))))
  , test "select matches inside the selection" $
      let needle = needle_ "ab"
       in assertEqual (Just ([r 0 0 0 1, r 0 3 0 4, r 1 2 1 3], 0)) (shape <$> selectMatches needle (buf "ab ab\nxxab ab") (single (r 0 0 1 4)))
  , test "no matches" $
      let needle = needle_ "zz"
       in assertEqual Nothing (shape <$> selectMatches needle (buf "ab") (single (r 0 0 0 1)))
  ]
  where
    r l1 c1 l2 c2 = Range (Pos l1 c1) (Pos l2 c2) Nothing
    sel_ rs i = fromMaybe (error "no ranges") (fromRanges rs i)
    needle_ t = fromMaybe (error "bad needle") (compileNeedle True t)
    shape sel = (ranges sel, primaryIndex sel)
    -- Point cursors at random offsets (at least 2 apart), one random edit
    -- applied to all of them, compared with the same edit on a string.
    multiModel :: Int -> Int -> CursorEdit -> Either String ()
    multiModel n seed kind = mapM_ (one kind) (take n (chunks (randoms seed)))
    chunks xs = let (a, b) = splitAt 8 xs in a : chunks b
    one kind rs = case rs of
      (r1 : r2 : r3 : more) ->
        let txt = T.intercalate "\n" (take (2 + r1 `mod` 4) ["hello", "", "a b c", "xyz", "q"])
            len = T.length txt
            offs = spread (map (`mod` (len + 1)) (take (1 + r2 `mod` 4) more))
            ins = ["X", "\n", "ab\ncd", ""] !! (r3 `mod` 3)
            sel = sel_ [let p = posOf txt o in Range p p Nothing | o <- offs] 0
            edit1 = case kind of
              Insert' -> insertAtHead ins
              Backspace' -> deleteBackward
              Delete' -> deleteForward
            (b, sel') = applyEdits (const edit1) (buf txt) sel
            (expectText, expectOffs) = case kind of
              Insert' -> (insertAll txt offs ins, [o + i * T.length ins + T.length ins | (i, o) <- zip [0 ..] offs])
              Backspace' -> deleteAll txt [o - 1 | o <- offs, o > 0] offs
              Delete' -> deleteAll txt [o | o <- offs, o < len] offs
            got = (B.toText b, map (offOf (B.toText b) . rangeHead) (ranges sel'))
         in if got == (expectText, nub expectOffs) then Right () else Left (show (txt, offs, ins, got, (expectText, expectOffs)))
      _ -> Right ()
    -- Random sorted, non-overlapping (often adjacent) ranges deleted at
    -- once, compared with deleting the characters they cover from a string.
    deleteModel rs = case rs of
      (r1 : r2 : more) ->
        let txt = T.intercalate "\n" (take (2 + r1 `mod` 3) ["hello", "", "ab c", "xyz"])
            len = T.length txt
            cuts = nub (sort (map (`mod` (len + 1)) (take (2 + 2 * (r2 `mod` 3)) more)))
            spans = pairs cuts
            sel = sel_ [Range (posOf txt a) (posOf txt e) Nothing | (a, e) <- spans] 0
            (b, _) = applyEdits (const deleteSelection) (buf txt) sel
            covered = [i | (a, e) <- spans, i <- [a .. min (len - 1) e]]
            -- A range from a line start to the end also takes the line
            -- break before it.
            trailing = [a - 1 | (a, e) <- take 1 (reverse spans), e >= len, a > 0, T.index txt (a - 1) == '\n']
            gone = covered <> trailing
            expect = T.pack [c | (i, c) <- zip [0 ..] (T.unpack txt), i `notElem` gone]
         in if null spans || B.toText b == expect then Right () else Left (show (txt, spans, B.toText b, expect))
      _ -> Right ()
    pairs (a : e : more) = (a, e) : pairs more
    pairs _ = []
    -- Sorted, at least 2 apart.
    spread = foldr keep [] . nub . sort
    keep o acc = case acc of
      (x : _) | x - o < 1 -> acc
      _ -> o : acc
    posOf t o = let before = T.take o t in Pos (T.count "\n" before) (T.length (T.takeWhileEnd (/= '\n') before))
    offOf t (Pos l c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" t)] + c
    insertAll t offs ins = foldr (\o acc -> T.take o acc <> ins <> T.drop o acc) t offs
    -- Delete the characters at the given indices; each cursor moves left
    -- by the deletions before it.
    deleteAll t dels offs =
      let t' = T.pack [c | (i, c) <- zip [0 ..] (T.unpack t), i `notElem` dels]
       in (t', [o - length [d | d <- dels, d < o] | o <- offs])

widthTests :: [Test]
widthTests =
  [ test "ascii is narrow" (assertEqual 1 (charWidth 'a'))
  , test "CJK is wide" (assertEqual 2 (charWidth '漢'))
  , test "emoji is wide" (assertEqual 2 (charWidth '😀'))
  , test "control chars show as ^X" (assertEqual ("^A", 2) (glyphs '\SOH' 2, charWidth '\SOH'))
  , test "tab expands to the next stop" (assertEqual 4 (displayCol 4 "\tx" 1))
  , test "tab after text" (assertEqual 4 (displayCol 4 "ab\tx" 3))
  , test "tab width is a parameter" (assertEqual (8, 2) (displayCol 8 "\tx" 1, displayCol 2 "a\tx" 2))
  , test "wide chars shift columns" (assertEqual 4 (displayCol 4 "漢字x" 2))
  , test "column inside a wide char maps to it" (assertEqual 1 (charIndexAtCol 4 "漢字x" 3))
  , test "column past the end" (assertEqual 3 (charIndexAtCol 4 "abc" 10))
  , test "j keeps the visual column across tabs" $
      let b = buf "\tabc\n    xyz"
          r = lineDown b (point (Pos 0 2))
       in assertEqual (Pos 1 5) (rangeHead r)
  ]

viewTests :: [Test]
viewTests =
  [ test "scrolls down with scrolloff" (assertEqual (View 3 0) (scrollToCursor (10, 80) 3 (9, 0) (View 0 0)))
  , test "scrolls up with scrolloff" (assertEqual (View 2 0) (scrollToCursor (10, 80) 3 (5, 0) (View 10 0)))
  , test "no scroll when visible" (assertEqual (View 0 0) (scrollToCursor (10, 80) 3 (4, 0) (View 0 0)))
  , test "scrolls right" (assertEqual (View 0 21) (scrollToCursor (10, 80) 3 (0, 100) (View 0 0)))
  ]

regexTests :: [Test]
regexTests =
  [ test "matches from the query files" $
      assertEqual
        (replicate 12 True <> replicate 6 False)
        ( map (uncurry m)
            [ ("^[A-Z][A-Z\\d_]*$", "MAX_SIZE2")
            , ("^_", "_unused")
            , ("^(self|super)$", "super")
            , ("^[A-Z]", "Maybe")
            , ("^(true|false)$", "true")
            , ("\\.(js|ts)$", "a.ts")
            , ("a.*?b", "xaxxbyb")
            , ("^a{2,3}$", "aaa")
            , ("\\bend\\b", "the end")
            , ("^[^0-9]+$", "abc")
            , ("x(?:ab)+y", "xababy")
            , ("^$", "")
            , ("^[A-Z][A-Z\\d_]*$", "Max")
            , ("^(self|super)$", "superb")
            , ("^a{2,3}$", "aaaa")
            , ("\\bend\\b", "endless")
            , ("^[^0-9]+$", "ab1")
            , ("x(?:ab)+y", "xy")
            ]
        )
  , test "the first match and its extent" $
      assertEqual [Just (1, 4), Just (2, 3), Nothing] [findRegex' "b+c" "abbcd", findRegex' "a*?b" "xxb", findRegex' "z" "abc"]
  , test "Lua patterns" $
      assertEqual [True, True, False, True] [lua "^%u%l+$" "Hello", lua "^%d+%.%d+$" "1.25", lua "^%a+$" "ab1", lua "^a.-b$" "axxb"]
  , test "bad patterns are errors" (assertEqual [True, True] (map (either (const True) (const False) . compileRegex) ["(ab", "[ab"]))
  ]
  where
    m pat str = either (const False) (`matchesRegex` str) (compileRegex pat)
    findRegex' pat str = either (const Nothing) (`findRegex` str) (compileRegex pat)
    lua pat str = either (const False) (`matchesRegex` str) (compileLua pat)
