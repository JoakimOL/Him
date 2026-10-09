-- | Rendering and the frame diff (against a terminal model).
module Test.Render
  ( renderTests
  , diffTests
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Him.Buffer qualified as B
import Data.Map.Strict qualified as Map
import Him.Document
import Him.Editor
import Data.Text.Encoding qualified as TE
import Him.Options
import Him.Picker
import Him.Actions.Picker (pickerHousekeeping)
import Him.Actions.Picker qualified as Picker
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Directory (listingDocument)
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (blankFrame, putCells, putText)
import Him.Selection
import Him.Terminal.Ansi (Color (..), Style (..), Underline (..), defaultStyle, packStyle, patchStyle, sgr, unpackStyle)
import Him.TextWidth (isWide)
import Him.Render (render)
import Him.Render.Frame (Cell (..), Frame (..), ScrollInfo (..), continuation)
import Him.Render.Theme (Theme (..), defaultTheme, scopeStyle)
import Data.IntMap.Strict qualified as IntMap
import Him.Syntax (SyntaxInfo (..), noSyntax)
import Him.Syntax.Span (LineSpan (..))
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Harness
import Test.Util

renderTests :: [Test]
renderTests =
  [ test "the picker previews an open buffer at the item's line" $
      let ed0 = newEditor (24, 100) (newDocument (Just "a.txt") (buf (T.intercalate "\n" [T.pack ("line " <> show n) | n <- [1 .. 50 :: Int]])))
          ed = ed0 {edPicker = Just (newPicker "t" [pickerItem "a.txt:30" (PickPosition "a.txt" 29 0 Nothing) ""]), edMode = Picking}
          f = render defaultTheme Nothing ed
       in assertEqual (Just ("a.txt", True), True)
            ( fmap (\(t, c) -> (t, either (const False) (\(_, l, _) -> l == 29) c)) (previewFor ed (PickPosition "a.txt" 29 0 Nothing))
            , any (T.isInfixOf "30 line 30") [rowText f r | r <- [0 .. 23]]
            )
  , test "the preview draws the document's syntax spans" $
      let doc = (newDocument (Just "a.txt") (buf "let x")) {docSyntax = noSyntax {siSpans = IntMap.singleton 0 [LineSpan 0 3 "keyword"]}}
          ed0 = newEditor (24, 100) doc
          ed = ed0 {edPicker = Just (newPicker "t" [pickerItem "a.txt" (PickFile "a.txt") ""]), edMode = Picking}
          f = render defaultTheme Nothing ed
          -- The preview's row (the text area's gutter also reads "1 let x").
          rows = [r | r <- [0 .. 23], "│" `T.isInfixOf` fst (T.breakOn "1 let x" (rowText f r)), "1 let x" `T.isInfixOf` rowText f r]
          cells = concat [maybe [] toList (Seq.lookup r (frameCells f)) | r <- take 1 rows]
          at c = [st | Cell ch st <- cells, ch == c]
          selected = themePopupSelected defaultTheme
          keyword = maybe selected (patchStyle selected) (scopeStyle defaultTheme "keyword")
       in assertEqual (True, True) (packStyle keyword `elem` at 'l', packStyle selected `elem` at 'x')
  , test "a preview's highlighting arrives after its text" $
      let ed0 = newEditor (24, 100) (newDocument Nothing (buf ""))
          spans = IntMap.singleton 0 [LineSpan 0 2 "keyword"]
          loaded = runNoIO (Picker.applyJobResult (PreviewLoaded "b.txt" (Right (buf "hi")))) ed0 {edPicker = Just (newPicker "t" [pickerItem "b.txt" (PickFile "b.txt") ""]), edMode = Picking}
          lit = runNoIO (Picker.applyJobResult (PreviewHighlighted "b.txt" spans)) loaded
       in assertEqual (Just (PreviewText (buf "hi") spans)) (Map.lookup "b.txt" (edPreviews lit))
  , test "a marked item has a dot, and the count says how many" $
      let ed0 = newEditor (24, 80) (newDocument Nothing (buf ""))
          ed = ed0 {edPicker = Just (toggleMark (newPicker "t" [pickerItem "x" (PickValue "") "", pickerItem "y" (PickValue "") ""])), edMode = Picking}
          rows = [rowText (render defaultTheme Nothing ed) r | r <- [0 .. 23]]
       in assertEqual (True, True) (any (T.isInfixOf "│●x") rows, any (T.isInfixOf "2/2 · 1 marked") rows)
  , test "a file that is not open is read for the preview" $
      let ed0 = newEditor (24, 100) (newDocument Nothing (buf ""))
          ed = ed0 {edPicker = Just (newPicker "t" [pickerItem "b.txt" (PickFile "b.txt") ""]), edMode = Picking}
       in assertEqual
            (Just ("b.txt", Left "loading…"), [StartJob (LoadPreview (optPreviewMaxSize defaultOptions) "b.txt")], Just (PreviewText (buf "hi") mempty))
            ( previewFor ed (PickFile "b.txt")
            , edEffects (runNoIO pickerHousekeeping ed)
            , Map.lookup "b.txt" (edPreviews (runNoIO (Picker.applyJobResult (PreviewLoaded "b.txt" (Right (buf "hi")))) ed))
            )
  , test "a listing colours its header and directories" $
      let doc = listingDocument "/x" 0 [DirEntry "f" False, DirEntry "d" True]
          f = render defaultTheme Nothing (newEditor (8, 30) doc)
          -- Column 6: past the gutter (sign lane, number, padding) and the cursor cell.
          styleOn row = fmap cellStyle (Seq.lookup 6 =<< Seq.lookup row (frameCells f))
       in assertEqual
            [Just (packStyle (themeDirectoryHeader defaultTheme)), Just (packStyle (themeDirectory defaultTheme)), Just (packStyle (themeText defaultTheme))]
            [styleOn 0, styleOn 2, styleOn 3]
  , test "a closed info box is redrawn, not copied from the row cache" $
      let ed = newEditor (12, 40) (newDocument Nothing (buf (T.intercalate "\n" (replicate 20 "some text here"))))
          withBox = ed {edInfo = Just (InfoBox "goto" [("g", "Go to the first line")] BottomRight Nothing)}
          f1 = render defaultTheme Nothing withBox
       in assertEqual (frameCells (render defaultTheme Nothing ed)) (frameCells (render defaultTheme (Just f1) ed))
  , test "gutter shows a sign lane and line numbers" (assertEqual "   1 hello" (T.take 10 (rowText (frameOf "hello") 0)))
  , test "wide chars use a continuation cell" $
      assertEqual [Just '漢', Just continuation, Just 'x'] (map (cellAt (frameOf "漢x") 0) [5, 6, 7])
  , test "control chars are drawn as ^X" (assertEqual "^[x" (T.take 3 (T.drop 5 (rowText (frameOf "\ESCx") 0))))
  , test "rendering with the previous frame gives the same frame" $
      let base = start "hello\nworld\nthird line"
          eds = [base, base {edMode = Insert}, base {edDoc = (edDoc base) {docSelection = single (Range (Pos 0 1) (Pos 1 2) Nothing)}}, base]
          withPrev = go Nothing eds
          go _ [] = []
          go p (e : es) = let f = render defaultTheme p e in f : go (Just f) es
       in assertEqual (map (frameCells . render defaultTheme Nothing) eds) (map frameCells withPrev)
  , test "long file names are shortened" $
      let ed = (start "") {edSize = (5, 30), edDoc = (newDocument (Just (replicate 60 'p' <> "/name.txt")) B.empty) {docDirty = True}}
          status = rowText (render defaultTheme Nothing ed) 3
       in assertEqual (True, True) ("[+]" `T.isInfixOf` status, "name.txt" `T.isInfixOf` status)
  ]
  where
    start t = newEditor (5, 40) (newDocument Nothing (buf t))
    frameOf t = render defaultTheme Nothing (start t)
    cellAt f r c = case drop c (maybe [] toList (lookupRow f r)) of
      (Cell ch _ : _) -> Just ch
      [] -> Nothing

diffTests :: [Test]
diffTests =
  [ test "styles survive packing" $
      let samples = [defaultStyle, defaultStyle {styleFg = Rgb 1 2 3, styleBg = Indexed 240, styleBold = True, styleReverse = True}, defaultStyle {styleFg = Ansi 9, styleItalic = True, styleUnderline = UnderlineLine}, defaultStyle {styleUnderline = UnderlineCurl, styleUnderlineColor = Rgb 255 0 7, styleDim = True, styleStrike = True}]
       in assertEqual samples (map (unpackStyle . packStyle) samples)
  , test "300 random frame sequences replay exactly in a terminal model" (randomDiffs 300)
  , test "identical frames redraw no rows" (assertEqual False ("top" `isInfix` emit (Just f1) f1))
  , test "only the changed row is drawn" $
      let out = emit (Just f1) f2
       in assertEqual (True, False) ("\ESC[2;1H" `isInfix` out, "\ESC[1;1H" `isInfix` out)
  , test "no previous frame clears the screen" (assertEqual True ("\ESC[2J" `isInfix` emit Nothing f1))
  ]
  where
    f1 = putText 0 0 defaultStyle "top" (blankFrame 3 10)
    f2 = putText 1 0 defaultStyle "changed" f1
    emit p f = toLazyByteString (diffFrames p f)
    isInfix needle hay = BL.toStrict needle `BS.isInfixOf` BL.toStrict hay

-- | Replays diff output on a minimal terminal model and checks that the
-- screen equals the new frame (characters and styles, cell by cell).
randomDiffs :: Int -> Either String ()
randomDiffs n = go n (randoms 5) Nothing emptyScreen
  where
    rows = 5
    cols = 12
    styles = [defaultStyle, defaultStyle {styleReverse = True}, defaultStyle {styleFg = Indexed 240}]
    chars = "ab  漢x" :: String
    emptyScreen = replicate rows (replicate cols (' ', sgrText defaultStyle))
    go 0 _ _ _ = Right ()
    go k rs prev screen =
      let (frame, rs') = randomFrame rs
          out = TE.decodeUtf8 (BL.toStrict (toLazyByteString (diffFrames prev frame)))
          screen' = replay (T.unpack out) screen
          expected = [[(c, sgrText (unpackStyle st)) | Cell c st <- toList row] | row <- toList (frameCells frame)]
       in if screen' == expected
            then go (k - 1) rs' (Just frame) screen'
            else Left ("mismatch at step " <> show (n - k) <> ": " <> show out)
    -- Random text, and a random view position for rows 0-3 (so the diff
    -- sometimes scrolls the terminal).
    randomFrame rs0 =
      let (f, rs1) = foldl addText (blankFrame rows cols, rs0) [0 .. 5 :: Int]
          (r, rs2) = case rs1 of
            (x : xs) -> (x, xs)
            [] -> (0, [])
       in ((sanitize f) {frameScroll = Just (ScrollInfo 0 4 (r `mod` 6))}, rs2)
      where
        addText (f, r1 : r2 : r3 : r4 : rest) _ =
          let t = T.pack [chars !! (x `mod` length chars) | x <- take (r3 `mod` 6) rest]
              st = packStyle (styles !! (r4 `mod` length styles))
              cells = concat [if isWide c then [Cell c st, Cell continuation st] else [Cell c st] | c <- T.unpack t]
           in (putCells (r1 `mod` rows) (r2 `mod` cols) cells f, drop 6 rest)
        addText acc _ = acc
    -- Frames from the renderer never contain half a wide character; make
    -- the random ones valid the same way.
    sanitize f = f {frameCells = fmap (Seq.fromList . fixRow . toList) (frameCells f)}
    fixRow (Cell a sa : Cell b sb : rest)
      | isWide a && b == continuation = Cell a sa : Cell b sb : fixRow rest
      | isWide a = Cell ' ' sa : fixRow (Cell b sb : rest)
      | a == continuation = Cell ' ' sa : fixRow (Cell b sb : rest)
    fixRow [Cell a sa] | isWide a || a == continuation = [Cell ' ' sa]
    fixRow (c : rest) = c : fixRow rest
    fixRow [] = []
    sgrText st = TE.decodeUtf8 (BL.toStrict (toLazyByteString (sgr st)))
    -- The terminal model: cursor, current SGR, scroll region, and a grid of
    -- (char, SGR).
    replay str scr = run str (0 :: Int, 0 :: Int) (sgrText defaultStyle) (0, rows - 1) scr
    run [] _ _ _ scr = scr
    run ('\ESC' : '[' : rest) cur cs region scr =
      let (params, rest1) = span (\c -> c >= '0' && c <= '?') rest
          (inter, rest2) = span (\c -> c >= ' ' && c <= '/') rest1
          blankRow = [(' ', cs) | _ <- [1 .. cols]]
          (rt, rb) = region
       in case rest2 of
            (final : rest3) -> case final of
              'H' -> let (r, c) = break (== ';') params in run rest3 (read r - 1, read (drop 1 c) - 1) cs region scr
              'm' -> run rest3 cur cs' region scr where cs' = T.pack ("\ESC[" <> params <> inter <> "m")
              'K' -> let (r, c) = cur in run rest3 cur cs region (setRow r [(c', (' ', cs)) | c' <- [c .. cols - 1]] scr)
              'J' -> run rest3 cur cs region [blankRow | _ <- [1 .. rows]]
              'r' -> case break (== ';') params of
                ("", _) -> run rest3 (0, 0) cs (0, rows - 1) scr
                (a, b) -> run rest3 (0, 0) cs (read a - 1, read (drop 1 b) - 1) scr
              'S' -> run rest3 cur cs region (scrollRows (read params) rt rb blankRow scr)
              'T' -> run rest3 cur cs region (scrollRows (negate (read params)) rt rb blankRow scr)
              _ -> run rest3 cur cs region scr
            [] -> scr
    run (ch : rest) (r, c) cs region scr
      | isWide ch = run rest (r, c + 2) cs region (setRow r [(c, (ch, cs)), (c + 1, (continuation, cs))] scr)
      | otherwise = run rest (r, c + 1) cs region (setRow r [(c, (ch, cs))] scr)
    -- Positive: contents move up.
    scrollRows d rt rb blankRow scr =
      [ if i < rt || i > rb
          then row
          else case i + d of
            j | j >= rt && j <= rb -> scr !! j
            _ -> blankRow
      | (i, row) <- zip [0 ..] scr
      ]
    setRow r updates scr =
      [ if i == r then [maybe old id (lookup j updates) | (j, old) <- zip [0 ..] row] else row
      | (i, row) <- zip [0 ..] scr
      ]
