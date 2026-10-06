-- | The chat buffer's layout (ADR chat-panel): a transcript of blocks (your
-- messages, the model's answers, what it did), then the prompt and the
-- message being typed. Output goes at the end of the transcript, above the
-- prompt, so typing is never in the way. The transcript only grows at its
-- last line, so each line keeps its number: what a line is ('ChatMark') is
-- kept by number, for drawing. The model's prose is wrapped to the window
-- as it arrives (code blocks are not). Pure.
module Him.Chat.Transcript
  ( chatPrompt
  , newChatDocument
  , chatState
  , appendModel
  , appendLines
  , appendUser
  , appendGap
  , lastMark
  , openLineEmpty
  , takeMessage
  , setMessage
  , wrapLine
  , Inline (..)
  , inlineSpans
  , codeBlockAt
  ) where

import Data.Char (isDigit)
import Data.IntMap.Strict qualified as IntMap
import Data.List (findIndex, isPrefixOf, tails)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Chat
import Him.Document (DocKind (..), Document (..), newDocument)
import Him.Position (Pos (..))
import Him.Selection (Range (..), mapRanges, point, single)

-- | Before the message being typed.
chatPrompt :: Text
chatPrompt = "› "

-- | An empty chat: an empty transcript line, then the prompt.
newChatDocument :: Document
newChatDocument =
  (newDocument Nothing (Buffer.fromText ("\n" <> chatPrompt)))
    { docKind = ChatDoc newChatState {csInput = input}
    , docSelection = single (point input)
    }
  where
    input = Pos 1 (T.length chatPrompt)

chatState :: Document -> Maybe ChatState
chatState d = case docKind d of
  ChatDoc cs -> Just cs
  _ -> Nothing

-- | The transcript's last line, the one output goes on to.
openLine :: ChatState -> Int
openLine cs = posLine (csInput cs) - 1

openLineEmpty :: Document -> Bool
openLineEmpty d = maybe True (\cs -> T.null (Buffer.lineAt (openLine cs) (docBuffer d))) (chatState d)

-- | The mark of the transcript's last finished line, and whether it is
-- blank.
lastMark :: Document -> Maybe (Maybe ChatMark, Bool)
lastMark d = case chatState d of
  Just cs | l <- openLine cs - 1, l >= 0 -> Just (IntMap.lookup l (csMarks cs), T.null (Buffer.lineAt l (docBuffer d)))
  _ -> Nothing

-- | Replace the transcript's last line with finished lines (with their
-- marks) and a new last line. What is below (the prompt, the input, and
-- cursors there) moves down with it.
replaceOpen :: [(Text, Maybe ChatMark)] -> Text -> Document -> Document
replaceOpen done open d = case docKind d of
  ChatDoc cs ->
    let l = openLine cs
        n = length done
        shift p
          | posLine p < l = p
          | posLine p == l = Pos (l + n) (min (posCol p) (T.length open))
          | otherwise = Pos (posLine p + n) (posCol p)
        marks = IntMap.fromList [(l + i, m) | (i, (_, Just m)) <- zip [0 ..] done]
     in d
          { docBuffer = Buffer.replaceLines l (l + 1) (map fst done <> [open]) (docBuffer d)
          , docSelection = mapRanges (\r -> r {rangeAnchor = shift (rangeAnchor r), rangeHead = shift (rangeHead r)}) (docSelection d)
          , docKind = ChatDoc cs {csInput = shift (csInput cs), csMarks = IntMap.union marks (IntMap.delete l (csMarks cs))}
          , docVersion = docVersion d + 1
          }
  _ -> d

-- | The model's text, as it streams: prose wrapped to the width, code
-- blocks marked.
appendModel :: Int -> Text -> Document -> Document
appendModel width t d = case chatState d of
  Just cs | not (T.null t) ->
    let open = Buffer.lineAt (openLine cs) (docBuffer d)
        (done, open', fence) = layoutModel width (csFence cs) (open <> t)
        d' = replaceOpen done open' d
     in d' {docKind = ChatDoc ((maybe cs id (chatState d')) {csFence = fence})}
  _ -> d

-- | Lines of text: finished ones with their marks, the last (unfinished)
-- one, and whether it is in a code block.
layoutModel :: Int -> Bool -> Text -> ([(Text, Maybe ChatMark)], Text, Bool)
layoutModel width fence0 s = go fence0 (T.splitOn "\n" s)
  where
    go fence = \case
      [] -> ([], "", fence)
      [l]
        | fence || isFence l -> ([], l, fence)
        | otherwise -> case wrapLine width l of
            ws -> ([(w, Nothing) | w <- init ws], last ws, fence)
      l : rest ->
        let (line, fence')
              | isFence l = ([(l, Just MarkFence)], not fence)
              | fence = ([(l, Just MarkCode)], fence)
              | otherwise = ([(w, Nothing) | w <- wrapLine width l], fence)
            (more, open, f) = go fence' rest
         in (line <> more, open, f)
    isFence l = "```" `T.isPrefixOf` T.stripStart l

-- | Whole lines with a mark (wrapped), after the transcript's text; the
-- last line is left empty.
appendLines :: Int -> ChatMark -> [Text] -> Document -> Document
appendLines width mark ls d0 =
  replaceOpen [(w, Just mark) | l <- ls, w <- wrapLine width l] "" (finishOpen width d0)

-- | Your message: its lines, with code blocks as code.
appendUser :: Int -> Text -> Document -> Document
appendUser width t d0 =
  let (done, open, _) = layoutModel width False (t <> "\n")
   in replaceOpen [(l, Just (maybe MarkUserText id m)) | (l, m) <- done] open (finishOpen width d0)

-- | Finish the transcript's last line, if anything is on it.
finishOpen :: Int -> Document -> Document
finishOpen width d = if openLineEmpty d then d else appendModel width "\n" d

-- | One blank line before what comes next (none at the top).
appendGap :: Int -> Document -> Document
appendGap width d0 =
  let d = finishOpen width d0
   in case lastMark d of
        Just (_, False) -> replaceOpen [("", Nothing)] "" d
        _ -> d

-- | Take the typed message: its text, and the document with the input
-- empty again (the cursor there).
takeMessage :: Document -> Maybe (Text, Document)
takeMessage d = case chatState d of
  Nothing -> Nothing
  Just cs ->
    let from = csInput cs
        buf = docBuffer d
        end = Buffer.endPos buf
     in Just
          ( Buffer.textRange from end buf
          , d
              { docBuffer = Buffer.deleteRange from end buf
              , docSelection = single (point from)
              , docKind = ChatDoc cs {csRecall = -1}
              , docVersion = docVersion d + 1
              }
          )

-- | Put a message in the input (an earlier one, recalled).
setMessage :: Text -> Document -> Document
setMessage t d = case takeMessage d of
  Just (_, d') | Just cs <- chatState d' ->
    let (buf, end) = Buffer.insertText (csInput cs) t (docBuffer d')
     in d' {docBuffer = buf, docSelection = single (point end)}
  _ -> d

-- | A line broken at spaces to fit the width; the lines after the first
-- are indented like it (past a list item's bullet). A word longer than the
-- width is cut.
wrapLine :: Int -> Text -> [Text]
wrapLine width t
  | width < 10 || T.length t <= width = [t]
  | otherwise = go t
  where
    lead = T.length (T.takeWhile (== ' ') t)
    rest = T.drop lead t
    bullet
      | any (`T.isPrefixOf` rest) ["- ", "* ", "• "] = 2
      | (ds, r) <- T.span isDigit rest, not (T.null ds), ". " `T.isPrefixOf` r = T.length ds + 2
      | otherwise = 0
    indent = if lead + bullet <= width `div` 2 then lead + bullet else 0
    pad = T.replicate indent " "
    go line
      | T.length line <= width = [line]
      | otherwise =
          let cut = case [i | i <- [width, width - 1 .. indent + 1], T.index line i == ' '] of
                i : _ -> i
                [] -> width
              (a, b) = T.splitAt cut line
           in T.stripEnd a : go (pad <> T.stripStart b)

-- | Markdown in the model's prose that is drawn differently.
data Inline = InlineCode | InlineBold | InlineHeading
  deriving stock (Eq, Show)

-- | Where a prose line has inline code (@`x`@), bold (@**x**@), or is a
-- heading: (start, end, kind), columns.
inlineSpans :: Text -> [(Int, Int, Inline)]
inlineSpans t
  | "#" `T.isPrefixOf` T.stripStart t = [(0, T.length t, InlineHeading)]
  | otherwise = go 0 (T.unpack t)
  where
    go i = \case
      '`' : rest | Just j <- findIndex (== '`') rest -> (i, i + j + 2, InlineCode) : go (i + j + 2) (drop (j + 1) rest)
      '*' : '*' : rest | Just j <- findIndex ("**" `isPrefixOf`) (tails rest), j > 0 -> (i, i + j + 4, InlineBold) : go (i + j + 4) (drop (j + 2) rest)
      _ : rest -> go (i + 1) rest
      [] -> []

-- | The code block a line is in (or, for any other line, the last one
-- above it, else the last one in the transcript): its lines, without the
-- fences.
codeBlockAt :: Int -> Document -> Maybe Text
codeBlockAt line d = do
  cs <- chatState d
  let marks = csMarks cs
      isCode l = IntMap.lookup l marks == Just MarkCode
      blockEnding l = let start = until (\k -> not (isCode (k - 1))) (subtract 1) l in [start .. l]
      ends = [l | (l, MarkCode) <- IntMap.toList marks, not (isCode (l + 1))]
      lines' = case () of
        _
          | isCode line -> [l | l <- blockEnding line ++ takeWhile isCode [line + 1 ..], l >= 0]
          | (e : _) <- reverse (filter (< line) ends) -> blockEnding e
          | (e : _) <- reverse ends -> blockEnding e
          | otherwise -> []
  case lines' of
    [] -> Nothing
    ls -> Just (T.intercalate "\n" [Buffer.lineAt l (docBuffer d) | l <- ls])
