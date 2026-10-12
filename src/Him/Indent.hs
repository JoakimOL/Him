-- | Indentation: what one level is in a document (tabs, or so many
-- spaces), how to guess it from the text, and indenting lines by levels.
-- Which one a document uses is decided when it is loaded ("Him.File"):
-- the project's @.editorconfig@, else the text itself, else the language's
-- custom, else the @[editor]@ settings.
module Him.Indent
  ( Indent (..)
  , IndentHint (..)
  , noHint
  , orElse
  , resolveIndent
  , indentUnit
  , detectIndent
  , indentLines
  , unindentLines
  ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Edit (LineEdit (..), editLines, selectedLines)
import Him.Selection (Selection)

-- | One level of indentation.
data Indent = Indent
  { indTabs :: !Bool
  -- ^ A tab character; otherwise spaces.
  , indWidth :: !Int
  -- ^ How many spaces (and how wide a level is with tabs).
  }
  deriving stock (Eq, Show)

-- | What one source says about indentation; either part may be unknown
-- (an @.editorconfig@ may give only a size).
data IndentHint = IndentHint
  { hintTabs :: !(Maybe Bool)
  , hintWidth :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

noHint :: IndentHint
noHint = IndentHint Nothing Nothing

-- | The first hint, with what it leaves out taken from the second.
orElse :: IndentHint -> IndentHint -> IndentHint
orElse (IndentHint t w) (IndentHint t' w') = IndentHint (t <|> t') (w <|> w')

-- | Hints in order of precedence, the last of which says everything (the
-- editor's settings).
resolveIndent :: [IndentHint] -> Indent -> Indent
resolveIndent hints (Indent tabs width) =
  let IndentHint t w = foldr orElse noHint hints
   in Indent (fromMaybe tabs t) (max 1 (fromMaybe width w))

-- | The text of one level.
indentUnit :: Indent -> Text
indentUnit (Indent tabs width)
  | tabs = "\t"
  | otherwise = T.replicate width " "

-- | Guess a text's indentation from its first lines: tabs when more lines
-- start with a tab than with spaces; else the step between consecutive
-- lines' indentation that occurs most (2, 4, 8, …). Nothing for text with
-- too little indentation to tell.
detectIndent :: Buffer -> IndentHint
detectIndent b
  | tabbed + spaced < 2 = noHint
  | tabbed > spaced = IndentHint (Just True) Nothing
  | otherwise = IndentHint (Just False) (fst <$> best)
  where
    sample = filter (not . T.all isSpace) (take 1000 (toLines b))
    leads = map (T.takeWhile (\c -> c == ' ' || c == '\t')) sample
    tabbed = length (filter ("\t" `T.isPrefixOf`) leads)
    spaced = length (filter ("  " `T.isPrefixOf`) leads)
    widths = mapMaybe (\l -> if T.all (== ' ') l then Just (T.length l) else Nothing) leads
    steps = [d | (a, c) <- zip widths (drop 1 widths), let d = c - a, d >= 2, d <= 8]
    counts = Map.fromListWith (+) [(d, 1 :: Int) | d <- steps]
    -- Most common first; a tie goes to the smaller step.
    best = case sortOn (\(d, n) -> (Down n, d)) (Map.toList counts) of
      x : _ -> Just x
      [] -> Nothing

-- | @>@: indent every line the selection touches by n levels (blank lines
-- stay as they are).
indentLines :: Indent -> Int -> Buffer -> Selection -> (Buffer, Selection)
indentLines ind n b sel =
  editLines
    [(l, LineEdit 0 0 (T.replicate (max 1 n) (indentUnit ind))) | l <- selectedLines b sel, not (T.all isSpace (lineAt l b))]
    b
    sel

-- | @<@: take up to n levels of indentation off every line the selection
-- touches. A tab is one level; so are 'indWidth' spaces (or fewer, at the
-- end of the indentation).
unindentLines :: Indent -> Int -> Buffer -> Selection -> (Buffer, Selection)
unindentLines ind n b sel =
  editLines [(l, LineEdit 0 k "") | l <- selectedLines b sel, let k = removable (lineAt l b), k > 0] b sel
  where
    removable line = go (max 1 n) 0 (T.unpack (T.takeWhile (\c -> c == ' ' || c == '\t') line))
    go 0 acc _ = acc
    go _ acc [] = acc
    go m acc ('\t' : rest) = go (m - 1) (acc + 1) rest
    go m acc cs = let k = length (take (indWidth ind) (takeWhile (== ' ') cs)) in go (m - 1) (acc + k) (drop k cs)
