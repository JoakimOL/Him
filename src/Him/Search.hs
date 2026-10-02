-- | Text search over a buffer.
--
-- Patterns are literal text. Like Helix's default "smart case", a pattern
-- without upper-case letters matches case-insensitively (ASCII letters).
-- The scan itself runs in C over whole blocks of lines at a time
-- ("Him.Buffer.findForwardFrom", "Him.Native").
module Him.Search
  ( Direction (..)
  , Needle
  , needleText
  , compileNeedle
  , Match (..)
  , findMatch
  , selectMatches
  ) where

import Data.Char (isAlpha, isAscii, isUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (lengthWord8)
import Him.Buffer
import Him.Native qualified as Native
import Him.Position (Pos (..))
import Him.Selection

data Direction = Forward | Backward
  deriving stock (Eq, Show)

data Needle = Needle
  { needleText :: !Text
  , needleFold :: !Bool
  , needleChars :: !Int
  }
  deriving stock (Eq, Show)

-- | 'Nothing' for an empty pattern or one spanning lines (matches never
-- cross lines). Case folding is only switched on when it can matter (the
-- pattern has ASCII letters, none upper-case); otherwise the exact search,
-- glibc's memmem, is used.
compileNeedle :: Text -> Maybe Needle
compileNeedle t
  | T.null t || T.any (\c -> c == '\n' || c == '\r') t = Nothing
  | otherwise = Just (Needle t fold (T.length t))
  where
    fold = not (T.any isUpper t) && T.any (\c -> isAscii c && isAlpha c) t

data Match = Match
  { matchStart :: !Pos
  , matchEnd :: !Pos
  -- ^ Inclusive, like a selection.
  , matchWrapped :: !Bool
  -- ^ The search passed the end (or start) of the buffer.
  }
  deriving stock (Eq, Show)

-- | The next match after a position ('Forward'), or the previous one before
-- it ('Backward'), wrapping around the buffer.
findMatch :: Direction -> Needle -> Buffer -> Pos -> Maybe Match
findMatch dir n buf (Pos l c) = case dir of
  Forward -> case findForwardFrom fwd (Pos l (c + 1)) buf of
    Just p -> Just (match False p)
    Nothing -> match True <$> findForwardFrom fwd (Pos 0 0) buf
  Backward -> case findBackwardBefore bwd bytes (Pos l c) buf of
    Just p -> Just (match False p)
    Nothing -> match True <$> findBackwardBefore bwd bytes (endPos buf) buf
  where
    fwd = Native.findForward (needleFold n) (needleText n)
    bwd = Native.findBackward (needleFold n) (needleText n)
    bytes = lengthWord8 (needleText n)
    match wrapped p@(Pos ml mc) = Match p (Pos ml (mc + needleChars n - 1)) wrapped

-- | Helix @s@: replace every range by the matches inside it (each match
-- wholly inside). 'Nothing' when no range contains a match. The first
-- match in the old primary range becomes primary.
selectMatches :: Needle -> Buffer -> Selection -> Maybe Selection
selectMatches n buf sel = fromRanges (concat found) prim
  where
    found = map (matchesIn n buf) (ranges sel)
    prim = sum (map length (take (primaryIndex sel) found))

matchesIn :: Needle -> Buffer -> Range -> [Range]
matchesIn n buf r = go (before (rangeStart r))
  where
    end = rangeEnd r
    -- findMatch searches after a position, so start one column earlier.
    before (Pos l c) = Pos l (c - 1)
    go p = case findMatch Forward n buf p of
      Just m
        | not (matchWrapped m)
        , matchEnd m <= end ->
            Range (matchStart m) (matchEnd m) Nothing : go (matchEnd m)
      _ -> []
