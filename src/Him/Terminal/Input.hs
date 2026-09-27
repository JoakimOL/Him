-- | Turning terminal input bytes into 'Key's.
--
-- The decoder is pure ('decodeKeys'); 'startInputReader' runs it on a
-- background thread and pushes events into a channel.
module Him.Terminal.Input
  ( decodeKeys
  , startInputReader
  , escapeTimeoutMicros
  ) where

import Control.Concurrent (Chan, forkIO, threadWaitRead, writeChan)
import Control.Monad (unless, void)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import Him.Event (Event (..))
import Him.Key
import System.Posix.IO (stdInput)
import System.Posix.IO.ByteString (fdRead)
import System.Timeout (timeout)

-- | How long to wait after a lone ESC byte before deciding it is the Esc key
-- and not the start of an escape sequence.
escapeTimeoutMicros :: Int
escapeTimeoutMicros = 30000

-- | Read stdin forever on a background thread, sending 'EvKey' events.
startInputReader :: Chan Event -> IO ()
startInputReader chan = void (forkIO (loop B.empty))
  where
    loop pending = do
      -- With an incomplete sequence pending, only wait a short while.
      ready <-
        if B.null pending
          then threadWaitRead stdInput >> pure True
          else isJust <$> timeout escapeTimeoutMicros (threadWaitRead stdInput)
      if ready
        then do
          bytes <- fdRead stdInput 4096
          unless (B.null bytes) $ do
            let (keys, rest) = decodeKeys False (pending <> bytes)
            mapM_ (writeChan chan . EvKey) keys
            loop rest
        else do
          let (keys, _) = decodeKeys True pending
          mapM_ (writeChan chan . EvKey) keys
          loop B.empty

data Step
  = -- | A key, and the remaining input.
    Decoded Key ByteString
  | -- | An unrecognised sequence was dropped.
    Skipped ByteString
  | -- | The input ends in the middle of a sequence.
    Incomplete
  | End

-- | Decode as many keys as possible. Returns the keys and any trailing bytes
-- that form an incomplete sequence (to be retried when more input arrives).
-- With @final = True@ no more input is coming, so an incomplete sequence is
-- interpreted literally (a lone ESC becomes the Esc key).
decodeKeys :: Bool -> ByteString -> ([Key], ByteString)
decodeKeys final = go []
  where
    go acc bs = case decodeOne final bs of
      Decoded k rest -> go (k : acc) rest
      Skipped rest -> go acc rest
      Incomplete -> (reverse acc, bs)
      End -> (reverse acc, B.empty)

decodeOne :: Bool -> ByteString -> Step
decodeOne final bs = case B.uncons bs of
  Nothing -> End
  Just (0x1b, rest) -> decodeEscape final rest
  Just (b, rest)
    | b < 0x80 -> Decoded (controlOrAscii b) rest
    | otherwise -> decodeUtf8Char final bs

controlOrAscii :: Word8 -> Key
controlOrAscii = \case
  0x0d -> plain KEnter
  0x0a -> plain KEnter
  0x09 -> plain KTab
  0x7f -> plain KBackspace
  0x08 -> plain KBackspace
  0x00 -> ctrl ' '
  b
    | b <= 26 -> ctrl (chr (fromIntegral b + 0x60))
    | b < 0x20 -> ctrl (chr (fromIntegral b + 0x40)) -- C-\ C-] C-^ C-_
    | otherwise -> plain (KChar (chr (fromIntegral b)))

-- | Bytes after an ESC.
decodeEscape :: Bool -> ByteString -> Step
decodeEscape final rest = case B.uncons rest of
  Nothing
    | final -> Decoded (plain KEsc) B.empty
    | otherwise -> Incomplete
  Just (0x5b, r) -> decodeCsi final r -- ESC [
  Just (0x4f, r) -> decodeSs3 final r -- ESC O
  Just (0x1b, _) -> Decoded (plain KEsc) rest
  Just _ -> case decodeOne final rest of
    Decoded k r -> Decoded (withMod Alt k) r
    Skipped r -> Skipped r
    Incomplete -> Incomplete
    End -> Decoded (plain KEsc) B.empty

-- | @ESC [ params final@. Parameter bytes are 0x30-0x3f, the final byte is
-- 0x40-0x7e.
decodeCsi :: Bool -> ByteString -> Step
decodeCsi final r =
  let (params, after) = B.span (\b -> b >= 0x30 && b <= 0x3f) r
   in case B.uncons after of
        Nothing
          | final -> Decoded (alt '[') r
          | otherwise -> Incomplete
        Just (fin, rest)
          | fin >= 0x40 && fin <= 0x7e -> maybe (Skipped rest) (`Decoded` rest) (csiKey params fin)
          | otherwise -> Decoded (alt '[') r

csiKey :: ByteString -> Word8 -> Maybe Key
csiKey params fin = applyMods <$> code
  where
    nums = map (fmap fst . BC.readInt) (BC.split ';' params)
    param i = case drop i nums of
      (Just n : _) -> n
      _ -> 1
    applyMods c = foldr withMod (plain c) (extraMods <> modsFromParam (param 1))
    extraMods = [Shift | fin == 0x5a]
    code = case chr (fromIntegral fin) of
      'A' -> Just KUp
      'B' -> Just KDown
      'C' -> Just KRight
      'D' -> Just KLeft
      'H' -> Just KHome
      'F' -> Just KEnd
      'P' -> Just (KF 1)
      'Q' -> Just (KF 2)
      'R' -> Just (KF 3)
      'S' -> Just (KF 4)
      'Z' -> Just KTab -- Shift-Tab; Shift added below
      '~' -> tildeKey (param 0)
      _ -> Nothing

-- | @ESC [ n ~@ sequences.
tildeKey :: Int -> Maybe KeyCode
tildeKey = \case
  1 -> Just KHome
  7 -> Just KHome
  4 -> Just KEnd
  8 -> Just KEnd
  2 -> Just KInsert
  3 -> Just KDelete
  5 -> Just KPageUp
  6 -> Just KPageDown
  n
    | n >= 11 && n <= 15 -> Just (KF (n - 10))
    | n >= 17 && n <= 21 -> Just (KF (n - 11))
    | n == 23 -> Just (KF 11)
    | n == 24 -> Just (KF 12)
    | otherwise -> Nothing

-- | xterm modifier parameter: 1 + (shift=1, alt=2, ctrl=4).
modsFromParam :: Int -> [Modifier]
modsFromParam p =
  [m | (bit, m) <- [(1, Shift), (2, Alt), (4, Ctrl)], (max 0 (p - 1) .&. bit) /= 0]

-- | @ESC O x@: arrows/home/end in "application mode", and F1-F4.
decodeSs3 :: Bool -> ByteString -> Step
decodeSs3 final r = case B.uncons r of
  Nothing
    | final -> Decoded (alt 'O') B.empty
    | otherwise -> Incomplete
  Just (fin, rest) -> maybe (Skipped rest) (`Decoded` rest) (csiKey B.empty fin)

-- | A multi-byte UTF-8 character.
decodeUtf8Char :: Bool -> ByteString -> Step
decodeUtf8Char final bs
  | len == 0 = Decoded replacement (B.drop 1 bs)
  | B.length bs < len = if final then Decoded replacement B.empty else Incomplete
  | otherwise =
      let (char, rest) = B.splitAt len bs
       in case T.unpack <$> decodeUtf8' char of
            Right [c] -> Decoded (plain (KChar c)) rest
            _ -> Decoded replacement rest
  where
    lead = B.head bs
    len
      | lead .&. 0xe0 == 0xc0 = 2
      | lead .&. 0xf0 == 0xe0 = 3
      | lead .&. 0xf8 == 0xf0 = 4
      | otherwise = 0 :: Int
    replacement = plain (KChar '\xfffd')
