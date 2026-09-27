-- | Putting the terminal into raw mode, and always getting it back out.
module Him.Terminal.Raw
  ( withRawTerminal
  ) where

import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BC
import System.IO (BufferMode (..), hFlush, hSetBinaryMode, hSetBuffering, stdout)
import System.Posix.IO (stdInput)
import System.Posix.Terminal

-- | Run an action with the terminal in raw mode on the alternate screen.
-- The original terminal state is restored afterwards, also when the action
-- throws.
--
-- Never touch the buffering/echo of the @stdin@ 'System.IO.Handle' while in
-- raw mode: GHC then saves the termios state itself and restores that
-- (raw!) state when the program exits. Input is read from the file
-- descriptor directly instead (see "Him.Terminal.Input").
withRawTerminal :: IO a -> IO a
withRawTerminal action = bracket enter leave (const action)
  where
    enter = do
      original <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (makeRaw original) WhenFlushed
      hSetBinaryMode stdout True
      hSetBuffering stdout (BlockBuffering Nothing)
      emit enterSeq
      pure original
    leave original = do
      emit leaveSeq
      setTerminalAttributes stdInput original WhenFlushed
    emit bytes = BC.hPut stdout bytes >> hFlush stdout
    -- Alternate screen on, clear it, cursor home.
    enterSeq = "\ESC[?1049h\ESC[2J\ESC[H"
    -- Reset style and cursor shape, show cursor, alternate screen off.
    leaveSeq = "\ESC[0m\ESC[0 q\ESC[?25h\ESC[?1049l"

-- | The equivalent of @cfmakeraw@: no echo, no line editing, no signals from
-- Ctrl-C/Ctrl-Z, no flow control, no CR/LF translation. Reads block until at
-- least one byte is available.
makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  foldl
    withoutMode
    attrs
    [ EnableEcho
    , EchoLF
    , ProcessInput
    , KeyboardInterrupts
    , ExtendedFunctions
    , StartStopOutput
    , MapCRtoLF
    , IgnoreCR
    , MapLFtoCR
    , InterruptOnBreak
    , CheckParity
    , StripHighBit
    , ProcessOutput
    ]
    `withBits` 8
    `withMinInput` 1
    `withTime` 0
