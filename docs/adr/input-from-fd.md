# Read input from the file descriptor, never through the `stdin` Handle

If `hSetBuffering stdin` or `hSetEcho` is called on a tty, GHC saves the termios state
itself and restores *that* state when the program exits. When those calls happen after
we enter raw mode, the saved state is already raw, so it undoes our restore. We found this
while adding raw mode. Input is read with `System.Posix.IO.ByteString.fdRead` on `stdInput`
(after `threadWaitRead`, which keeps it interruptible for timeouts).
