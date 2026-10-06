# Ctrl-Z suspends the editor

Raw mode turns off the terminal's own signal
keys (`ISIG`), so Ctrl-Z arrives as a key. It is bound to `suspend`, which queues a
`Suspend` effect. The main loop then:
1. calls the suspend action that `withRawTerminal` hands it: leave the alternate
   screen, restore the original terminal attributes, and stop the process with
   `SIGTSTP` (the default action stops every thread);
2. when the shell continues the process (`fg`), sets raw mode again and re-enters the
   alternate screen;
3. marks the editor `edRepaint`, so the next frame is drawn without diffing against the
   old one, and reads the window size again in case it changed meanwhile.
