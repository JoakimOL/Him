# Terminal size through a C shim

The `unix` package does not expose `ioctl(TIOCGWINSZ)`. `cbits/winsize.c` wraps it in a
single function, and `SIGWINCH` triggers a resize event.
*Alternative:* the cursor-position-report trick (`ESC[999C ESC[6n`). It is slower and racy.
