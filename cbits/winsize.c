#include <sys/ioctl.h>
#include <unistd.h>

/* Query the size of the terminal attached to stdout.
 * Returns 0 on success, -1 on failure (e.g. stdout is not a tty). */
int him_get_winsize(int *rows, int *cols)
{
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
        return -1;
    *rows = ws.ws_row;
    *cols = ws.ws_col;
    return 0;
}
