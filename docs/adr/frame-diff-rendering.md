# Render to a pure `Frame`, then diff

Components (`TextArea`, `Gutter`, `StatusLine`, `CommandLine`) each draw into a `Rect`.
The new frame is compared row by row with the previous one, and only changed rows are
written, in one `Builder` per frame. This avoids flicker and keeps redraws cheap.
