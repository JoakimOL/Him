# The renderer reuses rows and lets the terminal scroll

`render` takes the previous frame. Text-area rows are keyed (`RowKey`: line, text,
selection spans, cursors, scroll, width) and copied when unchanged, looked up by line so
scrolling keeps them. When the view moved less than a screen, `diffFrames` scrolls the
terminal's region (`DECSTBM` + `SU`/`SD`) and diffs against the shifted old frame. It
writes only changed cell runs and clears trailing blanks with `EL`. Tests replay the
output on a small terminal model with scroll regions and compare cell by cell.
