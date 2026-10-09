# Default keys avoid AltGr and dead keys; next / previous live in their menus

On Norwegian and most other European layouts, `[ ] { } $ @` need AltGr, and `^ ` ~`
are dead keys, which only give the character after a second press. Alt with an
AltGr character (`A-{`) cannot be typed at all, because Alt and AltGr collide.
Helix's defaults lean on these keys, so him's defaults moved off them.

- **Next / previous go in their subject's menu**, next to its other keys, as
  `n` / `p`:
  - git changes: `space g n` / `space g p`;
  - proposed chat changes: `space c n` / `space c p` (a new chat moved to `space c N`);
  - diagnostics: a new `space d` menu with `n` / `p`, `f` / `l` (first / last),
    `d` (this file's picker, which was `space x`) and `D` (a new workspace picker
    over every published diagnostic, like Helix's `space D`).

  The `[` / `]` keys and their prefix titles are gone. Users who want them back
  bind them in `[keys.normal]`.
- **Directory views** gave `space d` up: `space -` shows the file's directory (as in
  vim-vinegar) and `space .` shows the working directory (`.`). Both keys are
  unshifted on Norwegian and US layouts.
- **Pairs have letters** in `m i`, `m a`, `m s`, `m r` and `m d`: `b` (), `B` {}, `r` [],
  `c` <> ("crocodile"), `q` backticks. The bracket characters still work.
- **Listings:** `u` goes up. `-`, `^` and `backspace` stay.
- **Shifted keys are fine.** `/ ? : % * " ( )` need Shift on these layouts. That is
  normal for European layouts, so they stay.
- **When adding keys:** avoid AltGr and dead-key characters, and never put Alt on
  one. `< + - , .` and letters are unshifted on Nordic layouts.

*Alternatives:*
- `<` / `>` as previous / next prefixes. `>` is shifted on US layouts, and Helix uses
  both keys for indenting.
- Keeping `[` / `]` beside the menu keys. That would keep two ways to do the same
  thing and would not free the keys for anything else.
