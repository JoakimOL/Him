# Registers as in Vim and Helix, and the clipboard behind one provider API

The user asked for yanking to and pasting from the system clipboard as `+`, and for
named registers that don't overwrite each other, `:registers` and a way to clear them.
- **Picking a register.** `"` (`select_register`) waits for a key (`AwaitRegister`) and
  stores it in `edSelectedRegister`. The next command uses it, and `Him.Session` clears
  it after any bound command (or a key that matches nothing). `y`, `d`, `c`, `p`, `P`
  use it (`selectedRegister`), else `"`. As in Helix (not Vim), a named yank writes only
  that register. Any character names a register; it is created when yanked to.
- **Special registers.** `_` discards (`" _ d` deletes without touching `"`). `/` is
  the last search. `+` is the clipboard, `*` the primary selection. Insert mode's
  `C-r` + a register inserts it. `space y` / `space p` / `space P` are `" + y/p/P`.
  `R` replaces each selection with the register (Helix's `replace_with_yanked`; it
  leaves select mode), and `space R` replaces it with the clipboard.
  The status line shows a picked register, and the info box lists the registers while
  `"` or `C-r` waits.
- **The clipboard API** (`Him.Clipboard`): a `ClipboardProvider` record (`cbName`,
  `cbAvailable`, `cbGet`, `cbSet`) for the clipboard or the primary selection.
  `cfgClipboardProviders` lists them (tests use an in-memory one), and
  `[editor] clipboard-provider` names one, or `auto` takes the first available in
  Helix's order: `pasteboard` (macOS), `wayland` (`wl-copy` / `wl-paste`), `x-clip`,
  `x-sel`, `tmux`, `termcode` (OSC 52: it copies, also over ssh, but can't be read).
  `none` keeps `+` inside him. The copy programs' output goes to `/dev/null`, because
  `wl-copy` and `xclip` stay in the background and would hold a captured pipe open.
  Both directions time out after 2 s.
- **Through effects.** Actions can't see the `Config`, so writing `+` / `*` caches the
  values in `edRegisters` and queues `ClipboardSet`. Reading queues `ClipboardGet reg
  use` (`RegisterUse`: paste after/before, insert, refresh, show). Both are immediate
  effects that `Him.Session` performs with the config's providers
  (`Register.clipboardSet` / `clipboardGet`). When the clipboard holds what him copied
  (the values joined, one per line), the register keeps one value per range, so a
  multi-cursor yank pastes back range by range. When it can't be read, the cached
  value is used.
- **`:registers`** (`:reg`, also the `show_registers` action) reads `+` / `*` again if
  they exist, then shows a popup of every register with the start of its text (`⏎` for
  line breaks, `[n]` for several values). **`:clear-register [names]`** forgets the
  named ones, or all of them. Clearing `+` forgets him's copy only; the system
  clipboard stays as it is.

*Alternatives:* Vim's rule that every yank also fills `"` (the user wanted named
registers that leave the others alone); keeping the provider in `Editor` (it holds
functions, and `Editor` derives `Eq`/`Show`); reading the clipboard on every
`getRegister` (blocking IO in pure-looking code, and tests would touch the real
clipboard).
