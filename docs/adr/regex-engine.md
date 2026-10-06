# A small regex engine of our own

`Him.Regex` is a backtracking matcher:
literals and escapes, `.`, classes with `\d \w \s`, anchors and `\b`, groups
(including `(?:…)`), alternation, and greedy or lazy `* + ? {m,n}`. Lua patterns are
translated (`compileLua`). There are no backreferences or lookaround. It is used for
query predicates; all 205 `#match?` patterns in Helix's queries compile. It is the
starting point for regex search.
