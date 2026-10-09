# Hot byte loops in C, called with `unsafe` FFI on the `Text`'s array

`cbits/text.c` scans newlines (`memchr`) and searches (`memmem`, SSE2 two-byte scan).
`unsafe` calls cannot be interrupted by the GC, so the unpinned arrays can be passed
directly (`UnliftedFFITypes`). This keeps the editor to boot libraries plus two small C
files. (Later, tree-sitter added its vendored runtime and a shim: [ADR tree-sitter](tree-sitter.md).)
