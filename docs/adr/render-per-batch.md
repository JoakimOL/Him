# Render once per batch of input

The input thread queues all keys decoded from one read on a `TChan` (from `stm`). The
main loop handles every queued event (up to 512) before rendering once. That's what
makes typeahead (pastes, key repeat) cheap. Components write whole rows
(`putCells`) instead of single cells.
