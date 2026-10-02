/* Small helpers around the tree-sitter API for Him.Syntax.TreeSitter:
 * functions that take or return TSNode by value cannot be called through
 * Haskell's FFI, and one call per highlight request is cheaper than one per
 * capture. */
#include <stdint.h>
#include <tree_sitter/api.h>

/* Parse a whole text (no previous tree yet: full parse). */
TSTree *him_ts_parse(TSParser *parser, const char *text, uint32_t length)
{
    return ts_parser_parse_string(parser, NULL, text, length);
}

/* Run a query over the bytes [start, end) of a tree. For each capture of
 * each match, nine numbers are written to out: match number, pattern index,
 * capture index, start byte, end byte, start row, start column (bytes), end
 * row, end column. Captures of one match are consecutive. Returns the
 * number of captures, or -1 when more than max would be needed. */
int32_t him_ts_query(TSQueryCursor *cursor, const TSQuery *query, const TSTree *tree,
                     uint32_t start, uint32_t end, uint32_t *out, uint32_t max)
{
    TSNode root = ts_tree_root_node(tree);
    ts_query_cursor_set_byte_range(cursor, start, end);
    ts_query_cursor_exec(cursor, query, root);
    TSQueryMatch match;
    uint32_t n = 0, match_no = 0;
    while (ts_query_cursor_next_match(cursor, &match)) {
        for (uint16_t i = 0; i < match.capture_count; i++) {
            if (n >= max)
                return -1;
            TSNode node = match.captures[i].node;
            TSPoint s = ts_node_start_point(node), e = ts_node_end_point(node);
            uint32_t *r = out + 9 * n;
            r[0] = match_no;
            r[1] = match.pattern_index;
            r[2] = match.captures[i].index;
            r[3] = ts_node_start_byte(node);
            r[4] = ts_node_end_byte(node);
            r[5] = s.row;
            r[6] = s.column;
            r[7] = e.row;
            r[8] = e.column;
            n++;
        }
        match_no++;
    }
    return (int32_t)n;
}
