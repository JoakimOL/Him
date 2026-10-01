#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Byte-level helpers for UTF-8 text held in GHC byte arrays. Every function
 * takes (array, offset, length) so Haskell can pass a Text's internals
 * directly (unsafe FFI: the GC cannot move the array during the call). */

/* Number of occurrences of byte b. */
size_t him_count_byte(const uint8_t *arr, size_t off, size_t len, uint8_t b)
{
    const uint8_t *p = arr + off, *end = p + len;
    size_t n = 0;
    while ((p = memchr(p, b, end - p)) != NULL) {
        n++;
        p++;
    }
    return n;
}

/* Line start offsets: out[0] = 0, out[k] = (position of the k-th '\n') + 1,
 * and a final entry len + 1, as if the region ended with a newline. */
void him_line_starts(const uint8_t *arr, size_t off, size_t len, uint32_t *out)
{
    const uint8_t *start = arr + off, *p = start, *end = start + len;
    size_t k = 0;
    out[k++] = 0;
    while ((p = memchr(p, '\n', end - p)) != NULL) {
        p++;
        out[k++] = (uint32_t)(p - start);
    }
    out[k] = (uint32_t)(len + 1);
}

static inline uint8_t lower(uint8_t c) { return (c >= 'A' && c <= 'Z') ? c + 32 : c; }
static inline uint8_t upper(uint8_t c) { return (c >= 'a' && c <= 'z') ? c - 32 : c; }

static int matches_folded(const uint8_t *h, const uint8_t *n, size_t len)
{
    for (size_t i = 1; i < len; i++)
        if (lower(h[i]) != n[i])
            return 0;
    return 1;
}

/* First occurrence of the needle in the haystack, or -1. With fold != 0 the
 * needle must already be lower-case and ASCII letters match either case. */
ptrdiff_t him_find_forward(const uint8_t *harr, size_t hoff, size_t hlen,
                           const uint8_t *narr, size_t noff, size_t nlen, int fold)
{
    const uint8_t *h = harr + hoff, *n = narr + noff;
    if (nlen == 0 || nlen > hlen)
        return -1;
    if (!fold) {
        const uint8_t *p = memmem(h, hlen, n, nlen);
        return p ? p - h : -1;
    }
    const uint8_t lo = n[0], up = upper(n[0]);
    const uint8_t *p = h, *last = h + hlen - nlen; /* last possible start */
    while (p <= last) {
        size_t span = last - p + 1;
        const uint8_t *a = memchr(p, lo, span);
        const uint8_t *b = (up != lo) ? memchr(p, up, (a ? (size_t)(a - p) : span)) : NULL;
        const uint8_t *c = b ? b : a; /* b is only searched before a */
        if (!c)
            return -1;
        if (matches_folded(c, n, nlen))
            return c - h;
        p = c + 1;
    }
    return -1;
}

/* Last occurrence of the needle in the haystack, or -1. */
ptrdiff_t him_find_backward(const uint8_t *harr, size_t hoff, size_t hlen,
                            const uint8_t *narr, size_t noff, size_t nlen, int fold)
{
    const uint8_t *h = harr + hoff, *n = narr + noff;
    if (nlen == 0 || nlen > hlen)
        return -1;
    const uint8_t lo = n[0], up = fold ? upper(n[0]) : n[0];
    size_t span = hlen - nlen + 1; /* candidate starts are h[0 .. span) */
    while (span > 0) {
        const uint8_t *a = memrchr(h, lo, span);
        const uint8_t *b = (up != lo) ? memrchr(h, up, span) : NULL;
        const uint8_t *c = (a && b) ? (a > b ? a : b) : (a ? a : b);
        if (!c)
            return -1;
        if (fold ? matches_folded(c, n, nlen) : memcmp(c + 1, n + 1, nlen - 1) == 0)
            return c - h;
        span = c - h;
    }
    return -1;
}
