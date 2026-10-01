#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Byte-level helpers for UTF-8 text held in GHC byte arrays. Every function
 * takes (array, offset, length) so Haskell can pass a Text's internals
 * directly (unsafe FFI: the GC cannot move the array during the call). */

#ifdef __SSE2__
#include <emmintrin.h>
#endif

/* Number of occurrences of byte b. With SSE2: compare 16 bytes at a time
 * and add the 0/-1 results into byte counters, summed every 255 rounds
 * (before they can overflow) with _mm_sad_epu8. No call per occurrence. */
size_t him_count_byte(const uint8_t *arr, size_t off, size_t len, uint8_t b)
{
    const uint8_t *p = arr + off, *end = p + len;
    size_t n = 0;
#ifdef __SSE2__
    const __m128i vb = _mm_set1_epi8((char)b), zero = _mm_setzero_si128();
    while ((size_t)(end - p) >= 16) {
        size_t rounds = (size_t)(end - p) / 16;
        if (rounds > 255)
            rounds = 255;
        __m128i acc = zero;
        for (size_t i = 0; i < rounds; i++, p += 16)
            acc = _mm_sub_epi8(acc, _mm_cmpeq_epi8(_mm_loadu_si128((const __m128i *)p), vb));
        __m128i sums = _mm_sad_epu8(acc, zero);
        n += (size_t)_mm_cvtsi128_si32(sums) + (size_t)_mm_cvtsi128_si32(_mm_srli_si128(sums, 8));
    }
#endif
    for (; p < end; p++)
        n += (*p == b);
    return n;
}

/* Line start offsets: out[0] = 0, out[k] = (position of the k-th '\n') + 1,
 * and a final entry len + 1, as if the region ended with a newline. With
 * SSE2 the newlines of 16 bytes come out of one movemask. */
void him_line_starts(const uint8_t *arr, size_t off, size_t len, uint32_t *out)
{
    const uint8_t *start = arr + off, *p = start, *end = start + len;
    size_t k = 0;
    out[k++] = 0;
#ifdef __SSE2__
    const __m128i nl = _mm_set1_epi8('\n');
    for (; (size_t)(end - p) >= 16; p += 16) {
        unsigned m = (unsigned)_mm_movemask_epi8(_mm_cmpeq_epi8(_mm_loadu_si128((const __m128i *)p), nl));
        while (m) {
            out[k++] = (uint32_t)(p - start) + (uint32_t)__builtin_ctz(m) + 1;
            m &= m - 1;
        }
    }
#endif
    for (; p < end; p++)
        if (*p == '\n')
            out[k++] = (uint32_t)(p - start) + 1;
    out[k] = (uint32_t)(len + 1);
}

static inline uint8_t lower(uint8_t c) { return (c >= 'A' && c <= 'Z') ? c + 32 : c; }
static inline uint8_t upper(uint8_t c) { return (c >= 'a' && c <= 'z') ? c - 32 : c; }

/* First byte in [p, end) equal to a or b. With a == b this is memchr (which
 * glibc vectorises well); otherwise compare 16 bytes at a time with SSE2. */
static const uint8_t *find2(const uint8_t *p, const uint8_t *end, uint8_t a, uint8_t b)
{
    if (a == b)
        return p < end ? memchr(p, a, end - p) : NULL;
#ifdef __SSE2__
    const __m128i va = _mm_set1_epi8((char)a), vb = _mm_set1_epi8((char)b);
    for (; p + 16 <= end; p += 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)p);
        int m = _mm_movemask_epi8(_mm_or_si128(_mm_cmpeq_epi8(x, va), _mm_cmpeq_epi8(x, vb)));
        if (m)
            return p + __builtin_ctz(m);
    }
#endif
    for (; p < end; p++)
        if (*p == a || *p == b)
            return p;
    return NULL;
}

/* Last byte in [start, end) equal to a or b. */
static const uint8_t *rfind2(const uint8_t *start, const uint8_t *end, uint8_t a, uint8_t b)
{
    if (a == b)
        return end > start ? memrchr(start, a, end - start) : NULL;
#ifdef __SSE2__
    const __m128i va = _mm_set1_epi8((char)a), vb = _mm_set1_epi8((char)b);
    for (; end - 16 >= start; end -= 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)(end - 16));
        int m = _mm_movemask_epi8(_mm_or_si128(_mm_cmpeq_epi8(x, va), _mm_cmpeq_epi8(x, vb)));
        if (m)
            return end - 16 + (31 - __builtin_clz(m));
    }
#endif
    while (end > start) {
        end--;
        if (*end == a || *end == b)
            return end;
    }
    return NULL;
}

/* The needle position whose byte is rarest in (a sample of) the haystack.
 * Scanning for a rare byte means few candidates to verify, whatever the
 * text looks like (the idea behind ripgrep's rare-byte prefilter, but
 * measured on the actual text instead of a fixed frequency table). */
static size_t rarest(const uint8_t *h, size_t hlen, const uint8_t *n, size_t nlen, int fold)
{
    uint32_t count[256] = {0};
    size_t sample = hlen < 4096 ? hlen : 4096;
    for (size_t i = 0; i < sample; i++)
        count[fold ? lower(h[i]) : h[i]]++;
    size_t best = 0;
    for (size_t i = 1; i < nlen; i++)
        if (count[n[i]] < count[n[best]])
            best = i;
    return best;
}

static int equal_at(const uint8_t *w, const uint8_t *n, size_t nlen, int fold)
{
    if (!fold)
        return memcmp(w, n, nlen) == 0;
    for (size_t i = 0; i < nlen; i++)
        if (lower(w[i]) != n[i])
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
    if (!fold) { /* glibc's memmem (two-way, vectorised) is hard to beat */
        const uint8_t *p = memmem(h, hlen, n, nlen);
        return p ? p - h : -1;
    }
    size_t k = rarest(h, hlen, n, nlen, fold);
    const uint8_t a = n[k], b = upper(n[k]);
    /* The anchor byte of a match starting at s is at s + k. */
    const uint8_t *p = h + k, *end = h + hlen - nlen + k + 1;
    while ((p = find2(p, end, a, b)) != NULL) {
        if (equal_at(p - k, n, nlen, fold))
            return (p - k) - h;
        p++;
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
    size_t k = rarest(h, hlen, n, nlen, fold);
    const uint8_t a = n[k], b = fold ? upper(n[k]) : n[k];
    const uint8_t *start = h + k, *end = h + hlen - nlen + k + 1;
    const uint8_t *p;
    while ((p = rfind2(start, end, a, b)) != NULL) {
        if (equal_at(p - k, n, nlen, fold))
            return (p - k) - h;
        end = p;
    }
    return -1;
}
