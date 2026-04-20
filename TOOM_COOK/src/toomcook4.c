#include "poly.h"

#define mask36 0xFFFFFFFFFULL
#define mask33 0x1FFFFFFFFULL
#define mask30 0x3FFFFFFFULL
#define mask27 0x7FFFFFFULL
#define mask24 0xFFFFFFULL

static inline uint64_t coeff_mask_by_size(int N)
{
    switch (N)
    {
    case 1024:
        return mask24;
    case 256:
        return mask27;
    case 64:
        return mask30;
    case 16:
        return mask33;
    default:
        return mask36;
    }
}

static void evaluation(const uint64_t *a, uint64_t *aws, int j, int N)
{
    uint64_t r0 = a[j * 4];
    uint64_t r1 = a[1 + j * 4];
    uint64_t r2 = a[2 + j * 4];
    uint64_t r3 = a[3 + j * 4];

    uint64_t t0 = r0 + r2;
    uint64_t t1 = r1 + r3;
    aws[2 * N + j] = t0 + t1;
    aws[3 * N + j] = t0 - t1;

    t0 = (((r0 << 2) + r2) << 1);
    t1 = (r1 << 2) + r3;
    aws[4 * N + j] = t0 + t1;
    aws[5 * N + j] = t0 - t1;

    aws[1 * N + j] = (r3 << 3) + (r2 << 2) + (r1 << 1) + r0;
    aws[6 * N + j] = r0;
    aws[0 * N + j] = r3;
}

static void product(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    for (int i = 0; i < 4; i++)
    {
        c[i] = 0;
    }
    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            uint64_t prod = (a[i] * b[j]) & mask36;
            int k = i + j;
            if (k < 4)
            {
                c[k] = (c[k] + prod) & mask36;
            }
            else
            {
                c[k - 4] = (c[k - 4] - prod) & mask36;
            }
        }
    }
}

static void interpolation(const uint64_t *w, uint64_t *c, uint64_t *r, int i, int N)
{
    r[7] = r[0];
    r[8] = r[1];
    r[9] = r[2];

    const uint64_t p0 = w[0 * N + i];
    const uint64_t p1 = w[1 * N + i];
    const uint64_t p2 = w[2 * N + i];
    const uint64_t p3 = w[3 * N + i];
    const uint64_t p4 = w[4 * N + i];
    const uint64_t p5 = w[5 * N + i];
    const uint64_t p6 = w[6 * N + i];

    r[0] = p0;
    r[1] = p1;
    r[2] = p2;
    r[3] = p3;
    r[4] = p4;
    r[5] = p5;
    r[6] = p6;

    switch (N)
    {
    case 256:
        r[5] = (p5 - p4) & mask27;
        r[3] = ((p3 - p2) & mask27) >> 1;
        r[4] = (p4 - p0) & mask27;

        r[4] = ((r[4] << 1) + r[5] - (p6 << 7)) & mask27;
        r[2] = (p2 + r[3]) & mask27;

        r[1] = (p1 + p4 - (r[2] << 6) - r[2]) & mask27;
        r[2] = (r[2] - p6 - p0) & mask27;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask27;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask27) >> 3) * 0xAAAAABULL) & mask24;

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEFULL) & 0x1FFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask27) >> 1) * 0xE38E39ULL) & 0x1FFFFFFULL;
        r[2] = (r[2] - r[4]) & mask24;

        r[3] = (-r[3] - r[1]) & mask24;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;

    case 64:
        r[5] = (p5 - p4) & mask30;
        r[3] = ((p3 - p2) & mask30) >> 1;
        r[4] = (p4 - p0) & mask30;

        r[4] = ((r[4] << 1) + r[5] - (p6 << 7)) & mask30;
        r[2] = (p2 + r[3]) & mask30;

        r[1] = (p1 + p4 - (r[2] << 6) - r[2]) & mask30;
        r[2] = (r[2] - p6 - p0) & mask30;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask30;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask30) >> 3) * 0x2AAAAABULL) & mask27;

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEEFULL) & 0xFFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask30) >> 1) * 0x8E38E39ULL) & 0xFFFFFFFULL;
        r[2] = (r[2] - r[4]) & mask27;

        r[3] = (-r[3] - r[1]) & mask27;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;

    case 16:
        r[5] = (p5 - p4) & mask33;
        r[3] = ((p3 - p2) & mask33) >> 1;
        r[4] = (p4 - p0) & mask33;

        r[4] = ((r[4] << 1) + r[5] - (p6 << 7)) & mask33;
        r[2] = (p2 + r[3]) & mask33;

        r[1] = (p1 + p4 - (r[2] << 6) - r[2]) & mask33;
        r[2] = (r[2] - p6 - p0) & mask33;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask33;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask33) >> 3) * 0x2AAAAAABULL) & 0x3FFFFFFFULL;

        r[5] = (((r[5] + r[1]) >> 1) * 0x6EEEEEEFULL) & 0x7FFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask33) >> 1) * 0x38E38E39ULL) & 0x7FFFFFFFULL;
        r[2] = (r[2] - r[4]) & 0x3FFFFFFFULL;

        r[3] = (-r[3] - r[1]) & 0x3FFFFFFFULL;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;

    case 4:
        r[5] = (p5 - p4) & mask36;
        r[3] = ((p3 - p2) & mask36) >> 1;
        r[4] = (p4 - p0) & mask36;

        r[4] = ((r[4] << 1) + r[5] - (p6 << 7)) & mask36;
        r[2] = (p2 + r[3]) & mask36;

        r[1] = (p1 + p4 - (r[2] << 6) - r[2]) & mask36;
        r[2] = (r[2] - p6 - p0) & mask36;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask36;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask36) >> 3) * 0xAAAAAAABULL) & mask33;

        r[5] = (((r[5] + r[1]) >> 1) * 0x2EEEEEEEFULL) & 0x3FFFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask36) >> 1) * 0x238E38E39ULL) & 0x3FFFFFFFFULL;
        r[2] = (r[2] - r[4]) & mask33;

        r[3] = (-r[3] - r[1]) & mask33;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;
    }

    c[4 * i + 3] = r[3];
    if (i == 0)
    {
        c[4 * i] = r[6];
        c[4 * i + 1] = r[5];
        c[4 * i + 2] = r[4];
    }
    else
    {
        c[4 * i] = r[6] + r[9];
        c[4 * i + 1] = r[5] + r[8];
        c[4 * i + 2] = r[4] + r[7];
    }
}

static void toomcook4_recursive(const uint64_t *a, const uint64_t *b, uint64_t *c, int N)
{
    uint64_t aws[7 * (N / 4)], bws[7 * (N / 4)];
    uint64_t w[7 * (N / 4)];
    uint64_t r[10] = {0};

    for (int j = 0; j < (N / 4); j++)
    {
        evaluation(a, aws, j, (N / 4));
        evaluation(b, bws, j, (N / 4));
    }

    for (int i = 0; i < 7; i++)
    {
        if (N == 16)
            product(&aws[i * (N / 4)], &bws[i * (N / 4)], &w[i * (N / 4)]);
        else
            toomcook4_recursive(&aws[i * (N / 4)], &bws[i * (N / 4)], &w[i * (N / 4)], (N / 4));
    }

    for (int i = 0; i < (N / 4); i++)
    {
        interpolation(w, c, r, i, (N / 4));
    }

    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];

    const uint64_t mask = coeff_mask_by_size(N);
    for (int i = 0; i < N; i++)
    {
        c[i] &= mask;
    }
}

void toomcook4(const uint32_t *a, const uint32_t *b, uint32_t *c, int N)
{
    uint64_t *a64 = (uint64_t *)malloc((size_t)N * sizeof(uint64_t));
    uint64_t *b64 = (uint64_t *)malloc((size_t)N * sizeof(uint64_t));
    uint64_t *c64 = (uint64_t *)malloc((size_t)N * sizeof(uint64_t));
    if (a64 == NULL || b64 == NULL || c64 == NULL)
    {
        free(a64);
        free(b64);
        free(c64);
        return;
    }

    for (int i = 0; i < N; i++)
    {
        a64[i] = a[i];
        b64[i] = b[i];
    }

    toomcook4_recursive(a64, b64, c64, N);

    for (int i = 0; i < N; i++)
    {
        c[i] = (uint32_t)c64[i];
    }

    free(a64);
    free(b64);
    free(c64);
}
