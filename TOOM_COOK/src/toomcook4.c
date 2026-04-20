#include "poly.h"

#define mask39 0x7FFFFFFFFFULL
#define mask37 0x1FFFFFFFFFULL
#define mask36 0xFFFFFFFFFULL
#define mask34 0x3FFFFFFFFULL
#define mask33 0x1FFFFFFFFULL
#define mask31 0x7FFFFFFFULL
#define mask30 0x3FFFFFFFULL
#define mask28 0xFFFFFFFULL
#define mask27 0x7FFFFFFULL
#define mask25 0x1FFFFFFULL
#define mask24 0xFFFFFFULL

void toomcook4(const uint64_t *a, const uint64_t *b, uint64_t *c, int N)
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
            toomcook4(&aws[i * (N / 4)], &bws[i * (N / 4)], &w[i * (N / 4)], (N / 4));
    }

    for (int i = 0; i < (N / 4); i++)
    {
        interpolation(w, c, r, i, (N / 4));
    }

    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];

    for (int i = 0; i < N; i++)
    {
        switch (N)
        {
        case 1024:
            c[i] = c[i] & mask24;
            break;
        case 256:
            c[i] = c[i] & mask27;
            break;
        case 64:
            c[i] = c[i] & mask30;
            break;
        case 16:
            c[i] = c[i] & mask33;
            break;
        }
    }
}

void evaluation(const uint64_t *a, uint64_t *aws, int j, int N)
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
void product(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    uint64_t aws[7], bws[7], w[7];
    uint64_t r[6];
    evaluation(a, aws, 0, 1);
    evaluation(b, bws, 0, 1);
    for (int i = 0; i < 7; i++)
    {
        uint64_t aw = aws[i] & mask39;
        uint64_t bw = bws[i] & 0x1FFFFFFFULL;
        uint64_t bw_sign = bw & 0x10000000ULL;
        int64_t bw_signed = (int64_t)((bw | (uint64_t)(-(int64_t)bw_sign)) & mask39);
        int64_t mul = (int64_t)aw * bw_signed;
        w[i] = (uint64_t)mul & mask39;
    }

    r[5] = (w[5] - w[4]) & mask39;
    r[3] = ((w[3] - w[2]) & mask39) >> 1;
    r[4] = (w[4] - w[0]) & mask39;

    r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & mask39;
    r[2] = (w[2] + r[3]) & mask39;

    r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & mask39;
    r[2] = (r[2] - w[6] - w[0]) & mask39;

    r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask39;
    r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask39) >> 3) * 0xAAAAAAAABULL) & mask36;

    r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEEEEFULL) & mask37;
    r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask39) >> 1) * 0xE38E38E39ULL) & mask37;
    r[2] = (r[2] - r[4]) & mask36;

    r[3] = (-r[3] - r[1]) & mask36;
    r[5] = (r[1] - r[5]) >> 1;
    r[1] = r[1] - r[5];

    c[0] = (w[6] - r[2]) & mask36;
    c[1] = (r[5] - r[1]) & mask36;
    c[2] = (r[4] - w[0]) & mask36;
    c[3] = r[3] & mask36;
}

void interpolation(const uint64_t *w, uint64_t *c, uint64_t *r, int i, int N)
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

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEFULL) & mask25;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask27) >> 1) * 0xE38E39ULL) & mask25;
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

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEEFULL) & mask28;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask30) >> 1) * 0x8E38E39ULL) & mask28;
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
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & mask33) >> 3) * 0x2AAAAAABULL) & mask30;

        r[5] = (((r[5] + r[1]) >> 1) * 0x6EEEEEEFULL) & mask31;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask33) >> 1) * 0x38E38E39ULL) & mask31;
        r[2] = (r[2] - r[4]) & mask30;

        r[3] = (-r[3] - r[1]) & mask30;
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

        r[5] = (((r[5] + r[1]) >> 1) * 0x2EEEEEEEFULL) & mask34;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & mask36) >> 1) * 0x238E38E39ULL) & mask34;
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
