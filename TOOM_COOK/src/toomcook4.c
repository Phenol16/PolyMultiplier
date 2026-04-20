#include "poly.h"

#define mask36 0xFFFFFFFFFULL
#define mask33 0x1FFFFFFFFULL
#define mask30 0x3FFFFFFF
#define mask27 0x7FFFFFF
#define mask24 0xFFFFFF
void toomcook4(const uint32_t *a, const uint32_t *b, uint32_t *c, int N)
{
    /*     uint32_t acoeff[4][4][4][16], bcoeff[4][4][4][16];
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    for (int l = 0; l < 16; l++)
                    {
                        acoeff[i][j][k][l] = a[4 * (4 * (4 * l + k) + j) + i]; // acoeff[256*i + 64*j + 16*k + l]=a[64*l + 16*k + 4*j + i]
                        bcoeff[i][j][k][l] = b[4 * (4 * (4 * l + k) + j) + i];
                    }
                }
            }
        } */

    uint32_t aws[7 * (N / 4)], bws[7 * (N / 4)];
    uint32_t w[7 * (N / 4)];
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
void evaluation(const uint32_t *a, uint32_t *aws, int j, int N)
{
    uint32_t r[8];
    r[0] = a[j * 4];
    r[1] = a[1 + j * 4];
    r[2] = a[2 + j * 4];
    r[3] = a[3 + j * 4];

    r[4] = r[0] + r[2];
    r[5] = r[1] + r[3];
    r[6] = r[4] + r[5];
    r[7] = r[4] - r[5];

    aws[2 * N + j] = r[6];
    aws[3 * N + j] = r[7];

    r[4] = (((r[0] << 2) + r[2]) << 1);
    r[5] = (r[1] << 2) + r[3];
    r[6] = r[4] + r[5];
    r[7] = r[4] - r[5];

    aws[4 * N + j] = r[6];
    aws[5 * N + j] = r[7];

    r[4] = (r[3] << 3) + (r[2] << 2) + (r[1] << 1) + r[0];

    aws[1 * N + j] = r[4];
    aws[6 * N + j] = r[0];
    aws[0 * N + j] = r[3];
}
void interpolation(uint64_t *w, uint64_t *c, uint64_t *r, int i, int N)
{
    r[7] = r[0];
    r[8] = r[1];
    r[9] = r[2];
    r[0] = w[0 * N + i];
    r[1] = w[1 * N + i];
    r[2] = w[2 * N + i];
    r[3] = w[3 * N + i];
    r[4] = w[4 * N + i];
    r[5] = w[5 * N + i];
    r[6] = w[6 * N + i];
    switch (N)
    {
    case 256:
    {
        r[5] = (w[5] - w[4]) & 0x7FFFFFFULL;
        r[3] = ((w[3] - w[2]) & 0x7FFFFFFULL) >> 1;
        r[4] = (w[4] - w[0]) & 0x7FFFFFFULL;

        r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x7FFFFFFULL;
        r[2] = (w[2] + r[3]) & 0x7FFFFFFULL;

        r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & 0x7FFFFFFULL;
        r[2] = (r[2] - w[6] - w[0]) & 0x7FFFFFFULL;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFFULL;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & 0x7FFFFFFULL) >> 3) * 0xAAAAABULL) & 0xFFFFFFULL;

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEFULL) & 0x1FFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & 0x7FFFFFFULL) >> 1) * 0xE38E39ULL) & 0x1FFFFFFULL;
        r[2] = (r[2] - r[4]) & 0xFFFFFFULL;

        r[3] = (-r[3] - r[1]) & 0xFFFFFFULL;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;
    }
    case 64:
    {
        r[5] = (w[5] - w[4]) & 0x3FFFFFFFULL;
        r[3] = ((w[3] - w[2]) & 0x3FFFFFFFULL) >> 1;
        r[4] = (w[4] - w[0]) & 0x3FFFFFFFULL;

        r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x3FFFFFFFULL;
        r[2] = (w[2] + r[3]) & 0x3FFFFFFFULL;

        r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & 0x3FFFFFFFULL;
        r[2] = (r[2] - w[6] - w[0]) & 0x3FFFFFFFULL;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x3FFFFFFFULL;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & 0x3FFFFFFFULL) >> 3) * 0x2AAAAABULL) & 0x7FFFFFFULL;

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEEFULL) & 0xFFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & 0x3FFFFFFFULL) >> 1) * 0x8E38E39ULL) & 0xFFFFFFFULL;
        r[2] = (r[2] - r[4]) & 0x7FFFFFFULL;

        r[3] = (-r[3] - r[1]) & 0x7FFFFFFULL;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];

        break;
    }
    case 16:
    {
        r[5] = (w[5] - w[4]) & 0x1FFFFFFFFULL;
        r[3] = ((w[3] - w[2]) & 0x1FFFFFFFFULL) >> 1;
        r[4] = (w[4] - w[0]) & 0x1FFFFFFFFULL;

        r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x1FFFFFFFFULL;
        r[2] = (w[2] + r[3]) & 0x1FFFFFFFFULL;

        r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & 0x1FFFFFFFFULL;
        r[2] = (r[2] - w[6] - w[0]) & 0x1FFFFFFFFULL;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x1FFFFFFFFULL;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & 0x1FFFFFFFFULL) >> 3) * 0x2AAAAAABULL) & 0x3FFFFFFFULL;

        r[5] = (((r[5] + r[1]) >> 1) * 0x6EEEEEEFULL) & 0x7FFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & 0x1FFFFFFFFULL) >> 1) * 0x38E38E39ULL) & 0x7FFFFFFFULL;
        r[2] = (r[2] - r[4]) & 0x3FFFFFFFULL;

        r[3] = (-r[3] - r[1]) & 0x3FFFFFFFULL;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;
    }
    case 4:
    {
        r[5] = (w[5] - w[4]) & 0xFFFFFFFFFULL;
        r[3] = ((w[3] - w[2]) & 0xFFFFFFFFFULL) >> 1;
        r[4] = (w[4] - w[0]) & 0xFFFFFFFFFULL;

        r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0xFFFFFFFFFULL;
        r[2] = (w[2] + r[3]) & 0xFFFFFFFFFULL;

        r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & 0xFFFFFFFFFULL;
        r[2] = (r[2] - w[6] - w[0]) & 0xFFFFFFFFFULL;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0xFFFFFFFFFULL;
        r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & 0xFFFFFFFFFULL) >> 3) * 0xAAAAAAABULL) & 0x1FFFFFFFFULL;

        r[5] = (((r[5] + r[1]) >> 1) * 0x2EEEEEEEFULL) & 0x3FFFFFFFFULL;
        r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & 0xFFFFFFFFFULL) >> 1) * 0x238E38E39ULL) & 0x3FFFFFFFFULL;
        r[2] = (r[2] - r[4]) & 0x1FFFFFFFFULL;

        r[3] = (-r[3] - r[1]) & 0x1FFFFFFFFULL;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;
    }
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
void product(uint32_t *a, uint32_t *b, uint64_t *c)
{
    uint32_t aws[7], bws[7];
    uint64_t w[7];
    eval(a, aws, 0, 1);
    eval(b, bws, 0, 1);
    for (int i = 0; i < 7; i++)
    {
        uint64_t aw = (uint64_t)aws[i] & 0x7FFFFFFFFFULL; // 39 bit
        uint32_t bw = bws[i] & 0x1FFFFFFF;                // 29 bit
        w[i] = mul_signed_mq39_q29(aw, bw);
    }
    uint64_t r[6];
    r[5] = (w[5] - w[4]) & 0x7FFFFFFFFFULL;
    r[3] = ((w[3] - w[2]) & 0x7FFFFFFFFFULL) >> 1;
    r[4] = (w[4] - w[0]) & 0x7FFFFFFFFFULL;

    r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x7FFFFFFFFFULL;
    r[2] = (w[2] + r[3]) & 0x7FFFFFFFFFULL;

    r[1] = (w[1] + w[4] - (r[2] << 6) - r[2]) & 0x7FFFFFFFFFULL;
    r[2] = (r[2] - w[6] - w[0]) & 0x7FFFFFFFFFULL;

    r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFFFFFULL;
    r[4] = ((uint64_t)(((r[4] - (r[2] << 3)) & 0x7FFFFFFFFFULL) >> 3) * 0xAAAAAAAABULL) & 0xFFFFFFFFFULL;

    r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEEEEFULL) & 0x1FFFFFFFFFULL;
    r[1] = ((uint64_t)(((r[1] + (r[3] << 4)) & 0x7FFFFFFFFFULL) >> 1) * 0xE38E38E39ULL) & 0x1FFFFFFFFFULL;
    r[2] = (r[2] - r[4]) & 0xFFFFFFFFFULL;

    r[3] = (-r[3] - r[1]) & 0xFFFFFFFFFULL;
    r[5] = (r[1] - r[5]) >> 1;
    r[1] = r[1] - r[5];
    for (int j = 0; j < 4; j++)
    {
        c[j] &= mask36;
    }
}
uint64_t mul_signed_mq39_q29(uint64_t a, uint32_t b)
{
    uint32_t b_sign = b & 0x10000000;
    int64_t b_signed = (b | (-(int32_t)b_sign)) & 0x7FFFFFFFFFULL; // 29bit signed -> 39bit ring
    int64_t c = (int64_t)a * b_signed;
    return (uint64_t)c & 0x7FFFFFFFFFULL;
}