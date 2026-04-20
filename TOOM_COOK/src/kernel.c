#include "poly.h"

#define mask33 0x1FFFFFFFF
#define mask30 0x3FFFFFFF
#define mask28 0xFFFFFFF
#define mask27 0x7FFFFFF
#define mask25 0x1FFFFFF
#define mask24 0xFFFFFF
void kernel(uint32_t *a, uint32_t *b, uint32_t *c)
{
    uint32_t aws[7 * 4], bws[7 * 4];
    uint32_t w[7 * 4];

    for (int j = 0; j < 4; j++)
    {
        eval(a, aws, j, 4);
        eval(b, bws, j, 4);
    }
    for (int i = 0; i < 7; i++)
    {
        // 24+3+3，8+4+4
        dot_product(&aws[i * 4], &bws[i * 4], &w[i * 4]);
    }
    uint32_t r[10] = {0};
    for (int i = 0; i < 4; i++)
    {
        interp(w, c, r, i, 4);
    }
    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];
    for (int i = 0; i < 16; i++)
    {
        c[i] = c[i] & mask27;
    }
}

void eval(const uint32_t *a, uint32_t *aws, int j, int N)
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
void interp(uint32_t *w, uint32_t *c, uint32_t *r, int i, int N)
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
    case 16:
    {
        r[1] = (r[1] + r[4]) & mask27;
        r[5] = (r[5] - r[4]) & mask27;
        r[3] = ((r[3] - r[2]) >> 1) & mask27;
        r[4] = (r[4] - r[0]) & mask27;

        r[4] = ((r[4] << 1) + r[5] - (r[6] << 7)) & mask27;
        r[2] = (r[2] + r[3]) & mask27;

        r[1] = (r[1] + -(r[2] << 6) - r[2]) & mask27;
        r[2] = (r[2] - r[6] - r[0]) & mask27;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask27;
        r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) & mask27) >> 3) * 0xAAAAAAB) & mask27;

        r[5] = (((r[5] + r[1]) >> 1) * 0xEEEEEF) & mask25;
        r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) & mask27) >> 1) * 0xE38E39) & mask25;
        r[2] = (r[2] - r[4]) & mask24;

        r[3] = (-r[3] - r[1]) & mask24;
        r[5] = (r[1] - r[5]) >> 1;
        r[1] = r[1] - r[5];
        break;
    }
    case 4:
    {
        r[1] = (r[1] + r[4]) & mask30;
        r[5] = (r[5] - r[4]) & mask30;
        r[3] = ((r[3] - r[2]) >> 1) & mask30;
        r[4] = (r[4] - r[0]) & mask30;

        r[4] = ((r[4] << 1) + r[5] - (r[6] << 7)) & mask30;
        r[2] = (r[2] + r[3]) & mask30;

        r[1] = (r[1] + -(r[2] << 6) - r[2]) & mask30;
        r[2] = (r[2] - r[6] - r[0]) & mask30;

        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & mask30;
        r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) & mask30) >> 3) * 0x2AAAAAAB) & mask30;

        r[5] = (((r[5] + r[1]) >> 1) * 0x0EEEEEEF) & mask28;
        r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) & mask30) >> 1) * 0x08E38E39) & mask28;
        r[2] = (r[2] - r[4]) & mask27;

        r[3] = (-r[3] - r[1]) & mask27;
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
uint64_t mul_signed_mq33_q21(uint64_t a, uint32_t b)
{
    uint32_t b_sign = b & 0x100000;
    int64_t b_signed = (b | (-(int32_t)b_sign)) & 0x1FFFFFFFFULL; // 21bit signed -> 33bit ring
    int64_t c = (int64_t)a * b_signed;
    return (uint64_t)c & 0x1FFFFFFFFULL;
}
void dot_product(uint32_t *a, uint32_t *b, uint32_t *c)
{
    uint32_t aws[7], bws[7];
    uint64_t w[7];
    eval(a, aws, 0, 1);
    eval(b, bws, 0, 1);
    for (int i = 0; i < 7; i++)
    {
        // 24+3+3+3, 8+4+4+4+1
        uint64_t aw = (uint64_t)aws[i] & 0x1FFFFFFFFULL; // 33 bit
        uint32_t bw = bws[i] & 0x1FFFFF;                 // 21 bit
        w[i] = mul_signed_mq33_q21(aw, bw);
    }
    uint64_t r[6];

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

    c[0] = (w[6] - r[2]);
    c[1] = (r[5] - r[1]);
    c[2] = (r[4] - w[0]);
    c[3] = r[3];
    for (int j = 0; j < 4; j++)
    {
        c[j] &= mask30;
    }
}
/* uint32_t aws[7], bws[7];
uint64_t w[7];

for (int i = 0; i < 7; i++)
{
    // 24+3+3+3, 8+4+4+4+1
    uint32_t aw = aws[i];
    uint32_t bw = bws[i] & 0x1FFFFF; // 21 bit
    w[i] = mul_signed_mq33_q21(aw, bw);
}

uint64_t mul_signed_mq33_q21(uint32_t a, uint32_t b)
{
    uint32_t b_sign = b & 0x100000;
    int64_t b_signed = ((int64_t)(b | (uint32_t)(-(int32_t)b_sign))) & 0x1FFFFFFFFULL;
    int64_t c = (int64_t)(uint64_t)a * b_signed;
    return (uint64_t)c & 0x1FFFFFFFFULL;
} */
