#include "poly.h"

void toomcook44(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    // Evaluation
    uint32_t A[7], B[7];
    A[0] = a[3];
    A[1] = a[0] + (a[1] << 1) + (a[2] << 2) + (a[3] << 3);
    A[2] = a[0] + a[1] + a[2] + a[3];
    A[3] = a[0] - a[1] + a[2] - a[3];
    A[4] = (a[0] << 3) + (a[1] << 2) + (a[2] << 1) + a[3];
    A[5] = (a[0] << 3) - (a[1] << 2) + (a[2] << 1) - a[3];
    A[6] = a[0];

    B[0] = b[3];
    B[1] = b[0] + (b[1] << 1) + (b[2] << 2) + (b[3] << 3);
    B[2] = b[0] + b[1] + b[2] + b[3];
    B[3] = b[0] - b[1] + b[2] - b[3];
    B[4] = (b[0] << 3) + (b[1] << 2) + (b[2] << 1) + b[3];
    B[5] = (b[0] << 3) - (b[1] << 2) + (b[2] << 1) - b[3];
    B[6] = b[0];

    uint32_t w[7];
    for (int i = 0; i < 7; i++)
    {

        // for 4
        /*         // 24 + 3 + 1 signed
                uint32_t Aw = (A[i] & 0x7FFFFFF) | ((A[i] >> 4) & 0x8000000);
                // 8 + 4 + 1 signed
                uint32_t Bw = (B[i] & 0xFFF) | ((B[i] >> 19) & 0x1000);
                w[i] = mul_signed_mq28_q13(Aw, Bw); */

        // for 16
        // 24 + 3 + 3 + 1 signed
        uint32_t Aw = (A[i] & 0x3FFFFFFF) | ((A[i] >> 1) & 0x40000000);
        // 8 + 4 + 4 + 1 signed
        uint32_t Bw = (B[i] & 0xFFFF) | ((B[i] >> 15) & 0x10000);
        w[i] = mul_signed_mq31_q17(Aw, Bw);
    }
/*     for (int i = 0; i < 7; i++)
    {
        printf("Aw[%d]=%x   Bw[%d]=%x    w[%d]=%x\n", i, A[i], i, B[i], i, w[i]);
    } */
    // Interpolation
    uint32_t r[6];
    /* r[1] = (w[1] + w[4]) & 0x7FFFFFF;
    r[5] = (w[5] - w[4]) & 0x7FFFFFF;
    r[3] = ((w[3] - w[2]) >> 1) & 0x7FFFFFF;
    r[4] = (w[4] - w[0]) & 0x7FFFFFF;

    r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x7FFFFFF;
    r[2] = (w[2] + r[3]) & 0x7FFFFFF;
    r[1] = (r[1] - (r[2] << 6) - r[2]) & 0x7FFFFFF;

    r[2] = (r[2] - w[6] - w[0]) & 0x7FFFFFF;

    r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFF;
    r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) >> 3) & 0x7FFFFFF) * 0xAAAAAAB) & 0x7FFFFFF;

    r[5] = (r[5] + r[1]) & 0x7FFFFFF;
    r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) >> 1) & 0x7FFFFFF) * 0x8E38E39) & 0x7FFFFFF;
    r[2] = (r[2] - r[4]) & 0x7FFFFFF;

    r[3] = (-r[3] - r[1]) & 0x7FFFFFF;
    r[5] = ((r[1] - (((uint32_t)(r[5] >> 1) * 0xEEEEEEF) & 0x7FFFFFF)) >> 1) & 0x7FFFFFF; */
    r[1] = (w[1] + w[4]) & 0x3FFFFFFF;
    r[5] = (w[5] - w[4]) & 0x3FFFFFFF;
    r[3] = ((w[3] - w[2]) >> 1) & 0x3FFFFFFF;
    r[4] = (w[4] - w[0]) & 0x3FFFFFFF;

    r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x3FFFFFFF;
    r[2] = (w[2] + r[3]) & 0x3FFFFFFF;
    r[1] = (r[1] - (r[2] << 6) - r[2]) & 0x3FFFFFFF;

    r[2] = (r[2] - w[6] - w[0]) & 0x3FFFFFFF;

    r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x3FFFFFFF;
    r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) >> 3) & 0x3FFFFFFF) * 0x2AAAAAAB) & 0x3FFFFFFF;

    r[5] = (r[5] + r[1]) & 0x3FFFFFFF;
    r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) >> 1) & 0x3FFFFFFF) * 0x38E38E39) & 0x3FFFFFFF;
    r[2] = (r[2] - r[4]) & 0x3FFFFFFF;

    r[3] = (-r[3] - r[1]) & 0x3FFFFFFF;
    r[5] = ((r[1] - (((uint32_t)(r[5] >> 1) * 0x2EEEEEEF) & 0x3FFFFFFF)) >> 1) & 0x3FFFFFFF;

    r[1] = r[1] - r[5];

    // modulo
    c[0] = w[6] - r[2];
    c[1] = r[5] - r[1];
    c[2] = r[4] - w[0];
    c[3] = r[3];
    /*     for (int j = 0; j < 4; j++)
        {
            c[j] &= 0xFFFFFF;
        } */
    for (int j = 0; j < 4; j++)
    {
        c[j] &= 0x7FFFFFF;
    }
}

void toomcook44_unmodulo(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    // Evaluation
    uint32_t A[7], B[7];
    A[0] = a[3];
    A[1] = a[0] + (a[1] << 1) + (a[2] << 2) + (a[3] << 3);
    A[2] = a[0] + a[1] + a[2] + a[3];
    A[3] = a[0] - a[1] + a[2] - a[3];
    A[4] = (a[0] << 3) + (a[1] << 2) + (a[2] << 1) + a[3];
    A[5] = (a[0] << 3) - (a[1] << 2) + (a[2] << 1) - a[3];
    A[6] = a[0];

    B[0] = b[3];
    B[1] = b[0] + (b[1] << 1) + (b[2] << 2) + (b[3] << 3);
    B[2] = b[0] + b[1] + b[2] + b[3];
    B[3] = b[0] - b[1] + b[2] - b[3];
    B[4] = (b[0] << 3) + (b[1] << 2) + (b[2] << 1) + b[3];
    B[5] = (b[0] << 3) - (b[1] << 2) + (b[2] << 1) - b[3];
    B[6] = b[0];

    uint32_t w[7];
    for (int i = 0; i < 7; i++)
    {
        // 24 + 3 + 1 signed
        uint32_t Aw = (A[i] & 0x7FFFFFF) | ((A[i] >> 4) & 0x8000000);
        // 8 + 4 + 1 signed
        uint32_t Bw = (B[i] & 0xFFF) | ((B[i] >> 19) & 0x1000);
        w[i] = mul_signed_mq28_q13(Aw, Bw);
    }

    /*     for (int i = 0; i < 7; i++)
        {
            printf("Aw[%d]=%x,Bw[%d]=%x,w[%d]=%x\n", i, A[i], i, B[i], i, w[i]);
        } */
    // Interpolation
    uint32_t r[6];
    r[1] = (w[1] + w[4]) & 0x7FFFFFF;
    r[5] = (w[5] - w[4]) & 0x7FFFFFF;
    r[3] = ((w[3] - w[2]) >> 1) & 0x7FFFFFF;
    r[4] = (w[4] - w[0]) & 0x7FFFFFF;

    r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x7FFFFFF;
    r[2] = (w[2] + r[3]) & 0x7FFFFFF;
    r[1] = (r[1] - (r[2] << 6) - r[2]) & 0x7FFFFFF;

    r[2] = (r[2] - w[6] - w[0]) & 0x7FFFFFF;

    r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFF;
    r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) >> 3) & 0x7FFFFFF) * 0xAAAAAAB) & 0x7FFFFFF;

    r[5] = (r[5] + r[1]) & 0x7FFFFFF;
    r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) >> 1) & 0x7FFFFFF) * 0x8E38E39) & 0x7FFFFFF;
    r[2] = (r[2] - r[4]) & 0x7FFFFFF;

    r[3] = (-r[3] - r[1]) & 0x7FFFFFF;
    r[5] = ((r[1] - (((uint32_t)(r[5] >> 1) * 0xEEEEEEF) & 0x7FFFFFF)) >> 1) & 0x7FFFFFF;

    r[1] = r[1] - r[5];

    c[6] = w[6];
    c[5] = r[5];
    c[4] = r[4];
    c[3] = r[3];
    c[2] = r[2];
    c[1] = r[1];
    c[0] = w[0];

    for (int j = 0; j < 7; j++)
    {
        c[j] &= 0xFFFFFF;
    }
}