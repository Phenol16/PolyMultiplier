#include "poly.h"

#define mask24 &0xFFFFFF

void toomcook464(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    uint32_t aws[7 * 16], bws[7 * 16];
    uint32_t w[7 * 16];

    for (int j = 0; j < 16; j++)
    {
        eval(a, aws, j, 4);
        eval(b, bws, j, 4);
    }

    for (int i = 0; i < 7; i++)
    {
        for (int j = 0; j < 16; j++)
        {
            printf("aws[%d][%d]=%d,bws[%d][%d]=%d\n", i, j, aws[7 * i + j], i, j, bws[7 * i + j]);
        }
    }

    for (int i = 0; i < 7; i++)
    {
        // 24+3，8+4
        kernel(&aws[i * 16], &bws[i * 16], &w[i * 16]);
    }
    uint32_t r[10] = {0};
    for (int i = 0; i < 16; i++)
    {
        interp(w, c, r, i, 16);
    }
    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];
    for (int i = 0; i < 64; i++)
    {
        c[i] = c[i] mask24;
    }
}