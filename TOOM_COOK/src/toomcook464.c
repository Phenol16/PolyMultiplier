#include "poly.h"

#define mask24 &0xFFFFFF

void toomcook464(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    enum
    {
        BLOCKS = 16,
        POINTS = 7
    };

    uint32_t aws[7 * 16], bws[7 * 16];
    uint32_t w[7 * 16] = {0};

    for (int j = 0; j < BLOCKS; j++)
    {
        eval(a, aws, j, BLOCKS);
        eval(b, bws, j, BLOCKS);
    }

    for (int i = 0; i < POINTS; i++)
    {
        // 24+3，8+4
        kernel(&aws[i * BLOCKS], &bws[i * BLOCKS], &w[i * BLOCKS]);
    }

    uint32_t r[10] = {0};
    for (int i = 0; i < BLOCKS; i++)
    {
        interp(w, c, r, i, BLOCKS);
    }

    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];
    for (int i = 0; i < 64; i++)
    {
        c[i] = c[i] mask24;
    }
}
