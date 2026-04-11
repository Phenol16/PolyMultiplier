#include "poly.h"

void toomcook464(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    // evaluation
    uint32_t aws[7][16], bws[7][16];
    uint32_t w[7][16];

    // mod q = 2 ^ (24 + 3 + 3 + 3) due to division by 8 in interpolation
    for (int j = 0; j < 16; j++)
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

        aws[2][j] = r[6];
        aws[3][j] = r[7];

        r[4] = (((r[0] << 2) + r[2]) << 1);
        r[5] = (r[1] << 2) + r[3];
        r[6] = r[4] + r[5];
        r[7] = r[4] - r[5];

        aws[4][j] = r[6];
        aws[5][j] = r[7];

        r[4] = (r[3] << 3) + (r[2] << 2) + (r[1] << 1) + r[0];

        aws[1][j] = r[4];
        aws[6][j] = r[0];
        aws[0][j] = r[3];
    }
    // mod q = 2 ^ (8 + 4 + 4 + 4) due to integer carry addition
    for (int j = 0; j < 16; j++)
    {
        uint32_t r[8];
        r[0] = b[j * 4];
        r[1] = b[1 + j * 4];
        r[2] = b[2 + j * 4];
        r[3] = b[3 + j * 4];

        r[4] = r[0] + r[2];
        r[5] = r[1] + r[3];
        r[6] = r[4] + r[5];
        r[7] = r[4] - r[5];

        bws[2][j] = r[6];
        bws[3][j] = r[7];

        r[4] = (((r[0] << 2) + r[2]) << 1);
        r[5] = (r[1] << 2) + r[3];
        r[6] = r[4] + r[5];
        r[7] = r[4] - r[5];

        bws[4][j] = r[6];
        bws[5][j] = r[7];

        r[4] = (r[3] << 3) + (r[2] << 2) + (r[1] << 1) + r[0];

        bws[1][j] = r[4];
        bws[6][j] = r[0];
        bws[0][j] = r[3];
    }

    for (int i = 0; i < 7; i++)
    {
        toomcook416(aws[i], bws[i], w[i]);
    }
    /*     for (int i = 0; i < 7; i++)
        {
            for (int j = 0; j < 16; j++)
            {
                printf("w[%d][%d]=%d\t", i, j, w[i][j]);
            }
            printf("\n");
        } */

    // interpolation

}