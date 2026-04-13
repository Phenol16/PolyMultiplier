#include "poly.h"

void toomcook416(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    // evaluation
    uint32_t aws[7][4], bws[7][4];

    uint32_t w[7][4];

    // mod q = 2 ^ (24 + 3 + 3) due to division by 8 in interpolation
    for (int j = 0; j < 4; j++)
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

    // mod q = 2 ^ (8 + 4 + 4) due to integer carry addition
    for (int j = 0; j < 4; j++)
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
    /*     for (int i = 0; i < 7; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                printf("a[%d][%d]=%x b[%d][%d]=%x\n", i, j, aws[i][j], i, j, bws[i][j]);
            }
            printf("\n");
        } */
    for (int i = 0; i < 7; i++)
    {
        toomcook44(aws[i], bws[i], w[i]);
    }
/*     for (int i = 0; i < 7; i++)
    {
        printf("Point %d, a = [%x,%x,%x,%x],b = [%x,%x,%x,%x],w = [%x,%x,%x,%x]\n", i,
               aws[i][0], aws[i][1], aws[i][2], aws[i][3],
               bws[i][0], bws[i][1], bws[i][2], bws[i][3],
               w[i][0], w[i][1], w[i][2], w[i][3]);
    } */
    // interpolation
    uint32_t r[10] = {0};
    for (int i = 0; i < 4; i++)
    {
        r[7] = r[0];
        r[8] = r[1];
        r[9] = r[2];
        r[0] = w[0][i];
        r[1] = w[1][i];
        r[2] = w[2][i];
        r[3] = w[3][i];
        r[4] = w[4][i];
        r[5] = w[5][i];
        r[6] = w[6][i];

        r[1] = (r[1] + r[4]) & 0x7FFFFFF;
        r[5] = (r[5] - r[4]) & 0x7FFFFFF;
        r[3] = ((r[3] - r[2]) >> 1) & 0x7FFFFFF;

        r[4] = (((r[4] - r[0]) << 1) + r[5] - (r[6] << 7)) & 0x7FFFFFF;
        r[2] = (r[2] + r[3]) & 0x7FFFFFF;
        r[1] = (r[1] - (r[2] << 6) - r[2]) & 0x7FFFFFF;

        r[2] = (r[2] - r[6] - r[0]) & 0x7FFFFFF;
        r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFF;

        r[4] = ((uint32_t)(((r[4] - (r[2] << 3)) >> 3) & 0x7FFFFFF) * 0xAAAAAAB) & 0x7FFFFFF;
        r[5] = (r[5] + r[1]) & 0x7FFFFFF;

        r[1] = ((uint32_t)(((r[1] + (r[3] << 4)) >> 1) & 0x7FFFFFF) * 0x8E38E39) & 0x7FFFFFF;
        r[3] = (-r[3] - r[1]) & 0x7FFFFFF;

        r[5] = ((r[1] - (((uint32_t)(r[5] >> 1) * 0xEEEEEEF) & 0x7FFFFFF)) >> 1) & 0x7FFFFFF;
        r[2] = (r[2] - r[4]) & 0x7FFFFFF;

        r[1] = (r[1] - r[5]) & 0x7FFFFFF;

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

    c[0] -= r[2];
    c[1] -= r[1];
    c[2] -= r[0];

    for (int i = 0; i < 16; i++)
    {
        c[i] &= 0xFFFFFF;
    }

}
