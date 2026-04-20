#include "poly.h"

#define N 1024

/* void toomcook41024(const uint32_t *a, const uint32_t *b, uint32_t *c)
{
    uint32_t acoeff[4][4][4][16], bcoeff[4][4][4][16];
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
    }
         uint32_t *p = (uint32_t *)acoeff;
        for (int n = 0; n < 1024; n++)
        {
            printf("mem[%d] = %d\n", n, p[n]);
        }
}
 */