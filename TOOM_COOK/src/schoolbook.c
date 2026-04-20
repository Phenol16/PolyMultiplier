#include "poly.h"

void schoolbook(const uint32_t *a, const uint32_t *b, uint32_t *c, int n)
{
    for (int i = 0; i < n; i++)
    {
        c[i] = 0;
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = i; j < n; j++)
        {
            c[j] += a[i] * b[j - i];
        }
        for (int j = 0; j < i; j++)
        {
            c[j] -= a[i] * b[n + j - i];
        }
    }
    // mod q = 2^24
    for (int i = 0; i < n; i++)
    {
        c[i] &= 0xFFFFFF;
    }
    /*     printf("schoolbook\n");
        for (int i = 0; i < 16; i++)
        {
            printf("c[%d] = %x\n", i, c[i]);
        } */
}
