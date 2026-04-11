#include "poly.h"

void test_toomcook44()
{
    uint32_t a[4], b[4], c[4], d[4];
    for (int i = 0; i < 4; i++)
    {
        a[i] = i;
        b[i] = 1;
    }
    for (int i = 0; i < 4; ++i)
    {
        a[i] &= 0xFFFFFF;
        b[i] &= 0xFF;
    }
    schoolbook(a, b, c, 4);
    toomcook44(a, b, d);
    for (int i = 0; i < 4; i++)
    {
        printf("c[%d] = %d\n", i, c[i]);
    }
    for (int i = 0; i < 4; i++)
    {
        printf("d[%d] = %d\n", i, d[i]);
    }
}
void test_toomcook44_unmodulo()
{
    uint32_t a[4], b[4], c[4], d[7];
    for (int i = 0; i < 4; i++)
    {
        a[i] = 2 * i + 1;
        b[i] = 10 * i;
    }
    for (int i = 0; i < 4; ++i)
    {
        a[i] &= 0xFFFFFF;
        b[i] &= 0xFF;
    }
    schoolbook(a, b, c, 4);
    toomcook44_unmodulo(a, b, d);
    for (int i = 0; i < 4; i++)
    {
        printf("c[%d] = %x\n", i, c[i]);
    } /*
     for (int i = 0; i < 7; i++)
     {
         printf("d[%d] = %d\n", i, d[i]);
     } */
}
void test_toomcook416()
{
    /*     uint32_t c[16], d[16];
        uint32_t a[16] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3};
        uint32_t b[16] = {2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9, 0, 4, 5};
        for (int i = 0; i < 16; ++i)
        {
            a[i] &= 0xFFFFFF;
            b[i] &= 0xFF;
        } */
    uint32_t a[16], b[16], c[16], d[16];
    for (int i = 0; i < 16; i++)
    {
        a[i] = 1;
        b[i] = i * 200;
    }
    for (int i = 0; i < 16; ++i)
    {
        a[i] &= 0xFFFFFF;
        b[i] &= 0xFF;
    }
    schoolbook(a, b, c, 16);
    toomcook416(a, b, d);
    for (int i = 0; i < 16; i++)
    {
        if (c[i] != d[i])
        {
            printf("i=%d,c[%d] = %x,d[%d] = %x\n", i, i, c[i], i, d[i]);
        }
    }
}
void test_toomcook464()
{
    uint32_t a[64], b[64], c[64], d[64];
    for (int i = 0; i < 64; i++)
    {
        a[i] = 1;
        b[i] = i;
    }
    for (int i = 0; i < 64; ++i)
    {
        a[i] &= 0xFFFFFF;
        b[i] &= 0xFF;
    }
    schoolbook(a, b, c, 64);
    toomcook464(a, b, d);
/*     for (int i = 0; i < 64; i++)
    {
        if (c[i] != d[i])
        {
            printf("i=%d,c[%d] = %x,d[%d] = %x\n", i, i, c[i], i, d[i]);
        }
    } */
}