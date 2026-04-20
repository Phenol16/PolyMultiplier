#include "poly.h"

#define N 1024
#define mask24 0xFFFFFF
#define mask8 0xFF

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
    uint32_t c[4], d[7];
    /*     for (int i = 0; i < 4; i++)
        {
            a[i] = 2 * i + 1;
            b[i] = 200*(i+1);
        } */
    uint32_t a[4] = {0x3E9CB03C, 0x4448C4, 0x20E32E7F, 0x185DF036};
    uint32_t b[4] = {0x9B07, 0x783F, 0xC581, 0xAED9};
    for (int i = 0; i < 4; ++i)
    {
        a[i] &= 0x3FFFFFFF;
        b[i] &= 0xFFFF;
    }
    schoolbook(a, b, c, 4);
    // toomcook44_unmodulo(a, b, d);
    toomcook44(a, b, d);
    for (int i = 0; i < 4; i++)
    {
        printf("c[%d] = %x d[%d] = %x\n", i, c[i], i, d[i]);
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
        a[i] = 2 * i + 1;
        b[i] = 10 * (i + 1);
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
    /*     uint32_t a[16], b[16], c[16], d[16];

        srand(time(NULL));
        for (int n = 0; n < 100; n++)
        {

            for (int i = 0; i < 16; i++)
            {
                a[i] = rand();
                b[i] = rand();
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
            printf("n=%d,i=%d,c[%d] = %x,d[%d] = %x\n", n, i, i, c[i], i, d[i]);
            return;
        }
    }
}
printf("All random test pass\n");*/
}
void test_toomcook464()
{
    uint32_t a[N], b[N], c[N], d[N];
    for (int n = 0; n < 100; n++)
    {
        srand(time(NULL));
        for (int i = 0; i < N; i++)
        {
            a[i] = rand() & mask24;
            b[i] = rand() & mask8;
        }
        schoolbook(a, b, c, N);
        toomcook464(a, b, d);
        for (int i = 0; i < N; i++)
        {
            if (c[i] != d[i])
            {
                printf("i=%d,c[%d] = %x,d[%d] = %x\n", i, i, c[i], i, d[i]);
            }
        }
    }
}
void test_toomcook4()
{
    uint32_t a[N], b[N], c[N];
    uint64_t a64[N], b64[N], d[N];
    for (int n = 0; n < 100; n++)
    {
        srand(time(NULL));
        for (int i = 0; i < N; i++)
        {
            a[i] = rand() & mask24;
            b[i] = rand() & mask8;
            a64[i] = a[i];
            b64[i] = b[i];
        }
        schoolbook(a, b, c, N);
        toomcook4(a64, b64, d, N);
        for (int i = 0; i < N; i++)
        {
            if (c[i] != (uint32_t)d[i])
            {
                printf("n=%d,i=%d,c[%d] = %x,d[%d] = %llx\n", n, i, i, c[i], i, (unsigned long long)d[i]);
                if (i == 63)
                {
                    printf("fail!");
                    return;
                }
            }
        }
    }
    printf("pass!");
}
