#include "poly.h"
void top_module(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    uint64_t acoeff[4][4][4][16], bcoeff[4][4][4][16];
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
    // evaluation 模块：完成划分和估值，先划分成4x4x4x16的块，然后估值，输入16个数，返回 4x7 = 28 个数
    // 将估值得到的两组 7x7x7x4x4x4 = 28672 个16阶多项式逐批输入point
    // point 模块：输入两个16阶多项式，返回两个16阶多项式的环面乘
    // interpolation 模块：

}