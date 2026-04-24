+ 16阶多项式乘法及后续的多项式乘法
+ 4阶chisel（从简单开始），包含模块和激励

+ 规范chisel代码：tabulate test 
+ C语言改为24+3+3

+ interp 展开 
+ 乘法并行 main.c
+ reg插入
  
+ 1024 C算法
+ 参数化实现
  
+ a:24+3+3+3+3+3=39
+ b:8+4+4+4+4+4+1=29

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