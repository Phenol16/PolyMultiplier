/* #include "poly.h"
void core(const uint64_t *a, const uint64_t *b, uint64_t *c)
void toomcook4pipe(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    uint64_t acoeff[1024], bcoeff[1024];
    for (int i = 0; i < 4; i++)
{
    for (int j = 0; j < 4; j++)
    {
        for (int k = 0; k < 4; k++)
        {
            for (int l = 0; l < 16; l++)
            {
                acoeff[256*i + 64*j + 16*k + l] = a[4 * (4 * (4 * l + k) + j) + i]; // acoeff[256*i + 64*j + 16*k + l]=a[64*l + 16*k + 4*j + i]
                bcoeff[256*i + 64*j + 16*k + l] = b[4 * (4 * (4 * l + k) + j) + i];
            }
        }
    }
}
for(int cycle=0;i<64;i++)
{
    uint64_t aws[7 * 16], bws[7 * 16];
    for (int j = 0; j < 16; j++)
    {
        evaluation(acoeff, aws, j, 16);
        evaluation(bcoeff, bws, j, 16);
    }
    for (int i = 0; i < 7; i++)
    {
        product(&aws[i * 4], &bws[i * 4], &w[i * 4]);
    }



} */