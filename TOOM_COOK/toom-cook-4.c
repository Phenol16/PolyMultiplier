#include <mosfhet.h>

// #define DEBUG

int power7(int val)
{
  int mul = 1;
  for (int lvl = 0; lvl < val; ++lvl)
  {
    mul *= 7;
  }
  return mul;
}

/* out = in1*in2 */
void polynomial_naive_mod_mul_torus(TorusPolynomial out, TorusPolynomial in1, TorusPolynomial in2)
{
  const int N = in2->N;
  for (size_t j = 0; j < N; j++)
  {
    out->coeffs[j] = in1->coeffs[j] * in2->coeffs[0];
  }
  for (size_t i = 1; i < N; i++)
  {
    for (size_t j = i; j < N; j++)
    {
      out->coeffs[j] += in1->coeffs[j - i] * in2->coeffs[i];
    }
    for (size_t j = 0; j < i; j++)
    {
      out->coeffs[j] -= in1->coeffs[N + j - i] * in2->coeffs[i];
    }
  }
  // mod q = 2^24
  for (size_t j = 0; j < N; j++)
  {
    out->coeffs[j] &= 0xFFFFFF;
  }
}

uint32_t srl_mq(uint32_t in, uint32_t imm, uint32_t qbit)
{
  uint32_t mask = (1 << qbit) - 1;
  uint32_t high_bit_LSB = (in >> (qbit - 1)) & 1;
  uint32_t high_bit_MSB = high_bit_LSB << (qbit - imm - 1);
  uint32_t sign_ext = (-high_bit_MSB) & mask;
  uint32_t res = (in >> imm) | sign_ext;
  return res;
}

uint32_t srl_mq28(uint32_t in, uint32_t imm)
{
  uint32_t mask = (1 << 28) - 1;
  uint32_t high_bit_LSB = (in >> 27) & 1;
  uint32_t high_bit_MSB = high_bit_LSB << (27 - imm);
  uint32_t sign_ext = (-high_bit_MSB) & mask;
  uint32_t res = (in >> imm) | sign_ext;
  return res;
}

uint32_t mul_signed_mq28_q13(uint32_t a, uint32_t b)
{
  uint32_t a_sign = a & 0x8000000;
  uint32_t b_sign = b & 0x1000;
  int32_t a_signed = a | (-a_sign); // 28bit
  int32_t b_signed = b | (-b_sign); // 12bit
  int32_t c = a_signed * b_signed;
  return c & 0x7FFFFFF;
}

void toom_cook_mul(TorusPolynomial out, TorusPolynomial in1, TorusPolynomial in2, int parallel)
{
  assert(out->N == (1 << (parallel << 1)));
  int mul = power7(parallel);
  uint32_t aws1[mul], aws2[mul];
  /*
    Torus r1[out->N<<1];
    r1[0] = in1->coeffs[0];
    r1[1] = in1->coeffs[1];
    r1[2] = in1->coeffs[2];
    r1[3] = in1->coeffs[3];
    r1[4] = r1[0] + r1[2];
    r1[5] = r1[1] + r1[3];
    r1[6] = r1[4] + r1[5];
    r1[7] = r1[4] - r1[5];
    aws1[2] = r1[6];
    aws1[3] = r1[7];
    r1[4] = ((r1[0] << 2) + r1[2]) << 1;
    r1[5] = (r1[1] << 2) + r1[3];
    r1[6] = r1[4] + r1[5];
    r1[7] = r1[4] - r1[5];
    aws1[4] = r1[6];
    aws1[5] = r1[7];
    r1[4] = (r1[3] << 3) + (r1[2] << 2) + (r1[1] << 1) + r1[0];
    aws1[1] = r1[4];
    aws1[6] = r1[0];
    aws1[0] = r1[3];
  */
  // mod q = 2 ^ (24 + 3) due to division by 8 in interpolation
  aws1[0] = in1->coeffs[3];
  aws1[1] = in1->coeffs[0] + (in1->coeffs[1] << 1) + (in1->coeffs[2] << 2) + (in1->coeffs[3] << 3);
  aws1[2] = in1->coeffs[0] + in1->coeffs[1] + in1->coeffs[2] + in1->coeffs[3];
  aws1[3] = in1->coeffs[0] - in1->coeffs[1] + in1->coeffs[2] - in1->coeffs[3];
  aws1[4] = (in1->coeffs[0] << 3) + (in1->coeffs[1] << 2) + (in1->coeffs[2] << 1) + in1->coeffs[3];
  aws1[5] = (in1->coeffs[0] << 3) - (in1->coeffs[1] << 2) + (in1->coeffs[2] << 1) - in1->coeffs[3];
  aws1[6] = in1->coeffs[0];

  // mod q = 2 ^ (8 + 4) due to integer carry addition
  aws2[0] = in2->coeffs[3];
  aws2[1] = in2->coeffs[0] + (in2->coeffs[1] << 1) + (in2->coeffs[2] << 2) + (in2->coeffs[3] << 3);
  aws2[2] = in2->coeffs[0] + in2->coeffs[1] + in2->coeffs[2] + in2->coeffs[3];
  aws2[3] = in2->coeffs[0] - in2->coeffs[1] + in2->coeffs[2] - in2->coeffs[3];
  aws2[4] = (in2->coeffs[0] << 3) + (in2->coeffs[1] << 2) + (in2->coeffs[2] << 1) + in2->coeffs[3];
  aws2[5] = (in2->coeffs[0] << 3) - (in2->coeffs[1] << 2) + (in2->coeffs[2] << 1) - in2->coeffs[3];
  aws2[6] = in2->coeffs[0];

  // mod q = 2 ^ (24 + 3) due to division by 8 in interpolation
  // signed multiplications differ form unsigned multiplications
  int32_t w[mul];
  for (int i = 0; i < mul; i++)
  {
    w[i] = (int32_t)aws1[i] * (int32_t)aws2[i];
  }

  /*
    int32_t r[(out->N<<1) - 1];
    r[0] = w[6];
    r[1] = (-  90 * w[0] + 2 * w[1] -  60 * w[2] + 20 * w[3] + 5 * w[4] - 3 * w[5] -  90 * w[6]) / 180;
    r[2] = (    6 * w[0] + 0 * w[1] -   4 * w[2] -  4 * w[3] + 1 * w[4] + 1 * w[5] - 120 * w[6]) / 24;
    r[3] = (   45 * w[0] - 1 * w[1] +  27 * w[2] -  7 * w[3] - 1 * w[4] + 0 * w[5] +  45 * w[6]) / 18;
    r[4] = (-  30 * w[0] + 0 * w[1] +  16 * w[2] + 16 * w[3] - 1 * w[4] - 1 * w[5] +  96 * w[6]) / 24;
    r[5] = (- 360 * w[0] + 8 * w[1] - 120 * w[2] - 40 * w[3] + 5 * w[4] + 3 * w[5] - 360 * w[6]) / 180;
    r[6] = w[0];
    out->coeffs[0] = r[0] - r[4];
    out->coeffs[1] = r[1] - r[5];
    out->coeffs[2] = r[2] - w[6];
    out->coeffs[3] = r[3];
  */

  /*
    //  3^(-1) mod 2^32 = 0xAAAAAAAB     3^(-1) mod 2^27 = 0x2AAAAAB
    //  9^(-1) mod 2^32 = 0x38E38E39     9^(-1) mod 2^27 = 0x0E38E39
    // 15^(-1) mod 2^32 = 0xEEEEEEEF    15^(-1) mod 2^27 = 0x6EEEEEF
    // 30 +/- -> 19 +/-
    int32_t r[out->N<<1];
    r[1] = w[1] + w[4];
    r[5] = w[5] - w[4];
    r[3] = (w[3] - w[2]) / 2;
    r[4] = w[4] - w[0];

    r[7] = w[6] * 128;

    r[4] = r[4] * 2 + r[5];
    r[4] = r[4] - r[7];
    r[2] = w[2] + r[3];
    r[1] = r[1] - r[2] * 65;
    r[2] = r[2] - w[6];
    r[2] = r[2] - w[0];
    r[1] = r[1] + r[2] * 45;
    r[4] = (r[4] - r[2] * 8) / 24;
    r[5] = r[5] + r[1];
    r[1] = (r[1] + r[3] * 16) / 18;
    r[3] = - r[3] - r[1];
    r[5] = (r[1] - r[5] / 30) / 2;
    r[2] = r[2] - r[4];
    r[1] = r[1] - r[5];
    out->coeffs[0] = w[6] - r[2];
    out->coeffs[1] = r[5] - r[1];
    out->coeffs[2] = r[4] - w[0];
    out->coeffs[3] = r[3];
  */

  /*进位关系
    int32_t r[out->N<<1];
    r[1] = w[1] + w[4];                     r[5] = w[5] - w[4];                 r[3] = (w[3] - w[2]) / 2;     r[4] = w[4] - w[0];
    r[4] = r[4] * 2 + r[5] - w[6] * 128;    r[2] = w[2] + r[3];                 r[1] = r[1] - r[2] * 65;
    r[2] = r[2] - w[6] - w[0];
    r[1] = r[1] + r[2] * 45;                r[4] = (r[4] - r[2] * 8) / 24;
    r[5] = r[5] + r[1];                     r[1] = (r[1] + r[3] * 16) / 18;     r[2] = r[2] - r[4];
    r[3] = - r[3] - r[1];                   r[5] = (r[1] - r[5] / 30) / 2;
    r[1] = r[1] - r[5];
    out->coeffs[0] = w[6] - r[2];
    out->coeffs[1] = r[5] - r[1];
    out->coeffs[2] = r[4] - w[0];
    out->coeffs[3] = r[3];
  */

  // signed shift & unsigned division
  int32_t r[out->N << 1];
  r[1] = w[1] + w[4];
  r[5] = w[5] - w[4];
  r[3] = (w[3] - w[2]) >> 1;
  r[4] = w[4] - w[0];
  r[4] = (r[4] << 1) + r[5] - (w[6] << 7);
  r[2] = w[2] + r[3];
  r[1] = r[1] - (r[2] << 6) - r[2];
  r[2] = r[2] - w[6] - w[0];
  r[1] = r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5);
  r[4] = (uint32_t)((r[4] - (r[2] << 3)) >> 3) * 0xAAAAAAAB;
  r[5] = r[5] + r[1];
  r[1] = (uint32_t)((r[1] + (r[3] << 4)) >> 1) * 0x38E38E39;
  r[2] = r[2] - r[4];
  r[3] = -r[3] - r[1];
  r[5] = (r[1] - (uint32_t)(r[5] >> 1) * 0xEEEEEEEF) >> 1;
  r[1] = r[1] - r[5];
  out->coeffs[0] = w[6] - r[2];
  out->coeffs[1] = r[5] - r[1];
  out->coeffs[2] = r[4] - w[0];
  out->coeffs[3] = r[3];

  for (size_t j = 0; j < 4; j++)
  {
    out->coeffs[j] &= 0xFFFFFF;
  }
}

void toom_cook_mul_kernel(TorusPolynomial out, TorusPolynomial in1, TorusPolynomial in2, int parallel)
{
  assert(out->N == (1 << (parallel << 1)));
  int mul = power7(parallel);
  uint32_t aws1[mul], aws2[mul];

  // mod q = 2 ^ (24 + 3) due to division by 8 in interpolation
  aws1[0] = in1->coeffs[3];
  aws1[1] = in1->coeffs[0] + (in1->coeffs[1] << 1) + (in1->coeffs[2] << 2) + (in1->coeffs[3] << 3);
  aws1[2] = in1->coeffs[0] + in1->coeffs[1] + in1->coeffs[2] + in1->coeffs[3];
  aws1[3] = in1->coeffs[0] - in1->coeffs[1] + in1->coeffs[2] - in1->coeffs[3];
  aws1[4] = (in1->coeffs[0] << 3) + (in1->coeffs[1] << 2) + (in1->coeffs[2] << 1) + in1->coeffs[3];
  aws1[5] = (in1->coeffs[0] << 3) - (in1->coeffs[1] << 2) + (in1->coeffs[2] << 1) - in1->coeffs[3];
  aws1[6] = in1->coeffs[0];

  // mod q = 2 ^ (8 + 4) due to integer carry addition
  aws2[0] = in2->coeffs[3];
  aws2[1] = in2->coeffs[0] + (in2->coeffs[1] << 1) + (in2->coeffs[2] << 2) + (in2->coeffs[3] << 3);
  aws2[2] = in2->coeffs[0] + in2->coeffs[1] + in2->coeffs[2] + in2->coeffs[3];
  aws2[3] = in2->coeffs[0] - in2->coeffs[1] + in2->coeffs[2] - in2->coeffs[3];
  aws2[4] = (in2->coeffs[0] << 3) + (in2->coeffs[1] << 2) + (in2->coeffs[2] << 1) + in2->coeffs[3];
  aws2[5] = (in2->coeffs[0] << 3) - (in2->coeffs[1] << 2) + (in2->coeffs[2] << 1) - in2->coeffs[3];
  aws2[6] = in2->coeffs[0];

  uint32_t w[mul];
  for (int i = 0; i < mul; i++)
  {
    // 24 + 3 + 1 signed
    uint32_t aw1 = (aws1[i] & 0x7FFFFFF) | ((aws1[i] >> 4) & 0x8000000);
    // 8 + 4 + 1 signed
    uint32_t aw2 = (aws2[i] & 0xFFF) | ((aws2[i] >> 19) & 0x1000);
    w[i] = mul_signed_mq28_q13(aw1, aw2);

    // uint32_t c = mul_signed_mq28_q12(aws1[i], aws2[i]);
    // uint32_t c_sign = c & 0x8000000;
    // w[i] = c | c_sign;
  }

  uint32_t r[out->N << 1];
  r[1] = (w[1] + w[4]) & 0x7FFFFFF;
  r[5] = (w[5] - w[4]) & 0x7FFFFFF;
  r[3] = srl_mq28((w[3] - w[2]) & 0x7FFFFFF, 1);
  r[4] = (w[4] - w[0]) & 0x7FFFFFF;

  r[4] = ((r[4] << 1) + r[5] - (w[6] << 7)) & 0x7FFFFFF;
  r[2] = (w[2] + r[3]) & 0x7FFFFFF;
  r[1] = (r[1] - (r[2] << 6) - r[2]) & 0x7FFFFFF;

  r[2] = (r[2] - w[6] - w[0]) & 0x7FFFFFF;

  r[1] = (r[1] + r[2] + (r[2] << 2) + (r[2] << 3) + (r[2] << 5)) & 0x7FFFFFF;
  r[4] = ((uint32_t)srl_mq28((r[4] - (r[2] << 3)) & 0x7FFFFFF, 3) * 0xAAAAAAB) & 0x7FFFFFF;

  r[5] = (r[5] + r[1]) & 0x7FFFFFF;
  r[1] = ((uint32_t)srl_mq28((r[1] + (r[3] << 4)) & 0x7FFFFFF, 1) * 0x8E38E39) & 0x7FFFFFF;
  r[2] = (r[2] - r[4]) & 0x7FFFFFF;

  r[3] = (-r[3] - r[1]) & 0x7FFFFFF;
  r[5] = srl_mq28((r[1] - (((uint32_t)srl_mq28(r[5], 1) * 0xEEEEEEF) & 0x7FFFFFF)) & 0x7FFFFFF, 1);

  r[1] = r[1] - r[5];
  out->coeffs[0] = (w[6] - r[2]);
  out->coeffs[1] = (r[5] - r[1]);
  out->coeffs[2] = (r[4] - w[0]);
  out->coeffs[3] = r[3];

  for (size_t j = 0; j < 4; j++)
  {
    out->coeffs[j] &= 0xFFFFFF;
  }
}

void test_toom_cook_mul_kernel()
{
  const int N = 4, parallel = 1;
  TorusPolynomial poly_a_torus = polynomial_new_torus_polynomial(N);
  TorusPolynomial poly_b_torus = polynomial_new_torus_polynomial(N);
  TorusPolynomial poly_c_torus = polynomial_new_torus_polynomial(N);
  TorusPolynomial poly_d_torus = polynomial_new_torus_polynomial(N);

  // for (int i = 0; i < N; ++i) {
  //   poly_a_torus->coeffs[i] = i+1;
  //   poly_b_torus->coeffs[i] = i+1;
  // }
  generate_random_bytes(sizeof(Torus) * N, (uint8_t *)poly_a_torus->coeffs);
  generate_random_bytes(sizeof(Torus) * N, (uint8_t *)poly_b_torus->coeffs);
  for (int i = 0; i < N; ++i)
  {
    poly_a_torus->coeffs[i] &= 0xFFFFFF;
    poly_b_torus->coeffs[i] &= 0xFF;
  }

  toom_cook_mul_kernel(poly_c_torus, poly_a_torus, poly_b_torus, parallel);
  polynomial_naive_mod_mul_torus(poly_d_torus, poly_a_torus, poly_b_torus);

  Torus Delta = 0;
  for (int i = 0; i < 10 && i < N; ++i)
  {
    Torus temp;
    temp = poly_c_torus->coeffs[i] - poly_d_torus->coeffs[i];
    if (temp & 0x80000000)
      temp = 0 - temp;
    if (temp > Delta)
      Delta = temp;
  }
  printf("Poly.Mult Delta 0x%x \n", Delta);

  for (int i = 0; i < N; ++i)
  {
    printf("poly_c_torus[%d] = 0x%x \n", i, poly_c_torus->coeffs[i]);
    printf("poly_d_torus[%d] = 0x%x \n", i, poly_d_torus->coeffs[i]);
  }

  free_polynomial(poly_a_torus);
  free_polynomial(poly_b_torus);
  free_polynomial(poly_c_torus);
  free_polynomial(poly_d_torus);
}

int main(int argc, char const *argv[])
{
  test_toom_cook_mul_kernel();

  return 0;
}