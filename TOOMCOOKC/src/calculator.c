#include "poly.h"

uint32_t mul_signed_mq28_q13(uint32_t a, uint32_t b)
{
  uint32_t a_sign = a & 0x8000000;
  uint32_t b_sign = b & 0x1000;
  int32_t a_signed = a | (-a_sign); // 28bit
  int32_t b_signed = b | (-b_sign); // 13bit
  int32_t c = a_signed * b_signed;
  return c & 0x7FFFFFF;
}

/* uint32_t mul_signed_mq28_q17(uint32_t a, uint32_t b)
{
  uint32_t a_sign = a & 0x8000000;
  uint32_t b_sign = b & 0x10000;
  int32_t a_signed = (int32_t)(a | (-a_sign));
  int32_t b_signed = (int32_t)(b | (-b_sign));
  int32_t c = a_signed * b_signed;
  return c & 0x7FFFFFF;
} */

uint32_t mul_signed_mq31_q17(uint32_t a, uint32_t b)
{
  uint32_t a_sign = a & 0x40000000;
  uint32_t b_sign = b & 0x10000;
  int32_t a_signed = (int32_t)(a | (-a_sign));
  int32_t b_signed = (int32_t)(b | (-b_sign));
  int32_t c = a_signed * b_signed;
  return c & 0x3FFFFFFF;
}
