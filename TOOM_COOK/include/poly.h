#ifndef POLY_H
#define POLY_H
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void schoolbook(const uint32_t *a, const uint32_t *b, uint32_t *c, int n);

void toomcook44(const uint32_t *a, const uint32_t *b, uint32_t *c);
void test_toomcook44();
void toomcook44_unmodulo(const uint32_t *a, const uint32_t *b, uint32_t *c);
void test_toomcook44_unmodulo();

void toomcook416(const uint32_t *a, const uint32_t *b, uint32_t *c);
void test_toomcook416();

void kernel(uint32_t *a, uint32_t *b, uint32_t *c);
void eval(const uint32_t *a, uint32_t *aws, int j, int N);
void interp(uint32_t *w, uint32_t *c, uint32_t *r, int i, int N);
void dot_product(uint32_t *a, uint32_t *b, uint32_t *c);
uint64_t mul_signed_mq33_q21(uint64_t a, uint32_t b);

void toomcook464(const uint32_t *a, const uint32_t *b, uint32_t *c);
void test_toomcook464();

void toomcook4(const uint32_t *a, const uint32_t *b, uint32_t *c, int N);
void test_toomcook4();
void evaluation(const uint32_t *a, uint32_t *aws, int j, int N);
void product(uint32_t *a, uint32_t *b, uint64_t *c);
uint64_t mul_signed_mq39_q29(uint64_t a, uint32_t b);
void interpolation(uint64_t *w, uint64_t *c, uint64_t *r, int i, int N);

uint32_t mul_signed_mq28_q13(uint32_t a, uint32_t b);
// uint32_t mul_signed_mq28_q17(uint32_t a, uint32_t b);
uint32_t mul_signed_mq31_q17(uint32_t a, uint32_t b);
#endif