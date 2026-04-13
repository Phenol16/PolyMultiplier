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

void toomcook464(const uint32_t *a, const uint32_t *b, uint32_t *c);
void test_toomcook464();

uint32_t mul_signed_mq28_q13(uint32_t a, uint32_t b);
// uint32_t mul_signed_mq28_q17(uint32_t a, uint32_t b);
uint32_t mul_signed_mq31_q17(uint32_t a, uint32_t b);
#endif