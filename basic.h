#ifndef JLIB_BASIC_H
#define JLIB_BASIC_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#define SQUARE(x) (x*x)
#define TIMES2(x) (x+x)
#define HALF(x) (x*0.5f)
#define IS_POW_2(x) ((x & (x-1)) == 0)
#define SIGN_EXTEND_S64(x, n) (s64)((n >= 64) ? (s64)x : (s64)((s64)x | (s64)(-((s64)x >> ((s64)n - 1lu)) << (s64)n)))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define STATICARRLEN(x) (sizeof(x)/sizeof(*x))
#define STRLEN(x) ((sizeof(x)/sizeof(*x))-1)
#define Arr(T) T *
#define Map(K, V) struct { K key; V value; } *
#define Dict(V) struct { char *key; V value; } *
#define UNREACHABLE assert("UNREACHABLE"&&0)
#define UNIMPLEMENTED assert("UNIMPLEMENTED"&&0)
#define PASS assert("PASS"&&1)
#define DUNNO fprintf(stderr, "======\nDUNNO WHAT HAPPENS ON LINE %i IN %s()\n======\n", __LINE__, __func__)
#define INLINE __attribute__((always_inline)) inline
#if defined(stbsp_sprintf) && defined(stbsp_snprintf)
#undef sprintf
#define sprintf stbsp_sprintf
#undef snprintf
#define snprintf stbsp_snprintf
#endif
#define VEC2_IHAT ((Vector2){1.0f, 0.0f})
#define VEC2_JHAT ((Vector2){0.0f, 1.0f})
#define VEC2_ORIGIN ((Vector2){0.0f,0.0f})
#define VEC2_ZERO ((Vector2){0.0f,0.0f})
#define F32_NEGATIVE_ZERO (u32)(0x80000000)
#define member_size(type, member) sizeof(((type*)0)->member)

typedef int64_t s64;
typedef uint64_t u64;
typedef int32_t s32;
typedef uint32_t u32;
typedef int16_t s16;
typedef uint16_t u16;
typedef int8_t s8;
typedef uint8_t u8;
typedef float f32;
typedef double f64;

#endif
