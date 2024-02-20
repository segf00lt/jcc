#ifndef JLIB_BASIC_H
#define JLIB_BASIC_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#define IS_POW_2(x) ((x & (x-1)) == 0)
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define STATICARRLEN(x) (sizeof(x)/sizeof(*x))
#define STRLEN(x) ((sizeof(x)/sizeof(*x))-1)
#define SIGN_EXTEND(x, l) (-(((x >> (l-1)) << l) - x))
#define Arr(T) T *
#define Map(K, V) struct { K key; V value; } *
#define Dict(V) struct { char *key; V value; } *
#define UNREACHABLE assert(0)
#define JINLINE __attribute__((always_inline)) inline
#define STATICARRFOR(arr) for(size_t arr##_index = 0; arr##_index < STATICARRLEN(arr); ++arr##_index)

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
