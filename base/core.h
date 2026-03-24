#ifndef CORE_H
#define CORE_H


/*
 * various macro helpers from ryan fleury
 */

////////////////////////////////
//~ rjf: Codebase Keywords

#if PLATFORM_LINUX
#define _POSIX_C_SOURCE 200809L
#endif


#define internal      static
#define global        static
#define local_persist static

#if COMPILER_MSVC || (COMPILER_CLANG && PLATFORM_WINDOWS)
# pragma section(".rdata$", read)
# define read_only __declspec(allocate(".rdata$"))
#elif (COMPILER_CLANG && PLATFORM_LINUX)
# define read_only __attribute__((section(".rodata")))
#else
// NOTE(rjf): I don't know of a useful way to do this in GCC land.
// __attribute__((section(".rodata"))) looked promising, but it introduces a
// strange warning about malformed section attributes, and it doesn't look
// like writing to that section reliably produces access violations, strangely
// enough. (It does on Clang)
# define read_only
#endif

#if COMPILER_MSVC
# define thread_static __declspec(thread)
#elif COMPILER_CLANG || COMPILER_GCC
# define thread_static __thread
#endif

#if COMPILER_MSVC
# define force_inline __forceinline
#elif COMPILER_CLANG || COMPILER_GCC
# define force_inline __attribute__((always_inline)) inline
#endif

////////////////////////////////
//~ rjf: Linkage Keyword Macros

#if PLATFORM_WINDOWS
# define shared_function C_LINKAGE __declspec(dllexport)
#else
# define shared_function C_LINKAGE
#endif

#if LANG_CPP
# define C_LINKAGE_BEGIN extern "C"{
# define C_LINKAGE_END }
# define C_LINKAGE extern "C"
#else
# define C_LINKAGE_BEGIN
# define C_LINKAGE_END
# define C_LINKAGE
#endif

#ifndef alignof
#if COMPILER_MSVC
# define alignof(T) __alignof(T)
#elif COMPILER_CLANG
# define alignof(T) __alignof(T)
#elif COMPILER_GCC
# define alignof(T) __alignof__(T)
#else
# error alignof not defined for this compiler.
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>

#define func
#define SQUARE(x) ((x)*(x))
#define TIMES2(x) ((x)+(x))
#define HALF(x) ((x)*0.5f)
#define IS_POW_2(x) (((x) & ((x)-1)) == 0)
#define SIGN_EXTEND_S64(x, n) (s64)(((n) >= 64) ? (s64)(x) : (s64)((s64)(x) | (s64)(-((s64)(x) >> ((s64)(n) - 1lu)) << (s64)(n))))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define CLAMP(value, min, max) ( ((value) < (min)) ? (min) : ( MIN((value), (max)) ) )
// TODO jfd: fix this wrap macro
// #define WRAP(value, min, max)  ( ((value) >= (max)) ? (min) : ( ((value) < (min)) ? (max) : (value) ) )
#define CLAMP_BOT(a, b) MAX(a, b)
#define CLAMP_TOP(a, b) MIN(a, b)
#define ARRLEN(x) (sizeof((x))/sizeof(*(x)))
#define STRLEN(x) ((sizeof((x))/sizeof(*(x)))-1)
#define GLUE_(a,b) a##b
#define GLUE(a,b) GLUE_(a,b)
#define DUNNO fprintf(stderr, "======\nDUNNO WHAT HAPPENS ON LINE %i IN %s()\n======\n", __LINE__, __func__)
#define ALIGN_UP(x, align) (((x) + (align) - 1) & ~((align) - 1))
#define STRINGIFY(x) #x
#define GLSL(src) STRINGIFY(src)
#define SWAP(a, b, T) do { T tmp = (a); (a) = (b); (b) = tmp; }while(0)

#ifndef true
#define true ((b8)1u)
#endif

#ifndef false
#define false ((b8)0u)
#endif

// TODO custom stb sprintf decorator
//#if defined(stbsp_sprintf) && defined(stbsp_snprintf)
//#undef sprintf
//#define sprintf stbsp_sprintf
//#undef snprintf
//#define snprintf stbsp_snprintf
//#endif

#define VEC2_IHAT ((Vector2){1.0f, 0.0f})
#define VEC2_JHAT ((Vector2){0.0f, 1.0f})
#define VEC2_ORIGIN ((Vector2){0.0f,0.0f})
#define VEC2_ZERO ((Vector2){0.0f,0.0f})
#define f32_nEGATIVE_ZERO (u32)(0x80000000)
#define member_size(type, member) sizeof(((type*)0)->member)
#define member_offset(type, member) offsetof(type, member)

////////////////////////////////
//~ rjf: Units

#define KB(n)  (((u64)(n)) << 10)
#define MB(n)  (((u64)(n)) << 20)
#define GB(n)  (((u64)(n)) << 30)
#define TB(n)  (((u64)(n)) << 40)
#define THOUSAND(n)   ((n)*1000)
#define MILLION(n)    ((n)*1000000)
#define BILLION(n)    ((n)*1000000000)


////////////////////////////////
//~ rjf: Asserts

#if COMPILER_MSVC
# define TRAP() __debugbreak()
#elif COMPILER_CLANG || COMPILER_GCC
# define TRAP() __builtin_trap()
#else
# error Unknown trap intrinsic for this compiler.
#endif

// TODO need a stdio replacement
#define ASSERT_ALWAYS(x) do{if(!(x)) { fprintf(stderr, "ASSERT FAILED ON LINE %i OF FILE %s: %s\n", __LINE__, __FILE__, #x); TRAP();}}while(0)
#define ASSERT_ALWAYS_MESSAGE(x, msg, ...) do{if(!(x)) { fprintf(stderr, msg, __VA_ARGS__); TRAP();}}while(0)
#if BUILD_RELEASE /* prioritize debug build over release */
# define ASSERT(x) (void)(0)
# define ASSERT_MESSAGE(x, msg, ...) (void)(0)
#else
# define ASSERT(x) ASSERT_ALWAYS(x)
# define ASSERT_MESSAGE(x, msg, ...) ASSERT_ALWAYS_MESSAGE(x, msg, __VA_ARGS__)
#endif
#define UNREACHABLE   ASSERT_MESSAGE(0, "UNREACHABLE LINE %i FILE %s\n", __LINE__, __FILE__)
#define UNIMPLEMENTED   ASSERT_MESSAGE(0, "UNIMPLEMENTED LINE %i FILE %s\n", __LINE__, __FILE__)
#define TODO(msg)   ASSERT_MESSAGE(0, "TODO '%s' LINE %i FILE %s\n", msg, __LINE__, __FILE__)
// TODO these messages assume a string literal, eventually we should have alternatives for runtime strings
#define PANICF(msg, ...)    ASSERT_MESSAGE(0, "PANIC ON LINE %i OF FILE %s: "msg"\n", __LINE__, __FILE__, __VA_ARGS__)
#define PANIC(msg)    PANICF(msg"%s", "")
#define PASS          ASSERT(1)
#define NOOP ((void)0)
#define STATIC_ASSERT(expr, id) u8 GLUE(id, __LINE__)[(expr)?1:-1]
#define COWABUNGA fprintf(stderr, "COWABUNGA\n")


////////////////////////////////
//~ rjf: Memory Operation Macros

#define memory_copy(dst, src, size)    memmove((dst), (src), (size))
#define memory_set(dst, byte, size)    memset((dst), (byte), (size))
#define memory_compare(a, b, size)     memcmp((a), (b), (size))
#define memory_strlen(ptr)             strlen(ptr)
#define memory_zero(dst, size) memory_set(dst, 0, size)

#define memory_copy_struct(d,s)  memory_copy((d),(s),sizeof(*(d)))
#define memory_copy_array(d,s)   memory_copy((d),(s),sizeof(d))
#define memory_copy_typed(d,s,c) memory_copy((d),(s),sizeof(*(d))*(c))


////////////////////////////////
//~ rjf: Linked List Building Macros

// NOTE ~jfd 26/12/2025: NONE of these linked list macros should be used to produce a value

#define check_nil(nil,p) ((p) == 0 || (p) == nil)
#define set_nil(nil,p) ((p) = nil)

//- rjf: doubly-linked-lists
#define dll_insert_npz(nil,f,l,p,n,next,prev) (check_nil(nil,f) ? \
( (f) = (l) = (n), set_nil(nil,(n)->next), set_nil(nil,(n)->prev) ) :\
check_nil(nil,p) ? \
( (n)->next = (f), (f)->prev = (n), (f) = (n), set_nil(nil,(n)->prev) ) :\
((p)==(l)) ? \
( (l)->next = (n), (n)->prev = (l), (l) = (n), set_nil(nil, (n)->next) ) :\
( ( (!check_nil(nil,p) && check_nil(nil,(p)->next)) ? (0) : ( (p)->next->prev = (n) ) ), ((n)->next = (p)->next), ((p)->next = (n)), ((n)->prev = (p)) ) )

#define dll_push_back_npz(nil,f,l,n,next,prev) dll_insert_npz(nil,f,l,l,n,next,prev)

#define dll_push_front_npz(nil,f,l,n,next,prev) dll_insert_npz(nil,l,f,f,n,prev,next)

#define dll_remove_npz(nil,f,l,n,next,prev) (((n) == (f) ? (f) = (n)->next : (0)),\
((n) == (l) ? (l) = (l)->prev : (0)),\
(check_nil(nil,(n)->prev) ? (0) :\
((n)->prev->next = (n)->next)),\
(check_nil(nil,(n)->next) ? (0) :\
((n)->next->prev = (n)->prev)))

//- rjf: singly-linked, doubly-headed lists (queues)
#define sll_queue_push_nz(nil,f,l,n,next) (check_nil(nil,f)?\
((f)=(l)=(n),set_nil(nil,(n)->next)):\
((l)->next=(n),(l)=(n),set_nil(nil,(n)->next)))
#define sll_queue_push_front_nz(nil,f,l,n,next) (check_nil(nil,f)?\
((f)=(l)=(n),set_nil(nil,(n)->next)):\
((n)->next=(f),(f)=(n)))
#define sll_queue_pop_nz(nil,f,l,next) ((f)==(l)?\
(set_nil(nil,f),set_nil(nil,l)):\
((f)=(f)->next))

//- rjf: singly-linked, singly-headed lists (stacks)
#define sll_stack_push_n(f,n,next) ((n)->next=(f), (f)=(n))
#define sll_stack_pop_n(f,next) ((f)=(f)->next)

//- rjf: doubly-linked-list helpers
#define dll_insert_np(f,l,p,n,next,prev) dll_insert_npz(0,f,l,p,n,next,prev)
#define dll_push_back_np(f,l,n,next,prev) dll_push_back_npz(0,f,l,n,next,prev)
#define dll_push_front_np(f,l,n,next,prev) dll_push_front_npz(0,f,l,n,next,prev)
#define dll_remove_np(f,l,n,next,prev) dll_remove_npz(0,f,l,n,next,prev)
#define dll_insert_z(nil, f,l,p,n) dll_insert_npz(nil,f,l,p,n,next,prev)
#define dll_push_back_z(nil, f,l,n) dll_push_back_npz(nil,f,l,n,next,prev)
#define dll_push_front_z(nil, f,l,n) dll_push_front_npz(nil,f,l,n,next,prev)
#define dll_remove_z(nil, f,l,n) dll_remove_npz(nil,f,l,n,next,prev)
#define dll_insert(f,l,p,n) dll_insert_npz(0,f,l,p,n,next,prev)
#define dll_push_back(f,l,n) dll_push_back_npz(0,f,l,n,next,prev)
#define dll_push_front(f,l,n) dll_push_front_npz(0,f,l,n,next,prev)
#define dll_remove(f,l,n) dll_remove_npz(0,f,l,n,next,prev)

//- rjf: singly-linked, doubly-headed list helpers
#define sll_queue_push_n(f,l,n,next) sll_queue_push_nz(0,f,l,n,next)
#define sll_queue_push_front_n(f,l,n,next) sll_queue_push_front_nz(0,f,l,n,next)
#define sll_queue_pop_n(f,l,next) sll_queue_pop_nz(0,f,l,next)
#define sll_queue_push(f,l,n) sll_queue_push_nz(0,f,l,n,next)
#define sll_queue_push_front(f,l,n) sll_queue_push_front_nz(0,f,l,n,next)
#define sll_queue_pop(f,l) sll_queue_pop_nz(0,f,l,next)

//- rjf: singly-linked, singly-headed list helpers
#define sll_stack_push(f,n) sll_stack_push_n(f,n,next)
#define sll_stack_pop(f) sll_stack_pop_n(f,next)

#define defer_loop(begin, end) for(int __i__ = ((begin), 0); !__i__; __i__ += 1, (end))
#define defer_loop_check(begin, end) for(int __i__ = 2 * !(begin); (__i__ == 2 ? ((end), 0) : !__i__); __i__ += 1, (end))

typedef int64_t  s64;
typedef uint64_t u64;
typedef int32_t  s32;
typedef uint32_t u32;
typedef int16_t  s16;
typedef uint16_t u16;
typedef int8_t   s8;
typedef uint8_t  u8;
typedef float    f32;
typedef double   f64;

typedef uint8_t  b8;
typedef uint16_t b16;
typedef uint32_t b32;
typedef uint64_t b64;

#define MAX_U8 ((u8)~0)
#define MAX_U16 ((u16)~0)
#define MAX_S8 (((s8)~0)^(1u<<7))
#define MAX_S16 (((s16)~0)^(1u<<15))

#define MAX_U32 ((u32)~0)
#define MAX_U64 ((u64)~0)
#define MAX_S32 ((s32)(((s32)~0)^(1u<<31)))
#define MAX_S64 ((s64)(((s64)~0)^(1ull<<63)))


typedef void* void_ptr;
typedef char* char_ptr;

typedef void Void_func(void);

typedef enum Weekday {
  WEEKDAY_SUN = 0,
  WEEKDAY_MON,
  WEEKDAY_TUE,
  WEEKDAY_WED,
  WEEKDAY_THU,
  WEEKDAY_FRI,
  WEEKDAY_SAT,
  WEEKDAY_COUNT,
} Weekday;
STATIC_ASSERT(sizeof(Weekday) <= sizeof(u32), Weekday__is__right__size);

typedef enum Month {
  MONTH_JAN = 0,
  MONTH_FEB,
  MONTH_MAR,
  MONTH_APR,
  MONTH_MAY,
  MONTH_JUN,
  MONTH_JUL,
  MONTH_AUG,
  MONTH_SEP,
  MONTH_OCT,
  MONTH_NOV,
  MONTH_DEC,
  MONTH_COUNT,
} Month;
STATIC_ASSERT(sizeof(Month) <= sizeof(u32), Month__is__right__size);

typedef struct DateTime DateTime;
struct DateTime {
  u16 micro_sec; // [0,999]
  u16 msec; // [0,999]
  u16 sec;  // [0,60]
  u16 min;  // [0,59]
  u16 hour; // [0,24]
  u16 day;  // [0,30]
  Weekday weekday;
  Month month;
  u32 year; // 1 = 1 CE, 0 = 1 BC
};

typedef u64 DenseTime;

typedef u32 FilePropertyFlag;
enum {
  FILE_PROPERTY_IS_DIR   =   (1<<0),
  FILE_PROPERTY_READONLY =   (1<<1),
};

typedef struct FileProperties FileProperties;
struct FileProperties {
  u64 size;
  DenseTime modified;
  DenseTime created;
  FilePropertyFlag flags;
};

// DenseTime dense_time_from_date_time(DateTime date_time);
// DateTime date_time_from_dense_time(DenseTime time);
// DateTime date_time_from_micro_seconds(u64 time);
// DateTime date_time_from_unix_time(u64 unix_time);
// DenseTime dense_time_from_unix_time(u64 unix_time);


#endif

