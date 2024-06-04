#ifndef JLIB_ARENA_H
#define JLIB_ARENA_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct Arena Arena;
typedef struct Arena_save Arena_save;

#define JLIB_ARENA_INITIAL_BLOCK_BYTES (1<<13) // 8K bytes
#define JLIB_ARENA_TMP_INITIAL_BLOCK_BYTES (1<<11) // 2K bytes

#define arena_init(a) arena_init_full(a, false, JLIB_ARENA_INITIAL_BLOCK_BYTES)
#define arena_tmp(a) arena_init_full(a, true, JLIB_ARENA_TMP_INITIAL_BLOCK_BYTES)
void arena_init_full(Arena *a, bool cannot_grow, size_t initial_block_bytes);
void* arena_alloc(Arena *a, size_t bytes);
Arena_save arena_to_save(Arena *a);
void arena_from_save(Arena *a, Arena_save save);
void arena_step_back(Arena *a, size_t bytes);
void arena_free(Arena *a);
void arena_destroy(Arena *a);

#endif

#if defined(JLIB_ARENA_IMPL) != defined(_UNITY_BUILD_)

#ifdef _UNITY_BUILD_
#define JLIB_ARENA_IMPL
#endif

#ifndef INLINE
#define INLINE __attribute__((always_inline)) inline
#endif

INLINE uint64_t _round_pow_2(uint64_t n) {
    --n;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n |= n >> 32;
    ++n;
    return n;
}

struct Arena {
    bool cannot_grow;
    uint64_t pos;
    uint64_t cur_block;
    uint64_t n_blocks;
    size_t *block_sizes;
    uint8_t **blocks;
};

struct Arena_save {
    uint64_t pos;
    uint64_t cur_block;
};

INLINE void arena_init_full(Arena *a, bool cannot_grow, size_t initial_block_bytes) {
    *a = (Arena){0};
    a->cannot_grow = cannot_grow;
    a->blocks = malloc(sizeof(uint8_t*));
    a->block_sizes = malloc(sizeof(size_t));
    a->n_blocks = 1;
    a->blocks[0] = malloc(initial_block_bytes);
    a->block_sizes[0] = initial_block_bytes;
}

INLINE void* arena_alloc(Arena *a, size_t bytes) {
    if(bytes + a->pos >= a->block_sizes[a->cur_block]) {
        if(a->cannot_grow) {
            fprintf(stderr, "arena_alloc: arena cannot grow so malloc(%zu) bytes\n", bytes);
            return malloc(bytes);
        }
        size_t new_block_size = a->block_sizes[a->cur_block] << 1;
        while(new_block_size < bytes) new_block_size <<= 1;
        a->cur_block++;
        a->n_blocks++;
        a->blocks = realloc(a->blocks, (a->cur_block + 1) * new_block_size);
        a->block_sizes = realloc(a->block_sizes, (a->cur_block + 1) * sizeof(size_t));
        a->blocks[a->cur_block] = malloc(new_block_size);
        a->block_sizes[a->cur_block] = new_block_size;
        a->pos = 0;
    }

    void *ptr = a->blocks[a->cur_block] + a->pos;
    a->pos += bytes;
    memset(ptr, 0, bytes);

    return ptr;
}

INLINE void arena_step_back(Arena *a, size_t bytes) {
    while(bytes > a->pos) {
        bytes -= a->pos;
        a->cur_block--;
        a->pos = a->block_sizes[a->cur_block];
    }

    a->pos -= bytes;
}

INLINE void arena_free(Arena *a) {
    a->pos = a->cur_block = 0;
}

INLINE void arena_destroy(Arena *a) {
    if(a->blocks) {
        for(uint64_t i = 0; i <= a->cur_block; ++i)
            free(a->blocks[i]);
        free(a->blocks);
        free(a->block_sizes);
    }
    *a = (Arena){0};
}

INLINE Arena_save arena_to_save(Arena *a) {
    return (Arena_save){ .pos = a->pos, .cur_block = a->cur_block };
}

INLINE void arena_from_save(Arena *a, Arena_save save) {
    a->pos = save.pos;
    a->cur_block = save.cur_block;
}

#endif
