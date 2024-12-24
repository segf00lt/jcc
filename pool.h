#ifndef JLIB_POOL_H
#define JLIB_POOL_H

#include <stdlib.h>
#include <stdint.h>

/*
 * This is a chunked pool allocator. It's meant to allow you to allocate and
 * of the same size in roughly contiguous chunks of memory.
 * Since it gets memory from the OS in separate chunks saved in an array,
 * it can be used to store self referential structures like trees or linked lists
 * and free the entire thing at once.
 */

#ifndef POOL_INITIAL_CHUNK_COUNT
#define POOL_INITIAL_CHUNK_COUNT 1
#endif

typedef struct Pool Pool;
typedef struct Pool_save Pool_save;

void  pool_init(Pool *p, size_t item_size);
void* pool_alloc(Pool *p);
void  pool_free(Pool *p);
void  pool_destroy(Pool *p);
Pool_save pool_to_save(Pool *p);
void pool_from_save(Pool *p, Pool_save save);

#if defined(JLIB_POOL_IMPL) != defined(_UNITY_BUILD_)

#ifdef _UNITY_BUILD_
#define JLIB_POOL_IMPL
#endif

struct Pool {
	size_t item_size;
    uint64_t occupied;
	uint64_t cur_chunk;
	uint64_t chunks_available;
	uint8_t **chunks;
};

struct Pool_save {
    uint32_t occupied;
	uint32_t cur_chunk;
};

void pool_init(Pool *p, size_t item_size) {
	p->item_size = item_size;
    p->occupied = 0;
	p->cur_chunk = 0;
	p->chunks_available = POOL_INITIAL_CHUNK_COUNT;
	p->chunks = malloc(sizeof(void*) * POOL_INITIAL_CHUNK_COUNT);
	for(size_t i = 0; i < POOL_INITIAL_CHUNK_COUNT; ++i)
		p->chunks[i] = malloc(item_size << 6);
}

void* pool_alloc(Pool *p) {
    if(p->occupied >= 64) {
        p->occupied = 0;
        ++p->cur_chunk;
        if(p->cur_chunk >= p->chunks_available) {
            p->chunks_available <<= 1;
            p->chunks = realloc(p->chunks, sizeof(uint8_t*) * p->chunks_available);
            for(uint32_t i = p->cur_chunk; i < p->chunks_available; ++i)
                p->chunks[i] = malloc(p->item_size << 6);
        }
    }

    void *ptr = (void*)(p->chunks[p->cur_chunk] + p->occupied*p->item_size);
    memset(ptr, 0, p->item_size);

    ++p->occupied;

    return ptr;
}

void pool_free(Pool *p) {
    p->cur_chunk = p->occupied = 0;
}

void pool_destroy(Pool *p) {
    if(p->chunks) {
        for(uint32_t i = 0; i < p->chunks_available; ++i)
            free(p->chunks[i]);
        free(p->chunks);
    }
    *p = (Pool){0};
}

Pool_save pool_to_save(Pool *p) {
    return (Pool_save){ .occupied = p->occupied, .cur_chunk = p->cur_chunk };
}

void pool_from_save(Pool *p, Pool_save save) {
    p->occupied = save.occupied;
    p->cur_chunk = save.cur_chunk;
}

#endif

#endif
