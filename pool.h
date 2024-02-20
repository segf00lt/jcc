#ifndef JLIB_POOL_H
#define JLIB_POOL_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

/*
 * This is a chunked pool allocator. It's meant to allow you to allocate and
 * free items of the same size in roughly contiguous chunks of memory.
 */

typedef struct Pool Pool;
typedef struct Pool_free_node Pool_free_node;
typedef struct Pool_locator Pool_locator;

void pool_init(Pool *p, size_t item_size, size_t max_items, size_t initial_chunk_count);
void* pool_alloc(Pool *p);
void pool_cannibalize(Pool *p, Pool *food);
void pool_slim(Pool *p);
void pool_free(Pool *p, void *ptr);
void pool_free_all(Pool *p);
void pool_free_current_chunk(Pool *p);
void pool_free_chunk(Pool *p, size_t index);
void pool_destroy(Pool *p);

#ifdef JLIB_POOL_IMPL

struct Pool {
	size_t item_size;
	size_t max_items;
	size_t chunk_bytes;
	size_t chunks_in_use;
	size_t chunks_available;
	unsigned char **chunks;
	unsigned char *untouched_item;
	Pool_free_node *free;
};

struct Pool_free_node {
	Pool_free_node *next;
};

struct Pool_locator {
	uint32_t chunk, slot;
};

void pool_init(Pool *p, size_t item_size, size_t max_items, size_t initial_chunk_count) {
	assert("initial_chunk_count must be > 0" && initial_chunk_count > 0);
	item_size = (sizeof(Pool_free_node) > item_size) ? sizeof(Pool_free_node) : item_size;
	p->item_size = item_size;
	p->max_items = max_items;
	p->chunk_bytes = item_size * max_items;
	p->chunks_in_use = 1;
	p->chunks_available = initial_chunk_count;
	p->chunks = malloc(sizeof(unsigned char*) * initial_chunk_count);
	for(size_t i = 0; i < initial_chunk_count; ++i)
		p->chunks[i] = malloc(p->chunk_bytes);
	p->untouched_item = p->chunks[0];
	p->free = NULL;
}

void* pool_alloc(Pool *p) {
	void *ptr;

	if(!p->untouched_item && !p->free) {
		if(p->chunks_in_use >= p->chunks_available) {
			p->chunks_available += 2;
			p->chunks = realloc(p->chunks, sizeof(unsigned char *) * p->chunks_available);
			p->chunks[p->chunks_available-2] = malloc(p->chunk_bytes);
			p->chunks[p->chunks_available-1] = malloc(p->chunk_bytes);
		}
		p->untouched_item = p->chunks[p->chunks_in_use] + p->item_size;
		ptr = p->chunks[p->chunks_in_use];
		++p->chunks_in_use;
	} else if(!p->untouched_item && p->free) {
		ptr = p->free;
		p->free = p->free->next;
	} else {
		ptr = p->untouched_item;
		p->untouched_item += p->item_size;
		if(p->untouched_item >= p->chunks[p->chunks_in_use-1] + p->chunk_bytes)
			p->untouched_item = NULL;
	}

	return ptr;
}

void pool_free(Pool *p, void *ptr) {
#ifndef JLIB_POOL_UNSAFE
	unsigned char *free = ptr;
	size_t i;
	for(i = 0; i < p->chunks_in_use; ++i)
		if(free >= p->chunks[i] && free <= p->chunks[i] + p->chunk_bytes)
			break;
	assert("pointer is not in pool bounds" && i < p->chunks_in_use);
#endif
	Pool_free_node *free_node = ptr;
	free_node->next = p->free;
	p->free = free_node;
}

void pool_cannibalize(Pool *p, Pool *food) {
	assert("incompatible pool" && p->chunk_bytes == food->chunk_bytes);
	p->chunks = realloc(p->chunks,(p->chunks_available+food->chunks_available)*sizeof(unsigned char*));
	for(size_t i = 0, j = p->chunks_available; i < food->chunks_available; ++i, ++j)
		p->chunks[j] = food->chunks[i];
	p->chunks_available += food->chunks_available;
	free(food->chunks);
	*food = (Pool){0};
}

void pool_slim(Pool *p) {
	size_t n = p->chunks_available - p->chunks_in_use;
	size_t i = p->chunks_available - 1;
	while(n-- > 0)
		free(p->chunks[i--]);
	p->chunks_available = p->chunks_in_use;
	p->chunks = realloc(p->chunks, p->chunks_available * sizeof(unsigned char*));
	p->untouched_item = NULL;
}

void pool_free_all(Pool *p) {
	p->free = NULL;
	p->untouched_item = p->chunks[0];
}

void pool_free_current_chunk(Pool *p) {
	p->untouched_item = p->chunks[p->chunks_in_use-1];
}

void pool_free_locator(Pool *p, Pool_locator l) {
	assert("pool_free_locator unimplemented" && 0);
}

void pool_free_chunk(Pool *p, size_t index) {
	assert("pool_free_chunk unimplemented" && 0);
}

void pool_destroy(Pool *p) {
	assert("pool is not initialized" && p->chunks_available && p->chunks);
	p->free = NULL;
	for(size_t i = 0; i < p->chunks_available; ++i)
		free(p->chunks[i]);
	free(p->chunks);
}

#endif

#endif
