#ifndef JLIB_ARENA_H
#define JLIB_ARENA_H

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include "basic.h"

#ifndef JLIB_ARENA_DEFAULT_ALIGNMENT
#define JLIB_ARENA_DEFAULT_ALIGNMENT (sizeof(void*) * 2)
#endif

typedef struct Arena Arena;

void arena_init(Arena *a, void *buf, size_t buf_cap);
#define arena_alloc(a, size) arena_alloc_align(a, size, JLIB_ARENA_DEFAULT_ALIGNMENT)
void* arena_alloc_align(Arena *a, size_t size, size_t align);
void* arena_alloc_grow(Arena *a, size_t size);
#define arena_resize(a, ptr, cur_size, new_size) arena_resize_align(a, ptr, cur_size, new_size, JLIB_ARENA_DEFAULT_ALIGNMENT)
void* arena_resize_align(Arena *a, void *ptr, size_t cur_size, size_t new_size, size_t align);
#define arena_realloc(a, ptr, cur_size, new_size) arena_realloc_align(a, ptr, cur_size, new_size, JLIB_ARENA_DEFAULT_ALIGNMENT)
void* arena_realloc_align(Arena *a, void *ptr, size_t cur_size, size_t new_size, size_t align);
void arena_free(Arena *a);

#ifdef JLIB_ARENA_IMPL

static uintptr_t align_ptr_forward(uintptr_t ptr, size_t align) {
	uintptr_t p, a, mod;
	assert(IS_POW_2(align));
	p = ptr;
	a = (uintptr_t)align;
	mod = p & (a-1);
	if(mod) p += a - mod;
	return p;
}

struct Arena {
    bool can_grow
	unsigned char *buf;
	size_t buf_cap;
	size_t cur_offset;
};

void arena_init(Arena *a, void *buf, size_t buf_cap) {
	a->buf = (unsigned char*)buf;
	a->buf_cap = buf_cap;
    a->cur_offset = 0;
}

void* arena_alloc_grow(Arena *a, size_t size) {
	uintptr_t cur_ptr = (uintptr_t)a->buf + (uintptr_t)a->cur_offset;
	uintptr_t offset = align_ptr_forward(cur_ptr, JLIB_ARENA_DEFAULT_ALIGNMENT) - (uintptr_t)a->buf;

	if(offset+size > a->buf_cap) {
		size_t newbuf_cap = MAX(offset+size, a->buf_cap<<1);
		unsigned char *newbuf = malloc(newbuf_cap);
		memcpy(newbuf, a->buf, a->buf_cap);
		free(a->buf);
		a->buf = newbuf;
		a->buf_cap = newbuf_cap;
	}

	void *ptr = a->buf + offset; // NOTE make sure this is the same as &a->buf[offset]
	a->prev_offset = offset;
	a->cur_offset = offset + size;
	memset(ptr, 0, size);
	return ptr;
}

void* arena_alloc_align(Arena *a, size_t size, size_t align) {
	uintptr_t cur_ptr = (uintptr_t)a->buf + (uintptr_t)a->cur_offset;
	uintptr_t offset = align_ptr_forward(cur_ptr, align) - (uintptr_t)a->buf;

	if(offset+size > a->buf_cap)
		return NULL;

	void *ptr = a->buf + offset; // NOTE make sure this is the same as &a->buf[offset]
	a->prev_offset = offset;
	a->cur_offset = offset + size;
	memset(ptr, 0, size);
	return ptr;
}

void* arena_resize_align(Arena *a, void *ptr, size_t cur_size, size_t new_size, size_t align) {
	assert(IS_POW_2(align)); // align is pow 2

	if(ptr == NULL || cur_size == 0) {
		return arena_alloc_align(a, new_size, align);
	} else if(a->buf > (unsigned char*)ptr || (unsigned char*)ptr >= a->buf + a->buf_cap) {
		assert(0 && "memory is out of bounds of the buffer in this arena");
		return NULL;
	}

	if(a->buf + a->prev_offset == (unsigned char*)ptr) {
		a->cur_offset = a->prev_offset + new_size;
		if(a->cur_offset >= a->buf_cap)
			return NULL;
		if(new_size > cur_size)
			memset(a->buf + a->cur_offset, 0, new_size-cur_size);
		return ptr;
	}

	void *new_ptr = arena_alloc_align(a, new_size, align);
	if(!new_ptr) return NULL;
	size_t copy_size = (cur_size < new_size) ? cur_size : new_size;
	memmove(new_ptr, ptr, copy_size);
	return new_ptr;
}

void arena_free(Arena *a) {
	a->prev_offset = a->cur_offset = 0;
}

#endif

#endif
