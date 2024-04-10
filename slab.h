#ifndef JLIB_SLAB_H
#define JLIB_SLAB_H

#include <stdlib.h>
#include <stdint.h>

#ifndef SLAB_INITIAL_COUNT
#define SLAB_INITIAL_COUNT 4
#endif

typedef struct Slab {
    size_t slab_bytes;
    uint32_t vacant;
	uint32_t cur_slab;
	uint32_t slabs_available;
	uint8_t **slabs;
} Slab;

typedef struct Slab_save {
    uint32_t vacant;
	uint32_t cur_slab;
} Slab_save;

void  slab_init(Slab *s, size_t slab_bytes);
void* slab_alloc(Slab *s, size_t bytes);
void  slab_free(Slab *s);
void  slab_destroy(Slab *s);
Slab_save slab_to_save(Slab *s);
void slab_from_save(Slab *s, Slab_save save);

#endif

#if defined(JLIB_SLAB_IMPL) != defined(_UNITY_BUILD_)

#ifdef _UNITY_BUILD_
#define JLIB_SLAB_IMPL
#endif

void slab_init(Slab *s, size_t slab_bytes) {
	s->slab_bytes = slab_bytes;
    s->vacant = 0;
	s->cur_slab = 0;
	s->slabs_available = SLAB_INITIAL_COUNT;
	s->slabs = malloc(sizeof(uint8_t*) * SLAB_INITIAL_COUNT);
	for(size_t i = 0; i < SLAB_INITIAL_COUNT; ++i)
		s->slabs[i] = malloc(slab_bytes);
}

void* slab_alloc(Slab *s, size_t bytes) {
    s->vacant += bytes;

    if(s->vacant >= s->slab_bytes) {
        s->vacant = 0;
        ++s->cur_slab;
        if(s->cur_slab >= s->slabs_available) {
            s->slabs_available <<= 1;
            s->slabs = realloc(s->slabs, sizeof(uint8_t*) * s->slabs_available);
            for(uint32_t i = s->cur_slab; i < s->slabs_available; ++i)
                s->slabs[i] = malloc(s->slab_bytes);
        }
    }

    return (void*)(s->slabs[s->cur_slab] + s->vacant);
}

void slab_free(Slab *s) {
    s->cur_slab = s->vacant = 0;
}

void slab_destroy(Slab *s) {
    for(uint32_t i = 0; i < s->slabs_available; ++i)
        free(s->slabs[i]);
    free(s->slabs);
    *s = (Slab){0};
}

Slab_save slab_to_save(Slab *s) {
    return (Slab_save){ .vacant = s->vacant, .cur_slab = s->cur_slab };
}

void slab_from_save(Slab *s, Slab_save save) {
    s->vacant = save.vacant;
    s->cur_slab = save.cur_slab;
}

#endif
