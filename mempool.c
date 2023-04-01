#define MEMPOOL_PAGESIZE 128 /* number of objects in page */
#define MEMPOOL_INITIAL_PAGECOUNT 2 /* initial number of pages */

typedef struct Pool Pool;
struct Pool {
	size_t objsize;
	size_t curpage;
	size_t pagecount; /* length of void **pages */
	size_t objcount; /* debug */
	void *base; /* base of current page */
	void *free; /* first free space in current page */
	void **pages;
};

#define MEMPOOL_INIT(pool, obj) { \
	pool->objsize = sizeof(obj); \
	pool->pages = malloc(sizeof(void*) * MEMPOOL_INITIAL_PAGECOUNT); \
	pool->pages[0] = pool->base = pool->free = malloc(sizeof(obj) * MEMPOOL_PAGESIZE); \
	pool->pagecount = MEMPOOL_INITIAL_PAGECOUNT; \
	pool->objcount = 0; \
}

void* pool_alloc(Pool *pool) {
	register void *p;

	assert(pool->objsize != 0);

	if((char*)pool->free - (char*)pool->base >= pool->objsize * MEMPOOL_PAGESIZE) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = malloc(pool->objsize * MEMPOOL_PAGESIZE);
		pool->base = pool->free = pool->pages[pool->curpage];
	}

	memset(pool->free, 0, pool->objsize);
	p = pool->free;
	pool->free = (unsigned char*)pool->free + pool->objsize;

	return p;
}

void* pool_alloc_string(Pool *pool, size_t len, char *s) {
	register void *p;
	size_t i;

	if((char*)pool->free - (char*)pool->base + len >= pool->objsize * MEMPOOL_PAGESIZE) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = malloc(pool->objsize * MEMPOOL_PAGESIZE);
		pool->base = pool->free = pool->pages[pool->curpage];
	}

	for(i = 0; i < len; ++i)
		((char*)pool->free)[i] = s[i];
	((char*)pool->free)[i] = 0;
	p = pool->free;
	pool->free = (unsigned char*)pool->free + i + 1;

	return p;
}

void pool_clear_page(Pool *pool) {
	memset(pool->base, 0, pool->objsize * MEMPOOL_PAGESIZE);
	pool->free = pool->base;
}

void pool_free(Pool *pool) {
	for(size_t i = 0; i <= pool->curpage; ++i)
		free(pool->pages[i]);
	free(pool->pages);
}
