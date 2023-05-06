typedef struct Pool Pool;
struct Pool {
	size_t objsize;
	size_t pagesize;
	size_t curpage;
	size_t pagecount; /* length of void **pages */
	void *base; /* base of current page */
	void *free; /* first free space in current page */
	void **pages;
};

void pool_init(Pool *pool, size_t objsize, size_t pagesize, size_t initial_pagecount) {
	pool->pagesize = pagesize;
	pool->objsize = objsize;
	pool->pages = (void**)malloc(sizeof(void*) * initial_pagecount);
	pool->pages[0] = pool->base = pool->free = calloc(pagesize, objsize);
	pool->pagecount = initial_pagecount;
}

void* pool_alloc(Pool *pool) {
	void *p;

	assert(pool->objsize != 0);

	if((char*)pool->free - (char*)pool->base >= pool->objsize * pool->pagesize) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = (void**)realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = calloc(pool->pagesize, pool->objsize);
		pool->base = pool->free = pool->pages[pool->curpage];
	}

	p = pool->free;
	pool->free = (unsigned char*)pool->free + pool->objsize;

	return p;
}

void* pool_alloc_string(Pool *pool, size_t len, char *s) {
	void *p;
	size_t i;

	if((char*)pool->free - (char*)pool->base + len >= pool->objsize * pool->pagesize) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = (void**)realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = calloc(pool->pagesize, pool->objsize);
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
	memset(pool->base, 0, pool->objsize * pool->pagesize);
	pool->free = pool->base;
}

void pool_free(Pool *pool) {
	for(size_t i = 0; i <= pool->curpage; ++i)
		free(pool->pages[i]);
	free(pool->pages);
}
