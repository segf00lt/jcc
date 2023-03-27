typedef struct {
	size_t objsize;
	size_t pagesize; /* in bytes */
	size_t curpage;
	size_t pagecount;
	void *base; /* base of current page */
	void *free; /* first free space in current page */
	void **pages;
} Mempool;

void* mempool_alloc(Mempool *pool) {
	register void *p;

	assert(pool->objsize != 0);

	if((char*)pool->free - (char*)pool->base >= pool->pagesize) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = malloc(pool->pagesize);
		pool->base = pool->free = pool->pages[pool->curpage];
	}

	memset(pool->free, 0, pool->objsize);
	p = pool->free;
	pool->free = (unsigned char*)pool->free + pool->objsize;

	return p;
}

void* mempool_alloc_string(Mempool *pool, char *s, size_t len) {
	register void *p;
	size_t i;

	if((char*)pool->free - (char*)pool->base + len >= pool->pagesize) {
		++pool->curpage;
		if(pool->curpage >= pool->pagecount) {
			pool->pagecount <<= 1;
			pool->pages = realloc(pool->pages, pool->pagecount * sizeof(void*));
		}
		pool->pages[pool->curpage] = malloc(pool->pagesize);
		pool->base = pool->free = pool->pages[pool->curpage];
	}

	for(i = 0; i < len; ++i)
		((char*)pool->free)[i] = s[i];
	((char*)pool->free)[i] = 0;
	p = pool->free;
	pool->free = (unsigned char*)pool->free + i + 1;

	return p;
}

void mempool_clear_page(Mempool *pool) {
	memset(pool->base, 0, pool->pagesize);
	pool->free = pool->base;
}

void mempool_free(Mempool *pool) {
	for(size_t i = 0; i <= pool->curpage; ++i)
		free(pool->pages[i]);
	free(pool->pages);
}
