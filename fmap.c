typedef struct {
	int fd;
	char *buf;
	size_t size;
} Fmap;

int fmapfdopen(int fd, Fmap *mp) {
	struct stat s;
	assert(mp);
	assert(fd >= 0);
	*mp = (Fmap){0};
	mp->fd = fd;
	fstat(mp->fd, &s);
	mp->size = s.st_size + 1;
	mp->buf = (char*)mmap(0, mp->size, PROT_READ|PROT_WRITE, MAP_PRIVATE, mp->fd, 0);
	if(mp->buf == (void*)-1)
		return -1;
	mp->buf[s.st_size] = 0;
	return 0;
}

int fmapopen(char *name, int perms, Fmap *mp) {
	assert(mp);
	int fd = open(name, perms);
	fmapfdopen(fd, mp);
	return fd;
}

void fmapclose(Fmap *mp) {
	assert(mp);
	close(mp->fd);
	if(mp->buf) munmap(mp->buf, mp->size);
	*mp = (Fmap){0};
}
