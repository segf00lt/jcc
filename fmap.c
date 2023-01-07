typedef struct {
	int fd;
	char *buf;
	size_t size;
} Fmap;

int fmapopen(char *name, int perms, Fmap *mp) {
	if(!mp) return 0;

	*mp = (Fmap){0};
	mp->fd = open(name, perms);

	return mp->fd;
}

int fmapfdopen(int fd, Fmap *mp) {
	if(!mp) return 0;

	*mp = (Fmap){0};
	mp->fd = fd;

	return fd;
}

void fmapclose(Fmap *mp) {
	if(!mp) return;

	close(mp->fd);
	if(mp->buf) munmap(mp->buf, mp->size);
	*mp = (Fmap){0};
}

size_t fmapread(Fmap *mp) {
	struct stat s;

	if(!mp) return 0;

	fstat(mp->fd, &s);

	mp->size = s.st_size + 1;
	mp->buf = mmap(0, mp->size, PROT_READ | PROT_WRITE, MAP_PRIVATE, mp->fd, 0);

	if(mp->buf == (void*)-1)
		return 0;

	mp->buf[s.st_size] = 0; // null terminate

	return mp->size;
}

