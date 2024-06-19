CC = clang
FLAGS = -ggdb -O0 -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -D'_UNITY_BUILD_'
TARGET = jcc.c
LDFLAGS = -L'/usr/local/lib/' -lraylib -lm

all:
	ctags *.c *.h
	$(CC) $(FLAGS) $(TARGET) -o jcc $(LDFLAGS)

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
