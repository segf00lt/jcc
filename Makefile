CC = clang
FLAGS = -g -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -D'_UNITY_BUILD_'
TARGET = jcc.c
LDFLAGS = -L'/usr/local/lib/' -lraylib -lm

all:
	$(CC) $(FLAGS) $(TARGET) -o jcc $(LDFLAGS)

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
