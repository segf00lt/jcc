CC = clang
FLAGS = -g -O0 -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -D'_UNITY_BUILD_'
TARGET = jcc.c
LDFLAGS = -L'/usr/local/lib/' -lraylib -lm

UNAME_S = $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	FLAGS += -Wno-format -fsanitize=address
endif

all:
	ctags -w *.c *.h
	$(CC) $(FLAGS) $(TARGET) -o jcc $(LDFLAGS)

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
