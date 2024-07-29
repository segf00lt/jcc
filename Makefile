CC = clang
FLAGS = -g -O0 -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -D'_UNITY_BUILD_'
TARGET = jcc.c
LDFLAGS = -lraylib -lm

UNAME_S = $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	FLAGS += -Wno-format -L'/opt/homebrew/lib/' -I'/opt/homebrew/include/'
else
	LDFLAGS += -L'/usr/local/lib/'
endif

all:
	ctags -w *.c *.h
	$(CC) $(FLAGS) $(TARGET) -o jcc $(LDFLAGS)

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
