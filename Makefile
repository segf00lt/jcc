CC = clang
FLAGS = -g -O0 -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -Wno-format-pedantic -Wno-extra-semi -L'./third_party/raylib/' -I'./third_party/raylib/'
TARGET = jcc.c
LDFLAGS = -lraylib -lm

UNAME_S = $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	FLAGS += -Wno-format -framework CoreVideo -framework IOKit -framework Cocoa -framework GLUT -framework OpenGL #-L'/opt/homebrew/lib/' -I'/opt/homebrew/include/'
#else
#	LDFLAGS += -L'/usr/local/lib/'
endif

all:
	ctags -w --language-force=C --c-kinds=+zfx --extras=+q --fields=+n --recurse .
	$(CC) $(FLAGS) $(TARGET) -o jcc $(LDFLAGS)
#	ctags -w --language-force=C --c-kinds=+z --extras=+q --fields=+n *.c *.h

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
