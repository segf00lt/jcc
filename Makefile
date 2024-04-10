FLAGS = -g -Wall -Wpedantic -Werror -Wno-switch -Wno-comment -D'_UNITY_BUILD_'
TARGET = jcc.c

all:
	$(CC) $(FLAGS) $(TARGET) -o jcc

#test: all
#	cd test/; ./unittest; cd ..;
.PHONY: all
