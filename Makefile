CC = cc
FLAGS = -g -Wall -Wpedantic -Werror
TARGET = jcc.c

all:
	$(CC) $(FLAGS) $(TARGET) -o jcc

#test: all
#	cd test/; ./unittest; cd ..;
