CC = clang

C_SRC_DIR := src/c
SRCS := $(shell find $(C_SRC_DIR) -name *.c)
BIN_DIR := bin

EXE := ihsclox

CFLAGS := -Wall

.PHONY: clean run

clean:
	find . -perm +100 -type f -delete
	rm -f $(EXE)
	rm -f *.so
	rm -f *.o
	rm -rf *.dSYM/

build:
	$(CC) $(CFLAGS) $(SRCS) -o $(EXE)

run:
	@./$(EXE)
