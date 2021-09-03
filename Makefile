CC = clang

C_SRC_DIR := src/c
BIN_DIR := bin

EXE := $(BIN_DIR)/ihsclox

CFLAGS := -Wall

.PHONY: clean

clean:
	find . -perm +100 -type f -delete
	rm -f $(EXE)
	rm -f *.so
	rm -f *.o
	rm -rf *.dSYM/

build:
	$(CC) $(CFLAGS) $(C_SRC_DIR)/main.c -o $(EXE)
