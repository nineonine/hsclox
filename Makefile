CC = clang

C_SRC_DIR := src/c
SRCS := $(shell find $(C_SRC_DIR) -name *.c)
BIN_DIR := bin

EXE := ihsclox

CFLAGS := -Wall -g
CPPFLAGS=-DDEBUG_PRINT_CODE
ifeq ($(DEBUG), 1)
	CPPFLAGS += -DDEBUG_TRACE_EXECUTION
endif

.PHONY: clean run test help
.DEFAULT_GOAL := help

clean:	## clean up build artifacts
	find . -perm +100 -type f -delete
	rm -f $(EXE)
	rm -f *.so
	rm -f *.o
	rm -rf *.dSYM/

build:	## build interpeter
	$(CC) $(CFLAGS) $(CPPFLAGS) $(SRCS) -o $(EXE)

run:	## run interpreter
	@./$(EXE)

test:	## invoke tests
	@python3 test/driver.py -v

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
