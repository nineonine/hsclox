CC = clang

C_SRC_DIR := src/c
SRCS := $(shell find $(C_SRC_DIR) -name *.c)
BIN_DIR := bin

EXE := ihsclox

CFLAGS := -Wall -g
CPPFLAGS=-DDEBUG_PRINT_CODE
ifeq ($(DEBUG), 1) # DEBUGGING EXECUTION
	CPPFLAGS += -DDEBUG_TRACE_EXECUTION
else ifeq ($(DEBUG), 2) # DEBUGGING GC
	CPPFLAGS += -DDEBUG_LOG_GC -DDEBUG_STRESS_GC
endif

GHC_LIBDIR := $(shell ghc --print-libdir)
HS_RTS_INCLUDE := $(shell find $(GHC_LIBDIR) -type d -name "include" | grep rts)

.PHONY: clean run test help
.DEFAULT_GOAL := build

clean:	## clean up build artifacts
	cabal clean
	find . -perm +100 -type f -delete
	rm -f src/c/hscompiler.*
	rm -f src/c/Compiler_Stub.h
	rm -f $(EXE)
	rm -f *.so
	rm -f *.o
	rm -rf *.dSYM/

build:	## build interpeter
	cabal build
	find dist-newstyle/ -name 'hscompiler.*' -exec cp {} ./src/c/ \;
	find dist-newstyle/ -name 'Compiler_stub.h' -exec cp {} ./src/c/ \;
	$(CC) $(CFLAGS) $(CPPFLAGS) -I$(HS_RTS_INCLUDE) $(C_SRC_DIR)/hscompiler.o $(SRCS) -o $(EXE) -v

run:	## run interpreter
	@./$(EXE) $(SRC)

test:	## invoke tests
	@python3 test/driver.py -u regression -v

perftest:	## invoke perf tests
	@python3 test/driver.py -u perf -v

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
