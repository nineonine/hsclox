UNAME := $(shell uname)
GHC = ghc
GHC_PKG = $(shell dirname $(shell which $(GHC)))/ghc-pkg
CC = cc

C_SRC_DIR := src/c
HS_SRC_DIR := src/hs

SRCS := $(shell find $(C_SRC_DIR) -name *.c)
OBJS := $(patsubst %.c,%.o,$(SRCS))

EXE := ihsclox

CFLAGS := -Wall -g
CPPFLAGS=-DDEBUG_PRINT_CODE
ifeq ($(DEBUG), 1) # DEBUGGING EXECUTION
	CPPFLAGS += -DDEBUG_TRACE_EXECUTION
else ifeq ($(DEBUG), 2) # DEBUGGING GC
	CPPFLAGS += -DDEBUG_LOG_GC -DDEBUG_STRESS_GC
endif

.PHONY: clean run test help
.DEFAULT_GOAL := build

clean:	## clean up build artifacts
	cabal clean
	find . -perm +100 -type f -delete
	rm -f src/c/Compiler_Stub.h
	rm -f $(EXE)
	rm -f $(OBJS)
	rm -rf *.dSYM/

$(C_SRC_DIR)/libhsclox-ffi.dylib:	## build interpeter
	cabal build
	find dist-newstyle/ -name 'libhsclox-ffi.dylib' -exec cp {} $(C_SRC_DIR) \;
	find dist-newstyle/ -name 'Compiler_stub.h' -exec cp {} $(C_SRC_DIR) \;

build: $(C_SRC_DIR)/libhsclox-ffi.dylib
	$(CC) $(CFLAGS) $(CPPFLAGS) $(SRCS) -o $(EXE) -I./$(C_SRC_DIR) -I./$(HS_SRC_DIR) -lhsclox-ffi -L./$(C_SRC_DIR)
ifeq ($(UNAME), Darwin)
	install_name_tool -add_rpath @executable_path/$(C_SRC_DIR) $(EXE)
endif

run:	## run interpreter
	@./$(EXE) $(SRC)

test:	## invoke tests
	@python3 test/driver.py -u regression -v

perftest:	## invoke perf tests
	@python3 test/driver.py -u perf -v

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
