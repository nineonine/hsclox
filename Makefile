GHC = ghc
GHC_PKG = $(shell dirname $(shell which $(GHC)))/ghc-pkg
CC = cc

C_SRC_DIR := src/c
SRCS := $(shell find $(C_SRC_DIR) -name *.c)
OBJS := $(patsubst %.c,%.o,$(SRCS))
BIN_DIR := bin

EXE := ihsclox

HS_RTS_INCLUDE := $(shell $(GHC_PKG) field rts include-dirs --simple-output)
INCLUDES := $(addprefix -I,$(HS_RTS_INCLUDE))

CFLAGS := -Wall -g
CPPFLAGS=-DDEBUG_PRINT_CODE $(INCLUDES)
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
	rm -f src/c/hscompiler.*
	rm -f src/c/Compiler_Stub.h
	rm -f $(EXE)
	rm -f *.so
	rm -f *.o
	rm -rf *.dSYM/

$(C_SRC_DIR)/libhsclox-ffi.so:	## build interpeter
	cabal build
	find dist-newstyle/ -name 'hscompiler.*' -exec cp {} $(C_SRC_DIR) \;
	find dist-newstyle/ -name 'libhsclox-ffi.so' -exec cp {} $(C_SRC_DIR) \;
	find dist-newstyle/ -name 'Compiler_stub.h' -exec cp {} $(C_SRC_DIR) \;

build: $(C_SRC_DIR)/libhsclox-ffi.so $(OBJS)
	$(GHC) -no-hs-main $(CFLAGS) $(CPPFLAGS) -o $(EXE) -lhsclox-ffi -L$(C_SRC_DIR) $(OBJS)

run:	## run interpreter
	@./$(EXE) $(SRC)

test:	## invoke tests
	@python3 test/driver.py -u regression -v

perftest:	## invoke perf tests
	@python3 test/driver.py -u perf -v

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
