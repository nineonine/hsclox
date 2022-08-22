#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"
/* #ifdef __GLASGOW_HASKELL__ */
#include "Compiler_stub.h"
/* #endif */

#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "utils.h"
#include "vm.h"

#include "examples.h"

// static void constOp(Chunk* chunk, double i, int line) {
//     int constant = addConstant(chunk, i);
//     writeChunk(chunk, OP_CONSTANT, line);
//     writeChunk(chunk, constant, line);
// }

void repl() {
    char line[1024];
    for(;;) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        interpret(line);
    }
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }
    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) {
    char* source = readFile(path);
    InterpretResult result = interpret(source);
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, char* argv[]) {

    // Initialize Haskell Runtime _before_ any calls to the Haskell code
    // and then make a call to Haskell code
    // hsCompilerInit();
    // hscompile();
    // hsCompilerExit();

    initVM();

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
        exit(EXIT_SUCCESS);
    } else {
        fprintf(stderr, "Usage: hsclox [path]\n");
        exit(64);
    }

    freeVM();
    // freeChunk(&chunk);
    return 0;
}
