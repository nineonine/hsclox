#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "utils.h"
#include "vm.h"

static void constOp(Chunk* chunk, double i, int line) {
    int constant = addConstant(chunk, i);
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, constant, line);
}

int main(int argc, const char* argv[]) {
    initVM();
    Chunk chunk;
    initChunk(&chunk);

    constOp(&chunk, 1.2, 1);
    constOp(&chunk, 3.4, 1);
    writeChunk(&chunk, OP_ADD, 1);
    constOp(&chunk, 5.6, 1);
    writeChunk(&chunk, OP_DIVIDE, 1);
    writeChunk(&chunk, OP_NEGATE, 1);
    writeChunk(&chunk, OP_RETURN, 1);

    // disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);
    return 0;
}
