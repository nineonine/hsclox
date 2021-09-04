#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "utils.h"

int main(int argc, const char* argv[]) {
    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT, 1);
    writeChunk(&chunk, constant, 1);

    writeConstant(&chunk, 822, 2);
    writeConstant(&chunk, 865, 2);
    writeConstant(&chunk, 5125, 2);
    writeConstant(&chunk, 12, 2);

    writeConstant(&chunk, 322, 5);
    writeConstant(&chunk, 51, 6);

    writeChunk(&chunk, OP_RETURN, 8);

    disassembleChunk(&chunk, "test chunk");
    freeChunk(&chunk);
    return 0;
}
