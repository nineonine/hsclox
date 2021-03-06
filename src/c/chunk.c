#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"
#include "lines.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    chunk->linesCount = 0;
    initValueArray(&chunk->constants);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count+1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code,
            oldCapacity, chunk->capacity);
    }
    chunk->code[chunk->count] = byte;
    updateLineInfo(chunk, line);
    chunk->count++;
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

int addConstant(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}

void writeConstant(Chunk* chunk, Value value, int line) {
    writeValueArrayN(&chunk->constants, value, LONG_CONSTANT_SIZE);
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    writeChunk(chunk, chunk->constants.count - LONG_CONSTANT_SIZE, line);
}
