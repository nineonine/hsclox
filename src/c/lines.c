#include <assert.h>

#include "chunk.h"
#include "memory.h"

void updateLineInfo(Chunk* chunk, int line) {
    if (chunk->lines == NULL) {
        chunk->lines = GROW_ARRAY(int, chunk->lines, 0, 2);
        chunk->lines[0] = line;
        chunk->lines[1] = 1;
        chunk->linesCount = 2;
        return;
    } // TODO: this is ineficient. we can store idx of last inserted
    // to avoid traversing the whole list. This will only work if
    // incoming stream of lines is sorted
    for (int i = 0; i < chunk->linesCount; i+=2) {
        if (chunk->lines[i] == line) {
            chunk->lines[i+1] = chunk->lines[i+1] + 1;
            return;
        }
    }
    int lc = chunk->linesCount;
    chunk->lines = GROW_ARRAY(int, chunk->lines, lc, lc + 2);
    chunk->linesCount = lc + 2;
    chunk->lines[lc] = line;
    chunk->lines[lc+1] = 1;
}

// return -1 if line not found
int findLine(Chunk* chunk, int offset) {
    assert(offset>=0);
    if (chunk->lines == NULL) return -1;
    int lc = chunk->linesCount;
    for (int i = 0; i < lc; i+=2) {
        int numLines = chunk->lines[i+1];
        offset = offset - numLines;
        if (offset < 0) {
            return chunk->lines[i];
        }
    }
    return -1;
}
