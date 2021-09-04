#ifndef hsclox_lines_h
#define hsclox_lines_h

#include "chunk.h"

void updateLineInfo(Chunk* chunk, int line);
int findLine(Chunk* chunk, int offset);

#endif
