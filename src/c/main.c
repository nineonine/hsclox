#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "utils.h"
#include "vm.h"

#include "examples.h"

static void constOp(Chunk* chunk, double i, int line) {
    int constant = addConstant(chunk, i);
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, constant, line);
}

int main(int argc, const char* argv[]) {
    initVM();
    Chunk chunk;
    initChunk(&chunk);

    // EXAMPLE_1_mul_2_plus_3;
    EXAMPLE_neg_2_plus_3_div_5;
    // EXAMPLE_1_mul_2_plus_3;
    // EXAMPLE_four_sub_3_mul_neg_2_WITHOUT_NEGATE;
    // EXAMPLE_four_sub_3_mul_neg_2_WITHOUT_SUBTRACT;

    // disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);
    return 0;
}
