#ifndef hsclox_examples_h
#define hsclox_examples_h

// 1 * 2 + 3
#define EXAMPLE_1_mul_2_plus_3 \
    do { \
        constOp(&chunk, 1, 1); \
        constOp(&chunk, 2, 1); \
        writeChunk(&chunk, OP_MULTIPLY, 1); \
        constOp(&chunk, 3, 1); \
        writeChunk(&chunk, OP_ADD, 1); \
        writeChunk(&chunk, OP_RETURN, 1); \
    } while (false)

// -((2 + 3) / 5)
#define EXAMPLE_neg_2_plus_3_div_5 \
    do { \
        constOp(&chunk, 2, 1); \
        constOp(&chunk, 3, 1); \
        writeChunk(&chunk, OP_ADD, 1); \
        constOp(&chunk, 5, 1); \
        writeChunk(&chunk, OP_DIVIDE, 1); \
        writeChunk(&chunk, OP_NEGATE, 1); \
        writeChunk(&chunk, OP_RETURN, 1); \
    } while (false)

// 4 - 3 * -2 without OP_NEGATE
#define EXAMPLE_four_sub_3_mul_neg_2_WITHOUT_NEGATE \
    do { \
        constOp(&chunk, 4, 1); \
        constOp(&chunk, 3, 1); \
        constOp(&chunk, 2, 1); \
        constOp(&chunk, 0, 1); \
        constOp(&chunk, 1, 1); \
        writeChunk(&chunk, OP_SUBTRACT, 1); \
        writeChunk(&chunk, OP_MULTIPLY, 1); \
        writeChunk(&chunk, OP_MULTIPLY, 1); \
        writeChunk(&chunk, OP_SUBTRACT, 1); \
        writeChunk(&chunk, OP_RETURN, 1); \
    } while (false)

// 4 - 3 * -2 without OP_SUBTRACT
#define EXAMPLE_four_sub_3_mul_neg_2_WITHOUT_SUBTRACT \
    do { \
        constOp(&chunk, 4, 1); \
        constOp(&chunk, 3, 1); \
        constOp(&chunk, 2, 1); \
        writeChunk(&chunk, OP_NEGATE, 1); \
        writeChunk(&chunk, OP_MULTIPLY, 1); \
        writeChunk(&chunk, OP_NEGATE, 1); \
        writeChunk(&chunk, OP_ADD, 1); \
        writeChunk(&chunk, OP_RETURN, 1); \
    } while (false)

#endif
