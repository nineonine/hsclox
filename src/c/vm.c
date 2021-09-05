#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "memory.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stack = GROW_ARRAY(Value, NULL, 0, STACK_MAX);
    vm.stackSize = STACK_MAX;
    vm.sp = vm.stack;
}

void initVM() {
    resetStack();
}
void freeVM() {

}

static void growStack() {
    int newSize = GROW_CAPACITY(vm.stackSize);
    printf("GROW STACK. NEW SIZE => %i\n", newSize);
    vm.stack = GROW_ARRAY(Value, vm.stack, vm.stackSize, newSize);
    vm.stackSize = newSize;
}

void push(Value value) {
    int oldSize = vm.stackSize;
    if (vm.sp - vm.stack+1 > oldSize) { // stack overflow check
        growStack();
        vm.sp = vm.stack + oldSize; // update SP
    }
    *vm.sp = value;
    vm.sp++;
}
Value pop() {
    vm.sp--;
    return *vm.sp;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(a op b); \
    } while (false)

    for(;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("            ");
    for (Value* slot = vm.stack; slot < vm.sp; slot++) {
        printf("[ ");
        printValue(*slot);
        printf(" ]");
    }
    printf("\n");
    disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            case OP_NEGATE:   push(-pop()); break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }
#undef BINARY_OP
#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}
