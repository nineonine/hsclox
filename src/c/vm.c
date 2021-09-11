#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "lines.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stack = GROW_ARRAY(Value, NULL, 0, STACK_MAX);
    vm.stackSize = STACK_MAX;
    vm.sp = vm.stack;
}

static void runTimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = findLine(vm.chunk, instruction);
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);
}
void freeVM() {
    freeTable(&vm.strings);
    freeObjects();
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

Value popN(uint8_t n) {
    vm.sp -= n;
    return *vm.sp;
}

Value pop() {
    vm.sp--;
    return *vm.sp;
}

static Value peek(int distance) {
    return vm.sp[-1 - distance];
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());
    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
    do { \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runTimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(pop()); \
        double a = AS_NUMBER(pop()); \
        push(valueType(a op b)); \
    } while (false)

    for(;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n");
    for (Value* slot = vm.stack; slot < vm.sp; slot++) {
        printf("[ ");
        printValue(*slot);
        printf(" ]");
    }
    printf("\n\n");
    disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_POPN: {
                uint8_t n = READ_BYTE();
                popN(n);
                break;
            }
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(vm.stack[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                vm.stack[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runTimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
#ifdef DEBUG_TRACE_EXECUTION
                printf("\nGlobals:\n");
                for (int i = 0; i < vm.globals.capacity; i++) {
                    Entry entry = vm.globals.entries[i];
                    if (entry.key != NULL) {
                        printf("  ");
                        printf("%s", entry.key->chars);
                        printf(" => ");
                        printValue(entry.value);
                        printf("\n");
                    }
                }
#endif
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runTimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_EQUAL: {
                Value a = pop();
                Value b = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runTimeError("operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                vm.sp[-1] = BOOL_VAL(isFalsey(vm.sp[-1]));
                break;
            case OP_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runTimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm.sp[-1] = NUMBER_VAL(-(AS_NUMBER(vm.sp[-1])));
                break;
            case OP_PRINT:
                printValue(pop());
                printf("\n");
                break;
            case OP_RETURN: {
                return INTERPRET_OK;
            }
        }
    }
#undef BINARY_OP
#undef READ_BYTE
#undef READ_STRING
#undef READ_CONSTANT
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    resetStack();

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
