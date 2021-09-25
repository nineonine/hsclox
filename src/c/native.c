#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "object.h"
#include "value.h"
#include "vm.h"

#define DEFINE_NATIVE_0(vm, name, arity, function) \
    defineNative(vm, name, arity, function, NULL)
#define DEFINE_NATIVE_1(vm, name, arity, function, ty1) \
    do { \
        ValueType argTys[] = { ty1 }; \
        defineNative(vm, name, arity, function, argTys); \
    } while (false)

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value absNative(int argCount, Value* args) {
    return NUMBER_VAL(fabs(AS_NUMBER(args[0])));
}

static Value getEnvNative(int argCount, Value* args) {
    char* val = getenv(AS_CSTRING(args[0]));
    if (val == NULL) {
        return NIL_VAL;
    } else {
        return OBJ_VAL(copyString(val, (int)strlen(val)));
    }
}

static void defineNative(VM* vm, const char* name, int arity, NativeFn function,
                ValueType argTypes[]) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function, arity, argTypes)));
    tableSet(&vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
    popN(2);
}

void defineNatives(VM* vm) {
    DEFINE_NATIVE_0(vm, "clock", 0, clockNative);
    DEFINE_NATIVE_1(vm, "abs", 1, absNative, VAL_NUMBER);
    DEFINE_NATIVE_1(vm, "getEnv", 1, getEnvNative, VAL_OBJ);
}
