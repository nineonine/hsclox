#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "object.h"
#include "value.h"
#include "vm.h"

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


static void defineNative(VM* vm, const char* name, int arity, NativeFn function) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function, arity)));
    tableSet(&vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
    popN(2);
}

void defineNatives(VM* vm) {
    defineNative(vm, "clock", 0, clockNative);
    defineNative(vm, "abs", 1, absNative);
    defineNative(vm, "getEnv", 1, getEnvNative);
}
