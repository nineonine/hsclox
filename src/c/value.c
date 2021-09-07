#include <stdio.h>

#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

void writeValueArrayN(ValueArray* array, Value value, int i) {
    if (array->capacity < array->count+i) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values,
            oldCapacity, array->capacity);
    }
    array->values[array->count] = value;
    array->count+=i;
}

void writeValueArray(ValueArray* array, Value value) {
    writeValueArrayN(array, value, 1);
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    printf("%g", AS_NUMBER(value));
}
