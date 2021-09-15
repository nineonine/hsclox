#include <stdio.h>

#define PRINTARRAY(array, length) \
    for(int i = 0; i < length; i++) \
        printf("%d\t", array[i]);

#define ASSERT(condition, msg) \
    do { \
        if (!(condition)){ \
            printf("%s", msg); \
            abort(); \
        } \
    } while (false)
