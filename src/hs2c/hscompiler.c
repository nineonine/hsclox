#include <stdlib.h>
#include "HsFFI.h"
#include "Compiler_stub.h"

void hsCompilerInit(void)
{
    int argc = 2;
    char *argv[] = { (char *)"+RTS", (char *)"-A32m", NULL };
    char **pargv = argv;

    // Initialize Haskell runtime
    hs_init(&argc, &pargv);
}

void hsCompilerExit(void)
{
    hs_exit();
}

void compile(void)
{
    compileFromHs();
}
