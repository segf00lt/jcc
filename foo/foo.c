#include <stdio.h>
#include <stdint.h>
#include "foo.h"

int foo(uint64_t size, float f) {
    printf("foo is printing %f\n", f);
    return (int)(size >> 2);
}

double bar(struct Bob b) {
    return b.tau;
}
