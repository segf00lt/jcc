#include <stdio.h>
#include <stdint.h>

int foo(uint64_t size, float f) {
    printf("foo is printing %f\n", f);
    return (int)(size >> 2);
}
