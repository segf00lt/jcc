#include <stdio.h>
#include <stdint.h>
#include "foo.h"

int foo(uint64_t size, float f) {
    printf("foo is printing %f\n", f);
    return (int)(size >> 2);
}

struct Bob bar(struct Bob b) {
    b.tau *= 2.0;
    return b;
}

struct Bob birth_bob(int i) {
    struct Bob b = {0};
    b.i = i;
    return b;
}
