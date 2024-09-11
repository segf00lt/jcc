#include <stdint.h>

struct Bob {
    char c;
    double tau;
    int i;
};

int foo(uint64_t, float);
struct Bob bar(struct Bob);
struct Bob birth_bob(int i);
