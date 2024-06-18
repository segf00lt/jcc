#include "basic.h"

int main(void) {
    bool buf1[] = {
        1,1,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1
    };

    bool buf2[] = {
        1,1,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1
    };

    int D = STATICARRLEN(buf1);

    bool *tape = buf1;
    bool *next_tape = buf2;
    int N = 32;

    int rules[8] = { 0, 1, 1, 1, 0, 1, 1, 0 };

    /*
    for(int i = 0; i < D; ++i) {
        printf("%d ", tape[i]);
    }
    printf("\n");
    */

    for(; N > 0; --N) {
        for(int i = 0; i < D; ++i) {
            int acc =
                (tape[((i == 0) ? (D - 1) : (i - 1))] << 2) |
                (tape[i]<< 1)            |
                (tape[((i == D - 1) ? 0 : (i + 1))]);

            next_tape[i] = rules[acc];
        }
        bool *tmp = tape;
        tape = next_tape;
        next_tape = tmp;
    }

    for(int i = 0; i < D; ++i) {
        printf("%d ", tape[i]);
    }
    printf("\n");

    return 0;
}

