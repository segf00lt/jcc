#include "basic.h"

int fib(int n) {
    if(n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

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
    int N = fib(fib(8)) + fib(8);
    printf("%i\n",N);
    // 0 0 1 1 1 0 0 1 1 0 0 0 0 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 0 0
    // 1 0 0 0 0 0 1 1 1 0 1 1 0 0 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 1
    //
    // N = fib(21)
    // tape       1 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 1
    // next_tape  1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0
//N = fib(21) + fib(8)
//tape       1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 0 0 1 1
//next_tape  1 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0

    /*
       01 01 01 01 01 00 01 00 01 01 01 00 00 01 01 00 01 01 01 01 01 00
       00 00 01 00 00 01 01 00 01 00 00 00 01 01 01 01 01 00 01 00 01 01
       01 01 01 00 00 00 01 00 00 01 01 00 01 01 01 01 00

*/
    // tape       1 1 1 0 1 0 0 1 1 0 0 1 0 0 1 1 0 1 1 1 1 1 0 1 0 0 0 0 1 1
               // 1 1 1 0 1 0 0 1 1 0 0 1 0 0 1 1 0 1 1 1 1 1 0 1 0 0 0 0 1 1
               //
    // next_tape  1 0 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 0 0 0 0 0 1
               // 1 0 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 0 0 0 0 0 1

    int rules[8] = { 0, 1, 1, 1, 0, 1, 1, 0 };

    /*
    for(int i = 0; i < D; ++i) {
        printf("%d ", tape[i]);
    }
    printf("\n");
    */

    for(; N > 0; --N) {
        int ones_in_a_row = 0;
        for(int i = 0; i < D; ++i) {
            if(tape[i] == 1) ones_in_a_row++;

            if(ones_in_a_row == 4) {
                continue;
            } else if(ones_in_a_row == 3) {
                goto end_generation_loop;
            }

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
end_generation_loop:

    printf("tape       ");
    for(int i = 0; i < D; ++i) {
        printf("%d ", tape[i]);
    }
    printf("\n");

    printf("next_tape  ");
    for(int i = 0; i < D; ++i) {
        printf("%d ", next_tape[i]);
    }
    printf("\n");

    return 0;
}

