#ifndef TEST_DYNCALL_C
#define TEST_DYNCALL_C


typedef struct Bob Bob;
struct Bob {
  u64 b;
  float x[3];
};


internal bool
func foo(float a, Bob bobby) {
  float result = a * bobby.x[2] - (float)bobby.b;

  if(result > 0) {
    return true;
  }

  return false;
}


int main(void) {
  Arena *arena = arena_create(MB(1));

  DCCallVM *vm = dcNewCallVM(4096);
  dcMode(vm, DC_CALL_C_DEFAULT);
  dcReset(vm);

  DCaggr *bob_aggr_1 = dcNewAggr(2, sizeof(Bob));
  dcAggrField(bob_aggr_1, DC_SIGCHAR_FLOAT, member_offset(Bob, x), 3);
  dcAggrField(bob_aggr_1, DC_SIGCHAR_ULONGLONG, member_offset(Bob, b), 1);
  dcCloseAggr(bob_aggr_1);

  DCaggr *bob_aggr_2 = dcNewAggr(4, sizeof(Bob));
  dcAggrField(bob_aggr_2, DC_SIGCHAR_FLOAT, member_offset(Bob, x), 1);
  dcAggrField(bob_aggr_2, DC_SIGCHAR_FLOAT, member_offset(Bob, x) + sizeof(float), 1);
  dcAggrField(bob_aggr_2, DC_SIGCHAR_FLOAT, member_offset(Bob, x) + 2*sizeof(float), 1);
  dcAggrField(bob_aggr_2, DC_SIGCHAR_ULONGLONG, member_offset(Bob, b), 1);
  dcCloseAggr(bob_aggr_2);

  Bob bobby = {
    .x = { 0.3f, 1.24f, 7.325f },
    .b = 1027,
  };

  dcArgFloat(vm, 50.3f);
  dcArgAggr(vm, bob_aggr_1, &bobby);
  b32 result_1 = dcCallChar(vm, (DCpointer)&foo);

  OutputDebugStringA(cstrf(arena, "1st result = %d\n", result_1));

  dcArgFloat(vm, 50.3f);
  dcArgAggr(vm, bob_aggr_2, &bobby);
  b32 result_2 = dcCallChar(vm, (DCpointer)&foo);

  OutputDebugStringA(cstrf(arena, "2st result = %d\n", result_2));

  return 0;
}



#endif
