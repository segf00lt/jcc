#ifndef ARRAY_C
#define ARRAY_C

internal void
func arr_init_(__Arr_header *arr, Arena *arena, s64 stride, s64 cap) {
  arr->count = 0;
  arr->cap = cap;
  arr->arena = arena;
  arr->d = arena_push(arena, cap * stride, 1);
  memory_zero(arr->d, cap * stride);
}

internal void
func slice_init_(__Slice_header *slice, Arena *arena, s64 stride, s64 count) {
  slice->count = count;
  slice->d = arena_push(arena, count * stride, 1);
  memory_zero(slice->d, count * stride);
}

internal void*
func arr_push_no_zero_(__Arr_header *arr, s64 stride, s64 push_count) {
  ASSERT(arr->d && arr->cap && arr->arena);

  if(arr->count + push_count >= arr->cap) {
    s64 new_cap = arr->cap << 1;

    while(new_cap < arr->count + push_count) {
      new_cap <<= 1;
    }

    void *new_d = arena_push(arr->arena, new_cap * stride, 1);
    memory_copy(new_d, arr->d, stride * arr->count);
    memory_zero((u8*)new_d + stride*arr->count, stride*push_count);
    arr->d = new_d;
    arr->cap = new_cap;
  }

  void *result = (u8*)(arr->d) + arr->count;
  arr->count += push_count;

  return result;
}

#endif
