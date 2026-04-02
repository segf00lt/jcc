#ifndef STR_C
#define STR_C

#define STB_SPRINTF_IMPLEMENTATION
#include "../third_party/stb/stb_sprintf.h"

#define str_vsnprintf stbsp_vsnprintf

//#if defined(OS_WEB)
//#include <stdio.h>
//#define jlib_str_vsnprintf vsnprintf
//#else
//#endif


internal force_inline void
func str8_list_append_node_(Str8_list *list, Str8_node *node) {
  sll_queue_push(list->first, list->last, node);
  list->count++;
  list->total_len += node->str.len;
}

internal Str8
func str8_cat(Arena *a, Str8 str1, Str8 str2) {
  Str8 result = {0};
  s64 len = str1.len + str2.len;

  if(len <= 0) {
    return result;
  }

  result.len = len;
  result.s = push_array_no_zero(a, u8, len + 1);

  memory_copy(result.s, str1.s, str1.len);
  memory_copy(result.s + str1.len, str2.s, str2.len);
  result.s[len] = 0;

  return result;
}

internal void
func str8_list_insert_first_str(Arena *a, Str8_list *list, Str8 str) {
  Str8_node *node = push_struct(a, Str8_node);
  node->str = str;
  sll_queue_push_front(list->first, list->last, node);
  list->count++;
  list->total_len += str.len;
}

internal void
func str8_list_append_str(Arena *a, Str8_list *list, Str8 str) {
  Str8_node *node = push_struct(a, Str8_node);
  node->str = str;
  sll_queue_push(list->first, list->last, node);
  list->count++;
  list->total_len += str.len;
}

internal Str8_list
func str8_list_copy(Arena *a, Str8_list list) {
  Str8_list result = {0};

  for(Str8_node *node = list.first; node; node = node->next) {
    Str8_node *new_node = push_array_no_zero(a, Str8_node, 1);
    new_node->str = node->str;
    new_node->next = 0;
    str8_list_append_node_(&result, new_node);
  }

  return result;
}

internal Str8
func str8_list_join(Arena *a, Str8_list list, Str8 sep) {
  Str8 result = {0};

  Str8_node *node = list.first;

  if(list.count <= 0) return result;
  if(!node) return result;

  s64 len = ((sep.len > 0) ? (list.count - 1) : (0)) * sep.len + list.total_len;

  result =
  (Str8) {
    .s = push_array_no_zero(a, u8, len),
    .len = len,
  };

  s64 len_copied = 0;

  while(node && node->str.len <= 0) {
    node = node->next;
  }

  if(node) {
    memory_copy(result.s + len_copied, node->str.s, node->str.len);
    len_copied += node->str.len;

    node = node->next;
  }

  for(; node; node = node->next) {
    if(node->str.len <= 0) continue;

    memory_copy(result.s + len_copied, sep.s, sep.len);
    len_copied += sep.len;

    memory_copy(result.s + len_copied, node->str.s, node->str.len);
    len_copied += node->str.len;
  }

  if(len > len_copied) {
    arena_pop(a, len - len_copied);
    result.len = len_copied;
  }

  result.s[len_copied] = 0;

  return result;
}

internal Str8
func str8_escaped(Arena *a, Str8 str) {
  Str8 result;
  u8 *data = push_array_no_zero(a, u8, (str.len + 1) << 1);
  result.s = data;

  for (s64 i = 0; i < str.len; i++) {
    switch (str.s[i]) {
    case '"':
      *data++ = '\\'; /* escape the control character. */
      *data++ = '"';
      break;
    case '\\':
      *data++ = '\\'; /* escape the control character. */
      *data++ = '\\';
      break;
    case '\b':
      *data++ = '\\'; /* escape the control character. */
      *data++ = 'b';
      break;
    case '\f':
      *data++ = '\\'; /* escape the control character. */
      *data++ = 'f';
      break;
    case '\n':
      *data++ = '\\'; /* escape the control character. */
      *data++ = 'n';
      break;
    case '\r':
      *data++ = '\\'; /* escape the control character. */
      *data++ = 'r';
      break;
    case '\t':
      *data++ = '\\'; /* escape the control character. */
      *data++ = 't';
      break;
    default:
      *data++ = str.s[i];
      break;
    }
  }

  result.len = (s64)(data - result.s);
  result.s[result.len] = 0;

  return result;

}

internal Str8
func str8_get_line(Str8 str, s64 start_pos) {
  Str8 line = str8_strip_whitespace(str8_get_line_no_strip(str, start_pos));
  return line;
}

internal Str8
func str8_get_line_no_strip(Str8 str, s64 start_pos) {
  Str8 line;
  line = str8_slice(str, start_pos, -1);
  line = str8_slice(line, 0, str8_find_char(line, '\n'));
  return line;
}

internal Str8
func str8_slice(Str8 str, s64 begin, s64 end) {
  /* NOTE slice end is exclusive */

  if(end < 0) {
    end = str.len;
  }

  end = MIN(end, str.len);

  s64 new_len = end - begin;

  Str8 result = { .s = str.s + MAX(begin, 0), .len = new_len };

  return result;
}

internal Str8
func str8_strip_whitespace(Str8 str) {
  Str8 result;
  s64 begin = 0;
  s64 end = str.len - 1;

  for(; begin < str.len; begin++) {
    if(!is_space(str.s[begin])) {
      break;
    }
  }

  for(; end >= 0; end--) {
    if(!is_space(str.s[end])) {
      end += 1;
      break;
    }
  }

  if(end < 0) {
    end = str.len;
  }

  result = str8_slice(str, begin, end);

  return result;
}

internal b32
func str8_match(Str8 a, Str8 b) {
  if(a.len != b.len) {
    return 0;
  } else {
    return (b32)(memory_compare(a.s, b.s, a.len) == 0);
  }
}

internal b32
func str8_contains(Str8 str, Str8 substr) {
  s64 found = str8_find(str, substr);
  b32 result = (found >= 0);
  return result;
}

internal s64
func str8_find(Str8 haystack, Str8 needle) {
  s64 found = -1;

  for(s64 i = 0; i < haystack.len - needle.len; i++) {
    for(s64 j = 0; j < needle.len; j++) {
      if(haystack.s[i+j] != needle.s[j]) {
        goto continue_outer;
      }
    }

    found = i;
    goto end;

    continue_outer:
    ;
  }

  end:

  return found;
}

internal s64
func str8_find_char(Str8 haystack, u8 needle) {
  s64 found = -1;

  for(s64 i = 0; i < haystack.len; i++) {
    if(haystack.s[i] != needle) {
      continue;
    }

    found = i;
    break;
  }

  return found;
}

internal s64
func str8_find_first_whitespace(Str8 haystack) {
  s64 found = -1;

  for(s64 i = 0; i < haystack.len; i++) {
    if(!is_space(haystack.s[i])) {
      continue;
    }

    found = i;
    break;
  }

  return found;
}

internal Str8_find_results
func str8_find_all_chars(Arena *a, Str8 haystack, u8 needle, Arena *output_arena) {
  Str8_find_results results = {0};

  ASSERT(a != output_arena);

  Arena_scope scope = arena_scope_begin(a);

  TYPEDEF_ARRAY(s64);

  s64_array begin_indexes;
  s64_array end_indexes;
  arr_init_ex(begin_indexes, a, 64);
  arr_init_ex(end_indexes, a, 64);

  for(s64 i = 0; i < haystack.len; i++) {
    if(haystack.s[i] != needle) {
      continue;
    }

    arr_push(begin_indexes, i);
    arr_push(end_indexes, i);
  }

  if(begin_indexes.count > 0) {
    ASSERT(end_indexes.count > 0);

    results.begin_indexes = push_array_no_zero(output_arena, s64, begin_indexes.count);
    memory_copy(results.begin_indexes, begin_indexes.d, begin_indexes.count * arr_stride(begin_indexes));

    results.end_indexes = push_array_no_zero(output_arena, s64, end_indexes.count);
    memory_copy(results.end_indexes, end_indexes.d, end_indexes.count * arr_stride(end_indexes));

    results.count = begin_indexes.count;
  }

  arena_scope_end(scope);

  return results;
}

internal Str8_find_results
func str8_find_all(Arena *a, Str8 haystack, Str8 needle, Arena *output_arena) {
  Str8_find_results results = {0};

  ASSERT(a != output_arena);

  Arena_scope scope = arena_scope_begin(a);

  TYPEDEF_ARRAY(s64);

  s64_array begin_indexes;
  s64_array end_indexes;
  arr_init_ex(begin_indexes, a, 64);
  arr_init_ex(end_indexes, a, 64);

  for(s64 i = 0; i < haystack.len - needle.len; i++) {
    for(s64 j = 0; j < needle.len; j++) {
      if(haystack.s[i+j] != needle.s[j]) {
        goto continue_outer;
      }
    }

    arr_push(begin_indexes, i);
    arr_push(end_indexes, i + needle.len);

    continue_outer:
    ;
  }

  if(begin_indexes.count > 0) {
    ASSERT(end_indexes.count > 0);

    results.begin_indexes = push_array_no_zero(output_arena, s64, begin_indexes.count);
    memory_copy(results.begin_indexes, begin_indexes.d, begin_indexes.count * arr_stride(begin_indexes));

    results.end_indexes = push_array_no_zero(output_arena, s64, end_indexes.count);
    memory_copy(results.end_indexes, end_indexes.d, end_indexes.count * arr_stride(end_indexes));

    results.count = begin_indexes.count;
  }

  arena_scope_end(scope);

  return results;
}

internal b32
func str8_starts_with(Str8 str, Str8 start) {
  b32 result = 0;

  if(str.len >= start.len) {
    Str8 str_start = str;
    str_start.len = start.len;
    result = str8_match(str_start, start);
  }

  return result;
}

internal b32
func str8_ends_with(Str8 str, Str8 end) {
  b32 result = 0;

  if(str.len >= end.len) {
    Str8 str_end =
    {
      .s = str.s + str.len - end.len,
      .len = end.len,
    };
    result = str8_match(str_end, end);
  }

  return result;
}

internal b32
func str8_is_cident(Str8 str) {
  b32 result = 1;

  if(!is_alpha(str.s[0]) && str.s[0] != '_') {
    result = 0;
  } else {

    for(int i = 1; i < str.len; i++) {
      if(!is_alpha(str.s[i]) && str.s[i] != '_' && !is_decimal(str.s[i])) {
        result = 0;
        break;
      }
    }

  }

  return result;
}

internal b32
func str8_is_decimal(Str8 str) {
  b32 result = 1;

  for(int i = 0; i < str.len; i++) {
    if(!is_decimal(str.s[i])) {
      result = 0;
      break;
    }
  }

  return result;
}

internal Str8
func str8_match_begin_int(Str8 str, int base) {
  Str8 result = {0};
  s64 i = 0;

  switch(base) {
    default:
    break;
    case 10:
    {
      for(; i < str.len && is_decimal(str.s[i]); i++);
    } break;
    case 2:
    {
      for(; i < str.len && is_binary(str.s[i]); i++);
    } break;
    case 16:
    {
      for(; i < str.len && is_hex(str.s[i]); i++);
    } break;
  }

  if(i > 0) {
    result.s = str.s;
    result.len = i;
  }

  return result;
}

internal u64
func str8_parse_int(Str8 str, int base) {
  u64 result;
  switch(base) {
    default:
    result = 0;
    break;
    case 10:
    result = str8_parse_int_decimal(str);
    break;
    case 2:
    result = str8_parse_int_binary(str);
    break;
    case 16:
    result = str8_parse_int_hex(str);
    break;
  }
  return result;
}

internal u64
func str8_parse_int_decimal(Str8 str) {
  u64 result = 0;

  for(int i = 0; i < str.len; i++) {
    result *= 10;
    result += str.s[i] - '0';
  }

  return result;
}

internal u64
func str8_parse_int_binary(Str8 str) {
  UNIMPLEMENTED;
  return 0;
}

internal u64
func str8_parse_int_hex(Str8 str) {
  UNIMPLEMENTED;
  return 0;
}


internal Str8
func str8_match_begin_float(Str8 str) {
  Str8 result = {0};
  UNIMPLEMENTED;
  return result;
}

internal f64
func str8_parse_float(Str8 str) {
  // NOTE jfd 14/02/26: This was written by gpt, I may try and optimize it in the future

  u8 *p = str.s;
  u8 *end = str.s + str.len;

  // 1) Skip leading whitespace
  while(p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
    p++;
  }

  // 2) Sign
  int sign = 1;
  if(p < end) {
    if(*p == '-') { sign = -1; p++; }
    else if(*p == '+') { p++; }
  }

  // 3) Integer part
  f64 value = 0.0;
  while(p < end && *p >= '0' && *p <= '9') {
    value = value * 10.0 + (f64)(*p - '0');
    p++;
  }

  // 4) Fractional part
  if(p < end && *p == '.') {
    p++;

    f64 frac_scale = 0.1;
    while(p < end && *p >= '0' && *p <= '9') {
      value += (f64)(*p - '0') * frac_scale;
      frac_scale *= 0.1;
      p++;
    }
  }

  // 5) Exponent
  if(p < end && (*p == 'e' || *p == 'E')) {
    p++;

    int exp_sign = 1;
    if(p < end) {
      if(*p == '-') { exp_sign = -1; p++; }
      else if(*p == '+') { p++; }
    }

    int exp_value = 0;
    while(p < end && *p >= '0' && *p <= '9') {
      exp_value = exp_value * 10 + (*p - '0');
      p++;
    }

    int final_exp = exp_sign * exp_value;

    // Apply exponent using pow10
    f64 pow10 = 1.0;
    f64 base = 10.0;
    int abs_exp = final_exp < 0 ? -final_exp : final_exp;

    while(abs_exp) {
      if(abs_exp & 1) {
        pow10 *= base;
      }
      base *= base;
      abs_exp >>= 1;
    }

    if(final_exp < 0) {
      value /= pow10;
    } else {
      value *= pow10;
    }
  }

  return sign * value;
}

internal Str8
func str8_copy(Arena *a, Str8 str) {
  u8 *s = push_array_no_zero(a, u8, str.len + 1);
  memory_copy(s, str.s, str.len);
  s[str.len] = 0;
  return (Str8) { .s = s, .len = str.len };
}

internal force_inline Str8
func str8_from_cstr(Arena *a, char *cstr) {
  Str8 str = {0};
  if(cstr) {
    str = (Str8){ .s = (u8*)cstr, .len = memory_strlen(cstr) };
  }
  return str8_copy(a, str);
}

internal force_inline char*
func cstr_from_str8(Arena *a, Str8 str) {
  Str8 s_ = str8_copy(a, str);
  char *s = (char*)s_.s;
  return s;
}

internal Str8
func str8fv(Arena *a, char *fmt, va_list args) {
  va_list args2;
  va_copy(args2, args);
  u32 needed_bytes = str_vsnprintf(0, 0, fmt, args) + 1;
  Str8 result = {0};
  result.s = (u8*)arena_push(a, sizeof(u8) * needed_bytes, alignof(u8));
  result.len = str_vsnprintf((char*)result.s, needed_bytes, fmt, args2);
  result.s[result.len] = 0;
  va_end(args2);
  return result;
}

internal Str8
func str8f(Arena *a, char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  Str8 result = str8fv(a, fmt, args);
  va_end(args);
  return result;
}

internal char*
func cstrf(Arena *a, char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  Str8 result = str8fv(a, fmt, args);
  va_end(args);
  return (char*)(result.s);
}

internal Str8
func str8_to_lower(Arena *a, Str8 str) {
  Str8 lower_str = str8_copy(a, str);

  for(int i = 0; i < lower_str.len; i++) {
    u8 c = lower_str.s[i];
    lower_str.s[i] = to_lower(c);
  }

  return lower_str;
}

internal Str8
func str8_to_upper(Arena *a, Str8 str) {
  Str8 upper_str = str8_copy(a, str);

  for(int i = 0; i < upper_str.len; i++) {
    u8 c = upper_str.s[i];
    upper_str.s[i] = to_upper(c);
  }

  return upper_str;
}

internal Str8
func str8_chop_last_slash(Str8 str) {
  if(str.len > 0) {
    u8 *ptr = str.s + str.len - 1;
    for(;ptr >= str.s; ptr -= 1) {
      if(*ptr == '/' || *ptr == '\\')
      {
        break;
      }
    }
    if(ptr >= str.s) {
      str.len = (u64)(ptr - str.s);
    }
    else
    {
      str.len = 0;
    }
  }
  return str;
}

internal Str8_list
func str8_split_by_chars(Arena *a, Str8 str, u8 *sep_chars, s64 n_sep_chars) {
  Str8_list result = {0};
  Str8_node head = {0};
  Str8_node *node = &head;

  s64 begin = 0;

  s64 i = 0;
  for(;i < str.len;) {
    b8 found_match = 0;

    for(s64 j = 0; j < n_sep_chars; j++) {
      if(str.s[i] == sep_chars[j]) {
        found_match = 1;
        break;
      }
    }

    if(found_match) {
      if(i == 0) {
        i += 1;
        begin = i;
      } else {
        node->next = push_array_no_zero(a, Str8_node, 1);
        node = node->next;
        node->str.s = str.s + begin;
        node->str.len = i - begin;
        node->next = 0;
        i += 1;
        begin = i;

        result.count++;
        result.total_len += node->str.len;
      }

    } else {
      i++;
    }

  }

  if(begin < i) {
    node->next = push_array_no_zero(a, Str8_node, 1);
    node = node->next;
    node->str.s = str.s + begin;
    node->str.len = i - begin;
    node->next = 0;

    result.count++;
    result.total_len += node->str.len;
  }

  result.first = head.next;
  result.last = node;

  return result;
}

internal force_inline Str8_list
func str8_split_by_char(Arena *a, Str8 str, u8 sep_char) {
  return str8_split_by_chars(a, str, &sep_char, 1);
}

internal Str8_list
func str8_split_by_str(Arena *a, Str8 str, Str8 sep) {
  Str8_list result = {0};
  Str8_node head = {0};
  Str8_node *node = &head;

  s64 begin = 0;

  s64 i = 0;
  for(;i < str.len;) {
    s64 j = 0;
    b8 found_match = 1;

    for(;j < sep.len && i+j < str.len; j++) {
      if(str.s[i+j] != sep.s[j]) {
        found_match = 0;
        break;
      }
    }

    if(found_match) {
      if(i == 0) {
        i += j;
        begin = i;
      } else {
        node->next = push_array_no_zero(a, Str8_node, 1);
        node = node->next;
        node->str.s = str.s + begin;
        node->str.len = i - begin;
        node->next = 0;
        i += j;
        begin = i;

        result.count++;
        result.total_len += node->str.len;
      }

    } else {
      i++;
    }

  }

  if(begin < i) {
    node->next = push_array_no_zero(a, Str8_node, 1);
    node = node->next;
    node->str.s = str.s + begin;
    node->str.len = i - begin;
    node->next = 0;

    result.count++;
    result.total_len += node->str.len;
  }

  result.first = head.next;
  result.last = node;

  return result;
}

internal Str8
func str8_cstr_capped(void *cstr, void *cap) {
  char *ptr = (char *)cstr;
  char *opl = (char *)cap;
  for(;ptr < opl && *ptr != 0; ptr += 1);
  u64 size = (u64)(ptr - (char *)cstr);
  Str8 result = (Str8){ (u8*)cstr, size };
  return result;
}

#endif
