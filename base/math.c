#ifndef BASE_MATH_C
#define BASE_MATH_C

#include <math.h>

force_inline f32
func floor_f32(f32 f) {
  // TODO jfd: reimplement
  return (f32)floorf(f);
}


force_inline f64
func floor_f64(f64 f) {
  // TODO jfd: reimplement
  return (f64)floor(f);
}

force_inline f32
func round_f32(f32 f) {
  return (f32)roundf(f);
}

force_inline f32
func sqrt_f32(f32 f) {
  return (f32)sqrtf(f);
}

// TODO jfd: handle negative values in round_f32_to_s32()
force_inline s32
func round_f32_to_s32(f32 value) {
  s32 result = (s32)(value + 0.5f);
  return result;
}

force_inline f32
func max_f32(f32 a, f32 b) {
  // TODO jfd: reimplement
  return fmaxf(a, b);
}

force_inline f32
func abs_f32(f32 x) {
  u32 *bits = (u32*)&x;
  *bits &= ~(1 << 31);
  return x;
}

force_inline f32
func lerp_f32(f32 t, f32 begin, f32 end) {
  return t * (end - begin);
}

force_inline f32
func wrap_f32(f32 value, f32 min, f32 max)
{
    f32 range = max - min;
    f32 result = value - range*floorf((value - min)/range);
    return result;
}

force_inline v2
func round_v2(v2 v) {
  v2 result = {
    round_f32(v.x),
    round_f32(v.y),
  };
  return result;
}


force_inline v2
func wrap_v2(v2 v, v2 min, v2 max) {
  v2 result = {
    wrap_f32(v.x, min.x, max.x),
    wrap_f32(v.y, min.y, max.y),
  };
  return result;
}

force_inline v2
func max_v2(v2 a, v2 b) {
  v2 result = {
    max_f32(a.x, b.x),
    max_f32(a.y, b.y),
  };

  return result;
}

force_inline v2
func abs_v2(v2 v) {
  v2 result = {
    abs_f32(v.x),
    abs_f32(v.y),
  };
  return result;
}

force_inline v2
func truncate_v2(v2 v) {
  // TODO jfd: this kind of truncation is undefined behaviour
  v2 result = {
    (f32)(s32)v.x,
    (f32)(s32)v.y,
  };
  return result;
}

force_inline v2
func add_v2(v2 a, v2 b) {
  v2 result = {
    a.x + b.x,
    a.y + b.y,
  };
  return result;
}

force_inline v2
func sub_v2(v2 a, v2 b) {
  v2 result = {
    a.x - b.x,
    a.y - b.y,
  };
  return result;
}


force_inline v2
func mul_v2(v2 a, v2 b) {
  v2 result = {
    a.x * b.x,
    a.y * b.y,
  };
  return result;
}

force_inline f32
func dot_v2(v2 a, v2 b) {
  f32 result = a.x*b.x + a.y*b.y;
  return result;
}

force_inline f32
func len_sq_v2(v2 v) {
  f32 result = dot_v2(v, v);
  return result;
}

force_inline f32
func len_v2(v2 v) {
  f32 result = sqrt_f32(len_sq_v2(v));
  return result;
}

force_inline v2
func norm_v2(v2 v) {
  v2 result = scale_v2(v, 1.0f/len_v2(v));
  return result;
}

force_inline v2
func div_v2(v2 a, v2 b) {
  v2 result = {
    a.x / b.x,
    a.y / b.y,
  };
  return result;
}

force_inline v2
func scale_v2(v2 v, f32 a) {
  v2 result = {
    v.x * a,
    v.y * a,
  };

  return result;
}

force_inline v2
func lt_v2(v2 a, v2 b) {
  v2 result = {
    (f32)(a.x < b.x),
    (f32)(a.y < b.y),
  };
  return result;
}

force_inline v2
func gte_v2(v2 a, v2 b) {
  v2 result = {
    (f32)(a.x >= b.x),
    (f32)(a.y >= b.y),
  };
  return result;
}

force_inline v2
func lte_v2(v2 a, v2 b) {
  v2 result = {
    (f32)(a.x <= b.x),
    (f32)(a.y <= b.y),
  };
  return result;
}

force_inline v2
func gt_v2(v2 a, v2 b) {
  v2 result = {
    (f32)(a.x > b.x),
    (f32)(a.y > b.y),
  };
  return result;
}

force_inline v2
func add_value_v2(v2 v, f32 a) {
  v2 result = {
    v.x + a,
    v.y + a,
  };
  return result;
}

/////////////////////////////////////////
// v2_s16

force_inline v2_s16
func add_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x + b.x,
    a.y + b.y,
  };
  return result;
}

force_inline v2_s16
func sub_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x - b.x,
    a.y - b.y,
  };
  return result;
}

force_inline v2_s16
func mul_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x * b.x,
    a.y * b.y,
  };
  return result;
}

force_inline s16
func dot_v2_s16(v2_s16 a, v2_s16 b) {
  s16 result = a.x*b.x + a.y+b.y;
  return result;
}

force_inline v2_s16
func div_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x / b.x,
    a.y / b.y,
  };
  return result;
}

force_inline v2_s16
func scale_v2_s16(v2_s16 v, s16 a) {
  v2_s16 result = {
    v.x * a,
    v.y * a,
  };
  return result;
}

force_inline v2_s16
func lt_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x < b.x,
    a.y < b.y,
  };
  return result;
}

force_inline v2_s16
func gte_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x >= b.x,
    a.y >= b.y,
  };
  return result;
}

force_inline v2_s16
func lte_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x <= b.x,
    a.y <= b.y,
  };
  return result;
}

force_inline v2_s16
func gt_v2_s16(v2_s16 a, v2_s16 b) {
  v2_s16 result = {
    a.x > b.x,
    a.y > b.y,
  };
  return result;
}

force_inline v2_s16
func add_value_v2_s16(v2_s16 v, s16 a) {
  v2_s16 result = {
    v.x + a,
    v.y + a,
  };
  return result;
}

/////////////////////////////////////////
// v2_s32

force_inline v2_s32
func add_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x + b.x,
    a.y + b.y,
  };
  return result;
}

force_inline v2_s32
func sub_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x - b.x,
    a.y - b.y,
  };
  return result;
}

force_inline v2_s32
func mul_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x * b.x,
    a.y * b.y,
  };
  return result;
}

force_inline s32
func dot_v2_s32(v2_s32 a, v2_s32 b) {
  s32 result = a.x*b.x + a.y+b.y;
  return result;
}

force_inline v2_s32
func div_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x / b.x,
    a.y / b.y,
  };
  return result;
}

force_inline v2_s32
func scale_v2_s32(v2_s32 v, s32 a) {
  v2_s32 result = {
    v.x * a,
    v.y * a,
  };
  return result;
}

force_inline v2_s32
func lt_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x < b.x,
    a.y < b.y,
  };
  return result;
}

force_inline v2_s32
func gte_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x >= b.x,
    a.y >= b.y,
  };
  return result;
}

force_inline v2_s32
func lte_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x <= b.x,
    a.y <= b.y,
  };
  return result;
}

force_inline v2_s32
func gt_v2_s32(v2_s32 a, v2_s32 b) {
  v2_s32 result = {
    a.x > b.x,
    a.y > b.y,
  };
  return result;
}

force_inline v2_s32
func add_value_v2_s32(v2_s32 v, s32 a) {
  v2_s32 result = {
    v.x + a,
    v.y + a,
  };
  return result;
}


/////////////////////////////////////////
// v2_u32

force_inline v2_u32
func add_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x + b.x,
    a.y + b.y,
  };
  return result;
}

force_inline v2_u32
func sub_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x - b.x,
    a.y - b.y,
  };
  return result;
}

force_inline v2_u32
func mul_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x * b.x,
    a.y * b.y,
  };
  return result;
}

force_inline u32
func dot_v2_u32(v2_u32 a, v2_u32 b) {
  u32 result = a.x*b.x + a.y*b.y;
  return result;
}

force_inline v2_u32
func div_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x / b.x,
    a.y / b.y,
  };
  return result;
}

force_inline v2_u32
func scale_v2_u32(v2_u32 v, u32 a) {
  v2_u32 result = {
    v.x * a,
    v.y * a,
  };
  return result;
}

force_inline v2_u32
func lt_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x < b.x,
    a.y < b.y,
  };
  return result;
}

force_inline v2_u32
func gte_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x >= b.x,
    a.y >= b.y,
  };
  return result;
}

force_inline v2_u32
func lte_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x <= b.x,
    a.y <= b.y,
  };
  return result;
}

force_inline v2_u32
func gt_v2_u32(v2_u32 a, v2_u32 b) {
  v2_u32 result = {
    a.x > b.x,
    a.y > b.y,
  };
  return result;
}

force_inline v2_u32
func add_value_v2_u32(v2_u32 v, u32 a) {
  v2_u32 result = {
    v.x + a,
    v.y + a,
  };
  return result;
}


//////////////////////////////////////
// casting

force_inline v2_u32
func cast_v2_f32_to_u32(v2 v) {
  v2_u32 result = {
    (u32)v.x,
    (u32)v.y,
  };
  return result;
}


force_inline v2
func cast_v2_u32_to_f32(v2_u32 v) {
  v2 result = {
    (f32)v.x,
    (f32)v.y,
  };
  return result;
}


force_inline v2_s32
func cast_v2_u32_to_s32(v2_u32 v) {
  v2_s32 result = {
    (s32)v.x,
    (s32)v.y,
  };
  return result;
}

force_inline v2_s32
func cast_v2_f32_to_s32(v2 v) {
  v2_s32 result = {
    (s32)v.x,
    (s32)v.y,
  };
  return result;
}

force_inline v2
func cast_v2_s32_to_f32(v2_s32 v) {
  v2 result = {
    (f32)v.x,
    (f32)v.y,
  };
  return result;
}




#endif
