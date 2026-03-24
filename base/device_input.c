#ifndef DEVICE_INPUT_C
#define DEVICE_INPUT_C

internal b32
func is_modifier_key(Keyboard_key key) {
  b32 result = (key >= KBD_KEY_LEFT_SHIFT && key <= KBD_KEY_LEFT_META);
  return result;
}

#endif
