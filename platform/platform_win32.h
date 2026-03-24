#ifndef PLATFORM_WIN32_H
#define PLATFORM_WIN32_H


// NOTE jfd 22/01/2026:
// This file contains the types and function headers for platform specific code.
// DO NOT CALL THESE FROM GAME OR APP CODE

#include <windows.h>
#include <Xinput.h>
#include <Dsound.h>
#include <intrin.h>
#include <objbase.h>


typedef struct Platform_win32_window_dimensions Platform_win32_window_dimensions;
struct Platform_win32_window_dimensions {
  s32 width;
  s32 height;
};

typedef struct Platform_win32_backbuffer Platform_win32_backbuffer;
struct Platform_win32_backbuffer {
  BITMAPINFO bitmap_info;
  u8 *bitmap_memory;
  s32 bitmap_height;
  s32 bitmap_width;
  u32 bytes_per_pixel;
  u32 stride;
};

typedef struct Platform_win32_sound_output Platform_win32_sound_output;
struct Platform_win32_sound_output {
  int samples_per_second;
  DWORD buffer_size;
  u32 running_sample_index; // TODO jfd: should this be in bytes instead?
  int bytes_per_sample;
  DWORD safety_bytes;
};

typedef struct Platform_win32_debug_time_marker Platform_win32_debug_time_marker;
struct Platform_win32_debug_time_marker {
  DWORD output_play_cursor;
  DWORD output_write_cursor;
  DWORD output_location;
  DWORD output_byte_count;
  DWORD expected_flip_cursor;
  DWORD flip_play_cursor;
  DWORD flip_write_cursor;
};

typedef struct Platform_win32_debug_loop_recorder Platform_win32_debug_loop_recorder;
struct Platform_win32_debug_loop_recorder {
  HANDLE           game_state_file_handle;
  HANDLE           game_state_file_map_handle;
  HANDLE           game_input_file_handle;
  Game_input_slice recorded_game_input;
  Str8             recorded_game_state;
  b32              recording_loop;
  b32              playing_loop;
  b32              stop_playing_loop;
  s64              input_recording_write_index;
  s64              input_recording_play_index;
  b32              write_game_state_to_file;
  b32              read_game_state_from_file;
};

#define PLATFORM_WIN32_XINPUT_SET_STATE(name) DWORD name(DWORD dwUserIndex, XINPUT_VIBRATION *pVibration)
typedef PLATFORM_WIN32_XINPUT_SET_STATE(Platform_win32_xinput_set_state_func);

PLATFORM_WIN32_XINPUT_SET_STATE(_platform_win32_xinput_set_state_stub) {
  return 0;
}

#define PLATFORM_WIN32_XINPUT_GET_STATE(name) DWORD name(DWORD dwUserIndex, XINPUT_STATE *pState)
typedef PLATFORM_WIN32_XINPUT_GET_STATE(Platform_win32_xinput_get_state_func);

PLATFORM_WIN32_XINPUT_GET_STATE(_platform_win32_xinput_get_state_stub) {
  return 0;
}

#define PLATFORM_WIN32_DIRECT_SOUND_CREATE(name) HRESULT WINAPI name(LPGUID lpGuid, LPDIRECTSOUND* ppDS, LPUNKNOWN  pUnkOuter)
typedef PLATFORM_WIN32_DIRECT_SOUND_CREATE(Platform_win32_direct_sound_create_func);
PLATFORM_WIN32_DIRECT_SOUND_CREATE(_platform_win32_direct_sound_create_stub) {
  return 0;
}

TYPEDEF_SLICE(Platform_win32_debug_time_marker);



internal LRESULT CALLBACK platform_win32_main_window_callback(HWND window, UINT message, WPARAM w_param, LPARAM l_param);

internal void platform_win32_resize_backbuffer(Platform_win32_backbuffer *backbuffer, int window_width, int window_height);

internal void platform_win32_display_buffer_in_window(Platform_win32_backbuffer *backbuffer, HDC device_context, int window_width, int window_height, int x, int y, int width, int height);

internal Platform_win32_window_dimensions platform_win32_get_window_dimensions(HWND window_handle);

internal void platform_win32_load_xinput(void);

internal Platform_event* platform_win32_event_push(Arena *a, Platform_event_list *event_list, Platform_event_kind event_kind);
internal Platform_event* platform_win32_event_pop(Platform_event_list *event_list);

internal Keyboard_key platform_win32_keyboard_key_from_virtual_keycode(WPARAM virtual_keycode);

internal void platform_win32_get_game_input_from_events(Platform_event_list *event_list, Game *gp);

force_inline LARGE_INTEGER platform_win32_get_wall_clock(void);

force_inline f32 platform_win32_get_seconds_elapsed(LARGE_INTEGER begin, LARGE_INTEGER end);


#endif
