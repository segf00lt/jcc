#ifndef PLATFORM_WIN32_C
#define PLATFORM_WIN32_C


// NOTE jfd 22/01/2026:
// This file contains the global variables and function implementations for platform specific code.
// DO NOT CALL THESE FROM GAME OR APP CODE

/////////////////////////////////
// globals

#define GAME_MEMORY_BACKBUFFER_SIZE MB(100)

global Platform_win32_debug_loop_recorder debug_loop_recorder;

global b32 debug_paused;

global Platform platform;

global Arena *platform_main_arena;
global Arena *platform_debug_arena;
global Arena *platform_temp_arena;
global Arena *platform_event_arena;
global Arena *platform_file_arena;

global Platform_win32_xinput_set_state_func *platform_win32_xinput_set_state;
global Platform_win32_xinput_get_state_func *platform_win32_xinput_get_state;

global b32 platform_is_running;
global Platform_win32_backbuffer global_backbuffer;

global Platform_win32_sound_output _platform_sound_output_stub;
global Platform_win32_sound_output *platform_sound_output;

global LPDIRECTSOUNDBUFFER platform_sound_buffer; // this is what we write to

global Platform_event_list _platform_event_list_stub;
global Platform_event_list *platform_event_list;

global s64 platform_win32_perf_counter_frequency;
global b32 sleep_is_granular;

global HDC refresh_rate_query_device_context;
global int monitor_refresh_hz;
global int platform_win32_refresh_rate;

global f32 game_update_hz;
global f32 platform_target_seconds_per_frame;


global int debug_time_marker_index;
global Platform_win32_debug_time_marker_slice debug_time_markers;

global LARGE_INTEGER last_counter;
global LARGE_INTEGER flip_wall_clock;

global b32 debug_show_cursor;

global WINDOWPLACEMENT global_window_position = { sizeof(global_window_position) };


#ifdef HANDMADE_PROFILE
global u64 last_cycle_count
#endif

#ifdef HANDMADE_AUDIO_LATENCY_DEBUG
global DWORD audio_latency_bytes;
global f32 audio_latency_seconds;
#endif
global b32 sound_is_valid;
global s16 *samples;


#ifdef HANDMADE_HOTRELOAD
typedef Game* Game_init_proc(Platform *);
typedef void  Game_service_proc(Game *);
HMODULE            game_module;
Game_init_proc    *game_init;
Game_service_proc *game_update_and_render;
Game_service_proc *game_get_sound_samples;
#endif


/////////////////////////////////
// functions



internal void
func platform_win32_toggle_fullscreen(HWND window_handle) {

  DWORD style = GetWindowLong(window_handle, GWL_STYLE);
  if(style & WS_OVERLAPPEDWINDOW) {

    MONITORINFO monitor_info = { sizeof(monitor_info) };

    if(GetWindowPlacement(window_handle, &global_window_position) &&
      GetMonitorInfo(MonitorFromWindow(window_handle,
        MONITOR_DEFAULTTOPRIMARY), &monitor_info)
    ) {
      SetWindowLong(window_handle, GWL_STYLE, style & ~WS_OVERLAPPEDWINDOW);
      SetWindowPos(window_handle, HWND_TOP,
        monitor_info.rcMonitor.left, monitor_info.rcMonitor.top,
        monitor_info.rcMonitor.right - monitor_info.rcMonitor.left,
        monitor_info.rcMonitor.bottom - monitor_info.rcMonitor.top,
        SWP_NOOWNERZORDER | SWP_FRAMECHANGED
      );
    }

  } else {

    SetWindowLong(window_handle, GWL_STYLE, style | WS_OVERLAPPEDWINDOW);
    SetWindowPlacement(window_handle, &global_window_position);
    SetWindowPos(window_handle, 0, 0, 0, 0, 0,
      SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER |
      SWP_NOOWNERZORDER | SWP_FRAMECHANGED
    );

  }

}

internal Platform_win32_window_dimensions
func platform_win32_get_window_dimensions(HWND window_handle) {
  RECT client_rect;
  GetClientRect(window_handle, &client_rect);

  Platform_win32_window_dimensions window_dimensions = {
    .width = client_rect.right - client_rect.left,
    .height = client_rect.bottom - client_rect.top,
  };

  return window_dimensions;
}

internal Platform_win32_window_dimensions
func platform_win32_clear_window_and_get_dimensions(HWND window_handle, HDC device_context) {
  RECT client_rect;
  GetClientRect(window_handle, &client_rect);

  Platform_win32_window_dimensions window_dimensions = {
    .width = client_rect.right - client_rect.left,
    .height = client_rect.bottom - client_rect.top,
  };

  HBRUSH black = (HBRUSH)GetStockObject(DKGRAY_BRUSH);
  FillRect(device_context, &client_rect, black);

  return window_dimensions;
}

internal void
func platform_win32_resize_backbuffer(Platform_win32_backbuffer *backbuffer, int window_width, int window_height) {
  int width = 960;
  int height = 540;

  // TODO jfd: bulletproof this
  // maybe don't free first, free after, then free first if that fails

  // TODO jfd: free our DIBSection

  if(backbuffer->bitmap_memory) {
    platform_free(backbuffer->bitmap_memory);
  }

  backbuffer->bitmap_width = width;
  backbuffer->bitmap_height = height;

  backbuffer->bitmap_info.bmiHeader.biSize = sizeof(backbuffer->bitmap_info.bmiHeader);
  backbuffer->bitmap_info.bmiHeader.biWidth = backbuffer->bitmap_width;
  backbuffer->bitmap_info.bmiHeader.biHeight = -backbuffer->bitmap_height;
  backbuffer->bitmap_info.bmiHeader.biPlanes = 1;
  backbuffer->bitmap_info.bmiHeader.biBitCount = 32;
  backbuffer->bitmap_info.bmiHeader.biCompression = BI_RGB; // pixel format

  u64 bitmap_memory_size = backbuffer->bitmap_width*backbuffer->bitmap_height * backbuffer->bytes_per_pixel;
  backbuffer->bitmap_memory = platform_alloc(bitmap_memory_size);
  memory_zero(backbuffer->bitmap_memory, bitmap_memory_size);

  backbuffer->stride = backbuffer->bitmap_width * backbuffer->bytes_per_pixel;
}

internal void
func platform_win32_display_buffer_in_window(
  Platform_win32_backbuffer *backbuffer,
  HDC device_context,
  int window_width,
  int window_height,
  int x,
  int y,
  int width,
  int height
) {

  int dest_x = (window_width - backbuffer->bitmap_width) >> 1;
  int dest_y = (window_height - backbuffer->bitmap_height) >> 1;

  StretchDIBits(
    device_context,
    // 0, 0, window_width, window_height, // destination coordinates
    dest_x, dest_y, backbuffer->bitmap_width, backbuffer->bitmap_height, // destination coordinates
    0, 0, backbuffer->bitmap_width, backbuffer->bitmap_height,
    backbuffer->bitmap_memory,
    &backbuffer->bitmap_info,
    DIB_RGB_COLORS,
    SRCCOPY
  );

}


internal Platform_event*
func platform_win32_event_push(Arena *a, Platform_event_list *event_list, Platform_event_kind event_kind) {
  Platform_event *event = push_struct_no_zero(a, Platform_event);
  event->kind = event_kind;
  event->modifier_mask = platform_get_keyboard_modifiers();
  sll_queue_push(event_list->first, event_list->last, event);
  event_list->count++;
  return event;
}


internal Platform_event*
func platform_win32_event_pop(Platform_event_list *event_list) {
  Platform_event *event = event_list->last;
  sll_queue_pop(event_list->first, event_list->last);
  event_list->count--;
  return event;
}


internal Mouse_button
func platform_win32_mouse_button_from_virtual_keycode(WPARAM virtual_keycode) {
  Mouse_button button = 0;

  switch(virtual_keycode) {
    case VK_LBUTTON /* 0x01	Left mouse button */:
    button = MOUSE_BTN_LEFT;
    break;
    case VK_RBUTTON /* 0x02	Right mouse button */:
    button = MOUSE_BTN_RIGHT;
    break;
    case VK_MBUTTON /* 0x04	Middle mouse button */:
    button = MOUSE_BTN_MIDDLE;
    break;
  }

  return button;
}

internal Keyboard_key
func platform_win32_keyboard_key_from_virtual_keycode(WPARAM virtual_keycode) {
  Keyboard_key key = 0;

  switch(virtual_keycode) {
    case VK_CANCEL /* 0x03	Control-break processing */:
    break;
    case VK_XBUTTON1 /* 0x05	X1 mouse button */:
    break;
    case VK_XBUTTON2 /* 0x06	X2 mouse button */:
    break;
    // 0x07	Reserved
    case VK_BACK /* 0x08	Backspace key */:
    key = KBD_KEY_BACKSPACE;
    break;
    case VK_TAB /* 0x09	Tab key */:
    key = KBD_KEY_TAB;
    break;
    // 0x0A-0B	Reserved
    case VK_CLEAR /* 0x0C	Clear key */:
    break;
    case VK_RETURN /* 0x0D	Enter key */:
    key = KBD_KEY_ENTER;
    break;
    // 0x0E-0F	Unassigned
    case VK_SHIFT /* 0x10	Shift key */:
    key = KBD_KEY_LEFT_SHIFT;
    break;
    case VK_CONTROL /* 0x11	Ctrl key */:
    key = KBD_KEY_LEFT_CONTROL;
    break;
    case VK_MENU /* 0x12	Alt key */:
    key = KBD_KEY_LEFT_ALT;
    break;
    case VK_PAUSE /* 0x13	Pause key */:
    break;
    case VK_CAPITAL /* 0x14	Caps lock key */:
    key = KBD_KEY_CAPS_LOCK;
    break;
    case VK_ESCAPE /* 0x1B	Esc key */:
    key = KBD_KEY_ESCAPE;
    break;
    case VK_SPACE /* 0x20	Spacebar key */:
    key = KBD_KEY_SPACE;
    break;
    case VK_PRIOR /* 0x21	Page up key */:
    key = KBD_KEY_PAGE_UP;
    break;
    case VK_NEXT /* 0x22	Page down key */:
    key = KBD_KEY_PAGE_DOWN;
    break;
    case VK_END /* 0x23	End key */:
    key = KBD_KEY_END;
    break;
    case VK_HOME /* 0x24	Home key */:
    key = KBD_KEY_HOME;
    break;
    case VK_LEFT /* 0x25	Left arrow key */:
    key = KBD_KEY_LEFT_ARROW;
    break;
    case VK_UP /* 0x26	Up arrow key */:
    key = KBD_KEY_UP_ARROW;
    break;
    case VK_RIGHT /* 0x27	Right arrow key */:
    key = KBD_KEY_RIGHT_ARROW;
    break;
    case VK_DOWN /* 0x28	Down arrow key */:
    key = KBD_KEY_DOWN_ARROW;
    break;
    case VK_SNAPSHOT /* 0x2C	Print screen key */:
    key = KBD_KEY_PRINT_SCREEN;
    break;
    case VK_INSERT /* 0x2D	Insert key */:
    key = KBD_KEY_INSERT;
    break;
    case VK_DELETE /* 0x2E	Delete key */:
    key = KBD_KEY_DELETE;
    break;
    case 0x30 /* 0 key */:
    key = KBD_KEY_0;
    break;
    case 0x31 /* 1 key */:
    key = KBD_KEY_1;
    break;
    case 0x32 /* 2 key */:
    key = KBD_KEY_2;
    break;
    case 0x33 /* 3 key */:
    key = KBD_KEY_3;
    break;
    case 0x34 /* 4 key */:
    key = KBD_KEY_4;
    break;
    case 0x35 /* 5 key */:
    key = KBD_KEY_5;
    break;
    case 0x36 /* 6 key */:
    key = KBD_KEY_6;
    break;
    case 0x37 /* 7 key */:
    key = KBD_KEY_7;
    break;
    case 0x38 /* 8 key */:
    key = KBD_KEY_8;
    break;
    case 0x39 /* 9 key */:
    key = KBD_KEY_9;
    break;
    // 0x3A-40	Undefined
    case 0x41 /* A key */:
    key = KBD_KEY_A;
    break;
    case 0x42 /* B key */:
    key = KBD_KEY_B;
    break;
    case 0x43 /* C key */:
    key = KBD_KEY_C;
    break;
    case 0x44 /* D key */:
    key = KBD_KEY_D;
    break;
    case 0x45 /* E key */:
    key = KBD_KEY_E;
    break;
    case 0x46 /* F key */:
    key = KBD_KEY_F;
    break;
    case 0x47 /* G key */:
    key = KBD_KEY_G;
    break;
    case 0x48 /* H key */:
    key = KBD_KEY_H;
    break;
    case 0x49 /* I key */:
    key = KBD_KEY_I;
    break;
    case 0x4A /* J key */:
    key = KBD_KEY_J;
    break;
    case 0x4B /* K key */:
    key = KBD_KEY_K;
    break;
    case 0x4C /* L key */:
    key = KBD_KEY_L;
    break;
    case 0x4D /* M key */:
    key = KBD_KEY_M;
    break;
    case 0x4E /* N key */:
    key = KBD_KEY_N;
    break;
    case 0x4F /* O key */:
    key = KBD_KEY_O;
    break;
    case 0x50 /* P key */:
    key = KBD_KEY_P;
    break;
    case 0x51 /* Q key */:
    key = KBD_KEY_Q;
    break;
    case 0x52 /* R key */:
    key = KBD_KEY_R;
    break;
    case 0x53 /* S key */:
    key = KBD_KEY_S;
    break;
    case 0x54 /* T key */:
    key = KBD_KEY_T;
    break;
    case 0x55 /* U key */:
    key = KBD_KEY_U;
    break;
    case 0x56 /* V key */:
    key = KBD_KEY_V;
    break;
    case 0x57 /* W key */:
    key = KBD_KEY_W;
    break;
    case 0x58 /* X key */:
    key = KBD_KEY_X;
    break;
    case 0x59 /* Y key */:
    key = KBD_KEY_Y;
    break;
    case 0x5A /* Z key */:
    key = KBD_KEY_Z;
    break;
    case VK_LWIN /* 0x5B	Left Windows logo key */:
    key = KBD_KEY_LEFT_META;
    break;
    case VK_RWIN /* 0x5C	Right Windows logo key */:
    key = KBD_KEY_RIGHT_META;
    break;
    // 0x5E	Reserved
    case VK_NUMPAD0 /* 0x60	Numeric keypad 0 key */:
    key = KBD_KEY_0;
    break;
    case VK_NUMPAD1 /* 0x61	Numeric keypad 1 key */:
    key = KBD_KEY_1;
    break;
    case VK_NUMPAD2 /* 0x62	Numeric keypad 2 key */:
    key = KBD_KEY_2;
    break;
    case VK_NUMPAD3 /* 0x63	Numeric keypad 3 key */:
    key = KBD_KEY_3;
    break;
    case VK_NUMPAD4 /* 0x64	Numeric keypad 4 key */:
    key = KBD_KEY_4;
    break;
    case VK_NUMPAD5 /* 0x65	Numeric keypad 5 key */:
    key = KBD_KEY_5;
    break;
    case VK_NUMPAD6 /* 0x66	Numeric keypad 6 key */:
    key = KBD_KEY_6;
    break;
    case VK_NUMPAD7 /* 0x67	Numeric keypad 7 key */:
    key = KBD_KEY_7;
    break;
    case VK_NUMPAD8 /* 0x68	Numeric keypad 8 key */:
    key = KBD_KEY_8;
    break;
    case VK_NUMPAD9 /* 0x69	Numeric keypad 9 key */:
    key = KBD_KEY_9;
    break;

    case VK_F1 /* 0x70	F1 key */:
    key = KBD_KEY_F1;
    break;
    case VK_F2 /* 0x71	F2 key */:
    key = KBD_KEY_F2;
    break;
    case VK_F3 /* 0x72	F3 key */:
    key = KBD_KEY_F3;
    break;
    case VK_F4 /* 0x73	F4 key */:
    key = KBD_KEY_F4;
    break;
    case VK_F5 /* 0x74	F5 key */:
    key = KBD_KEY_F5;
    break;
    case VK_F6 /* 0x75	F6 key */:
    key = KBD_KEY_F6;
    break;
    case VK_F7 /* 0x76	F7 key */:
    key = KBD_KEY_F7;
    break;
    case VK_F8 /* 0x77	F8 key */:
    key = KBD_KEY_F8;
    break;
    case VK_F9 /* 0x78	F9 key */:
    key = KBD_KEY_F9;
    break;
    case VK_F10 /* 0x79	F10 key */:
    key = KBD_KEY_F10;
    break;
    case VK_F11 /* 0x7A	F11 key */:
    key = KBD_KEY_F11;
    break;
    case VK_F12 /* 0x7B	F12 key */:
    key = KBD_KEY_F12;
    break;
    // 0x88-8F	Reserved
    case VK_SCROLL /* 0x91	Scroll lock key */:
    break;
    // 0x92-96	OEM specific
    // 0x97-9F	Unassigned
    case VK_LSHIFT /* 0xA0	Left Shift key */:
    key = KBD_KEY_LEFT_SHIFT;
    break;
    case VK_RSHIFT /* 0xA1	Right Shift key */:
    key = KBD_KEY_RIGHT_SHIFT;
    break;
    case VK_LCONTROL /* 0xA2	Left Ctrl key */:
    key = KBD_KEY_LEFT_CONTROL;
    break;
    case VK_RCONTROL /* 0xA3	Right Ctrl key */:
    key = KBD_KEY_RIGHT_CONTROL;
    break;
    case VK_LMENU /* 0xA4	Left Alt key */:
    key = KBD_KEY_LEFT_ALT;
    break;
    case VK_RMENU /* 0xA5	Right Alt key */:
    key = KBD_KEY_RIGHT_ALT;
    break;
    // 0xB8-B9	Reserved
    case VK_OEM_1 /* 0xBA	It can vary by keyboard. For the US ANSI keyboard , the Semiсolon and Colon key */:
    break;
    case VK_OEM_PLUS /* 0xBB	For any country/region, the Equals and Plus key */:
    break;
    case VK_OEM_COMMA /* 0xBC	For any country/region, the Comma and Less Than key */:
    break;
    case VK_OEM_MINUS /* 0xBD	For any country/region, the Dash and Underscore key */:
    break;
    case VK_OEM_PERIOD /* 0xBE	For any country/region, the Period and Greater Than key */:
    break;
    case VK_OEM_2 /* 0xBF	It can vary by keyboard. For the US ANSI keyboard, the Forward Slash and Question Mark key */:
    break;
    case VK_OEM_3 /* 0xC0	It can vary by keyboard. For the US ANSI keyboard, the Grave Accent and Tilde key */:
    break;
    case VK_OEM_4 /* 0xDB	It can vary by keyboard. For the US ANSI keyboard, the Left Brace key */:
    break;
    case VK_OEM_5 /* 0xDC	It can vary by keyboard. For the US ANSI keyboard, the Backslash and Pipe key */:
    break;
    case VK_OEM_6 /* 0xDD	It can vary by keyboard. For the US ANSI keyboard, the Right Brace key */:
    break;
    case VK_OEM_7 /* 0xDE	It can vary by keyboard. For the US ANSI keyboard, the Apgfxtrophe and Double Quotation Mark key */:
    break;
    case VK_OEM_8 /* 0xDF	It can vary by keyboard. For the Canadian CSA keyboard, the Right Ctrl key */:
    break;
    // 0xE0	Reserved
    // 0xE1	OEM specific
    case VK_OEM_102 /* 0xE2	It can vary by keyboard. For the European ISO keyboard, the Backslash and Pipe key */:
    break;
    // 0xC1-C2	Reserved
    case VK_GAMEPAD_A /* 0xC3	Gamepad A button */:
    break;
    case VK_GAMEPAD_B /* 0xC4	Gamepad B button */:
    break;
    case VK_GAMEPAD_X /* 0xC5	Gamepad X button */:
    break;
    case VK_GAMEPAD_Y /* 0xC6	Gamepad Y button */:
    break;
    case VK_GAMEPAD_RIGHT_SHOULDER /* 0xC7	Gamepad Right Shoulder button */:
    break;
    case VK_GAMEPAD_LEFT_SHOULDER /* 0xC8	Gamepad Left Shoulder button */:
    break;
    case VK_GAMEPAD_LEFT_TRIGGER /* 0xC9	Gamepad Left Trigger button */:
    break;
    case VK_GAMEPAD_RIGHT_TRIGGER /* 0xCA	Gamepad Right Trigger button */:
    break;
    case VK_GAMEPAD_DPAD_UP /* 0xCB	Gamepad D-pad Up button */:
    break;
    case VK_GAMEPAD_DPAD_DOWN /* 0xCC	Gamepad D-pad Down button */:
    break;
    case VK_GAMEPAD_DPAD_LEFT /* 0xCD	Gamepad D-pad Left button */:
    break;
    case VK_GAMEPAD_DPAD_RIGHT /* 0xCE	Gamepad D-pad Right button */:
    break;
    case VK_GAMEPAD_MENU /* 0xCF	Gamepad Menu/Start button */:
    break;
    case VK_GAMEPAD_VIEW /* 0xD0	Gamepad View/Back button */:
    break;
    case VK_GAMEPAD_LEFT_THUMBSTICK_BUTTON /* 0xD1	Gamepad Left Thumbstick button */:
    break;
    case VK_GAMEPAD_RIGHT_THUMBSTICK_BUTTON /* 0xD2	Gamepad Right Thumbstick button */:
    break;
    case VK_GAMEPAD_LEFT_THUMBSTICK_UP /* 0xD3	Gamepad Left Thumbstick up */:
    break;
    case VK_GAMEPAD_LEFT_THUMBSTICK_DOWN /* 0xD4	Gamepad Left Thumbstick down */:
    break;
    case VK_GAMEPAD_LEFT_THUMBSTICK_RIGHT /* 0xD5	Gamepad Left Thumbstick right */:
    break;
    case VK_GAMEPAD_LEFT_THUMBSTICK_LEFT /* 0xD6	Gamepad Left Thumbstick left */:
    break;
    case VK_GAMEPAD_RIGHT_THUMBSTICK_UP /* 0xD7	Gamepad Right Thumbstick up */:
    break;
    case VK_GAMEPAD_RIGHT_THUMBSTICK_DOWN /* 0xD8	Gamepad Right Thumbstick down */:
    break;
    case VK_GAMEPAD_RIGHT_THUMBSTICK_RIGHT /* 0xD9	Gamepad Right Thumbstick right */:
    break;
    case VK_GAMEPAD_RIGHT_THUMBSTICK_LEFT /* 0xDA	Gamepad Right Thumbstick left */:
    break;
    // 0xE6	OEM specific
    case VK_PACKET /* 0xE7	Used to pass Unicode characters as if they were keystrokes. The VK_PACKET key is the low word of a 32-bit Virtual Key value used for non-keyboard input methods. For more information, see Remark in KEYBDINPUT, SendInput, WM_KEYDOWN, and WM_KEYUP */:
    break;
    // 0xE8	Unassigned
  }

  return key;
}


// TODO jfd: mouse clicks and scroll wheel
internal void
func platform_win32_get_game_input_from_events(Platform_event_list *event_list, Game *gp) {
  Game_input *input = &gp->input;

  memory_zero(input->key_released, sizeof(input->key_released));

  input->scroll_delta = (v2){0};

  for(Platform_event *event = event_list->first; event; event = event->next) {

    switch(event->kind) {
      case EVENT_KEY_PRESS: {

        switch(event->key) {
          case KBD_KEY_LEFT_CONTROL: case KBD_KEY_RIGHT_CONTROL: {
            input->modifier_mask |= KBD_MOD_CONTROL;
          } break;
          case KBD_KEY_LEFT_SHIFT: case KBD_KEY_RIGHT_SHIFT: {
            input->modifier_mask |= KBD_MOD_SHIFT;
          } break;
          case KBD_KEY_LEFT_ALT: case KBD_KEY_RIGHT_ALT: {
            input->modifier_mask |= KBD_MOD_ALT;
          } break;
          default: {
          } break;
        }
        input->key_pressed[event->key] += 1 + event->repeat_count;

      } break;
      case EVENT_KEY_RELEASE: {

        switch(event->key) {
          case KBD_KEY_LEFT_CONTROL: case KBD_KEY_RIGHT_CONTROL: {
            input->modifier_mask &= ~KBD_MOD_CONTROL;
          } break;
          case KBD_KEY_LEFT_SHIFT: case KBD_KEY_RIGHT_SHIFT: {
            input->modifier_mask &= ~KBD_MOD_SHIFT;
          } break;
          case KBD_KEY_LEFT_ALT: case KBD_KEY_RIGHT_ALT: {
            input->modifier_mask &= ~KBD_MOD_ALT;
          } break;
          default: {
          } break;
        }
        input->key_pressed[event->key] = 0;
        input->key_released[event->key] = true;

      } break;
      case EVENT_MOUSE_MOVE: {
        input->mouse_pos = event->mouse_pos;
        input->mouse_delta.x = event->mouse_pos.x - input->mouse_pos.x;
        input->mouse_delta.y = event->mouse_pos.y - input->mouse_pos.y;
      } break;
      case EVENT_MOUSE_SCROLL: {
        input->scroll_delta = event->scroll_delta;
      } break;
    }
  }

  // TODO jfd: remove this
  event_list->count = 0;
  event_list->first = event_list->last = 0;
  arena_clear(platform_event_arena);

}

internal LRESULT CALLBACK
func platform_win32_main_window_callback(HWND window, UINT message, WPARAM wParam, LPARAM lParam) {
  LRESULT result = 0;

  switch(message) {
    case WM_SIZE: {

      Platform_win32_window_dimensions window_dimensions = platform_win32_get_window_dimensions(window);

      platform_win32_resize_backbuffer(&global_backbuffer, window_dimensions.width, window_dimensions.height);
    } break;
    case WM_DESTROY: {
      platform_is_running = false;
    } break;
    case WM_CLOSE: {
      platform_is_running = false;
    } break;
    case WM_ACTIVATEAPP: {
    } break;

    case WM_SYSKEYDOWN:
    case WM_SYSKEYUP:
    {
      // NOTE jfd: ryan fleury does this, but I have no idea why
      // if(wParam != VK_MENU && (wParam < VK_F1 || VK_F24 < wParam || wParam == VK_F4))
      // {
      //   result = DefWindowProc(window, message, wParam, lParam);
      // }
    } // fallthrough;
    case WM_KEYDOWN:
    case WM_KEYUP: {
      u32 virtual_keycode = (u32)wParam;

      Keyboard_key key = platform_win32_keyboard_key_from_virtual_keycode(virtual_keycode);

      u16 key_repeat_count = (u16)lParam & 0x7fff;
      b32 was_down = ((u32)lParam & (1 << 30)) != 0;
      b32 is_down = ((u32)lParam & (1 << 31)) == 0;

      b32 release = 0;
      b16 is_repeat = 0;

      if(!is_down) {
        release = 1;
      } else if(was_down) {
        is_repeat = 1;
      }

      Platform_event *event = platform_win32_event_push(platform_event_arena, platform_event_list, release ? EVENT_KEY_RELEASE : EVENT_KEY_PRESS);

      // TODO jfd: distinguish right and left modifiers with is_right_sided (???)

      event->key = key;
      event->is_repeat = is_repeat;
      event->repeat_count = key_repeat_count;

      #ifdef HANDMADE_INTERNAL
      if(is_down) {

        if(key == KBD_KEY_F8) {
          debug_paused = !debug_paused;
          #ifdef HANDMADE_HOTRELOAD

        } else if(key == KBD_KEY_F2) {

          if(debug_loop_recorder.recording_loop) {
            debug_loop_recorder.recording_loop = false;
            debug_loop_recorder.playing_loop = true;
            debug_loop_recorder.read_game_state_from_file = true;
          } else if(debug_loop_recorder.playing_loop) {
            debug_loop_recorder.playing_loop = false;
            debug_loop_recorder.recording_loop = false;
            debug_loop_recorder.stop_playing_loop = true;
          } else {
            debug_loop_recorder.recording_loop = true;
            debug_loop_recorder.input_recording_write_index = 0;
            debug_loop_recorder.write_game_state_to_file = true;
          }

        } else if(key == KBD_KEY_F3) {
          // TODO jfd: play loop
          // debug_loop_recorder.recording_loop = false;
          // debug_loop_recorder.playing_loop = true;
          // debug_loop_recorder.read_game_state_from_file = true;

          #endif
        } else if(key == KBD_KEY_F11) {
          platform_win32_toggle_fullscreen(window);
        }

      }
      #endif

      if((event->key == KBD_KEY_LEFT_ALT || event->key == KBD_KEY_RIGHT_ALT) && event->modifier_mask & KBD_MOD_ALT) {
        event->modifier_mask &= ~KBD_MOD_ALT;
      }

      if((event->key == KBD_KEY_LEFT_CONTROL || event->key == KBD_KEY_RIGHT_CONTROL) && event->modifier_mask & KBD_MOD_CONTROL) {
        event->modifier_mask &= ~KBD_MOD_CONTROL;
      }

      if((event->key == KBD_KEY_LEFT_SHIFT || event->key == KBD_KEY_RIGHT_SHIFT) && event->modifier_mask & KBD_MOD_SHIFT) {
        event->modifier_mask &= ~KBD_MOD_SHIFT;
      }

      // TODO jfd: fill in the other fields

    } break;

    case WM_CHAR: {
    } break;

    case WM_MOUSEMOVE: {
      s16 x_pos = (s16)LOWORD(lParam);
      s16 y_pos = (s16)HIWORD(lParam);

      Platform_event *event = platform_win32_event_push(platform_event_arena, platform_event_list, EVENT_MOUSE_MOVE);
      event->mouse_pos.x = (f32)x_pos;
      event->mouse_pos.y = (f32)y_pos;

    } break;

    case WM_MOUSEWHEEL: {
      s16 wheel_delta = HIWORD(wParam);
      Platform_event *event = platform_win32_event_push(platform_event_arena, platform_event_list, EVENT_MOUSE_SCROLL);
      POINT p;
      p.x = (s32)(s16)LOWORD(lParam);
      p.y = (s32)(s16)HIWORD(lParam);
      ScreenToClient(window, &p);
      event->mouse_pos.x = (f32)p.x;
      event->mouse_pos.y = (f32)p.y;
      event->scroll_delta.y = -(f32)wheel_delta;
    } break;
    case WM_ERASEBKGND: {
      return 1; // tell Windows "I handled it"
    }
    case WM_SETCURSOR: {
      if(debug_show_cursor) {
        result = DefWindowProc(window, message, wParam, lParam);
      } else {
        SetCursor(0);
      }
    } break;
    case WM_PAINT: {
      PAINTSTRUCT paint;

      HDC device_context = BeginPaint(window, &paint);

      Platform_win32_window_dimensions window_dimensions = platform_win32_clear_window_and_get_dimensions(window, device_context);

      int x = paint.rcPaint.left;
      int y = paint.rcPaint.top;
      int width = paint.rcPaint.right - x;
      int height = paint.rcPaint.bottom - y;

      platform_win32_display_buffer_in_window(&global_backbuffer, device_context, window_dimensions.width, window_dimensions.height, x, y, width, height);
      EndPaint(window, &paint);
    } break;
    default: {
      result = DefWindowProc(window, message, wParam, lParam);
    } break;
  }

  return result;
}



internal void
func platform_win32_load_xinput(void) {
  void *lib = platform_library_load("Xinput1_4.dll");
  if(lib) {
    platform_win32_xinput_get_state = (Platform_win32_xinput_get_state_func*)platform_library_load_function(lib, "XInputGetState");
    platform_win32_xinput_set_state = (Platform_win32_xinput_set_state_func*)platform_library_load_function(lib, "XInputSetState");
  }
}



internal void
func platform_win32_init_dsound(HWND window_handle, s32 sound_buffer_size, s32 samples_per_second) {
  // NOTE jfd: load the library

  void *lib = platform_library_load("dsound.dll");

  if(lib) {
    // NOTE jfd: get a direct sound object
    Platform_win32_direct_sound_create_func *direct_sound_create =
    (Platform_win32_direct_sound_create_func*)platform_library_load_function(lib, "DirectSoundCreate8");

    LPDIRECTSOUND direct_sound;

    if(direct_sound_create && SUCCEEDED(direct_sound_create(0, &direct_sound, 0))) {

      u16 n_channels = 2;
      u16 bits_per_sample = 16;
      u16 block_align = (n_channels * bits_per_sample) >> 3;

      WAVEFORMATEX wave_format = {0};
      wave_format.wFormatTag = WAVE_FORMAT_PCM; // play 1 or 2 channel PCM data
      wave_format.nChannels = n_channels;
      wave_format.nSamplesPerSec = samples_per_second;
      wave_format.nAvgBytesPerSec = samples_per_second * block_align;
      wave_format.nBlockAlign = block_align;
      wave_format.wBitsPerSample = bits_per_sample;
      wave_format.cbSize = 0;

      if(SUCCEEDED(direct_sound->lpVtbl->SetCooperativeLevel(direct_sound, window_handle, DSSCL_PRIORITY))) {

        DSBUFFERDESC buffer_description = {0};
        buffer_description.dwSize = sizeof(DSBUFFERDESC);
        buffer_description.dwFlags =
        DSBCAPS_PRIMARYBUFFER |
        0;

        // NOTE jfd: create a primary buffer
        LPDIRECTSOUNDBUFFER primary_buffer;

        if(SUCCEEDED(direct_sound->lpVtbl->CreateSoundBuffer(direct_sound, &buffer_description, &primary_buffer, 0))) {

          HRESULT hr = primary_buffer->lpVtbl->SetFormat(primary_buffer, &wave_format);
          if(SUCCEEDED(hr)) {
            OutputDebugStringA("created primary buffer\n");
          } else {
            UNIMPLEMENTED;
          }

        } else {
          UNIMPLEMENTED;
        }

      } else {
        UNIMPLEMENTED;
      }

      // NOTE jfd: create secondary buffer for writing

      DSBUFFERDESC buffer_description = {0};
      buffer_description.dwSize = sizeof(DSBUFFERDESC);
      buffer_description.dwFlags =
      DSBCAPS_GETCURRENTPOSITION2 |
      0;
      buffer_description.dwBufferBytes = sound_buffer_size;
      buffer_description.lpwfxFormat = &wave_format;

      HRESULT hr = direct_sound->lpVtbl->CreateSoundBuffer(direct_sound, &buffer_description, &platform_sound_buffer, 0);
      if(SUCCEEDED(hr)) {
        OutputDebugStringA("created secondary buffer\n");
      } else {
        UNREACHABLE;
      }

      // NOTE jfd: play sound

    } else {
      UNIMPLEMENTED;
    }

  }
}

internal void
func platform_win32_debug_draw_vertical_line(Platform_win32_backbuffer *backbuffer, int x, int top, int bottom, u32 color) {

  if(top <= 0) {
    top = 0;
  }

  if(bottom > backbuffer->bitmap_height) {
    bottom = backbuffer->bitmap_height;
  }

  if(x >= 0 && x < backbuffer->bitmap_width) {

    u8 *pixel = (u8*)backbuffer->bitmap_memory + x*backbuffer->bytes_per_pixel + top*backbuffer->stride;
    for(int y = top; y < bottom; y++) {
      *(u32*)pixel = color;
      pixel += backbuffer->stride;
    }

  }

}

internal void
func platform_win32_debug_draw_sound_buffer_marker(
  Platform_win32_backbuffer *backbuffer,
  Platform_win32_sound_output *sound_output,
  f32 c,
  int padding_x,
  int top,
  int bottom,
  DWORD value,
  u32 color
) {
  f32 x_float_val = c * (f32)value;
  int x = padding_x + (int)x_float_val;
  platform_win32_debug_draw_vertical_line(backbuffer, x, top, bottom, color);
}

internal void
func platform_win32_debug_audio_sync_display(
  Platform_win32_backbuffer *backbuffer,
  Platform_win32_sound_output *sound_output,
  Platform_win32_debug_time_marker_slice markers,
  int current_marker_index,
  f32 target_seconds_per_frame
) {
  int padding_x = 16;
  int padding_y = 20;

  int line_height = 64;
  f32 c = ((f32)backbuffer->bitmap_width - (2*padding_x)) / (f32)platform_sound_output->buffer_size;

  for(int marker_index = 0; marker_index < markers.count; marker_index++) {
    Platform_win32_debug_time_marker *this_marker = &markers.d[marker_index];
    ASSERT(this_marker->output_play_cursor < sound_output->buffer_size);
    ASSERT(this_marker->output_write_cursor < sound_output->buffer_size);
    ASSERT(this_marker->output_location < sound_output->buffer_size);
    ASSERT(this_marker->flip_play_cursor < sound_output->buffer_size);
    ASSERT(this_marker->flip_write_cursor < sound_output->buffer_size);

    u32 play_color = 0xffffffff;
    u32 write_color = 0xff00ff00;
    u32 expected_flip_color = 0xffffff00;

    int top = padding_y;
    int bottom = padding_y + line_height;

    if(marker_index == current_marker_index) {
      top += line_height + padding_y;
      bottom += line_height + padding_y;

      int first_top = top;

      platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->output_play_cursor, play_color);
      platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->output_write_cursor, write_color);

      top += line_height + padding_y;
      bottom += line_height + padding_y;

      platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->output_location, play_color);
      platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->output_location + this_marker->output_byte_count, write_color);

      top += line_height + padding_y;
      bottom += line_height + padding_y;

      platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, first_top, bottom, this_marker->expected_flip_cursor, expected_flip_color);

    }

    platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->flip_play_cursor, play_color);
    platform_win32_debug_draw_sound_buffer_marker(backbuffer, sound_output, c, padding_x, top, bottom, this_marker->flip_write_cursor, write_color);

  }

}


internal void
func platform_win32_clear_sound_buffer(Platform_win32_sound_output *sound_output) {
  void *region1;
  DWORD region1_size;
  void *region2;
  DWORD region2_size;

  if(SUCCEEDED(platform_sound_buffer->lpVtbl->Lock(
    platform_sound_buffer,
    0,
    sound_output->buffer_size,
    &region1,
    &region1_size,
    &region2,
    &region2_size,
    0
  )))
  {
    memory_zero(region1, region1_size);
    memory_zero(region2, region2_size);

    platform_sound_buffer->lpVtbl->Unlock(platform_sound_buffer, region1, region1_size, region2, region2_size);
  }
}

internal void
func platform_win32_fill_sound_buffer(Platform_win32_sound_output *sound_output, DWORD byte_to_lock_at, DWORD bytes_to_write, Game_sound_buffer *source_buffer) {

  void *region1;
  DWORD region1_size;
  void *region2;
  DWORD region2_size;

  if(SUCCEEDED(platform_sound_buffer->lpVtbl->Lock(
    platform_sound_buffer,
    byte_to_lock_at,
    bytes_to_write,
    &region1,
    &region1_size,
    &region2,
    &region2_size,
    0
  )))
  {

    s16 *sample_out1 = (s16*)region1;
    int region1_sample_count = region1_size / sound_output->bytes_per_sample;
    int region2_sample_count = region2_size / sound_output->bytes_per_sample;

    s16 *source_sample = source_buffer->samples;

    for(s32 sample_index = 0; sample_index < region1_sample_count; sample_index++) {
      *sample_out1++ = *source_sample++;
      *sample_out1++ = *source_sample++;
      ++sound_output->running_sample_index;
    }

    s16 *sample_out2 = (s16*)region2;
    for(s32 sample_index = 0; sample_index < region2_sample_count; sample_index++) {
      *sample_out2++ = *source_sample++;
      *sample_out2++ = *source_sample++;
      ++sound_output->running_sample_index;
    }

    platform_sound_buffer->lpVtbl->Unlock(platform_sound_buffer, region1, region1_size, region2, region2_size);

  } /* if(Lock()) */

}


internal FILETIME
func platform_win32_get_last_file_write_time(char *path) {
  WIN32_FILE_ATTRIBUTE_DATA file_info;
  ASSERT(GetFileAttributesExA(path, GetFileExInfoStandard, &file_info));
  FILETIME result = file_info.ftLastWriteTime;
  return result;
}


force_inline LARGE_INTEGER
func platform_win32_get_wall_clock(void) {
  LARGE_INTEGER result;
  QueryPerformanceCounter(&result);
  return result;
}

force_inline f32
func platform_win32_get_seconds_elapsed(LARGE_INTEGER begin, LARGE_INTEGER end) {
  f32 result =
  (f32)(end.QuadPart - begin.QuadPart) / (f32)platform_win32_perf_counter_frequency;

  return result;
}


#ifdef HANDMADE_HOTRELOAD
internal void
func platform_win32_debug_setup_loop_recorder(void) {

  debug_loop_recorder.game_state_file_handle = CreateFileA("game.state", GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
  LARGE_INTEGER game_state_file_size;
  ASSERT(GetFileSizeEx(debug_loop_recorder.game_state_file_handle, &game_state_file_size));
  ASSERT((u64)game_state_file_size.QuadPart == GAME_MEMORY_BACKBUFFER_SIZE);

  debug_loop_recorder.game_state_file_map_handle =
  CreateFileMappingA(
    debug_loop_recorder.game_state_file_handle,
    0,
    PAGE_READWRITE,
    game_state_file_size.HighPart,
    game_state_file_size.LowPart,
    0
  );

  if(!debug_loop_recorder.game_state_file_map_handle) {
    DWORD err = GetLastError();
    PANICF("file failed map error code %d\n", err);
  }

  debug_loop_recorder.recorded_game_state.len = (s64)game_state_file_size.QuadPart;
  debug_loop_recorder.recorded_game_state.s =
  (u8*)MapViewOfFile(debug_loop_recorder.game_state_file_map_handle, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);

  ASSERT(debug_loop_recorder.recorded_game_state.s);

}

internal void
func platform_win32_debug_shutdown_loop_recorder(void) {

  UnmapViewOfFile((void*)debug_loop_recorder.recorded_game_state.s);
  CloseHandle(debug_loop_recorder.game_state_file_map_handle);
  CloseHandle(debug_loop_recorder.game_state_file_handle);

}

internal void
func platform_win32_debug_run_loop_recorder(Game *gp) {

  if(debug_loop_recorder.recording_loop) {

    if(debug_loop_recorder.write_game_state_to_file) {
      debug_loop_recorder.write_game_state_to_file = false;
      memory_copy(debug_loop_recorder.recorded_game_state.s, platform.backbuffer, platform.backbuffer_size);

      debug_loop_recorder.game_input_file_handle = CreateFileA("game.input", GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
      ASSERT(debug_loop_recorder.game_input_file_handle);

    }

    LARGE_INTEGER input_write_offset;
    input_write_offset.QuadPart = debug_loop_recorder.input_recording_write_index * sizeof(Game_input);
    debug_loop_recorder.input_recording_write_index++;
    ASSERT(SetFilePointerEx(debug_loop_recorder.game_input_file_handle, input_write_offset, 0, FILE_BEGIN));

    DWORD bytes_written;
    ASSERT(WriteFile(debug_loop_recorder.game_input_file_handle, (void*)(&gp->input), sizeof(Game_input), &bytes_written, 0));
    ASSERT(bytes_written == sizeof(Game_input));

  } else if(debug_loop_recorder.playing_loop) {

    if(debug_loop_recorder.read_game_state_from_file) {
      debug_loop_recorder.read_game_state_from_file = false;

      CloseHandle(debug_loop_recorder.game_input_file_handle);
      Str8 recorded_game_input_data = platform_read_entire_file(platform_file_arena, "game.input");

      debug_loop_recorder.recorded_game_input.d = (Game_input*)(recorded_game_input_data.s);
      debug_loop_recorder.recorded_game_input.count = debug_loop_recorder.input_recording_write_index;

      debug_loop_recorder.input_recording_play_index = debug_loop_recorder.recorded_game_input.count;
    }

    if(debug_loop_recorder.input_recording_play_index >= debug_loop_recorder.recorded_game_input.count) {
      debug_loop_recorder.input_recording_play_index = 0;
      memory_copy(platform.backbuffer, debug_loop_recorder.recorded_game_state.s, debug_loop_recorder.recorded_game_state.len);
    }

    gp->input = debug_loop_recorder.recorded_game_input.d[debug_loop_recorder.input_recording_play_index++];

  } else if(debug_loop_recorder.stop_playing_loop) {
    debug_loop_recorder.stop_playing_loop = false;

    memory_zero(&gp->input, sizeof(gp->input));
  }

}
#endif

#ifdef HANDMADE_HOTRELOAD
internal void
func platform_win32_load_game_code(void) {

  {

    DWORD attrs = GetFileAttributesA("game.dll");
    int result = (attrs != INVALID_FILE_ATTRIBUTES && !(attrs & FILE_ATTRIBUTE_DIRECTORY));
    if(!result) {
      attrs = GetFileAttributesA("game.dll.live");
      result = (attrs != INVALID_FILE_ATTRIBUTES && !(attrs & FILE_ATTRIBUTE_DIRECTORY));
      if(!result) {
        OutputDebugStringA("game.dll.live not found\n");
        UNREACHABLE;
      }
    } else {
      if(!MoveFileEx("game.dll", "game.dll.live", MOVEFILE_REPLACE_EXISTING)) {
        OutputDebugStringA("module file rename failed\n");
        UNREACHABLE;
      }
    }

  }

  game_module = LoadLibraryA("game.dll.live");
  if(!game_module) {
    OutputDebugStringA("failed to load module\n");
    UNREACHABLE;
  }

  game_init = (Game_init_proc*)GetProcAddress(game_module, "game_init");
  ASSERT(game_init);

  game_update_and_render = (Game_service_proc*)GetProcAddress(game_module, "game_update_and_render");
  ASSERT(game_update_and_render);

  game_get_sound_samples = (Game_service_proc*)GetProcAddress(game_module, "game_get_sound_samples");
  ASSERT(game_get_sound_samples);

}

internal void
func platform_win32_unload_game_code(void) {
  FreeLibrary(game_module);
  game_module = 0;
  game_init = 0;
  game_update_and_render = 0;
  game_get_sound_samples = 0;
}
#endif


int CALLBACK
WinMain(HINSTANCE instance, HINSTANCE prevInstance, LPSTR cmdLine, int showCode) {

  WNDCLASSA window_class = {0};
  HWND window_handle = {0};
  MSG message = {0};

  Game *gp = 0;

  { /* init */

    LARGE_INTEGER performance_counter_frequency_result;
    QueryPerformanceFrequency(&performance_counter_frequency_result);
    platform_win32_perf_counter_frequency = performance_counter_frequency_result.QuadPart;

    UINT scheduler_granularity_ms = 1;
    sleep_is_granular = timeBeginPeriod(scheduler_granularity_ms);

    platform_main_arena  = arena_create(MB(100));
    platform_debug_arena = arena_create(MB(5));
    platform_temp_arena  = arena_create(KB(5));
    platform_event_arena = arena_create(KB(50));
    platform_file_arena  = arena_create(MB(100));

    window_class = (WNDCLASSA){0};
    window_class.style = CS_OWNDC | CS_HREDRAW | CS_VREDRAW;
    window_class.lpfnWndProc = platform_win32_main_window_callback;
    window_class.hInstance = instance;
    window_class.lpszClassName = "Handmade Hero";

    #ifdef HANDMADE_INTERNAL
    debug_show_cursor = true;
    window_class.hCursor = LoadCursor(0, IDC_CROSS);
    #endif

    global_backbuffer.bytes_per_pixel = 4;
    platform_event_list = &_platform_event_list_stub;

    {

      platform.backbuffer_size = GAME_MEMORY_BACKBUFFER_SIZE;
      platform.backbuffer =
      VirtualAlloc(
        (void*)(u64)0x200000000, // NOTE jfd: this is going to be the base address for all the allocations done in the game code
        platform.backbuffer_size,
        MEM_RESERVE|MEM_COMMIT,
        PAGE_READWRITE
      );

    }

    #ifdef HANDMADE_HOTRELOAD
    platform_win32_load_game_code();
    #endif


    if(RegisterClass(&window_class)) {
      window_handle =
      CreateWindowExA(
        0,
        window_class.lpszClassName,
        "Handmade Hero",
        WS_OVERLAPPEDWINDOW | WS_VISIBLE,
        100,
        50,
        1500,
        900,
        0,
        0,
        instance,
        0);

      if(window_handle) {

        ASSERT(SUCCEEDED(CoInitializeEx(0, COINIT_MULTITHREADED)));

        monitor_refresh_hz = 60;
        refresh_rate_query_device_context = GetDC(window_handle);
        platform_win32_refresh_rate = GetDeviceCaps(refresh_rate_query_device_context, VREFRESH);
        ReleaseDC(window_handle, refresh_rate_query_device_context);
        if(platform_win32_refresh_rate > 1) {
          monitor_refresh_hz = platform_win32_refresh_rate;
        }
        game_update_hz = (f32)monitor_refresh_hz / 2.0f;
        platform_target_seconds_per_frame = 1.0f/(f32)game_update_hz;

        // NOTE jfd: graphics test

        // NOTE jfd: sound test
        platform_sound_output = &_platform_sound_output_stub;
        platform_sound_output->samples_per_second    = THOUSAND(48);
        platform_sound_output->buffer_size           = platform_sound_output->samples_per_second * sizeof(s16) * 2;
        platform_sound_output->running_sample_index  = 0  ;
        platform_sound_output->bytes_per_sample      = sizeof(s16)*2;
        // TODO jfd: actually compute this variance and see what the lowest reasonable value is
        platform_sound_output->safety_bytes =
        ((platform_sound_output->bytes_per_sample * platform_sound_output->samples_per_second) / (DWORD)game_update_hz) / 3;


        platform_win32_init_dsound(window_handle, platform_sound_output->buffer_size, platform_sound_output->samples_per_second);
        platform_win32_clear_sound_buffer(platform_sound_output);
        platform_sound_buffer->lpVtbl->Play(platform_sound_buffer, 0, 0, DSBPLAY_LOOPING);

        samples = (s16*)push_array_no_zero(platform_main_arena, u8, platform_sound_output->buffer_size);

        platform_is_running = true;

        gp = game_init(&platform);

        #ifdef HANDMADE_HOTRELOAD
        platform_win32_debug_setup_loop_recorder();
        #endif

        debug_time_marker_index = 0;
        slice_init(debug_time_markers, 60, platform_debug_arena);

        last_counter = platform_win32_get_wall_clock();
        flip_wall_clock = platform_win32_get_wall_clock();

        #ifdef HANDMADE_PROFILE
        last_cycle_count = __rdtsc(); // NOTE jfd: get timestamp in cycles
        #endif

        #ifdef HANDMADE_AUDIO_LATENCY_DEBUG
        audio_latency_bytes = 0;
        audio_latency_seconds = 0;
        #endif
        sound_is_valid = false;


      } else {
      }

    } else {
      // TODO jfd: logging
    }

  } /* init */


  while(platform_is_running) {

    #ifdef HANDMADE_HOTRELOAD
    if(platform_file_exists("game.dll")) {
      platform_win32_unload_game_code();
      platform_win32_load_game_code();
      gp->did_reload = true;
    }
    #endif

    // NOTE jfd: get input messages
    while(PeekMessageA(&message, window_handle, 0, 0, PM_REMOVE)) {
      if(message.message == WM_QUIT) {
        platform_is_running = false;
      }
      TranslateMessage(&message); // NOTE jfd: has to do with keyboard messages
      DispatchMessage(&message);

    }

    #ifdef HANDMADE_INTERNAL
    if(debug_paused) {
      continue;
    }
    #endif

    // TODO jfd: Implement gamepad input
    #if 0
    { /* get gamepad input */

      for(DWORD controller_index = 0; controller_index < XUSER_MAX_COUNT; controller_index++) {
        XINPUT_STATE controller_state;
        if(os_win32_xinput_get_state(controller_index, &controller_state) == ERROR_SUCCESS) {
          // NOTE jfd: this controller is plugged in
          XINPUT_GAMEPAD *gamepad = &controller_state.Gamepad;

          b8 dpad_up    = !!(gamepad->wButtons & XINPUT_GAMEPAD_DPAD_UP);
          b8 dpad_down  = !!(gamepad->wButtons & XINPUT_GAMEPAD_DPAD_DOWN);
          b8 dpad_left  = !!(gamepad->wButtons & XINPUT_GAMEPAD_DPAD_LEFT);
          b8 dpad_right = !!(gamepad->wButtons & XINPUT_GAMEPAD_DPAD_RIGHT);

          b8 start_button = !!(gamepad->wButtons & XINPUT_GAMEPAD_START);
          b8 back_button  = !!(gamepad->wButtons & XINPUT_GAMEPAD_BACK);

          b8 left_thumb     = !!(gamepad->wButtons & XINPUT_GAMEPAD_LEFT_THUMB);
          b8 right_thumb    = !!(gamepad->wButtons & XINPUT_GAMEPAD_RIGHT_THUMB);
          b8 left_shoulder  = !!(gamepad->wButtons & XINPUT_GAMEPAD_LEFT_SHOULDER);
          b8 right_shoulder = !!(gamepad->wButtons & XINPUT_GAMEPAD_RIGHT_SHOULDER);

          b8 a_button = !!(gamepad->wButtons & XINPUT_GAMEPAD_A);
          b8 b_button = !!(gamepad->wButtons & XINPUT_GAMEPAD_B);
          b8 x_button = !!(gamepad->wButtons & XINPUT_GAMEPAD_X);
          b8 y_button = !!(gamepad->wButtons & XINPUT_GAMEPAD_Y);

          s16 left_stick_x = gamepad->sThumbLX;
          s16 left_stick_y = gamepad->sThumbLY;
          s16 right_stick_x = gamepad->sThumbRX;
          s16 right_stick_y = gamepad->sThumbRY;

        } else {
          // NOTE jfd: this controller is not available
        }
      }

    } /* get gamepad input */
    #endif

    platform_win32_get_game_input_from_events(platform_event_list, gp);

    #ifdef HANDMADE_HOTRELOAD
    platform_win32_debug_run_loop_recorder(gp);
    #endif

    // TODO jfd: cleanup how we pass render data to and from the game
    gp->t = platform_target_seconds_per_frame;
    gp->render.pixels = global_backbuffer.bitmap_memory;
    gp->render.width  = global_backbuffer.bitmap_width;
    gp->render.height = global_backbuffer.bitmap_height;
    gp->render.stride = global_backbuffer.stride;

    game_update_and_render(gp);

    // LARGE_INTEGER audio_wall_clock = platform_win32_get_wall_clock();
    // f32 from_begin_to_audio_seconds = platform_win32_get_seconds_elapsed(flip_wall_clock, audio_wall_clock);

    DWORD play_cursor;
    DWORD write_cursor;

    if(SUCCEEDED(platform_sound_buffer->lpVtbl->GetCurrentPosition(platform_sound_buffer, &play_cursor, &write_cursor))) {

      /* NOTE jfd:
           Here is how sound output computation works.

           We define a safety value that is the number of samples we think our game update loop
           may vary by (let's say up to 2ms).

           When we wake up to write audio, we will look and see what the play cursor position is and we
           will forecast ahead where we think the play cursor will be on the
           next frame boundary.

           We will then look to see if the write cursor is before that by at least our safety value.
           If it is, the target fill position is that frame boundary plus one frame. This gives us perfect
           audio sync in the case of a card that has low enough latency.

           If the write cursor is after that safety margin, then we assume we can never sync the audio perfectly,
           so we will write one frame's worth of audio plus the safety margin's worth of guard samples.
        */

      if(!sound_is_valid) {
        // NOTE jfd: On the first sound buffer write we need to set the running sample index for the sound
        //           output to be the place where the sound card is letting us write to.
        platform_sound_output->running_sample_index = write_cursor / platform_sound_output->bytes_per_sample;
        sound_is_valid = true;
      }

      DWORD byte_to_lock_at =
      (platform_sound_output->running_sample_index * platform_sound_output->bytes_per_sample) % platform_sound_output->buffer_size;

      DWORD expected_sound_bytes_per_frame =
      (DWORD)((platform_sound_output->bytes_per_sample * platform_sound_output->samples_per_second) / (DWORD)game_update_hz);

      // TODO jfd: find out what the heck casey used this for
      // f32 seconds_left_until_flip = (target_seconds_per_frame - from_begin_to_audio_seconds);
      // DWORD expected_bytes_until_flip =
      // (DWORD)((seconds_left_until_flip/target_seconds_per_frame) * (f32)expected_sound_bytes_per_frame);

      DWORD expected_frame_boundary_byte = play_cursor + expected_sound_bytes_per_frame;

      DWORD safe_write_cursor = write_cursor;
      if(safe_write_cursor < play_cursor) {
        safe_write_cursor += platform_sound_output->buffer_size;
      }
      ASSERT(safe_write_cursor >= play_cursor);
      safe_write_cursor += platform_sound_output->safety_bytes;
      b32 audio_card_is_low_latency = (safe_write_cursor < expected_frame_boundary_byte);

      DWORD target_cursor = 0;
      if(audio_card_is_low_latency) {
        target_cursor = expected_frame_boundary_byte + expected_sound_bytes_per_frame;
      } else {
        target_cursor = write_cursor + expected_sound_bytes_per_frame + platform_sound_output->safety_bytes;
      }
      target_cursor %= platform_sound_output->buffer_size;

      DWORD bytes_to_write = 0;
      if(byte_to_lock_at > target_cursor) {
        bytes_to_write = (platform_sound_output->buffer_size - byte_to_lock_at);
        bytes_to_write += target_cursor;
      } else {
        bytes_to_write = target_cursor - byte_to_lock_at;
      }

      // NOTE jfd: get sound samples from game
      gp->sound = (Game_sound_buffer){0};
      gp->sound.samples_per_second = platform_sound_output->samples_per_second;
      gp->sound.sample_count = bytes_to_write / platform_sound_output->bytes_per_sample;
      gp->sound.samples = samples;
      game_get_sound_samples(gp);

      #ifdef HANDMADE_AUDIO_LATENCY_DEBUG
      Platform_win32_debug_time_marker *marker = &debug_time_markers.d[debug_time_marker_index];
      marker->output_play_cursor = play_cursor;
      marker->output_write_cursor = write_cursor;
      marker->output_location = byte_to_lock_at;
      marker->output_byte_count = bytes_to_write;
      marker->expected_flip_cursor = expected_frame_boundary_byte;

      DWORD debug_play_cursor;
      DWORD debug_write_cursor;
      platform_sound_buffer->lpVtbl->GetCurrentPosition(platform_sound_buffer, &debug_play_cursor, &debug_write_cursor);

      // NOTE jfd: This value is the audio latency in bytes
      if(debug_write_cursor < debug_play_cursor) {
        audio_latency_bytes = debug_play_cursor - debug_write_cursor;
      } else {
        audio_latency_bytes = debug_write_cursor - debug_play_cursor;
      }

      audio_latency_seconds =
      (((f32)audio_latency_bytes / (f32)platform_sound_output->bytes_per_sample) /
        (f32)platform_sound_output->samples_per_second);

      arena_clear(platform_temp_arena);
      OutputDebugStringA(cstrf(platform_temp_arena,
        "byte to lock at: %u   bytes to write: %u  -  play cursor: %u  write cursor: %u  audio latency bytes: %d  audio latency secs: %f\n",
        byte_to_lock_at, bytes_to_write,
        debug_play_cursor, debug_write_cursor,
        audio_latency_bytes, audio_latency_seconds));

      #endif

      platform_win32_fill_sound_buffer(platform_sound_output, byte_to_lock_at, bytes_to_write, &gp->sound);

    } else {
      sound_is_valid = false;
    }

    LARGE_INTEGER work_counter = platform_win32_get_wall_clock();
    f32 work_seconds_elapsed = platform_win32_get_seconds_elapsed(last_counter, work_counter);

    f32 seconds_elapsed_for_frame = work_seconds_elapsed;
    if(seconds_elapsed_for_frame > platform_target_seconds_per_frame) {
      // TODO jfd: MISSED FRAME RATE!!!!!
      // TODO jfd: logging
    } else {
      while(seconds_elapsed_for_frame < platform_target_seconds_per_frame) {
        if(sleep_is_granular) {
          DWORD sleep_for_ms = (DWORD)(1000.0f * (platform_target_seconds_per_frame - seconds_elapsed_for_frame));
          platform_sleep_ms(sleep_for_ms);
        }
        LARGE_INTEGER check_counter = platform_win32_get_wall_clock();
        seconds_elapsed_for_frame = platform_win32_get_seconds_elapsed(last_counter, check_counter);
      }
    }

    // NOTE jfd: blit to screen
    HDC device_context = {0};
    defer_loop(device_context = GetDC(window_handle), ReleaseDC(window_handle, device_context)) {
      Platform_win32_window_dimensions window_dimensions = platform_win32_get_window_dimensions(window_handle);

      #ifdef HANDMADE_DEBUG_SOUND
      platform_win32_debug_audio_sync_display(&global_backbuffer, platform_sound_output, debug_time_markers, debug_time_marker_index - 1, platform_target_seconds_per_frame);
      #endif

      platform_win32_display_buffer_in_window(&global_backbuffer, device_context, window_dimensions.width, window_dimensions.height, 0, 0, window_dimensions.width, window_dimensions.height);
      flip_wall_clock = platform_win32_get_wall_clock();
    }

    #ifdef HANDMADE_INTERNAL
    DWORD debug_play_cursor;
    DWORD debug_write_cursor;
    if(SUCCEEDED(platform_sound_buffer->lpVtbl->GetCurrentPosition(platform_sound_buffer, &debug_play_cursor, &debug_write_cursor))) {
      ASSERT(debug_time_marker_index < debug_time_markers.count);
      Platform_win32_debug_time_marker *marker = &debug_time_markers.d[debug_time_marker_index++];

      if(debug_time_marker_index >= debug_time_markers.count) {
        debug_time_marker_index = 0;
      }
      platform_sound_buffer->lpVtbl->GetCurrentPosition(platform_sound_buffer, &marker->flip_play_cursor, &marker->flip_write_cursor);
    }
    #endif

    LARGE_INTEGER end_counter = platform_win32_get_wall_clock();

    #ifdef HANDMADE_PROFILE
    s64 end_cycle_count = __rdtsc();
    s64 elapsed_cycles = end_cycle_count - last_cycle_count;
    last_cycle_count = end_cycle_count;

    f32 elapsed_time = platform_win32_get_seconds_elapsed(last_counter, end_counter);
    f32 ms_per_frame = THOUSAND(elapsed_time);
    f32 fps = 1.0f / elapsed_time;

    OutputDebugStringA(
      cstrf(platform_temp_arena,
        "frame time (ms):  %f    fps:  %f    mega cycles: %d\n",
        ms_per_frame, fps, elapsed_cycles / MILLION(1))
    );
    arena_clear(platform_temp_arena);
    #endif

    last_counter = end_counter;

  }

  #ifdef HANDMADE_HOTRELOAD
  platform_win32_debug_shutdown_loop_recorder();
  #endif

  return 0;
}



#endif
