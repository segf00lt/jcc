#ifndef PLATFORM_CORE_WIN32_C
#define PLATFORM_CORE_WIN32_C

#ifndef PLATFORM_WIN32_H
#include <windows.h>
#include <intrin.h>
#include <objbase.h>
#endif

platform_core_api void*
func platform_alloc(u64 bytes) {
  void *ptr = VirtualAlloc(0, bytes, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
  ASSERT(ptr);
  return ptr;
}

platform_core_api void
func platform_free(void *ptr) {
  ASSERT(ptr);
  VirtualFree(ptr, 0, MEM_RELEASE);
}

platform_core_api void*
func platform_library_load(char *path) {
  void *result = 0;
  HMODULE module = LoadLibraryA(path);
  ASSERT(module);
  result = (void*)module;
  return result;
}

platform_core_api void
func platform_library_unload(void *lib) {
  ASSERT(lib);
  FreeLibrary((HMODULE)lib);
}

platform_core_api Void_func*
func platform_library_load_function(void *lib, char *name) {
  ASSERT(lib); // TODO jfd 23/02/26: these asserts need to be something more robust for shipping
  Void_func *result = (Void_func*)GetProcAddress((HMODULE)lib, name);
  ASSERT(result);
  return result;
}

platform_core_api b32
func platform_file_exists(char *path) {
  DWORD attrs = GetFileAttributesA(path);
  b32 result = (attrs != INVALID_FILE_ATTRIBUTES && !(attrs & FILE_ATTRIBUTE_DIRECTORY));
  return result;
}

platform_core_api Str8
func platform_get_current_dir(Arena *a) {
  DWORD buf_size = GetCurrentDirectory(0, NULL);
  ASSERT(buf_size > 0);

  char *buf = push_array_no_zero(a, char, buf_size);
  ASSERT(GetCurrentDirectory(buf_size, buf) != 0);

  Str8 result = { .s = (u8*)buf, .len = buf_size - 1 };

  return result;
}

platform_core_api b32
func platform_set_current_dir(char *dir_path) {
  b32 result = 0;

  result = SetCurrentDirectory(dir_path);

  return result;
}

platform_core_api b32
func platform_move_file(char *old_path, char *new_path) {
  b32 result = 0;

  result = MoveFileEx(old_path, new_path, MOVEFILE_REPLACE_EXISTING);

  return result;
}

platform_core_api b32
func platform_remove_file(char *path) {
  b32 result = !!(DeleteFileA(path));

  return result;
}

platform_core_api b32
func platform_make_dir(char *dir_path) {
  b32 result = CreateDirectoryA(dir_path, 0);
  return result;
}


platform_core_api Str8
func platform_read_entire_file(Arena *arena, char *path) {
  Str8 data;

  HANDLE file_handle = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
  LARGE_INTEGER file_size;
  b32 got_size = GetFileSizeEx(file_handle, &file_size);
  ASSERT(got_size);
  data.len = file_size.QuadPart;

  data.s = push_array_no_zero(arena, u8, data.len);
  ASSERT(data.s);

  #if 0
  u8 *ptr = data.s;
  s64 total_len = data.len;
  while(total_len > 0) {

    DWORD dword_len;
    if(total_len >= (s64)MAX_S32) {
      dword_len = (DWORD)MAX_S32;
    } else {
      dword_len = (DWORD)total_len;
    }

    DWORD bytes_read;
    b32 success = ReadFile(file_handle, (void*)ptr, dword_len, &bytes_read, 0);
    ASSERT(success);
    ASSERT((s64)bytes_read == dword_len);

    ptr += bytes_read;
    total_len -= bytes_read;

  }
  CloseHandle(file_handle);
  #else

  HANDLE file_map_handle =
  CreateFileMappingA(
    file_handle,
    0,
    PAGE_READONLY,
    file_size.HighPart,
    file_size.LowPart,
    0
  );

  if(!file_map_handle) {
    DWORD err = GetLastError();
    PANICF("file failed map error code %d\n", err);
  }

  void *file_map_view = MapViewOfFile(file_map_handle, FILE_MAP_READ, 0, 0, 0);
  memory_copy(data.s, file_map_view, data.len);

  UnmapViewOfFile(file_map_view);
  CloseHandle(file_map_handle);
  CloseHandle(file_handle);

  #endif

  return data;
}

platform_core_api b32
func platform_write_entire_file(Str8 data, char *path) {
  b32 success = true;

  HANDLE file_handle = CreateFileA(path, GENERIC_WRITE | GENERIC_READ, 0, 0, CREATE_ALWAYS, 0, 0);

  #if 0
  u8 *ptr = data.s;
  s64 total_len = data.len;
  while(total_len > 0) {

    DWORD dword_len;
    if(total_len >= (s64)MAX_S32) {
      dword_len = (DWORD)MAX_S32;
    } else {
      dword_len = (DWORD)total_len;
    }

    DWORD bytes_written;
    success = WriteFile(file_handle, (void*)ptr, dword_len, &bytes_written, 0);
    ASSERT((s64)bytes_written == dword_len);

    ptr += bytes_written;
    total_len -= bytes_written;

  }
  CloseHandle(file_handle);
  #else

  LARGE_INTEGER file_size;
  file_size.QuadPart = data.len;

  HANDLE file_map_handle =
  CreateFileMappingA(
    file_handle,
    0,
    PAGE_READWRITE,
    file_size.HighPart,
    file_size.LowPart,
    0
  );

  if(!file_map_handle) {
    DWORD err = GetLastError();
    PANICF("file failed map error code %d\n", err);
  }

  void *file_map_view = MapViewOfFile(file_map_handle, FILE_MAP_WRITE, 0, 0, 0);
  memory_copy(file_map_view, data.s, data.len);
  FlushViewOfFile(file_map_view, 0);

  UnmapViewOfFile(file_map_view);
  CloseHandle(file_map_handle);
  CloseHandle(file_handle);

  #endif

  return success;
}


platform_core_api void
func platform_sleep_ms(u32 ms) {
  Sleep((DWORD)ms);
}

platform_core_api Keyboard_modifier
func platform_get_keyboard_modifiers(void) {
  Keyboard_modifier modifier_mask = 0;

  if(GetKeyState(VK_CONTROL) & 0x8000) {
    modifier_mask |= KBD_MOD_CONTROL;
  }

  if(GetKeyState(VK_SHIFT) & 0x8000) {
    modifier_mask |= KBD_MOD_SHIFT;
  }

  if(GetKeyState(VK_MENU) & 0x8000) {
    modifier_mask |= KBD_MOD_ALT;
  }

  if((GetKeyState(VK_LWIN) & 0x8000) || (GetKeyState(VK_RWIN) & 0x8000)) {
    modifier_mask |= KBD_MOD_META;
  }

  return modifier_mask;
}


#endif
