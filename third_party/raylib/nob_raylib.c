
#ifndef NOB_IMPLEMENTATION

#define NOB_IMPLEMENTATION
#include "nob.h"

#endif

#define RAYLIB_PATH "./third_party/raylib"

enum {
  RAYLIB_BUILD_FIRST = 0,
  RAYLIB_BUILD_STATIC = 0,
  RAYLIB_BUILD_SHARED,
  RAYLIB_BUILD_DEBUG,
  RAYLIB_BUILD_WEB,
  RAYLIB_BUILD_COUNT,
};

char *raylib_build_name_strings[RAYLIB_BUILD_COUNT] = {
  [RAYLIB_BUILD_STATIC] = "static",
  [RAYLIB_BUILD_SHARED] = "shared",
  [RAYLIB_BUILD_DEBUG]  = "debug",
  [RAYLIB_BUILD_WEB]    = "web",
};

char *raylib_compiler_linux = "clang";
char *raylib_compiler_mac = "clang";
char *raylib_compiler_windows = "cl";
char *raylib_compiler_web = "emcc";

char *raylib_linker_linux = "ar";
char *raylib_linker_mac = "ar";

char *shared_build_cflag_mac = "-dynamiclib";
char *shared_build_ext_mac = ".dylib";

char *shared_build_cflag_linux = "-shared";
char *shared_build_ext_linux = ".so";

char *raylib_cflags_mac[] = {
  "-Wall",
  "-D_GNU_SOURCE",
  "-DPLATFORM_DESKTOP_GLFW",
  "-DGRAPHICS_API_OPENGL_33",
  "-Wno-missing-braces",
  "-Werror=pointer-arith",
  "-fno-strict-aliasing",
  "-std=c99",
  "-O1",
  "-Werror=implicit-function-declaration",
};

char *raylib_cflags_linux[] = {
  "-Wall",
  "-D_GNU_SOURCE",
  "-DPLATFORM_DESKTOP_GLFW",
  "-DGRAPHICS_API_OPENGL_33",
  "-Wno-missing-braces",
  "-Werror=pointer-arith",
  "-fno-strict-aliasing",
  "-std=c99",
  "-fPIC",
  "-O1",
  "-Werror=implicit-function-declaration",
  "-D_GLFW_X11",
};

char *raylib_cflags_web[] = {
  "-Wall",
  "-D_GNU_SOURCE",
  "-DPLATFORM_WEB",
  "-DGRAPHICS_API_OPENGL_ES2",
  "-Wno-missing-braces",
  "-Werror=pointer-arith",
  "-fno-strict-aliasing",
  "-std=gnu99",
  "-fPIC",
  "-Os",
  "-Os",
  "-sWASM_WORKERS=1",
  "-matomics",
  "-mbulk-memory",
  "-sUSE_PTHREADS=1",
};

char *raylib_debug_cflags_mac[] = {
  "-g",
  "-Wall",
  "-D_GNU_SOURCE",
  "-DPLATFORM_DESKTOP_GLFW",
  "-DGRAPHICS_API_OPENGL_33",
  "-Wno-missing-braces",
  "-Werror=pointer-arith",
  "-fno-strict-aliasing",
  "-std=c99",
  "-O0",
  "-Werror=implicit-function-declaration",
};

char *raylib_debug_cflags_linux[] = {
  "-g",
  "-Wall",
  "-D_GNU_SOURCE",
  "-DPLATFORM_DESKTOP_GLFW",
  "-DGRAPHICS_API_OPENGL_33",
  "-Wno-missing-braces",
  "-Werror=pointer-arith",
  "-fno-strict-aliasing",
  "-std=c99",
  "-fPIC",
  "-O0",
  "-Werror=implicit-function-declaration",
  "-D_GLFW_X11",
};

char *raylib_ldflags_mac[] = {
  "-install_name",
  "@rpath/libraylib.dylib",
  "-framework",
  "OpenGL",
  "-framework",
  "Cocoa",
  "-framework",
  "IOKit",
  "-framework",
  "CoreAudio",
  "-framework",
  "CoreVideo",
};

char *raylib_ldflags_linux[] = {
  "-Wl,-soname,libraylib.so",
  "-lGL",
  "-lc",
  "-lm",
  "-lpthread",
  "-ldl",
  "-lrt",
};

char *raylib_include_flags[] = {
  "-I./third_party/raylib",
  "-I./third_party/raylib/external/glfw/include",
};


#define RAYLIB_FILES \
X(rcore) \
X(rshapes) \
X(rtextures) \
X(rtext) \
X(utils) \
X(rglfw) \
X(rmodels) \
X(raudio) \

#define RAYLIB_FILES_WEB \
X(rcore) \
X(rshapes) \
X(rtextures) \
X(rtext) \
X(utils) \
X(rmodels) \
X(raudio) \

char *raylib_src_files[] = {
  #define X(x) RAYLIB_PATH"/"#x".c",
  RAYLIB_FILES
  #undef X
};

char *raylib_object_files[] = {
  #define X(x) RAYLIB_PATH"/"#x".o",
  RAYLIB_FILES
  #undef X
};

char *raylib_files[] = {
  #define X(x) #x,
  RAYLIB_FILES
  #undef X
};

char *raylib_src_files_web[] = {
  #define X(x) RAYLIB_PATH"/"#x".c",
  RAYLIB_FILES_WEB
  #undef X
};

char *raylib_object_files_web[] = {
  #define X(x) RAYLIB_PATH"/"#x".o",
  RAYLIB_FILES_WEB
  #undef X
};

char *raylib_files_web[] = {
  #define X(x) #x,
  RAYLIB_FILES_WEB
  #undef X
};



#define RAYLIB_SHARED_LIB_NAME_WINDOWS "raylib.dll"
#define RAYLIB_SHARED_LIB_NAME_MAC "libraylib.dylib"
#define RAYLIB_SHARED_LIB_NAME_LINUX "libraylib.so"


#define RAYLIB_BUILD_DIR             RAYLIB_PATH"/build"

#define RAYLIB_STATIC_BUILD_DIR      RAYLIB_BUILD_DIR"/static"
#define RAYLIB_SHARED_BUILD_DIR      RAYLIB_BUILD_DIR"/shared"
#define RAYLIB_DEBUG_BUILD_DIR       RAYLIB_BUILD_DIR"/debug"
#define RAYLIB_WEB_BUILD_DIR         RAYLIB_BUILD_DIR"/web"

#define RAYLIB_STATIC_LIB_PATH       RAYLIB_STATIC_BUILD_DIR"/libraylib.a"

#define RAYLIB_SHARED_LIB_PATH_WINDOWS     RAYLIB_SHARED_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_WINDOWS
#define RAYLIB_DEBUG_LIB_PATH_WINDOWS      RAYLIB_DEBUG_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_WINDOWS

#define RAYLIB_SHARED_LIB_PATH_MAC         RAYLIB_SHARED_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_MAC
#define RAYLIB_DEBUG_LIB_PATH_MAC          RAYLIB_DEBUG_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_MAC

#define RAYLIB_SHARED_LIB_PATH_LINUX       RAYLIB_SHARED_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_LINUX
#define RAYLIB_DEBUG_LIB_PATH_LINUX        RAYLIB_DEBUG_BUILD_DIR"/"RAYLIB_SHARED_LIB_NAME_LINUX

#define RAYLIB_WEB_LIB_PATH          RAYLIB_WEB_BUILD_DIR"/libraylib.web.a"




int build_raylib_mac(void) {
  nob_log(NOB_INFO, "building raylib");

  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_STATIC_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_SHARED_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_DEBUG_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_WEB_BUILD_DIR));

  Nob_Cmd compile_cmds[RAYLIB_BUILD_COUNT * NOB_ARRAY_LEN(raylib_files)] = {0};
  int compile_cmds_count = 0;

  Nob_Cmd linker_cmds[RAYLIB_BUILD_COUNT] = {0};
  int linker_cmds_count = 0;

  for(int build_type = RAYLIB_BUILD_FIRST; build_type < RAYLIB_BUILD_COUNT; build_type++) {
    nob_log(NOB_INFO, "%s build", raylib_build_name_strings[build_type]);

    if(build_type == RAYLIB_BUILD_WEB) {
      nob_log(NOB_INFO, "skipping web build");
      continue; // TODO jfd: raylib web build
    } else {

      for(int i = 0; i < NOB_ARRAY_LEN(raylib_files); i++) {
        NOB_ASSERT(compile_cmds_count < NOB_ARRAY_LEN(compile_cmds));

        Nob_Cmd *compile_cmd = &compile_cmds[compile_cmds_count++];

        nob_cmd_append(compile_cmd,
          raylib_compiler_mac,
          "-x",
          "objective-c",
          "-c",
          raylib_src_files[i],
          "-o",
          raylib_object_files[i],
        );

        if(build_type == RAYLIB_BUILD_DEBUG) {
          nob_da_append_many(compile_cmd, raylib_debug_cflags_mac, NOB_ARRAY_LEN(raylib_debug_cflags_mac));
        } else {
          nob_da_append_many(compile_cmd, raylib_cflags_mac, NOB_ARRAY_LEN(raylib_cflags_mac));
        }

        nob_da_append_many(compile_cmd, raylib_include_flags, NOB_ARRAY_LEN(raylib_include_flags));

        if(!strcmp("rglfw", raylib_files[i])) {
          nob_cmd_append(compile_cmd, "-U_GNU_SOURCE");
        }

      }

    }


    NOB_ASSERT(linker_cmds_count < NOB_ARRAY_LEN(linker_cmds));
    Nob_Cmd *linker_cmd = &linker_cmds[linker_cmds_count++];

    if(build_type == RAYLIB_BUILD_DEBUG) {
      nob_cmd_append(linker_cmd,
        raylib_compiler_mac,
        shared_build_cflag_mac,
        "-o",
        RAYLIB_DEBUG_LIB_PATH_MAC
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));
      nob_da_append_many(linker_cmd, raylib_ldflags_mac, NOB_ARRAY_LEN(raylib_ldflags_mac));

    } else if(build_type == RAYLIB_BUILD_SHARED) {
      nob_cmd_append(linker_cmd,
        raylib_compiler_mac,
        shared_build_cflag_mac,
        "-o",
        RAYLIB_SHARED_LIB_PATH_MAC
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));
      nob_da_append_many(linker_cmd, raylib_ldflags_mac, NOB_ARRAY_LEN(raylib_ldflags_mac));

    } else if(build_type == RAYLIB_BUILD_STATIC) {
      nob_cmd_append(linker_cmd,
        raylib_linker_mac,
        "rcs",
        RAYLIB_STATIC_LIB_PATH
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));

    } else if(build_type == RAYLIB_BUILD_WEB) {
      NOB_UNREACHABLE("web builds unimplemented");
    } else {
      NOB_UNREACHABLE("unsupported build type");
    }

  }

  nob_log(NOB_INFO, "compiling all builds of raylib");
  Nob_Procs procs = {0};

  for(int i = 0; i < compile_cmds_count; i++) {
    nob_procs_append_with_flush(&procs, nob_cmd_run_async(compile_cmds[i]), 512);
  }

  NOB_ASSERT(nob_procs_wait_and_reset(&procs));

  nob_log(NOB_INFO, "linking all builds of raylib");

  for(int i = 0; i < linker_cmds_count; i++) {
    nob_procs_append_with_flush(&procs, nob_cmd_run_async(linker_cmds[i]), 512);
  }

  NOB_ASSERT(nob_procs_wait_and_reset(&procs));

  return 1;
}


int build_raylib_linux(void) {
  nob_log(NOB_INFO, "building raylib");

  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_STATIC_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_SHARED_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_DEBUG_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_WEB_BUILD_DIR));

  Nob_Cmd compile_cmds[RAYLIB_BUILD_COUNT * NOB_ARRAY_LEN(raylib_files)] = {0};
  int compile_cmds_count = 0;

  Nob_Cmd linker_cmds[RAYLIB_BUILD_COUNT] = {0};
  int linker_cmds_count = 0;

  for(int build_type = RAYLIB_BUILD_FIRST; build_type < RAYLIB_BUILD_COUNT; build_type++) {
    nob_log(NOB_INFO, "%s build", raylib_build_name_strings[build_type]);

    if(build_type == RAYLIB_BUILD_WEB) {
      nob_log(NOB_INFO, "skipping web build");
      continue; // TODO jfd: raylib web build
    } else {

      for(int i = 0; i < NOB_ARRAY_LEN(raylib_files); i++) {
        NOB_ASSERT(compile_cmds_count < NOB_ARRAY_LEN(compile_cmds));

        Nob_Cmd *compile_cmd = &compile_cmds[compile_cmds_count++];

        nob_cmd_append(compile_cmd,
          raylib_compiler_linux,
          "-c",
          raylib_src_files[i],
          "-o",
          raylib_object_files[i],
        );

        if(build_type == RAYLIB_BUILD_DEBUG) {
          nob_da_append_many(compile_cmd, raylib_debug_cflags_linux, NOB_ARRAY_LEN(raylib_debug_cflags_linux));
        } else {
          nob_da_append_many(compile_cmd, raylib_cflags_linux, NOB_ARRAY_LEN(raylib_cflags_linux));
        }

        nob_da_append_many(compile_cmd, raylib_include_flags, NOB_ARRAY_LEN(raylib_include_flags));

        if(!strcmp("rglfw", raylib_files[i])) {
          nob_cmd_append(compile_cmd, "-U_GNU_SOURCE");
        }

      }

    }


    NOB_ASSERT(linker_cmds_count < NOB_ARRAY_LEN(linker_cmds));
    Nob_Cmd *linker_cmd = &linker_cmds[linker_cmds_count++];

    if(build_type == RAYLIB_BUILD_DEBUG) {
      nob_cmd_append(linker_cmd,
        raylib_compiler_linux,
        shared_build_cflag_linux,
        "-o",
        RAYLIB_DEBUG_LIB_PATH_LINUX
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));
      nob_da_append_many(linker_cmd, raylib_ldflags_linux, NOB_ARRAY_LEN(raylib_ldflags_linux));

    } else if(build_type == RAYLIB_BUILD_SHARED) {
      nob_cmd_append(linker_cmd,
        raylib_compiler_linux,
        shared_build_cflag_linux,
        "-o",
        RAYLIB_SHARED_LIB_PATH_LINUX
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));
      nob_da_append_many(linker_cmd, raylib_ldflags_linux, NOB_ARRAY_LEN(raylib_ldflags_linux));

    } else if(build_type == RAYLIB_BUILD_STATIC) {
      nob_cmd_append(linker_cmd,
        raylib_linker_linux,
        "rcs",
        RAYLIB_STATIC_LIB_PATH
      );
      nob_da_append_many(linker_cmd, raylib_object_files, NOB_ARRAY_LEN(raylib_object_files));

    } else if(build_type == RAYLIB_BUILD_WEB) {
      NOB_UNREACHABLE("web builds unimplemented");
    } else {
      NOB_UNREACHABLE("unsupported build type");
    }

  }

  nob_log(NOB_INFO, "compiling all builds of raylib");
  Nob_Procs procs = {0};

  for(int i = 0; i < compile_cmds_count; i++) {
    nob_procs_append_with_flush(&procs, nob_cmd_run_async(compile_cmds[i]), 512);
  }

  NOB_ASSERT(nob_procs_wait_and_reset(&procs));

  nob_log(NOB_INFO, "linking all builds of raylib");

  for(int i = 0; i < linker_cmds_count; i++) {
    nob_procs_append_with_flush(&procs, nob_cmd_run_async(linker_cmds[i]), 512);
  }

  NOB_ASSERT(nob_procs_wait_and_reset(&procs));

  return 1;
}


int build_raylib_win32(void) {
  nob_log(NOB_INFO, "building raylib");

  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_STATIC_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_SHARED_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_DEBUG_BUILD_DIR));
  NOB_ASSERT(nob_mkdir_if_not_exists(RAYLIB_WEB_BUILD_DIR));

  Nob_Procs compile_procs = {0};

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "cl",
      "/nologo",
      "/c",
      "/O2",
      "/MD",
      "/DPLATFORM_DESKTOP_GLFW",
      "/DGRAPHICS_API_OPENGL_33",
      "/Ithird_party\\raylib\\",
      "/Ithird_party\\raylib\\external\\glfw\\include",
      "/Fo:third_party\\raylib\\build\\static\\",
      "third_party\\raylib\\rcore.c",
      "third_party\\raylib\\rshapes.c",
      "third_party\\raylib\\rtextures.c",
      "third_party\\raylib\\rtext.c",
      "third_party\\raylib\\utils.c",
      "third_party\\raylib\\rglfw.c",
      "third_party\\raylib\\rmodels.c",
      "third_party\\raylib\\raudio.c"
    );
    nob_procs_append_with_flush(&compile_procs, nob_cmd_run_async(cmd), 1024);
  }

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "cl",
      "/nologo",
      "/c",
      "/O2",
      "/MD",
      "/DBUILD_LIBTYPE_SHARED",
      "/DPLATFORM_DESKTOP_GLFW",
      "/DGRAPHICS_API_OPENGL_33",
      "/Ithird_party\\raylib\\",
      "/Ithird_party\\raylib\\external\\glfw\\include",
      "/Fo:third_party\\raylib\\build\\shared\\",
      "third_party\\raylib\\rcore.c",
      "third_party\\raylib\\rshapes.c",
      "third_party\\raylib\\rtextures.c",
      "third_party\\raylib\\rtext.c",
      "third_party\\raylib\\utils.c",
      "third_party\\raylib\\rglfw.c",
      "third_party\\raylib\\rmodels.c",
      "third_party\\raylib\\raudio.c"
    );
    nob_procs_append_with_flush(&compile_procs, nob_cmd_run_async(cmd), 1024);
  }

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "cl",
      "/nologo",
      "/c",
      "/Z7",
      "/Od",
      "/MDd",
      "/D_DEBUG",
      "/DBUILD_LIBTYPE_SHARED",
      "/DPLATFORM_DESKTOP_GLFW",
      "/DGRAPHICS_API_OPENGL_33",
      "/Ithird_party\\raylib\\",
      "/Ithird_party\\raylib\\external\\glfw\\include",
      "/Fo:third_party\\raylib\\build\\debug\\",
      "third_party\\raylib\\rcore.c",
      "third_party\\raylib\\rshapes.c",
      "third_party\\raylib\\rtextures.c",
      "third_party\\raylib\\rtext.c",
      "third_party\\raylib\\utils.c",
      "third_party\\raylib\\rglfw.c",
      "third_party\\raylib\\rmodels.c",
      "third_party\\raylib\\raudio.c"
    );
    nob_procs_append_with_flush(&compile_procs, nob_cmd_run_async(cmd), 1024);
  }

  NOB_ASSERT(nob_procs_wait(compile_procs));
  Nob_Procs link_procs = {0};

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "lib",
      "/nologo",
      "/OUT:third_party\\raylib\\build\\static\\raylib.lib",
      "third_party\\raylib\\build\\static\\rcore.obj",
      "third_party\\raylib\\build\\static\\rshapes.obj",
      "third_party\\raylib\\build\\static\\rtextures.obj",
      "third_party\\raylib\\build\\static\\rtext.obj",
      "third_party\\raylib\\build\\static\\utils.obj",
      "third_party\\raylib\\build\\static\\rglfw.obj",
      "third_party\\raylib\\build\\static\\rmodels.obj",
      "third_party\\raylib\\build\\static\\raudio.obj"
    );
    nob_procs_append_with_flush(&link_procs, nob_cmd_run_async(cmd), 1024);
  }

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "link",
      "/nologo",
      "/DLL",
      "/OUT:third_party\\raylib\\build\\shared\\raylib.dll",
      "/IMPLIB:third_party\\raylib\\build\\shared\\raylib.lib",
      "third_party\\raylib\\build\\shared\\rcore.obj",
      "third_party\\raylib\\build\\shared\\rshapes.obj",
      "third_party\\raylib\\build\\shared\\rtextures.obj",
      "third_party\\raylib\\build\\shared\\rtext.obj",
      "third_party\\raylib\\build\\shared\\utils.obj",
      "third_party\\raylib\\build\\shared\\rglfw.obj",
      "third_party\\raylib\\build\\shared\\rmodels.obj",
      "third_party\\raylib\\build\\shared\\raudio.obj",
      "user32.lib",
      "opengl32.lib",
      "gdi32.lib",
      "winmm.lib",
      "shell32.lib"
    );
    nob_procs_append_with_flush(&link_procs, nob_cmd_run_async(cmd), 1024);
  }

  {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd,
      "link",
      "/nologo",
      "/DLL",
      "/DEBUG",
      "/INCREMENTAL:NO",
      "/PDB:third_party\\raylib\\build\\debug\\raylib.pdb",
      "/OUT:third_party\\raylib\\build\\debug\\raylib.dll",
      "/IMPLIB:third_party\\raylib\\build\\debug\\raylib.lib",
      "third_party\\raylib\\build\\debug\\rcore.obj",
      "third_party\\raylib\\build\\debug\\rshapes.obj",
      "third_party\\raylib\\build\\debug\\rtextures.obj",
      "third_party\\raylib\\build\\debug\\rtext.obj",
      "third_party\\raylib\\build\\debug\\utils.obj",
      "third_party\\raylib\\build\\debug\\rglfw.obj",
      "third_party\\raylib\\build\\debug\\rmodels.obj",
      "third_party\\raylib\\build\\debug\\raudio.obj",
      "user32.lib",
      "opengl32.lib",
      "gdi32.lib",
      "winmm.lib",
      "shell32.lib"
    );
    nob_procs_append_with_flush(&link_procs, nob_cmd_run_async(cmd), 1024);
  }

  NOB_ASSERT(nob_procs_wait(link_procs));

  return 1;
}
