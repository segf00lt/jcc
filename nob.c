#define NOB_IMPLEMENTATION
#include "nob.h"

#include "third_party/raylib/nob_raylib.c"

int win32_build(void) {
  Nob_Cmd cmd = {0};
  nob_cmd_append(&cmd,
    "cl",
    "/nologo",
    "/W4",
    "/wd4100",
    "/wd4201",
    "/wd4459",
    "/wd4477",
    "/wd4063",
    "/wd4456",
    "/wd4244",
    "/wd4146",
    "/wd4267",
    "/Zi",
    "/Od",
    "/MDd",
    "/Fe:jcc.exe",
    "jcc_build.c",
    "third_party\\dyncall-1.4\\dyncall\\dyncall_s.lib",
    "third_party\\dyncall-1.4\\dynload\\dynload_s.lib",
    "user32.lib",
    "dxguid.lib",
    "winmm.lib",
    "ole32.lib",
    "/link",
    "/INCREMENTAL:NO",
    ""
  );
  if(!nob_cmd_run_sync_and_reset(&cmd)) return 0;
  return 1;
}

int win32_test_dyncall_build(void) {
  Nob_Cmd cmd = {0};
  nob_cmd_append(&cmd,
    "cl",
    "/nologo",
    "/W4",
    "/wd4100",
    "/wd4201",
    "/wd4459",
    "/wd4477",
    "/wd4063",
    "/wd4456",
    "/wd4244",
    "/wd4146",
    "/wd4267",
    "/Zi",
    "/Od",
    "/MDd",
    "/Fe:test_dyncall.exe",
    "test_dyncall_build.c",
    "third_party\\dyncall-1.4\\dyncall\\dyncall_s.lib",
    "third_party\\dyncall-1.4\\dynload\\dynload_s.lib",
    "user32.lib",
    "dxguid.lib",
    "winmm.lib",
    "ole32.lib",
    "/link",
    "/INCREMENTAL:NO",
    ""
  );
  if(!nob_cmd_run_sync_and_reset(&cmd)) return 0;
  return 1;
}

int main(int argc, char **argv) {
  NOB_GO_REBUILD_URSELF(argc, argv);

  if(!win32_build()) return 1;

  return 0;
  if(!build_raylib_win32()) return 1;
  if(!win32_test_dyncall_build()) return 1;


  return 0;
}

