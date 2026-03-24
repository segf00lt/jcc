#define _UNITY_BUILD_


#include "base/base_include.h"
#include "platform/platform.h"

#include "third_party/stb/stb_sprintf.h"
#include "third_party/stb/stb_ds.h"
#include "third_party/dyncall-1.4/dyncall/dyncall.h"
#include "third_party/dyncall-1.4/dynload/dynload.h"


#include "pool.h"

#include "lexer.c"
#include "preload.c"

#include "jcc.c"

#include "base/base_include.c"

#include "platform/platform.c"
#include "platform/platform_core_win32.c"
