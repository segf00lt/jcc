#ifndef PLATFORM_INCLUDE_C
#define PLATFORM_INCLUDE_C


#include "platform.c"


#if PLATFORM_WINDOWS

#include "platform_core_win32.c"
#include "platform_win32.c"

#elif PLATFORM_LINUX

#include "platform_core_win32.c"
#include "platform_linux.c"

#elif PLATFORM_MAC

#include "platform_core_macos.c"
#include "platform_macos.c"

#elif PLATFORM_WEB

#error unsupported platform

#else
#error unsupported platform
#endif



#endif
