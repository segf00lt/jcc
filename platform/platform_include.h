#ifndef PLATFORM_INCLUDE_H
#define PLATFORM_INCLUDE_H


#include "platform.h"


#if PLATFORM_WINDOWS

#include "platform_win32.h"

#elif PLATFORM_LINUX

#include "platform_linux.h"

#elif PLATFORM_MAC

#include "platform_macos.h"

#elif PLATFORM_WEB

#error unsupported platform

#else
#error unsupported platform
#endif



#endif
