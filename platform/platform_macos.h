#ifndef PLATFORM_MACOS_H
#define PLATFORM_MACOS_H

// NOTE jfd: mach.h uses internal somewhere
#undef internal

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <dlfcn.h>

#include <mach/mach.h>
#include <mach/mach_vm.h>


#define internal static



internal u64        platform_macos_get_page_size(void);
internal u8*        platform_macos_debug_alloc_memory_block(u64 page_count);


#endif
