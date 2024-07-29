#include <stdio.h>
#include <string.h>
#include <dlfcn.h>

int main(void) {
    void *wrapper_handle = dlopen("libc/jcc_wrap_foo.dylib", RTLD_NOW);
    void (*myfoo)(int) = dlsym(wrapper_handle, "__jcc__wrap__foo");

    myfoo(12);

    dlclose(wrapper_handle);

    return 0;
}
