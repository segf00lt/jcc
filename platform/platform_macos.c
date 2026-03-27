#ifndef PLATFORM_MACOS_C
#define PLATFORM_MACOS_C

// globals

global Arena *platform_arena;



#ifdef APP_HOTRELOAD
typedef App* App_init_proc(Platform *);
typedef void App_service_proc(App *);
void *app_module;
App_init_proc *app_init;
App_service_proc *app_update_and_render;
App_service_proc *app_get_sound_samples;

internal void
func platform_macos_load_app_code(void) {


  {
    struct stat st;
    int result = (stat("app.dylib", &st) == 0);
    if(!result) {
      result = (stat("app.dylib.live", &st) == 0);
      if(!result) {
        UNREACHABLE;
      }
    } else {
      if(rename("app.dylib", "app.dylib.live") == 1) {
        UNREACHABLE;
      }
    }

  }

  app_module = dlopen("app.dylib.live", RTLD_NOW);
  if(!app_module) {
    UNREACHABLE;
  }

  app_init = (App_init_proc*)dlsym(app_module, "app_init");
  ASSERT(app_init);

  app_update_and_render = (App_service_proc*)dlsym(app_module, "app_update_and_render");
  ASSERT(app_update_and_render);

  app_get_sound_samples = (App_service_proc*)dlsym(app_module, "app_get_sound_samples");

}

internal void
func platform_macos_unload_app_code(void) {
  dlclose(app_module);
  app_init = 0;
  app_update_and_render = 0;
  app_get_sound_samples = 0;
  app_module = 0;
}

#endif


// functions

int main(void) {

  #if 0
  u64 platform_page_size = platform_macos_get_page_size();

  u64 platform_arena_page_count = 10;
  u64 platform_arena_bytes = platform_arena_page_count * platform_page_size;

  void *platform_memory_block = platform_macos_debug_alloc_memory_block(platform_arena_page_count);

  platform_arena = arena_create_ex(platform_arena_bytes, 1, platform_memory_block);
  #else

  platform_arena = arena_create_ex(MB(5), 0, 0);

  #endif

  #ifdef APP_HOTRELOAD
  platform_macos_load_app_code();
  #endif

  App *ap = app_init(0);

  #if 0
  Str8 texts[] = {
    str8_lit("hello"),
    str8_lit("goodbye"),
  };
  Str8_list texts_list = {0};
  for(int i = 0; i < (int)ARRLEN(texts); i++) {
    str8_list_append_str(ap->main_arena, &texts_list, texts[i]);
  }
  Embedding_vector_slice vectors = get_embedding_vectors_for_texts(ap, texts_list);
  ASSERT(vectors.count == texts_list.count);
  #else
  for(;;) {

    #ifdef APP_HOTRELOAD
    if(platform_file_exists("app.dylib")) {
      platform_macos_unload_app_code();
      platform_macos_load_app_code();
    }
    #endif

    if(ap->flags & APP_QUIT) {
      break;
    }

    app_update_and_render(ap);

  }
  #endif

  return 0;
}


internal u64
func platform_macos_get_page_size(void) {
  vm_size_t page;
  host_page_size(mach_host_self(), &page);

  u64 result = (u64)page;

  return result;
}

internal u8*
func platform_macos_debug_alloc_memory_block(u64 page_count) {
  mach_vm_address_t base = 0x200000000;

  u64 page_size = platform_macos_get_page_size();

  mach_vm_size_t size = page_size * page_count;

  kern_return_t kr = mach_vm_allocate(
    mach_task_self(),
    &base,
    size,
    VM_FLAGS_FIXED
  );

  if(kr != KERN_SUCCESS) {
    fprintf(stderr, "mach_vm_allocate failed: %s (%d)\n",
      mach_error_string(kr), kr);
    return 0;
  }

  kr = mach_vm_protect(
    mach_task_self(),
    base,
    size,
    FALSE,
    VM_PROT_READ | VM_PROT_WRITE
  );

  if(kr != KERN_SUCCESS) {
    fprintf(stderr, "mach_vm_allocate failed: %s (%d)\n",
      mach_error_string(kr), kr);
    return 0;
  }

  return (u8*)(void*)base;
}



#endif
