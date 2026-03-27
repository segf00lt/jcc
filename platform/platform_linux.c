#ifndef PLATFORM_LINUX_C
#define PLATFORM_LINUX_C

// globals

Arena *main_arena;
App *ap;
struct stat st;

// functions


#ifdef APP_HOTRELOAD
typedef App* App_init_proc(Platform *);
typedef void App_service_proc(App *);
void *app_module;
App_init_proc *app_init;
App_service_proc *app_update_and_render;
App_service_proc *app_get_sound_samples;

internal void
func platform_linux_load_app_code(void) {


  {
    struct stat st;
    int result = (stat("app.so", &st) == 0);
    if(!result) {
      result = (stat("app.so.live", &st) == 0);
      if(!result) {
        UNREACHABLE;
      }
    } else {
      if(rename("app.so", "app.so.live") == 1) {
        UNREACHABLE;
      }
    }

  }

  app_module = dlopen("app.so.live");
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
func platform_linux_unload_app_code(void) {
  dlclose(app_module);
  app_init = 0;
  app_update_and_render = 0;
  app_get_sound_samples = 0;
  app_module = 0;
}

#endif



int main(int argc, char **argv) {

  { /* init */

    main_arena = arena_create_ex(MB(5), 0, 0);



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
    #endif

    SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    InitWindow(1000, 800, "prompt");
    SetTargetFPS(60);
    SetTraceLogLevel(LOG_DEBUG);
    SetExitKey(0);

    platform_linux_load_app_code();

    ap = app_init();

  } /* init */

  { /* main */

    for(;;) {

      if(ap->flags & APP_QUIT) {
        CloseWindow();
        break;
      }

      #ifdef APP_HOTRELOAD
      if(platform_file_exists(MODULE".so")) {
        platform_linux_unload_app_code();
        platform_linux_load_app_code();
        return;
      }
      #endif

      {
        if(IsKeyPressed(KEY_ENTER)) {
          ap->say_hello_to_model = true;
        }
      }

      app_update_new(ap);

      { /* render */
        Color color = RED;
        local_persist b32 flip = 1;
        if(flip) {
          color = BLUE;
        }



        BeginDrawing();


        ClearBackground(color);


        EndDrawing();

        if(WindowShouldClose()) {
          CloseWindow();
          break;
        }
        flip = !flip;

      } /* render */


    }

  } /* main */

  return 0;
}


#endif
