#define TYPEINFODECL                \
    X(void    ,  VOID     ,   0)    \
    X(int     ,  INT      ,   1)    \
    X(float   ,  FLOAT    ,   2)    \
    X(bool    ,  BOOL     ,   2)    \
    X(type    ,  TYPE     ,   4)    \
    X(pointer ,  POINTER  ,   5)    \
    X(array   ,  ARRAY    ,   6)    \
    X(string  ,  STRING   ,   7)    \
    X(struct  ,  STRUCT   ,   8)    \
    X(enum    ,  ENUM     ,   9)    \
    X(proc    ,  PROC     ,  10)    \
    X(any     ,  ANY      ,  11)    \

typedef struct Type_info Type_info;
typedef struct Type_info_struct_member Type_info_struct_member;
#define X(a, b, c) typedef struct Type_info_##a Type_info_##a;
TYPEINFODECL
#undef X

typedef enum Type_info_tag {
    TYPE_INFO_TAG_INVALID = -1,
#define X(a, b, c) TYPE_INFO_TAG_##b = c,
    TYPEINFODECL
#undef X
} Type_info_tag;

typedef enum Allocator_mode {
    ALLOC_MODE_ALLOCATE = 0,
    ALLOC_MODE_RESIZE   = 1,
    ALLOC_MODE_FREE     = 2,
    ALLOC_MODE_FREE_ALL = 3,
} Allocator_mode;

typedef struct String_view String_view; // this will be our internal version of the language's string
typedef void* (*Allocator)(s32 mode, s64 size, s64 oldsize, void *old_memory_ptr, void *allocator_data, s64 options);
typedef struct Array_view Array_view;
typedef struct Dynamic_array Dynamic_array;
typedef struct Any Any;

struct String_view {
    char *data;
    u64 len;
};

struct Array_view {
    void *data;
    u64 count;
};

struct Dynamic_array {
    void *data;
    u64 count;
    u64 cap;
    Allocator allocator;
    void *allocator_data;
};

struct Any {
    void *data;
    Type_info *type;
};

struct Type_info {
    Type_info_tag tag;
};

struct Type_info_int {
    Type_info base;

    u32 bits;
    bool sign;
};

struct Type_info_float {
    Type_info base;

    u32 bits;
};

struct Type_info_pointer {
    Type_info base;

    Type_info *pointer_to;
};

struct Type_info_array {
    Type_info base;

    Type_info *array_of;
    u64 array_count;
    u32 array_kind; // STATIC, DYNAMIC, VIEW
};

//TODO say if member is constant or was imported with using
struct Type_info_struct_member {
    String_view name;
    Type_info *type;
    u64 offset;
};

struct Type_info_struct {
    Type_info base;

    String_view name;
    Array_view members;
};

struct Type_info_proc {
    Type_info base;

    Array_view param_types;
    Array_view return_types;
};

