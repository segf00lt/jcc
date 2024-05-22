#include <raylib.h>
#include <stdarg.h>
#include "basic.h"
#include "stb_ds.h"
#include "stb_sprintf.h"
#include "pool.h"
#include "arena.h"

#include "lexer.c"


#define PIPE_STAGES              \
    X(NONE)                      \
    X(PARSE)                     \
    X(TYPECHECK)                 \
    X(SIZE)                      \
    X(IR)                        \

#define JOB_STATES               \
    X(READY)                     \
    X(WAIT)                      \
    X(ERROR)                     \

#define TYPECHECK_STEPS                 \
    X(NONE)                             \
    X(VARDECL_BEGIN)                    \
    X(VARDECL_BIND_TYPE)                \
    X(VARDECL_INITIALIZE)               \
    X(VARDECL_END)                      \
    X(PROCDECL_BEGIN)                   \
    X(PROCDECL_PARAMS_BEGIN)            \
    X(PROCDECL_PARAMS_BIND_TYPE)        \
    X(PROCDECL_PARAMS_INITIALIZE)       \
    X(PROCDECL_PARAMS_END)              \
    X(PROCDECL_RETURN_VALUES)           \
    X(PROCDECL_BODY)                    \
    X(PROCDECL_END)                     \
    X(STATEMENT_BEGIN)                  \
    X(STATEMENT_LEFT)                   \
    X(STATEMENT_RIGHT)                  \
    X(STATEMENT_END)                    \

#define ASTKINDS                 \
    X(ifstatement)               \
    X(switchstatement)           \
    X(casestatement)             \
    X(whilestatement)            \
    X(forstatement)              \
    X(breakstatement)            \
    X(continuestatement)         \
    X(returnstatement)           \
    X(statement)                 \
    X(block)                     \
    X(vardecl)                   \
    X(procdecl)                  \
    X(paramdecl)                 \
    X(retdecl)                   \
    X(expr_list)                 \
    X(expr)                      \
    X(param)                     \
    X(atom)                      \
    X(array_literal)             \
    X(call)                      \

#define OPERATOR_PREC_TABLE             \
    /* indexing */\
    X((Token)'.',                    10)\
    X((Token)'[',                    10)\
    /* multiplicative */\
    X((Token)'*',                    9)\
    X((Token)'/',                    9)\
    X((Token)'%',                    9)\
    /* additive */\
    X((Token)'+',                    8)\
    X((Token)'-',                    8)\
    /* bitwise */\
    X((Token)'^',                    7)\
    X((Token)'&',                    7)\
    X((Token)'|',                    7)\
    X(TOKEN_LSHIFT,                  6)\
    X(TOKEN_RSHIFT,                  6)\
    /* comparison */\
    X((Token)'<',                    5)\
    X((Token)'>',                    5)\
    X(TOKEN_LESSEQUAL,               5)\
    X(TOKEN_GREATEQUAL,              5)\
    X(TOKEN_EXCLAMEQUAL,             5)\
    X(TOKEN_EQUALEQUAL,              5)\
    /* logical */\
    X(TOKEN_AND,                     4)\
    X(TOKEN_OR,                      4)\

#define VALUEKINDS                      \
    X(NIL)                              \
    X(BOOL)                             \
    X(CHAR)                             \
    X(INT)                              \
    X(UINT)                             \
    X(FLOAT)                            \
    X(TYPE)                             \
    X(STRING)                           \
    X(ARRAY)                            \
    X(STRUCT)                           \
    X(PARAM)                            \
    X(TOKEN)                            \

#define TYPEKINDS                       \
    X(VOID)                             \
    X(BOOL)                             \
    X(CHAR)                             \
    X(S8)                               \
    X(U8)                               \
    X(S16)                              \
    X(U16)                              \
    X(S32)                              \
    X(U32)                              \
    X(S64)                              \
    X(U64)                              \
    X(INT)                              \
    X(FLOAT)                            \
    X(F32)                              \
    X(F64)                              \
    X(TYPE)                             \
    X(ARRAY)                            \
    X(DYNAMIC_ARRAY)                    \
    X(ARRAY_VIEW)                       \
    X(POINTER)                          \
    X(STRUCT)                           \
    X(UNION)                            \
    X(PROC)                             \

#define MESSAGEKINDS                    \
    X(ERROR)                            \
    X(WARNING)                          \
    X(INFO)                             \


#define TOKEN_TO_TYPEKIND(t) (Typekind)((t-TOKEN_VOID)+TYPE_KIND_VOID)


typedef struct Scope_entry* Scope;
typedef int Jobid;
typedef struct Value Value;
typedef struct Sym Sym;
typedef struct Type Type;
typedef struct Job Job;
typedef struct Job_memory Job_memory;
typedef struct Message Message;
typedef struct AST AST;
#define X(x) typedef struct AST_##x AST_##x;
ASTKINDS
#undef X


typedef enum ASTkind {
    AST_KIND_INVALID = -1,
#define X(x) AST_KIND_##x,
    ASTKINDS
#undef X
    AST_KIND_MAX,
} ASTkind;

typedef enum Typecheck_step {
    TYPECHECK_STEP_INVALID = -1,
#define X(s) TYPECHECK_STEP_##s,
    TYPECHECK_STEPS
#undef X
} Typecheck_step;

typedef enum Pipe_stage {
    PIPE_STAGE_INVALID = -1,
#define X(s) PIPE_STAGE_##s,
    PIPE_STAGES
#undef X
} Pipe_stage;

typedef enum Job_state {
    JOB_STATE_INVALID = -1,
#define X(s) JOB_STATE_##s,
    JOB_STATES
#undef X
} Job_state;

typedef enum Valuekind {
    VALUE_KIND_INVALID = -1,
#define X(x) VALUE_KIND_##x,
    VALUEKINDS
#undef X
} Valuekind;

typedef enum Typekind {
    TYPE_KIND_INVALID = -1,
#define X(x) TYPE_KIND_##x,
    TYPEKINDS
#undef X
} Typekind;

char *typekind_debug[] = {
#define X(x) #x,
    TYPEKINDS
#undef X
};

typedef enum Messagekind {
    MESSAGE_KIND_INVALID = -1,
#define X(x) MESSAGE_KIND_##x,
    MESSAGEKINDS
#undef X
} Messagekind;


struct Scope_entry {
    char *key;
    Sym *value;
};

struct Message {
    Messagekind kind;
    char *text;
    Loc_info loc;
};

struct Job_memory {
    struct {
        bool scratch : 1;
        bool value : 1;
        bool sym : 1;
        bool type : 1;
        bool ast : 1;
    } active;

    Arena                scratch;
    Pool                 value;
    Pool                 sym;
    Pool                 type;

#define X(x) Pool ast_##x;
    ASTKINDS
#undef X
};

struct Job {
    Jobid                id;

    Pipe_stage           pipe_stage;
    Job_state            state;
    Typecheck_step       step;

    char                *handling_name;
    char                *waiting_on_name;
    u64                  job_id_waiting;

    Lexer               *lexer;

    AST                 *root;

    Type                *cur_proc_type;

    /* blocks */
    Arr(Scope)           scopes;
    Arr(AST*)            tree_pos_stack;

    /* expressions */
    Arr(Value*)          value_stack;
    Arr(Type*)           type_stack;
    Arr(AST**)           expr;
    u64                  expr_pos;

    Type                *proc_type;
    AST_paramdecl       *cur_paramdecl;
    AST_retdecl         *cur_retdecl;

    /* return statement */
    Arr(AST**)           expr_list;
    u64                  expr_list_pos;

    bool                 is_top_level;

    Arr(Message)         messages;
    
    Job_memory allocator;
};

struct Sym {
    char *name;
    Loc_info loc;
    Jobid declared_by;
    bool constant;
    Type *type;
    union {
        Value *value;
    };
    AST *initializer;
};

struct AST {
    ASTkind kind;
    Loc_info loc;
};

struct AST_param {
    AST base;
    char *name;
    AST *value;
    AST_param *next;
};

struct AST_call {
    AST base;
    Type **type_annotation_list; /* because callables can return multiple values */
    Value **value_annotation_list;
    int n_types_returned;
    AST *callee;
    bool has_named_params;
    int n_params;
    int first_named_param;
    AST_param *params;
};

struct AST_expr {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    Token token;
    AST *left;
    AST *right;
};

struct AST_expr_list {
    AST base;
    AST *expr;
    AST_expr_list *next;
};

struct AST_array_literal {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    int n;
    AST *type;
    AST_expr_list *elements;
};

struct AST_atom {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    Sym *symbol_annotation;
    Token token;
    union {
        s64 integer;
        u64 uinteger;
        f32 floating;
        f64 dfloating;
        char character;
        char *text;
    };
};

struct AST_vardecl {
    AST base;
    AST *next;
    char *name;
    bool is_top_level;
    bool constant;
    bool uninitialized;
    AST *type;
    AST *init;
};

struct AST_paramdecl {
    AST base;
    AST_paramdecl *next;
    char *name;
    bool is_top_level;
    AST *type;
    AST *init;
    int index;
};

struct AST_retdecl {
    AST base;
    AST_retdecl *next;
    AST *expr;
    int index;
};

struct AST_ifstatement {
    AST base;
    AST *next;
    char *label;
    AST *condition;
    AST *body;
    AST *branch;
};

struct AST_switchstatement {
    AST base;
    AST *next;
    char *label;
    AST *expr;
    AST *cases;
};

struct AST_casestatement {
    AST base;
    AST_casestatement *next;
    AST *expr;
    AST *body;
};

struct AST_whilestatement {
    AST base;
    AST *next;
    char *label;
    AST *condition;
    AST *body;
};

struct AST_forstatement {
    AST base;
    AST *next;
    char *label;
    AST *expr;
    AST *body;
};

struct AST_breakstatement {
    AST base;
    AST *next;
    char *label;
};

struct AST_continuestatement {
    AST base;
    AST *next;
    char *label;
};

struct AST_returnstatement {
    AST base;
    AST *next;
    AST_expr_list *expr_list;
};

struct AST_block {
    AST base;
    bool declarative; /* NOTE struct and union */
    bool visited;
    AST *down;
    AST *next;
};

struct AST_statement {
    AST base;
    AST *next;
    bool deferred;
    Token assign_op;
    AST *left;
    AST *right;
};

struct AST_procdecl {
    AST base;
    AST *next;
    char *name;
    bool is_top_level;
    AST_paramdecl *params;
    AST_retdecl *rets;
    bool type_checked_body;
    bool has_defaults;
    bool varargs;
    int first_default_param;
    int n_params;
    int n_rets;
    AST *body;
};

struct Value {
    Valuekind kind;
    char *name;
    union {
        s64 integer;
        u64 uinteger;
        char character;
        bool boolean;
        f32 floating;
        f64 dfloating;
        Token token;
        char *str;
        Type *type;
        struct {
            Type *type;
            Value **elements;
            u64 n;
        } array;
        struct {
            Type *type;
            char **member_names;
            Value **members;
            u64 n;
        } record;
    } val;
};

struct Type {
    Typekind kind;
    u32 bytes;
    union {
        struct { /* struct and union */
            char *name;
            struct {
                Type  **types;
                char  **names;
                Value **values;
                u64 n;
            } member;
            struct {
                Type **types;
                u64 n;
            } as;
            struct {
                Type **types;
                u64 n;
            } use;
        } record;

        struct { /* proc, macro, func */
            char *name;
            bool varargs;
            bool has_defaults;
            u64 first_default_param;
            struct {
                Type  **types;
                char  **names;
                Value **values;
                u64 n;
            } param;

            struct {
                Type  **types;
                u64 n;
            } ret;
        } proc;

        struct { /* static array, dynamic or slice */
            u64 n; /* used for capacity of static array */
            Type *of;
        } array;

        struct {
            Type *to;
        } pointer;
    };
};

/* function headers */
Job     job_spawn(Jobid *id_alloc, Pipe_stage stage);
void    job_die(Job *jp);
AST*    job_alloc_ast(Job *jp, ASTkind kind);
Value*  job_alloc_value(Job *jp, Valuekind kind);
Type*   job_alloc_type(Job *jp, Typekind kind);
Sym*    job_alloc_sym(Job *jp);
char*   job_alloc_text(Job *jp, char *s, char *e);
void*   job_alloc_scratch(Job *jp, size_t bytes);

void    job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void    job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void    job_runner(char *src, char *src_path);
Sym*    job_scope_lookup(Job *jp, char *name);
void    job_scope_enter(Job *jp, Sym sym);
void    job_report_all_messages(Job *jp);
void    job_report_mutual_dependency(Job *jp1, Job *jp2);
void    job_assert(Job *jp, bool condition, Loc_info loc, char *fmt, ...);
char*   job_type_to_str(Job *jp, Type *t);
void    linearize_expr(Job *jp, AST **astpp);

Value*  atom_to_value(Job *jp, AST_atom *atom);
Type*   atom_to_type(Job *jp, AST_atom *atom);

Value*  evaluate_unary(Job *jp, Value *a, AST_expr *op_ast);
Value*  evaluate_binary(Job *jp, Value *a, Value *b, AST_expr *op_ast);

Type*   typecheck_unary(Job *jp, Type *a, AST_expr *op_ast);
Type*   typecheck_binary(Job *jp, Type *a, Type *b, AST_expr *op_ast);
Type*   typecheck_assign(Job *jp, Type *a, Type *b, Token op);
void    typecheck_expr(Job *jp);
void    typecheck_vardecl(Job *jp);
void    typecheck_procdecl(Job *jp);
void    typecheck_ifstatement(Job *jp);
void    typecheck_whilestatement(Job *jp);

int     getprec(Token t);

AST*    parse_procdecl(Job *jp);
AST*    parse_procblock(Job *jp);
AST*    parse_statement(Job *jp);
AST*    parse_controlflow(Job *jp);
AST*    parse_vardecl(Job *jp);
AST*    parse_paramdecl(Job *jp);
AST*    parse_expr(Job *jp);
AST*    parse_expr_increase_prec(Job *jp, AST *left, int min_prec);
AST*    parse_expr_decrease_prec(Job *jp, int min_prec);
AST*    parse_term(Job *jp);

void*   global_alloc_scratch(size_t bytes);
Sym*    global_alloc_sym(void);
Value*  global_alloc_value(Valuekind kind);
Type*   global_alloc_type(Typekind kind);
char*   global_alloc_text(char *s, char *e);

Sym*    global_scope_lookup(char *name);
void    global_scope_enter(Sym sym);

char*   global_strdup(char *s);

/* globals */

Type builtin_type[] = {
    { .kind = TYPE_KIND_VOID,  .bytes = 0 },
    { .kind = TYPE_KIND_BOOL,  .bytes = 1 },
    { .kind = TYPE_KIND_CHAR,  .bytes = 1 },
    { .kind = TYPE_KIND_S8,    .bytes = 1 },
    { .kind = TYPE_KIND_U8,    .bytes = 1 },
    { .kind = TYPE_KIND_S16,   .bytes = 2 },
    { .kind = TYPE_KIND_U16,   .bytes = 2 },
    { .kind = TYPE_KIND_S32,   .bytes = 4 },
    { .kind = TYPE_KIND_U32,   .bytes = 4 },
    { .kind = TYPE_KIND_S64,   .bytes = 8 },
    { .kind = TYPE_KIND_U64,   .bytes = 8 },
    { .kind = TYPE_KIND_INT,   .bytes = 8 },
    { .kind = TYPE_KIND_FLOAT, .bytes = 4 },
    { .kind = TYPE_KIND_F32,   .bytes = 4 },
    { .kind = TYPE_KIND_F64,   .bytes = 8 },
    { .kind = TYPE_KIND_TYPE,  .bytes = 8 },
};

char *builtin_type_to_str[] = {
    "void",
    "bool",
    "char",
    "s8",
    "u8",
    "s16",
    "u16",
    "s32",
    "u32",
    "s64",
    "u64",
    "int",
    "float",
    "f32",
    "f64",
    "Type",
};

Value builtin_value[] = {
    { .kind = VALUE_KIND_NIL, },
};

stbds_string_arena global_string_allocator;
Arena      global_scratch_allocator;
Pool       global_sym_allocator;
Pool       global_type_allocator;
Pool       global_value_allocator;
Dict(Sym*) global_scope;

/* function declarations */
Job job_spawn(Jobid *id_alloc, Pipe_stage pipe_stage) {
    *id_alloc += 1;
    Job job = { .id = *id_alloc, .pipe_stage = pipe_stage, .state = JOB_STATE_READY, };
    return job;
}

void job_init_allocator_scratch(Job *jp) {
    arena_init_full(&jp->allocator.scratch, true, JLIB_ARENA_INITIAL_BLOCK_BYTES);
    jp->allocator.active.scratch = true;
}

void job_init_allocator_value(Job *jp) {
    pool_init(&jp->allocator.value, sizeof(Value));
    jp->allocator.active.value = true;
}

void job_init_allocator_sym(Job *jp) {
    pool_init(&jp->allocator.sym, sizeof(Sym));
    jp->allocator.active.sym = true;
}

void job_init_allocator_type(Job *jp) {
    pool_init(&(jp->allocator.type), sizeof(Type));
    jp->allocator.active.type = true;
}

void job_init_allocator_ast(Job *jp) {
#define X(x) pool_init(&(jp->allocator.ast_##x), sizeof(AST_##x));
    ASTKINDS;
#undef X
    jp->allocator.active.ast = true;
}

void job_die(Job *jp) {
    jp->id = -1;

    for(int i = 0; i < arrlen(jp->scopes); ++i)
        if(jp->scopes[i])
            shfree(jp->scopes[i]);

    if(jp->scopes) arrfree(jp->scopes);
    if(jp->tree_pos_stack) arrfree(jp->tree_pos_stack);
    if(jp->value_stack) arrfree(jp->value_stack);
    if(jp->type_stack) arrfree(jp->type_stack);
    if(jp->expr) arrfree(jp->expr);

    if(jp->allocator.active.scratch) arena_destroy(&jp->allocator.scratch);
    if(jp->allocator.active.value) pool_destroy(&jp->allocator.value);
    if(jp->allocator.active.sym) pool_destroy(&jp->allocator.sym);
    if(jp->allocator.active.type) pool_destroy(&(jp->allocator.type));

    if(jp->allocator.active.ast) {
#define X(x) pool_destroy(&(jp->allocator.ast_##x));
        ASTKINDS;
#undef X
    }
}

Value* job_alloc_value(Job *jp, Valuekind kind) {
    Value *ptr = pool_alloc(&jp->allocator.value);
    ptr->kind = kind;
    return ptr;
}

Type* job_alloc_type(Job *jp, Typekind kind) {
    if(kind >= TYPE_KIND_VOID && kind <= TYPE_KIND_TYPE)
        return builtin_type + kind;
    Type *ptr = pool_alloc(&jp->allocator.type);
    ptr->kind = kind;
    return ptr;
}

AST* job_alloc_ast(Job *jp, ASTkind kind) {
    AST *ptr = NULL;
    switch(kind) {
        default:
            assert("invalid ast kind"&&0);
            break;
#define X(x) case AST_KIND_##x: ptr = pool_alloc(&jp->allocator.ast_##x); break;
            ASTKINDS
#undef X
    }
    ptr->kind = kind;
    ptr->loc = jp->lexer->loc;
    return ptr;
}

void* job_alloc_scratch(Job *jp, size_t bytes) {
    return arena_alloc(&jp->allocator.scratch, bytes);
}

void job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]) {
#define X(x) save[(int)AST_KIND_##x] = pool_to_save(&jp->allocator.ast_##x);
    ASTKINDS
#undef X
}

void job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]) {
#define X(x) pool_from_save(&jp->allocator.ast_##x,  save[(int)AST_KIND_##x]);
    ASTKINDS
#undef X
}

char* job_alloc_text(Job *jp, char *s, char *e) {
    char *ptr = arena_alloc(&jp->allocator.scratch, (e - s) + 1);
    char *p = ptr;

    while(s < e) {
        *p = *s;
        ++p;
        ++s;
    }

    return ptr;
}

Sym* job_alloc_sym(Job *jp) {
    return pool_alloc(&jp->allocator.sym);
}

char* global_alloc_text(char *s, char *e) {
    return stralloclen(&global_string_allocator, s, (size_t)(e - s));
}

Sym* global_alloc_sym(void) {
    return pool_alloc(&global_sym_allocator);
}

char* global_strdup(char *s) {
    return stralloc(&global_string_allocator, s);
}

Sym* job_scope_lookup(Job *jp, char *name) {
    for(int i = arrlen(jp->scopes) - 1; i >= 0; --i) {
        Scope scope = jp->scopes[i];
        Sym *symp = shget(scope, name);
        if(symp)
            return symp;
    }
    return NULL;
}

void job_scope_enter(Job *jp, Sym sym) {
    int last = arrlen(jp->scopes) - 1;
    assert(last >= 0);
    Scope scope = jp->scopes[last];
    Sym *symp = job_alloc_sym(jp);
    *symp = sym;
    shput(scope, sym.name, symp);
    jp->scopes[last] = scope;
}

Sym* global_scope_lookup(char *name) {
    return shget(global_scope, name);
}

void global_scope_enter(Sym sym) {
    Sym *symp = global_alloc_sym();
    *symp = sym;
    symp->name = sym.name;
    shput(global_scope, symp->name, symp);
}

INLINE int count_digits(int n) {
    int count = 0;
    while(n > 0) {
        ++count;
        n /= 10;
    }
    return count;
}

void job_report_all_messages(Job *jp) {
    for(int i = 0; i < arrlen(jp->messages); ++i) {
        Message msg = jp->messages[i];
        fprintf(stderr, "jcc:%i:%i:error: %s\n| %i    %.*s\n%*s^^^\n",
                msg.loc.line, msg.loc.col, msg.text, msg.loc.line, (int)(msg.loc.text.e - msg.loc.text.s), msg.loc.text.s,
                msg.loc.col + 6 + count_digits(msg.loc.line), "");
    }
}

void job_assert(Job *jp, bool condition, Loc_info loc, char *fmt, ...) {
    if(condition) return;

    va_list args;

    va_start(args, fmt);

    size_t n = 64;
    char *buf = arena_alloc(&jp->allocator.scratch, n);

    size_t n_written = stbsp_vsnprintf(buf, n, fmt, args);

    va_end(args);

    while(n_written >= n) {
        arena_step_back(&jp->allocator.scratch, n);
        n <<= 1;
        buf = arena_alloc(&jp->allocator.scratch, n);
        va_start(args, fmt);
        n_written = stbsp_vsnprintf(buf, n, fmt, args);
        va_end(args);
    }

    arena_step_back(&jp->allocator.scratch, n - n_written - 1);

    Message msg = {
        .kind = MESSAGE_KIND_ERROR,
        .text = buf,
        .loc  = loc,
    };

    arrpush(jp->messages, msg);
    jp->state = JOB_STATE_ERROR;
}

void job_report_mutual_dependency(Job *jp1, Job *jp2) {
    Loc_info loc1 = arrlast(jp1->tree_pos_stack)->loc;
    Loc_info loc2 = arrlast(jp2->tree_pos_stack)->loc;
    fprintf(stderr, "jcc:error: mutual dependency between '%s' and '%s'\n| %i    %.*s\n----\n| %i    %.*s\n\n",
            jp1->handling_name, jp2->handling_name,
            loc1.line, (int)(loc1.text.e - loc1.text.s), loc1.text.s,
            loc2.line, (int)(loc2.text.e - loc2.text.s), loc2.text.s);
}

Arr(char) _type_to_str(Type *t, Arr(char) tmp_buf) {
    if(t->kind >= TYPE_KIND_VOID && t->kind <= TYPE_KIND_TYPE) {
        char *tstr = builtin_type_to_str[t->kind];
        char *p = arraddnptr(tmp_buf, strlen(tstr));
        while(*tstr) {
            *p = *tstr;
            ++p;
            ++tstr;
        }
    } else if(t->kind >= TYPE_KIND_ARRAY && t->kind <= TYPE_KIND_ARRAY_VIEW) {
        arrpush(tmp_buf, '[');
        if(t->kind == TYPE_KIND_ARRAY) {
            u64 cur_len = arrlen(tmp_buf);
            char *p = arraddnptr(tmp_buf, 21);
            u64 n = stbsp_sprintf(p, "%lu", t->array.n);
            arrsetlen(tmp_buf, cur_len + n);
        } else if(t->kind == TYPE_KIND_DYNAMIC_ARRAY) {
            arrpush(tmp_buf, '.');
            arrpush(tmp_buf, '.');
        }
        arrpush(tmp_buf, ']');
        tmp_buf = _type_to_str(t->array.of, tmp_buf);
    } else if(t->kind == TYPE_KIND_POINTER) {
        arrpush(tmp_buf, '*');
        tmp_buf = _type_to_str(t->pointer.to, tmp_buf);
    } else if(t->kind == TYPE_KIND_PROC) {
        arrpush(tmp_buf, '(');
        for(u64 i = 0; i < t->proc.param.n; ++i) {
            Type *param_type = t->proc.param.types[i];
            tmp_buf = _type_to_str(param_type, tmp_buf);
            arrpush(tmp_buf, ',');
            arrpush(tmp_buf, ' ');
        }

        tmp_buf[arrlen(tmp_buf) - 2] = ')';
        arrpush(tmp_buf, '-');
        arrpush(tmp_buf, '>');
        arrpush(tmp_buf, ' ');

        for(u64 i = 0; i < t->proc.ret.n; ++i) {
            Type *ret_type = t->proc.ret.types[i];
            tmp_buf = _type_to_str(ret_type, tmp_buf);
            arrpush(tmp_buf, ',');
            arrpush(tmp_buf, ' ');
        }

        arrsetlen(tmp_buf, arrlen(tmp_buf) - 2);
    } else {
        UNIMPLEMENTED;
    }

    return tmp_buf;
}

char *global_type_to_str(Type *t) {
    Arr(char) tmp_buf = NULL;
    arrsetcap(tmp_buf, 256);
    tmp_buf = _type_to_str(t, tmp_buf);
    arrpush(tmp_buf, 0);
    char *s = global_alloc_scratch(arrlen(tmp_buf));
    memcpy(s, tmp_buf, arrlen(tmp_buf));
    arrfree(tmp_buf);
    return s;
}

char* job_type_to_str(Job *jp, Type *t) {
    Arr(char) tmp_buf = NULL;
    arrsetcap(tmp_buf, 256);
    tmp_buf = _type_to_str(t, tmp_buf);
    arrpush(tmp_buf, 0);
    char *s = job_alloc_scratch(jp, arrlen(tmp_buf));
    memcpy(s, tmp_buf, arrlen(tmp_buf));
    arrfree(tmp_buf);
    return s;
}

INLINE void* global_alloc_scratch(size_t bytes) {
    return arena_alloc(&global_scratch_allocator, bytes);
}

int getprec(Token t) {
    switch(t) {
#define X(t, prec) case (Token)t: return prec;
        OPERATOR_PREC_TABLE
#undef X
        default: return -1;
    }
}

AST* parse_procdecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_PROC) {
        *lexer = unlex;
        return NULL;
    }

    t = lex(lexer);
    job_assert(jp, t == TOKEN_IDENT, lexer->loc, "expected identifier after 'proc' keyword");

    Loc_info procdecl_loc = lexer->loc;

    AST_procdecl *node = (AST_procdecl*)job_alloc_ast(jp, AST_KIND_procdecl);
    node->name = global_alloc_text(lexer->text.s, lexer->text.e);
    node->base.loc = procdecl_loc;
    node->is_top_level = jp->is_top_level;


    t = lex(lexer);
    job_assert(jp, t == '(', lexer->loc, "expected '(' to begin parameter list");

    int n_params = 0;
    AST_paramdecl head;
    AST_paramdecl *param_list = &head;

    bool must_be_default = false;

    while(true) {
        param_list->next = (AST_paramdecl*)parse_paramdecl(jp);
        param_list = param_list->next;
        param_list->index = n_params;
        if(!must_be_default && param_list->init != NULL) {
            node->first_default_param = n_params;
            must_be_default = true;
        }

        if(must_be_default && param_list->init == NULL) {
            job_assert(jp, 0, param_list->base.loc, "once first default parameter is declared, all parameters must have default values");
            return (AST*)node;
        }
        t = lex(lexer);

        if(param_list) ++n_params;

        if(t != ',') {
            job_assert(jp, t == ')', lexer->loc, "expected ',' or ')' in parameter list");
            break;
        }
    }

    job_assert(jp, t == ')', lexer->loc, "expected ')' to end parameter list");

    node->has_defaults = must_be_default;
    node->params = head.next;
    node->n_params = n_params;

    unlex = *lexer;
    t = lex(lexer);

    if(t != '{') {
        *lexer = unlex;
        int n_rets = 0;
        AST_retdecl head;
        AST_retdecl *ret_list = &head;

        while(true) {
            ret_list->next = (AST_retdecl*)job_alloc_ast(jp, AST_KIND_retdecl);
            ret_list->next->expr = parse_expr(jp);
            ret_list = ret_list->next;
            ret_list->index = n_rets;
            unlex = *lexer;
            t = lex(lexer);

            if(ret_list->expr) ++n_rets;

            if(t != ',') {
                job_assert(jp, t == '{' || t == ';', lexer->loc, "expected ',' or '{' or ';' in return type list");
                break;
            }
        }

        job_assert(jp, t == '{' || t == ';', lexer->loc, "expected '{' or ';' after return types");

        node->rets = head.next;
        node->n_rets = n_rets;
    }

    job_assert(jp, t == '{' || t == ';', lexer->loc, "expected beginning of procedure body or ';'");
    
    if(t == ';') return (AST*)node;

    *lexer = unlex;

    node->body = parse_procblock(jp);

    return (AST*)node;
}

AST* parse_procblock(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != '{') {
        *lexer = unlex;
        return NULL;
    }

    AST_block *block = (AST_block*)job_alloc_ast(jp, AST_KIND_block);

    AST_statement head = {0};
    AST_statement *statement_list = &head;

    while(true) {
        statement_list->next = parse_controlflow(jp);
        if(!statement_list->next) statement_list->next = parse_procdecl(jp);
        if(!statement_list->next) statement_list->next = parse_procblock(jp);
        if(!statement_list->next) statement_list->next = parse_vardecl(jp);
        if(!statement_list->next) statement_list->next = parse_statement(jp);

        unlex = *lexer;
        t = lex(lexer);
        if(t == '}') {
            break;
        } else if(t == 0) {
            job_assert(jp, 0, lexer->loc, "unexpected end of source");
            break;
        }
        *lexer = unlex;

        statement_list = (AST_statement*)(statement_list->next);
    }

    block->down = head.next;

    return (AST*)block;
}

AST* parse_controlflow(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t < TOKEN_IF || t > TOKEN_WHILE) {
        *lexer = unlex;
        return NULL;
    }

    switch(t) {
        default:
            UNREACHABLE;
        case TOKEN_IF:
            {
                AST_ifstatement *node = (AST_ifstatement*)job_alloc_ast(jp, AST_KIND_ifstatement);

                unlex = *lexer;
                t = lex(lexer);
                if(t == ':') {
                    t = lex(lexer);
                    job_assert(jp, t == TOKEN_IDENT, lexer->loc, "expected label");
                    node->label = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                } else {
                    *lexer = unlex;
                }

                node->condition = parse_expr(jp);

                job_assert(jp, node->condition, node->base.loc, "if statement missing condition");

                unlex = *lexer;
                t = lex(lexer);
                *lexer = unlex;

                AST *(*body_func)(Job *) = parse_procblock;

                bool multiline = (t == '{');

                if(!multiline) body_func = parse_statement;

                node->body = body_func(jp);

                unlex = *lexer;
                t = lex(lexer);

                AST_ifstatement head = {0};
                AST_ifstatement *branch = &head;

                while(t == TOKEN_ELSE) {
                    unlex = *lexer;
                    t = lex(lexer);

                    if(t == TOKEN_IF) {
                        branch->branch = job_alloc_ast(jp, AST_KIND_ifstatement);
                        branch = (AST_ifstatement*)branch->branch;
                        branch->condition = parse_expr(jp);
                        job_assert(jp, branch->condition, node->base.loc, "else-if statement missing condition");
                        branch->body = body_func(jp);
                        if(multiline) job_assert(jp, node->body, node->base.loc, "expected multi line block");
                    } else {
                        *lexer = unlex;
                        branch->branch = body_func(jp);
                        if(multiline) job_assert(jp, node->body, node->base.loc, "expected multi line block");
                    }

                    unlex = *lexer;
                    t = lex(lexer);
                }

                *lexer = unlex;
                node->branch = head.branch;

                return (AST*)node;
            }
            break;      
        case TOKEN_SWITCH:
            UNIMPLEMENTED;
            break;  
        case TOKEN_FOR:
            {
                AST_forstatement *node = (AST_forstatement*)job_alloc_ast(jp, AST_KIND_forstatement);

                unlex = *lexer;
                t = lex(lexer);
                if(t == ':') {
                    t = lex(lexer);
                    job_assert(jp, t == TOKEN_IDENT, lexer->loc, "expected label");
                    node->label = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                } else {
                    *lexer = unlex;
                }

                node->expr = parse_expr(jp);

                job_assert(jp, node->expr, node->base.loc, "for statement missing expression");

                unlex = *lexer;
                t = lex(lexer);
                *lexer = unlex;

                if(t != '{')
                    node->body = parse_statement(jp);
                else
                    node->body = parse_procblock(jp);

                return (AST*)node;
            }
            break;     
        case TOKEN_WHILE:
            {
                AST_whilestatement *node = (AST_whilestatement*)job_alloc_ast(jp, AST_KIND_whilestatement);

                unlex = *lexer;
                t = lex(lexer);
                if(t == ':') {
                    t = lex(lexer);
                    job_assert(jp, t == TOKEN_IDENT, lexer->loc, "expected label");
                    node->label = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                } else {
                    *lexer = unlex;
                }

                node->condition = parse_expr(jp);

                job_assert(jp, node->condition, node->base.loc, "while statement missing condition");

                unlex = *lexer;
                t = lex(lexer);
                *lexer = unlex;

                if(t != '{')
                    node->body = parse_statement(jp);
                else
                    node->body = parse_procblock(jp);

                return (AST*)node;
            }
            break;   
    }

    return NULL;
}

AST* parse_statement(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t == TOKEN_DEFER || (t != TOKEN_RETURN && t != TOKEN_CONTINUE && t != TOKEN_BREAK)) {
        AST_statement *node = (AST_statement*)job_alloc_ast(jp, AST_KIND_statement);

        if(t == TOKEN_DEFER)
            node->deferred = true;
        else
            *lexer = unlex;

        node->left = parse_expr(jp);

        t = lex(lexer);

        if(t == '=' || (t >= TOKEN_PLUSEQUAL && t <= TOKEN_TILDEEQUAL)) {
            node->assign_op = t;
            node->right = parse_expr(jp);
        }

        job_assert(jp, t == ';', lexer->loc, "expected ';' at end of statement");

        return (AST*)node;
    }

    if(t < TOKEN_CONTINUE || t > TOKEN_RETURN) {
        job_assert(jp, 0, lexer->loc, "expected 'continue', 'break' or 'return'");
        return NULL;
    }

    switch(t) {
        default:
            UNREACHABLE;
        case TOKEN_CONTINUE:
            {
                AST_continuestatement *node = (AST_continuestatement*)job_alloc_ast(jp, AST_KIND_continuestatement);
                t = lex(lexer);
                if(t == TOKEN_IDENT) {
                    node->label = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                } else if(t != ';') {
                    job_assert(jp, 0, lexer->loc, "expected label or ';' in continue statement");
                }

                return (AST*)node;
            }
            break;
        case TOKEN_BREAK:
            {
                AST_breakstatement *node = (AST_breakstatement*)job_alloc_ast(jp, AST_KIND_breakstatement);
                t = lex(lexer);
                if(t == TOKEN_IDENT) {
                    node->label = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                } else if(t != ';') {
                    job_assert(jp, 0, lexer->loc, "expected label or ';' in break statement");
                }

                return (AST*)node;
            }
            break;   
        case TOKEN_RETURN:
            {
                AST_returnstatement *node = (AST_returnstatement*)job_alloc_ast(jp, AST_KIND_returnstatement);
                AST_expr_list head = {0};
                AST_expr_list *expr_list = &head;

                unlex = *lexer;
                t = lex(lexer);
                if(t != ';') {
                    *lexer = unlex;
                    while(t != ';') {
                        expr_list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                        expr_list = expr_list->next;
                        expr_list->expr = parse_expr(jp);
                        t = lex(lexer);
                    }

                    job_assert(jp, t == ';', lexer->loc, "expected ';' at end of return statement");
                    node->expr_list = head.next;
                }

                return (AST*)node;
            }
            break;  
    }

}

AST* parse_vardecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer lexer_reset = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        *lexer = lexer_reset;
        return NULL;
    }

    char *name_s = lexer->text.s;
    char *name_e = lexer->text.e;
    Loc_info vardecl_loc = lexer->loc;

    t = lex(lexer);

    if(t != ':') {
        *lexer = lexer_reset;
        return NULL;
    }

    AST_vardecl *node = (AST_vardecl*)job_alloc_ast(jp, AST_KIND_vardecl);
    node->name = global_alloc_text(name_s, name_e);
    node->base.loc = vardecl_loc;
    node->is_top_level = jp->is_top_level;

    Lexer unlex = *lexer;

    t = lex(lexer);

    if(t == '=' || t == ':') {
        node->constant = (t == ':');
        unlex = *lexer;
        t = lex(lexer);
        if(t == TOKEN_LONGDASH) {
            job_assert(jp, node->constant == false, node->base.loc, "cannot make constant uninitialized");
            node->uninitialized = true;
        } else {
            *lexer = unlex;
            node->init = parse_expr(jp);
            t = lex(lexer);
        }
    } else {
        *lexer = unlex;
        Loc_info type_loc = lexer->loc;
        node->type = parse_expr(jp);

        job_assert(jp, node->type, type_loc, "expected type declarator");

        t = lex(lexer);
        if(t == '=' || t == ':') {
            node->constant = (t == ':');
            node->init = parse_expr(jp);
            t = lex(lexer);
        } else if(t != ';' && t != ')' && t != ',' && t != '{') {
            job_assert(jp, 0, lexer->loc, "expected '=', ':' or separator in declaration");
        }
    }

    job_assert(jp, t==';' || t==')' || t==',' || t=='{', lexer->loc, "expected punctuation at end of variable declaration");

    return (AST*)node;
}

AST* parse_paramdecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer lexer_reset = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        *lexer = lexer_reset;
        return NULL;
    }
    char *name_s = lexer->text.s;
    char *name_e = lexer->text.e;
    Loc_info paramdecl_loc = lexer->loc;

    t = lex(lexer);

    if(t != ':') {
        *lexer = lexer_reset;
        return NULL;
    }

    AST_paramdecl *node = (AST_paramdecl*)job_alloc_ast(jp, AST_KIND_paramdecl);
    node->name = global_alloc_text(name_s, name_e);
    node->base.loc = paramdecl_loc;
    node->is_top_level = jp->is_top_level;

    Lexer unlex = *lexer;

    Loc_info type_loc = lexer->loc;
    node->type = parse_expr(jp);

    job_assert(jp, node->type, type_loc, "expected type declarator");

    unlex = *lexer;
    t = lex(lexer);

    if(t == '=') {
        node->init = parse_expr(jp);
        unlex = *lexer;
        t = lex(lexer);
    }

    job_assert(jp,  t == ')' || t == ',', lexer->loc, "expected punctuation at end of parameter declaration");

    *lexer = unlex;

    return (AST*)node;
}

AST* parse_expr(Job *jp) {
    return parse_expr_decrease_prec(jp, 0);
}

AST* parse_expr_increase_prec(Job *jp, AST *left, int min_prec) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    AST_expr *op;

    Token t = lex(lexer);
    int prec = getprec(t);

    if(prec <= min_prec) {
        *lexer = unlex;
        return left;
    }

    op = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
    op->token = t;
    op->right = parse_expr_decrease_prec(jp, prec);

    return (AST*)op;
}

AST* parse_expr_decrease_prec(Job *jp, int min_prec) {
    AST *left, *node;
    AST_expr *op;

    left = parse_term(jp);

    if(left == NULL)
        return NULL;

    while(true) {
        node = parse_expr_increase_prec(jp, left, min_prec);
        if(node == left) break;
        assert(node->kind == AST_KIND_expr);
        op = (AST_expr*)node;
        op->left = left;
        left = node;
    }

    return left;
}

AST* parse_term(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    AST *node = NULL;
    AST_call *call_op = NULL;
    AST_expr *index_op = NULL;
    AST_expr *index_op_next = NULL;
    AST_expr *expr = NULL;
    AST_expr *unary = NULL;
    AST_expr *array_type = NULL;
    AST_atom *atom = NULL;

    switch(t) {
        default:
            *lexer = unlex;
            return NULL;
        case TOKEN_CAST:
            t = lex(lexer);
            job_assert(jp, t == '(', lexer->loc, "expected '(' after 'cast' keyword");
            if(jp->state == JOB_STATE_ERROR) return NULL;
            unary = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            unary->token = TOKEN_CAST;
            unary->left = parse_expr(jp);
            t = lex(lexer);
            job_assert(jp, t == ')', lexer->loc, "unbalanced parenthesis");
            if(jp->state == JOB_STATE_ERROR) return NULL;
            node = parse_term(jp);
            break;
        case '.':
            t = lex(lexer);
            job_assert(jp, t=='[', lexer->loc, "expected '[' after '.' to mark beginning of array literal");
            if(jp->state == JOB_STATE_ERROR) return NULL;
            // NOTE copypasta
            {
                AST_array_literal *array_lit = (AST_array_literal*)job_alloc_ast(jp, AST_KIND_array_literal);
                array_lit->type = node;
                AST_expr_list head = {0};
                AST_expr_list *list = &head;
                int n = 0;
                while(true) {
                    list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                    list->next->expr = parse_expr(jp);
                    list = list->next;
                    ++n;
                    if(lex(lexer) == ']')
                        break;
                }
                array_lit->n = n;
                array_lit->elements = head.next;
                array_lit->base.loc.col -= 2; /* NOTE hack to correct array literal column location */
                node = (AST*)array_lit;

                if(unary) {
                    unary->right = node;
                    node = (AST*)unary;
                }

                return node;
            }
            break;
        case '[':
            array_type = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            array_type->token = t;
            array_type->right = parse_expr(jp);
            t = lex(lexer);
            job_assert(jp, t==']', lexer->loc, "unbalanced square bracket");
            array_type->left = parse_term(jp);
            node = (AST*)array_type;
            return node;
        case '+': case '-': case '!': case '~': case '&': case '*':
            unary = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            unary->token = t;
            node = parse_term(jp);
            break;
        case '(':
            expr = (AST_expr*)parse_expr(jp);
            t = lex(lexer);
            job_assert(jp, t==')', lexer->loc, "unbalanced parenthesis");
            node = (AST*)expr;
            break;
        case TOKEN_TWODOT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            node = (AST*)atom;
            break;
        case TOKEN_VOID:
        case TOKEN_INT:
        case TOKEN_FLOAT:
        case TOKEN_CHAR: 
        case TOKEN_BOOL: 
        case TOKEN_U8:   case TOKEN_U16:  case TOKEN_U32:  case TOKEN_U64:  
        case TOKEN_S8:   case TOKEN_S16:  case TOKEN_S32:  case TOKEN_S64:  
        case TOKEN_F32:  case TOKEN_F64:  
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            node = (AST*)atom;
            break;
        case TOKEN_IDENT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->text = global_alloc_text(lexer->text.s, lexer->text.e);
            node = (AST*)atom;
            break;
        case TOKEN_INTLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->integer = lexer->integer;
            node = (AST*)atom;
            break;
        case TOKEN_HEXLIT: case TOKEN_BINLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->uinteger = lexer->uinteger;
            node = (AST*)atom;
            break;
        case TOKEN_FLOATLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->floating = lexer->floating;
            node = (AST*)atom;
            break;
        case TOKEN_STRINGLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->text = global_alloc_text(lexer->text.s, lexer->text.e);
            node = (AST*)atom;
            break;
    }

    /* postfix operators */
    unlex = *lexer;
    t = lex(lexer);

    if(t == '(') {
        call_op = (AST_call*)job_alloc_ast(jp, AST_KIND_call);
        call_op->callee = node;
        call_op->base.loc = node->loc;

        AST_param head = { .base = { .kind = AST_KIND_param, .loc = lexer->loc }, };
        AST_param *param = &head;

        int n_params = 0;
        int first_named_param = 0;
        bool must_be_named = false;
        while(param) {
            /* NOTE don't unlex before entering a loop if you don't unlex inside */
            unlex = *lexer;
            t = lex(lexer);

            bool named_param = false;

            if(t == TOKEN_IDENT) {
                char *s = lexer->text.s;
                char *e = lexer->text.e;
                t = lex(lexer);
                if(t == '=') {
                    named_param = true;
                    param->name = global_alloc_text(s, e);
                } else {
                    *lexer = unlex;
                }
            } else {
                *lexer = unlex;
            }

            if(named_param && !must_be_named) {
                must_be_named = true;
                first_named_param = n_params;
            }

            if(must_be_named) {
                job_assert(jp, named_param, call_op->base.loc,
                        "once a named parameter is passed, all subsequent parameters must be named");
            }

            expr = (AST_expr*)parse_expr(jp);

            if(expr == NULL) {
                job_assert(jp, !named_param, param->base.loc, "named parameter has no initializer");
                t = lex(lexer);
                assert("expected closing paren"&&(t == ')'));
                break;
            } else {
                param->value = (AST*)expr;
            }

            t = lex(lexer);

            if(t == ',') {
                param->next = (AST_param*)job_alloc_ast(jp, AST_KIND_param);
            } else if(t != ')') {
                job_assert(jp, 0, lexer->loc, "expected comma or end of parameter list");
            }

            param = param->next;

            ++n_params;
        }

        if(head.name == NULL && head.value == NULL) {
            call_op->params = NULL;
        } else {
            call_op->params = (AST_param*)job_alloc_ast(jp, AST_KIND_param);
            *(call_op->params) = head;
        }

        call_op->n_params = n_params;
        call_op->first_named_param = first_named_param;
        call_op->has_named_params = must_be_named;

        if(must_be_named) {
            job_assert(jp,
                    call_op->callee->kind == AST_KIND_atom && ((AST_atom*)call_op->callee)->token == TOKEN_IDENT,
                    call_op->base.loc,
                    "named parameters can only be passed to a named procedure");
        }

        node = (AST*)call_op;

        unlex = *lexer;
        t = lex(lexer);
    }

    if(t == '.' || t == '[') {
        index_op = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
        index_op->token = t;
        index_op->left = node;
        while(true) {
            if(t == '.') {
                t = lex(lexer);
                if(t == '[') {
                    AST_array_literal *array_lit = (AST_array_literal*)job_alloc_ast(jp, AST_KIND_array_literal);
                    array_lit->type = node;
                    AST_expr_list head = {0};
                    AST_expr_list *list = &head;
                    int n = 0;
                    while(true) {
                        list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                        list->next->expr = parse_expr(jp);
                        list = list->next;
                        ++n;
                        if(lex(lexer) == ']')
                            break;
                    }
                    array_lit->n = n;
                    array_lit->elements = head.next;
                    array_lit->base.loc.col = array_lit->type->loc.col; /* NOTE hack to correct array literal column location */
                    node = (AST*)array_lit;

                    if(unary) {
                        unary->right = node;
                        node = (AST*)unary;
                    }

                    return node;
                }
                job_assert(jp, t == TOKEN_IDENT, lexer->loc, "expected identifier on right hand side of '.' operator");
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = t;
                atom->text = global_alloc_text(lexer->text.s, lexer->text.e);
                index_op->right = (AST*)atom;
            } else if(t == '[') {
                expr = (AST_expr*)parse_expr(jp);
                t = lex(lexer);
                job_assert(jp, t==']', lexer->loc, "unbalanced square bracket");
                index_op->right = (AST*)expr;
            }

            unlex = *lexer;
            t = lex(lexer);

            if(t == '.' || t == '[') {
                index_op_next = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                index_op_next->left = (AST*)index_op;
                index_op = index_op_next;
            } else {
                break;
            }
        }

        node = (AST*)index_op;
    }

    *lexer = unlex;

    if(unary) {
        unary->right = node;
        node = (AST*)unary;
    }

    return node;
}

void print_ast_expr(AST *expr, int indent) {
    AST_expr *e;
    AST_atom *a;
    AST_call *c;
    if(expr == NULL)
        return;
    switch(expr->kind) {
        case AST_KIND_expr:
            e = (AST_expr*)expr;
            for(int i = 0; i < indent; ++i) printf("  ");
            if(e->token < TOKEN_INVALID) {
                printf("%c\n", e->token);
            } else if(e->token > TOKEN_KEYWORD) {
                printf("%s\n", TOKEN_TO_KEYWORD(e->token));
            }
            print_ast_expr(e->left, indent + 1);
            print_ast_expr(e->right, indent + 1);
            break;
        case AST_KIND_atom:
            a = (AST_atom*)expr;
            for(int i = 0; i < indent; ++i) printf("  ");
            if(a->token < TOKEN_INVALID) {
                printf("%c\n", a->token);
            } else if(a->token > TOKEN_KEYWORD) {
                printf("%s\n", TOKEN_TO_KEYWORD(a->token));
            } else {
                switch(a->token) {
                    case TOKEN_IDENT: case TOKEN_STRINGLIT:
                        printf("%s\n", a->text);
                        break;
                    case TOKEN_HEXLIT:
                    case TOKEN_BINLIT:
                    case TOKEN_UINTLIT:
                        printf("%lu\n", a->uinteger);
                        break;
                    case TOKEN_INTLIT:
                        printf("%lu\n", a->integer);
                        break;
                    case TOKEN_FLOATLIT:
                        printf("%f\n", a->floating);
                        break;
                    default: UNREACHABLE;
                }
            }
            break;
        case AST_KIND_call:
            c = (AST_call*)expr;
            for(int i = 0; i < indent; ++i) printf("  ");
            printf("call\n");
            print_ast_expr(c->callee, indent + 1);
            for(AST_param *param = c->params; param; param = param->next) {
                for(int i = 0; i < indent; ++i) printf("  ");
                printf("param");
                if(param->name)
                    printf(" %s\n", param->name);
                else
                    printf("\n");
                print_ast_expr(param->value, indent + 1);
            }
            break;
    }
}

void job_runner(char *src, char *src_path) {
    Arr(Job) job_queue = NULL;
    Arr(Job) job_queue_next = NULL;
    Lexer lexer = {0};
    lexer_init(&lexer, src, src_path);

    bool all_waiting = false;

    Jobid id_alloc = -1;

    arrpush(job_queue, job_spawn(&id_alloc, PIPE_STAGE_PARSE));
    job_queue[0].lexer = &lexer;
    job_queue[0].is_top_level = true;

    job_init_allocator_scratch(&job_queue[0]);

    while(arrlen(job_queue) > 0) {
        for(int i = 0; i < arrlen(job_queue); /*++i*/) {
            Job *jp = job_queue + i;

            switch(jp->pipe_stage) {
                case PIPE_STAGE_PARSE:
                    while(true) {
                        job_init_allocator_ast(jp);

                        AST *ast = NULL;

                        if(ast == NULL) ast = parse_procdecl(jp);
                        if(ast == NULL) ast = parse_vardecl(jp);

                        if(ast == NULL) {
                            Token t = lex(jp->lexer);
                            assert(t == 0);

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                arrsetlen(job_queue_next, 0);
                            }

                            job_die(jp);
                            break;
                        }

                        //TODO clean job forking
                        Job new_job = job_spawn(&id_alloc, PIPE_STAGE_TYPECHECK);

                        new_job.allocator = jp->allocator; /* copy ast allocator */
                        job_init_allocator_scratch(&new_job);
                        job_init_allocator_value(&new_job);
                        job_init_allocator_sym(&new_job);
                        job_init_allocator_type(&new_job);

                        new_job.root = ast;
                        arrpush(new_job.tree_pos_stack, ast);

                        arrpush(job_queue_next, new_job);
                        ++i;
                    }
                    break;
                case PIPE_STAGE_TYPECHECK:
                    {
                        AST *ast = arrlast(jp->tree_pos_stack);

                        bool popped_stack = false;

                        while(ast == NULL) {
                            if(arrlen(jp->tree_pos_stack) <= 0) {
                                arrfree(jp->value_stack);
                                arrfree(jp->type_stack);
                                arrfree(jp->expr);
                                arrfree(jp->scopes);
                                jp->pipe_stage = PIPE_STAGE_SIZE;
                                ++i;
                                popped_stack = true;
                                break;
                            } else {
                                arrsetlen(jp->tree_pos_stack, arrlen(jp->tree_pos_stack) - 1);
                                ast = arrlast(jp->tree_pos_stack);
                            }
                        }

                        if(popped_stack)
                            continue;

                        assert(ast->kind >= AST_KIND_ifstatement && ast->kind <= AST_KIND_procdecl);

                        if(ast->kind == AST_KIND_vardecl) {
                            if(jp->step == TYPECHECK_STEP_NONE)
                                jp->step = TYPECHECK_STEP_VARDECL_BEGIN + 1;
                            typecheck_vardecl(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++i;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }
                        } else if(ast->kind == AST_KIND_procdecl) {
                            AST_procdecl *ast_procdecl = (AST_procdecl*)ast;

                            if(ast_procdecl->type_checked_body) {
                                jp->cur_proc_type = NULL;
                                arrlast(jp->tree_pos_stack) = ast_procdecl->next;
                                printf("\ntype checked the body of '%s'!!!!!!!!!\n\n", ast_procdecl->name);
                                break;
                            }

                            if(jp->step == TYPECHECK_STEP_NONE) {
                                arrpush(jp->scopes, NULL);
                                jp->step = TYPECHECK_STEP_PROCDECL_BEGIN + 1;
                            }
                            typecheck_procdecl(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++i;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            if(ast_procdecl->body == NULL) {
                                arrlast(jp->tree_pos_stack) = ast_procdecl->next;
                            } else {
                                ast_procdecl->type_checked_body = true;
                                arrpush(jp->tree_pos_stack, ast_procdecl->body);
                            }

                            jp->step = TYPECHECK_STEP_NONE;
                        } else if(ast->kind == AST_KIND_block) {
                            AST_block *ast_block = (AST_block*)ast;

                            if(ast_block->visited) {
                                Scope s = arrpop(jp->scopes);
                                shfree(s);
                                arrlast(jp->tree_pos_stack) = ast_block->next;
                                continue;
                            }

                            ast_block->visited = true;
                            arrpush(jp->tree_pos_stack, ast_block->down);
                            arrpush(jp->scopes, NULL);
                        } else if(ast->kind == AST_KIND_ifstatement) {
                            AST_ifstatement *ast_if = (AST_ifstatement*)ast;

                            if(arrlen(jp->expr) == 0)
                                linearize_expr(jp, &ast_if->condition);
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++i;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            arrsetlen(jp->type_stack, 0);
                            arrsetlen(jp->value_stack, 0);
                            arrsetlen(jp->expr, 0);
                            jp->expr_pos = 0;

                            arrlast(jp->tree_pos_stack) = ast_if->next;

                            if(ast_if->branch)
                                arrpush(jp->tree_pos_stack, ast_if->branch);

                            arrpush(jp->tree_pos_stack, ast_if->body);
                        } else if(ast->kind == AST_KIND_whilestatement) {
                            //TODO test
                            AST_whilestatement *ast_while = (AST_whilestatement*)ast;

                            if(arrlen(jp->expr) == 0)
                                linearize_expr(jp, &ast_while->condition);
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++i;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            arrsetlen(jp->type_stack, 0);
                            arrsetlen(jp->value_stack, 0);
                            arrsetlen(jp->expr, 0);
                            jp->expr_pos = 0;

                            arrlast(jp->tree_pos_stack) = ast_while->next;
                            arrpush(jp->tree_pos_stack, ast_while->body);

                        } else if(ast->kind == AST_KIND_forstatement) {
                            //TODO test
                            AST_forstatement *ast_for = (AST_forstatement*)ast;

                            if(arrlen(jp->expr) == 0)
                                linearize_expr(jp, &ast_for->expr);
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++i;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            arrsetlen(jp->type_stack, 0);
                            arrsetlen(jp->value_stack, 0);
                            arrsetlen(jp->expr, 0);
                            jp->expr_pos = 0;

                            arrlast(jp->tree_pos_stack) = ast_for->next;
                            arrpush(jp->tree_pos_stack, ast_for->body);

                        } else if(ast->kind == AST_KIND_switchstatement) {
                            UNIMPLEMENTED;
                        } else if(ast->kind == AST_KIND_continuestatement) {
                            arrlast(jp->tree_pos_stack) = ((AST_continuestatement*)ast)->next;
                        } else if(ast->kind == AST_KIND_breakstatement) {
                            arrlast(jp->tree_pos_stack) = ((AST_breakstatement*)ast)->next;
                        } else if(ast->kind == AST_KIND_statement) {
                            //TODO test
                            AST_statement *ast_statement = (AST_statement*)ast;
                            if(jp->step == TYPECHECK_STEP_NONE)
                                jp->step = TYPECHECK_STEP_STATEMENT_LEFT;

                            assert(jp->step >= TYPECHECK_STEP_STATEMENT_BEGIN && jp->step <= TYPECHECK_STEP_STATEMENT_END);

                            bool type_error_in_statement = false;

                            if(jp->step == TYPECHECK_STEP_STATEMENT_LEFT) {
                                if(arrlen(jp->expr) == 0)
                                    linearize_expr(jp, &ast_statement->left);
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++i;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                if(jp->state == JOB_STATE_ERROR) {
                                    type_error_in_statement = true;
                                    jp->state = JOB_STATE_READY;
                                }

                                jp->step = TYPECHECK_STEP_STATEMENT_RIGHT;
                            }

                            if(ast_statement->right == NULL) {
                                jp->step = TYPECHECK_STEP_NONE;
                                arrlast(jp->tree_pos_stack) = ast_statement->next;
                                continue;
                            }

                            if(jp->step == TYPECHECK_STEP_STATEMENT_RIGHT) {
                                if(arrlen(jp->expr) == 0)
                                    linearize_expr(jp, &ast_statement->right);
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++i;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                type_error_in_statement = (jp->state == JOB_STATE_ERROR);

                                jp->step = TYPECHECK_STEP_STATEMENT_END;
                            }

                            if(type_error_in_statement) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            Type *type_left = ((AST_expr*)(ast_statement->left))->type_annotation;
                            Type *type_right = ((AST_expr*)(ast_statement->right))->type_annotation;
                            Type *t = typecheck_assign(jp, type_left, type_right, ast_statement->assign_op);

                            job_assert(jp, t->kind != TYPE_KIND_VOID, ast_statement->base.loc,
                                    "invalid assignment of '%s' to '%s'",
                                    job_type_to_str(jp, type_right),
                                    job_type_to_str(jp, type_left));

                            jp->step = TYPECHECK_STEP_NONE;
                            arrlast(jp->tree_pos_stack) = ast_statement->next;

                        } else if(ast->kind == AST_KIND_returnstatement) {
                            AST_returnstatement *ast_return = (AST_returnstatement*)ast;
                            Type *cur_proc_type = jp->cur_proc_type;

                            //NOTE in future when macros are added, we could have a value kind for the macro
                            //     expansion which would annotate the node, that way we wouldn't need to use
                            //     Arr(AST**) for jp->expr
                            if(arrlen(jp->expr_list) == 0) {
                                for(AST_expr_list *list = ast_return->expr_list; list; list = list->next) {
                                    arrpush(jp->expr_list, &list->expr);
                                }
                                jp->expr_list_pos = 0;
                            }

                            job_assert(jp, cur_proc_type->proc.ret.n == arrlen(jp->expr_list), ast_return->base.loc,
                                    "mismatch in number of return values when returning from '%s', expected '%lu' got '%lu'",
                                    cur_proc_type->proc.name, cur_proc_type->proc.ret.n, arrlen(jp->expr_list));

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            for(u64 expr_list_pos = jp->expr_list_pos; expr_list_pos < arrlen(jp->expr_list); ++expr_list_pos) {
                                if(arrlen(jp->expr) == 0)
                                    linearize_expr(jp, jp->expr_list[expr_list_pos]);
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    jp->expr_list_pos = expr_list_pos;
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++i;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                AST_expr *ret_expr = *(AST_expr**)(jp->expr_list[expr_list_pos]);

                                Type *ret_expr_type = ret_expr->type_annotation;
                                Type *expect_type = cur_proc_type->proc.ret.types[expr_list_pos];

                                Type *t = typecheck_assign(jp, expect_type, ret_expr_type, '=');
                                job_assert(jp, t->kind != TYPE_KIND_VOID, ret_expr->base.loc,
                                        "in return from '%s' expected type '%s', got '%s'",
                                        cur_proc_type->proc.name,
                                        job_type_to_str(jp, expect_type),
                                        job_type_to_str(jp, ret_expr_type));
                            }

                            arrsetlen(jp->expr_list, 0);
                            jp->expr_list_pos = 0;

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++i;
                                continue;
                            }

                            //TODO warn about unreachable code
                            if(ast_return->next) {
                                job_assert(jp, 0, ast_return->next->loc,
                                        "unreachable code after return from '%s'",
                                        cur_proc_type->proc.name);
                                ++i;
                                continue;
                            }

                            arrlast(jp->tree_pos_stack) = ast_return->next;
                        } else {
                            UNIMPLEMENTED;
                        }
                    }
                    break;
                case PIPE_STAGE_SIZE:
                    {
                        UNIMPLEMENTED;
                    }
                    break;
                case PIPE_STAGE_IR:
                    {
                        UNIMPLEMENTED;
                    }
                    break;
            }

        }

        if(arrlen(job_queue_next) == arrlen(job_queue)) {
            int waiting_count = 0;
            for(int i = 0; i < arrlen(job_queue_next); ++i) {
                if(job_queue[i].state == JOB_STATE_WAIT)
                    ++waiting_count;
            }

            if(arrlen(job_queue) == waiting_count) {
                all_waiting = true;
                break;
            }
        }

        Arr(Job) tmp = job_queue;
        job_queue = job_queue_next;
        job_queue_next = tmp;
        arrsetlen(job_queue_next, 0);
    }
    
    if(all_waiting) {
        Dict(char*) name_graph = NULL;
        Dict(Job*) job_graph = NULL;

        for(u64 i = 0; i < arrlen(job_queue); ++i) {
            Job *jp = job_queue + i;
            shput(name_graph, jp->handling_name, jp->waiting_on_name);
            shput(job_graph, jp->handling_name, jp);
        }

        for(u64 i = 0; i < shlen(name_graph); ++i) {
            char *slow = name_graph[i].key;
            char *fast = name_graph[i].key;
            char *save1 = NULL;
            char *save2 = NULL;

            bool has_cycle = false;

            while(slow && fast && !has_cycle) {
                slow = shget(name_graph, slow);
                if(slow == NULL) break;

                save1 = fast;
                fast = shget(name_graph, fast);
                if(fast == NULL) break;

                save2 = fast;
                fast = shget(name_graph, fast);
                if(fast == NULL) break;

                if(!strcmp(slow, fast))
                    has_cycle = true;
            }

            if(has_cycle) {
                Job *jp1 = shget(job_graph, save2);
                Job *jp2 = shget(job_graph, fast);
                job_report_mutual_dependency(jp1, jp2);
            } else {
                //TODO better error printing
                //     probably need a custom print function with more formats
                Job *jp = shget(job_graph, save1);
                job_assert(jp, 0, arrlast(jp->tree_pos_stack)->loc, "undeclared identifier '%s'", save2);
                job_report_all_messages(jp);
            }
        }
    }

    arrfree(job_queue);
    arrfree(job_queue_next);
}

Value *atom_to_value(Job *jp, AST_atom *atom) {
    Value *vp = NULL;
    switch(atom->token) {
        default:
            UNIMPLEMENTED;
        case TOKEN_STRINGLIT:
            vp = job_alloc_value(jp, VALUE_KIND_STRING);
            vp->val.str = atom->text;
            break;
        case TOKEN_TWODOT:
            vp = job_alloc_value(jp, VALUE_KIND_TOKEN);
            vp->val.token = atom->token;
            break;
        case TOKEN_TRUE:
            vp = job_alloc_value(jp, VALUE_KIND_BOOL);
            vp->val.boolean = true;;
            break;
        case TOKEN_FALSE:
            vp = job_alloc_value(jp, VALUE_KIND_BOOL);
            vp->val.boolean = false;;
            break;
        case TOKEN_S8: case TOKEN_S16: case TOKEN_S32: case TOKEN_S64:
        case TOKEN_U8: case TOKEN_U16: case TOKEN_U32: case TOKEN_U64:
        case TOKEN_VOID:
        case TOKEN_BOOL:
        case TOKEN_CHAR:
        case TOKEN_FLOAT:
        case TOKEN_F32: case TOKEN_F64:
        case TOKEN_INT:
            vp = job_alloc_value(jp, VALUE_KIND_TYPE);
            vp->val.type = job_alloc_type(jp, TOKEN_TO_TYPEKIND(atom->token));
            break;
        case TOKEN_INTLIT:
            vp = job_alloc_value(jp, VALUE_KIND_INT);
            vp->val.integer = atom->integer;
            break;
        case TOKEN_UINTLIT: case TOKEN_HEXLIT: case TOKEN_BINLIT:
            vp = job_alloc_value(jp, VALUE_KIND_UINT);
            vp->val.uinteger = atom->uinteger;
            break;
        case TOKEN_FLOATLIT:
            vp = job_alloc_value(jp, VALUE_KIND_FLOAT);
            vp->val.floating = atom->floating;
            break;
    }
    return vp;
}

Type *atom_to_type(Job *jp, AST_atom *atom) {
    Type *tp = NULL;
    switch(atom->token) {
        default:
            UNIMPLEMENTED;
        case TOKEN_STRINGLIT:
            tp = job_alloc_type(jp, TYPE_KIND_ARRAY);
            tp->array.n = strlen(atom->text);
            tp->array.of = builtin_type+TYPE_KIND_CHAR;
            break;
        case TOKEN_TWODOT: /* we do this to make checking array constructors easier */
            tp = job_alloc_type(jp, TYPE_KIND_U64);
            break;
        case TOKEN_TRUE:
        case TOKEN_FALSE:
            tp = job_alloc_type(jp, TYPE_KIND_BOOL);
            break;
        case TOKEN_VOID:
        case TOKEN_BOOL:
        case TOKEN_INT:
        case TOKEN_S8: case TOKEN_S16: case TOKEN_S32: case TOKEN_S64:
        case TOKEN_U8: case TOKEN_U16: case TOKEN_U32: case TOKEN_U64:
        case TOKEN_FLOAT:
        case TOKEN_CHAR:
            tp = job_alloc_type(jp, TYPE_KIND_TYPE);
            break;
        case TOKEN_UINTLIT: case TOKEN_HEXLIT: case TOKEN_BINLIT:
            tp = job_alloc_type(jp, TYPE_KIND_U64);
            break;
        case TOKEN_INTLIT:
            tp = job_alloc_type(jp, TYPE_KIND_INT);
            break;
        case TOKEN_FLOATLIT:
            tp = job_alloc_type(jp, TYPE_KIND_FLOAT);
            break;
    }
    return tp;
}

Value* evaluate_unary(Job *jp, Value *a, AST_expr *op_ast) {
    if(a->kind == VALUE_KIND_NIL)
        return a;

    Value *result = NULL;

    Token op = op_ast->token;

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            if(a->kind == VALUE_KIND_TYPE) {
                result = job_alloc_value(jp, VALUE_KIND_TYPE);
                result->val.type = job_alloc_type(jp, TYPE_KIND_ARRAY_VIEW);
                result->val.type->array.of = a->val.type;
            } else {
                UNREACHABLE;
            }
            break;
        case '+':
            result = a;
            break;
        case '-':
            if(a->kind >= VALUE_KIND_FLOAT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = -a->val.floating;
            } else {
                result = job_alloc_value(jp, VALUE_KIND_INT);
                result->val.integer = -a->val.integer;
            }
            break;
        case '!':
            result = job_alloc_value(jp, VALUE_KIND_INT);
            result->val.integer = !a->val.integer;
            break;
        case '~':
            result = job_alloc_value(jp, VALUE_KIND_INT);
            result->val.integer = ~a->val.integer;
            break;
        case '*':
            if(a->kind == VALUE_KIND_TYPE) {
                result = job_alloc_value(jp, VALUE_KIND_TYPE);
                result->val.type = job_alloc_type(jp, TYPE_KIND_POINTER);
                result->val.type->pointer.to = a->val.type;
            } else {
                result = builtin_value+VALUE_KIND_NIL;
            }
            break;
    }

    return result;
}

Value* evaluate_binary(Job *jp, Value *a, Value *b, AST_expr *op_ast) {
    Value *result = NULL;

    Token op = op_ast->token;

    switch(op) {
        default:
            UNREACHABLE;
        case TOKEN_CAST:
            result = builtin_value+VALUE_KIND_NIL;
            break;
        case '[':
            if(a->kind == VALUE_KIND_TYPE) {
                result = job_alloc_value(jp, VALUE_KIND_TYPE);
                if(b->kind == VALUE_KIND_TOKEN) {
                    assert(b->val.token == TOKEN_TWODOT);
                    result->val.type = job_alloc_type(jp, TYPE_KIND_DYNAMIC_ARRAY);
                    result->val.type->array.of = a->val.type;
                } else {
                    result->val.type = job_alloc_type(jp, TYPE_KIND_ARRAY);
                    result->val.type->array.of = a->val.type;
                    result->val.type->array.n = b->val.integer;
                }
            } else {
                result = builtin_value+VALUE_KIND_NIL;
            }
            break;
        case '-':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating - (float)b->val.integer;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating - (float)b->val.uinteger;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.integer - b->val.floating;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.uinteger - b->val.floating;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger - b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '*':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating * (float)b->val.integer;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating * (float)b->val.uinteger;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.integer * b->val.floating;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.uinteger * b->val.floating;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger * b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '/':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating / (float)b->val.integer;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating / (float)b->val.uinteger;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.integer / b->val.floating;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.uinteger / b->val.floating;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger / b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '+':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating + (float)b->val.integer;
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating + (float)b->val.uinteger;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.integer + b->val.floating;
            } else if(b->kind == VALUE_KIND_FLOAT && a->kind == VALUE_KIND_UINT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = (float)a->val.uinteger + b->val.floating;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger + b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '%': case '&': case '|': case '^': case TOKEN_LSHIFT: case TOKEN_RSHIFT: 
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger + b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case TOKEN_AND:
            UNIMPLEMENTED;
            break;
        case TOKEN_OR:
            UNIMPLEMENTED;
            break;
    }

    return result;
}

Type* typecheck_unary(Job *jp, Type *a, AST_expr *op_ast) {
    assert(a->kind != TYPE_KIND_VOID);

    Token op = op_ast->token;

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            if(a->kind != TYPE_KIND_TYPE) {
                job_assert(jp, 0, op_ast->base.loc, "invalid type %s to array declarator", job_type_to_str(jp, a));
                return builtin_type+TYPE_KIND_VOID;
            }
            return builtin_type+TYPE_KIND_TYPE;
        case '+': case '-':
            if(a->kind < TYPE_KIND_BOOL || a->kind > TYPE_KIND_F64) {
                job_assert(jp, 0, op_ast->base.loc, "invalid type %s to '%c'", job_type_to_str(jp, a), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            return a;
        case '!':
            if(a->kind < TYPE_KIND_BOOL || a->kind > TYPE_KIND_INT) {
                job_assert(jp, 0, op_ast->base.loc, "invalid type %s to '%c'", job_type_to_str(jp, a), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            return builtin_type+TYPE_KIND_BOOL;
        case '~':
            if(a->kind < TYPE_KIND_BOOL || a->kind > TYPE_KIND_INT) {
                job_assert(jp, 0, op_ast->base.loc, "invalid type %s to '%c'", job_type_to_str(jp, a), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            return a;
        case '*':
            if(a->kind == TYPE_KIND_TYPE) {
                return builtin_type+TYPE_KIND_TYPE;
            }
            if(a->kind != TYPE_KIND_POINTER) {
                job_assert(jp, 0, op_ast->base.loc, "invalid type %s to '%c'", job_type_to_str(jp, a), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            return a->pointer.to;
    }
}

Type* typecheck_assign(Job *jp, Type *a, Type *b, Token op) {
    assert(a->kind != TYPE_KIND_VOID && b->kind != TYPE_KIND_VOID);

    switch(op) {
        default:
            UNREACHABLE;
        case '=':
            if(a->kind < TYPE_KIND_TYPE) {
                if(b->kind >= TYPE_KIND_TYPE) return builtin_type+TYPE_KIND_VOID;

                if(a->kind == b->kind) return a;

                if(a->kind >= TYPE_KIND_INT || b->kind == TYPE_KIND_INT) /* integers are very flexible */
                    return a;

                if(b->kind > a->kind) return builtin_type+TYPE_KIND_VOID;
                if(b->kind == TYPE_KIND_CHAR) return builtin_type+TYPE_KIND_VOID;
                if(b->kind == TYPE_KIND_BOOL) return builtin_type+TYPE_KIND_VOID;

                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_TYPE && b->kind == TYPE_KIND_TYPE) {
                return a;
            } else if(a->kind == TYPE_KIND_ARRAY_VIEW) {
                if( b->kind < TYPE_KIND_ARRAY || b->kind > TYPE_KIND_ARRAY_VIEW) return builtin_type+TYPE_KIND_VOID;
                if(a->array.of != b->array.of) return builtin_type+TYPE_KIND_VOID;

                a->array.n = b->array.n;

                return a;
            } else if(a->kind == TYPE_KIND_ARRAY || a->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                if(a->kind != b->kind) return builtin_type+TYPE_KIND_VOID;
                if(a->array.of != b->array.of) return builtin_type+TYPE_KIND_VOID;
                if(a->kind == TYPE_KIND_ARRAY && a->array.n < b->array.n) return builtin_type+TYPE_KIND_VOID;

                return a;
            } else {
                UNIMPLEMENTED;
            }
            break;
        case TOKEN_PLUSEQUAL: case TOKEN_MINUSEQUAL: case TOKEN_TIMESEQUAL: case TOKEN_DIVEQUAL:
            if(a->kind < TYPE_KIND_TYPE && b->kind < TYPE_KIND_TYPE) {
                if(b->kind > a->kind) return builtin_type+TYPE_KIND_VOID;

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind <= TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                return builtin_type+TYPE_KIND_VOID;
            } else {
                return builtin_type+TYPE_KIND_VOID;
            }
            break;   
        case TOKEN_MODEQUAL: case TOKEN_ANDEQUAL: case TOKEN_OREQUAL:
        case TOKEN_LSHIFTEQUAL:
        case TOKEN_RSHIFTEQUAL:
        case TOKEN_XOREQUAL: case TOKEN_TILDEEQUAL:
            if(a->kind < TYPE_KIND_FLOAT && b->kind < TYPE_KIND_FLOAT) {
                if(b->kind > a->kind) return builtin_type+TYPE_KIND_VOID;

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                return builtin_type+TYPE_KIND_VOID;
            } else {
                return builtin_type+TYPE_KIND_VOID;
            }
            break;   
    }
}

bool type_compare(Type *a, Type *b) {
    if(a->kind != b->kind) return false;

    if(a->kind <= TYPE_KIND_TYPE) return true;

    if(a->kind == TYPE_KIND_POINTER)
        return type_compare(a->pointer.to, b->pointer.to);

    if(a->kind >= TYPE_KIND_ARRAY && a->kind <= TYPE_KIND_ARRAY_VIEW)
        return (a->array.n == b->array.n) && type_compare(a->array.of, b->array.of);

    if(a->kind == TYPE_KIND_PROC) {
        UNIMPLEMENTED;
    }

    UNIMPLEMENTED;

    return false;
}

Type* typecheck_binary(Job *jp, Type *a, Type *b, AST_expr *op_ast) {
    assert(a->kind != TYPE_KIND_VOID && b->kind != TYPE_KIND_VOID);

    Type *a_save = a;
    Type *b_save = b;
    Token op = op_ast->token;

    switch(op) {
        default:
            UNREACHABLE;
        case TOKEN_CAST:
            if(type_compare(a, b))
                return a;

            if(a->kind > TYPE_KIND_VOID && a->kind < TYPE_KIND_TYPE) {
                if((b->kind > TYPE_KIND_VOID && b->kind < TYPE_KIND_TYPE) ||
                   (b->kind == TYPE_KIND_POINTER && b->pointer.to->kind == TYPE_KIND_VOID))
                    return a;
                job_assert(jp, 0, op_ast->base.loc, "cannot cast '%s' to '%s'", job_type_to_str(jp, b), job_type_to_str(jp, a));
                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_ARRAY_VIEW) {
                if((b->kind >= TYPE_KIND_ARRAY && b->kind <= TYPE_KIND_ARRAY_VIEW && type_compare(a->array.of, b->array.of)) ||
                   (b->kind == TYPE_KIND_POINTER && b->pointer.to->kind == TYPE_KIND_VOID))
                    return a;
                job_assert(jp, 0, op_ast->base.loc, "cannot cast '%s' to '%s'", job_type_to_str(jp, b), job_type_to_str(jp, a));
                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_POINTER) {
                if(a->pointer.to->kind == TYPE_KIND_VOID || b->pointer.to->kind == TYPE_KIND_VOID)
                    return a;
                job_assert(jp, 0, op_ast->base.loc, "cannot cast '%s' to '%s'", job_type_to_str(jp, b), job_type_to_str(jp, a));
                return builtin_type+TYPE_KIND_VOID;
            } else {
                job_assert(jp, 0, op_ast->base.loc, "cannot cast '%s' to '%s'", job_type_to_str(jp, b), job_type_to_str(jp, a));
                return builtin_type+TYPE_KIND_VOID;
            }
            break;
        case '[':
            job_assert(jp, b->kind >= TYPE_KIND_BOOL && b->kind <= TYPE_KIND_INT, op_ast->base.loc,
                    "subscript expression of type '%s' cannot coerce to 'int'", job_type_to_str(jp, a));

            if(a->kind == TYPE_KIND_TYPE) {
                return builtin_type+TYPE_KIND_TYPE;
            } else {
                assert("only pointers and array can be subscripted"&&a->kind >= TYPE_KIND_ARRAY && a->kind <= TYPE_KIND_POINTER);
                if(a->kind == TYPE_KIND_POINTER) {
                    return a->pointer.to;
                } else {
                    //TODO array bounds checking
                    return a->array.of;
                }
            }
            return builtin_type+TYPE_KIND_VOID;
            break;
        case '+': case '-': case '*': case '/':
            if(a->kind < b->kind) { // commutative
                Type *tmp = a;
                a = b;
                b = tmp;
            }

            if(a->kind < TYPE_KIND_TYPE && b->kind < TYPE_KIND_TYPE) {
                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                job_assert(jp, 0, op_ast->base.loc, "invalid types '%s' and '%s' to '%c'",
                        job_type_to_str(jp, a_save), job_type_to_str(jp, b_save), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_TYPE && b->kind == TYPE_KIND_TYPE) {
                return a;
            } else {
                job_assert(jp, 0, op_ast->base.loc, "invalid types '%s' and '%s' to '%c'",
                        job_type_to_str(jp, a_save), job_type_to_str(jp, b_save), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            break;
        case '%': case '&': case '|': case '^': case TOKEN_LSHIFT: case TOKEN_RSHIFT: 
            if(a->kind < b->kind) { // commutative
                Type *tmp = a;
                a = b;
                b = tmp;
            }

            if(a->kind < TYPE_KIND_FLOAT && b->kind < TYPE_KIND_FLOAT) {
                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                job_assert(jp, 0, op_ast->base.loc, "invalid types '%s' and '%s' to '%c'",
                        job_type_to_str(jp, a_save), job_type_to_str(jp, b_save), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            } else {
                job_assert(jp, 0, op_ast->base.loc, "invalid types '%s' and '%s' to '%c'",
                        job_type_to_str(jp, a_save), job_type_to_str(jp, b_save), (char)op);
                return builtin_type+TYPE_KIND_VOID;
            }
            break;
        case TOKEN_AND: case TOKEN_OR:
            UNIMPLEMENTED;
            break;
    }
}

void typecheck_expr(Job *jp) {
    assert(jp->expr && arrlen(jp->expr) > 0);
    Arr(Value*) value_stack = jp->value_stack;
    Arr(Type*) type_stack = jp->type_stack;
    Arr(AST**) expr = jp->expr;
    u64 pos = jp->expr_pos;

    for(; pos < arrlen(expr); ++pos) {
        ASTkind kind = expr[pos][0]->kind;
        if(kind == AST_KIND_atom) {
            AST_atom *atom = (AST_atom*)expr[pos][0];
            if(atom->token == TOKEN_IDENT) {
                Sym *sym = NULL;
                sym = job_scope_lookup(jp, atom->text);

                if(!sym) sym = global_scope_lookup(atom->text);

                if(!sym) { 
                    jp->state = JOB_STATE_WAIT;
                    jp->waiting_on_name = atom->text;
                    break;
                }

                atom->symbol_annotation = sym;
                atom->type_annotation = sym->type;
                arrpush(type_stack, sym->type);

                if(sym->constant) {
                    atom->value_annotation = sym->value;
                    arrpush(value_stack, sym->value);
                } else {
                    atom->value_annotation = builtin_value+VALUE_KIND_NIL;
                    arrpush(value_stack, builtin_value+VALUE_KIND_NIL);
                }

            } else {
                Type *t = atom_to_type(jp, atom);
                Value *v = atom_to_value(jp, atom);

                atom->type_annotation = t;
                atom->value_annotation = v;

                arrpush(type_stack, t);
                arrpush(value_stack, v);
            }
        } else if(kind == AST_KIND_expr) {
            AST_expr *node = (AST_expr*)(expr[pos][0]);

            Type *result_type = NULL;
            Value *result_value = NULL;

            if(node->left && node->right) {
                Type *b_type = arrpop(type_stack);
                Type *a_type = arrpop(type_stack);

                Value *b_value = arrpop(value_stack);
                Value *a_value = arrpop(value_stack);

                if(node->token == TOKEN_CAST) {
                    job_assert(jp, a_type->kind == TYPE_KIND_TYPE, node->base.loc, "cast must be to type");
                    if(jp->state == JOB_STATE_ERROR)
                        break;
                    a_type = a_value->val.type;
                }

                result_type = typecheck_binary(jp, a_type, b_type, node);

                result_value = evaluate_binary(jp, a_value, b_value, node);

                node->type_annotation = result_type;
                node->value_annotation = result_value;

                arrpush(type_stack, result_type);
                arrpush(value_stack, result_value);
            } else {
                Type *a_type = arrpop(type_stack);

                result_type = typecheck_unary(jp, a_type, node);

                Value *a_value = arrpop(value_stack);

                result_value = evaluate_unary(jp, a_value, node);

                node->type_annotation = result_type;
                node->value_annotation = result_value;
                
                arrpush(type_stack, result_type);
                arrpush(value_stack, result_value);
            }

            if(jp->state == JOB_STATE_ERROR)
                break;
        } else if(kind == AST_KIND_array_literal) {
            AST_array_literal *array_lit = (AST_array_literal*)(expr[pos][0]);
            Type *array_elem_type = NULL;
            u64 i = 0;

            if(array_lit->type) {
                Type *t = arrpop(type_stack);
                Value *t_value = arrpop(value_stack);
                job_assert(jp, t->kind == TYPE_KIND_TYPE, array_lit->type->loc,
                        "expected type expression before array literal, not '%s'",
                        job_type_to_str(jp, t));
                array_elem_type = t_value->val.type;
                i = arrlen(type_stack) - array_lit->n;
            } else {
                i = arrlen(type_stack) - array_lit->n;
                array_elem_type = type_stack[i++];
            }

            for(; i < arrlen(type_stack); ++i) {
                Type *t = typecheck_assign(jp, array_elem_type, type_stack[i], '=');
                job_assert(jp, t->kind != TYPE_KIND_VOID, array_lit->base.loc,
                        "cannot have element of type '%s' in array literal with element type '%s'",
                        job_type_to_str(jp, type_stack[i]), job_type_to_str(jp, array_elem_type));
            }
            Value **elements = job_alloc_scratch(jp, sizeof(Value*) * array_lit->n);
            i = arrlen(value_stack) - array_lit->n;
            u64 j = 0;
            while(j < array_lit->n)
                elements[j++] = value_stack[i++];
            Value *array_val = job_alloc_value(jp, VALUE_KIND_ARRAY);
            array_val->val.array.n = array_lit->n;
            array_val->val.array.type = array_elem_type;
            array_val->val.array.elements = elements;

            arrsetlen(type_stack, arrlen(type_stack) - array_lit->n);
            arrsetlen(value_stack, arrlen(value_stack) - array_lit->n);

            Type *array_type = job_alloc_type(jp, TYPE_KIND_ARRAY);
            array_type->array.of = array_elem_type;
            array_type->array.n = array_lit->n;

            array_lit->type_annotation = array_type;
            array_lit->value_annotation = array_val;

            arrpush(type_stack, array_type);
            arrpush(value_stack, array_val);
        } else if(kind == AST_KIND_param) {
            AST_param *paramp = (AST_param*)expr[pos][0];
            arrlast(value_stack)->name = paramp->name;
        } else if(kind == AST_KIND_call) {
            assert(arrlast(type_stack)->kind == TYPE_KIND_PROC);

            AST_call *callp = (AST_call*)expr[pos][0];
            Type *proc_type = arrpop(type_stack);
            arrsetlen(value_stack, arrlen(value_stack) - 1);

            u8 params_passed[proc_type->proc.param.n]; //TODO the language should have variable capacity stack arrays
            memset(params_passed, 0, proc_type->proc.param.n);
            for(int i = proc_type->proc.first_default_param; i < proc_type->proc.param.n; ++i)
                params_passed[i] = 2;

            assert(arrlen(type_stack) == arrlen(value_stack));

            if(proc_type->proc.has_defaults && callp->n_params < proc_type->proc.first_default_param) {
                job_assert(jp, 0, callp->base.loc, "not enough parameters in call");
                return;
            } else if(!proc_type->proc.has_defaults && callp->n_params < proc_type->proc.param.n) {
                job_assert(jp, 0, callp->base.loc, "not enough parameters in call");
                return;
            } else if(!proc_type->proc.varargs && callp->n_params > proc_type->proc.param.n) {
                job_assert(jp, 0, callp->base.loc, "too many parameters in call");
                return;
            }
            
            int base_index = arrlen(type_stack) - callp->n_params;
            int n_positional_params = callp->has_named_params ? callp->first_named_param : callp->n_params;

            for(int i = 0; i < n_positional_params; ++i) {
                Type *param_type = type_stack[base_index + i];
                Type *expected_type = proc_type->proc.param.types[i];

                Type *t = typecheck_assign(jp, expected_type, param_type, '=');

                if(t->kind == TYPE_KIND_VOID) {
                    if(proc_type->proc.name == NULL) {
                        job_assert(jp, 0, callp->base.loc,
                                "cannot pass type '%s' to parameter of type '%s'",
                                job_type_to_str(jp, param_type),
                                job_type_to_str(jp, expected_type));
                    } else {
                        job_assert(jp, 0, callp->base.loc,
                                "cannot pass type '%s' to parameter '%s' of type '%s'",
                                job_type_to_str(jp, param_type),
                                proc_type->proc.param.names[i],
                                job_type_to_str(jp, expected_type));
                    }
                }

                params_passed[i] = 1;
            }

            if(callp->has_named_params) {
                assert(proc_type->proc.name != NULL);

                base_index = arrlen(type_stack) - callp->n_params + callp->first_named_param;

                for(int i = 0; i < callp->n_params; ++i) {
                    Type *param_type = type_stack[base_index + i];
                    char *param_name = value_stack[base_index + i]->name;

                    Type *expected_type = NULL;
                    int param_index = 0;
                    for(; param_index < proc_type->proc.param.n; ++param_index) {
                        if(!strcmp(param_name, proc_type->proc.param.names[param_index])) {
                            expected_type = proc_type->proc.param.types[param_index];
                            break;
                        }
                    }

                    if(expected_type == NULL) {
                        //TODO custom formatting for printing locations
                        job_assert(jp, 0, callp->base.loc,
                                "procedure '%s' has no parameter named '%s'", proc_type->proc.name, param_name);
                    }

                    if(params_passed[param_index] == 1) {
                        //TODO custom formatting for printing locations
                        job_assert(jp, 0, callp->base.loc,
                                "parameter '%s' was passed multiple times", param_name);
                    }

                    params_passed[param_index] = 1;

                    Type *t = typecheck_assign(jp, expected_type, param_type, '=');

                    job_assert(jp, t->kind != TYPE_KIND_VOID, callp->base.loc,
                            "invalid type '%s' passed to parameter '%s' in call to '%s', expected type '%s'",
                            job_type_to_str(jp, param_type),
                            proc_type->proc.param.names[param_index],
                            proc_type->proc.name,
                            job_type_to_str(jp, expected_type));
                }
            }

            for(int i = 0; i < proc_type->proc.param.n; ++i) {
                if(params_passed[i] == 0) {
                    assert(proc_type->proc.name != NULL);
                    job_assert(jp, 0, callp->base.loc,
                            "missing parameter '%s' in call to '%s'",
                            proc_type->proc.param.names[i], proc_type->proc.name);
                    break;
                }
            }

            if(jp->state == JOB_STATE_ERROR) return;

            bool is_call_expr = (callp->n_params == arrlen(type_stack) && pos == arrlen(expr) - 1);

            arrsetlen(type_stack, arrlen(type_stack) - callp->n_params);
            arrsetlen(value_stack, arrlen(value_stack) - callp->n_params);

            if(is_call_expr) { /* if the only thing in the expr is a call */
                assert(arrlen(type_stack) == 0);

                callp->n_types_returned = proc_type->proc.ret.n;
                callp->type_annotation_list = job_alloc_scratch(jp, sizeof(Type*) * proc_type->proc.ret.n);
                for(int i = 0; i < proc_type->proc.ret.n; ++i) {
                    callp->type_annotation_list[i] = proc_type->proc.ret.types[i];
                }
            } else {
                callp->n_types_returned = 1;
                callp->type_annotation_list = job_alloc_scratch(jp, sizeof(Type*));
                callp->type_annotation_list[0] = proc_type->proc.ret.types[0];
                arrpush(type_stack, proc_type->proc.ret.types[0]);
                arrpush(value_stack, builtin_value+VALUE_KIND_NIL);
            }

        } else {
            UNIMPLEMENTED;
        }
    }

    jp->value_stack = value_stack;
    jp->type_stack = type_stack;
    jp->expr = expr;
    jp->expr_pos = pos;
}

void linearize_expr(Job *jp, AST **astpp) {
    if(!*astpp) return;

    if(astpp[0]->kind == AST_KIND_expr) {
        AST_expr *expr = (AST_expr*)*astpp;
        linearize_expr(jp, &expr->left);
        linearize_expr(jp, &expr->right);
        arrpush(jp->expr, astpp);
    } else if(astpp[0]->kind == AST_KIND_atom) {
        arrpush(jp->expr, astpp);
    } else if(astpp[0]->kind == AST_KIND_array_literal) {
        AST_array_literal *array_lit = (AST_array_literal*)(astpp[0]);
        for(AST_expr_list *list = array_lit->elements; list; list = list->next) {
            linearize_expr(jp, &list->expr);
        }

        linearize_expr(jp, &array_lit->type);

        arrpush(jp->expr, astpp);
    } else if(astpp[0]->kind == AST_KIND_param) {
        AST_param *param = (AST_param*)(astpp[0]);
        linearize_expr(jp, &param->value);
        arrpush(jp->expr, astpp);
        linearize_expr(jp, (AST**)&param->next);
    } else if(astpp[0]->kind == AST_KIND_call) {
        AST_call *callp = (AST_call*)(astpp[0]);
        linearize_expr(jp, (AST**)&callp->params);
        linearize_expr(jp, &callp->callee);
        arrpush(jp->expr, astpp);
    } else {
        UNIMPLEMENTED;
    }
}

void typecheck_procdecl(Job *jp) {
    assert(jp->step > TYPECHECK_STEP_PROCDECL_BEGIN && jp->step < TYPECHECK_STEP_PROCDECL_END);
    AST_procdecl *ast = (AST_procdecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_procdecl);

    if(jp->handling_name == NULL)
        jp->handling_name = ast->name;

    bool is_top_level = ast->is_top_level;

    if(jp->proc_type == NULL) {
        jp->proc_type = job_alloc_type(jp, TYPE_KIND_PROC);
        jp->proc_type->proc.name = ast->name;
    }
    Type *proc_type = jp->proc_type;

    if(jp->step >= TYPECHECK_STEP_PROCDECL_PARAMS_BEGIN && jp->step <= TYPECHECK_STEP_PROCDECL_PARAMS_END) {
        u64 n = proc_type->proc.param.n;

        if(n == 0) {
            jp->cur_paramdecl = ast->params;
            n = proc_type->proc.param.n = ast->n_params;
            proc_type->proc.param.names = (char**)job_alloc_scratch(jp, sizeof(char*) * n);
            proc_type->proc.param.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
            proc_type->proc.param.values = (Value**)job_alloc_scratch(jp, sizeof(Value*) * n);
        }

        if(jp->step == TYPECHECK_STEP_PROCDECL_PARAMS_BEGIN)
            jp->step = TYPECHECK_STEP_PROCDECL_PARAMS_BIND_TYPE;

        for(AST_paramdecl *p = jp->cur_paramdecl; p; p = p->next) {
            proc_type->proc.param.names[p->index] = p->name;

            bool initialize = (p->init != NULL);

            if(jp->step == TYPECHECK_STEP_PROCDECL_PARAMS_BIND_TYPE) {
                if(arrlen(jp->expr) == 0)
                    linearize_expr(jp, &p->type);
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR)
                    UNIMPLEMENTED;

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                if(initialize)
                    jp->step = TYPECHECK_STEP_PROCDECL_PARAMS_INITIALIZE;
            }

            if(initialize && jp->step == TYPECHECK_STEP_PROCDECL_PARAMS_INITIALIZE) {
                if(arrlen(jp->expr) == 0)
                    linearize_expr(jp, &p->init);
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR)
                    UNIMPLEMENTED;

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                jp->step = TYPECHECK_STEP_PROCDECL_PARAMS_BIND_TYPE;
            }

            Type *bind_type = ((AST_expr*)(p->type))->value_annotation->val.type;
            Value *init_value = NULL;
            Type *init_type = NULL;

            if(initialize) {
                init_value = ((AST_expr*)(p->init))->value_annotation;
                init_type = ((AST_expr*)(p->init))->type_annotation;

                job_assert(jp, init_value->kind != VALUE_KIND_NIL, p->base.loc,
                        "parameter default values must evaluate at compile time");

                Type *t = typecheck_assign(jp, bind_type, init_type, '=');

                job_assert(jp, t->kind != TYPE_KIND_VOID, p->base.loc,
                        "invalid assignment of '%s' to parameter of type '%s'",
                        job_type_to_str(jp, init_type), job_type_to_str(jp, bind_type));

                if(jp->state == JOB_STATE_ERROR) return;

                if(t->kind >= TYPE_KIND_FLOAT && t->kind <= TYPE_KIND_F64 && init_value->kind == VALUE_KIND_INT) {
                    init_value->kind = VALUE_KIND_FLOAT;
                    init_value->val.floating = (float)init_value->val.integer;
                } else if(t->kind >= TYPE_KIND_FLOAT && t->kind <= TYPE_KIND_F64 && init_value->kind == VALUE_KIND_UINT) {
                    init_value->kind = VALUE_KIND_FLOAT;
                    init_value->val.floating = (float)init_value->val.uinteger;
                }

                bind_type = t;
            }

            Sym *ptr = job_scope_lookup(jp, p->name);

            if(ptr) {
                job_assert(jp, 0, ast->base.loc,
                        "multiple declaration of parameter '%s' in header of '%s'",
                        p->name, ast->name, ptr->loc.line);
                return;
            }

            Sym sym = {
                .name = p->name,
                .loc = p->base.loc,
                .declared_by = jp->id,
                .type = bind_type,
                .value = init_value,
                .initializer = p->init,
            };

            job_scope_enter(jp, sym);

            proc_type->proc.param.types[p->index] = bind_type;
            proc_type->proc.param.values[p->index] = init_value;
        }

        if(jp->step == TYPECHECK_STEP_PROCDECL_PARAMS_BIND_TYPE)
            jp->step = TYPECHECK_STEP_PROCDECL_RETURN_VALUES;
    }

    if(jp->step == TYPECHECK_STEP_PROCDECL_RETURN_VALUES) {
        u64 n = proc_type->proc.ret.n;

        if(n == 0) {
            jp->cur_retdecl = ast->rets;
            n = proc_type->proc.ret.n = ast->n_rets;
            proc_type->proc.ret.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
        }

        for(AST_retdecl *r = jp->cur_retdecl; r; r = r->next) {
            if(arrlen(jp->expr) == 0)
                linearize_expr(jp, &r->expr);
            typecheck_expr(jp);

            if(jp->state == JOB_STATE_WAIT) {
                jp->cur_retdecl = r;
                return;
            }

            if(jp->state == JOB_STATE_ERROR)
                UNIMPLEMENTED;

            arrsetlen(jp->type_stack, 0);
            arrsetlen(jp->value_stack, 0);
            arrsetlen(jp->expr, 0);
            jp->expr_pos = 0;

            assert(((AST_expr*)(r->expr))->type_annotation->kind == TYPE_KIND_TYPE);
            Type *ret_type = ((AST_expr*)(r->expr))->value_annotation->val.type;
            proc_type->proc.ret.types[r->index] = ret_type;
        }
    }

    Sym *ptr = is_top_level ? global_scope_lookup(ast->name) : job_scope_lookup(jp, ast->name);

    if(ptr) {
        job_assert(jp, 0, ast->base.loc,
                "multiple declaration of identifier '%s', previously declared at line %i",
                ast->name, ptr->loc.line);
        return;
    }

    proc_type->proc.first_default_param = ast->first_default_param;
    proc_type->proc.varargs = ast->varargs;
    proc_type->proc.has_defaults = ast->has_defaults;

    Sym proc_sym = {
        .name = ast->name,
        .loc = ast->base.loc,
        .declared_by = jp->id,
        .constant = true,
        .type = proc_type,
    };

    jp->cur_proc_type = proc_type;

    if(is_top_level) {
        global_scope_enter(proc_sym);
    } else {
        UNIMPLEMENTED;
    }
}

void typecheck_vardecl(Job *jp) {
    assert(jp->step > TYPECHECK_STEP_VARDECL_BEGIN && jp->step < TYPECHECK_STEP_VARDECL_END);
    AST_vardecl *ast = (AST_vardecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_vardecl);

    if(jp->handling_name == NULL)
        jp->handling_name = ast->name;

    bool infer_type = (ast->type == NULL);
    bool initialize = (ast->init != NULL);
    bool is_top_level = ast->is_top_level;

    if(infer_type) jp->step = TYPECHECK_STEP_VARDECL_INITIALIZE;

    if(jp->step == TYPECHECK_STEP_VARDECL_BIND_TYPE) {
        if(arrlen(jp->expr) == 0)
            linearize_expr(jp, &ast->type);
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        arrsetlen(jp->type_stack, 0);
        arrsetlen(jp->value_stack, 0);
        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        jp->step = TYPECHECK_STEP_VARDECL_INITIALIZE;
    }

    if(!initialize) jp->step = TYPECHECK_STEP_VARDECL_END;

    if(jp->step == TYPECHECK_STEP_VARDECL_INITIALIZE) {
        if(arrlen(jp->expr) == 0)
            linearize_expr(jp, &ast->init);
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        arrsetlen(jp->type_stack, 0);
        arrsetlen(jp->value_stack, 0);
        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        jp->step = TYPECHECK_STEP_VARDECL_END;
    }

    if(jp->state == JOB_STATE_ERROR) return;

    char *name = ast->name;
    Type *bind_type = NULL;
    Value *init_value = NULL;
    Type *init_type = NULL;

    if(!infer_type) {
        job_assert(jp, ((AST_expr*)ast->type)->value_annotation->kind == VALUE_KIND_TYPE, ast->type->loc,
                "expected type to bind to '%s'", name);
        if(jp->state == JOB_STATE_ERROR) return;
        bind_type = ((AST_expr*)ast->type)->value_annotation->val.type;
    }

    if(initialize) {
        //TODO multi-identifier declarations so we can initialize from a function that returns multiple values
        if(ast->init->kind == AST_KIND_call) {
            init_type = ((AST_call*)ast->init)->type_annotation_list[0];
        } else {
            init_value = ((AST_expr*)ast->init)->value_annotation;
            init_type = ((AST_expr*)ast->init)->type_annotation;
        }

        if(!infer_type) {
            Type *t = typecheck_assign(jp, bind_type, init_type, '=');
            job_assert(jp, t->kind != TYPE_KIND_VOID, ast->base.loc,
                    "invalid assignment of '%s' to %s of type '%s'",
                    job_type_to_str(jp, init_type), ast->constant ? "constant" : "variable", job_type_to_str(jp, bind_type));

            if(jp->state == JOB_STATE_ERROR) return;

            if(t->kind >= TYPE_KIND_FLOAT && t->kind <= TYPE_KIND_F64 && init_value->kind == VALUE_KIND_INT) {
                init_value->kind = VALUE_KIND_FLOAT;
                init_value->val.floating = (float)init_value->val.integer;
            } else if(t->kind >= TYPE_KIND_FLOAT && t->kind <= TYPE_KIND_F64 && init_value->kind == VALUE_KIND_UINT) {
                init_value->kind = VALUE_KIND_FLOAT;
                init_value->val.floating = (float)init_value->val.uinteger;
            }

            bind_type = t;
        } else {
            bind_type = init_type;
        }
    }

    if(is_top_level) {
        Sym *ptr = global_scope_lookup(name);

        if(ptr) {
            job_assert(jp, 0, ast->base.loc,
                    "multiple declaration of identifier '%s', previously declared at line %i",
                    name, ptr->loc.line);
            return;
        }

        Sym sym = {
            .name = name,
            .loc = ast->base.loc,
            .declared_by = jp->id,
            .constant = ast->constant,
            .type = bind_type,
            .value = init_value,
            .initializer = ast->init,
        };

        global_scope_enter(sym);
    } else {
        UNIMPLEMENTED;
    }
}


char *test_src[] = {
"i: float = 12.2 - 4;\n",
"pointer_to_array_of_int: *[12]int;\n",

"test_array := int.[1 + 4, 2, 3];\n",

"my_string: [6]char = \"abcde\";\n"
"my_string: int = 132;\n",

"NUMBER_THIRTEEN : 13;\n"
"i int = NUMBER_THIRTEEN * 100;\n",

//"f := atan2(i * 1.4e3 + 0.3, y + \"this wouldn't pass typechecking lol\", z);\n"
"i: int = 12;\n"
"x : y;\n"
"TWO_PI := PI * 2;\n"
"PI :: TWO_PI / 2;\n"
"A :: B;\n"
"B :: C;\n"
"C :: A;\n"
"s := \"hello sailor\";\n"
"test_array_2 := .[1 + 4, 2, 3];\n"
"proc test_proc(i: int, c: [3]*[..]char, f: float = 3.14) int, char;\n",

"two := add_one(x=1);\n"
"proc add_one(x: int) int;\n"
,
"n := my_proc(10, cast(u64)cast(*void)cast(*int)cast(*void)12, cast(*void)cast([3]char)\"123\");\n"
"proc my_proc(a: int, b: u64, s: []char = \"abc\") int, *void;\n"
,
};

void print_sym(Sym sym) {
    printf("name: %s\n", sym.name);
    printf("declared_by: %d\n", sym.declared_by);
    printf("constant: %s\n", sym.constant ? "true" : "false");
    printf("type: %s\n", global_type_to_str(sym.type));
    printf("value: %p\n", (void *)sym.value);
}

int main(void) {
    arena_init(&global_scratch_allocator);
    pool_init(&global_sym_allocator, sizeof(Sym));
    pool_init(&global_type_allocator, sizeof(Type));
    pool_init(&global_value_allocator, sizeof(Value));

    char *test_src_file = LoadFileText("test/main.jpl");

    //job_runner(test_src[7], "not a file");
    job_runner(test_src_file, "test/main.jpl");
    for(int i = 0; i < shlen(global_scope); ++i) {
        if(global_scope[i].value) {
            print_sym(global_scope[i].value[0]);
            printf("\n");
        }
    }
    return 0;
}
