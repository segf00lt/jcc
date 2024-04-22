#include "basic.h"
#include "stb_ds.h"
#include "pool.h"
#include "arena.h"

#include "lexer.c"


#define PIPE_STAGES              \
    X(PARSE)                     \
    X(TYPECHECK)                 \
    X(SIZE)                      \
    X(IR)                        \

#define JOB_STATES               \
    X(READY)                     \
    X(WAIT)                      \
    X(BLOCK)                     \

#define ASTKINDS                 \
    X(vardecl)                   \
    X(expr)                      \
    X(param)                     \
    X(atom)                      \
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

#define TYPECHECK_STEPS                 \
    X(NONE)                             \
    X(VARDECL_BEGIN)                    \
    X(VARDECL_BIND_TYPE)                \
    X(VARDECL_INITIALIZE)               \
    X(VARDECL_END)                      \

#define VALUEKINDS                      \
    X(NIL)                              \
    X(BOOL)                             \
    X(CHAR)                             \
    X(INT)                              \
    X(UINT)                             \
    X(FLOAT)                            \
    X(TYPE)                             \
    X(STRING)                           \
    X(ARR_OR_STRUCT_LIT)                \
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
    X(SLICE)                            \
    X(POINTER)                          \
    X(STRUCT)                           \
    X(UNION)                            \
    X(PROC)                             \


typedef struct Scope_entry* Scope;
typedef int Jobid;
typedef struct Value Value;
typedef struct Sym Sym;
typedef struct Type Type;
typedef struct Job Job;
typedef struct Job_memory Job_memory;
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


struct Scope_entry {
    char *key;
    Sym *value;
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

    char                *waiting_on_name;
    u64                  job_id_waiting;

    Lexer               *lexer;

    Arr(Scope)           scopes;
    Arr(AST*)            tree_pos_stack;
    Arr(Value*)          value_stack;
    Arr(Type*)           type_stack;
    Arr(AST**)           expr;
    u64                  expr_pos;

    bool                 must_const_evaluate; /* used when we process const expressions */
    
    Job_memory allocator;
};

struct Sym {
    char *name;
    Jobid declared_by;
    bool constant;
    Type *type;
    Value *value;
};

struct AST {
    ASTkind kind;
};

struct AST_param {
    AST base;
    char *name;
    AST *value;
    AST_param *next;
};

struct AST_call {
    AST base;
    AST *callee;
    AST_param *params;
};

struct AST_expr {
    AST base;
    Token token;
    struct {
        AST *left;
        AST *right;
    };
};

struct AST_atom {
    AST base;
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
    char *name;
    bool constant;
    AST *type;
    AST *init;
    AST *next;
};

struct Value {
    Valuekind kind;
    union {
        s64 integer;
        u64 uinteger;
        f32 floating;
        f64 dfloating;
        char character;
        bool boolean;
        Token token;
        char *str;
        Type *type;
        struct {
            Type *type;
            char **member_names;
            Value **values;
        } arr_or_struct_lit;
        struct {
            char *name;
            Value *value;
        } param;
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
        } procedure;

        struct { /* static array, dynamic or slice */
            u64 n;
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
char*   job_alloc_str(Job *jp, char *s);

void    job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void    job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void    job_runner(char *src, char *src_path);
Sym*    job_scope_lookup(Job *jp, char *name);
void    job_scope_enter(Job *jp, Sym sym);
void    linearize_expr(Job *jp, AST **astpp);

Value*  atom_to_value(Job *jp, AST_atom *atom);
Type*   atom_to_type(Job *jp, AST_atom *atom);

Value*  evaluate_unary(Job *jp, Type *type, Value *a, Token op);
Value*  evaluate_binary(Job *jp, Type *type, Value *a, Value *b, Token op);

Type*   typecheck_unary(Job *jp, Type *a, Token op);
Type*   typecheck_binary(Job *jp, Type *a, Type *b, Token op);
void    typecheck_expr(Job *jp);
void    typecheck_vardecl(Job *jp);
void    typecheck_procdecl(Job *jp);
void    typecheck_ifstatement(Job *jp);
void    typecheck_whilestatement(Job *jp);

int     getprec(Token t);

AST*    parse_vardecl(Job *jp);
AST*    parse_type_expr(Job *jp);
AST*    parse_expr(Job *jp);
AST*    parse_expr_increase_prec(Job *jp, AST *left, int min_prec);
AST*    parse_expr_decrease_prec(Job *jp, int min_prec);
AST*    parse_term(Job *jp);

void*   global_alloc(size_t bytes);
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
    return ptr;
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

int getprec(Token t) {
    switch(t) {
#define X(t, prec) case (Token)t: return prec;
        OPERATOR_PREC_TABLE
#undef X
        default: return -1;
    }
}

/*
 * vardecl: ident type_expr (('=' | ':') expr)? ';'
 *       | ident (':=' | '::') expr ';'
 */
AST* parse_vardecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        *lexer = unlex;
        return NULL;
    }

    AST_vardecl *node = (AST_vardecl*)job_alloc_ast(jp, AST_KIND_vardecl);
    node->name = global_alloc_text(lexer->text.s, lexer->text.e);

    unlex = *lexer;
    t = lex(lexer);

    if(t == TOKEN_WALRUS || t == TOKEN_TWOCOLON) {
        node->constant = (t == TOKEN_TWOCOLON);
        node->init = parse_expr(jp);
        t = lex(lexer);
    } else {
        *lexer = unlex;
        node->type = parse_expr(jp);

        t = lex(lexer);
        if(t == '=' || t == ':') {
            node->constant = (t == ':');
            node->init = parse_expr(jp);
            t = lex(lexer);
        } else if(t != ';' && t != ')' && t != ',' && t != '{') {
            assert("expected '=', ':' or separator in vardecl"&&0);
        }
    }

    //TODO compiler error messages
    assert("expected separator at end of variable declaration"&&!(t != ';' && t != ')' && t != ',' && t != '{'));

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
        case '[':
            array_type = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            array_type->token = t;
            array_type->left = parse_expr(jp);
            t = lex(lexer);
            //TODO compiler error messages
            assert("unbalanced square bracket"&&(t == ']'));
            array_type->right = parse_term(jp);
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
            //TODO compiler error messages
            assert("unbalanced parenthesis"&&(t==')'));
            node = (AST*)expr;
            break;
        case TOKEN_TWODOT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            node = (AST*)atom;
            break;
        case TOKEN_VOID:
        case TOKEN_INT:  case TOKEN_UINT: 
        case TOKEN_FLOAT:
        case TOKEN_CHAR: 
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

        AST_param head = {0};
        AST_param *param = &head;


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

            expr = (AST_expr*)parse_expr(jp);

            if(expr == NULL) {
                if(named_param) {
                    //TODO compiler error messages
                    assert("named param has no initializer value"&&0);
                }
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
                //TODO compiler error messages
                assert("expected comma or end of parameter list"&&0);
            }

            param = param->next;
        }

        if(head.name == NULL && head.value == NULL) {
            call_op->params = NULL;
        } else {
            call_op->params = (AST_param*)job_alloc_ast(jp, AST_KIND_param);
            *(call_op->params) = head;
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
                //TODO compiler error messages
                assert("expected identifier in struct member reference"&&(t==TOKEN_IDENT));
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = t;
                atom->text = global_alloc_text(lexer->text.s, lexer->text.e);
                index_op->right = (AST*)atom;
            } else if(t == '[') {
                expr = (AST_expr*)parse_expr(jp);
                t = lex(lexer);
                //TODO compiler error messages
                assert("unbalanced square bracket"&&(t == ']'));
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
    Arr(Job) job_queue_wait = NULL;
    Lexer lexer = {0};
    lexer_init(&lexer, src, src_path);

    Jobid id_alloc = -1;

    arrpush(job_queue, job_spawn(&id_alloc, PIPE_STAGE_PARSE));
    job_queue[0].lexer = &lexer;

    while(arrlen(job_queue) > 0) {
        for(u64 i = 0; i < arrlen(job_queue); ++i) {
            Job *jp = job_queue + i;

            switch(jp->pipe_stage) {
                case PIPE_STAGE_PARSE:
                    while(true) {
                        job_init_allocator_ast(jp);

                        //TODO loop with all top level parse functions
                        AST *ast = parse_vardecl(jp);

                        if(ast == NULL) {
                            Token t = lex(jp->lexer);
                            assert(t == 0);
                            job_die(jp);
                            break;
                        }

                        Job new_job = job_spawn(&id_alloc, PIPE_STAGE_TYPECHECK);

                        new_job.allocator = jp->allocator; /* copy ast allocator */
                        job_init_allocator_scratch(&new_job);
                        job_init_allocator_value(&new_job);
                        job_init_allocator_sym(&new_job);
                        job_init_allocator_type(&new_job);

                        arrpush(new_job.tree_pos_stack, ast);

                        arrpush(job_queue_next, new_job);
                    }
                    break;
                case PIPE_STAGE_TYPECHECK:
                    printf("hey mum look! I'm typechecking\n");
                    AST *ast = arrlast(jp->tree_pos_stack);
                    if(ast->kind == AST_KIND_vardecl) {
                        if(jp->step == TYPECHECK_STEP_NONE)
                            jp->step = TYPECHECK_STEP_VARDECL_BEGIN + 1;
                        typecheck_vardecl(jp);

                        if(jp->state == JOB_STATE_WAIT) {
                            arrpush(job_queue_wait, *jp);
                            continue;
                        }

                    } else {
                        UNIMPLEMENTED;
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

        Arr(Job) tmp = job_queue;
        job_queue = job_queue_next;
        job_queue_next = tmp;
        arrsetlen(job_queue_next, 0);

        for(int i = 0; i < arrlen(job_queue_wait); ++i) {
            job_queue_wait[i].state = JOB_STATE_READY;
            arrpush(job_queue, job_queue_wait[i]);
        }
        arrsetlen(job_queue_wait, 0);
    }

    arrfree(job_queue);
    arrfree(job_queue_next);
    if(job_queue_wait) arrfree(job_queue_wait);
}

Value *atom_to_value(Job *jp, AST_atom *atom) {
    Value *vp = NULL;
    switch(atom->token) {
        default:
            UNIMPLEMENTED;
        case TOKEN_TWODOT:
            vp = job_alloc_value(jp, VALUE_KIND_TOKEN);
            vp->val.token = atom->token;
            break;
        case TOKEN_INT:
            vp = job_alloc_value(jp, VALUE_KIND_TYPE);
            vp->val.type = job_alloc_type(jp, TYPE_KIND_INT);
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
        case TOKEN_TWODOT: /* we do this to make checking array constructors easier */
            tp = job_alloc_type(jp, TYPE_KIND_U64);
            break;
        case TOKEN_INT:
        case TOKEN_UINT:
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

Value* evaluate_unary(Job *jp, Type *type, Value *a, Token op) {
    if(a->kind == VALUE_KIND_NIL)
        return a;

    Value *result = NULL;

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            if(type->kind == TYPE_KIND_TYPE) {
                result = job_alloc_value(jp, VALUE_KIND_TYPE);
                result->val.type = job_alloc_type(jp, TYPE_KIND_SLICE);
                result->val.type->array.of = a->val.type;
            } else {
                UNREACHABLE;
            }
            break;
        case '+':
            result = a;
            break;
        case '-':
            if(type->kind >= TYPE_KIND_FLOAT) {
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
            if(type->kind == TYPE_KIND_TYPE) {
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

Value* evaluate_binary(Job *jp, Type *type, Value *a, Value *b, Token op) {
    Value *result = NULL;

    /* NOTE we return nil value when we wish to ignore the evaluation */

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            if(type->kind == TYPE_KIND_TYPE) {
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
        case '+': case '-': case '*': case '/':
            result = builtin_value+VALUE_KIND_NIL;
            break;
        case '%': case '&': case '|': case '^': case TOKEN_LSHIFT: case TOKEN_RSHIFT: 
            result = builtin_value+VALUE_KIND_NIL;
            break;
        case TOKEN_AND: case TOKEN_OR:
            result = builtin_value+VALUE_KIND_NIL;
            break;
    }

    return result;
}

Type* typecheck_unary(Job *jp, Type *a, Token op) {
    assert(a->kind != TYPE_KIND_VOID);

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            if(a->kind != TYPE_KIND_TYPE) {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            return builtin_type+TYPE_KIND_TYPE;
        case '+': case '-':
            if(a->kind < TYPE_KIND_BOOL && a->kind > TYPE_KIND_F64) {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            return a;
        case '!':
            if(a->kind < TYPE_KIND_BOOL && a->kind > TYPE_KIND_INT) {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            return builtin_type+TYPE_KIND_BOOL;
        case '~':
            if(a->kind < TYPE_KIND_BOOL && a->kind > TYPE_KIND_INT) {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            return a;
        case '*':
            if(a->kind == TYPE_KIND_TYPE) {
                return builtin_type+TYPE_KIND_TYPE;
            }
            if(a->kind != TYPE_KIND_POINTER) {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            return a->pointer.to;
    }
}

Type* typecheck_binary(Job *jp, Type *a, Type *b, Token op) {
    assert(a->kind != TYPE_KIND_VOID && b->kind != TYPE_KIND_VOID);

    switch(op) {
        default:
            UNREACHABLE;
        case '[':
            //TODO compiler error messages
            assert("subscript must coerce to integer value"&& b->kind >= TYPE_KIND_BOOL && b->kind <= TYPE_KIND_INT);

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
            break;
        case '+': case '-': case '*': case '/':
            if(a->kind > b->kind) { // commutative
                Type *tmp = a;
                a = b;
                b = tmp;
            }

            if(a->kind < TYPE_KIND_TYPE && b->kind < TYPE_KIND_TYPE) {
                if(b->kind > a->kind) {
                    //TODO compiler error messages
                    return builtin_type+TYPE_KIND_VOID;
                }

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_TYPE && b->kind == TYPE_KIND_TYPE) {
                return a;
            } else {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            break;
        case '%': case '&': case '|': case '^': case TOKEN_LSHIFT: case TOKEN_RSHIFT: 
            if(a->kind > b->kind) { // commutative
                Type *tmp = a;
                a = b;
                b = tmp;
            }

            if(a->kind < TYPE_KIND_FLOAT && b->kind < TYPE_KIND_FLOAT) {
                if(b->kind > a->kind) {
                    //TODO compiler error messages
                    return builtin_type+TYPE_KIND_VOID;
                }

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            } else {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            break;
        case '=':
            if(a->kind < TYPE_KIND_TYPE && b->kind < TYPE_KIND_TYPE) {
                if(b->kind > a->kind) {
                    //TODO compiler error messages
                    return builtin_type+TYPE_KIND_VOID;
                }

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            } else if(a->kind == TYPE_KIND_TYPE && b->kind == TYPE_KIND_TYPE) {
                return a;
            } else {
                UNIMPLEMENTED;
            }
            break;
        case TOKEN_PLUSEQUAL: case TOKEN_MINUSEQUAL: case TOKEN_TIMESEQUAL: case TOKEN_DIVEQUAL:
            if(a->kind < TYPE_KIND_TYPE && b->kind < TYPE_KIND_TYPE) {
                if(b->kind > a->kind) {
                    //TODO compiler error messages
                    return builtin_type+TYPE_KIND_VOID;
                }

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind <= TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            } else {
                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            }
            break;   
        case TOKEN_MODEQUAL: case TOKEN_ANDEQUAL: case TOKEN_OREQUAL:
        case TOKEN_LSHIFTEQUAL:
        case TOKEN_RSHIFTEQUAL:
        case TOKEN_XOREQUAL: case TOKEN_TILDEEQUAL:
            if(a->kind < TYPE_KIND_FLOAT && b->kind < TYPE_KIND_FLOAT) {
                if(b->kind > a->kind) {
                    //TODO compiler error messages
                    return builtin_type+TYPE_KIND_VOID;
                }

                if(a->kind >= TYPE_KIND_INT)
                    return a;
                if(b->kind == TYPE_KIND_CHAR)
                    return a;
                if(((a->kind ^ b->kind) & 0x1) == 0) /* NOTE the unsigned types are even, signed are odd */
                    return a;

                //TODO compiler error messages
                return builtin_type+TYPE_KIND_VOID;
            } else {
                //TODO compiler error messages
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

                if(jp->must_const_evaluate && !sym->constant) {
                    //TODO compiler error messages
                    assert("expected constant expression"&&0);
                }

                arrpush(value_stack, sym->value);
                arrpush(type_stack, sym->type);
            } else {
                Value *v = atom_to_value(jp, atom);
                Type *t = atom_to_type(jp, atom);
                arrpush(value_stack, v);
                arrpush(type_stack, t);
            }
        } else if(kind == AST_KIND_expr) {
            AST_expr *node = (AST_expr*)(expr[pos][0]);

            Type *result_type = NULL;
            Value *result_value = NULL;

            if(node->left && node->right) {
                Token op = node->token;

                Type *b_type = arrpop(type_stack);
                Type *a_type = arrpop(type_stack);

                result_type = typecheck_binary(jp, a_type, b_type, op);

                Value *b_value = arrpop(value_stack);
                Value *a_value = arrpop(value_stack);

                //TODO casts
                a_value = typecast(jp, result_type, a_type, a_value);
                b_value = typecast(jp, result_type, b_type, b_value);

                result_value = evaluate_binary(jp, result_type, a_value, b_value, op);
            } else {
                Token op = node->token;

                Type *a_type = arrpop(type_stack);

                result_type = typecheck_unary(jp, a_type, op);

                Value *a_value = arrpop(value_stack);

                a_value = typecast(jp, result_type, a_type, a_value);

                result_value = evaluate_unary(jp, result_type, a_value, op);
            }

            //TODO compiler error messages
            assert("type checking error"&&result_type->kind != TYPE_KIND_VOID);
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
    } else {
        UNIMPLEMENTED;
    }
}

void typecheck_vardecl(Job *jp) {
    assert(jp->step > TYPECHECK_STEP_VARDECL_BEGIN && jp->step < TYPECHECK_STEP_VARDECL_END);
    AST_vardecl *ast = (AST_vardecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_vardecl);

    bool infer_type = (ast->type == NULL);
    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);

    if(infer_type)
        jp->step = TYPECHECK_STEP_VARDECL_INITIALIZE;

    if(jp->step == TYPECHECK_STEP_VARDECL_BIND_TYPE) {
        if(arrlen(jp->expr) == 0)
            linearize_expr(jp, &ast->type);
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        assert(arrlen(jp->value_stack) == 1 && arrlen(jp->type_stack) == 1);

        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        jp->step = TYPECHECK_STEP_VARDECL_INITIALIZE;
    }

    if(jp->step == TYPECHECK_STEP_VARDECL_INITIALIZE) {
        if(arrlen(jp->expr) == 0)
            linearize_expr(jp, &ast->init);
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        assert((arrlen(jp->value_stack) == 2 && arrlen(jp->type_stack) == 2) || (arrlen(jp->value_stack) == 1 && arrlen(jp->type_stack) == 1));

        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        jp->step = TYPECHECK_STEP_VARDECL_END;
    }

    char *name = ast->name;
    Value *init_value = arrpop(jp->value_stack);
    Type *init_type = arrpop(jp->type_stack);
    Type *bind_type = NULL;

    if(!infer_type) {
        assert(arrlen(jp->value_stack) == 1 && arrlen(jp->type_stack) == 1);
        bind_type = arrpop(jp->value_stack)->val.type;

        Type *t = arrpop(jp->type_stack);

        //TODO compiler error messages
        assert(t->kind == TYPE_KIND_TYPE&&"result of type bind expression is not a type");

        t = typecheck_binary(jp, bind_type, init_type, '=');

        //TODO compiler error messages
        assert("assignment of incompatible types"&&t->kind > TYPE_KIND_VOID);
    } else {
        bind_type = init_type;
    }


    if(is_top_level) {
        Sym *ptr = global_scope_lookup(name);

        //TODO compiler error messages
        assert("identifier already exists"&& ptr == NULL);

        Sym sym = {
            .name = name,
            .declared_by = jp->id,
            .constant = ast->constant,
            .type = bind_type,
            .value = init_value,
        };

        global_scope_enter(sym);
    } else {
        UNIMPLEMENTED;
    }
}


char *test_src[] = {
"f := atan2(i * 1.4e3 + 0.3, y + \"this wouldn't pass typechecking lol\", z);\n"
"pointer_to_array_of_int *[12]int;\n"
"i int = 12;\n"
"PI :: 3.14;\n"
"s := \"hello sailor\";\n",

"i int = NUMBER_THIRTEEN;\n"
"NUMBER_THIRTEEN :: 13;\n",

"i int = 12 + 4;\n",
};

void print_sym(Sym sym) {
    printf("name: %s\n", sym.name);
    printf("declared_by: %d\n", sym.declared_by);
    printf("constant: %s\n", sym.constant ? "true" : "false");
    printf("type: %p\n", (void *)sym.type);
    printf("value: %p\n", (void *)sym.value);
}

int main(void) {
    arena_init(&global_scratch_allocator);
    pool_init(&global_sym_allocator, sizeof(Sym));
    pool_init(&global_type_allocator, sizeof(Type));
    pool_init(&global_value_allocator, sizeof(Value));
    job_runner(test_src[2], "not a file");
    for(int i = 0; i < shlen(global_scope); ++i) {
        if(global_scope[i].value) {
            print_sym(global_scope[i].value[0]);
            printf("\n");
        }
    }
    return 0;
}
