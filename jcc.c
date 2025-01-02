#include <raylib.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <limits.h>
#include <unistd.h>
#include "basic.h"
#include "stb_ds.h"
#include "stb_sprintf.h"
#include "pool.h"
#include "arena.h"

#include "lexer.c"
#include "preload.c"


#define PIPE_STAGES                     \
    X(NONE)                             \
    X(PARSE)                            \
    X(TYPECHECK)                        \
    X(IR)                               \

#define JOB_STATES                      \
    X(READY)                            \
    X(WAIT)                             \
    X(ERROR)                            \

#define ASTKINDS                        \
    X(ifstatement)                      \
    X(switchstatement)                  \
    X(casestatement)                    \
    X(whilestatement)                   \
    X(forstatement)                     \
    X(breakstatement)                   \
    X(continuestatement)                \
    X(returnstatement)                  \
    X(usingstatement)                   \
    X(statement)                        \
    X(push_context)                     \
    X(block)                            \
    X(import_directive)                 \
    X(run_directive)                    \
    X(structdecl)                       \
    X(uniondecl)                        \
    X(vardecl)                          \
    X(procdecl)                         \
    X(paramdecl)                        \
    X(retdecl)                          \
    X(expr_list)                        \
    X(member_list)                      \
    X(expr)                             \
    X(param)                            \
    X(atom)                             \
    X(array_literal)                    \
    X(struct_literal)                   \
    X(call)                             \
    X(proctype)                         \

#define OPERATOR_PREC_TABLE             \
    /* indexing */\
    X((Token)'.',                    10)\
    X((Token)'[',                    10)\
    /* multiplicative */\
    X((Token)'*',                     9)\
    X((Token)'/',                     9)\
    X((Token)'%',                     9)\
    /* additive */\
    X((Token)'+',                     8)\
    X((Token)'-',                     8)\
    /* bitwise */\
    X((Token)'^',                     7)\
    X((Token)'&',                     7)\
    X((Token)'|',                     7)\
    X(TOKEN_LSHIFT,                   6)\
    X(TOKEN_RSHIFT,                   6)\
    /* comparison */\
    X((Token)'<',                     5)\
    X((Token)'>',                     5)\
    X(TOKEN_LESSEQUAL,                5)\
    X(TOKEN_GREATEQUAL,               5)\
    X(TOKEN_EXCLAMEQUAL,              5)\
    X(TOKEN_EQUALEQUAL,               5)\
    /* logical */\
    X(TOKEN_AND,                      4)\
    X(TOKEN_OR,                       4)\

#define VALUEKINDS                      \
    X(NIL)                              \
    X(BOOL)                             \
    X(CHAR)                             \
    X(INT)                              \
    X(UINT)                             \
    X(FLOAT)                            \
    X(DFLOAT)                           \
    X(TYPE)                             \
    X(TYPEINFO)                         \
    X(STRING)                           \
    X(ARRAY)                            \
    X(STRUCT)                           \
    X(PARAM)                            \
    X(TOKEN)                            \
    X(POINTER)                          \
    X(PROC)                             \

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
    X(STRING)                           \
    X(ARRAY)                            \
    X(DYNAMIC_ARRAY)                    \
    X(ARRAY_VIEW)                       \
    X(POINTER)                          \
    X(STRUCT)                           \
    X(UNION)                            \
    X(ENUM)                             \
    X(PROC)                             \
    X(WILDCARD)                         \

#define MESSAGEKINDS                    \
    X(ERROR)                            \
    X(WARNING)                          \
    X(INFO)                             \

#define IR_INT_BINOPS                   \
    X(ADD,     +)                       \
    X(SUB,     -)                       \
    X(MUL,     *)                       \
    X(DIV,     /)                       \
    X(MOD,     %)                       \
    X(AND,     &)                       \
    X(OR,      |)                       \
    X(XOR,     ^)                       \
    X(LSHIFT, <<)                       \
    X(RSHIFT, >>)                       \
    X(EQ,     ==)                       \
    X(NE,     !=)                       \
    X(LE,     <=)                       \
    X(GT,      >)                       \

#define IR_INT_UNOPS                    \
    X(NOT, ~)                           \
    X(NEG, -)                           \

#define IR_FLOAT_BINOPS                 \
    X(FADD,     +)                      \
    X(FSUB,     -)                      \
    X(FMUL,     *)                      \
    X(FDIV,     /)                      \

#define IR_FLOAT_CMPOPS                 \
    X(FEQ,     ==)                      \
    X(FNE,     !=)                      \
    X(FLE,     <=)                      \
    X(FGT,      >)                      \

#define IR_FLOAT_UNOPS                  \
    X(FNEG, -)                          \

#define IROPCODES                         \
    X(NOOP,            _)                 \
    IR_INT_BINOPS                         \
    IR_INT_UNOPS                          \
    IR_FLOAT_BINOPS                       \
    IR_FLOAT_CMPOPS                       \
    IR_FLOAT_UNOPS                        \
    X(IF,              _)                 \
    X(IFZ,             _)                 \
    X(JMP,             _)                 \
    X(CALL,            _)                 \
    X(RET,             _)                 \
    X(LABEL,           _)                 \
    X(LOAD,            _)                 \
    X(STOR,            _)                 \
    X(LOADF,           _)                 \
    X(STORF,           _)                 \
    X(CALCPTROFFSET,   _)                 \
    X(ADDRVAR,         _)                 \
    X(GETVAR,          _)                 \
    X(SETVAR,          _)                 \
    X(SETARG,          _)                 \
    X(SETRET,          _)                 \
    X(GETARG,          _)                 \
    X(GETRET,          _)                 \
    X(GETVARF,         _)                 \
    X(SETVARF,         _)                 \
    X(SETARGF,         _)                 \
    X(SETRETF,         _)                 \
    X(GETARGF,         _)                 \
    X(GETRETF,         _)                 \
    X(ITOF,            _)                 \
    X(FTOB,            _)                 \
    X(ITOB,            _)                 \
    X(FTOI,            _)                 \
    X(ITOI,            _)                 \
    X(FTOF,            _)                 \
    X(GETCONTEXTARG,   _)                 \
    X(SETCONTEXTARG,   _)                 \
    X(HINT_BEGIN_FOREIGN_CALL, _)         \
    X(HINT_END_FOREIGN_CALL,   _)         \
    X(HINT_BEGIN_CALL, _)                 \
    X(HINT_END_CALL,   _)                 \
    X(HINT_BEGIN_PASS_NON_SCALAR, _)      \
    X(HINT_END_PASS_NON_SCALAR,   _)      \

#define IRSEGMENTS                    \
    X(LOCAL,    "local_segment")      \
    X(GLOBAL,   "global_segment")     \
    X(BSS,      "bss_segment")        \
    X(TYPE,     "type_segment")       \
    X(STRING,   "string_segment")     \
    X(CODE,     "code_segment")       \

#define PLATFORMS    \
    X(X64)           \
    X(ARM64)         \

#define AST_KIND_IS_RECORD(kind) ((bool)(kind == AST_KIND_structdecl || kind == AST_KIND_uniondecl))

#define AST_KIND_IS_AGGREGATE_LITERAL(kind) ((bool)(kind == AST_KIND_array_literal || kind == AST_KIND_struct_literal))

#define TOKEN_TO_TYPEKIND(t) (Typekind)((t-TOKEN_VOID)+TYPE_KIND_VOID)

#define TYPE_KIND_IS_SIGNED_INT(kind) ((bool)(kind == TYPE_KIND_INT || kind == TYPE_KIND_S8 || kind == TYPE_KIND_S16 || kind == TYPE_KIND_S32 || kind == TYPE_KIND_S64))

#define TYPE_KIND_IS_UNSIGNED_INT(kind) ((bool)(kind == TYPE_KIND_U8 || kind == TYPE_KIND_U16 || kind == TYPE_KIND_U32 || kind == TYPE_KIND_U64))

#define TYPE_KIND_IS_NOT_SCALAR(kind) ((bool)(kind >= TYPE_KIND_TYPE && kind != TYPE_KIND_POINTER))

#define TYPE_KIND_IS_SCALAR(kind) ((bool)(kind < TYPE_KIND_TYPE || kind == TYPE_KIND_POINTER))

#define TYPE_KIND_IS_RECORD(kind) ((bool)(kind == TYPE_KIND_STRUCT || kind == TYPE_KIND_UNION))

#define TYPE_KIND_IS_RECORD_OR_ARRAY(kind) ((bool)(TYPE_KIND_IS_RECORD(kind) || kind == TYPE_KIND_ARRAY))

#define TYPE_KIND_IS_FLOAT(kind) ((bool)(kind >= TYPE_KIND_FLOAT && kind <= TYPE_KIND_F64))

#define TYPE_KIND_IS_FLOAT32(kind) ((bool)(kind == TYPE_KIND_FLOAT || kind == TYPE_KIND_F32))

#define TYPE_KIND_IS_INTEGER(kind) ((bool)(kind >= TYPE_KIND_BOOL && kind <= TYPE_KIND_INT))

#define TYPE_KIND_IS_INTEGER_OR_FLOAT(kind) ((bool)(TYPE_KIND_IS_INTEGER(kind) || TYPE_KIND_IS_FLOAT(kind)))

#define TYPE_KIND_IS_ARRAY_LIKE(kind) ((bool)(kind >= TYPE_KIND_ARRAY && kind <= TYPE_KIND_ARRAY_VIEW))

#define TYPE_KIND_IS_VIEW_LIKE(kind) ((bool)(kind == TYPE_KIND_ARRAY_VIEW || kind == TYPE_KIND_STRING))

#define TYPE_IS_VOID_POINTER(t) ((bool)((assert(t), 1) && t->kind == TYPE_KIND_POINTER && (assert(t->pointer.to), 1) && t->pointer.to->kind == TYPE_KIND_VOID))

#define TOKEN_IS_BITWISE_OP(token) ((bool)(token == '|' || token == TOKEN_LSHIFT || token == TOKEN_RSHIFT || token == '&' || token == '^' || token == '~'))

#define IROP_IS_INT_ARITH(op) ((bool)(op >= IROP_ADD && op <= IROP_NEG))


typedef struct Scope_entry* Scope;
typedef int    Jobid;
typedef struct Value Value;
typedef struct Sym Sym;
typedef struct Type Type;
typedef struct IRlabel IRlabel;
typedef struct IRinst IRinst;
typedef union  IRvalue IRvalue;
typedef struct IRmachine IRmachine;
typedef void (*IR_foreign_proc)(IRmachine *);
typedef struct IRproc IRproc;
typedef struct Job Job;
typedef struct Job_memory Job_memory;
typedef struct Message Message;
typedef struct AST AST;
typedef struct AST_expr_base AST_expr_base;
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

typedef enum IRop {
    IROP_INVALID = -1,
#define X(x, _) IROP_##x,
    IROPCODES
#undef X
} IRop;

typedef enum IRsegment {
    IRSEG_INVALID = -1,
#define X(x,_) IRSEG_##x,
    IRSEGMENTS
#undef X
} IRsegment;

typedef enum Platform {
    PLATFORM_INVALID = -1,
#define X(x) PLATFORM_##x,
    PLATFORMS
#undef X
} Platform;

char *IRop_debug[] = {
#define X(x, _) #x,
    IROPCODES
#undef X
};

char *IRsegment_debug[] = {
#define X(x,_) #x,
    IRSEGMENTS
#undef X
};

char *ASTkind_debug[] = {
#define X(x) #x,
    ASTKINDS
#undef X
};

union IRvalue {
    u64 integer;
    f32 floating32;
    f64 floating64;
};

struct IRlabel {
    char *key;
    Token keyword;
    u64 continue_label;
    u64 break_label;
    Loc_info loc;
};

#define IRMACHINE_BODY                \
    X(Arr(u64) pc_stack;)             \
    X(Arr(int) procid_stack;)         \
    X(Arr(u64*) jump_table_stack;)    \
    \
    X(u8 *global_segment;)            \
    X(u8 *bss_segment;)               \
    X(u8 *local_segment;)             \
    \
    X(u64 global_segment_size;)       \
    X(u64 bss_segment_size;)          \
    X(u64 local_segment_size;)        \
    \
    X(u64 iregs[8];)                  \
    X(f32 f32regs[8];)                \
    X(f64 f64regs[8];)                \
    \
    X(Arr(IRvalue) ports;)            \
    X(u64 context_pointer;)           \

struct IRmachine {
#define X(x) x
    IRMACHINE_BODY
#undef X
};

const u64 IR_LOCAL_SEGMENT_BYTES = 1<<16;

struct IRinst {
    IRop opcode;
    union {
        union {
            Type *proc_type;
        } hint;

        struct {
            u64 operand_bytes[3];
            u64 reg[3];
            IRvalue imm;
            bool immediate;
            bool sign;
        } arith;

        struct {
            u64 cond_reg;
            u64 label_id;
        } branch;

        struct {
            u64 reg_dest;
        } getcontextarg;

        struct {
            u64 reg_src;
        } setcontextarg;

        struct {
            u64 id_reg;
            u64 id_imm;
            char *name;
            bool c_call;
            bool immediate;
        } call;

        struct {
            bool c_call;
        } ret;

        struct {
            u64 id;
        } label;

        struct {
            u64 reg_dest;
            u64 reg_src_ptr;
            u64 offset_reg;
            u64 stride;
        } calcptroffset;

        struct {
            u64 reg_dest;
            u64 reg_src_ptr;
            IRvalue imm;
            u64 offset_reg;
            u64 byte_offset_imm;
            u64 bytes;
            bool has_immediate_offset;
            bool has_indirect_offset;
            bool immediate;
        } load;

        struct {
            u64 reg_dest_ptr;
            u64 reg_src;
            IRvalue imm;
            u64 offset_reg;
            u64 byte_offset_imm;
            u64 bytes;
            bool has_immediate_offset;
            bool has_indirect_offset;
            bool immediate;
        } stor;

        struct {
            IRsegment segment;
            u64 offset;
            Sym *sym;

            u64 reg_dest;
        } addrvar;

        struct {
            IRsegment segment;
            u64 offset;
            Sym *sym;

            u64 reg_dest;
            u64 bytes;
        } getvar;

        struct {
            IRsegment segment;
            u64 offset;
            Sym *sym;

            u64 reg_src;
            IRvalue imm;
            u64 bytes;
            bool immediate;
        } setvar;

        struct {
            u64 port;
            u64 bytes;
            u64 reg_src;
            bool c_call;
        } setport;

        struct {
            u64 reg_dest;
            u64 bytes;
            u64 port;
            bool c_call;
        } getport;

        struct {
            u64 to_reg;
            u64 from_reg;
            u64 to_bytes;
            u64 from_bytes;
            bool sign;
        } typeconv;

    };

    Loc_info loc;
};

struct IRproc {
    int procid;
    char *name;
    bool is_foreign;
    bool is_inline;
    bool was_polymorphed;
    union {
        struct {
            IRinst *instructions;
            u64 n_instructions;
            u64 *jump_table;
            u64 local_segment_size;
            char *assembly;
        };
        struct {
            IR_foreign_proc foreign_proc;
            void *wrapper_dll;
        };
    };
};

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
    Arena                *scratch;
    Pool                 *value;
    Pool                 *sym;
    Pool                 *type;

#define X(x) Pool ast_##x;
    ASTKINDS
#undef X
        struct {
            bool scratch : 1;
            bool value : 1;
            bool sym : 1;
            bool type : 1;
            bool ast : 1;
        } active;

};

struct Job {
    Jobid                            id;
    Jobid                            parent_job;

    Pipe_stage                       pipe_stage;
    Job_state                        state;

    char                            *handling_name;
    Sym                             *symbol;

    Lexer                           *lexer;
    int                              parsing_uniondecl;
    int                              parser_encountered_polymorphic_var;

    Pool                            *global_sym_allocator;
    Scope                           *global_scope;

    AST                             *root;
    Arr(AST*)                        tree_pos_stack;

    union {
        Loc_info                     non_returning_path_loc;
        Loc_info                     returning_path_loc;
    };


    /* typechecking */
    char                            *waiting_on_name;
    Jobid                            waiting_on_id;
    Type                            *cur_proc_type;
    Arr(Sym*)                        run_dependencies;

    /* procedure polymorphism */
    Arr(Type*)                       save_polymorphic_proc_param_and_return_types;
    AST                             *save_polymorphic_proc_body;

    /* struct and union */                
    Arr(Type*)                       record_types;

    /* blocks */                                      
    //bool                             sharing_scopes;
    Arr(Scope)                       scopes;

    /* expressions */                                 
    //TODO remove these stacks
    Arr(Value*)                      value_stack;
    Arr(Type*)                       type_stack;
    Arr(AST*)                        expr;
    u64                              expr_pos;

    AST_paramdecl                   *cur_paramdecl;
    AST_retdecl                     *cur_retdecl;

    /* return statement */                
    Arr(AST_expr_list*)              expr_list;
    u64                              expr_list_pos;


    /* code generation */
    Arr(AST_statement*)              defer_list_stack;
    u64                              max_local_offset;
    Arr(u64)                         local_offset;
    IRlabel*                         label_table;
    Arr(u64)                         continue_label;
    Arr(u64)                         break_label;
    u64                              label_alloc;
    Arr(IRinst)                      instructions;
    u64                              reg_alloc;
    u64                              float_reg_alloc;

    Loc_info                         cur_loc;

    IRmachine                        interp;

    u64                              cur_run_local_segment_size;

    Arr(Message)                     messages;

    Job_memory allocator;

    bool                             parser_at_top_level : 1;
    bool                             dont_free_allocators : 1;
    bool                             dont_free_ast_allocators : 1;
    bool                             doing_polymorph : 1;
    bool                             typechecker_encountered_wildcard : 1;

};

struct Sym {
    char *name;
    Loc_info loc;
    Jobid declared_by;
    int procid;
    IRsegment segment;
    u64 segment_offset;
    Type *type;
    //union {
    //};
    Value *value;
    AST *initializer;
    AST *polymorphic_proc_ast;
    Jobid being_polymorphed_by_jobid;
    struct {
        Arr(Type**) key;
        Arr(int) procid;
        u64 n_wildcards;
    } polymorph_cache;
    bool job_encountered_error : 1;
    bool ir_generated : 1;
    bool ready_to_run : 1;
    bool is_system : 1;
    bool is_foreign : 1;
    bool is_global : 1;
    bool is_argument : 1;
    bool is_record_argument : 1;
    bool constant : 1;
    bool is_polymorphic_procedure : 1;
    bool is_polymorphic_var : 1;
    bool is_being_used_in_polymorph : 1;
};

struct AST {
    ASTkind kind;
    u32 weight;
    Loc_info loc;
};

struct AST_import_directive {
    AST base;
    char *path;
    bool just_load_the_source;
};

struct AST_run_directive {
    AST base;
    Type *type_annotation; /* for use in expressions */
    Value *value_annotation;
    Type **type_annotation_list; /* because callables can return multiple values */
    Value **value_annotation_list;
    AST_call *call_to_run;
    int n_types_returned;
};

struct AST_expr_base {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
};

struct AST_expr {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    union {
        u64 dot_offset_annotation;
    };
    Token token;
    AST *left;
    AST *right;
};

struct AST_param {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    char *name;
    AST *value;
    AST_param *next;
    int index;
    bool has_nested_call : 1;
    bool is_vararg : 1;
};

struct AST_call {
    AST base;
    Type *type_annotation; /* for use in expressions */
    Value *value_annotation;
    Type **type_annotation_list; /* because callables can return multiple values */
    Value **value_annotation_list;
    AST *callee;
    AST_param *params;
    int n_types_returned;
    int n_params;
    int first_named_param;
    bool has_named_params : 1;
    bool checked_call : 1;
};

struct AST_array_literal {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    int n_elements;
    AST *type;
    AST_expr_list *elements;
    Arr(AST**) flattened_array_literal;
    Arr(u64) dimensions;
};

struct AST_struct_literal {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    int n_members;
    char *name;
    AST_member_list *members;
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

struct AST_member_list {
    AST base;
    char *name;
    AST *value;
    AST_member_list *next;
};

struct AST_expr_list {
    AST base;
    AST *expr;
    AST_expr_list *next;
};

struct AST_proctype {
    AST base;
    Type *type_annotation;
    Value *value_annotation;
    AST_expr_list *params;
    AST_expr_list *rets;
    int n_params;
    int n_rets;
};

#define AST_RECORD_BODY         \
    AST base;                   \
    AST *next;                  \
    Sym *symbol_annotation;     \
    char *name;                 \
    AST_paramdecl *params;      \
    int first_default_param;    \
    int n_params;               \
    int n_members;              \
    int n_using;                \
    AST *body;                  \
    Type *record_type;          \
    bool has_defaults : 1;      \
    bool checked_params : 1;    \
    bool visited : 1;           \
    bool is_name_spaced : 1;    \

struct AST_structdecl {
    AST_RECORD_BODY
};

struct AST_uniondecl {
    AST_RECORD_BODY
};

struct AST_vardecl {
    AST base;
    AST *next;
    Sym *symbol_annotation;
    char *name;
    AST *type;
    AST *init;
    bool constant : 1;
    bool uninitialized : 1;
    bool checked_type : 1;
    bool checked_init : 1;
};

struct AST_paramdecl {
    AST base;
    AST_paramdecl *next;
    Sym *symbol_annotation;
    char *name;
    AST *type;
    AST *init;
    int index;
    bool checked_type : 1;
    bool checked_init : 1;
    bool vararg : 1;
    bool is_polymorphic : 1;
    bool type_has_wildcard : 1;
};

struct AST_retdecl {
    AST base;
    AST_retdecl *next;
    AST *expr;
    int index;
    bool checked_expr : 1;
    bool type_has_wildcard : 1;
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
    union {
        AST *expr;
        AST *begin_range_expr;
    };
    AST *end_range_expr;
    AST *body;
    union {
        Type *expr_type;
        Type *begin_range_type;
    };
    Type *end_range_type;
    Sym *it_symbol;
    Sym *it_index_symbol;
    Sym *named_it_symbol;
    Sym *named_it_index_symbol;
    u64 checked_begin_range : 1;
    u64 checked_end_range : 1;
    u64 by_pointer : 1;
    u64 reverse_order : 1;
    u64 is_range_for : 1;
    u64 visited : 1;
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

struct AST_usingstatement {
    AST base;
    AST *next;
    char *name;
};

struct AST_returnstatement {
    AST base;
    AST *next;
    AST_expr_list *expr_list;
};

struct AST_push_context {
    AST base;
    AST *next;
    char *context_ident;
    Sym *symbol_annotation;
    AST *down;
    bool visited : 1;
};

struct AST_block {
    AST base;
    AST *next;
    AST *down;
    bool visited : 1;
};

struct AST_statement {
    AST base;
    AST *next;
    Token assign_op;
    AST *left;
    AST *right;
    bool deferred : 1;
    bool checked_left : 1;
    bool checked_right : 1;
    bool dont_push_local_offset : 1;
};

struct AST_procdecl {
    AST base;
    AST *next;
    Sym *symbol_annotation;
    char *name;
    char *foreign_lib_str;
    Type *proc_type;
    AST_paramdecl *params;
    AST_retdecl *rets;
    int first_default_param;
    int n_polymorphic_params;
    int n_wildcards;
    int n_params;
    int n_rets;
    AST *body;
    bool c_call : 1;
    bool must_inline : 1;
    bool is_foreign : 1;
    bool is_system : 1;
    bool is_polymorphic : 1;
    bool polymorphic_params_ready : 1;
    bool ready_to_polymorph : 1;
    bool type_checked_body : 1;
    bool varargs : 1;
    bool has_defaults : 1;
    bool checked_params : 1;
    bool checked_rets : 1;
    bool visited : 1;
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
        String_view str;
        Type *type;
        Type_info *typeinfo;
        int procid;
        /*
           struct {
           IRsegment segment;
           u64 offset;
           } pointer;
           */
        struct {
            Value **elements;
            u64 n;
        } array;
        struct {
            char **member_names;
            Value **members;
            u64 n;
        } record;
    } val;
};

struct Type {
    Typekind kind;
    u64 bytes;
    u64 align;
    union {
        struct {
            char *name;
            Type *matched;
        } wildcard;

        struct { /* struct and union */
            char *name;
            bool has_defaults;
            bool is_nested;
            u64 first_default_param;
            struct {
                Type    **types;
                char    **names;
                Value   **values;
                u64       n;
            } param;
            struct {
                Type    **types;
                char    **names;
                Value   **values;
                u64      *offsets;
                Loc_info *locs;
                u64       i;
                u64       n;
            } member;
            struct {
                Type  **types;
                u64    *offsets;
                u64     i;
                u64     n;
            } use;
            struct {
                Type **types;
                u64   *offsets;
                u64    n;
            } flattened_scalars;
        } record;

        struct { /* proc, macro, func */
            char *name;
            u64 first_default_param;
            Type **wildcards;
            u64 ith_wildcard;
            u64 n_wildcards;
            u64 non_scalar_return_count;
            struct {
                char   *vararg_name;
                Type  **types;
                char  **names;
                Value **values;
                bool   *is_polymorphic;
                bool   *has_wildcard;
                int     n;
            } param;
            struct {
                Type  **types;
                bool   *has_wildcard;
                int     n;
            } ret;
            bool is_polymorphic : 1;
            bool varargs : 1;
            bool has_defaults : 1;
            bool is_foreign : 1;
            bool is_system : 1;
            bool c_call : 1;
        } proc;

        struct { /* static array, dynamic or view */
            u64  n; /* used for capacity of static array */
            u64  element_stride;
            Type *of;
        } array;

        struct {
            Type *to;
        } pointer;
    };
};

/* function headers */
Job         job_spawn(Jobid *jobid_alloc, Pipe_stage stage);
void        job_die(Job *jp);
AST*        job_alloc_ast(Job *jp, ASTkind kind);
Value*      job_alloc_value(Job *jp, Valuekind kind);
Type*       job_alloc_type(Job *jp, Typekind kind);
Sym*        job_alloc_sym(Job *jp);
Sym*        job_alloc_global_sym(Job *jp);
char*       job_alloc_text(Job *jp, char *s, char *e);
void*       job_alloc_scratch(Job *jp, size_t bytes);
char*       job_sprint(Job *jp, char *fmt, ...);

void        job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void        job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]);
bool        job_runner(char *src, char *src_path);
Sym*        job_scope_lookup(Job *jp, char *name);
Sym*        job_current_scope_lookup(Job *jp, char *name);
void        job_scope_enter(Job *jp, Sym *sym);
void        job_report_all_messages(Job *jp);
void        job_report_mutual_dependency(Job *jp1, Job *jp2);
void        job_error(Job *jp, Loc_info loc, char *fmt, ...);
char*       job_type_to_str(Job *jp, Type *t);
char*       job_type_to_ctype_str(Job *jp, Type *t);
IRlabel     job_label_lookup(Job *jp, char *name);
bool        job_label_create(Job *jp, IRlabel label);
char*       job_op_token_to_str(Job *jp, Token op);
Type_info*  job_make_type_info(Job *jp, Type *t);
void        linearize_expr(Job *jp, AST *ast);

bool        is_lvalue(AST *ast);
bool        all_paths_return(Job *jp, AST *ast);
bool        has_nested_call(AST *ast);
void        add_implicit_casts_to_expr(Job *jp, AST *ast);
u64         align_up(u64 offset, u64 align);
u64         next_pow_2(u64 v);
AST*        arena_dup_ast(Arena *a, AST* ast);
AST*        ast_copy(Arena *arena, AST *root);
void        serialize_value(Job *jp, u8 *dest, Value *v, Type *t);
bool        records_have_member_name_conflicts(Job *jp, Loc_info using_loc, Type *a, Type *b);
void        do_run_directive(Job *jp, AST_call *call_to_run, Type *proc_type);
void        copy_array_data_to_value(Job *jp, Value *v, Type *t, u8 *data);
Value*      make_empty_array_value(Job *jp, Type *t);
void        annotate_aggregate_literal(Job *jp, Type *t, AST *ast_lit);

Arr(AST*)   ir_linearize_expr(Arr(AST*) ir_expr, AST *ast);
void        ir_gen(Job *jp);
void        ir_gen_foreign_proc_x64(Job *jp);
void        ir_gen_foreign_proc_arm64(Job *jp); //TODO
Arr(Type*)  ir_gen_call_x64(Job *jp, Arr(Type*) type_stack, AST_call *ast_call);
Arr(Type*)  ir_gen_call_arm64(Job *jp, Arr(Type*) type_stack, AST_call *ast_call); //TODO
void        ir_gen_block(Job *jp, AST *ast);
void        ir_gen_deferred(Job *jp, AST_statement *defer_list);
void        ir_gen_statement(Job *jp, AST_statement *ast_statement);
void        ir_gen_logical_expr(Job *jp, AST *ast);
void        ir_gen_struct_init(Job *jp, Type *struct_type, IRsegment segment, u64 offset);
void        ir_gen_expr(Job *jp, AST *ast);
u64         ir_gen_array_literal(Job *jp, Type *array_type, AST_array_literal *ast_array);
u64         ir_gen_copy_array_literal(Job *jp, Type *array_type, AST_array_literal *ast_array, u64 offset, u64 ptr_reg);
void        ir_gen_memorycopy(Job *jp, u64 bytes, u64 align, u64 to_ptr_reg, u64 from_ptr_reg);
u64         ir_gen_array_from_value(Job *jp, Type *array_type, Value *v);
void        ir_gen_entry_point_preamble(Job *jp);
void        ir_run(Job *jp, int procid);
void        show_ir_loc(IRinst inst);

void        ir_gen_C(Job *jp, IRproc irproc, Type *proc_type);

Value*      atom_to_value(Job *jp, AST_atom *atom);
Type*       atom_to_type(Job *jp, AST_atom *atom);

Value*      evaluate_unary(Job *jp, Value *a, AST_expr *op_ast);
Value*      evaluate_binary(Job *jp, Value *a, Value *b, AST_expr *op_ast);

void        proc_table_add(int procid, IRproc procdata);

bool        types_are_same(Type *a, Type *b);
Type*       typecheck_operation(Job *jp, Type *a, Type *b, Token op, AST **left, AST **right);
Type*       typecheck_dot(Job *jp, Type *a, char *field, u64 *offsetp);
void        typecheck_expr(Job *jp);
void        typecheck_vardecl(Job *jp);
void        typecheck_procdecl(Job *jp);
void        typecheck_polymorphic_procdecl(Job *jp);
void        typecheck_structdecl(Job *jp);

int         getprec(Token t);

//TODO better parser errors
AST*        parse_top_level_statement(Job *jp);
AST*        parse_directive_statement(Job *jp);
AST*        parse_load_or_import_directive(Job *jp);
AST*        parse_run_directive(Job *jp);
AST*        parse_procdecl(Job *jp);
AST*        parse_structdecl(Job *jp);
AST*        parse_anonstruct(Job *jp);
AST*        parse_structblock(Job *jp, int *n_members, int *n_using);
AST*        parse_procblock(Job *jp);
AST*        parse_pushcontext(Job *jp);
AST*        parse_usingstatement(Job *jp);
AST*        parse_statement(Job *jp);
AST*        parse_controlflow(Job *jp);
AST*        parse_vardecl(Job *jp);
AST*        parse_paramdecl(Job *jp);
AST*        parse_expr(Job *jp);
AST*        parse_expr_increase_prec(Job *jp, AST *left, int min_prec);
AST*        parse_expr_decrease_prec(Job *jp, int min_prec);
AST*        parse_prefix(Job *jp);
AST*        parse_postfix(Job *jp);
AST*        parse_term(Job *jp);
AST*        parse_call(Job *jp);
AST*        parse_array_lit(Job *jp);
AST*        parse_struct_lit(Job *jp);
AST*        parse_rvalue(Job *jp);

void*       global_alloc_scratch(size_t bytes);
Sym*        global_scope_lookup(Job *jp, char *name);
void        global_scope_enter(Job *jp, Sym *sym);

void        print_sym(Sym sym);
void        sprint_ir_inst(IRinst inst, FILE *f);
void        print_ir_inst(IRinst inst, FILE *f);
void        print_ir_machine(IRmachine *machine, FILE *f);
void        print_ast_expr(AST *expr, int indent);

/* globals */

Type builtin_type[] = {
    { .kind = TYPE_KIND_VOID,   .bytes = 0, .align = 0 },
    { .kind = TYPE_KIND_BOOL,   .bytes = 1, .align = 1 },
    { .kind = TYPE_KIND_CHAR,   .bytes = 1, .align = 1 },
    { .kind = TYPE_KIND_S8,     .bytes = 1, .align = 1 },
    { .kind = TYPE_KIND_U8,     .bytes = 1, .align = 1 },
    { .kind = TYPE_KIND_S16,    .bytes = 2, .align = 2 },
    { .kind = TYPE_KIND_U16,    .bytes = 2, .align = 2 },
    { .kind = TYPE_KIND_S32,    .bytes = 4, .align = 4 },
    { .kind = TYPE_KIND_U32,    .bytes = 4, .align = 4 },
    { .kind = TYPE_KIND_S64,    .bytes = 8, .align = 8 },
    { .kind = TYPE_KIND_U64,    .bytes = 8, .align = 8 },
    { .kind = TYPE_KIND_INT,    .bytes = 8, .align = 8 },
    { .kind = TYPE_KIND_FLOAT,  .bytes = 4, .align = 4 },
    { .kind = TYPE_KIND_F32,    .bytes = 4, .align = 4 },
    { .kind = TYPE_KIND_F64,    .bytes = 8, .align = 8 },
    { .kind = TYPE_KIND_TYPE,   .bytes = 8, .align = 8 },
    { .kind = TYPE_KIND_STRING, .bytes = sizeof(String_view), .align = _Alignof(String_view) },
};
Type builtin_wildcard_type = { .kind = TYPE_KIND_WILDCARD };

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

size_t ast_kind_size_table[] = {
#define X(x) sizeof(AST_##x),
    ASTKINDS
#undef X
};

Value builtin_value[] = {
    { .kind = VALUE_KIND_NIL, },
};

Arena                      global_scratch_allocator;
Pool                       global_sym_allocator;
Pool                       global_type_allocator;
Pool                       global_value_allocator;
Scope                      global_scope;

Arena                      string_arena;

Arena                      type_info_arena;
Arr(Type_info*)            type_info_table;
Arr(Type*)                 type_info_lookup_type;

u64                        global_segment_offset;
Arr(Sym*)                  global_data_table;
u64                        bss_segment_offset;
Arr(Sym*)                  bss_data_table;

Map(int, IRproc)           proc_table;
int                        procid_alloc;

Platform                   target_platform;

Type *type_String_view;
Type *type_Array_view;
Type *type_Dynamic_array;
Type *type_Any;
Type *type_Context;
Type type_Context_pointer;
Type *type_Temporary_storage;
Type type_Temporary_storage_pointer;

Sym *sym_default_allocator;
Sym *sym_temporary_allocator;

Jobid jobid_alloc = 0;
Arr(Job) job_queue = NULL;
Arr(Job) job_queue_next = NULL;
int job_queue_pos = 0;


/* function declarations */

u64 debug_arrlen(void *a) {
    return ((a) ? (ptrdiff_t) stbds_header(a)->length : 0);
}

Job job_spawn(Jobid *jobid_alloc, Pipe_stage pipe_stage) {
    *jobid_alloc += 1;
    Job job = {
        .id = *jobid_alloc,
        .pipe_stage = pipe_stage,
        .state = JOB_STATE_READY,
    };

    return job;
}

void job_init_allocator_scratch(Job *jp) {
    arena_init_full(jp->allocator.scratch, false, JLIB_ARENA_INITIAL_BLOCK_BYTES);
    jp->allocator.active.scratch = true;
}

void job_init_allocator_value(Job *jp) {
    pool_init(jp->allocator.value, sizeof(Value));
    jp->allocator.active.value = true;
}

void job_init_allocator_sym(Job *jp) {
    pool_init(jp->allocator.sym, sizeof(Sym));
    jp->allocator.active.sym = true;
}

void job_init_allocator_type(Job *jp) {
    pool_init((jp->allocator.type), sizeof(Type));
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
        shfree(jp->scopes[i]);

    arrfree(jp->messages);
    arrfree(jp->record_types);
    arrfree(jp->run_dependencies);
    arrfree(jp->scopes);
    arrfree(jp->tree_pos_stack);
    arrfree(jp->value_stack);
    arrfree(jp->type_stack);
    arrfree(jp->expr);
    arrfree(jp->expr_list);
    arrfree(jp->defer_list_stack);
    arrfree(jp->local_offset);
    arrfree(jp->continue_label);
    arrfree(jp->break_label);
    arrfree(jp->instructions);

    if(jp->state == JOB_STATE_ERROR) {
        if(jp->symbol) {
            jp->symbol->job_encountered_error = true;
        }
    } else {
        if(jp->dont_free_allocators == false) {
            if(jp->allocator.scratch && jp->allocator.scratch != &global_scratch_allocator)
                arena_destroy(jp->allocator.scratch);

            if(jp->allocator.value) pool_destroy(jp->allocator.value);
            if(jp->allocator.sym) pool_destroy(jp->allocator.sym);
            if(jp->allocator.type) pool_destroy(jp->allocator.type);
        }

        if(jp->dont_free_ast_allocators == false) {
#define X(x) pool_destroy(&(jp->allocator.ast_##x));
            ASTKINDS
#undef X
        }
    }

}

INLINE Value* job_alloc_value(Job *jp, Valuekind kind) {
    Value *ptr = pool_alloc(jp->allocator.value);
    ptr->kind = kind;
    return ptr;
}

//NOTE it turns out that caching the types does not significantly reduce memory usage
INLINE Type* job_alloc_type(Job *jp, Typekind kind) {
    if(kind >= TYPE_KIND_VOID && kind <= TYPE_KIND_STRING)
        return builtin_type + kind;
    Type *ptr = pool_alloc(jp->allocator.type);
    ptr->kind = kind;

    switch(kind) {
        default:
            break;
        case TYPE_KIND_PROC:
            ptr->bytes = 8;
            ptr->align = 8;
            break;
        case TYPE_KIND_POINTER:
            ptr->bytes = 8;
            ptr->align = 8;
            break;
        case TYPE_KIND_DYNAMIC_ARRAY:
            ptr->bytes = sizeof(Dynamic_array); // needs to be bigger for allowing an allocator pointer
            ptr->align = _Alignof(Dynamic_array);
            break;
        case TYPE_KIND_ARRAY_VIEW:
            ptr->bytes = sizeof(Array_view);
            ptr->align = _Alignof(Array_view);
            break;
        case TYPE_KIND_ARRAY:
            ptr->bytes = 0;
            break;
    }

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
    if(jp->lexer) ptr->loc = jp->lexer->loc;
    return ptr;
}

void* job_alloc_scratch(Job *jp, size_t bytes) {
    return arena_alloc(jp->allocator.scratch, bytes);
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
    char *ptr = arena_alloc(jp->allocator.scratch, (e - s) + 1);
    char *p = ptr;
    u64 len = (e - s);

    u64 r = 0, w = 0;

    while(r < len) {
        if(s[r] == '\\') {
            switch(s[r+1]) {
                default:
                    break;
                case 'n':
                    r++;
                    p[w++] = '\n';
                    break;
                case 'b':
                    r++;
                    p[w++] = '\b';
                    break;
                case 't':
                    r++;
                    p[w++] = '\t';
                    break;
                case 'v':
                    r++;
                    p[w++] = '\v';
                    break;
                case 'r':
                    r++;
                    p[w++] = '\r';
                    break;
                case '"':
                case '\'':
                case '\\':
                    break;
            }
        } else {
            p[w++] = s[r];
        }

        r++;
    }

    arena_step_back(jp->allocator.scratch, r - w);

    return ptr;
}

INLINE Sym* job_alloc_sym(Job *jp) {
    return pool_alloc(jp->allocator.sym);
}

INLINE Sym* job_alloc_global_sym(Job *jp) {
    return pool_alloc(jp->global_sym_allocator);
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

Sym* job_current_scope_lookup(Job *jp, char *name) {
    Scope scope = arrlast(jp->scopes);
    Sym *symp = shget(scope, name);
    if(symp)
        return symp;
    else
        return NULL;
}

void job_scope_enter(Job *jp, Sym *sym) {
    assert(arrlen(jp->scopes) > 0);
    printf("job %i accessing scope %li\n", jp->id, arrlen(jp->scopes) - 1);
    Scope scope = arrlast(jp->scopes);
    shput(scope, sym->name, sym);
    arrlast(jp->scopes) = scope;
}

INLINE Sym* global_scope_lookup(Job *jp, char *name) {
    return shget(*(jp->global_scope), name);
}

void global_scope_enter(Job *jp, Sym *sym) {
    printf("job %i accessing global scope\n", jp->id);
    jp->symbol = sym;
    sym->is_global = true;
    shput(*(jp->global_scope), sym->name, sym);
}

bool job_label_create(Job *jp, IRlabel label) {
    int i = shgeti(jp->label_table, label.key);
    if(i >= 0) return false;
    shputs(jp->label_table, label);
    return true;
}

IRlabel job_label_lookup(Job *jp, char *name) {
    return shgets(jp->label_table, name);
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

char* arena_vsprint(Arena *a, char *fmt, ...) {
    va_list args;

    size_t n = 64;
    char *buf = arena_alloc(a, n);

    va_start(args, fmt);
    size_t n_written = 1 + stbsp_vsnprintf(buf, n, fmt, args);
    va_end(args);

    while(n_written >= n) {
        arena_step_back(a, n);
        n <<= 1;
        buf = arena_alloc(a, n);
        va_start(args, fmt);
        n_written = 1 + stbsp_vsnprintf(buf, n, fmt, args);
        va_end(args);
    }

    if(n > n_written) {
        arena_step_back(a, n - n_written);
    }

    return buf;
}

#define job_sprint(jp, fmt, ...) ((char*)arena_vsprint(jp->allocator.scratch, fmt, __VA_ARGS__))
#define global_sprint(jp, fmt, ...) ((char*)arena_vsprint(&global_scratch_allocator, fmt, __VA_ARGS__))

void job_error(Job *jp, Loc_info loc, char *fmt, ...) {
    va_list args;

    va_start(args, fmt);

    size_t n = 64;
    char *buf = arena_alloc(jp->allocator.scratch, n);

    size_t n_written = stbsp_vsnprintf(buf, n, fmt, args);

    va_end(args);

    while(n_written >= n) {
        arena_step_back(jp->allocator.scratch, n);
        n <<= 1;
        buf = arena_alloc(jp->allocator.scratch, n);
        va_start(args, fmt);
        n_written = stbsp_vsnprintf(buf, n, fmt, args);
        va_end(args);
    }

    arena_step_back(jp->allocator.scratch, n - n_written - 1);

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

Type_info* job_make_type_info(Job *jp, Type *t) {
    int i = 0;
    for(; i < arrlen(type_info_lookup_type); ++i) {
        if(types_are_same(t, type_info_lookup_type[i])) {
            fprintf(stderr,"been made before\n");
            break;
        }
    }

    if(i < arrlen(type_info_lookup_type)) {
        return type_info_table[i];
    }

    arrpush(type_info_lookup_type, t);

    Type_info *tinfo = NULL;

    bool written_to_table = false;

    switch(t->kind) {
        default:
            UNREACHABLE;
        case TYPE_KIND_BOOL:
            tinfo = arena_alloc(&type_info_arena, sizeof(Type_info));
            tinfo->tag = TYPE_INFO_TAG_BOOL;
            break;
        case TYPE_KIND_VOID:
            tinfo = arena_alloc(&type_info_arena, sizeof(Type_info));
            tinfo->tag = TYPE_INFO_TAG_VOID;
            break;
        case TYPE_KIND_TYPE:
            tinfo = arena_alloc(&type_info_arena, sizeof(Type_info));
            tinfo->tag = TYPE_INFO_TAG_TYPE;
            break;
        case TYPE_KIND_STRING:
            tinfo = arena_alloc(&type_info_arena, sizeof(Type_info));
            tinfo->tag = TYPE_INFO_TAG_STRING;
            break;
        case TYPE_KIND_CHAR:
            tinfo = arena_alloc(&type_info_arena, sizeof(Type_info));
            tinfo->tag = TYPE_INFO_TAG_CHAR;
            break;
        case TYPE_KIND_U8:
        case TYPE_KIND_U16:
        case TYPE_KIND_U32:
        case TYPE_KIND_U64:
            {
                Type_info_int *tinfo_int = arena_alloc(&type_info_arena, sizeof(Type_info_int));
                tinfo_int->base.tag = TYPE_INFO_TAG_INT;
                tinfo_int->bits = builtin_type[t->kind].bytes << 3;
                tinfo_int->sign = false;
                tinfo = (Type_info*)tinfo_int;
            }
            break;
        case TYPE_KIND_S8:
        case TYPE_KIND_S16:
        case TYPE_KIND_S32:
        case TYPE_KIND_S64:
        case TYPE_KIND_INT:
            {
                fprintf(stderr,"here making type info\n");
                Type_info_int *tinfo_int = arena_alloc(&type_info_arena, sizeof(Type_info_int));
                tinfo_int->base.tag = TYPE_INFO_TAG_INT;
                tinfo_int->bits = builtin_type[t->kind].bytes << 3lu;
                tinfo_int->sign = true;
                tinfo = (Type_info*)tinfo_int;
            }
            break;
        case TYPE_KIND_FLOAT:
        case TYPE_KIND_F32:
        case TYPE_KIND_F64:
            {
                Type_info_float *tinfo_float = arena_alloc(&type_info_arena, sizeof(Type_info_float));
                tinfo_float->base.tag = TYPE_INFO_TAG_FLOAT;
                tinfo_float->bits = builtin_type[t->kind].bytes << 3;
                tinfo = (Type_info*)tinfo_float;
            }
            break;
        case TYPE_KIND_ARRAY:
        case TYPE_KIND_DYNAMIC_ARRAY:
        case TYPE_KIND_ARRAY_VIEW:
            {
                Type_info_array *tinfo_array = arena_alloc(&type_info_arena, sizeof(Type_info_array));
                tinfo = (Type_info*)tinfo_array;
                written_to_table = true;
                arrpush(type_info_table, tinfo);

                tinfo_array->base.tag = TYPE_INFO_TAG_ARRAY;
                tinfo_array->array_of = job_make_type_info(jp, t->array.of);
                tinfo_array->array_count = t->array.n;
                if(t->kind == TYPE_KIND_ARRAY) {
                    tinfo_array->array_kind = 0;
                } else if(t->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                    tinfo_array->array_kind = 1;
                } else {
                    assert(t->kind == TYPE_KIND_ARRAY_VIEW);
                    tinfo_array->array_kind = 2;
                }
            }
            break;
        case TYPE_KIND_POINTER:
            {
                Type_info_pointer *tinfo_pointer = arena_alloc(&type_info_arena, sizeof(Type_info_pointer));
                tinfo = (Type_info*)tinfo_pointer;
                written_to_table = true;
                arrpush(type_info_table, tinfo);

                tinfo_pointer->base.tag = TYPE_INFO_TAG_POINTER;
                tinfo_pointer->pointer_to = job_make_type_info(jp, t->pointer.to);
            }
            break;
        case TYPE_KIND_STRUCT:
        case TYPE_KIND_UNION:
            {
                Type_info_struct *tinfo_struct = arena_alloc(&type_info_arena, sizeof(Type_info_struct));
                tinfo_struct->base.tag = TYPE_INFO_TAG_STRUCT;
                written_to_table = true;
                arrpush(type_info_table, tinfo);

                String_view s = {0};

                if(t->record.name) {
                    s.len = strlen(t->record.name);
                    s.data = memcpy(arena_alloc(&string_arena, s.len), t->record.name, s.len);
                }
                tinfo_struct->name = s;

                u64 members_count = t->record.member.n;

                for(u64 i = 0; i < t->record.use.n; ++i) {
                    Type *use_t = t->record.use.types[i];
                    members_count += use_t->record.member.n;
                }

                tinfo_struct->members.count = members_count;

                Type_info_struct_member *tinfo_struct_members = arena_alloc(&type_info_arena, sizeof(Type_info_struct_member) * members_count);

                tinfo_struct->members.data = (void*)tinfo_struct_members;

                int member_index = 0;

                for(u64 i = 0; i < t->record.use.n; ++i) {
                    Type *use_t = t->record.use.types[i];
                    for(u64 j = 0; j < use_t->record.member.n; ++j) {

                        s.len = strlen(use_t->record.member.names[j]);
                        s.data = memcpy(arena_alloc(&string_arena, s.len), use_t->record.member.names[j], s.len);
                        tinfo_struct_members[member_index].name = s;
                        tinfo_struct_members[member_index].type = job_make_type_info(jp, use_t->record.member.types[j]);
                        tinfo_struct_members[member_index].offset = use_t->record.member.offsets[j] + t->record.use.offsets[i];

                        member_index++;
                    }
                }

                for(u64 i = 0; i < t->record.member.n; ++i) {
                    s.len = strlen(t->record.member.names[i]);
                    s.data = memcpy(arena_alloc(&string_arena, s.len), t->record.member.names[i], s.len);
                    tinfo_struct_members[member_index].name = s;
                    tinfo_struct_members[member_index].type = job_make_type_info(jp, t->record.member.types[i]);
                    tinfo_struct_members[member_index].offset = t->record.member.offsets[i];

                    member_index++;
                }

                assert(member_index == members_count);

                tinfo = (Type_info*)tinfo_struct;
            }
            break;
        case TYPE_KIND_PROC:
            UNIMPLEMENTED;
            break;
        case TYPE_KIND_ENUM:
            UNIMPLEMENTED;
            break;
    }

    tinfo->bytes = t->bytes;
    tinfo->align = t->align;

    if(!written_to_table)
        arrpush(type_info_table, tinfo);

    return tinfo;
}

//TODO rewrite _type_to_str()
Arr(char) _type_to_str(Type *t, Arr(char) tmp_buf) {
    if(t->kind >= TYPE_KIND_VOID && t->kind <= TYPE_KIND_TYPE) {
        char *tstr = builtin_type_to_str[t->kind];
        char *p = arraddnptr(tmp_buf, strlen(tstr));
        while(*tstr) {
            *p = *tstr;
            ++p;
            ++tstr;
        }
    } else if(t->kind == TYPE_KIND_STRING) {
        char *tstr = "string";
        char *p = arraddnptr(tmp_buf, strlen(tstr));
        while(*tstr) {
            *p = *tstr;
            ++p;
            ++tstr;
        }
    } else if(TYPE_KIND_IS_ARRAY_LIKE(t->kind)) {
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
        if(t->proc.param.n == 0) {
            char *p = arraddnptr(tmp_buf, token_keyword_lengths[TOKEN_VOID - TOKEN_INVALID - 1] + 2);
            char cpy[] = "void) ";
            for(int i = 0; i < STRLEN(cpy); ++i) p[i] = cpy[i]; 
        } else {
            for(u64 i = 0; i < t->proc.param.n; ++i) {
                Type *param_type = t->proc.param.types[i];
                tmp_buf = _type_to_str(param_type, tmp_buf);
                arrpush(tmp_buf, ',');
                arrpush(tmp_buf, ' ');
            }

            if(t->proc.varargs) {
                //for(int i = 0; t->proc.param.vararg_name[i]; ++i)
                //    arrpush(tmp_buf, t->proc.param.vararg_name[i]);
                //arrpush(tmp_buf, ':');
                //arrpush(tmp_buf, ' ');
                arrpush(tmp_buf, '.');
                arrpush(tmp_buf, '.');
                arrpush(tmp_buf, ')');
                arrpush(tmp_buf, ' ');
            } else {
                tmp_buf[arrlen(tmp_buf) - 2] = ')';
            }
        }

        arrpush(tmp_buf, '-');
        arrpush(tmp_buf, '>');
        arrpush(tmp_buf, ' ');

        if(t->proc.ret.n == 0) {
            char *p = arraddnptr(tmp_buf, 4);
            stbsp_sprintf(p, "void");
        } else {
            for(u64 i = 0; i < t->proc.ret.n; ++i) {
                Type *ret_type = t->proc.ret.types[i];
                tmp_buf = _type_to_str(ret_type, tmp_buf);
                arrpush(tmp_buf, ',');
                arrpush(tmp_buf, ' ');
            }

            arrsetlen(tmp_buf, arrlen(tmp_buf) - 2);
        }
    } else if(TYPE_KIND_IS_RECORD(t->kind)) {
        if(!t->record.name) {
            char *s = "anonymous struct";
            char *p = arraddnptr(tmp_buf, strlen(s));
            strcpy(p, s);
        } else {
            char *p = arraddnptr(tmp_buf, strlen(t->record.name));
            strcpy(p, t->record.name);
        }
    } else {
        UNIMPLEMENTED;
    }

    return tmp_buf;
}

Arr(char) _ctype_to_str(Type *t, Arr(char) tmp_buf) {
    if(t->kind >= TYPE_KIND_VOID && t->kind <= TYPE_KIND_TYPE) {
        char *tstr = builtin_type_to_str[t->kind];
        char *p = arraddnptr(tmp_buf, strlen(tstr));
        while(*tstr) {
            *p = *tstr;
            ++p;
            ++tstr;
        }
        //} else if(t->kind >= TYPE_KIND_ARRAY && t->kind <= TYPE_KIND_ARRAY_VIEW) {
        //    arrpush(tmp_buf, '[');
        //    if(t->kind == TYPE_KIND_ARRAY) {
        //        u64 cur_len = arrlen(tmp_buf);
        //        char *p = arraddnptr(tmp_buf, 21);
        //        u64 n = stbsp_sprintf(p, "%lu", t->array.n);
        //        arrsetlen(tmp_buf, cur_len + n);
        //    } else if(t->kind == TYPE_KIND_DYNAMIC_ARRAY) {
        //        arrpush(tmp_buf, '.');
        //        arrpush(tmp_buf, '.');
        //    }
        //    arrpush(tmp_buf, ']');
        //    tmp_buf = _type_to_str(t->array.of, tmp_buf);
} else if(t->kind == TYPE_KIND_POINTER) {
    tmp_buf = _type_to_str(t->pointer.to, tmp_buf);
    arrpush(tmp_buf, '*');
} else if(TYPE_KIND_IS_RECORD(t->kind)) {
    UNIMPLEMENTED;
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

char* job_type_to_ctype_str(Job *jp, Type *t) {
    Arr(char) tmp_buf = NULL;
    arrsetcap(tmp_buf, 256);
    tmp_buf = _ctype_to_str(t, tmp_buf);
    arrpush(tmp_buf, 0);
    char *s = job_alloc_scratch(jp, arrlen(tmp_buf));
    memcpy(s, tmp_buf, arrlen(tmp_buf));
    arrfree(tmp_buf);
    return s;
}

INLINE char* job_op_token_to_str(Job *jp, Token op) {
    assert(op != TOKEN_INVALID);

    char *s = NULL;

    if(op > TOKEN_INVALID) {
        int n = token_keyword_lengths[op - TOKEN_INVALID - 1];
        s = job_alloc_scratch(jp, n + 1);
        int i = 0;
        while(i < n) {
            s[i] = token_keywords[op - TOKEN_INVALID - 1][i];
            i++;
        }
        s[i] = 0;
    } else {
        s = job_alloc_scratch(jp, 1);
        *s = (char)op;
    }

    return s;
}

INLINE void* global_alloc_scratch(size_t bytes) {
    return arena_alloc(&global_scratch_allocator, bytes);
}

INLINE bool is_lvalue(AST *ast) {
    if(ast == NULL) return false;
    if(ast->kind == AST_KIND_atom) {
        AST_atom *atom = (AST_atom*)ast;
        if(atom->token == TOKEN_IDENT)
            return true;
        if(atom->token == TOKEN_CONTEXT)
            return true;

        return false;
    } else if(ast->kind == AST_KIND_expr) {
        AST_expr *expr = (AST_expr*)ast;
        if(expr->token == '[') return true;
        if(expr->token == '.') return true;
        if(expr->token == '>' && expr->left == NULL) return true;
        return false;
    } else {
        return false;
    }
}

bool has_nested_call(AST *ast) {
    if(!ast) return false;

    switch(ast->kind) {
        case AST_KIND_atom:
            break;
        case AST_KIND_expr:
            {
                AST_expr *ast_expr = (AST_expr*)ast;
                if(has_nested_call(ast_expr->left)) return true;
                if(has_nested_call(ast_expr->right)) return true;
                break;
            }
            break;
        case AST_KIND_array_literal:
            {
                AST_array_literal *ast_array = (AST_array_literal*)ast;
                for(AST_expr_list *expr_list = ast_array->elements; expr_list; expr_list = expr_list->next) {
                    if(has_nested_call(expr_list->expr))
                        return true;
                }
            }
            break;
        case AST_KIND_call:
            return true;
    }

    return false;
}

//TODO all implicit casts should happen in typecheck_operation()
void add_implicit_casts_to_expr(Job *jp, AST *ast) {
    if(!ast) return;

    if(ast->kind == AST_KIND_expr) {
        AST_expr *expr = (AST_expr*)ast;

        if(expr->left == NULL) return;
        if(expr->right == NULL) return;

        if(expr->left->kind != AST_KIND_atom) add_implicit_casts_to_expr(jp, expr->left);
        if(expr->right->kind != AST_KIND_atom) add_implicit_casts_to_expr(jp, expr->right);

        Type *left = ((AST_expr_base*)(expr->left))->type_annotation;
        Type *right = ((AST_expr_base*)(expr->right))->type_annotation;

        if(left == NULL) return;
        if(right == NULL) return;

        if(TYPE_KIND_IS_FLOAT(left->kind) && !TYPE_KIND_IS_FLOAT(right->kind)) {
            assert(right->kind <= TYPE_KIND_INT);
            AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            cast_expr->token = TOKEN_CAST;
            cast_expr->left = NULL;
            cast_expr->type_annotation = left;
            cast_expr->right = expr->right;
            expr->right = (AST*)cast_expr;
        } else if(left->kind == TYPE_KIND_FLOAT && right->kind == TYPE_KIND_F64) {
            AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            cast_expr->token = TOKEN_CAST;
            cast_expr->left = NULL;
            cast_expr->type_annotation = right;
            cast_expr->right = expr->right;
            expr->left = (AST*)cast_expr;
        } else if(left->kind == TYPE_KIND_F64 && right->kind == TYPE_KIND_FLOAT) {
            AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            cast_expr->token = TOKEN_CAST;
            cast_expr->left = NULL;
            cast_expr->type_annotation = left;
            cast_expr->right = expr->right;
            expr->right = (AST*)cast_expr;
        } else if(!TYPE_KIND_IS_FLOAT(left->kind) && TYPE_KIND_IS_FLOAT(right->kind)) {
            assert(left->kind <= TYPE_KIND_INT);
            AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            cast_expr->token = TOKEN_CAST;
            cast_expr->left = NULL;
            cast_expr->type_annotation = right;
            cast_expr->right = expr->left;
            expr->left = (AST*)cast_expr;
            //} else if(left->kind == TYPE_KIND_INT && TYPE_KIND_IS_INTEGER(right->kind) && right->kind != TYPE_KIND_INT) {
            //    AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            //    cast_expr->token = TOKEN_CAST;
            //    cast_expr->left = NULL;
            //    cast_expr->type_annotation = right;
            //    cast_expr->right = expr->left;
            //    expr->left = (AST*)cast_expr;
            //} else if(TYPE_KIND_IS_INTEGER(left->kind) && left->kind != TYPE_KIND_INT && right->kind == TYPE_KIND_INT) {
            //    AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            //    cast_expr->token = TOKEN_CAST;
            //    cast_expr->left = NULL;
            //    cast_expr->type_annotation = left;
            //    cast_expr->right = expr->right;
            //    expr->left = (AST*)cast_expr;
    }
    } else if(ast->kind == AST_KIND_call || ast->kind == AST_KIND_run_directive) {
        AST_call *callp;
        if(ast->kind == AST_KIND_run_directive) {
            callp = (AST_call*)(((AST_run_directive*)ast)->call_to_run);
        } else {
            callp = (AST_call*)ast;
        }

        for(AST_param *p = callp->params; p; p = p->next) {
            add_implicit_casts_to_expr(jp, p->value);
        }
    } else {
        return;
    }

    //} else if(ast->kind == AST_KIND_array_literal) {
    //    PASS;
    //} else if(ast->kind == AST_KIND_param) {
    //    PASS;
    //} else if(ast->kind == AST_KIND_call || ast->kind == AST_KIND_run_directive) {
    //    PASS;
    //} else {
    //    print_ast_expr(ast, 0);
    //    UNREACHABLE;
    //}
}

void serialize_value(Job *jp, u8 *dest, Value *v, Type *t) {
    assert(v && t && dest);

    if(TYPE_KIND_IS_NOT_SCALAR(t->kind)) {
        if(TYPE_KIND_IS_RECORD(t->kind)) {
            UNIMPLEMENTED;
        } else if(TYPE_KIND_IS_ARRAY_LIKE(t->kind)) {
            if(t->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                UNIMPLEMENTED;
            } else if(t->kind == TYPE_KIND_ARRAY) {
                assert(v->kind == VALUE_KIND_ARRAY);

                u64 stride = t->array.of->bytes;

                if(v->val.array.n < t->array.n) {
                    memset(dest + v->val.array.n * stride, 0, (t->array.n - v->val.array.n) * stride);
                }

                for(u64 i = 0; i < v->val.array.n; ++i) {
                    serialize_value(jp, dest + i * stride, v->val.array.elements[i], t->array.of);
                }
            } else {
                assert(t->kind == TYPE_KIND_ARRAY_VIEW);
                UNIMPLEMENTED;
            }
        } else {
            UNIMPLEMENTED;
        }
    } else {
        if(t->kind == TYPE_KIND_POINTER) { 
            UNIMPLEMENTED;
        } else {
            memcpy(dest, (void*)&(v->val), t->bytes);
        }
    }
}

bool all_paths_return(Job *jp, AST *ast) {
    if(ast->kind == AST_KIND_block) {
        AST_block *block = (AST_block*)ast;
        ast = block->down;
    }

    AST_statement *ast_statement = (AST_statement*)ast;

    bool cannot_end_block;

    AST *prev = NULL;

    while(ast_statement) {
        cannot_end_block = false;

        switch(ast_statement->base.kind) {
            default:
                UNREACHABLE;
            case AST_KIND_vardecl:
            case AST_KIND_breakstatement:
            case AST_KIND_continuestatement:
            case AST_KIND_statement:
                cannot_end_block = true;
                break;
            case AST_KIND_switchstatement:
                UNIMPLEMENTED;
            case AST_KIND_ifstatement:
                {
                    AST_ifstatement *ast_if = (AST_ifstatement*)ast_statement;
                    if(!all_paths_return(jp, ast_if->body)) {
                        cannot_end_block = true;
                    } else {
                        AST *branch = ast_if->branch;
                        while(branch && branch->kind == AST_KIND_ifstatement) {
                            AST_ifstatement *ast_else_if = (AST_ifstatement*)branch;
                            bool branch_returns = all_paths_return(jp, ast_else_if->body);
                            if(branch_returns && ast_if->next == NULL) return true;
                            if(!branch_returns && ast_if->next == NULL) {
                                return false;
                            }
                            branch = ast_else_if->branch;
                        }

                        if(branch) {
                            bool else_returns = all_paths_return(jp, branch);
                            if(else_returns && ast_if->next == NULL) return true;
                            if(!else_returns && ast_if->next == NULL) {
                                return false;
                            }
                            if(!else_returns) jp->non_returning_path_loc = (Loc_info){0};
                        }
                    }
                }
                break;
            case AST_KIND_whilestatement:
                {
                    AST_whilestatement *ast_while = (AST_whilestatement*)ast_statement;
                    if(!all_paths_return(jp, ast_while->body)) {
                        cannot_end_block = true;
                    }
                }
                break;
            case AST_KIND_forstatement:
                {
                    AST_forstatement *ast_for = (AST_forstatement*)ast_statement;
                    if(!all_paths_return(jp, ast_for->body)) {
                        cannot_end_block = true;
                    }
                }
                break;
            case AST_KIND_returnstatement:
                if(ast_statement->next != NULL) {
                    job_error(jp, ast_statement->next->loc, "unreachable code");
                    break;
                }
                break;
            case AST_KIND_block:
                {
                    AST_block *ast_block = (AST_block*)ast_statement;
                    if(!all_paths_return(jp, ast_block->down)) {
                        cannot_end_block = true;
                    }
                }
                break;
        }

        prev = (AST*)ast_statement;
        ast_statement = (AST_statement*)(ast_statement->next);
    }

    if(cannot_end_block) {
        jp->non_returning_path_loc = prev->loc;
        return false;
    }

    return true;
}

//TODO is annotate_aggregate_literal() really necessary?
//void annotate_aggregate_literal(Job *jp, Type *t, AST *ast_lit) {
//    if(ast_list->kind == AST_KIND_struct_literal) {
//        UNIMPLEMENTED;
//
//        assert(TYPE_KIND_IS_RECORD(t->kind));
//
//        AST_struct_literal *struct_lit = (AST_struct_literal*)ast_lit;
//
//        struct_lit->type_annotation = t;
//
//        for(AST_member_list *members = struct_lit->members; members; members = members->next) {
//            if(AST_KIND_IS_AGGREGATE_LITERAL(members->value->kind)) {
//                annotate_aggregate_literal(jp, t->record.member.types, elements->expr);
//            }
//        }
//
//    } else if(ast_lit->kind == AST_KIND_array_literal) {
//        assert(t->kind == TYPE_KIND_ARRAY);
//
//        AST_array_literal *array_lit = (AST_array_literal*)ast_lit;
//
//        array_lit->type_annotation = t;
//
//        if(TYPE_KIND_IS_RECORD_OR_ARRAY(t->array.of)) {
//
//            for(AST_expr_list *elements = array_lit->elements; elements; elements = elements->next) {
//                if(!AST_KIND_IS_AGGREGATE_LITERAL(elements->expr->kind)) {
//                    return;
//                }
//
//                annotate_aggregate_literal(jp, t->array.of, elements->expr);
//            }
//
//        }
//
//    } else {
//        UNREACHABLE;
//    }
//}

Value* make_empty_array_value(Job *jp, Type *t) {
    Value *v = job_alloc_value(jp, VALUE_KIND_ARRAY);
    v->val.array.n = t->array.n;
    v->val.array.elements = job_alloc_scratch(jp, sizeof(Value*) * t->array.n);
    if(t->array.of->kind == TYPE_KIND_ARRAY) {
        for(int i = 0; i < t->array.n; ++i)
            v->val.array.elements[i] = make_empty_array_value(jp, t->array.of);
    } else {
        Valuekind kind;

        switch(t->array.of->kind) {
            default:
                UNREACHABLE;
            case TYPE_KIND_BOOL:
                kind = VALUE_KIND_BOOL;
                break;
            case TYPE_KIND_CHAR:
                kind = VALUE_KIND_CHAR;
                break;
            case TYPE_KIND_S8:
            case TYPE_KIND_S16:
            case TYPE_KIND_S32:
            case TYPE_KIND_S64:
            case TYPE_KIND_INT:
                kind = VALUE_KIND_INT;
                break;
            case TYPE_KIND_U8:
            case TYPE_KIND_U16:
            case TYPE_KIND_U32:
            case TYPE_KIND_U64:
                kind = VALUE_KIND_UINT;
                break;
            case TYPE_KIND_FLOAT:
            case TYPE_KIND_F32:
                kind = VALUE_KIND_FLOAT;
                break;
            case TYPE_KIND_F64:
                UNIMPLEMENTED;
                break;
        }

        for(int i = 0; i < t->array.n; ++i)
            v->val.array.elements[i] = job_alloc_value(jp, kind);
    }

    return v;
}

u64 ir_gen_array_from_value(Job *jp, Type *array_type, Value *v) {
    assert(array_type->kind == TYPE_KIND_ARRAY);
    assert(v->kind == VALUE_KIND_ARRAY);

    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), array_type->align);

    u64 offset = arrlast(jp->local_offset);
    assert(array_type->bytes == array_type->array.element_stride * array_type->array.n);
    u64 expected_size = array_type->bytes;

    if(array_type->array.of->kind == TYPE_KIND_ARRAY) {
        for(int i = 0; i < v->val.array.n; ++i) {
            ir_gen_array_from_value(jp, array_type->array.of, v->val.array.elements[i]);
        }
    } else {
        arrlast(jp->local_offset) += expected_size;

        if(TYPE_KIND_IS_NOT_SCALAR(array_type->array.of->kind)) {
            UNIMPLEMENTED;
        } else {
            u64 stride = array_type->array.element_stride;

            //u64 *regp = &(jp->reg_alloc);

            IRinst inst = {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = offset,
                    .bytes = stride,
                },
            };

            if(TYPE_KIND_IS_FLOAT(array_type->array.of->kind)) {
                //regp = &(jp->float_reg_alloc);

                inst = (IRinst) {
                    .opcode = IROP_SETVARF,
                        .setvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = offset,
                            .bytes = stride,
                        },
                };
            }

            for(int i = 0; i < v->val.array.n; ++i) {
                Value *elem = v->val.array.elements[i];
                assert(elem->kind != VALUE_KIND_NIL);

                inst.setvar.immediate = true;
                if(array_type->array.of->kind == TYPE_KIND_F64) {
                    if(elem->kind == VALUE_KIND_INT) {
                        inst.setvar.imm.floating64 = (f64)(elem->val.integer);
                    } else if(elem->kind == VALUE_KIND_FLOAT) {
                        inst.setvar.imm.floating64 = (f64)(elem->val.floating);
                    } else if(elem->kind == VALUE_KIND_DFLOAT) {
                        inst.setvar.imm.floating64 = (f64)(elem->val.dfloating);
                    } else {
                        UNREACHABLE;
                    } 
                } else if(array_type->array.of->kind >= TYPE_KIND_FLOAT) {
                    //TODO implicit casts should be resolved in the typechecker
                    //TODO overhaul typechecking and implement number type
                    if(elem->kind == VALUE_KIND_INT) {
                        inst.setvar.imm.floating32 = (f32)(elem->val.integer);
                    } else {
                        inst.setvar.imm.floating32 = elem->val.floating;
                    }
                } else {
                    inst.setvar.imm.integer = elem->val.integer;
                }

                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
                inst.setvar.offset += stride;
            }
        }
    }

    u64 cur_offset = arrlast(jp->local_offset);
    u64 expected_offset = offset + expected_size;

    for(u64 step = 8; cur_offset < expected_offset; cur_offset += step) {
        while(cur_offset + step > expected_offset) step >>= 1;
        IRinst inst =
            (IRinst) {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = cur_offset,
                    .bytes = step,
                    .immediate = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    arrlast(jp->local_offset) = expected_offset;

    return offset;
}

INLINE void show_ir_loc(IRinst inst) {
    fprintf(stderr, "jcc:%i:%i: showing IR inst location\n| %i    %.*s\n%*s^^^\n",
            inst.loc.line, inst.loc.col, inst.loc.line, (int)(inst.loc.text.e - inst.loc.text.s), inst.loc.text.s,
            inst.loc.col + 6 + count_digits(inst.loc.line), "");
}

INLINE void print_ir_machine(IRmachine *machine, FILE *f) {
    fprintf(f, "IRmachine = {\n");

    fprintf(f, "  pc_stack = { ");
    for (int i = 0; i < arrlen(machine->pc_stack); i++) {
        fprintf(f, "%lu ", machine->pc_stack[i]);
    }
    fprintf(f, "}\n");

    fprintf(f, "  procid_stack = { ");
    for (int i = 0; i < arrlen(machine->procid_stack); i++) {
        fprintf(f, "%d ", machine->procid_stack[i]);
    }
    fprintf(f, "}\n");

    fprintf(f, "  jump_table_stack = { ");
    for (int i = 0; i < arrlen(machine->jump_table_stack); i++) {
        fprintf(f, "0x%p ", machine->jump_table_stack[i]);
    }
    fprintf(f, "}\n");

    //fprintf(f, "  global_segment = { ");
    //for(int i = 0; i < machine->global_segment_size; ++i)
    //    fprintf(f, "%X ", machine->global_segment[i]);
    //fprintf(f, "}\n");

    //fprintf(f, "  bss_segment = { ");
    //for(int i = 0; i < machine->bss_segment_size; ++i)
    //    fprintf(f, "%X ", machine->bss_segment[i]);
    //fprintf(f, "}\n");

    //fprintf(f, "  local_segment = { ");
    //for(int i = 0; i < machine->local_segment_size; ++i)
    //    fprintf(f, "%X ", machine->local_segment[i]);
    //fprintf(f, "}\n");

    fprintf(f, "  iregs = { ");
    for (int i = 0; i < 4; i++) {
        fprintf(f, "%lu ", machine->iregs[i]);
    }
    fprintf(f, "}\n");

    fprintf(f, "  f32regs = { ");
    for (int i = 0; i < 4; i++) {
        fprintf(f, "%f ", machine->f32regs[i]);
    }
    fprintf(f, "}\n");

    fprintf(f, "  f64regs = { ");
    for (int i = 0; i < 4; i++) {
        fprintf(f, "%lf ", machine->f64regs[i]);
    }
    fprintf(f, "}\n");

    /*
       fprintf(f, "  ports = { ");
       for (int i = 0; i < arrlen(machine->ports); i++) {
       __builtin_dump_struct(&machine->ports[i], fprintf, f);
       }
       fprintf(f, "}\n");
       */

    fprintf(f, "}\n");
}

INLINE void print_ir_inst(IRinst inst, FILE *f) {
    char *opstr = IRop_debug[inst.opcode];

    switch(inst.opcode) {
        default:
            UNREACHABLE;
        case IROP_GETCONTEXTARG:
        case IROP_SETCONTEXTARG:
        case IROP_HINT_BEGIN_FOREIGN_CALL:
        case IROP_HINT_END_FOREIGN_CALL:
        case IROP_HINT_BEGIN_CALL:
        case IROP_HINT_END_CALL:
        case IROP_HINT_BEGIN_PASS_NON_SCALAR:
        case IROP_HINT_END_PASS_NON_SCALAR:
        case IROP_NOOP:
            fprintf(f, "%s\n", opstr);
            break;
        case IROP_ADD: case IROP_SUB:
        case IROP_MUL: case IROP_DIV: case IROP_MOD:
        case IROP_AND: case IROP_OR: case IROP_XOR:
        case IROP_LSHIFT: case IROP_RSHIFT:
        case IROP_EQ: case IROP_NE: case IROP_LE: case IROP_GT:
        case IROP_FADD: case IROP_FSUB:
        case IROP_FMUL: case IROP_FDIV:
        case IROP_FEQ: case IROP_FNE: case IROP_FLE: case IROP_FGT:
            if(inst.arith.immediate) {
                if(inst.opcode >= IROP_FADD) {
                    if(inst.arith.operand_bytes[2] == 8) {
                        fprintf(f, "%s f_%lu %luB, f_%lu %luB, %g %luB\n",
                                opstr,
                                inst.arith.reg[0], inst.arith.operand_bytes[0],
                                inst.arith.reg[1], inst.arith.operand_bytes[1],
                                inst.arith.imm.floating64, inst.arith.operand_bytes[2]
                               );
                    } else {
                        fprintf(f, "%s f_%lu %luB, f_%lu %luB, %f %luB\n",
                                opstr,
                                inst.arith.reg[0], inst.arith.operand_bytes[0],
                                inst.arith.reg[1], inst.arith.operand_bytes[1],
                                inst.arith.imm.floating32, inst.arith.operand_bytes[2]
                               );
                    }
                } else {
                    fprintf(f, "%s r_%lu %luB, r_%lu %luB, %lu %luB\n",
                            opstr,
                            inst.arith.reg[0], inst.arith.operand_bytes[0],
                            inst.arith.reg[1], inst.arith.operand_bytes[1],
                            inst.arith.imm.integer, inst.arith.operand_bytes[2]
                           );
                }
            } else {
                if(inst.opcode >= IROP_FADD) {
                    fprintf(f, "%s f_%lu %luB, f_%lu %luB, f_%lu %luB\n",
                            opstr,
                            inst.arith.reg[0], inst.arith.operand_bytes[0],
                            inst.arith.reg[1], inst.arith.operand_bytes[1],
                            inst.arith.reg[2], inst.arith.operand_bytes[2]
                           );
                } else {
                    fprintf(f, "%s r_%lu %luB, r_%lu %luB, r_%lu %luB\n",
                            opstr,
                            inst.arith.reg[0], inst.arith.operand_bytes[0],
                            inst.arith.reg[1], inst.arith.operand_bytes[1],
                            inst.arith.reg[2], inst.arith.operand_bytes[2]
                           );
                }
            }
            break;
        case IROP_NOT: case IROP_NEG:
        case IROP_FNEG:
            assert(inst.arith.immediate == false);
            fprintf(f, "%s r_%lu %luB, r_%lu %luB\n",
                    opstr,
                    inst.arith.reg[0], inst.arith.operand_bytes[0],
                    inst.arith.reg[1], inst.arith.operand_bytes[1]
                   );
            break;
        case IROP_IF:
        case IROP_IFZ:
            fprintf(f, "%s r_%lu label %lu\n", opstr, inst.branch.cond_reg, inst.branch.label_id);
            break;
        case IROP_JMP:
            fprintf(f, "%s label %lu\n", opstr, inst.branch.label_id);
            break;
        case IROP_CALL:
            if(inst.call.immediate)
                fprintf(f, "%s %s %s %lu\n", opstr, inst.call.c_call ? "#c_call": "", inst.call.name, inst.call.id_imm);
            else
                fprintf(f, "%s r_%lu\n", opstr, inst.call.id_reg);
            break;
        case IROP_RET:
            fprintf(f, "%s %s\n", opstr, inst.call.c_call ? "#c_call": "");
            break;
        case IROP_LABEL:
            fprintf(f, "%s %lu\n", opstr, inst.label.id);
            break;
        case IROP_CALCPTROFFSET:
            fprintf(f, "%s r_%lu, ptr_r_%lu, offset_r_%lu, stride %luB\n",
                    opstr,
                    inst.calcptroffset.reg_dest,
                    inst.calcptroffset.reg_src_ptr,
                    inst.calcptroffset.offset_reg,
                    inst.calcptroffset.stride);
            break;
        case IROP_ADDRVAR:
            fprintf(f, "%s r_%lu, segment %s, offset %lu\n", opstr, inst.addrvar.reg_dest, IRsegment_debug[inst.addrvar.segment], inst.addrvar.offset);
            break;
        case IROP_LOAD:
        case IROP_LOADF:
            fprintf(f, "%s ", opstr);

            if(inst.opcode == IROP_LOADF) {
                if(inst.load.bytes == 8) {
                    fprintf(f, "f_%lu, ", inst.load.reg_dest);
                } else {
                    fprintf(f, "f_%lu, ", inst.load.reg_dest);
                }
            } else {
                fprintf(f, "r_%lu, ", inst.load.reg_dest);
            }

            if(inst.load.immediate) {
                if(inst.opcode == IROP_LOADF) {
                    if(inst.load.bytes == 8)
                        fprintf(f, "%g, ", inst.load.imm.floating64);
                    else
                        fprintf(f, "%f, ", inst.load.imm.floating32);
                } else {
                    fprintf(f, "%lu, ", inst.load.imm.integer);
                }
            } else {
                fprintf(f, "ptr_r_%lu, ", inst.load.reg_src_ptr);
            }

            if(inst.load.has_indirect_offset) {
                fprintf(f, "offset_r%lu, ", inst.load.offset_reg);
            } else if(inst.load.has_immediate_offset) {
                fprintf(f, "byte offset imm %lu, ", inst.load.byte_offset_imm);
            }

            fprintf(f, "%luB\n", inst.load.bytes);
            break;
        case IROP_STOR:
        case IROP_STORF:
            fprintf(f, "%s ptr_r_%lu, ", opstr, inst.stor.reg_dest_ptr);
            if(inst.stor.has_indirect_offset) fprintf(f, "offset_r_%lu, ", inst.stor.offset_reg);
            else if(inst.stor.has_immediate_offset) fprintf(f, "byte offset imm %lu, ", inst.stor.byte_offset_imm);
            if(inst.stor.immediate) {
                if(inst.opcode == IROP_STORF) {
                    if(inst.stor.bytes == 8) {
                        fprintf(f, "%g, ", inst.stor.imm.floating64);
                    } else {
                        fprintf(f, "%f, ", inst.stor.imm.floating32);
                    }
                } else {
                    fprintf(f, "%lu, ", inst.stor.imm.integer);
                }
            } else {
                if(inst.opcode == IROP_STORF) {
                    fprintf(f, "f_%lu, ", inst.stor.reg_src);
                } else {
                    fprintf(f, "r_%lu, ", inst.stor.reg_src);
                }
            }
            fprintf(f, "%luB\n", inst.stor.bytes);
            break;
        case IROP_GETVAR:
            fprintf(f, "%s r_%lu, segment %s, addr %lu, %luB\n", opstr, inst.getvar.reg_dest, IRsegment_debug[inst.getvar.segment], inst.getvar.offset, inst.getvar.bytes);
            break;
        case IROP_GETVARF:
            fprintf(f, "%s f_%lu, segment %s, addr %lu, %luB\n", opstr, inst.getvar.reg_dest, IRsegment_debug[inst.getvar.segment], inst.getvar.offset, inst.getvar.bytes);
            break;
        case IROP_SETVAR:
            if(inst.setvar.immediate)
                fprintf(f, "%s segment %s, addr %lu, %lu, %luB\n",   opstr, IRsegment_debug[inst.setvar.segment], inst.setvar.offset, inst.setvar.imm.integer, inst.setvar.bytes);
            else
                fprintf(f, "%s segment %s, addr %lu, r_%lu, %luB\n", opstr, IRsegment_debug[inst.setvar.segment], inst.setvar.offset, inst.setvar.reg_src, inst.setvar.bytes);
            break;
        case IROP_SETVARF:
            if(inst.setvar.immediate) {
                if(inst.setvar.bytes == 8)
                    fprintf(f, "%s segment %s, addr %lu, %g, %luB\n",opstr, IRsegment_debug[inst.setvar.segment], inst.setvar.offset,inst.setvar.imm.floating64,inst.setvar.bytes);
                else
                    fprintf(f, "%s segment %s, addr %lu, %f, %luB\n",opstr, IRsegment_debug[inst.setvar.segment], inst.setvar.offset,inst.setvar.imm.floating32,inst.setvar.bytes);
            } else {
                fprintf(f, "%s segment %s, addr %lu, f_%lu, %luB\n", opstr, IRsegment_debug[inst.setvar.segment], inst.setvar.offset, inst.setvar.reg_src, inst.setvar.bytes);
            }
            break;
        case IROP_SETARG: case IROP_SETRET:
            fprintf(f, "%s %s p_%lu, r_%lu, %luB\n",
                    opstr,
                    inst.setport.c_call ? "#c_call": "",
                    inst.setport.port, inst.setport.reg_src, inst.setport.bytes);
            break;
        case IROP_SETARGF: case IROP_SETRETF:
            fprintf(f, "%s %s p_%lu, f_%lu, %luB\n",
                    opstr,
                    inst.setport.c_call ? "#c_call": "",
                    inst.setport.port, inst.setport.reg_src, inst.setport.bytes);
            break;
        case IROP_GETARG: case IROP_GETRET:
            fprintf(f, "%s %s r_%lu, p_%lu, %luB\n",
                    opstr,
                    inst.getport.c_call ? "#c_call": "",
                    inst.getport.reg_dest, inst.getport.port, inst.getport.bytes);
            break;
        case IROP_GETARGF: case IROP_GETRETF:
            fprintf(f, "%s %s f_%lu, p_%lu, %luB\n",
                    opstr,
                    inst.getport.c_call ? "#c_call": "",
                    inst.getport.reg_dest, inst.getport.port, inst.getport.bytes);
            break;
        case IROP_ITOF:
            fprintf(f, "%s f_%lu %luB, r_%lu %luB\n",
                    opstr,
                    inst.typeconv.to_reg,
                    inst.typeconv.to_bytes,
                    inst.typeconv.from_reg,
                    inst.typeconv.from_bytes);
            break;
        case IROP_FTOI:
            fprintf(f, "%s r_%lu %luB, f_%lu %luB\n",
                    opstr,
                    inst.typeconv.to_reg,
                    inst.typeconv.to_bytes,
                    inst.typeconv.from_reg,
                    inst.typeconv.from_bytes);
            break;
        case IROP_ITOI:
            fprintf(f, "%s r_%lu %luB, r_%lu %luB\n",
                    opstr,
                    inst.typeconv.to_reg,
                    inst.typeconv.to_bytes,
                    inst.typeconv.from_reg,
                    inst.typeconv.from_bytes);
            break;
        case IROP_FTOB:
            fprintf(f, "%s r_%lu %luB, f_%lu %luB\n",
                    opstr,
                    inst.typeconv.to_reg,
                    inst.typeconv.to_bytes,
                    inst.typeconv.from_reg,
                    inst.typeconv.from_bytes);
            break;
        case IROP_FTOF:
            fprintf(f, "%s f_%lu %luB, f_%lu %luB\n",
                    opstr,
                    inst.typeconv.to_reg,
                    inst.typeconv.to_bytes,
                    inst.typeconv.from_reg,
                    inst.typeconv.from_bytes);
            break;
    }

}

INLINE void proc_table_add(int procid, IRproc procdata) {
    if(!procdata.is_foreign) {
        IRinst *instructions = procdata.instructions;
        fprintf(stderr, "begin code for '%s'()\n\n", procdata.name);
        IRinst inst = {0};
        for(u64 i = 0; i < procdata.n_instructions; ++i) {
            bool loc_changed = (inst.loc.line != instructions[i].loc.line);
            inst = instructions[i];
            if(loc_changed) {
                fprintf(stderr, "\n");
                show_ir_loc(inst);
            }

            fprintf(stderr, "%lu: ", i);
            print_ir_inst(inst, stderr);
        }
        fprintf(stderr, "\nend code for '%s'()\n\n\n", procdata.name);
    }

    hmput(proc_table, procid, procdata);
}

int getprec(Token t) {
    switch(t) {
#define X(t, prec) case (Token)t: return prec;
        OPERATOR_PREC_TABLE
#undef X
        default: return -1;
    }
}

INLINE AST* parse_top_level_statement(Job *jp) {
    AST *ast = NULL;

    if(ast == NULL) ast = parse_procdecl(jp);
    if(ast == NULL) ast = parse_structdecl(jp);
    if(ast == NULL) ast = parse_directive_statement(jp);
    if(ast == NULL) ast = parse_vardecl(jp);

    return ast;
}

INLINE AST* parse_directive_statement(Job *jp) {
    Lexer *lexer = jp->lexer;
    AST *ast = NULL;

    if(ast == NULL) ast = parse_run_directive(jp);
    if(ast == NULL) ast = parse_load_or_import_directive(jp);
    if(ast == NULL) return NULL;

    Token t = lex(lexer);

    if(t != ';') job_error(jp, lexer->loc, "expected ';' after top level directive");

    return ast;
}

AST* parse_load_or_import_directive(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_LOAD_DIRECTIVE && t != TOKEN_IMPORT_DIRECTIVE) {
        *lexer = unlex;
        return NULL;
    }

    Token str_token = lex(lexer);

    if(str_token != TOKEN_STRINGLIT) {
        job_error(jp, lexer->loc, "expected string literal");
        return NULL;
    }

    AST_import_directive *ast = (AST_import_directive*)job_alloc_ast(jp, AST_KIND_import_directive);
    ast->path = job_alloc_text(jp, lexer->text.s, lexer->text.e);
    ast->just_load_the_source = (t == TOKEN_LOAD_DIRECTIVE);

    return (AST*)ast;
}

AST* parse_run_directive(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_RUN_DIRECTIVE) {
        *lexer = unlex;
        return NULL;
    }

    AST_call *ast_call = (AST_call*)parse_call(jp);

    if(ast_call == NULL) {
        assert(jp->state == JOB_STATE_ERROR);
        return NULL;
    }

    AST_run_directive *ast = (AST_run_directive*)job_alloc_ast(jp, AST_KIND_run_directive);
    ast->base.weight = ast_call->base.weight + 1;
    ast->call_to_run = ast_call;

    return (AST*)ast;
}

AST* parse_usingstatement(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_USING) {
        *lexer = unlex;
        return NULL;
    }

    AST_usingstatement *node = (AST_usingstatement*)job_alloc_ast(jp, AST_KIND_usingstatement);

    t = lex(lexer);
    if(t != TOKEN_IDENT) {
        job_error(jp, lexer->loc, "expected identifier after 'using' keyword");
        return (AST*)node;
    }

    node->name = job_alloc_text(jp, lexer->text.s, lexer->text.e);

    t = lex(lexer);
    if(t != ';') {
        job_error(jp, lexer->loc, "expected ';' to end 'using' statement");
    }

    return (AST*)node;
}

AST* parse_structblock(Job *jp, int *n_members, int *n_using) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);
    assert(t == '{');

    AST_statement head = {0};
    AST_statement *statement_list = &head;

    AST_usingstatement using_head = {0};
    AST_usingstatement *using_list = &using_head;

    while(true) {
        bool got_using = false;

        statement_list->next = parse_anonstruct(jp);
        if(!statement_list->next) statement_list->next = parse_vardecl(jp);

        if(!statement_list->next) {
            using_list->next = parse_usingstatement(jp);
            got_using = true;
        }

        if(!statement_list->next && !using_list->next) {
            t = lex(lexer);
            job_error(jp, lexer->loc, "illegal statement in declarative block");
            return NULL;
        }

        if(got_using) {
            using_list = (AST_usingstatement*)(using_list->next);
            *n_using += 1;
        } else {
            statement_list = (AST_statement*)(statement_list->next);

            if(statement_list->base.kind == AST_KIND_structdecl || statement_list->base.kind == AST_KIND_uniondecl) {
                AST_structdecl *ast_struct = (AST_structdecl*)(AST*)statement_list;
                if(ast_struct->name) {
                    char *s = (statement_list->base.kind == AST_KIND_uniondecl) ? "union" : "struct";
                    job_error(jp, ast_struct->base.loc,
                            "a named nested %ss must be created by saying 'name: %s {...}'", s, s);
                    break;
                }
                *n_members += ast_struct->n_members;
            } else {
                *n_members += 1;
                if(jp->parsing_uniondecl > 0) {
                    AST_vardecl *ast_var = (AST_vardecl*)(AST*)statement_list;
                    if(ast_var->init || ast_var->uninitialized) {
                        job_error(jp, ast_var->base.loc, "members of unions cannot be initialized");
                        break;
                    }
                }
            }
        }

        unlex = *lexer;
        t = lex(lexer);

        if(t == '}') {
            statement_list->next = NULL;
            break;
        } else if(t == 0) {
            job_error(jp, lexer->loc, "unexpected end of source");
            break;
        }
        *lexer = unlex;
    }

    if(using_head.next) {
        using_list->next = head.next;
        head.next = using_head.next;
    }

    return head.next;
}

AST* parse_anonstruct(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_STRUCT && t != TOKEN_UNION) {
        *lexer = unlex;
        return NULL;
    }

    Token decl_token = t;

    AST_structdecl *node = (AST_structdecl*)job_alloc_ast(jp,
            (decl_token == TOKEN_UNION) ? AST_KIND_uniondecl : AST_KIND_structdecl);
    node->base.loc = lexer->loc;

    if(decl_token == TOKEN_UNION)
        jp->parsing_uniondecl++;

    unlex = *lexer;
    t = lex(lexer);

    if(t != '{') {
        job_error(jp, lexer->loc, "expected '{' to begin %s body",
                (decl_token == TOKEN_UNION) ? "union" : "struct");
        return NULL;
    }

    *lexer = unlex;
    node->body = parse_structblock(jp, &(node->n_members), &(node->n_using));

    if(decl_token == TOKEN_UNION)
        jp->parsing_uniondecl--;

    return (AST*)node;
}

AST* parse_structdecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    bool top_level = jp->parser_at_top_level;

    if(top_level) {
        jp->allocator.scratch = &global_scratch_allocator;
    }

    Token t = lex(lexer);

    if(t != TOKEN_STRUCT && t != TOKEN_UNION) {
        *lexer = unlex;
        return NULL;
    }

    Token decl_token = t;

    Loc_info decl_loc = lexer->loc;

    AST_structdecl *node = (AST_structdecl*)job_alloc_ast(jp,
            (decl_token == TOKEN_UNION) ? AST_KIND_uniondecl : AST_KIND_structdecl);

    if(decl_token == TOKEN_UNION)
        jp->parsing_uniondecl++;

    t = lex(lexer);
    if(t != TOKEN_IDENT) {
        job_error(jp, lexer->loc, "expected identifier after '%s' keyword",
                (decl_token == TOKEN_UNION) ? "union" : "struct");
        return (AST*)node;
    }

    node->base.loc = decl_loc;
    node->name = job_alloc_text(jp, lexer->text.s, lexer->text.e);

    unlex = *lexer;
    t = lex(lexer);
    if(t == '(') {
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
                job_error(jp, param_list->base.loc,
                        "once first default parameter is declared, all parameters must have default values");
                return (AST*)node;
            }
            t = lex(lexer);

            if(param_list) ++n_params;

            if(t != ',') {
                break;
            }
        }

        if(t != ')') job_error(jp, lexer->loc, "expected ')' to end parameter list");

        node->has_defaults = must_be_default;
        node->params = head.next;
        node->n_params = n_params;
    } else {
        *lexer = unlex;
    }

    unlex = *lexer;
    t = lex(lexer);

    if(t != '{')
        job_error(jp, lexer->loc, "expected '{' to begin %s body",
                (decl_token == TOKEN_UNION) ? "union" : "struct");

    *lexer = unlex;
    node->body = parse_structblock(jp, &(node->n_members), &(node->n_using));

    if(decl_token == TOKEN_UNION)
        jp->parsing_uniondecl--;

    return (AST*)node;
}

AST* parse_procdecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    bool top_level = jp->parser_at_top_level;

    Arena *save_allocator = jp->allocator.scratch;

    if(top_level) {
        jp->allocator.scratch = &global_scratch_allocator;
    }

    Token t = lex(lexer);

    if(t != TOKEN_PROC) {
        *lexer = unlex;
        return NULL;
    }

    Loc_info procdecl_loc = lexer->loc;
    AST_procdecl *node = (AST_procdecl*)job_alloc_ast(jp, AST_KIND_procdecl);

    t = lex(lexer);
    if(t != TOKEN_IDENT) {
        job_error(jp, lexer->loc, "expected identifier after 'proc' keyword");
        return (AST*)node;
    }

    node->name = job_alloc_text(jp, lexer->text.s, lexer->text.e);
    node->base.loc = procdecl_loc;

    t = lex(lexer);

    if(t != '(') job_error(jp, lexer->loc, "expected '(' to begin parameter list");

    int n_params = 0;
    int n_polymorphic_params = 0;
    int n_wildcards = 0;
    AST_paramdecl head = {0};
    AST_paramdecl *param_list = &head;

    bool must_be_default = false;

    while(true) {
        unlex = *lexer;
        t = lex(lexer);
        if(t == TOKEN_IDENT) {
            char *s = lexer->text.s;
            char *e = lexer->text.e;
            t = lex(lexer);
            if(t == ':') {
                t = lex(lexer);
                if(t == TOKEN_TWODOT) {
                    node->varargs = true;
                    AST_paramdecl *p = (AST_paramdecl*)job_alloc_ast(jp, AST_KIND_paramdecl);
                    p->name = job_alloc_text(jp, s, e);
                    p->vararg = true;
                    param_list->next = p;
                    param_list = param_list->next;
                    param_list->index = n_params;
                    t = lex(lexer);
                    break;
                } else {
                    *lexer = unlex;
                    param_list->next = (AST_paramdecl*)parse_paramdecl(jp);
                    if(param_list->next->is_polymorphic) {
                        n_polymorphic_params++;
                        n_wildcards += jp->parser_encountered_polymorphic_var;
                        jp->parser_encountered_polymorphic_var = 0;
                    }

                    if(param_list->next == NULL) {
                        t = lex(lexer);
                        break;
                    }

                }

            } else {
                break;
            }
        } else {
            break;
        }

        param_list = param_list->next;
        param_list->index = n_params;
        if(!must_be_default && param_list->init != NULL) {
            node->first_default_param = n_params;
            must_be_default = true;
        }

        if(must_be_default && param_list->init == NULL) {
            job_error(jp, param_list->base.loc,
                    "once first default parameter is declared, all parameters must have default values");
            return (AST*)node;
        }
        t = lex(lexer);

        if(param_list) ++n_params;

        if(t != ',') {
            break;
        }
    }

    if(t != ')') job_error(jp, lexer->loc, "expected ')' to end parameter list");

    node->has_defaults = must_be_default;
    node->params = head.next;
    node->n_params = n_params;
    node->is_polymorphic = (n_polymorphic_params > 0);
    node->n_polymorphic_params = n_polymorphic_params;
    node->n_wildcards = n_wildcards;

    unlex = *lexer;
    t = lex(lexer);

    if(t != '{' && t != ';' && (t < TOKEN_FOREIGN_DIRECTIVE || t > TOKEN_INLINE_DIRECTIVE)) {
        int n_rets = 0;
        AST_retdecl head = {0};
        AST_retdecl *ret_list = &head;

        if(t == TOKEN_VOID) {
            node->n_rets = 0;
            unlex = *lexer;
            t = lex(lexer);
        } else {
            *lexer = unlex;
            while(true) {
                ret_list->next = (AST_retdecl*)job_alloc_ast(jp, AST_KIND_retdecl);
                ret_list->next->expr = parse_expr(jp);
                ret_list = ret_list->next;
                ret_list->index = n_rets;
                unlex = *lexer;
                t = lex(lexer);

                if(ret_list->expr) ++n_rets;

                if(t != ',') {
                    break;
                }
            }

            node->rets = head.next;
            node->n_rets = n_rets;
        }
    }

    if(t == TOKEN_SYSTEM_DIRECTIVE) {
        node->is_foreign = true;
        node->c_call = true;
        node->is_system = true;
        assert(!node->must_inline);
        t = lex(lexer);
        if(t != ';')
            job_error(jp, lexer->loc, "expected ';' at end of foreign procedure header");
        return (AST*)node;
    } else if(t == TOKEN_FOREIGN_DIRECTIVE) {
        node->is_foreign = true;
        node->c_call = true;
        assert(!node->must_inline);
        unlex = *lexer;
        t = lex(lexer);
        node->foreign_lib_str = job_alloc_text(jp, lexer->text.s, lexer->text.e);
        t = lex(lexer);
        if(t != ';')
            job_error(jp, lexer->loc, "expected ';' at end of foreign procedure header");
        return (AST*)node;
    } else {
        if(t == TOKEN_INLINE_DIRECTIVE) {
            node->must_inline = true;
            unlex = *lexer;
            t = lex(lexer);
        }

        if(t == TOKEN_C_CALL_DIRECTIVE) {
            node->c_call = true;
            unlex = *lexer;
            t = lex(lexer);
        }
    }

    if(t != '{' && t != ';') {
        job_error(jp, lexer->loc, "expected '{' or ';'");
        return (AST*)node;
    }

    if(t == ';') return (AST*)node;

    *lexer = unlex;

    jp->parser_at_top_level = false;

    if(node->is_polymorphic == false)
        jp->allocator.scratch = save_allocator;

    node->body = parse_procblock(jp);

    jp->parser_at_top_level = top_level;

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

    Pool_save save[AST_KIND_MAX] = {0};
    job_ast_allocator_to_save(jp, save);
    AST_block *block = (AST_block*)job_alloc_ast(jp, AST_KIND_block);

    AST_statement head = {0};
    AST_statement *statement_list = &head;

    while(true) {
        statement_list->next = parse_controlflow(jp);
        if(!statement_list->next) statement_list->next = parse_pushcontext(jp);
        if(!statement_list->next) statement_list->next = parse_procdecl(jp);
        if(!statement_list->next) statement_list->next = parse_procblock(jp);
        if(!statement_list->next) statement_list->next = parse_vardecl(jp);
        if(!statement_list->next) statement_list->next = parse_statement(jp);

        if(jp->state == JOB_STATE_ERROR) {
            job_ast_allocator_from_save(jp, save);
            return NULL;
        }

        if(!statement_list->next) {
            return NULL;
        }

        statement_list = (AST_statement*)(statement_list->next);

        unlex = *lexer;
        t = lex(lexer);

        if(t == '}') {
            statement_list->next = NULL;
            break;
        } else if(t == 0) {
            job_error(jp, lexer->loc, "unexpected end of source");
            break;
        }
        *lexer = unlex;
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
                if(t == TOKEN_IDENT) {
                    char *s = lexer->text.s;
                    char *e = lexer->text.e;
                    t = lex(lexer);
                    if(t != ':')
                        *lexer = unlex;
                    else
                        node->label = job_alloc_text(jp, s, e);
                } else {
                    *lexer = unlex;
                }

                node->condition = parse_expr(jp);

                if(node->condition == NULL) job_error(jp, node->base.loc, "if statement missing condition");

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
                        if(branch->condition == NULL) job_error(jp, branch->base.loc, "else-if statement missing condition");
                        branch->body = body_func(jp);
                    } else {
                        *lexer = unlex;
                        branch->branch = body_func(jp);
                        if(!branch->branch) {
                            job_error(jp, lexer->loc, "else statement has no body");
                        }
                    }

                    if(node->body == NULL) job_error(jp, node->base.loc, "missing body");

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

                if(t == '!') {
                    node->reverse_order = true;
                    unlex = *lexer;
                    t = lex(lexer);
                }

                if(t == '*') {
                    node->by_pointer = true;
                    unlex = *lexer;
                    t = lex(lexer);
                }

                if(t == TOKEN_IDENT) {
                    char *s = lexer->text.s;
                    char *e = lexer->text.e;
                    t = lex(lexer);
                    if(t != ':')
                        *lexer = unlex;
                    else
                        node->label = job_alloc_text(jp, s, e);
                } else {
                    *lexer = unlex;
                }

                char *dot_pos = NULL;
                for(int i = 0; lexer->pos[i] != 0 && lexer->pos[i] != ';' && lexer->pos[i] != '{'; ++i) {
                    if(lexer->pos[i] == '.' && lexer->pos[i+1] == '.') {
                        dot_pos = lexer->pos + i;
                        lexer->pos[i] = ';';
                        lexer->pos[i+1] = ' ';
                        node->is_range_for = true;
                        break;
                    }
                }

                if(node->is_range_for) {
                    node->begin_range_expr = parse_expr(jp);
                    if(node->begin_range_expr == NULL) job_error(jp, node->base.loc, "for statement missing expression");

                    t = lex(lexer);
                    assert(t == ';');

                    node->end_range_expr = parse_expr(jp);
                    if(node->end_range_expr == NULL) job_error(jp, node->base.loc, "for statement missing expression");

                    dot_pos[0] = '.';
                    dot_pos[1] = '.';
                } else {
                    node->expr = parse_expr(jp);
                    if(node->expr == NULL) job_error(jp, node->base.loc, "for statement missing expression");
                }

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
                if(t == TOKEN_IDENT) {
                    char *s = lexer->text.s;
                    char *e = lexer->text.e;
                    t = lex(lexer);
                    if(t != ':')
                        *lexer = unlex;
                    else
                        node->label = job_alloc_text(jp, s, e);
                } else {
                    *lexer = unlex;
                }

                node->condition = parse_expr(jp);

                if(node->condition == NULL) job_error(jp, node->base.loc, "while statement missing condition");

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

AST* parse_pushcontext(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_PUSH_CONTEXT) {
        *lexer = unlex;
        return NULL;
    }

    AST_push_context *ast_push = (AST_push_context*)job_alloc_ast(jp, AST_KIND_push_context);

    t = lex(lexer);

    if(t != TOKEN_IDENT) {
        job_error(jp, lexer->loc, "expected identifier after 'push_context'");
        return NULL;
    }

    ast_push->context_ident = job_alloc_text(jp, lexer->text.s, lexer->text.e);
    ast_push->down = parse_procblock(jp);

    if(!ast_push->down) {
        job_error(jp, lexer->loc, "expected '{' to begin 'push_context' block");
        return NULL;
    }

    return (AST*)ast_push;
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

        if(node->left == NULL) {
            t = lex(lexer);
            job_error(jp, lexer->loc, "empty statement");
            return NULL;
        }

        t = lex(lexer);

        if(t == '=' || (t >= TOKEN_PLUSEQUAL && t <= TOKEN_XOREQUAL)) {
            node->assign_op = t;
            node->right = parse_rvalue(jp);
            t = lex(lexer);
            if(!is_lvalue(node->left)) job_error(jp, node->left->loc, "left hand side of assignment must be lvalue");
        } else if(t == TOKEN_PLUSPLUS || t == TOKEN_MINUSMINUS) {
            node->assign_op = t;
            t = lex(lexer);
            if(!is_lvalue(node->left)) job_error(jp, node->left->loc, "left hand side of assignment must be lvalue");
        }

        if(t != ';') job_error(jp, lexer->loc, "expected ';' at end of statement");

        return (AST*)node;
    }

    if(t < TOKEN_CONTINUE || t > TOKEN_RETURN) {
        job_error(jp, lexer->loc, "expected 'continue', 'break' or 'return'");
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
                    t = lex(lexer);
                }

                if(t != ';') {
                    job_error(jp, lexer->loc, "expected label or ';' in continue statement");
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
                    t = lex(lexer);
                }

                if(t != ';') {
                    job_error(jp, lexer->loc, "expected label or ';' in break statement");
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
                        expr_list->expr = parse_rvalue(jp);
                        t = lex(lexer);
                    }

                    if(t != ';') job_error(jp, lexer->loc, "expected ';' at end of return statement");
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

    bool top_level = jp->parser_at_top_level;

    if(top_level) {
        jp->allocator.scratch = &global_scratch_allocator;
    }

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
    node->name = job_alloc_text(jp, name_s, name_e);
    node->base.loc = vardecl_loc;

    Lexer unlex = *lexer;

    t = lex(lexer);

    if(t == '=' || t == ':') {
        node->constant = (t == ':');
        unlex = *lexer;
        t = lex(lexer);
        if(t == TOKEN_LONGDASH) {
            if(node->constant) job_error(jp, node->base.loc, "cannot make constant uninitialized");
            node->uninitialized = true;
        } else {
            *lexer = unlex;
            node->init = parse_rvalue(jp);
        }
        t = lex(lexer);
    } else {
        *lexer = unlex;
        Loc_info type_loc = lexer->loc; //TODO is this right?

        node->type = parse_expr(jp);

        if(node->type == NULL) job_error(jp, type_loc, "expected type declarator");

        t = lex(lexer);
        if(t == '=' || t == ':') {
            node->constant = (t == ':');
            //node->init = parse_expr(jp);
            //t = lex(lexer);
            unlex = *lexer;
            t = lex(lexer);
            if(t == TOKEN_LONGDASH) {
                if(node->constant) job_error(jp, node->base.loc, "cannot make constant uninitialized");
                node->uninitialized = true;
            } else {
                *lexer = unlex;
                node->init = parse_rvalue(jp);
                if(node->init == NULL) job_error(jp, lexer->loc, "expected initializer expression");
            }
            t = lex(lexer);
        } else if(t != ';' && t != ')' && t != ',' && t != '{') {
            job_error(jp, lexer->loc, "expected '=', ':' or separator in declaration");
        }
    }


    if(t != ';' && t != ')' && t != ',' && t != '{')
        job_error(jp, lexer->loc, "expected punctuation at end of variable declaration");

    return (AST*)node;
}

AST* parse_paramdecl(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer lexer_reset = *lexer;

    Token t = lex(lexer);

    if(t == TOKEN_VOID) {
        return NULL;
    }

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
    node->name = job_alloc_text(jp, name_s, name_e);
    node->base.loc = paramdecl_loc;

    Lexer unlex = *lexer;

    Loc_info type_loc = lexer->loc;
    node->type = parse_expr(jp);

    node->is_polymorphic = (jp->parser_encountered_polymorphic_var > 0);

    if(node->type == NULL) job_error(jp, type_loc, "expected type declarator");

    unlex = *lexer;
    t = lex(lexer);

    if(t == '=') {
        node->init = parse_rvalue(jp);
        unlex = *lexer;
        t = lex(lexer);
    }

    if(t != ')' && t != ',')
        job_error(jp, lexer->loc, "expected punctuation at end of parameter declaration");

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
    if(op->right == NULL) {
        job_error(jp, lexer->loc, "expected operand on right of '%c'", (char)t);
        return NULL;
    }
    op->base.weight += op->right->weight;

    return (AST*)op;
}

AST* parse_expr_decrease_prec(Job *jp, int min_prec) {
    AST *left, *node;
    AST_expr *op;

    left = parse_prefix(jp);

    if(left == NULL)
        return NULL;

    while(true) {
        node = parse_expr_increase_prec(jp, left, min_prec);
        if(node == left) break;
        if(node == NULL) break;
        assert(node->kind == AST_KIND_expr);
        node->weight += left->weight;
        op = (AST_expr*)node;
        op->left = left;
        left = node;
    }

    return left;
}

AST* parse_prefix(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    AST *node = NULL;
    AST_expr *expr = NULL;

    switch(t) {
        default:
            *lexer = unlex;
            node = parse_postfix(jp);
            break;
        case TOKEN_SIZEOF:
        case TOKEN_ALIGNOF:
        case TOKEN_TYPEOF:
        case TOKEN_TYPEINFO:
            if(lex(lexer) != '(') {
                job_error(jp, lexer->loc,
                        "expected '(' after '%s' keyword",
                        TOKEN_TO_KEYWORD(t));
                return NULL;
            }

            expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            expr->token = t;
            expr->right = parse_expr(jp);

            t = lex(lexer);

            if(t != ')') {
                job_error(jp, lexer->loc, "unbalanced parenthesis");
                return NULL;
            }

            node = (AST*)expr;
            node->weight += expr->right->weight;
            break;
        case TOKEN_CAST:
            t = lex(lexer);

            if(t != '(') {
                job_error(jp, lexer->loc, "expected '(' after 'cast' keyword");
                return NULL;
            }

            expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            expr->token = TOKEN_CAST;
            expr->left = parse_expr(jp);

            t = lex(lexer);

            if(t != ')') {
                job_error(jp, lexer->loc, "unbalanced parenthesis");
                return NULL;
            }

            expr->right = parse_prefix(jp);

            if(expr->right == NULL) {
                job_error(jp, expr->base.loc, "expected operand for cast");
                return NULL;
            }

            node = (AST*)expr;
            node->weight += expr->right->weight;
            break;
        case '+': case '-': case '!': case '~': case '@': case '>':
            expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            expr->token = t;
            expr->right = parse_prefix(jp);

            /*
               if((t == '>' || t == '@') && expr->right && !is_lvalue(expr->right))
               job_error(jp, expr->base.loc,
               (t == '>')
               ? "operand of pointer dereference '>' must be lvalue"
               : "operand of address operator '@' must be lvalue"
               );
               */
            if(expr->right == NULL) {
                job_error(jp, expr->base.loc, "expected operand for '%c'", (char)t);
                return NULL;
            }

            if(t == '@' && expr->right && !is_lvalue(expr->right)) {
                job_error(jp, expr->base.loc, "operand of address operator '@' must be lvalue");
                return NULL;
            }

            node = (AST*)expr;
            node->weight += expr->right->weight;
            break;
    }

    return node;
}

AST* parse_postfix(Job *jp) {
    AST *node = NULL;
    AST_expr *expr = NULL;
    AST_atom *atom = NULL;

    AST *term = parse_term(jp);

    if(term == NULL) {
        return NULL;
    }

    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    while(t == '.' || t == '[') {
        expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
        expr->token = t;
        expr->left = term;
        expr->base.weight += term->weight;

        if(t == '[') {
            expr->right = parse_expr(jp);
            if(expr->right == NULL) {
                unlex = *lexer;
                t = lex(lexer);
                if(t == ']') {
                    expr->token = '>';
                    expr->right = expr->left;
                    expr->left = NULL;
                } else {
                    return (AST*)expr;
                }
            } else {
                expr->base.weight += expr->right->weight;
                t = lex(lexer);
                if(t != ']') {
                    job_error(jp, lexer->loc, "unbalanced square bracket");
                    return NULL;
                }

                //if(!is_lvalue(expr->left))
                //    job_error(jp, expr->base.loc, "operand of subscript operator must be lvalue");
            }
        } else if(t == '.') {
            t = lex(lexer);

            if(t != TOKEN_IDENT) {
                job_error(jp, lexer->loc, "identifier expected on right of '.' operator");
                return NULL;
            }
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = TOKEN_IDENT;
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e);
            expr->right = (AST*)atom;
            expr->base.weight += 1;
        } else {
            UNREACHABLE;
        }

        term = (AST*)expr;
        unlex = *lexer;
        t = lex(lexer);
    }

    *lexer = unlex;

    node = (AST*)term;

    return node;
}

AST* parse_term(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Lexer unlex_all = *lexer;
    Token t = lex(lexer);

    AST *node = NULL;
    AST_expr *expr = NULL;
    AST_expr *pointer_type = NULL;
    AST_expr *array_type = NULL;
    AST_atom *atom = NULL;

    switch(t) {
        default:
            *lexer = unlex;
            return NULL;
        case '$':
            {
                t = lex(lexer);
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = TOKEN_POLYMORPHIC_IDENT;
                atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                node = (AST*)atom;
                node->weight = 1;
                jp->parser_encountered_polymorphic_var++;
            }
            break;
        case TOKEN_STRUCT: case TOKEN_UNION:
            {
                *lexer = unlex;
                node = parse_anonstruct(jp);
                if(jp->state == JOB_STATE_ERROR) {
                    return NULL;
                }
                return node;
            }
            break;
        case TOKEN_PROC:
            // Allocator :: proc (s32, s64, s64, *void, *void, s64) *void;
            // multi_return: proc (s32, int) (*int, bool);
            //
            {
                t = lex(lexer);
                if(t != '(') {
                    job_error(jp, lexer->loc, "expected '(' after 'proc' keyword");
                    return NULL;
                }

                AST_expr_list head = {0};
                AST_expr_list *param_list = &head;

                int n_params = 0;

                t = ',';
                while(t == ',') {
                    AST *expr = parse_expr(jp);

                    if(!expr) return NULL;
                    param_list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                    param_list = param_list->next;
                    param_list->expr = expr;

                    t = lex(lexer);
                    n_params++;
                }

                param_list = head.next;

                if(t != ')') {
                    job_error(jp, lexer->loc, "expected ')' to terminate proc type parameter list");
                    return NULL;
                }

                head = (AST_expr_list){0};
                AST_expr_list *ret_list = &head;

                unlex = *lexer;
                t = lex(lexer);

                int n_rets = 0;
                if(t == '(') {
                    t = ',';
                    while(t == ',') {
                        AST *expr = parse_expr(jp);

                        if(!expr) return NULL;
                        ret_list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                        ret_list = ret_list->next;
                        ret_list->expr = expr;

                        t = lex(lexer);
                        n_rets++;
                    }

                    if(t != ')') {
                        job_error(jp, lexer->loc, "expected ')' to terminate proc type return list");
                        return NULL;
                    }
                } else {
                    *lexer = unlex;
                    AST *expr = parse_expr(jp);
                    if(!expr) return NULL;
                    ret_list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
                    ret_list = ret_list->next;
                    ret_list->expr = expr;
                    n_rets = 1;
                }

                ret_list = head.next;

                AST_proctype *proctype = (AST_proctype*)job_alloc_ast(jp, AST_KIND_proctype);
                proctype->params = param_list;
                proctype->rets = ret_list;
                proctype->n_params = n_params;
                proctype->n_rets = n_rets;

                node = (AST*)proctype;
            }
            break;
        case TOKEN_CONTEXT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_RUN_DIRECTIVE:
            *lexer = unlex;
            node = parse_run_directive(jp);
            break;
        case '*':
            pointer_type = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            pointer_type->token = '*';
            pointer_type->right = parse_term(jp);
            node = (AST*)pointer_type;
            node->weight = pointer_type->right->weight;
            break;
        case '[':
            array_type = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
            array_type->token = t;

            // NOTE
            // putting the expr inside the [ ] on the right is a little weird
            // but it is done to be consistent with the other use of '[' for subscript

            unlex = *lexer;
            t = lex(lexer);
            if(t == TOKEN_TWODOT) {
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = TOKEN_TWODOT;
                array_type->right = (AST*)atom;
            } else {
                *lexer = unlex;
                array_type->right = parse_expr(jp);
            }
            t = lex(lexer);
            if(t != ']') job_error(jp, lexer->loc, "unbalanced square bracket");
            array_type->left = parse_term(jp);
            if(array_type->left == NULL) {
                *lexer = unlex_all;
                return NULL;
            }
            node = (AST*)array_type;
            node->weight = array_type->left->weight;
            if(array_type->right)
                node->weight += array_type->right->weight;
            break;
        case '(':
            expr = (AST_expr*)parse_expr(jp);
            t = lex(lexer);
            if(t != ')') job_error(jp, lexer->loc, "unbalanced parenthesis");
            node = (AST*)expr;
            break;
        case TOKEN_IDENT:
            {
                Lexer unlex2 = *lexer; // terrible name I'm sorry

                t = lex(lexer);

                if(t == '(') {
                    *lexer = unlex;
                    return parse_call(jp);
                }

                *lexer = unlex2;

                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = TOKEN_IDENT;
                atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                node = (AST*)atom;
                node->weight = 1;
            }
            break;
        case TOKEN_VOID:
        case TOKEN_INT:
        case TOKEN_FLOAT:
        case TOKEN_CHAR:
        case TOKEN_BOOL:
        case TOKEN_U8:   case TOKEN_U16:  case TOKEN_U32:  case TOKEN_U64:
        case TOKEN_S8:   case TOKEN_S16:  case TOKEN_S32:  case TOKEN_S64:
        case TOKEN_F32:  case TOKEN_F64:
        case TOKEN_STRING:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_CHARLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->character = lexer->character;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_INTLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->integer = lexer->integer;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_HEXLIT: case TOKEN_BINLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->uinteger = lexer->uinteger;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_FLOATLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
            atom->token = t;
            atom->floating = lexer->floating;
            node = (AST*)atom;
            node->weight = 1;
            break;
        case TOKEN_STRINGLIT:
            {
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                atom->token = t;
                atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e);
                node = (AST*)atom;
                node->weight = 1;
            }
            break;
    }

    return node;
}

AST* parse_call(Job *jp) {
    AST *node = NULL;
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        job_error(jp, lexer->loc, "expected call");
        return NULL;
    }

    AST_atom *callee = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
    callee->token = TOKEN_IDENT;
    callee->text = job_alloc_text(jp, lexer->text.s, lexer->text.e);

    AST_call *call_op = (AST_call*)job_alloc_ast(jp, AST_KIND_call);
    call_op->callee = (AST*)callee; //TODO if calls can only be done to a name the call should just have a name field
    call_op->base.loc = lexer->loc;

    t = lex(lexer);

    if(t != '(') {
        job_error(jp, lexer->loc, "expected beginning of parameter list");
        return NULL;
    }

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
                param->name = job_alloc_text(jp, s, e);
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
            if(!named_param)
                job_error(jp, call_op->base.loc,
                        "once a named parameter is passed, all subsequent parameters must be named");
        }

        param->value = parse_array_lit(jp);
        if(param->value == NULL) param->value = parse_expr(jp);

        if(param->value == NULL) {
            if(named_param)
                job_error(jp, param->base.loc, "named parameter has no initializer");
            t = lex(lexer);
            assert("expected closing paren"&&(t == ')'));
            break;
        }

        t = lex(lexer);

        if(t == ',') {
            param->next = (AST_param*)job_alloc_ast(jp, AST_KIND_param);
        } else if(t != ')') {
            job_error(jp, lexer->loc, "expected comma or end of parameter list");
        }

        param->index = n_params;
        call_op->base.weight += param->value->weight;
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
        if(call_op->callee->kind != AST_KIND_atom || ((AST_atom*)call_op->callee)->token != TOKEN_IDENT)
            job_error(jp,
                    call_op->base.loc,
                    "named parameters can only be passed to a named procedure");
    }

    node = (AST*)call_op;

    return node;
}

AST* parse_array_lit(Job *jp) {
    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    AST *type_expr = NULL;

    if(t == '[') {
        while(t != ']' && t != ',') {
            t = lex(lexer);
        }

        if(t == ']') {
            *lexer = unlex;
            type_expr = parse_expr(jp);

            if(type_expr == NULL) {
                *lexer = unlex;
                t = lex(lexer);
                assert(t == '[');
            } else {
                t = lex(lexer);
                if(t != ':') {
                    *lexer = unlex;
                    return NULL;
                }

                t = lex(lexer);
                if(t != '[') {
                    *lexer = unlex;
                    return NULL;
                }
            }
        } else {
            *lexer = unlex;
            t = lex(lexer);

            if(t != '[') {
                *lexer = unlex;
                return NULL;
            }
        }
    } else {
        *lexer = unlex;
        type_expr = parse_expr(jp);

        t = lex(lexer);
        if(t != ':') {
            *lexer = unlex;
            return NULL;
        }

        t = lex(lexer);
        if(t != '[') {
            *lexer = unlex;
            return NULL;
        }
    }

    AST_array_literal *array_ast = (AST_array_literal*)job_alloc_ast(jp, AST_KIND_array_literal);
    array_ast->base.loc = lexer->loc;
    array_ast->type = type_expr;

    AST_expr_list head;
    AST_expr_list *elem_list = &head;

    while(t != ']') {
        AST *elem = parse_rvalue(jp);

        if(!elem) {
            t = lex(lexer);
            if(t != ']') {
                job_error(jp, lexer->loc, "expected ']' at end of array literal");
                return NULL;
            }
        }

        array_ast->n_elements++;

        elem_list->next = (AST_expr_list*)job_alloc_ast(jp, AST_KIND_expr_list);
        elem_list = elem_list->next;
        elem_list->expr = elem;

        t = lex(lexer);
        if(t != ',' && t != ']') {
            job_error(jp, lexer->loc, "expected ']' or ','");
            return NULL;
        }

        if(t == ',') {
            unlex = *lexer;
            t = lex(lexer);

            if(t != ']') {
                *lexer = unlex;
            }
        }
    }

    array_ast->elements = head.next;

    return (AST*)array_ast;
}

AST* parse_struct_lit(Job *jp) {
    UNIMPLEMENTED;

    Lexer *lexer = jp->lexer;
    Lexer unlex = *lexer;
    Token t = lex(lexer);

    if(t != '{' && t != TOKEN_IDENT) {
        *lexer = unlex;
        return NULL;
    }

    char *struct_name = NULL;
    char *s = lexer->text.s;
    char *e = lexer->text.e;

    if(t == TOKEN_IDENT) {
        t = lex(lexer);

        if(t != ':') {
            *lexer = unlex;
            return NULL;
        }

        t = lex(lexer);

        if(t != '{') {
            *lexer = unlex;
            return NULL;
        }

        struct_name = job_alloc_text(jp, s, e);
    }

    AST_struct_literal *struct_ast = (AST_struct_literal*)job_alloc_ast(jp, AST_KIND_struct_literal);
    struct_ast->name = struct_name;

    unlex = *lexer;
    t = lex(lexer);

    AST_member_list head;
    AST_member_list *list = &head;

    while(t != '}') {
        if(t != '.') {
            job_error(jp, lexer->loc, "expected '.' to begin member");
            return NULL;
        }

        unlex = *lexer;
        t = lex(lexer);

        if(t != TOKEN_IDENT) {
            job_error(jp, lexer->loc, "expected identifier");
            return NULL;
        }

        list->next = (AST_member_list*)job_alloc_ast(jp, AST_KIND_member_list);
        list = list->next;
        list->name = job_alloc_text(jp, lexer->text.s, lexer->text.e);
        list->value = parse_rvalue(jp);

        t = lex(lexer);

        if(t != '}' && t != ',') {
            job_error(jp, lexer->loc, "expected '}' or ','");
            return NULL;
        }

        t = lex(lexer);
    }

    return (AST*)struct_ast;
}

AST* parse_rvalue(Job *jp) {
    AST *node = parse_array_lit(jp);
    //TODO if(!node) node = parse_struct_lit(jp);
    if(!node) node = parse_expr(jp);

    if(!node) {
        job_error(jp, jp->lexer->loc, "expected array literal, struct literal or expression");
    }

    return node;
}

void print_value(Value *v) {
    if(!v) return;
    switch(v->kind) {
        case VALUE_KIND_NIL:
            printf("nil");
            break;
        case VALUE_KIND_BOOL:
            printf("%s", v->val.boolean ? "true" : "false");
            break;
        case VALUE_KIND_CHAR:
            printf("'%c'", v->val.character);
            break;
        case VALUE_KIND_INT:
            printf("%li", v->val.integer);
            break;
        case VALUE_KIND_UINT:
            printf("%lu", v->val.uinteger);
            break;
        case VALUE_KIND_FLOAT:
            printf("%f", v->val.floating);
            break;
        case VALUE_KIND_TYPE:
            printf("%s (%zu bytes, %zu align)", global_type_to_str(v->val.type), v->val.type->bytes, v->val.type->align);
            break;
        case VALUE_KIND_STRING:
            printf("%s", v->val.str.data);
            break;
        case VALUE_KIND_ARRAY:
            printf("[ ");
            if(v->val.array.n > 0) {
                for(u64 i = 0; i < v->val.array.n - 1; ++i) {
                    print_value(v->val.array.elements[i]);
                    printf(", ");
                }
                print_value(v->val.array.elements[v->val.array.n - 1]);
            }
            printf(" ]");
            break;
        case VALUE_KIND_STRUCT:
            UNIMPLEMENTED;
            break;
        case VALUE_KIND_PARAM:
            UNIMPLEMENTED;
            break;
        case VALUE_KIND_TOKEN:
            printf("%s", token_debug[v->val.token]);
            break;
    }
}

void print_ast_expr(AST *expr, int indent) {
    AST_expr *e;
    AST_atom *a;
    AST_call *c;
    AST_array_literal *l;
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
        case AST_KIND_array_literal:
            l = (AST_array_literal*)expr;
            for(int i = 0; i < indent; ++i) printf("  ");
            printf("array_literal\n");
            print_ast_expr(l->type, indent + 1);
            for(AST_expr_list *element = l->elements; element; element = element->next) {
                for(int i = 0; i < indent; ++i) printf("  ");
                printf("element\n");
                print_ast_expr(element->expr, indent + 1);
            }
            break;
    }
}

void ir_gen_entry_point_preamble(Job *jp) {
    UNIMPLEMENTED;
}

void ir_gen(Job *jp) {
    AST *node = jp->root;

    if(node->kind == AST_KIND_procdecl) {
        AST_procdecl *ast_proc = (AST_procdecl*)node;

        if(ast_proc->body == NULL) {
            if(ast_proc->is_foreign && ast_proc->c_call)
                ir_gen_foreign_proc_x64(jp);
            return;
        }

        Sym *sym = ast_proc->symbol_annotation;
        Type *proc_type = sym->type;

        if(sym->ir_generated) {
            for(int i = 0; i < arrlen(jp->run_dependencies); ++i) {
                //TODO more structured dependency checking to avoid bugs like not compiling recursive procs
                Sym *dep = jp->run_dependencies[i];
                if(dep != sym && dep->ready_to_run == false) {
                    jp->waiting_on_name = dep->name;
                    jp->waiting_on_id = dep->declared_by;
                    jp->state = JOB_STATE_WAIT;
                    return;
                }
            }

            arrfree(jp->run_dependencies);
            sym->ready_to_run = true;
            return;
        }

        assert(proc_type->kind == TYPE_KIND_PROC);

        u64 local_offset = 0;

        {
            IRinst inst;
            inst = (IRinst) {
                .opcode = IROP_GETCONTEXTARG,
                    .getcontextarg = { .reg_dest = 0, },
            };
            arrpush(jp->instructions, inst);

            inst = (IRinst) {
                .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = 0,
                        .reg_src = 0,
                        .bytes = 8,
                    },
            };
            arrpush(jp->instructions, inst);
            local_offset += 8;
        }

        int non_scalar_returns = 0;

        for(u64 i = 0; i < proc_type->proc.ret.n; ++i) {
            Type *ret_type = proc_type->proc.ret.types[i];
            if(TYPE_KIND_IS_NOT_SCALAR(ret_type->kind)) {
                IRinst inst_read = {
                    .opcode = IROP_GETARG,
                    .getport = {
                        .reg_dest = 0,
                        .bytes = 8,
                        .port = non_scalar_returns,
                        .c_call = ast_proc->c_call,
                    },
                };

                IRinst inst_write = {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = local_offset,
                        .reg_src = 0,
                        .bytes = 8,
                    },
                };

                arrpush(jp->instructions, inst_read);
                arrpush(jp->instructions, inst_write);

                local_offset += 8;
                non_scalar_returns++;
            }
        }

        for(AST_paramdecl *p = ast_proc->params; p; p = p->next) {
            Sym *p_sym = p->symbol_annotation;
            p_sym->segment_offset = local_offset;
            u64 p_bytes = TYPE_KIND_IS_NOT_SCALAR(p_sym->type->kind) ? 8LU : p_sym->type->bytes;
            local_offset += p_bytes;

            bool is_float = (p_sym->type->kind >= TYPE_KIND_FLOAT && p_sym->type->kind <= TYPE_KIND_F64);

            if(TYPE_KIND_IS_NOT_SCALAR(p_sym->type->kind)) {
                IRinst inst_read = {
                    .opcode = IROP_GETARG,
                    .getport = {
                        .reg_dest = 0,
                        .bytes = 8,
                        .port = non_scalar_returns + p->index,
                        .c_call = ast_proc->c_call,
                    },
                };

                IRinst inst_write = {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = p_sym->segment_offset,
                        .reg_src = 0,
                        .bytes = 8,
                    },
                };

                arrpush(jp->instructions, inst_read);
                arrpush(jp->instructions, inst_write);
            } else {
                IRinst inst_read = {
                    .opcode = is_float ? IROP_GETARGF : IROP_GETARG,
                    .getport = {
                        .reg_dest = 0,
                        .bytes = p_bytes,
                        .port = non_scalar_returns + p->index,
                        .c_call = ast_proc->c_call,
                    },
                };

                IRinst inst_write = {
                    .opcode = is_float ? IROP_SETVARF : IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = p_sym->segment_offset,
                        .reg_src = 0,
                        .bytes = p_bytes,
                    },
                };

                arrpush(jp->instructions, inst_read);
                arrpush(jp->instructions, inst_write);
            }
        }

        jp->cur_proc_type = proc_type;

        jp->label_alloc = 1;
        jp->reg_alloc = 0;

        assert(arrlen(jp->local_offset) == 0);
        arrpush(jp->local_offset, local_offset);

        AST_block *body = (AST_block*)(ast_proc->body);

        ir_gen_block(jp, body->down);

        if(jp->max_local_offset < arrlast(jp->local_offset))
            jp->max_local_offset = arrlast(jp->local_offset);

        arrsetlen(jp->local_offset, 0);

        if(jp->state == JOB_STATE_ERROR) return;

        if(proc_type->proc.ret.n == 0) {
            IRinst end = { .opcode = IROP_RET };
            arrpush(jp->instructions, end);
        }

        u64 *jump_table = global_alloc_scratch(sizeof(u64) * jp->label_alloc);

        for(int i = 0; i < arrlen(jp->instructions); ++i) {
            IRinst *inst = jp->instructions + i;
            if(inst->opcode == IROP_LABEL)
                jump_table[inst->label.id] = i + 1;
        }
        jp->label_alloc = 0;

        u64 n_instructions = arrlen(jp->instructions);

        IRinst *instructions = global_alloc_scratch(sizeof(IRinst) * n_instructions);
        memcpy(instructions, jp->instructions, n_instructions * sizeof(IRinst));

        arrsetlen(jp->instructions, 0);

        assert(sym->is_foreign == false);

        IRproc proc_data = {
            .procid = sym->procid,
            .name = sym->name,
            .is_foreign = false,
            .instructions = instructions,
            .n_instructions = n_instructions,
            .jump_table = jump_table,
            .local_segment_size = jp->max_local_offset,
        };

        //printf("%s max_local_offset: %lu\n", sym->name, jp->max_local_offset);
        //procedure_table_add(proc, sym->procid, sym->name, length, jp->max_local_offset, jump_table);
        proc_table_add(sym->procid, proc_data);

        sym->ir_generated = true;

        for(int i = 0; i < arrlen(jp->run_dependencies); ++i) {
            Sym *dep = jp->run_dependencies[i];
            if(dep != sym && dep->ready_to_run == false) {
                jp->state = JOB_STATE_WAIT;
                return;
            }
        }

        arrfree(jp->run_dependencies);
        sym->ready_to_run = true;

        if(proc_type->proc.is_polymorphic) {
            sym->being_polymorphed_by_jobid = jp->parent_job;
            body->down = jp->save_polymorphic_proc_body;
        }

    } else if(node->kind == AST_KIND_vardecl) {
        AST_vardecl *ast_global = (AST_vardecl*)node;
        Sym *sym = ast_global->symbol_annotation;

        if(sym->constant) return;

        if(sym->value) {
            sym->segment = IRSEG_GLOBAL;
            sym->segment_offset = global_segment_offset;
            arrpush(global_data_table, sym);
            global_segment_offset += sym->type->bytes;
        } else {
            sym->segment = IRSEG_BSS;
            sym->segment_offset = bss_segment_offset;
            arrpush(bss_data_table, sym);
            bss_segment_offset += sym->type->bytes;
        }

    } else if(node->kind == AST_KIND_structdecl) {
        printf("no IR to generate\n");
    } else if(node->kind == AST_KIND_uniondecl) {
        printf("no IR to generate\n");
    } else {
        UNREACHABLE;
    }
}

// cc -shared -o libfoo.so foo.c
// cc -dynamiclib -o libfoo.dylib foo.c -install_name @rpath/libfoo.dylib
// cc -dynamiclib -o jcc_wrap_foo.dylib jcc_wrap_foo.c -L'.' -lfoo -install_name @rpath/jcc_wrap_foo.dylib -Wl,-rpath,@loader_path

void ir_gen_foreign_proc_x64(Job *jp) {
    Arena_save scratch_save = arena_to_save(jp->allocator.scratch);

    AST_procdecl *ast_proc = (AST_procdecl*)(jp->root);
    bool is_system = ast_proc->is_system;

    char *foreign_lib_str = ast_proc->foreign_lib_str;

    char *dynamic_lib_path;
    char *static_lib_path; 
    char *header_path;

    if(is_system) {
        dynamic_lib_path = NULL;
        static_lib_path = NULL;
        header_path = NULL;
    } else {
        if(!DirectoryExists(ast_proc->foreign_lib_str)) {
            job_error(jp, ast_proc->base.loc, "couldn't find foreign library '%s', no such directory",
                    ast_proc->foreign_lib_str);
            return;
        }

#if defined(__MACH__)
        dynamic_lib_path = job_sprint(jp, "%s/lib%s.dylib", foreign_lib_str, foreign_lib_str);
#else
        dynamic_lib_path = job_sprint(jp, "%s/lib%s.so", foreign_lib_str, foreign_lib_str);
#endif

        static_lib_path = job_sprint(jp, "%s/lib%s.a", foreign_lib_str, foreign_lib_str);
        header_path = job_sprint(jp, "%s/%s.h", foreign_lib_str, foreign_lib_str);

        if(!FileExists(dynamic_lib_path)) {
            job_error(jp, ast_proc->base.loc,
                    "couldn't find '%s', this file must exist in order to use the foreign library '%s'",
                    dynamic_lib_path, foreign_lib_str);
            return;
        }

        if(!FileExists(static_lib_path)) {
            job_error(jp, ast_proc->base.loc,
                    "couldn't find '%s', this file must exist in order to use the foreign library '%s'",
                    static_lib_path, foreign_lib_str);
            return;
        }

        if(!FileExists(header_path)) {
            job_error(jp, ast_proc->base.loc,
                    "couldn't find '%s', this file must exist in order to use the foreign library '%s'",
                    header_path, foreign_lib_str);
            return;
        }

    }

    char *wrapper_preamble =
        "#include <stdint.h>\n"
        "#include <stdbool.h>\n"
        "#include <stddef.h>\n"
        "#define Arr(T) T *\n"
        "typedef int64_t s64;\n"
        "typedef uint64_t u64;\n"
        "typedef int32_t s32;\n"
        "typedef uint32_t u32;\n"
        "typedef int16_t s16;\n"
        "typedef uint16_t u16;\n"
        "typedef int8_t s8;\n"
        "typedef uint8_t u8;\n"
        "typedef float f32;\n"
        "typedef double f64;\n"
        "typedef union IRvalue {\n"
        "    u64 integer;\n"
        "    f32 floating32;\n"
        "    f64 floating64;\n"
        "} IRvalue;\n"
        "typedef struct IRmachine {\n"
#define X(x) #x "\n"
        IRMACHINE_BODY
#undef X
        "} IRmachine;\n";

    Sym *proc_sym = ast_proc->symbol_annotation;
    Type *proc_type = proc_sym->type;

    if(proc_type->proc.ret.n > 1) {
        job_error(jp, ast_proc->base.loc, "foreign procedures cannot have multiple return values");
        return;
    }

    char *wrapper_name = job_sprint(jp, "__jcc__wrap__%s", proc_sym->name);
    char *wrapper_path = job_sprint(jp, "%s.c", wrapper_name);

#if defined(__MACH__)
    char *wrapper_dl_path = job_sprint(jp, "%s.dylib", wrapper_name);
#else
    char *wrapper_dl_path = job_sprint(jp, "%s.so", wrapper_name);
#endif

    FILE *wrapper_file = fopen(wrapper_path, "w");
    int has_non_scalar_return =
        (proc_type->proc.ret.n > 0) && TYPE_KIND_IS_NOT_SCALAR(proc_type->proc.ret.types[0]->kind);

    if(is_system) {
        char *system_preamble =
            "#include <assert.h>\n"
            "#include <ctype.h>\n"
            "#include <errno.h>\n"
            "#include <float.h>\n"
            "#include <limits.h>\n"
            "#include <locale.h>\n"
            "#include <math.h>\n"
            "#include <setjmp.h>\n"
            "#include <signal.h>\n"
            "#include <stdarg.h>\n"
            "#include <stddef.h>\n"
            "#include <stdio.h>\n"
            "#include <stdlib.h>\n"
            "#include <string.h>\n"
            "#include <time.h>\n"
            "\n"
            "#include <stdbool.h>\n"
            "#include <stdint.h>\n"
            "#include <inttypes.h>\n"
            "#include <tgmath.h>\n"
            "#include <complex.h>\n"
            "#include <fenv.h>\n"
            "\n"
            "#include <unistd.h>\n"
            "#include <sys/types.h>\n"
            "#include <sys/stat.h>\n"
            "#include <fcntl.h>\n"
            "#include <dirent.h>\n"
            "#include <pthread.h>\n"
            "#include <semaphore.h>\n"
            "#include <errno.h>\n"
            "#include <dlfcn.h>\n"
            "#include <netdb.h>\n"
            "#include <netinet/in.h>\n"
            "#include <arpa/inet.h>\n"
            "#include <sys/socket.h>\n"
            "#include <sys/time.h>\n"
            "#include <sys/wait.h>\n"
            "#include <pwd.h>\n"
            "#include <grp.h>\n"
            "\n"
            "#define Arr(T) T *\n"
            "typedef int64_t s64;\n"
            "typedef uint64_t u64;\n"
            "typedef int32_t s32;\n"
            "typedef uint32_t u32;\n"
            "typedef int16_t s16;\n"
            "typedef uint16_t u16;\n"
            "typedef int8_t s8;\n"
            "typedef uint8_t u8;\n"
            "typedef float f32;\n"
            "typedef double f64;\n"
            "typedef union IRvalue {\n"
            "    u64 integer;\n"
            "    f32 floating32;\n"
            "    f64 floating64;\n"
            "} IRvalue;\n"
            "typedef struct IRmachine {\n"
#define X(x) #x "\n"
            IRMACHINE_BODY
#undef X
            "} IRmachine;\n";

        fprintf(wrapper_file, "%s\nvoid %s(IRmachine *interp) {\n", system_preamble, wrapper_name);
    } else {
        fprintf(wrapper_file, "%s\n#include \"%s\"\nvoid %s(IRmachine *interp) {\n", wrapper_preamble, header_path, wrapper_name);
    }

    assert(proc_sym->is_foreign);

    int port_index = 0;

    for(int i = 0; i < proc_type->proc.param.n; ++i) {
        char *param_name = proc_type->proc.param.names[i];
        Type *param_type = proc_type->proc.param.types[i];

        char *param_ctype_str;
        if(TYPE_KIND_IS_RECORD(param_type->kind)) {
            //TODO is it better to not require the C header file and generate it from our code?
            // 
            // its better to generate the necessary headers from our own code, headers take time
            // to compile
            //
            // yes it is definitely better, but then again, it would be better to generate the wrapper in assembly
            // so until we do that it will use C
            assert(param_type->record.name);
            param_ctype_str =
                job_sprint(jp, "%s %s",
                        (param_type->kind == TYPE_KIND_UNION) ? "union" : "struct",
                        param_type->record.name);
        } else {
            param_ctype_str = job_type_to_ctype_str(jp, param_type);
        }

        port_index = i;

        fprintf(wrapper_file, "%s %s;\n", param_ctype_str, param_name);

        if(TYPE_KIND_IS_NOT_SCALAR(param_type->kind)) {
            fprintf(wrapper_file,
                    "%s = *(%s*)(void*)(interp->ports[%i].integer);\n",
                    param_name, param_ctype_str, port_index + has_non_scalar_return);
        } else {
            if(param_type->kind == TYPE_KIND_F64) {
                fprintf(wrapper_file,
                        "%s = interp->ports[%i].floating64;\n",
                        param_name, port_index + has_non_scalar_return);
            } else if(TYPE_KIND_IS_FLOAT(param_type->kind)) {
                fprintf(wrapper_file,
                        "%s = interp->ports[%i].floating32;\n",
                        param_name, port_index + has_non_scalar_return);
            } else {
                fprintf(wrapper_file,
                        "%s = (%s)(interp->ports[%i].integer);\n",
                        param_name, param_ctype_str, port_index + has_non_scalar_return);
            }
            ++port_index;
        }
    }

    if(proc_type->proc.ret.n == 0) {
        fprintf(wrapper_file, "%s(", proc_sym->name);
        int i;
        if(proc_type->proc.param.n == 0) {
            fprintf(wrapper_file, ");\n");
        } else {
            for(i = 0; i < proc_type->proc.param.n - 1; ++i) {
                char *param_name = proc_type->proc.param.names[i];
                fprintf(wrapper_file, "%s, ", param_name);
            }
            char *param_name = proc_type->proc.param.names[i];
            fprintf(wrapper_file, "%s);\n", param_name);
        }
    } else {
        Type *ret_type = proc_type->proc.ret.types[0];

        if(TYPE_KIND_IS_NOT_SCALAR(ret_type->kind)) {
            char *ret_ctype_str =
                job_sprint(jp, "%s %s",
                        (ret_type->kind == TYPE_KIND_UNION) ? "union" : "struct",
                        ret_type->record.name);
            fprintf(wrapper_file, "%s r = %s(", ret_ctype_str, proc_sym->name);
            int i;
            if(proc_type->proc.param.n == 0) {
                fprintf(wrapper_file, ");\n");
            } else {
                for(i = 0; i < proc_type->proc.param.n - 1; ++i) {
                    char *param_name = proc_type->proc.param.names[i];
                    fprintf(wrapper_file, "%s, ", param_name);
                }
                char *param_name = proc_type->proc.param.names[i];
                fprintf(wrapper_file, "%s);\n", param_name);
            }

            fprintf(wrapper_file, "*(%s*)(void*)(interp->ports[0].integer) = r;\n", ret_ctype_str);
        } else {
            char *ret_ctype_str = job_type_to_ctype_str(jp, ret_type);
            fprintf(wrapper_file, "%s r = %s(", ret_ctype_str, proc_sym->name);
            int i;
            if(proc_type->proc.param.n == 0) {
                fprintf(wrapper_file, ");\n");
            } else {
                for(i = 0; i < proc_type->proc.param.n - 1; ++i) {
                    char *param_name = proc_type->proc.param.names[i];
                    fprintf(wrapper_file, "%s, ", param_name);
                }
                char *param_name = proc_type->proc.param.names[i];
                fprintf(wrapper_file, "%s);\n", param_name);
            }

            char *port_field = "integer";
            char *cast_str = "(u64)";

            if(ret_type->kind == TYPE_KIND_F64) {
                port_field = "floating64";
                cast_str = "(f64)";
            } else if(TYPE_KIND_IS_FLOAT(ret_type->kind)) {
                port_field = "floating32";
                cast_str = "(f32)";
            }

            fprintf(wrapper_file, "interp->ports[0].%s = %sr;\n", port_field, cast_str);
        }
    }

    fprintf(wrapper_file, "}\n");
    fclose(wrapper_file);

    char *wrapper_compile;

#if defined(__MACH__)
    if(is_system) {
        wrapper_compile = job_sprint(jp,
                "cc -g -dynamiclib -o %s %s -install_name @rpath/%s",
                wrapper_dl_path,
                wrapper_path,
                wrapper_dl_path);
    } else {
        wrapper_compile = job_sprint(jp,
                "cc -g -dynamiclib -o %s %s -L'%s' -l'%s' -install_name @rpath/%s -Wl,-rpath,'%s'",
                wrapper_dl_path,
                wrapper_path,
                foreign_lib_str,
                foreign_lib_str,
                wrapper_dl_path,
                foreign_lib_str);
    }
#else
    if(is_system) {
        wrapper_compile = job_sprint(jp,
                "cc -g -shared -o %s %s",
                wrapper_dl_path,
                wrapper_path);
    } else {
        wrapper_compile = job_sprint(jp,
                "cc -g -shared -o %s %s -L'%s' -l'%s' -Wl,-rpath,'%s'",
                wrapper_dl_path,
                wrapper_path,
                foreign_lib_str,
                foreign_lib_str,
                foreign_lib_str);
    }
#endif

    assert(system(wrapper_compile) == 0);
    assert(FileExists(wrapper_dl_path));

    char full_wrapper_dl_path[PATH_MAX];
    assert(realpath(wrapper_dl_path, full_wrapper_dl_path));

    void *wrapper_module = dlopen(full_wrapper_dl_path, RTLD_NOW);
    printf("%s\n", dlerror());
    assert(wrapper_module);
    IR_foreign_proc foreign_proc = (IR_foreign_proc)dlsym(wrapper_module, wrapper_name);

    if(!foreign_proc) {
        assert("didn't find symbol"&&0);
    }

    IRproc proc_data = {
        .procid = proc_sym->procid,
        .name = proc_sym->name,
        .is_foreign = true,
        .foreign_proc = foreign_proc,
        .wrapper_dll = wrapper_module,
    };

    proc_table_add(proc_sym->procid, proc_data);

    proc_sym->ready_to_run = true;

    arena_from_save(jp->allocator.scratch, scratch_save);
}

void ir_gen_C(Job *jp, IRproc irproc, Type *proc_type) {
    char *IR_C_ireg_map[] = { "reg0",  "reg1",  "reg2",  "reg3",  "reg4",  "reg5",  };
    char *IR_C_f32reg_map[] = { "f32reg0",  "f32reg1",  "f32reg2",  "f32reg3",  "f32reg4",  "f32reg5",  };
    char *IR_C_f64reg_map[] = { "f64reg0",  "f64reg1",  "f64reg2",  "f64reg3",  "f64reg4",  "f64reg5",  };

    fprintf(stderr, "\ngenerating C code for %s procid %i\n\n", irproc.name, irproc.procid);

    if(irproc.is_foreign) {
        return;
    }

    Arr(u32) float32_constants = NULL;
    Arr(u64) float64_constants = NULL;

    u64 highest_argument_index = proc_type->proc.param.n;
    u64 highest_return_index = 0;

    IRinst *instructions = irproc.instructions;
    u64 n_instructions = irproc.n_instructions;

    char line_buf[256];
    u64 code_buf_cap = 4096;
    u64 code_buf_used = 0;
    char *code_buf = malloc(code_buf_cap);

    bool is_foreign = false;
    bool generating_call = false;
    Type *proc_type_of_generated_call = NULL;

    for(int i = 0; i < n_instructions; ++i) {
        u64 n_written = 0;
        IRinst inst = instructions[i];

        switch(inst.opcode) {
            default:
                UNREACHABLE;
            case IROP_NOOP:
                break;
            case IROP_MUL:
            case IROP_DIV:
            case IROP_MOD:
            case IROP_LSHIFT:
            case IROP_RSHIFT:
            case IROP_EQ:
            case IROP_NE:
            case IROP_LE:
            case IROP_GT:
            case IROP_ADD:
            case IROP_SUB:
            case IROP_AND:
            case IROP_OR:
            case IROP_XOR:
                {
                    char *operator = NULL;

                    int dest_bytes = inst.arith.operand_bytes[0];
                    int a_bytes = inst.arith.operand_bytes[1];
                    int b_bytes = inst.arith.operand_bytes[2];

                    int dest_reg = inst.arith.reg[0];
                    int a_reg = inst.arith.reg[1];
                    int b_reg = inst.arith.reg[2];

                    switch(inst.opcode) {
#define X(x, op) case IROP_##x: \
                        operator = #op;
                        break;
                        IR_INT_BINOPS;
#undef X
                    }

                    char sign = (inst.arith.sign) ? 's' : 'u';
                    if(inst.arith.immediate) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (%c%i)((%c%i)%s %s (%c%i)(0x%lx));\n",
                                IR_C_ireg_map[dest_reg],
                                sign, dest_bytes << 3l,
                                sign, a_bytes << 3l,
                                IR_C_ireg_map[a_reg],
                                operator,
                                sign, b_bytes << 3l,
                                inst.arith.imm.integer);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (%c%i)((%c%i)%s %s (%c%i)(%s));\n",
                                IR_C_ireg_map[dest_reg],
                                sign, dest_bytes << 3l,
                                sign, a_bytes << 3l,
                                IR_C_ireg_map[a_reg],
                                operator,
                                sign, b_bytes << 3l,
                                IR_C_ireg_map[b_reg]);
                    }
                }
                break;   
            case IROP_NEG:
            case IROP_NOT:
                {
                    assert(inst.arith.operand_bytes[0] == inst.arith.operand_bytes[1]);

                    int reg = inst.arith.reg[0];
                    int bytes = inst.arith.operand_bytes[0];
                    char op = (inst.opcode == IROP_NOT) ? '~' : '-';

                    char sign = (inst.arith.sign) ? 's' : 'u';

                    if(inst.arith.immediate) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = %c(%c%i)0x%lx;\n",
                                IR_C_ireg_map[reg],
                                op,
                                sign, bytes << 3l,
                                inst.arith.imm.integer);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = %c(%c%i)%s;\n",
                                IR_C_ireg_map[reg],
                                op,
                                sign, bytes << 3l,
                                IR_C_ireg_map[reg]);
                    }
                }
                break;

            case IROP_FADD:
            case IROP_FSUB:
            case IROP_FMUL:
            case IROP_FDIV:
                {
                    char *operator = NULL;

                    int dest_bytes = inst.arith.operand_bytes[0];
                    int a_bytes = inst.arith.operand_bytes[1];
                    int b_bytes = inst.arith.operand_bytes[2];

                    assert(dest_bytes == a_bytes && a_bytes == b_bytes);

                    int dest_reg = inst.arith.reg[0];
                    int a_reg = inst.arith.reg[1];
                    int b_reg = inst.arith.reg[2];

                    char **freg_map = (dest_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;

                    switch(inst.opcode) {
#define X(x, op) case IROP_##x: \
                        operator = #op;
                        break;
                        IR_FLOAT_BINOPS;
#undef X
                    }

                    if(inst.arith.immediate) {
                        int n_float_constants;

                        if(dest_bytes == 8) {
                            n_float_constants = arrlen(float64_constants);
                            arrpush(float64_constants, inst.arith.imm.integer);
                        } else {
                            n_float_constants = arrlen(float32_constants);
                            arrpush(float32_constants, (u32)inst.arith.imm.integer);
                        }

                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (f%i)((f%i)%s %s (*(f%i*)&_jcc_internal_float%i_constant_%i);\n",
                                freg_map[dest_reg],
                                dest_bytes << 3l,
                                a_bytes << 3l,
                                freg_map[a_reg],
                                operator,
                                b_bytes << 3l,
                                b_bytes << 3l,
                                n_float_constants);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (f%i)((f%i)%s %s (f%i)(%s));\n",
                                freg_map[dest_reg],
                                dest_bytes << 3l,
                                a_bytes << 3l,
                                freg_map[a_reg],
                                operator,
                                b_bytes << 3l,
                                freg_map[b_reg]);
                    }
                }
                break;

            case IROP_FNEG:
                {
                    assert(inst.arith.operand_bytes[0] == inst.arith.operand_bytes[1]);

                    int reg = inst.arith.reg[0];
                    int bytes = inst.arith.operand_bytes[0];

                    if(inst.arith.immediate) {
                        int n_float_constants;

                        if(bytes == 8) {
                            n_float_constants = arrlen(float64_constants);
                            arrpush(float64_constants, inst.arith.imm.integer);
                        } else {
                            n_float_constants = arrlen(float32_constants);
                            arrpush(float32_constants, (u32)inst.arith.imm.integer);
                        }

                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "reg%i = -(*(f%i*)&_jcc_internal_float%i_constant_%i);\n",
                                reg,
                                bytes << 3l,
                                bytes << 3l,
                                n_float_constants);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "f%ireg%i = -f%ireg%i;\n",
                                bytes << 3l,
                                reg,
                                bytes << 3l,
                                reg);
                    }
                }
                break;

            case IROP_FEQ:
            case IROP_FNE:
            case IROP_FLE:
            case IROP_FGT:
                {
                    char *operator = NULL;

                    int dest_bytes = inst.arith.operand_bytes[0];
                    assert(dest_bytes == 1);
                    int a_bytes = inst.arith.operand_bytes[1];
                    int b_bytes = inst.arith.operand_bytes[2];

                    int dest_reg = inst.arith.reg[0];
                    int a_reg = inst.arith.reg[1];
                    int b_reg = inst.arith.reg[2];

                    char **freg_map = (dest_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;

                    switch(inst.opcode) {
#define X(x, op) case IROP_##x: \
                        operator = #op;
                        break;
                        IR_FLOAT_CMPOPS;
#undef X
                    }

                    if(inst.arith.immediate) {
                        int n_float_constants;

                        if(dest_bytes == 8) {
                            n_float_constants = arrlen(float64_constants);
                            arrpush(float64_constants, inst.arith.imm.integer);
                        } else {
                            n_float_constants = arrlen(float32_constants);
                            arrpush(float32_constants, (u32)inst.arith.imm.integer);
                        }

                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "reg%i = (bool)((f%i)%s %s (*(f%i*)&_jcc_internal_float%i_constant_%i));\n",
                                dest_reg,
                                a_bytes << 3l,
                                freg_map[a_reg],
                                operator,
                                b_bytes << 3l,
                                b_bytes << 3l,
                                n_float_constants);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (bool)((f%i)%s %s (f%i)(%s));\n",
                                IR_C_ireg_map[dest_reg],
                                a_bytes << 3l,
                                freg_map[a_reg],
                                operator,
                                b_bytes << 3l,
                                freg_map[b_reg]);
                    }
                }
                break;

            case IROP_LABEL:
                n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                        "%s_label_%lx:\n",
                        irproc.name, inst.label.id);
                break;
            case IROP_IF:
            case IROP_IFZ:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "if((bool)%s == (bool)%i) goto %s_label_%lx;\n",
                        IR_C_ireg_map[inst.branch.cond_reg],
                        (inst.opcode == IROP_IF) ? true : false,
                        irproc.name, inst.branch.label_id);
                break;
            case IROP_JMP:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "goto %s_label_%lx;\n",
                        irproc.name, inst.branch.label_id);
                break;

            case IROP_GETCONTEXTARG:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "(void*)reg%lu = context_pointer;\n",
                        inst.getcontextarg.reg_dest);
                break;
            case IROP_SETCONTEXTARG:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "context_pointer = (void*)reg%lu;\n",
                        inst.setcontextarg.reg_src);
                break;

            case IROP_HINT_BEGIN_FOREIGN_CALL:
            case IROP_HINT_BEGIN_CALL:
                assert(!generating_call);
                is_foreign = (inst.opcode == IROP_HINT_BEGIN_FOREIGN_CALL);
                generating_call = true;
                proc_type_of_generated_call = inst.hint.proc_type;
                break;

            case IROP_HINT_END_FOREIGN_CALL:
            case IROP_HINT_END_CALL:
                assert(generating_call);
                generating_call = false;
                is_foreign = false;
                proc_type_of_generated_call = NULL;
                break;

            case IROP_HINT_BEGIN_PASS_NON_SCALAR:
            case IROP_HINT_END_PASS_NON_SCALAR:
                //n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                //        "/* %s */\n",
                //        IRop_debug[inst.opcode]);
                break;

            case IROP_CALL:
                {
                    assert(generating_call);

                    char *proc_name = proc_type_of_generated_call->proc.name;

                    if(inst.call.immediate == false) {
                        if(is_foreign) {
                            UNIMPLEMENTED;
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf), "{\n");

                            proc_name = "proc_pointer";
                            if(proc_type_of_generated_call->proc.ret.n == 1) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "union _Port (*proc_pointer)(void*, ");
                                for(u64 i = 0; i < proc_type_of_generated_call->proc.param.n; ++i) {
                                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                            "union _Port, ");
                                }

                                n_written -= 1;
                                line_buf[n_written-1] = ')';

                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        ";\n");
                            } else {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "void (*proc_pointer)(void*, ");

                                if(proc_type_of_generated_call->proc.ret.n > 1) {
                                    for(u64 i = 0; i < proc_type_of_generated_call->proc.ret.n; ++i) {
                                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                                "union _Port*, ");
                                    }
                                }

                                for(u64 i = 0; i < proc_type_of_generated_call->proc.param.n; ++i) {
                                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                            "union _Port, ");
                                }

                                n_written -= 1;
                                line_buf[n_written-1] = ')';

                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        " = (void*)(reg%lu);\n",
                                        inst.call.id_reg);
                            }
                        }
                    }

                    if(is_foreign) {
                        if(proc_type_of_generated_call->proc.ret.n == 1) {
                            if(TYPE_KIND_IS_RECORD(proc_type_of_generated_call->proc.ret.types[0]->kind)) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "*(%s %s*)(arg0.integer) = %s(",
                                        (proc_type_of_generated_call->proc.ret.types[0]->kind == TYPE_KIND_UNION)
                                        ? "union" : "struct",
                                        proc_type_of_generated_call->proc.ret.types[0]->record.name, proc_name);
                            } else if(proc_type_of_generated_call->proc.ret.types[0]->kind == TYPE_KIND_F64) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "ret0.floating64 = %s(",
                                        proc_name);
                            } else if(TYPE_KIND_IS_FLOAT32(proc_type_of_generated_call->proc.ret.types[0]->kind)) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "ret0.floating32 = %s(",
                                        proc_name);
                            } else {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "ret0.integer = %s(",
                                        proc_name);
                            }
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s(", proc_name);
                        }

                        for(u64 i = 0; i < proc_type_of_generated_call->proc.param.n; ++i) {
                            Type *t = proc_type_of_generated_call->proc.param.types[i];

                            if(TYPE_KIND_IS_RECORD(t->kind)) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "*(%s %s*)(arg%lu.integer), ",
                                        (t->kind == TYPE_KIND_UNION)
                                        ? "union" : "struct",
                                        t->record.name,
                                        i);
                            } else if(t->kind == TYPE_KIND_F64) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "(f64)(arg%lu.floating64), ",
                                        i);
                            } else if(TYPE_KIND_IS_FLOAT32(t->kind)) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "(f32)(arg%lu.floating32), ",
                                        i);
                            } else {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "(%s)(arg%lu.integer), ",
                                        job_type_to_ctype_str(jp, t),
                                        i);
                            }
                        }

                        n_written -= 1;
                        line_buf[n_written-1] = ')';

                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                ";\n");
                    } else {
                        if(proc_type_of_generated_call->proc.ret.n == 1) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "ret0 = %s(context_pointer, ", proc_name);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s(context_pointer, ", proc_name);
                        }

                        if(proc_type_of_generated_call->proc.ret.n > 1) {
                            for(u64 i = 0; i < proc_type_of_generated_call->proc.ret.n; ++i) {
                                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                        "&ret%lu, ",
                                        i);
                            }
                        }

                        for(u64 i = 0; i < proc_type_of_generated_call->proc.param.n; ++i) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "arg%lu, ",
                                    i);
                        }

                        n_written -= 1;
                        line_buf[n_written-1] = ')';

                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                ";\n");
                    }

                    if(inst.call.immediate == false) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf), "};\n");
                    }
                }
                break;
            case IROP_RET:
                if(proc_type->proc.ret.n == 1) {
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "return ret0;\n");
                } else {
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "return;\n");
                }
                break;

            case IROP_SETARG:
            case IROP_SETRET:
                {
                    char *dest;
                    char *field_op;

                    if(inst.opcode == IROP_SETARG && inst.setport.port >= highest_argument_index) {
                        highest_argument_index = inst.setport.port;
                    } else if(inst.opcode == IROP_SETRET && inst.setport.port >= highest_return_index) {
                        highest_return_index = inst.setport.port;
                    }

                    if(inst.opcode == IROP_SETRET && proc_type->proc.ret.n != 1) {
                        dest = "retp";
                        field_op = "->";
                    } else if(inst.opcode == IROP_SETRET) {
                        dest = "ret";
                        field_op = ".";
                    } else {
                        dest = "arg";
                        field_op = ".";
                    }

                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s%lu%sinteger = (u%lu)reg%lu;\n",
                            dest,
                            inst.setport.port,
                            field_op,
                            inst.setport.bytes << 3lu,
                            inst.setport.reg_src);
                }
                break;
            case IROP_SETARGF:
            case IROP_SETRETF:
                {
                    char *dest;
                    char *field_op;

                    if(inst.opcode == IROP_SETARGF && inst.setport.port >= highest_argument_index) {
                        highest_argument_index = inst.setport.port;
                    } else if(inst.opcode == IROP_SETRETF && inst.setport.port >= highest_return_index) {
                        highest_return_index = inst.setport.port;
                    }

                    if(inst.opcode == IROP_SETRETF && proc_type->proc.ret.n != 1) {
                        dest = "retp";
                        field_op = "->";
                    } else if(inst.opcode == IROP_SETRETF) {
                        dest = "ret";
                        field_op = ".";
                    } else {
                        dest = "arg";
                        field_op = ".";
                    }

                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s%lu%sfloating%lu = (f%lu)f%lureg%lu;\n",
                            dest,
                            inst.setport.port,
                            field_op,
                            inst.setport.bytes << 3lu,
                            inst.setport.bytes << 3lu,
                            inst.setport.bytes << 3lu,
                            inst.setport.reg_src);
                }
                break;
            case IROP_GETARG:
            case IROP_GETRET:
                {
                    if(inst.opcode == IROP_GETARG && inst.getport.port >= highest_argument_index) {
                        highest_argument_index = inst.getport.port;
                    } else if(inst.opcode == IROP_GETRET && inst.getport.port >= highest_return_index) {
                        highest_return_index = inst.getport.port;
                    }

                    char *src = (inst.opcode == IROP_GETRET) ? "ret" : "arg";
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "reg%lu = (u%lu)(%s%lu.integer);\n",
                            inst.getport.reg_dest,
                            inst.getport.bytes << 3lu,
                            src,
                            inst.getport.port);
                }
                break;
            case IROP_GETARGF:
            case IROP_GETRETF:
                {
                    if(inst.opcode == IROP_GETARGF && inst.getport.port >= highest_argument_index) {
                        highest_argument_index = inst.getport.port;
                    } else if(inst.opcode == IROP_GETRETF && inst.getport.port >= highest_return_index) {
                        highest_return_index = inst.getport.port;
                    }

                    char *src = (inst.opcode == IROP_GETRETF) ? "ret" : "arg";
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "f%lureg%lu = (f%lu)(%s%lu.floating%lu);\n",
                            inst.getport.bytes << 3lu,
                            inst.getport.reg_dest,
                            inst.getport.bytes << 3lu,
                            src,
                            inst.getport.port,
                            inst.getport.bytes << 3lu);
                }
                break;


            case IROP_ADDRVAR:
                {
                    char *segment_name = NULL;
                    switch(inst.addrvar.segment) {
#define X(segment,name) case IRSEG_##segment: \
                        segment_name = name;  \
                        break;
                        IRSEGMENTS;
#undef X
                    }

                    if(inst.addrvar.segment == IRSEG_TYPE) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (u64)(void*)_type_info_%lx;\n",
                                IR_C_ireg_map[inst.addrvar.reg_dest],
                                inst.addrvar.offset);
                    } else if(inst.addrvar.segment == IRSEG_CODE) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "/* %s */\n",
                                IRop_debug[inst.opcode]);
                    } else if(inst.setvar.segment == IRSEG_GLOBAL || inst.setvar.segment == IRSEG_BSS) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (u64)(&%s);\n",
                                IR_C_ireg_map[inst.addrvar.reg_dest],
                                inst.setvar.sym->name);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = (u64)(((u8*)%s)+0x%lx);\n",
                                IR_C_ireg_map[inst.addrvar.reg_dest],
                                segment_name,
                                inst.addrvar.offset);
                    }
                }
                break;
            case IROP_GETVAR:
                {
                    char *segment_name = NULL;
                    switch(inst.getvar.segment) {
#define X(segment,name) case IRSEG_##segment: \
                        segment_name = name;  \
                        break;
                        IRSEGMENTS;
#undef X
                    }

                    if(inst.getvar.segment == IRSEG_TYPE) {
                        UNREACHABLE;
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "/* %s */\n",
                                IRop_debug[inst.opcode]);
                    } else if(inst.getvar.segment == IRSEG_CODE) {
                        UNREACHABLE;
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "/* %s */\n",
                                IRop_debug[inst.opcode]);
                    } else if(inst.setvar.segment == IRSEG_GLOBAL || inst.setvar.segment == IRSEG_BSS) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = *(u%lu*)(&%s);\n",
                                IR_C_ireg_map[inst.getvar.reg_dest],
                                inst.getvar.bytes << 3lu,
                                inst.getvar.sym->name);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "%s = *(u%lu*)(((u8*)%s)+0x%lx);\n",
                                IR_C_ireg_map[inst.getvar.reg_dest],
                                inst.getvar.bytes << 3lu,
                                segment_name,
                                inst.getvar.offset);
                    }
                }
                break;
            case IROP_SETVAR:
                {
                    char *segment_name = NULL;
                    switch(inst.setvar.segment) {
#define X(segment,name) case IRSEG_##segment: \
                        segment_name = name;  \
                        break;
                        IRSEGMENTS;
#undef X
                    }

                    if(inst.setvar.segment == IRSEG_TYPE) {
                        UNREACHABLE;
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "/* %s */\n",
                                IRop_debug[inst.opcode]);
                    } else if(inst.setvar.segment == IRSEG_CODE) {
                        UNREACHABLE;
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "/* %s */\n",
                                IRop_debug[inst.opcode]);
                    } else if(inst.setvar.segment == IRSEG_GLOBAL || inst.setvar.segment == IRSEG_BSS) {
                        if(inst.setvar.immediate) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(u%lu*)(&%s) = (u%lu)0x%lx;\n",
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.sym->name,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.imm.integer);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(u%lu*)(&%s) = (u%lu)%s;\n",
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.sym->name,
                                    inst.setvar.bytes << 3lu,
                                    IR_C_ireg_map[inst.setvar.reg_src]);
                        }
                    } else {
                        if(inst.setvar.immediate) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(u%lu*)(((u8*)%s)+0x%lx) = (u%lu)0x%lx;\n",
                                    inst.setvar.bytes << 3lu,
                                    segment_name,
                                    inst.setvar.offset,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.imm.integer);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(u%lu*)(((u8*)%s)+%lu) = (u%lu)%s;\n",
                                    inst.setvar.bytes << 3lu,
                                    segment_name,
                                    inst.setvar.offset,
                                    inst.setvar.bytes << 3lu,
                                    IR_C_ireg_map[inst.setvar.reg_src]);
                        }
                    }
                }
                break;
            case IROP_GETVARF:
                {
                    char *segment_name = NULL;
                    switch(inst.getvar.segment) {
#define X(segment,name) case IRSEG_##segment: \
                        segment_name = name;  \
                        break;
                        IRSEGMENTS;
#undef X
                    }

                    if(inst.setvar.segment == IRSEG_GLOBAL || inst.setvar.segment == IRSEG_BSS) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "f%lureg%lu = *(f%lu*)(&%s);\n",
                                inst.getvar.bytes << 3lu,
                                inst.getvar.reg_dest,
                                inst.getvar.bytes << 3lu,
                                inst.getvar.sym->name);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "f%lureg%lu = *(f%lu*)(((u8*)%s)+0x%lx);\n",
                                inst.getvar.bytes << 3lu,
                                inst.getvar.reg_dest,
                                inst.getvar.bytes << 3lu,
                                segment_name,
                                inst.getvar.offset);
                    }
                }
                break;
            case IROP_SETVARF:
                {
                    char *segment_name = NULL;
                    switch(inst.setvar.segment) {
#define X(segment,name) case IRSEG_##segment: \
                        segment_name = name;  \
                        break;
                        IRSEGMENTS;
#undef X
                    }

                    int n_float_constants;

                    if(inst.setvar.immediate) {
                        if(inst.setvar.bytes == 8) {
                            n_float_constants = arrlen(float64_constants);
                            arrpush(float64_constants, inst.arith.imm.integer);
                        } else {
                            n_float_constants = arrlen(float32_constants);
                            arrpush(float32_constants, (u32)inst.arith.imm.integer);
                        }
                    }

                    if(inst.setvar.segment == IRSEG_GLOBAL || inst.setvar.segment == IRSEG_BSS) {
                        if(inst.setvar.immediate) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(f%lu*)(&%s) = (*(f%lu*)&_jcc_internal_float%lu_constant_%i);\n",
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.sym->name,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.bytes << 3lu,
                                    n_float_constants);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(f%lu*)(&%s) = f%lureg%lu;\n",
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.sym->name,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.reg_src);
                        }
                    } else {
                        if(inst.setvar.immediate) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(f%lu*)(((u8*)%s)+0x%lx) = (*(f%lu*)&_jcc_internal_float%lu_constant_%i);\n",
                                    inst.setvar.bytes << 3lu,
                                    segment_name,
                                    inst.setvar.offset,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.bytes << 3lu,
                                    n_float_constants);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "*(f%lu*)(((u8*)%s)+0x%lx) = f%lureg%lu;\n",
                                    inst.setvar.bytes << 3lu,
                                    segment_name,
                                    inst.setvar.offset,
                                    inst.setvar.bytes << 3lu,
                                    inst.setvar.reg_src);
                        }
                    }
                }
                break;

            case IROP_LOAD:
            case IROP_LOADF:
                {
                    char **reg_map = IR_C_ireg_map;
                    bool is_float_op = false;

                    if(inst.opcode == IROP_LOADF) {
                        is_float_op = true;
                        reg_map = (inst.load.bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    }

                    if(inst.load.immediate) {
                        if(is_float_op) {
                            int n_float_constants;

                            if(inst.setvar.bytes == 8) {
                                n_float_constants = arrlen(float64_constants);
                                arrpush(float64_constants, inst.arith.imm.integer);
                            } else {
                                n_float_constants = arrlen(float32_constants);
                                arrpush(float32_constants, (u32)inst.arith.imm.integer);
                            }

                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "reg%lu = (*(f%lu*)&_jcc_internal_float%lu_constant_%i);\n",
                                    inst.load.reg_dest,
                                    inst.load.bytes << 3lu,
                                    inst.load.bytes << 3lu,
                                    n_float_constants);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s = (u%lu)0x%lx;\n",
                                    reg_map[inst.load.reg_dest],
                                    inst.load.bytes << 3lu,
                                    inst.load.imm.integer);
                        }
                    } else {
                        if(inst.load.has_immediate_offset) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s = *(%c%lu*)((u8*)%s + 0x%lx);\n",
                                    reg_map[inst.load.reg_dest],
                                    is_float_op ? 'f' : 'u',
                                    inst.load.bytes << 3lu,
                                    reg_map[inst.load.reg_src_ptr],
                                    inst.load.byte_offset_imm);
                        } else if(inst.load.has_indirect_offset) {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s = ((%c%lu*)%s)[%s];\n",
                                    reg_map[inst.load.reg_dest],
                                    is_float_op ? 'f' : 'u',
                                    inst.load.bytes << 3lu,
                                    reg_map[inst.load.reg_src_ptr],
                                    reg_map[inst.load.offset_reg]);
                        } else {
                            n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                    "%s = *(%c%lu*)%s;\n",
                                    reg_map[inst.load.reg_dest],
                                    is_float_op ? 'f' : 'u',
                                    inst.load.bytes << 3lu,
                                    reg_map[inst.load.reg_src_ptr]);
                        }
                    }
                }
                break;
            case IROP_STOR:
            case IROP_STORF:
                {
                    char **reg_map = IR_C_ireg_map;
                    bool is_float_op = false;

                    if(inst.opcode == IROP_LOADF) {
                        is_float_op = true;
                        reg_map = (inst.stor.bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    }

                    if(inst.stor.has_immediate_offset) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "*(%c%lu*)((u8*)%s + 0x%lx) = %s;\n",
                                is_float_op ? 'f' : 'u',
                                inst.stor.bytes << 3lu,
                                IR_C_ireg_map[inst.stor.reg_dest_ptr],
                                inst.stor.byte_offset_imm,
                                reg_map[inst.stor.reg_src]);
                    } else if(inst.stor.has_indirect_offset) {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "((%c%lu*)%s)[%s] = %s;\n",
                                is_float_op ? 'f' : 'u',
                                inst.stor.bytes << 3lu,
                                IR_C_ireg_map[inst.stor.reg_dest_ptr],
                                reg_map[inst.stor.offset_reg],
                                reg_map[inst.stor.reg_src]);
                    } else {
                        n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                                "*(%c%lu*)%s = %s;\n",
                                is_float_op ? 'f' : 'u',
                                inst.stor.bytes << 3lu,
                                IR_C_ireg_map[inst.stor.reg_dest_ptr],
                                reg_map[inst.stor.reg_src]);
                    }
                }
                break;
            case IROP_CALCPTROFFSET:
                {
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s = (u64)( ((u8*)%s) + %s * %lu );\n",
                            IR_C_ireg_map[inst.calcptroffset.reg_dest],
                            IR_C_ireg_map[inst.calcptroffset.reg_src_ptr],
                            IR_C_ireg_map[inst.calcptroffset.offset_reg],
                            inst.calcptroffset.stride);
                }
                break;

            case IROP_ITOF:
                {
                    char **to_reg_map = (inst.typeconv.to_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s = (f%lu)(%c%lu)%s;\n",
                            to_reg_map[inst.typeconv.to_reg],
                            inst.typeconv.to_bytes << 3lu,
                            inst.typeconv.sign ? 's' : 'u',
                            inst.typeconv.from_bytes << 3lu,
                            IR_C_ireg_map[inst.typeconv.from_reg]);
                }
                break;
            case IROP_FTOB:
                {
                    char **from_reg_map = (inst.typeconv.from_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s = (bool)(%s != (f%lu)0.0);\n",
                            IR_C_ireg_map[inst.typeconv.to_reg],
                            from_reg_map[inst.typeconv.from_reg],
                            inst.typeconv.from_bytes << 3lu);
                }
                break;
            case IROP_ITOB:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "%s = (bool)%s;\n",
                        IR_C_ireg_map[inst.typeconv.to_reg],
                        IR_C_ireg_map[inst.typeconv.from_reg]);
                break;
            case IROP_FTOI:
                {
                    char **from_reg_map = (inst.typeconv.from_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s = (%c%lu)%s;\n",
                            IR_C_ireg_map[inst.typeconv.to_reg],
                            inst.typeconv.sign ? 's' : 'u',
                            inst.typeconv.to_bytes << 3lu,
                            from_reg_map[inst.typeconv.from_reg]);
                }
                break;
            case IROP_ITOI:
                n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                        "%s = (%c%lu)(u%lu)%s;\n",
                        IR_C_ireg_map[inst.typeconv.to_reg],
                        inst.typeconv.sign ? 's' : 'u',
                        inst.typeconv.to_bytes << 3lu,
                        inst.typeconv.from_bytes << 3lu,
                        IR_C_ireg_map[inst.typeconv.from_reg]);
                break;
            case IROP_FTOF:
                {
                    char **to_reg_map = (inst.typeconv.to_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    char **from_reg_map = (inst.typeconv.from_bytes == 8) ? IR_C_f64reg_map : IR_C_f32reg_map;
                    n_written += stbsp_snprintf(line_buf+n_written, sizeof(line_buf),
                            "%s = (f%lu)%s;\n",
                            to_reg_map[inst.typeconv.to_reg],
                            inst.typeconv.to_bytes << 3lu,
                            from_reg_map[inst.typeconv.from_reg]);
                }
                break;
        }

        n_written += 1;

        assert(n_written < sizeof(line_buf));

        char inst_number_buf[70];
        int n_digits = stbsp_sprintf(inst_number_buf, "/* %d:    */ ", i);

        if(n_written + n_digits + code_buf_used >= code_buf_cap) {
            while(n_written + n_digits + code_buf_used >= code_buf_cap) code_buf_cap <<= 1;
            code_buf = realloc(code_buf, code_buf_cap);
        }

        if(n_written > 1) {
            for(u64 i = 0; i < n_digits;) code_buf[code_buf_used++] = inst_number_buf[i++];
        }
        for(u64 i = 0; i < n_written - 1;) code_buf[code_buf_used++] = line_buf[i++];

    }

    u64 preamble_buf_cap = 4096;
    u64 preamble_buf_used = 0;
    char *preamble_buf = malloc(preamble_buf_cap);

    if(proc_type->proc.ret.n == 1) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                "union _Port %s(void *context_pointer, ",
                proc_type->proc.name);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];

        for(u64 i = 0; i < proc_type->proc.non_scalar_return_count; ++i) {
            int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                    "union _Port arg%lu, ",
                    i);

            if(n_written + preamble_buf_used >= preamble_buf_cap) {
                while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
                preamble_buf = realloc(preamble_buf, preamble_buf_cap);
            }

            for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
        }

        for(u64 i = 0; i < proc_type->proc.param.n; ++i) {
            int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                    "union _Port arg%lu, ",
                    i + proc_type->proc.non_scalar_return_count);

            if(n_written + preamble_buf_used >= preamble_buf_cap) {
                while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
                preamble_buf = realloc(preamble_buf, preamble_buf_cap);
            }

            for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
        }

        preamble_buf_used -= 2;

        n_written = stbsp_snprintf(line_buf, sizeof(line_buf), ") {\n");

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    } else {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                "void %s(void *context_pointer, ",
                proc_type->proc.name);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];

        for(u64 i = 0; i < proc_type->proc.ret.n; ++i) {
            int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                    "union _Port *retp%lu, ",
                    i);

            if(n_written + preamble_buf_used >= preamble_buf_cap) {
                while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
                preamble_buf = realloc(preamble_buf, preamble_buf_cap);
            }

            for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
        }

        for(u64 i = 0; i < proc_type->proc.non_scalar_return_count; ++i) {
            int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                    "union _Port arg%lu, ",
                    i);

            if(n_written + preamble_buf_used >= preamble_buf_cap) {
                while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
                preamble_buf = realloc(preamble_buf, preamble_buf_cap);
            }

            for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
        }

        for(u64 i = 0; i < proc_type->proc.param.n; ++i) {
            int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                    "union _Port arg%lu, ",
                    i + proc_type->proc.non_scalar_return_count);

            if(n_written + preamble_buf_used >= preamble_buf_cap) {
                while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
                preamble_buf = realloc(preamble_buf, preamble_buf_cap);
            }

            for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
        }

        preamble_buf_used -= 2;

        n_written = stbsp_snprintf(line_buf, sizeof(line_buf), ") {\n");

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(int i = 0; i < arrlen(float32_constants); ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                "const u64 _jcc_internal_float32_constant_%i = 0x%x;\n",
                i,
                float32_constants[i]);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(int i = 0; i < arrlen(float64_constants); ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf),
                "const u64 _jcc_internal_float64_constant_%i = 0x%lx;\n",
                i,
                float64_constants[i]);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(int i = 0; i < STATICARRLEN(IR_C_ireg_map); ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf), "u64 %s;\n", IR_C_ireg_map[i]);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(int i = 0; i < STATICARRLEN(IR_C_f32reg_map); ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf), "f32 %s;\n", IR_C_f32reg_map[i]);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(int i = 0; i < STATICARRLEN(IR_C_f64reg_map); ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf), "f64 %s;\n", IR_C_f64reg_map[i]);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(u64 i = proc_type->proc.param.n; i < highest_argument_index; ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf), "union _Port arg%lu;\n", i);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    for(u64 i = 0; i < highest_return_index; ++i) {
        int n_written = stbsp_snprintf(line_buf, sizeof(line_buf), "union _Port ret%lu;\n", i);

        if(n_written + preamble_buf_used >= preamble_buf_cap) {
            while(n_written + preamble_buf_used >= preamble_buf_cap) preamble_buf_cap <<= 1;
            preamble_buf = realloc(preamble_buf, preamble_buf_cap);
        }

        for(u64 i = 0; i < n_written;) preamble_buf[preamble_buf_used++] = line_buf[i++];
    }

    preamble_buf[preamble_buf_used] = 0;
    code_buf[code_buf_used] = 0;

    fprintf(stderr, "%s\n%s\n}\n", preamble_buf, code_buf);
    fprintf(stderr, "highest_argument_index = %lu\nhighest_return_index = %lu",
            highest_argument_index,
            highest_return_index);
    free(preamble_buf);
    free(code_buf);
}

void ir_run(Job *jp, int procid) {
    IRmachine interp = jp->interp;
    // NOTE
    // we assume the necessary setup to run has already been done
    // data is put in and extracted from the IRmachine elsewhere

    IRproc procedure = hmget(proc_table, procid);
    IRinst *instructions = procedure.instructions;
    u64 n_instructions = procedure.n_instructions;
    u64 *jump_table = procedure.jump_table;
    interp.local_segment_size = procedure.local_segment_size;

    assert(procedure.is_foreign == false);

    u64 pc = 0;
    u8 *local_base = interp.local_segment;

    assert(interp.local_segment && arrlen(interp.ports) > 0);

    IRinst inst;
    u64 imask;
    bool go = true;

    while(go && pc < n_instructions) {
        //bool loc_changed = (inst.loc.line != instructions[pc].loc.line && inst.loc.line != 0 && instructions[pc].loc.line != 0);
        inst = instructions[pc];
        imask = 0;


        u8 *varptr = NULL;

        if(     inst.opcode == IROP_ADDRVAR ||
                inst.opcode == IROP_GETVAR  ||
                inst.opcode == IROP_SETVAR  ||
                inst.opcode == IROP_SETVARF ||
                inst.opcode == IROP_GETVARF)
        {
            switch(inst.addrvar.segment) {
                default:
                    UNREACHABLE;
                case IRSEG_LOCAL:
                    varptr = local_base + inst.addrvar.offset;
                    break;
                case IRSEG_GLOBAL:
                    varptr = interp.global_segment + inst.addrvar.offset;
                    break;
                case IRSEG_BSS:
                    varptr = interp.bss_segment + inst.addrvar.offset;
                    break;
                case IRSEG_TYPE:
                case IRSEG_STRING:
                    varptr = (u8*)(inst.addrvar.offset);
                    break;
            }
        }


        //if(loc_changed) {
        //    fprintf(stderr, "\n");
        //    show_ir_loc(inst);
        //}
        //print_ir_machine(&interp, stderr);
        //fprintf(stderr, "%lu: ", pc);
        //print_ir_inst(inst, stderr);
        //sleep(1);

        switch(inst.opcode) {
            default:
                printf("jcc: error: bad instruction at '%lu'\n", pc);
                UNREACHABLE;
            case IROP_GETCONTEXTARG:
                interp.iregs[inst.getcontextarg.reg_dest] = interp.context_pointer;
                break;
            case IROP_SETCONTEXTARG:
                interp.context_pointer = interp.iregs[inst.setcontextarg.reg_src];
                break;
            case IROP_LABEL:
            case IROP_NOOP:
            case IROP_HINT_BEGIN_FOREIGN_CALL:
            case IROP_HINT_END_FOREIGN_CALL:
            case IROP_HINT_BEGIN_CALL:
            case IROP_HINT_END_CALL:
            case IROP_HINT_BEGIN_PASS_NON_SCALAR:
            case IROP_HINT_END_PASS_NON_SCALAR:
                break;
#define X(opcode, opsym) \
            case IROP_##opcode: \
                                \
                imask = (inst.arith.operand_bytes[0] < 8) \
                ? ((1l << (inst.arith.operand_bytes[0] << 3l)) - 1l) \
                : (u64)(-1l); \
                if(inst.arith.sign) { \
                    s64 a = SIGN_EXTEND_S64(((s64)interp.iregs[inst.arith.reg[1]]), (inst.arith.operand_bytes[1] << 3lu)); \
                    s64 b = SIGN_EXTEND_S64(((s64)interp.iregs[inst.arith.reg[2]]), (inst.arith.operand_bytes[2] << 3lu)); \
                    if(inst.arith.immediate) \
                    b = SIGN_EXTEND_S64(((s64)inst.arith.imm.integer), (inst.arith.operand_bytes[2] << 3lu)); \
                    interp.iregs[inst.arith.reg[0]] = \
                    (s64)imask & (s64)(a opsym b); \
                } else { \
                    if(inst.arith.immediate) { \
                        interp.iregs[inst.arith.reg[0]] = \
                        imask & (interp.iregs[inst.arith.reg[1]] opsym inst.arith.imm.integer); \
                    } else { \
                        interp.iregs[inst.arith.reg[0]] = \
                        imask & (interp.iregs[inst.arith.reg[1]] opsym interp.iregs[inst.arith.reg[2]]); \
                    } \
                } \
                break;
                IR_INT_BINOPS;
#undef X
#define X(opcode, opsym) \
            case IROP_##opcode: \
                                \
                imask = (inst.arith.operand_bytes[0] < 8) \
                ? ((1l << (inst.arith.operand_bytes[0] << 3l)) - 1l) \
                : (u64)(-1l); \
                interp.iregs[inst.arith.reg[0]] = imask & (opsym interp.iregs[inst.arith.reg[1]]); \
                break;
                IR_INT_UNOPS;
#undef X
#define X(opcode, opsym) \
            case IROP_##opcode:\
                               \
                if(inst.arith.operand_bytes[0] == 8) { \
                    interp.f64regs[inst.arith.reg[0]] = \
                    interp.f64regs[inst.arith.reg[1]] opsym \
                    (inst.arith.immediate ? inst.arith.imm.floating64 \
                     : interp.f64regs[inst.arith.reg[2]]); \
                } else { \
                    interp.f32regs[inst.arith.reg[0]] = \
                    interp.f32regs[inst.arith.reg[1]] opsym \
                    (inst.arith.immediate ? inst.arith.imm.floating32 \
                     : interp.f32regs[inst.arith.reg[2]]); \
                } \
                break;
                IR_FLOAT_BINOPS;
#undef X
#define X(opcode, opsym) \
            case IROP_##opcode:\
                               \
                if(inst.arith.operand_bytes[1] == 8 && inst.arith.operand_bytes[2] == 8) { \
                    interp.iregs[inst.arith.reg[0]] = \
                    (bool)(interp.f64regs[inst.arith.reg[1]] opsym \
                            (inst.arith.immediate ? inst.arith.imm.floating64 \
                             : interp.f64regs[inst.arith.reg[2]])); \
                } else if(inst.arith.operand_bytes[1] == 4 && inst.arith.operand_bytes[2] == 4) { \
                    interp.iregs[inst.arith.reg[0]] = \
                    (bool)(interp.f32regs[inst.arith.reg[1]] opsym \
                            (inst.arith.immediate ? inst.arith.imm.floating32 \
                             : interp.f32regs[inst.arith.reg[2]])); \
                } else { \
                    UNREACHABLE; \
                } \
                break;
                IR_FLOAT_CMPOPS;
#undef X
            case IROP_FNEG:
                if(inst.arith.operand_bytes[0] == 8) {
                    interp.f64regs[inst.arith.reg[0]] = -interp.f64regs[inst.arith.reg[1]];
                } else {
                    interp.f32regs[inst.arith.reg[0]] = -interp.f32regs[inst.arith.reg[1]];
                }
                break;

            case IROP_IF:
                if(interp.iregs[inst.branch.cond_reg] != 0) {
                    pc = jump_table[inst.branch.label_id];
                    continue;
                }
                break;
            case IROP_IFZ:
                if(interp.iregs[inst.branch.cond_reg] == 0) {
                    pc = jump_table[inst.branch.label_id];
                    continue;
                }
                break;
            case IROP_JMP:
                pc = jump_table[inst.branch.label_id];
                continue;
            case IROP_CALL:
                {
                    u64 new_procid = inst.call.id_imm;
                    if(!inst.call.immediate) new_procid = interp.iregs[inst.call.id_reg];
                    IRproc new_procedure = hmget(proc_table, new_procid);

                    if(new_procedure.is_foreign) {
                        IR_foreign_proc foreign_proc = new_procedure.foreign_proc;
                        foreign_proc(&interp);
                        ++pc;
                    } else {
                        //fprintf(stderr, "calling %s, stepping stack pointer by %lu\n",
                        //        new_procedure.name,
                        //        procedure.local_segment_size);
                        arrpush(interp.procid_stack, procid);
                        arrpush(interp.jump_table_stack, jump_table);
                        arrpush(interp.pc_stack, pc + 1);
                        local_base += procedure.local_segment_size;
                        assert(local_base < interp.local_segment + IR_LOCAL_SEGMENT_BYTES);
                        procid = new_procid;
                        jump_table = new_procedure.jump_table;
                        instructions = new_procedure.instructions;
                        n_instructions = new_procedure.n_instructions;
                        procedure = new_procedure;
                        pc = 0;
                    }
                }
                continue;
            case IROP_RET:
                if(arrlen(interp.pc_stack) == 0) {
                    assert(local_base == interp.local_segment);
                    assert(arrlen(interp.procid_stack) == 0);
                    go = false;
                    continue;
                }

                jump_table = arrpop(interp.jump_table_stack);
                pc = arrpop(interp.pc_stack);
                procid = arrpop(interp.procid_stack);

                procedure = hmget(proc_table, procid);
                local_base -= procedure.local_segment_size;
                assert(local_base >= interp.local_segment);
                instructions = procedure.instructions;
                n_instructions = procedure.n_instructions;
                continue;
            case IROP_LOAD:
                if(inst.load.immediate) {
                    imask = (inst.load.bytes < 8)
                        ? ((u64)((1LU << (inst.load.bytes << 3LU)) - 1LU))
                        : ((u64)(-1));
                    interp.iregs[inst.load.reg_dest] = imask & inst.load.imm.integer;
                } else {
                    u8 *ptr = (u8*)(interp.iregs[inst.load.reg_src_ptr]);
                    u64 offset = 0;
                    if(inst.load.has_immediate_offset) {
                        offset = inst.load.byte_offset_imm;
                        switch(inst.load.bytes) {
                            case 1:
                                interp.iregs[inst.load.reg_dest] = ptr[offset];
                                break;
                            case 2:
                                interp.iregs[inst.load.reg_dest] = *(u16*)(ptr + offset);
                                break;
                            case 4:
                                interp.iregs[inst.load.reg_dest] = *(u32*)(ptr + offset);
                                break;
                            case 8:
                                interp.iregs[inst.load.reg_dest] = *(u64*)(ptr + offset);
                                break;
                        }
                    } else if(inst.load.has_indirect_offset) {
                        offset = interp.iregs[inst.load.offset_reg];
                        switch(inst.load.bytes) {
                            case 1:
                                interp.iregs[inst.load.reg_dest] = ptr[offset];
                                break;
                            case 2:
                                interp.iregs[inst.load.reg_dest] = ((u16*)ptr)[offset];
                                break;
                            case 4:
                                interp.iregs[inst.load.reg_dest] = ((u32*)ptr)[offset];
                                break;
                            case 8:
                                interp.iregs[inst.load.reg_dest] = ((u64*)ptr)[offset];
                                break;
                        }
                    } else {
                        switch(inst.load.bytes) {
                            case 1:
                                interp.iregs[inst.load.reg_dest] = *ptr;
                                break;
                            case 2:
                                interp.iregs[inst.load.reg_dest] = *((u16*)ptr);
                                break;
                            case 4:
                                interp.iregs[inst.load.reg_dest] = *((u32*)ptr);
                                break;
                            case 8:
                                interp.iregs[inst.load.reg_dest] = *((u64*)ptr);
                                break;
                        }
                    }
                }
                break;
            case IROP_STOR:
                {
                    u64 src_value = inst.stor.immediate ? inst.stor.imm.integer : interp.iregs[inst.stor.reg_src];
                    u8 *ptr = (u8*)(interp.iregs[inst.stor.reg_dest_ptr]);
                    u64 offset = 0;

                    if(inst.stor.has_immediate_offset) {
                        offset = inst.stor.byte_offset_imm;
                        switch(inst.stor.bytes) {
                            case 1:
                                ptr[offset] = (u8)src_value;
                                break;
                            case 2:
                                *(u16*)(ptr + offset) = (u16)src_value;
                                break;
                            case 4:
                                *(u32*)(ptr + offset) = (u32)src_value;
                                break;
                            case 8:
                                *(u64*)(ptr + offset) = (u64)src_value;
                                break;
                        }
                    } else if(inst.stor.has_indirect_offset) {
                        offset = interp.iregs[inst.stor.offset_reg];
                        switch(inst.stor.bytes) {
                            case 1:
                                ptr[offset] = (u8)src_value;
                                break;
                            case 2:
                                ((u16*)ptr)[offset] = (u16)src_value;
                                break;
                            case 4:
                                ((u32*)ptr)[offset] = (u32)src_value;
                                break;
                            case 8:
                                ((u64*)ptr)[offset] = (u64)src_value;
                                break;
                        }
                    } else {
                        switch(inst.stor.bytes) {
                            case 1:
                                *ptr = (u8)src_value;
                                break;
                            case 2:
                                *((u16*)ptr) = (u16)src_value;
                                break;
                            case 4:
                                *((u32*)ptr) = (u32)src_value;
                                break;
                            case 8:
                                *((u64*)ptr) = (u64)src_value;
                                break;
                        }
                    }
                }
                break;
            case IROP_LOADF:
                if(inst.load.bytes == 8) {
                    if(inst.load.immediate) {
                        interp.f64regs[inst.load.reg_dest] = inst.load.imm.floating64;
                    } else {
                        f64 *ptr = (f64*)(interp.iregs[inst.load.reg_src_ptr]);
                        f64 load_val;
                        if(inst.load.has_immediate_offset)
                            load_val = *(f64*)(((u8*)ptr) + inst.load.byte_offset_imm);
                        else if(inst.load.has_indirect_offset)
                            load_val = ptr[interp.iregs[inst.load.offset_reg]];
                        else
                            load_val = *ptr;
                        interp.f64regs[inst.load.reg_dest] = load_val;
                    }
                } else {
                    if(inst.load.immediate) {
                        interp.f32regs[inst.load.reg_dest] = inst.load.imm.floating32;
                    } else {
                        f32 *ptr = (f32*)(interp.iregs[inst.load.reg_src_ptr]);
                        f32 load_val;
                        if(inst.load.has_immediate_offset)
                            load_val = *(f32*)(((u8*)ptr) + inst.load.byte_offset_imm);
                        else if(inst.load.has_indirect_offset)
                            load_val = ptr[interp.iregs[inst.load.offset_reg]];
                        else
                            load_val = *ptr;
                        interp.f32regs[inst.load.reg_dest] = load_val;
                    }
                }
                break;
            case IROP_STORF:
                if(inst.stor.bytes == 8) {
                    f64 src_value = inst.stor.immediate ? inst.stor.imm.floating64 : interp.f64regs[inst.stor.reg_src];
                    if(inst.stor.has_immediate_offset) {
                        u64 offset = inst.stor.byte_offset_imm;
                        u8 *ptr = (u8*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        *(f64*)(ptr + offset) = src_value;
                    } else if(inst.stor.has_indirect_offset) {
                        u64 offset = interp.iregs[inst.stor.offset_reg];
                        f64 *ptr = (f64*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        ptr[offset] = src_value;
                    } else {
                        f64 *ptr = (f64*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        *ptr = src_value;
                    }
                } else {
                    f32 src_value = inst.stor.immediate ? inst.stor.imm.floating32 : interp.f32regs[inst.stor.reg_src];
                    if(inst.stor.has_immediate_offset) {
                        u64 offset = inst.stor.byte_offset_imm;
                        u8 *ptr = (u8*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        *(f32*)(ptr + offset) = src_value;
                    } else if(inst.stor.has_indirect_offset) {
                        u64 offset = interp.iregs[inst.stor.offset_reg];
                        f32 *ptr = (f32*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        ptr[offset] = src_value;
                    } else {
                        f32 *ptr = (f32*)(interp.iregs[inst.stor.reg_dest_ptr]);
                        *ptr = src_value;
                    }
                }
                break;
            case IROP_CALCPTROFFSET:
                interp.iregs[inst.calcptroffset.reg_dest] =
                    (u64)(((u8*)(interp.iregs[inst.calcptroffset.reg_src_ptr])) + 
                            interp.iregs[inst.calcptroffset.offset_reg] * inst.calcptroffset.stride);
                break;
            case IROP_ADDRVAR:
                interp.iregs[inst.addrvar.reg_dest] = (u64)(void*)varptr;
                break;
            case IROP_GETVAR:
                switch(inst.getvar.bytes) {
                    case 1:
                        interp.iregs[inst.getvar.reg_dest] = *varptr;
                        break;
                    case 2:
                        interp.iregs[inst.getvar.reg_dest] = *(u16*)varptr;
                        break;
                    case 4:
                        interp.iregs[inst.getvar.reg_dest] = *(u32*)varptr;
                        break;
                    case 8:
                        interp.iregs[inst.getvar.reg_dest] = *(u64*)varptr;
                        break;
                }
                break;
            case IROP_SETVAR:
                switch(inst.setvar.bytes) {
                    case 1:
                        *varptr =
                            inst.setvar.immediate
                            ? (inst.setvar.imm.integer & 0xff)
                            : (u8)(interp.iregs[inst.setvar.reg_src]);
                        break;
                    case 2:
                        *(u16*)varptr =
                            inst.setvar.immediate
                            ? (inst.setvar.imm.integer & 0xffff)
                            : (u16)(interp.iregs[inst.setvar.reg_src]);
                        break;
                    case 4:
                        *(u32*)varptr =
                            inst.setvar.immediate
                            ? (inst.setvar.imm.integer & 0xffffffff)
                            : (u32)(interp.iregs[inst.setvar.reg_src]);
                        break;
                    case 8:
                        *(u64*)varptr =
                            inst.setvar.immediate
                            ? inst.setvar.imm.integer
                            : (u64)(interp.iregs[inst.setvar.reg_src]);
                        break;
                }
                break;
            case IROP_SETARG:
            case IROP_SETRET:
                {
                    if(inst.setport.port >= arrlen(interp.ports))
                        arrsetlen(interp.ports, inst.setport.port + 1);
                    imask = (inst.setport.bytes < 8)
                        ? ((1LU << (inst.setport.bytes << 3LU)) - 1LU)
                        : (u64)(-1);
                    interp.ports[inst.setport.port].integer = imask & interp.iregs[inst.setport.reg_src];
                }
                break;
            case IROP_GETARG:
            case IROP_GETRET:
                {
                    if(inst.getport.port >= arrlen(interp.ports))
                        arrsetlen(interp.ports, inst.getport.port + 1);
                    imask = (inst.getport.bytes < 8)
                        ? ((1LU << (inst.getport.bytes << 3LU)) - 1LU)
                        : (u64)(-1);
                    interp.iregs[inst.getport.reg_dest] = imask & interp.ports[inst.getport.port].integer;
                }
                break;
            case IROP_GETVARF:
                if(inst.getvar.bytes == 8)
                    interp.f64regs[inst.getvar.reg_dest] = *(f64*)(varptr);
                else
                    interp.f32regs[inst.getvar.reg_dest] = *(f32*)(varptr);
                break;
            case IROP_SETVARF:
                if(inst.setvar.bytes == 8)
                    *(f64*)(varptr) =
                        inst.setvar.immediate
                        ? inst.setvar.imm.floating64
                        : interp.f64regs[inst.setvar.reg_src];
                else
                    *(f32*)(varptr) =
                        inst.setvar.immediate
                        ? inst.setvar.imm.floating32
                        : interp.f32regs[inst.setvar.reg_src];
                break;
            case IROP_SETARGF:
            case IROP_SETRETF:
                if(inst.setport.port >= arrlen(interp.ports))
                    arrsetlen(interp.ports, inst.setport.port + 1);
                if(inst.setport.bytes == 8)
                    interp.ports[inst.setport.port].floating64 = interp.f64regs[inst.setport.reg_src];
                else
                    interp.ports[inst.setport.port].floating32 = interp.f32regs[inst.setport.reg_src];
                break;
            case IROP_GETARGF:
            case IROP_GETRETF:
                if(inst.getport.port >= arrlen(interp.ports))
                    arrsetlen(interp.ports, inst.getport.port + 1);
                if(inst.getport.bytes == 8)
                    interp.f64regs[inst.getport.reg_dest] = interp.ports[inst.getport.port].floating64;
                else
                    interp.f32regs[inst.getport.reg_dest] = interp.ports[inst.getport.port].floating32;
                break;
            case IROP_ITOF:
                if(inst.typeconv.to_bytes == 8) {
                    if(inst.typeconv.sign) {
                        switch(inst.typeconv.from_bytes) {
                            case 1:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(s8)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 2:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(s16)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 4:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(s32)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 8:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(s64)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                        }
                    } else {
                        switch(inst.typeconv.from_bytes) {
                            case 1:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(u8)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 2:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(u16)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 4:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(u32)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 8:
                                interp.f64regs[inst.typeconv.to_reg] = (f64)(u64)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                        }
                    }
                } else {
                    if(inst.typeconv.sign) {
                        switch(inst.typeconv.from_bytes) {
                            case 1:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(s8)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 2:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(s16)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 4:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(s32)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 8:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(s64)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                        }
                    } else {
                        switch(inst.typeconv.from_bytes) {
                            case 1:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(u8)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 2:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(u16)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 4:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(u32)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                            case 8:
                                interp.f32regs[inst.typeconv.to_reg] = (f32)(u64)(interp.iregs[inst.typeconv.from_reg]);
                                break;
                        }
                    }
                }
                break;
            case IROP_FTOB:
                if(inst.typeconv.from_bytes == 8)
                    interp.iregs[inst.typeconv.to_reg] = (bool)((f64)0.0 != interp.f64regs[inst.typeconv.from_reg]);
                else
                    interp.iregs[inst.typeconv.to_reg] = (bool)((f32)0.0 != interp.f32regs[inst.typeconv.from_reg]);
                break;
            case IROP_ITOB:
                interp.iregs[inst.typeconv.to_reg] = (bool)(interp.iregs[inst.typeconv.from_reg]);
                break;
            case IROP_FTOI:
                if(inst.typeconv.from_bytes == 8) {
                    imask = (inst.typeconv.to_bytes < 8)
                        ? ((1LU << (inst.typeconv.to_bytes << 3LU)) - 1LU)
                        : (u64)(-1);
                    interp.iregs[inst.typeconv.to_reg] = imask & (u64)(interp.f64regs[inst.typeconv.from_reg]);
                } else {
                    imask = (inst.typeconv.to_bytes < 8)
                        ? ((1LU << (inst.typeconv.to_bytes << 3LU)) - 1LU)
                        : (u64)(-1);
                    interp.iregs[inst.typeconv.to_reg] = imask & (u64)(interp.f32regs[inst.typeconv.from_reg]);
                }
                break;
            case IROP_ITOI:
                {
                    imask = (inst.typeconv.to_bytes < 8)
                        ? ((1LU << (inst.typeconv.to_bytes << 3LU)) - 1LU)
                        : (u64)(-1);
                    u64 masked = imask & interp.iregs[inst.typeconv.from_reg];
                    interp.iregs[inst.typeconv.to_reg] = 
                        inst.typeconv.sign
                        ? SIGN_EXTEND_S64(masked, (inst.typeconv.to_bytes << 3lu))
                        : masked;
                }
                break;
            case IROP_FTOF:
                if(inst.typeconv.to_bytes == 8 && inst.typeconv.from_bytes == 4) {
                    interp.f64regs[inst.typeconv.to_reg] = (f64)(interp.f32regs[inst.typeconv.from_reg]);
                } else if(inst.typeconv.to_bytes == 4 && inst.typeconv.from_bytes == 8) {
                    interp.f32regs[inst.typeconv.to_reg] = (f32)(interp.f64regs[inst.typeconv.from_reg]);
                }
                break;
        }

        ++pc;
    }
}

void copy_array_data_to_value(Job *jp, Value *v, Type *t, u8 *data) {
    assert(t->kind == TYPE_KIND_ARRAY); 
    assert(v->kind == VALUE_KIND_ARRAY);
    assert(t->array.n == v->val.array.n);

    if(t->array.of->kind == TYPE_KIND_ARRAY) {
        for(int i = 0; i < t->array.n; ++i) {
            copy_array_data_to_value(jp, v->val.array.elements[i], t->array.of, data + i * t->array.element_stride);
        }
    } else {
        if(TYPE_KIND_IS_NOT_SCALAR(t->array.of->kind)) {
            UNIMPLEMENTED;
        } else {
            for(int i = 0; i < t->array.n; ++i) {
                memcpy((void*)(&(v->val.array.elements[i]->val)), data + i * t->array.element_stride, t->array.element_stride);
            }
        }
    }

}

/*
 * generate each expression and write it to local (or global) data then return the offset
 *
 * for initializing a dynamic array from an array literal we add a copy, I can't be bothered to
 * do anything more complicated
 *
 * initializing a view is just a matter of setting the pointer to the local address
 *
 * for now I'll only do local array literals, later on handle globals
 *
 */
u64 ir_gen_array_literal(Job *jp, Type *array_type, AST_array_literal *ast_array) {
    assert(array_type->kind == TYPE_KIND_ARRAY);
    assert(array_type->kind == ast_array->type_annotation->kind);

    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), array_type->align);

    u64 offset = arrlast(jp->local_offset);
    assert(array_type->bytes == array_type->array.element_stride * array_type->array.n);
    u64 expected_size = array_type->bytes;

    if(array_type->array.of->kind == TYPE_KIND_ARRAY) {
        for(AST_expr_list *elem = ast_array->elements; elem; elem = elem->next) {
            ir_gen_array_literal(jp, array_type->array.of, (AST_array_literal*)(elem->expr));
        }
    } else {
        arrlast(jp->local_offset) += expected_size;

        if(TYPE_KIND_IS_NOT_SCALAR(array_type->array.of->kind)) {
            UNIMPLEMENTED;
        } else {
            u64 stride = array_type->array.element_stride;

            u64 *regp = &(jp->reg_alloc);

            IRinst inst = {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = offset,
                    .bytes = stride,
                },
            };

            if(TYPE_KIND_IS_FLOAT(array_type->array.of->kind)) {
                regp = &(jp->float_reg_alloc);

                inst = (IRinst) {
                    .opcode = IROP_SETVARF,
                        .setvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = offset,
                            .bytes = stride,
                        },
                };
            }

            for(AST_expr_list *elem = ast_array->elements; elem; elem = elem->next) {
                AST_expr_base *expr_base = (AST_expr_base*)(elem->expr);

                assert(types_are_same(expr_base->type_annotation, array_type->array.of));

                inst.setvar.immediate = false;
                ir_gen_expr(jp, elem->expr);
                (*regp)--;
                inst.setvar.reg_src = *regp;

                //TODO strip unecessary LOAD and SETVAR instructions in a second pass

                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
                inst.setvar.offset += stride;
            }
        }
    }

    u64 cur_offset = arrlast(jp->local_offset);
    u64 expected_offset = offset + expected_size;

    //TODO filling in of array literal with 0s is wrong
    for(u64 step = 8; cur_offset < expected_offset; cur_offset += step) {
        while(cur_offset + step > expected_offset) step >>= 1;
        IRinst inst =
            (IRinst) {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = cur_offset,
                    .bytes = step,
                    .immediate = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    arrlast(jp->local_offset) = expected_offset;

    return offset;
}

u64 ir_gen_copy_array_literal(Job *jp, Type *array_type, AST_array_literal *ast_array, u64 offset, u64 ptr_reg) {
    assert(array_type->kind == TYPE_KIND_ARRAY);
    assert(array_type->kind == ast_array->type_annotation->kind);

    assert(array_type->bytes == array_type->array.element_stride * array_type->array.n);
    u64 expected_size = array_type->bytes;

    if(array_type->array.of->kind == TYPE_KIND_ARRAY) {
        for(AST_expr_list *elem = ast_array->elements; elem; elem = elem->next) {
            offset = ir_gen_copy_array_literal(jp, array_type->array.of, (AST_array_literal*)(elem->expr), offset, ptr_reg);
        }
    } else {
        if(TYPE_KIND_IS_NOT_SCALAR(array_type->array.of->kind)) {
            UNIMPLEMENTED;
        } else {
            u64 stride = array_type->array.element_stride;

            u64 *regp = &(jp->reg_alloc);

            IRinst inst = {
                .opcode = IROP_STOR,
                .stor = {
                    .reg_dest_ptr = ptr_reg,
                    .byte_offset_imm = offset,
                    .bytes = stride,
                    .has_immediate_offset = true,
                },
            };

            if(TYPE_KIND_IS_FLOAT(array_type->array.of->kind)) {
                regp = &(jp->float_reg_alloc);

                inst = (IRinst) {
                    .opcode = IROP_STORF,
                        .stor = {
                            .reg_dest_ptr = ptr_reg,
                            .byte_offset_imm = offset,
                            .bytes = stride,
                            .has_immediate_offset = true,
                        },
                };
            }

            for(AST_expr_list *elem = ast_array->elements; elem; elem = elem->next) {
                AST_expr_base *expr_base = (AST_expr_base*)(elem->expr);
                if(expr_base->value_annotation == NULL || expr_base->value_annotation->kind == VALUE_KIND_NIL) {
                    inst.stor.immediate = false;
                    ir_gen_expr(jp, elem->expr);
                    (*regp)--;
                    inst.stor.reg_src = *regp;
                } else {
                    inst.stor.immediate = true;
                    if(array_type->array.of->kind == TYPE_KIND_F64) {
                        if(expr_base->value_annotation->kind == VALUE_KIND_INT) {
                            inst.stor.imm.floating64 = (f64)(expr_base->value_annotation->val.integer);
                        } else if(expr_base->value_annotation->kind == VALUE_KIND_FLOAT) {
                            inst.stor.imm.floating64 = (f64)(expr_base->value_annotation->val.floating);
                        } else {
                            assert(expr_base->value_annotation->kind == VALUE_KIND_DFLOAT);
                            inst.stor.imm.floating64 = (f64)(expr_base->value_annotation->val.dfloating);
                        }
                    } else if(array_type->array.of->kind >= TYPE_KIND_FLOAT) {
                        //TODO implicit casts should be resolved in the typechecker
                        if(expr_base->value_annotation->kind == VALUE_KIND_INT) {
                            inst.stor.imm.floating32 = (f32)(expr_base->value_annotation->val.integer);
                        } else {
                            assert(expr_base->value_annotation->kind == VALUE_KIND_FLOAT);
                            inst.stor.imm.floating32 = expr_base->value_annotation->val.floating;
                        }
                    } else {
                        inst.stor.imm.integer = expr_base->value_annotation->val.integer;
                    }
                }
                inst.stor.byte_offset_imm = offset;
                offset += stride;
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            }
        }
    }

    //NOTE this assumes that the initial offset passed was 0
    for(u64 step = 8; offset < expected_size; offset += step) {
        while(offset + step > expected_size) step >>= 1;
        IRinst inst =
            (IRinst) {
                .opcode = IROP_STOR,
                .stor = {
                    .reg_dest_ptr = ptr_reg,
                    .byte_offset_imm = offset,
                    .bytes = step,
                    .has_immediate_offset = true,
                    .immediate = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    return offset;
}

INLINE void ir_gen_memorycopy(Job *jp, u64 bytes, u64 align, u64 to_ptr_reg, u64 from_ptr_reg) {
    IRinst inst;

    jp->reg_alloc++;
    for(u64 offset = 0; offset < bytes; offset += align) {
        inst =
            (IRinst) {
                .opcode = IROP_LOAD,
                .load = {
                    .reg_dest = jp->reg_alloc,
                    .reg_src_ptr = from_ptr_reg,
                    .byte_offset_imm = offset,
                    .bytes = align,
                    .has_immediate_offset = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        inst =
            (IRinst) {
                .opcode = IROP_STOR,
                .stor = {
                    .reg_dest_ptr = to_ptr_reg,
                    .reg_src = jp->reg_alloc,
                    .byte_offset_imm = offset,
                    .bytes = align,
                    .has_immediate_offset = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }
    jp->reg_alloc--;
}

INLINE void ir_gen_statement(Job *jp, AST_statement *ast_statement) {
    IRinst inst = {0};
    IRinst inst_read = {0};
    IRinst inst_write = {0};
    IRop opcode_read = 0;
    IRop opcode_write = 0;
    IRsegment segment = -1;
    u64 op_bytes = 0;
    Type *op_type = NULL;
    u64 read_reg = 0;

    if(ast_statement->right == NULL && ast_statement->assign_op == 0) {
        //TODO make sure statements have an effect at this point
        ir_gen_expr(jp, ast_statement->left);
        return;
    }

    assert(((AST_expr_base*)(ast_statement->left))->type_annotation);

    //TODO this is kind horrible
    if(TYPE_KIND_IS_NOT_SCALAR((((AST_expr_base*)ast_statement->left))->type_annotation->kind) && TYPE_KIND_PROC != ((((AST_expr_base*)ast_statement->left))->type_annotation->kind)) {
        assert(ast_statement->assign_op == '=' && ast_statement->right);

        Type *left_type = ((AST_expr_base*)(ast_statement->left))->type_annotation;
        Type *right_type = ((AST_expr_base*)(ast_statement->right))->type_annotation;

        if(left_type->kind == TYPE_KIND_ARRAY) {
            if(ast_statement->right->kind == AST_KIND_array_literal) {
                ir_gen_expr(jp, ast_statement->left);
                ir_gen_copy_array_literal(jp, left_type, (AST_array_literal*)(ast_statement->right), 0, jp->reg_alloc - 1);
                jp->reg_alloc--;
                assert(jp->reg_alloc == 0);
                return;
            } else {
                assert(right_type->kind == TYPE_KIND_ARRAY);
                ir_gen_expr(jp, ast_statement->right);
                ir_gen_expr(jp, ast_statement->left);
                assert(jp->reg_alloc == 2);
                ir_gen_memorycopy(jp, left_type->bytes, left_type->align, jp->reg_alloc - 1, jp->reg_alloc - 2);
                jp->reg_alloc -= 2;
                assert(jp->reg_alloc == 0);
                return;
            }
        } else if(left_type->kind == TYPE_KIND_ARRAY_VIEW) {
            assert(right_type->kind == TYPE_KIND_ARRAY_VIEW);
            ir_gen_expr(jp, ast_statement->right);
            ir_gen_expr(jp, ast_statement->left);
            assert(jp->reg_alloc == 2);
            ir_gen_memorycopy(jp, left_type->bytes, left_type->align, jp->reg_alloc - 1, jp->reg_alloc - 2);
            jp->reg_alloc -= 2;
            assert(jp->reg_alloc == 0);
        } else if(left_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
            assert(right_type->kind == TYPE_KIND_DYNAMIC_ARRAY);
            ir_gen_expr(jp, ast_statement->right);
            ir_gen_expr(jp, ast_statement->left);
            assert(jp->reg_alloc == 2);
            ir_gen_memorycopy(jp, left_type->bytes, left_type->align, jp->reg_alloc - 1, jp->reg_alloc - 2);
            jp->reg_alloc -= 2;
            assert(jp->reg_alloc == 0);
        } else if(left_type->kind == TYPE_KIND_PROC) {
            assert(ast_statement->right);

            ir_gen_expr(jp, ast_statement->right);
            assert(jp->reg_alloc == 1);

            jp->reg_alloc--;

            if(ast_statement->left->kind == AST_KIND_atom) {
                AST_atom *left_atom = (AST_atom*)(ast_statement->left);
                Sym *sym = left_atom->symbol_annotation;
                assert(sym->type->kind == TYPE_KIND_PROC);

                IRinst inst = {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = sym->segment_offset,
                        .sym = sym,
                        .reg_src = jp->reg_alloc,
                        .bytes = 8,
                    },
                };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                assert(jp->reg_alloc == 0);
            } else {
                UNIMPLEMENTED;
            }

        } else if(TYPE_KIND_IS_RECORD(left_type->kind)) {
            ir_gen_expr(jp, ast_statement->right);
            ir_gen_expr(jp, ast_statement->left);
            //TODO this wastes a register
            assert(jp->reg_alloc == 2);
            ir_gen_memorycopy(jp, left_type->bytes, left_type->align, jp->reg_alloc - 1, jp->reg_alloc - 2);
            jp->reg_alloc -= 2;
            assert(jp->reg_alloc == 0);
        } else if(left_type->kind == TYPE_KIND_STRING) {
            ir_gen_expr(jp, ast_statement->right);
            ir_gen_expr(jp, ast_statement->left);
            assert(jp->reg_alloc == 2);
            ir_gen_memorycopy(jp, left_type->bytes, left_type->align, jp->reg_alloc - 1, jp->reg_alloc - 2);
            jp->reg_alloc -= 2;
            assert(jp->reg_alloc == 0);
        } else {
            UNIMPLEMENTED;
        }
    } else {
        if(ast_statement->right) {
            ir_gen_expr(jp, ast_statement->right);
        } else {
            assert(ast_statement->assign_op == TOKEN_PLUSPLUS || ast_statement->assign_op == TOKEN_MINUSMINUS);
            jp->reg_alloc++;
        }

        if(ast_statement->left->kind != AST_KIND_atom) {
            assert(ast_statement->left->kind == AST_KIND_expr);
            assert(is_lvalue(ast_statement->left));

            AST_expr *left_expr = (AST_expr*)ast_statement->left;

            //TODO merge the 2 cases below
            if(TYPE_KIND_IS_FLOAT(left_expr->type_annotation->kind)) {
                bool strided_access = false;
                bool has_field_offset = false;
                u64 field_offset = 0;

                //NOTE copypasta (serious copypasta)
                if(left_expr->token == '[') {
                    ir_gen_expr(jp, left_expr->left);

                    Type *array_type = ((AST_expr_base*)(left_expr->left))->type_annotation;
                    if(array_type->kind == TYPE_KIND_ARRAY_VIEW || array_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                        inst =
                            (IRinst) {
                                .opcode = IROP_LOAD,
                                .load = {
                                    .reg_dest = jp->reg_alloc - 1,
                                    .reg_src_ptr = jp->reg_alloc - 1,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    } else {
                        assert(array_type->kind == TYPE_KIND_ARRAY || array_type->kind == TYPE_KIND_POINTER);
                    }

                    ir_gen_expr(jp, left_expr->right);
                    assert(jp->reg_alloc == 2 && jp->float_reg_alloc == 1);
                    strided_access = true;
                } else if(left_expr->token == '.') {
                    ir_gen_expr(jp, left_expr->left);

                    AST_expr_base *dot_expr_left = (AST_expr_base*)(left_expr->left);

                    char *field = ((AST_atom*)(left_expr->right))->text;

                    Type *operand_type = dot_expr_left->type_annotation;

                    if(operand_type->kind == TYPE_KIND_POINTER) {
                        assert(operand_type->pointer.to->kind != TYPE_KIND_ARRAY);
                        assert(TYPE_KIND_IS_RECORD(operand_type->pointer.to->kind) || TYPE_KIND_IS_ARRAY_LIKE(operand_type->pointer.to->kind));
                        operand_type = operand_type->pointer.to;
                    }

                    if(TYPE_KIND_IS_RECORD(operand_type->kind)) {
                        Type *record_type = operand_type;

                        for(u64 i = 0; i < record_type->record.member.n; ++i) {
                            if(!strcmp(field, record_type->record.member.names[i])) {
                                field_offset = record_type->record.member.offsets[i];
                                break;
                            }
                        }
                    } else if(operand_type->kind == TYPE_KIND_STRING) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(String_view, data);
                        } else if(!strcmp(field, "len")) {
                            field_offset = offsetof(String_view, len);
                        } else {
                            UNREACHABLE;
                        }
                    } else if(operand_type->kind == TYPE_KIND_ARRAY_VIEW) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(Array_view, data);
                        } else if(!strcmp(field, "count")) {
                            field_offset = offsetof(Array_view, count);
                        } else {
                            UNREACHABLE;
                        }
                    } else if(operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(Dynamic_array, data);
                        } else if(!strcmp(field, "count")) {
                            field_offset = offsetof(Dynamic_array, count);
                        } else if(!strcmp(field, "cap")) {
                            field_offset = offsetof(Dynamic_array, cap);
                        } else if(!strcmp(field, "allocator")) {
                            field_offset = offsetof(Dynamic_array, allocator);
                        } else if(!strcmp(field, "allocator_data")) {
                            field_offset = offsetof(Dynamic_array, allocator_data);
                        } else {
                            UNREACHABLE;
                        }
                    } else {
                        UNREACHABLE;
                    }

                    assert(jp->reg_alloc == 1 && jp->float_reg_alloc == 1);
                    has_field_offset = true;
                } else {
                    assert(left_expr->token == '>' && left_expr->left == NULL);
                    ir_gen_expr(jp, left_expr->right);
                    assert(jp->reg_alloc == 1 && jp->float_reg_alloc == 1);
                }

                op_type = left_expr->type_annotation;
                op_bytes = op_type->bytes;

                read_reg = jp->float_reg_alloc;

                inst_read =
                    (IRinst) {
                        .opcode = IROP_LOADF,
                        .load = {
                            .reg_dest = read_reg,
                            .reg_src_ptr = 0,
                            .bytes = left_expr->type_annotation->bytes,
                        }
                    };

                inst_write =
                    (IRinst) {
                        .opcode = IROP_STORF,
                        .stor = {
                            .reg_dest_ptr = 0,
                            .reg_src = 0,
                            .bytes = left_expr->type_annotation->bytes,
                        }
                    };

                if(strided_access) {
                    inst_read.load.has_indirect_offset = true;
                    inst_read.load.offset_reg = 1;
                    inst_write.stor.has_indirect_offset = true;
                    inst_write.stor.offset_reg = 1;
                }

                if(has_field_offset) {
                    assert(!strided_access);
                    inst_read.load.has_immediate_offset = true;
                    inst_read.load.byte_offset_imm = field_offset;
                    inst_write.stor.has_immediate_offset = true;
                    inst_write.stor.byte_offset_imm = field_offset;
                }
            } else {
                bool strided_access = false;
                bool has_field_offset = false;
                u64 field_offset = 0;

                //NOTE copypasta (serious copypasta)
                if(left_expr->token == '[') {
                    ir_gen_expr(jp, left_expr->left);

                    Type *subscripted_type = ((AST_expr_base*)(left_expr->left))->type_annotation;
                    if(TYPE_KIND_IS_VIEW_LIKE(subscripted_type->kind) || subscripted_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                        inst =
                            (IRinst) {
                                .opcode = IROP_LOAD,
                                .load = {
                                    .reg_dest = jp->reg_alloc - 1,
                                    .reg_src_ptr = jp->reg_alloc - 1,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    } else {
                        assert(subscripted_type->kind == TYPE_KIND_ARRAY || subscripted_type->kind == TYPE_KIND_POINTER);
                    }

                    ir_gen_expr(jp, left_expr->right);
                    assert(jp->reg_alloc == 3);
                    strided_access = true;
                } else if(left_expr->token == '.') {
                    ir_gen_expr(jp, left_expr->left);

                    AST_expr_base *dot_expr_left = (AST_expr_base*)(left_expr->left);

                    char *field = ((AST_atom*)(left_expr->right))->text;

                    Type *operand_type = dot_expr_left->type_annotation;

                    if(operand_type->kind == TYPE_KIND_POINTER) {
                        assert(operand_type->pointer.to->kind != TYPE_KIND_ARRAY);
                        assert(TYPE_KIND_IS_RECORD(operand_type->pointer.to->kind) || TYPE_KIND_IS_ARRAY_LIKE(operand_type->pointer.to->kind));
                        operand_type = operand_type->pointer.to;
                    }

                    if(TYPE_KIND_IS_RECORD(operand_type->kind)) {
                        Type *record_type = operand_type;

                        for(u64 i = 0; i < record_type->record.member.n; ++i) {
                            if(!strcmp(field, record_type->record.member.names[i])) {
                                field_offset = record_type->record.member.offsets[i];
                                break;
                            }
                        }
                    } else if(operand_type->kind == TYPE_KIND_STRING) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(String_view, data);
                        } else if(!strcmp(field, "len")) {
                            field_offset = offsetof(String_view, len);
                        } else {
                            UNREACHABLE;
                        }
                    } else if(operand_type->kind == TYPE_KIND_ARRAY_VIEW) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(Array_view, data);
                        } else if(!strcmp(field, "count")) {
                            field_offset = offsetof(Array_view, count);
                        } else {
                            UNREACHABLE;
                        }
                    } else if(operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                        if(!strcmp(field, "data")) {
                            field_offset = offsetof(Dynamic_array, data);
                        } else if(!strcmp(field, "count")) {
                            field_offset = offsetof(Dynamic_array, count);
                        } else if(!strcmp(field, "cap")) {
                            field_offset = offsetof(Dynamic_array, cap);
                            fprintf(stderr,"field_offset = %lu\n", field_offset);
                        } else if(!strcmp(field, "allocator")) {
                            field_offset = offsetof(Dynamic_array, allocator);
                        } else if(!strcmp(field, "allocator_data")) {
                            field_offset = offsetof(Dynamic_array, allocator_data);
                        } else {
                            UNREACHABLE;
                        }
                    } else {
                        UNREACHABLE;
                    }

                    assert(jp->reg_alloc == 2);
                    has_field_offset = true;
                } else {
                    assert(left_expr->token == '>' && left_expr->left == NULL);
                    ir_gen_expr(jp, left_expr->right);
                    assert(jp->reg_alloc == 2);
                }

                assert(jp->reg_alloc == 2 || jp->reg_alloc == 3);

                op_type = left_expr->type_annotation;
                op_bytes = op_type->bytes;

                read_reg = jp->reg_alloc;

                inst_read =
                    (IRinst) {
                        .opcode = IROP_LOAD,
                        .load = {
                            .reg_dest = read_reg,
                            .reg_src_ptr = 1,
                            .bytes = left_expr->type_annotation->bytes,
                        }
                    };

                inst_write =
                    (IRinst) {
                        .opcode = IROP_STOR,
                        .stor = {
                            .reg_dest_ptr = 1,
                            .reg_src = 0,
                            .bytes = left_expr->type_annotation->bytes,
                        }
                    };

                if(strided_access) {
                    inst_read.load.has_indirect_offset = true;
                    inst_read.load.offset_reg = 2;
                    inst_write.stor.has_indirect_offset = true;
                    inst_write.stor.offset_reg = 2;
                }

                if(has_field_offset) {
                    assert(!strided_access);
                    inst_read.load.has_immediate_offset = true;
                    inst_read.load.byte_offset_imm = field_offset;
                    inst_write.stor.has_immediate_offset = true;
                    inst_write.stor.byte_offset_imm = field_offset;
                }
            }
        } else {
            AST_atom *atom = (AST_atom*)ast_statement->left;
            Sym *sym = atom->symbol_annotation;

            assert(sym->type->kind != TYPE_KIND_VOID);

            if(sym->is_global) {
                if(sym->value) {
                    segment = IRSEG_GLOBAL;
                } else {
                    segment = IRSEG_BSS;
                }
            } else {
                segment = IRSEG_LOCAL;
            }

            if(sym->type->kind >= TYPE_KIND_FLOAT && sym->type->kind <= TYPE_KIND_F64) {
                opcode_read = IROP_GETVARF;
                opcode_write = IROP_SETVARF;
            } else {
                opcode_read = IROP_GETVAR;
                opcode_write = IROP_SETVAR;
            }

            op_type = sym->type;
            op_bytes = op_type->bytes;

            read_reg = 1;

            inst_read =
                (IRinst) {
                    .opcode = opcode_read,
                    .getvar = {
                        .segment = segment,
                        .offset = sym->segment_offset,
                        .sym = sym,
                        .reg_dest = read_reg,
                        .bytes = sym->type->bytes,
                    }
                };

            inst_write =
                (IRinst) {
                    .opcode = opcode_write,
                    .setvar = {
                        .segment = segment,
                        .offset = sym->segment_offset,
                        .sym = sym,
                        .reg_src = 0,
                        .bytes = sym->type->bytes,
                    }
                };
        }

        if(ast_statement->assign_op != '=') {
            inst_read.loc = jp->cur_loc;
            arrpush(jp->instructions, inst_read);

            inst =
                (IRinst) {
                    .arith = {
                        .operand_bytes = { op_bytes, op_bytes, op_bytes },
                        .reg = { 0, read_reg, 0 },
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);
        }

        switch(ast_statement->assign_op) {
            default:
                UNREACHABLE;
            case '=':
                break;
            case TOKEN_PLUSEQUAL:
                if(op_type->kind == TYPE_KIND_POINTER) {
                    assert(op_type->pointer.to->kind != TYPE_KIND_VOID);
                    arrlast(jp->instructions) =
                        (IRinst) {
                            .opcode = IROP_CALCPTROFFSET,
                            .calcptroffset = {
                                .reg_dest = 0,
                                .reg_src_ptr = read_reg,
                                .offset_reg = 0,
                                .stride = op_type->pointer.to->bytes,
                            },
                        };
                } else {
                    arrlast(jp->instructions).opcode =
                        (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                        ? IROP_FADD
                        : IROP_ADD;
                }
                break;
            case TOKEN_MINUSEQUAL:
                if(op_type->kind == TYPE_KIND_POINTER) {
                    assert(op_type->pointer.to->kind != TYPE_KIND_VOID);
                    arrlast(jp->instructions) =
                        (IRinst) {
                            .opcode = IROP_NEG,
                            .arith = {
                                .operand_bytes = { op_bytes, op_bytes },
                                .reg = { 0, 0 },
                            },
                        };
                    inst =
                        (IRinst) {
                            .opcode = IROP_CALCPTROFFSET,
                            .calcptroffset = {
                                .reg_dest = 0,
                                .reg_src_ptr = read_reg,
                                .offset_reg = 0,
                                .stride = op_type->pointer.to->bytes,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                } else {
                    arrlast(jp->instructions).opcode =
                        (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                        ? IROP_FSUB
                        : IROP_SUB;
                }
                break;
            case TOKEN_TIMESEQUAL:
                arrlast(jp->instructions).opcode =
                    (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                    ? IROP_FMUL
                    : IROP_MUL;
                break;
            case TOKEN_DIVEQUAL:
                arrlast(jp->instructions).opcode =
                    (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                    ? IROP_FDIV
                    : IROP_DIV;
                break;
            case TOKEN_MODEQUAL:
                arrlast(jp->instructions).opcode = IROP_MOD;
                break;
            case TOKEN_ANDEQUAL:
                arrlast(jp->instructions).opcode = IROP_AND;
                break;
            case TOKEN_OREQUAL:
                arrlast(jp->instructions).opcode = IROP_OR;
                break;
            case TOKEN_LSHIFTEQUAL:
                arrlast(jp->instructions).opcode = IROP_LSHIFT;
                break;
            case TOKEN_RSHIFTEQUAL:
                arrlast(jp->instructions).opcode = IROP_RSHIFT;
                break;
            case TOKEN_XOREQUAL:
                arrlast(jp->instructions).opcode = IROP_XOR;
                break;
            case TOKEN_PLUSPLUS:
                if(op_type->kind == TYPE_KIND_POINTER) {
                    assert(op_type->pointer.to->kind != TYPE_KIND_VOID);
                    arrlast(jp->instructions) =
                        (IRinst) {
                            .opcode = IROP_LOAD,
                            .load = {
                                .reg_dest = 0,
                                .imm.integer = 1,
                                .bytes = 8,
                                .immediate = true,
                            },
                        };
                    inst =
                        (IRinst) {
                            .opcode = IROP_CALCPTROFFSET,
                            .calcptroffset = {
                                .reg_dest = 0,
                                .reg_src_ptr = read_reg,
                                .offset_reg = 0,
                                .stride = op_type->pointer.to->bytes,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                } else {
                    arrlast(jp->instructions).opcode =
                        (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                        ? IROP_FADD
                        : IROP_ADD;
                    if(op_type->kind == TYPE_KIND_F64)
                        arrlast(jp->instructions).arith.imm.floating64 = 1.0;
                    else if(op_type->kind >= TYPE_KIND_FLOAT)
                        arrlast(jp->instructions).arith.imm.floating32 = 1.0;
                    else
                        arrlast(jp->instructions).arith.imm.integer = 1;
                    arrlast(jp->instructions).arith.immediate = true;
                }
                break;
            case TOKEN_MINUSMINUS:
                if(op_type->kind == TYPE_KIND_POINTER) {
                    assert(op_type->pointer.to->kind != TYPE_KIND_VOID);
                    arrlast(jp->instructions) =
                        (IRinst) {
                            .opcode = IROP_LOAD,
                            .load = {
                                .reg_dest = 0,
                                .imm.integer = -1,
                                .bytes = 8,
                                .immediate = true,
                            },
                        };
                    inst =
                        (IRinst) {
                            .opcode = IROP_CALCPTROFFSET,
                            .calcptroffset = {
                                .reg_dest = 0,
                                .reg_src_ptr = read_reg,
                                .offset_reg = 0,
                                .stride = op_type->pointer.to->bytes,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                } else {
                    arrlast(jp->instructions).opcode =
                        (op_type->kind >= TYPE_KIND_FLOAT && op_type->kind <= TYPE_KIND_F64)
                        ? IROP_FSUB
                        : IROP_SUB;
                    if(op_type->kind == TYPE_KIND_F64)
                        arrlast(jp->instructions).arith.imm.floating64 = 1.0;
                    else if(op_type->kind >= TYPE_KIND_FLOAT)
                        arrlast(jp->instructions).arith.imm.floating32 = 1.0;
                    else
                        arrlast(jp->instructions).arith.imm.integer = 1;
                    arrlast(jp->instructions).arith.immediate = true;
                }
                break;
        }

        inst_write.loc = jp->cur_loc;
        arrpush(jp->instructions, inst_write);
    }


    jp->reg_alloc = 0;
    jp->float_reg_alloc = 0;
}

void ir_gen_block(Job *jp, AST *ast) {
    IRinst inst;

    AST_statement *defer_list = NULL;
    jp->cur_loc = ast->loc;

    while(ast != NULL) {
        jp->cur_loc = ast->loc;

        switch(ast->kind) {
            default:
                UNIMPLEMENTED;
            case AST_KIND_procdecl:
                //TODO ignore need to spawn child job for nested procedures
                UNIMPLEMENTED;
                break;
            case AST_KIND_ifstatement:
                {
                    arrpush(jp->defer_list_stack, defer_list);

                    AST_ifstatement *ast_if = (AST_ifstatement*)ast;

                    u64 last_label = jp->label_alloc;
                    jp->label_alloc++;
                    u64 first_label = jp->label_alloc;
                    jp->label_alloc++;

                    //TODO add implicit casts to if and while conditions
                    ir_gen_expr(jp, ast_if->condition);

                    assert(jp->reg_alloc == 1);
                    jp->reg_alloc = 0;

                    inst =
                        (IRinst) {
                            .opcode = IROP_IF,
                            .branch = {
                                .cond_reg = jp->reg_alloc,
                                .label_id = first_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    Arr(u64) branch_labels = NULL;
                    Arr(AST*) branches = NULL;
                    arrpush(branches, ast_if->branch);

                    IRlabel if_label;

                    if(ast_if->label) {
                        if_label = (IRlabel) {
                            .key = ast_if->label,
                                .keyword = TOKEN_IF,
                                .break_label = last_label,
                                .loc = ast->loc,
                        };

                        if(!job_label_create(jp, if_label)) {
                            if_label = job_label_lookup(jp, ast_if->label);
                            job_error(jp, ast->loc, "label '%s' redeclared, originally declared on line '%i'",
                                    ast_if->label, if_label.loc.line);
                            return;
                        }
                    }

                    while(arrlast(branches) && arrlast(branches)->kind == AST_KIND_ifstatement) {
                        AST_ifstatement *ast_else_if = (AST_ifstatement*)arrlast(branches);
                        jp->cur_loc = ast_else_if->base.loc;

                        ir_gen_expr(jp, ast_else_if->condition);

                        assert(jp->reg_alloc == 1);
                        jp->reg_alloc = 0;
                        inst =
                            (IRinst) {
                                .opcode = IROP_IF,
                                .branch = {
                                    .cond_reg = jp->reg_alloc,
                                    .label_id = jp->label_alloc,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                        arrpush(branch_labels, jp->label_alloc);
                        arrpush(branches, ast_else_if->branch);

                        jp->label_alloc++;
                    }

                    if(arrlast(branches) == NULL)
                        arrsetlen(branches, arrlen(branches) - 1);

                    if(arrlen(branches) > 0) {
                        assert(arrlen(branches) == arrlen(branch_labels) || arrlen(branches) == arrlen(branch_labels) + 1);

                        if(arrlast(branches)->kind != AST_KIND_ifstatement) {
                            ir_gen_block(jp, arrpop(branches));

                            if(jp->state == JOB_STATE_ERROR) return;
                        }

                        while(arrlen(branches) > 0) {
                            AST_ifstatement *ast_else_if = (AST_ifstatement*)arrpop(branches);

                            jp->cur_loc = ast_else_if->base.loc;
                            inst =
                                (IRinst) {
                                    .opcode = IROP_JMP,
                                    .branch = {
                                        .label_id = last_label,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            inst =
                                (IRinst) {
                                    .opcode = IROP_LABEL,
                                    .label = {
                                        .id = arrpop(branch_labels),
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            ir_gen_block(jp, ast_else_if->body);
                            if(jp->state == JOB_STATE_ERROR) return;
                        }
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_JMP,
                            .branch = {
                                .label_id = last_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = first_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ir_gen_block(jp, ast_if->body);

                    if(jp->state == JOB_STATE_ERROR) return;

                    if(ast_if->label) {
                        shdel(jp->label_table, if_label.key);
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = last_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    arrfree(branch_labels);
                    arrfree(branches);

                    ast = ast_if->next;

                    arrsetlen(jp->defer_list_stack, arrlen(jp->defer_list_stack) - 1);
                }
                break;
            case AST_KIND_whilestatement:
                {
                    arrpush(jp->defer_list_stack, defer_list);

                    AST_whilestatement *ast_while = (AST_whilestatement*)ast;

                    u64 last_label = jp->label_alloc;
                    jp->label_alloc++;

                    u64 cond_label = jp->label_alloc;
                    jp->label_alloc++;

                    u64 body_label = jp->label_alloc;
                    jp->label_alloc++;

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = cond_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ir_gen_expr(jp, ast_while->condition);

                    assert(jp->reg_alloc == 1);
                    jp->reg_alloc = 0;

                    inst =
                        (IRinst) {
                            .opcode = IROP_IF,
                            .branch = {
                                .cond_reg = jp->reg_alloc,
                                .label_id = body_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_JMP,
                            .branch = {
                                .label_id = last_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = body_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    IRlabel while_label;

                    if(ast_while->label) {
                        while_label = (IRlabel) {
                            .key = ast_while->label,
                                .keyword = TOKEN_WHILE,
                                .continue_label = cond_label,
                                .break_label = last_label,
                                .loc = ast->loc,
                        };

                        if(!job_label_create(jp, while_label)) {
                            while_label = job_label_lookup(jp, ast_while->label);
                            job_error(jp, ast->loc, "label '%s' redeclared, originally declared on line '%i'",
                                    ast_while->label, while_label.loc.line);
                            return;
                        }
                    }

                    arrpush(jp->break_label, last_label);
                    arrpush(jp->continue_label, cond_label);

                    ir_gen_block(jp, ast_while->body);

                    if(jp->state == JOB_STATE_ERROR) return;

                    arrsetlen(jp->break_label, arrlen(jp->break_label) - 1);
                    arrsetlen(jp->continue_label, arrlen(jp->continue_label) - 1);

                    if(ast_while->label) {
                        shdel(jp->label_table, while_label.key);
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_JMP,
                            .branch = {
                                .label_id = cond_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = last_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ast = ast_while->next;

                    arrsetlen(jp->defer_list_stack, arrlen(jp->defer_list_stack) - 1);
                }
                break;
            case AST_KIND_forstatement:
                {
                    arrpush(jp->defer_list_stack, defer_list);

                    AST_forstatement *ast_for = (AST_forstatement*)ast;

                    u64 last_label = jp->label_alloc;
                    jp->label_alloc++;

                    u64 update_label = jp->label_alloc;
                    jp->label_alloc++;

                    u64 cond_label = jp->label_alloc;
                    jp->label_alloc++;

                    u64 body_label = jp->label_alloc;
                    jp->label_alloc++;

                    arrpush(jp->local_offset, arrlast(jp->local_offset));

                    u64 it_index_addr = 0;

                    if(!ast_for->is_range_for) {
                        assert(ast_for->it_index_symbol->type->align == 8 && ast_for->it_index_symbol->type->bytes == 8);

                        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), 8);
                        it_index_addr = arrlast(jp->local_offset);
                        arrlast(jp->local_offset) += 8;
                        ast_for->it_index_symbol->segment_offset = it_index_addr;
                    }

                    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), ast_for->it_symbol->type->align);
                    u64 it_addr = arrlast(jp->local_offset);
                    Type *it_type = ast_for->it_symbol->type;
                    arrlast(jp->local_offset) += ast_for->it_symbol->type->bytes;
                    ast_for->it_symbol->segment_offset = it_addr;

                    if(ast_for->label) {
                        ast_for->named_it_symbol->segment_offset = it_addr;
                        if(!ast_for->is_range_for) {
                            ast_for->named_it_index_symbol->segment_offset = it_index_addr;
                        }
                    }

                    if(ast_for->is_range_for) {
                        u64 end_range_addr = 0;

                        /* setup begin and end of range */
                        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), ast_for->end_range_type->align);
                        end_range_addr = arrlast(jp->local_offset);
                        arrlast(jp->local_offset) += ast_for->end_range_type->bytes;

                        ir_gen_expr(jp, ast_for->end_range_expr);
                        assert(jp->reg_alloc == 1);
                        jp->reg_alloc = 0;

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = end_range_addr,
                                    .reg_src = 0,
                                    .bytes = ast_for->end_range_type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        ir_gen_expr(jp, ast_for->begin_range_expr);
                        assert(jp->reg_alloc == 1);
                        jp->reg_alloc = 0;

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_addr,
                                    .reg_src = 0,
                                    .bytes = ast_for->begin_range_type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_JMP,
                                .branch = {
                                    .label_id = cond_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = update_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        /* increment or decrement it depending on the order of the loop */
                        inst =
                            (IRinst) {
                                .opcode = IROP_GETVAR,
                                .getvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_addr,
                                    .reg_dest = 0,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = ast_for->reverse_order ? IROP_SUB : IROP_ADD,
                                .arith = {
                                    .operand_bytes = { 8, 8, 8 },
                                    .reg = { 0, 0, },
                                    .imm.integer = 1,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_addr,
                                    .reg_src = 0,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = cond_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        assert(jp->reg_alloc == 0);

                        inst =
                            (IRinst) {
                                .opcode = IROP_GETVAR,
                                .getvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = end_range_addr,
                                    .reg_dest = 1,
                                    .bytes = ast_for->end_range_type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        if(ast_for->reverse_order) {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LE,
                                    .arith = {
                                        .operand_bytes = { it_type->bytes, ast_for->end_range_type->bytes, it_type->bytes },
                                        .reg = { 0, 1, 0 },
                                        .sign = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            //TODO should ranges be inclusive?
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LE,
                                    .arith = {
                                        .operand_bytes = { it_type->bytes, it_type->bytes, ast_for->end_range_type->bytes },
                                        .reg = { 0, 0, 1 },
                                        .sign = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }

                        inst =
                            (IRinst) {
                                .opcode = IROP_IF,
                                .branch = {
                                    .cond_reg = 0,
                                    .label_id = body_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_JMP,
                                .branch = {
                                    .label_id = last_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = body_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                    } else {
                        /* save the address of the iterable */
                        ir_gen_expr(jp, ast_for->expr);
                        assert(jp->reg_alloc == 1);
                        jp->reg_alloc = 0;

                        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), 8);
                        u64 iterable_addr = arrlast(jp->local_offset);
                        arrlast(jp->local_offset) += 8;

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = iterable_addr,
                                    .reg_src = 0,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        if(ast_for->reverse_order) {
                            u64 iterable_count_offset = 0;

                            if(ast_for->expr_type->kind == TYPE_KIND_ARRAY) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = 0,
                                            .imm.integer = ast_for->expr_type->array.n - 1,
                                            .bytes = 8,
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else {
                                if(ast_for->expr_type->kind == TYPE_KIND_ARRAY_VIEW) {
                                    iterable_count_offset = offsetof(Array_view, count);
                                } else if(ast_for->expr_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                                    iterable_count_offset = offsetof(Dynamic_array, count);
                                } else if(ast_for->expr_type->kind == TYPE_KIND_STRING) {
                                    iterable_count_offset = offsetof(String_view, len);
                                } 

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = 0,
                                            .reg_src_ptr = 0,
                                            .byte_offset_imm = iterable_count_offset,
                                            .bytes = 8,
                                            .has_immediate_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SUB,
                                        .arith = {
                                            .operand_bytes = { 8, 8, 8 },
                                            .reg = { 0, 0 },
                                            .imm.integer = 1,
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }

                            inst =
                                (IRinst) {
                                    .opcode = IROP_SETVAR,
                                    .setvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = it_index_addr,
                                        .reg_src = 0,
                                        .bytes = 8,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                        } else {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = 0,
                                        .bytes = 8,
                                        .immediate = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            inst =
                                (IRinst) {
                                    .opcode = IROP_SETVAR,
                                    .setvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = it_index_addr,
                                        .bytes = 8,
                                        .immediate = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }

                        inst =
                            (IRinst) {
                                .opcode = IROP_JMP,
                                .branch = {
                                    .label_id = cond_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = update_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        /* increment or decrement the index depending on the order of the loop */
                        inst =
                            (IRinst) {
                                .opcode = IROP_GETVAR,
                                .getvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_index_addr,
                                    .reg_dest = jp->reg_alloc,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = ast_for->reverse_order ? IROP_SUB : IROP_ADD,
                                .arith = {
                                    .operand_bytes = { 8, 8, 8 },
                                    .reg = { jp->reg_alloc, jp->reg_alloc, },
                                    .imm.integer = 1,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_index_addr,
                                    .reg_src = jp->reg_alloc,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = cond_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        assert(jp->reg_alloc == 0);

                        // generate the condition depending on loop order
                        if(ast_for->reverse_order) {
                            // here we just check if it_index >= 0, signed comparison
                            assert(jp->reg_alloc == 0);

                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = 1,
                                        .bytes = 8,
                                        .immediate = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            inst =
                                (IRinst) {
                                    .opcode = IROP_LE,
                                    .arith = {
                                        .operand_bytes = { 1, 8, 8 },
                                        .reg = { 0, 1, 0 },
                                        .sign = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            //TODO might be a better idea if the count and cap of arrays is an s64, this avoids potential sign problems

                            if(ast_for->expr_type->kind == TYPE_KIND_ARRAY) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = jp->reg_alloc + 1,
                                            .imm.integer = ast_for->expr_type->array.n,
                                            .bytes = 8,
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_GETVAR,
                                        .getvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = iterable_addr,
                                            .reg_dest = jp->reg_alloc + 1,
                                            .bytes = 8,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                u64 iterable_count_offset = 0;

                                if(ast_for->expr_type->kind == TYPE_KIND_ARRAY_VIEW) {
                                    iterable_count_offset = offsetof(Array_view, count);
                                } else if(ast_for->expr_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                                    iterable_count_offset = offsetof(Dynamic_array, count);
                                } else if(ast_for->expr_type->kind == TYPE_KIND_STRING) {
                                    iterable_count_offset = offsetof(String_view, len);
                                } 

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = jp->reg_alloc + 1,
                                            .reg_src_ptr = jp->reg_alloc + 1,
                                            .byte_offset_imm = iterable_count_offset,
                                            .bytes = 8,
                                            .has_immediate_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }

                            /* at this point the it_index should be in register number 'jp->reg_alloc'  */

                            assert(jp->reg_alloc == 0);

                            inst =
                                (IRinst) {
                                    .opcode = IROP_GT,
                                    .arith = {
                                        .operand_bytes = { 1, 8, 8 },
                                        .reg = { 0, 1, 0 },
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }

                        inst =
                            (IRinst) {
                                .opcode = IROP_IF,
                                .branch = {
                                    .cond_reg = jp->reg_alloc,
                                    .label_id = body_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_JMP,
                                .branch = {
                                    .label_id = last_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_LABEL,
                                .label = {
                                    .id = body_label,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        /* use the index to grab the address of the current element of the array */
                        u64 iterable_data_offset = 0;

                        inst =
                            (IRinst) {
                                .opcode = IROP_GETVAR,
                                .getvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = iterable_addr,
                                    .reg_dest = jp->reg_alloc,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_GETVAR,
                                .getvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = it_index_addr,
                                    .reg_dest = jp->reg_alloc + 1,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        if(ast_for->expr_type->kind != TYPE_KIND_ARRAY) {
                            if(ast_for->expr_type->kind == TYPE_KIND_ARRAY_VIEW) {
                                iterable_data_offset = offsetof(Array_view, data);
                            } else if(ast_for->expr_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                                iterable_data_offset = offsetof(Dynamic_array, data);
                            } else if(ast_for->expr_type->kind == TYPE_KIND_STRING) {
                                iterable_data_offset = offsetof(String_view, data);
                            } 

                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = jp->reg_alloc,
                                        .reg_src_ptr = jp->reg_alloc,
                                        .bytes = sizeof(void*),
                                        .byte_offset_imm = iterable_data_offset,
                                        .has_immediate_offset = true,
                                    }
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }

                        inst =
                            (IRinst) {
                                .opcode = IROP_CALCPTROFFSET,
                                .calcptroffset = {
                                    .reg_dest = jp->reg_alloc,
                                    .reg_src_ptr = jp->reg_alloc,
                                    .offset_reg = jp->reg_alloc + 1,
                                    .stride = it_type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        /* if the for loop is iterating by pointer don't do the memcopy in to 'it', otherwise do the copy */
                        if(ast_for->by_pointer) {
                            assert(it_type->kind == TYPE_KIND_POINTER);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_SETVAR,
                                    .setvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = it_addr,
                                        .reg_src = jp->reg_alloc,
                                        .bytes = 8,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_ADDRVAR,
                                    .addrvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = it_addr,
                                        .reg_dest = jp->reg_alloc + 1,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            for(u64 offset = 0; offset < it_type->bytes; offset += it_type->align) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = jp->reg_alloc + 2,
                                            .reg_src_ptr = jp->reg_alloc,
                                            .byte_offset_imm = offset,
                                            .bytes = it_type->align,
                                            .has_immediate_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_STOR,
                                        .stor = {
                                            .reg_dest_ptr = jp->reg_alloc + 1,
                                            .reg_src = jp->reg_alloc + 2,
                                            .byte_offset_imm = offset,
                                            .bytes = it_type->align,
                                            .has_immediate_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }
                        }
                    }

                    IRlabel for_label;

                    if(ast_for->label) {
                        for_label = (IRlabel) {
                            .key = ast_for->label,
                                .keyword = TOKEN_FOR,
                                .continue_label = update_label,
                                .break_label = last_label,
                                .loc = ast->loc,
                        };

                        if(!job_label_create(jp, for_label)) {
                            for_label = job_label_lookup(jp, ast_for->label);
                            job_error(jp, ast->loc, "label '%s' redeclared, originally declared on line '%i'",
                                    ast_for->label, for_label.loc.line);
                            return;
                        }
                    }

                    arrpush(jp->break_label, last_label);
                    arrpush(jp->continue_label, cond_label);

                    ir_gen_block(jp, ast_for->body);

                    if(jp->state == JOB_STATE_ERROR) return;

                    arrsetlen(jp->break_label, arrlen(jp->break_label) - 1);
                    arrsetlen(jp->continue_label, arrlen(jp->continue_label) - 1);

                    if(ast_for->label) {
                        shdel(jp->label_table, for_label.key);
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_JMP,
                            .branch = {
                                .label_id = update_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = {
                                .id = last_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ast = ast_for->next;

                    arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

                    arrsetlen(jp->defer_list_stack, arrlen(jp->defer_list_stack) - 1);
                }
                break;
            case AST_KIND_push_context:
                {
                    AST_push_context *ast_push = (AST_push_context*)ast;
                    AST_block *ast_block = (AST_block*)(ast_push->down);

                    arrpush(jp->local_offset, arrlast(jp->local_offset));

                    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), 8);

                    u64 context_save_offset = arrlast(jp->local_offset);
                    arrlast(jp->local_offset) += 8;

                    assert(jp->reg_alloc == 0);

                    inst =
                        (IRinst) {
                            .opcode = IROP_GETVAR,
                            .getvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = 0,
                                .reg_dest = 0,
                                .bytes = 8,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = context_save_offset,
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_ADDRVAR,
                            .addrvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = ast_push->symbol_annotation->segment_offset,
                                .reg_dest = 0,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = 0,
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    arrpush(jp->defer_list_stack, defer_list);

                    ir_gen_block(jp, ast_block->down);

                    arrsetlen(jp->defer_list_stack, arrlen(jp->defer_list_stack) - 1);

                    inst =
                        (IRinst) {
                            .opcode = IROP_GETVAR,
                            .getvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = context_save_offset,
                                .reg_dest = 0,
                                .bytes = 8,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = 0,
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    if(jp->max_local_offset < arrlast(jp->local_offset))
                        jp->max_local_offset = arrlast(jp->local_offset);

                    if(jp->state == JOB_STATE_ERROR) return;

                    arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

                    printf("jp->reg_alloc %lu\n", jp->reg_alloc);
                    assert(jp->reg_alloc == 0);
                    assert(jp->float_reg_alloc == 0);

                    ast = ast_push->next;
                }
                break;
            case AST_KIND_block:
                {
                    AST_block *ast_block = (AST_block*)ast;

                    arrpush(jp->local_offset, arrlast(jp->local_offset));

                    arrpush(jp->defer_list_stack, defer_list);

                    ir_gen_block(jp, ast_block->down);

                    arrsetlen(jp->defer_list_stack, arrlen(jp->defer_list_stack) - 1);

                    if(jp->max_local_offset < arrlast(jp->local_offset))
                        jp->max_local_offset = arrlast(jp->local_offset);

                    if(jp->state == JOB_STATE_ERROR) return;

                    arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

                    printf("jp->reg_alloc %lu\n", jp->reg_alloc);
                    assert(jp->reg_alloc == 0);
                    assert(jp->float_reg_alloc == 0);

                    ast = ast_block->next;
                }
                break;
            case AST_KIND_returnstatement:
                {
                    AST_returnstatement *ast_return = (AST_returnstatement*)ast;

                    ir_gen_deferred(jp, defer_list);
                    for(int i = arrlen(jp->defer_list_stack) - 1; i >= 0; --i) {
                        AST_statement *d = jp->defer_list_stack[i];
                        ir_gen_deferred(jp, d);
                    }

                    Type *proc_type = jp->cur_proc_type;
                    u64 i = 0;
                    int non_scalar_returns = 0;
                    for(AST_expr_list *expr_list = ast_return->expr_list; expr_list; expr_list = expr_list->next) {
                        Type *ret_type = proc_type->proc.ret.types[i];

                        if(TYPE_KIND_IS_NOT_SCALAR(ret_type->kind)) {
                            if(ret_type->kind == TYPE_KIND_ARRAY) {
                                if(expr_list->expr->kind == AST_KIND_array_literal) {
                                    assert(jp->float_reg_alloc == 0);
                                    assert(jp->reg_alloc == 0);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_GETVAR,
                                            .getvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = 8 + (non_scalar_returns << 3),
                                                .reg_dest = 0,
                                                .bytes = 8,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                    jp->reg_alloc++;
                                    ir_gen_copy_array_literal(jp, ret_type, (AST_array_literal*)(expr_list->expr), 0, 0);
                                    jp->reg_alloc--;
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETRET,
                                            .setport = {
                                                .port = i,
                                                .bytes = 8,
                                                .reg_src = 0,
                                                .c_call = proc_type->proc.c_call,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else {
                                    assert(jp->float_reg_alloc == 0);
                                    assert(jp->reg_alloc == 0);
                                    ir_gen_expr(jp, expr_list->expr);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_GETVAR,
                                            .getvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = non_scalar_returns << 3,
                                                .reg_dest = jp->reg_alloc,
                                                .bytes = 8,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                    ir_gen_memorycopy(jp, ret_type->bytes, ret_type->align, jp->reg_alloc, jp->reg_alloc - 1);
                                    assert(jp->reg_alloc == 1);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETRET,
                                            .setport = {
                                                .port = i,
                                                .bytes = 8,
                                                .reg_src = jp->reg_alloc,
                                                .c_call = proc_type->proc.c_call,
                                            },
                                        };
                                    jp->reg_alloc--;
                                    assert(jp->reg_alloc == 0);
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                }
                            } else if(ret_type->kind == TYPE_KIND_ARRAY_VIEW || ret_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                                if(expr_list->expr->kind == AST_KIND_array_literal) {
                                    job_error(jp, expr_list->base.loc, "can't return local array literal as array view");
                                    return;
                                } else {
                                    AST_expr_base *expr_base = (AST_expr_base*)(expr_list->expr);
                                    Type *expr_type = expr_base->type_annotation;

                                    if(expr_type->kind == TYPE_KIND_ARRAY) {
                                        //TODO maybe throw error if user tries to return a local array as a view
                                        assert(jp->float_reg_alloc == 0);
                                        assert(jp->reg_alloc == 0);
                                        ir_gen_expr(jp, expr_list->expr);
                                        assert(jp->reg_alloc == 1);
                                        jp->reg_alloc--;
                                        assert(jp->reg_alloc == 0);

                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_GETVAR,
                                                .getvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = 8 + (non_scalar_returns << 3),
                                                    .reg_dest = jp->reg_alloc + 1,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);

                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_STOR,
                                                .stor = {
                                                    .reg_dest_ptr = jp->reg_alloc + 1,
                                                    .reg_src = jp->reg_alloc,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);

                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_STOR,
                                                .stor = {
                                                    .reg_dest_ptr = jp->reg_alloc + 1,
                                                    .imm.integer = expr_type->array.n,
                                                    .immediate = true,
                                                    .bytes = 8,
                                                    .has_immediate_offset = true,
                                                    .byte_offset_imm = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);

                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETRET,
                                                .setport = {
                                                    .port = i,
                                                    .bytes = 8,
                                                    .reg_src = jp->reg_alloc + 1,
                                                    .c_call = proc_type->proc.c_call,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                    } else {
                                        //TODO too much implicit state
                                        assert(expr_type->kind == TYPE_KIND_ARRAY_VIEW || expr_type->kind == TYPE_KIND_DYNAMIC_ARRAY);
                                        assert(jp->float_reg_alloc == 0);
                                        assert(jp->reg_alloc == 0);
                                        ir_gen_expr(jp, expr_list->expr);
                                        assert(jp->reg_alloc == 1);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_GETVAR,
                                                .getvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = 8 + (non_scalar_returns << 3),
                                                    .reg_dest = jp->reg_alloc,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        ir_gen_memorycopy(jp, ret_type->bytes, ret_type->align, jp->reg_alloc, jp->reg_alloc - 1);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETRET,
                                                .setport = {
                                                    .port = i,
                                                    .bytes = 8,
                                                    .reg_src = jp->reg_alloc,
                                                    .c_call = proc_type->proc.c_call,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        jp->reg_alloc--;
                                        assert(jp->reg_alloc == 0);
                                    }
                                }
                            } else {
                                AST_expr_base *expr_base = (AST_expr_base*)(expr_list->expr);
                                Type *expr_type = expr_base->type_annotation;

                                assert(TYPE_KIND_IS_RECORD(expr_type->kind) || expr_type->kind == TYPE_KIND_STRING);

                                assert(jp->float_reg_alloc == 0);
                                assert(jp->reg_alloc == 0);
                                ir_gen_expr(jp, expr_list->expr);
                                assert(jp->reg_alloc == 1);
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_GETVAR,
                                        .getvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = 8 + (non_scalar_returns << 3),
                                            .reg_dest = jp->reg_alloc,
                                            .bytes = 8,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                                ir_gen_memorycopy(jp, ret_type->bytes, ret_type->align, jp->reg_alloc, jp->reg_alloc - 1);
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETRET,
                                        .setport = {
                                            .port = i,
                                            .bytes = 8,
                                            .reg_src = jp->reg_alloc,
                                            .c_call = proc_type->proc.c_call,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                                jp->reg_alloc--;
                                assert(jp->reg_alloc == 0);
                            }

                            non_scalar_returns++;
                        } else {
                            ir_gen_expr(jp, expr_list->expr);

                            IRop opcode;

                            if(TYPE_KIND_IS_FLOAT(ret_type->kind)) {
                                opcode = IROP_SETRETF;
                                assert(jp->float_reg_alloc == 1);
                                jp->float_reg_alloc = 0;
                            } else {
                                opcode = IROP_SETRET;
                                assert(jp->reg_alloc == 1);
                                jp->reg_alloc = 0;
                            }
                            inst =
                                (IRinst) {
                                    .opcode = opcode,
                                    .setport = {
                                        .port = i,
                                        .bytes = proc_type->proc.ret.types[i]->bytes,
                                        .reg_src = 0,
                                        .c_call = proc_type->proc.c_call,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }

                        i++;
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_RET,
                            .ret.c_call = proc_type->proc.c_call,
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ast = ast_return->next;
                }
                break;
            case AST_KIND_vardecl:
                {
                    AST_vardecl *ast_vardecl = (AST_vardecl*)ast;
                    ast = ast_vardecl->next;

                    if(ast_vardecl->constant) {
                        assert(jp->float_reg_alloc == 0);
                        assert(jp->reg_alloc == 0);
                        continue;
                    }

                    if(ast_vardecl->uninitialized) {
                        assert(jp->float_reg_alloc == 0);
                        assert(jp->reg_alloc == 0);
                        arrlast(jp->local_offset) =
                            align_up(arrlast(jp->local_offset), ast_vardecl->symbol_annotation->type->align);
                        ast_vardecl->symbol_annotation->segment_offset = arrlast(jp->local_offset);
                        arrlast(jp->local_offset) += ast_vardecl->symbol_annotation->type->bytes;
                        continue;
                    }

                    AST *init_expr = ast_vardecl->init;
                    Sym *sym = ast_vardecl->symbol_annotation;
                    //TODO the local_offset should be incremented here, we need to refactor ir_gen_array_literal() to not increment local_offset for this

                    assert(sym->segment == IRSEG_LOCAL);

                    arrlast(jp->local_offset) =
                        align_up(arrlast(jp->local_offset), sym->type->align);
                    sym->segment_offset = arrlast(jp->local_offset);

                    Type *var_type = sym->type;

                    if(TYPE_KIND_IS_NOT_SCALAR(var_type->kind)) {
                        if(var_type->kind == TYPE_KIND_ARRAY) {
                            if(init_expr) {
                                if(init_expr->kind == AST_KIND_array_literal) {
                                    AST_array_literal *array_literal = (AST_array_literal*)(init_expr);
                                    u64 array_data_offset = ir_gen_array_literal(jp, var_type, array_literal);
                                    assert(array_data_offset == sym->segment_offset);
                                } else {
                                    u64 offset = arrlast(jp->local_offset);
                                    arrlast(jp->local_offset) += var_type->bytes;
                                    ir_gen_expr(jp, init_expr);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADDRVAR,
                                            .addrvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = offset,
                                                .reg_dest = jp->reg_alloc,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                    assert(jp->reg_alloc == 1);
                                    ir_gen_memorycopy(jp, var_type->bytes, var_type->align, jp->reg_alloc, jp->reg_alloc - 1);
                                    jp->reg_alloc--;
                                    assert(jp->reg_alloc == 0);
                                }
                            } else {
                                u64 array_data_offset = sym->segment_offset;
                                assert(var_type->bytes == var_type->array.element_stride * var_type->array.n);
                                u64 total_bytes = var_type->bytes;
                                arrlast(jp->local_offset) += total_bytes;

                                for(u64 b = 0, step = 8; b < total_bytes; b += step) {
                                    while(b + step > total_bytes) step >>= 1;
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETVAR,
                                            .setvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = array_data_offset + b,
                                                .bytes = step,
                                                .immediate = true,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                }
                            }
                        } else if(var_type->kind == TYPE_KIND_ARRAY_VIEW) {
                            arrlast(jp->local_offset) += var_type->bytes;

                            if(init_expr) {
                                if(init_expr->kind == AST_KIND_array_literal) {
                                    AST_array_literal *array_literal = (AST_array_literal*)(init_expr);
                                    u64 array_data_offset = ir_gen_array_literal(jp, array_literal->type_annotation, array_literal);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADDRVAR,
                                            .addrvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = array_data_offset,
                                                .reg_dest = 0,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);

                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETVAR,
                                            .setvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = sym->segment_offset,
                                                .sym = sym,
                                                .reg_src = 0,
                                                .bytes = 8,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);

                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETVAR,
                                            .setvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = sym->segment_offset + 8,
                                                .sym = sym,
                                                .imm.integer = array_literal->type_annotation->array.n,
                                                .bytes = 8,
                                                .immediate = true,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else {
                                    AST_expr_base *expr_base = (AST_expr_base*)(init_expr);
                                    Type *init_type = expr_base->type_annotation;

                                    if(init_type->kind == TYPE_KIND_ARRAY_VIEW) {
                                        ir_gen_expr(jp, init_expr);
                                        assert(jp->reg_alloc == 1);
                                        jp->reg_alloc = 0;
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_LOAD,
                                                .load = {
                                                    .reg_dest = jp->reg_alloc + 1,
                                                    .reg_src_ptr = jp->reg_alloc,
                                                    .bytes = 8,
                                                }
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_LOAD,
                                                .load = {
                                                    .reg_dest = jp->reg_alloc + 2,
                                                    .reg_src_ptr = jp->reg_alloc,
                                                    .bytes = 8,
                                                    .byte_offset_imm = 8,
                                                    .has_immediate_offset = true,
                                                }
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETVAR,
                                                .setvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = sym->segment_offset,
                                                    .sym = sym,
                                                    .reg_src = jp->reg_alloc + 1,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETVAR,
                                                .setvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = sym->segment_offset + 8,
                                                    .sym = sym,
                                                    .reg_src = jp->reg_alloc + 2,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                    } else if(init_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                                        UNIMPLEMENTED;
                                    } else if(init_type->kind == TYPE_KIND_ARRAY) {
                                        ir_gen_expr(jp, init_expr);
                                        assert(jp->reg_alloc == 1);
                                        jp->reg_alloc = 0;
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETVAR,
                                                .setvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = sym->segment_offset,
                                                    .sym = sym,
                                                    .reg_src = jp->reg_alloc,
                                                    .bytes = 8,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETVAR,
                                                .setvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = sym->segment_offset + 8,
                                                    .sym = sym,
                                                    .imm.integer = init_type->array.n,
                                                    .bytes = 8,
                                                    .immediate = true,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                    } else {
                                        UNREACHABLE;
                                    }
                                }
                            }
                        } else if(TYPE_KIND_IS_RECORD(var_type->kind)) {

                            arrlast(jp->local_offset) += var_type->bytes;

                            if(var_type->kind == TYPE_KIND_UNION) {
                                if(init_expr) {
                                    UNIMPLEMENTED;
                                } else {
                                    u64 total_bytes = var_type->bytes;
                                    u64 offset = sym->segment_offset;
                                    for(u64 b = 0, step = 8; b < total_bytes; b += step) {
                                        while(b + step > total_bytes) step >>= 1;
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_SETVAR,
                                                .setvar = {
                                                    .segment = IRSEG_LOCAL,
                                                    .offset = offset + b,
                                                    .bytes = step,
                                                    .immediate = true,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                    }
                                }
                            } else {
                                if(init_expr) {
                                    ir_gen_expr(jp, init_expr);
                                    assert(jp->reg_alloc == 1);

                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADDRVAR,
                                            .addrvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = sym->segment_offset,
                                                .sym = sym,
                                                .reg_dest = jp->reg_alloc,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);

                                    ir_gen_memorycopy(jp, var_type->bytes, var_type->align, jp->reg_alloc, jp->reg_alloc - 1);

                                    jp->reg_alloc--;

                                    assert(jp->reg_alloc == 0);

                                } else {
                                    ir_gen_struct_init(jp, var_type, IRSEG_LOCAL, sym->segment_offset);
                                }
                            }

                        } else if(var_type->kind == TYPE_KIND_STRING) {
                            arrlast(jp->local_offset) += var_type->bytes;

                            if(init_expr) {
                                ir_gen_expr(jp, init_expr);
                                assert(jp->reg_alloc == 1);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_ADDRVAR,
                                        .addrvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = sym->segment_offset,
                                            .sym = sym,
                                            .reg_dest = jp->reg_alloc,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                ir_gen_memorycopy(jp, var_type->bytes, var_type->align, jp->reg_alloc, jp->reg_alloc - 1);

                                jp->reg_alloc--;

                                assert(jp->reg_alloc == 0);

                            } else {
                                u64 total_bytes = var_type->bytes;
                                u64 offset = sym->segment_offset;
                                for(u64 b = 0, step = 8; b < total_bytes; b += step) {
                                    while(b + step > total_bytes) step >>= 1;
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_SETVAR,
                                            .setvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = offset + b,
                                                .bytes = step,
                                                .immediate = true,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                }
                            }
                        } else if(var_type->kind == TYPE_KIND_PROC) {
                            arrlast(jp->local_offset) += var_type->bytes;

                            if(init_expr) {
                                ir_gen_expr(jp, init_expr);
                                assert(jp->reg_alloc == 1);

                                jp->reg_alloc--;

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = sym->segment_offset,
                                            .sym = sym,
                                            .reg_src = jp->reg_alloc,
                                            .bytes = 8,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                assert(jp->reg_alloc == 0);

                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = sym->segment_offset,
                                            .sym = sym,
                                            .bytes = 8,
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }
                        } else if(var_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                            //TODO this needs to be at the top of the block
                            //cause I keep forgetting it gaaaaaaaaaaaaaaaaaaaaaaaaaaa
                            arrlast(jp->local_offset) += var_type->bytes;

                            if(init_expr) {
                                AST_expr_base *init_expr_base = (AST_expr_base*)init_expr;
                                if(init_expr->kind == AST_KIND_array_literal) {
                                    UNIMPLEMENTED;
                                } else {
                                    //TODO make the array manipulation functions so we can have easy assignment from the other array types
                                    assert(init_expr_base->type_annotation->kind == TYPE_KIND_DYNAMIC_ARRAY);
                                    ir_gen_expr(jp, init_expr);

                                    assert(jp->reg_alloc == 1);

                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADDRVAR,
                                            .addrvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = sym->segment_offset,
                                                .sym = sym,
                                                .reg_dest = jp->reg_alloc,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);

                                    ir_gen_memorycopy(jp, var_type->bytes, var_type->align, jp->reg_alloc, jp->reg_alloc - 1);

                                    jp->reg_alloc--;
                                }
                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = sym->segment_offset + offsetof(Dynamic_array, data),
                                            .sym = sym,
                                            .bytes = 8,
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                                inst.setvar.offset = sym->segment_offset + offsetof(Dynamic_array, count);
                                inst.setvar.bytes = member_size(Dynamic_array, count);
                                arrpush(jp->instructions, inst);
                                inst.setvar.offset = sym->segment_offset + offsetof(Dynamic_array, cap);
                                inst.setvar.bytes = member_size(Dynamic_array, cap);
                                arrpush(jp->instructions, inst);
                                inst.setvar.offset = sym->segment_offset + offsetof(Dynamic_array, allocator);
                                inst.setvar.bytes = member_size(Dynamic_array, allocator);
                                arrpush(jp->instructions, inst);
                                inst.setvar.offset = sym->segment_offset + offsetof(Dynamic_array, allocator_data);
                                inst.setvar.bytes = member_size(Dynamic_array, allocator_data);
                                arrpush(jp->instructions, inst);
                            }
                        } else {
                            UNIMPLEMENTED;
                        }
                    } else {
                        arrlast(jp->local_offset) += var_type->bytes;

                        if(init_expr) {
                            ir_gen_expr(jp, init_expr);

                            IRop opcode;
                            u64 reg_src;

                            if(TYPE_KIND_IS_FLOAT(var_type->kind)) {
                                opcode = IROP_SETVARF;
                                jp->float_reg_alloc--;
                                reg_src = jp->float_reg_alloc;
                                assert(jp->float_reg_alloc == 0);
                            } else {
                                opcode = IROP_SETVAR;
                                jp->reg_alloc--;
                                reg_src = jp->reg_alloc;
                                assert(jp->reg_alloc == 0);
                            }

                            inst =
                                (IRinst) {
                                    .opcode = opcode,
                                    .setvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = sym->segment_offset,
                                        .sym = sym,
                                        .reg_src = reg_src,
                                        .bytes = var_type->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            inst =
                                (IRinst) {
                                    .opcode = (var_type->kind >= TYPE_KIND_FLOAT && var_type->kind <= TYPE_KIND_F64)
                                        ? IROP_SETVARF
                                        : IROP_SETVAR,
                                    .setvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = sym->segment_offset,
                                        .sym = sym,
                                        .bytes = var_type->bytes,
                                        .immediate = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }
                    }

                    jp->reg_alloc = 0;
                    jp->float_reg_alloc = 0;
                }
                break;
            case AST_KIND_statement:
                {
                    AST_statement *ast_statement = (AST_statement*)ast;

                    if(!ast_statement->dont_push_local_offset)
                        arrpush(jp->local_offset, arrlast(jp->local_offset));

                    if(ast_statement->deferred) {
                        ast = ast_statement->next;
                        ast_statement->next = (AST*)defer_list;
                        defer_list = ast_statement;
                    } else {
                        ir_gen_statement(jp, ast_statement);
                        ast = ast_statement->next;
                    }

                    if(arrlast(jp->local_offset) > jp->max_local_offset) jp->max_local_offset = arrlast(jp->local_offset);

                    if(!ast_statement->dont_push_local_offset)
                        arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

                    jp->reg_alloc = 0;
                    jp->float_reg_alloc = 0;
                }
                break;
            case AST_KIND_continuestatement:
            case AST_KIND_breakstatement:
                {
                    if(jp->label_alloc == 0) {
                        job_error(jp, ast->loc, "%s statement is not in control flow block",
                                (ast->kind == AST_KIND_breakstatement) ? "'break'" : "'continue'");
                        return;
                    }

                    if(ast->kind == AST_KIND_continuestatement && arrlen(jp->continue_label) <= 0) {
                        job_error(jp, ast->loc, "cannot perform 'continue' statement within non 'for' of 'while' block");
                        return;
                    }

                    AST_breakstatement *ast_break = (AST_breakstatement*)ast;
                    u64 break_label = (arrlen(jp->break_label) > 0) ? arrlast(jp->break_label) : 0;
                    u64 continue_label = (arrlen(jp->continue_label) > 0) ? arrlast(jp->continue_label) : 0;

                    if(ast->kind == AST_KIND_breakstatement && ast_break->label == NULL && arrlen(jp->break_label) <= 0) {
                        job_error(jp, ast->loc, "cannot perform generic 'break' statement within non 'for' of 'while' block");
                        return;
                    }

                    if(ast_break->label) {
                        IRlabel label = job_label_lookup(jp, ast_break->label);

                        if(label.key == NULL) {
                            job_error(jp, ast->loc, "label '%s' does not exist", ast_break->label);
                            return;
                        }

                        if(label.keyword == TOKEN_IF && ast->kind == AST_KIND_continuestatement) {
                            job_error(jp, ast->loc, "cannot continue on 'if' label");
                            return;
                        }

                        break_label = label.break_label;
                        continue_label = label.continue_label;
                    }

                    if(ast->kind == AST_KIND_continuestatement && (defer_list || arrlen(jp->defer_list_stack) > 0)) {
                        ir_gen_deferred(jp, defer_list);
                        AST_statement *d = arrlast(jp->defer_list_stack);
                        ir_gen_deferred(jp, d);
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_JMP,
                            .branch = {
                                .label_id =
                                    (ast->kind == AST_KIND_breakstatement)
                                    ? break_label
                                    : continue_label,
                            }
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    ast = ast_break->next;
                }
                break;
        }
    }

    if(defer_list) ir_gen_deferred(jp, defer_list);
}

INLINE void ir_gen_deferred(Job *jp, AST_statement *defer_list) {
    while(defer_list) {
        assert(defer_list->base.kind == AST_KIND_statement);
        ir_gen_statement(jp, defer_list);
        defer_list = (AST_statement*)(defer_list->next);
    }
}

void ir_gen_logical_expr(Job *jp, AST *ast) {
    IRinst inst = {0};
    u64 cur_label = jp->label_alloc++;

    if(ast->kind != AST_KIND_expr) {
        assert(ast->kind > AST_KIND_expr);
        ir_gen_expr(jp, ast);
    } else {
        AST_expr *expr = (AST_expr*)ast;

        switch(expr->token) {
            default:
                ir_gen_expr(jp, ast);
                break;
            case TOKEN_AND:
            case TOKEN_OR:
                {
                    assert(expr->left && expr->right);
                    ir_gen_logical_expr(jp, expr->left);

                    jp->reg_alloc--;

                    AST_expr *expr_left = (AST_expr*)(expr->left);
                    Type *type_left = expr_left->type_annotation;

                    if(type_left->kind >= TYPE_KIND_FLOAT && type_left->kind <= TYPE_KIND_F64) {
                        jp->float_reg_alloc--;
                        inst =
                            (IRinst) {
                                .opcode = IROP_FTOB,
                                .typeconv = {
                                    .to_reg = jp->reg_alloc,
                                    .from_reg = jp->float_reg_alloc,
                                    .to_bytes = 1,
                                    .from_bytes = type_left->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    }

                    inst =
                        (IRinst) {
                            .opcode = (expr->token == TOKEN_OR) ? IROP_IF : IROP_IFZ,
                            .branch = {
                                .cond_reg = jp->reg_alloc,
                                .label_id = cur_label,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                    ir_gen_logical_expr(jp, expr->right);

                    jp->reg_alloc--;

                    AST_expr *expr_right = (AST_expr*)(expr->right);
                    Type *type_right = expr_right->type_annotation;

                    if(type_right->kind >= TYPE_KIND_FLOAT && type_right->kind <= TYPE_KIND_F64) {
                        jp->float_reg_alloc--;
                        inst =
                            (IRinst) {
                                .opcode = IROP_FTOB,
                                .typeconv = {
                                    .to_reg = jp->reg_alloc,
                                    .from_reg = jp->float_reg_alloc,
                                    .to_bytes = 1,
                                    .from_bytes = type_right->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    }

                    inst =
                        (IRinst) {
                            .opcode = IROP_LABEL,
                            .label = { .id = cur_label, },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                    jp->reg_alloc++;
                }
                break;
            case '!':
                {
                    assert(expr->right);
                    ir_gen_logical_expr(jp, expr->right);
                    jp->reg_alloc--;
                    AST_expr *expr_right = (AST_expr*)(expr->right);
                    Type *type_right = expr_right->type_annotation;
                    IRop opcode = IROP_EQ;
                    u64 reg = jp->reg_alloc;
                    if(type_right->kind >= TYPE_KIND_FLOAT && type_right->kind <= TYPE_KIND_F64) {
                        opcode = IROP_FEQ;
                        jp->float_reg_alloc--;
                        reg = jp->float_reg_alloc;
                    }
                    inst =
                        (IRinst) {
                            .opcode = opcode,
                            .arith = {
                                .operand_bytes = { 1, type_right->bytes, type_right->bytes },
                                .reg = { jp->reg_alloc, reg },
                                .immediate = true,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                    jp->reg_alloc++;
                }
                break;
        }
    }
}

//TODO struct initialization needs to be improved
void ir_gen_struct_init(Job *jp, Type *struct_type, IRsegment segment, u64 offset) {
    IRinst inst = {0};

    u64 n_use = struct_type->record.use.n;
    u64 n_members = struct_type->record.member.n;

    Type **use_types = struct_type->record.use.types;
    u64 *use_offsets = struct_type->record.use.offsets;

    Type **member_types = struct_type->record.member.types;
    Value **member_values = struct_type->record.member.values;
    u64 *member_offsets = struct_type->record.member.offsets;

    for(u64 i = 0; i < n_use; ++i) {
        ir_gen_struct_init(jp, use_types[i], segment, offset + use_offsets[i]);
    }

    for(u64 i = 0; i < n_members; ++i) {
        if(TYPE_KIND_IS_NOT_SCALAR(member_types[i]->kind)) {
            if(member_types[i]->kind == TYPE_KIND_STRUCT) {
                ir_gen_struct_init(jp, member_types[i], segment, offset + member_offsets[i]);
            } else {
                if(member_values[i]) {
                    if(member_types[i]->kind == TYPE_KIND_PROC) {
                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = segment,
                                    .offset = offset + member_offsets[i],
                                    .imm.integer = member_values[i]->val.procid,
                                    .bytes = 8,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    } else {
                        UNIMPLEMENTED;
                    }
                } else {
                    u64 total_bytes = member_types[i]->bytes;
                    for(u64 b = 0, step = 8; b < total_bytes; b += step) {
                        while(b + step > total_bytes) step >>= 1;
                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = segment,
                                    .offset = offset + member_offsets[i] + b,
                                    .bytes = step,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    }
                }
            }
        } else {
            if(member_values[i]) {
                if(member_values[i]->kind == VALUE_KIND_NIL) continue;

                IRop opcode;
                IRvalue imm;

                if(member_types[i]->kind == TYPE_KIND_F64) {
                    opcode = IROP_SETVARF;
                    imm.floating64 = member_values[i]->val.dfloating;
                } else if(TYPE_KIND_IS_FLOAT32(member_types[i]->kind)) {
                    opcode = IROP_SETVARF;
                    imm.floating32 = member_values[i]->val.floating;
                } else {
                    opcode = IROP_SETVAR;
                    imm.integer = member_values[i]->val.integer;
                }

                assert(jp->float_reg_alloc == 0);
                assert(jp->reg_alloc == 0);

                inst =
                    (IRinst) {
                        .opcode = opcode,
                        .setvar = {
                            .segment = segment,
                            .offset = offset + member_offsets[i],
                            .imm = imm,
                            .bytes = member_types[i]->bytes,
                            .immediate = true,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            } else {
                inst =
                    (IRinst) {
                        .opcode = (member_types[i]->kind >= TYPE_KIND_FLOAT && member_types[i]->kind <= TYPE_KIND_F64)
                            ? IROP_SETVARF
                            : IROP_SETVAR,
                        .setvar = {
                            .segment = segment,
                            .offset = offset + member_offsets[i],
                            .bytes = member_types[i]->bytes,
                            .immediate = true,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            }
        }
    }
}

void ir_gen_expr(Job *jp, AST *ast) {
    Arr(AST*) ir_expr = ir_linearize_expr(NULL, ast);

    Arr(Type*) type_stack = NULL;

    for(u64 pos = 0; pos < arrlen(ir_expr); ++pos) {
        AST *cur_ast = ir_expr[pos];
        ASTkind kind = cur_ast->kind;

        if(kind == AST_KIND_atom) {
            AST_atom *atom = (AST_atom*)cur_ast;
            IRinst inst = {0};

            if(atom->token == TOKEN_IDENT) {
                Sym *sym = atom->symbol_annotation;

                assert(sym->type->kind != TYPE_KIND_VOID);

                arrpush(type_stack, sym->type);

                if(sym->constant) {
                    if(TYPE_KIND_IS_NOT_SCALAR(sym->type->kind)) {
                        if(sym->type->kind == TYPE_KIND_PROC) {
                            assert(sym->ready_to_run);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = jp->reg_alloc++,
                                        .imm.integer = sym->procid,
                                        .bytes = 8,
                                        .immediate = true,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            UNIMPLEMENTED;
                        }
                    } else {
                        IRop opcode = IROP_LOAD;
                        IRvalue imm = { .integer = sym->value->val.integer };
                        u64 *reg_destp = &(jp->reg_alloc);

                        if(sym->type->kind == TYPE_KIND_F64) {
                            opcode = IROP_LOADF;
                            imm.floating64 = sym->value->val.dfloating;
                            reg_destp = &(jp->float_reg_alloc);
                        } else if(sym->type->kind >= TYPE_KIND_FLOAT) {
                            opcode = IROP_LOADF;
                            imm.floating32 = sym->value->val.floating;
                            reg_destp = &(jp->float_reg_alloc);
                        }

                        inst =
                            (IRinst) {
                                .opcode = opcode,
                                .load = {
                                    .reg_dest = (*reg_destp)++,
                                    .bytes = sym->type->bytes,
                                    .imm = imm,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    }
                } else {
                    u64 *reg_destp = &(jp->reg_alloc);

                    if(TYPE_KIND_IS_NOT_SCALAR(sym->type->kind)) {
                        if(sym->is_argument) {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_GETVAR,
                                    .getvar = {
                                        .segment = IRSEG_LOCAL,
                                        .offset = sym->segment_offset,
                                        .sym = sym,
                                        .reg_dest = (*reg_destp)++,
                                        .bytes = 8,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        } else {
                            IRsegment segment = IRSEG_LOCAL;

                            if(sym->is_global) {
                                if(sym->value) {
                                    segment = IRSEG_GLOBAL;
                                } else {
                                    segment = IRSEG_BSS;
                                }
                            }

                            inst =
                                (IRinst) {
                                    .opcode = IROP_ADDRVAR,
                                    .addrvar = {
                                        .segment = segment,
                                        .offset = sym->segment_offset,
                                        .sym = sym,
                                        .reg_dest = (*reg_destp)++,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                        }
                    } else {
                        IRop opcode = IROP_GETVAR;
                        IRsegment segment = IRSEG_LOCAL;

                        if(sym->is_global) {
                            if(sym->value) {
                                segment = IRSEG_GLOBAL;
                            } else {
                                segment = IRSEG_BSS;
                            }
                        }

                        if(TYPE_KIND_IS_FLOAT(sym->type->kind)) {
                            opcode = IROP_GETVARF;
                            reg_destp = &(jp->float_reg_alloc);
                        }

                        inst =
                            (IRinst) {
                                .opcode = opcode,
                                .getvar = {
                                    .segment = segment,
                                    .offset = sym->segment_offset,
                                    .sym = sym,
                                    .reg_dest = (*reg_destp)++,
                                    .bytes = sym->type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    }
                }
            } else if(atom->token == TOKEN_CONTEXT) {
                inst =
                    (IRinst) {
                        .opcode = IROP_GETVAR,
                        .getvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = 0,
                            .reg_dest = jp->reg_alloc++,
                            .bytes = 8,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                arrpush(type_stack, atom->type_annotation);
            } else if(atom->token == '@') {
                atom->token = TOKEN_IDENT;

                Sym *sym = atom->symbol_annotation;
                assert(sym->type->kind != TYPE_KIND_VOID);
                Type *pointer_to_sym_type = job_alloc_type(jp, TYPE_KIND_POINTER);
                pointer_to_sym_type->pointer.to = sym->type;

                arrpush(type_stack, pointer_to_sym_type);

                IRsegment segment = IRSEG_LOCAL;

                if(sym->is_global) {
                    if(sym->value) {
                        segment = IRSEG_GLOBAL;
                    } else {
                        segment = IRSEG_BSS;
                    }
                }

                inst =
                    (IRinst) {
                        .opcode = IROP_ADDRVAR,
                        .addrvar = {
                            .segment = segment,
                            .offset = sym->segment_offset,
                            .sym = sym,
                            .reg_dest = jp->reg_alloc++,
                        },
                    };

                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            } else {
                Type *atom_type = atom->type_annotation;
                arrpush(type_stack, atom_type);

                if(TYPE_KIND_IS_NOT_SCALAR(atom_type->kind)) {
                    if(atom_type->kind == TYPE_KIND_STRING) {
                        Value *v = atom->value_annotation;
                        char *s = arena_alloc(&string_arena, v->val.str.len);

                        for(int i = 0; i < v->val.str.len; ++i)
                            s[i] = v->val.str.data[i];

                        arrlast(jp->local_offset) =
                            align_up(arrlast(jp->local_offset), _Alignof(String_view));

                        inst =
                            (IRinst) {
                                .opcode = IROP_ADDRVAR,
                                .addrvar = {
                                    .segment = IRSEG_STRING,
                                    .offset = (u64)(void*)s,
                                    .reg_dest = jp->reg_alloc,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = arrlast(jp->local_offset) + offsetof(String_view, data),
                                    .reg_src = jp->reg_alloc,
                                    .bytes = 8,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_SETVAR,
                                .setvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = arrlast(jp->local_offset) + offsetof(String_view, len),
                                    .imm.integer = v->val.str.len,
                                    .bytes = 8,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        inst =
                            (IRinst) {
                                .opcode = IROP_ADDRVAR,
                                .addrvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = arrlast(jp->local_offset),
                                    .reg_dest = jp->reg_alloc++,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);

                        arrlast(jp->local_offset) += sizeof(String_view);
                    } else {
                        UNIMPLEMENTED;
                    }
                } else {
                    IRop opcode = IROP_LOAD;
                    IRvalue imm = { .integer = atom->value_annotation->val.integer };
                    u64 *reg_destp = &(jp->reg_alloc);

                    if(atom_type->kind == TYPE_KIND_F64) {
                        opcode = IROP_LOADF;
                        imm.floating64 = atom->value_annotation->val.dfloating;
                        reg_destp = &(jp->float_reg_alloc);
                    } else if(atom_type->kind >= TYPE_KIND_FLOAT) {
                        opcode = IROP_LOADF;
                        imm.floating32 = atom->value_annotation->val.floating;
                        reg_destp = &(jp->float_reg_alloc);
                    }

                    inst =
                        (IRinst) {
                            .opcode = opcode,
                            .load = {
                                .reg_dest = (*reg_destp)++,
                                .bytes = atom_type->bytes,
                                .imm = imm,
                                .immediate = true,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                }
            }

        } else if(kind == AST_KIND_expr) {
            AST_expr *node = (AST_expr*)cur_ast;
            IRinst inst = {0};

            if(node->value_annotation && node->value_annotation->kind != VALUE_KIND_NIL) {
                Type *result_type = node->type_annotation;
                Value *result_value = node->value_annotation;

                assert(result_type->kind != TYPE_KIND_VOID);

                IRop opcode = IROP_LOAD;
                IRvalue imm = { .integer = node->value_annotation->val.integer };
                u64 *reg_destp = &(jp->reg_alloc);

                if(node->token == TOKEN_TYPEINFO && result_value->kind == VALUE_KIND_TYPEINFO) {
                    assert(result_type->kind == TYPE_KIND_POINTER && result_type->pointer.to->kind == TYPE_KIND_STRUCT);
                    inst =
                        (IRinst) {
                            .opcode = IROP_ADDRVAR,
                            .addrvar = {
                                .segment = IRSEG_TYPE,
                                .offset = (u64)(void*)(result_value->val.typeinfo),
                                .reg_dest = jp->reg_alloc++,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    arrpush(type_stack, result_type);
                    continue;
                }

                if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind)) {
                    printf("result_type = %s\n", job_type_to_str(jp, result_type));
                    if(result_type->kind == TYPE_KIND_ARRAY) {
                        u64 array_offset = ir_gen_array_from_value(jp, result_type, result_value);
                        inst =
                            (IRinst) {
                                .opcode = IROP_ADDRVAR,
                                .addrvar = {
                                    .segment = IRSEG_LOCAL,
                                    .offset = array_offset,
                                    .reg_dest = jp->reg_alloc++,
                                },
                            };

                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                        arrpush(type_stack, result_type);
                        continue;
                    } else {
                        UNIMPLEMENTED;
                    }
                } else {
                    if(result_type->kind == TYPE_KIND_F64) {
                        opcode = IROP_LOADF;
                        imm.floating64 = result_value->val.dfloating;
                        reg_destp = &(jp->float_reg_alloc);
                    } else if(result_type->kind >= TYPE_KIND_FLOAT) {
                        opcode = IROP_LOADF;
                        imm.floating32 = result_value->val.floating;
                        reg_destp = &(jp->float_reg_alloc);
                    }

                    inst =
                        (IRinst) {
                            .opcode = opcode,
                            .load = {
                                .reg_dest = (*reg_destp)++,
                                .bytes = result_type->bytes,
                                .imm = imm,
                                .immediate = true,
                            },
                        };

                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                    arrpush(type_stack, result_type);
                    continue;
                }
            }

            if(node->token == TOKEN_AND || node->token == TOKEN_OR || node->token == '!') {
                ir_gen_logical_expr(jp, cur_ast);
                assert(node->type_annotation->kind == TYPE_KIND_BOOL);
                arrpush(type_stack, node->type_annotation);
            } else if(node->token == '@' && !(node->left && node->right)) {
                //TODO maybe merge '@' for dot exprs with '@' for subscript

                arrpush(type_stack, node->type_annotation);

                AST_expr *operand_expr = (AST_expr*)(node->right);

                assert(operand_expr->token == '.');

                ir_gen_expr(jp, operand_expr->left);

                jp->reg_alloc--;

                AST_expr_base *left = (AST_expr_base*)(operand_expr->left);

                Type *left_type = left->type_annotation;

                if(left_type->kind == TYPE_KIND_POINTER) {
                    assert(left_type->pointer.to->kind != TYPE_KIND_ARRAY);
                    assert(TYPE_KIND_IS_RECORD(left_type->pointer.to->kind) ||
                            TYPE_KIND_IS_VIEW_LIKE(left_type->pointer.to->kind) ||
                            TYPE_KIND_IS_ARRAY_LIKE(left_type->pointer.to->kind));
                    left_type = left_type->pointer.to;
                }

                assert(left_type->kind != TYPE_KIND_ARRAY);
                assert(operand_expr->right->kind == AST_KIND_atom);

                char *field = ((AST_atom*)(operand_expr->right))->text;

                u64 offset;

                if(left_type->kind == TYPE_KIND_ARRAY_VIEW) {
                    if(!strcmp(field, "data")) {
                        offset = offsetof(Array_view, data);
                    } else if(!strcmp(field, "count")) {
                        offset = offsetof(Array_view, count);
                    } else {
                        UNREACHABLE;
                    }
                } else if(left_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                    if(!strcmp(field, "data")) {
                        offset = offsetof(Dynamic_array, data);
                    } else if(!strcmp(field, "count")) {
                        offset = offsetof(Dynamic_array, count);
                    } else if(!strcmp(field, "cap")) {
                        offset = offsetof(Dynamic_array, cap);
                    } else if(!strcmp(field, "allocator")) {
                        offset = offsetof(Dynamic_array, allocator);
                    } else if(!strcmp(field, "allocator_data")) {
                        offset = offsetof(Dynamic_array, allocator_data);
                    } else {
                        UNREACHABLE;
                    }
                } else if(TYPE_KIND_IS_RECORD(left_type->kind)) {
                    offset = operand_expr->dot_offset_annotation;

                } else if(left_type->kind == TYPE_KIND_STRING) {
                    if(!strcmp(field, "data")) {
                        offset = offsetof(String_view, data);
                    } else if(!strcmp(field, "len")) {
                        offset = offsetof(String_view, len);
                    } else {
                        UNREACHABLE;
                    }

                } else {
                    UNIMPLEMENTED;
                }

                inst =
                    (IRinst) {
                        .opcode = IROP_ADD,
                        .arith = {
                            .operand_bytes = { 8, 8, 8 },
                            .reg = { jp->reg_alloc, jp->reg_alloc, },
                            .imm.integer = offset,
                            .immediate = true,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                jp->reg_alloc++;

            } else if(node->token == '.') {
                Type *result_type = node->type_annotation;

                AST_expr_base *left = (AST_expr_base*)(node->left);

                Type *operand_type;
                if(left->type_annotation->kind == TYPE_KIND_ARRAY) {
                    operand_type = left->type_annotation;
                } else {
                    operand_type = arrpop(type_stack);
                    if(operand_type->kind == TYPE_KIND_POINTER) {
                        assert(operand_type->pointer.to->kind != TYPE_KIND_ARRAY);
                        assert(TYPE_KIND_IS_RECORD(operand_type->pointer.to->kind) ||
                                TYPE_KIND_IS_VIEW_LIKE(operand_type->pointer.to->kind) ||
                                TYPE_KIND_IS_ARRAY_LIKE(operand_type->pointer.to->kind));
                        operand_type = operand_type->pointer.to;
                    }
                    jp->reg_alloc--;
                }

                arrpush(type_stack, result_type);
                char *field = ((AST_atom*)(node->right))->text;

                if(operand_type->kind == TYPE_KIND_ARRAY) {
                    if(!strcmp(field, "count")) {
                        assert(operand_type->array.n > 0);

                        arrsetlen(jp->instructions, arrlen(jp->instructions) - 1); // cancel previous instruction

                        inst =
                            (IRinst) {
                                .opcode = IROP_LOAD,
                                .load = {
                                    .reg_dest = jp->reg_alloc++,
                                    .imm.integer = operand_type->array.n,
                                    .bytes = 8,
                                    .immediate = true,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                    } else  {
                        assert(!strcmp(field, "data"));
                    }
                } else if(operand_type->kind == TYPE_KIND_ARRAY_VIEW) {
                    assert(!strcmp(field, "data") || !strcmp(field, "count"));

                    inst =
                        (IRinst) {
                            .opcode = IROP_LOAD,
                            .load = {
                                .reg_dest = jp->reg_alloc,
                                .reg_src_ptr = jp->reg_alloc,
                                .bytes = 8,
                            },
                        };

                    if(!strcmp(field, "count")) {
                        inst.load.has_immediate_offset = true;
                        inst.load.byte_offset_imm = 8;
                    }
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    jp->reg_alloc++;

                } else if(operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                    inst =
                        (IRinst) {
                            .opcode = IROP_LOAD,
                            .load = {
                                .reg_dest = jp->reg_alloc,
                                .reg_src_ptr = jp->reg_alloc,
                            },
                        };
                    if(!strcmp(field, "data")) {
                        inst.load.bytes = member_size(Dynamic_array, data);
                    } else {
                        inst.load.has_immediate_offset = true;
                        if(!strcmp(field, "count")) {
                            inst.load.byte_offset_imm = offsetof(Dynamic_array, count);
                            inst.load.bytes = member_size(Dynamic_array, count);
                        } else if(!strcmp(field, "cap")) {
                            inst.load.byte_offset_imm = offsetof(Dynamic_array, cap);
                            inst.load.bytes = member_size(Dynamic_array, cap);
                        } else if(!strcmp(field, "allocator")) {
                            inst.load.byte_offset_imm = offsetof(Dynamic_array, allocator);
                            inst.load.bytes = member_size(Dynamic_array, allocator);
                        } else if(!strcmp(field, "allocator_data")) {
                            inst.load.byte_offset_imm = offsetof(Dynamic_array, allocator_data);
                            inst.load.bytes = member_size(Dynamic_array, allocator_data);
                        } else {
                            UNREACHABLE;
                        }
                    }

                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    jp->reg_alloc++;

                } else if(TYPE_KIND_IS_RECORD(operand_type->kind)) {

                    u64 field_offset = node->dot_offset_annotation;

                    if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind) && result_type->kind != TYPE_KIND_PROC) {
                        inst = (IRinst) {
                            .opcode = IROP_ADD,
                                .arith = {
                                    .operand_bytes = { 8, 8, 8 },
                                    .reg = { jp->reg_alloc, jp->reg_alloc, 0 },
                                    .imm.integer = field_offset,
                                    .immediate = true,
                                },
                        };
                        jp->reg_alloc++;
                    } else if(TYPE_KIND_IS_FLOAT(result_type->kind)) {
                        inst = (IRinst) {
                            .opcode = IROP_LOADF,
                                .load = {
                                    .reg_dest = jp->float_reg_alloc,
                                    .reg_src_ptr = jp->reg_alloc,
                                    .byte_offset_imm = field_offset,
                                    .bytes = result_type->bytes,
                                    .has_immediate_offset = true,
                                },
                        };
                        jp->float_reg_alloc++;
                    } else {
                        inst = (IRinst) {
                            .opcode = IROP_LOAD,
                                .load = {
                                    .reg_dest = jp->reg_alloc,
                                    .reg_src_ptr = jp->reg_alloc,
                                    .byte_offset_imm = field_offset,
                                    .bytes = result_type->bytes,
                                    .has_immediate_offset = true,
                                },
                        };
                        jp->reg_alloc++;
                    }
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                } else if(operand_type->kind == TYPE_KIND_STRING) {
                    assert(!strcmp(field, "data") || !strcmp(field, "len"));

                    inst =
                        (IRinst) {
                            .opcode = IROP_LOAD,
                            .load = {
                                .reg_dest = jp->reg_alloc,
                                .reg_src_ptr = jp->reg_alloc,
                                .bytes = 8,
                            },
                        };

                    if(!strcmp(field, "len")) {
                        inst.load.has_immediate_offset = true;
                        inst.load.byte_offset_imm = 8;
                    }
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);

                    jp->reg_alloc++;

                } else {
                    UNIMPLEMENTED;
                }

            } else if(!(node->left && node->right)) {
                Type *result_type = node->type_annotation;
                Type *operand_type = arrpop(type_stack);
                arrpush(type_stack, result_type);

                u64 *reg_destp = &(jp->reg_alloc);
                u64 *reg_srcp = &(jp->reg_alloc);

                if(result_type->kind >= TYPE_KIND_FLOAT && result_type->kind <= TYPE_KIND_F64)
                    reg_destp = &(jp->float_reg_alloc);
                if(operand_type->kind >= TYPE_KIND_FLOAT && operand_type->kind <= TYPE_KIND_F64)
                    reg_srcp = &(jp->float_reg_alloc);

                (*reg_srcp)--;

                switch(node->token) {
                    default:
                        UNREACHABLE;
                    case TOKEN_CAST:
                        //TODO implicit cast everything
                        {
                            bool result_is_record_and_operand_is_record =
                                (TYPE_KIND_IS_RECORD(result_type->kind) && TYPE_KIND_IS_RECORD(operand_type->kind));

                            bool result_is_pointer_and_operand_is_pointer =
                                (result_type->kind == TYPE_KIND_POINTER && operand_type->kind == TYPE_KIND_POINTER);

                            bool result_is_view_and_operand_is_view =
                                (result_type->kind == TYPE_KIND_ARRAY_VIEW && operand_type->kind == TYPE_KIND_ARRAY_VIEW);

                            bool result_is_view_and_operand_is_array =
                                (result_type->kind == TYPE_KIND_ARRAY_VIEW && operand_type->kind == TYPE_KIND_ARRAY);

                            bool result_is_view_and_operand_is_dynamic_array =
                                (result_type->kind == TYPE_KIND_ARRAY_VIEW && operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY);

                            //bool result_is_string_and_operand_is_view =
                            //    (result_type->kind == TYPE_KIND_STRING && operand_type->kind == TYPE_KIND_ARRAY_VIEW);

                            //bool result_is_string_and_operand_is_dynamic_array =
                            //    (result_type->kind == TYPE_KIND_STRING && operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY);

                            bool result_is_view_and_operand_is_string =
                                (result_type->kind == TYPE_KIND_ARRAY_VIEW && operand_type->kind == TYPE_KIND_STRING);

                            bool result_is_array_and_operand_is_array =
                                (result_type->kind == TYPE_KIND_ARRAY && operand_type->kind == TYPE_KIND_ARRAY);

                            bool result_is_non_scalar_and_operand_is_non_scalar =
                                (TYPE_KIND_IS_NOT_SCALAR(result_type->kind) && TYPE_KIND_IS_NOT_SCALAR(operand_type->kind));

                            bool result_is_pointer_and_operand_is_array_like_or_string =
                                (result_type->kind == TYPE_KIND_POINTER && (TYPE_KIND_IS_ARRAY_LIKE(operand_type->kind) || operand_type->kind == TYPE_KIND_STRING));

                            bool result_is_any = (result_type == type_Any);

                            bool result_is_scalar_or_proc_and_operand_is_scalar_or_proc =
                                (
                                 (TYPE_KIND_IS_SCALAR(result_type->kind) || result_type->kind == TYPE_KIND_PROC) &&
                                 (TYPE_KIND_IS_SCALAR(operand_type->kind) || operand_type->kind == TYPE_KIND_PROC)
                                );


                            if(result_is_record_and_operand_is_record) {
                                u64 use_offset = 0;

                                for(u64 i = 0; i < operand_type->record.use.n; ++i) {
                                    if(result_type == operand_type->record.use.types[i]) {
                                        use_offset = operand_type->record.use.offsets[i];
                                        break;
                                    }
                                }

                                if(use_offset > 0) {
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADD,
                                            .arith = {
                                                .operand_bytes = { 8, 8, 8 },
                                                .reg = { *reg_destp, *reg_srcp, 0 },
                                                .imm.integer = use_offset,
                                                .immediate = true,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                }

                            } else if(result_is_pointer_and_operand_is_pointer) {

                                if(TYPE_KIND_IS_RECORD(result_type->pointer.to->kind) && TYPE_KIND_IS_RECORD(operand_type->pointer.to->kind)) {
                                    Type *p_to_operand_type = operand_type->pointer.to;
                                    Type *p_to_result_type = result_type->pointer.to;

                                    u64 use_offset = 0;

                                    for(u64 i = 0; i < p_to_operand_type->record.use.n; ++i) {
                                        if(p_to_result_type == p_to_operand_type->record.use.types[i]) {
                                            use_offset = p_to_operand_type->record.use.offsets[i];
                                            break;
                                        }
                                    }

                                    if(use_offset > 0) {
                                        inst =
                                            (IRinst) {
                                                .opcode = IROP_ADD,
                                                .arith = {
                                                    .operand_bytes = { 8, 8, 8 },
                                                    .reg = { *reg_destp, *reg_srcp, 0 },
                                                    .imm.integer = use_offset,
                                                    .immediate = true,
                                                },
                                            };
                                        inst.loc = jp->cur_loc;
                                        arrpush(jp->instructions, inst);
                                    }
                                } else if(TYPE_KIND_IS_NOT_SCALAR(result_type->pointer.to->kind) && TYPE_KIND_IS_NOT_SCALAR(operand_type->pointer.to->kind)) {
                                    Type *p_to_operand_type = operand_type->pointer.to;
                                    Type *p_to_result_type = result_type->pointer.to;

                                    if(p_to_result_type == type_String_view                   && p_to_operand_type->kind == TYPE_KIND_STRING)        PASS;
                                    else if(p_to_result_type == type_Array_view               && p_to_operand_type->kind == TYPE_KIND_ARRAY_VIEW)    PASS;
                                    else if(p_to_result_type == type_Dynamic_array            && p_to_operand_type->kind == TYPE_KIND_DYNAMIC_ARRAY) PASS;
                                    else if(p_to_result_type->kind == TYPE_KIND_STRING        && p_to_operand_type       == type_String_view)        PASS;
                                    else if(p_to_result_type->kind == TYPE_KIND_ARRAY_VIEW    && p_to_operand_type       == type_Array_view)         PASS;
                                    else if(p_to_result_type->kind == TYPE_KIND_DYNAMIC_ARRAY && p_to_operand_type       == type_Dynamic_array)      PASS;
                                    else UNIMPLEMENTED;
                                }

                            } else if(result_is_view_and_operand_is_view) {
                                PASS;
                            } else if(result_is_view_and_operand_is_array) {
                                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), operand_type->align);
                                u64 operand_offset = arrlast(jp->local_offset);

                                arrlast(jp->local_offset) += sizeof(Array_view);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = operand_offset + offsetof(Array_view, data),
                                            .reg_src = *reg_srcp,
                                            .bytes = member_size(Array_view, data),
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = operand_offset + offsetof(Array_view, count),
                                            .imm.integer = operand_type->array.n,
                                            .bytes = member_size(Array_view, count),
                                            .immediate = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_ADDRVAR,
                                        .addrvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = operand_offset,
                                            .reg_dest = *reg_destp,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                            } else if(result_is_view_and_operand_is_dynamic_array) {
                                UNIMPLEMENTED;
                            } else if(result_is_view_and_operand_is_string) {
                                UNIMPLEMENTED;
                            } else if(result_is_array_and_operand_is_array) {
                                UNIMPLEMENTED;
                            } else if(result_is_non_scalar_and_operand_is_non_scalar) {
                                PASS;
                            } else if(result_is_pointer_and_operand_is_array_like_or_string) {
                                if(operand_type->kind != TYPE_KIND_ARRAY) {
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_LOAD,
                                            .load = {
                                                .reg_dest = *reg_destp,
                                                .reg_src_ptr = *reg_srcp,
                                                .bytes = 8,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                }
                            } else if(result_is_any) {
                                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), operand_type->align);
                                u64 operand_offset = arrlast(jp->local_offset);

                                if(TYPE_KIND_IS_NOT_SCALAR(operand_type->kind)) {
                                    UNIMPLEMENTED;
                                    arrlast(jp->local_offset) += 8;
                                } else {
                                    inst =
                                        (IRinst) {
                                            .opcode = TYPE_KIND_IS_FLOAT(operand_type->kind) ? IROP_SETVARF : IROP_SETVAR,
                                            .setvar = {
                                                .segment = IRSEG_LOCAL,
                                                .offset = operand_offset,
                                                .reg_src = *reg_srcp,
                                                .bytes = operand_type->bytes,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                    arrlast(jp->local_offset) += operand_type->bytes;
                                }

                                assert(result_type == type_Any);

                                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), _Alignof(Any));

                                jp->reg_alloc++;

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_ADDRVAR,
                                        .addrvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = operand_offset,
                                            .reg_dest = jp->reg_alloc,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = arrlast(jp->local_offset) + offsetof(Any, data),
                                            .reg_src = jp->reg_alloc,
                                            .bytes = member_size(Any, data),
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_ADDRVAR,
                                        .addrvar = {
                                            .segment = IRSEG_TYPE,
                                            .offset = (u64)(void*)job_make_type_info(jp, operand_type),
                                            .reg_dest = jp->reg_alloc,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_SETVAR,
                                        .setvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = arrlast(jp->local_offset) + offsetof(Any, type),
                                            .reg_src = jp->reg_alloc,
                                            .bytes = member_size(Any, type),
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                jp->reg_alloc--;

                                inst =
                                    (IRinst) {
                                        .opcode = IROP_ADDRVAR,
                                        .addrvar = {
                                            .segment = IRSEG_LOCAL,
                                            .offset = arrlast(jp->local_offset),
                                            .reg_dest = *reg_destp,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);

                                arrlast(jp->local_offset) += sizeof(Any);

                            } else if(result_is_scalar_or_proc_and_operand_is_scalar_or_proc) {
                                inst =
                                    (IRinst) {
                                        .typeconv = {
                                            .to_reg = *reg_destp,
                                            .from_reg = *reg_srcp,
                                            .to_bytes = result_type->bytes,
                                            .from_bytes = operand_type->bytes,
                                        },
                                    };
                                if(TYPE_KIND_IS_FLOAT(result_type->kind) && TYPE_KIND_IS_INTEGER(operand_type->kind)) {
                                    inst.opcode = IROP_ITOF;
                                    inst.loc = jp->cur_loc;
                                    inst.typeconv.sign = TYPE_KIND_IS_SIGNED_INT(operand_type->kind);
                                    arrpush(jp->instructions, inst);
                                } else if(TYPE_KIND_IS_FLOAT(result_type->kind) && TYPE_KIND_IS_FLOAT(operand_type->kind)) {
                                    inst.opcode = IROP_FTOF;
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else if(TYPE_KIND_IS_FLOAT(operand_type->kind) && TYPE_KIND_IS_INTEGER(result_type->kind)) {
                                    inst.opcode = IROP_FTOI;
                                    inst.loc = jp->cur_loc;
                                    inst.typeconv.sign = TYPE_KIND_IS_SIGNED_INT(result_type->kind); // this is ignored by the interpreter
                                    arrpush(jp->instructions, inst);
                                } else if(TYPE_KIND_IS_INTEGER(operand_type->kind) && TYPE_KIND_IS_INTEGER(result_type->kind)) {
                                    inst.opcode = IROP_ITOI;
                                    inst.loc = jp->cur_loc;
                                    inst.typeconv.sign = TYPE_KIND_IS_SIGNED_INT(result_type->kind);
                                    arrpush(jp->instructions, inst);
                                }
                            } else {
                                UNIMPLEMENTED;
                            }

                            (*reg_destp)++;
                            break;
                        }
                    case '+':
                        printf("schmuck\n");
                        (*reg_destp)++;
                        break;
                    case '-':
                        inst =
                            (IRinst) {
                                .opcode = (result_type->kind >= TYPE_KIND_FLOAT && result_type->kind <= TYPE_KIND_F64)
                                    ? IROP_FNEG
                                    : IROP_NEG,
                                .arith = {
                                    .operand_bytes = { result_type->bytes, operand_type->bytes },
                                    .reg = { *reg_destp, *reg_srcp },
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                        (*reg_destp)++;
                        break;
                    case '~':
                        inst =
                            (IRinst) {
                                .opcode = IROP_NOT,
                                .arith = {
                                    .operand_bytes = { result_type->bytes, operand_type->bytes },
                                    .reg = { jp->reg_alloc, jp->reg_alloc },
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                        jp->reg_alloc++;
                        break;
                    case '>':
                        //TODO remove float_reg_alloc
                        if(TYPE_KIND_IS_NOT_SCALAR(node->type_annotation->kind)) {
                            //TODO this wastes a register
                            jp->reg_alloc++;
                        } else if(TYPE_KIND_IS_FLOAT(node->type_annotation->kind)) {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOADF,
                                    .load = {
                                        .reg_dest = jp->float_reg_alloc,
                                        .reg_src_ptr = jp->reg_alloc,
                                        .bytes = result_type->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            jp->float_reg_alloc++;
                        } else {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = jp->reg_alloc,
                                        .reg_src_ptr = jp->reg_alloc,
                                        .bytes = result_type->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            jp->reg_alloc++;
                        }
                        break;
                }
            } else {
                Type *result_type = node->type_annotation;
                Type *a_type;
                Type *b_type;
                u64 a_reg;
                u64 b_reg;
                u64 tmp;

                u64 *result_regp = &(jp->reg_alloc);

                if(TYPE_KIND_IS_FLOAT(result_type->kind))
                    result_regp = &(jp->float_reg_alloc);


                a_type = arrpop(type_stack);
                b_type = arrpop(type_stack);

                bool operands_are_float = false;

                if(TYPE_KIND_IS_FLOAT(a_type->kind)) {
                    assert(TYPE_KIND_IS_FLOAT(b_type->kind));
                    a_reg = --(jp->float_reg_alloc);
                    b_reg = --(jp->float_reg_alloc);
                    operands_are_float = true;
                } else {
                    a_reg = --(jp->reg_alloc);
                    b_reg = --(jp->reg_alloc);
                }

                if(node->left->weight >= node->right->weight) {
                    Type *tmp_t = a_type;
                    a_type = b_type;
                    b_type = tmp_t;
                    tmp = a_reg;
                    a_reg = b_reg;
                    b_reg = tmp;
                }

                u64 result_bytes = result_type->bytes;
                u64 a_bytes = a_type->bytes;
                u64 b_bytes = b_type->bytes;

                arrpush(type_stack, result_type);

                bool is_arith = false;

                if(!TOKEN_IS_BITWISE_OP(node->token) && TYPE_KIND_IS_SIGNED_INT(a_type->kind)) {
                    if(TYPE_KIND_IS_SIGNED_INT(b_type->kind))
                        inst.arith.sign = true;
                } else if(!TOKEN_IS_BITWISE_OP(node->token) && TYPE_KIND_IS_SIGNED_INT(b_type->kind)) {
                    if(TYPE_KIND_IS_SIGNED_INT(a_type->kind))
                        inst.arith.sign = true;
                }

                switch(node->token) {
                    default:
                        UNREACHABLE;
                    case '@':
                        inst =
                            (IRinst) {
                                .opcode = IROP_CALCPTROFFSET,
                                .calcptroffset = {
                                    .reg_dest = *result_regp,
                                    .reg_src_ptr = a_reg,
                                    .offset_reg = b_reg,
                                    .stride = result_type->bytes,
                                },
                            };
                        inst.loc = jp->cur_loc;
                        arrpush(jp->instructions, inst);
                        (*result_regp)++;
                        break;
                    case '[':
                        assert(b_type->kind <= TYPE_KIND_INT);

                        //TODO array bounds checking
                        if(a_type->kind == TYPE_KIND_ARRAY) {
                            if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind)) {
                                if(result_type->kind == TYPE_KIND_ARRAY_VIEW) {
                                    UNIMPLEMENTED;
                                } else if(result_type->kind == TYPE_KIND_ARRAY) {
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_MUL,
                                            .arith = {
                                                .operand_bytes = { 8, b_bytes, 8 },
                                                .reg = { b_reg, b_reg },
                                                .imm.integer = a_type->array.element_stride,
                                                .immediate = true,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_ADD,
                                            .arith = {
                                                .operand_bytes = { 8, 8, 8 },
                                                .reg = { *result_regp, a_reg, b_reg },
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else if(TYPE_KIND_IS_RECORD(result_type->kind)) {
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_CALCPTROFFSET,
                                            .calcptroffset = {
                                                .reg_dest = *result_regp,
                                                .reg_src_ptr = a_reg,
                                                .offset_reg = b_reg,
                                                .stride = result_type->bytes,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else {
                                    UNIMPLEMENTED;
                                }

                            } else if(TYPE_KIND_IS_FLOAT(result_type->kind)) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOADF,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }

                        } else if(a_type->kind == TYPE_KIND_ARRAY_VIEW || a_type->kind == TYPE_KIND_STRING || a_type->kind == TYPE_KIND_DYNAMIC_ARRAY) {
                            inst =
                                (IRinst) {
                                    .opcode = IROP_LOAD,
                                    .load = {
                                        .reg_dest = a_reg,
                                        .reg_src_ptr = a_reg,
                                        .bytes = 8,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);

                            if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind)) {

                                if(TYPE_KIND_IS_RECORD(result_type->kind)) {
                                    inst =
                                        (IRinst) {
                                            .opcode = IROP_CALCPTROFFSET,
                                            .calcptroffset = {
                                                .reg_dest = *result_regp,
                                                .reg_src_ptr = a_reg,
                                                .offset_reg = b_reg,
                                                .stride = result_type->bytes,
                                            },
                                        };
                                    inst.loc = jp->cur_loc;
                                    arrpush(jp->instructions, inst);
                                } else {
                                    UNIMPLEMENTED;
                                }

                            } else if(TYPE_KIND_IS_FLOAT(result_type->kind)) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOADF,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }
                        } else {
                            assert(a_type->kind == TYPE_KIND_POINTER);

                            if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind)) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_CALCPTROFFSET,
                                        .calcptroffset = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .stride = result_type->bytes,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else if(TYPE_KIND_IS_FLOAT(result_type->kind)) {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOADF,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            } else {
                                inst =
                                    (IRinst) {
                                        .opcode = IROP_LOAD,
                                        .load = {
                                            .reg_dest = *result_regp,
                                            .reg_src_ptr = a_reg,
                                            .offset_reg = b_reg,
                                            .bytes = result_type->bytes,
                                            .has_indirect_offset = true,
                                        },
                                    };
                                inst.loc = jp->cur_loc;
                                arrpush(jp->instructions, inst);
                            }
                        }

                        (*result_regp)++;

                        break;
                    case '+':
                        if(a_type->kind == TYPE_KIND_POINTER && b_type->kind != TYPE_KIND_POINTER) {
                            assert(result_type->kind == TYPE_KIND_POINTER);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_CALCPTROFFSET,
                                    .calcptroffset = {
                                        .reg_dest = *result_regp,
                                        .reg_src_ptr = a_reg,
                                        .offset_reg = b_reg,
                                        .stride = result_type->pointer.to->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            (*result_regp)++;
                        } else if(a_type->kind != TYPE_KIND_POINTER && b_type->kind == TYPE_KIND_POINTER) {
                            assert(result_type->kind == TYPE_KIND_POINTER);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_CALCPTROFFSET,
                                    .calcptroffset = {
                                        .reg_dest = *result_regp,
                                        .reg_src_ptr = b_reg,
                                        .offset_reg = a_reg,
                                        .stride = result_type->pointer.to->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            (*result_regp)++;
                        } else {
                            inst.opcode = operands_are_float ? IROP_FADD : IROP_ADD;
                            is_arith = true;
                        }
                        break;
                    case '-':
                        if(a_type->kind == TYPE_KIND_POINTER && b_type->kind != TYPE_KIND_POINTER) {
                            assert(result_type->kind == TYPE_KIND_POINTER);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_NEG,
                                    .arith = {
                                        .operand_bytes = { b_bytes, b_bytes },
                                        .reg = { b_reg, b_reg },
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_CALCPTROFFSET,
                                    .calcptroffset = {
                                        .reg_dest = *result_regp,
                                        .reg_src_ptr = a_reg,
                                        .offset_reg = b_reg,
                                        .stride = result_type->pointer.to->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            (*result_regp)++;
                        } else if(a_type->kind != TYPE_KIND_POINTER && b_type->kind == TYPE_KIND_POINTER) {
                            assert(result_type->kind == TYPE_KIND_POINTER);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_NEG,
                                    .arith = {
                                        .operand_bytes = { a_bytes, a_bytes },
                                        .reg = { a_reg, a_reg },
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            inst =
                                (IRinst) {
                                    .opcode = IROP_CALCPTROFFSET,
                                    .calcptroffset = {
                                        .reg_dest = *result_regp,
                                        .reg_src_ptr = b_reg,
                                        .offset_reg = a_reg,
                                        .stride = result_type->pointer.to->bytes,
                                    },
                                };
                            inst.loc = jp->cur_loc;
                            arrpush(jp->instructions, inst);
                            (*result_regp)++;
                        } else {
                            inst.opcode = operands_are_float ? IROP_FSUB : IROP_SUB;
                            is_arith = true;
                        }
                        break;
                    case '*':
                        inst.opcode = operands_are_float ? IROP_FMUL : IROP_MUL;
                        is_arith = true;
                        break;
                    case '/':
                        inst.opcode = operands_are_float ? IROP_FDIV : IROP_DIV;
                        is_arith = true;
                        break;
                    case '%':
                        inst.opcode = IROP_MOD;
                        is_arith = true;
                        break;
                    case '&':
                        inst.opcode = IROP_AND;
                        is_arith = true;
                        break;
                    case '|':
                        inst.opcode = IROP_OR;
                        is_arith = true;
                        break;
                    case '^':
                        inst.opcode = IROP_XOR;
                        is_arith = true;
                        break;
                    case TOKEN_LSHIFT:
                        inst.opcode = IROP_LSHIFT;
                        is_arith = true;
                        break;
                    case TOKEN_RSHIFT:
                        inst.opcode = IROP_RSHIFT;
                        is_arith = true;
                        break;
                    case '<':
                        tmp = a_bytes;
                        a_bytes = b_bytes;
                        b_bytes = tmp;
                        tmp = a_reg;
                        a_reg = b_reg;
                        b_reg = tmp;
                    case '>':
                        inst.opcode = operands_are_float ? IROP_FGT : IROP_GT;
                        is_arith = true;
                        break;
                    case TOKEN_GREATEQUAL:
                        tmp = a_bytes;
                        a_bytes = b_bytes;
                        b_bytes = tmp;
                        tmp = a_reg;
                        a_reg = b_reg;
                        b_reg = tmp;
                    case TOKEN_LESSEQUAL:
                        //if(TYPE_KIND_IS_SIGNED_INT(a_type->kind)) {
                        //    assert(TYPE_KIND_IS_SIGNED_INT(b_type->kind));
                        //    inst.arith.sign = true;
                        //}
                        inst.opcode = operands_are_float ? IROP_FLE : IROP_LE;
                        is_arith = true;
                        break;
                    case TOKEN_EQUALEQUAL:
                        inst.opcode = operands_are_float ? IROP_FEQ : IROP_EQ;
                        is_arith = true;
                        break;
                    case TOKEN_EXCLAMEQUAL:
                        inst.opcode = operands_are_float ? IROP_FNE : IROP_NE;
                        is_arith = true;
                        break;
                }

                if(is_arith) {
                    inst =
                        (IRinst) {
                            .opcode = inst.opcode,
                            .arith = {
                                .operand_bytes = { result_bytes, a_bytes, b_bytes },
                                .reg = { *result_regp, a_reg, b_reg },
                                .sign = inst.arith.sign,
                            },
                        };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                    (*result_regp)++;
                }
            }
        } else if(kind == AST_KIND_array_literal) {
            AST_array_literal *array_literal = (AST_array_literal*)cur_ast;
            u64 array_offset = ir_gen_array_literal(jp, array_literal->type_annotation, array_literal);

            IRinst inst = {
                .opcode = IROP_ADDRVAR,
                .addrvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = array_offset,
                    .reg_dest = jp->reg_alloc++,
                },
            };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            arrpush(type_stack, array_literal->type_annotation);

        } else if(kind == AST_KIND_call) {
            AST_call *ast_call = (AST_call*)cur_ast;
            Arr(Type*) (*ir_gen_call)(Job*, Arr(Type*), AST_call*);

            if(target_platform == PLATFORM_X64) {
                ir_gen_call = ir_gen_call_x64;
            } else if(target_platform == PLATFORM_ARM64) {
                UNIMPLEMENTED;
            } else {
                UNREACHABLE;
            }

            type_stack = ir_gen_call(jp, type_stack, ast_call);

        } else {
            UNIMPLEMENTED;
        }
    }

    //NOTE expressions may need to allocate local data for structs
    //if(arrlast(jp->local_offset) > jp->max_local_offset) jp->max_local_offset = arrlast(jp->local_offset);
    //arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

    arrfree(ir_expr);
}

Arr(Type*) ir_gen_call_x64(Job *jp, Arr(Type*) type_stack, AST_call *ast_call) {
    Pool_save ast_save[AST_KIND_MAX] = {0};
    job_ast_allocator_to_save(jp, ast_save);

    IRinst inst = {0};

    // NOTE
    // If the argument is a struct, or something that isn't register sized,
    // we allocate local space and put the struct there, then push the address
    // on to the stack. Need to think of how we do this for calling C functions
    //
    // According to the amd64 ABI 8byte arguments are passed in registers until these
    // registers are exhausted, any remaining 8byte arguments are pushed on the stack.
    // only then are non 8byte or unaligned arguments (structs and arrays) copied on
    // to the stack. Arguments are placed on the stack in reverse order because it makes
    // varargs functions easier to implement.
    //
    // Returning a struct in the amd64 ABI involves allocating local space and passing a pointer
    // to this memory in the first argument register %rdi. Upon returning the function places
    // this same address in %rax
    //

    if(ast_call->n_types_returned > 1) {
        UNIMPLEMENTED;
    }

    if(ast_call->value_annotation) {
        Type *result_type = ast_call->type_annotation;
        Value *result_value = ast_call->value_annotation;

        assert(result_type->kind != TYPE_KIND_VOID);

        IRop opcode = IROP_LOAD;
        IRvalue imm = { .integer = result_value->val.integer };
        u64 *reg_destp = &(jp->reg_alloc);

        if(TYPE_KIND_IS_NOT_SCALAR(result_type->kind)) {
            printf("result_type = %s\n", job_type_to_str(jp, result_type));
            if(result_type->kind == TYPE_KIND_ARRAY) {
                u64 array_offset = ir_gen_array_from_value(jp, result_type, result_value);
                inst =
                    (IRinst) {
                        .opcode = IROP_ADDRVAR,
                        .addrvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = array_offset,
                            .reg_dest = jp->reg_alloc++,
                        },
                    };

                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
                arrpush(type_stack, result_type);
                return type_stack;
            } else {
                UNIMPLEMENTED;
            }
        } else {
            if(result_type->kind == TYPE_KIND_F64) {
                opcode = IROP_LOADF;
                imm.floating64 = result_value->val.dfloating;
                reg_destp = &(jp->float_reg_alloc);
            } else if(result_type->kind >= TYPE_KIND_FLOAT) {
                opcode = IROP_LOADF;
                imm.floating32 = result_value->val.floating;
                reg_destp = &(jp->float_reg_alloc);
            }

            inst =
                (IRinst) {
                    .opcode = opcode,
                    .load = {
                        .reg_dest = (*reg_destp)++,
                        .bytes = result_type->bytes,
                        .imm = imm,
                        .immediate = true,
                    },
                };

            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);
            arrpush(type_stack, result_type);
            return type_stack;
        }
    }


    assert(ast_call->callee->kind == AST_KIND_atom);

    AST_atom *callee = (AST_atom*)(ast_call->callee);
    Sym *callee_symbol = callee->symbol_annotation;
    /*
       printf("\n");
       print_sym(*callee_symbol);
       printf("\n");
       */
    assert(callee_symbol);
    //Type *callee_type = callee_symbol->type;
    Type *callee_type = callee->type_annotation;
    assert(callee_type->kind == TYPE_KIND_PROC);

    bool varargs = callee_type->proc.varargs;
    bool c_call = callee_type->proc.c_call;
    bool is_foreign = callee_type->proc.is_foreign;

    assert(is_foreign == c_call);

    Arr(AST_param*) params = NULL;
    Arr(u64) saved_param_offsets = NULL;

    arrsetlen(params, MAX(ast_call->n_params, callee_type->proc.param.n));
    //arrsetlen(saved_param_offsets, ast_call->n_params);

    bool params_passed[arrlen(params)];

    for(int i = 0; i < STATICARRLEN(params_passed); ++i) params_passed[i] = false;

    u64 non_scalar_return_addrs[callee_type->proc.ret.n];
    int non_scalar_return_count = 0;

    Type *return_type = NULL;

    if(callee_type->proc.is_polymorphic) {
        if(ast_call->n_types_returned > 1) {
            UNIMPLEMENTED;

        } else if(ast_call->n_types_returned == 1) {
            return_type = ast_call->type_annotation;
            Type *t = ast_call->type_annotation;
            if(TYPE_KIND_IS_NOT_SCALAR(t->kind)) {
                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), t->align);
                non_scalar_return_addrs[non_scalar_return_count++] = arrlast(jp->local_offset);
                arrlast(jp->local_offset) += t->bytes;
            }
        }
    } else {
        if(ast_call->n_types_returned > 0)
            return_type = callee_type->proc.ret.types[0];

        for(int i = 0; i < callee_type->proc.ret.n; ++i) {
            Type *t = callee_type->proc.ret.types[i];
            if(TYPE_KIND_IS_NOT_SCALAR(t->kind)) {
                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), t->align);
                non_scalar_return_addrs[non_scalar_return_count++] = arrlast(jp->local_offset);
                arrlast(jp->local_offset) += t->bytes;
            }
        }
    }

    callee_type->proc.non_scalar_return_count = non_scalar_return_count;

    if(callee_type->proc.param.names == NULL) {
        for(AST_param *p = ast_call->params; p; p = p->next) {
            p->has_nested_call = has_nested_call(p->value);

            if(p->index >= callee_type->proc.param.n)
                p->is_vararg = true;

            params[p->index] = p;
            params_passed[p->index] = true;
        }
    } else {
        for(AST_param *p = ast_call->params; p; p = p->next) {
            p->has_nested_call = has_nested_call(p->value);

            if(p->index >= callee_type->proc.param.n)
                p->is_vararg = true;

            if(p->name) {
                for(u64 i = 0; i < callee_type->proc.param.n; ++i) {
                    if(!strcmp(p->name, callee_type->proc.param.names[i])) {
                        p->index = i;
                        break;
                    }
                }
            } else if(!p->is_vararg) {
                p->name = callee_type->proc.param.names[p->index];
            }

            params[p->index] = p;
            params_passed[p->index] = true;
        }

        /* handle default parameters*/

        //TODO maybe we should have a helper function to generate the IR for a value of a given type

        for(int i = 0; i < callee_type->proc.param.n; ++i) {
            if(params_passed[i]) continue;
            AST_param *p = (AST_param *)job_alloc_ast(jp, AST_KIND_param);
            p->index = i;
            p->name = callee_type->proc.param.names[i];
            p->type_annotation = callee_type->proc.param.types[i];
            p->value_annotation = callee_type->proc.param.values[i];
            p->value = job_alloc_ast(jp, AST_KIND_atom);
            ((AST_atom*)(p->value))->type_annotation = p->type_annotation; //TODO this is a terrible hack
            ((AST_atom*)(p->value))->value_annotation = p->value_annotation;
            params[i] = p;
            params_passed[i] = true;
        }
    }

    //TODO this is error prone, need a better way of saving the registers
    //
    // The problem here is that it is expected that any registers being used
    // are in use within the expression. This means that if the left hand side
    // of an assignment is generated before the right registers that shouldn't be
    // saved, will.
    // My idea is to save the values of jp->reg_alloc and jp->float_reg_alloc to
    // local variables that will store the lowest register in use, then we check
    // if the registers being saved are higher than what we saved as the lower bound
    // then we don't save them. I think we can guarantee that each nested call expression
    // won't need to have the outer registers saved as well.
    Arr(u64) iregs_saved = NULL;
    Arr(u64) fregs_saved = NULL;
    Arr(u64) register_save_offsets = NULL;
    arrsetlen(iregs_saved, jp->reg_alloc);
    arrsetlen(fregs_saved, jp->float_reg_alloc);
    arrsetlen(register_save_offsets, arrlen(type_stack));

    // save regs
    for(int i = arrlen(type_stack) - 1; i >= 0; --i) {
        Type *t = type_stack[i];

        if(TYPE_KIND_IS_NOT_SCALAR(t->kind))
            UNREACHABLE;

        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), t->align);

        if(TYPE_KIND_IS_FLOAT(t->kind)) {
            jp->float_reg_alloc--;
            fregs_saved[i] = jp->float_reg_alloc;
            register_save_offsets[i] = arrlast(jp->local_offset);
            inst =
                (IRinst) {
                    .opcode = IROP_SETVARF,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = arrlast(jp->local_offset),
                        .reg_src = jp->float_reg_alloc,
                        .bytes = t->bytes,
                    },
                };
        } else {
            jp->reg_alloc--;
            iregs_saved[i] = jp->reg_alloc;
            register_save_offsets[i] = arrlast(jp->local_offset);
            inst =
                (IRinst) {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = arrlast(jp->local_offset),
                        .reg_src = jp->reg_alloc,
                        .bytes = t->bytes,
                    },
                };
        }

        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
        arrlast(jp->local_offset) += t->bytes;
    }

    // save results of nested calls and varargs to local memory
    for(int i = 0; i < arrlen(params); ++i) {
        AST_param *p = params[i];

        bool param_is_neither_nested_call_nor_vararg = (!p->has_nested_call && !p->is_vararg);

        if(param_is_neither_nested_call_nor_vararg) continue;

        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), p->type_annotation->align);

        arrpush(saved_param_offsets, arrlast(jp->local_offset));

        if(TYPE_KIND_IS_NOT_SCALAR(p->type_annotation->kind)) {
            ir_gen_expr(jp, p->value);

            inst =
                (IRinst) {
                    .opcode = IROP_ADDRVAR,
                    .addrvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = arrlast(jp->local_offset),
                        .reg_dest = jp->reg_alloc,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            arrlast(jp->local_offset) += p->type_annotation->bytes;
            ir_gen_memorycopy(jp, p->type_annotation->bytes, p->type_annotation->align, jp->reg_alloc, jp->reg_alloc - 1);

            jp->reg_alloc--;
        } else {
            ir_gen_expr(jp, p->value);

            IRop opcode;
            u64 reg;

            if(TYPE_KIND_IS_FLOAT(p->type_annotation->kind)) {
                //assert(jp->float_reg_alloc == 1);
                jp->float_reg_alloc--;
                opcode = IROP_SETVARF;
                reg = jp->float_reg_alloc;
            } else {
                //assert(jp->reg_alloc == 1);
                jp->reg_alloc--;
                opcode = IROP_SETVAR;
                reg = jp->reg_alloc;
            }

            inst =
                (IRinst) {
                    .opcode = opcode,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = arrlast(jp->local_offset),
                        .reg_src = reg,
                        .bytes = p->type_annotation->bytes,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);
            arrlast(jp->local_offset) += p->type_annotation->bytes;
        }
    }

    inst =
        (IRinst) {
            .opcode = is_foreign ? IROP_HINT_BEGIN_FOREIGN_CALL : IROP_HINT_BEGIN_CALL,
            .hint = { .proc_type = callee_type },
        };
    inst.loc = jp->cur_loc;
    arrpush(jp->instructions, inst);

    if(varargs) {
        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), _Alignof(Any));

        u64 begin_varargs_array_offset = arrlast(jp->local_offset);
        u64 n_varargs = arrlen(params) - callee_type->proc.param.n;

        arrlast(jp->local_offset) += n_varargs * sizeof(Any);
        u64 cur_varargs_array_offset = arrlast(jp->local_offset) - sizeof(Any);

        while(arrlen(params) > callee_type->proc.param.n) {
            AST_param *p = arrpop(params);
            assert(p->is_vararg);

            u64 param_data_offset = arrpop(saved_param_offsets);

            inst =
                (IRinst) {
                    .opcode = IROP_ADDRVAR,
                    .addrvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = param_data_offset,
                        .reg_dest = jp->reg_alloc,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            inst =
                (IRinst) {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = cur_varargs_array_offset + offsetof(Any, data),
                        .reg_src = jp->reg_alloc,
                        .bytes = 8,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            inst =
                (IRinst) {
                    .opcode = IROP_ADDRVAR,
                    .addrvar = {
                        .segment = IRSEG_TYPE,
                        .offset = (u64)(void*)job_make_type_info(jp, p->type_annotation),
                        .reg_dest = jp->reg_alloc,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            inst =
                (IRinst) {
                    .opcode = IROP_SETVAR,
                    .setvar = {
                        .segment = IRSEG_LOCAL,
                        .offset = cur_varargs_array_offset + offsetof(Any, type),
                        .reg_src = jp->reg_alloc,
                        .bytes = 8,
                    },
                };
            inst.loc = jp->cur_loc;
            arrpush(jp->instructions, inst);

            cur_varargs_array_offset -= sizeof(Any);
            //arrlast(jp->local_offset) += sizeof(Any);
        }
        //printf("cur_varargs_array_offset = %lu\narrlast(jp->local_offset) = %lu\n", cur_varargs_array_offset, arrlast(jp->local_offset) - ((n_varargs + 1) * sizeof(Any)));
        //UNREACHABLE;
        assert(cur_varargs_array_offset == arrlast(jp->local_offset) - ((n_varargs + 1) * sizeof(Any)));

        //TODO is this really necessary if we don't pass any arguments in the varargs list?
        inst =
            (IRinst) {
                .opcode = IROP_ADDRVAR,
                .addrvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = begin_varargs_array_offset,
                    .reg_dest = jp->reg_alloc,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), _Alignof(Array_view));

        inst =
            (IRinst) {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = arrlast(jp->local_offset) + offsetof(Array_view, data),
                    .reg_src = jp->reg_alloc,
                    .bytes = 8,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        inst =
            (IRinst) {
                .opcode = IROP_SETVAR,
                .setvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = arrlast(jp->local_offset) + offsetof(Array_view, count),
                    .imm.integer = n_varargs,
                    .bytes = 8,
                    .immediate = true,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        inst =
            (IRinst) {
                .opcode = IROP_ADDRVAR,
                .addrvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = arrlast(jp->local_offset),
                    .reg_dest = jp->reg_alloc,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        arrlast(jp->local_offset) += sizeof(Array_view);

        inst =
            (IRinst) {
                .opcode = IROP_SETARG,
                .setport = {
                    .port = non_scalar_return_count + callee_type->proc.param.n,
                    .reg_src = jp->reg_alloc,
                    .bytes = 8,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

    }

    assert(arrlen(params) == callee_type->proc.param.n);

    while(arrlen(params) > 0) {
        AST_param *p = arrpop(params);

        if(p->has_nested_call) {
            if(TYPE_KIND_IS_NOT_SCALAR(p->type_annotation->kind)) {

                assert(jp->reg_alloc == 0);

                inst =
                    (IRinst) {
                        .opcode = IROP_ADDRVAR,
                        .getvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = arrpop(saved_param_offsets),
                            .reg_dest = jp->reg_alloc,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                if(is_foreign) {
                    inst = (IRinst) { .opcode = IROP_HINT_BEGIN_PASS_NON_SCALAR, };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                }

                inst =
                    (IRinst) {
                        .opcode = IROP_SETARG,
                        .setport = {
                            .port = non_scalar_return_count + p->index,
                            .reg_src = jp->reg_alloc,
                            .bytes = 8,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                if(is_foreign) {
                    inst = (IRinst) { .opcode = IROP_HINT_END_PASS_NON_SCALAR, };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                }

            } else {
                IRop opcode1 = IROP_GETVAR;
                IRop opcode2 = IROP_SETARG;
                u64 reg = jp->reg_alloc;

                if(TYPE_KIND_IS_FLOAT(p->type_annotation->kind)) {
                    opcode1 = IROP_GETVARF;
                    opcode2 = IROP_SETARGF;
                    reg = jp->float_reg_alloc;
                }

                inst =
                    (IRinst) {
                        .opcode = opcode1,
                        .getvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = arrpop(saved_param_offsets),
                            .reg_dest = reg,
                            .bytes = p->type_annotation->bytes,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                inst =
                    (IRinst) {
                        .opcode = opcode2,
                        .setport = {
                            .port = non_scalar_return_count + p->index,
                            .bytes = p->type_annotation->bytes,
                            .reg_src = reg,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            }
        } else {
            if(TYPE_KIND_IS_NOT_SCALAR(p->type_annotation->kind)) {

                //TODO a note on passing structs by const pointer
                //
                // it would be better in future for us to just pass the pointer
                // to the structs, and in the callee allocate local space and
                // copy the contents of the struct inside the callee body

                ir_gen_expr(jp, p->value);
                assert(jp->reg_alloc == 1);

                arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), p->type_annotation->align);

                inst =
                    (IRinst) {
                        .opcode = IROP_ADDRVAR,
                        .addrvar = {
                            .segment = IRSEG_LOCAL,
                            .offset = arrlast(jp->local_offset),
                            .reg_dest = jp->reg_alloc,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                if(is_foreign) {
                    inst = (IRinst) { .opcode = IROP_HINT_BEGIN_PASS_NON_SCALAR, };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                }

                arrlast(jp->local_offset) += p->type_annotation->bytes;
                ir_gen_memorycopy(jp, p->type_annotation->bytes, p->type_annotation->align, jp->reg_alloc, jp->reg_alloc - 1);

                inst =
                    (IRinst) {
                        .opcode = IROP_SETARG,
                        .setport = {
                            .port = non_scalar_return_count + p->index,
                            .reg_src = jp->reg_alloc,
                            .bytes = 8,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);

                if(is_foreign) {
                    inst = (IRinst) { .opcode = IROP_HINT_END_PASS_NON_SCALAR, };
                    inst.loc = jp->cur_loc;
                    arrpush(jp->instructions, inst);
                }

                jp->reg_alloc--;
            } else {
                //Type *expect_param_type = callee_type->proc.param.types[p->index];

                //assert(types_are_same(expect_param_type, p->type_annotation));

                ir_gen_expr(jp, p->value);

                IRop opcode;
                u64 reg;

                if(TYPE_KIND_IS_FLOAT(p->type_annotation->kind)) {
                    jp->float_reg_alloc--;
                    opcode = IROP_SETARGF;
                    reg = jp->float_reg_alloc;
                } else {
                    jp->reg_alloc--;
                    opcode = IROP_SETARG;
                    reg = jp->reg_alloc;
                }

                inst =
                    (IRinst) {
                        .opcode = opcode,
                        .setport = {
                            .port = non_scalar_return_count + p->index,
                            .bytes = p->type_annotation->bytes,
                            .reg_src = reg,
                        },
                    };
                inst.loc = jp->cur_loc;
                arrpush(jp->instructions, inst);
            }
        }
    }

    //TODO we need to take the order that arguments are passed into account when generating assembly

    for(int i = non_scalar_return_count - 1; i >= 0 ; --i) {
        u64 offset = non_scalar_return_addrs[i];
        inst =
            (IRinst) {
                .opcode = IROP_ADDRVAR,
                .addrvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = offset,
                    .reg_dest = jp->reg_alloc,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
        inst =
            (IRinst) {
                .opcode = IROP_SETARG,
                .setport = {
                    .port = i,
                    .bytes = 8,
                    .reg_src = jp->reg_alloc,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    /* pass context pointer */
    if(!callee_type->proc.is_foreign) {
        inst =
            (IRinst) {
                .opcode = IROP_GETVAR,
                .getvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = 0,
                    .reg_dest = jp->reg_alloc,
                    .bytes = 8,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        inst =
            (IRinst) {
                .opcode = IROP_SETCONTEXTARG,
                .setcontextarg = { .reg_src = jp->reg_alloc },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    if(callee_symbol->constant == false) {
        inst =
            (IRinst) {
                .opcode = IROP_GETVAR,
                .getvar = {
                    .segment = callee_symbol->segment,
                    .offset = callee_symbol->segment_offset,
                    .reg_dest = jp->reg_alloc,
                    .bytes = 8,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);

        inst =
            (IRinst) {
                .opcode = IROP_CALL,
                .call = {
                    .id_reg = jp->reg_alloc,
                    .immediate = false,
                    .c_call = c_call,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    } else {
        inst =
            (IRinst) {
                .opcode = IROP_CALL,
                .call = {
                    .name = callee_symbol->name,
                    .id_imm = callee_symbol->procid,
                    .immediate = true,
                    .c_call = c_call,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    //TODO this might be wrong
    if(arrlen(iregs_saved) > 0) jp->reg_alloc = arrlast(iregs_saved) + 1;
    if(arrlen(fregs_saved) > 0) jp->float_reg_alloc = arrlast(fregs_saved) + 1;

    for(int i = arrlen(type_stack) - 1; i >= 0; --i) {
        Type *t = type_stack[i];

        Arr(u64) regs_saved = iregs_saved;
        IRop opcode = IROP_GETVAR;

        if(TYPE_KIND_IS_NOT_SCALAR(t->kind))
            UNREACHABLE;

        if(TYPE_KIND_IS_FLOAT(t->kind)) {
            regs_saved = fregs_saved;
            opcode = IROP_GETVARF;
        }

        inst =
            (IRinst) {
                .opcode = opcode,
                .getvar = {
                    .segment = IRSEG_LOCAL,
                    .offset = arrpop(register_save_offsets),
                    .reg_dest = arrpop(regs_saved),
                    .bytes = t->bytes,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    arrfree(register_save_offsets);
    arrfree(fregs_saved);
    arrfree(iregs_saved);

    if(callee_type->proc.ret.n == 0) {
        inst =
            (IRinst) {
                .opcode = is_foreign ? IROP_HINT_END_FOREIGN_CALL : IROP_HINT_END_CALL,
                .hint = { .proc_type = callee_type },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
        return type_stack;
    }

    if(TYPE_KIND_IS_NOT_SCALAR(return_type->kind)) {
        inst =
            (IRinst) {
                .opcode = IROP_GETRET,
                .getport = {
                    .reg_dest = jp->reg_alloc++,
                    .bytes = 8,
                    .port = 0,
                    .c_call = c_call,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    } else {
        IRop opcode;
        u64 reg_dest;
        if(TYPE_KIND_IS_FLOAT(return_type->kind)) {
            opcode = IROP_GETRETF;
            reg_dest = jp->float_reg_alloc;
            jp->float_reg_alloc++;
        } else {
            opcode = IROP_GETRET;
            reg_dest = jp->reg_alloc;
            jp->reg_alloc++;
        }

        inst =
            (IRinst) {
                .opcode = opcode,
                .getport = {
                    .reg_dest = reg_dest,
                    .bytes = return_type->bytes,
                    .port = 0,
                    .c_call = c_call,
                },
            };
        inst.loc = jp->cur_loc;
        arrpush(jp->instructions, inst);
    }

    inst =
        (IRinst) {
            .opcode = is_foreign ? IROP_HINT_END_FOREIGN_CALL : IROP_HINT_END_CALL,
            .hint = { .proc_type = callee_type },
        };
    inst.loc = jp->cur_loc;
    arrpush(jp->instructions, inst);

    arrpush(type_stack, return_type);

    arrfree(params);
    arrfree(saved_param_offsets);
    job_ast_allocator_from_save(jp, ast_save);

    return type_stack;
}

INLINE u64 align_up(u64 offset, u64 align) {
    if(align == 0) return offset;

    return (offset + align - 1) & ~(align - 1);
}

INLINE u64 next_pow_2(u64 v) {
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v++;
    return v;
}

INLINE AST* arena_dup_ast(Arena *a, AST* ast) {
    assert(ast);
    ASTkind kind = ast->kind;
    assert(kind >= 0 && kind < AST_KIND_MAX);
    AST *ptr = NULL;
    size_t bytes = ast_kind_size_table[kind];
    ptr = (AST*)arena_alloc(a, bytes);
    memcpy((void*)ptr, (void*)ast, bytes);
    return ptr;
}

INLINE AST* ast_copy(Arena *arena, AST *root) {
    AST_block head = {
        .base = { .kind = AST_KIND_block },
        .next = NULL,
        .visited = false,
        .down = root,
    };
    Arr(AST*) cur_list = NULL;
    Arr(AST*) next_list = NULL;
    arrpush(cur_list, (AST*)&head);
    while(true) {
        int n = arrlen(cur_list);
        for(int i = 0; i < n; ++i) {
            AST *ast = cur_list[i];
            switch(ast->kind) {
                case AST_KIND_ifstatement:
                    {
                        AST_ifstatement *ast_if = (AST_ifstatement*)ast;
                        if(ast_if->next) {
                            ast_if->next = arena_dup_ast(arena, ast_if->next);
                            arrpush(next_list, ast_if->next);
                        }
                        ast_if->condition = arena_dup_ast(arena, ast_if->condition);
                        arrpush(next_list, ast_if->condition);
                        ast_if->body = arena_dup_ast(arena, ast_if->body);
                        arrpush(next_list, ast_if->body);
                        if(ast_if->branch) {
                            ast_if->branch = arena_dup_ast(arena, ast_if->branch);
                            arrpush(next_list, ast_if->branch);
                        }
                    }
                    break;
                case AST_KIND_switchstatement:
                    UNIMPLEMENTED;
                    break;
                case AST_KIND_casestatement:
                    UNIMPLEMENTED;
                    break;
                case AST_KIND_whilestatement:
                    {
                        AST_whilestatement *ast_while = (AST_whilestatement*)ast;
                        if(ast_while->next) {
                            ast_while->next = arena_dup_ast(arena, ast_while->next);
                            arrpush(next_list, ast_while->next);
                        }
                        ast_while->condition = arena_dup_ast(arena, ast_while->condition);
                        arrpush(next_list, ast_while->condition);
                        ast_while->body = arena_dup_ast(arena, ast_while->body);
                        arrpush(next_list, ast_while->body);
                    }
                    break;
                case AST_KIND_forstatement:
                    UNIMPLEMENTED;
                    break;
                case AST_KIND_breakstatement:
                    {
                        AST_breakstatement *ast_break = (AST_breakstatement*)ast;
                        if(ast_break->next) {
                            ast_break->next = arena_dup_ast(arena, ast_break->next);
                            arrpush(next_list, ast_break->next);
                        }
                    }
                    break;
                case AST_KIND_continuestatement:
                    {
                        AST_continuestatement *ast_continue = (AST_continuestatement*)ast;
                        if(ast_continue->next) {
                            ast_continue->next = arena_dup_ast(arena, ast_continue->next);
                            arrpush(next_list, ast_continue->next);
                        }
                    }
                    break;
                case AST_KIND_push_context:
                    {
                        AST_push_context *ast_push_context = (AST_push_context*)ast;
                        if(ast_push_context->next) {
                            ast_push_context->next = arena_dup_ast(arena, ast_push_context->next);
                            arrpush(next_list, ast_push_context->next);
                        }
                        ast_push_context->down = arena_dup_ast(arena, ast_push_context->down);
                        arrpush(next_list, ast_push_context->down);
                    }
                    break;
                case AST_KIND_returnstatement:
                    {
                        AST_returnstatement *ast_return = (AST_returnstatement*)ast;
                        if(ast_return->next) {
                            ast_return->next = arena_dup_ast(arena, ast_return->next);
                            arrpush(next_list, ast_return->next);
                        }
                        if(ast_return->expr_list) {
                            ast_return->expr_list = (AST_expr_list*)arena_dup_ast(arena, (AST*)(ast_return->expr_list));
                            arrpush(next_list, (AST*)(ast_return->expr_list));
                        }
                    }
                    break;
                case AST_KIND_usingstatement:
                    {
                        AST_usingstatement *ast_using = (AST_usingstatement*)ast;
                        if(ast_using->next) {
                            ast_using->next = arena_dup_ast(arena, ast_using->next);
                            arrpush(next_list, ast_using->next);
                        }
                    }
                    break;
                case AST_KIND_statement:
                    {
                        AST_statement *ast_statement = (AST_statement*)ast;
                        if(ast_statement->next) {
                            ast_statement->next = arena_dup_ast(arena, ast_statement->next);
                            arrpush(next_list, ast_statement->next);
                        }
                        ast_statement->left = arena_dup_ast(arena, ast_statement->left);
                        arrpush(next_list, ast_statement->left);
                        if(ast_statement->right) {
                            ast_statement->right = arena_dup_ast(arena, ast_statement->right);
                            arrpush(next_list, ast_statement->right);
                        }
                    }
                    break;
                case AST_KIND_block:
                    {
                        AST_block *ast_block = (AST_block*)ast;
                        if(ast_block->next) {
                            ast_block->next = arena_dup_ast(arena, ast_block->next);
                            arrpush(next_list, ast_block->next);
                        }
                        ast_block->down = arena_dup_ast(arena, ast_block->down);
                        arrpush(next_list, ast_block->down);
                    }
                    break;
                case AST_KIND_run_directive:
                    {
                        AST_run_directive *ast_run = (AST_run_directive*)ast;
                        assert(ast_run->call_to_run);
                        ast_run->call_to_run = (AST_call*)arena_dup_ast(arena, (AST*)(ast_run->call_to_run));
                        arrpush(next_list, (AST*)(ast_run->call_to_run));
                    }
                    break;
                case AST_KIND_structdecl:
                case AST_KIND_uniondecl:
                    {
                        AST_structdecl *ast_struct = (AST_structdecl*)ast;
                        if(ast_struct->next) {
                            ast_struct->next = arena_dup_ast(arena, ast_struct->next);
                            arrpush(next_list, ast_struct->next);
                        }
                        if(ast_struct->params) {
                            ast_struct->params = (AST_paramdecl*)arena_dup_ast(arena, (AST*)(ast_struct->params));
                            arrpush(next_list, (AST*)(ast_struct->params));
                        }
                        ast_struct->body = arena_dup_ast(arena, ast_struct->body);
                        arrpush(next_list, ast_struct->body);
                    }
                    break;
                case AST_KIND_vardecl:
                    {
                        AST_vardecl *ast_var = (AST_vardecl*)ast;
                        if(ast_var->next) {
                            ast_var->next = arena_dup_ast(arena, ast_var->next);
                            arrpush(next_list, ast_var->next);
                        }
                        assert(ast_var->type || ast_var->init);
                        if(ast_var->type) {
                            ast_var->type = arena_dup_ast(arena, ast_var->type);
                            arrpush(next_list, ast_var->type);
                        }
                        if(ast_var->init) {
                            ast_var->init = arena_dup_ast(arena, ast_var->init);
                            arrpush(next_list, ast_var->init);
                        }
                    }
                    break;
                case AST_KIND_procdecl:
                    {
                        AST_procdecl *ast_proc = (AST_procdecl*)ast;
                        if(ast_proc->next) {
                            ast_proc->next = arena_dup_ast(arena, ast_proc->next);
                            arrpush(next_list, ast_proc->next);
                        }
                        if(ast_proc->params) {
                            ast_proc->params = (AST_paramdecl*)arena_dup_ast(arena, (AST*)(ast_proc->params));
                            arrpush(next_list, (AST*)(ast_proc->params));
                        }
                        if(ast_proc->rets) {
                            ast_proc->rets = (AST_retdecl*)arena_dup_ast(arena, (AST*)(ast_proc->rets));
                            arrpush(next_list, (AST*)(ast_proc->rets));
                        }
                        if(ast_proc->body) {
                            ast_proc->body = arena_dup_ast(arena, ast_proc->body);
                            arrpush(next_list, ast_proc->body);
                        }
                    }
                    break;
                case AST_KIND_paramdecl:
                    {
                        AST_paramdecl *ast_param = (AST_paramdecl*)ast;
                        if(ast_param->next) {
                            ast_param->next = (AST_paramdecl*)arena_dup_ast(arena, (AST*)(ast_param->next));
                            arrpush(next_list, (AST*)(ast_param->next));
                        }
                        assert(ast_param->type);
                        if(ast_param->type) {
                            ast_param->type = arena_dup_ast(arena, ast_param->type);
                            arrpush(next_list, ast_param->type);
                        }
                        if(ast_param->init) {
                            ast_param->init = arena_dup_ast(arena, ast_param->init);
                            arrpush(next_list, ast_param->init);
                        }
                    }
                    break;
                case AST_KIND_retdecl:
                    {
                        AST_retdecl *ast_ret = (AST_retdecl*)ast;
                        if(ast_ret->next) {
                            ast_ret->next = (AST_retdecl*)arena_dup_ast(arena, (AST*)(ast_ret->next));
                            arrpush(next_list, (AST*)(ast_ret->next));
                        }
                        assert(ast_ret->expr);
                        if(ast_ret->expr) {
                            ast_ret->expr = arena_dup_ast(arena, ast_ret->expr);
                            arrpush(next_list, ast_ret->expr);
                        }
                    }
                    break;
                case AST_KIND_expr_list:
                    {
                        AST_expr_list *ast_expr = (AST_expr_list*)ast;
                        ast_expr->expr = arena_dup_ast(arena, ast_expr->expr);
                        arrpush(next_list, ast_expr->expr);
                        if(ast_expr->next) {
                            ast_expr->next = (AST_expr_list*)arena_dup_ast(arena, (AST*)(ast_expr->next));
                            arrpush(next_list, (AST*)(ast_expr->next));
                        }
                    }
                    break;
                case AST_KIND_member_list:
                    {
                        AST_member_list *ast_member = (AST_member_list*)ast;
                        ast_member->value = arena_dup_ast(arena, ast_member->value);
                        arrpush(next_list, ast_member->value);
                        if(ast_member->next) {
                            ast_member->next = (AST_member_list*)arena_dup_ast(arena, (AST*)(ast_member->next));
                            arrpush(next_list, (AST*)(ast_member->next));
                        }
                    }
                    break;
                case AST_KIND_expr:
                    {
                        AST_expr *ast_expr = (AST_expr*)ast;
                        if(ast_expr->left) {
                            ast_expr->left = arena_dup_ast(arena, ast_expr->left);
                            arrpush(next_list, (AST*)(ast_expr->left));
                        }
                        if(ast_expr->right) {
                            ast_expr->right = arena_dup_ast(arena, ast_expr->right);
                            arrpush(next_list, (AST*)(ast_expr->right));
                        }
                    }
                    break;
                case AST_KIND_param:
                    {
                        AST_param *ast_param = (AST_param*)ast;
                        ast_param->value = arena_dup_ast(arena, ast_param->value);
                        arrpush(next_list, (AST*)(ast_param->value));
                        if(ast_param->next) {
                            ast_param->next = (AST_param*)arena_dup_ast(arena, (AST*)(ast_param->next));
                            arrpush(next_list, (AST*)(ast_param->next));
                        }
                    }
                    break;
                case AST_KIND_atom:
                    break;
                case AST_KIND_array_literal:
                    {
                        AST_array_literal *ast_array = (AST_array_literal*)ast;
                        if(ast_array->type) {
                            ast_array->type = arena_dup_ast(arena, ast_array->type);
                            arrpush(next_list, ast_array->type);
                        }
                        assert(ast_array->elements);
                        ast_array->elements = (AST_expr_list*)arena_dup_ast(arena, (AST*)(ast_array->elements));
                        arrpush(next_list, (AST*)(ast_array->elements));
                    }
                    break;
                case AST_KIND_struct_literal:
                    {
                        AST_struct_literal *ast_struct = (AST_struct_literal*)ast;
                        assert(ast_struct->members);
                        ast_struct->members = (AST_member_list*)arena_dup_ast(arena, (AST*)(ast_struct->members));
                        arrpush(next_list, (AST*)(ast_struct->members));
                    }
                    break;
                case AST_KIND_call:
                    {
                        AST_call *ast_call = (AST_call*)ast;
                        ast_call->callee = arena_dup_ast(arena, ast_call->callee);
                        arrpush(next_list, (AST*)(ast_call->callee));
                        ast_call->params = (AST_param*)arena_dup_ast(arena, (AST*)(ast_call->params));
                        arrpush(next_list, (AST*)(ast_call->params));
                    }
                    break;
                case AST_KIND_proctype:
                    {
                        AST_proctype *ast_proc = (AST_proctype*)ast;
                        if(ast_proc->params) {
                            ast_proc->params = (AST_expr_list*)arena_dup_ast(arena, (AST*)(ast_proc->params));
                            arrpush(next_list, (AST*)(ast_proc->params));
                        }
                        if(ast_proc->rets) {
                            ast_proc->rets = (AST_expr_list*)arena_dup_ast(arena, (AST*)(ast_proc->rets));
                            arrpush(next_list, (AST*)(ast_proc->rets));
                        }
                    }
                    break;
            }
        }
        if(arrlen(next_list) == 0) {
            break;
        }
        arrsetlen(cur_list, 0);
        Arr(AST*) tmp;
        tmp = cur_list;
        cur_list = next_list;
        next_list = tmp;
    }
    arrfree(cur_list);
    arrfree(next_list);
    return head.down;
}

//TODO better name conflicts
bool records_have_member_name_conflicts(Job *jp, Loc_info using_loc, Type *a, Type *b) {
    assert(TYPE_KIND_IS_RECORD(a->kind) && TYPE_KIND_IS_RECORD(b->kind));

    for(u64 i = 0; i < a->record.member.i; ++i) {
        char *name = a->record.member.names[i];
        for(u64 j = 0; j < b->record.member.i; ++j) {
            if(!strcmp(name, b->record.member.names[j])) {
                job_error(jp, using_loc,
                        "name conflict between '%s' and '%s', they share the member name '%s'",
                        a->record.name, b->record.name, name);
                return true;
            }
        }
    }

    for(u64 u = 0; u < a->record.use.i; ++u) {
        Type *use_t = a->record.use.types[u];
        for(u64 i = 0; i < use_t->record.member.i; ++i) {
            char *name = use_t->record.member.names[i];
            for(u64 j = 0; j < b->record.member.i; ++j) {
                if(!strcmp(name, b->record.member.names[j])) {
                    job_error(jp, using_loc,
                            "name conflict between '%s' and '%s', they share the member name '%s'",
                            a->record.name, b->record.name, name);
                    return true;
                }
            }
        }
    }

    return false;
}

bool job_runner(char *src, char *src_path) {
    Lexer lexer = {0};
    lexer_init(&lexer, src, src_path);

    bool had_error = false;

    bool all_waiting = false;

    arrpush(job_queue, job_spawn(&jobid_alloc, PIPE_STAGE_PARSE));
    job_queue[0].lexer = &lexer;
    job_queue[0].global_sym_allocator = &global_sym_allocator;
    job_queue[0].global_scope = &global_scope;

    while(arrlen(job_queue) > 0) {
        for(job_queue_pos = 0; job_queue_pos < arrlen(job_queue); ) {
            Job *jp = job_queue + job_queue_pos;

            switch(jp->pipe_stage) {
                case PIPE_STAGE_PARSE:
                    while(true) {
                        jp->allocator = (Job_memory){0};
                        jp->allocator.scratch = arena_alloc(&global_scratch_allocator, sizeof(Arena));
                        jp->parser_at_top_level = true;

                        job_init_allocator_ast(jp);
                        job_init_allocator_scratch(jp);

                        AST *ast = parse_top_level_statement(jp);

                        if(ast == NULL) {
                            Token t = lex(jp->lexer);
                            //assert(t == 0 || jp->state == JOB_STATE_ERROR);

                            if(t != 0) {
                                job_error(jp, jp->lexer->loc, "invalid statement at top level");
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                arrsetlen(job_queue_next, 0);
                            }

                            if(jp->allocator.scratch && jp->allocator.scratch != &global_scratch_allocator)
                                arena_destroy(jp->allocator.scratch);

                            jp->id = -1;

                            break;
                        }

                        //TODO clean job forking and spawning
                        Job new_job = job_spawn(&jobid_alloc, PIPE_STAGE_TYPECHECK);

                        new_job.allocator = jp->allocator;
                        new_job.global_sym_allocator = jp->global_sym_allocator;
                        new_job.global_scope = jp->global_scope;

                        if(ast->kind == AST_KIND_procdecl) {
                            new_job.allocator.scratch = malloc(sizeof(Arena));
                            new_job.allocator.value = malloc(sizeof(Pool));
                            new_job.allocator.sym = malloc(sizeof(Pool));
                            new_job.allocator.type = malloc(sizeof(Pool));

                            job_init_allocator_scratch(&new_job);
                            job_init_allocator_value(&new_job);
                            job_init_allocator_sym(&new_job);
                            job_init_allocator_type(&new_job);

                            AST_procdecl *ast_procdecl = (AST_procdecl*)ast;
                            if(ast_procdecl->is_polymorphic) {
                                new_job.dont_free_ast_allocators = true;
                            }
                        } else {
                            new_job.allocator.scratch = &global_scratch_allocator;
                            new_job.allocator.value = &global_value_allocator;
                            new_job.allocator.sym = &global_sym_allocator;
                            new_job.allocator.type = &global_type_allocator;
                            new_job.dont_free_allocators = true;
                        }

                        new_job.root = ast;
                        arrpush(new_job.tree_pos_stack, ast);

                        arrpush(job_queue_next, new_job);
                        ++job_queue_pos;
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
                                jp->pipe_stage = PIPE_STAGE_IR;
                                //arrpush(job_queue_next, *jp);
                                //Job push_job = *jp;
                                //arrpush(job_queue, push_job);
                                //++job_queue_pos;
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
                            AST_vardecl *ast_vardecl = (AST_vardecl*)ast;

                            typecheck_vardecl(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).waiting_on_name = NULL;
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            arrlast(jp->tree_pos_stack) = ast_vardecl->next;
                        } else if(ast->kind == AST_KIND_structdecl || ast->kind == AST_KIND_uniondecl) {
                            //TODO structs and unions
                            AST_structdecl *ast_struct = (AST_structdecl*)ast;

                            if(ast_struct->n_params > 0) {
                                UNIMPLEMENTED; //TODO parametrized structs
                            }

                            if(ast_struct->visited) {
                                Type *record_type = ast_struct->record_type;

                                //NOTE only align the record when you're done declaring it
                                record_type->bytes = align_up(record_type->bytes, record_type->align);

                                assert(record_type == arrlast(jp->record_types));

                                if(ast_struct->params) {
                                    Scope s = arrpop(jp->scopes);
                                    shfree(s);
                                }

                                arrsetlen(jp->record_types, arrlen(jp->record_types) - 1);

                                if(ast_struct->name) {
                                    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);
                                    Sym *symp = ast_struct->symbol_annotation;

                                    if(is_top_level) {
                                        global_scope_enter(jp, symp);
                                    } else {
                                        job_scope_enter(jp, symp);
                                    }
                                } else {

                                    //TODO this way of handling anonymous sub structs is a bit weird
                                    if(arrlen(jp->record_types) > 0 && !ast_struct->is_name_spaced) {
                                        Type *inner_record = record_type;

                                        Type *outer_record = arrlast(jp->record_types);

                                        u64 offset;
                                        if(outer_record->kind == TYPE_KIND_UNION) {
                                            offset = 0;
                                            if(inner_record->bytes > outer_record->bytes)
                                                outer_record->bytes = inner_record->bytes;
                                        } else {
                                            offset = align_up(outer_record->bytes, inner_record->align);
                                            outer_record->bytes = offset + inner_record->bytes;
                                        }

                                        if(inner_record->align > outer_record->align)
                                            outer_record->align = inner_record->align;

                                        for(u64 i = 0; i < inner_record->record.member.n; ++job_queue_pos) {
                                            u64 outer_i = outer_record->record.member.i;

                                            for(u64 j = 0; j < outer_i; ++j) {
                                                if(!strcmp(outer_record->record.member.names[j], inner_record->record.member.names[i])) {
                                                    job_error(jp, inner_record->record.member.locs[i],
                                                            "multiple declaration of struct field '%s', previously declared on line %i",
                                                            inner_record->record.member.names[i],
                                                            outer_record->record.member.locs[i].line);
                                                }
                                            }

                                            outer_record->record.member.names[outer_i] = inner_record->record.member.names[i];
                                            outer_record->record.member.types[outer_i] = inner_record->record.member.types[i];
                                            outer_record->record.member.values[outer_i] = inner_record->record.member.values[i];
                                            outer_record->record.member.locs[outer_i] = inner_record->record.member.locs[i];
                                            outer_record->record.member.offsets[outer_i] = inner_record->record.member.offsets[i] + offset;

                                            outer_record->record.member.i++;
                                        }
                                    }

                                }

                                /* flatten struct */
                                Arr(Type*) flattened_types = NULL;
                                Arr(u64) flattened_offsets = NULL;
                                u64 offset = 0;
                                int i_member = 0;
                                int i_using = 0;

                                while(true) {

                                    while(i_using < record_type->record.use.n && record_type->record.member.offsets[i_member] > offset) {
                                        Type *t = record_type->record.use.types[i_using];

                                        offset = record_type->record.use.offsets[i_using];

                                        assert(TYPE_KIND_IS_RECORD(t->kind));

                                        for(u64 i = 0; i < t->record.flattened_scalars.n; ++i) {
                                            arrpush(flattened_types, t->record.flattened_scalars.types[i]);
                                            arrpush(flattened_offsets, t->record.flattened_scalars.offsets[i] + offset);
                                        }

                                        i_using++;
                                    }

                                    offset = record_type->record.member.offsets[i_member];

                                    Type *t = record_type->record.member.types[i_member];

                                    if(TYPE_KIND_IS_RECORD(t->kind)) {
                                        for(u64 i = 0; i < t->record.flattened_scalars.n; ++i) {
                                            arrpush(flattened_types, t->record.flattened_scalars.types[i]);
                                            arrpush(flattened_offsets, t->record.flattened_scalars.offsets[i] + offset);
                                        }
                                    } else {
                                        arrpush(flattened_types, t);
                                        arrpush(flattened_offsets, record_type->record.member.offsets[i_member]);
                                    }

                                    i_member++;

                                    if(i_member >= record_type->record.member.n) break;

                                    offset = record_type->record.member.offsets[i_member];
                                }

                                assert(arrlen(flattened_offsets) == arrlen(flattened_types));

                                record_type->record.flattened_scalars.n = arrlen(flattened_types);

                                record_type->record.flattened_scalars.types = job_alloc_scratch(jp, arrlen(flattened_types) * sizeof(Type*));
                                record_type->record.flattened_scalars.offsets = job_alloc_scratch(jp, arrlen(flattened_offsets) * sizeof(u64));

                                memcpy(record_type->record.flattened_scalars.types, flattened_types, arrlen(flattened_types) * sizeof(Type*));
                                memcpy(record_type->record.flattened_scalars.offsets, flattened_offsets, arrlen(flattened_types) * sizeof(u64));

                                arrfree(flattened_types);
                                arrfree(flattened_offsets);

                                {
                                    fprintf(stderr, "\nFLATTENED %s\n", record_type->record.name);
                                    for(u64 i = 0; i < record_type->record.flattened_scalars.n; ++i) {
                                        fprintf(stderr, "%s\t\toffset %lu\n",
                                                job_type_to_str(jp, record_type->record.flattened_scalars.types[i]),
                                                record_type->record.flattened_scalars.offsets[i]);
                                    }
                                    fprintf(stderr, "\n");
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                arrlast(jp->tree_pos_stack) = ast_struct->next;
                                continue;
                            }

                            if(ast_struct->visited == false) {
                                if(ast_struct->params) {
                                    arrpush(jp->scopes, NULL);
                                }
                                arrpush(jp->record_types, NULL);
                                ast_struct->visited = true;
                            }
                            typecheck_structdecl(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).waiting_on_name = NULL;
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            assert(ast_struct->body);
                            arrpush(jp->tree_pos_stack, ast_struct->body);
                        } else if(ast->kind == AST_KIND_usingstatement) {
                            AST_usingstatement *ast_using = (AST_usingstatement*)ast;

                            Sym *using_sym = NULL;
                            using_sym = job_scope_lookup(jp, ast_using->name);

                            if(!using_sym) using_sym = global_scope_lookup(jp, ast_using->name);

                            if(!using_sym) {
                                jp->state = JOB_STATE_WAIT;
                                jp->waiting_on_name = ast_using->name;
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            if(using_sym->type->kind != TYPE_KIND_TYPE) {
                                UNIMPLEMENTED;
                            }

                            Type *using_type = using_sym->value->val.type;

                            if(!TYPE_KIND_IS_RECORD(using_type->kind)) {
                                job_error(jp, ast->loc,
                                        "'%s' must be a struct or union to use using within struct or union",
                                        ast_using->name);
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            if(arrlen(jp->record_types) > 0) {
                                if(using_type->record.use.n > 0) {
                                    job_error(jp, ast->loc,
                                            "only single level struct/union inclusion is allowed, '%s' can't have it's own 'using's",
                                            ast_using->name);
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                Type *record_type = arrlast(jp->record_types);

                                //TODO should using be allowed within anonymous records???
                                if(record_type->record.name == NULL) {
                                    job_error(jp, ast->loc,
                                            "'using' can't be used within an anonymous %s",
                                            (record_type->kind == TYPE_KIND_UNION) ? "union" : "struct");
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(using_type->kind != record_type->kind) {
                                    job_error(jp, ast->loc,
                                            "'using' can only include structs within structs and unions within unions");
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(record_type == using_type) {
                                    job_error(jp, ast->loc,
                                            "circular use of name '%s'",
                                            ast_using->name);
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                //TODO better name conflicts
                                if(records_have_member_name_conflicts(jp, ast_using->base.loc, record_type, using_type)) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                bool in_union = (record_type->kind == TYPE_KIND_UNION);

                                if(using_type->align > record_type->align) {
                                    record_type->align = using_type->align;
                                }

                                u64 bytes = record_type->bytes;

                                u64 i = record_type->record.use.i;
                                record_type->record.use.i++;
                                record_type->record.use.types[i] = using_type;

                                if(in_union) {
                                    if(using_type->bytes > bytes)
                                        bytes = using_type->bytes;
                                } else {
                                    u64 offset = align_up(bytes, using_type->align);
                                    record_type->record.use.offsets[i] = offset;
                                    bytes = align_up(offset + using_type->bytes, record_type->align);
                                }

                                record_type->bytes = bytes;

                            } else {
                                UNIMPLEMENTED;
                            }

                            arrlast(jp->tree_pos_stack) = ast_using->next;

                        } else if(ast->kind == AST_KIND_procdecl) {
                            AST_procdecl *ast_procdecl = (AST_procdecl*)ast;

                            if(ast_procdecl->is_polymorphic) {
                                if(ast_procdecl->visited == false) {
                                    arrpush(jp->scopes, NULL);
                                    ast_procdecl->visited = true;
                                }

                                if(ast_procdecl->polymorphic_params_ready == false) {
                                    typecheck_polymorphic_procdecl(jp);

                                    if(jp->state == JOB_STATE_WAIT) {
                                        arrpush(job_queue_next, *jp);
                                        arrlast(job_queue_next).waiting_on_name = NULL;
                                        arrlast(job_queue_next).state = JOB_STATE_READY;
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    if(jp->state == JOB_STATE_ERROR) {
                                        job_report_all_messages(jp);
                                        job_die(jp);
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    arrlast(jp->tree_pos_stack) = ast_procdecl->next;

                                    if(ast_procdecl->next == NULL) {
                                        ast_procdecl->visited = false;
                                        job_die(jp);
                                        ++job_queue_pos;
                                        continue;
                                    }

                                } else {
                                    if(ast_procdecl->type_checked_body) {
                                        jp->cur_proc_type = NULL;
                                        arrlast(jp->tree_pos_stack) = ast_procdecl->next;

                                        ast_procdecl->type_checked_body = false;
                                        ast_procdecl->visited = false;
                                        AST_block *ast_block = (AST_block*)ast_procdecl->body;
                                        ast_block->visited = true;

                                        Scope s = arrpop(jp->scopes);
                                        for(int i = 0; i < shlen(s); ++i) {
                                            if(s[i].value) {
                                                print_sym(s[i].value[0]);
                                                printf("\n");
                                            }
                                        }
                                        shfree(s);
                                        fprintf(stderr, "\ntype checked the body of '%s'!!!!!!!!!\n\n", ast_procdecl->name);
                                        break;
                                    } else {
                                        Type *proc_type = ast_procdecl->proc_type;
                                        jp->cur_proc_type = proc_type;

                                        for(u64 i = 0; i < proc_type->proc.n_wildcards; ++i) {
                                            Type *wildcard = proc_type->proc.wildcards[i];
                                            Sym *sym = job_alloc_sym(jp);
                                            Value *v = job_alloc_value(jp, VALUE_KIND_TYPE);
                                            v->val.type = wildcard->wildcard.matched;
                                            wildcard->wildcard.matched = NULL;

                                            *sym =
                                                (Sym) {
                                                    .name = wildcard->wildcard.name,
                                                    .loc = ast_procdecl->base.loc,
                                                    .declared_by = jp->id,
                                                    .type = builtin_type+TYPE_KIND_TYPE, //TODO type can't be builtin if we have the .what
                                                    .value = v,
                                                    .constant = true,
                                                };

                                            job_scope_enter(jp, sym);
                                        }

                                        {
                                            Scope s = arrlast(jp->scopes);
                                            for(int i = 0; i < shlen(s); ++i) {
                                                if(s[i].value) {
                                                    print_sym(s[i].value[0]);
                                                    printf("\n");
                                                }
                                            }
                                        }

                                        // NOTE
                                        // mark the polymorphic proc's symbol with something that says it is in use by another job
                                        // which will cause any other jobs trying to do the same polymorph to wait until the job which currently
                                        // owns the polymorphic proc has finished (it's like a lock, but it's fake!)
                                        // see Sym.is_being_used_in_polymorph

                                        AST_paramdecl *param_decls = ast_procdecl->params;
                                        for(u64 i = 0; i < proc_type->proc.param.n; ++i) {
                                            char *param_name = proc_type->proc.param.names[i];
                                            Type *param_type = proc_type->proc.param.types[i];

                                            fprintf(stderr, "%s: %s\n", param_name, job_type_to_str(jp, param_type));

                                            Sym *sym = job_alloc_sym(jp);

                                            *sym =
                                                (Sym) {
                                                    .name = param_name,
                                                    .loc = ast_procdecl->base.loc,
                                                    .declared_by = jp->id,
                                                    .type = param_type,
                                                    .is_argument = true,
                                                };

                                            param_decls->symbol_annotation = sym;
                                            param_decls = param_decls->next;

                                            job_scope_enter(jp, sym);
                                        }

                                        ast_procdecl->type_checked_body = true;
                                        AST_block *ast_block = (AST_block*)ast_procdecl->body;
                                        ast_block->visited = true;
                                        jp->save_polymorphic_proc_body = ast_block->down;
                                        AST *body_copy = ast_copy(jp->allocator.scratch, ast_block->down);
                                        ast_block->down = body_copy;
                                        /* NOTE skip the body block so that params are in the same scope as body */
                                        arrpush(jp->tree_pos_stack, body_copy);
                                    }
                                }

                            } else {
                                if(ast_procdecl->type_checked_body) {
                                    jp->cur_proc_type = NULL;
                                    arrlast(jp->tree_pos_stack) = ast_procdecl->next;
                                    Scope s = arrpop(jp->scopes);
                                    for(int i = 0; i < shlen(s); ++i) {
                                        if(s[i].value) {
                                            print_sym(s[i].value[0]);
                                            printf("\n");
                                        }
                                    }
                                    shfree(s);
                                    fprintf(stderr, "\ntype checked the body of '%s'!!!!!!!!!\n\n", ast_procdecl->name);
                                    break;
                                }

                                if(ast_procdecl->visited == false) {
                                    arrpush(jp->scopes, NULL);
                                    ast_procdecl->visited = true;
                                }

                                typecheck_procdecl(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).waiting_on_name = NULL;
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(ast_procdecl->body == NULL) {
                                    arrlast(jp->tree_pos_stack) = ast_procdecl->next;
                                } else {
                                    ast_procdecl->type_checked_body = true;
                                    AST_block *ast_block = (AST_block*)ast_procdecl->body;
                                    ast_block->visited = true;
                                    /* NOTE skip the body block so that params are in the same scope as body */
                                    arrpush(jp->tree_pos_stack, ast_block->down);
                                }
                            }

                        } else if(ast->kind == AST_KIND_push_context) {
                            AST_push_context *ast_push = (AST_push_context*)ast;

                            Sym *new_context_sym = job_scope_lookup(jp, ast_push->context_ident);

                            if(!new_context_sym) new_context_sym = global_scope_lookup(jp, ast_push->context_ident);

                            if(!new_context_sym) {
                                job_error(jp, ast_push->base.loc, "undeclared identifier '%s' in push_context block",
                                        ast_push->context_ident);

                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            ast_push->symbol_annotation = new_context_sym;

                            arrlast(jp->tree_pos_stack) = ast_push->next;

                            arrpush(jp->tree_pos_stack, ast_push->down);

                        } else if(ast->kind == AST_KIND_block) {
                            AST_block *ast_block = (AST_block*)ast;

                            if(ast_block->visited) {
                                Scope s = arrpop(jp->scopes);
                                /*
                                   for(int i = 0; i < shlen(s); ++i) {
                                   if(s[i].value) {
                                   print_sym(s[i].value[0]);
                                   printf("\n");
                                   }
                                   }
                                   */
                                ast_block->visited = false;
                                shfree(s);
                                arrlast(jp->tree_pos_stack) = ast_block->next;
                                continue;
                            }

                            ast_block->visited = true;
                            arrpush(jp->tree_pos_stack, ast_block->down);
                            arrpush(jp->scopes, NULL);
                        } else if(ast->kind == AST_KIND_ifstatement) {
                            AST_ifstatement *ast_if = (AST_ifstatement*)ast;

                            if(arrlen(jp->expr) == 0) {
                                linearize_expr(jp, ast_if->condition);
                            }
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).waiting_on_name = NULL;
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
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
                            AST_whilestatement *ast_while = (AST_whilestatement*)ast;

                            if(arrlen(jp->expr) == 0) {
                                linearize_expr(jp, ast_while->condition);
                            }
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                arrpush(job_queue_next, *jp);
                                arrlast(job_queue_next).waiting_on_name = NULL;
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            arrsetlen(jp->type_stack, 0);
                            arrsetlen(jp->value_stack, 0);
                            arrsetlen(jp->expr, 0);
                            jp->expr_pos = 0;

                            arrlast(jp->tree_pos_stack) = ast_while->next;
                            arrpush(jp->tree_pos_stack, ast_while->body);

                        } else if(ast->kind == AST_KIND_forstatement) {
                            AST_forstatement *ast_for = (AST_forstatement*)ast;

                            if(ast_for->visited) {
                                Scope s = arrpop(jp->scopes);
                                for(int i = 0; i < shlen(s); ++i) {
                                    if(s[i].value) {
                                        print_sym(s[i].value[0]);
                                        printf("\n");
                                    }
                                }
                                ast_for->visited = false;
                                shfree(s);
                                arrlast(jp->tree_pos_stack) = ast_for->next;
                                continue;
                            }

                            Type *for_elem_type;

                            if(ast_for->is_range_for) {
                                if(ast_for->by_pointer) {
                                    job_error(jp, ast_for->base.loc, "range for cannot be iterated by pointer");
                                }

                                Type *for_begin_expr_type = NULL;

                                if(!ast_for->checked_begin_range) {
                                    if(arrlen(jp->expr) == 0) {
                                        linearize_expr(jp, ast_for->begin_range_expr);
                                    }
                                    typecheck_expr(jp);
                                    for_begin_expr_type = ((AST_expr_base*)(ast_for->begin_range_expr))->type_annotation;
                                    ast_for->begin_range_type = for_begin_expr_type;

                                    if(jp->state == JOB_STATE_WAIT) {
                                        arrpush(job_queue_next, *jp);
                                        arrlast(job_queue_next).waiting_on_name = NULL;
                                        arrlast(job_queue_next).state = JOB_STATE_READY;
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    if(jp->state == JOB_STATE_ERROR) {
                                        job_report_all_messages(jp);
                                        job_die(jp);
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    arrsetlen(jp->type_stack, 0);
                                    arrsetlen(jp->value_stack, 0);
                                    arrsetlen(jp->expr, 0);
                                    jp->expr_pos = 0;
                                }

                                assert(for_begin_expr_type);

                                Type *for_end_expr_type = NULL;

                                if(!ast_for->checked_end_range) {
                                    if(arrlen(jp->expr) == 0) {
                                        linearize_expr(jp, ast_for->end_range_expr);
                                    }
                                    typecheck_expr(jp);
                                    for_end_expr_type = ((AST_expr_base*)(ast_for->end_range_expr))->type_annotation;

                                    ast_for->end_range_type = for_end_expr_type;

                                    if(jp->state == JOB_STATE_WAIT) {
                                        arrpush(job_queue_next, *jp);
                                        arrlast(job_queue_next).waiting_on_name = NULL;
                                        arrlast(job_queue_next).state = JOB_STATE_READY;
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    if(jp->state == JOB_STATE_ERROR) {
                                        job_report_all_messages(jp);
                                        job_die(jp);
                                        ++job_queue_pos;
                                        continue;
                                    }

                                    arrsetlen(jp->type_stack, 0);
                                    arrsetlen(jp->value_stack, 0);
                                    arrsetlen(jp->expr, 0);
                                    jp->expr_pos = 0;
                                }

                                assert(for_end_expr_type);

                                if(!TYPE_KIND_IS_INTEGER(for_begin_expr_type->kind)) {
                                    job_error(jp, ast_for->base.loc,
                                            "the beginning of a for loop range must be integers, not '%s'",
                                            job_type_to_str(jp, for_begin_expr_type));
                                }

                                if(!TYPE_KIND_IS_INTEGER(for_end_expr_type->kind)) {
                                    job_error(jp, ast_for->base.loc,
                                            "the end of a for loop range must be integers, not '%s'",
                                            job_type_to_str(jp, for_end_expr_type));
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                for_elem_type = for_begin_expr_type;
                            } else {
                                if(arrlen(jp->expr) == 0) {
                                    linearize_expr(jp, ast_for->expr);
                                }
                                typecheck_expr(jp);
                                Type *for_expr_type = ((AST_expr_base*)(ast_for->expr))->type_annotation;

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).waiting_on_name = NULL;
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                if(!TYPE_KIND_IS_ARRAY_LIKE(for_expr_type->kind) && for_expr_type->kind != TYPE_KIND_STRING) {
                                    job_error(jp, ast_for->expr->loc,
                                            "for loop expression must evaluate to array type not '%s'",
                                            job_type_to_str(jp, for_expr_type));
                                }

                                ast_for->expr_type = for_expr_type;

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                for_elem_type = TYPE_KIND_IS_ARRAY_LIKE(for_expr_type->kind) ? for_expr_type->array.of : builtin_type+TYPE_KIND_CHAR;

                                if(ast_for->by_pointer) {
                                    //NOTE allocating types inline in this way makes caching a bit more difficult 
                                    Type *t = job_alloc_type(jp, TYPE_KIND_POINTER);
                                    t->pointer.to = for_elem_type;
                                    for_elem_type = t;
                                }
                            }

                            arrpush(jp->scopes, NULL);

                            Sym *symp = NULL;

                            if(ast_for->label) {
                                Sym named_it = {
                                    .name = ast_for->label,
                                    .loc = ast_for->base.loc,
                                    .declared_by = jp->id,
                                    .segment = IRSEG_LOCAL,
                                    .type = for_elem_type,
                                };

                                symp = job_alloc_sym(jp);
                                *symp = named_it;
                                job_scope_enter(jp, symp);
                                ast_for->named_it_symbol = symp;

                                if(!ast_for->is_range_for) {
                                    char suffix[] = "_index";
                                    char *it_index_name = job_alloc_scratch(jp, 1 + strlen(ast_for->label) + STRLEN(suffix));
                                    int i;
                                    for(i = 0; ast_for->label[i]; ++i) it_index_name[i] = ast_for->label[i];
                                    int j;
                                    for(j = 0; suffix[j]; ++j) it_index_name[i++] = suffix[j];

                                    Sym named_it_index = {
                                        .name = it_index_name,
                                        .loc = ast_for->base.loc,
                                        .declared_by = jp->id,
                                        .segment = IRSEG_LOCAL,
                                        .type = builtin_type+TYPE_KIND_INT,
                                    };

                                    symp = job_alloc_sym(jp);
                                    *symp = named_it_index;
                                    job_scope_enter(jp, symp);
                                    ast_for->named_it_index_symbol = symp;
                                }
                            }

                            Sym it_sym = {
                                .name = "it",
                                .loc = ast_for->base.loc,
                                .declared_by = jp->id,
                                .segment = IRSEG_LOCAL,
                                .type = for_elem_type,
                            };

                            symp = job_alloc_sym(jp);
                            *symp = it_sym;
                            job_scope_enter(jp, symp);
                            ast_for->it_symbol = symp;

                            if(!ast_for->is_range_for) {
                                Sym it_index_sym = {
                                    .name = "it_index",
                                    .loc = ast_for->base.loc,
                                    .declared_by = jp->id,
                                    .segment = IRSEG_LOCAL,
                                    .type = builtin_type+TYPE_KIND_INT,
                                };

                                symp = job_alloc_sym(jp);
                                *symp = it_index_sym;
                                job_scope_enter(jp, symp);
                                ast_for->it_index_symbol = symp;
                            }

                            ast_for->visited = true;

                            arrpush(jp->tree_pos_stack, ast_for->body);

                        } else if(ast->kind == AST_KIND_switchstatement) {
                            UNIMPLEMENTED;
                        } else if(ast->kind == AST_KIND_continuestatement) {
                            arrlast(jp->tree_pos_stack) = ((AST_continuestatement*)ast)->next;
                        } else if(ast->kind == AST_KIND_breakstatement) {
                            arrlast(jp->tree_pos_stack) = ((AST_breakstatement*)ast)->next;
                        } else if(ast->kind == AST_KIND_statement) {
                            AST_statement *ast_statement = (AST_statement*)ast;
                            bool type_error_in_statement = false;

                            ast_statement->dont_push_local_offset =
                                (ast_statement->right && ast_statement->right->kind == AST_KIND_array_literal);

                            if(ast_statement->checked_left == false) {
                                if(arrlen(jp->expr) == 0) {
                                    linearize_expr(jp, ast_statement->left);
                                }
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).waiting_on_name = NULL;
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++job_queue_pos;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                if(ast_statement->left->kind == AST_KIND_atom) {
                                    AST_atom *left_atom = (AST_atom*)(ast_statement->left);
                                    assert(left_atom->token == TOKEN_IDENT);
                                    assert(left_atom->symbol_annotation != NULL);
                                    if(left_atom->symbol_annotation->constant)
                                        job_error(jp, left_atom->base.loc, "cannot assign to constant '%s'", left_atom->symbol_annotation->name);
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    type_error_in_statement = true;
                                    jp->state = JOB_STATE_READY;
                                }

                                ast_statement->checked_left = true;
                            }

                            if(ast_statement->right == NULL && ast_statement->assign_op == 0) {
                                arrlast(jp->tree_pos_stack) = ast_statement->next;

                                if(type_error_in_statement) {
                                    jp->state = JOB_STATE_ERROR;
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                }

                                continue;
                            } else if(ast_statement->assign_op == TOKEN_PLUSPLUS || ast_statement->assign_op == TOKEN_MINUSMINUS) {
                                assert(ast_statement->right == NULL);

                                Type *type = ((AST_expr*)(ast_statement->left))->type_annotation;

                                if(type_error_in_statement) {
                                    jp->state = JOB_STATE_ERROR;
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                Type *t = typecheck_operation(jp, type, NULL, ast_statement->assign_op, &(ast_statement->left), NULL);

                                if(!t) {
                                    job_error(jp, ast_statement->base.loc,
                                            "invalid type '%s' to '%s'",
                                            job_type_to_str(jp, type),
                                            job_op_token_to_str(jp, ast_statement->assign_op));
                                    type_error_in_statement = true;
                                }

                                arrlast(jp->tree_pos_stack) = ast_statement->next;

                                if(type_error_in_statement) {
                                    jp->state = JOB_STATE_ERROR;
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                continue;
                            }

                            if(type_error_in_statement) {
                                jp->state = JOB_STATE_ERROR;
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            Type *type_left = ((AST_expr_base*)(ast_statement->left))->type_annotation;
                            assert(type_left);
                            assert(ast_statement->right != NULL);
                            assert(ast_statement->assign_op == '=' ||
                                    (ast_statement->assign_op >= TOKEN_PLUSEQUAL && ast_statement->assign_op <= TOKEN_XOREQUAL));

                            if(ast_statement->checked_right == false) {
                                //TODO should this check be for array views that take an array lit as well?
                                if(type_left->kind == TYPE_KIND_ARRAY && ast_statement->right->kind == AST_KIND_array_literal) {
                                    AST_array_literal *array_lit = (AST_array_literal*)(ast_statement->right);

                                    array_lit->type_annotation = type_left;
                                }

                                if(arrlen(jp->expr) == 0) {
                                    linearize_expr(jp, ast_statement->right);
                                }
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).waiting_on_name = NULL;
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++job_queue_pos;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                type_error_in_statement = (jp->state == JOB_STATE_ERROR);

                                ast_statement->checked_right = true;
                            }

                            if(type_error_in_statement) {
                                jp->state = JOB_STATE_ERROR;
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            Type *type_right = NULL;
                            if(ast_statement->right) type_right = ((AST_expr_base*)(ast_statement->right))->type_annotation;

                            Type *t = typecheck_operation(jp,
                                    type_left, type_right,
                                    ast_statement->assign_op,
                                    &(ast_statement->left), &(ast_statement->right));

                            if(!t) {
                                job_error(jp, ast_statement->base.loc,
                                        "invalid assignment of '%s' to '%s'",
                                        job_type_to_str(jp, type_right),
                                        job_type_to_str(jp, type_left));
                            }

                            if(ast_statement->left->kind == AST_KIND_expr) {
                                AST_expr *left_expr = (AST_expr*)(ast_statement->left);
                                if(left_expr->token == '.') {
                                    AST_expr_base *dot_left = (AST_expr_base*)(left_expr->left);
                                    if(dot_left->type_annotation->kind == TYPE_KIND_ARRAY) {
                                        job_error(jp, left_expr->base.loc,
                                                "cannot assign to fields of '%s'",
                                                job_type_to_str(jp, dot_left->type_annotation));
                                    } 
                                }
                            }

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            if(!types_are_same(type_left, type_right)) {
                                AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                                cast_expr->token = TOKEN_CAST;
                                cast_expr->left = NULL;
                                cast_expr->type_annotation = type_left;
                                cast_expr->right = ast_statement->right;
                                ast_statement->right = (AST*)cast_expr;
                            }

                            arrlast(jp->tree_pos_stack) = ast_statement->next;

                        } else if(ast->kind == AST_KIND_returnstatement) {
                            AST_returnstatement *ast_return = (AST_returnstatement*)ast;
                            Type *cur_proc_type = jp->cur_proc_type;

                            //NOTE in future when macros are added, we could have a value kind for the macro
                            //     expansion which would annotate the node, that way we wouldn't need to use
                            //     Arr(AST**) for jp->expr
                            if(arrlen(jp->expr_list) == 0) {
                                for(AST_expr_list *list = ast_return->expr_list; list; list = list->next) {
                                    arrpush(jp->expr_list, list);
                                }
                                jp->expr_list_pos = 0;
                            }

                            if(cur_proc_type->proc.ret.n != arrlen(jp->expr_list))
                                job_error(jp, ast_return->base.loc,
                                        "mismatch in number of return values when returning from '%s', expected '%lu' got '%lu'",
                                        cur_proc_type->proc.name, cur_proc_type->proc.ret.n, arrlen(jp->expr_list));

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            bool job_needs_to_wait = false;
                            bool job_needs_to_fork = false;

                            for(u64 expr_list_pos = jp->expr_list_pos; expr_list_pos < arrlen(jp->expr_list); ++expr_list_pos) {
                                AST_expr_base *ret_expr = (AST_expr_base*)(jp->expr_list[expr_list_pos]->expr);

                                if(arrlen(jp->expr) == 0) {
                                    linearize_expr(jp, (AST*)ret_expr);
                                }
                                typecheck_expr(jp);

                                if(jp->state == JOB_STATE_WAIT) {
                                    jp->expr_list_pos = expr_list_pos;
                                    arrpush(job_queue_next, *jp);
                                    arrlast(job_queue_next).waiting_on_name = NULL;
                                    arrlast(job_queue_next).state = JOB_STATE_READY;
                                    ++job_queue_pos;
                                    job_needs_to_wait = true;
                                    break;
                                }

                                if(jp->state == JOB_STATE_ERROR) {
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                arrsetlen(jp->type_stack, 0);
                                arrsetlen(jp->value_stack, 0);
                                arrsetlen(jp->expr, 0);
                                jp->expr_pos = 0;

                                Type *ret_expr_type = ret_expr->type_annotation;
                                Type *expect_type = cur_proc_type->proc.ret.types[expr_list_pos];

                                Type *t = typecheck_operation(jp, expect_type, ret_expr_type, '=', NULL, &(jp->expr_list[expr_list_pos]->expr));
                                if(!t) {
                                    job_error(jp, ret_expr->base.loc,
                                            "in return from '%s' expected type '%s', got '%s'",
                                            cur_proc_type->proc.name,
                                            job_type_to_str(jp, expect_type),
                                            job_type_to_str(jp, ret_expr_type));
                                }

                                if(jp->state != JOB_STATE_ERROR && !types_are_same(expect_type, ret_expr_type)) {
                                    AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                                    cast_expr->token = TOKEN_CAST;
                                    cast_expr->left = NULL;
                                    cast_expr->type_annotation = expect_type;
                                    cast_expr->right = (AST*)ret_expr;
                                    jp->expr_list[expr_list_pos]->expr = (AST*)cast_expr;
                                }
                            }

                            if(job_needs_to_wait) continue;
                            if(job_needs_to_fork) continue;

                            arrsetlen(jp->expr_list, 0);
                            jp->expr_list_pos = 0;

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                                job_die(jp);
                                ++job_queue_pos;
                                continue;
                            }

                            if(ast_return->next) {
                                job_error(jp, ast_return->next->loc,
                                        "unreachable code after return from '%s'",
                                        cur_proc_type->proc.name);
                                ++job_queue_pos;
                                continue;
                            }

                            arrlast(jp->tree_pos_stack) = ast_return->next;

                        } else if(ast->kind == AST_KIND_run_directive) {
                            if(arrlen(jp->expr) == 0)
                                linearize_expr(jp, ast);
                            typecheck_expr(jp);

                            if(jp->state == JOB_STATE_WAIT) {
                                Job push_job = *jp;
                                arrpush(job_queue_next, push_job);
                                arrlast(job_queue_next).waiting_on_name = NULL;
                                arrlast(job_queue_next).state = JOB_STATE_READY;
                                ++job_queue_pos;
                                continue;
                            }

                            arrsetlen(jp->type_stack, 0);
                            arrsetlen(jp->value_stack, 0);
                            arrsetlen(jp->expr, 0);
                            jp->expr_pos = 0;

                            if(jp->state == JOB_STATE_ERROR) {
                                job_report_all_messages(jp);
                            }

                            //{
                            //    printf("\nlocal segment dump\n");
                            //    for(int i = 0; i < 9; ++job_queue_pos) {
                            //        for(int j = 0; j < 30; ++j) {
                            //            printf("%.2X ", jp->interp.local_segment[i*30 + j]);
                            //        }
                            //        printf("\n");
                            //    }
                            //    printf("\n");
                            //}

                            //{
                            //    printf("\nport dump\n");
                            //    for(int i = 0; i < arrlen(jp->interp.ports); ++job_queue_pos) {
                            //        printf("port %i: { .i = %lu, .f32 = %f, .f64 = %g }\n",
                            //                i,
                            //                jp->interp.ports[i].integer,
                            //                jp->interp.ports[i].floating32,
                            //                jp->interp.ports[i].floating64);
                            //    }
                            //}

                            job_die(jp);
                            ++job_queue_pos;

                        } else if(ast->kind == AST_KIND_import_directive) {
                            AST_import_directive *ast_import = (AST_import_directive*)ast;

                            char *import_path;

                            if(ast_import->just_load_the_source) {
                                if(!FileExists(ast_import->path)) {
                                    job_error(jp, ast->loc, "failed import, no such file '%s'", ast_import->path);
                                    job_report_all_messages(jp);
                                    job_die(jp);
                                    ++job_queue_pos;
                                    continue;
                                }

                                import_path = ast_import->path;
                            } else {
                                UNIMPLEMENTED;
                            }

                            arrpush(job_queue, job_spawn(&jobid_alloc, PIPE_STAGE_PARSE));
                            char *src = LoadFileText(import_path);
                            lexer_init(&lexer, src, ast_import->path);
                            arrlast(job_queue).lexer = &lexer;
                            arrlast(job_queue).global_sym_allocator = &global_sym_allocator;
                            arrlast(job_queue).global_scope = &global_scope;

                            job_die(jp);
                            ++job_queue_pos;

                        } else {
                            UNIMPLEMENTED;
                        }
                    }
                    break;
                case PIPE_STAGE_IR:
                    {
                        if(!jp->handling_name) {
                            assert(AST_KIND_IS_RECORD(jp->root->kind));
                            job_die(jp);
                            ++job_queue_pos;
                            continue;
                        }

                        Sym *handling_sym = global_scope_lookup(jp, jp->handling_name);
                        printf("procid %d, %s IR generation in progress\n", handling_sym->procid, jp->handling_name);

                        ir_gen(jp);

                        if(jp->state == JOB_STATE_ERROR) {
                            job_report_all_messages(jp);
                            job_die(jp);
                            ++job_queue_pos;
                            continue;
                        }

                        if(jp->state == JOB_STATE_WAIT) {
                            arrpush(job_queue_next, *jp);
                            arrlast(job_queue_next).waiting_on_name = NULL;
                            arrlast(job_queue_next).state = JOB_STATE_READY;
                            ++job_queue_pos;
                            continue;
                        }

                        if(handling_sym->type->kind == TYPE_KIND_PROC && !handling_sym->is_foreign) {
                            IRproc procedure = hmget(proc_table, handling_sym->procid);
                            ir_gen_C(jp, procedure, handling_sym->type);
                        }

                        job_die(jp);

                        //IRproc procedure = hmget(proc_table, handling_sym->procid);
                        //if(!procedure.is_foreign) {
                        //    IRinst *instructions = procedure.instructions;
                        //    for(int i = 0; i < procedure.n_instructions; ++job_queue_pos) {
                        //        printf("%i: ",i);
                        //        print_ir_inst(instructions[i], stdout);
                        //    }
                        //}
                        ++job_queue_pos;
                        break;
                    }
                    break;
            }

        }

        //TODO add a job id graph to the dependency tracker
        //TODO there's something wrong with detection of undeclared identifiers
        if(arrlen(job_queue_next) == arrlen(job_queue)) {
            int waiting_count = 0;
            for(int i = 0; i < arrlen(job_queue_next); ++i) {
                //NOTE things might be waiting because the job had to fork
                if(job_queue[i].state == JOB_STATE_WAIT && job_queue[i].waiting_on_name) {
                    Job *jp = job_queue + i;
                    Sym *s = global_scope_lookup(jp, jp->waiting_on_name);

                    if(s) {
                        Jobid waiting_on_id = s->declared_by;

                        bool wait = false;

                        for(int i = 0; i < arrlen(job_queue); ++i) {
                            if(job_queue[i].id == waiting_on_id) {
                                wait = true;
                            }
                        }

                        if(wait)
                            ++waiting_count;
                        else
                            jp->state = JOB_STATE_READY;

                    } else {
                        ++waiting_count;
                    }

                } else if(job_queue[i].state == JOB_STATE_WAIT && job_queue[i].waiting_on_id > 0) {
                    Job *jp = job_queue + i;

                    bool wait = false;

                    for(int i = 0; i < arrlen(job_queue); ++i) {
                        if(job_queue[i].id == jp->waiting_on_id) {
                            wait = true;
                        }
                    }

                    if(wait)
                        ++waiting_count;
                    else
                        jp->state = JOB_STATE_READY;

                } else {
                    assert(job_queue[i].state != JOB_STATE_WAIT);
                }
            }

            if(arrlen(job_queue) == waiting_count) {
                all_waiting = true;
                break;
            }
        }

        for(int i = 0; i < arrlen(job_queue); ++i) {
            if(job_queue[i].state == JOB_STATE_ERROR) {
                had_error = true;
                break;
            }
        }

        if(had_error)
            break;

        Arr(Job) tmp = job_queue;
        job_queue = job_queue_next;
        job_queue_next = tmp;
        arrsetlen(job_queue_next, 0);
    }

    if(all_waiting) {
        u64 n_jobs = arrlen(job_queue);
        printf("%zu\n",n_jobs);
        Dict(char*) name_graph = NULL;
        Dict(Job*) job_graph = NULL;
        Dict(bool) reported_undeclared_names = NULL;

        for(u64 i = 0; i < arrlen(job_queue); ++i) {
            Job *jp = job_queue + i;
            if(jp->waiting_on_name) {
                if(jp->handling_name) {
                    Job *jp2 = shget(job_graph, jp->handling_name);
                    if(!jp2 || jp->id != jp2->id) {
                        shput(name_graph, jp->handling_name, jp->waiting_on_name);
                        shput(job_graph, jp->handling_name, jp);
                    }
                }
            } else if(jp->waiting_on_id > 0) {
                UNIMPLEMENTED; //TODO allow jobs to wait on other jobs directly instead of through a symbol dependency
            }
        }

        for(u64 i = 0; i < shlen(name_graph); ++i) {
            char *slow = name_graph[i].key;
            char *fast = name_graph[i].key;
            char *save1 = NULL;
            char *save2 = NULL;

            bool has_cycle = false;
            bool save1_got_to_undeclared = false;
            bool save2_got_to_undeclared = false;

            while(slow && fast && !has_cycle) {
                slow = shget(name_graph, slow);
                if(slow == NULL) break;

                save1 = fast;
                fast = shget(name_graph, fast);
                if(fast == NULL) {
                    save1_got_to_undeclared = true;
                    break;
                }

                save2 = fast;
                fast = shget(name_graph, fast);
                if(fast == NULL) {
                    save2_got_to_undeclared = true;
                    break;
                }

                if(!strcmp(slow, fast))
                    has_cycle = true;
            }

            if(save1 || save2) {
                if(has_cycle) {
                    Job *jp1 = shget(job_graph, save2);
                    Job *jp2 = shget(job_graph, fast);
                    job_report_mutual_dependency(jp1, jp2);
                } else if(save1_got_to_undeclared) {
                    //TODO better error printing
                    //     probably need a custom print function with more formats
                    if(!shget(reported_undeclared_names, save1)) {
                        Job *jp = shget(job_graph, save2);
                        job_error(jp, arrlast(jp->tree_pos_stack)->loc, "undeclared identifier '%s'", save1);
                        job_report_all_messages(jp);
                        arrsetlen(jp->messages, 0);
                        shput(reported_undeclared_names, save1, true);
                    }
                } else if(save2_got_to_undeclared) {
                    if(!shget(reported_undeclared_names, save2)) {
                        Job *jp = shget(job_graph, save1);
                        job_error(jp, arrlast(jp->tree_pos_stack)->loc, "undeclared identifier '%s'", save2);
                        job_report_all_messages(jp);
                        arrsetlen(jp->messages, 0);
                        shput(reported_undeclared_names, save2, true);
                    }
                }
            }
        }
    }

    //if(!had_error) {
    //    for(int i = 0; i < shlen(global_scope); ++i) {
    //        if(global_scope[i].value) {
    //            print_sym(global_scope[i].value[0]);
    //            printf("\n");
    //        }
    //    }
    //}

    arrfree(job_queue);
    arrfree(job_queue_next);

    return had_error;
}

Value *atom_to_value(Job *jp, AST_atom *atom) {
    Value *vp = NULL;
    switch(atom->token) {
        default:
            UNREACHABLE;
        case TOKEN_STRINGLIT:
            vp = job_alloc_value(jp, VALUE_KIND_STRING);
            vp->val.str.data = atom->text;
            vp->val.str.len = strlen(atom->text);
            break;
        case TOKEN_TWODOT:
            vp = job_alloc_value(jp, VALUE_KIND_TOKEN);
            vp->val.token = atom->token;
            break;
        case TOKEN_TRUE:
            vp = job_alloc_value(jp, VALUE_KIND_BOOL);
            vp->val.boolean = true;
            break;
        case TOKEN_FALSE:
            vp = job_alloc_value(jp, VALUE_KIND_BOOL);
            vp->val.boolean = false;
            break;
        case TOKEN_CHARLIT:
            vp = job_alloc_value(jp, VALUE_KIND_CHAR);
            vp->val.character = atom->character;
            break;
        case TOKEN_S8: case TOKEN_S16: case TOKEN_S32: case TOKEN_S64:
        case TOKEN_U8: case TOKEN_U16: case TOKEN_U32: case TOKEN_U64:
        case TOKEN_VOID:
        case TOKEN_BOOL:
        case TOKEN_CHAR:
        case TOKEN_FLOAT:
        case TOKEN_F32: case TOKEN_F64:
        case TOKEN_INT:
        case TOKEN_STRING:
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
            UNREACHABLE;
        case TOKEN_STRINGLIT:
            tp = job_alloc_type(jp, TYPE_KIND_STRING);
            break;
        case TOKEN_TWODOT: /* we do this to make checking array constructors easier */
            tp = job_alloc_type(jp, TYPE_KIND_U64);
            break;
        case TOKEN_TRUE:
        case TOKEN_FALSE:
            tp = job_alloc_type(jp, TYPE_KIND_BOOL);
            break;
        case TOKEN_CHARLIT:
            tp = job_alloc_type(jp, TYPE_KIND_CHAR);
            break;
        case TOKEN_VOID:
        case TOKEN_BOOL:
        case TOKEN_INT:
        case TOKEN_S8: case TOKEN_S16: case TOKEN_S32: case TOKEN_S64:
        case TOKEN_U8: case TOKEN_U16: case TOKEN_U32: case TOKEN_U64:
        case TOKEN_F32: case TOKEN_F64:
        case TOKEN_FLOAT:
        case TOKEN_CHAR:
        case TOKEN_STRING:
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

//TODO evaluate_unary and evaluate_binary are horrible
Value* evaluate_unary(Job *jp, Value *a, AST_expr *op_ast) {
    Value *result = NULL;

    Token op = op_ast->token;

    switch(op) {
        case TOKEN_SIZEOF:
            result = job_alloc_value(jp, VALUE_KIND_UINT);
            if(a->kind == VALUE_KIND_TYPE)
                result->val.uinteger = a->val.type->bytes;
            else
                result->val.uinteger = ((AST_expr_base*)(op_ast->right))->type_annotation->bytes;
            break;
        case TOKEN_ALIGNOF:
            result = job_alloc_value(jp, VALUE_KIND_UINT);
            if(a->kind == VALUE_KIND_TYPE)
                result->val.uinteger = a->val.type->align;
            else
                result->val.uinteger = ((AST_expr_base*)(op_ast->right))->type_annotation->align;
            break;
        case TOKEN_TYPEOF:
            result = job_alloc_value(jp, VALUE_KIND_TYPE);
            result->val.type = ((AST_expr_base*)(op_ast->right))->type_annotation;
            break;
        case TOKEN_TYPEINFO:
            result = job_alloc_value(jp, VALUE_KIND_TYPEINFO);
            result->val.typeinfo = job_make_type_info(jp, ((AST_expr_base*)(op_ast->right))->type_annotation);
            break;
    }
    if(result) return result;

    if(a->kind == VALUE_KIND_NIL) return a;

    switch(op) {
        default:
            UNREACHABLE;
        case '>': case '@':
            result = builtin_value+VALUE_KIND_NIL;
            break;
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
            //TODO better evaluation
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
                UNREACHABLE;
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
                    result->val.type->array.element_stride = a->val.type->bytes;
                    result->val.type->bytes = result->val.type->array.element_stride * result->val.type->array.n;
                    result->val.type->align = a->val.type->align;
                    if(result->val.type->array.n == 0)
                        job_error(jp, op_ast->base.loc, "static array size expression evaluates to 0");
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
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_FLOAT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating - b->val.floating;
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
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_FLOAT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating * b->val.floating;
            } else if(a->kind == VALUE_KIND_INT && b->kind == VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_INT);
                result->val.integer = a->val.integer * b->val.integer;
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
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_FLOAT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating / b->val.floating;
            } else if(a->kind == VALUE_KIND_INT && b->kind == VALUE_KIND_INT) {
                result = job_alloc_value(jp, VALUE_KIND_INT);
                result->val.integer = a->val.integer / b->val.integer;
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
            } else if(a->kind == VALUE_KIND_FLOAT && b->kind == VALUE_KIND_FLOAT) {
                result = job_alloc_value(jp, VALUE_KIND_FLOAT);
                result->val.floating = a->val.floating + b->val.floating;
            } else {
                UNREACHABLE;
            }
            break;
        case '%':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger % b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '&':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger & b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '|':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger | b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '^':
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger ^ b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case TOKEN_LSHIFT:
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger << b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case TOKEN_RSHIFT:
            if(a->kind == VALUE_KIND_NIL || b->kind == VALUE_KIND_NIL) {
                result = builtin_value+VALUE_KIND_NIL;
            } else if(a->kind >= VALUE_KIND_BOOL && a->kind <= VALUE_KIND_UINT && b->kind >= VALUE_KIND_BOOL && b->kind <= VALUE_KIND_UINT) {
                result = job_alloc_value(jp, (a->kind > b->kind) ? a->kind : b->kind);
                result->val.uinteger = a->val.uinteger >> b->val.uinteger;
            } else {
                UNREACHABLE;
            }
            break;
        case '>': case '<': case TOKEN_GREATEQUAL: case TOKEN_LESSEQUAL: case TOKEN_EXCLAMEQUAL: case TOKEN_EQUALEQUAL:
            result = builtin_value+VALUE_KIND_NIL; //TODO evaluate
            break;
        case TOKEN_AND:
            result = builtin_value+VALUE_KIND_NIL; //TODO evaluate
                                                   //UNIMPLEMENTED;
            break;
        case TOKEN_OR:
            result = builtin_value+VALUE_KIND_NIL; //TODO evaluate
                                                   //UNIMPLEMENTED;
            break;
    }

    return result;
}

bool types_are_same(Type *a, Type *b) {

    if(a->kind == TYPE_KIND_WILDCARD) {
        if(a->wildcard.matched)
            a = a->wildcard.matched;
        else
            return true;
    }

    if(b->kind == TYPE_KIND_WILDCARD) {
        if(b->wildcard.matched)
            b = b->wildcard.matched;
        else
            return true;
    }

    if(a->kind != b->kind) return false;

    if(a->kind == TYPE_KIND_STRING && b->kind == TYPE_KIND_STRING)
        return true;

    if(a->kind <= TYPE_KIND_TYPE) return true;

    if(a->kind == TYPE_KIND_POINTER)
        return types_are_same(a->pointer.to, b->pointer.to);

    if(a->kind == TYPE_KIND_ARRAY)
        return (a->array.n == b->array.n) && types_are_same(a->array.of, b->array.of);

    if(a->kind >= TYPE_KIND_DYNAMIC_ARRAY && a->kind <= TYPE_KIND_ARRAY_VIEW)
        return types_are_same(a->array.of, b->array.of);

    if(a->kind == TYPE_KIND_PROC) {
        //TODO add something to require names of parameters to be checked as well
        //     this will be good for documenting intent
        if(a->proc.param.n != b->proc.param.n)
            return false;

        if(a->proc.ret.n != b->proc.ret.n)
            return false;

        if(a->proc.varargs != b->proc.varargs)
            return false;

        if(a->proc.c_call != b->proc.c_call)
            return false;

        int n_params = a->proc.param.n;
        int n_rets = a->proc.ret.n;

        for(int i = 0; i < n_params; ++i) {
            if(!types_are_same(a->proc.param.types[i], b->proc.param.types[i]))
                return false;
        }

        for(int i = 0; i < n_rets; ++i) {
            if(!types_are_same(a->proc.ret.types[i], b->proc.ret.types[i]))
                return false;
        }

        return true;
    }

    if(a->kind == TYPE_KIND_ENUM) {
        UNIMPLEMENTED;
    }

    if(TYPE_KIND_IS_RECORD(a->kind)) {
        if(a->record.name && b->record.name) {
            if(strcmp(a->record.name, b->record.name) != 0)
                return false;
        }

        if(a->record.member.n != b->record.member.n)
            return false;

        Type    **a_member_types = a->record.member.types;
        char    **a_member_names = a->record.member.names;
        u64      *a_member_offsets = a->record.member.offsets;

        Type    **b_member_types = b->record.member.types;
        char    **b_member_names = b->record.member.names;
        u64      *b_member_offsets = b->record.member.offsets;

        for(u64 n = a->record.member.n, i = 0; i < n; ++i) {
            if(strcmp(a_member_names[i], b_member_names[i])) return false;
            if(a_member_offsets[i] != b_member_offsets[i]) return false;
            if(!types_are_same(a_member_types[i], b_member_types[i])) return false;
            //TODO should we compare the initial values?
        }

        return true;
    }

    return false;
}

Type* typecheck_operation(Job *jp, Type *a, Type *b, Token op, AST **left, AST **right) {

    if(a && a->kind == TYPE_KIND_WILDCARD) {
        assert(a->wildcard.matched == NULL);
    }

    if(b && b->kind == TYPE_KIND_WILDCARD) {
        assert(b->wildcard.matched == NULL);
    }

    if(!(a && b)) { /* unary operators */
        switch(op) {
            default:
                UNREACHABLE;
            case TOKEN_SIZEOF:
                return builtin_type+TYPE_KIND_U64;
                break;
            case TOKEN_ALIGNOF:
                return builtin_type+TYPE_KIND_U64;
                break;
            case TOKEN_TYPEOF:
                return builtin_type+TYPE_KIND_TYPE;
                break;
            case TOKEN_TYPEINFO:
                {
                    Type *result = job_alloc_type(jp, TYPE_KIND_POINTER);
                    Sym *sym_type_info = global_scope_lookup(jp, "Type_info");
                    result->pointer.to = sym_type_info->value->val.type;
                    return result;
                }
                break;
            case TOKEN_PLUSPLUS: case TOKEN_MINUSMINUS:
                if(a->kind == TYPE_KIND_BOOL)  return a;
                if(a->kind == TYPE_KIND_CHAR)  return a;
                if(a->kind == TYPE_KIND_S8)    return a;
                if(a->kind == TYPE_KIND_U8)    return a;
                if(a->kind == TYPE_KIND_S16)   return a;
                if(a->kind == TYPE_KIND_U16)   return a;
                if(a->kind == TYPE_KIND_S32)   return a;
                if(a->kind == TYPE_KIND_U32)   return a;
                if(a->kind == TYPE_KIND_S64)   return a;
                if(a->kind == TYPE_KIND_U64)   return a;
                if(a->kind == TYPE_KIND_INT)   return a;
                if(a->kind == TYPE_KIND_FLOAT) return a;
                if(a->kind == TYPE_KIND_F32)   return a;
                if(a->kind == TYPE_KIND_F64)   return a;
                if(a->kind == TYPE_KIND_POINTER && a->pointer.to->kind != TYPE_KIND_VOID)
                    return a;
                break;
            case '[':
                if(a->kind == TYPE_KIND_TYPE) return builtin_type+TYPE_KIND_TYPE;
                if(a->kind == TYPE_KIND_WILDCARD) return builtin_type+TYPE_KIND_TYPE;
                break;
            case '+': case '-':
                if(a->kind == TYPE_KIND_BOOL)  return a;
                if(a->kind == TYPE_KIND_CHAR)  return a;
                if(a->kind == TYPE_KIND_S8)    return a;
                if(a->kind == TYPE_KIND_U8)    return a;
                if(a->kind == TYPE_KIND_S16)   return a;
                if(a->kind == TYPE_KIND_U16)   return a;
                if(a->kind == TYPE_KIND_S32)   return a;
                if(a->kind == TYPE_KIND_U32)   return a;
                if(a->kind == TYPE_KIND_S64)   return a;
                if(a->kind == TYPE_KIND_U64)   return a;
                if(a->kind == TYPE_KIND_INT)   return a;
                if(a->kind == TYPE_KIND_FLOAT) return a;
                if(a->kind == TYPE_KIND_F32)   return a;
                if(a->kind == TYPE_KIND_F64)   return a;
                break;
            case '!': case '~':
                if(a->kind == TYPE_KIND_BOOL)  return a;
                if(a->kind == TYPE_KIND_CHAR)  return a;
                if(a->kind == TYPE_KIND_S8)    return a;
                if(a->kind == TYPE_KIND_U8)    return a;
                if(a->kind == TYPE_KIND_S16)   return a;
                if(a->kind == TYPE_KIND_U16)   return a;
                if(a->kind == TYPE_KIND_S32)   return a;
                if(a->kind == TYPE_KIND_U32)   return a;
                if(a->kind == TYPE_KIND_S64)   return a;
                if(a->kind == TYPE_KIND_U64)   return a;
                if(a->kind == TYPE_KIND_INT)   return a;
                break;
            case '*':
                if(a->kind == TYPE_KIND_TYPE) return builtin_type+TYPE_KIND_TYPE;
                if(a->kind == TYPE_KIND_WILDCARD) return builtin_type+TYPE_KIND_TYPE;
                break;
            case '>':
                if(a->kind == TYPE_KIND_POINTER && a->pointer.to->kind != TYPE_KIND_VOID)
                    return a->pointer.to;
                break;
            case '@':
                {
                    Type *t = job_alloc_type(jp, TYPE_KIND_POINTER);
                    t->pointer.to = a;
                    return t;
                }
                break;
        }
    } else {
        switch(op) {
            case TOKEN_CAST:
                if(TYPE_KIND_IS_INTEGER_OR_FLOAT(a->kind) && TYPE_KIND_IS_INTEGER_OR_FLOAT(b->kind))
                    return a;

                if(TYPE_KIND_IS_INTEGER(a->kind) && b->kind == TYPE_KIND_POINTER && b->pointer.to->kind == TYPE_KIND_VOID)
                    return a;

                if(TYPE_KIND_IS_INTEGER(b->kind) && a->kind == TYPE_KIND_POINTER && a->pointer.to->kind == TYPE_KIND_VOID)
                    return a;

                if(a->kind == TYPE_KIND_POINTER && b->kind == TYPE_KIND_POINTER) {
                    if(a->pointer.to->kind == TYPE_KIND_POINTER && b->pointer.to->kind == TYPE_KIND_POINTER) {
                        return typecheck_operation(jp, a->pointer.to, b->pointer.to, op, NULL, NULL);
                    }

                    if(a->pointer.to == b->pointer.to)
                        return a;

                    if(types_are_same(a->pointer.to, b->pointer.to))
                        return a;

                    if(a->pointer.to->kind == TYPE_KIND_VOID || b->pointer.to->kind == TYPE_KIND_VOID)
                        return a;

                    if(!TYPE_KIND_IS_NOT_SCALAR(a->pointer.to->kind) && !TYPE_KIND_IS_NOT_SCALAR(b->pointer.to->kind))
                        return a;

                    if(TYPE_KIND_IS_RECORD(a->pointer.to->kind) && TYPE_KIND_IS_RECORD(b->pointer.to->kind)) {

                        Type *a_rec = a->pointer.to;
                        Type *b_rec = b->pointer.to;

                        if(a_rec->record.name && b_rec->record.name) {
                            for(u64 i = 0; i < b_rec->record.use.n; ++i) {
                                if(a_rec == b_rec->record.use.types[i]) {
                                    return a;
                                }
                            }

                            for(u64 i = 0; i < a_rec->record.use.n; ++i) {
                                if(b_rec == a_rec->record.use.types[i]) {
                                    if(i == 0)
                                        return a;
                                }
                            }
                        }

                    }

                }

                if(a->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_ARRAY_LIKE(b->kind))
                    if(types_are_same(a->pointer.to, b->array.of) || a->pointer.to->kind == TYPE_KIND_VOID)
                        return a;

                if(a->kind == TYPE_KIND_POINTER && b->kind == TYPE_KIND_PROC)
                    if(a->pointer.to->kind == TYPE_KIND_VOID)
                        return a;

                if(a->kind == TYPE_KIND_PROC && b->kind == TYPE_KIND_POINTER)
                    if(b->pointer.to->kind == TYPE_KIND_VOID)
                        return a;

                if(a->kind == TYPE_KIND_ARRAY_VIEW && TYPE_KIND_IS_ARRAY_LIKE(b->kind))
                    if(types_are_same(a->pointer.to, b->array.of))
                        return a;

                {
                    if(a == type_Any) return a;
                    if(b == type_Any) return a;

                    if(a == type_String_view              && b->kind == TYPE_KIND_STRING)        return a;
                    if(a == type_Array_view               && b->kind == TYPE_KIND_ARRAY_VIEW)    return a;
                    if(a == type_Dynamic_array            && b->kind == TYPE_KIND_DYNAMIC_ARRAY) return a;
                    if(a->kind == TYPE_KIND_STRING        && b       == type_String_view)        return a;
                    if(a->kind == TYPE_KIND_ARRAY_VIEW    && b       == type_Array_view)         return a;
                    if(a->kind == TYPE_KIND_DYNAMIC_ARRAY && b       == type_Dynamic_array)      return a;
                }


                if(TYPE_KIND_IS_RECORD(a->kind) && TYPE_KIND_IS_RECORD(b->kind)) {
                    if(a->record.name && b->record.name) {
                        for(u64 i = 0; i < b->record.use.n; ++i) {
                            if(a == b->record.use.types[i]) {
                                return a;
                            }
                        }

                        for(u64 i = 0; i < a->record.use.n; ++i) {
                            if(b == a->record.use.types[i]) {
                                if(i == 0)
                                    return a;
                            }
                        }
                    }
                }

                break;
            case '[':
                if(!TYPE_KIND_IS_INTEGER(b->kind))
                    return NULL;

                if(a->kind == TYPE_KIND_TYPE)        return a;
                if(a->kind == TYPE_KIND_WILDCARD)    return builtin_type+TYPE_KIND_TYPE;
                if(a->kind == TYPE_KIND_POINTER)     return a->pointer.to;
                if(a->kind == TYPE_KIND_STRING)      return builtin_type+TYPE_KIND_CHAR;
                if(TYPE_KIND_IS_ARRAY_LIKE(a->kind)) return a->array.of;

                break;
            case '%':
            case '^': case '&': case '|':
            case TOKEN_LSHIFT: case TOKEN_RSHIFT:
                if(TYPE_KIND_IS_FLOAT(a->kind) || TYPE_KIND_IS_FLOAT(b->kind))
                    return NULL;
            case '*': case '/':
            case '+': case '-':
                assert(left && right);

                if(a->kind > b->kind) { /* type commutative */
                    Type *tmp = a;
                    a = b;
                    b = tmp;
                }

                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_BOOL)      return a;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_CHAR)      return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S8)        return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_U8)        return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S16)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_U16)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S32)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_U32)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S64)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_U64)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_INT)       return b;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_CHAR)      return builtin_type+TYPE_KIND_CHAR;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S8)        return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_U8)        return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S16)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_U16)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S32)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_U32)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S64)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_U64)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_INT)       return b;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_S8)          return a;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U8)          return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_S16)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U16)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_S32)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U32)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_S64)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U64)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_INT)         return b;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_U8)          return a;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S16)         return builtin_type+TYPE_KIND_U16;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_U16)         return b;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S32)         return builtin_type+TYPE_KIND_U32;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_U32)         return b;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S64)         return builtin_type+TYPE_KIND_U64;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_U64)         return b;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_INT)         return b;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_S16)        return a;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U16)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_S32)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U32)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_S64)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U64)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_INT)        return b;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_U16)        return a;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_S32)        return builtin_type+TYPE_KIND_U32;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_U32)        return b;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_S64)        return builtin_type+TYPE_KIND_U64;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_U64)        return b;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_INT)        return b;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_S32)        return a;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_U32)        return b;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_S64)        return b;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_U64)        return b;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_INT)        return b;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_U32)        return a;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_S64)        return builtin_type+TYPE_KIND_U64;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_U64)        return b;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_INT)        return b;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_S64)        return a;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_U64)        return b;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_INT)        return b;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_U64)        return a;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_INT)        return a;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_INT)        return a;
                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_FLOAT)      return b;
                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F32)        return b;
                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F64)        return b;

                if(a->kind == TYPE_KIND_FLOAT && b->kind == TYPE_KIND_FLOAT)    return a;
                if(a->kind == TYPE_KIND_FLOAT && b->kind == TYPE_KIND_F32)      return b;
                if(a->kind == TYPE_KIND_FLOAT && b->kind == TYPE_KIND_F64)      return b;

                if(a->kind == TYPE_KIND_F32 && b->kind == TYPE_KIND_F32)        return a;
                if(a->kind == TYPE_KIND_F32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_F64 && b->kind == TYPE_KIND_F64)        return a;

                if(a->kind == TYPE_KIND_POINTER && b->kind == TYPE_KIND_POINTER && types_are_same(a, b)) return builtin_type+TYPE_KIND_U64;

                if(a->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_INTEGER(b->kind)) return a;
                if(b->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_INTEGER(a->kind)) return b;

                break;
            case '<':
            case '>':
            case TOKEN_LESSEQUAL:
            case TOKEN_GREATEQUAL:
                if(a->kind > b->kind) { /* type commutative */
                    Type *tmp = a;
                    a = b;
                    b = tmp;
                }

                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S8)        return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S16)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S32)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_S64)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S8)        return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S16)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S32)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_S64)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U8)          return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U16)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U32)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_U64)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S16)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S32)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_S64)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U16)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U32)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_U64)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_S32)        return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_S64)        return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_U32)        return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_U64)        return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_S64)        return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_U64)        return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_U64)        return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F64)        return NULL;

                //if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_FLOAT)      return NULL;
                //if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F32)        return NULL;
                //if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_FLOAT && b->kind == TYPE_KIND_F64)      return NULL;

                if(a->kind == TYPE_KIND_F32 && b->kind == TYPE_KIND_F64)        return NULL;

                return builtin_type+TYPE_KIND_BOOL;

                break;
            case TOKEN_EXCLAMEQUAL:
            case TOKEN_EQUALEQUAL:
                if(a->kind > b->kind) { /* type commutative */
                    Type *tmp = a;
                    a = b;
                    b = tmp;
                }

                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_BOOL && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_FLOAT)     return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F32)       return NULL;
                if(a->kind == TYPE_KIND_CHAR && b->kind == TYPE_KIND_F64)       return NULL;

                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_S8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_FLOAT)       return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F32)         return NULL;
                if(a->kind == TYPE_KIND_U8 && b->kind == TYPE_KIND_F64)         return NULL;

                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U16 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U32 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_S64 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_U64 && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_FLOAT)      return NULL;
                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F32)        return NULL;
                if(a->kind == TYPE_KIND_INT && b->kind == TYPE_KIND_F64)        return NULL;

                if(a->kind == TYPE_KIND_FLOAT && b->kind == TYPE_KIND_F64)      return NULL;

                if(a->kind == TYPE_KIND_F32 && b->kind == TYPE_KIND_F64)        return NULL;

                return builtin_type+TYPE_KIND_BOOL;

                break;
            case TOKEN_AND:
            case TOKEN_OR:
                if(a->kind > b->kind) { /* type commutative */
                    Type *tmp = a;
                    a = b;
                    b = tmp;
                }

                if(TYPE_KIND_IS_RECORD(a->kind) || TYPE_KIND_IS_RECORD(b->kind)) return NULL;

                return builtin_type+TYPE_KIND_BOOL;

                break;
            case '=':
                {
                    bool checked = false;

                    if(types_are_same(a, b))
                        return a;

                    if(!checked && TYPE_KIND_IS_FLOAT(a->kind) && TYPE_KIND_IS_FLOAT(b->kind))     checked = true;
                    if(!checked && TYPE_KIND_IS_INTEGER(a->kind) && TYPE_KIND_IS_INTEGER(b->kind)) checked = true;
                    if(!checked && TYPE_KIND_IS_FLOAT(a->kind) && b->kind == TYPE_KIND_INT)        checked = true;

                    if(!checked && TYPE_IS_VOID_POINTER(a) && b->kind == TYPE_KIND_POINTER)  checked = true;
                    if(!checked && a->kind == TYPE_KIND_POINTER && TYPE_IS_VOID_POINTER(b))  checked = true;

                    if(!checked
                            && a->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_ARRAY_LIKE(b->kind)
                            && types_are_same(a->pointer.to, b->array.of)) {
                        checked = true;
                    }

                    if(!checked
                            && a->kind == TYPE_KIND_POINTER && b->kind == TYPE_KIND_STRING
                            && types_are_same(a->pointer.to, builtin_type+TYPE_KIND_CHAR)) {
                        checked = true;
                    }

                    if(!checked
                            && a->kind == TYPE_KIND_ARRAY && b->kind == TYPE_KIND_ARRAY
                            && (a->array.n >= b->array.n) && types_are_same(a->array.of, b->array.of)) {
                        checked = true;
                    }

                    //TODO static array type checking is really horrible
                    //if(!checked && a->kind == TYPE_KIND_ARRAY && b->kind == TYPE_KIND_ARRAY) {
                    //    if(right && right[0]->kind == AST_KIND_array_literal) {
                    //        AST_array_literal *right_array_lit = (AST_array_literal*)*right;

                    //        int elements_checked = 0;
                    //        for(AST_expr_list *expr_list = right_array_lit->elements; expr_list; expr_list = expr_list->next) {
                    //            AST_expr_base *elem_expr = (AST_expr_base*)(expr_list->expr);
                    //            if(typecheck_operation(jp, a->array.of, elem_expr->type_annotation, '=', NULL, &(expr_list->expr)))
                    //                elements_checked++;
                    //        }

                    //        if(elements_checked == right_array_lit->n_elements)
                    //            checked = typecheck_operation(jp, a->array.of, b->array.of, '=', NULL, NULL);
                    //        else
                    //            checked = false;
                    //    } else {
                    //        checked = (a->array.n == b->array.n && typecheck_operation(jp, a->array.of, b->array.of, '=', NULL, NULL));
                    //    }
                    //}

                    if(!checked
                            && a->kind == TYPE_KIND_ARRAY_VIEW && TYPE_KIND_IS_ARRAY_LIKE(b->kind) 
                            && types_are_same(a->array.of, b->array.of)) {
                        checked = true;
                    }

                    if(!checked
                            && a->kind == TYPE_KIND_STRING && TYPE_KIND_IS_ARRAY_LIKE(b->kind) 
                            && types_are_same(builtin_type+TYPE_KIND_CHAR, b->array.of)) {
                        checked = true;
                    }

                    //if(!checked
                    //        && a->kind == TYPE_KIND_DYNAMIC_ARRAY && TYPE_KIND_IS_ARRAY_LIKE(b->kind)
                    //        && typecheck_operation(jp, a->array.of, b->array.of, '=', NULL, NULL)) {
                    //    checked = true;
                    //}

                    if(!checked
                            && a->kind == TYPE_KIND_ARRAY_VIEW && b->kind == TYPE_KIND_STRING 
                            && types_are_same(a->array.of, builtin_type+TYPE_KIND_CHAR)) {
                        checked = true;
                    }

                    if(!checked) {
                        if(a == type_Any)                                                                 checked = true;
                        else if(b == type_Any)                                                            checked = true;
                        else if(a == type_String_view              && b->kind == TYPE_KIND_STRING)        checked = true;
                        else if(a == type_Array_view               && b->kind == TYPE_KIND_ARRAY_VIEW)    checked = true;
                        else if(a == type_Dynamic_array            && b->kind == TYPE_KIND_DYNAMIC_ARRAY) checked = true;
                        else if(a->kind == TYPE_KIND_STRING        && b       == type_String_view)        checked = true;
                        else if(a->kind == TYPE_KIND_ARRAY_VIEW    && b       == type_Array_view)         checked = true;
                        else if(a->kind == TYPE_KIND_DYNAMIC_ARRAY && b       == type_Dynamic_array)      checked = true;

                        else if(a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to->kind == TYPE_KIND_DYNAMIC_ARRAY &&
                                b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to == type_Dynamic_array)
                            checked = true;

                        else if(b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to->kind == TYPE_KIND_DYNAMIC_ARRAY &&
                                a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to == type_Dynamic_array)
                            checked = true;

                        else if(a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to->kind == TYPE_KIND_ARRAY_VIEW &&
                                b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to == type_Array_view)
                            checked = true;

                        else if(b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to->kind == TYPE_KIND_ARRAY_VIEW &&
                                a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to == type_Array_view)
                            checked = true;

                        else if(a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to->kind == TYPE_KIND_STRING &&
                                b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to == type_String_view)
                            checked = true;

                        else if(b->kind == TYPE_KIND_POINTER &&
                                b->pointer.to->kind == TYPE_KIND_STRING &&
                                a->kind == TYPE_KIND_POINTER &&
                                a->pointer.to == type_String_view)
                            checked = true;
                    }

                    if(checked) {
                        if(right) {
                            assert(*right);

                            if(a->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_NOT_SCALAR(b->kind)) {
                                if(TYPE_KIND_IS_ARRAY_LIKE(b->kind) || b->kind == TYPE_KIND_STRING) {
                                    AST_expr *dot_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                                    AST_atom *field_atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                                    field_atom->text = "data";
                                    dot_expr->token = '.';
                                    dot_expr->left = *right;
                                    dot_expr->right = (AST*)field_atom;
                                    dot_expr->type_annotation = job_alloc_type(jp, TYPE_KIND_POINTER);
                                    dot_expr->type_annotation->pointer.to =
                                        (b->kind == TYPE_KIND_STRING) ? (builtin_type+TYPE_KIND_CHAR) : b->array.of;
                                    *right = (AST*)dot_expr;
                                } else {
                                    UNREACHABLE;
                                    //AST_expr *addr_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                                    //addr_expr->token = '@';
                                    //addr_expr->right = *right;
                                    //addr_expr->type_annotation = job_alloc_type(jp, TYPE_KIND_POINTER);
                                    //addr_expr->type_annotation->pointer.to = b;
                                    //*right = (AST*)addr_expr;
                                }
                                //} else if((a->kind == TYPE_KIND_DYNAMIC_ARRAY || a->kind == TYPE_KIND_ARRAY_VIEW) && b->kind == TYPE_KIND_ARRAY) {
                                //    PASS;
                                //AST_expr *dot_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                                //AST_atom *field_atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_atom);
                                //field_atom->text = "data";
                                //dot_expr->token = '.';
                                //dot_expr->left = *right;
                                //dot_expr->right = (AST*)field_atom;
                                //dot_expr->type_annotation = job_alloc_type(jp, TYPE_KIND_POINTER);
                                //dot_expr->type_annotation->pointer.to = b->array.of;
                                //*right = (AST*)dot_expr;
                        } else if(a->kind == TYPE_KIND_ARRAY && b->kind == TYPE_KIND_ARRAY && right[0]->kind == AST_KIND_array_literal) {
                            assert(right[0]->kind == AST_KIND_array_literal);
                            AST_array_literal *right_array = (AST_array_literal*)*right;
                            right_array->type_annotation = a;
                        } else if(a->kind == TYPE_KIND_STRING && TYPE_KIND_IS_ARRAY_LIKE(b->kind)) {
                            PASS;
                        } else {
                            AST_expr *cast_expr = (AST_expr*)job_alloc_ast(jp, AST_KIND_expr);
                            cast_expr->token = TOKEN_CAST;
                            cast_expr->right = *right;
                            cast_expr->type_annotation = a;
                            cast_expr->value_annotation = job_alloc_value(jp, VALUE_KIND_NIL);

                            AST_expr_base *right_expr = (AST_expr_base*)*right;
                            Value *right_expr_value = right_expr->value_annotation;

                            if(right_expr_value && a != type_Any) {
                                if(right_expr_value->kind == VALUE_KIND_INT) {
                                    if(TYPE_KIND_IS_FLOAT32(a->kind)) {
                                        cast_expr->value_annotation->kind = VALUE_KIND_FLOAT;
                                        cast_expr->value_annotation->val.floating = (f32)(right_expr_value->val.integer);
                                    } else if(a->kind == TYPE_KIND_F64) {
                                        cast_expr->value_annotation->kind = VALUE_KIND_DFLOAT;
                                        cast_expr->value_annotation->val.dfloating = (f64)(right_expr_value->val.integer);
                                    } else if(!TYPE_KIND_IS_INTEGER(a->kind)) {
                                        UNREACHABLE;
                                    }

                                } else if(cast_expr->value_annotation->kind == VALUE_KIND_FLOAT) {
                                    if(a->kind == TYPE_KIND_F64) {
                                        cast_expr->value_annotation->kind = VALUE_KIND_DFLOAT;
                                        cast_expr->value_annotation->val.dfloating = (f64)(right_expr_value->val.floating);
                                    } else if(!TYPE_KIND_IS_FLOAT32(a->kind)) {
                                        UNREACHABLE;
                                    }

                                } else if(cast_expr->value_annotation->kind == VALUE_KIND_DFLOAT) {
                                    if(TYPE_KIND_IS_FLOAT32(a->kind)) {
                                        cast_expr->value_annotation->kind = VALUE_KIND_FLOAT;
                                        cast_expr->value_annotation->val.floating = (f32)(right_expr_value->val.dfloating);
                                    } else if(a->kind != TYPE_KIND_F64) {
                                        UNREACHABLE;
                                    }

                                }
                            }

                            *right = (AST*)cast_expr;
                        }
                        }

                        return a;
                    }

                }
                break;
            case TOKEN_PLUSEQUAL: case TOKEN_MINUSEQUAL:
                if(a->kind == TYPE_KIND_POINTER && TYPE_KIND_IS_INTEGER(b->kind)
                        && a->pointer.to->kind != TYPE_KIND_VOID)
                    return a;
            case TOKEN_TIMESEQUAL: case TOKEN_DIVEQUAL:
                if(TYPE_KIND_IS_FLOAT(a->kind)   && b->kind == TYPE_KIND_INT)      return a;
                if(TYPE_KIND_IS_FLOAT(a->kind)   && TYPE_KIND_IS_FLOAT(b->kind))   return a;
                if(TYPE_KIND_IS_INTEGER(a->kind) && TYPE_KIND_IS_INTEGER(b->kind)) return a;
                break;
            case TOKEN_MODEQUAL:
            case TOKEN_ANDEQUAL: case TOKEN_OREQUAL: case TOKEN_XOREQUAL:
            case TOKEN_LSHIFTEQUAL: case TOKEN_RSHIFTEQUAL:
                if(TYPE_KIND_IS_INTEGER(a->kind) && TYPE_KIND_IS_INTEGER(b->kind)) return a;
                break;
        }
    }

    return NULL;
}

Type* typecheck_dot(Job *jp, Type *a, char *field, u64 *offsetp) {
    Type *result = NULL;

    if(a->kind == TYPE_KIND_POINTER) {
        a = a->pointer.to;
    }

    if(a->kind == TYPE_KIND_STRUCT || a->kind == TYPE_KIND_UNION) {
        for(u64 i = 0; i < a->record.member.n; ++i) {
            if(!strcmp(field, a->record.member.names[i])) {
                result = a->record.member.types[i];
                *offsetp = a->record.member.offsets[i];
                break;
            }
        }

        if(!result) {
            for(u64 i = 0; i < a->record.use.n; ++i) {
                Type *a_using = a->record.use.types[i];
                u64 o = 0;
                result = typecheck_dot(jp, a_using, field, &o);
                *offsetp = o + a->record.use.offsets[i];
                if(result) break;
            }
        }
    } else if(a->kind == TYPE_KIND_ENUM) {
        UNIMPLEMENTED;
    } else if(a->kind == TYPE_KIND_STRING) {
        if(!strcmp(field, "len")) {
            result = job_alloc_type(jp, TYPE_KIND_U64);
        } else if(!strcmp(field, "data")) {
            result = job_alloc_type(jp, TYPE_KIND_POINTER);
            result->pointer.to = builtin_type+TYPE_KIND_CHAR;
        }
    } else if(TYPE_KIND_IS_ARRAY_LIKE(a->kind)) {
        if(!strcmp(field, "count")) {
            result = job_alloc_type(jp, TYPE_KIND_U64);
        } else if(!strcmp(field, "cap") && a->kind == TYPE_KIND_DYNAMIC_ARRAY) {
            result = job_alloc_type(jp, TYPE_KIND_U64);
        } else if(!strcmp(field, "allocator") && a->kind == TYPE_KIND_DYNAMIC_ARRAY) {
            result = type_Dynamic_array->record.member.types[3];
        } else if(!strcmp(field, "allocator_data") && a->kind == TYPE_KIND_DYNAMIC_ARRAY) {
            result = type_Dynamic_array->record.member.types[4];
        } else if(!strcmp(field, "data")) {
            result = job_alloc_type(jp, TYPE_KIND_POINTER);
            result->pointer.to = a->array.of;
        }
    }

    return result;
}

void typecheck_expr(Job *jp) {
    assert(jp->expr && arrlen(jp->expr) > 0);
    Arr(Value*) value_stack = jp->value_stack;
    Arr(Type*) type_stack = jp->type_stack;
    Arr(AST*) expr = jp->expr;
    u64 pos = jp->expr_pos;

    for(; pos < arrlen(expr); ++pos) {
        ASTkind kind = expr[pos]->kind;

        if(jp->state == JOB_STATE_ERROR) return;

        if(kind == AST_KIND_atom) {
            AST_atom *atom = (AST_atom*)expr[pos];
            if(atom->token == TOKEN_IDENT) {
                Sym *sym = NULL;
                sym = job_scope_lookup(jp, atom->text);

                if(!sym) sym = global_scope_lookup(jp, atom->text);

                if(!sym) {
                    jp->state = JOB_STATE_WAIT;
                    jp->waiting_on_name = atom->text;
                    break;
                }

                atom->symbol_annotation = sym;
                atom->type_annotation = sym->type;
                arrpush(type_stack, sym->type);

                if(sym->type->kind == TYPE_KIND_WILDCARD) {
                    fprintf(stderr, "typechecker encountered wildcard\n");
                    jp->typechecker_encountered_wildcard = true;
                }

                if(sym->constant) {
                    atom->value_annotation = sym->value;
                    arrpush(value_stack, sym->value);

                    if(sym->type->kind == TYPE_KIND_PROC)
                        arrpush(jp->run_dependencies, sym);

                } else {
                    atom->value_annotation = builtin_value+VALUE_KIND_NIL;
                    arrpush(value_stack, builtin_value+VALUE_KIND_NIL);
                }
            } else if(atom->token == TOKEN_POLYMORPHIC_IDENT) {
                Sym *sym = job_scope_lookup(jp, atom->text);

                assert(jp->cur_proc_type && jp->cur_proc_type->proc.is_polymorphic);

                if(sym) {
                    //TODO custom printf formatting
                    job_error(jp, atom->base.loc, "polymorphic variable '%s' already declared", atom->text);
                    break;
                }


                sym = job_alloc_sym(jp);
                sym->name = atom->text;
                sym->type = job_alloc_type(jp, TYPE_KIND_WILDCARD);
                sym->type->wildcard.name = atom->text;
                sym->is_polymorphic_var = true;
                sym->constant = true;

                Value *v = job_alloc_value(jp, VALUE_KIND_TYPE);
                v->val.type = sym->type;

                sym->value = v;

                job_scope_enter(jp, sym);

                jp->cur_proc_type->proc.wildcards[jp->cur_proc_type->proc.ith_wildcard++] = sym->type;

                atom->type_annotation = sym->type;
                atom->value_annotation = v;

                arrpush(type_stack, sym->type);
                arrpush(value_stack, v);

            } else if(atom->token == TOKEN_CONTEXT) {
                if(type_Context_pointer.kind != TYPE_KIND_POINTER) {
                    Sym *sym = global_scope_lookup(jp, "Context");
                    if(!sym) {
                        jp->state = JOB_STATE_WAIT;
                        jp->waiting_on_name = "Context";
                        break;
                    }
                    atom->type_annotation = global_alloc_scratch(sizeof(Type));
                    atom->type_annotation->kind = TYPE_KIND_POINTER;
                    atom->type_annotation->pointer.to = sym->value->val.type;
                } else {
                    atom->type_annotation = &type_Context_pointer;
                }
                arrpush(type_stack, atom->type_annotation);
                arrpush(value_stack, builtin_value+VALUE_KIND_NIL);
            } else {
                Type *t = atom_to_type(jp, atom);
                atom->type_annotation = t;

                Value *v = atom_to_value(jp, atom);
                atom->value_annotation = v;

                arrpush(type_stack, t);
                arrpush(value_stack, v);
            }
        } else if(kind == AST_KIND_expr) {
            AST_expr *node = (AST_expr*)(expr[pos]);

            Type *result_type = NULL;
            Value *result_value = NULL;

            if(!(node->left && node->right)) {
                Type *a_type = arrpop(type_stack);

                result_type = typecheck_operation(jp, a_type, NULL, node->token, &(node->left), &(node->right));
                node->type_annotation = result_type;
                if(!result_type) {
                    job_error(jp, node->base.loc, "invalid type '%s' to '%s' operator",
                            job_type_to_str(jp, a_type),
                            job_op_token_to_str(jp, node->token));
                    break;
                }

                Value *a_value = arrpop(value_stack);

                result_value = evaluate_unary(jp, a_value, node);

                node->value_annotation = result_value;

                arrpush(type_stack, result_type);
                arrpush(value_stack, result_value);
            } else if(node->left && node->right && node->token == '.') {
                Type *a_type = arrpop(type_stack);
                char *field = ((AST_atom*)(node->right))->text;

                u64 offset = 0;
                result_type = typecheck_dot(jp, a_type, field, &offset);

                if(!result_type) {
                    job_error(jp, node->base.loc, "type '%s' has no field '%s'",
                            job_type_to_str(jp, a_type),
                            field);
                    break;
                }

                //TODO evaluate this properly
                if(a_type->kind == TYPE_KIND_ARRAY && !strcmp(field, "count")) {
                    arrlast(value_stack) = job_alloc_value(jp, VALUE_KIND_INT);
                    arrlast(value_stack)->val.integer = a_type->array.n;
                } else {
                    arrlast(value_stack) = builtin_value+VALUE_KIND_NIL;
                }

                node->type_annotation = result_type;
                node->value_annotation = arrlast(value_stack);
                node->dot_offset_annotation = offset;

                arrpush(type_stack, result_type);
            } else {
                Type *b_type = arrpop(type_stack);
                Type *a_type = arrpop(type_stack);

                Value *b_value = arrpop(value_stack);
                Value *a_value = arrpop(value_stack);

                if(node->token == TOKEN_CAST) {
                    if(a_type->kind != TYPE_KIND_TYPE)
                        job_error(jp, node->base.loc, "cast must be to type");
                    if(jp->state == JOB_STATE_ERROR)
                        break;
                    a_type = a_value->val.type;

                    //NOTE this was a really really dumb idea
                    node->left = NULL; // the type will already be on the cast
                }

                result_type = typecheck_operation(jp, a_type, b_type, node->token, &(node->left), &(node->right));
                node->type_annotation = result_type;
                if(!result_type) {
                    job_error(jp, node->base.loc, "invalid types '%s' and '%s' to '%s' operator",
                            job_type_to_str(jp, a_type),
                            job_type_to_str(jp, b_type),
                            job_op_token_to_str(jp, node->token));
                    break;
                }

                if(a_value && b_value)
                    result_value = evaluate_binary(jp, a_value, b_value, node);

                node->value_annotation = result_value;

                arrpush(type_stack, result_type);
                arrpush(value_stack, result_value);
            }

        } else if(kind == AST_KIND_array_literal) {
            AST_array_literal *array_lit = (AST_array_literal*)(expr[pos]);

            Type *array_elem_type = NULL;
            bool infer_type = true;

            Arr(u64) dimensions = NULL;

            if(array_lit->type_annotation && array_lit->type) {
                Type *t = ((AST_expr_base*)(array_lit->type))->value_annotation->val.type;
                Type *result_type = typecheck_operation(jp, array_lit->type_annotation->array.of, t, '=', NULL, NULL);

                if(result_type == NULL) {
                    job_error(jp, array_lit->base.loc, "cannot assign array of %s to %s",
                            job_type_to_str(jp, t),
                            job_type_to_str(jp, array_lit->type_annotation));
                    break;
                }

            } else if(array_lit->type) {
                array_lit->type_annotation = job_alloc_type(jp, TYPE_KIND_ARRAY);
                array_lit->type_annotation->array.of = ((AST_expr_base*)(array_lit->type))->value_annotation->val.type;
                array_lit->type_annotation->align = array_lit->type_annotation->array.of->align;
                array_lit->type_annotation->array.n = array_lit->n_elements;
                array_lit->type_annotation->array.element_stride = array_lit->type_annotation->array.of->bytes;
                array_lit->type_annotation->bytes = array_lit->type_annotation->array.element_stride * array_lit->type_annotation->array.n;
            }

            if(array_lit->type_annotation) {
                assert(array_lit->type_annotation->kind == TYPE_KIND_ARRAY);
                for(Type *t = array_lit->type_annotation; ; t = t->array.of) {
                    if(t->kind != TYPE_KIND_ARRAY) {
                        array_elem_type = t;
                        infer_type = false;
                        break;
                    }
                    arrpush(dimensions, t->array.n);
                }
            } else {
                array_elem_type = ((AST_expr_base*)(array_lit->flattened_array_literal[0][0]))->type_annotation;
            }

            int second_pass_len = 0;

            bool encountered_error = false;

            for(int i = 0; i < arrlen(array_lit->flattened_array_literal); ++i) {
                AST **astpp = array_lit->flattened_array_literal[i];
                Type *t = ((AST_expr_base*)(*astpp))->type_annotation;

                if(infer_type && array_elem_type->kind == TYPE_KIND_INT && TYPE_KIND_IS_FLOAT(t->kind)) {
                    array_elem_type = t;
                    second_pass_len = i;
                } else if(infer_type && array_elem_type->kind == TYPE_KIND_INT && t->kind == TYPE_KIND_CHAR) {
                    array_elem_type = t;
                    second_pass_len = i;
                } else {
                    Type *result_type = typecheck_operation(jp, array_elem_type, t, '=', NULL, astpp);
                    if(result_type == NULL) {
                        job_error(jp, array_lit->base.loc,
                                "%s and %s cannot both be elements of the same array or sub-array",
                                job_type_to_str(jp, array_elem_type),
                                job_type_to_str(jp, t));

                        encountered_error = true;
                        break;
                    }
                }
            }

            if(encountered_error) break;

            for(int i = 0; i < second_pass_len; ++i) {
                AST **astpp = array_lit->flattened_array_literal[i];
                Type *t = ((AST_expr_base*)(*astpp))->type_annotation;

                Type *result_type = typecheck_operation(jp, array_elem_type, t, '=', NULL, astpp);
                if(result_type == NULL) {
                    job_error(jp, array_lit->base.loc,
                            "%s and %s cannot both be elements of the same array or sub-array",
                            job_type_to_str(jp, array_elem_type),
                            job_type_to_str(jp, t));

                    encountered_error = true;
                    break;
                }
            }

            if(dimensions) {
                u64 len_inferred_dims = arrlen(array_lit->dimensions);
                u64 len_dims = arrlen(dimensions);

                if(len_dims != len_inferred_dims) {
                    job_error(jp, array_lit->base.loc,
                            "mismatched dimensions between array literal and %s",
                            job_type_to_str(jp, array_lit->type_annotation));
                    break;
                }

                for(int i = 0; i < arrlen(dimensions); ++i) {
                    if(dimensions[i] < array_lit->dimensions[i]) {
                        job_error(jp, array_lit->base.loc,
                                "mismatched dimensions between array literal and %s",
                                job_type_to_str(jp, array_lit->type_annotation));
                        encountered_error = true;
                        break;
                    }
                }

                if(encountered_error) break;
            } else {
                Type *t = job_alloc_type(jp, TYPE_KIND_ARRAY);
                assert(array_elem_type);
                t->array.of = array_elem_type;
                t->array.element_stride = array_elem_type->bytes;
                t->array.n = arrpop(array_lit->dimensions);
                t->bytes = t->array.element_stride * t->array.n;
                t->align = array_elem_type->align;
                array_lit->type_annotation = t;
                while(arrlen(array_lit->dimensions)) {
                    t = job_alloc_type(jp, TYPE_KIND_ARRAY);
                    t->array.element_stride = array_lit->type_annotation->bytes;
                    t->array.of = array_lit->type_annotation;
                    t->array.n = arrpop(array_lit->dimensions);
                    t->bytes = t->array.element_stride * t->array.n;
                    t->align = array_lit->type_annotation->align;
                    array_lit->type_annotation = t;
                }
            }

            arrpush(type_stack, array_lit->type_annotation);

            bool generate_value = true;

            for(int i = 0; i < arrlen(array_lit->flattened_array_literal) - 1; ++i) {
                Value *v = ((AST_expr_base*)(array_lit->flattened_array_literal[i][0]))->value_annotation;
                if(!v || v->kind == VALUE_KIND_NIL) {
                    generate_value = false;
                    break;
                }
            }

            //TODO propagate the array type through the sub arrays
            Value *array_val = NULL;

            if(generate_value) {
                array_val = job_alloc_value(jp, VALUE_KIND_ARRAY);

                Arr(AST_expr_list*) array_elem_list_stack = NULL;
                Arr(Type*) array_type_stack = NULL;
                Arr(int) array_val_elems_pos_stack = NULL;
                Arr(Value**) array_val_elems_stack = NULL;

                arrpush(array_type_stack, array_lit->type_annotation);
                arrpush(array_elem_list_stack, array_lit->elements);
                array_val->val.array.elements = job_alloc_scratch(jp, sizeof(Value*) * array_lit->n_elements);
                array_val->val.array.n = array_lit->n_elements;
                arrpush(array_val_elems_stack, array_val->val.array.elements);
                arrpush(array_val_elems_pos_stack, 0);

                while(arrlen(array_elem_list_stack) > 0) {
                    AST_expr_list *e = arrpop(array_elem_list_stack);
                    Type *array_type = arrpop(array_type_stack);
                    Value **array_val_elems = arrpop(array_val_elems_stack);
                    int array_val_elems_pos = arrpop(array_val_elems_pos_stack);

                    for(; e; e = e->next) {
                        if(e->expr->kind == AST_KIND_array_literal) {
                            assert(array_type->array.of->kind == TYPE_KIND_ARRAY);

                            AST_array_literal *a = (AST_array_literal*)(e->expr);
                            a->type_annotation = array_type->array.of;

                            if(e->next) {
                                arrpush(array_type_stack, array_type);
                                arrpush(array_elem_list_stack, e->next);
                                arrpush(array_val_elems_stack, array_val_elems);
                                arrpush(array_val_elems_pos_stack, array_val_elems_pos + 1);
                            }

                            arrpush(array_elem_list_stack, a->elements);
                            arrpush(array_type_stack, array_type->array.of);

                            array_val_elems[array_val_elems_pos] = job_alloc_value(jp, VALUE_KIND_ARRAY);
                            array_val_elems[array_val_elems_pos]->val.array.elements =
                                job_alloc_scratch(jp, sizeof(Value*) * a->n_elements);
                            array_val_elems[array_val_elems_pos]->val.array.n = a->n_elements;

                            arrpush(array_val_elems_stack, array_val_elems[array_val_elems_pos]->val.array.elements);
                            arrpush(array_val_elems_pos_stack, 0);

                            break;
                        } else {
                            array_val_elems[array_val_elems_pos++] = ((AST_expr_base*)(e->expr))->value_annotation;
                        }
                    }

                }

                arrfree(array_type_stack);
                arrfree(array_val_elems_pos_stack);
                arrfree(array_val_elems_stack);
                arrfree(array_elem_list_stack);
            } else {
                Arr(AST_expr_list*) array_elem_list_stack = NULL;
                Arr(Type*) array_type_stack = NULL;

                arrpush(array_type_stack, array_lit->type_annotation);
                arrpush(array_elem_list_stack, array_lit->elements);

                while(arrlen(array_elem_list_stack) > 0) {
                    AST_expr_list *e = arrpop(array_elem_list_stack);
                    Type *array_type = arrpop(array_type_stack);

                    for(; e; e = e->next) {
                        if(e->expr->kind == AST_KIND_array_literal) {
                            assert(array_type->array.of->kind == TYPE_KIND_ARRAY);

                            AST_array_literal *a = (AST_array_literal*)(e->expr);
                            a->type_annotation = array_type->array.of;

                            if(e->next) {
                                arrpush(array_type_stack, array_type);
                                arrpush(array_elem_list_stack, e->next);
                            }

                            arrpush(array_elem_list_stack, a->elements);
                            arrpush(array_type_stack, array_type->array.of);

                            break;
                        }
                    }

                }

                arrfree(array_type_stack);
                arrfree(array_elem_list_stack);
            }

            array_lit->value_annotation = array_val;

            arrpush(value_stack, array_val);

            arrfree(array_lit->flattened_array_literal);
            arrfree(array_lit->dimensions);
            arrfree(dimensions);

        } else if(kind == AST_KIND_param) {
            AST_param *paramp = (AST_param*)expr[pos];
            paramp->type_annotation = arrlast(type_stack);
            paramp->value_annotation = arrlast(value_stack);
        } else if(kind == AST_KIND_call || kind == AST_KIND_run_directive) {
            //assert(arrlast(type_stack)->kind == TYPE_KIND_PROC);

            bool run_at_compile_time = (kind == AST_KIND_run_directive);

            AST_run_directive *ast_run = NULL;
            AST_call *callp = NULL;

            if(run_at_compile_time) {
                ast_run = (AST_run_directive*)expr[pos];
                callp = ast_run->call_to_run;
            } else {
                callp = (AST_call*)expr[pos];
            }

            bool is_call_expr = (pos == arrlen(expr) - 1);

            Type *proc_type = ((AST_expr_base*)(callp->callee))->type_annotation;
            assert(callp->callee->kind == AST_KIND_atom);
            assert(proc_type->kind == TYPE_KIND_PROC);

            Sym *proc_sym = NULL; //NOTE 10/11/24 this is only used for polymorphic procedure stuff
            assert(callp->callee->kind == AST_KIND_atom);
            proc_sym = ((AST_atom*)(callp->callee))->symbol_annotation;

            bool already_doing_polymorph = false;

            if(proc_type->proc.is_polymorphic) {
                assert(proc_sym->is_polymorphic_procedure);

                already_doing_polymorph = (proc_sym->being_polymorphed_by_jobid == jp->id);

                if(proc_sym->is_being_used_in_polymorph && proc_sym->being_polymorphed_by_jobid != jp->id) {
                    jp->state = JOB_STATE_WAIT;
                    jp->waiting_on_id = proc_sym->being_polymorphed_by_jobid;
                    break;
                }
            }

            if(!callp->checked_call) {
                arrsetlen(type_stack, arrlen(type_stack) - 1);
                arrsetlen(value_stack, arrlen(value_stack) - 1);

                u8 params_passed[proc_type->proc.param.n];
                for(int i = 0; i < proc_type->proc.first_default_param; ++i)
                    params_passed[i] = 0;
                for(int i = proc_type->proc.first_default_param; i < proc_type->proc.param.n; ++i)
                    params_passed[i] = 2;

                assert(arrlen(type_stack) == arrlen(value_stack));

                if(proc_type->proc.has_defaults && callp->n_params < proc_type->proc.first_default_param) {
                    job_error(jp, callp->base.loc, "not enough parameters in call");
                    return;
                } else if(!proc_type->proc.has_defaults && callp->n_params < proc_type->proc.param.n) {
                    job_error(jp, callp->base.loc, "not enough parameters in call");
                    return;
                } else if(!proc_type->proc.varargs && callp->n_params > proc_type->proc.param.n) {
                    job_error(jp, callp->base.loc, "too many parameters in call");
                    return;
                }

                if(callp->has_named_params) assert(proc_type->proc.name != NULL);

                for(AST_param *param_list = callp->params; param_list && param_list->index < proc_type->proc.param.n; param_list = param_list->next) {
                    int i = param_list->index;
                    Type *param_type = param_list->type_annotation;
                    char *param_name = param_list->name;
                    Type *expected_type = proc_type->proc.param.types[i];
                    int param_index = i;

                    assert(param_type == param_list->type_annotation);

                    if(param_name) {
                        for(param_index = 0; param_index < proc_type->proc.param.n; ++param_index) {
                            if(!strcmp(param_name, proc_type->proc.param.names[param_index])) {
                                expected_type = proc_type->proc.param.types[param_index];
                                break;
                            }
                        }

                        if(expected_type == NULL) {
                            //TODO custom formatting for printing locations
                            job_error(jp, callp->base.loc,
                                    "procedure '%s' has no parameter named '%s'", proc_type->proc.name, param_name);
                        }

                        if(params_passed[param_index] == 1) {
                            //TODO custom formatting for printing locations
                            job_error(jp, callp->base.loc,
                                    "parameter '%s' was passed multiple times", param_name);
                        }
                    }

                    params_passed[param_index] = 1;

                    if(proc_type->proc.is_polymorphic && !already_doing_polymorph) {
                        if(proc_type->proc.param.is_polymorphic[param_index] || proc_type->proc.param.has_wildcard[param_index]) {
                            Type *expect = expected_type;
                            Type *have = param_type;
                            Type *new_expected_type = NULL;
                            Type **dest = &new_expected_type;

                            if(expect->kind == TYPE_KIND_POINTER) {
                                if(TYPE_KIND_IS_ARRAY_LIKE(have->kind) && types_are_same(expect->pointer.to, have->array.of)) {
                                    *dest = job_alloc_type(jp, expect->kind);
                                    dest = &((*dest)->pointer.to);
                                    expect = expect->pointer.to;
                                    have = have->array.of;
                                } else if(have->kind == TYPE_KIND_POINTER && types_are_same(expect->pointer.to, have->pointer.to)) {
                                    *dest = job_alloc_type(jp, expect->kind);
                                    dest = &((*dest)->pointer.to);
                                    expect = expect->pointer.to;
                                    have = have->pointer.to;
                                } else {
                                    UNIMPLEMENTED;
                                    //job_error(jp, param_list->base.loc, "cannot pass
                                }
                            } else if(expect->kind == TYPE_KIND_ARRAY_VIEW) {
                                if(TYPE_KIND_IS_ARRAY_LIKE(have->kind) && types_are_same(expect->array.of, have->array.of)) {
                                    *dest = job_alloc_type(jp, expect->kind);
                                    dest = &((*dest)->array.of);
                                    expect = expect->array.of;
                                    have = have->array.of;
                                } else {
                                    UNIMPLEMENTED;
                                    //job_error(jp, param_list->base.loc, "cannot pass
                                }
                            }

                            while(true) {

                                if(expect->kind != TYPE_KIND_WILDCARD && expect->kind != have->kind) {
                                    UNIMPLEMENTED;
                                    //job_error(jp, param_list->base.loc, "cannot pass
                                    break;
                                }

                                if(expect->kind == TYPE_KIND_WILDCARD) {
                                    if(expect->wildcard.matched == NULL) {
                                        assert(proc_type->proc.param.is_polymorphic[param_index]);
                                        expect->wildcard.matched = have;
                                        *dest = have;
                                        break;
                                    } else {
                                        expect = expect->wildcard.matched;
                                        *dest = expect;
                                        break;
                                    }
                                } else if(expect->kind == TYPE_KIND_POINTER) {
                                    *dest = job_alloc_type(jp, expect->kind);
                                    dest = &((*dest)->pointer.to);
                                    expect = expect->pointer.to;
                                    have = have->pointer.to;
                                } else if(TYPE_KIND_IS_ARRAY_LIKE(expect->kind)) {
                                    *dest = job_alloc_type(jp, expect->kind);
                                    **dest = *have;
                                    dest = &((*dest)->array.of);
                                    expect = expect->array.of;
                                    have = have->array.of;
                                } else if(expect->kind == TYPE_KIND_STRUCT) {
                                    UNIMPLEMENTED;
                                } else if(expect->kind == TYPE_KIND_UNION) {
                                    UNIMPLEMENTED;
                                } else if(expect->kind == TYPE_KIND_ENUM) {
                                    UNIMPLEMENTED;
                                } else if(expect->kind == TYPE_KIND_PROC) {
                                    UNIMPLEMENTED;
                                } else {
                                    UNREACHABLE;
                                }

                            }

                            expected_type = new_expected_type;
                        }

                        arrpush(jp->save_polymorphic_proc_param_and_return_types, proc_type->proc.param.types[i]);
                        proc_type->proc.param.types[i] = expected_type;

                    }

                    Type *t = typecheck_operation(jp, expected_type, param_type, '=', NULL, &(param_list->value));
                    param_list->type_annotation = t;

                    if(!t) {
                        job_error(jp, param_list->base.loc,
                                "invalid type '%s' passed to parameter '%s' in call to '%s', expected type '%s'",
                                job_type_to_str(jp, param_type),
                                proc_type->proc.param.names[param_index],
                                proc_type->proc.name ? proc_type->proc.name : job_type_to_str(jp, proc_type),
                                job_type_to_str(jp, expected_type));
                    }

                    if(proc_type->proc.param.names) {
                        param_list->name = proc_type->proc.param.names[param_index];
                    }

                }

                for(int i = 0; i < proc_type->proc.param.n; ++i) {
                    if(params_passed[i] == 0) {
                        assert(proc_type->proc.name != NULL);
                        job_error(jp, callp->base.loc,
                                "missing parameter '%s' in call to '%s'",
                                proc_type->proc.param.names[i], proc_type->proc.name);
                        break;
                    }
                }

                if(run_at_compile_time) {
                    for(int i = arrlen(value_stack) - callp->n_params; i < arrlen(value_stack); ++i) {
                        if(value_stack[i]->kind == VALUE_KIND_NIL) {
                            job_error(jp, callp->base.loc, "parameters to '#run' directive must const evaluate");
                            break;
                        }
                    }
                }

                arrsetlen(type_stack, arrlen(type_stack) - callp->n_params);
                arrsetlen(value_stack, arrlen(value_stack) - callp->n_params);

                if(!is_call_expr && proc_type->proc.ret.n == 0) {
                    job_error(jp, callp->base.loc, "attempted to use void procedure in expression");
                }

                if(jp->state == JOB_STATE_ERROR) return;

                if(proc_type->proc.is_polymorphic && !already_doing_polymorph) {
                    for(u64 i = 0; i < proc_type->proc.ret.n; ++i) {
                        if(proc_type->proc.ret.has_wildcard[i]) {
                            Type *ret_type = proc_type->proc.ret.types[i];
                            Type *new_ret_type = NULL;
                            Type **dest = &new_ret_type;

                            while(true) {
                                if(ret_type->kind == TYPE_KIND_WILDCARD) {
                                    assert(ret_type->wildcard.matched != NULL);
                                    ret_type = ret_type->wildcard.matched;
                                } else if(ret_type->kind == TYPE_KIND_POINTER) {
                                    *dest = job_alloc_type(jp, ret_type->kind);
                                    dest = &((*dest)->pointer.to);
                                    ret_type = ret_type->pointer.to;
                                } else if(TYPE_KIND_IS_ARRAY_LIKE(ret_type->kind)) {
                                    *dest = job_alloc_type(jp, ret_type->kind);
                                    **dest = *ret_type;
                                    dest = &((*dest)->array.of);
                                    ret_type = ret_type->array.of;
                                    //} else if(ret_type->kind == TYPE_KIND_STRUCT) {
                                    //    UNIMPLEMENTED;
                                    //} else if(ret_type->kind == TYPE_KIND_UNION) {
                                    //    UNIMPLEMENTED;
                                    //} else if(ret_type->kind == TYPE_KIND_ENUM) {
                                    //    UNIMPLEMENTED;
                                    //} else if(ret_type->kind == TYPE_KIND_PROC) {
                                    //    UNIMPLEMENTED;
                            } else {
                                *dest = ret_type;
                                break;
                            }

                            }

                            arrpush(jp->save_polymorphic_proc_param_and_return_types, proc_type->proc.ret.types[i]);
                            proc_type->proc.ret.types[i] = new_ret_type;
                        }
                    }
                }

                if(is_call_expr) { /* if the only thing in the expr is a call */
                    callp->n_types_returned = proc_type->proc.ret.n;

                    if(callp->n_types_returned > 0) {
                        callp->type_annotation = proc_type->proc.ret.types[0];
                        callp->type_annotation_list = job_alloc_scratch(jp, sizeof(Type*) * proc_type->proc.ret.n);
                        for(int i = 0; i < proc_type->proc.ret.n; ++i) {
                            callp->type_annotation_list[i] = proc_type->proc.ret.types[i];
                        }
                    }
                } else {
                    callp->n_types_returned = 1;
                    callp->type_annotation = proc_type->proc.ret.types[0];
                    arrpush(type_stack, proc_type->proc.ret.types[0]);
                }

                if(ast_run) {
                    ast_run->n_types_returned = callp->n_types_returned;
                    ast_run->type_annotation = callp->type_annotation;
                    ast_run->type_annotation_list = callp->type_annotation_list;
                }

                callp->checked_call = true;

                if(proc_type->proc.is_polymorphic && !already_doing_polymorph) {
                    /* job_fork() */

                    assert(proc_sym);

                    bool cached = false;

                    for(int i = 0; i < arrlen(proc_sym->polymorph_cache.key); ++i) {
                        Type **key = proc_sym->polymorph_cache.key[i];
                        int procid = proc_sym->polymorph_cache.procid[i];
                        Type **wildcards = proc_type->proc.wildcards;
                        u64 n_wildcards = proc_sym->polymorph_cache.n_wildcards;

                        int matched = 0;

                        for(int j = 0; j < n_wildcards; ++j) {
                            if(types_are_same(wildcards[j]->wildcard.matched, key[j])) {
                                matched++;
                            }
                        }

                        if(matched == proc_sym->polymorph_cache.n_wildcards) {
                            for(int j = 0; j < n_wildcards; ++j) {
                                wildcards[j]->wildcard.matched = NULL;
                            }
                            cached = true;
                            proc_sym->procid = procid;
                            proc_sym->ir_generated = true;
                            break;
                        }
                    }

                    if(!cached) {
                        proc_sym->procid = procid_alloc++;

                        Type **key;
                        if(proc_sym->is_global) {
                            key = global_alloc_scratch(sizeof(Type*) * proc_type->proc.n_wildcards);
                            for(int i = 0; i < proc_type->proc.n_wildcards; ++i) {
                                key[i] = proc_type->proc.wildcards[i]->wildcard.matched;
                            }
                        } else {
                            UNIMPLEMENTED;
                        }

                        arrpush(proc_sym->polymorph_cache.key, key);
                        arrpush(proc_sym->polymorph_cache.procid, proc_sym->procid);

                        Job new_job = job_spawn(&jobid_alloc, PIPE_STAGE_TYPECHECK);

                        new_job.global_sym_allocator = jp->global_sym_allocator;
                        new_job.global_scope = jp->global_scope;

                        new_job.allocator.scratch = malloc(sizeof(Arena));
                        new_job.allocator.value = malloc(sizeof(Pool));
                        new_job.allocator.sym = malloc(sizeof(Pool));
                        new_job.allocator.type = malloc(sizeof(Pool));

                        proc_sym->is_being_used_in_polymorph = true;
                        proc_sym->being_polymorphed_by_jobid = new_job.id;

                        arrpush(new_job.tree_pos_stack, proc_sym->polymorphic_proc_ast);
                        new_job.root = proc_sym->polymorphic_proc_ast;
                        new_job.handling_name = proc_sym->name;
                        new_job.parent_job = jp->id;

                        job_init_allocator_ast(&new_job);
                        job_init_allocator_scratch(&new_job);
                        job_init_allocator_value(&new_job);
                        job_init_allocator_sym(&new_job);
                        job_init_allocator_type(&new_job);

                        //NOTE arrins causes jp to become invalid when the job_queue is resized
                        //     leaving this here just because
                        //arrins(job_queue, job_queue_pos + 1, new_job);
                        arrpush(job_queue_next, new_job);

                        jp->state = JOB_STATE_WAIT;
                        jp->waiting_on_id = new_job.id;
                        jp->waiting_on_name = NULL;

                        break;
                    }
                }

            }

            if(proc_type->proc.is_polymorphic && jp->cur_proc_type != proc_type) {

                proc_sym->is_being_used_in_polymorph = false;
                proc_sym->being_polymorphed_by_jobid = -1;

                assert(callp->callee->kind == AST_KIND_atom);
                AST_atom *callee = (AST_atom*)(callp->callee);
                callee->symbol_annotation = job_alloc_sym(jp);
                *(callee->symbol_annotation) = *proc_sym;

                proc_sym->ir_generated = false;

                u64 save_i = 0;

                for(u64 i = 0; i < proc_type->proc.param.n; ++i) {
                    proc_type->proc.param.types[i] = jp->save_polymorphic_proc_param_and_return_types[save_i];
                    save_i++;
                }

                for(u64 i = 0; i < proc_type->proc.ret.n; ++i) {
                    proc_type->proc.ret.types[i] = jp->save_polymorphic_proc_param_and_return_types[save_i];
                    save_i++;
                }

                arrsetlen(jp->save_polymorphic_proc_param_and_return_types, 0);
            }

            if(run_at_compile_time) {
                AST_call *call_to_run = callp;

                //NOTE make sure procedure has been compiled to IR
                assert(call_to_run->callee->kind == AST_KIND_atom);
                AST_atom *callee = (AST_atom*)(call_to_run->callee);
                assert(callee->symbol_annotation);
                assert(callee->symbol_annotation->name);

                //TODO check if procedure is ready to run by looking up procid in procedure table
                if(callee->symbol_annotation->ready_to_run == false) {
                    if(callee->symbol_annotation->job_encountered_error) {
                        job_error(jp, callp->callee->loc,
                                "'%s' could not compile because of an error",
                                callee->symbol_annotation->name);
                    } else {
                        jp->state = JOB_STATE_WAIT;
                        jp->waiting_on_name = callee->symbol_annotation->name;
                    }
                    break;
                }

                assert(callee->symbol_annotation->procid >= 0);

                jp->label_alloc = 1;
                jp->reg_alloc = 0;
                //arrpush(jp->local_offset, 0);

                jp->cur_run_local_segment_size = 0;
                for(AST_param *p = call_to_run->params; p; p = p->next)
                    jp->cur_run_local_segment_size += p->type_annotation->bytes;
                for(int i = 0; i < proc_type->proc.param.n; ++i)
                    jp->cur_run_local_segment_size += proc_type->proc.param.types[i]->bytes;
                for(int i = 0; i < proc_type->proc.ret.n; ++i)
                    jp->cur_run_local_segment_size += proc_type->proc.ret.types[i]->bytes;
                //printf("jp->cur_run_local_segment_size %lu\n", jp->cur_run_local_segment_size);

                if(global_segment_offset > 0) {
                    jp->interp.global_segment = realloc(jp->interp.global_segment, global_segment_offset);
                }

                if(bss_segment_offset > 0) {
                    jp->interp.bss_segment = realloc(jp->interp.bss_segment, bss_segment_offset);
                    memset(jp->interp.bss_segment, 0, bss_segment_offset);
                }

                if(!jp->interp.local_segment)
                    jp->interp.local_segment = malloc(IR_LOCAL_SEGMENT_BYTES);

                //NOTE leaving this dirty for testing purposes
                //memset(jp->interp.local_segment, 0, 1<<15);

                for(int i = 0; i < arrlen(global_data_table); ++i) {
                    Sym *s = global_data_table[i];
                    Type *t = s->type;
                    Value *v = s->value;

                    /*TODO important refactor needed, read below
                     * 
                     * The separation between Sym, Type and Value is starting to cause some ergonomics problems.
                     * It would be a good idea to try and unify the things that fly around in the language in to
                     * one Entity struct, like in a game engine.
                     */

                    serialize_value(jp, jp->interp.global_segment + s->segment_offset, v, t);
                }

                arrsetlen(jp->interp.ports, 16);
                for(int i = 0; i < 16; ++i) jp->interp.ports[i] = (IRvalue){0};

                arrpush(jp->local_offset, arrlast(jp->local_offset));
                assert(arrlast(jp->local_offset) == 0);

                /* ir_gen_entry_point_preamble */ {
                    IRinst inst;

                    inst =
                        (IRinst) {
                            .opcode = IROP_ADDRVAR,
                            .addrvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = 8,
                                .reg_dest = 0,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = 0,
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    ir_gen_struct_init(jp, type_Context, IRSEG_LOCAL, 8);

                    u64 context_offset = 8;

                    arrlast(jp->local_offset) = 8 + type_Context->bytes;

                    inst =
                        (IRinst) {
                            .opcode = IROP_ADDRVAR,
                            .addrvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = arrlast(jp->local_offset),
                                .reg_dest = 0,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    u64 temporary_storage_pointer_offset = context_offset + offsetof(Context, temporary_storage);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = temporary_storage_pointer_offset,
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), _Alignof(Temporary_storage));
                    u64 temporary_storage_offset = arrlast(jp->local_offset);

                    ir_gen_struct_init(jp, type_Temporary_storage, IRSEG_LOCAL, temporary_storage_offset);

                    arrlast(jp->local_offset) += type_Temporary_storage->bytes;
                    arrlast(jp->local_offset) = align_up(arrlast(jp->local_offset), 8);

                    //TODO __DEFAULT_TEMPORARY_STORAGE_SIZE : u32 : 4096;

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = temporary_storage_offset + offsetof(Temporary_storage, size),
                                .imm.integer = 4096,
                                .bytes = 8,
                                .immediate = true,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_ADDRVAR,
                            .addrvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = arrlast(jp->local_offset),
                                .reg_dest = 0,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    inst =
                        (IRinst) {
                            .opcode = IROP_SETVAR,
                            .setvar = {
                                .segment = IRSEG_LOCAL,
                                .offset = temporary_storage_offset + offsetof(Temporary_storage, data),
                                .reg_src = 0,
                                .bytes = 8,
                            },
                        };
                    arrpush(jp->instructions, inst);

                    arrlast(jp->local_offset) += 4096;

                }

                ir_gen_expr(jp, (AST*)call_to_run);

                if(arrlast(jp->local_offset) > jp->max_local_offset) jp->max_local_offset = arrlast(jp->local_offset);
                arrsetlen(jp->local_offset, arrlen(jp->local_offset) - 1);

                jp->cur_run_local_segment_size = jp->max_local_offset;
                IRinst ret_inst = { .opcode = IROP_RET };
                arrpush(jp->instructions, ret_inst);

                printf("banana cakes\n");
                for(int i = 0; i < arrlen(jp->instructions); ++i) {
                    printf("%i: ", i);
                    print_ir_inst(jp->instructions[i], stdout);
                }

                assert(arrlast(jp->local_offset) == 0);
                //arrsetlen(jp->local_offset, 0);
                jp->reg_alloc = 0;
                jp->label_alloc = 0;

                IRproc run_directive_tmp_procedure = {
                    .procid = -1,
                    .instructions = jp->instructions,
                    .n_instructions = arrlen(jp->instructions),
                    .local_segment_size = jp->cur_run_local_segment_size,
                };
                proc_table_add(-1, run_directive_tmp_procedure);

                printf("running '%s'\n", proc_type->proc.name);
                ir_run(jp, -1);

                jp->cur_run_local_segment_size = 0;

                arrsetlen(jp->instructions, 0);

                assert(arrlen(jp->interp.ports) >= proc_type->proc.ret.n);

                Value* return_value_array[proc_type->proc.ret.n];

                for(u64 i = 0; i < proc_type->proc.ret.n; ++i) {
                    Type *t = proc_type->proc.ret.types[i];
                    Value *v;

                    if(t->kind == TYPE_KIND_ARRAY) {
                        v = make_empty_array_value(jp, t);
                    } else {
                        v = job_alloc_value(jp, VALUE_KIND_NIL);
                    }

                    switch(t->kind) {
                        default:
                            UNREACHABLE;
                        case TYPE_KIND_ARRAY:
                            copy_array_data_to_value(jp, v, t, (u8*)(void*)(jp->interp.ports[i].integer));
                            break;
                        case TYPE_KIND_BOOL:
                            v->kind = VALUE_KIND_BOOL;
                            v->val.boolean = (bool)(jp->interp.ports[i].integer);
                            break;
                        case TYPE_KIND_CHAR:
                            v->kind = VALUE_KIND_CHAR;
                            v->val.character = (char)(jp->interp.ports[i].integer);
                            break;
                        case TYPE_KIND_S8:
                        case TYPE_KIND_S16:
                        case TYPE_KIND_S32:
                        case TYPE_KIND_S64:
                        case TYPE_KIND_INT:
                            v->kind = VALUE_KIND_INT;
                            v->val.integer = (s64)(jp->interp.ports[i].integer);
                            printf("~~~~~~ %li\n", v->val.integer);
                            break;
                        case TYPE_KIND_U8:
                        case TYPE_KIND_U16:
                        case TYPE_KIND_U32:
                        case TYPE_KIND_U64:
                            v->kind = VALUE_KIND_UINT;
                            v->val.uinteger = (u64)(jp->interp.ports[i].integer);
                            printf("~~~~~~ %lu\n", v->val.integer);
                            break;
                        case TYPE_KIND_FLOAT:
                        case TYPE_KIND_F32:
                            v->kind = VALUE_KIND_FLOAT;
                            v->val.floating = jp->interp.ports[i].floating32;
                            printf("~~~~~~ %f\n", v->val.floating);
                            break;
                        case TYPE_KIND_F64:
                            v->kind = VALUE_KIND_DFLOAT;
                            v->val.dfloating = jp->interp.ports[i].floating64;
                            printf("~~~~~~ %f\n", v->val.dfloating);
                            break;
                    }

                    return_value_array[i] = v;
                }

                if(proc_type->proc.ret.n > 0) {
                    if(is_call_expr) {
                        callp->value_annotation_list = job_alloc_scratch(jp, sizeof(Value*) * proc_type->proc.ret.n);
                        for(int i = 0; i < proc_type->proc.ret.n; ++i) {
                            callp->value_annotation_list[i] = return_value_array[i];
                        }
                        callp->value_annotation = return_value_array[0];
                    } else {
                        callp->value_annotation = return_value_array[0];
                        arrpush(value_stack, return_value_array[0]);
                    }

                    if(ast_run) {
                        ast_run->value_annotation = callp->value_annotation;
                        ast_run->value_annotation_list = callp->value_annotation_list;
                    }
                }

                expr[pos] = (AST*)callp;

            } else if(!is_call_expr) {
                arrpush(value_stack, builtin_value+VALUE_KIND_NIL);
            }

        } else if(kind == AST_KIND_proctype) {
            //TODO allow names to be given to the parameters in the proctype
            //NOTE I think this and other features should be added after we do an
            //     overhaul of the compiler pipeline to unify types, ASTs, syms and values
            //

            AST_proctype *ast_proctype = (AST_proctype*)(expr[pos]);
            Type *proc_type = job_alloc_type(jp, TYPE_KIND_PROC);

            proc_type->proc.param.types = job_alloc_scratch(jp, sizeof(Type*) * ast_proctype->n_params);
            proc_type->proc.ret.types   = job_alloc_scratch(jp, sizeof(Type*) * ast_proctype->n_rets);
            proc_type->proc.param.n     = ast_proctype->n_params;
            proc_type->proc.ret.n       = ast_proctype->n_rets;

            for(int i = ast_proctype->n_rets - 1; i >= 0; --i) {
                Type *t  = arrpop(type_stack);
                Value *v = arrpop(value_stack);

                assert(t->kind == TYPE_KIND_TYPE);

                proc_type->proc.ret.types[i] = v->val.type;
            }

            for(int i = ast_proctype->n_params - 1; i >= 0; --i) {
                Type *t  = arrpop(type_stack);
                Value *v = arrpop(value_stack);

                assert(t->kind == TYPE_KIND_TYPE);

                proc_type->proc.param.types[i] = v->val.type;
            }

            Type *tpush = job_alloc_type(jp, TYPE_KIND_TYPE);
            Value *vpush = job_alloc_value(jp, VALUE_KIND_TYPE);
            vpush->val.type = proc_type;

            ast_proctype->type_annotation = tpush;
            ast_proctype->value_annotation = vpush;

            arrpush(type_stack, tpush);
            arrpush(value_stack, vpush);
        } else if(AST_KIND_IS_RECORD(kind)) {
            AST_structdecl *ast_struct = (AST_structdecl*)(expr[pos]);

            if(ast_struct->record_type) {
                Value *v = job_alloc_value(jp, VALUE_KIND_TYPE);
                v->val.type = ast_struct->record_type;
                arrpush(type_stack, ast_struct->record_type);
                arrpush(value_stack, v);
            } else {
                /* job_fork() */

                Job new_job = job_spawn(&jobid_alloc, PIPE_STAGE_TYPECHECK);

                new_job.allocator = jp->allocator;
                new_job.global_sym_allocator = jp->global_sym_allocator;
                new_job.global_scope = jp->global_scope;
                new_job.dont_free_allocators = true;
                new_job.dont_free_ast_allocators = true;

                new_job.root = expr[pos];
                arrpush(new_job.tree_pos_stack, expr[pos]);

                arrpush(job_queue, new_job);

                jp->state = JOB_STATE_WAIT;
                jp->waiting_on_id = new_job.id;
                jp->waiting_on_name = NULL;

                break;
            }
        } else {
            UNIMPLEMENTED;
        }
    }

    if(jp->state == JOB_STATE_READY) {
        AST *expr_root = arrlast(jp->expr);
        add_implicit_casts_to_expr(jp, expr_root);
    }

    jp->value_stack = value_stack;
    jp->type_stack = type_stack;
    jp->expr = expr;
    jp->expr_pos = pos;
}

void linearize_expr(Job *jp, AST *ast) {
    if(!ast) return;

    if(ast->kind == AST_KIND_expr) {
        AST_expr *expr = (AST_expr*)ast;
        linearize_expr(jp, expr->left);
        if(expr->token != '.') linearize_expr(jp, expr->right);
        arrpush(jp->expr, ast);
    } else if(ast->kind == AST_KIND_atom) {
        arrpush(jp->expr, ast);
    } else if(ast->kind == AST_KIND_array_literal) {
        AST_array_literal *array_lit = (AST_array_literal*)ast;

        Arr(AST_expr_list*) array_elem_list_stack = NULL;
        Arr(bool) elems_are_array_literals_stack = NULL;

        arrpush(array_elem_list_stack, array_lit->elements);
        arrpush(elems_are_array_literals_stack, (array_lit->elements->expr->kind == AST_KIND_array_literal));

        arrpush(array_lit->dimensions, array_lit->n_elements);

        bool encountered_error = false;

        int depth = 1;

        while(arrlen(array_elem_list_stack) > 0) {
            AST_expr_list *e = arrpop(array_elem_list_stack);
            bool elems_are_array_literals = arrpop(elems_are_array_literals_stack);

            for(; e; e = e->next) {
                bool e_is_array_lit = (e->expr->kind == AST_KIND_array_literal);

                //TODO should this be allowed in the case of elements which are themselves arrays?
                if(elems_are_array_literals != e_is_array_lit) {
                    job_error(jp, array_lit->base.loc,
                            "array literals and scalars cannot both be elements of the same array or sub-array");
                    encountered_error = true;
                    break;
                }

                if(e_is_array_lit) {
                    AST_array_literal *a = (AST_array_literal*)(e->expr);

                    if(e->next) {
                        arrpush(array_elem_list_stack, e->next);
                        arrpush(elems_are_array_literals_stack, (e->next->expr->kind == AST_KIND_array_literal));
                    }

                    arrpush(array_elem_list_stack, a->elements);
                    arrpush(elems_are_array_literals_stack, (a->elements->expr->kind == AST_KIND_array_literal));

                    if(depth >= arrlen(array_lit->dimensions))
                        arrpush(array_lit->dimensions, 0);

                    if(a->n_elements > array_lit->dimensions[depth])
                        array_lit->dimensions[depth] = a->n_elements;

                    depth++;

                    break;
                } else {
                    arrpush(array_lit->flattened_array_literal, &(e->expr));
                    linearize_expr(jp, e->expr);
                }
            }

            if(!e) depth--;

            if(encountered_error) break;

        }

        arrfree(array_elem_list_stack);
        arrfree(elems_are_array_literals_stack);

        if(encountered_error) return;

        if(array_lit->type)
            linearize_expr(jp, array_lit->type);

        arrpush(jp->expr, ast);
    } else if(ast->kind == AST_KIND_param) {
        AST_param *param = (AST_param*)ast;
        linearize_expr(jp, param->value);
        arrpush(jp->expr, ast);
        linearize_expr(jp, (AST*)param->next);
    } else if(ast->kind == AST_KIND_call || ast->kind == AST_KIND_run_directive) {
        AST_call *callp;
        if(ast->kind == AST_KIND_run_directive) {
            AST_run_directive *ast_run = (AST_run_directive*)ast;
            callp = ast_run->call_to_run;
        } else {
            callp = (AST_call*)ast;
        }
        linearize_expr(jp, (AST*)(callp->params));
        linearize_expr(jp, callp->callee);
        arrpush(jp->expr, ast);
    } else if(ast->kind == AST_KIND_proctype) {
        AST_proctype *proctype = (AST_proctype*)ast;

        AST_expr_list *list = proctype->params;

        while(list) {
            linearize_expr(jp, list->expr);
            list = list->next;
        }

        list = proctype->rets;

        while(list) {
            linearize_expr(jp, list->expr);
            list = list->next;
        }

        arrpush(jp->expr, ast);
    } else if(AST_KIND_IS_RECORD(ast->kind)) {
        arrpush(jp->expr, ast);
    } else {
        UNIMPLEMENTED;
    }
}

Arr(AST*) ir_linearize_expr(Arr(AST*) ir_expr, AST *ast) {
    if(!ast) return ir_expr;

    if(ast->kind == AST_KIND_expr) {
        AST_expr *expr = (AST_expr*)ast;

        if(expr->value_annotation && expr->value_annotation->kind != VALUE_KIND_NIL) {
            arrpush(ir_expr, ast);
            return ir_expr;
        }

        if(expr->token == TOKEN_AND || expr->token == TOKEN_OR || expr->token == '!') {
            arrpush(ir_expr, ast);
            return ir_expr;
        }

        if(expr->token == '@') {
            assert(expr->right && !expr->left);

            //TODO don't edit the tree in this way, it makes the edit annoying to reverse
            if(expr->right->kind == AST_KIND_atom) {
                AST_atom *atom = (AST_atom*)(expr->right);
                assert(atom->token == TOKEN_IDENT || atom->token == TOKEN_CONTEXT);
                atom->token = '@';
                arrpush(ir_expr, expr->right);
                return ir_expr;
            }

            AST_expr *addr_operand = (AST_expr*)(expr->right);

            if(addr_operand->token == '>') {
                assert(addr_operand->right && !addr_operand->left);
                return ir_linearize_expr(ir_expr, addr_operand->right);
            }

            if(addr_operand->token == '.') {
                arrpush(ir_expr, ast);
                return ir_expr;
            }

            assert(addr_operand->token == '[');

            if(addr_operand->left && addr_operand->right && addr_operand->left->weight >= addr_operand->right->weight) {
                ir_expr = ir_linearize_expr(ir_expr, addr_operand->left);
                ir_expr = ir_linearize_expr(ir_expr, addr_operand->right);
            } else {
                ir_expr = ir_linearize_expr(ir_expr, addr_operand->right);
                ir_expr = ir_linearize_expr(ir_expr, addr_operand->left);
            }

            addr_operand->token = '@';

            arrpush(ir_expr, expr->right);
            return ir_expr;
        }

        if(expr->token == '.') {
            ir_expr = ir_linearize_expr(ir_expr, expr->left);
            arrpush(ir_expr, ast);
            return ir_expr;
        }

        if(expr->left && expr->right && expr->left->weight >= expr->right->weight) {
            ir_expr = ir_linearize_expr(ir_expr, expr->left);
            ir_expr = ir_linearize_expr(ir_expr, expr->right);
        } else {
            ir_expr = ir_linearize_expr(ir_expr, expr->right);
            ir_expr = ir_linearize_expr(ir_expr, expr->left);
        }
        arrpush(ir_expr, ast);
    } else if(ast->kind == AST_KIND_atom) {
        arrpush(ir_expr, ast);
    } else if(ast->kind == AST_KIND_array_literal) {
        arrpush(ir_expr, ast);
    } else if(ast->kind == AST_KIND_call) {
        arrpush(ir_expr, ast);
    } else if(ast->kind == AST_KIND_run_directive) {
        arrpush(ir_expr, (AST*)(((AST_run_directive*)ast)->call_to_run));
    } else {
        UNIMPLEMENTED;
    }

    return ir_expr;
}

//TODO use global allocators when inside typecheck_structdecl()
void typecheck_structdecl(Job *jp) {
    AST_structdecl *ast = (AST_structdecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_structdecl || ast->base.kind == AST_KIND_uniondecl);

    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);
    bool is_union = (ast->base.kind == AST_KIND_uniondecl);
    bool has_params = (ast->params != NULL);
    bool is_nested = (arrlen(jp->record_types) > 1);

    if(is_top_level && jp->handling_name == NULL)
        jp->handling_name = ast->name;

    if(has_params && is_nested) {
        char *s = is_union ? "union" : "struct";
        job_error(jp, ast->base.loc,
                "nested %s's are part of the outer %s, they cannot have parameters", s, s);
        return;
    }

    arrlast(jp->record_types) = job_alloc_type(jp, is_union ? TYPE_KIND_UNION : TYPE_KIND_STRUCT);

    arrlast(jp->record_types)->record.name = ast->name;
    arrlast(jp->record_types)->record.is_nested = is_nested;

    {
        u64 n = ast->n_members;
        arrlast(jp->record_types)->record.member.n = n;
        //arrsetlen(arrlast(jp->record_types)->record.member.types, n);
        //arrsetlen(arrlast(jp->record_types)->record.member.names, n);
        //arrsetlen(arrlast(jp->record_types)->record.member.values, n);
        //arrsetlen(arrlast(jp->record_types)->record.member.offsets, n);
        //arrsetlen(arrlast(jp->record_types)->record.member.locs, n);
        arrlast(jp->record_types)->record.member.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
        arrlast(jp->record_types)->record.member.names = (char**)job_alloc_scratch(jp, sizeof(char*) * n);
        arrlast(jp->record_types)->record.member.values = (Value**)job_alloc_scratch(jp, sizeof(Value*) * n);
        arrlast(jp->record_types)->record.member.offsets = (u64*)job_alloc_scratch(jp, sizeof(u64) * n);
        arrlast(jp->record_types)->record.member.locs = (Loc_info*)job_alloc_scratch(jp, sizeof(Loc_info) * n);
    }

    {
        u64 n = ast->n_using;
        arrlast(jp->record_types)->record.use.n = n;
        //arrsetlen(arrlast(jp->record_types)->record.use.types, n);
        //arrsetlen(arrlast(jp->record_types)->record.use.offsets, n);
        arrlast(jp->record_types)->record.use.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
        arrlast(jp->record_types)->record.use.offsets = (u64*)job_alloc_scratch(jp, sizeof(u64) * n);
    }

    ast->record_type = arrlast(jp->record_types);
    Type *record_type = arrlast(jp->record_types);

    if(is_nested) {
        return;
    }

    //NOTE copypasta from typecheck_procdecl()
    if(has_params && ast->checked_params == false) {
        u64 n = record_type->record.param.n;

        if(n == 0) {
            jp->cur_paramdecl = ast->params;
            n = record_type->record.param.n = ast->n_params;
            record_type->record.param.names = (char**)job_alloc_scratch(jp, sizeof(char*) * n);
            record_type->record.param.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
            record_type->record.param.values = (Value**)job_alloc_scratch(jp, sizeof(Value*) * n);
        }

        for(AST_paramdecl *p = jp->cur_paramdecl; p; p = p->next) {
            record_type->record.param.names[p->index] = p->name;

            bool initialize = (p->init != NULL);

            if(p->checked_type == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->type);
                }
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    return;
                }

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_type = true;
            }

            if(initialize && p->checked_init == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->init);
                }
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    return;
                }

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_init = true;
            }

            if(jp->state == JOB_STATE_ERROR) return;

            Type *bind_type = ((AST_expr*)(p->type))->value_annotation->val.type;
            Value *init_value = NULL;
            Type *init_type = NULL;

            if(initialize) {
                init_value = ((AST_expr*)(p->init))->value_annotation;
                init_type = ((AST_expr*)(p->init))->type_annotation;

                if(init_value->kind == VALUE_KIND_NIL)
                    job_error(jp, p->base.loc,
                            "parameter default values must evaluate at compile time");

                Type *t = typecheck_operation(jp, bind_type, init_type, '=', NULL, &(p->init));

                if(!t) {
                    job_error(jp, p->base.loc,
                            "invalid assignment of '%s' to parameter of type '%s'",
                            job_type_to_str(jp, init_type), job_type_to_str(jp, bind_type));
                }

                if(jp->state == JOB_STATE_ERROR) return;

                bind_type = t;
            }

            Sym *ptr = job_current_scope_lookup(jp, p->name);

            if(ptr) {
                job_error(jp, p->base.loc,
                        "multiple declaration of parameter '%s' in '%s'",
                        p->name, ast->name, ptr->loc.line);
                return;
            }

            Sym sym = {
                .name = p->name,
                .loc = p->base.loc,
                .constant = true,
                .declared_by = jp->id,
                .is_argument = true,
                .type = bind_type,
                .value = init_value,
                .initializer = p->init,
            };

            Sym *symp = job_alloc_sym(jp);
            *symp = sym;
            job_scope_enter(jp, symp);

            record_type->record.param.types[p->index] = bind_type;
            record_type->record.param.values[p->index] = init_value;

            p->symbol_annotation = symp;
        }
    }

    record_type->record.first_default_param = ast->first_default_param;
    record_type->record.has_defaults = ast->has_defaults;

    //assert(ast->name);

    if(ast->name) {
        Value *sym_value = job_alloc_value(jp, VALUE_KIND_TYPE);
        sym_value->val.type = record_type;

        Sym record_sym = {
            .name = ast->name,
            .loc = ast->base.loc,
            .declared_by = jp->id,
            .constant = true,
            .type = builtin_type+TYPE_KIND_TYPE, //TODO type can't be builtin if we have the .what
            .value = sym_value,
        };

        Sym *ptr = is_top_level ? global_scope_lookup(jp, ast->name) : job_current_scope_lookup(jp, ast->name);

        if(ptr) {
            job_error(jp, ast->base.loc,
                    "multiple declaration of identifier '%s', previously declared at line %i",
                    ast->name, ptr->loc.line);
            return;
        }

        arrlast(jp->record_types) = record_type;
        Sym *symp = job_alloc_global_sym(jp);

        *symp = record_sym;
        ast->symbol_annotation = symp;
    }
}

void typecheck_polymorphic_procdecl(Job *jp) {
    //NOTE copypasta of typecheck_procdecl()
    AST_procdecl *ast = (AST_procdecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_procdecl);

    assert(ast->is_polymorphic);

    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);

    if(is_top_level && jp->handling_name == NULL)
        jp->handling_name = ast->name;

    Arena *save_scratch = jp->allocator.scratch;
    Pool *save_sym = jp->allocator.sym;
    Pool *save_type = jp->allocator.type;
    Pool *save_value = jp->allocator.value;

    if(is_top_level) {
        jp->allocator.scratch = &global_scratch_allocator;
        //jp->allocator.sym = &global_sym_allocator;
        jp->allocator.type = &global_type_allocator;
        jp->allocator.value = &global_value_allocator;
    }

    if(ast->proc_type == NULL) {
        ast->proc_type = job_alloc_type(jp, TYPE_KIND_PROC);
        ast->proc_type->proc.name = ast->name;
        ast->proc_type->proc.is_polymorphic = true;
    }
    Type *proc_type = ast->proc_type;
    jp->cur_proc_type = proc_type;

    if(proc_type->proc.wildcards == NULL) {
        proc_type->proc.n_wildcards = ast->n_wildcards;
        proc_type->proc.wildcards = (Type**)job_alloc_scratch(jp, sizeof(Type*) * ast->n_wildcards);
    }

    //NOTE copypasta of typecheck_procdecl()
    if(ast->checked_params == false) {
        u64 n = proc_type->proc.param.n;

        if(n == 0) {
            jp->cur_paramdecl = ast->params;
            n = proc_type->proc.param.n = ast->n_params;
            proc_type->proc.param.names = (char**)job_alloc_scratch(jp, sizeof(char*) * n);
            proc_type->proc.param.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
            proc_type->proc.param.values = (Value**)job_alloc_scratch(jp, sizeof(Value*) * n);
            proc_type->proc.param.is_polymorphic = (bool*)job_alloc_scratch(jp, sizeof(bool) * n);
            proc_type->proc.param.has_wildcard = (bool*)job_alloc_scratch(jp, sizeof(bool) * n);
        }

        for(AST_paramdecl *p = jp->cur_paramdecl; p; p = p->next) {
            if(p->vararg) {
                proc_type->proc.param.vararg_name = p->name;
                if(!ast->c_call) {
                    Type *varargs_type = job_alloc_type(jp, TYPE_KIND_ARRAY_VIEW);
                    varargs_type->array.of = type_Any;

                    Sym *ptr = job_current_scope_lookup(jp, p->name);

                    if(ptr) {
                        job_error(jp, p->base.loc,
                                "multiple declaration of parameter '%s' in header of '%s'",
                                p->name, ast->name, ptr->loc.line);
                        jp->allocator.scratch = save_scratch;
                        jp->allocator.sym = save_sym;
                        jp->allocator.type = save_type;
                        jp->allocator.value = save_value;
                        return;
                    }

                    Sym sym = {
                        .name = p->name,
                        .loc = p->base.loc,
                        .declared_by = jp->id,
                        .is_argument = true,
                        .type = varargs_type,
                        .value = NULL,
                        .initializer = p->init,
                    };

                    Sym *symp = job_alloc_sym(jp);
                    *symp = sym;
                    job_scope_enter(jp, symp);
                    p->symbol_annotation = symp;
                }
                break;
            }

            proc_type->proc.param.names[p->index] = p->name;

            bool initialize = (p->init != NULL);

            if(p->is_polymorphic && initialize) {
                job_error(jp, p->base.loc, "cannot have default value for a parameter whose type depends on a polymorphic variable");
                continue;
            }

            if(p->checked_type == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->type);
                }
                typecheck_expr(jp);

                if(jp->typechecker_encountered_wildcard) {
                    if(initialize) {
                        job_error(jp, p->base.loc, "cannot have default value for a parameter whose type depends on a polymorphic variable");
                    }
                    p->type_has_wildcard = true;
                    proc_type->proc.param.has_wildcard[p->index] = true;
                    jp->typechecker_encountered_wildcard = false;
                }

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR)
                    UNIMPLEMENTED;

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_type = true;
            }

            if(initialize && p->checked_init == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->init);
                }
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR)
                    UNIMPLEMENTED;

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_init = true;
            }

            Type *bind_type = ((AST_expr*)(p->type))->value_annotation->val.type;
            Value *init_value = NULL;
            Type *init_type = NULL;

            if(initialize) {
                init_value = ((AST_expr*)(p->init))->value_annotation;
                init_type = ((AST_expr*)(p->init))->type_annotation;

                if(init_value->kind == VALUE_KIND_NIL)
                    job_error(jp, p->base.loc,
                            "parameter default values must evaluate at compile time");

                Type *t = typecheck_operation(jp, bind_type, init_type, '=', NULL, &(p->init));

                if(!t) {
                    job_error(jp, p->base.loc,
                            "invalid assignment of '%s' to parameter of type '%s'",
                            job_type_to_str(jp, init_type), job_type_to_str(jp, bind_type));
                }

                if(jp->state == JOB_STATE_ERROR) {
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                bind_type = t;
            }

            Sym *ptr = job_current_scope_lookup(jp, p->name);

            if(ptr) {
                job_error(jp, p->base.loc,
                        "multiple declaration of parameter '%s' in header of '%s'",
                        p->name, ast->name, ptr->loc.line);
                jp->allocator.scratch = save_scratch;
                jp->allocator.sym = save_sym;
                jp->allocator.type = save_type;
                jp->allocator.value = save_value;
                return;
            }

            Sym sym = {
                .name = p->name,
                .loc = p->base.loc,
                .declared_by = jp->id,
                .is_argument = true,
                .type = bind_type,
                .value = init_value,
                .initializer = p->init,
            };

            Sym *symp = job_alloc_sym(jp);
            *symp = sym;
            job_scope_enter(jp, symp);

            proc_type->proc.param.types[p->index] = bind_type;
            proc_type->proc.param.values[p->index] = init_value;

            proc_type->proc.param.is_polymorphic[p->index] = p->is_polymorphic;

            p->symbol_annotation = symp;
        }

        ast->checked_params = true;
    }

    //NOTE copypasta of typecheck_procdecl()
    if(ast->checked_rets == false) {
        u64 n = proc_type->proc.ret.n;

        if(n == 0) {
            jp->cur_retdecl = ast->rets;
            n = proc_type->proc.ret.n = ast->n_rets;
            proc_type->proc.ret.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
            proc_type->proc.ret.has_wildcard = (bool*)job_alloc_scratch(jp, sizeof(bool) * n);
        }

        for(AST_retdecl *r = jp->cur_retdecl; r; r = r->next) {
            if(arrlen(jp->expr) == 0) {
                linearize_expr(jp, r->expr);
            }
            typecheck_expr(jp);

            if(jp->state == JOB_STATE_WAIT) {
                jp->cur_retdecl = r;
                jp->allocator.scratch = save_scratch;
                jp->allocator.sym = save_sym;
                jp->allocator.type = save_type;
                jp->allocator.value = save_value;
                return;
            }

            if(jp->state == JOB_STATE_ERROR)
                UNIMPLEMENTED;

            if(jp->typechecker_encountered_wildcard) {
                r->type_has_wildcard = true;
                proc_type->proc.ret.has_wildcard[r->index] = true;
                jp->typechecker_encountered_wildcard = false;
            }

            arrsetlen(jp->type_stack, 0);
            arrsetlen(jp->value_stack, 0);
            arrsetlen(jp->expr, 0);
            jp->expr_pos = 0;

            Type *ret_type;

            if(((AST_expr_base*)(r->expr))->type_annotation->kind == TYPE_KIND_TYPE) {
                ret_type = ((AST_expr*)(r->expr))->value_annotation->val.type;
            } else if(((AST_expr_base*)(r->expr))->type_annotation->kind == TYPE_KIND_WILDCARD) {
                ret_type = ((AST_expr*)(r->expr))->type_annotation;
            } else {
                UNREACHABLE;
            }

            proc_type->proc.ret.types[r->index] = ret_type;
        }

        ast->checked_rets = true;
    }

    if(ast->n_rets > 0 && ast->body && !all_paths_return(jp, ast->body)) {
        job_error(jp, jp->non_returning_path_loc,
                "code path terminates with no return in procedure '%s'",
                ast->name);
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
        return;
    }

    assert(!ast->is_foreign && !ast->is_system && !ast->c_call);

    //NOTE copypasta of typecheck_procdecl()
    proc_type->proc.first_default_param = ast->first_default_param;
    proc_type->proc.varargs = ast->varargs;
    proc_type->proc.has_defaults = ast->has_defaults;

    Sym proc_sym = {
        .name = ast->name,
        .loc = ast->base.loc,
        .declared_by = jp->id,
        .constant = true,
        .procid = -1,
        .polymorphic_proc_ast = (AST*)ast,
        .is_foreign = ast->is_foreign,
        .is_system = ast->is_system,
        .is_polymorphic_procedure = true,
        .type = proc_type,
        .polymorph_cache.n_wildcards = proc_type->proc.n_wildcards,
    };

    Sym *ptr = is_top_level ? global_scope_lookup(jp, ast->name) : job_current_scope_lookup(jp, ast->name);

    if(ptr) {
        job_error(jp, ast->base.loc,
                "multiple declaration of identifier '%s', previously declared at line %i",
                ast->name, ptr->loc.line);
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
        return;
    }

    jp->cur_proc_type = proc_type;
    //TODO if(is_top_level) { // local procedures
    Sym *symp = job_alloc_global_sym(jp);

    ast->symbol_annotation = symp;

    if(is_top_level) {
        *symp = proc_sym;
        global_scope_enter(jp, symp);
    } else {
        *symp = proc_sym;
        job_scope_enter(jp, symp);
    }

    if(is_top_level) {
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
    }

    ast->polymorphic_params_ready = true;

}

void typecheck_procdecl(Job *jp) {
    AST_procdecl *ast = (AST_procdecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_procdecl);

    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);

    Arena *save_scratch = jp->allocator.scratch;
    Pool *save_sym = jp->allocator.sym;
    Pool *save_type = jp->allocator.type;
    Pool *save_value = jp->allocator.value;

    if(is_top_level) {
        jp->allocator.scratch = &global_scratch_allocator;
        //jp->allocator.sym = &global_sym_allocator;
        jp->allocator.type = &global_type_allocator;
        jp->allocator.value = &global_value_allocator;
    }

    if(is_top_level && jp->handling_name == NULL)
        jp->handling_name = ast->name;

    if(ast->proc_type == NULL) {
        ast->proc_type = job_alloc_type(jp, TYPE_KIND_PROC);
        ast->proc_type->proc.name = ast->name;
    }
    Type *proc_type = ast->proc_type;

    if(ast->checked_params == false) {
        u64 n = proc_type->proc.param.n;

        if(n == 0) {
            jp->cur_paramdecl = ast->params;
            n = proc_type->proc.param.n = ast->n_params;
            proc_type->proc.param.names = (char**)job_alloc_scratch(jp, sizeof(char*) * n);
            proc_type->proc.param.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
            proc_type->proc.param.values = (Value**)job_alloc_scratch(jp, sizeof(Value*) * n);
        }

        for(AST_paramdecl *p = jp->cur_paramdecl; p; p = p->next) {
            if(p->vararg) {
                proc_type->proc.param.vararg_name = p->name;
                if(!ast->c_call) {
                    Type *varargs_type = job_alloc_type(jp, TYPE_KIND_ARRAY_VIEW);
                    varargs_type->array.of = type_Any;

                    Sym *ptr = job_current_scope_lookup(jp, p->name);

                    if(ptr) {
                        job_error(jp, p->base.loc,
                                "multiple declaration of parameter '%s' in header of '%s'",
                                p->name, ast->name, ptr->loc.line);
                        jp->allocator.scratch = save_scratch;
                        jp->allocator.sym = save_sym;
                        jp->allocator.type = save_type;
                        jp->allocator.value = save_value;
                        return;
                    }

                    Sym sym = {
                        .name = p->name,
                        .loc = p->base.loc,
                        .declared_by = jp->id,
                        .is_argument = true,
                        .type = varargs_type,
                        .value = NULL,
                        .initializer = p->init,
                    };

                    Sym *symp = job_alloc_sym(jp);
                    *symp = sym;
                    job_scope_enter(jp, symp);
                    p->symbol_annotation = symp;
                }
                break;
            }

            proc_type->proc.param.names[p->index] = p->name;

            bool initialize = (p->init != NULL);

            if(p->checked_type == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->type);
                }
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR) {
                    UNIMPLEMENTED;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_type = true;
            }

            if(initialize && p->checked_init == false) {
                if(arrlen(jp->expr) == 0) {
                    linearize_expr(jp, p->init);
                }
                typecheck_expr(jp);

                if(jp->state == JOB_STATE_WAIT) {
                    jp->cur_paramdecl = p;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                if(jp->state == JOB_STATE_ERROR) {
                    UNIMPLEMENTED;
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                arrsetlen(jp->type_stack, 0);
                arrsetlen(jp->value_stack, 0);
                arrsetlen(jp->expr, 0);
                jp->expr_pos = 0;

                p->checked_init = true;
            }

            Type *bind_type = ((AST_expr*)(p->type))->value_annotation->val.type;
            Value *init_value = NULL;
            Type *init_type = NULL;

            if(initialize) {
                init_value = ((AST_expr*)(p->init))->value_annotation;
                init_type = ((AST_expr*)(p->init))->type_annotation;

                if(init_value->kind == VALUE_KIND_NIL)
                    job_error(jp, p->base.loc,
                            "parameter default values must evaluate at compile time");

                Type *t = typecheck_operation(jp, bind_type, init_type, '=', NULL, &(p->init));

                if(!t) {
                    job_error(jp, p->base.loc,
                            "invalid assignment of '%s' to parameter of type '%s'",
                            job_type_to_str(jp, init_type), job_type_to_str(jp, bind_type));
                }

                if(jp->state == JOB_STATE_ERROR) {
                    jp->allocator.scratch = save_scratch;
                    jp->allocator.sym = save_sym;
                    jp->allocator.type = save_type;
                    jp->allocator.value = save_value;
                    return;
                }

                bind_type = t;
            }

            Sym *ptr = job_current_scope_lookup(jp, p->name);

            if(ptr) {
                job_error(jp, p->base.loc,
                        "multiple declaration of parameter '%s' in header of '%s'",
                        p->name, ast->name, ptr->loc.line);
                jp->allocator.scratch = save_scratch;
                jp->allocator.sym = save_sym;
                jp->allocator.type = save_type;
                jp->allocator.value = save_value;
                return;
            }

            Sym sym = {
                .name = p->name,
                .loc = p->base.loc,
                .declared_by = jp->id,
                .is_argument = true,
                .type = bind_type,
                .value = init_value,
                .initializer = p->init,
            };

            Sym *symp = job_alloc_sym(jp);
            *symp = sym;
            job_scope_enter(jp, symp);

            proc_type->proc.param.types[p->index] = bind_type;
            proc_type->proc.param.values[p->index] = init_value;

            p->symbol_annotation = symp;
        }

        ast->checked_params = true;
    }

    if(ast->checked_rets == false) {
        u64 n = proc_type->proc.ret.n;

        if(n == 0) {
            jp->cur_retdecl = ast->rets;
            n = proc_type->proc.ret.n = ast->n_rets;
            proc_type->proc.ret.types = (Type**)job_alloc_scratch(jp, sizeof(Type*) * n);
        }

        for(AST_retdecl *r = jp->cur_retdecl; r; r = r->next) {
            if(arrlen(jp->expr) == 0) {
                linearize_expr(jp, r->expr);
            }
            typecheck_expr(jp);

            if(jp->state == JOB_STATE_WAIT) {
                jp->cur_retdecl = r;
                jp->allocator.scratch = save_scratch;
                jp->allocator.sym = save_sym;
                jp->allocator.type = save_type;
                jp->allocator.value = save_value;
                return;
            }

            if(jp->state == JOB_STATE_ERROR) {
                UNIMPLEMENTED;
                jp->allocator.scratch = save_scratch;
                jp->allocator.sym = save_sym;
                jp->allocator.type = save_type;
                jp->allocator.value = save_value;
                return;
            }

            arrsetlen(jp->type_stack, 0);
            arrsetlen(jp->value_stack, 0);
            arrsetlen(jp->expr, 0);
            jp->expr_pos = 0;

            assert(((AST_expr*)(r->expr))->type_annotation->kind == TYPE_KIND_TYPE);
            Type *ret_type = ((AST_expr*)(r->expr))->value_annotation->val.type;
            proc_type->proc.ret.types[r->index] = ret_type;
        }

        ast->checked_rets = true;
    }

    if(ast->n_rets > 0 && ast->body && !all_paths_return(jp, ast->body)) {
        job_error(jp, jp->non_returning_path_loc,
                "code path terminates with no return in procedure '%s'",
                ast->name);
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
        return;
    }

    proc_type->proc.first_default_param = ast->first_default_param;
    proc_type->proc.varargs = ast->varargs;
    proc_type->proc.has_defaults = ast->has_defaults;
    proc_type->proc.c_call = ast->c_call;
    proc_type->proc.is_foreign = ast->is_foreign;
    proc_type->proc.is_system = ast->is_system;

    Value *procid_value = global_alloc_scratch(sizeof(Value));
    procid_value->kind = VALUE_KIND_PROC;
    procid_value->val.procid = procid_alloc;

    Sym proc_sym = {
        .name = ast->name,
        .loc = ast->base.loc,
        .declared_by = jp->id,
        .procid = procid_alloc,
        .value = procid_value,
        .constant = true,
        .segment = IRSEG_CODE,
        .is_foreign = ast->is_foreign,
        .is_system = ast->is_system,
        .type = proc_type,
    };

    procid_alloc++;

    Sym *ptr = is_top_level ? global_scope_lookup(jp, ast->name) : job_current_scope_lookup(jp, ast->name);

    if(ptr) {
        job_error(jp, ast->base.loc,
                "multiple declaration of identifier '%s', previously declared at line %i",
                ast->name, ptr->loc.line);
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
        return;
    }

    jp->cur_proc_type = proc_type;
    //TODO if(is_top_level) { // local procedures
    Sym *symp = job_alloc_global_sym(jp);

    ast->symbol_annotation = symp;

    if(is_top_level) {
        *symp = proc_sym;
        global_scope_enter(jp, symp);
    } else {
        *symp = proc_sym;
        job_scope_enter(jp, symp);
    }

    if(is_top_level) {
        jp->allocator.scratch = save_scratch;
        jp->allocator.sym = save_sym;
        jp->allocator.type = save_type;
        jp->allocator.value = save_value;
    }
}

void typecheck_vardecl(Job *jp) {
    AST_vardecl *ast = (AST_vardecl*)arrlast(jp->tree_pos_stack);
    assert(ast->base.kind == AST_KIND_vardecl);

    bool infer_type = (ast->type == NULL);
    bool initialize = (ast->init != NULL);
    bool is_top_level = (arrlen(jp->tree_pos_stack) == 1);

    if(is_top_level && jp->handling_name == NULL)
        jp->handling_name = ast->name;

    if(infer_type) ast->checked_type = true;

    char *name = ast->name;
    Type *bind_type = NULL;
    Value *init_value = NULL;
    Type *init_type = NULL;

    if(ast->checked_type == false) {
        if(arrlen(jp->expr) == 0) {
            linearize_expr(jp, ast->type);
        }
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        arrsetlen(jp->type_stack, 0);
        arrsetlen(jp->value_stack, 0);
        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        ast->checked_type = true;

        if(!AST_KIND_IS_RECORD(ast->type->kind)) {
            if(((AST_expr_base*)ast->type)->value_annotation->kind != VALUE_KIND_TYPE) {
                job_error(jp, ast->type->loc,
                        "expected type to bind to '%s'", name);
                return;
            }
        }

    }

    if(ast->type) {
        if(AST_KIND_IS_RECORD(ast->type->kind)) {
            //NOTE maybe anonstructs could be given their own ast node
            bind_type = ((AST_structdecl*)(ast->type))->record_type;
        } else {
            bind_type = ((AST_expr*)ast->type)->value_annotation->val.type;
        }
    }

    if(!initialize) ast->checked_init = true;

    if(ast->checked_init == false) {

        //TODO should this check be for array views that take an array lit as well?
        if(bind_type && bind_type->kind == TYPE_KIND_ARRAY && ast->init->kind == AST_KIND_array_literal) {
            AST_array_literal *array_lit = (AST_array_literal*)(ast->init);

            array_lit->type_annotation = bind_type;
        }

        if(arrlen(jp->expr) == 0) {
            linearize_expr(jp, ast->init);
        }
        typecheck_expr(jp);

        if(jp->state == JOB_STATE_WAIT)
            return;

        arrsetlen(jp->type_stack, 0);
        arrsetlen(jp->value_stack, 0);
        arrsetlen(jp->expr, 0);
        jp->expr_pos = 0;

        ast->checked_init = true;
    }

    if(jp->state == JOB_STATE_ERROR) return;

    if(initialize) {
        //TODO multi-identifier declarations so we can initialize from a function that returns multiple values
        init_value = ((AST_expr*)ast->init)->value_annotation;
        init_type = ((AST_expr*)ast->init)->type_annotation;

        //if(!ast->constant && is_top_level && (TYPE_KIND_IS_NOT_SCALAR(init_type->kind) || init_type->kind == TYPE_KIND_POINTER)) {
        //    job_error(jp, ast->base.loc,
        //            "initialization of non scalar global variables is currently unimplemented");
        //    return;
        //}

        if(!infer_type) {
            //TODO check assignment of arrays differently maybe, this is a bit bodged
            if(bind_type->kind == TYPE_KIND_ARRAY &&
                    init_type->kind == TYPE_KIND_ARRAY && bind_type->array.n > init_type->array.n &&
                    ast->init->kind == AST_KIND_array_literal) {
                init_type->array.n = bind_type->array.n;
            }

            Type *t = typecheck_operation(jp, bind_type, init_type, '=', NULL, &(ast->init));

            if(!t) {
                job_error(jp, ast->base.loc,
                        "invalid assignment of '%s' to %s of type '%s'",
                        job_type_to_str(jp, init_type), ast->constant ? "constant" : "variable", job_type_to_str(jp, bind_type));
            }

            if(jp->state == JOB_STATE_ERROR) return;

            bind_type = t;
        } else {
            bind_type = init_type;
        }
    }

    if(arrlen(jp->record_types) > 0) {

        Type *record_type = arrlast(jp->record_types);

        bool in_union = (record_type->kind == TYPE_KIND_UNION);
        char *record_str =  in_union ? "union" : "struct";

        if(ast->constant) {
            //NOTE do const members even make sense?
            job_error(jp, ast->base.loc,
                    "%s members cannot be declared constant", record_str);
            return;
        }

        if(initialize && init_value->kind == VALUE_KIND_NIL) {
            job_error(jp, ast->base.loc,
                    "%s member initializer must be constant", record_str);
            return;
        }

        if(ast->uninitialized) {
            assert(!init_value);
            init_value = builtin_value+VALUE_KIND_NIL;
        }

        //TODO better name conflicts
        for(u64 i = 0; i < record_type->record.member.i; ++i) {
            if(!strcmp(name, record_type->record.member.names[i])) {
                job_error(jp, ast->base.loc,
                        "multiple declaration of %s member '%s', previously declared at line %i",
                        record_str, name, record_type->record.member.locs[i].line);
                return;
            }
        }

        //TODO better name conflicts
        for(u64 u = 0; u < record_type->record.use.i; ++u) {
            Type *use_t = record_type->record.use.types[u];

            for(u64 i = 0; i < use_t->record.member.i; ++i) {
                if(!strcmp(name, use_t->record.member.names[i])) {
                    job_error(jp, ast->base.loc,
                            "in '%s', member name '%s' conflicts with the included %s '%s' which has a member of the same name",
                            record_type->record.name,
                            name,
                            (use_t->kind == TYPE_KIND_UNION) ? "union" : "struct",
                            use_t->record.name);
                    return;
                }
            }
        }

        if(bind_type->align > record_type->align) {
            record_type->align = bind_type->align;
        }

        u64 i = record_type->record.member.i;

        record_type->record.member.i++;

        record_type->record.member.types[i] = bind_type;
        record_type->record.member.names[i] = name;
        record_type->record.member.values[i] = init_value;
        record_type->record.member.locs[i] = ast->base.loc;

        if(in_union) {
            if(bind_type->bytes > record_type->bytes)
                record_type->bytes = bind_type->bytes;
        } else {
            u64 offset = align_up(record_type->bytes, bind_type->align);
            record_type->record.member.offsets[i] = offset;
            record_type->bytes = offset + bind_type->bytes; //NOTE only align the record when you're done declaring it
        }

    } else {
        Sym sym = {
            .name = name,
            .loc = ast->base.loc,
            .declared_by = jp->id,
            .constant = ast->constant,
            .type = bind_type,
            .value = init_value,
            .initializer = ast->init,
        };

        if(is_top_level && init_value && init_value->kind == VALUE_KIND_NIL) {
            job_error(jp, ast->base.loc,
                    "attempted to initialize global '%s' to non constant expression",
                    name);
            return;
        }

        Sym *ptr = is_top_level ? global_scope_lookup(jp, ast->name) : job_current_scope_lookup(jp, ast->name);

        if(ptr) {
            job_error(jp, ast->base.loc,
                    "multiple declaration of identifier '%s', previously declared at line %i",
                    name, ptr->loc.line);
            return;
        }

        if(is_top_level) {
            Sym *symp = job_alloc_global_sym(jp);
            *symp = sym;
            ast->symbol_annotation = symp;
            global_scope_enter(jp, symp);
        } else {
            Sym *symp = job_alloc_sym(jp);
            *symp = sym;
            ast->symbol_annotation = symp;
            job_scope_enter(jp, symp);
        }
    }

}

void print_sym(Sym sym) {
    printf("name: %s\n", sym.name);
    printf("declared_by: %d\n", sym.declared_by);
    printf("constant: %s\n", sym.constant ? "true" : "false");
    printf("procid: %i\n", sym.procid);
    printf("segment: %s\n", IRsegment_debug[sym.segment]);
    printf("segment_offset: %lu\n", sym.segment_offset);
    printf("type: %s\n", global_type_to_str(sym.type));
    printf("value: ");
    print_value(sym.value);
    printf("\n");
    printf("size_in_bytes: %lu\n", sym.type->bytes);
}

//VERSION 1.0
//
//TODO finish globals
//TODO struct literals
//TODO output C
//TODO procedures for interfacing with the compiler (e.g. add_source_file())
//TODO debug info (maybe better to do this in the interpreter? seeing as we output C) (internal and convert to gdb debug format)

//EXTRA
//
//TODO proc types with argument names
//TODO c-style for loops (use 'while' keyword)
//TODO enums
//TODO local procedures and structs
//TODO directives (#import, #assert, etc)
//TODO parametrized structs
//TODO proc polymorphism with parametrized structs

//VERSION 2.0
//TODO redesign from scratch
//TODO macros
//TODO iterators
//TODO compiler intercept and event loop
//TODO output x64 machine code

Dynamic_array      _gdb_stupid_1; // because gdb is stupid
Any                _gdb_stupid_2;
Context            _gdb_stupid_3;
Temporary_storage  _gdb_stupid_4;

int main(int argc, char **argv) {
    assert(argc == 2);

    char *preload_path = "preload.jpl";
    assert(FileExists(preload_path));

    char *basic_path = "basic.jpl";
    assert(FileExists(basic_path));

    char *path = argv[1];
    assert(FileExists(path));

    arena_init(&global_scratch_allocator);
    pool_init(&global_sym_allocator, sizeof(Sym));
    pool_init(&global_type_allocator, sizeof(Type));
    pool_init(&global_value_allocator, sizeof(Value));

    arena_init(&string_arena);
    arena_init(&type_info_arena);

    char *preload_src = LoadFileText(preload_path);

    if(job_runner(preload_src, preload_path))
        return 0;

    type_String_view    = shget(global_scope, "_String_view")->value->val.type;
    type_Array_view     = shget(global_scope, "_Array_view")->value->val.type;
    type_Dynamic_array  = shget(global_scope, "_Dynamic_array")->value->val.type;
    type_Any            = shget(global_scope, "Any")->value->val.type;
    type_Context        = shget(global_scope, "Context")->value->val.type;
    type_Context_pointer =
        (Type) { .kind = TYPE_KIND_POINTER, .pointer = { .to = type_Context } };
    sym_default_allocator = shget(global_scope, "__default_allocator");

    type_Temporary_storage        = shget(global_scope, "Temporary_storage")->value->val.type;
    type_Temporary_storage_pointer =
        (Type) { .kind = TYPE_KIND_POINTER, .pointer = { .to = type_Context } };
    sym_temporary_allocator = shget(global_scope, "__temporary_allocator");

    assert(sizeof(Array_view) == type_Array_view->bytes);
    assert(_Alignof(Array_view) == type_Array_view->align);
    assert(sizeof(String_view) == type_String_view->bytes);
    assert(_Alignof(String_view) == type_String_view->align);
    assert(sizeof(Dynamic_array) == type_Dynamic_array->bytes);
    assert(_Alignof(Dynamic_array) == type_Dynamic_array->align);
    assert(sizeof(Any) == type_Any->bytes);
    assert(_Alignof(Any) == type_Any->align);

    char *basic_src = LoadFileText(basic_path);

    if(job_runner(basic_src, basic_path))
        return 0;

    char *test_src_file = LoadFileText(path);

    job_runner(test_src_file, path);

    for(int i = 0; i < hmlen(proc_table); ++i) {
        IRproc p = (proc_table + i)->value;
        if(p.is_foreign) {
            dlclose(p.wrapper_dll);
        }
    }

    //arena_destroy(&global_scratch_allocator);
    //pool_destroy(&global_sym_allocator);
    //pool_destroy(&global_type_allocator);
    //pool_destroy(&global_value_allocator);

    return 0;
}
