#include "basic.h"
#include "stb_ds.h"
#include "pool.h"

#include "lexer.c"


#define PIPE_STAGES              \
    X(PARSE)                     \
    X(TYPE)                      \
    X(SIZE)                      \
    X(IR)

#define JOB_STATES               \
    X(READY)                     \
    X(WAIT)                      \
    X(BLOCK)                     \
    X(FREE)

#define ASTKINDS                 \
    X(vardec)                    \
    X(expr)                      \
    X(param)                     \
    X(atom)                      \
    X(call)

#define OPERATOR_PREC_TABLE     \
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
    X(TOKEN_OR,                      4)


typedef struct Job Job;

typedef enum ASTkind {
    AST_KIND_INVALID = -1,
#define X(x) AST_KIND_##x,
    ASTKINDS
#undef X
    AST_KIND_MAX,
} ASTkind;

typedef struct AST AST;
#define X(x) typedef struct AST_##x AST_##x;
ASTKINDS
#undef X

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


struct Job {
    u64 id;

    Job_state job_state;
    Pipe_stage pipe_stage;

    char *ident_waiting;
    u64 job_id_waiting;

    Lexer lexer;
    union {
        /* pipe_stage == PIPE_STAGE_PARSE */
        Arr(AST*) global_statements; 

        /* pipe_stage >= PIPE_STAGE_TYPE */
        AST *ast;
    };

    /* pipe_stage == PIPE_STAGE_TYPE */
    Arr(Dict(u64)) scopes;
    Arr(Token) operators;
    Arr(u64) operand_type_ids;
    Arr(AST**) expr_buf;
    Arr(AST*) tree_pos;

    Arr(char) text_allocator;

#define X(x) Pool ast_##x##_allocator;
    ASTKINDS
#undef X
};


struct AST {
    ASTkind kind;
    int checked;
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

struct AST_vardec {
    AST base;
    char *name;
    bool constant;
    AST *type;
    AST *init;
    AST *next;
};


AST* job_alloc_ast(Job *jp, ASTkind kind);
char* job_alloc_text(Job *jp, char *s, size_t bytes);
void job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]);
void job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]);

int getprec(Token t);

AST* parse_vardec(Job *jp, Token separator);
AST* parse_type_expr(Job *jp);
AST* parse_expr(Job *jp);
AST* parse_expr_increase_prec(Job *jp, AST *left, int min_prec);
AST* parse_expr_decrease_prec(Job *jp, int min_prec);
AST* parse_term(Job *jp);


AST* job_alloc_ast(Job *jp, ASTkind kind) {
    AST *ptr = NULL;
    switch(kind) {
        default:
            assert("invalid ast kind"&&0);
            break;
#define X(x) case AST_KIND_##x: ptr = pool_alloc(&jp->ast_##x##_allocator); break;
            ASTKINDS
#undef X
    }
    ptr->kind = kind;
    ptr->checked = false;
    return ptr;
}

void job_ast_allocator_to_save(Job *jp, Pool_save save[AST_KIND_MAX]) {
#define X(x) save[(int)AST_KIND_##x] = pool_to_save(&jp->ast_##x##_allocator);
    ASTKINDS
#undef X
}

void job_ast_allocator_from_save(Job *jp, Pool_save save[AST_KIND_MAX]) {
#define X(x) pool_from_save(&jp->ast_##x##_allocator,  save[(int)AST_KIND_##x]);
    ASTKINDS
#undef X
}

char* job_alloc_text(Job *jp, char *s, size_t s_len){
    char *ptr = arraddnptr(jp->text_allocator, s_len + 1);
    strncpy(ptr, s, s_len);
    ptr[s_len] = 0;
    return ptr;
}

INLINE int getprec(Token t) {
    switch(t) {
#define X(t, prec) case (Token)t: return prec;
        OPERATOR_PREC_TABLE
#undef X
        default: return -1;
    }
}

/*
 * vardec: ident type_expr (('=' | ':') expr)? ';'
 *       | ident (':=' | '::') expr ';'
 */
AST* parse_vardec(Job *jp, Token separator) {
    Lexer *lexer = &jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        *lexer = unlex;
        return NULL;
    }

    AST_vardec *node = (AST_vardec*)job_alloc_ast(jp, AST_KIND_vardec);
    node->name = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);

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
            node->constant = (t == TOKEN_TWOCOLON);
            node->init = parse_expr(jp);
            t = lex(lexer);
        } else if(t != separator) {
            assert("expected '=', ':' or separator in vardec"&&0);
        }
    }

    //TODO compiler error messages
    assert("expected separator at end of variable declaration"&&(t == separator));

    return (AST*)node;
}

AST* parse_expr(Job *jp) {
    return parse_expr_decrease_prec(jp, 0);
}

AST* parse_expr_increase_prec(Job *jp, AST *left, int min_prec) {
    Lexer *lexer = &jp->lexer;
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
    Lexer *lexer = &jp->lexer;
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
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
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
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
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
                    param->name = job_alloc_text(jp, s, e - s);
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
                atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
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

char *test_src = 
"f := atan2(i * 1.4e3 + 0.3, y + \"this wouldn't pass typechecking lol\", z);\n"
"pointer_to_array_of_int *[12]int;\n"
"i int = 12;\n"
"PI :: 3.14;\n"
"s := \"hello sailor\";\n"
;

int main(void) {
    //Arr(Job) job_queue = NULL;

    Job job = {
        .id = 0,
        .job_state = JOB_STATE_READY,
        .pipe_stage = PIPE_STAGE_PARSE,  
    };

    lexer_init(&job.lexer, test_src, "not a file");

#define X(x) pool_init(&job.ast_##x##_allocator, sizeof(AST_##x));
    ASTKINDS;
#undef X

    while(true) {
        AST *ast = parse_vardec(&job, ';');

        if(ast == NULL) {
            Token t = lex(&job.lexer);
            assert(t == 0);
            printf("stopping all jobs...\n");
            break;
        }

        arrpush(job.global_statements, ast);
    }

    for(int i = 0; i < arrlen(job.global_statements); ++i) {
        AST_vardec *node = (AST_vardec*)job.global_statements[i];
        printf("vardec: %s\n", node->name);
        printf("type:\n");
        print_ast_expr(node->type, 1);
        printf("init:\n");
        print_ast_expr(node->init, 1);
        printf("\n");
    }

    //arrpush(job_queue, job);
    //for(int i = 0; i < arrlen(job_queue); ++i) {
    //}

    return 0;
}
