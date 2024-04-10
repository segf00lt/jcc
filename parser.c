#define _UNITY_BUILD_

#include "basic.h"
#include "lexer.c"
#include "stb_ds.h"
#include "slab.h"

#define ASTKINDS                 \
    X(VARDEC)                    \
    X(EXPR)                      \
    X(PARAM)                     \
    X(ATOM)                      \
    X(CALL)

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

#define OPERATOR_PREC_TABLE     \
    /* indexing */\
    X((Token)'.',                    10)\
    X((Token)'[',                    10)\
    /* additive */\
    X((Token)'+',                    9)\
    X((Token)'-',                    9)\
    /* multiplicative */\
    X((Token)'*',                    8)\
    X((Token)'/',                    8)\
    X((Token)'%',                    8)\
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
    /* separators */\
    X(')',                          -1)\
    X(',',                          -1)\
    X(';',                          -1)\
    X('{',                          -1)\
    X(']',                          -1)

typedef struct AST AST;
typedef struct AST_call AST_call;
typedef struct AST_param AST_param;
typedef struct AST_expr AST_expr;
typedef struct AST_atom AST_atom;
typedef struct AST_vardec AST_vardec;

typedef struct Job Job;

typedef enum ASTkind {
#define X(x) AST_KIND_##x,
    ASTKINDS
#undef X
} ASTkind;

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
    AST *ast;

    Slab ast_allocator;
    Arr(char) text_allocator;

    Arr(Dict(u64)) scopes;
    Arr(Token) operators;
    Arr(u64) operand_type_ids;
    Arr(AST**) expr_buf;
    Arr(AST*) tree_pos;
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


// a + -v[i][j] --> (a + (-((v[i])[j])))
//              +
//             / \
//            a   -
//                 \
//                 [
//                / \
//               [   j
//              / \
//             v   i
//
//
// some_struct.arr_member[i][j].sub_memeber
//
//                           .
//                          / \
//                         /   sub_memeber
//                        [
//                       / \
//                      [   j 
//                     / \
//                    .   i
//                   / \
//                  /   \
//                 /     \
//      some_struct       arr_member
//
//
// *[SIZE * CAP]u8
// u8[SIZE * CAP]*
//
INLINE int getprec(Token t) {
    switch(t) {
#define X(t, prec) case t: return prec;
        OPERATOR_PREC_TABLE
#undef X
        default: return -1;
    }
}

/*
 * vardec: ident type_expr (('=' | ':') expr)? ';'
 *       | ident (':=' | '::') expr ';'
 */

AST* job_alloc_ast(Job *jp, ASTkind, size_t bytes);
char* job_alloc_text(Job *jp, char *s, size_t bytes);

AST* parse_vardec(Job *jp);
AST* parse_type_expr(Job *jp);
AST* parse_expr(Job *jp);
AST* parse_expr_increase_prec(Job *jp, AST *left, int min_prec);
AST* parse_expr_decrease_prec(Job *jp, int min_prec);
AST* parse_term(Job *jp);


AST* job_alloc_ast(Job *jp, ASTkind kind, size_t bytes) {
    AST *ptr = slab_alloc(&jp->ast_allocator, bytes);
    ptr->kind = kind;
    return ptr;
}

char* job_alloc_text(Job *jp, char *s, size_t s_len){
    char *ptr = arraddnptr(jp->text_allocator, s_len + 1);
    strncpy(ptr, s, s_len);
    ptr[s_len] = 0;
    return ptr;
}

AST* parse_vardec(Job *jp) {
    Lexer *lexer = &jp->lexer;
    Lexer unlex = *lexer;

    Token t = lex(lexer);

    if(t != TOKEN_IDENT) {
        *lexer = unlex;
        return NULL;
    }

    AST_vardec *node = (AST_vardec*)job_alloc_ast(jp, AST_KIND_VARDEC, sizeof(AST_vardec));
    node->name = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);

    unlex = *lexer;
    t = lex(lexer);

    if(t == TOKEN_WALRUS || t == TOKEN_TWOCOLON) {
        node->constant = (t == TOKEN_TWOCOLON);
        node->init = parse_expr(jp);
    } else {
        *lexer = unlex;
        node->type = parse_expr(jp);

        t = lex(lexer);
        if(t == '=' || t == ':') {
            node->constant = (t == TOKEN_TWOCOLON);
            node->init = parse_expr(jp);
            t = lex(lexer);
        }

        //TODO compiler error messages
        assert("expected semicolon at end of variable declaration"&&(t == ';'));
    }

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

    op = (AST_expr*)job_alloc_ast(jp, AST_KIND_EXPR, sizeof(AST_expr));
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
        assert(node->kind == AST_KIND_EXPR);
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
    AST_atom *atom = NULL;

    switch(t) {
        default:
            //TODO compiler error messages
            assert("unimplemented"&&0);
            break;
        case ')': /* calls with no parameters will end up here */
            *lexer = unlex;
            return NULL;
            break;
        case '+': case '-': case '!': case '~': case '&': case '*':
            unary = (AST_expr*)job_alloc_ast(jp, AST_KIND_EXPR, sizeof(AST_expr));
            unary->token = t;
            node = parse_term(jp);
            break;
        case '[':
            unary = (AST_expr*)job_alloc_ast(jp, AST_KIND_EXPR, sizeof(AST_expr));
            unary->token = t;
            node = parse_expr(jp);
            t = lex(lexer);
            //TODO compiler error messages
            assert("unbalanced square bracket"&&(t == ']'));
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
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            node = (AST*)atom;
            break;
        case TOKEN_IDENT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
            node = (AST*)atom;
            break;
        case TOKEN_INTLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            atom->integer = lexer->integer;
            node = (AST*)atom;
            break;
        case TOKEN_HEXLIT: case TOKEN_BINLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            atom->uinteger = lexer->uinteger;
            node = (AST*)atom;
            break;
        case TOKEN_FLOATLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
            node = (AST*)atom;
            break;
        case TOKEN_STRINGLIT:
            atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
            atom->token = t;
            atom->text = job_alloc_text(jp, lexer->text.s, lexer->text.e - lexer->text.s);
            node = (AST*)atom;
            break;
    }

    /* postfix operators */
    unlex = *lexer;
    t = lex(lexer);

    if(t == '(') {
        call_op = (AST_call*)job_alloc_ast(jp, AST_KIND_CALL, sizeof(AST_call));
        call_op->callee = node;

        Slab_save save = slab_make_save(&jp->ast_allocator);

        AST_param head;
        AST_param *param = (AST_param*)job_alloc_ast(jp, AST_KIND_PARAM, sizeof(AST_param));
        head.next = param;

        while(true) {
            bool named_param = false;

            unlex = *lexer;
            t = lex(lexer);

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
                slab_from_save(&jp->ast_allocator, save);
            } else {
                param->value = (AST*)expr;
            }

            param->next = (AST_param*)job_alloc_ast(jp, AST_KIND_PARAM, sizeof(AST_param));
            param = param->next;
        }

    } else if(t == '.' || t == '[') {
        index_op = (AST_expr*)job_alloc_ast(jp, AST_KIND_EXPR, sizeof(AST_expr));
        index_op->token = t;
        index_op->left = node;
        while(true) {
            if(t == '.') {
                t = lex(lexer);
                //TODO compiler error messages
                assert("expected identifier in struct member reference"&&(t==TOKEN_IDENT));
                atom = (AST_atom*)job_alloc_ast(jp, AST_KIND_ATOM, sizeof(AST_atom));
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
                index_op_next = (AST_expr*)job_alloc_ast(jp, AST_KIND_EXPR, sizeof(AST_expr));
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

//TODO test parser
char *test_src = 
"i int = 12;\n"
"f := 1.4e3;\n"
"PI :: 3.14;\n"
"s := \"hello sailor\";\n"
;

int main(void) {
    return 0;
}
