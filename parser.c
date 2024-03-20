#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "basic.h"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

typedef char Token;
typedef struct AST {
    char c;
    struct AST *left;
    union {
        struct AST *right;
        struct AST *next;
    };
} AST;

AST ast_buf[32] = {0};
AST *ast_alloc = ast_buf;

char *lexer_pos = NULL;

Token lex(void);
Token unlex(int n);
Token lexpeek(void);
int getprec(Token op);
AST* term(void);
AST* expr(void);
AST* expr_increase_prec(AST *left, int min_prec);
AST* expr_decrease_prec(int min_prec);

INLINE int getprec(Token op) {
    switch(op) {
        case '+': case '-': return 1;
        case '/': return 2;
        case '*': return 3;
    }
    return 0;
}

INLINE Token lex(void) {
    Token t;
    while(*lexer_pos == ' ') ++lexer_pos;
    t = *lexer_pos++;
    return t;
}

INLINE Token unlex(int n) {
    lexer_pos -= n;
    return *lexer_pos;
}

INLINE Token lexpeek(void) {
    while(*lexer_pos == ' ') ++lexer_pos;
    return *lexer_pos;
}

AST* expr(void) {
    return expr_decrease_prec(0);
}

AST* expr_increase_prec(AST *left, int min_prec) {
    AST *op;

    Token t = lex();
    int prec = getprec(t);

    if(prec <= min_prec) {
        unlex(1);
        return left;
    }

    op = ast_alloc++;
    op->c = t;
    op->right = expr_decrease_prec(prec);

    return op;
}

AST* expr_decrease_prec(int min_prec) {
    AST *left, *node;

    left = term();

    while(true) {
        node = expr_increase_prec(left, min_prec);
        if(node == left) break;
        node->left = left;
        left = node;
    }

    return left;
}

AST* term(void) {
    AST *node;
    Token t = lex();

    if(t == '(') {
        node = expr();
        t = lex();
        assert(t == ')');
    } else {
        assert(isalpha(t));
        node = ast_alloc++;
        node->c = t;
    }

    return node;
}

AST* shunting_term(void) {
    AST *node;
    Token t = lex();

    assert(isalpha(t));
    node = ast_alloc++;
    node->c = t;

    return node;
}
#define IS_UNOP_PREFIX(op) (op == '-' || op == '&' || op == '~' || op == '*' || op == '!')
#define IS_UNOP_POSTFIX(op) (op == '$' || op == '@')
#define IS_BINOP(op) (op == '+' || op == '-' || op == '*' || op == '/' || op == '%')

AST* shunting_expr(void) {
    Token op = 0;
    Token t = 0;
    Arr(AST*) operand_starts = NULL;
    Arr(AST*) operand_ends = NULL;
    Arr(Token) binops = NULL;
    Arr(Token) unops = NULL;
    Arr(Token) operators = NULL;
    AST *node = NULL;
    bool last_was_not_atom = true;

    while(true) {
        t = lex(); 

        if(t == '(') {
            arrpush(binops, '(');
            arrpush(unops, '(');
            last_was_not_atom = true;
            continue;
        } else if(t == ')') {
            // pop all binary ops
            assert(!last_was_not_atom);
            while(arrlen(binops) > 0 && arrlast(binops) != '(') {
                node = ast_alloc++; 
                node->c = arrpop(binops);
                arrpop(operand_ends)->next = node;
                arrlast(operand_ends)->next = arrpop(operand_starts);
                operand_ends[arrlen(operand_ends)-1] = node;
            }
            // pop '('
            assert(arrlast(binops) == '(');
            assert(arrlast(unops) == '(');
            arrpop(binops);
            arrpop(unops);
        } else if(IS_UNOP_PREFIX(t) && last_was_not_atom) {
            arrpush(unops, t);
        } else if(IS_UNOP_POSTFIX(t)) {
            // tack postfix on end
            u64 last = arrlen(operand_ends) - 1;
            node = ast_alloc++;
            node->c = t;
            operand_ends[last]->next = node;
            operand_ends[last] = node;
        } else if(IS_BINOP(t)) {
            assert(!last_was_not_atom);
            // pop all unary prefix
            u64 last = arrlen(operand_ends) - 1;
            while(arrlen(unops) > 0 && arrlast(unops) != '(') {
                node = ast_alloc++; 
                node->c = arrpop(unops);
                operand_ends[last]->next = node;
                operand_ends[last] = node;
            }
            // pop all higher prec binary ops
            while(arrlen(binops) > 0 && arrlast(binops) != '(' && getprec(t) <= getprec(arrlast(binops))) {
                node = ast_alloc++; 
                node->c = arrpop(binops);
                arrpop(operand_ends)->next = node;
                arrlast(operand_ends)->next = arrpop(operand_starts);
                operand_ends[arrlen(operand_ends)-1] = node;
            }
            // push t to binops
            arrpush(binops, t);

            last_was_not_atom = true;
            continue;
        } else if(t == ';' || t == ',') {
            u64 last = arrlen(operand_ends) - 1;

            while(arrlen(unops) > 0 && arrlast(unops) != '(') {
                node = ast_alloc++; 
                node->c = arrpop(unops);
                operand_ends[last]->next = node;
                operand_ends[last] = node;
            }

            while(arrlen(binops) > 0 && arrlast(binops) != '(') {
                node = ast_alloc++; 
                node->c = arrpop(binops);
                arrpop(operand_ends)->next = node;
                arrlast(operand_ends)->next = arrpop(operand_starts);
                operand_ends[arrlen(operand_ends)-1] = node;
            }
            node = arrpop(operand_starts);
            arrpop(operand_ends);

            // make sure stacks are clear and stop
            assert(arrlen(operand_starts) == 0);
            assert(arrlen(operand_ends) == 0);
            assert(arrlen(binops) == 0);
            assert(arrlen(unops) == 0);
            break;
        } else {
            unlex(1);
            node = shunting_term();
            arrpush(operand_starts, node);
            arrpush(operand_ends, node);
        }

        last_was_not_atom = false;
    }

    return node;
}

void print_ast(AST *node) {
    if(!node) return;
    print_ast(node->left);
    print_ast(node->right);
    printf("%c ", node->c);
}

void print_ast_linear(AST *node) {
    if(!node) return;
    printf("%c ", node->c);
    print_ast_linear(node->next);
}

char src1[] = "(a + b)";
char src2[] = "-(a * b + c)$ - d * e;";

int main(void) {
    lexer_pos = src1;
    AST *e = expr();
    printf("%s\n",src1);
    print_ast(e);
    printf("\n");

    lexer_pos = src2;
    e = shunting_expr();
    printf("%s\n",src2);
    print_ast_linear(e);
    printf("\n");
    return 0;
}
