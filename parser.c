#define STB_DS_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "basic.h"
#include "stb_ds.h"

typedef char Token;
typedef struct AST {
    char c;
    struct AST *left;
    struct AST *right;
} AST;

AST ast_buf[32] = {0};
AST *ast_alloc = ast_buf;

char src[] = "a + b * (c + f) / d - e";
char *lexer_pos = src;

Token lex(void);
void unlex(int n);
int getprec(Token op);
AST* term(void);
AST* expr(void);
AST* expr_increase_prec(AST *left, int min_prec);
AST* expr_decrease_prec(int min_prec);

JINLINE int getprec(Token op) {
    switch(op) {
        case '+': case '-': return 1;
        case '/': return 2;
        case '*': return 3;
    }
    return 0;
}

JINLINE Token lex(void) {
    Token t;
    while(*lexer_pos == ' ') ++lexer_pos;
    t = *lexer_pos++;
    return t;
}

JINLINE void unlex(int n) {
    lexer_pos -= n;
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

Arr(AST*) operand_stack = NULL;
Arr(Token) operator_stack = NULL;

AST* expr_shunting_yard(void) {
/*
 * term()
 * op()
 * term()
 * on next op compare precedence against the last op's precedence
 * if prec has dropped pop off the previous operators forming the sub expressions whilst
 * precedence of the current op remains lower than the op at stack top
 * else push the op and continue as normal
 *
 * The expr needs to be written to a linear array, linking will take too long.
 * Should this array be stored in a statement node?
 * Maybe all parts of the AST should be stored in array style, instead of being a
 * self referential structure. In certain places the indirection of the tree representation
 * is useful.
 */
    while(true) {
        AST *node = term();
        Token op = lex();
        int cur_prec = getprec(op);
        while(getprec(arrlast(operator_stack)) > cur_prec) {
        }
    }
}

void print_ast(AST *node) {
    if(!node) return;
    print_ast(node->left);
    print_ast(node->right);
    printf("%c ", node->c);
}

int main(void) {
    AST *e = expr();
    printf("%s\n",src);
    print_ast(e);
    printf("\n");
    return 0;
}
