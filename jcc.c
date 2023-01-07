/* reduced C grammar taken from https://intrepidgeeks.com/tutorial/c-bnf-grammar
 *
 *
 * Terminals: 
 * typedef-name integer-constant character-constant floating-constant 
 * enumeration-constant identifier
 *
 * translation-unit: (function-definition | declaration)+
 *
 * function-definition: 
 * declaration-specifiers? declarator declaration* block
 *
 * declaration: declaration-specifiers init-declarator% ";"
 * 
 * declaration-specifiers: 
 * (storage-class-specifier | type-specifier | type-qualifier)+
 * 
 * storage-class-specifier: 
 * ("auto" | "register" | "static" | "extern" | "typedef")
 * 
 * type-specifier: ("void" | "char" | "short" | "int" | "long" | "float" |
 * "double" | "signed" | "unsigned" | struct-or-union-specifier | 
 * 
 * enum-specifier | typedef-name)
 * 
 * type-qualifier: ("const" | "volatile")
 * 
 * struct-or-union-specifier: 
 * ("struct" | "union") ( identifier? "{" struct-declaration+ "}" | identifier)
 * 
 * init-declarator: declarator ("=" initializer)?
 * 
 * struct-declaration: 
 * 
 * (type-specifier | type-qualifier)+ struct-declarator%
 * 
 * struct-declarator: declarator | declarator? ":" constant-expression
 * 
 * enum-specifier: "enum" (identifier | identifier? "{" enumerator% "}")
 * 
 * enumerator: identifier ("=" constant-expression)?
 * 
 * declarator: 
 * pointer? (identifier | "(" declarator ")") (
 * 		"[" constant-expression? "]" |
 * 		"(" parameter-type-list ")" |
 * 		"(" identifier%? ")")*
 * 
 * pointer:
 * ("*" type-qualifier*)*
 * 
 * parameter-type-list: parameter-declaration% ("," "...")?
 * 
 * parameter-declaration: declaration-specifiers (declarator | abstract-declarator)?
 * 
 * initializer: assignment-expression | "{" initializer% ","? "}"
 * 
 * type-name: (type-specifier | type-qualifier)+ abstract-declarator?
 * 
 * abstract-declarator: 
 * pointer ("(" abstract-declarator ")")? (
 * 		"[" constant-expression? "]" |
 * 		"(" parameter-type-list? ")")*
 * 
 * statement:
 * ((identifier | "case" constant-expression | "default") ":")*
 * (expression? ";" | 
 *  block | 
 *  "if" "(" expression ")" statement |
 *  "if" "(" expression ")" statement "else" statement |
 *  "switch" "(" expression ")" statement |
 *  "while" "(" expression ")" statement |
 *  "do" statement "while" "(" expression ")" ";" |
 *  "for" "(" expression? ";" expression? ";" expression? ")" statement |
 *  "goto" identifier ";" |
 *  "continue" ";" |
 *  "break" ";" |
 *  "return" expression? ";"
 * )
 * 
 * block: "{" declaration* statement* "}"
 * 
 * expression: 
 * assignment-expression%
 * 
 * assignment-expression:
 * (unary-expression (
 * "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
 * ))* conditional-expression
 * 
 * conditional-expression:
 * logical-OR-expression ( "?" expression ":" conditional-expression )?
 * 
 * constant-expression: conditional-expression
 * 
 * logical-OR-expression:
 * logical-AND-expression ( "||" logical-AND-expression )*
 * 
 * logical-AND-expression:
 * inclusive-OR-expression ( "&&" inclusive-OR-expression )*
 * 
 * inclusive-OR-expression:
 * exclusive-OR-expression ( "|" exclusive-OR-expression )*
 * 
 * exclusive-OR-expression:
 * AND-expression ( "^" AND-expression )*
 * 
 * AND-expression:
 * equality-expression ( "&" equality-expression )*
 * 
 * equality-expression:
 * relational-expression ( ("==" | "!=") relational-expression )*
 * 
 * relational-expression:
 * 
 * shift-expression ( ("<" | ">" | "<=" | ">=") shift-expression )*
 * 
 * shift-expression:
 * 
 * additive-expression ( ("<<" | ">>") additive-expression )*
 * 
 * additive-expression:
 * multiplicative-expression ( ("+" | "-") multiplicative-expression )*
 * 
 * multiplicative-expression:
 * cast-expression ( ("*" | "/" | "%") cast-expression )*
 * 
 * cast-expression:
 * ( "(" type-name ")" )* unary-expression
 * 
 * unary-expression:
 * ("++" | "--" | "sizeof" )* ("sizeof" "(" type-name ")" | ("&" | "*" | "+" | "-" | "~" | "!" )* cast-expression | postfix-expression)
 * 
 * 
 * 
 * postfix-expression:
 * (identifier | constant | string | "(" expression ")") (
 * 		"[" expression "]"             |
 * 		"(" assignment-expression% ")" |
 * 		"." identifier                 |
 * 		"->" identifier                |
 * 		"++"                           |
 * 		"--")*
 * 
 * constant: integer-constant | character-constant | floating-constant | enumeration-constant
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <error.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <libgen.h>
#include <assert.h>
#include "fmap.c"

#define ARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */

#define AST_PAGE_SIZE 256
#define STRPOOL_PAGE_SIZE 512
#define AST_INITIAL_PAGE_COUNT 4
#define STRPOOL_INITIAL_PAGE_COUNT 4

#define STRPOOL_INIT(pool) { \
	pool.pages = malloc(STRPOOL_INITIAL_PAGE_COUNT * sizeof(char*)); \
	pool.cap = STRPOOL_INITIAL_PAGE_COUNT; \
	pool.cur = 0; \
	pool.pages[pool.cur] = malloc(STRPOOL_PAGE_SIZE); \
	pool.base = pool.free = pool.pages[ast.cur]; \
}

#define AST_INIT(ast) { \
	ast.pages = malloc(AST_INITIAL_PAGE_COUNT * sizeof(AST_node*)); \
	ast.cap = AST_INITIAL_PAGE_COUNT; \
	ast.cur = 0; \
	ast.nodecount = 1; \
	ast.pages[ast.cur] = malloc(AST_PAGE_SIZE * sizeof(AST_node)); \
	ast.base = ast.free = ast.pages[ast.cur]; \
}

#define SYM_TAB_INIT(tab) {\
	tab.cap = 128;\
	tab.data = calloc(tab.cap, sizeof(Sym));\
	tab.count = tab.static_count = tab.this_count = tab.arg_count = tab.local_count = 0;\
}

typedef struct {
	char *base;
	char *free;
	char **pages;
	size_t cur;
	size_t cap;
} Strpool;

/* TODO how do we give good compiler feedback? (errors, warnings, hints etc) */

typedef struct {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
	int line;
	int col;
	char *line_s;
	char *line_e;
} Lexer;

typedef struct syntax_node {
	unsigned int id; /* debug */
	int kind;
	char *val;
	struct syntax_node *down; /* down */
	struct syntax_node *next; /* across */
	int line;
	int col;
	char *line_s;
	char *line_e;
} AST_node;

typedef struct {
	/* since we never free individual nodes AST.free stores the first vacant
	 * node in the current page */
	AST_node *root;
	AST_node *free;
	AST_node *base; /* base of current page */
	AST_node **pages;
	size_t cur; /* index of current page */
	size_t cap; /* capacity of **pages */
	size_t nodecount; /* debug */
} AST;

typedef struct {
	int pos; /* position in memory segment */
	int seg; /* memory segment */
	char *type; /* data type string */
	char *name;
} Sym;

typedef struct {
	Sym *data;
	char *name;
	size_t cap;
	size_t count;
	/* TODO segment count union */
	size_t static_count;
	size_t arg_count;
	size_t local_count;
} Sym_tab;

/* parsing */
int lex(void);
void parse(void);

int lex(void) {
	char *tp, *s;
	int check;

	if(lexer.ptr[0] == '/' && lexer.ptr[1] == '/') { /* single-line comments */
		while(*lexer.ptr != '\n') ++lexer.ptr;
		++lexer.ptr;
	} else if(lexer.ptr[0] == '/' && lexer.ptr[1] == '*') { /* multi-line comments */
		while(!(lexer.ptr[0] == '*' && lexer.ptr[1] == '/')) ++lexer.ptr;
		lexer.ptr += 2;
	} else if(isspace(*lexer.ptr)) { /* whitespace */
		while(isspace(*lexer.ptr)) ++lexer.ptr;
	}

	tp = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0)
		return T_END;

	lexer.text_s = lexer.text_e = 0;
	lexer.token = 0;

	/* symbol */
	switch(*tp) {
	case '{': case '}': case '(': case ')': case '[': case ']': case '.': case ',': case ';':
	case '+': case '-': case '*': case '/': case '&': case '|': case '<': case '>': case '=': case '~':
		++lexer.ptr;
		return (lexer.token = *tp);
	}

	/* keyword */
	for(int i = 0; i < ARRLEN(keyword); ++i) {
		check = tp[keyword_length[i]];
		if(strstr(tp,keyword[i]) == tp &&
				(check == ' ' ||
				 check == ';' ||
				 check == '(' ||
				 check == ')' ||
				 check == ',' ||
				 check == '{' ||
				 check == '}')
		  )
		{
			lexer.ptr += keyword_length[i];
			return (lexer.token = T_CLASS + i);
		}
	}

	/* numbers, strings and identifiers */
	for(check = 0, s = tp; *s && isdigit(*s); ++s)
		++check;
	if(check) {
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		if(!check)
			error(1, 0, "mismatched quotes");
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STRING);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = T_ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;
	return (lexer.token = T_ID);
}

int main(void) {
	return 0;
}
