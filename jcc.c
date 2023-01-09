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

enum TOKENS {
	T_ASSIGNPLUS = 256,
	T_ASSIGNSUB,
	T_ASSIGNMUL,
	T_ASSIGNDIV,
	T_ASSIGNMOD,
	T_ASSIGNNOT,
	T_ASSIGNAND,
	T_ASSIGNOR,
	T_ASSIGNXOR,
	T_INC,
	T_DEC,
	T_NEQ,
	T_LTEQ,
	T_GTEQ,
	T_EQ,
	T_LSHIFT,
	T_RSHIFT,
	T_AND,
	T_OR,
	T_CONST,
	T_DO,
	T_ENUM,
	T_INTERN,
	T_EXTERN,
	T_REGISTER,
	T_SIZEOF,
	T_STRUCT,
	T_TYPEDEF,
	T_UNION,
	T_STATIC,
	T_INT, T_CHAR, T_VOID,
	T_DOUBLE, T_FLOAT, T_LONG, T_SHORT,
	T_UNSIGNED,
	T_SIGNED,
	T_NULL,
	T_IF, T_ELSE,
	T_WHILE, T_RETURN,
	T_FOR, T_GOTO, T_CONTINUE, T_BREAK,
	T_SWITCH, T_CASE, T_DEFAULT,
	T_NUMBER,
	T_STRING,
	T_CHARCONST,
	T_ID,
	T_END,
	T_ILLEGAL,
};

char *tokens[] = {
	"T_ASSIGNPLUS",
	"T_ASSIGNSUB",
	"T_ASSIGNMUL",
	"T_ASSIGNDIV",
	"T_ASSIGNMOD",
	"T_ASSIGNNOT",
	"T_ASSIGNAND",
	"T_ASSIGNOR",
	"T_ASSIGNXOR",
	"T_INC",
	"T_DEC",
	"T_NEQ",
	"T_LTEQ",
	"T_GTEQ",
	"T_EQ",
	"T_LSHIFT",
	"T_RSHIFT",
	"T_AND",
	"T_OR",
	"T_CONST",
	"T_DO",
	"T_ENUM",
	"T_INTERN",
	"T_EXTERN",
	"T_REGISTER",
	"T_SIZEOF",
	"T_STRUCT",
	"T_TYPEDEF",
	"T_UNION",
	"T_STATIC",
	"T_INT", "T_CHAR", "T_VOID",
	"T_DOUBLE", "T_FLOAT", "T_LONG", "T_SHORT",
	"T_UNSIGNED",
	"T_SIGNED",
	"T_NULL",
	"T_IF", "T_ELSE",
	"T_WHILE", "T_RETURN",
	"T_FOR", "T_GOTO", "T_CONTINUE", "T_BREAK",
	"T_SWITCH", "T_CASE", "T_DEFAULT",
	"T_NUMBER",
	"T_STRING",
	"T_CHARCONST",
	"T_ID",
	"T_END",
	"T_ILLEGAL",
};

enum SEGMENTS {
	S_ARGUMENT,
	S_LOCAL,
	S_TEMP,
	S_STATIC,
	S_CONSTANT,
};

char *segments[] = {
	"argument",
	"local",
	"temp",
	"static",
	"constant",
};

enum NODES {
	N_SUBROUTINEDEC = 1,
	N_PARAMETERLIST,
	N_SUBROUTINEBODY,
	N_VARDEC,
	N_STATEMENTS,
	N_IFSTATEMENT,
	N_ELSESTATEMENT,
	N_WHILESTATEMENT,
	N_RETURNSTATEMENT,
	N_EXPRESSIONLIST,
	N_EXPRESSION,
	N_TERM,
	N_SUBROUTINECALL,
	N_OP,
	N_UNOP,
	N_INTCONST,
	N_STRINGCONST,
	N_KEYCONST,
	N_STORAGEQUALIFIER,
	N_TYPE,
	N_VARNAME,
	N_SUBROUTINENAME,
};

char *nodes[] = {
	0,
	"SUBROUTINEDEC",
	"PARAMETERLIST",
	"SUBROUTINEBODY",
	"VARDEC",
	"STATEMENTS",
	"IFSTATEMENT",
	"ELSESTATEMENT",
	"WHILESTATEMENT",
	"RETURNSTATEMENT",
	"EXPRESSIONLIST",
	"EXPRESSION",
	"TERM",
	"SUBROUTINECALL",
	"OP",
	"UNOP",
	"INTCONST",
	"STRINGCONST",
	"KEYCONST",
	"STORAGEQUALIFIER",
	"TYPE",
	"VARNAME",
	"SUBROUTINENAME",
};

char *keyword[] = {
	"const",
	"do",
	"enum",
	"intern",
	"extern",
	"register",
	"sizeof",
	"struct",
	"typedef",
	"union",
	"static",
	"int", "char", "void",
	"double", "float", "long", "short",
	"unsigned",
	"signed",
	"null",
	"if", "else",
	"while", "return",
	"for", "goto", "continue", "break",
	"switch", "case", "default",
};

size_t keyword_length[] = {
	STRLEN("const"),
	STRLEN("do"),
	STRLEN("enum"),
	STRLEN("intern"),
	STRLEN("extern"),
	STRLEN("register"),
	STRLEN("sizeof"),
	STRLEN("struct"),
	STRLEN("typedef"),
	STRLEN("union"),
	STRLEN("static"),
	STRLEN("int"), STRLEN("char"), STRLEN("void"),
	STRLEN("double"), STRLEN("float"), STRLEN("long"), STRLEN("short"),
	STRLEN("unsigned"),
	STRLEN("signed"),
	STRLEN("null"),
	STRLEN("if"), STRLEN("else"),
	STRLEN("while"), STRLEN("return"),
	STRLEN("for"), STRLEN("goto"), STRLEN("continue"), STRLEN("break"),
	STRLEN("switch"), STRLEN("case"), STRLEN("default"),
};

enum OPERATORS {
	OP_ADD = 0,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_AND,
	OP_OR,
	OP_LT,
	OP_GT,
	OP_EQ,
	OP_NOT,
	OP_NEG,
	OP_INDEX,
	OP_COMMA,
};

char *operators[] = { "+", "-", "*", "/", "&", "|", "<", ">", "=", "~", "-", "[", "," };

typedef struct {
	char *base;
	char *free;
	char **pages;
	size_t cur;
	size_t cap;
} Strpool;

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

Lexer lexer;

/* parsing */
int lex(void);
void parse(void);

int lex(void) {
	char *tp, *s;
	int check;

	lexer.text_s = lexer.text_e = 0;

	if(lexer.ptr[0] == '/' && lexer.ptr[1] == '/') { /* single-line comments */
		while(*lexer.ptr != '\n') ++lexer.ptr;
		lexer.col = 1;
		++lexer.line;
		++lexer.ptr;
		lexer.line_s = lexer.ptr;
	} else if(lexer.ptr[0] == '/' && lexer.ptr[1] == '*') { /* multi-line comments */
		while(!(lexer.ptr[0] == '*' && lexer.ptr[1] == '/')) lexer.line += (*lexer.ptr++ == '\n');
		lexer.ptr += 2;
	}

	while(isspace(*lexer.ptr)) { /* whitespace */
		if(*lexer.ptr != '\n') {
			++lexer.col;
		} else {
			lexer.col = 1;
			++lexer.line;
			lexer.line_s = lexer.ptr + 1;
		}
		++lexer.ptr;
	}

	if(lexer.ptr >= lexer.line_e) {
		lexer.line_s = lexer.line_e;
		for(s = lexer.ptr; *s && *s != '\n'; ++s);
		lexer.line_e = s + 1;
	}

	tp = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0)
		return T_END;

	lexer.token = 0;

	/* symbol */
	if(tp[1] == '=') {
		switch(*tp) {
		case '+':
			lexer.token = T_ASSIGNPLUS;
			break;
		case '-':
			lexer.token = T_ASSIGNSUB;
			break;
		case '*':
			lexer.token = T_ASSIGNMUL;
			break;
		case '/':
			lexer.token = T_ASSIGNDIV;
			break;
		case '%':
			lexer.token = T_ASSIGNMOD;
			break;
		case '&':
			lexer.token = T_ASSIGNAND;
			break;
		case '|':
			lexer.token = T_ASSIGNOR;
			break;
		case '~':
			lexer.token = T_ASSIGNNOT;
			break;
		case '!':
			lexer.token = T_NEQ;
			break;
		case '<':
			lexer.token = T_LTEQ;
			break;
		case '>':
			lexer.token = T_GTEQ;
			break;
		case '=':
			lexer.token = T_EQ;
			break;
		}
		lexer.ptr += 2;
		lexer.col += 2;
		return lexer.token;
	}

	switch(*tp) {
	case '{': case '}': case '(': case ')': case '[': case ']': case '.':
	case ',': case ';': case '=': case '~': case '*': case '/': case '<':
	case '>': case '&': case '|': case '+': case '-':
		++lexer.col;
		++lexer.ptr;
		lexer.token = *tp;
	}

	if(lexer.token) {
		if(tp[1] == tp[0] && *tp != '*') {
			++lexer.col;
			++lexer.ptr;
			switch(*tp) {
			case '<':
				lexer.token = T_LSHIFT;
				break;
			case '>':
				lexer.token = T_RSHIFT;
				break;
			case '&':
				lexer.token = T_AND;
				break;
			case '|':
				lexer.token = T_OR;
				break;
			case '+':
				lexer.token = T_INC;
				break;
			case '-':
				lexer.token = T_DEC;
				break;
			}
		}
		return lexer.token;
	}

	/* keyword */
	for(int i = 0; i < ARRLEN(keyword); ++i) {
		check = tp[keyword_length[i]];
		if(strstr(tp, keyword[i]) == tp &&
				(check == ' ' ||
				 check == '\t'||
				 check == '\n'||
				 check == ';' ||
				 check == '(' ||
				 check == ')' ||
				 check == ',' ||
				 check == '.' ||
				 check == '{' ||
				 check == '}')
		  )
		{
			lexer.ptr += keyword_length[i];
			lexer.col += keyword_length[i];
			return (lexer.token = T_CONST + i);
		}
	}

	/* numbers, strings and identifiers */
	s = tp;
	if(s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) { /* hex */
		s += 2;
		for(check = 0; *s && (isdigit(*s) || (*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'F')); ++s)
			++check;
	} else if(s[0] == '0' && (s[1] == 'b' || s[1] == 'B')) { /* binary */
		s += 2;
		for(check = 0; *s && (*s == '0' || *s == '1'); ++s)
			++check;
	} else if(s[0] == '0' && isdigit(s[1])) { /* octal */
		++s;
		for(check = 0; *s && *s >= '0' && *s <= '7'; ++s)
			++check;
	} else {
		for(check = 0, s = tp; *s && isdigit(*s); ++s)
			++check;
	}
	if(check) {
		lexer.col += s - tp;
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		if(!check) // TODO
			error(1, 0, "mismatched double quotes");
		lexer.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STRING);
	}

	if(*tp == '\'') {
		for(s = tp + 1; (check = *s) && *s != '\''; ++s);
		if(!check) // TODO
			error(1, 0, "mismatched single quotes");
		lexer.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_CHARCONST);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = T_ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.col += s - tp;
	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;
	return (lexer.token = T_ID);
}

int main(void) {
	Fmap fm;
	fmapopen("test.c", O_RDONLY, &fm);
	fmapread(&fm);
	lexer.ptr = lexer.src = fm.buf;
	lexer.line = lexer.col = 1;
	lexer.line_s = lexer.ptr;
	for(lexer.line_e = lexer.ptr; *lexer.line_e != '\n'; ++lexer.line_e);
	++lexer.line_e;

	while(lex() != T_END) {
		printf("TOKEN\n");
		if(lexer.token < T_ASSIGNPLUS)
			printf("\ttoken: %c\n", lexer.token);
		else
			printf("\ttoken: %s\n", tokens[lexer.token - T_ASSIGNPLUS]);
		printf("\tline_num: %i\n", lexer.line);
		printf("\tcol_num: %i\n", lexer.col - (int)(lexer.ptr - lexer.unget));
		printf("\ttoken_text: ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stdout);
		printf("\n");
		printf("\tline_text: ");
		fwrite(lexer.line_s, 1, lexer.line_e - lexer.line_s, stdout);
		printf("\n\n");
	}
	fmapclose(&fm);
	return 0;
}
