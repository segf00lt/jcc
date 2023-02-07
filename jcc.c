#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
//#include <error.h>
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
	T_ARROW = 256,
	T_ASSIGNPLUS,
	T_ASSIGNSUB,
	T_ASSIGNMUL,
	T_ASSIGNDIV,
	T_ASSIGNMOD,
	T_ASSIGNNOT,
	T_ASSIGNAND,
	T_ASSIGNOR,
	T_ASSIGNXOR,
	T_BAR,
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

	T_ENUM,
	T_REGISTER,
	T_SIZEOF,
	T_STRUCT,
	T_UNION,
	T_INLINE,
	T_DEFER,

	T_INT, T_CHAR, T_VOID, T_BOOL,
	T_DOUBLE, T_FLOAT, T_LONG, T_SHORT,
	T_U8, T_U16, T_U32, T_U64,
	T_S8, T_S16, T_S32, T_S64,

	T_IF, T_ELSE,
	T_WHILE, T_RETURN,
	T_FOR, T_GOTO, T_CONTINUE, T_BREAK,
	T_SWITCH, T_CASE, T_DEFAULT,

	T_NUMBER,
	T_STRING,
	T_CHARACTER,
	T_TRUE, T_FALSE,

	T_ID,
	T_END,
	T_ILLEGAL,
};

char *tokens_debug[] = {
	"T_ARROW",
	"T_ASSIGNPLUS",
	"T_ASSIGNSUB",
	"T_ASSIGNMUL",
	"T_ASSIGNDIV",
	"T_ASSIGNMOD",
	"T_ASSIGNNOT",
	"T_ASSIGNAND",
	"T_ASSIGNOR",
	"T_ASSIGNXOR",
	"T_BAR",
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
	"T_ENUM",
	"T_REGISTER",
	"T_SIZEOF",
	"T_STRUCT",
	"T_UNION",
	"T_INLINE",
	"T_DEFER",
	"T_INT", "T_CHAR", "T_VOID", "T_BOOL",
	"T_DOUBLE", "T_FLOAT", "T_LONG", "T_SHORT",
	"T_U8", "T_U16", "T_U32", "T_U64",
	"T_S8", "T_S16", "T_S32", "T_S64",
	"T_IF", "T_ELSE",
	"T_WHILE", "T_RETURN",
	"T_FOR", "T_GOTO", "T_CONTINUE", "T_BREAK",
	"T_SWITCH", "T_CASE", "T_DEFAULT",
	"T_NUMBER",
	"T_STRING",
	"T_CHARACTER",
	"T_TRUE", "T_FALSE",
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
	N_DECLARATION = 1,
	N_CONSTANT,
	N_DEFER,
	N_FUNCTION,
	N_STRUCTURE,
	N_UNIONTURE,
	N_ENUMERATION,
	N_PARAMLIST,
	N_BLOCK,
	N_STATEMENT,
	N_IFSTATEMENT,
	N_ELSESTATEMENT,
	N_WHILESTATEMENT,
	N_RETURNSTATEMENT,
	N_FORSTATEMENT,
	N_EXPRESSIONLIST,
	N_EXPRESSION,
	N_ARITH,
	N_FACTOR,
	N_TERM,
	N_CALL,
	N_POINTER,
	N_ARRAY,
	N_OP,
	N_UNOP,
	N_INTCONST,
	N_FLOATCONST,
	N_STRCONST,
	N_CHARCONST,
	N_BOOLCONST,
	N_STORAGEQUALIFIER,
	N_TYPE,
	N_ID,
};

char *nodes_debug[] = {
	0,
	"DECLARATION",
	"CONSTANT",
	"DEFER",
	"FUNCTION",
	"STRUCTURE",
	"UNIONTURE",
	"ENUMERATION",
	"PARAMLIST",
	"BLOCK",
	"STATEMENT",
	"IFSTATEMENT",
	"ELSESTATEMENT",
	"WHILESTATEMENT",
	"RETURNSTATEMENT",
	"FORSTATEMENT",
	"EXPRESSIONLIST",
	"EXPRESSION",
	"ARITH",
	"FACTOR",
	"TERM",
	"CALL",
	"POINTER",
	"ARRAY",
	"OP",
	"UNOP",
	"INTCONST",
	"FLOATCONST",
	"STRCONST",
	"CHARCONST",
	"BOOLCONST",
	"STORAGEQUALIFIER",
	"TYPE",
	"ID",
};

char *keyword[] = {
	"enum",
	"register",
	"sizeof",
	"struct",
	"union",
	"inline",
	"defer",
	"int", "char", "void", "bool",
	"double", "float", "long", "short",
	"u8", "u16", "u32", "u64",
	"s8", "s16", "s32", "s64",
	"if", "else",
	"while", "return",
	"for", "goto", "continue", "break",
	"switch", "case", "default",
};

size_t keyword_length[] = {
	STRLEN("enum"),
	STRLEN("register"),
	STRLEN("sizeof"),
	STRLEN("struct"),
	STRLEN("union"),
	STRLEN("inline"),
	STRLEN("defer"),
	STRLEN("int"), STRLEN("char"), STRLEN("void"), STRLEN("bool"),
	STRLEN("double"), STRLEN("float"), STRLEN("long"), STRLEN("short"),
	STRLEN("u8"), STRLEN("u16"), STRLEN("u32"), STRLEN("u64"),
	STRLEN("s8"), STRLEN("s16"), STRLEN("s32"), STRLEN("s64"),
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
	int line;
	int col;
	char *line_s;
	char *line_e;
} Info;

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
	Info info;
} Lexer;

typedef struct syntax_node {
	unsigned int id; /* debug */
	int kind;
	char *val;
	struct syntax_node *down; /* down */
	struct syntax_node *next; /* across */
	Info info;
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
	Info info;
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
AST ast;
Strpool spool;
Sym_tab globaltab;
Sym_tab filetab;
Sym_tab functab;
Sym_tab blocktab[8]; /* hard limit on amount of nesting that can be done */
size_t depth;

/* utility functions */
void cleanup(void);
char* strpool_alloc(Strpool *pool, size_t len, char *s);
void strpool_free(Strpool *pool);
void debug_ast_alloc(AST *ast);
void debug_parser(AST_node *node, size_t depth);
void debug_sym_tab(AST_node *node);

/* AST functions */
AST_node* ast_alloc_node(AST *ast, int kind, char *val, Info *info);
void ast_free(AST *ast);

/*
 * NOTE maybe parse expressions with shunting yard
 */

/* parsing */
int lex(void);
void parse(void);
AST_node* declaration(void);
AST_node* initializer(void);
AST_node* type(void);
AST_node* function(void);
AST_node* vardec(void);
AST_node* structure(void);
AST_node* unionation(void);
AST_node* enumeration(void);
AST_node* block(void);
AST_node* paramlist(void);
AST_node* statement(void);
AST_node* ifstatement(void);
AST_node* whilestatement(void);
AST_node* forstatement(void);
AST_node* returnstatement(void);
AST_node* expressionlist(void);
AST_node* expression(void);
//AST_node* arith(void);
//AST_node* factor(void);
//AST_node* term(void);
AST_node* call(void);

/* symbol table functions */
void sym_tab_build(Sym_tab *tab, AST_node *node);
void sym_tab_grow(Sym_tab *tab);
void sym_tab_def(Sym_tab *tab, Sym *symbol);
size_t sym_tab_hash(Sym_tab *tab, char *name);
Sym* sym_tab_prevt(Sym_tab *tab, char *name);
void sym_tab_clear(Sym_tab *tab);
void sym_tab_print(Sym_tab *tab);

char* strpool_alloc(Strpool *pool, size_t len, char *s) {
	size_t count, i;
	char *p;

	count = pool->free - pool->base;
	if(count + len >= STRPOOL_PAGE_SIZE) {
		++pool->cur;
		if(pool->cur >= pool->cap) {
			pool->cap <<= 1;
			pool->pages = realloc(pool->pages, pool->cap * sizeof(char*));
		}
		pool->pages[pool->cur] = malloc(STRPOOL_PAGE_SIZE);
		pool->base = pool->free = pool->pages[pool->cur];
	}

	for(i = 0; i < len; ++i) pool->free[i] = s[i];
	pool->free[i] = 0;

	p = pool->free;
	pool->free += i + 1;

	return p;
}

void strpool_free(Strpool *pool) {
	for(size_t i = 0; i <= pool->cur; ++i)
		free(pool->pages[i]);
	free(pool->pages);
}

/*
void debug_ast_alloc(AST *ast) {
	for(size_t i = 0; i <= ast->cur; ++i) {
		fprintf(stderr, "PAGE %zu\n", i);
		for(size_t j = 0; ast->pages[i][j].kind; ++j) {
			fprintf(stderr, "\taddress: %p\n"
					"\tid: %u\n"
					"\tkind: %s\n"
					"\tval: %s\n"
					"\tdown: %p\n"
					"\tnext: %p\n"
					"\tline: %i\n"
					"\tcol: %i\n",
					(void*)(ast->pages[i]+j),
					ast->pages[i][j].id,
					nodes[ast->pages[i][j].kind],
					ast->pages[i][j].val,
					(void*)(ast->pages[i][j].down),
					(void*)(ast->pages[i][j].next),
					ast->pages[i][j].info.line,
					ast->pages[i][j].info.col);
		}
	}
}

void debug_parser(AST_node *node, size_t depth) {
	for(; node; node = node->next) {
		int i;
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "id: %u\n", node->id);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "kind: %s\n", nodes[node->kind]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "val: %s\n", node->val);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "down: %u\n", node->down ? node->down->id : 0);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "next: %u\n\n", node->next ? node->next->id : 0);
		if(node->down) {
			++depth;
			debug_parser(node->down, depth);
			--depth;
		}
	}
}

void debug_sym_tab(AST_node *node) {
	sym_tab_build(&classtab, node);
	fprintf(symout, "CLASS SYMBOLS: %s\n\n", classtab.name);
	sym_tab_print(&classtab);
	for(node = ast.subroutines; node; node = node->next) {
		sym_tab_build(&functab, node);
		fprintf(symout, "\nSUBROUTINE SYMBOLS: %s\n\n", functab.name);
		sym_tab_print(&functab);
	}
	sym_tab_clear(&classtab);
	sym_tab_clear(&functab);
}
*/

AST_node* ast_alloc_node(AST *ast, int kind, char *val, Info *info) {
	if(ast->free - ast->base >= AST_PAGE_SIZE) {
		++ast->cur;
		if(ast->cur >= ast->cap) {
			ast->cap <<= 1;
			ast->pages = realloc(ast->pages, ast->cap * sizeof(AST_node*));
		}
		ast->pages[ast->cur] = malloc(AST_PAGE_SIZE * sizeof(AST_node));
		ast->base = ast->free = ast->pages[ast->cur];
	}

	*(ast->free) =
		(AST_node){
			.id = ast->nodecount++,
			.kind = kind,
			.val = val,
			.down = NULL,
			.next = NULL,
			.info = *info
		};
	return ast->free++;
}

void ast_free(AST *ast) {
	for(size_t i = 0; i <= ast->cur; ++i)
		free(ast->pages[i]);
	free(ast->pages);
}

//void sym_tab_build(Sym_tab *tab, AST_node *node) {
//	AST_node *child;
//	Sym symbol;
//	size_t *tmp;
//
//	switch(node->kind) {
//	case N_CLASS:
//		tab->name = node->val;
//		node = node->down;
//		if(node->kind != N_CLASSVARDEC)
//			break;
//
//		for(; node && node->kind == N_CLASSVARDEC; node = node->next) {
//			child = node->down;
//			assert(child->kind == N_STORAGEQUALIFIER);
//			if(child->val == keyword[T_FIELD - T_CLASS]) {
//				symbol.seg = S_THIS;
//				tmp = &(tab->this_count);
//			} else if(child->val == keyword[T_STATIC - T_CLASS]) {
//				symbol.seg = S_STATIC;
//				tmp = &(tab->static_count);
//			}
//
//			child = child->next;
//			assert(child->kind == N_TYPE);
//			symbol.type = child->val;
//			while((child = child->next)) {
//				symbol.pos = (*tmp)++;
//				assert(child->kind == N_VARNAME);
//				symbol.name = child->val;
//				sym_tab_def(tab, &symbol);
//			}
//		}
//		break;
//	case N_SUBROUTINEDEC:
//		node = node->down;
//		assert(node->kind == N_STORAGEQUALIFIER);
//		if(node->val == keyword[T_METHOD - T_CLASS]) {
//			symbol = (Sym){
//				.pos = tab->arg_count++,
//				.seg = S_ARGUMENT,
//				.type = keyword[T_THIS - T_CLASS],
//				.name = keyword[T_THIS - T_CLASS],
//			};
//			sym_tab_def(tab, &symbol);
//		}
//
//		node = node->next->next;
//		assert(node->kind == N_SUBROUTINENAME);
//		tab->name = node->val;
//
//		node = node->next;
//		assert(node->kind == N_PARAMETERLIST);
//
//		symbol.seg = S_ARGUMENT; /* the segment will be ARG for the duration of the loop */
//		for(child = node->down; child; child = child->next) {
//			symbol.pos = tab->arg_count++;
//			assert(child->kind == N_TYPE);
//			symbol.type = child->val;
//			child = child->next;
//			assert(child->kind == N_VARNAME);
//			symbol.name = child->val;
//			sym_tab_def(tab, &symbol);
//		}
//
//		node = node->next;
//		assert(node->kind == N_SUBROUTINEBODY);
//		node = node->down;
//		if(node->kind != N_VARDEC)
//			break;
//
//		symbol.seg = S_LOCAL;
//		for(; node && node->kind == N_VARDEC; node = node->next) {
//			child = node->down;
//			assert(child->kind == N_TYPE);
//			symbol.type = child->val;
//			while((child = child->next)) {
//				symbol.pos = tab->local_count++;
//				assert(child->kind == N_VARNAME);
//				symbol.name = child->val;
//				sym_tab_def(tab, &symbol);
//			}
//		}
//		break;
//	}
//}
//
//void sym_tab_grow(Sym_tab *tab) {
//	Sym *old_data = tab->data;
//	size_t old_cap = tab->cap;
//	tab->data = calloc((tab->cap <<= 1), sizeof(Sym));
//	for(size_t i = 0; i < old_cap; ++i) {
//		if(!old_data[i].name)
//			continue;
//		sym_tab_def(tab, old_data + i);
//	}
//	free(old_data);
//}
//
//size_t sym_tab_hash(Sym_tab *tab, char *name) {
//	size_t hash = 0;
//	while(*name) hash += *(name++) * 31;
//	return hash % tab->cap;
//}
//
//void sym_tab_def(Sym_tab *tab, Sym *symbol) {
//	if(tab->count >= tab->cap) sym_tab_grow(tab);
//	size_t i = sym_tab_hash(tab, symbol->name);
//	while(tab->data[i].name) {
//		if(!strcmp(tab->data[i].name, symbol->name))
//			return;
//		i = (i + 1) % tab->cap;
//	}
//	tab->data[i] = *symbol;
//	++tab->count;
//}
//
//Sym* sym_tab_prevt(Sym_tab *tab, char *name) {
//	size_t i, origin;
//	origin = i = sym_tab_hash(tab, name);
//	do {
//		if(tab->data[i].name && !strcmp(tab->data[i].name, name))
//			return tab->data + i;
//		i = (i + 1) % tab->cap;
//	} while(i != origin);
//	return 0;
//}
//
//void sym_tab_clear(Sym_tab *tab) {
//	/* NOTE make sure you still have a pointer to the string pool when you call this */
//	tab->count = tab->static_count = tab->this_count = tab->arg_count = tab->local_count = 0;
//	for(size_t i = 0; i < tab->cap; ++i) tab->data[i].name = 0;
//}
//
//void sym_tab_print(Sym_tab *tab) {
//	for(size_t i = 0; i < tab->cap; ++i) {
//		if(tab->data[i].name == 0) continue;
//		fprintf(symout, "SYMBOL\n\tname: %s\n\ttype: %s\n\tseg: %s\n\tpos: %i\n\n",
//				tab->data[i].name, tab->data[i].type,
//				segments[tab->data[i].seg], tab->data[i].pos);
//	}
//}

void parse_error(Lexer *l, char *expect) {
	fprintf(stderr, "jcc: parse error: expected %s got '", expect);
	fwrite(l->text_s, 1, l->text_e - l->text_s, stderr);
	fprintf(stderr, "'\n>\tline: %i, col: %i\n>\t", l->info.line, l->info.col - (int)(l->ptr - l->unget));
	fwrite(l->info.line_s, 1, l->info.line_e - l->info.line_s, stderr);
	exit(1);
}

void myerror(char *fmt, ...) {
	va_list args;

	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(1);
}

int lex(void) {
	char *tp, *s;
	int check;

	lexer.text_s = lexer.text_e = NULL;

	while(1) {
		while(isspace(*lexer.ptr)) { /* whitespace */
			if(*lexer.ptr != '\n') {
				++lexer.info.col;
			} else {
				lexer.info.col = 1;
				++lexer.info.line;
				lexer.info.line_e = lexer.info.line_s = lexer.ptr + 1;
			}
			++lexer.ptr;
		}

		/* single-line comments and prepocessor lines */
		if((lexer.ptr[0] == '/' && lexer.ptr[1] == '/') || lexer.ptr[0] == '#') {
			while(*lexer.ptr != '\n')
				++lexer.ptr;
			lexer.info.col = 1;
		} else if(lexer.ptr[0] == '/' && lexer.ptr[1] == '*') { /* multi-line comments */
			while(!(lexer.ptr[0] == '*' && lexer.ptr[1] == '/'))
				lexer.info.line += (*lexer.ptr++ == '\n');
			lexer.ptr += 2;
		} else
			break;
	}

	if(lexer.ptr >= lexer.info.line_e) {
		lexer.info.line_s = lexer.info.line_e;
		for(s = lexer.ptr; *s && *s != '\n'; ++s);
		lexer.info.line_e = s + 1;
	}

	tp = lexer.text_s = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0)
		return T_END;

	lexer.token = 0;

	if(tp[0] == '-' && tp[1] == '-' && tp[2] == '-')
		return (lexer.token = T_BAR);

	if(tp[0] == '-' && tp[1] == '>')
		return (lexer.token = T_ARROW);

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
		lexer.text_e = tp + 2;
		lexer.ptr += 2;
		lexer.info.col += 2;
		return lexer.token;
	}

	switch(*tp) {
	case '{': case '}': case '(': case ')': case '[': case ']': case '.':
	case ',': case ';': case '=': case '~': case '*': case '/': case '<':
	case '>': case '&': case '|': case '+': case '-':
		++lexer.info.col;
		++lexer.ptr;
		lexer.token = *tp;
	}

	if(lexer.token) {
		if(tp[1] == tp[0] && *tp != '*') {
			++lexer.info.col;
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
			lexer.text_e = tp + 2;
		} else
			lexer.text_e = tp + 1;

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
			lexer.text_e = tp + keyword_length[i];
			lexer.ptr += keyword_length[i];
			lexer.info.col += keyword_length[i];
			return (lexer.token = T_ENUM + i);
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
		lexer.info.col += s - tp;
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		lexer.info.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STRING);
	}

	if(*tp == '\'') {
		for(s = tp + 1; (check = *s) && *s != '\''; ++s);
		lexer.info.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_CHARACTER);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = T_ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.info.col += s - tp;
	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;
	return (lexer.token = T_ID);
}

int prevtahead(int expect) {
	register int t;
	if((t = lex()) != expect) {
		parse_error(&lexer, tokens_debug[expect - T_ASSIGNPLUS]);
	}
	return t;
}

void parse(void) {
}

/*
 * declaration: identifier (',' identifier)* ':' type? ('='|':') (';'|intconst ';'|floatconst ';'|strconst ';'|keyconst ';'|function|structure|unionation|enumeration)
 */
AST_node* declaration(void) {
	register int t, prevt;
	AST_node *root, *child;

	prevt = 0;

	t = lex();

	if(t != T_ID)
		parse_error(&lexer, "identifier");

	root = ast_alloc_node(&ast, N_DECLARATION, NULL, &lexer.info);

	root->down = child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
	child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	while((t = lex()) == ',') {
		t = lex();
		if(t != T_ID)
			parse_error(&lexer, "identifier");
		child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child = child->next;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	if(t != ':')
		parse_error(&lexer, "':'");

	while(1) {
		t = lex();

		if(t == '*') {
			child->next = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
			child = child->next;
			continue;
		}

		if(t != '[')
			break;

		prevt = t = lex();

		if(t != ']' && t != T_NUMBER & t != T_ID)
			parse_error(&lexer, "']', integer constant, or identifier");

		t = lex();

		if(prevt != ']' && t != ']')
			parse_error(&lexer, "']'");

		child->next = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);
		child = child->next;

		if(prevt != ']')
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	if((t >= T_INT && t <= T_S64) || t == T_ID) {
		child->next = ast_alloc_node(&ast, N_TYPE, NULL, &lexer.info);
		child = child->next;

		if(t == T_ID)
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		else
			child->val = keyword[t - T_ENUM];

		t = lex();
	}

	if(t == ':') {
		child->next = ast_alloc_node(&ast, N_CONSTANT, NULL, &lexer.info);
		child = child->next;
	} else if(t != '=')
		parse_error(&lexer, "'=' or ':'");

	t = lex();

	if((t >= T_NUMBER  && t <= T_FALSE) || t == T_BAR || t == T_ID) {
		child->next = ast_alloc_node(&ast, 0, NULL, &lexer.info);
		child = child->next;
	}

	switch(t) {
	case T_NUMBER:
		child->kind = N_INTCONST;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		break;
	case T_STRING:
		child->kind = N_STRCONST;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		break;
	case T_CHARACTER:
		child->kind = N_CHARCONST;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		break;
	case T_TRUE: case T_FALSE:
		child->kind = N_BOOLCONST;
		child->val = keyword[t - T_ENUM];
		t = lex();
		break;
	case T_BAR:
		child->kind = N_UNINIT;
		t = lex();
		break;
	case T_ID:
		child->kind = N_ID;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		break;
	}

	if(t == ';')
		return root;

	/* body */
	if((child->next = function())) return root;
	if((child->next = structure())) return root;
	if((child->next = unionation())) return root;
	if((child->next = enumeration())) return root;

	parse_error(&lexer, "initializer or body");

	return NULL;
}

/*
 * initializer:
 */
AST_node* initializer(void) {
	register int t;
	AST_node *root;

	t = lex();

	if((t >= T_NUMBER  && t <= T_FALSE) || t == T_BAR || t == T_ID)
		root = ast_alloc_node(&ast, 0, NULL, &lexer.info);

	switch(t) {
	case T_NUMBER:
		root->kind = N_INTCONST;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_STRING:
		root->kind = N_STRCONST;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_CHARACTER:
		root->kind = N_CHARCONST;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_TRUE: case T_FALSE:
		root->kind = N_BOOLCONST;
		root->val = keyword[t - T_ENUM];
		break;
	case T_BAR:
		root->kind = N_UNINIT;
		break;
	case T_ID:
		root->kind = N_ID;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	}

	if(!root)
		parse_error(&lexer, "initializer");

	return root;
}

/*
 * type: (pointer|array)* ('int'|'float'|'char'|...|'bool'|('('type (',' type)* ')' ('->' type)?))
 * NOTE function pointers are NOT allowed for functions that take or return function pointers
 */
AST_node* type(void) {
	register int t, prevt, loop;
	AST_node *root, *child;
	char *s, *e;

	loop = prevt = 0;
	t = lex();

	if(t == '*') {
		root = child = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
		loop = 1;
	} else if(t == '[') {
		prevt = t = lex();

		if(t != ']' && t != T_NUMBER & t != T_ID)
			parse_error(&lexer, "']', integer constant, or identifier");

		t = lex();

		if(prevt != ']' && t != ']')
			parse_error(&lexer, "']'");

		root = child = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);

		if(prevt != ']')
			root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		loop = 1;
	}

	while(loop) {
		t = lex();

		if(t == '*') {
			child->next = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
			child = child->next;
			continue;
		}

		if(t != '[')
			break;

		prevt = t = lex();

		if(t != ']' && t != T_NUMBER & t != T_ID)
			parse_error(&lexer, "']', integer constant, or identifier");

		t = lex();

		if(prevt != ']' && t != ']')
			parse_error(&lexer, "']'");

		child->next = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);
		child = child->next;

		if(prevt != ']')
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	if(!(t >= T_INT && t <= T_S64) && t != T_ID && t != '(')
		parse_error(&lexer, "type");

	child->next = ast_alloc_node(&ast, N_TYPE, NULL, &lexer.info);
	child = child->next;
	if(t != T_ID && t != '(')
		child->val = keyword[t - T_ENUM];
	else if(t != '(')
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	else {
		/* no one in their right mind would ever make a function pointer to a function
		 * that takes a function pointer 
		 * if you think you would do something like that go away
		 */
		s = lexer.text_s;
		while(1) {
			while(1) {
				t = lex();

				if(t == '*')
					continue;

				if(t != '[')
					break;

				prevt = t = lex();

				if(t != ']' && t != T_NUMBER & t != T_ID)
					parse_error(&lexer, "']', integer constant, or identifier");

				t = lex();

				if(prevt != ']' && t != ']')
					parse_error(&lexer, "']'");
			}

			if(!(t >= T_INT && t <= T_S64) && t != T_ID)
				parse_error(&lexer, "builtin type, struct or union");

			t = lex();

			if(t == ',')
				continue;
			if(t == ')')
				break;

			parse_error(&lexer, "',' or ')'");
		}

		t = lex();
		if(t == T_ARROW) {
			while(1) {
				t = lex();

				if(t == '*')
					continue;

				if(t != '[')
					break;

				prevt = t = lex();

				if(t != ']' && t != T_NUMBER & t != T_ID)
					parse_error(&lexer, "']', integer constant, or identifier");

				t = lex();

				if(prevt != ']' && t != ']')
					parse_error(&lexer, "']'");
			}

			if(!(t >= T_INT && t <= T_S64) && t != T_ID)
				parse_error(&lexer, "builtin type, struct or union");
		}
		e = lexer.text_e;
		child->val = strpool_alloc(&spool, e - s, s);
	}

	return root;
}

/*
 * function: '(' paramlist ')' ('->' type)? ('inline'? '{' block '}')
 */
AST_node* function(void) {
	register int t;
	AST_node *root, *child;

	t = lex();

	if(t != '(') {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_FUNCTION, NULL, &lexer.info);

	t = lex();
	if(t != '(')
		parse_error(&lexer, "'('");
	root->down = child = paramlist();
	t = lex();
	if(t != ')')
		parse_error(&lexer, "')'");

	t = lex();

	if(t == T_ARROW) {
		child->next = type();
		child = child->next;
	}

	t = lex();

	if(t == T_INLINE) {
		child->next = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[T_INLINE - T_ENUM], &lexer.info);
		child = child->next;
		t = lex();
	}

	if((t = lex()) != '{')
		parse_error(&lexer, "'{'");

	child->next = block();

	if((t = lex()) != '}')
		parse_error(&lexer, "'}'");

	return root;
}

/*
 * paramlist: identifier (',' identifier)* ':' type((',' identifier)+ ':' type)*
 */
AST_node* paramlist(void) {
	register int t;
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_PARAMLIST, NULL, &lexer.info);

	t = lex();
	if(t != T_ID)
		parse_error(&lexer, "identifier");
	root->down = child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
	child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	while((t = lex()) == ',') {
		t = lex();
		if(t != T_ID)
			parse_error(&lexer, "identifier");
		child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child = child->next;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	if(t != ':')
		parse_error(&lexer, "':'");

	child->next = type();
	child = child->next;

	while(1) {
		t = lex();
		if(t != ',')
			break;
		while(t == ',') {
			t = lex();
			if(t != T_ID)
				parse_error(&lexer, "identifier");
			child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
			child = child->next;
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
			t = lex();
		}

		if(t != ':')
			parse_error(&lexer, "':'");

		child->next = type();
		child = child->next;
	}

	lexer.info.col -= lexer.ptr - lexer.unget;
	lexer.ptr = lexer.unget;

	return root;
}

/*
 * block: (statement|ifstatement|whilestatement|forstatement|returnstatement|declaration)*
 */
AST_node* block(void) {
	register int t;
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_BLOCK, NULL, &lexer.info);

	if((child = statement())) root->down = child;
	else if((child = ifstatement())) root->down = child;
	else if((child = whilestatement())) root->down = child;
	else if((child = forstatement())) root->down = child;
	else if((child = returnstatement())) root->down = child;
	else if((child = function())) root->down = child;
	else if((child = structure())) root->down = child;
	else if((child = unionation())) root->down = child;
	else if((child = enumeration())) root->down = child;

	if((t = lex()) == '{') {
		child = block();
		if((t = lex()) != '}')
			parse_error(&lexer, "'}'");
	}

	for(; child; child = child->next) {
		if((child->next = statement())) continue;
		if((child->next = ifstatement())) continue;
		if((child->next = whilestatement())) continue;
		if((child->next = forstatement())) continue;
		if((child->next = returnstatement())) continue;
		if((child->next = function())) continue;
		if((child->next = structure())) continue;
		if((child->next = unionation())) continue;
		if((child->next = enumeration())) continue;

		if((t = lex()) == '{') {
			child->next = block();
			if((t = lex()) != '}')
				parse_error(&lexer, "'}'");
			continue;
		}

		break;
	}

	return root;
}

/*
 * structure: 'struct' '{' ((identifier ':' type?)|(unionation) ('=' initializer)? ';')+  '}'
 */
AST_node* structure(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_STRUCT)
		return NULL;

	root = ast_alloc_node(&ast, N_STRUCTURE, NULL, &lexer.info);

	t = lex();
	if(t != '{')
		parse_error(&lexer, "'{'");

	if(t == T_ID) {
		child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

		t = lex();
		if(t != ':')
			parse_error(&lexer, "':'");
		child->next = type();

		if(child->next)
			child = child->next;
	} else if(t == T_UNION) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = unionation();
	} else
		parse_error(&lexer, "identifier or 'union'");

	t = lex();

	if(t == '=') {
		if(!(child->next = initializer()))
			parse_error(&lexer, "initializer");
		child = child->next;
	}

	if(t != ';')
		parse_error(&lexer, "';'");

	while((t = lex()) != '}') {
		if(t == T_ID) {
			child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
			child = child->next;
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

			t = lex();
			if(t != ':')
				parse_error(&lexer, "':'");
			child->next = type();

			if(child->next)
				child = child->next;
		} else if(t == T_UNION) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child-> = unionation();
		} else
			parse_error(&lexer, "identifier or 'union'");

		t = lex();

		if(t == '=') {
			if(!(child->next = initializer()))
				parse_error(&lexer, "initializer");
			child = child->next;
		}

		if(t != ';')
			parse_error(&lexer, "';'");
	}

	return root;
}

/*
 * unionation: 'union' '{' (declaration)+ '}'
 */
AST_node* unionation(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_UNION)
		return NULL;

	root = ast_alloc_node(&ast, N_STRUCTURE, NULL, &lexer.info);

	t = lex();
	if(t != '{')
		parse_error(&lexer, "'{'");

	if(t != T_ID)
		parse_error(&lexer, "identifier");
	child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
	child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	t = lex();
	if(t != ':')
		parse_error(&lexer, "':'");
	child->next = type();

	t = lex();
	if(t != ';')
		parse_error(&lexer, "';'");

	while((t = lex()) != '}') {
		if(t != T_ID)
			parse_error(&lexer, "identifier");
		child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

		t = lex();
		if(t != ':')
			parse_error(&lexer, "':'");
		child->next = type();

		t = lex();
		if(t != ';')
			parse_error(&lexer, "';'");
	}

	return root;
}

/*
 * enumeration: 'enum' builtintype '{' identifier ('=' (intconst|identifier))? (',' identifier ('=' (intconst|identifier))?) ','? '}' ';'
 */
AST_node* enumeration(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_ENUM)
		return NULL;

	root = ast_alloc_node(&ast, N_ENUMERATION, NULL, &lexer.info);

	t = lex();
	if(t >= T_INT && t <= T_S64) {
		root->down = child = ast_alloc_node(&ast, N_TYPE, NULL, &lexer.info);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	t = lex();
	if(t != '{')
		parse_error(&lexer, "'{'");
	t = lex();
	while(t != '}') {
		if(t != T_ID)
			parse_error(&lexer, "identifier");
		child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child = child->next;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		if(t == '=') {
			t = lex();
			if(t != T_NUMBER && t != T_CHARACTER)
				parse_error(&lexer, "numerical or character constant");
			child->next = ast_alloc_node(&ast, N_INTCONST, NULL, &lexer.info);
			child = child->next;
			if(t == T_CHARACTER)
				child->kind = N_CHARCONST;
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		}
		t = lex();
		if(t != ',' && t != '}')
			parse_error(&lexer, "',' or '}'");
	}

	return root;
}

/*
 * statement: ('defer')? expression (',' expression)* ';'
 */
AST_node* statement(void) {
	register int t;
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_STATEMENT, NULL, &lexer.info);

	t = lex();
	if(t == T_DEFER) {
		root->down = child = ast_alloc_node(&ast, N_DEFER, NULL, &lexer.info);
		child->next = expression();
	} else {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root->down = child = expression();
	}

	while((t = lex()) == ',') {
		child->next = expression();
		child = child->next;
	}

	if(t != ';')
		parse_error(&lexer, "';'");

	return root;
}

/*
 * ifstatement:
 * 	'if' '(' expression ')' '{' block '}'
 */
AST_node* ifstatement(void) {
	AST_node *root, *child;
	
	if(lex() != T_IF)
		return NULL;

	root = ast_alloc_node(&ast, N_IFSTATEMENT, NULL, &lexer.info);

	if(lex() != '(')
		parse_error(&lexer, "'('");
	root->down = child = expression();
	if(lex() != ')')
		parse_error(&lexer, "')'");

	if(lex() != '{')
		parse_error(&lexer, "'{'");
	child->next = block();
	if(lex() != '}')
		parse_error(&lexer, "'}'");

	return root;
}

/*
 * whilestatement:
 * 	'while' '(' expression ')' '{' block '}'
 */
AST_node* whilestatement(void) {
	AST_node *root, *child;
	
	if(lex() != T_WHILE)
		return NULL;

	root = ast_alloc_node(&ast, N_WHILESTATEMENT, NULL, &lexer.info);

	if(lex() != '(')
		parse_error(&lexer, "'('");
	root->down = child = expression();
	if(lex() != ')')
		parse_error(&lexer, "')'");

	if(lex() != '{')
		parse_error(&lexer, "'{'");
	child->next = block();
	if(lex() != '}')
		parse_error(&lexer, "'}'");

	return root;
}

/*
 * returnstatement: 'return' expression ';'
 */
AST_node* returnstatement(void) {
	AST_node *root;
	
	if(lex() != T_RETURN)
		return NULL;

	root = ast_alloc_node(&ast, N_RETURNSTATEMENT, NULL, &lexer.info);

	root->down = expression();

	if(lex() != ';')
		parse_error(&lexer, "';'");

	return root;
}

/*
 * forstatement:
 * 	'for' '('  ';' expression ';' expression ')' '{' block '}'
 */
AST_node* forstatement(void) {
	return NULL;
}

AST_node* expression(void) {
	register int t;
	AST_node *root, child;

	return root;
}

/*
 * call: identifier '(' expression ')'
 */
AST_node* call(void) {
	register int t;
	AST_node *root, *child;
	char *s, *e, *unget;

	t = lex();
	
	if(t != T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	s = lexer.text_s;
	e = lexer.text_e;
	unget = lexer.unget;

	if(lex() != '(') {
		lexer.info.col -= lexer.ptr - unget;
		lexer.ptr = unget;
		return NULL;
	}
		

	root = ast_alloc_node(&ast, N_CALL, NULL, &lexer.info);
	root->val = strpool_alloc(&spool, e - s, s);

	root->down = child = expression();
	while((t = lex()) == ',') {
		child->next = expression();
		child = child->next;
	}

	if(t != ')')
		parse_error(&lexer, "')'");

	return root;
}

void lexer_init(Lexer *l, char *buf) {
	l->token = 0;
	l->text_s = l->text_e = l->unget = NULL;
	l->ptr = l->src = buf;
	l->info = (Info){ .line = 1, .col = 1, .line_s = buf };
	for(l->info.line_e = l->ptr; *(l->info.line_e) != '\n'; ++(l->info.line_e));
	++(l->info.line_e);
}

/*
void cleanup(void) {
	strpool_free(&spool);
	ast_free(&ast);
	free(functab.data);
	fmapclose(&fm);
	fclose(xmlout);
	fclose(astout);
	fclose(symout);
	fclose(vmout);
}
*/

int main(void) {
	Fmap fm;
	fmapopen("test.c", O_RDONLY, &fm);
	fmapread(&fm);
	lexer_init(&lexer, fm.buf);

	while(lex() != T_END && lexer.token != T_ILLEGAL) {
		printf("TOKEN\n");
		if(lexer.token < T_ASSIGNPLUS)
			printf("\ttoken: %c\n", lexer.token);
		else
			printf("\ttoken: %s\n", tokens_debug[lexer.token - T_ASSIGNPLUS]);
		printf("\tline_num: %i\n", lexer.info.line);
		printf("\tcol_num: %i\n", lexer.info.col - (int)(lexer.ptr - lexer.unget));
		printf("\ttoken_text: ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stdout);
		printf("\n");
		printf("\tline_text: ");
		fwrite(lexer.info.line_s, 1, lexer.info.line_e - lexer.info.line_s, stdout);
	}
	fmapclose(&fm);
	printf("\nTESTING PEEK\n");
	fmapopen("test.c", O_RDONLY, &fm);
	fmapread(&fm);
	lexer_init(&lexer, fm.buf);
	prevtahead(T_VOID);
	fmapclose(&fm);
	return 0;
}
