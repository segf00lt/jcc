#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <libgen.h>
#include <assert.h>
#if defined(__MACH__)
#include <err.h>
#else
#include <error.h>
#endif
#include "fmap.c"
#include "mempool.c"

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
	tab.count = tab.global_count = = tab.arg_count = tab.local_count = 0;\
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
	T_ASSIGNLSHIFT,
	T_ASSIGNRSHIFT,
	T_INC,
	T_DEC,
	T_EQ,
	T_NEQ,
	T_LTEQ,
	T_GTEQ,
	T_LSHIFT,
	T_RSHIFT,

	T_ENUM,
	T_REGISTER,
	T_SIZEOF,
	T_FUNC,
	T_STRUCT,
	T_UNION,
	T_INLINE,
	T_DEFER,
	T_CAST,

	T_INT, T_CHAR, T_VOID, T_BOOL,
	T_FLOAT, T_F32, T_F64,
	T_TYPE,
	T_U8, T_U16, T_U32, T_U64,
	T_S8, T_S16, T_S32, T_S64,

	T_IF, T_ELIF, T_ELSE,
	T_WHILE, T_RETURN,
	T_FOR, T_GOTO, T_CONTINUE, T_BREAK,
	T_SWITCH, T_CASE, T_DEFAULT,
	T_AND, T_OR, T_NOT,

	T_NUMBER,
	T_STRING,
	T_CHARACTER,
	T_TRUE, T_FALSE,
	T_BAR,

	T_ID,
	T_END,
	T_ILLEGAL,
};

char *tokens_debug[] = {
	"T_ASSIGNPLUS",
	"T_ASSIGNSUB",
	"T_ASSIGNMUL",
	"T_ASSIGNDIV",
	"T_ASSIGNMOD",
	"T_ASSIGNNOT",
	"T_ASSIGNAND",
	"T_ASSIGNOR",
	"T_ASSIGNXOR",
	"T_ASSIGNLSHIFT",
	"T_ASSIGNRSHIFT",
	"T_INC",
	"T_DEC",
	"T_NEQ",
	"T_LTEQ",
	"T_GTEQ",
	"T_EQ",
	"T_LSHIFT",
	"T_RSHIFT",
	"T_ENUM",
	"T_REGISTER",
	"T_SIZEOF",
	"T_FUNC",
	"T_STRUCT",
	"T_UNION",
	"T_INLINE",
	"T_DEFER",
	"T_CAST",
	"T_INT", "T_CHAR", "T_VOID", "T_BOOL",
	"T_FLOAT", "T_F32", "T_F64",
	"T_TYPE",
	"T_U8", "T_U16", "T_U32", "T_U64",
	"T_S8", "T_S16", "T_S32", "T_S64",
	"T_IF", "T_ELIF", "T_ELSE",
	"T_WHILE", "T_RETURN",
	"T_FOR", "T_GOTO", "T_CONTINUE", "T_BREAK",
	"T_SWITCH", "T_CASE", "T_DEFAULT",
	"T_AND", "T_OR", "T_NOT",
	"T_NUMBER",
	"T_STRING",
	"T_CHARACTER",
	"T_TRUE", "T_FALSE",
	"T_BAR",
	"T_ID",
	"T_END",
	"T_ILLEGAL",
};

enum SEGMENTS {
	S_ARGUMENT = 0,
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
	N_DEC = 1,
	N_CONSTDEC,
	N_TYPE,
	N_INITIALIZER,
	N_RETURNTYPE,
	N_DEFER,
	N_FUNCTION,
	N_STRUCTURE,
	N_UNIONATION,
	N_ENUMERATION,
	N_BLOCK,
	N_STATEMENT,
	N_IFSTATEMENT,
	N_ELSESTATEMENT,
	N_WHILESTATEMENT,
	N_RETURNSTATEMENT,
	N_FORSTATEMENT,
	N_EXPRESSION,
	N_LOGICAL,
	N_COMPARE,
	N_SHIFT,
	N_BITWISE,
	N_ARITH,
	N_FACTOR,
	N_UNARY,
	N_TERM,
	N_CALL,
	N_POINTER,
	N_ARRAY,
	N_OP,
	N_UNOP,
	N_INTLIT,
	N_FLOATLIT,
	N_STRLIT,
	N_CHARLIT,
	N_BOOLLIT,
	N_ARRAYLIT,
	N_STRUCTLIT,
	N_UNINIT,
	N_STORAGEQUALIFIER,
	N_BUILTINTYPE,
	N_ID,
};

char *nodes_debug[] = {
	0,
	"N_DEC",
	"N_CONSTDEC",
	"N_TYPE",
	"N_INITIALIZER",
	"N_RETURNTYPE",
	"N_DEFER",
	"N_FUNCTION",
	"N_STRUCTURE",
	"N_UNIONATION",
	"N_ENUMERATION",
	"N_BLOCK",
	"N_STATEMENT",
	"N_IFSTATEMENT",
	"N_ELSESTATEMENT",
	"N_WHILESTATEMENT",
	"N_RETURNSTATEMENT",
	"N_FORSTATEMENT",
	"N_EXPRESSION",
	"N_LOGICAL",
	"N_COMPARE",
	"N_SHIFT",
	"N_BITWISE",
	"N_ARITH",
	"N_FACTOR",
	"N_UNARY",
	"N_TERM",
	"N_CALL",
	"N_POINTER",
	"N_ARRAY",
	"N_OP",
	"N_UNOP",
	"N_INTLIT",
	"N_FLOATLIT",
	"N_STRLIT",
	"N_CHARLIT",
	"N_BOOLLIT",
	"N_ARRAYLIT",
	"N_STRUCTLIT",
	"N_UNINIT",
	"N_STORAGEQUALIFIER",
	"N_BUILTINTYPE",
	"N_ID",
};

char *keyword[] = {
	"enum",
	"register",
	"sizeof",
	"func",
	"struct",
	"union",
	"inline",
	"defer",
	"cast",
	"int", "char", "void", "bool",
	"float", "f32", "f64",
	"Type",
	"u8", "u16", "u32", "u64",
	"s8", "s16", "s32", "s64",
	"if", "elif", "else",
	"while", "return",
	"for", "goto", "continue", "break",
	"switch", "case", "default",
	"and", "or", "not",
};

char *constant[] = {
	"true",
	"false",
};

size_t keyword_length[] = {
	STRLEN("enum"),
	STRLEN("register"),
	STRLEN("sizeof"),
	STRLEN("func"),
	STRLEN("struct"),
	STRLEN("union"),
	STRLEN("inline"),
	STRLEN("defer"),
	STRLEN("cast"),
	STRLEN("int"), STRLEN("char"), STRLEN("void"), STRLEN("bool"),
	STRLEN("float"), STRLEN("f32"), STRLEN("f64"),
	STRLEN("Type"),
	STRLEN("u8"), STRLEN("u16"), STRLEN("u32"), STRLEN("u64"),
	STRLEN("s8"), STRLEN("s16"), STRLEN("s32"), STRLEN("s64"),
	STRLEN("if"), STRLEN("elif"), STRLEN("else"),
	STRLEN("while"), STRLEN("return"),
	STRLEN("for"), STRLEN("goto"), STRLEN("continue"), STRLEN("break"),
	STRLEN("switch"), STRLEN("case"), STRLEN("default"),
	STRLEN("and"), STRLEN("or"), STRLEN("not"),
};

enum OPERATORS {
	OP_ASSIGNPLUS = 9000,
	OP_ASSIGNSUB,
	OP_ASSIGNMUL,
	OP_ASSIGNDIV,
	OP_ASSIGNMOD,
	OP_ASSIGNNOT,
	OP_ASSIGNAND,
	OP_ASSIGNOR,
	OP_ASSIGNXOR,
	OP_ASSIGNLSHIFT,
	OP_ASSIGNRSHIFT,
	OP_ASSIGN,
	OP_INC,
	OP_DEC,
	OP_EQ,
	OP_NEQ,
	OP_LTEQ,
	OP_GTEQ,
	OP_LT,
	OP_GT,
	OP_LSHIFT,
	OP_RSHIFT,
	OP_LAND,
	OP_LOR,
	OP_LNOT,
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_MOD,
	OP_AND,
	OP_OR,
	OP_XOR,
	OP_NOT,
	OP_NEG,
	OP_DOT,
	OP_CAST,
	OP_ADDR,
	OP_DEREF,
	OP_INDEX,
};

char *operators_debug[] = {
	"OP_ASSIGNPLUS",
	"OP_ASSIGNSUB",
	"OP_ASSIGNMUL",
	"OP_ASSIGNDIV",
	"OP_ASSIGNMOD",
	"OP_ASSIGNNOT",
	"OP_ASSIGNAND",
	"OP_ASSIGNOR",
	"OP_ASSIGNXOR",
	"OP_ASSIGNLSHIFT",
	"OP_ASSIGNRSHIFT",
	"OP_ASSIGN",
	"OP_INC",
	"OP_DEC",
	"OP_EQ",
	"OP_NEQ",
	"OP_LTEQ",
	"OP_GTEQ",
	"OP_LT",
	"OP_GT",
	"OP_LSHIFT",
	"OP_RSHIFT",
	"OP_LAND",
	"OP_LOR",
	"OP_LNOT",
	"OP_ADD",
	"OP_SUB",
	"OP_MUL",
	"OP_DIV",
	"OP_MOD",
	"OP_AND",
	"OP_OR",
	"OP_XOR",
	"OP_NOT",
	"OP_NEG",
	"OP_DOT",
	"OP_CAST",
	"OP_ADDR",
	"OP_DEREF",
	"OP_INDEX",
};

enum type_tag {
	TY_INT = 0, TY_BOOL, TY_FLOAT, TY_ARRAY, TY_POINTER,
	TY_FUNC, TY_STRUCT, TY_ENUM, TY_UNION,
	TY_VOID, TY_TYPE,
};

char *type_tag_debug[] = {
	"TY_INT", "TY_BOOL", "TY_FLOAT", "TY_ARRAY", "TY_POINTER",
	"TY_FUNC", "TY_STRUCT", "TY_ENUM", "TY_UNION",
	"TY_VOID", "TY_TYPE",
};

typedef struct Debug_info Debug_info;
typedef struct Strpool Strpool;
typedef struct Lexer Lexer;
typedef struct AST_node AST_node;
typedef struct AST AST;
typedef struct Type_info Type_info;
typedef struct Type_info_Int Type_info_Int;
typedef struct Type_info_Float Type_info_Float;
typedef struct Type_info_Pointer Type_info_Pointer;
typedef struct Type_info_Array Type_info_Array;
typedef struct Type_info_Func Type_info_Func;
typedef struct Type_info_Struct Type_info_Struct;
typedef struct Type_info_Enum Type_info_Enum;
typedef struct Type_info_Union Type_info_Union;
typedef struct Type_member Type_member; /* function arguments, return values, struct, enum or union member */
typedef struct Type_info_pending Type_info_pending;
typedef struct Sym Sym;
typedef struct Sym_tab Sym_tab;

struct Debug_info {
	int line;
	int col;
	char *line_s;
	char *line_e;
};

struct Strpool {
	char *base;
	char *free;
	char **pages;
	size_t cur;
	size_t cap;
};

struct Lexer {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
	int eof;
	Debug_info info;
};

struct AST_node {
	unsigned int id; /* debug */
	int kind;
	union {
		char *val;
		int op;
		int typesig;
	};
	AST_node *down; /* down */
	AST_node *next; /* across */
	Debug_info info;
};

struct AST {
	/* since we never free individual nodes AST.free stores the first vacant
	 * node in the current page */
	AST_node *root;
	AST_node *free;
	AST_node *base; /* base of current page */
	AST_node **pages;
	size_t cur; /* index of current page */
	size_t cap; /* capacity of **pages */
	size_t nodecount; /* debug */
};

struct Type_info_Int {
	unsigned int bits : 8;
	unsigned int sign : 1;
};

struct Type_info_Float {
	unsigned int bits;
};

struct Type_info_Pointer {
	Type_info *pointer_to;
};

struct Type_info_Array {
	Type_info *array_of;
	union {
		AST_node *max_index_expr;
		//Inst *max_index_inst;
	};
	size_t max_index;
};

struct Type_info_Func {
	Type_member *arg_types;
	Type_member *return_types;
};

struct Type_info_Struct {
	char *name;
	Type_member *members;
};

struct Type_info_Enum {
	char *name;
	Type_member *members;
};

struct Type_info_Union {
	char *name;
	Type_member *members;
};

struct Type_member {
	bool constant;
	char *name;
	Type_info *type;
	Type_member *next;
	size_t byte_offset;
	union {
		AST_node *initial_val_expr;
		//Inst *initial_val_inst;
	};
};

struct Type_info {
	int tag;
	size_t bytes;
	union {
		Type_info_Int Int;
		Type_info_Float Float;
		Type_info_Pointer Pointer;
		Type_info_Array Array;
		Type_info_Func Func;
		Type_info_Struct Struct;
		Type_info_Enum Enum;
		Type_info_Union Union;
	};
};

Type_info builtin_types[] = {
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 1 } },
	{ .tag = TY_VOID },
	{ .tag = TY_BOOL, .bytes = 1u },
	{ .tag = TY_FLOAT, .bytes = 4u, .Int = { .bits = 32u } },
	{ .tag = TY_FLOAT, .bytes = 4u, .Int = { .bits = 32u, .sign = 1 } },
	{ .tag = TY_FLOAT, .bytes = 8u, .Int = { .bits = 64u, .sign = 1 } },
	{ .tag = TY_TYPE },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 2u, .Int = { .bits = 16u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 4u, .Int = { .bits = 32u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 2u, .Int = { .bits = 16u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 4u, .Int = { .bits = 32u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 1 } },
};

struct Sym {
	size_t pos; /* position in memory segment */
	size_t size; /* size in bytes */
	unsigned int seg : 3; /* memory segment */
	unsigned int constant : 1;
	char *name;
	Type_info *type;
	union { /* initial value may be Type_info*, char*, AST_node* */
		Type_info *t;
		AST_node *a;
		// Inst *i;
	} val;
	Debug_info info;
};

struct Sym_tab {
	Sym *data;
	char *name;
	size_t cap;
	size_t count;
	/* TODO segment count union */
	union {
		size_t global_count;
		struct {
			size_t arg_count;
			size_t local_count;
		};
	};
};

Lexer lexer;
AST ast;
Strpool spool;
Sym_tab pendingtab; /* unresolved symbols get defined here */
Sym_tab globaltab;
Sym_tab functab;
Sym_tab blocktab[8]; /* hard limit on amount of nesting that can be done */
size_t depth;

/* utility functions */
void cleanup(void);
void debug_ast_alloc(AST *ast);
void debug_parser(AST_node *node, size_t depth);
void debug_sym_tab(AST_node *node);
void myerror(char *fmt, ...);

/* Strpool functions */
char* strpool_alloc(Strpool *pool, size_t len, char *s);
void strpool_free(Strpool *pool);

/* AST functions */
AST_node* ast_alloc_node(AST *ast, int kind, char *val, Debug_info *info);
void ast_free(AST *ast);

/* parsing */
int lex(void);
void parse(void);
AST_node* declaration(void);
AST_node* vardec(void);
AST_node* literal(void);
AST_node* type(void);
AST_node* function(void);
AST_node* structure(void);
AST_node* unionation(void);
AST_node* enumeration(void);
AST_node* block(void);
AST_node* statement(void);
AST_node* ifstatement(void);
AST_node* whilestatement(void);
AST_node* forstatement(void);
AST_node* returnstatement(void);
AST_node* expression(void);
AST_node* logical(void);
AST_node* compare(void);
AST_node* shift(void);
AST_node* bitwise(void);
AST_node* arith(void);
AST_node* factor(void);
AST_node* unary(void);
AST_node* member(void);
AST_node* term(void);
AST_node* call(void);

/* type system funcitons */
void type_info_print(Type_info *tp);
Type_info* build_type(Mempool *type_pool, Mempool *member_pool, AST_node *node);
Type_info* infer_type(Mempool *type_pool, Mempool *member_pool, AST_node *node);

/* symbol table functions */
void sym_tab_build(Sym_tab *tab, AST_node *node);
void sym_tab_grow(Sym_tab *tab);
void sym_tab_def(Sym_tab *tab, Sym *symbol);
void sym_tab_undef(Sym_tab *tab, char *name);
void sym_tab_copy(Sym_tab *dest, Sym_tab *src);
size_t sym_tab_hash(Sym_tab *tab, char *name);
Sym* sym_tab_look(Sym_tab *tab, char *name);
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

void debug_ast_alloc(AST *ast) {
	for(size_t i = 0; i <= ast->cur; ++i) {
		fprintf(stderr, "PAGE %zu\n", i);
		for(size_t j = 0; j < ast->nodecount; ++j) {
			fprintf(stderr, "\taddress: %p\n"
					"\tid: %u\n"
					"\tkind: %s\n"
					"\tval: %s\n"
					"\tdown: %p\n"
					"\tnext: %p\n"
					"\tline: %i\n"
					"\tcol: %i\n\n",
					(void*)(ast->pages[i]+j),
					ast->pages[i][j].id,
					nodes_debug[ast->pages[i][j].kind],
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
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "id: %u\n", node->id);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "kind: %s\n", nodes_debug[node->kind]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		if(node->kind == N_BUILTINTYPE)
			fprintf(stderr, "type: %s\n", keyword[node->typesig+T_INT-T_ENUM]);
		else if(node->kind != N_OP && node->kind != N_UNOP)
			fprintf(stderr, "val: %s\n", node->val);
		else
			fprintf(stderr, "op: %s\n", operators_debug[node->op-OP_ASSIGNPLUS]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "down: %u\n", node->down ? node->down->id : 0);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "next: %u\n\n", node->next ? node->next->id : 0);
		if(node->down) {
			++depth;
			debug_parser(node->down, depth);
			--depth;
		}
	}
}

AST_node* ast_alloc_node(AST *ast, int kind, char *val, Debug_info *info) {
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

void type_info_print(Type_info *tp) {
}

// NOTE maybe put type_pool and member_pool in global scope
Type_info* build_type(Mempool *type_pool, Mempool *member_pool, AST_node *node) {
	AST_node *child;
	Type_info *tinfo;
	Type_member *member;
	Sym *symptr;
	Sym symbol = {0};

	if(node->kind == N_TYPE)
		node = node->down;

	switch(node->kind) {
	case N_BUILTINTYPE:
		tinfo = builtin_types + node->typesig;
		break;
	case N_POINTER:
		tinfo = mempool_alloc(type_pool);
		tinfo->Pointer.pointer_to = build_type(type_pool, member_pool, node->down); 
		break;
	case N_ARRAY:
		tinfo = mempool_alloc(type_pool);
		tinfo->Array.max_index_expr = node->down; /* resolve later */
		tinfo->Array.array_of = build_type(type_pool, member_pool, node->down->next); 
		break;
	case N_FUNCTION:
		// TODO make sure this works
		node = node->down;
		assert(node->kind == N_TYPE);
		tinfo = mempool_alloc(type_pool);
		tinfo->Func.arg_types = member = mempool_alloc(member_pool);
		member->type = build_type(type_pool, member_pool, node);
		node = node->next;

		do {
			member->next = mempool_alloc(member_pool);
			member = member->next;
			member->type = build_type(type_pool, member_pool, node);
			member = member->next;
			node = node->next;
		} while(node && (node->kind == N_TYPE || node->kind == N_POINTER || node->kind == N_ARRAY));

		assert(node && node->kind == N_RETURNTYPE);

		tinfo->Func.arg_types = member = mempool_alloc(member_pool);
		member->type = build_type(type_pool, member_pool, node);
		node = node->next;

		do {
			member->next = mempool_alloc(member_pool);
			member = member->next;
			member->type = build_type(type_pool, member_pool, node);
			member = member->next;
			node = node->next;
		} while(node && (node->kind == N_TYPE || node->kind == N_POINTER || node->kind == N_ARRAY));

		assert(node == NULL);

		break;
	case N_STRUCTURE: case N_UNIONATION:
		node = node->down;
		assert(node->kind == N_DEC || node->kind == N_CONSTDEC || node->kind == N_STRUCTURE || node->kind == N_UNIONATION);
		tinfo = mempool_alloc(type_pool);
		tinfo->Struct.members = member = mempool_alloc(member_pool);

		while(node) {
			if(node->kind == N_DEC || node->kind == N_CONSTDEC) {
				child = node->down;
				assert(child->kind == N_ID);
				member->name = child->val;
				child = child->next;
				if(child->kind != N_TYPE)
					myerror("jcc: error: struct or union member missing explicit type\nline: %i, col: %i\n",
							child->info.line, child->info.col);
				member->type = build_type(type_pool, member_pool, child);
				child = child->next;
				if(child && child->kind == N_INITIALIZER)
					member->initial_val_expr = child;
				member->constant = (node->kind == N_CONSTDEC);
			} else if(node->kind == N_STRUCTURE || node->kind == N_UNIONATION) {
				member->name = NULL;
				member->type = build_type(type_pool, member_pool, node);
			} else {
				assert(0);
			}

			node = node->next;
			member->next = mempool_alloc(member_pool);
			member = member->next;
		}

		break;
	case N_ENUMERATION: /* TODO what are the semantics of an enum in a struct */
		break;
	case N_ID:
		/* lookup identifier in symbol table, expect a Type
		 * if it isn't in the table, create the symptr and append tinfo to the pending list
		 */
		for(size_t i = depth; i >= 0; --i) {
			symptr = sym_tab_look(blocktab+depth, node->val);
			if(symptr)
				break;
		}
		if(!symptr)
			symptr = sym_tab_look(&functab, node->val);
		if(!symptr)
			symptr = sym_tab_look(&globaltab, node->val);
		if(!symptr)
			symptr = sym_tab_look(&pendingtab, node->val);

		if(symptr) {
			tinfo = symptr->val.t;
		} else { /* define symbol in pendingtab */
			symbol.name = node->val;
			symbol.val.t = mempool_alloc(type_pool);
			sym_tab_def(&pendingtab, &symbol);
			/* NOTE when we find the definition of this symbol remember not to
			 * allocate a new Type_info for val.t */
		}
		break;
	default:
		assert(0);
		break;
	}

	return tinfo;
}

Type_info* infer_type(Mempool *pool, Mempool *member_pool, AST_node *node) {
	return NULL;
}

void sym_tab_build(Sym_tab *tab, AST_node *node) {
	//AST_node *child, *tmp;
	//Sym sym;

	//for(; node; node = node->next) {
	//	sym = (Sym){0};
	//	switch(node->kind) {
	//	case N_STRUCTURE: case N_UNIONATION:
	//		break;
	//	case N_ENUMERATION:
	//		child = node->down;
	//		sym.seg = S_CONSTANT;
	//		sym.type = keyword[T_U64 - T_ENUM];
	//		if(child->kind == N_BUILTINTYPE) {
	//			sym.type = child->val;
	//			child = child->next;
	//		}
	//		if(child->next->kind >= N_INTLIT && child->next->kind <= N_BOOLLIT) {
	//			sym.val = child->next;
	//			/* TODO check type */
	//		}
	//		while(child) {
	//			sym.name = child->val;
	//			sym.info = child->info;
	//			sym_tab_def(tab, &sym);
	//			child = child->next;
	//		}
	//		break;
	//	case N_CONSTDEC:
	//		sym.constant = 1;
	//		tmp = child;
	//		while(tmp->kind == N_ID)
	//			tmp = tmp->next;
	//		switch(tmp->kind) {
	//		case N_BUILTINTYPE:
	//			sym.type = tmp->val;
	//			break;
	//		case N_FUNCTION:
	//			break;
	//		case N_STRUCTURE:
	//			break;
	//		case N_UNIONATION:
	//			break;
	//		case N_ENUMERATION:
	//			break;
	//		}
	//		break;
	//	case N_DEC:
	//		break;
	//	}
	//}
}

void sym_tab_grow(Sym_tab *tab) {
	Sym *old_data = tab->data;
	size_t old_cap = tab->cap;
	tab->data = calloc((tab->cap <<= 1), sizeof(Sym));
	for(size_t i = 0; i < old_cap; ++i) {
		if(!old_data[i].name)
			continue;
		sym_tab_def(tab, old_data + i);
	}
	free(old_data);
}

size_t sym_tab_hash(Sym_tab *tab, char *name) {
	size_t hash = 0;
	while(*name) hash += *(name++) * 31;
	return hash % tab->cap;
}

void sym_tab_def(Sym_tab *tab, Sym *symbol) {
	if(tab->count >= tab->cap) sym_tab_grow(tab);
	size_t i = sym_tab_hash(tab, symbol->name);
	while(tab->data[i].name) {
		if(!strcmp(tab->data[i].name, symbol->name))
			return;
		i = (i + 1) % tab->cap;
	}
	tab->data[i] = *symbol;
	++tab->count;
}

void sym_tab_undef(Sym_tab *tab, char *name) {
	size_t i, origin;
	origin = i = sym_tab_hash(tab, name);
	do {
		if(tab->data[i].name && !strcmp(tab->data[i].name, name))
			tab->data[i].name = NULL;
		i = (i + 1) % tab->cap;
	} while(i != origin);
}

Sym* sym_tab_look(Sym_tab *tab, char *name) {
	size_t i, origin;
	origin = i = sym_tab_hash(tab, name);
	do {
		if(tab->data[i].name && !strcmp(tab->data[i].name, name))
			return tab->data + i;
		i = (i + 1) % tab->cap;
	} while(i != origin);
	return NULL;
}

void sym_tab_clear(Sym_tab *tab) {
	/* NOTE make sure you still have a pointer to the string pool when you call this */
	tab->count = tab->global_count = tab->arg_count = tab->local_count = 0;
	for(size_t i = 0; i < tab->cap; ++i) tab->data[i].name = NULL;
}

void sym_tab_print(Sym_tab *tab) {
	for(size_t i = 0; i < tab->cap; ++i) {
		if(tab->data[i].name == 0) continue;
		/* TODO print Type_info */
		//fprintf(stderr, "SYMBOL\n\tname: %s\n\ttype: %p\n\tseg: %s\n\tpos: %zu\n\n",
		//		tab->data[i].name, tab->data[i].type,
		//		segments[tab->data[i].seg], tab->data[i].pos);
	}
}

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

	if(*lexer.ptr == 0) {
		lexer.eof = 1;
		return T_END;
	}

	lexer.token = 0;

	if(tp[0] == '-' && tp[1] == '-' && tp[2] == '-') {
		lexer.info.col += 3;
		lexer.ptr += 3;
		return (lexer.token = T_BAR);
	}

	if(tp[0] == '<' && tp[1] == '<' && tp[2] == '=') {
		lexer.info.col += 3;
		lexer.ptr += 3;
		return (lexer.token = T_ASSIGNLSHIFT);
	}

	if(tp[0] == '>' && tp[1] == '>' && tp[2] == '=') {
		lexer.info.col += 3;
		lexer.ptr += 3;
		return (lexer.token = T_ASSIGNRSHIFT);
	}

	/* symbol */
	if(tp[1] == '=' && *tp != ':') {
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
	case '>': case '&': case '|': case '+': case '-': case '^': case '%':
	case ':':
		++lexer.info.col;
		++lexer.ptr;
		lexer.token = *tp;
	}

	if(lexer.token) {
		if(tp[1] == tp[0] && (*tp == '<' || *tp == '>' || *tp == '+' || *tp == '-')) {
			++lexer.info.col;
			++lexer.ptr;
			switch(*tp) {
			case '<':
				lexer.token = T_LSHIFT;
				break;
			case '>':
				lexer.token = T_RSHIFT;
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

	/* true, false, numbers, strings and identifiers */
	if(strstr(tp, "true") == tp) {
		lexer.text_e = tp + STRLEN("true");
		lexer.ptr += STRLEN("true");
		lexer.info.col += STRLEN("true");
		return (lexer.token = T_TRUE);
	}

	if(strstr(tp, "false") == tp) {
		lexer.text_e = tp + STRLEN("false");
		lexer.ptr += STRLEN("false");
		lexer.info.col += STRLEN("false");
		return (lexer.token = T_FALSE);
	}

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

void parse(void) {
	AST_node *node;

	ast.root = node = declaration();
	if(node)
		for(node->next = declaration(); node->next; node->next = declaration())
			node = node->next;
	if(!lexer.eof) {
		parse_error(&lexer, "end of file");
	}
}

/*
 * TODO
 * topdeclaration
 */

/*
 * declaration: identifier (',' identifier)* ':' type? ('='|':') (literal';'|function|structure|unionation|enumeration)
 */
AST_node* declaration(void) {
	register int t;
	AST_node *root, *child;
	char *unget1, *unget2;

	t = lex();
	unget1 = lexer.unget;

	if(t == T_ENUM) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return enumeration();
	}

	if(t == T_STRUCT) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return structure();
	}

	if(t == T_UNION) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return unionation();
	}

	if(t != T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	unget2 = lexer.unget;
	while((t = lex()) == ',') {
		t = lex();
		if(t != T_ID)
			break;
	}

	if(t != ':') {
		lexer.info.col -= lexer.ptr - unget1;
		lexer.ptr = unget1;
		return NULL;
	}

	lexer.info.col -= lexer.ptr - unget2;
	lexer.ptr = unget2;
	t = lex();

	root = ast_alloc_node(&ast, N_DEC, NULL, &lexer.info);
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

	t = lex();

	if(child->next && t == ';')
		return root;
	else if(!child->next && t == ';')
		parse_error(&lexer, "type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc_node(&ast, N_INITIALIZER, NULL, &lexer.info);
	child = child->next;

	if(t == ':')
		root->kind = N_CONSTDEC;
	else if(t != '=')
		parse_error(&lexer, "'=' or ':'");

	/* body */
	if((child->down = function())) return root;
	if((child->down = structure())) return root;
	if((child->down = unionation())) return root;
	if((child->down = enumeration())) return root;

	child->down = expression();

	if(child->down) {
		t = lex();
		if(t != ';')
			parse_error(&lexer, "';'");
		return root;
	}

	parse_error(&lexer, "expression or body");

	return NULL;
}

AST_node* vardec(void) {
	register int t;
	AST_node *root, *child;
	char *unget1, *unget2;

	t = lex();
	unget1 = lexer.unget;

	if(t != T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	unget2 = lexer.unget;
	while((t = lex()) == ',') {
		t = lex();
		if(t != T_ID)
			break;
	}

	if(t != ':') {
		lexer.info.col -= lexer.ptr - unget1;
		lexer.ptr = unget1;
		return NULL;
	}

	lexer.info.col -= lexer.ptr - unget2;
	lexer.ptr = unget2;
	t = lex();

	root = ast_alloc_node(&ast, N_DEC, NULL, &lexer.info);
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

	t = lex();

	if(child->next && (t == ',' || t == ')')) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	}
	else if(!child->next && (t == ',' || t == ')'))
		parse_error(&lexer, "type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc_node(&ast, N_INITIALIZER, NULL, &lexer.info);
	child = child->next;

	if(t != '=')
		parse_error(&lexer, "'=' or ':'");

	child->down = expression();

	if(child->down)
		return root;

	parse_error(&lexer, "expression");

	return NULL;
}

AST_node* literal(void) {
	register int t;
	AST_node *root;
	//char *s, *e;

	root = NULL;
	t = lex();

	if((t >= T_NUMBER  && t <= T_FALSE) || t == T_BAR || t == '{')
		root = ast_alloc_node(&ast, 0, NULL, &lexer.info);
	else if(t != T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root = type();
	} else {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	switch(t) {
	case T_NUMBER:
		root->kind = N_INTLIT;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_STRING:
		root->kind = N_STRLIT;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_CHARACTER:
		root->kind = N_CHARLIT;
		root->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_TRUE: case T_FALSE:
		root->kind = N_BOOLLIT;
		root->val = constant[t - T_TRUE];
		break;
	case T_BAR:
		root->kind = N_UNINIT;
		break;
	//case '.':
	//	s = lexer.text_s;
	//	if(lex() != T_ID)
	//		parse_error(&lexer, "identifier");
	//	if(lex() != ')')
	//		parse_error(&lexer, "')'");
	//	if(lex() != '{')
	//		parse_error(&lexer, "'{'");
	//	while((t = lex()) != '}') {
	//	}
	//	break;
	}

	return root;
}

/*
 * type: (pointer|array)* ('int'|'float'|'char'|...|'bool'|('('type (',' type)* ')' ('->' type)?))
 */
AST_node* type(void) {
	register int t;
	bool loop, indirect, lastwasexpr;
	AST_node *root, *child, *node;

	t = lex();

	loop = indirect = lastwasexpr = false;
	if(t == '*' || t == '[') {
		/* TODO this is HORRIBLE */
		indirect = 1;

		root = ast_alloc_node(&ast, N_TYPE, NULL, &lexer.info);

		if(t == '*') {
			root->down = child = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
			loop = true;
		} else {
			root->down = child = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);

			t = lex();
			if(t != ']') {
				lastwasexpr = true;
				lexer.info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
				child->down = expression();
				if(!child->down)
					parse_error(&lexer, "expression");
				child = child->down;
				t = lex();
				if(t != ']')
					parse_error(&lexer, "']'");
			}

			loop = true;
		}

		while(loop) {
			t = lex();

			if(t == '*') {
				if(lastwasexpr) {
					child->next = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
					child = child->next;
				} else {
					child->down = ast_alloc_node(&ast, N_POINTER, NULL, &lexer.info);
					child = child->down;
				}
				lastwasexpr = false;
				continue;
			}

			if(t != '[') {
				break;
			}

			if(lastwasexpr) {
				child->next = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);
				child = child->next;
			} else {
				child->down = ast_alloc_node(&ast, N_ARRAY, NULL, &lexer.info);
				child = child->down;
			}

			t = lex();
			if(t == ']') {
				lastwasexpr = false;
				continue;
			}

			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->down = expression();
			if(!child->down)
				parse_error(&lexer, "expression");
			child = child->down;

			lastwasexpr = true;

			t = lex();
			if(t != ']')
				parse_error(&lexer, "']'");
		}
	}

	if(!(t >= T_INT && t <= T_S64) && t != T_ID &&
			t != T_FUNC && t != T_STRUCT && t != T_UNION && t != T_ENUM) {
		if(indirect)
			parse_error(&lexer, "type");

		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}


	if(!indirect) {
		root = ast_alloc_node(&ast, N_TYPE, NULL, &lexer.info);
		root->down = child = ast_alloc_node(&ast, N_BUILTINTYPE, NULL, &lexer.info);
	} else {
		if(lastwasexpr) {
			child->next = ast_alloc_node(&ast, N_BUILTINTYPE, NULL, &lexer.info);
			child = child->next;
		} else {
			child->down = ast_alloc_node(&ast, N_BUILTINTYPE, NULL, &lexer.info);
			child = child->down;
		}
	}

	switch(t) {
	case T_ID:
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child->kind = N_ID;
		break;
	case T_FUNC:
		child->kind = N_FUNCTION;
		t = lex();
		if(t != '(')
			parse_error(&lexer, "'('");
		child->down = node = type();
		t = lex();
		while(t != ')') {
			node->next = type();
			node = node->next;
			t = lex();
			if(t != ',' && t != ')')
				parse_error(&lexer, "',' or ')'");
		}
		t = lex();
		if(t != '(') {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		} else {
			node->next = ast_alloc_node(&ast, N_RETURNTYPE, NULL, &lexer.info);
			node = node->next;
			node->next = type();
			node = node->next;
			t = lex();
			while(t != ')') {
				node->next = type();
				node = node->next;
				t = lex();
				if(t != ',' && t != ')')
					parse_error(&lexer, "',' or ')'");
			}
		}
		break;
	case T_STRUCT:
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root->down = structure();
		break;
	case T_ENUM:
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root->down = enumeration();
		break;
	case T_UNION:
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root->down = unionation();
		break;
	default: /* builtin type */
		assert(t >= T_INT && t <= T_S64);
		child->typesig = t - T_INT;
		break;
	}

	return root;
}

/*
 * function: 'func' '(' (vardec (',' vardec)*)? ')' (type)? (',' type)* ('inline'? '{' block '}')
 */
AST_node* function(void) {
	register int t;
	AST_node *root, *child, *node;

	t = lex();

	if(t != T_FUNC) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;

	root = ast_alloc_node(&ast, N_FUNCTION, NULL, &lexer.info);

	t = lex();
	if(t != '(')
		parse_error(&lexer, "'('");

	node = vardec();
	if(node)
		root->down = child = node;

	t = lex();
	while(t == ',') {
		child->next = vardec();
		if(child->next)
			child = child->next;
		t = lex();
	}

	if(t != ')')
		parse_error(&lexer, "')'");

	/* return type */
	node = type();
	if(node) {
		if(!child) {
			root->down = child = ast_alloc_node(&ast, N_RETURNTYPE, NULL, &lexer.info);
			child->next = node;
		} else {
			child->next = ast_alloc_node(&ast, N_RETURNTYPE, NULL, &lexer.info);
			child = child->next;
			child->next = node;
			child = child->next;
		}
	}

	t = lex();
	if(t == ',') {
		child->next = type();
		if(child->next)
			child = child->next;
		t = lex();
		while(t == ',') {
			child->next = type();
			if(child->next)
				child = child->next;
			t = lex();
		}
	}

	if(t == T_INLINE) {
		node = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[T_INLINE - T_ENUM], &lexer.info);
		if(!child)
			root->down = child = node;
		else {
			child->next = node;
			child = child->next;
		}

		t = lex();
	}

	if(t != '{')
		parse_error(&lexer, "'{'");

	node = block();
	if(!child && node)
		root->down = child = node;
	else if(node) {
		child->next = node;
		child = child->next;
	}

	t = lex();
	if(t != '}')
		parse_error(&lexer, "'}'");

	return root;
}

/*
 * block: (statement|ifstatement|whilestatement|forstatement|returnstatement|declaration)*
 */
AST_node* block(void) {
	register int t;
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_BLOCK, NULL, &lexer.info);

	if((child = returnstatement())) root->down = child;
	else if((child = ifstatement())) root->down = child;
	else if((child = whilestatement())) root->down = child;
	else if((child = forstatement())) root->down = child;
	//else if((child = structure())) root->down = child;
	//else if((child = unionation())) root->down = child;
	//else if((child = enumeration())) root->down = child;
	//else if((child = function())) root->down = child;
	else if((child = declaration())) root->down = child;
	else if((child = statement())) root->down = child;

	t = lex();
	if(t == '{') {
		child = block();
		if((t = lex()) != '}')
			parse_error(&lexer, "'}'");
	} else if(t == '}') {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	} else {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	for(; child; child = child->next) {
		if((child->next = returnstatement())) continue;
		if((child->next = ifstatement())) continue;
		if((child->next = whilestatement())) continue;
		if((child->next = forstatement())) continue;
		//if((child->next = structure())) continue;
		//if((child->next = unionation())) continue;
		//if((child->next = enumeration())) continue;
		//if((child->next = function())) continue;
		if((child->next = declaration())) continue;
		if((child->next = statement())) continue;

		if((t = lex()) == '{') {
			child->next = block();
			if((t = lex()) != '}')
				parse_error(&lexer, "'}'");
			continue;
		}

		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;

		break;
	}

	return root;
}

/*
 * structure: 'struct' '{' ( (vardec) | (unionation) ('=' literal)? ';' )+  '}'
 */
AST_node* structure(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_STRUCT) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_STRUCTURE, NULL, &lexer.info);

	t = lex();
	if(t != '{')
		parse_error(&lexer, "'{'");

	t = lex();
	if(t == T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = declaration();
	} else if(t == T_UNION) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = unionation();
		t = lex();
		if(t == '=') {
			parse_error(&lexer, "next member");
		} else {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else if(t == T_STRUCT) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = structure();
		t = lex();
		if(t == '=') {
			parse_error(&lexer, "next member");
		} else {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else {
		parse_error(&lexer, "identifier or 'union'");
	}

	root->down = child;

	while((t = lex()) != '}') {
		if(t == T_ID) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = declaration();
		} else if(t == T_UNION) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t == '=') {
				parse_error(&lexer, "next member");
			} else {
				lexer.info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else if(t == T_STRUCT) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = structure();
			t = lex();
			if(t == '=') {
				parse_error(&lexer, "next member");
			} else {
				lexer.info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error(&lexer, "identifier or 'union'");
		}

		child = child->next;
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
	if(t != T_UNION) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_UNIONATION, NULL, &lexer.info);

	t = lex();
	if(t != '{')
		parse_error(&lexer, "'{'");

	t = lex();
	if(t == T_ID) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = declaration();
	} else if(t == T_UNION) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = unionation();
		t = lex();
		if(t == '=') {
			parse_error(&lexer, "next member");
		} else {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else if(t == T_STRUCT) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = structure();
		t = lex();
		if(t == '=') {
			parse_error(&lexer, "next member");
		} else {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else {
		parse_error(&lexer, "identifier or 'union'");
	}

	root->down = child;

	while((t = lex()) != '}') {
		if(t == T_ID) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = declaration();
		} else if(t == T_UNION) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t == '=') {
				parse_error(&lexer, "next member");
			} else {
				lexer.info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else if(t == T_STRUCT) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = structure();
			t = lex();
			if(t == '=') {
				parse_error(&lexer, "next member");
			} else {
				lexer.info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error(&lexer, "identifier or 'union'");
		}

		child = child->next;
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
	if(t != T_ENUM) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_ENUMERATION, NULL, &lexer.info);

	t = lex();
	if(t >= T_INT && t <= T_S64) {
		root->down = child = ast_alloc_node(&ast, N_BUILTINTYPE, NULL, &lexer.info);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != '{')
		parse_error(&lexer, "'{'");

	t = lex();
	if(t != T_ID)
		parse_error(&lexer, "identifier");

	if(!child) {
		root->down = child = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else {
		child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		child = child->next;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	t = lex();
	if(t == '=') {
		t = lex();
		if(t != T_NUMBER && t != T_CHARACTER)
			parse_error(&lexer, "numerical or character constant");
		child->next = ast_alloc_node(&ast, N_INTLIT, NULL, &lexer.info);
		child = child->next;
		if(t == T_CHARACTER)
			child->kind = N_CHARLIT;
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ',' && t != '}')
		parse_error(&lexer, "',' or '}'");

	while(t != '}') {
		t = lex();

		if(t == '}')
			break;

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
			child->next = ast_alloc_node(&ast, N_INTLIT, NULL, &lexer.info);
			child = child->next;
			if(t == T_CHARACTER)
				child->kind = N_CHARLIT;
			child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
			t = lex();
		}

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

	t = lex();
	if(t == ';' || t == '}' || t == T_END) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}


	if(t == T_DEFER) {
		root = ast_alloc_node(&ast, N_DEFER, NULL, &lexer.info);
		root->down = child = expression();
	} else {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root = ast_alloc_node(&ast, N_STATEMENT, NULL, &lexer.info);
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
 * 	'if' '(' expression ')' '{' block '}' ('elif' '(' expression ')' '{' block '}')* ('else' '{' block '}')?
 */
AST_node* ifstatement(void) {
	register int t;
	AST_node *root, *child;

	if(lex() != T_IF) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

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
	child = child->next;

	while(1) {
		t = lex();
		if(t == T_ELIF) {
			if(lex() != '(')
				parse_error(&lexer, "'('");
			child->next = expression();
			if(lex() != ')')
				parse_error(&lexer, "')'");
			child = child->next;
		} else if(t != T_ELSE) {
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			break;
		}

		if(lex() != '{')
			parse_error(&lexer, "'{'");
		child->next = block();
		if(lex() != '}')
			parse_error(&lexer, "'}'");
		child = child->next;
	}

	return root;
}

/*
 * whilestatement:
 * 	'while' '(' expression ')' '{' block '}'
 */
AST_node* whilestatement(void) {
	AST_node *root, *child;

	if(lex() != T_WHILE) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

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
	register int t;
	AST_node *root, *child;

	if(lex() != T_RETURN) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_RETURNSTATEMENT, NULL, &lexer.info);

	root->down = child = expression();

	t = lex();
	while(t == ',') {
		child->next = expression();
		child = child->next;
		t = lex();
	}

	if(t != ';')
		parse_error(&lexer, "';'");

	return root;
}

/*
 * forstatement: 'for' '(' (vardec ','?)* ';' expression ';' expression ')' '{' block '}'
 */
AST_node* forstatement(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_FOR) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	t = lex();
	if(t != '(')
		parse_error(&lexer, "'('");

	root = ast_alloc_node(&ast, N_FORSTATEMENT, NULL, &lexer.info);
	root->down = child = vardec();

	t = lex();
	while(child && t == ',') {
		child->next = vardec();
		if(child->next)
			child = child->next;
		else
			break;
		t = lex();
	}

	if(t != ';')
		parse_error(&lexer, "';'");
	if(child)
		child->next = expression();
	else
		root->down = child = expression();
	if(child->next)
		child = child->next;

	t = lex();
	if(t != ';')
		parse_error(&lexer, "';'");

	if(child)
		child->next = expression();
	else
		root->down = child = expression();
	if(child->next)
		child = child->next;

	t = lex();
	if(t != ')')
		parse_error(&lexer, "')'");

	if(lex() != '{')
		parse_error(&lexer, "'{'");

	if(child)
		child->next = block();
	else
		root->down = child = block();

	if(lex() != '}')
		parse_error(&lexer, "'}'");

	return root;
}

/*
 * expression:
 *	logical '=' expression
 *	logical T_ASSIGNPLUS expression
 *	logical T_ASSIGNSUB expression
 *	logical T_ASSIGNMUL expression
 *	logical T_ASSIGNDIV expression
 *	logical T_ASSIGNMOD expression
 *	logical T_ASSIGNNOT expression
 *	logical T_ASSIGNAND expression
 *	logical T_ASSIGNOR expression
 *	logical T_ASSIGNXOR expression
 *	logical T_ASSIGNLSHIFT expression
 *	logical T_ASSIGNRSHIFT expression
 * 	logical
 *
 * logical:
 * 	compare 'and' logical
 * 	compare 'or' logical
 *	compare
 * 
 * compare:
 * 	shift T_EQ compare
 * 	shift T_NEQ compare
 * 	shift T_LTEQ compare
 * 	shift T_GTEQ compare
 * 	shift '<' compare
 * 	shift '>' compare
 * 	shift
 *
 * shift:
 *	bitwise T_LSHIFT shift
 *	bitwise T_RSHIFT shift
 *	bitwise
 *
 * bitwise:
 * 	arith '&' bitwise
 * 	arith '|' bitwise
 * 	arith '^' bitwise
 * 	arith
 *
 * arith:
 * 	factor '+' arith
 * 	factor '-' arith
 * 	factor
 *
 * factor:
 * 	term '*' factor
 * 	term '/' factor
 * 	term '%' factor
 * 	term
 *
 * TODO improve expression parsing
 * unary:
 * 	'*' unary
 * 	'&' unary
 * 	T_NOT unary
 * 	'-' unary
 * 	'~' unar
 * 	T_INC unary
 * 	T_DEC unary
 * 	'cast' '(' type ')' unary
 * 	unary '[' expression ']'
 * 	unary '.' identifier
 * 	term
 *
 * term:
 *	literal
 * 	call
 * 	identifier
 * 	'(' expression ')'
 *
 */
AST_node* expression(void) {
	register int t;
	AST_node *root, *child;

	child = logical();

	if(!child)
		return NULL;

	t = lex();
	if(t != '=' && !(t >= T_ASSIGNPLUS && t <= T_ASSIGNRSHIFT)) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_EXPRESSION, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	root->down->op = (t == '=') ? OP_ASSIGN : (t - T_ASSIGNPLUS) + OP_ASSIGNPLUS;
	root->down->down = child;
	root->down->next = expression();

	if(!root->down->next)
		parse_error(&lexer, "rvalue");

	return root;
}

AST_node* logical(void) {
	register int t;
	AST_node *root, *child;

	child = compare();

	if(!child)
		return NULL;

	t = lex();

	if(t != T_AND && t != T_OR) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_LOGICAL, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	root->down->op = (t == T_AND) ? OP_LAND : OP_LOR;
	root->down->down = child;
	root->down->next = logical();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* compare(void) {
	register int t;
	AST_node *root, *child;

	child = shift();

	if(!child)
		return NULL;

	t = lex();
	if(t != '>' && t != '<' && !(t >= T_EQ && t <= T_GTEQ)) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_COMPARE, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	switch(t) {
	case '<':
		root->down->op = OP_LT;
		break;
	case '>':
		root->down->op = OP_GT;
		break;
	case T_EQ: case T_NEQ: case T_LTEQ: case T_GTEQ:
		root->down->op = (t - T_EQ) + OP_EQ;
		break;
	}
	root->down->down = child;
	root->down->next = compare();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* shift(void) {
	register int t;
	AST_node *root, *child;

	child = bitwise();

	if(!child)
		return NULL;

	t = lex();
	if(t != T_LSHIFT && t != T_RSHIFT) {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_SHIFT, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	root->down->op = (t == T_LSHIFT) ? OP_LSHIFT : OP_RSHIFT;
	root->down->down = child;
	root->down->next = shift();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* bitwise(void) {
	register int t;
	AST_node *root, *child;

	child = arith();

	if(!child)
		return NULL;

	t = lex();
	if(t != '&' && t != '|' && t != '^') {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_BITWISE, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	switch(t) {
	case '&':
		root->down->op = OP_AND;
		break;
	case '|':
		root->down->op = OP_OR;
		break;
	case '^':
		root->down->op = OP_XOR;
		break;
	}
	root->down->down = child;
	root->down->next = bitwise();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* arith(void) {
	register int t;
	AST_node *root, *child;

	child = factor();

	if(!child)
		return NULL;

	t = lex();
	if(t != '+' && t != '-') {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_ARITH, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	root->down->op = (t == '+') ? OP_ADD : OP_SUB;
	root->down->down = child;
	root->down->next = arith();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* factor(void) {
	register int t;
	AST_node *root, *child;

	child = unary();

	if(!child)
		return NULL;

	t = lex();
	if(t != '*' && t != '/' && t != '%') {
		lexer.info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_FACTOR, NULL, &lexer.info);
	root->down = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
	switch(t) {
	case '*':
		root->down->op = OP_MUL;
		break;
	case '/':
		root->down->op = OP_DIV;
		break;
	case '%':
		root->down->op = OP_MOD;
		break;
	}
	root->down->down = child;
	root->down->next = factor();

	if(!root->down->next)
		parse_error(&lexer, "right operand");

	return root;
}

AST_node* unary(void) {
	register int t;
	AST_node *root, *child;

	root = child = NULL;
	t = lex();

	switch(t) {
		case '*':
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_DEREF;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case '&':
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_ADDR;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case T_NOT:
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_LNOT;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case '-':
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_NEG;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case '~':
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_NOT;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case T_INC:
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_INC;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case T_DEC:
			child = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->op = OP_DEC;
			child->next = unary();
			if(!child)
				parse_error(&lexer, "term");
			break;
		case T_CAST:
			t = lex();

			child = type();
			if(!child)
				parse_error(&lexer, "type");
			t = lex();
			if(t != ')')
				parse_error(&lexer, "')'");
			child->next = unary();
			break;
		default:
			lexer.info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			break;
	}

	if(!child)
		return member();

	root = ast_alloc_node(&ast, N_UNARY, NULL, &lexer.info);
	root->down = child;
	return root;
}

AST_node* member(void) {
	register int t;
	AST_node *root, *child;

	root = child = term();

	if(!root)
		return NULL;

	t = lex();
	while(t == '[' || t == '.') {
		if(t == '[') {
			child->next = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->next->op = OP_INDEX;
			child = child->next;
			child->down = expression();
			if(!child->down)
				parse_error(&lexer, "expression");
			t = lex();
			if(t != ']')
				parse_error(&lexer, "']'");
		} else if(t == '.') {
			child->next = ast_alloc_node(&ast, N_OP, NULL, &lexer.info);
			child->next->op = OP_DOT;
			child = child->next;
			t = lex();
			if(t != T_ID)
				parse_error(&lexer, "identifier");
			child->next = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
			child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
			child = child->next;
		}

		t = lex();
	}

	lexer.info.col -= lexer.ptr - lexer.unget;
	lexer.ptr = lexer.unget;

	return root;
}

AST_node* term(void) {
	register int t;
	AST_node *node;

	node = call();
	if(node)
		return node;

	node = literal();
	if(node)
		return node;

	t = lex();
	if(t == T_ID) {
		node = ast_alloc_node(&ast, N_ID, NULL, &lexer.info);
		node->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		return node;
	}

	if(t == '(') {
		node = expression();
		t = lex();
		if(t != ')')
			parse_error(&lexer, "')'");
		return node;
	}

	lexer.info.col -= lexer.ptr - lexer.unget;
	lexer.ptr = lexer.unget;

	return NULL;
}


/*
 * call: identifier '(' expression (',' expression)* ')'
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
	l->info = (Debug_info){ .line = 1, .col = 1, .line_s = buf };
	for(l->info.line_e = l->ptr; *(l->info.line_e) != '\n'; ++(l->info.line_e));
	++(l->info.line_e);
}

void compile(void) {
	//AST_node *root = ast.root;

	/* build global tab */
}

/*
void cleanup(void) {
	strpool_free(&spool);
	ast_free(&ast);
	free(functab.data);
	fmapclose(&fm);
	fclose(astout);
	fclose(symout);
}
*/

int main(int argc, char **argv) {
	if(argc == 1) {
		return 1;
	}
	Fmap fm;
	if(fmapopen(argv[1], O_RDONLY, &fm) < 0)
#if defined(__MACH__)
		err(1, "%s: no such file or directory", argv[1]);
#else
		error(1, 0, "%s: no such file or directory", argv[1]);
#endif
	fmapread(&fm);
	lexer_init(&lexer, fm.buf);
	AST_INIT(ast);
	STRPOOL_INIT(spool);

	parse();
	debug_parser(ast.root, 0);
	//debug_ast_alloc(&ast);

	fmapclose(&fm);
	ast_free(&ast);
	strpool_free(&spool);
	return 0;
}
