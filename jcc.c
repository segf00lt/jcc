#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
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
#include "pool.c"

#define ARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */

#define SYM_TAB_INIT(tab, s) {\
	tab.cap = 128;\
	tab.data = calloc(tab.cap, sizeof(Sym));\
	tab.count = 0;\
	tab.scope = s;\
	tab.seg_count.data = 0;\
	tab.seg_count.bss = 0;\
	tab.seg_count.arg = 0;\
	tab.seg_count.stack = 0;\
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
	T_STRING,
	T_U8, T_U16, T_U32, T_U64,
	T_S8, T_S16, T_S32, T_S64,

	T_IF, T_ELIF, T_ELSE,
	T_WHILE, T_RETURN,
	T_FOR, T_GOTO, T_CONTINUE, T_BREAK,
	T_SWITCH, T_CASE, T_DEFAULT,
	T_AND, T_OR, T_NOT,

	T_INTEGER,
	T_STR,
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
	"T_STRING",
	"T_U8", "T_U16", "T_U32", "T_U64",
	"T_S8", "T_S16", "T_S32", "T_S64",
	"T_IF", "T_ELIF", "T_ELSE",
	"T_WHILE", "T_RETURN",
	"T_FOR", "T_GOTO", "T_CONTINUE", "T_BREAK",
	"T_SWITCH", "T_CASE", "T_DEFAULT",
	"T_AND", "T_OR", "T_NOT",
	"T_INTEGER",
	"T_STR",
	"T_CHARACTER",
	"T_TRUE", "T_FALSE",
	"T_BAR",
	"T_ID",
	"T_END",
	"T_ILLEGAL",
};

enum SEGMENTS {
	SEG_DATA,
	SEG_BSS,
	SEG_TEXT,
	SEG_HEAP,
	SEG_ARG,
	SEG_STACK,
};

char *segments_debug[] = {
	"SEG_DATA",
	"SEG_BSS",
	"SEG_TEXT",
	"SEG_HEAP",
	"SEG_ARG",
	"SEG_STACK",
};

enum NODES {
	N_DEC,
	N_CONSTDEC,
	N_IDLIST,
	N_TYPE,
	N_INITIALIZER,
	N_ARGUMENTS,
	N_RETURNTYPE,
	N_DEFER,
	N_FUNCTION,
	N_STRUCTURE,
	N_UNIONATION,
	N_ENUMERATION,
	N_BLOCK,
	N_FUNCSIGNATURE,
	N_STATEMENT,
	N_IFSTATEMENT,
	N_ELSESTATEMENT,
	N_WHILESTATEMENT,
	N_RETURNSTATEMENT,
	N_FORSTATEMENT,
	N_ASSIGNMENT,
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
	"N_DEC",
	"N_CONSTDEC",
	"N_IDLIST",
	"N_TYPE",
	"N_INITIALIZER",
	"N_ARGUMENTS",
	"N_RETURNTYPE",
	"N_DEFER",
	"N_FUNCTION",
	"N_STRUCTURE",
	"N_UNIONATION",
	"N_ENUMERATION",
	"N_BLOCK",
	"N_FUNCSIGNATURE",
	"N_STATEMENT",
	"N_IFSTATEMENT",
	"N_ELSESTATEMENT",
	"N_WHILESTATEMENT",
	"N_RETURNSTATEMENT",
	"N_FORSTATEMENT",
	"N_ASSIGNMENT",
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
	"string",
	"u8", "u16", "u32", "u64",
	"s8", "s16", "s32", "s64",
	"if", "elif", "else",
	"while", "return",
	"for", "goto", "continue", "break",
	"switch", "case", "default",
	"and", "or", "not",
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
	STRLEN("string"),
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

enum TYPE_TAG {
	TY_INT = 0, TY_CHAR, TY_VOID, TY_BOOL,
	TY_FLOAT, TY_F32, TY_F64,
	TY_TYPE, TY_STRING,
	TY_U8, TY_U16, TY_U32, TY_U64,
	TY_S8, TY_S16, TY_S32, TY_S64,

	TY_ARRAY, TY_POINTER,
	TY_FUNC, TY_STRUCT, TY_ENUM, TY_UNION,
};

char *type_tag_debug[] = {
	"TY_INT", "TY_CHAR", "TY_VOID", "TY_BOOL",
	"TY_FLOAT", "TY_F32", "TY_F64",
	"TY_TYPE", "TY_STRING",
	"TY_U8", "TY_U16", "TY_U32", "TY_U64",
	"TY_S8", "TY_S16", "TY_S32", "TY_S64",

	"TY_ARRAY", "TY_POINTER",
	"TY_FUNC", "TY_STRUCT", "TY_ENUM", "TY_UNION",
};

enum OPCODE {
	BCOP_SET, // set register to immediate value
	BCOP_ADD,
	BCOP_SUB,
	BCOP_MUL,
	BCOP_DIV,
	BCOP_MOD,
	BCOP_AND,
	BCOP_OR,
	BCOP_XOR,
	BCOP_NOT,
	BCOP_LSHIFT,
	BCOP_RSHIFT,
	BCOP_JMP,
	BCOP_JLT,
	BCOP_JGE,
	BCOP_JNE,
	BCOP_JEQ,
	BCOP_LOADB,
	BCOP_LOADS,
	BCOP_LOADH,
	BCOP_LOADW,
	BCOP_STOREB,
	BCOP_STORES,
	BCOP_STOREH,
	BCOP_STOREW,
	BCOP_PUSHW,
	BCOP_POPW,
	BCOP_PUSHH,
	BCOP_POPH,
	BCOP_PUSHB,
	BCOP_POPB,
	BCOP_PUSHS,
	BCOP_POPS,
	BCOP_CALL,
	BCOP_RET,

	/* special opcodes */
	BCOP_INT,
	BCOP_HALT,
};

char *opcode_debug[] = {
	"BCOP_SET",
	"BCOP_ADD",
	"BCOP_SUB",
	"BCOP_MUL",
	"BCOP_DIV",
	"BCOP_MOD",
	"BCOP_AND",
	"BCOP_OR",
	"BCOP_XOR",
	"BCOP_NOT",
	"BCOP_LSHIFT",
	"BCOP_RSHIFT",
	"BCOP_JMP",
	"BCOP_JLT",
	"BCOP_JGE",
	"BCOP_JNE",
	"BCOP_JEQ",
	"BCOP_LOADB",
	"BCOP_LOADS",
	"BCOP_LOADH",
	"BCOP_LOADW",
	"BCOP_STOREB",
	"BCOP_STORES",
	"BCOP_STOREH",
	"BCOP_STOREW",
	"BCOP_PUSHW",
	"BCOP_POPW",
	"BCOP_PUSHH",
	"BCOP_POPH",
	"BCOP_PUSHB",
	"BCOP_POPB",
	"BCOP_PUSHS",
	"BCOP_POPS",
	"BCOP_CALL",
	"BCOP_RET",
	"BCOP_INT",
	"BCOP_HALT",
};

enum SCOPE {
	SCOPE_GLOBAL,
	SCOPE_ARGUMENT,
	SCOPE_LOCAL,
};

char *scope_debug[] = {
	"SCOPE_GLOBAL",
	"SCOPE_ARGUMENT",
	"SCOPE_LOCAL",
};

/* typedefs */
typedef struct Debug_info Debug_info;
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
typedef struct Type_member Type_member; /* function arguments and return values, struct, enum or union member */
typedef struct Type_info_pending Type_info_pending;
typedef struct Sym Sym;
typedef struct Sym_tab Sym_tab;
typedef uint64_t BCreg;
typedef uint8_t BCbyte;
typedef struct BCmem BCmem;
typedef struct BCinst BCinst;

struct Debug_info {
	int line;
	int col;
	char *line_s;
	char *line_e;
};

struct Lexer {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
	int eof;
	Debug_info debug_info;
};

struct AST_node {
	unsigned int id; /* debug */
	int kind;
	bool visited;
	union {
		char *str;
		int op;
		int typesig;
		int boolean;
	} val;
	AST_node *down; /* down */
	AST_node *next; /* across */
	Debug_info debug_info;
};

struct AST {
	/* since we never free individual nodes AST.free stores the first vacant
	 * node in the current page */
	AST_node *root;
	Pool pool;
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
	AST_node *max_index_expr;
	size_t max_index;
};

struct Type_info_Func {
	Type_member *arg_types;
	Type_member *return_types;
};

struct Type_info_Struct {
	char *name;
	Type_member *members;
	Debug_info debug_info;
};

struct Type_info_Enum {
	char *name;
	Type_member *members;
	Debug_info debug_info;
};

struct Type_info_Union {
	char *name;
	Type_member *members;
	Debug_info debug_info;
};

struct Type_member {
	char *name;
	bool constant;
	size_t byte_offset;
	AST_node *initial_val_expr;
	Type_info *type;
	Type_member *next;
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
	{ .tag = TY_FLOAT, .bytes = 4u, .Float = { .bits = 32u } },
	{ .tag = TY_FLOAT, .bytes = 4u, .Float = { .bits = 32u} },
	{ .tag = TY_FLOAT, .bytes = 8u, .Float = { .bits = 64u} },
	{ .tag = TY_TYPE, .bytes = sizeof(Type_info) },
	{ .tag = TY_STRING },
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
	char *name;
	unsigned int constant : 1;
	uint64_t seg : 3; /* memory segment */
	uint64_t addr : 64 - 3; /* address in memory segment */
	Type_info *type;
	union {
		Type_info *t;
		AST_node *a;
	} val;
	Debug_info debug_info;
};

struct Sym_tab {
	Sym *data;
	int scope;
	char *name;
	size_t cap;
	size_t count;
	size_t text_count;
	union {
		struct {
			size_t data;
			size_t bss;
		};
		struct {
			size_t arg;
			size_t stack;
		};
	} seg_count;
};

struct BCinst {
	uint64_t opcode;
	union {
		uint64_t dest_r;
		struct {
			uint64_t seg : 3;
			uint64_t addr : 64 - 3;
		};
	};
	uint64_t r1;
	union {
		uint64_t r2;
		uint64_t imm;
	};
};

struct BCmem {
	BCreg *regs;
	/* the memory 6 segments are:
	 * data
	 * bss
	 * text
	 * heap
	 * arg
	 * stack
	 * 
	 * we use the first 3 bits of an addr to
	 * index the segment, the rest of the bits
	 * are used for indexing within that segment
	 */
	BCbyte *seg[6];
	BCbyte *stack_ptr;
};

/* globals */
Fmap fm; /* source file Fmap */
Pool string_pool;
Pool type_pool;
Pool member_pool;
Lexer lexer;
AST ast;
Sym_tab pendingtab; /* unresolved symbols get defined here */
Sym_tab globaltab;
Sym_tab functab;
Sym_tab blocktab[8]; /* hard limit on amount of nesting that can be done */
int scope_depth = -1;
BCmem bcmem;

/* utility functions */
void cleanup(void);
void myerror(char *fmt, ...);
void parse_error(char *expect);

/* AST functions */
AST_node* ast_alloc_node(AST *ast, int kind, Debug_info *debug_info);
void ast_print(AST_node *node, size_t depth);

/* lexer functions */
void lexer_init(Lexer *l, char *buf);

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
AST_node* assignment(void);
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
void type_info_print(Type_info *tp, size_t depth);
Type_info* type_info_build(Pool *type_pool, Pool *member_pool, AST_node *node, Type_info *dest);
void resolve_compound_type(Type_info *tinfo);

/* symbol table functions */
void sym_tab_build(Sym_tab *tab, AST_node *node);
void sym_tab_grow(Sym_tab *tab);
int sym_tab_def(Sym_tab *tab, Sym *symbol);
int sym_tab_undef(Sym_tab *tab, char *name);
void sym_tab_copy(Sym_tab *dest, Sym_tab *src);
size_t sym_tab_hash(Sym_tab *tab, char *name);
Sym* sym_tab_look(Sym_tab *tab, char *name);
void sym_tab_clear(Sym_tab *tab);
void sym_tab_print(Sym_tab *tab);

/* bytecode interpreter */
int bc_interpreter(BCinst *prog, BCmem *mem);

void ast_print(AST_node *node, size_t depth) {
	int i;
	for(; node; node = node->next) {
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "AST_NODE\n");
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "id: %u\n", node->id);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "kind: %s\n", nodes_debug[node->kind]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		if(node->kind == N_BUILTINTYPE)
			fprintf(stderr, "type: %s\n", type_tag_debug[node->val.typesig]);
		else if(node->kind != N_OP && node->kind != N_UNOP)
			fprintf(stderr, "str: %s\n", node->val.str);
		else if(node->kind == N_BOOLLIT)
			fprintf(stderr, "boolean: %u\n", node->val.boolean);
		else
			fprintf(stderr, "op: %s\n", operators_debug[node->val.op-OP_ASSIGNPLUS]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "down: %u\n", node->down ? node->down->id : 0);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "next: %u\n\n", node->next ? node->next->id : 0);
		if(node->down)
			ast_print(node->down, depth+1);
	}
}

AST_node* ast_alloc_node(AST *ast, int kind, Debug_info *debug_info) {
	register AST_node *node;
	node = pool_alloc(&(ast->pool));
	*node = (AST_node){
			.id = ast->nodecount++,
			.kind = kind,
			.down = NULL,
			.next = NULL,
			.debug_info = *debug_info
		};
	return node;
}

void type_info_print(Type_info *tp, size_t depth) {
	Type_member *member;
	register size_t i;
	char *compound_type_name;

	for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
	fprintf(stderr, "TYPE_INFO\n");
	for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
	fprintf(stderr, "tag: %s\n", type_tag_debug[tp->tag]);
	for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
	fprintf(stderr, "bytes: %zu\n", tp->bytes);
	switch(tp->tag) {
	case TY_INT:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "bits: %u\n", tp->Int.bits);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "sign: %u\n", tp->Int.sign);
		break;
	case TY_FLOAT:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "bits: %u\n", tp->Float.bits);
		break;
	case TY_TYPE:
		break;
	case TY_VOID:
		break;
	case TY_BOOL:
		break;
	case TY_STRING:
		break;
	case TY_ARRAY:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "max_index: %zu\n", tp->Array.max_index);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "array_of: %s\n", type_tag_debug[tp->Array.array_of->tag]);
		ast_print(tp->Array.max_index_expr, depth+1);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		type_info_print(tp->Array.array_of, depth+1);
		break;
	case TY_POINTER:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "pointer_to: %s\n", type_tag_debug[tp->Pointer.pointer_to->tag]);
		type_info_print(tp->Pointer.pointer_to, depth+1);
		break;
	case TY_FUNC:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "arguments:\n");
		++depth;
		for(member = tp->Func.arg_types; member; member = member->next) {
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "ARGUMENT\n");
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "name: %s\n", member->name);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "constant: %d\n", member->constant);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "byte_offset: %zu\n", member->byte_offset);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "initial_val_expr:\n");
			ast_print(member->initial_val_expr, depth+1);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "type:\n");
			type_info_print(member->type, depth+1);
		}
		--depth;
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "return values:\n");
		++depth;
		for(member = tp->Func.return_types; member; member = member->next) {
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "RETURN\n");
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "name: %s\n", member->name);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "constant: %d\n", member->constant);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "byte_offset: %zu\n", member->byte_offset);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "initial_val_expr:\n");
			ast_print(member->initial_val_expr, depth+1);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "type:\n");
			type_info_print(member->type, depth+1);
		}
		--depth;
		break;
	case TY_STRUCT: case TY_UNION:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		if(tp->tag == TY_STRUCT) {
			compound_type_name = tp->Struct.name;
			fprintf(stderr, "name: %s\n", tp->Struct.name);
			member = tp->Struct.members;
		} else {
			compound_type_name = tp->Union.name;
			fprintf(stderr, "name: %s\n", tp->Union.name);
			member = tp->Union.members;
		}
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "members:\n");
		++depth;
		for(; member; member = member->next) {
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "MEMBER\n");
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "name: %s\n", member->name);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "constant: %d\n", member->constant);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "byte_offset: %zu\n", member->byte_offset);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "initial_val_expr:\n");
			ast_print(member->initial_val_expr, depth+1);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "type:\n");
			if(member->type->tag == TY_POINTER || member->type->tag == TY_ARRAY) {
				assert(compound_type_name);
				if(member->type->Pointer.pointer_to != tp && member->type->Array.array_of != tp) {
					type_info_print(member->type, depth+1);
				}
				++depth;
				for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
				fprintf(stderr, "TYPE_INFO\n");
				for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
				fprintf(stderr, "tag: %s\n", type_tag_debug[member->type->tag]);
				for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
				fprintf(stderr, "bytes: %zu\n", tp->bytes);
				for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
				if(member->type->tag == TY_POINTER) {
					fprintf(stderr, "pointer_to: %s\n", compound_type_name);
				} else {
					fprintf(stderr, "array_of: %s\n", compound_type_name);
				}
			} else
				type_info_print(member->type, depth+1);
		}
		--depth;
		break;
	case TY_ENUM:
		break;
	default:
		assert(0);
	}
}

Type_info* type_info_build(Pool *type_pool, Pool *member_pool, AST_node *node, Type_info *dest) {
	AST_node *child, *sibling, *member_initial_val_expr;
	AST_node *arguments;
	Type_info *tinfo, *member_type;
	Type_member *member;
	Sym *symptr;
	bool is_pending;
	Sym symbol = {0};

	if(node->kind == N_TYPE)
		node = node->down;

	assert(node->kind != N_EXPRESSION);

	tinfo = NULL;
	symptr = NULL;

	switch(node->kind) {
	case N_STRUCTLIT:
		myerror("jcc: error: feature unimplemented\non compiler source line: %i\n in function %s\n",
				__LINE__, __func__);
		break;
	case N_STRLIT:
		//tinfo = builtin_types + TY_STRING; TODO create string type
		myerror("jcc: error: feature unimplemented\non compiler source line: %i\n in function %s\n",
				__LINE__, __func__);
		break;
	case N_BOOLLIT:
		tinfo = builtin_types + TY_BOOL;
		break;
	case N_INTLIT:
		tinfo = builtin_types + TY_INT;
		break;
	case N_FLOATLIT:
		tinfo = builtin_types + TY_FLOAT;
		break;
	case N_CHARLIT:
		tinfo = builtin_types + TY_CHAR;
		break;
	case N_BUILTINTYPE:
		tinfo = builtin_types + node->val.typesig;
		break;
	case N_POINTER:
		if(dest)
			tinfo = dest;
		else
			tinfo = pool_alloc(type_pool);

		tinfo->tag = TY_POINTER;
		tinfo->bytes = 8u;
		tinfo->Pointer.pointer_to = type_info_build(type_pool, member_pool, node->down,NULL); 
		break;
	case N_ARRAY:
		if(dest)
			tinfo = dest;
		else
			tinfo = pool_alloc(type_pool);

		tinfo->tag = TY_ARRAY;
		tinfo->bytes = 8u << 2;
		if(node->down->kind == N_EXPRESSION) {
			tinfo->Array.max_index_expr = node->down; /* resolve later */
			node = node->down->next;
		} else {
			node = node->down;
		}
		tinfo->Array.array_of = type_info_build(type_pool, member_pool, node,NULL); 
		break;
	case N_FUNCSIGNATURE:
		node = node->down;

		if(dest)
			tinfo = dest;
		else
			tinfo = pool_alloc(type_pool);

		tinfo->tag = TY_FUNC;
		tinfo->bytes = 8u;

		if(node->kind == N_ARGUMENTS) {
			arguments = node->down;
			child = arguments;
			tinfo->Func.arg_types = member = pool_alloc(member_pool);
			while(true) {
				member->type = type_info_build(type_pool, member_pool, child,NULL);
				child = child->next;
				if(!child)
					break;
				member->next = pool_alloc(member_pool);
				member = member->next;
			}
			node = node->next;
		}

		if(node->kind == N_BLOCK || node->kind == N_STORAGEQUALIFIER)
			break;
		else /* node is return type */
			node = node->down;

		tinfo->Func.return_types = member = pool_alloc(member_pool);
		while(true) {
			member->type = type_info_build(type_pool, member_pool, node, NULL);
			node = node->next;
			if(!(node && node->kind != N_BLOCK && node->kind != N_STORAGEQUALIFIER))
				break;
			member->next = pool_alloc(member_pool);
			member = member->next;
		}
		break;
	case N_FUNCTION:
		node = node->down;

		if(dest)
			tinfo = dest;
		else
			tinfo = pool_alloc(type_pool);

		tinfo->tag = TY_FUNC;
		tinfo->bytes = 8u;
		if(node->kind == N_ARGUMENTS) {
			arguments = node->down;
			tinfo->Func.arg_types = member = pool_alloc(member_pool);
			while(arguments && arguments->kind == N_DEC) {
				child = arguments->down;
				assert(child->kind == N_ID || child->kind == N_IDLIST);
				sibling = child->next;

				if(child->kind == N_IDLIST)
					child = child->down;
				assert(child->kind == N_ID);

				member_type = NULL;
				member_initial_val_expr = NULL;

				if(sibling->kind == N_INITIALIZER) {
					member_initial_val_expr = sibling;
					sibling = sibling->down;
				} else if(sibling->next && sibling->next->kind == N_INITIALIZER)
					member_initial_val_expr = sibling->next;

				member_type = type_info_build(type_pool, member_pool, sibling,NULL);
				while(child->kind == N_ID) {
					member->name = child->val.str;
					member->type = member_type;
					member->initial_val_expr = member_initial_val_expr;
					child = child->next;
					if(!child)
						break;
					if(child->kind != N_ID)
						break;
					member->next = pool_alloc(member_pool);
					member = member->next;
				}
				arguments = arguments->next;
			}

			node = node->next;
		}

		if(node->kind == N_BLOCK || node->kind == N_STORAGEQUALIFIER)
			break;
		else /* node is return type */
			node = node->down;

		tinfo->Func.return_types = member = pool_alloc(member_pool);
		while(true) {
			member->type = type_info_build(type_pool, member_pool, node, NULL);
			node = node->next;
			if(!(node && node->kind != N_BLOCK && node->kind != N_STORAGEQUALIFIER))
				break;
			member->next = pool_alloc(member_pool);
			member = member->next;
		}
		break;
	case N_STRUCTURE: case N_UNIONATION:
		assert(node->down->kind==N_DEC||node->down->kind==N_CONSTDEC||node->down->kind==N_STRUCTURE||node->down->kind==N_UNIONATION);

		if(dest)
			tinfo = dest;
		else
			tinfo = pool_alloc(type_pool);

		tinfo->bytes = 0;

		if(node->kind == N_STRUCTURE)
			tinfo->tag = TY_STRUCT;
		else
			tinfo->tag = TY_UNION;
		tinfo->Struct.members = member = pool_alloc(member_pool);
		node = node->down;

		while(node) {
			if(node->kind == N_DEC || node->kind == N_CONSTDEC) {
				child = node->down;
				sibling = child->next;
				if(sibling->kind != N_TYPE)
					myerror("jcc: error: struct or union member missing explicit type\nline: %i, col: %i\n",
							sibling->debug_info.line, sibling->debug_info.col);
				if(child->kind == N_IDLIST)
					child = child->down;
				member->type = type_info_build(type_pool, member_pool, sibling,NULL);
				sibling = sibling->next;
				member->initial_val_expr = NULL;
				if(sibling && sibling->kind == N_INITIALIZER)
					member->initial_val_expr = sibling;
				member->constant = (node->kind == N_CONSTDEC);
				while(true) {
					member->name = child->val.str;
					child = child->next;
					if(!(child && child->kind == N_ID))
						break;
					member->next = pool_alloc(member_pool);
					member->next->type = member->type;
					member->next->constant = member->constant;
					member->next->initial_val_expr = member->initial_val_expr;
					member = member->next;
				}
			} else if(node->kind == N_STRUCTURE || node->kind == N_UNIONATION) {
				member->name = NULL;
				member->type = type_info_build(type_pool, member_pool, node,NULL);
			} else if(node->kind == N_ENUMERATION) {
				myerror("jcc: error: feature unimplemented\non compiler source line: %i\n in function %s\n",
						__LINE__, __func__); // TODO what does this mean?
			} else {
				assert(0);
			}

			node = node->next;
			if(node) {
				member->next = pool_alloc(member_pool);
				member = member->next;
			}
		}
		break;
	case N_ENUMERATION:
		myerror("jcc: error: feature unimplemented\non compiler source line: %i\n in function %s\n",
				__LINE__, __func__); // TODO
		break;
	case N_ID:
		/* lookup identifier in symbol table, expect a Type if it isn't
		 * in the table, create the symptr and append tinfo to the pending list */
		is_pending = false;
		for(int i = scope_depth; i >= 0; --i) {
			symptr = sym_tab_look(blocktab+scope_depth, node->val.str);
			if(symptr)
				break;
		}
		if(!symptr)
			symptr = sym_tab_look(&functab, node->val.str);
		if(!symptr)
			symptr = sym_tab_look(&globaltab, node->val.str);
		if(!symptr) {
			symptr = sym_tab_look(&pendingtab, node->val.str);
			is_pending = true;
		}

		if(symptr) {
			if(!symptr->constant && !is_pending)
				myerror("jcc: error: use of non constant symbol '%s' as type\nline: %i, col: %i\n",
						symptr->name, node->debug_info.line, node->debug_info.col);
			tinfo = symptr->val.t;
			break;
		} else { /* define symbol in pendingtab */
			symbol.name = node->val.str;
			symbol.val.t = pool_alloc(type_pool);
			symbol.val.t->bytes = 0;
			symbol.debug_info = node->debug_info;
			sym_tab_def(&pendingtab, &symbol);
			tinfo = symbol.val.t;
			/* NOTE when we find the definition of this symbol remember not to
			 * allocate a new Type_info for val.t */
		}
		break;
	default:
		fprintf(stderr, "######### printing node\n");
		fwrite(node->debug_info.line_s, 1, node->debug_info.line_e - node->debug_info.line_s, stderr);
		ast_print(node,0);
		assert(0);
		break;
	}

	return tinfo;
}

void resolve_compound_type(Type_info *tinfo) {
	Type_member *member;
	size_t offset;
	size_t largest;

	if(tinfo->bytes != 0 || tinfo->tag == TY_VOID)
		return;

	if(tinfo->tag == TY_STRUCT) {
		member = tinfo->Struct.members;
		offset = 0;

		while(member) {
			if(member->type == tinfo)
				myerror("jcc: error: struct '%s' was recursively defined on line: %i, col: %i\n",
						tinfo->Struct.name, tinfo->Struct.debug_info.line, tinfo->Struct.debug_info.col);

			if(member->type->bytes == 0)
				resolve_compound_type(member->type);
			assert(member->type->bytes != 0);
			member->byte_offset = offset;
			offset += member->type->bytes;
			member = member->next;
		}

		tinfo->bytes = offset;
	} else if(tinfo->tag == TY_UNION) {
		member = tinfo->Union.members;
		largest = 0;

		while(member) {
			if(member->type == tinfo)
				myerror("jcc: error: union '%s' was recursively defined on line: %i, col: %i\n",
						tinfo->Union.name, tinfo->Union.debug_info.line, tinfo->Union.debug_info.col);

			if(member->type->bytes == 0)
				resolve_compound_type(member->type);
			assert(member->type->bytes != 0);
			if(member->type->bytes > largest)
				largest = member->type->bytes;
			member = member->next;
		}

		tinfo->bytes = largest;
	} else {
		assert(tinfo->bytes == 0);
	}
}

void sym_tab_build(Sym_tab *tab, AST_node *root) {
	AST_node *node, *child, *sibling;
	Type_info *tinfo;
	Sym sym;
	Sym *symptr;
	size_t *seg_count;

	/* declare compound types first */
	for(node = root; node; node = node->next) {
		if(node->kind != N_CONSTDEC && node->kind != N_DEC)
			continue;

		tinfo = NULL;
		child = node->down;

		sibling = child->next;
		if(sibling->kind != N_INITIALIZER)
			sibling = sibling->next;
		if(!sibling)
			continue;

		assert(sibling->kind == N_INITIALIZER);

		sibling = sibling->down;
		if(sibling->kind != N_STRUCTURE && sibling->kind != N_UNIONATION && sibling->kind != N_ENUMERATION)
			continue;

		node->visited = true;
		sym = (Sym){0};

		sym.constant = (node->kind == N_CONSTDEC);
		sym.type = builtin_types + TY_TYPE;

		assert(child->kind == N_ID || child->kind == N_IDLIST);

		if(child->kind == N_IDLIST)
			child = child->down;
		while(child && child->kind == N_ID) {
			sym.name = child->val.str;
			sym.debug_info = child->debug_info;

			symptr = sym_tab_look(&pendingtab, sym.name);

			if(!symptr)
				tinfo = pool_alloc(&type_pool);
			if(symptr) {
				if(!sym.constant)
					myerror("jcc: error: '%s' was not declared as constant, to use it in type declarations it must be constant\nline: %i, col: %i\n",
							symptr->name, child->debug_info.line, child->debug_info.col);
				tinfo = symptr->val.t;
				*symptr = (Sym){0};
			}

			sym.val.t = tinfo;
			sym.seg = SEG_DATA;
			sym.addr = tab->seg_count.data;
			tab->seg_count.data += sym.type->bytes;

			if(!sym_tab_def(tab, &sym)) {
				myerror("jcc: error: redefinition of '%s', line: %i, col: %i\n",
						child->val.str, child->debug_info.line, child->debug_info.col);
			}

			type_info_build(&type_pool, &member_pool, sibling, tinfo);

			if(tinfo->tag == TY_STRUCT) {
				tinfo->Struct.name = sym.name;
				tinfo->Struct.debug_info = sym.debug_info;
			} else if(tinfo->tag == TY_UNION) {
				tinfo->Union.name = sym.name;
				tinfo->Union.debug_info = sym.debug_info;
			} else if(tinfo->tag == TY_ENUM) {
				tinfo->Enum.name = sym.name;
				tinfo->Enum.debug_info = sym.debug_info;
			} else
				assert(0);

			child = child->next;
		}
	}

	for(size_t i = 0; i < pendingtab.cap; ++i) {
		symptr = pendingtab.data + i;
		if(symptr->name != NULL)
			myerror("jcc: error: '%s' was not defined, first referenced on line: %i, col: %i\n",
						symptr->name, symptr->debug_info.line, symptr->debug_info.col);
	}

	/* set sizes of structs and unions */
	for(size_t i = 0; i < type_pool.pagecount; ++i) {
		Type_info *tinfo_array;
		tinfo_array = type_pool.pages[i];
		for(size_t j = 0; j < type_pool.pagesize; ++j) {
			tinfo = tinfo_array + j;
			if(tinfo->tag != TY_STRUCT && tinfo->tag != TY_UNION)
				continue;
			if(tinfo->bytes != 0)
				continue;
			resolve_compound_type(tinfo);
		}
	}

	for(node = root; node; node = node->next) {
		if(node->visited || (node->kind != N_CONSTDEC && node->kind != N_DEC && node->kind != N_ENUMERATION))
			continue;

		if(node->kind == N_ENUMERATION) {
			myerror("jcc: error: feature unimplemented\non compiler source line: %i\n in function %s\n",
					__LINE__, __func__); // TODO implement anonymous enum
		}

		sym = (Sym){0};

		child = node->down;
		assert(child->kind == N_ID || child->kind == N_IDLIST);
		sibling = child->next;

		if(sibling->kind == N_TYPE) {
			tinfo = type_info_build(&type_pool, &member_pool, sibling, NULL);
			sibling = sibling->next;
		} else if(sibling->kind == N_INITIALIZER) {
			assert(sibling->down->kind != N_EXPRESSION);
			tinfo = type_info_build(&type_pool, &member_pool, sibling->down, NULL);
		} else {
			assert(0);
		}

		sym.type = tinfo;
		if(sibling) {
			if(tinfo->tag == TY_TYPE) {
				if(sibling->down->down->kind != N_TYPE)
					myerror("jcc: error: attempted to initialize var of type 'Type' with wrong initializer\nline: %i, col: %i\n", sibling->debug_info.line, sibling->debug_info.col);
				sym.val.t = type_info_build(&type_pool, &member_pool, sibling->down->down, NULL);
			} else
				sym.val.a = sibling;
		}

		sym.constant = (node->kind == N_CONSTDEC);

		switch(tab->scope) {
		case SCOPE_GLOBAL:
			if(sym.val.a == NULL && sym.val.t == NULL) {
				sym.seg = SEG_BSS;
				seg_count = &(tab->seg_count.bss);
			} else {
				sym.seg = SEG_DATA;
				seg_count = &(tab->seg_count.data);
			}
			break;
		case SCOPE_ARGUMENT:
			sym.seg = SEG_ARG;
			seg_count = &(tab->seg_count.arg);
			break;
		case SCOPE_LOCAL:
			sym.seg = SEG_STACK;
			seg_count = &(tab->seg_count.stack);
			break;
		default:
			assert(0);
		}

		if(tinfo->tag == TY_FUNC) {
			sym.seg = SEG_TEXT;
			seg_count = &(tab->text_count);
		}

		if(child->kind == N_IDLIST)
			child = child->down;
		while(child && child->kind == N_ID) {
			sym.name = child->val.str;
			sym.debug_info = child->debug_info;
			sym.addr = *seg_count;
			*seg_count += sym.type->bytes;
			if(!sym_tab_def(tab, &sym)) {
				myerror("jcc: error: redefinition of '%s', line: %i, col: %i\n",
						child->val.str, child->debug_info.line, child->debug_info.col);
			}
			child = child->next;
		}
	}
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

int sym_tab_def(Sym_tab *tab, Sym *symbol) {
	if(tab->count >= tab->cap) sym_tab_grow(tab);
	size_t i = sym_tab_hash(tab, symbol->name);
	while(tab->data[i].name) {
		if(!strcmp(tab->data[i].name, symbol->name))
			return 0;
		i = (i + 1) % tab->cap;
	}
	tab->data[i] = *symbol;
	++tab->count;
	return 1;
}

int sym_tab_undef(Sym_tab *tab, char *name) {
	size_t i, origin;
	origin = i = sym_tab_hash(tab, name);
	do {
		if(tab->data[i].name && !strcmp(tab->data[i].name, name)) {
			tab->data[i].name = NULL;
			return 1;
		}
		i = (i + 1) % tab->cap;
	} while(i != origin);
	return 0;
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
	tab->count = 0;
	tab->seg_count.data = 0;
	tab->seg_count.bss = 0;
	tab->seg_count.arg = 0;
	tab->seg_count.stack = 0;
	for(size_t i = 0; i < tab->cap; ++i)
		tab->data[i].name = NULL;
}

void sym_tab_print(Sym_tab *tab) {
	fprintf(stderr, "SYMBOL TABLE: %s\n",tab->name);
	for(size_t i = 0; i < tab->cap; ++i) {
		if(tab->data[i].name == 0)
			continue;
		fprintf(stderr, "SYM\n*   name: %s\n*   constant: %i\n*   seg: %s\n*   addr: %lu\n*   type:\n",
				tab->data[i].name, tab->data[i].constant, segments_debug[tab->data[i].seg], (uint64_t)(tab->data[i].addr));
		type_info_print(tab->data[i].type, 2);
		fprintf(stderr, "*   val:\n");
		if(tab->data[i].type->tag == TY_TYPE)
			type_info_print(tab->data[i].val.t, 2);
		else
			ast_print(tab->data[i].val.a, 2);
	}
}

int bc_interpreter(BCinst *prog, BCmem *mem) {
	BCinst *pc;
	BCreg *r1, *r2;
	register uint64_t r1_data, r2_data, imm, addr, seg;

	pc = prog;
	while(true) {
		r1 = mem->regs + pc->r1;
		r2 = mem->regs + pc->r2;
		r1_data = *r1;
		r2_data = *r2;
		imm = pc->imm;
		addr = pc->addr;
		seg = pc->seg;

		switch(pc->opcode) {
		case BCOP_SET:
			mem->regs[pc->dest_r] = imm;
			break;
		case BCOP_ADD:
			mem->regs[pc->dest_r] = r1_data + r2_data;
			break;
		case BCOP_SUB:
			mem->regs[pc->dest_r] = r1_data - r2_data;
			break;
		case BCOP_MUL:
			mem->regs[pc->dest_r] = r1_data * r2_data;
			break;
		case BCOP_DIV:
			mem->regs[pc->dest_r] = r1_data / r2_data;
			break;
		case BCOP_MOD:
			mem->regs[pc->dest_r] = r1_data % r2_data;
			break;
		case BCOP_AND:
			mem->regs[pc->dest_r] = r1_data & r2_data;
			break;
		case BCOP_OR:
			mem->regs[pc->dest_r] = r1_data | r2_data;
			break;
		case BCOP_XOR:
			mem->regs[pc->dest_r] = r1_data ^ r2_data;
			break;
		case BCOP_NOT:
			mem->regs[pc->dest_r] = ~r1_data;
			break;
		case BCOP_LSHIFT:
			mem->regs[pc->dest_r] = r1_data << r2_data;
			break;
		case BCOP_RSHIFT:
			mem->regs[pc->dest_r] = r1_data >> r2_data;
			break;
		case BCOP_JMP:
			pc = prog + addr;
			break;
		case BCOP_JLT:
			if(r1_data < r2_data)
				pc = prog + addr;
			break;
		case BCOP_JGE:
			if(r1_data >= r2_data)
				pc = prog + addr;
			break;
		case BCOP_JNE:
			if(r1_data != r2_data)
				pc = prog + addr;
			break;
		case BCOP_JEQ:
			if(r1_data == r2_data)
				pc = prog + addr;
			break;
		case BCOP_LOADB:
			*r1 = *(uint8_t*)(mem->seg[seg]+addr) & 0xff;
			break;
		case BCOP_LOADS:
			*r1 = *(uint16_t*)(mem->seg[seg]+addr) & 0xffff;
			break;
		case BCOP_LOADH:
			*r1 = *(uint32_t*)(mem->seg[seg]+addr) & 0xffffffff;
			break;
		case BCOP_LOADW:
			*r1 = *(uint64_t*)(mem->seg[seg]+addr);
			break;
		case BCOP_STOREB:
			*(uint8_t*)(mem->seg[seg]+addr) = (uint8_t)(*r1 & 0xff);
			break;
		case BCOP_STORES:
			*(uint16_t*)(mem->seg[seg]+addr) = (uint16_t)(*r1 & 0xffff);
			break;
		case BCOP_STOREH:
			*(uint32_t*)(mem->seg[seg]+addr) = (uint32_t)(*r1 & 0xffffffff);
			break;
		case BCOP_STOREW:
			*(uint64_t*)(mem->seg[seg]+addr) = *r1;
			break;
		case BCOP_PUSHW:
			*(uint64_t*)(mem->stack_ptr) = *r1;
			mem->stack_ptr += sizeof(uint64_t);
			break;
		case BCOP_POPW:
			*r1 = *(uint64_t*)(mem->stack_ptr);
			mem->stack_ptr -= sizeof(uint64_t);
			break;
		case BCOP_PUSHH:
			*(uint32_t*)(mem->stack_ptr) = *r1;
			mem->stack_ptr += sizeof(uint32_t);
			break;
		case BCOP_POPH:
			*r1 = *(uint32_t*)(mem->stack_ptr);
			mem->stack_ptr -= sizeof(uint32_t);
			break;
		case BCOP_PUSHS:
			*(uint16_t*)(mem->stack_ptr) = *r1;
			mem->stack_ptr += sizeof(uint16_t);
			break;
		case BCOP_POPS:
			*r1 = *(uint16_t*)(mem->stack_ptr);
			mem->stack_ptr -= sizeof(uint16_t);
			break;
		case BCOP_PUSHB:
			*(uint8_t*)(mem->stack_ptr) = *r1;
			mem->stack_ptr += sizeof(uint8_t);
			break;
		case BCOP_POPB:
			*r1 = *(uint8_t*)(mem->stack_ptr);
			mem->stack_ptr -= sizeof(uint8_t);
			break;
		case BCOP_CALL:
			break;
		case BCOP_RET:
			break;
		case BCOP_INT:
			break;
		case BCOP_HALT:
			return 0;
		}

		++pc;
	}
}

void parse_error(char *expect) {
	fprintf(stderr, "jcc: parse error: expected %s got '", expect);
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fprintf(stderr, "'\n>\tline: %i, col: %i\n>\t", lexer.debug_info.line, lexer.debug_info.col - (int)(lexer.ptr - lexer.unget));
	fwrite(lexer.debug_info.line_s, 1, lexer.debug_info.line_e - lexer.debug_info.line_s, stderr);
	exit(1);
}

void myerror(char *fmt, ...) {
	va_list args;

	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(1);
}

void lexer_init(Lexer *l, char *buf) {
	l->token = 0;
	l->text_s = l->text_e = l->unget = NULL;
	l->ptr = l->src = buf;
	l->debug_info = (Debug_info){ .line = 1, .col = 1, .line_s = buf };
	for(l->debug_info.line_e = l->ptr; *(l->debug_info.line_e) != '\n'; ++(l->debug_info.line_e));
	++(l->debug_info.line_e);
}

int lex(void) {
	char *tp, *s;
	int check;

	lexer.text_s = lexer.text_e = NULL;

	while(true) {
		while(isspace(*lexer.ptr)) { /* whitespace */
			if(*lexer.ptr != '\n') {
				++lexer.debug_info.col;
			} else {
				lexer.debug_info.col = 1;
				++lexer.debug_info.line;
				lexer.debug_info.line_e = lexer.debug_info.line_s = lexer.ptr + 1;
			}
			++lexer.ptr;
		}

		/* single-line comments and prepocessor lines */
		if((lexer.ptr[0] == '/' && lexer.ptr[1] == '/') || lexer.ptr[0] == '#') {
			while(*lexer.ptr != '\n')
				++lexer.ptr;
			lexer.debug_info.col = 1;
		} else if(lexer.ptr[0] == '/' && lexer.ptr[1] == '*') { /* multi-line comments */
			while(!(lexer.ptr[0] == '*' && lexer.ptr[1] == '/'))
				lexer.debug_info.line += (*lexer.ptr++ == '\n');
			lexer.ptr += 2;
		} else
			break;
	}

	if(lexer.ptr >= lexer.debug_info.line_e) {
		lexer.debug_info.line_s = lexer.debug_info.line_e;
		for(s = lexer.ptr; *s && *s != '\n'; ++s);
		lexer.debug_info.line_e = s + 1;
	}

	tp = lexer.text_s = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0) {
		lexer.eof = 1;
		return T_END;
	}

	lexer.token = 0;

	if(tp[0] == '-' && tp[1] == '-' && tp[2] == '-') {
		lexer.debug_info.col += 3;
		lexer.ptr += 3;
		return (lexer.token = T_BAR);
	}

	if(tp[0] == '<' && tp[1] == '<' && tp[2] == '=') {
		lexer.debug_info.col += 3;
		lexer.ptr += 3;
		return (lexer.token = T_ASSIGNLSHIFT);
	}

	if(tp[0] == '>' && tp[1] == '>' && tp[2] == '=') {
		lexer.debug_info.col += 3;
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
		lexer.debug_info.col += 2;
		return lexer.token;
	}

	switch(*tp) {
	case '{': case '}': case '(': case ')': case '[': case ']': case '.':
	case ',': case ';': case '=': case '~': case '*': case '/': case '<':
	case '>': case '&': case '|': case '+': case '-': case '^': case '%':
	case ':':
		++lexer.debug_info.col;
		++lexer.ptr;
		lexer.token = *tp;
	}

	if(lexer.token) {
		if(tp[1] == tp[0] && (*tp == '<' || *tp == '>' || *tp == '+' || *tp == '-')) {
			++lexer.debug_info.col;
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
			lexer.debug_info.col += keyword_length[i];
			return (lexer.token = T_ENUM + i);
		}
	}

	/* true, false, numbers, strings and identifiers */
	if(strstr(tp, "true") == tp) {
		lexer.text_e = tp + STRLEN("true");
		lexer.ptr += STRLEN("true");
		lexer.debug_info.col += STRLEN("true");
		return (lexer.token = T_TRUE);
	}

	if(strstr(tp, "false") == tp) {
		lexer.text_e = tp + STRLEN("false");
		lexer.ptr += STRLEN("false");
		lexer.debug_info.col += STRLEN("false");
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
		lexer.debug_info.col += s - tp;
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_INTEGER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		lexer.debug_info.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STR);
	}

	if(*tp == '\'') {
		for(s = tp + 1; (check = *s) && *s != '\''; ++s);
		lexer.debug_info.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_CHARACTER);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = T_ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.debug_info.col += s - tp;
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
		parse_error("end of file");
	}
}

/*
 * TODO
 * globalstatement
AST_node* globalstatement(void) {
	register size_t t;
	AST_node *node;


}
 */

/*
 * declaration: identifier (',' identifier)* ':' type? ('='|':') (literal';'|function|structure|unionation|enumeration)
 */
AST_node* declaration(void) {
	register int t;
	AST_node *root, *child, *tmp;
	char *unget1, *unget2;
	bool explicit_type = false;

	t = lex();
	unget1 = lexer.unget;

	if(t == T_ENUM) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return enumeration();
	}

	if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
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
		lexer.debug_info.col -= lexer.ptr - unget1;
		lexer.ptr = unget1;
		return NULL;
	}

	lexer.debug_info.col -= lexer.ptr - unget2;
	lexer.ptr = unget2;
	t = lex();

	root = ast_alloc_node(&ast, N_DEC, &lexer.debug_info);
	root->down = child = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
	child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);

	t = lex();

	if(t == ',') {
		tmp = child;
		root->down = child = ast_alloc_node(&ast, N_IDLIST, &lexer.debug_info);
		child->down = tmp;
	}

	while(t == ',') {
		t = lex();
		if(t != T_ID)
			parse_error("identifier");
		tmp->next = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		tmp = tmp->next;
		tmp->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ':')
		parse_error("':'");

	child->next = type();
	explicit_type = (bool)child->next;

	t = lex();

	if(child->next && t == ';')
		return root;
	else if(!child->next && t == ';')
		parse_error("type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc_node(&ast, N_INITIALIZER, &lexer.debug_info);
	child = child->next;

	if(t == ':')
		root->kind = N_CONSTDEC;
	else if(t != '=')
		parse_error("'=' or ':'");

	/* body */
	if((child->down = function())) return root;
	if((child->down = structure())) return root;
	if((child->down = unionation())) return root;
	if((child->down = enumeration())) return root;

	if(explicit_type)
		child->down = expression();
	else
		child->down = literal();

	if(child->down) {
		t = lex();
		if(t != ';')
			parse_error("';'");
		return root;
	}

	parse_error("initializer");

	return NULL;
}

AST_node* vardec(void) {
	register int t;
	AST_node *root, *child, *tmp;
	char *unget1, *unget2;
	bool explicit_type = false;

	t = lex();
	unget1 = lexer.unget;

	if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
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
		lexer.debug_info.col -= lexer.ptr - unget1;
		lexer.ptr = unget1;
		return NULL;
	}

	lexer.debug_info.col -= lexer.ptr - unget2;
	lexer.ptr = unget2;
	t = lex();

	root = ast_alloc_node(&ast, N_DEC, &lexer.debug_info);
	root->down = child = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
	child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);

	t = lex();

	if(t == ',') {
		tmp = child;
		root->down = child = ast_alloc_node(&ast, N_IDLIST, &lexer.debug_info);
		child->down = tmp;
	}

	while(t == ',') {
		t = lex();
		if(t != T_ID)
			parse_error("identifier");
		tmp->next = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		tmp = tmp->next;
		tmp->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ':')
		parse_error("':'");

	child->next = type();
	explicit_type = (bool)child->next;

	t = lex();

	if(child->next && (t == ',' || t == ')')) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	}
	else if(!child->next && (t == ',' || t == ')'))
		parse_error("type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc_node(&ast, N_INITIALIZER, &lexer.debug_info);
	child = child->next;

	if(t != '=')
		parse_error("'=' or ':'");

	if(explicit_type)
		child->down = expression();
	else
		child->down = literal();

	if(child->down)
		return root;

	parse_error("initializer");

	return NULL;
}

AST_node* literal(void) {
	register int t;
	AST_node *root;

	root = NULL;
	t = lex();

	if((t >= T_INTEGER  && t <= T_FALSE) || t == T_BAR || t == '{')
		root = ast_alloc_node(&ast, 0, &lexer.debug_info);
	else if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root = type();
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	switch(t) {
	case T_INTEGER:
		root->kind = N_INTLIT;
		root->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_STR:
		root->kind = N_STRLIT;
		root->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_CHARACTER:
		root->kind = N_CHARLIT;
		root->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_TRUE: case T_FALSE:
		root->kind = N_BOOLLIT;
		root->val.boolean = (t == T_TRUE);
		break;
	case T_BAR:
		root->kind = N_UNINIT;
		break;
	//case '.':
	//	s = lexer.text_s;
	//	if(lex() != T_ID)
	//		parse_error("identifier");
	//	if(lex() != ')')
	//		parse_error("')'");
	//	if(lex() != '{')
	//		parse_error("'{'");
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

		root = ast_alloc_node(&ast, N_TYPE, &lexer.debug_info);

		if(t == '*') {
			root->down = child = ast_alloc_node(&ast, N_POINTER, &lexer.debug_info);
			loop = true;
		} else {
			root->down = child = ast_alloc_node(&ast, N_ARRAY, &lexer.debug_info);

			t = lex();
			if(t != ']') {
				lastwasexpr = true;
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
				child->down = expression();
				if(!child->down)
					parse_error("expression");
				child = child->down;
				t = lex();
				if(t != ']')
					parse_error("']'");
			}

			loop = true;
		}

		while(loop) {
			t = lex();

			if(t == '*') {
				if(lastwasexpr) {
					child->next = ast_alloc_node(&ast, N_POINTER, &lexer.debug_info);
					child = child->next;
				} else {
					child->down = ast_alloc_node(&ast, N_POINTER, &lexer.debug_info);
					child = child->down;
				}
				lastwasexpr = false;
				continue;
			}

			if(t != '[') {
				break;
			}

			if(lastwasexpr) {
				child->next = ast_alloc_node(&ast, N_ARRAY, &lexer.debug_info);
				child = child->next;
			} else {
				child->down = ast_alloc_node(&ast, N_ARRAY, &lexer.debug_info);
				child = child->down;
			}

			t = lex();
			if(t == ']') {
				lastwasexpr = false;
				continue;
			}

			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->down = expression();
			if(!child->down)
				parse_error("expression");
			child = child->down;

			lastwasexpr = true;

			t = lex();
			if(t != ']')
				parse_error("']'");
		}
	}

	if(!(t >= T_INT && t <= T_S64) && t != T_ID &&
			t != T_FUNC && t != T_STRUCT && t != T_UNION && t != T_ENUM) {
		if(indirect)
			parse_error("type");

		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	node = NULL;

	switch(t) {
	case T_STRUCT:
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		node = structure();
		break;
	case T_ENUM:
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		node = enumeration();
		break;
	case T_UNION:
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		node = unionation();
		break;
	}

	if(!node)
		node = ast_alloc_node(&ast, 0, &lexer.debug_info);

	if(!indirect) {
		root = ast_alloc_node(&ast, N_TYPE, &lexer.debug_info);
		root->down = child = node;
	} else {
		if(lastwasexpr) {
			child->next = node;
			child = child->next;
		} else {
			child->down = node;
			child = child->down;
		}
	}

	switch(t) {
	case T_STRUCT: case T_UNION: case T_ENUM:
		break;
	case T_ID:
		child->kind = N_ID;
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_FUNC:
		child->kind = N_FUNCSIGNATURE;
		t = lex();
		if(t != '(')
			parse_error("'('");
		child->down = ast_alloc_node(&ast, N_ARGUMENTS, &lexer.debug_info);
		child = child->down;
		child->down = node = type();
		t = lex();
		if(t != ',' && t != ')')
			parse_error("',' or ')'");
		while(t != ')') {
			node->next = type();
			node = node->next;
			t = lex();
			if(t != ',' && t != ')')
				parse_error("',' or ')'");
		}
		t = lex();
		if(t != '(') {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		} else {
			child->next = ast_alloc_node(&ast, N_RETURNTYPE, &lexer.debug_info);
			child = child->next;
			child->down = node = type();
			t = lex();
			if(t != ',' && t != ')')
				parse_error("',' or ')'");
			while(node && t != ')') {
				node->next = type();
				node = node->next;
				t = lex();
				if(t != ',' && t != ')')
					parse_error("',' or ')'");
			}
		}
		break;
	default: /* builtin type */
		assert(t >= T_INT && t <= T_S64);
		child->kind = N_BUILTINTYPE;
		child->val.typesig = t - T_INT + TY_INT;
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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;

	root = ast_alloc_node(&ast, N_FUNCTION, &lexer.debug_info);

	t = lex();
	if(t == '(') {
		node = vardec();
		if(node) {
			root->down = child = ast_alloc_node(&ast, N_ARGUMENTS, &lexer.debug_info);
			child->down = node;
		}

		t = lex();
		while(t == ',') {
			node->next = vardec();
			if(node->next)
				node = node->next;
			t = lex();
		}

		if(t != ')')
			parse_error("')'");
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	/* return type */
	node = type();
	if(node) {
		if(!child) {
			root->down = child = ast_alloc_node(&ast, N_RETURNTYPE, &lexer.debug_info);
			child->down = node;
		} else {
			child->next = ast_alloc_node(&ast, N_RETURNTYPE, &lexer.debug_info);
			child = child->next;
			child->down = node;
		}
	}

	t = lex();
	if(t == ',') {
		node->next = type();
		if(node->next)
			node = node->next;
		t = lex();
		while(t == ',') {
			node->next = type();
			if(node->next)
				node = node->next;
			t = lex();
		}
	}

	if(t == T_INLINE) {
		node = ast_alloc_node(&ast, N_STORAGEQUALIFIER, &lexer.debug_info);
		node->val.str = keyword[T_INLINE-T_ENUM];
		if(!child)
			root->down = child = node;
		else {
			child->next = node;
			child = child->next;
		}

		t = lex();
	}

	if(t != '{')
		parse_error("'{'");

	node = block();
	if(!child && node)
		root->down = child = node;
	else if(node) {
		child->next = node;
		child = child->next;
	}

	t = lex();
	if(t != '}')
		parse_error("'}'");

	return root;
}

/*
 * block: (statement|ifstatement|whilestatement|forstatement|returnstatement|declaration)*
 */
AST_node* block(void) {
	register int t;
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_BLOCK, &lexer.debug_info);

	if((child = returnstatement())) root->down = child;
	else if((child = ifstatement())) root->down = child;
	else if((child = whilestatement())) root->down = child;
	else if((child = forstatement())) root->down = child;
	else if((child = declaration())) root->down = child;
	else if((child = statement())) root->down = child;

	t = lex();
	if(t == '{') {
		child = block();
		if((t = lex()) != '}')
			parse_error("'}'");
	} else if(t == '}') {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	for(; child; child = child->next) {
		if((child->next = returnstatement())) continue;
		if((child->next = ifstatement())) continue;
		if((child->next = whilestatement())) continue;
		if((child->next = forstatement())) continue;
		if((child->next = declaration())) continue;
		if((child->next = statement())) continue;

		if((t = lex()) == '{') {
			child->next = block();
			if((t = lex()) != '}')
				parse_error("'}'");
			continue;
		}

		lexer.debug_info.col -= lexer.ptr - lexer.unget;
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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_STRUCTURE, &lexer.debug_info);

	t = lex();
	if(t != '{')
		parse_error("'{'");

	t = lex();
	if(t == T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = declaration();
	} else if(t == T_UNION) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = unionation();
		t = lex();
		if(t == '=') {
			parse_error("next member");
		} else {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else if(t == T_STRUCT) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = structure();
		t = lex();
		if(t == '=') {
			parse_error("next member");
		} else {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else {
		parse_error("identifier or 'union'");
	}

	root->down = child;

	while((t = lex()) != '}') {
		if(t == T_ID) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = declaration();
		} else if(t == T_UNION) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t == '=') {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else if(t == T_STRUCT) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = structure();
			t = lex();
			if(t == '=') {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error("identifier or 'union'");
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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_UNIONATION, &lexer.debug_info);

	t = lex();
	if(t != '{')
		parse_error("'{'");

	t = lex();
	if(t == T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = declaration();
	} else if(t == T_UNION) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = unionation();
		t = lex();
		if(t == '=') {
			parse_error("next member");
		} else {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else if(t == T_STRUCT) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		child = structure();
		t = lex();
		if(t == '=') {
			parse_error("next member");
		} else {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		}
	} else {
		parse_error("identifier or 'union'");
	}

	root->down = child;

	while((t = lex()) != '}') {
		if(t == T_ID) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = declaration();
		} else if(t == T_UNION) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t == '=') {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else if(t == T_STRUCT) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = structure();
			t = lex();
			if(t == '=') {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error("identifier or 'union'");
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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = NULL;
	root = ast_alloc_node(&ast, N_ENUMERATION, &lexer.debug_info);

	t = lex();
	if(t >= T_INT && t <= T_S64) {
		root->down = child = ast_alloc_node(&ast, N_BUILTINTYPE, &lexer.debug_info);
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != '{')
		parse_error("'{'");

	t = lex();
	if(t != T_ID)
		parse_error("identifier");

	if(!child) {
		root->down = child = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else {
		child->next = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	t = lex();
	if(t == '=') {
		t = lex();
		if(t != T_INTEGER && t != T_CHARACTER)
			parse_error("numerical or character constant");
		child->next = ast_alloc_node(&ast, N_INTLIT, &lexer.debug_info);
		child = child->next;
		if(t == T_CHARACTER)
			child->kind = N_CHARLIT;
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ',' && t != '}')
		parse_error("',' or '}'");

	while(t != '}') {
		t = lex();

		if(t == '}')
			break;

		if(t != T_ID)
			parse_error("identifier");
		child->next = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		if(t == '=') {
			t = lex();
			if(t != T_INTEGER && t != T_CHARACTER)
				parse_error("numerical or character constant");
			child->next = ast_alloc_node(&ast, N_INTLIT, &lexer.debug_info);
			child = child->next;
			if(t == T_CHARACTER)
				child->kind = N_CHARLIT;
			child->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
			t = lex();
		}

		if(t != ',' && t != '}')
			parse_error("',' or '}'");
	}

	return root;
}

/*
 * statement: ('defer')? expression (assign expression)? ';'
 */
AST_node* statement(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t == ';' || t == '}' || t == T_END) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	if(t == T_DEFER) {
		root = ast_alloc_node(&ast, N_DEFER, &lexer.debug_info);
		root->down = child = assignment();
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root = ast_alloc_node(&ast, N_STATEMENT, &lexer.debug_info);
		root->down = child = assignment();
	}

	t = lex();
	if(t != ';')
		parse_error("';'");

	return root;
}

/*
 * ifstatement:
 * 	'if' ' ' expression '{' block '}' ('elif' ' ' expression '{' block '}')* ('else' '{' block '}')?
 */
AST_node* ifstatement(void) {
	register int t;
	AST_node *root, *child;

	if(lex() != T_IF) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_IFSTATEMENT, &lexer.debug_info);

	root->down = child = expression();

	if(lex() != '{')
		parse_error("'{'");
	child->next = block();
	if(lex() != '}')
		parse_error("'}'");
	child = child->next;

	while(1) {
		t = lex();
		if(t == T_ELIF) {
			child->next = expression();
			child = child->next;
		} else if(t != T_ELSE) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			break;
		}

		if(lex() != '{')
			parse_error("'{'");
		child->next = block();
		if(lex() != '}')
			parse_error("'}'");
		child = child->next;
	}

	return root;
}

/*
 * whilestatement:
 * 	'while' ' ' expression '{' block '}'
 */
AST_node* whilestatement(void) {
	AST_node *root, *child;

	if(lex() != T_WHILE) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_WHILESTATEMENT, &lexer.debug_info);

	root->down = child = expression();

	if(lex() != '{')
		parse_error("'{'");
	child->next = block();
	if(lex() != '}')
		parse_error("'}'");

	return root;
}

/*
 * returnstatement: 'return' expression ';'
 */
AST_node* returnstatement(void) {
	register int t;
	AST_node *root, *child;

	if(lex() != T_RETURN) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_RETURNSTATEMENT, &lexer.debug_info);

	root->down = child = assignment();

	t = lex();
	while(t == ',') {
		child->next = assignment();
		child = child->next;
		t = lex();
	}

	if(t != ';')
		parse_error("';'");

	return root;
}

/*
 * forstatement: 'for' (vardec ','?)* ';' expression ';' expression '{' block '}'
 */
AST_node* forstatement(void) {
	register int t;
	AST_node *root, *child;

	t = lex();
	if(t != T_FOR) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_FORSTATEMENT, &lexer.debug_info);
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
		parse_error("';'");
	if(child)
		child->next = expression();
	else
		root->down = child = expression();
	if(child->next)
		child = child->next;

	t = lex();
	if(t != ';')
		parse_error("';'");

	if(child)
		child->next = assignment();
	else
		root->down = child = assignment();
	if(child->next)
		child = child->next;
	t = lex();
	while(t == ',') {
		child->next = assignment();
		if(child->next)
			child = child->next;
		t = lex();
	}

	if(t != '{')
		parse_error("'{'");

	if(child)
		child->next = block();
	else
		root->down = child = block();

	t = lex();
	if(t != '}')
		parse_error("'}'");

	return root;
}

AST_node* assignment(void) {
	register int t;
	AST_node *root, *child;

	child = expression();

	t = lex();
	if(t != '=' && !(t >= T_ASSIGNPLUS && t <= T_ASSIGNRSHIFT)) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_ASSIGNMENT, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	root->down->val.op = (t == '=') ? OP_ASSIGN : (t - T_ASSIGNPLUS) + OP_ASSIGNPLUS;
	root->down->down = child;
	root->down->next = expression();

	return root;
}

AST_node* expression(void) {
	AST_node *root, *child;

	child = logical();

	if(!child)
		return NULL;

	root = ast_alloc_node(&ast, N_EXPRESSION, &lexer.debug_info);
	root->down = child;

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_LOGICAL, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	root->down->val.op = (t == T_AND) ? OP_LAND : OP_LOR;
	root->down->down = child;
	root->down->next = logical();

	if(!root->down->next)
		parse_error("right operand");

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_COMPARE, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '<':
		root->down->val.op = OP_LT;
		break;
	case '>':
		root->down->val.op = OP_GT;
		break;
	case T_EQ: case T_NEQ: case T_LTEQ: case T_GTEQ:
		root->down->val.op = (t - T_EQ) + OP_EQ;
		break;
	}
	root->down->down = child;
	root->down->next = compare();

	if(!root->down->next)
		parse_error("right operand");

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_SHIFT, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	root->down->val.op = (t == T_LSHIFT) ? OP_LSHIFT : OP_RSHIFT;
	root->down->down = child;
	root->down->next = shift();

	if(!root->down->next)
		parse_error("right operand");

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_BITWISE, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '&':
		root->down->val.op = OP_AND;
		break;
	case '|':
		root->down->val.op = OP_OR;
		break;
	case '^':
		root->down->val.op = OP_XOR;
		break;
	}
	root->down->down = child;
	root->down->next = bitwise();

	if(!root->down->next)
		parse_error("right operand");

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_ARITH, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	root->down->val.op = (t == '+') ? OP_ADD : OP_SUB;
	root->down->down = child;
	root->down->next = arith();

	if(!root->down->next)
		parse_error("right operand");

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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc_node(&ast, N_FACTOR, &lexer.debug_info);
	root->down = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '*':
		root->down->val.op = OP_MUL;
		break;
	case '/':
		root->down->val.op = OP_DIV;
		break;
	case '%':
		root->down->val.op = OP_MOD;
		break;
	}
	root->down->down = child;
	root->down->next = factor();

	if(!root->down->next)
		parse_error("right operand");

	return root;
}

AST_node* unary(void) {
	register int t;
	AST_node *root, *child;

	root = child = NULL;
	t = lex();

	switch(t) {
		case '*':
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_DEREF;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case '&':
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_ADDR;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case T_NOT:
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_LNOT;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case '-':
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_NEG;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case '~':
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_NOT;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case T_INC:
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_INC;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case T_DEC:
			child = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->val.op = OP_DEC;
			child->next = unary();
			if(!child)
				parse_error("term");
			break;
		case T_CAST:
			t = lex();

			child = type();
			if(!child)
				parse_error("type");
			t = lex();
			if(t != ')')
				parse_error("')'");
			child->next = unary();
			break;
		default:
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			break;
	}

	if(!child)
		return member();

	root = ast_alloc_node(&ast, N_UNARY, &lexer.debug_info);
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
			child->next = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->next->val.op = OP_INDEX;
			child = child->next;
			child->down = expression();
			if(!child->down)
				parse_error("expression");
			t = lex();
			if(t != ']')
				parse_error("']'");
		} else if(t == '.') {
			child->next = ast_alloc_node(&ast, N_OP, &lexer.debug_info);
			child->next->val.op = OP_DOT;
			child = child->next;
			t = lex();
			if(t != T_ID)
				parse_error("identifier");
			child->next = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
			child->next->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
			child = child->next;
		}

		t = lex();
	}

	lexer.debug_info.col -= lexer.ptr - lexer.unget;
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
		node = ast_alloc_node(&ast, N_ID, &lexer.debug_info);
		node->val.str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		return node;
	}

	if(t == '(') {
		node = expression();
		t = lex();
		if(t != ')')
			parse_error("')'");
		return node;
	}

	lexer.debug_info.col -= lexer.ptr - lexer.unget;
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
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	s = lexer.text_s;
	e = lexer.text_e;
	unget = lexer.unget;

	if(lex() != '(') {
		lexer.debug_info.col -= lexer.ptr - unget;
		lexer.ptr = unget;
		return NULL;
	}

	root = ast_alloc_node(&ast, N_CALL, &lexer.debug_info);
	root->val.str = pool_alloc_string(&string_pool, e - s, s);

	root->down = child = expression();
	while((t = lex()) == ',') {
		child->next = expression();
		child = child->next;
	}

	if(t != ')')
		parse_error("')'");

	return root;
}

void compile(void) {
	//AST_node *root = ast.root;

	/* build global tab */
}

void cleanup(void) {
	pool_free(&ast.pool);
	pool_free(&string_pool);
	pool_free(&type_pool);
	pool_free(&member_pool);
	free(pendingtab.data);
	free(globaltab.data);
	free(functab.data);
	fmapclose(&fm);
}

int main(int argc, char **argv) {
	if(argc == 1) {
		printf("sizeof BCinst = %zu\n",sizeof(BCinst));
		return 1;
	}
	if(fmapopen(argv[1], O_RDONLY, &fm) < 0)
#if defined(__MACH__)
		err(1, "%s: no such file or directory", argv[1]);
#else
		error(1, 0, "%s: no such file or directory", argv[1]);
#endif

	atexit(cleanup);

	SYM_TAB_INIT(pendingtab,SCOPE_GLOBAL);
	SYM_TAB_INIT(globaltab,SCOPE_GLOBAL);
	SYM_TAB_INIT(functab,SCOPE_LOCAL);
	fmapread(&fm);
	lexer_init(&lexer, fm.buf);
	pool_init(&ast.pool, sizeof(AST_node), 256, 4);
	pool_init(&string_pool, sizeof(char), 512, 4);
	pool_init(&type_pool, sizeof(Type_info), 128, 1);
	pool_init(&member_pool, sizeof(Type_member), 128, 1);

	parse();
	ast_print(ast.root, 0);

	globaltab.name = argv[1];
	sym_tab_build(&globaltab, ast.root);
	sym_tab_print(&globaltab);

	for(AST_node *node = ast.root; node; node = node->next) {
		AST_node *child;
		if(node->kind != N_DEC && node->kind != N_CONSTDEC)
			continue;
		child = node->down;
		assert(child->kind == N_ID || child->kind == N_IDLIST);
		if(child->kind == N_ID)
			functab.name = child->val.str;
		else
			functab.name = NULL;
		while(child && child->kind != N_INITIALIZER)
			child = child->next;
		if(!child)
			continue;
		child = child->down;
		if(child->kind != N_FUNCTION)
			continue;
		child = child->down;
		while(child->kind != N_BLOCK)
			child = child->next;
		child = child->down;
		sym_tab_build(&functab, child);
		sym_tab_print(&functab);
		sym_tab_clear(&functab);
	}

	return 0;
}
