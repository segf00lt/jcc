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
	tab.sym_count = 0;\
	tab.scope = s;\
	tab.seg_count.data = 0;\
	tab.seg_count.bss = 0;\
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
	T_SIZEOF,
	T_FUNC,
	T_STRUCT,
	T_UNION,
	T_INLINE,
	T_DEFER,
	T_CAST,

	T_INT, T_BOOL, T_FLOAT, T_STRING, T_TYPE, T_VOID,
	T_CHAR,
	T_U8, T_U16, T_U32, T_U64,
	T_S8, T_S16, T_S32, T_S64,
	T_F32, T_F64,

	T_IF, T_ELIF, T_ELSE,
	T_WHILE, T_RETURN,
	T_FOR, T_GOTO, T_CONTINUE, T_BREAK,
	T_SWITCH, T_CASE, T_DEFAULT,
	T_USING,
	T_AND, T_OR, T_NOT,

	T_TRUE, T_FALSE,
	T_INTEGER,
	T_FLOATING,
	T_STR,
	T_CHARACTER,
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
	"T_SIZEOF",
	"T_FUNC",
	"T_STRUCT",
	"T_UNION",
	"T_INLINE",
	"T_DEFER",
	"T_CAST",
	"T_INT", "T_BOOL", "T_FLOAT", "T_STRING", "T_TYPE", "T_VOID",
	"T_CHAR",
	"T_U8", "T_U16", "T_U32", "T_U64",
	"T_S8", "T_S16", "T_S32", "T_S64",
	"T_F32", "T_F64",
	"T_IF", "T_ELIF", "T_ELSE",
	"T_WHILE", "T_RETURN",
	"T_FOR", "T_GOTO", "T_CONTINUE", "T_BREAK",
	"T_SWITCH", "T_CASE", "T_DEFAULT",
	"T_USING",
	"T_AND", "T_OR", "T_NOT",
	"T_TRUE", "T_FALSE",
	"T_INTEGER",
	"T_FLOATING",
	"T_STR",
	"T_CHARACTER",
	"T_BAR",
	"T_ID",
	"T_END",
	"T_ILLEGAL",
};

enum Seg_tag {
	SEG_DATA = 0,
	SEG_BSS,
	SEG_TEXT,
	SEG_HEAP,
	SEG_STACK,
	SEG_COMPILER, // internal use only
};

char *segments_debug[] = {
	"SEG_DATA",
	"SEG_BSS",
	"SEG_TEXT",
	"SEG_HEAP",
	"SEG_STACK",
	"SEG_COMPILER",
};

enum AST_kind_tag {
	N_DEC,
	N_CONSTDEC,
	N_IDLIST,
	N_TYPE,
	N_INITIALIZER,
	N_ARGLIST,
	N_RETLIST,
	N_FUNCTION,
	N_STRUCTURE,
	N_UNIONATION,
	N_ENUMERATION,
	N_BLOCK,
	N_LABEL,
	N_STATEMENT,
	N_DEFERSTATEMENT,
	N_IFSTATEMENT,
	N_WHILESTATEMENT,
	N_RETURNSTATEMENT,
	N_FORSTATEMENT,
	N_EXPRESSION,
	N_POINTER,
	N_ARRAY,
	N_OP,
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
	"N_ARGLIST",
	"N_RETLIST",
	"N_FUNCTION",
	"N_STRUCTURE",
	"N_UNIONATION",
	"N_ENUMERATION",
	"N_BLOCK",
	"N_LABEL",
	"N_STATEMENT",
	"N_DEFERSTATEMENT",
	"N_IFSTATEMENT",
	"N_WHILESTATEMENT",
	"N_RETURNSTATEMENT",
	"N_FORSTATEMENT",
	"N_EXPRESSION",
	"N_POINTER",
	"N_ARRAY",
	"N_OP",
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
	"sizeof",
	"func",
	"struct",
	"union",
	"inline",
	"defer",
	"cast",
	"int", "bool", "float", "string", "Type", "void",
	"char",
	"u8", "u16", "u32", "u64",
	"s8", "s16", "s32", "s64",
	"f32", "f64",
	"if", "elif", "else",
	"while", "return",
	"for", "goto", "continue", "break",
	"switch", "case", "default",
	"using",
	"and", "or", "not",
	"true", "false",
};

size_t keyword_length[] = {
	STRLEN("enum"),
	STRLEN("sizeof"),
	STRLEN("func"),
	STRLEN("struct"),
	STRLEN("union"),
	STRLEN("inline"),
	STRLEN("defer"),
	STRLEN("cast"),
	STRLEN("int"), STRLEN("bool"), STRLEN("float"), STRLEN("string"), STRLEN("Type"), STRLEN("void"),
	STRLEN("char"),
	STRLEN("u8"), STRLEN("u16"), STRLEN("u32"), STRLEN("u64"),
	STRLEN("s8"), STRLEN("s16"), STRLEN("s32"), STRLEN("s64"),
	STRLEN("f32"), STRLEN("f64"),
	STRLEN("if"), STRLEN("elif"), STRLEN("else"),
	STRLEN("while"), STRLEN("return"),
	STRLEN("for"), STRLEN("goto"), STRLEN("continue"), STRLEN("break"),
	STRLEN("switch"), STRLEN("case"), STRLEN("default"),
	STRLEN("using"),
	STRLEN("and"), STRLEN("or"), STRLEN("not"),
	STRLEN("true"), STRLEN("false"),
};

enum AST_op_tag {
	OP_ASSIGNPLUS = 0,
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
	OP_SUBSCRIPT,
	OP_CALL,
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
	"OP_SUBSCRIPT",
	"OP_CALL",
};

enum Type_tag {
	TY_INT = 0, TY_BOOL,
	TY_FLOAT,
	TY_STRING, TY_TYPE, TY_VOID,
	TY_ARRAY, TY_POINTER, TY_VOIDPOINTER,

	TY_CHAR, 
	TY_U8, TY_U16, TY_U32, TY_U64,
	TY_S8, TY_S16, TY_S32, TY_S64,
	TY_F32, TY_F64,

	TY_FUNC, TY_STRUCT, TY_ENUM, TY_UNION,
};

char *type_tag_debug[] = {
	"TY_INT", "TY_BOOL", "TY_FLOAT",
	"TY_STRING","TY_TYPE", "TY_VOID", 
	"TY_ARRAY", "TY_POINTER", "TY_VOIDPOINTER",

	"TY_CHAR", 
	"TY_U8", "TY_U16", "TY_U32", "TY_U64",
	"TY_S8", "TY_S16", "TY_S32", "TY_S64",
	"TY_F32", "TY_F64",

	"TY_FUNC", "TY_STRUCT", "TY_ENUM", "TY_UNION",
};

enum Opcode_tag {
	BCOP_SET,
	BCOP_ADD,
	BCOP_SUB,
	BCOP_MUL,
	BCOP_DIV,
	BCOP_MOD,
	BCOP_NOT,
	BCOP_AND,
	BCOP_OR,
	BCOP_XOR,
	BCOP_LSHIFT,
	BCOP_RSHIFT,
	BCOP_JMP,
	BCOP_BLT,
	BCOP_BGE,
	BCOP_BNE,
	BCOP_BEQ,
	BCOP_LOAD,
	BCOP_STORE,
	BCOP_PUSH,
	BCOP_POP,
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
	"BCOP_NOT",
	"BCOP_AND",
	"BCOP_OR",
	"BCOP_XOR",
	"BCOP_LSHIFT",
	"BCOP_RSHIFT",
	"BCOP_JMP",
	"BCOP_BLT",
	"BCOP_BGE",
	"BCOP_BNE",
	"BCOP_BEQ",
	"BCOP_LOAD",
	"BCOP_STORE",
	"BCOP_PUSH",
	"BCOP_POP",
	"BCOP_CALL",
	"BCOP_RET",
	"BCOP_INT",
	"BCOP_HALT",
};

enum Scope_tag {
	SCOPE_GLOBAL,
	SCOPE_LOCAL,
};

char *scope_debug[] = {
	"SCOPE_GLOBAL",
	"SCOPE_LOCAL",
};

/* typedefs */
typedef enum AST_kind_tag AST_kind_tag;
typedef enum AST_op_tag AST_op_tag;
typedef enum Type_tag Type_tag;
typedef enum Seg_tag Seg_tag;
typedef enum Opcode_tag Opcode_tag;
typedef enum Scope_tag Scope_tag;
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
	AST_kind_tag kind;
	bool visited;
	union {
		char *str;
		AST_op_tag op;
		int typesig;
		int boolean;
	};
	AST_node *down; /* down */
	AST_node *next; /* across */
	Debug_info debug_info;
};

struct AST {
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
	Type_member *ret_types;
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
	Debug_info debug_info;
};

struct Type_info {
	Type_tag tag;
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

struct Sym {
	char *name;
	unsigned int constant : 1;
	Seg_tag seg : 3; /* memory segment */
	uint64_t addr : 61; /* address in memory segment */
	Type_info *type;
	union {
		Type_info *tinfo;
		AST_node *ast;
		uint64_t label : 64 - 3;
	} val;
	Debug_info debug_info;
};

struct Sym_tab {
	Sym *data;
	Scope_tag scope;
	char *name;
	size_t cap;
	size_t sym_count;
	size_t text_count;
	union {
		struct {
			size_t data;
			size_t bss;
		};
		size_t stack;
	} seg_count;
};

struct BCinst {
	Opcode_tag opcode : 10;
	uint64_t size : 7;
	uint64_t sign : 1;
	uint64_t floating : 1;
	uint64_t immediate : 1;
	union {
		uint64_t reg; /* data register */
		uint64_t ptr; /* pointer register */
		uint64_t addr; /* immediate address */
	};
	union {
		struct {
			uint64_t r1;
			uint64_t r2;
		};
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
	union {
		struct {
			size_t text_count;
			size_t text_size;
		};
		struct {
			size_t heap_count;
			size_t heap_size;
		};
	};
	BCbyte *seg[6];
	BCbyte *stack_ptr;
};

/* globals */
Fmap fm; /* source file Fmap */
Pool string_pool;
Pool type_pool;
Pool member_pool;
Lexer lexer;
AST ast = {0};
Sym_tab pendingtab; /* unresolved symbols get defined here */
Sym_tab tabstack[10]; /* hard limit on amount of nesting that can be done */
int scope_depth = 0;
BCmem bcmem = {0};
Type_info builtin_types[] = {
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 1 } },
	{ .tag = TY_BOOL, .bytes = 1u },
	{ .tag = TY_FLOAT, .bytes = 4u, .Float = { .bits = 32u } },
	{ .tag = TY_STRING },
	{ .tag = TY_TYPE, .bytes = sizeof(Type_info) },
	{ .tag = TY_VOID },
	{ .tag = TY_ARRAY, .bytes = 8u },
	{ .tag = TY_POINTER, .bytes = 8u },
	{ .tag = TY_VOIDPOINTER, .bytes = 8u },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 2u, .Int = { .bits = 16u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 4u, .Int = { .bits = 32u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 0 } },
	{ .tag = TY_INT, .bytes = 1u, .Int = { .bits = 8u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 2u, .Int = { .bits = 16u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 4u, .Int = { .bits = 32u, .sign = 1 } },
	{ .tag = TY_INT, .bytes = 8u, .Int = { .bits = 64u, .sign = 1 } },
	{ .tag = TY_FLOAT, .bytes = 4u, .Float = { .bits = 32u} },
	{ .tag = TY_FLOAT, .bytes = 8u, .Float = { .bits = 64u} },
};

/* utility functions */
void cleanup(void);
void myerror(char *fmt, ...);
void parse_error(char *expect);

/* AST functions */
AST_node* ast_alloc(AST *ast, int kind, Debug_info *debug_info);
void ast_print(AST_node *node, size_t depth, bool skip_next);

/* lexer functions */
void lexer_init(Lexer *l, char *buf);

/* parsing */
int lex(void);
void parse(void);
AST_node* declaration(void);
AST_node* vardec(void);
AST_node* literal(void);
AST_node* typename(void);
AST_node* function(void);
AST_node* structure(void);
AST_node* unionation(void);
AST_node* enumeration(void);
AST_node* block(void);
AST_node* statement(void);
AST_node* label(void);
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
AST_node* postfix(void);
AST_node* term(void);

/* type system funcitons */
void type_info_print(Type_info *tp, size_t depth);
void resolve_compound_type(Type_info *tinfo);
Type_info* recursive_type_merge(Type_info *ta, Type_info *tb);
Type_info* simple_type_merge(Type_info *ta, Type_info *tb);
Type_info* typecheck(AST_node *node);
Type_info* build_type(Pool *type_pool, Pool *member_pool, AST_node *node);
Type_info* infer_type(Pool *type_pool, Pool *member_pool, AST_node *node);
void build_compound_type(Pool *type_pool, Pool *member_pool, AST_node *node, Type_info *dest);

/* symbol table functions */
void declare_constants(Sym_tab *tab, AST_node *node);
void declare_var(Sym_tab *tab, AST_node *node);
void sym_tab_grow(Sym_tab *tab);
int define_symbol(Sym_tab *tab, Sym *symbol);
int sym_tab_undef(Sym_tab *tab, char *name);
void sym_tab_copy(Sym_tab *dest, Sym_tab *src);
size_t sym_tab_hash(Sym_tab *tab, char *name);
Sym* lookup_symbol(char *name);
Sym* pending_symbol(char *name);
Sym* sym_tab_look(Sym_tab *tab, char *name);
void sym_tab_clear(Sym_tab *tab);
void sym_tab_print(Sym_tab *tab);

/* bytecode interpreter */
BCinst* bc_allocinst(BCmem *mem);
int bc_interpreter(BCinst *prog, BCmem *mem);

/* compilation stage 1 functions */
void compile(AST_node *root);
void compile_function(AST_node *node);
void compile_block(AST_node *node);
void compile_expression(AST_node *node);
void compile_term(AST_node *node);
void compile_call(AST_node *node);

void ast_print(AST_node *node, size_t depth, bool skip_next) {
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
			fprintf(stderr, "type: %s\n", type_tag_debug[node->typesig]);
		else if(node->kind == N_BOOLLIT)
			fprintf(stderr, "boolean: %u\n", node->boolean);
		else if(node->kind != N_OP)
			fprintf(stderr, "str: %s\n", node->str);
		else
			fprintf(stderr, "op: %s\n", operators_debug[node->op]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "down: %u\n", node->down ? node->down->id : 0);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "next: %u\n\n", node->next ? node->next->id : 0);
		if(node->down)
			ast_print(node->down, depth+1, false);
		if(skip_next)
			break;
	}
}

AST_node* ast_alloc(AST *ast, int kind, Debug_info *debug_info) {
	register AST_node *node;
	node = pool_alloc(&(ast->pool));
	*node = (AST_node){
			.id = ast->nodecount++,
			.kind = kind,
			.down = NULL,
			.next = NULL,
			.visited = false,
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
		type_info_print(tp->Array.array_of, depth+1);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "max_index_expr:\n");
		ast_print(tp->Array.max_index_expr, depth+1, true);
		break;
	case TY_POINTER:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "pointer_to: %s\n", type_tag_debug[tp->Pointer.pointer_to->tag]);
		type_info_print(tp->Pointer.pointer_to, depth+1);
		break;
	case TY_VOIDPOINTER:
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "pointer_to: void\n");
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
			ast_print(member->initial_val_expr, depth+1, true);
			for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
			fprintf(stderr, "type:\n");
			type_info_print(member->type, depth+1);
		}
		--depth;
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "return values:\n");
		++depth;
		for(member = tp->Func.ret_types; member; member = member->next) {
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
			ast_print(member->initial_val_expr, depth+1, true);
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
			ast_print(member->initial_val_expr, depth+1, true);
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

#define TYPE_TAG_IS_RECURSIVE(tag) ((tag >= TY_FUNC && tag <= TY_UNION) || tag == TY_ARRAY || tag == TY_POINTER)

#define TYPE_TAG_IS_SIMPLE(tag) (tag >= TY_INT && tag <= TY_VOID)

#define TYPES_CAN_MERGE_RECURSIVE(tag_a, tag_b) (TYPE_TAG_IS_RECURSIVE(tag_a) && TYPE_TAG_IS_RECURSIVE(tag_b))

#define TYPES_CAN_MERGE_SIMPLE(tag_a, tag_b) \
	((tag_a != TY_POINTER || TYPE_TAG_IS_SIMPLE(tag_b)) || (tag_b != TY_POINTER || TYPE_TAG_IS_SIMPLE(tag_a)))

Type_info* recursive_type_merge(Type_info *ta, Type_info *tb) {
	assert(ta && tb);
	assert(TYPES_CAN_MERGE_RECURSIVE(ta->tag, tb->tag));

	Type_info *tinfo = NULL, *subtype_a = NULL, *subtype_b = NULL;
	Type_member *member_a = NULL, *member_b = NULL;

	if(ta->tag != tb->tag)
		return NULL;

	switch(ta->tag) {
	case TY_FUNC:
		member_a = ta->Func.arg_types;
		member_b = tb->Func.arg_types;
		break;
	case TY_STRUCT:
		if(ta->Struct.name && tb->Struct.name) {
			if(strcmp(ta->Struct.name, tb->Struct.name))
				return NULL;
			else
				return ta;
		} else if(ta->Struct.name || tb->Struct.name) {
			return NULL;
		}
		member_a = ta->Struct.members;
		member_b = tb->Struct.members;
		break;
	case TY_UNION:
		if(ta->Union.name && tb->Union.name) {
			if(strcmp(ta->Struct.name, tb->Struct.name))
				return NULL;
			else
				return ta;
		} else if(ta->Union.name || tb->Union.name) {
			return NULL;
		}
		member_a = ta->Union.members;
		member_b = tb->Union.members;
		break;
	case TY_ENUM:
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
		break;
	case TY_ARRAY:
		subtype_a = ta->Array.array_of;
		subtype_b = tb->Array.array_of;
		break;
	case TY_POINTER:
		subtype_a = ta->Pointer.pointer_to;
		subtype_b = tb->Pointer.pointer_to;
		break;
	default:
		assert(0);
	}

	if(subtype_a && subtype_b) {
		if(TYPES_CAN_MERGE_RECURSIVE(subtype_a->tag, subtype_b->tag))
			return recursive_type_merge(subtype_a, subtype_b);
		else if(TYPES_CAN_MERGE_SIMPLE(subtype_a->tag, subtype_b->tag))
			return simple_type_merge(subtype_a, subtype_b);
		else
			return NULL;
	}

	while(member_a && member_b) {
		if(TYPES_CAN_MERGE_RECURSIVE(member_a->type->tag, member_b->type->tag))
			tinfo = recursive_type_merge(member_a->type, member_b->type);
		else if(TYPES_CAN_MERGE_SIMPLE(member_a->type->tag, member_b->type->tag))
			tinfo = simple_type_merge(member_a->type, member_b->type);
		else
			tinfo = NULL;

		if(!tinfo)
			return NULL;

		member_a = member_a->next;
		member_b = member_b->next;

		if(member_a && member_b) {
			continue;
		} else if(ta->tag == TY_FUNC && !member_a && !member_b) {
			member_a = ta->Func.ret_types;
			member_b = tb->Func.ret_types;
		} else {
			return NULL;
		}
	}

	return ta;
}

Type_info* simple_type_merge(Type_info *ta, Type_info *tb) {
	assert(ta && tb);
	assert(TYPES_CAN_MERGE_SIMPLE(ta->tag, tb->tag));

	if(ta->tag > tb->tag) {
		Type_info *tmp = ta;
		ta = tb;
		tb = tmp;
	}

	if(ta->tag == tb->tag)
		return ta;

	if(ta->tag != TY_INT)
		return NULL;

	if(tb->tag == TY_FLOAT || tb->tag == TY_POINTER)
		return tb;

	return NULL;
}

Type_info* typecheck(AST_node *node) {
	AST_node *save = NULL;
	Type_info *tinfo = NULL, *tinfo_down = NULL, *tinfo_next = NULL;
	Type_info *merged_type = NULL;
	Type_member *memberptr = NULL;
	Sym *symptr = NULL;
	char *tstr_down = NULL, *tstr_next = NULL, *opstr = NULL;
	bool invalid = false;

	if(node->kind == N_EXPRESSION)
		node = node->down;

	if(node->kind != N_OP) {
		switch(node->kind) {
		case N_UNINIT:
			tinfo = builtin_types + TY_VOID;
			break;
		case N_STRUCTURE: case N_UNIONATION: case N_ENUMERATION:
			tinfo = builtin_types + TY_TYPE;
			break;
		case N_TYPE:
			if(node->down->kind == N_ID) {
				symptr = lookup_symbol(node->str);
				if(!symptr)
					myerror("undefined identifier %s at %DBG", node->str, &node->debug_info);
				if(symptr->type->tag != TY_TYPE)
					myerror("invalid type %s at %DBG", node->str, &node->debug_info);
			}
			tinfo = builtin_types + TY_TYPE;
			break;
		case N_INTLIT: // TODO differentiate between int literals
			tinfo = builtin_types + TY_INT;
			break;
		case N_FLOATLIT:
			tinfo = builtin_types + TY_FLOAT;
			break;
			break;
		case N_CHARLIT:
			tinfo = builtin_types + TY_CHAR;
			break;
		case N_BOOLLIT:
			tinfo = builtin_types + TY_BOOL;
			break;
		case N_ID:
			symptr = lookup_symbol(node->str);
			if(!symptr)
				myerror("undefined identifier %s at %DBG", node->str, &node->debug_info);
			tinfo = symptr->type;
			break;
		case N_STRLIT:
		case N_ARRAYLIT:
		case N_STRUCTLIT:
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
			break;
		default:
			assert(0);
		}

		return tinfo;
	}

	if(node->op >= OP_LAND && node->op <= OP_LNOT) {
		typecheck(node->down);
		if(node->next)
			typecheck(node->next);
		return builtin_types + TY_BOOL;
	}

	if(node->op == OP_CALL) {
		tinfo = typecheck(node->down);
		if(tinfo->tag != TY_FUNC)
			myerror("call expression does not evaluate to function at%DBG", &node->debug_info);
		memberptr = tinfo->Func.arg_types;
		if(!tinfo->Func.ret_types)
			tinfo = builtin_types + TY_VOID;
		else
			tinfo = tinfo->Func.ret_types->type;
		save = node;
		node = node->next;
		for(; node && memberptr; node = node->next, memberptr = memberptr->next) {
			tinfo_down = typecheck(node->down);
			tinfo_next = memberptr->type;

			if(TYPES_CAN_MERGE_RECURSIVE(tinfo_down->tag, tinfo_next->tag))
				merged_type = recursive_type_merge(tinfo_down, tinfo_next);

			tstr_down = type_tag_debug[tinfo_down->tag];
			tstr_next = type_tag_debug[tinfo_next->tag];

			if(!merged_type && tinfo_down->tag != tinfo_next->tag)
				myerror("invalid assignment of %s to %s at%DBG", tstr_next, tstr_down, &node->debug_info);
		}

		if(node || memberptr) {
			myerror("wrong number of arguments in call at%DBG", &save->debug_info);
		}

		return tinfo;
	}

	if(node->op == OP_DOT) {
		bool is_pointer_to_compound;
		Type_member *member_stack[12];
		int member_stack_top = 0;
		Type_info *member_type = NULL;
		char *compound_type_str = NULL;
		char *memberstr = NULL;
		tinfo = typecheck(node->down);

		assert(node->next->kind == N_ID);
		memberstr = node->next->str;

		is_pointer_to_compound =
			(tinfo->tag == TY_POINTER &&
			 tinfo->Pointer.pointer_to->tag >= TY_STRUCT &&
			 tinfo->Pointer.pointer_to->tag <= TY_UNION);
		if(is_pointer_to_compound)
			tinfo = tinfo->Pointer.pointer_to;

		switch(tinfo->tag) {
		case TY_ENUM:
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
			compound_type_str = tinfo->Enum.name;
			memberptr = tinfo->Enum.members;
			break;
		case TY_STRUCT:
			compound_type_str = tinfo->Struct.name;
			memberptr = tinfo->Struct.members;
			break;
		case TY_UNION:
			compound_type_str = tinfo->Union.name;
			memberptr = tinfo->Union.members;
			break;
		case TY_ARRAY:
			myerror("use of '.' on array at%DBG", &node->debug_info);
			break;
		default:
			myerror("use of '.' on non compound type at%DBG", &node->debug_info);
			break;
		}

		/* look through members, push member list on stack whenever
		 * we encounter an anonymous struct or union */
		member_stack[member_stack_top] = memberptr;
		while(member_stack_top >= 0) {
			assert(member_stack_top < ARRLEN(member_stack));
			memberptr = member_stack[member_stack_top];

			for(; memberptr; memberptr = memberptr->next) {
				if(!memberptr->name) {
					assert(memberptr->type->tag == TY_STRUCT || memberptr->type->tag == TY_UNION);
					member_stack[member_stack_top] = memberptr->next;
					++member_stack_top;
					if(memberptr->type->tag == TY_STRUCT)
						memberptr = memberptr->type->Struct.members;
					else if(memberptr->type->tag == TY_UNION)
						memberptr = memberptr->type->Union.members;
					member_stack[member_stack_top] = memberptr;
				}

				if(!strcmp(memberstr, memberptr->name)) {
					member_type = memberptr->type;
					member_stack_top = -1;
					break;
				}
			}

			--member_stack_top;
		}
		member_stack_top = 0;

		if(!member_type)
			myerror("%s has no member %s%DBG", compound_type_str, memberstr, &node->debug_info);

		tinfo = member_type;

		return tinfo;
	}

	if(node->op == OP_ASSIGN) {
		assert(node->down && node->next);
		memberptr = NULL;

		tinfo_next = typecheck(node->next);
		if(node->next->kind == N_OP && node->next->op == OP_CALL)
			memberptr = tinfo_next->Func.ret_types;

		if(node->down->kind == N_IDLIST) {
			save = node->down;
			node = node->down->down;
			for(; node; node = node->next) {
				if(node->str == NULL)
					invalid = false;
				else
					tinfo_down = typecheck(node);

				if(TYPES_CAN_MERGE_RECURSIVE(tinfo_down->tag, tinfo_next->tag))
					merged_type = recursive_type_merge(tinfo_down, tinfo_next);

				tstr_down = type_tag_debug[tinfo_down->tag];
				tstr_next = type_tag_debug[tinfo_next->tag];

				if(!merged_type && tinfo_down->tag != tinfo_next->tag)
					myerror("invalid assignment of %s to %s at%DBG", tstr_next, tstr_down, &node->debug_info);

				if(memberptr) {
					memberptr = memberptr->next;
					if(!memberptr) {
						node = node->next;
						break;
					}
					tinfo_next = memberptr->type;
				}
			}

			if(node || memberptr)
				myerror("mismatched number of operands in assignment at%DBG", &save->debug_info);
		} else {
			tinfo_down = typecheck(node->down);
			tstr_down = type_tag_debug[tinfo_down->tag];
			tstr_next = type_tag_debug[tinfo_next->tag];

			if(TYPES_CAN_MERGE_RECURSIVE(tinfo_down->tag, tinfo_next->tag))
				merged_type = recursive_type_merge(tinfo_down, tinfo_next);

			if(!merged_type && tinfo_down->tag != tinfo_next->tag)
				myerror("invalid assignment of %s to %s at%DBG", tstr_next, tstr_down, &node->debug_info);
		}

		return tinfo_down;
	}

	if(node->down)
		tinfo_down = typecheck(node->down);

	opstr = operators_debug[node->op];
	if(tinfo_down)
		tstr_down = type_tag_debug[tinfo_down->tag];

	assert(tinfo_down);

	if(node->op == OP_CAST) {
		Type_info *tinfo_to, *tinfo_from;
		char *tstr_to, *tstr_from;

		unsigned char cast_table[9][5] = {
			{TY_INT, TY_FLOAT, TY_VOIDPOINTER, TY_BOOL,-1},
			{TY_BOOL, TY_INT,-1,-1,-1},
			{TY_FLOAT, TY_INT, TY_BOOL,-1,-1},
			{TY_STRING, TY_ARRAY, TY_POINTER, TY_VOIDPOINTER, TY_BOOL},
			{-1},{-1}, // TYPE and VOID don't cast
			{TY_ARRAY, TY_STRING, TY_POINTER, TY_VOIDPOINTER, TY_BOOL},
			{TY_POINTER, TY_INT, TY_VOIDPOINTER, TY_BOOL,-1},
			{TY_VOIDPOINTER, TY_INT, TY_POINTER, TY_BOOL,-1},
		};

		assert(node->next->kind == N_TYPE);

		AST_node *cast_type_node = node->next->down;

		if(cast_type_node->kind == N_ID) {
			symptr = lookup_symbol(node->str);
			if(!symptr)
				myerror("undefined identifier %s at %DBG", node->str, &node->debug_info);
			if(symptr->type->tag != TY_TYPE)
				myerror("invalid type %s at %DBG", node->str, &node->debug_info);
			tinfo_next = symptr->val.tinfo;
		} else if(cast_type_node->kind == N_FUNCTION) {
			myerror("attempted to cast %s to function type at%DBG", tstr_down, &node->debug_info);
		} else if(cast_type_node->kind >= N_STRUCTURE && cast_type_node->kind <= N_ENUMERATION) {
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
		} else {
			tinfo_next = build_type(&type_pool, &member_pool, cast_type_node);
		}

		tstr_next = type_tag_debug[tinfo_next->tag];

		if(tinfo_down->tag == TY_FUNC || tinfo_down->tag == TY_VOID || tinfo_down->tag == TY_TYPE)
			myerror("attempted to cast %s to %s at%DBG", tstr_down, tstr_next, &node->debug_info);

		if(tinfo_down->tag >= TY_STRUCT)
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);

		assert(tinfo_down->tag >= TY_INT && tinfo_down->tag <= TY_VOIDPOINTER);

		tinfo_to = tinfo_next;
		tinfo_from = tinfo_down;
		tstr_to = tstr_next;
		tstr_from = tstr_down;

		for(int i = 0; i < ARRLEN(cast_table[tinfo_from->tag]); ++i) {
			if(tinfo_to->tag == cast_table[tinfo_to->tag][i]) {
				tinfo = tinfo_to;
				break;
			}
		}

		if(!tinfo)
			myerror("attempted to cast %s to %s at%DBG", tstr_down, tstr_next, &node->debug_info);

		bool from_is_array = (tinfo_from->tag == TY_ARRAY);
		bool to_is_array = (tinfo->tag == TY_ARRAY);
		bool to_is_array_of_char = (to_is_array && tinfo->Array.array_of == builtin_types+TY_CHAR);
		bool to_is_pointer_to_char = (tinfo->tag == TY_POINTER && tinfo->Pointer.pointer_to == builtin_types+TY_CHAR);
		bool to_is_not_array_or_pointer = (!to_is_array && tinfo->tag != TY_POINTER);
		bool to_is_string = (tinfo->tag == TY_STRING);
		bool from_is_array_of_char = (from_is_array && tinfo_from->Array.array_of == builtin_types+TY_CHAR);

		switch(tinfo_from->tag) {
		case TY_STRING:
			if(to_is_array_of_char || to_is_pointer_to_char || to_is_not_array_or_pointer)
				return tinfo;
			break;
		case TY_ARRAY:
			if((to_is_string || to_is_pointer_to_char) && from_is_array_of_char)
				return tinfo;
			if(from_is_array && to_is_array)
				return tinfo;
			break;
		default:
			return tinfo;
			break;
		}
		myerror("attempted to cast %s to %s at%DBG", tstr_from, tstr_to, &node->debug_info);
		assert(0);
	}

	if(node->next)
		tinfo_next = typecheck(node->next);

	bool down_is_int = (tinfo_down->tag == TY_INT);
	bool next_is_int = (tinfo_next && tinfo_next->tag == TY_INT);
	bool down_is_float = (tinfo_down->tag == TY_FLOAT);
	bool next_is_float = (tinfo_next && tinfo_next->tag == TY_FLOAT);
	bool down_is_pointer = (tinfo_down->tag == TY_POINTER);
	bool down_is_array = (tinfo_down->tag == TY_ARRAY);
	bool down_is_string = (tinfo_down->tag == TY_STRING);
	bool next_is_string = (tinfo_next && tinfo_next->tag == TY_STRING);

	if(tinfo_next) {
		tstr_next = type_tag_debug[tinfo_next->tag];
	} else { // unary ops
		switch(node->op) {
		case OP_INC: case OP_DEC:
			invalid = !(down_is_int || down_is_pointer || down_is_float);
			tinfo = tinfo_down;
			break;
		case OP_NOT:
			invalid = !(down_is_int || down_is_pointer);
			tinfo = tinfo_down;
			break;
		case OP_NEG:
			invalid = !(down_is_int || down_is_pointer || down_is_float);
			tinfo = tinfo_down;
			break;
		case OP_ADDR:
			tinfo = builtin_types + TY_POINTER;
			tinfo->Pointer.pointer_to = tinfo_down;
			break;
		case OP_DEREF:
			invalid = (tinfo_down->tag != TY_POINTER && tinfo_down->tag != TY_ARRAY);
			tinfo = tinfo_down->Pointer.pointer_to;
			break;
		default:
			assert(0);
		}

		if(invalid)
			myerror("invalid operand %s to %s at%DBG", tstr_down, opstr, &node->debug_info);

		return tinfo;
	}

	if(node->op == OP_SUBSCRIPT) {
		if(!( (down_is_pointer || down_is_array || down_is_string) && next_is_int ))
			myerror("invalid operands %s and %s to %s at%DBG", tstr_down, tstr_next, opstr, &node->debug_info);
		return tinfo_down->Array.array_of;
	}

	bool types_are_recursive = false;

	if(TYPES_CAN_MERGE_RECURSIVE(tinfo_down->tag, tinfo_next->tag)) {
		merged_type = recursive_type_merge(tinfo_down, tinfo_next);
		if(!merged_type)
			myerror("invalid operands %s and %s to %s at%DBG", tstr_down, tstr_next, opstr, &node->debug_info);
		types_are_recursive = true;
		tinfo = merged_type;
	} else if(TYPES_CAN_MERGE_SIMPLE(tinfo_down->tag, tinfo_next->tag)) {
		merged_type = simple_type_merge(tinfo_down, tinfo_next);
		if(!merged_type)
			myerror("invalid operands %s and %s to %s at%DBG", tstr_down, tstr_next, opstr, &node->debug_info);
		tinfo = merged_type;
	}
	
	if(node->op >= OP_ASSIGNPLUS && node->op <= OP_ASSIGNDIV) {
		invalid =
			!(down_is_float && (next_is_float || next_is_int)) &&
			!(down_is_int && next_is_int) &&
			!(down_is_pointer && next_is_int);
		tinfo = tinfo_down;
	}

	if((node->op >= OP_ASSIGNMOD && node->op <= OP_ASSIGNRSHIFT) || node->op == OP_LSHIFT || node->op == OP_RSHIFT) {
		invalid = !((down_is_int || down_is_pointer) && next_is_int);
		tinfo = tinfo_down;
	}

	if(node->op == OP_EQ || node->op == OP_NEQ || (node->op >= OP_LTEQ && node->op <= OP_GT)) {
		invalid =
			!(down_is_string && next_is_string) &&
			!(down_is_float && next_is_float) &&
			!(down_is_int && next_is_int) &&
			!(types_are_recursive && merged_type);
		tinfo = builtin_types + TY_BOOL;
	}

	if(node->op >= OP_MOD && node->op <= OP_XOR)
		invalid = (merged_type->tag != TY_INT && merged_type->tag != TY_POINTER);

	if(node->op >= OP_ADD && node->op <= OP_DIV)
		invalid = (merged_type->tag != TY_INT && merged_type->tag != TY_POINTER && merged_type->tag != TY_FLOAT);

	if(invalid)
		myerror("invalid operands %s and %s to %s at%DBG", tstr_down, tstr_next, opstr, &node->debug_info);

	return tinfo;
}

Type_info* infer_type(Pool *type_pool, Pool *member_pool, AST_node *node) {
	AST_node *child, *sibling, *member_initial_val_expr;
	AST_node *arguments;
	Type_info *tinfo, *member_type;
	Type_member *member;
	Sym *symptr;

	assert(node->kind == N_INITIALIZER);
	node = node->down;

	switch(node->kind) {
	case N_TYPE: case N_STRUCTURE: case N_UNIONATION: case N_ENUMERATION:
		tinfo = builtin_types + TY_TYPE;
		break;
	case N_UNINIT:
		myerror("cant infer type from uninitialized value%DBG", &node->debug_info);
		break;
	case N_STRUCTLIT:
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
		break;
	case N_STRLIT:
		tinfo = builtin_types + TY_STRING;
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
		break;
	case N_OP:
		tinfo = typecheck(node);
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
		tinfo = builtin_types + node->typesig;
		break;
	case N_FUNCTION:
		node = node->down;
		tinfo = pool_alloc(type_pool);
		tinfo->tag = TY_FUNC;
		tinfo->bytes = 8u;
		if(node->kind == N_ARGLIST && node->down->kind != N_TYPE && node->down->down->typesig != TY_VOID) {
			Type_member head;
			arguments = node->down;
			member = &head;
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
				} else if(sibling->next && sibling->next->kind == N_INITIALIZER)
					member_initial_val_expr = sibling->next;

				if(sibling->kind == N_INITIALIZER)
					member_type = infer_type(type_pool, member_pool, sibling);
				else if(sibling->kind == N_TYPE)
					member_type = build_type(type_pool, member_pool, sibling);
				else assert(0);

				while(child->kind == N_ID) {
					member->next = pool_alloc(member_pool);
					member = member->next;
					member->debug_info = child->debug_info;
					member->name = child->str;
					member->type = member_type;
					member->initial_val_expr = member_initial_val_expr;
					child = child->next;
					if(!child)
						break;
					if(child->kind != N_ID)
						break;
				}
				arguments = arguments->next;
			}

			member = head.next;
			tinfo->Func.arg_types = member;
			node = node->next;
		}

		if(node->kind == N_BLOCK || node->kind == N_STORAGEQUALIFIER)
			break;
		else /* node is return type */
			node = node->down;

		if(node->down->kind == N_BUILTINTYPE && node->down->typesig == TY_VOID)
			break;

		tinfo->Func.ret_types = member = pool_alloc(member_pool);
		while(true) {
			member->type = build_type(type_pool, member_pool, node);
			node = node->next;
			if(!(node && node->kind != N_BLOCK && node->kind != N_STORAGEQUALIFIER))
				break;
			member->next = pool_alloc(member_pool);
			member = member->next;
		}
		break;
	case N_ID:
		symptr = lookup_symbol(node->str);
		if(!symptr)
			myerror("undefined identifier %s at%DBG", node->str, &node->debug_info);
		tinfo = symptr->type;
		break;
	default:
		assert(0);
		break;
	}

	return tinfo;
}

Type_info* build_type(Pool *type_pool, Pool *member_pool, AST_node *node) {
	AST_node *child;
	AST_node *arguments;
	Type_info *tinfo;
	Type_member *member;
	Sym *symptr;
	bool is_pending;
	Sym symbol = {0};

	if(node->kind == N_TYPE)
		node = node->down;
	assert((node->kind >= N_FUNCTION && node->kind <= N_ENUMERATION) ||
			node->kind == N_ID ||
			node->kind == N_POINTER ||
			node->kind == N_ARRAY ||
			node->kind == N_BUILTINTYPE);

	tinfo = NULL;
	symptr = NULL;

	switch(node->kind) {
	case N_STRUCTURE: case N_UNIONATION: case N_ENUMERATION:
		tinfo = pool_alloc(type_pool);
		build_compound_type(type_pool, member_pool, node, tinfo);
		break;
	case N_BUILTINTYPE:
		tinfo = builtin_types + node->typesig;
		break;
	case N_POINTER:
		if(node->down->kind == N_BUILTINTYPE && node->down->typesig == TY_VOID) {
			tinfo = builtin_types + TY_VOIDPOINTER;
			break;
		}

		tinfo = pool_alloc(type_pool);
		tinfo->tag = TY_POINTER;
		tinfo->bytes = 8u;
		tinfo->Pointer.pointer_to = build_type(type_pool, member_pool, node->down); 
		break;
	case N_ARRAY:
		tinfo = pool_alloc(type_pool);
		tinfo->tag = TY_ARRAY;
		tinfo->bytes = 8u << 2;
		if(node->next) {
			Type_info *expr_type = typecheck(node->next);
			if(expr_type->tag != TY_INT)
				myerror("array size must evaluate to 'int' at%DBG", &node->debug_info);
			tinfo->Array.max_index_expr = node->next; /* resolve later */
		}
		tinfo->Array.array_of = build_type(type_pool, member_pool, node->down); 
		break;
	case N_FUNCTION:
		node = node->down;

		tinfo = pool_alloc(type_pool);
		tinfo->tag = TY_FUNC;
		tinfo->bytes = 8u;

		if(node->kind == N_ARGLIST &&
				!(node->down->kind == N_TYPE && node->down->down->typesig == TY_VOID)) {
			arguments = node->down;
			child = arguments;
			tinfo->Func.arg_types = member = pool_alloc(member_pool);
			while(true) {
				member->type = build_type(type_pool, member_pool, child);
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

		if(node->down->kind == N_BUILTINTYPE && node->down->typesig == TY_VOID)
			break;

		tinfo->Func.ret_types = member = pool_alloc(member_pool);
		while(true) {
			member->type = build_type(type_pool, member_pool, node);
			node = node->next;
			if(!(node && node->kind != N_BLOCK && node->kind != N_STORAGEQUALIFIER))
				break;
			member->next = pool_alloc(member_pool);
			member = member->next;
		}
		break;
	case N_ID:
		is_pending = false;
		symptr = pending_symbol(node->str);
		if(symptr)
			is_pending = true;
		else
			symptr = lookup_symbol(node->str);

		if(symptr) {
			if(!symptr->constant && !is_pending)
				myerror("use of non constant symbol '%s' as type%DBG", symptr->name, &node->debug_info);
			tinfo = symptr->val.tinfo;
		} else {
			symbol.name = node->str;
			tinfo = pool_alloc(type_pool);
			tinfo->tag = TY_STRUCT;
			tinfo->Struct.name = node->str;
			tinfo->bytes = 0;
			symbol.debug_info = node->debug_info;
			symbol.val.tinfo = tinfo;
			define_symbol(&pendingtab, &symbol);
		}
		break;
	default:
		assert(0);
		break;
	}

	return tinfo;
}

void build_compound_type(Pool *type_pool, Pool *member_pool, AST_node *node, Type_info *dest) {
	AST_node *child, *sibling;
	Type_member *member;

	assert(node->kind==N_STRUCTURE||node->kind == N_ENUMERATION||node->kind==N_UNIONATION);

	if(node->kind == N_STRUCTURE) {
		dest->tag = TY_STRUCT;
		dest->Struct.members = member = pool_alloc(member_pool);
	} else if(node->kind == N_UNIONATION) {
		dest->tag = TY_UNION;
		dest->Union.members = member = pool_alloc(member_pool);
	} else if(node->kind == N_ENUMERATION) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	} else {
		assert(0);
	}

	node = node->down;

	while(node) {
		if(node->kind == N_DEC || node->kind == N_CONSTDEC) {
			child = node->down;
			sibling = child->next;
			if(sibling->kind != N_TYPE)
				myerror("struct or union member missing explicit type%DBG", &sibling->debug_info);
			if(child->kind == N_IDLIST)
				child = child->down;
			member->type = build_type(type_pool, member_pool, sibling);
			sibling = sibling->next;
			member->initial_val_expr = NULL;
			if(sibling && sibling->kind == N_INITIALIZER)
				member->initial_val_expr = sibling;
			member->constant = (node->kind == N_CONSTDEC);
			while(true) {
				member->name = child->str;
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
			member->type = pool_alloc(type_pool);
			build_compound_type(type_pool, member_pool, node, member->type);
		} else if(node->kind == N_ENUMERATION) {
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
		} else {
			assert(0);
		}

		node = node->next;
		if(node) {
			member->next = pool_alloc(member_pool);
			member = member->next;
		}
	}

}

void resolve_compound_type(Type_info *tinfo) {
	Type_member *member;
	size_t offset;
	size_t largest;
	char *type_tag_str, *name , *member_type_name;
	Sym *symptr;

	if(tinfo->bytes != 0 || tinfo->tag == TY_VOID)
		return;

	if(tinfo->tag == TY_STRUCT) {
		member = tinfo->Struct.members;
		name = tinfo->Struct.name;
		type_tag_str = "struct";
	} else if(tinfo->tag == TY_UNION) {
	 	member = tinfo->Union.members;
		name = tinfo->Union.name;
		type_tag_str = "union";
	} else {
		assert(0);
	}

	offset = 0;
	largest = 0;

	while(member) {
		if(member->type == tinfo)
			myerror("%s %s was recursively defined at%DBG",
				type_tag_str, name, &(tinfo->Struct.debug_info));

		if(tinfo->tag == TY_STRUCT)
			member->byte_offset = offset;

		if(member->type->bytes == 0) {
			if(member->type->tag == TY_STRUCT) {
				member_type_name = member->type->Struct.name;
			} else if(member->type->tag == TY_UNION) {
				member_type_name = member->type->Union.name;
			} else if(member->type->tag == TY_ENUM) {
				member_type_name = member->type->Enum.name;
			} else {
				assert(member->type->tag == TY_VOID);
				member = member->next;
				continue;
			}
			if(member_type_name) {
				symptr = pending_symbol(member_type_name);
				if(symptr)
					myerror("type %s was not defined, first referenced at%DBG", member_type_name, &symptr->debug_info);
			}
			resolve_compound_type(member->type);
		}
		offset += member->type->bytes;
		if(member->type->bytes > largest)
			largest = member->type->bytes;
		member = member->next;
	}

	tinfo->bytes = offset;
	if(tinfo->tag == TY_UNION)
		tinfo->bytes = largest;
}

void declare_var(Sym_tab *tab, AST_node *node) {
	AST_node *child, *sibling, *id_node, *initializer_node;
	Type_info *tinfo, *tinfo_init;
	Sym sym;
	size_t *seg_count;

	assert(node->kind == N_DEC || (node->kind >= N_STRUCTURE && node->kind <= N_ENUMERATION));
	initializer_node = NULL;

	if(node->kind == N_ENUMERATION) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	} else if(node->kind == N_STRUCTURE) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	} else if(node->kind == N_UNIONATION) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	}

	sym = (Sym){0};

	child = node->down;
	assert(child->kind == N_ID || child->kind == N_IDLIST);
	if(child->kind == N_IDLIST)
		id_node = child->down;
	else
		id_node = child;
	sibling = child->next;

	bool has_explicit_type = false;

	if(sibling->kind == N_TYPE) {
		has_explicit_type = true;
		tinfo = build_type(&type_pool, &member_pool, sibling);
		sibling = sibling->next;
	} else if(sibling->kind == N_INITIALIZER) {
		initializer_node = sibling->down;
		tinfo = infer_type(&type_pool, &member_pool, sibling);
	} else {
		assert(0);
	}

	sym.type = tinfo;
	if(sibling) {
		char *tstr, *tstr_init;
		tstr = tstr_init = NULL;
		assert(sibling->kind == N_INITIALIZER);
		initializer_node = sibling->down;
		tinfo_init = typecheck(initializer_node);
		if(has_explicit_type && tinfo_init->tag != TY_VOID) {
			tstr_init = type_tag_debug[tinfo_init->tag];
			tstr = type_tag_debug[tinfo->tag];

			if(tinfo_init->tag == TY_FUNC || tinfo->tag == TY_FUNC)
				myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);

			bool init_is_compound = (tinfo_init->tag >= TY_STRUCT && tinfo_init->tag <= TY_UNION);
			bool var_is_compound = (tinfo->tag >= TY_STRUCT && tinfo->tag <= TY_UNION);

			if(!(init_is_compound && var_is_compound && tinfo_init == tinfo) &&
			   !(tinfo_init->tag == tinfo->tag)) // NOTE don't check bytes yet
				myerror("invalid initialization of %s to %s at%DBG", tstr, tstr_init, &node->debug_info);
		}

		if(tinfo->tag == TY_TYPE)
			sym.val.tinfo = build_type(&type_pool, &member_pool, initializer_node);
		else
			sym.val.ast = initializer_node;
	}

	switch(tab->scope) {
	case SCOPE_GLOBAL:
		if(initializer_node && initializer_node->kind == N_UNINIT) {
			sym.seg = SEG_BSS;
			seg_count = &(tab->seg_count.bss);
		} else {
			sym.seg = SEG_DATA;
			seg_count = &(tab->seg_count.data);
		}
		break;
	case SCOPE_LOCAL:
		sym.seg = SEG_STACK;
		seg_count = &(tab->seg_count.stack);
		break;
	default:
		assert(0);
	}

	for(; id_node; id_node = id_node->next) {
		sym.name = id_node->str;
		sym.debug_info = id_node->debug_info;
		sym.addr = *seg_count;
		*seg_count += sym.type->bytes;
		if(!define_symbol(tab, &sym)) {
			myerror("redefinition of '%s' at%DBG", child->str, &child->debug_info);
		}
	}
}

void declare_constants(Sym_tab *tab, AST_node *root) {
	AST_node *node, *child, *sibling, *id_node, *type_node, *initializer_node;
	Type_info *tinfo, *tinfo_init;
	Sym sym;
	Sym *symptr;
	Debug_info debug_info;
	char *name;

	for(node = root; node; node = node->next) {
		if(node->kind != N_CONSTDEC)
			continue;

		tinfo = tinfo_init = NULL;
		child = node->down;
		type_node = initializer_node = NULL;

		sibling = child->next;

		if(child->kind == N_IDLIST)
			child = child->down;
		assert(child && child->kind == N_ID);
		id_node = child;

		if(sibling->kind != N_INITIALIZER) {
			type_node = sibling;
			assert(type_node->kind == N_TYPE);
			sibling = sibling->next;
		}

		assert(sibling && sibling->kind == N_INITIALIZER);
		initializer_node = sibling->down;

		if(type_node) {
			char *tstr, *tstr_init;
			tstr = tstr_init = NULL;
			tinfo = build_type(&type_pool, &member_pool, type_node);
			tinfo_init = typecheck(initializer_node);
			if(tinfo_init->tag == TY_VOID)
				myerror("invalid uninitialized constant at%DBG", &node->debug_info);

			tstr_init = type_tag_debug[tinfo_init->tag];
			tstr = type_tag_debug[tinfo->tag];

			if(tinfo_init->tag == TY_FUNC || tinfo->tag == TY_FUNC)
				myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);

			bool init_is_compound = (tinfo_init->tag >= TY_STRUCT && tinfo_init->tag <= TY_UNION);
			bool var_is_compound = (tinfo->tag >= TY_STRUCT && tinfo->tag <= TY_UNION);

			if(!(init_is_compound && var_is_compound && tinfo_init == tinfo) &&
			   !(tinfo_init->tag == tinfo->tag && tinfo->bytes >= tinfo_init->bytes))
				myerror("invalid initialization of %s to %s at%DBG", tstr, tstr_init, &node->debug_info);
		}

		sym = (Sym){0};
		sym.constant = true;

		if(initializer_node->kind >= N_STRUCTURE && initializer_node->kind <= N_ENUMERATION) {
			if(id_node->next)
				myerror("compound type cannot be declared with id list%DBG", &child->debug_info);
			sym.type = builtin_types + TY_TYPE;
			symptr = pending_symbol(id_node->str);

			if(!symptr) {
				tinfo = pool_alloc(&type_pool);
				name = id_node->str;
				debug_info = id_node->debug_info;
			} else {
				if(!sym.constant)
					myerror("%s was not declared as constant, to use it in type declarations it must be constant%DBG", symptr->name, &child->debug_info);
				tinfo = symptr->val.tinfo;
				name = symptr->name;
				debug_info = symptr->debug_info;
				*symptr = (Sym){0};
			}

			sym.val.tinfo = tinfo;
			sym.seg = SEG_DATA;
			sym.addr = tab->seg_count.data;
			sym.name = name;
			sym.debug_info = debug_info;
			tab->seg_count.data += tinfo->bytes;

			build_compound_type(&type_pool, &member_pool, initializer_node, tinfo);

			if(tinfo->tag == TY_STRUCT) {
				tinfo->Struct.name = name;
				tinfo->Struct.debug_info = debug_info;
			} else if(tinfo->tag == TY_UNION) {
				tinfo->Union.name = name;
				tinfo->Union.debug_info = debug_info;
			} else if(tinfo->tag == TY_ENUM) {
				tinfo->Enum.name = name;
				tinfo->Enum.debug_info = debug_info;
			} else {
				type_info_print(tinfo, 0);
				assert(0);
			}

			if(!define_symbol(tab, &sym))
				myerror("redefinition of '%s'%DBG", name, &child->debug_info);
		} else {
			tinfo = infer_type(&type_pool, &member_pool, sibling);
			sym.type = tinfo;
			sym.val.ast = initializer_node;
			sym.debug_info = child->debug_info;
			if(sibling->kind == N_FUNCTION) {
				if(id_node->next)
					myerror("function cannot be declared with id list%DBG", &child->debug_info);
				sym.seg = SEG_TEXT;
			} else {
				sym.seg = SEG_DATA;
				sym.addr = tab->seg_count.data;
				tab->seg_count.data += tinfo->bytes;
			}

			for(; id_node; id_node = id_node->next) {
				sym.name = id_node->str;
				if(!define_symbol(tab, &sym)) {
					myerror("redefinition of %s%DBG", child->str, &child->debug_info);
				}
			}
		}

		/* rewrite ast */
		if(initializer_node->kind >= N_FUNCTION && initializer_node->kind <= N_ENUMERATION) {
			assert(child->kind == N_ID);
			initializer_node->str = child->str;
			initializer_node->next = node->next;
			*node = *initializer_node;
		}

		node->visited = true;
	}

	/* set sizes of struct and union */
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

	sym_tab_clear(&pendingtab);
}

void sym_tab_grow(Sym_tab *tab) {
	Sym *old_data = tab->data;
	size_t old_cap = tab->cap;
	tab->data = calloc((tab->cap <<= 1), sizeof(Sym));
	for(size_t i = 0; i < old_cap; ++i) {
		if(!old_data[i].name)
			continue;
		define_symbol(tab, old_data + i);
	}
	free(old_data);
}

size_t sym_tab_hash(Sym_tab *tab, char *name) {
	size_t hash = 0;
	while(*name) hash += *(name++) * 31;
	return hash % tab->cap;
}

int define_symbol(Sym_tab *tab, Sym *symbol) {
	if(tab->sym_count >= tab->cap)
		sym_tab_grow(tab);
	size_t i = sym_tab_hash(tab, symbol->name);
	while(tab->data[i].name) {
		if(!strcmp(tab->data[i].name, symbol->name))
			return 0;
		i = (i + 1) % tab->cap;
	}
	tab->data[i] = (Sym){0};
	tab->data[i] = *symbol;
	++tab->sym_count;
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

Sym *pending_symbol(char *name) {
	size_t i, origin;
	origin = i = sym_tab_hash(&pendingtab, name);
	do {
		if(pendingtab.data[i].name && !strcmp(pendingtab.data[i].name, name))
			return pendingtab.data + i;
		i = (i + 1) % pendingtab.cap;
	} while(i != origin);

	return NULL;
}

Sym* lookup_symbol(char *name) {
	Sym_tab *tab = NULL;
	size_t j = 0, origin = 0;
	Sym *sp = NULL;
	for(int i = scope_depth; i >= 0; --i) {
		tab = tabstack + i;
		origin = j = sym_tab_hash(tab, name);
		do {
			if(tab->data[j].name && !strcmp(tab->data[j].name, name)) {
				sp = tab->data + j;
				break;
			}
			j = (j + 1) % tab->cap;
		} while(j != origin);

		if(sp)
			break;
	}

	return sp;
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
	tab->name = 0;
	tab->sym_count = 0;
	tab->seg_count.data = 0;
	tab->seg_count.bss = 0;
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
			type_info_print(tab->data[i].val.tinfo, 2);
		else
			ast_print(tab->data[i].val.ast, 2, true);
	}
}

BCinst* bc_allocinst(BCmem *mem) {
	BCinst *inst;

	if(mem->text_count >= mem->text_size) {
		mem->text_size <<= 1;
		mem->seg[SEG_TEXT] = realloc(mem->seg[SEG_TEXT], sizeof(BCinst) * mem->text_size);
	}

	inst = (BCinst*)(mem->seg[SEG_TEXT]) + mem->text_count;
	mem->text_count++;
	return inst;
}

BCbyte* bc_growheap(BCmem *mem) {
	BCbyte *heap;

	if(mem->heap_count >= mem->heap_size) {
		mem->heap_size <<= 1;
		mem->seg[SEG_HEAP] = realloc(mem->seg[SEG_HEAP], sizeof(BCbyte) * mem->heap_size);
	}

	heap = mem->seg[SEG_HEAP] + mem->heap_count;
	mem->heap_count++;
	return heap;
}

int bc_interpreter(BCinst *prog, BCmem *mem) {
	BCinst *curinst;
	BCreg *data, *r1, *r2;
	uint64_t r1_data, r2_data, imm, addr, seg, pc, size, sign;
	uint64_t mask;
	uint64_t *memptr;

	pc = 0;
	for(curinst = prog; ; ++curinst) {
		size = curinst->size;
		sign = curinst->sign;
		mask = -1l << size;
		data = mem->regs + curinst->reg;
		r1 = mem->regs + curinst->r1;
		r2 = mem->regs + curinst->r2;
		r1_data = *r1;
		r2_data = *r2;
		imm = curinst->imm;
		if(curinst->immediate) {
			addr = mem->regs[curinst->addr] >> 3;
			seg = mem->regs[curinst->addr] & 0x7;
		} else {
			addr = mem->regs[curinst->ptr] >> 3;
			seg = mem->regs[curinst->ptr] & 0x7;
		}
		memptr = (uint64_t*)(mem->seg[seg]+addr);
		if(curinst->opcode >= BCOP_PUSH && curinst->opcode == BCOP_RET)
			memptr = (uint64_t*)(mem->stack_ptr);

		switch(curinst->opcode) {
		case BCOP_SET:
			if(curinst->immediate)
				*data = imm;
			else
				*data = *r1;
			break;
		case BCOP_ADD:
			*data = r1_data + r2_data;
			break;
		case BCOP_SUB:
			*data = r1_data - r2_data;
			break;
		case BCOP_MUL:
			*data = r1_data * r2_data;
			break;
		case BCOP_DIV:
			*data = r1_data / r2_data;
			break;
		case BCOP_MOD:
			*data = r1_data % r2_data;
			break;
		case BCOP_AND:
			*data = r1_data & r2_data;
			break;
		case BCOP_OR:
			*data = r1_data | r2_data;
			break;
		case BCOP_XOR:
			*data = r1_data ^ r2_data;
			break;
		case BCOP_NOT:
			*data = ~r1_data;
			break;
		case BCOP_LSHIFT:
			*data = r1_data << r2_data;
			break;
		case BCOP_RSHIFT:
			*data = r1_data >> r2_data;
			break;
		case BCOP_JMP:
			assert(seg == SEG_TEXT);
			curinst = prog + addr;
			break;
		case BCOP_BLT:
			assert(seg == SEG_TEXT);
			if(sign) {
				if((int64_t)r1_data < (int64_t)r2_data)
					curinst = prog + addr;
			} else {
				if(r1_data < r2_data)
					curinst = prog + addr;
			}
			break;
		case BCOP_BGE:
			assert(seg == SEG_TEXT);
			if(sign) {
				if((int64_t)r1_data >= (int64_t)r2_data)
					curinst = prog + addr;
			} else {
				if(r1_data >= r2_data)
					curinst = prog + addr;
			}
			break;
		case BCOP_BNE:
			assert(seg == SEG_TEXT);
			if(r1_data != r2_data)
				curinst = prog + addr;
			break;
		case BCOP_BEQ:
			assert(seg == SEG_TEXT);
			if(r1_data == r2_data)
				curinst = prog + addr;
			break;
		case BCOP_LOAD:
			*r1 = *memptr ^ mask;
			break;
		case BCOP_STORE:
			*memptr &= mask;
			*memptr |= *r1 ^ mask;
			break;
		case BCOP_PUSH:
			*memptr &= mask;
			*memptr |= *r1 ^ mask;
			mem->stack_ptr += sizeof(size >> 3);
			break;
		case BCOP_POP:
			mem->stack_ptr -= sizeof(size >> 3);
			*r1 = *memptr ^ mask;
			break;
		case BCOP_CALL:
			assert(seg == SEG_TEXT);
			pc = ((curinst - prog) << 3) | (SEG_TEXT & 0x7);
			*memptr = pc;
			mem->stack_ptr += sizeof(pc);
			break;
		case BCOP_RET:
			mem->stack_ptr -= sizeof(pc);
			pc = *memptr >> 3;
			curinst = prog + pc;
			break;
		case BCOP_INT:
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
			break;
		case BCOP_HALT:
			return 0;
		}

		// check for overflows
		*data ^= mask;

		// sign extend LOAD and POP
		*r1 = -((sign << size) - *r1);
	}
}

void parse_error(char *expect) {
	fprintf(stderr, "jcc: error: parser expected %s got '", expect);
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fprintf(stderr, "'\n> line: %i, col: %i\n> ", lexer.debug_info.line, lexer.debug_info.col - (int)(lexer.ptr - lexer.unget));
	fwrite(lexer.debug_info.line_s, 1, lexer.debug_info.line_e - lexer.debug_info.line_s, stderr);
	exit(1);
}

// TODO add more custom formats
void myerror(char *fmt, ...) {
	va_list args;
	char *s;
	char buf[256];
	Debug_info *dbg;
	va_start(args, fmt);
	s = NULL;
	fprintf(stderr, "jcc: error: ");
	while(true) {
		s = strstr(fmt,"%DBG");
		if(s == NULL) {
			vfprintf(stderr, fmt, args);
			break;
		}
		assert(s-fmt < 256);
		strncpy(buf, fmt, s - fmt);
		buf[s-fmt] = 0;
		vfprintf(stderr, buf, args);
		fmt = s + STRLEN("%DBG");
		dbg = va_arg(args, Debug_info*);
		fprintf(stderr, "\n> line: %i, col: %i\n> ", dbg->line, dbg->col);
		fwrite(dbg->line_s, 1, dbg->line_e - dbg->line_s, stderr);
	}
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

	tp = s = NULL;
	lexer.text_s = lexer.text_e = NULL;
	check = 0;

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
		check = 1;
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
		default:
			check = 0;
			break;
		}
		if(check) {
			lexer.text_e = tp + 2;
			lexer.ptr += 2;
			lexer.debug_info.col += 2;
			return lexer.token;
		}
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

	/* numbers, strings and identifiers */
	s = tp;
	if(s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) { /* hex */
		s += 2;
		for(check = 0; *s && (isdigit(*s) || (*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'F') || *s == '_'); ++s)
			++check;
	} else if(s[0] == '0' && (s[1] == 'b' || s[1] == 'B')) { /* binary */
		s += 2;
		for(check = 0; *s && (*s == '0' || *s == '1' || *s == '_'); ++s)
			++check;
	} else if(s[0] == '0' && isdigit(s[1])) { /* octal */
		++s;
		for(check = 0; *s && *s >= '0' && *s <= '7'; ++s)
			++check;
	} else {
		for(check = 0, s = tp; *s && isdigit(*s); ++s)
			++check;
		if(*s == '.') {
			++s;
			for(check = 0; *s && isdigit(*s); ++s)
				++check;
			if(check) {
				lexer.debug_info.col += s - tp;
				lexer.text_s = tp;
				lexer.text_e = lexer.ptr = s;
				return (lexer.token = T_FLOATING);
			}
		}
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
		myerror("illegal top level statement at%DBG", &lexer.debug_info);
	}
}

/*
 * declaration: identifier ':' type? ('='|':') (literal';'|function|structure|unionation|enumeration)
 */
AST_node* declaration(void) {
	register int t;
	AST_node *root, *child;
	char *unget1, *unget2;

	t = lex();
	unget1 = lexer.unget;

	if(t == T_STRUCT) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return structure();
	}

	if(t == T_UNION) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return unionation();
	}

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

	root = ast_alloc(&ast, N_DEC, &lexer.debug_info);
	root->down = ast_alloc(&ast, N_IDLIST, &lexer.debug_info);
	child = ast_alloc(&ast, N_ID, &lexer.debug_info);
	child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	root->down->down = child;

	t = lex();

	while(t == ',') {
		t = lex();
		if(t != T_ID)
			parse_error("identifier");
		child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ':')
		parse_error("':'");

	child = root->down;

	child->next = typename();

	t = lex();

	if(child->next && t == ';')
		return root;
	else if(!child->next && t == ';')
		parse_error("type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc(&ast, N_INITIALIZER, &lexer.debug_info);
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

	child->down = expression();

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
	AST_node *root, *child;
	char *unget1, *unget2;

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

	root = ast_alloc(&ast, N_DEC, &lexer.debug_info);
	root->down = ast_alloc(&ast, N_IDLIST, &lexer.debug_info);
	child = ast_alloc(&ast, N_ID, &lexer.debug_info);
	child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	root->down->down = child;

	t = lex();

	while(t == ',') {
		t = lex();
		if(t != T_ID)
			parse_error("identifier");
		child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
	}

	if(t != ':')
		parse_error("':'");

	child = root->down;

	child->next = typename();

	t = lex();

	if(child->next && (t == ',' || t == ')' || t == '{' || t == ';')) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	}
	else if(!child->next && (t == ',' || t == ')' || t == '{'))
		parse_error("type");

	if(child->next)
		child = child->next;

	child->next = ast_alloc(&ast, N_INITIALIZER, &lexer.debug_info);
	child = child->next;

	if(t != '=')
		parse_error("'=' or ':'");

	child->down = expression();

	if(child->down) {
		t = lex();
		if(t == ',' || t == ')' || t == '{' || t == ';') {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
		} else if(t != ';')
			parse_error("';'");

		return root;
	} else
		parse_error("initializer");

	return NULL;
}

AST_node* literal(void) {
	register int t;
	AST_node *root;

	root = NULL;
	t = lex();

	if((t >= T_TRUE  && t <= T_BAR) || t == '.' || t == '{')
		root = ast_alloc(&ast, 0, &lexer.debug_info);
	else if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		root = typename();
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	switch(t) {
	case T_INTEGER:
		root->kind = N_INTLIT;
		root->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_FLOATING:
		root->kind = N_FLOATLIT;
		root->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_STR:
		root->kind = N_STRLIT;
		root->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_CHARACTER:
		root->kind = N_CHARLIT;
		root->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_TRUE: case T_FALSE:
		root->kind = N_BOOLLIT;
		root->boolean = (t == T_TRUE);
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
AST_node* typename(void) {
	register int t;
	AST_node *root, *child, *node;
	AST_node tmp;
	AST_node *tail;
	AST_node *head;

	tail = head = child = NULL;

	t = lex();
	if(t == '*' || t == '[') {
		node = &tmp;
		while(t == '*' || t == '[') {
			node->down = ast_alloc(&ast, N_POINTER, &lexer.debug_info);
			node = node->down;
			if(t == '[') {
				node->kind = N_ARRAY;
				node->next = expression();
				t = lex();
				if(t != ']')
					parse_error("']'");
			}
			t = lex();
		}
		head = tmp.down;
		tail = node;
	}

	if(!(t >= T_INT && t <= T_F64) && t != T_ID &&
			t != T_FUNC && t != T_STRUCT && t != T_UNION && t != T_ENUM) {
		if(child)
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
		node = ast_alloc(&ast, 0, &lexer.debug_info);

	root = ast_alloc(&ast, N_TYPE, &lexer.debug_info);
	if(head && tail) {
		root->down = head;
		child = tail;
		child->down = node;
		child = child->down;
	} else {
		root->down = node;
		child = node;
	}

	switch(t) {
	case T_STRUCT: case T_UNION: case T_ENUM:
		break;
	case T_ID:
		child->kind = N_ID;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_FUNC:
		child->kind = N_FUNCTION;
		t = lex();
		if(t != '(')
			parse_error("'('");
		child->down = ast_alloc(&ast, N_ARGLIST, &lexer.debug_info);
		child = child->down;
		child->down = node = typename();
		t = lex();
		if(t != ',' && t != ')')
			parse_error("',' or ')'");
		while(t != ')') {
			node->next = typename();
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
			child->next = ast_alloc(&ast, N_RETLIST, &lexer.debug_info);
			child = child->next;
			child->down = node = typename();
			t = lex();
			if(t != ',' && t != ')')
				parse_error("',' or ')'");
			while(node && t != ')') {
				node->next = typename();
				node = node->next;
				t = lex();
				if(t != ',' && t != ')')
					parse_error("',' or ')'");
			}
		}
		break;
	default: /* builtin type */
		assert(t >= T_INT && t <= T_F64);
		child->kind = N_BUILTINTYPE;
		child->typesig = t - T_INT;
		if(t >= T_CHAR)
			child->typesig += 3;
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

	root = ast_alloc(&ast, N_FUNCTION, &lexer.debug_info);

	t = lex();
	if(t == '(') {
		node = vardec();
		if(node) {
			root->down = child = ast_alloc(&ast, N_ARGLIST, &lexer.debug_info);
			child->down = node;
		}

		t = lex();
		while(t == ',') {
			node->next = vardec();
			if(node->next)
				node = node->next;
			t = lex();
		}

		if(t == T_VOID) {
			node = ast_alloc(&ast, N_TYPE, &lexer.debug_info);
			node->down = ast_alloc(&ast, N_BUILTINTYPE, &lexer.debug_info);
			node->down->typesig = T_VOID;
			t = lex();
		}

		if(t != ')')
			parse_error("')'");
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	/* return type */
	node = typename();
	if(node) {
		if(!child) {
			root->down = child = ast_alloc(&ast, N_RETLIST, &lexer.debug_info);
			child->down = node;
		} else {
			child->next = ast_alloc(&ast, N_RETLIST, &lexer.debug_info);
			child = child->next;
			child->down = node;
		}
	}

	t = lex();
	if(t == ',') {
		node->next = typename();
		if(node->next)
			node = node->next;
		t = lex();
		while(t == ',') {
			node->next = typename();
			if(node->next)
				node = node->next;
			t = lex();
		}
	}

	if(t == T_INLINE) {
		node = ast_alloc(&ast, N_STORAGEQUALIFIER, &lexer.debug_info);
		node->str = keyword[T_INLINE-T_ENUM];
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
	AST_node tmp;

	child = &tmp;
	root = ast_alloc(&ast, N_BLOCK, &lexer.debug_info);

	for(; child; child = child->next) {
		if((child->next = returnstatement())) continue;
		if((child->next = ifstatement())) continue;
		if((child->next = whilestatement())) continue;
		if((child->next = forstatement())) continue;
		if((child->next = declaration())) continue;
		if((child->next = vardec())){
			t = lex();
			if(t != ';')
				parse_error("';'");
			continue;
		}
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

	root->down = tmp.next;

	return root;
}

/*
 * structure: 'struct' '{' ( (vardec) | (unionation) ('=' literal)? ';' )+  '}'
 */
AST_node* structure(void) {
	register int t;
	AST_node *root, *child;
	AST_node tmp;

	t = lex();
	if(t != T_STRUCT) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = &tmp;
	root = ast_alloc(&ast, N_STRUCTURE, &lexer.debug_info);

	t = lex();
	if(t != '{')
		parse_error("'{'");

	t = lex();
	while(t != '}') {
		if(t == T_ID) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = vardec();
			t = lex();
			if(t != ';')
				parse_error("';'");
		} else if(t == T_UNION) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t != '}' && t != T_ID && t != T_STRUCT && t != T_UNION) {
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
			if(t != '}' && t != T_ID && t != T_STRUCT && t != T_UNION) {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error("identifier or 'union'");
		}

		child = child->next;
		t = lex();
	}

	root->down = tmp.next;

	return root;
}

/*
 * unionation: 'union' '{' (declaration)+ '}'
 */
AST_node* unionation(void) {
	register int t;
	AST_node *root, *child;
	AST_node tmp;

	t = lex();
	if(t != T_UNION) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	child = &tmp;
	root = ast_alloc(&ast, N_UNIONATION, &lexer.debug_info);

	t = lex();
	if(t != '{')
		parse_error("'{'");

	t = lex();
	while(t != '}') {
		if(t == T_ID) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = vardec();
			t = lex();
			if(t != ';')
				parse_error("';'");
		} else if(t == T_UNION) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			child->next = unionation();
			t = lex();
			if(t != '}' && t != T_ID && t != T_STRUCT && t != T_UNION) {
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
			if(t != '}' && t != T_ID && t != T_STRUCT && t != T_UNION) {
				parse_error("next member");
			} else {
				lexer.debug_info.col -= lexer.ptr - lexer.unget;
				lexer.ptr = lexer.unget;
			}
		} else {
			parse_error("identifier or 'union'");
		}

		child = child->next;
		t = lex();
	}

	root->down = tmp.next;

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
	root = ast_alloc(&ast, N_ENUMERATION, &lexer.debug_info);

	t = lex();
	if(t >= T_INT && t <= T_S64) {
		root->down = child = ast_alloc(&ast, N_BUILTINTYPE, &lexer.debug_info);
		child->typesig = t - T_INT;
		t = lex();
	}

	if(t != '{')
		parse_error("'{'");

	t = lex();
	if(t != T_ID)
		parse_error("identifier");

	if(!child) {
		root->down = child = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else {
		child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
	}

	t = lex();
	if(t == '=') {
		t = lex();
		if(t != T_INTEGER && t != T_CHARACTER)
			parse_error("numerical or character constant");
		child->next = ast_alloc(&ast, N_INTLIT, &lexer.debug_info);
		child = child->next;
		if(t == T_CHARACTER)
			child->kind = N_CHARLIT;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
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
		child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
		t = lex();
		if(t == '=') {
			t = lex();
			if(t != T_INTEGER && t != T_CHARACTER)
				parse_error("numerical or character constant");
			child->next = ast_alloc(&ast, N_INTLIT, &lexer.debug_info);
			child = child->next;
			if(t == T_CHARACTER)
				child->kind = N_CHARLIT;
			child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
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

	root = ast_alloc(&ast, N_STATEMENT, &lexer.debug_info);

	if(t == T_DEFER) {
		root->kind = N_DEFERSTATEMENT;
	} else {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
	}

	root->down = child = assignment();

	t = lex();

	if(t != ';') {
		parse_error("';'");
	}

	return root;
}

AST_node* label(void) {
	register int t;
	AST_node *root;
	char *unget;
	char *text_s, *text_e;

	unget = lexer.ptr;

	t = lex();
	if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	text_s = lexer.text_s;
	text_e = lexer.text_e;

	t = lex();
	if(t != ':') {
		lexer.debug_info.col -= lexer.ptr - unget;
		lexer.ptr = unget;
		return NULL;
	}

	root = ast_alloc(&ast, N_LABEL, &lexer.debug_info);
	root->str = pool_alloc_string(&string_pool, text_e - text_s, text_s);

	return root;
}

/*
 * ifstatement:
 * 	'if' ' ' (identifier ':')? expression '{' block '}' ('elif' ' ' expression '{' block '}')* ('else' '{' block '}')?
 */
AST_node* ifstatement(void) {
	register int t;
	AST_node *root, *child;

	t = lex();

	if(t != T_IF) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc(&ast, N_IFSTATEMENT, &lexer.debug_info);

	child = label();

	if(!child) {
		root->down = child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->down = expression();
	} else {
		root->down = child;
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->next->down = expression();
		child = child->next;
	}

	if(lex() != '{')
		parse_error("'{'");
	child->next = block();
	if(lex() != '}')
		parse_error("'}'");
	child = child->next;

	while(1) {
		t = lex();
		if(t == T_ELIF) {
			child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
			child->next->down = expression();
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
 * 	'while' ' ' (identifier ':')? expression '{' block '}'
 */
AST_node* whilestatement(void) {
	register int t;
	AST_node *root, *child;

	if(lex() != T_WHILE) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc(&ast, N_WHILESTATEMENT, &lexer.debug_info);

	child = label();

	if(!child) {
		root->down = child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->down = expression();
	} else {
		root->down = child;
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->next->down = expression();
		child = child->next;
	}

	t = lex();
	if(t != '{')
		parse_error("'{'");
	child->next = block();
	t = lex();
	if(t != '}')
		parse_error("'}'");

	return root;
}

/*
 * returnstatement: 'return' expression (',' expression)* ';'
 */
AST_node* returnstatement(void) {
	register int t;
	AST_node *root, *child;

	if(lex() != T_RETURN) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc(&ast, N_RETURNSTATEMENT, &lexer.debug_info);

	root->down = child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
	child->down = expression();

	t = lex();
	while(t == ',') {
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->next->down = expression();
		child = child->next;
		t = lex();
	}

	if(t != ';')
		parse_error("';'");

	return root;
}

/*
 * forstatement: 'for' ' ' (identifier ':')? (vardec ','?)* ';' expression ';' expression '{' block '}'
 */
AST_node* forstatement(void) {
	register int t;
	AST_node *root, *child, *node;

	t = lex();
	if(t != T_FOR) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}

	root = ast_alloc(&ast, N_FORSTATEMENT, &lexer.debug_info);

	child = label();

	node = vardec();
	if(!node)
		node = assignment();

	if(!child) {
		root->down = child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->down = expression();
	} else {
		root->down = child;
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->next->down = expression();
		child = child->next;
	}

	t = lex();
	while(child && t == ',') {
		child->next = vardec();
		if(child->next)
			child = child->next;
		else {
			child->next = assignment();
			if(!child->next)
				break;
		}
		t = lex();
	}

	if(t != ';')
		parse_error("';'");
	if(child) {
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->next->down = expression();
	} else {
		root->down = child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child->down = expression();
	}
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
	if(t != '=' && t != ',' && !(t >= T_ASSIGNPLUS && t <= T_ASSIGNRSHIFT) && t != T_INC && t != T_DEC) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	if(!child) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		parse_error("expression");
	}

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->down = child;

	if(t == ',') {
		root->down = ast_alloc(&ast, N_IDLIST, &lexer.debug_info);
		root->down->down = child;
	}

	while(t == ',') {
		child->next = expression();
		if(!child->next)
			child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
		child = child->next;
		t = lex();
	}

	if(t != '=' && root->down->kind == N_IDLIST)
		myerror("multiple assignment must use '=' operator%DBG", &root->down->debug_info);

	root->op = (t == '=') ? OP_ASSIGN : (t - T_ASSIGNPLUS);

	if(t == T_INC) {
		root->op = OP_INC;
	} else if(t == T_DEC) {
		root->op = OP_DEC;
	} else {
		root->next = expression();
		if(!root->next) {
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			parse_error("expression");
		}
	}

	return root;
}

AST_node* expression(void) {
	return logical();
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->op = (t == T_AND) ? OP_LAND : OP_LOR;
	root->down = child;
	root->next = logical();

	if(!root->next)
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '<':
		root->op = OP_LT;
		break;
	case '>':
		root->op = OP_GT;
		break;
	case T_EQ: case T_NEQ: case T_LTEQ: case T_GTEQ:
		root->op = (t - T_EQ) + OP_EQ;
		break;
	}
	root->down = child;
	root->next = compare();

	if(!root->next)
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->op = (t == T_LSHIFT) ? OP_LSHIFT : OP_RSHIFT;
	root->down = child;
	root->next = shift();

	if(!root->next)
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '&':
		root->op = OP_AND;
		break;
	case '|':
		root->op = OP_OR;
		break;
	case '^':
		root->op = OP_XOR;
		break;
	}
	root->down = child;
	root->next = bitwise();

	if(!root->next)
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->op = (t == '+') ? OP_ADD : OP_SUB;
	root->down = child;
	root->next = arith();

	if(!root->next)
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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	switch(t) {
	case '*':
		root->op = OP_MUL;
		break;
	case '/':
		root->op = OP_DIV;
		break;
	case '%':
		root->op = OP_MOD;
		break;
	}
	root->down = child;
	root->next = factor();

	if(!root->next)
		parse_error("right operand");

	return root;
}

AST_node* unary(void) {
	register int t;
	AST_node *root;

	root = NULL;
	t = lex();

	switch(t) {
		case '*':
			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_DEREF;
			root->down = unary();
			if(!root->down)
				parse_error("term");
			break;
		case '&':
			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_ADDR;
			root->down = unary();
			if(!root->down)
				parse_error("term");
			break;
		case T_NOT:
			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_LNOT;
			root->down = unary();
			if(!root->down)
				parse_error("term");
			break;
		case '-':
			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_NEG;
			root->down = unary();
			if(!root->down)
				parse_error("term");
			break;
		case '~':
			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_NOT;
			root->down = unary();
			if(!root->down)
				parse_error("term");
			break;
		case T_CAST:
			t = lex();

			root = ast_alloc(&ast, N_OP, &lexer.debug_info);
			root->op = OP_CAST;
			root->next = typename();
			if(!root)
				parse_error("type");
			t = lex();
			if(t != ')')
				parse_error("')'");
			root->down = unary();
			break;
		default:
			lexer.debug_info.col -= lexer.ptr - lexer.unget;
			lexer.ptr = lexer.unget;
			break;
	}

	if(!root)
		return postfix();

	return root;
}

// use shunting yard to parse postfix operators . [] ()
AST_node* postfix(void) {
	register int t;
	AST_node *child, *node, *root;

	child = term();
	root = child;

	if(!child)
		return NULL;

	t = lex();
	while (t == '.' || t == '(' || t == '[') {
		if(t == '.') {
			t = lex();
			if(t != T_ID)
				parse_error("identifier");
			child = ast_alloc(&ast, N_OP, &lexer.debug_info);
			child->op = OP_DOT;
			child->next = ast_alloc(&ast, N_ID, &lexer.debug_info);
			child->next->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
			child->down = root;
			root = child;
		} else if(t == '[') {
			child = ast_alloc(&ast, N_OP, &lexer.debug_info);
			child->op = OP_SUBSCRIPT;
			child->next = expression();
			if(!child->next)
				parse_error("expression");
			t = lex();
			if(t != ']')
				parse_error("']'");
			child->down = root;
			root = child;
		} else if(t == '(') {
			child = ast_alloc(&ast, N_OP, &lexer.debug_info);
			child->op = OP_CALL;
			child->down = root;
			child->next = expression();
			if(child->next) {
				node = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
				node->down = child->next;
				child->next = node;
				t = lex();
				while(t == ',') {
					node->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
					node->next->down = expression();
					node = node->next;
					t = lex();
				}
			} else {
				t = lex();
			}

			if(t != ')')
				parse_error("')'");

			root = child;
		} else {
			assert(0);
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

	node = literal();
	if(node)
		return node;
	t = lex();
	if(t == T_ID) {
		node = ast_alloc(&ast, N_ID, &lexer.debug_info);
		node->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);
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

void compile(AST_node *root) {
	AST_node *node;
	//TODO process directives (import, load, etc)

	declare_constants(&tabstack[0], root);

	for(node = root; node; node = node->next) {
		if(node->kind == N_DEC)
			declare_var(&tabstack[0], node);
		if(node->kind != N_FUNCTION)
			continue;
		++scope_depth;
		compile_function(node);
		--scope_depth;
	}
}

void compile_function(AST_node *node) {
	Sym_tab *tab = NULL;
	Sym *symptr = NULL;
	Sym symbol = {0};
	Type_member *mp = NULL, *args = NULL, *rets = NULL;
	Type_info *functype = NULL;
	char *funcname = node->str;

	tab = tabstack + scope_depth;
	symptr = lookup_symbol(node->str);
	assert(symptr);

	functype = symptr->type;
	args = functype->Func.arg_types;
	rets = functype->Func.ret_types;
	tab->name = funcname;

	// declare arguments
	for(mp = args; mp; mp = mp->next) {
		symbol.name = mp->name;
		symbol.type = mp->type;
		symbol.seg = SEG_STACK;
		symbol.val.ast = mp->initial_val_expr;
		symbol.addr = tab->seg_count.stack;
		symbol.debug_info = mp->debug_info;
		tab->seg_count.stack += mp->type->bytes;
		define_symbol(tab, &symbol);
	}

	// declare named return values
	for(mp = rets; mp; mp = mp->next) {
		if(!mp->name)
			continue;
		symbol.name = mp->name;
		symbol.type = mp->type;
		symbol.seg = SEG_STACK;
		symbol.val.ast = mp->initial_val_expr;
		symbol.addr = tab->seg_count.stack;
		symbol.debug_info = mp->debug_info;
		tab->seg_count.stack += mp->type->bytes;
		define_symbol(tab, &symbol);
	}

	node = node->down;
	while(node && node->kind != N_BLOCK)
		node = node->next;
	node = node->down;

	declare_constants(tabstack+scope_depth, node);
	for(; node; node = node->next) {
		if(node->kind == N_DEC)
			declare_var(tabstack+scope_depth, node);
		if(node->kind != N_STATEMENT)
			continue;
		typecheck(node->down);
	}
	sym_tab_print(tabstack+scope_depth);
	sym_tab_clear(tabstack+scope_depth);
}

void compile_block(AST_node *node) {
	AST_node *child;
	AST_node defer_head;
	AST_node *defer_tail = &defer_head;
	assert(0);
//	BCinst *inst;

	assert(node->kind == N_BLOCK);

	for(node = node->down; node; node = node->next) {
		child = node->down;
		switch(node->kind) {
		case N_DEFERSTATEMENT:
			node->down = child->next;
			defer_tail->next = node;
			defer_tail = defer_tail->next;
			break;
		case N_STATEMENT:
			if(child->kind == N_OP) {
				compile_expression(child);
				continue;
			}

			assert(child->kind == N_OP);

			if(child->op == OP_ASSIGN) {

			}

			break;
		case N_IFSTATEMENT:
			break;
		case N_WHILESTATEMENT:
			break;
		case N_FORSTATEMENT:
			break;
		case N_RETURNSTATEMENT:
			break;
		default:
			assert(0);
			break;
		}
	}
}

void compile_expression(AST_node *node) {
}

void compile_term(AST_node *node) {
}

void compile_call(AST_node *node) {
}

void cleanup(void) {
	pool_free(&ast.pool);
	pool_free(&string_pool);
	pool_free(&type_pool);
	pool_free(&member_pool);
	for(int i = 0; i < ARRLEN(tabstack); ++i)
		if(tabstack[i].data)
			free(tabstack[i].data);
	free(pendingtab.data);
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
	for(int i = 0; i < ARRLEN(tabstack); ++i) {
		SYM_TAB_INIT(tabstack[i],SCOPE_LOCAL);
	}
	fmapread(&fm);
	lexer_init(&lexer, fm.buf);
	pool_init(&ast.pool, sizeof(AST_node), 256, 4);
	pool_init(&string_pool, sizeof(char), 512, 4);
	pool_init(&type_pool, sizeof(Type_info), 128, 1);
	pool_init(&member_pool, sizeof(Type_member), 128, 1);

	parse();
	ast_print(ast.root, 0, false);
	//exit(0);

	tabstack[0].name = argv[1];
	fprintf(stderr,"################ TESTING COMPILE FUNCTIONS #####################\n");
	compile(ast.root);
	//ast_print(ast.root, 0, false);
	sym_tab_print(&tabstack[0]);
	sym_tab_clear(&tabstack[0]);
	sym_tab_clear(&pendingtab);
	return 0;
}
