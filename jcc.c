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
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define SYM_TAB_DEFAULT ((uint64_t)-1)
#define Array(T) T *
#define STATICARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */
#define SYM_TAB_INIT(tab) { \
	tab.name = NULL;\
	tab.hashmap = NULL;\
	shdefault(tab.hashmap, SYM_TAB_DEFAULT);\
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
	T_TWODOT,
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
	"T_TWODOT",
	"T_ID",
	"T_END",
	"T_ILLEGAL",
};

enum BCseg {
	SEG_TEXT = 0,
	SEG_BSS,
	SEG_DATA,
	SEG_STACK,
	SEG_HEAP,
};

char *segments_debug[] = {
	"SEG_TEXT",
	"SEG_BSS",
	"SEG_DATA",
	"SEG_STACK",
	"SEG_HEAP",
};

enum AST_kind {
	N_DECL,
	N_TYPE,
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
	N_POINTER,
	N_ARRAY,
	N_OP,
	N_EXPRESSION,
	N_INTLIT,
	N_FLOATLIT,
	N_STRLIT,
	N_CHARLIT,
	N_BOOLLIT,
	N_ARRAYLIT,
	N_STRUCTLIT,
	N_RANGELIT,
	N_UNINIT,
	N_STORAGEQUALIFIER,
	N_BUILTINTYPE,
	N_ID,
};

char *nodes_debug[] = {
	"N_DECL",
	"N_TYPE",
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
	"N_POINTER",
	"N_ARRAY",
	"N_OP",
	"N_EXPRESSION",
	"N_INTLIT",
	"N_FLOATLIT",
	"N_STRLIT",
	"N_CHARLIT",
	"N_BOOLLIT",
	"N_ARRAYLIT",
	"N_STRUCTLIT",
	"N_RANGELIT",
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

enum AST_op {
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
	OP_ASSIGNCONST,
	OP_INC,
	OP_DEC,
	OP_BINDTYPE,
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
	OP_RANGE,
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
	"OP_ASSIGNCONST",
	"OP_INC",
	"OP_DEC",
	"OP_BINDTYPE",
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
	"OP_RANGE",
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

enum Sym_kind {
	SYM_CONST,
	SYM_VAR,
	SYM_TYPE,
	SYM_FUNC,
};

char *sym_kind_debug[] = {
	"SYM_CONST",
	"SYM_VAR",
	"SYM_TYPE",
	"SYM_FUNC",
};

enum BCtype {
	BC_WORD = 0,
	BC_HALF,
	BC_SHORT,
	BC_BYTE,
	BC_FLOAT,
	BC_DOUBLE,
	BC_PTR,
};

char *BCtype[] = {
	"BC_WORD",
	"BC_HALF",
	"BC_SHORT",
	"BC_BYTE",
	"BC_FLOAT",
	"BC_DOUBLE",
	"BC_PTR",
};

enum BCop {
	BCOP_SET,
	BCOP_SET_IMM,
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

	BCOP_SET_FLOAT,
	BCOP_SET_FLOAT_IMM,
	BCOP_ADD_FLOAT,
	BCOP_SUB_FLOAT,
	BCOP_MUL_FLOAT,
	BCOP_DIV_FLOAT,

	BCOP_SET_DOUBLE,
	BCOP_SET_DOUBLE_IMM,
	BCOP_ADD_DOUBLE,
	BCOP_SUB_DOUBLE,
	BCOP_MUL_DOUBLE,
	BCOP_DIV_DOUBLE,

	BCOP_JMP,
	BCOP_BLT,
	BCOP_BGE,
	BCOP_BLTU,
	BCOP_BGEU,
	BCOP_BNE,
	BCOP_BEQ,
	BCOP_BLT_FLOAT,
	BCOP_BGE_FLOAT,
	BCOP_BLT_DOUBLE,
	BCOP_BGE_DOUBLE,

	BCOP_JMP_IMM_ADDR,
	BCOP_BLT_IMM_ADDR,
	BCOP_BGE_IMM_ADDR,
	BCOP_BLTU_IMM_ADDR,
	BCOP_BGEU_IMM_ADDR,
	BCOP_BNE_IMM_ADDR,
	BCOP_BEQ_IMM_ADDR,
	BCOP_BLT_FLOAT_IMM_ADDR,
	BCOP_BGE_FLOAT_IMM_ADDR,
	BCOP_BLT_DOUBLE_IMM_ADDR,
	BCOP_BGE_DOUBLE_IMM_ADDR,

	BCOP_LOAD_WORD,
	BCOP_STORE_WORD,
	BCOP_PUSH_WORD,
	BCOP_POP_WORD,

	BCOP_LOAD_BYTE,
	BCOP_LOAD_UBYTE,
	BCOP_STORE_BYTE,
	BCOP_PUSH_BYTE,
	BCOP_POP_BYTE,
	BCOP_POP_UBYTE,

	BCOP_LOAD_HALF,
	BCOP_LOAD_UHALF,
	BCOP_STORE_HALF,
	BCOP_PUSH_HALF,
	BCOP_POP_HALF,
	BCOP_POP_UHALF,

	BCOP_LOAD_FLOAT,
	BCOP_STORE_FLOAT,
	BCOP_PUSH_FLOAT,
	BCOP_POP_FLOAT,

	BCOP_LOAD_DOUBLE,
	BCOP_STORE_DOUBLE,
	BCOP_PUSH_DOUBLE,
	BCOP_POP_DOUBLE,

	BCOP_LOAD_WORD_IMM_ADDR,
	BCOP_STORE_WORD_IMM_ADDR,

	BCOP_LOAD_BYTE_IMM_ADDR,
	BCOP_LOAD_UBYTE_IMM_ADDR,
	BCOP_STORE_BYTE_IMM_ADDR,

	BCOP_LOAD_HALF_IMM_ADDR,
	BCOP_LOAD_UHALF_IMM_ADDR,
	BCOP_STORE_HALF_IMM_ADDR,

	BCOP_LOAD_FLOAT_IMM_ADDR,
	BCOP_STORE_FLOAT_IMM_ADDR,

	BCOP_LOAD_DOUBLE_IMM_ADDR,
	BCOP_STORE_DOUBLE_IMM_ADDR,

	BCOP_CALL,
	BCOP_RET,

	BCOP_INTERRUPT,
	BCOP_HALT,
};

char *opcode_debug[] = {
	"BCOP_SET",
	"BCOP_SET_IMM",
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

	"BCOP_SET_FLOAT",
	"BCOP_SET_FLOAT_IMM",
	"BCOP_ADD_FLOAT",
	"BCOP_SUB_FLOAT",
	"BCOP_MUL_FLOAT",
	"BCOP_DIV_FLOAT",

	"BCOP_SET_DOUBLE",
	"BCOP_SET_DOUBLE_IMM",
	"BCOP_ADD_DOUBLE",
	"BCOP_SUB_DOUBLE",
	"BCOP_MUL_DOUBLE",
	"BCOP_DIV_DOUBLE",

	"BCOP_JMP",
	"BCOP_BLT",
	"BCOP_BGE",
	"BCOP_BLTU",
	"BCOP_BGEU",
	"BCOP_BNE",
	"BCOP_BEQ",
	"BCOP_BLT_FLOAT",
	"BCOP_BGE_FLOAT",
	"BCOP_BLT_DOUBLE",
	"BCOP_BGE_DOUBLE",

	"BCOP_JMP_IMM_ADDR",
	"BCOP_BLT_IMM_ADDR",
	"BCOP_BGE_IMM_ADDR",
	"BCOP_BLTU_IMM_ADDR",
	"BCOP_BGEU_IMM_ADDR",
	"BCOP_BNE_IMM_ADDR",
	"BCOP_BEQ_IMM_ADDR",
	"BCOP_BLT_FLOAT_IMM_ADDR",
	"BCOP_BGE_FLOAT_IMM_ADDR",
	"BCOP_BLT_DOUBLE_IMM_ADDR",
	"BCOP_BGE_DOUBLE_IMM_ADDR",

	"BCOP_LOAD_WORD",
	"BCOP_STORE_WORD",
	"BCOP_PUSH_WORD",
	"BCOP_POP_WORD",

	"BCOP_LOAD_BYTE",
	"BCOP_LOAD_UBYTE",
	"BCOP_STORE_BYTE",
	"BCOP_PUSH_BYTE",
	"BCOP_POP_BYTE",
	"BCOP_POP_UBYTE",

	"BCOP_LOAD_HALF",
	"BCOP_LOAD_UHALF",
	"BCOP_STORE_HALF",
	"BCOP_PUSH_HALF",
	"BCOP_POP_HALF",
	"BCOP_POP_UHALF",

	"BCOP_LOAD_FLOAT",
	"BCOP_STORE_FLOAT",
	"BCOP_PUSH_FLOAT",
	"BCOP_POP_FLOAT",

	"BCOP_LOAD_DOUBLE",
	"BCOP_STORE_DOUBLE",
	"BCOP_PUSH_DOUBLE",
	"BCOP_POP_DOUBLE",

	"BCOP_LOAD_WORD_IMM_ADDR",
	"BCOP_STORE_WORD_IMM_ADDR",

	"BCOP_LOAD_BYTE_IMM_ADDR",
	"BCOP_LOAD_UBYTE_IMM_ADDR",
	"BCOP_STORE_BYTE_IMM_ADDR",

	"BCOP_LOAD_HALF_IMM_ADDR",
	"BCOP_LOAD_UHALF_IMM_ADDR",
	"BCOP_STORE_HALF_IMM_ADDR",

	"BCOP_LOAD_FLOAT_IMM_ADDR",
	"BCOP_STORE_FLOAT_IMM_ADDR",

	"BCOP_LOAD_DOUBLE_IMM_ADDR",
	"BCOP_STORE_DOUBLE_IMM_ADDR",

	"BCOP_CALL",
	"BCOP_RET",

	"BCOP_INTERRUPT",
	"BCOP_HALT",
};

enum Scope {
	SCOPE_GLOBAL = 0,
	SCOPE_ARG,
	SCOPE_LOCAL,
};

char *scope_debug[] = {
	"SCOPE_GLOBAL",
	"SCOPE_ARG",
	"SCOPE_LOCAL",
};

/* typedefs */
typedef struct Debug_info Debug_info;
typedef struct Lexer Lexer;
typedef enum AST_kind AST_kind;
typedef enum AST_op AST_op;
typedef struct AST_node AST_node;
typedef struct AST AST;
typedef enum Type_tag Type_tag;
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
typedef enum Scope Scope;
typedef enum Sym_kind Sym_kind;
typedef struct Sym Sym;
typedef struct Sym_index_map Sym_index_map;
typedef struct Sym_tab Sym_tab;
typedef enum BCseg BCseg;
typedef enum BCop BCop;
typedef struct BCptr BCptr;
typedef uint64_t BCword;
typedef uint32_t BChalf;
typedef uint8_t BCbyte;
typedef float BCfloat;
typedef union BCreg BCreg;
typedef double BCdouble;
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
	AST_kind kind;
	bool visited;
	union {
		char *str;
		AST_op op;
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

struct Sym_index_map {
	char *key;
	uint64_t value;
};

struct Sym {
	char *name;
	bool constant;
	Scope scope;
	size_t pos;
	Type_info *type;
	union {
		Type_info *tinfo;
		AST_node *ast;
		uint64_t label : 64 - 3;
	} val;
	Debug_info debug_info;
};

struct Sym_tab {
	char *name;
	Sym_index_map *hashmap;
	Scope scope;
	struct {
		size_t text;
		size_t data;
		size_t bss;
		size_t stack;
	} count;
};

struct BCptr {
	BCseg seg : 4;
	uint64_t offset : 60;
};

union BCreg {
	BCword w;
	BCfloat f;
	BCdouble d;
	BCptr p;
};

struct BCinst {
	BCop opcode : 8;
	union {
		uint64_t dest_reg; /* destination register */
		uint64_t ptr_reg; /* pointer register */
		BCptr ptr; /* immediate address */
	};
	union {
		struct {
			uint64_t r1;
			uint64_t r2;
		};
		uint64_t imm_i;
		float imm_f;
		double imm_d;
	};
};

struct BCmem {
	/* the memory 6 segments are:
	 * data
	 * bss
	 * text
	 * heap
	 * arg
	 * stack
	 * 
	 * we use the first 3 bits of an address to
	 * index the segment, the rest of the bits
	 * are used for indexing within that segment
	 */
	BCreg *regs;
	BCbyte *stack_ptr;
	BCbyte *seg[6];
};

/* globals */
Fmap fm; /* source file Fmap */
Pool string_pool;
Lexer lexer;
AST ast = {0};
Array(Type_info) type_info_array = NULL;
Array(Type_member) member_array = NULL;
Array(Sym) sym_array = NULL;
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
AST_node* range(void);
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
Type_info* implicit_cast_compound(Type_info *ta, Type_info *tb);
Type_info* implicit_cast(Type_info *ta, Type_info *tb);
Type_info* typecheck(AST_node *node);
void build_compound_type(Pool *type_pool, Pool *member_pool, AST_node *node, Type_info *dest);

/* symbol table functions */
bool define_symbol(Sym_tab *tab, Sym symbol);
Sym* lookup_symbol(char *name);
void sym_print(Sym *sym);

/* bytecode interpreter */
BCinst* bc_allocinst(BCmem *mem);
int bc_interpreter(BCinst *prog, BCmem *mem);

/* compilation stage 1 functions */
void compile(AST_node *root);
void compile_declaration(Sym_tab *tab, AST_node *node);
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
		fprintf(stderr, "down: %i\n", node->down ? node->down->id : -1);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, stderr);
		fprintf(stderr, "next: %i\n\n", node->next ? node->next->id : -1);
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

#define TYPE_TAG_IS_COMPOUND(tag) ((tag >= TY_FUNC && tag <= TY_UNION) || tag == TY_ARRAY || tag == TY_POINTER)

#define TYPE_TAG_IS_SIMPLE(tag) (tag >= TY_INT && tag <= TY_VOID)

#define TYPES_ARE_COMPOUND(tag_a, tag_b) (TYPE_TAG_IS_COMPOUND(tag_a) && TYPE_TAG_IS_COMPOUND(tag_b))

#define TYPES_ARE_SIMPLE_OR_POINTER(tag_a, tag_b) \
	((tag_a != TY_POINTER || TYPE_TAG_IS_SIMPLE(tag_b)) || (tag_b != TY_POINTER || TYPE_TAG_IS_SIMPLE(tag_a)))

Type_info* implicit_cast_compound(Type_info *ta, Type_info *tb) {
	assert(ta && tb);
	assert(TYPES_ARE_COMPOUND(ta->tag, tb->tag));

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
		if(TYPES_ARE_COMPOUND(subtype_a->tag, subtype_b->tag))
			return implicit_cast_compound(subtype_a, subtype_b);
		else if(TYPES_ARE_SIMPLE_OR_POINTER(subtype_a->tag, subtype_b->tag))
			return implicit_cast(subtype_a, subtype_b);
		else
			return NULL;
	}

	while(member_a && member_b) {
		if(TYPES_ARE_COMPOUND(member_a->type->tag, member_b->type->tag))
			tinfo = implicit_cast_compound(member_a->type, member_b->type);
		else if(TYPES_ARE_SIMPLE_OR_POINTER(member_a->type->tag, member_b->type->tag))
			tinfo = implicit_cast(member_a->type, member_b->type);
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

Type_info* implicit_cast(Type_info *ta, Type_info *tb) {
	assert(ta && tb);
	assert(TYPES_ARE_SIMPLE_OR_POINTER(ta->tag, tb->tag));

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
	//Sym *symptr = NULL;
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
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
			/*
			symptr = lookup_symbol(node->str);
			if(!symptr)
				myerror("undefined identifier %s at %DBG", node->str, &node->debug_info);
			tinfo = symptr->type;
			*/
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

			if(TYPES_ARE_COMPOUND(tinfo_down->tag, tinfo_next->tag))
				merged_type = implicit_cast_compound(tinfo_down, tinfo_next);

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
			assert(member_stack_top < STATICARRLEN(member_stack));
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

		if(node->down->kind == N_EXPRESSION) {
			save = node->down;
			node = node->down;
			for(; node; node = node->next) {
				if(node->str == NULL)
					invalid = false;
				else
					tinfo_down = typecheck(node->down);

				if(TYPES_ARE_COMPOUND(tinfo_down->tag, tinfo_next->tag))
					merged_type = implicit_cast_compound(tinfo_down, tinfo_next);

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

			if(TYPES_ARE_COMPOUND(tinfo_down->tag, tinfo_next->tag))
				merged_type = implicit_cast_compound(tinfo_down, tinfo_next);

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
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
			/*
			symptr = lookup_symbol(node->str);
			if(!symptr)
				myerror("undefined identifier %s at %DBG", node->str, &node->debug_info);
			if(symptr->type->tag != TY_TYPE)
				myerror("invalid type %s at %DBG", node->str, &node->debug_info);
			tinfo_next = symptr->val.tinfo;
			*/
		} else if(cast_type_node->kind == N_FUNCTION) {
			myerror("attempted to cast %s to function type at%DBG", tstr_down, &node->debug_info);
		} else if(cast_type_node->kind >= N_STRUCTURE && cast_type_node->kind <= N_ENUMERATION) {
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
		} else {
			tinfo_next = builtin_types + cast_type_node->typesig;
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

		for(int i = 0; i < STATICARRLEN(cast_table[tinfo_from->tag]); ++i) {
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

	if(TYPES_ARE_COMPOUND(tinfo_down->tag, tinfo_next->tag)) {
		merged_type = implicit_cast_compound(tinfo_down, tinfo_next);
		if(!merged_type)
			myerror("invalid operands %s and %s to %s at%DBG", tstr_down, tstr_next, opstr, &node->debug_info);
		types_are_recursive = true;
		tinfo = merged_type;
	} else if(TYPES_ARE_SIMPLE_OR_POINTER(tinfo_down->tag, tinfo_next->tag)) {
		merged_type = implicit_cast(tinfo_down, tinfo_next);
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

bool define_symbol(Sym_tab *tab, Sym symbol) {
	uint64_t i;
	i = shget(tab->hashmap, symbol.name);
	if(i == SYM_TAB_DEFAULT)
		return false;
	arrput(sym_array, symbol);
	i = arrlen(sym_array) - 1;
	shput(tab->hashmap, symbol.name, i);
	return true;
}

Sym* lookup_symbol(char *name) {
	Sym_tab *tab = NULL;
	Sym *sp = NULL;
	uint64_t index;
	for(int i = scope_depth; i >= 0; --i) {
		tab = tabstack + i;
		index = shget(tab->hashmap, name);
		if(index != SYM_TAB_DEFAULT) {
			sp = sym_array + index;
			break;
		}
	}

	return sp;
}

void sym_print(Sym *sym) {
	fprintf(stderr, "SYM\n*   name: %s\n*   constant: %i\n*   scop: %s\n*   pos: %zu\n*   type:\n",
			sym->name, sym->constant, scope_debug[sym->scope], sym->pos);
	type_info_print(sym->type, 2);
	fprintf(stderr, "*   val:\n");
	if(sym->type->tag == TY_TYPE)
		type_info_print(sym->val.tinfo, 2);
	else
		ast_print(sym->val.ast, 2, true);
}

int bc_interpreter(BCinst *prog, BCmem *mem) {
	BCinst *curinst;
	BCptr *ptr_reg;
	BCreg *dest, *r1, *r2;
	BCword *wptr;
	BChalf *hptr;
	BCbyte *bptr;
	BCfloat *fptr;
	BCdouble *dptr;
	BCptr addr;
	BCseg seg;
	uint64_t offset;
	bool sign;
	uint64_t imm_i;
	float imm_f;
	double imm_d;

	for(curinst = prog; ; ++curinst) {
		ptr_reg = &mem->regs[curinst->ptr_reg].p;
		dest = mem->regs + curinst->dest_reg;
		r1 = mem->regs + curinst->r1;
		r2 = mem->regs + curinst->r2;
		imm_i = curinst->imm_i;
		imm_f = curinst->imm_f;
		imm_d = curinst->imm_d;

		if(curinst->opcode >= BCOP_LOAD_WORD_IMM_ADDR && curinst->opcode <= BCOP_STORE_DOUBLE_IMM_ADDR)
			addr = curinst->ptr;
		else if(curinst->opcode >= BCOP_JMP_IMM_ADDR && curinst->opcode <= BCOP_BGE_DOUBLE_IMM_ADDR)
			addr = curinst->ptr;
		else
			addr = *ptr_reg;

		seg = addr.seg;
		offset = addr.offset;

		switch(curinst->opcode) {
		case BCOP_SET:
			dest->w = r1->w;
			break;
		case BCOP_SET_IMM:
			dest->w = imm_i;
			break;
		case BCOP_ADD:
			dest->w = r1->w + r2->w;
			break;
		case BCOP_SUB:
			dest->w = r1->w - r2->w;
			break;
		case BCOP_MUL:
			dest->w = r1->w * r2->w;
			break;
		case BCOP_DIV:
			dest->w = r1->w / r2->w;
			break;
		case BCOP_MOD:
			dest->w = r1->w % r2->w;
			break;
		case BCOP_AND:
			dest->w = r1->w & r2->w;
			break;
		case BCOP_OR:
			dest->w = r1->w | r2->w;
			break;
		case BCOP_XOR:
			dest->w = r1->w ^ r2->w;
			break;
		case BCOP_NOT:
			dest->w = ~r1->w;
			break;
		case BCOP_LSHIFT:
			dest->w = r1->w << r2->w;
			break;
		case BCOP_RSHIFT:
			dest->w = r1->w >> r2->w;
			break;

		case BCOP_SET_FLOAT:
			dest->f = r1->f;
			break;
		case BCOP_SET_FLOAT_IMM:
			dest->f = imm_f;
			break;
		case BCOP_ADD_FLOAT:
			dest->f = r1->f + r2->f;
			break;
		case BCOP_SUB_FLOAT:
			dest->f = r1->f - r2->f;
			break;
		case BCOP_MUL_FLOAT:
			dest->f = r1->f * r2->f;
			break;
		case BCOP_DIV_FLOAT:
			dest->f = r1->f / r2->f;
			break;

		case BCOP_SET_DOUBLE:
			dest->d = r1->d;
			break;
		case BCOP_SET_DOUBLE_IMM:
			dest->d = imm_d;
			break;
		case BCOP_ADD_DOUBLE:
			dest->d = r1->d + r2->d;
			break;
		case BCOP_SUB_DOUBLE:
			dest->d = r1->d - r2->d;
			break;
		case BCOP_MUL_DOUBLE:
			dest->d = r1->d * r2->d;
			break;
		case BCOP_DIV_DOUBLE:
			dest->d = r1->d / r2->d;
			break;

		case BCOP_JMP:
		case BCOP_JMP_IMM_ADDR:
			assert(seg == SEG_TEXT);
			curinst = prog + *ptr_reg;
			break;
		case BCOP_BLT:
		case BCOP_BLT_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if((int64_t)r1->w < (int64_t)r2->w)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BGE:
		case BCOP_BGE_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if((int64_t)r1->w >= (int64_t)r2->w)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BLTU:
		case BCOP_BLTU_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->w < r2->w)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BGEU:
		case BCOP_BGEU_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->w >= r2->w)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BLT_FLOAT:
		case BCOP_BLT_FLOAT_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->f < r2->f)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BGE_FLOAT:
		case BCOP_BGE_FLOAT_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->f >= r2->f)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BLT_DOUBLE:
		case BCOP_BLT_DOUBLE_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->d < r2->d)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BGE_DOUBLE:
		case BCOP_BGE_DOUBLE_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->d >= r2->d)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BNE:
		case BCOP_BNE_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->w != r2->w)
				curinst = prog + *ptr_reg;
			break;
		case BCOP_BEQ:
		case BCOP_BEQ_IMM_ADDR:
			assert(seg == SEG_TEXT);
			if(r1->w == r2->w)
				curinst = prog + *ptr_reg;
			break;

		case BCOP_LOAD_WORD:
		case BCOP_LOAD_WORD_IMM_ADDR:
			wptr = (BCword*)(mem->seg[seg] + addr);
			r1->w = *wptr;
			break;
		case BCOP_STORE_WORD:
		case BCOP_STORE_WORD_IMM_ADDR:
			wptr = (BCword*)(mem->seg[seg] + addr);
			*wptr = r1->w;
			break;
		case BCOP_PUSH_WORD:
			wptr = (BCword*)mem->stack_ptr;
			*wptr = r1->w;
			mem->stack_ptr += sizeof(BCword);
			break;
		case BCOP_POP_WORD:
			mem->stack_ptr -= sizeof(BCword);
			wptr = (BCword*)mem->stack_ptr;
			r1->w = *wptr;
			break;

		case BCOP_LOAD_BYTE:
		case BCOP_LOAD_BYTE_IMM_ADDR:
			bptr = mem->seg[seg] + addr;
			sign = *bptr >> (sizeof(BCbyte)-1);
			r1->w = -((sign << sizeof(BCbyte)) - *(BCword*)bptr);
			break;
		case BCOP_LOAD_UBYTE:
		case BCOP_LOAD_UBYTE_IMM_ADDR:
			bptr = mem->seg[seg] + addr;
			r1->w = *(BCword*)bptr;
			break;
		case BCOP_STORE_BYTE:
		case BCOP_STORE_BYTE_IMM_ADDR:
			bptr = mem->seg[seg] + addr;
			*bptr = (BCbyte)r1->w;
			break;
		case BCOP_PUSH_BYTE:
			bptr = mem->stack_ptr;
			*bptr = (BCbyte)r1->w;
			mem->stack_ptr += sizeof(BCbyte);
			break;
		case BCOP_POP_BYTE:
			mem->stack_ptr -= sizeof(BCbyte);
			bptr = mem->stack_ptr;
			sign = *bptr >> (sizeof(BCbyte)-1);
			r1->w = -((sign << sizeof(BCbyte)) - *(BCword*)bptr);
			break;
		case BCOP_POP_UBYTE:
			mem->stack_ptr -= sizeof(BCbyte);
			bptr = mem->stack_ptr;
			r1->w = *(BCword*)bptr;
			break;

		case BCOP_LOAD_HALF:
		case BCOP_LOAD_HALF_IMM_ADDR:
			hptr = (BChalf*)(mem->seg[seg] + addr);
			sign = *hptr >> (sizeof(BChalf)-1);
			r1->w = -((sign << sizeof(BChalf)) - *(BCword*)hptr);
			break;
		case BCOP_LOAD_UHALF:
		case BCOP_LOAD_UHALF_IMM_ADDR:
			hptr = (BChalf*)(mem->seg[seg] + addr);
			r1->w = *(BCword*)hptr;
			break;
		case BCOP_STORE_HALF:
		case BCOP_STORE_HALF_IMM_ADDR:
			hptr = (BChalf*)(mem->seg[seg] + addr);
			*hptr = (BChalf)r1->w;
			break;
		case BCOP_PUSH_HALF:
			hptr = (BChalf*)mem->stack_ptr;
			*hptr = (BChalf)r1->w;
			mem->stack_ptr += sizeof(BChalf);
			break;
		case BCOP_POP_HALF:
			mem->stack_ptr -= sizeof(BChalf);
			hptr = (BChalf*)mem->stack_ptr;
			sign = *hptr >> (sizeof(BChalf)-1);
			r1->w = -((sign << sizeof(BChalf)) - *(BCword*)hptr);
			break;
		case BCOP_POP_UHALF:
			mem->stack_ptr -= sizeof(BChalf);
			hptr = (BChalf*)mem->stack_ptr;
			r1->w = *(BCword*)hptr;
			break;

		case BCOP_LOAD_FLOAT:
		case BCOP_LOAD_FLOAT_IMM_ADDR:
			fptr = (BCfloat*)(mem->seg[seg] + addr);
			r1->f = *fptr;
			break;
		case BCOP_STORE_FLOAT:
		case BCOP_STORE_FLOAT_IMM_ADDR:
			fptr = (BCfloat*)(mem->seg[seg] + addr);
			*fptr = r1->f;
			break;
		case BCOP_PUSH_FLOAT:
			fptr = (BCfloat*)mem->stack_ptr;
			*fptr = r1->f;
			mem->stack_ptr += sizeof(BCfloat);
			break;
		case BCOP_POP_FLOAT:
			mem->stack_ptr -= sizeof(BCfloat);
			fptr = (BCfloat*)mem->stack_ptr;
			r1->f = *fptr;
			break;

		case BCOP_LOAD_DOUBLE:
		case BCOP_LOAD_DOUBLE_IMM_ADDR:
			dptr = (BCdouble*)(mem->seg[seg] + addr);
			r1->d = *dptr;
			break;
		case BCOP_STORE_DOUBLE:
		case BCOP_STORE_DOUBLE_IMM_ADDR:
			dptr = (BCdouble*)(mem->seg[seg] + addr);
			*dptr = r1->d;
			break;
		case BCOP_PUSH_DOUBLE:
			dptr = (BCdouble*)mem->stack_ptr;
			*dptr = r1->d;
			mem->stack_ptr += sizeof(BCdouble);
			break;
		case BCOP_POP_DOUBLE:
			mem->stack_ptr -= sizeof(BCdouble);
			dptr = (BCdouble*)mem->stack_ptr;
			r1->d = *dptr;
			break;

		case BCOP_CALL:
			assert(seg == SEG_TEXT);
			wptr = (BCword*)mem->stack_ptr;
			*wptr = ((curinst - prog) << 0x3 | (SEG_TEXT & 0x7));
			mem->stack_ptr += sizeof(BCword);
			break;
		case BCOP_RET:
			mem->stack_ptr -= sizeof(BCword);
			wptr = (BCword*)mem->stack_ptr;
			curinst = prog + (*wptr >> 0x3);
			break;

		case BCOP_INTERRUPT:
			myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__,__func__);
			break;
		case BCOP_HALT:
			return 0;
		}

		assert(mem->stack_ptr >= mem->seg[SEG_STACK] && mem->stack_ptr < (mem->seg[SEG_STACK] + mem->stack_size));
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

	if(tp[0] == '.' && tp[1] == '.') {
		lexer.debug_info.col += 2;
		lexer.ptr += 2;
		return (lexer.token = T_TWODOT);
	}

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
	for(int i = 0; i < STATICARRLEN(keyword); ++i) {
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
		if(*s == '.' && s[1] != '.') {
			++s;
			for(check = 0; *s && isdigit(*s); ++s)
				++check;
			if(check) {
				lexer.debug_info.col += s - tp;
				lexer.text_s = tp;
				lexer.text_e = lexer.ptr = s;
				return (lexer.token = T_FLOATING);
			} else {
				check = 0;
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
	AST_node *root, *child, *id_node, *bind_node, *type_node, *assign_node;
	char *unget1, *unget2;

	id_node = type_node = assign_node = NULL;

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

	root = ast_alloc(&ast, N_DECL, &lexer.debug_info);
	id_node = child = ast_alloc(&ast, N_ID, &lexer.debug_info);
	child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);

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

	type_node = typename();

	t = lex();

	bind_node = ast_alloc(&ast, N_OP, &lexer.debug_info);
	bind_node->op = OP_BINDTYPE;
	bind_node->down = id_node;
	bind_node->next = type_node;

	root->down = assign_node = ast_alloc(&ast, N_OP, &lexer.debug_info);
	assign_node->op = OP_ASSIGN;
	assign_node->down = bind_node;

	if(type_node && t == ';')
		return root;
	else if(!type_node && t != ':' && t != '=')
		parse_error("type");

	if(t == ':')
		assign_node->op = OP_ASSIGNCONST;
	else if(t != '=')
		parse_error("'=' or ':'");

	/* body */
	if((assign_node->next = function())) return root;
	if((assign_node->next = structure())) return root;
	if((assign_node->next = unionation())) return root;
	if((assign_node->next = enumeration())) return root;

	assign_node->next = expression();

	if(!assign_node->next)
		parse_error("initializer");

	t = lex();
	if(t != ';')
		parse_error("';'");

	return root;
}

AST_node* vardec(void) {
	register int t;
	AST_node *root, *child, *id_node, *bind_node, *type_node, *assign_node;
	char *unget1, *unget2;

	id_node = type_node = assign_node = NULL;

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

	root = ast_alloc(&ast, N_DECL, &lexer.debug_info);
	id_node = child = ast_alloc(&ast, N_ID, &lexer.debug_info);
	child->str = pool_alloc_string(&string_pool, lexer.text_e - lexer.text_s, lexer.text_s);

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

	type_node = typename();

	t = lex();

	bind_node = ast_alloc(&ast, N_OP, &lexer.debug_info);
	bind_node->op = OP_BINDTYPE;
	bind_node->down = id_node;
	bind_node->next = type_node;

	root->down = assign_node = ast_alloc(&ast, N_OP, &lexer.debug_info);
	assign_node->op = OP_ASSIGN;
	assign_node->down = bind_node;

	if(type_node && (t == ';' || t == ',' || t == ')')) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return root;
	} else if(!type_node && t != '=')
		parse_error("type");

	if(t != '=')
		parse_error("'='");

	assign_node->next = expression();

	if(!assign_node->next)
		parse_error("initializer");

	t = lex();
	if(t != ';' && t != ',' && t != ')')
		parse_error("';'");

	lexer.debug_info.col -= lexer.ptr - lexer.unget;
	lexer.ptr = lexer.unget;

	return root;
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
	case T_ID:
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

	if(head && tail) {
		root = head;
		child = tail;
		child->down = node;
		child = child->down;
	} else {
		root = node;
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
	char *text_s, *text_e;
	char *unget;

	t = lex();
	if(t != T_ID) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return NULL;
	}
	unget = lexer.unget;
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
 * forstatement: 'for' ' ' (label)? expression '{' block '}'
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

	root = ast_alloc(&ast, N_FORSTATEMENT, &lexer.debug_info);


	child = label();

	if(child) {
		root->down = child;
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child = child->next;
	} else {
		child = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		root->down = child;
	}
	child->down = expression();
	if(!child->down)
		parse_error("expression");

	t = lex();
	if(t != '{')
		parse_error("'{'");

	if(child)
		child->next = block();
	else
		child = block();

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
		root->down = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		root->down->down = child;
		child = root->down;
	}

	while(t == ',') {
		child->down = expression();
		child->next = ast_alloc(&ast, N_EXPRESSION, &lexer.debug_info);
		child = child->next;
		t = lex();
	}

	if(t != '=' && root->down->kind == N_EXPRESSION)
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
	return range();
}

AST_node* range(void) {
	int t;
	AST_node *root, *child;

	child = logical();

	if(!child)
		return NULL;

	t = lex();
	if(t != T_TWODOT) {
		lexer.debug_info.col -= lexer.ptr - lexer.unget;
		lexer.ptr = lexer.unget;
		return child;
	}

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->op = OP_RANGE;
	root->down = child;
	root->next = expression();
	if(!root->next)
		parse_error("expression");

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

	root = ast_alloc(&ast, N_OP, &lexer.debug_info);
	root->op = (t == T_AND) ? OP_LAND : OP_LOR;
	root->down = child;
	root->next = logical();

	if(!root->next)
		parse_error("next operand");

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
		parse_error("next operand");

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
		parse_error("next operand");

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
		parse_error("next operand");

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
		parse_error("next operand");

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
		parse_error("next operand");

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

/*
void compile(AST_node *root) {
	AST_node *node;
	//TODO process directives (import, load, etc)

	declare_constants(&tabstack[0], root);

	for(node = root; node; node = node->next) {
		if(node->kind == N_DECL)
			declare_symbol(&tabstack[0], node);
		if(node->kind != N_FUNCTION)
			continue;
		++scope_depth;
		compile_function(node);
		--scope_depth;
	}
}

void compile_declaration(Sym_tab *tab, AST_node *node) {
	AST_node *assign_node, *bind_node, *type_node, *id_node, *initial_node;
	Type_info *tinfo, *tinfo_init;
	Sym sym;
	size_t *seg_count;

	assert(node->kind == N_DECL || (node->kind >= N_STRUCTURE && node->kind <= N_ENUMERATION));

	if(node->kind == N_ENUMERATION) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	} else if(node->kind == N_STRUCTURE) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	} else if(node->kind == N_UNIONATION) {
		myerror("feature unimplemented on compiler source line: %i in function %s\n", __LINE__, __func__);
	}

	sym = (Sym){0};

	assign_node = node->down;
	bind_node = assign_node->down;
	initial_node = assign_node->next;
	id_node = bind_node->down;
	type_node = bind_node->next;

	switch(tab->scope) {
	case SCOPE_GLOBAL:
		if(initial_node && initial_node->kind == N_UNINIT) {
			sym.seg = SEG_BSS;
			seg_count = &(tab->count.bss);
		} else {
			sym.seg = SEG_DATA;
			seg_count = &(tab->count.data);
		}
		break;
	case SCOPE_LOCAL:
		sym.seg = SEG_STACK;
		seg_count = &(tab->count.stack);
		break;
	default:
		assert(0);
	}

	for(; id_node; id_node = id_node->next) {
		sym.name = id_node->str;
		sym.debug_info = id_node->debug_info;
		sym.pos = *seg_count;
		*seg_count += sym.type->bytes;
		if(!define_symbol(tab, sym)) {
			myerror("redefinition of '%s' at%DBG", id_node->str, &id_node->debug_info);
		}
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
		if(node->kind == N_DECL)
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
*/

void cleanup(void) {
	pool_free(&ast.pool);
	pool_free(&string_pool);
	for(int i = 0; i < STATICARRLEN(tabstack); ++i)
		if(tabstack[i].hashmap)
			hmfree(tabstack[i].hashmap);
	arrfree(sym_array);
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

	for(int i = 0; i < STATICARRLEN(tabstack); ++i)
		SYM_TAB_INIT(tabstack[i]);
	lexer_init(&lexer, fm.buf);
	pool_init(&ast.pool, sizeof(AST_node), 256, 4);
	pool_init(&string_pool, sizeof(char), 4096, 4);

	parse();
	ast_print(ast.root, 0, false);
	//exit(0);

	tabstack[0].name = argv[1];
	fprintf(stderr,"################ TESTING COMPILE FUNCTIONS #####################\n");
	//compile(ast.root);
	//ast_print(ast.root, 0, false);
	return 0;
}
