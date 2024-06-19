#include <ctype.h>
#include "basic.h"
#include <string.h>


#define TOKENS                               \
    X(IDENT,                              "")\
    X(INTLIT,                             "")\
    X(HEXLIT,                             "")\
    X(BINLIT,                             "")\
    X(UINTLIT,                            "")\
    X(FLOATLIT,                           "")\
    X(DFLOATLIT,                          "")\
    X(STRINGLIT,                          "")\
    X(KEYWORD,                            "")\
    X(IMPORT_DIRECTIVE,            "#import")\
    X(LOAD_DIRECTIVE,                "#load")\
    X(RUN_DIRECTIVE,                  "#run")\
    X(IF_DIRECTIVE,                    "#if")\
    X(FOREIGN_DIRECTIVE,          "#foreign")\
    X(FOREIGN_LIB_DIRECTIVE,  "#foreign_lib")\
    X(C_CALL_DIRECTIVE,            "#c_call")\
    X(INLINE_DIRECTIVE,            "#inline")\
    X(PROC,                           "proc")\
    X(FUNC,                           "func")\
    X(MACRO,                         "macro")\
    X(STRUCT,                       "struct")\
    X(ENUM,                           "enum")\
    X(UNION,                         "union")\
    X(USING,                         "using")\
    X(DEFER,                         "defer")\
    X(IF,                               "if")\
    X(ELSE,                           "else")\
    X(SWITCH,                       "switch")\
    X(CASE,                           "case")\
    X(FOR,                             "for")\
    X(WHILE,                         "while")\
    X(CONTINUE,                   "continue")\
    X(BREAK,                         "break")\
    X(RETURN,                       "return")\
    X(TRUE,                           "true")\
    X(FALSE,                         "false")\
    X(VOID,                           "void")\
    X(BOOL,                           "bool")\
    X(CHAR,                           "char")\
    X(S8,                               "s8")\
    X(U8,                               "u8")\
    X(S16,                             "s16")\
    X(U16,                             "u16")\
    X(S32,                             "s32")\
    X(U32,                             "u32")\
    X(S64,                             "s64")\
    X(U64,                             "u64")\
    X(INT,                             "int")\
    X(FLOAT,                         "float")\
    X(F32,                             "f32")\
    X(F64,                             "f64")\
    X(CAST,                           "cast")\
    X(LESSEQUAL,                        "<=")\
    X(GREATEQUAL,                       ">=")\
    X(EXCLAMEQUAL,                      "!=")\
    X(EQUALEQUAL,                       "==")\
    X(AND,                              "&&")\
    X(OR,                               "||")\
    X(LONGDASH,                        "---")\
    X(TWODOT,                           "..")\
    X(MINUSMINUS,                       "--")\
    X(PLUSPLUS,                         "++")\
    X(PLUSEQUAL,                        "+=")\
    X(MINUSEQUAL,                       "-=")\
    X(TIMESEQUAL,                       "*=")\
    X(DIVEQUAL,                         "/=")\
    X(MODEQUAL,                         "%=")\
    X(ANDEQUAL,                         "&=")\
    X(OREQUAL,                          "|=")\
    X(LSHIFTEQUAL,                     "<<=")\
    X(RSHIFTEQUAL,                     ">>=")\
    X(XOREQUAL,                         "^=")\
    X(LSHIFT,                           "<<")\
    X(RSHIFT,                           ">>")

#define TOKEN_IS_TYPE_KEYWORD(t) (t >= TOKEN_VOID && t <= TOKEN_F64)

#define TOKEN_TO_KEYWORD(t) token_keywords[t-TOKEN_INVALID-1]

#define KEYWORD_TO_TOKEN(k) (Token)(k+TOKEN_INVALID+1)

#define TOKEN_DEBUG(t) token_debug[t-TOKEN_INVALID-1]

#define IS_KEYWORD_CHAR(c) (isalnum(c) || c == '_')


typedef enum Token {
    TOKEN_INVALID = 256,
#define X(T,S) TOKEN_##T,
    TOKENS
#undef X
} Token;


typedef struct Loc_info {
    char *src_path;
    int line;
    int col;
    struct {
        char *s, *e;
    } text;
} Loc_info;

typedef struct Lexer {
	Token token;
    char *src_path;
	char *src;
	char *pos;
    union {
        s64 integer;
        u64 uinteger;
        f32 floating;
        f64 dfloating;
        char character;
        struct {
            char *s, *e;
        } text;
    };
    Loc_info loc;
	Loc_info loc_next;
} Lexer;


char *token_debug[] = {
#define X(T,S) #T,
    TOKENS
#undef X
};

char *token_keywords[] = {
#define X(T,S) S,
    TOKENS
#undef X
};

u64 token_keyword_lengths[] = {
#define X(T,S) STRLEN(S),
    TOKENS
#undef X
};

char token_chars[] = "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~";


INLINE void lexer_init(Lexer *l, char *src, char *src_path) {
    *l = (Lexer){0};
    l->token = TOKEN_INVALID;
    l->src_path = src_path;
    l->src = src;
    l->pos = l->src;
    l->loc.src_path = src_path;
    l->loc.line = l->loc.col = 1;
    l->loc.text.s = l->pos;
    l->loc.text.e = l->pos;

    while(*l->loc.text.e != '\n') ++l->loc.text.e;

    l->loc_next = l->loc;
}

INLINE Token get_literal(char *s, int n) {
    int i = 0;

    if(s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        for(i = 2; i < n; ++i) {
            if(!isxdigit(s[i]))
                return TOKEN_INVALID;
        }
        return TOKEN_HEXLIT;
    } else if(s[0] == '0' && (s[1] == 'b' || s[1] == 'B')) {
        for(i = 2; i < n; ++i) {
            if (s[i] != '0' && s[i] != '1')
                return TOKEN_INVALID;
        }
        return TOKEN_BINLIT;
    } else if(isdigit(s[0])) {
        i = 0;

        bool has_dot = false;
        bool has_exp = false;

        for(; i < n; i++) {
            if(isdigit(s[i])) {
                continue;
            } else if(s[i] == '.' && !has_dot) {
                has_dot = true;
            } else if((s[i] == 'e' || s[i] == 'E') && !has_exp && i > 0 && isdigit(s[i - 1])) {
                if(s[i+1] == '-' || s[i+1] == '+')
                    ++i;
                has_exp = true;
            } else {
                return TOKEN_INVALID;
            }
        }

        if(has_dot || has_exp)
            return TOKEN_FLOATLIT;
        else
            return TOKEN_INTLIT;
    } else {
        return TOKEN_INVALID;
    }
}

INLINE float strn_to_float(char *s, int n) {
    float result = 0.0f;
    float multiplier = 0.1f;
    int i = 0;

    for(; i < n && isdigit(s[i]); ++i) {
        result *= 10.0f;
        result += s[i] - '0';
    }

    if(s[i] == '.') {
        for(++i; i < n && isdigit(s[i]); ++i) {
            result += (s[i] - '0') * multiplier;
            multiplier *= 0.1f;
        }
    }

    if(s[i] == 'e' || s[i] == 'E') {
        int exponent = 0;
        int exp_sign = 1;

        ++i;

        if(s[i] == '-') {
            exp_sign = -1;
            ++i;
        } else if(s[i] == '+') {
            ++i;
        }

        for(; i < n && isdigit(s[i]); ++i) {
            exponent *= 10.0f;
            exponent += s[i] - '0';
        }

        if(exp_sign == -1) {
            while(exponent--) result *= 0.1f;
        } else {
            while(exponent--) result *= 10.0f;
        }
    }

    return result;
}

INLINE s64 strn_to_int(char *s, int n) {
    s64 result = 0;

    for(int i = 0; i < n; ++i) {
        result *= 10;
        result += s[i] - '0';
    }

    return result;
}

INLINE u64 strn_to_hex(char *s, int n) {
    u64 result = 0;

    if(s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        n -= 2;
    }

    for(int i = 0; i < n; ++i) {
        result <<= 4;
        char a = isupper(s[i]) ? 'A' : 'a';
        result += isdigit(s[i]) ? (s[i] - '0') : (s[i] - a  + 10);
    }

    return result;
}

INLINE u64 strn_to_bin(char *s, int n) {
    u64 result = 0;

    if(s[0] == '0' && (s[1] == 'b' || s[1] == 'B')) {
        s += 2;
        n -= 2;
    }

    for(int i = 0; i < n; ++i) {
        result <<= 1;
        result |= s[i] - '0';
    }

    return result;
}

INLINE Token lex(Lexer *l) {
    if(l->token == 0) {
        return 0;
    }

	char *tp = NULL, *s = NULL;

	l->text.s = l->text.e = NULL;

    bool found_newline = false;
	while(true) {
		while(isspace(*l->pos)) { /* whitespace */
			if(*l->pos != '\n') {
				++l->loc_next.col;
			} else {
				l->loc_next.col = 1;
				++l->loc_next.line;
                found_newline = true;
			}

			++l->pos;
		}

		/* single-line comments */
		if((l->pos[0] == '/' && l->pos[1] == '/')) {
			while(*l->pos != '\n')
				++l->pos;
            ++l->pos;
			l->loc_next.col = 1;
            ++l->loc_next.line;
            found_newline = true;
		} else if(l->pos[0] == '/' && l->pos[1] == '*') { /* multi-line comments */
			while(!(l->pos[0] == '*' && l->pos[1] == '/')) {
                if(*l->pos != '\n') {
                    ++l->loc_next.col;
                } else {
                    l->loc_next.col = 1;
                    ++l->loc_next.line;
                    found_newline = true;
                }

                ++l->pos;
            }
			l->pos += 2;
		} else {
			break;
        }
	}

    if(*l->pos == 0) {
        return 0;
    }

    if(found_newline) {
        l->loc_next.text.e = l->loc_next.text.s = l->pos;
        while(*l->loc_next.text.e != '\n') ++l->loc_next.text.e;
    }

    l->loc = l->loc_next;

	tp = l->text.s = l->pos;

    l->token = TOKEN_INVALID;

    /* keywords */
    for(int i = TOKEN_KEYWORD + 1 - TOKEN_INVALID; i < STATICARRLEN(token_keywords); ++i) {
        int keyword_length = token_keyword_lengths[i];
        if(strstr(tp, token_keywords[i]) == tp && !IS_KEYWORD_CHAR(tp[keyword_length])) {
            l->text.e = tp + keyword_length;
            l->pos += keyword_length;
            l->loc_next.col += keyword_length;
            return (l->token = KEYWORD_TO_TOKEN(i));
        }
    }

    /* literals */
    if(*tp == '\'') {
        if(tp[2] != '\'') {
            // TODO lexer error
            return (l->token = TOKEN_INVALID);
        }
    } else if(*tp == '"') {
        for(s = tp + 1; *s != '\n' && *s != 0 && *s != '"'; ++s)
            if(*s == '\\') ++s;
        if(*s != '"') {
            // TODO lexer error
            return (l->token = TOKEN_INVALID);
        }
        l->text.e = s;
        l->text.s = tp + 1;
        l->pos = s + 1;
        l->loc_next.col += l->pos - tp;
        return (l->token = TOKEN_STRINGLIT);
    } else {
        /* find possible end of number */
        s = tp;
        if(*s == '-' || *s == '+') ++s;
        for(;isxdigit(*s)||*s=='.'||*s=='x'||*s=='X'||*s=='b'||*s=='B'||*s=='e'||*s=='E'||*s=='-'||*s=='+'; ++s);
        int n = s - tp;

        l->token = get_literal(tp, n);

        switch(l->token) {
            case TOKEN_INVALID:
                break;
            default:
                printf("%s unimplemented\n", TOKEN_DEBUG(l->token));
                assert(0);
                break;
            case TOKEN_INTLIT:
                l->integer = strn_to_int(tp, n);
                break;
            case TOKEN_HEXLIT:
                l->uinteger = strn_to_hex(tp, n);
                break;
            case TOKEN_BINLIT:
                l->uinteger = strn_to_bin(tp, n);
                break;
            case TOKEN_FLOATLIT:
                l->floating = strn_to_float(tp, n);
                break;
        }

        if(l->token != TOKEN_INVALID) {
            l->pos += n;
            l->loc_next.col += n;
            return l->token;
        }
    }

    /* chars */
    for(int i = 0; i < STRLEN(token_chars); ++i)
        if(!(*l->pos ^ token_chars[i])) {
            l->token = *l->pos;
            ++l->pos;
            ++l->loc_next.col;
            return l->token;
        }

    /* identifier */
    if(isalpha(*tp) || *tp == '_') {
        for(s = tp; isalnum(*s) || *s == '_'; ++s);
        l->text.s = tp;
        l->text.e = s;
        l->pos = s;
        l->loc_next.col += s - tp;
        return (l->token = TOKEN_IDENT);
    }

    return (l->token = TOKEN_INVALID);
}
