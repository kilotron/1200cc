#pragma once
#include <stdlib.h>
#include <stdarg.h>
#include "util.h"

#define PRINT_TO_FILE 0x1
#define PRINT_TO_CONSOLE 0x2

#define PRINT_IN_REGS 0x1
#define PRINT_OUT_REGS 0x2
#define PRINT_USE 0x4
#define PRINT_DEF 0x8


// Tokens

typedef struct {
	int type;		// type can have the value TK_ID, TK_NUM, etc.
	int value;		// Number literal or char literal
	char *lexeme;	// The name of an identifier or a keyword, string literal, etc.
	char *p;		// pointer to beginning of this token in source file
	int len;
} Token;

enum {
	TK_BAD = 0,
	TK_LP = 40,		// Left parenthesis (
	TK_RP,			// Right parenthesis )
	TK_TIMES,		// *
	TK_PLUS,		// +
	TK_COMMA,		// ,
	TK_MINUS,		// -
	TK_DIV = 47,	// /
	TK_SC = 59,		// Semicolon ;
	TK_LS,			// <
	TK_ASSIGN,		// =
	TK_GT,			// >
	TK_LBRKT = 91,	// Left bracket [
	TK_RBRKT = 93,	// Right bracket ]
	TK_LBRACE = 123,// Left brace {
	TK_RBRACE = 125,// Right brace }

	TK_ID = 256,	// Identifier
	TK_CONST,		// Keyword 'const'
	TK_INT,
	TK_CHAR,
	TK_VOID,
	TK_MAIN,
	TK_IF,
	TK_ELSE,
	TK_WHILE,
	TK_FOR,
	TK_RETURN,
	TK_SCANF,
	TK_PRINTF,

	TK_NUML,		// Number literal
	TK_CHARL,		// Char literal
	TK_STRL,		// String literal

	TK_EQ,			// ==
	TK_NE,			// !=
	TK_GE,			// >=
	TK_LE,			// <=
	TK_EOF
};

void lexer_init(char *path);
Token *next_token();
Vector * tokenize(char * path);
void lexer_demo(char *path);
void parser_demo(Token *s, Token *e, char *fmt, ...);

#define WORD_SIZE 4
// Type is used to describe the type of a Symbol, extra info of a Type.
typedef struct Type Type;
typedef struct Type{
	int type;		// type can have value TYPE_INT, TYPE_ARRAY, etc.
	char *name;		// for int or char only.
	int size;		// size of this type.

	// if type == TYPE_ARRAY, array_of is Type of the element of this array.
	// len is the number of elements in this array.
	Type *array_of;	
	int len;
	
	/* if type == TYPE_FUNC, ret is the return type of this function.
	 * If the function has no parameters, params_list is NULL 
	 * else params_list is a Vector of Node* of ID, params_list->len is the number
	 * of parameters.
	 * 
	 * If the function has no return value, ret is NULL, else ret is the 
	 * return type. */
	Vector *params_list;
	Type *ret;
} Type;

enum {
	TYPE_INT,
	TYPE_CONST_INT,
	TYPE_CHAR,
	TYPE_CONST_CHAR,
	TYPE_VOID,
	TYPE_ARRAY,
	TYPE_FUNC
};

Type *new_type(int type);

// address descriptor
typedef struct {
	//int mem_addr;
	int reg_num;
	bool in_reg;
	bool in_mem;
} Addr_Des;

// register descriptor
typedef struct {
	Map *temp_reg;
	Map *saved_reg;
	//Symbol *t_reg[32];
} Reg_Des;

#define SYMBOL_LOCAL 0x1
#define SYMBOL_TEMP 0x2
#define SYMBOL_PARAM 0x4

// An identifier can be a Symbol.
typedef struct {
	char *name;
	Type *type;
	int offset;		// relative address
	int flag;
	Addr_Des addr_des;
	int value;		// for const
	bool live;
} Symbol;

Symbol *new_symbol(char *name, Type *type);

// Symbol table
typedef struct Env{
	Map *symbols;	// symbols maps string(name of a symbol) -> Symbol
	struct Env *prev;		// An Env has a pointer to its parent, but not vice-versa.
	int offset;
	int num_args;
} Env;

typedef struct {
	Vector *strings;
} String_Table;

typedef struct Node Node;
typedef struct Node {
	int nd_type;		// type of this Node, can have the value ND_IF, ND_STMT, etc.
	//Type *id_type;

	/* ND_EXPR: op is the operator, left is left operand, right is right operand.
	 * operands can be of type ND_EXPR, ND_ID, ND_NUML, ND_FUNC_CALL
	 * 
	 * ND_EXPR_UNARY: op is the operator, left is the operand. right is NULL.
	 *
	 * ND_COND: op is relation operator, left is left operand, 
	 * right is right operand. op is NULL => right is NULL and left != NULL.*/
	Token *op;
	Node *left;			// left-hand side
	Node *right;		// right-hand side
	bool with_paren;

	int value;			// signed integer or char for constant
	char *string;		// for printf stmt
	int string_label;

	/* 'if' (cond) stmt1 ['else' stmt2]: 
	 * If nd_type == ND_IF, stmt2 is optional and thus can be NULL.
	 * 
	 * 'while' (cond) stmt1
	 *
	 * 'for' (init; cond; inc) stmt1*/
	Node *stmt1;		// of type ND_STMT
	Node *stmt2;		// of type ND_STMT
	Node *cond;			// of type ND_COND
	Node *init;			// of type ND_FOR_INIT
	Node *inc;			// of type ND_FOR_INC

	Node *const_decl;	// for ND_COMPOUND or ND_PROGRAM
	Node *var_decl;

	Vector *stmts;		// for ND_STMTS

	Symbol *symbol;		// for identifier
	Vector *args;		// for function call, vector of Node*
	Vector *defs;		// constant define.
	Env *env;			// function declaration
	Token *token;
} Node;

// for Node->nd_type
enum {
	ND_ASSIGN = 61,

	ND_NUML,
	ND_CHARL,
	ND_ID,
	ND_ARR_ACCESS,

	ND_IF,
	ND_WHILE,
	ND_FOR,
	ND_FOR_INIT,
	ND_FOR_INC,
	ND_STMTS,
	ND_COMPOUND,
	ND_FUNC_CALL,
	ND_READ_STMT,
	ND_WRITE_STMT,
	ND_RETURN_STMT,
	ND_NULL_STMT,

	ND_EXPR,
	ND_COND,
	ND_CONST_DECL,
	ND_CONST_DEF,
	ND_VAR_DECL,
	ND_VAR_DEF,
	ND_FUNC_DECL,
	ND_MAIN
};

Node * new_node(int nd_type);
Node * new_const_def_node(Type * type, Symbol * symbol, int value);
Node * new_id_node(Token * token, Symbol * symbol);
Node * new_array_access_node(Symbol * id, Node * index);
Node * new_expr_node(Token * op, Node * left, Node * right);
Node * new_cond_node(Token * op, Node * left, Node * right);
Node * new_if_node(Node * cond, Node * then, Node * els);
Node * new_while_node(Node * cond, Node * body);
Node * new_for_node(Node * init, Node * cond, Node * inc, Node * body);
Node * new_for_init_node(Symbol * id, Node * expr);
Node * new_for_inc_node(Node * left, Node * expr);
Node * new_assignment_node(Node * lhs, Node * rhs);
Node * new_func_decl_node(Symbol * func_id, Node * body, Env *env);

enum {
	REG_TEMP = 0, // temporary variable
	REG_VAR = 1,
	REG_NUM = 2	// constant
};

typedef struct {
	int type;
	int vn;			// virtual reggister number, zero for variables
	int rn;
	Symbol *symbol;	// for variables
	int value;		// for number
	// REG_TEMP: false, REG_VAR: symbol->type, REG_NUM:default: false
	// to keep track of the type of a (temporary) variable or literal
	// used in a print statement, may not be consistent elsewhere.
	bool is_char;
	bool is_return_value;
} Reg;

// intermediate representation

enum {
	IR_TIMES = 42,
	IR_ADD = 43,
	IR_SUB = 45,
	IR_DIV = 47,
	IR_LS = 60,
	IR_ASSIGN = 61,
	IR_GT = 62,
	IR_EQ = 256,
	IR_NE,
	IR_GE,
	IR_LE,
	IR_EXPR_BRANCH,	// if (expr) ...
	IR_GOTO,
	IR_DECL,		// const and var
	IR_FUNC_DECL,	// function declaration
	IR_ASSIGN_ARR,
	IR_ARR_ACCESS,
	IR_FUNC_CALL,
	IR_READ,
	IR_WRITE,
	IR_RETURN,
	IR_LABEL
};

typedef struct {
	int op;
	bool if_false;
	Reg *arg1;
	Reg *arg2;
	Reg *result;
	Token *token;
	int size;		// for load/store

	int b_label;	// label before this code
	int a_label;	// label after this code
	int to_label;	// for goto instruction

	// function call, declaration
	char *name;
	int num_args;
	Vector *args;	// args of Reg *
	Env *env;		// function declaration

	// special case: for printf stmt
	char *string;
	int string_label;

	bool is_leader;	// for partitioning bb
	Vector *next_use;	// next-use symbols: variables in arg1, arg2, args and result
	Vector *def;	// lists the variables that may be assigned a value by the instruction
	Vector *out;	// variables live at end of this ir, globals excluded.

	bool is_return_value;// not used
} IR;

typedef struct {
	Node *const_decl;
	Node *var_decl;
	Vector *funcs;		// vector of Node(ND_FUNC_DECL)
	Node *main_func;
} Program_AST;

#define REG_V0 2
#define REG_A0 4
#define REG_A3 7
#define REG_T0 8
#define REG_T1 9
#define REG_T2 10
#define REG_T3 11
#define REG_T4 12
#define REG_T5 13
#define REG_T6 14
#define REG_T7 15
#define REG_S0 16
#define REG_S7 23
#define REG_T8 24
#define REG_T9 25
#define REG_FP 30

// basic block
typedef struct {
	int label;
	Vector *ir;
	Vector *pred;	// predcessor, vec of BB*
	Vector *succ;	// successor
	Vector *in_regs;// vec of Symbol*
	Vector *out_regs;
	Vector *def;	// vec of Symbol*
	Vector *use;
} BB;

typedef struct {
	BB *global_vars;
	Vector *funcs;	// vec of vec(func) of bb
} Program;

Program_AST *new_program();
Program_AST * parse(Vector *_tokens);
Vector * gen_ir(Program_AST *prog);
void ir_demo(Vector *ir, char * path, int print_option);
void inter_demo(Program *prog, char * path, int print_option);
void basic_block_demo(Program *prog, char *path);
Program * partition_program(Vector *ir);
bool gen_mips(Program *prog, char *path, int flag);
// error.c
void errorf(Token *t, const char *fmt, ...);
void warningf(Token *t, const char *fmt, ...);
char *type2string(int type);
char *type2str(int type);
bool is_saved_reg(int reg_num);

void optimization(Program * prog);
void data_flow_analysis(Program *prog);
void get_next_use_info(Program *prog);
void print_regs_of_bb(BB *bb, int print_option);
void print_ir(FILE *fp, IR *t);
IR * new_ir(int op);