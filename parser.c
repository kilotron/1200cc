#include "1200cc.h"
#define MATCH_OR_ERROR(token_type, label) if (!match(token_type)) goto label
#define MATCH_OR_RETURN_NULL(token_type) if (!match(token_type)) return NULL
#define NOT_NULL_OR_ERROR(var, label) if (var == NULL) goto label
#define SET_ERROR_IF_NULL(var) if (var == NULL) error = true
#define SEMANTIC_ERROR(semantic_error) semantic_error = true; error_in_program = true

extern bool error_in_program;
Env *top;			// Symbol table at the top of stack, in other words, current symbol table.
String_Table string_table;
Vector *tokens;		// all tokens in source program.
Token *look;		// lookahead token.
Token *look_prev;	// for demo
int pos;			// index of next token. pos can be modified in move() and move_step().
Symbol *current_func;// function declaration type-checking, modified in enter_symbol only
bool current_func_has_return; // modified in enter_symbol or return_stmt

Env *new_env(Env *prev);
static Symbol * lookup_symbol(char *name);
static bool enter_symbol(Token *token, Symbol *symbol);
static Symbol * assert_decld_id(Token *token);
static void move();
static void move_step(int step);
static bool match(int type);
static Token *lookahead(int i);
static void test(TypeSet *expected, TypeSet *tol);
static void skip_to(TypeSet *to);
static void skip_to_next_stmt();
static Program_AST * program();
static Node * const_decl(bool is_local);
static Vector * const_def(bool is_local);
static Node * signed_integer();
static Node * var_decl(bool is_local);
static Vector *var_def(bool is_local);
static Type *type_specifier();
static Node *func_with_ret_value_decl();
static Symbol * func_with_ret_value_decl_head();
static Vector * param_list();
static Node *func_without_ret_value_decl();
static Node * main_func();
static Node * compound_stmt();
static Node *stmts();
static Node *stmt();
static Node * if_stmt();
static Node * condition();
static Node * while_stmt();
static Node * for_stmt();
static Node * for_init();
static Node * for_inc();
static Node * func_call();
static Vector *args_list();
static Node * assign_stmt();
static Node * read_stmt();
static Node * write_stmt();
static Node * return_stmt();
static Node * null_stmt();
static Node * expr();
static Node * term();
static Node * factor();
static Type *type_of_expr(Node *e);

/*Pre-conditions: tokens is not null.*/
Program_AST * parse(Vector *_tokens)
{
	top = new_env(NULL);
	string_table.strings = new_vec();
	tokens = _tokens;
	pos = 0;
	move();	// look is not NULL since tokens has at least a token of TK_EOF.
	return program();
}

Env *new_env(Env *prev)
{
	Env *env = calloc(1, sizeof(Env));
	env->symbols = new_map();
	env->prev = prev;
	return env;
}

static Symbol * lookup_symbol(char *name)
{
	Symbol *symbol;
	for (Env *e = top; e != NULL; e = e->prev) {
		symbol = map_get(e->symbols, name);
		if (symbol != NULL)
			return symbol;
	}
	return NULL;
}

/* Post-conditions: Returns true if this id is entered into table
successfully false if this id is redeclared.*/
static bool enter_symbol(Token *token, Symbol *symbol) 
{
	Symbol *s = map_get(top->symbols, symbol->name);	// DO NOT use lookup_symbol()
	if (s != NULL) {
		errorf(token, "redeclaration of '%s'", symbol->name);
		return false;
	}
	if (symbol->flag & SYMBOL_PARAM)
		top->num_args++;
	if (symbol->type->type == TYPE_FUNC) {
		current_func = symbol;
		current_func_has_return = false;
	}
	map_put(top->symbols, token->lexeme, symbol);
	return true;
}

static int enter_string(char *s)
{
	vec_put(string_table.strings, s);
	return string_table.strings->len;
}

/* If the identifier has already been declared, returns the symbol table
entry of this identifier else returns a Symbol of type TYPE_INT
and enter it into the symbol table.*/
static Symbol * assert_decld_id(Token *token)
{
	Symbol *symbol = lookup_symbol(token->lexeme);
	if (symbol == NULL) {
		Type *type = new_type(TYPE_INT);
		// the type of the undeclared identifier is set to int
		// this identifier is entered to symbol table.
		errorf(token, "undeclared identifer");
		SEMANTIC_ERROR(error_in_program);
		if (look->type == '(' || look->type == TK_SC) {
			type = new_type(TYPE_FUNC);
		}
		else if (look->type == '[') {
			type = new_type(TYPE_ARRAY);
			type->array_of = new_type(TYPE_INT);
		}
		symbol = new_symbol(token->lexeme, type);
		enter_symbol(token, symbol);
	}

	return symbol;
}

/* Post-conditions: look is set to next token. If end of tokens is reached,
look remain to be of type TK_EOF. pos < tokens->len*/
static void move()
{
	look_prev = look;
	look = vec_get(tokens, pos);
	if (pos + 1 < tokens->len)
		pos++;
	else
		pos = tokens->len - 1;
}

/* Pre-conditions: step >= 1.
Post-conditions: look is set to next (step) token. If end of tokens is reached,
look remain to be of type TK_EOF. move() is a special case of move_step(): move_step(1)
pos < tokens->len.*/
static void move_step(int step)
{
	if (pos + step - 1 < tokens->len)
		look = vec_get(tokens, pos + step - 1);
	else
		look = vec_get(tokens, tokens->len);

	if (pos + step < tokens->len)
		pos += step;
	else
		pos = tokens->len - 1;
}

/* Pre-conditions: param type is token's type, e.g. TK_INT, TK_PLUS
Post-conditions: If look->type matches param type, moves to next token and returns
true else reports error and returns false.*/
static bool match(int type)
{
	if (look->type == type) {
		move();
		return true;
	}
	else {
		errorf(look, "%s is expected before %s", type2str(type), type2str(look->type));
		return false;
	}
}

/* kind of like treat or trick.
match -> return true
skip -> return false.*/
static bool match_or_skip(int type, TypeSet *ty_set)
{
	if (look->type == type) {
		move();
		return true;
	}
	else {
		errorf(look, "%s is expected before %s", type2str(type), type2str(look->type));
		if (ty_set != NULL)
			skip_to(ty_set);
		return false;
	}
}

/* Pre-conditions: 1 <= i.
Post-conditions: 
lookhead(0) is look; lookahead(1) is the token after look.
returns next next token if i == 1...
If pos + i - 1 is out of bounds returns the last element of tokens(TK_EOF).*/
static Token *lookahead(int i)
{
	Token *t = vec_get(tokens, pos + i - 1);
	return t == NULL ? vec_get(tokens, tokens->len - 1) : t;
}

/* Pre-conditions: expected != NULL, tol != NULL.(tol is tolerance)
Post-condition: If the type of next token is in TypeSet expected do nothing,
else skip some tokens until the type of a token T if in Set tol, 
and set look to the token after T.*/
static void test(TypeSet *expected, TypeSet *tol)
{
	if (!typeset_isin(expected, look->type))
		skip_to(typeset_union(expected, tol));
}

/* Pre-conditions: to != NULL.
Post-conditions: Token look is in TypeSet to or look->type is TK_EOF.*/
static void skip_to(TypeSet *to)
{
	while (!typeset_isin(to, look->type) && look->type != TK_EOF) {
		move();
	}
}

static void skip_to_next_stmt()
{
	TypeSet *to = new_typeset(13, TK_IF, TK_WHILE, TK_FOR, TK_LBRACE, TK_ID,
		TK_PRINTF, TK_SCANF, TK_RETURN, TK_RBRACE, TK_SC);
	skip_to(to);
}

static void skip_to_end_of_block_or_stmt()
{
	unsigned int nesting_depth = 0;
	
	while (true) {
		switch (look->type) {
		case TK_EOF:
			return;
		case ';':
			if (!nesting_depth) {
				if (lookahead(1)->type == TK_ELSE) {
					break;
				}
				move();
				return;
			}
			break;
		case '}':
			if (nesting_depth == 0 || --nesting_depth == 0) {
				if (lookahead(1)->type == TK_ELSE) {
					break;
				}
				move();
				return;
			}
		case '{': case '(':
			// roughly tracking matching symbol
			nesting_depth++;
			break;
		case ')':
			nesting_depth--;
			break;
		}

		move();
	}
}

static Program_AST * program()
{
	Program_AST *prog = new_program();
	Vector *funcs = new_vec();
	Token *start = look;
	bool error = false;

	prog->funcs = funcs;

	// [<常量说明>]
	if (look->type == TK_CONST)
		prog->const_decl = const_decl(false);

	// [<变量说明>]
after_const_decl_in_program:

	if (eq_oneof(2, look->type, TK_INT, TK_CHAR)) {
		// cases: <变量说明>，<有返回值的函数定义>
		// lookahead(1) is expected to be TK_ID in both cases.
		if (eq_oneof(3, lookahead(2)->type, TK_COMMA, TK_SC, TK_LBRKT)) {
			prog->var_decl = var_decl(false);
			SET_ERROR_IF_NULL(prog->var_decl);
		}
		else if (eq_oneof(2, lookahead(2)->type, TK_LP, TK_LBRACE)) {
			Node *n = func_with_ret_value_decl();
			if (n == NULL)
				error = true;
			else
				vec_put(funcs, n);
		}
		else {	// error handling.
			skip_to_end_of_block_or_stmt();
			goto after_const_decl_in_program;
		}
	}

	// {有返回值|无返回值的函数定义}
	while (look->type != TK_EOF) {
		if (eq_oneof(2, look->type, TK_INT, TK_CHAR)) {
			Node *n = func_with_ret_value_decl();
			if (n == NULL)
				error = true;
			else
				vec_put(funcs, n);
			continue;
		}

		if (look->type == TK_VOID && lookahead(1)->type == TK_ID) {
			Node *n = func_without_ret_value_decl();
			if (n == NULL)
				error = true;
			else
				vec_put(funcs, n);
			continue;
		}

		// <主函数>
		if (look->type == TK_VOID && lookahead(1)->type == TK_MAIN) {
			prog->main_func = main_func();
			SET_ERROR_IF_NULL(prog->main_func);
			if (look->type != TK_EOF) {
				errorf(look, "tokens after Function 'main' is ignored");
			}
			break;
		}

		errorf(look, "expected function declaration");
		skip_to_end_of_block_or_stmt();
	}
	
	if (!error) {
		parser_demo(start, look_prev, "<程序>");
		return prog;
	}
	else {
		parser_demo(start, look_prev, "错误的<程序>");
		return NULL;
	}
	
}

/*<常量说明> -> const <常量定义> ; {const <常量定义> ;}*/
static Node * const_decl(bool is_local)
{
	Token *start = look;
	Vector *defs = new_vec(), *sub_defs;
	Node *node = new_node(ND_CONST_DECL);
	bool error = false;

	node->defs = defs;
	while (look->type == TK_CONST) {
		move();
		sub_defs = const_def(is_local);

		if (sub_defs == NULL) {	// error occurs in const_def
			error = true;
			skip_to_end_of_block_or_stmt();
			continue;
		}

		vec_appendv(defs, sub_defs);
		if (!match(';')) {
			error = true;
			if (eq_oneof(3, look->type, TK_CONST, TK_INT, TK_CHAR, TK_VOID)) {
				/* What follows a missing semicolon might be 'const const_def'
					, var_decl, func_decl, or main_func. In such cases, we assume
					that the ';' is lost.*/
				continue;
			}
			else {
				/* If the token following a missing semicolon is neither of 
					tokens listed above, we don't know what happens. In this case,
					skips to end of a block or statement.*/
				skip_to_end_of_block_or_stmt();
			}
		}	// else match(';') is true
	}

	if (!error) {
		parser_demo(start, look_prev, "<常量说明>");
		return node;
	}
	else {
		parser_demo(start, look_prev, "错误的<常量说明>");
		return NULL;	// to indicate error occurs when parsing const_delc.
	}
}

/*<常量定义> -> int <标识符> = <整数> {, <标识符> = <整数>}*/
static Vector * const_def(bool is_local)
{
	Token *token, *start = look;
	Symbol *symbol;
	Vector *defs = new_vec();
	Node *node;
	int value;
	Type *type = new_type(TYPE_CONST_INT);	// default type
	
	if (look->type == TK_CHAR) {
		type = new_type(TYPE_CONST_CHAR);
	}
	else if (look->type != TK_INT) {
		errorf(look, "expected %s or %s before %s", type2str(TK_CHAR),
			type2str(TK_INT), type2str(look->type));
	}

	do {
		move(); // after a move, look is expected to be of type TK_ID
		token = look;
		MATCH_OR_ERROR(TK_ID, error_const_def);
		symbol = new_symbol(token->lexeme, type);
		if (is_local)
			symbol->flag = SYMBOL_LOCAL;
		MATCH_OR_ERROR('=', error_const_def);
		if (type->type == TYPE_CONST_INT) {
			Node *si = signed_integer();
			NOT_NULL_OR_ERROR(si, error_const_def);
			value = si->value;
		}
		else {	// char constant
			value = look->value;
			MATCH_OR_ERROR(TK_CHARL, error_const_def);
		}
		symbol->value = value;
		node = new_const_def_node(type, symbol, value);
		if (enter_symbol(token, symbol))	// returns true ~ success
			vec_put(defs, node);
	} while (look->type == ',');

	parser_demo(start, look_prev, "<常量定义>");
	return defs;

error_const_def:
	parser_demo(start, look_prev, "错误的<常量定义>");
	return NULL;
}

static Node * signed_integer()
{
	Token *start = look;
	int sign = 1;
	Node * node = new_node(ND_NUML);

	if (look->type == '-') {
		node->token = look;
		sign = -1;
		move();
	}
	else if (look->type == '+') {
		node->token = look;
		move();
	}

	if (look->type == TK_NUML) {
		node->token = look;
		node->value = sign * look->value;
		move();
	}
	else {
		errorf(look, "expected integer before %s", type2str(look->type));
		parser_demo(start, look_prev, "错误的<整数>");
		return NULL;
	}
	parser_demo(start, look_prev, "<整数>");
	return node;
}

static Node * var_decl(bool is_local)
{
	Token *start = look;
	Vector *defs = new_vec(), *sub_defs;
	Node *node = new_node(ND_VAR_DECL);
	bool error = false;

	node->defs = defs;
	while ((look->type == TK_INT || look->type == TK_CHAR) 
		&& eq_oneof(3, lookahead(2)->type, ',', ';', '[')) {
		sub_defs = var_def(is_local);
		if (sub_defs == NULL) {	// error occurs in var_def
			error = true;
			skip_to_end_of_block_or_stmt();
			continue;
		}

		vec_appendv(defs, sub_defs);

		if (!match(';')) {
			error = true;
			if (eq_oneof(3, look->type, TK_INT, TK_CHAR, TK_VOID)) {
				/* What follows a missing semicolon might be var_decl, func_decl,
				   or main_func. In such cases, we assume that the ';' is lost.*/
				continue;
			}
			else {
				/* If the token following a missing semicolon is neither of
					tokens listed above, we don't know what happens. In this case,
					skips to end of a block or statement.*/
				skip_to_end_of_block_or_stmt();
			}
		}
	}
	if (!error) {
		parser_demo(start, look_prev, "<变量说明>");
		return node;
	}
	else {
		parser_demo(start, look_prev, "错误的<变量说明>");
		return NULL;
	}
}

/* <变量定义> -> <类型标识符> (<标识符> | <标识符>[无符号整数]) 
				{, (<标识符> | <标识符>[无符号整数])} 
Pre-condtions: look->type is either TK_INT or TK_CHAR.*/
static Vector *var_def(bool is_local)
{
	Type *type;
	Token *token, *start = look;
	Symbol *symbol;
	Node *node;
	Vector *defs = new_vec();

	/* No error handling for type_specifier since look->type is 
	   either TK_INT or TK_CHAR.*/
	type = type_specifier();

	while (true) {
		token = look;
		MATCH_OR_ERROR(TK_ID, error_in_var_def);

		if (look->type == '[') {
			Type *type_of_array = new_type(TYPE_ARRAY);
			type_of_array->array_of = type;
			move();

			if (look->type == TK_NUML) {
				type_of_array->len = look->value;
				move();
			}
			else {
				errorf(look, "expected an unsigned interger");
				goto error_in_var_def;
			}

			type_of_array->size = WORD_SIZE * type_of_array->len;
			
			MATCH_OR_ERROR(']', error_in_var_def);
			symbol = new_symbol(token->lexeme, type_of_array);
		}
		else {
			symbol = new_symbol(token->lexeme, type);
		}

		if (is_local)
			symbol->flag = SYMBOL_LOCAL;
		if (enter_symbol(token, symbol)) {
			node = new_node(ND_VAR_DEF);
			node->symbol = symbol;
			vec_put(defs, node);
		}

		if (look->type != ',') {
			break;
		}
		move();
	}
	parser_demo(start, look_prev, "<变量定义>");
	return defs;

error_in_var_def:
	parser_demo(start, look_prev, "错误的<变量定义>");
	return NULL;
}

/* Post-conditions: Returns the Type of look(expected a type specifier), 
 and gets the next token. If look is not a type, reports error and returns
 Type int.*/
static Type *type_specifier()
{
	Token *start = look;
	Type *type = new_type(TYPE_INT);	// default type
	if (look->type == TK_CHAR) {
		type = new_type(TYPE_CHAR);
	}
	else if (look->type != TK_INT) {
		errorf(look, "expected 'int' or 'char' before %s", type2str(look->type));
	}
	move();
	parser_demo(start, look_prev, "<类型标识符>");
	return type;
}

/* Pre-conditions: look is 'int' or 'char'.*/
static Node *func_with_ret_value_decl()
{
	Env *saved_env, *this_env;
	Symbol *func_id;
	Node *body;
	Token *start= look;

	func_id = func_with_ret_value_decl_head();
	NOT_NULL_OR_ERROR(func_id, error_in_head_of_func_with_ret_value_decl);

	saved_env = top;
	top = new_env(top);
	this_env = top;
	if (look->type == '(') {
		move();
		func_id->type->params_list = param_list();
		NOT_NULL_OR_ERROR(func_id->type->params_list, error_in_func_with_ret_value_decl);
		MATCH_OR_ERROR(')', error_in_func_with_ret_value_decl);
	}
	MATCH_OR_ERROR('{', error_in_func_with_ret_value_decl);
	body = compound_stmt();
	if (body == NULL) {	// error handling
		if (look->type == '}') {
			move();
		}
		top = saved_env;
		parser_demo(start, look_prev, "错误的<有返回值函数定义>");
		return NULL;
	}
	MATCH_OR_ERROR('}', error_in_func_with_ret_value_decl);
	if (!current_func_has_return) {
		errorf(start, "return with no value, in function returning '%s'",
			current_func->type->ret->name);
		SEMANTIC_ERROR(error_in_program);
	}
	top = saved_env;
	parser_demo(start, look_prev, "<有返回值函数定义>");
	return new_func_decl_node(func_id, body, this_env);

error_in_func_with_ret_value_decl:
	top = saved_env;
error_in_head_of_func_with_ret_value_decl:
	skip_to_end_of_block_or_stmt();
	parser_demo(start, look_prev, "错误的<有返回值函数定义>");
	return NULL;
}

/* Pre-conditions: look is 'int' or 'char'.*/
static Symbol * func_with_ret_value_decl_head()
{
	Type *type = new_type(TYPE_FUNC);
	Token *token, *start = look;
	Symbol *symbol;

	if (look->type == TK_INT) {
		type->ret = new_type(TYPE_INT);
	}
	else if (look->type == TK_CHAR) {
		type->ret = new_type(TYPE_CHAR);
	}	// else never happens

	move();
	token = look;
	MATCH_OR_ERROR(TK_ID, error_in_func_with_ret_value_decl_head);
	symbol = new_symbol(token->lexeme, type);
	enter_symbol(token, symbol);

	parser_demo(start, look_prev, "<声明头部>");
	return symbol;

error_in_func_with_ret_value_decl_head:
	parser_demo(start, look_prev, "错误的<声明头部>");
	return NULL;
}

/*Post-conditions: The identifier is entered into symbol table.
Returns a Vector of parameters(of type Node * of id).*/
static Vector * param_list()
{
	Type *type;
	Token *token, *start = look;
	Symbol *symbol;
	Node *node;
	Vector *params = new_vec();	// vector of Node *

	while (true) {
		type = type_specifier();
		token = look;
		MATCH_OR_ERROR(TK_ID, error_in_param_list);
		symbol = new_symbol(token->lexeme, type);
		symbol->flag = SYMBOL_PARAM | SYMBOL_LOCAL;
		enter_symbol(token, symbol);
		node = new_id_node(token, symbol);
		vec_put(params, node);
		if (look->type != ',') {
			break;
		}
		move();
	}
	top->offset = 0;	// start of local variables.
	parser_demo(start, look_prev, "<值参数表>");
	return params;

error_in_param_list:
	parser_demo(start, look_prev, "错误的<值参数表>");
	return NULL;
}

/*Pre-conditions: look is 'void'*/
static Node *func_without_ret_value_decl()
{
	Type *type = new_type(TYPE_FUNC);
	type->ret = NULL;
	Token *token, *start = look;
	Symbol *symbol;
	Env *saved_env, *this_env;
	Node *body;

	match(TK_VOID);
	token = look;
	MATCH_OR_ERROR(TK_ID, error_in_head_of_func_without_ret_value_decl);
	symbol = new_symbol(token->lexeme, type);
	enter_symbol(token, symbol);
	saved_env = top;
	top = new_env(top);
	this_env = top;
	if (look->type == '(') {
		move();
		symbol->type->params_list = param_list();
		NOT_NULL_OR_ERROR(symbol->type->params_list, error_in_func_without_ret_value_decl);
		MATCH_OR_ERROR(')', error_in_head_of_func_without_ret_value_decl);
	}
	MATCH_OR_ERROR('{', error_in_func_without_ret_value_decl);
	body = compound_stmt();
	if (body == NULL) {
		if (look->type == '}')
			move();
		top = saved_env;
		parser_demo(start, look_prev, "错误的<无返回值函数定义>");
		return NULL;
	}
	MATCH_OR_ERROR('}', error_in_func_without_ret_value_decl);
	top = saved_env;
	parser_demo(start, look_prev, "<无返回值函数定义>");
	return new_func_decl_node(symbol, body, this_env);
	
error_in_func_without_ret_value_decl:
	top = saved_env;
error_in_head_of_func_without_ret_value_decl:
	skip_to_end_of_block_or_stmt();
	parser_demo(start, look_prev, "错误的<无返回值函数定义>");
	return NULL;
}

static Node * main_func()
{
	Node *node = new_node(ND_MAIN);
	Env *saved_env;
	Token *start = look, *token;
	bool error = false;

	match(TK_VOID);
	token = look;	// expected to be 'main'
	enter_symbol(token, new_symbol(token->lexeme, new_type(TYPE_FUNC)));
	match(TK_MAIN);
	if (match('(') && match(')') && match('{')) {
		;
	}
	else {
		error = true;
		skip_to(new_typeset(1, '{'));
	}
	saved_env = top;
	top = new_env(top);
	node->env = top;
	node->stmt1 = compound_stmt();
	if (node->stmt1 == NULL) {
		error = true;
		if (look->type == '}')
			move();
	}
	else {
		if (!match('}'))
			error = true;
	}
	top = saved_env;
	if (!error) {
		parser_demo(start, look_prev, "<主函数>");
		return node;
	}
	else {
		parser_demo(start, look_prev, "错误<主函数>");
		return NULL;
	}
	
}


static Node * compound_stmt()
{
	Node *node = new_node(ND_COMPOUND);
	Token *start = look;
	bool error = false;

	if (look->type == TK_CONST) {
		node->const_decl = const_decl(true);
		SET_ERROR_IF_NULL(node->const_decl);
	}

	if (eq_oneof(2, look->type, TK_INT, TK_CHAR)) {
		node->var_decl = var_decl(true);
		SET_ERROR_IF_NULL(node->var_decl);
	}

	node->stmt1 = stmts();	// 语句列
	SET_ERROR_IF_NULL(node->stmt1);

	if (!error) {
		parser_demo(start, look_prev, "<复合语句>");
		return node;
	}
	else {
		parser_demo(start, look_prev, "错误的<复合语句>");
		return NULL;
	}
	
}

static Node *stmts()
{
	Node * node = new_node(ND_STMTS), *st = NULL;
	Token *start = look;
	bool error = false;
	node->stmts = new_vec();
	while (eq_oneof(9, look->type, TK_IF, TK_WHILE, TK_FOR, TK_LBRACE, TK_ID,
		TK_SCANF, TK_PRINTF, TK_RETURN, TK_SC)) {
		st = stmt();
		if (st != NULL) {
			vec_put(node->stmts, st);
		}
		else {
			error = true;
		}
	}

	if (error) {
		parser_demo(start, look_prev, "错误的<语句列>");
		return NULL;
	}
	else {
		parser_demo(start, look_prev, "<语句列>");
		return node;
	}
}

/* When functions stmt() returns, we assume that Token look is in a possibly
   correct position(The position after end of a block or statement).*/
static Node *stmt()
{
	Node *node = NULL;
	Token *start = look;
	bool error = false;

	if (eq_oneof(5, look->type, TK_IF, TK_WHILE, TK_FOR, TK_LBRACE, TK_SC)) {
		switch (look->type) {
		case TK_IF:
			node = if_stmt();
			break;
		case TK_WHILE:
			node = while_stmt();
			break;
		case TK_FOR:
			node = for_stmt();
			break;
		case TK_LBRACE:
			move();
			node = stmts();
			if (node == NULL) {
				// the '}' may be skipped if error occurs in stmts()
				if (look->type == '}')
					move();
			}
			else if (!match(TK_RBRACE)){
				goto error_in_stmt;
			}
			break;
		case TK_SC:
			node = null_stmt();
			break;
		}
		SET_ERROR_IF_NULL(node);	// no need to skip, just report syntax error.
	}
	else if (eq_oneof(4, look->type, TK_SCANF, TK_PRINTF, TK_RETURN, TK_ID)){
		// stmt ends with a ';' if error occurs we need to skip some tokens
		switch (look->type) {
		case TK_SCANF:
			node = read_stmt();
			break;
		case TK_PRINTF:
			node = write_stmt();
			break;
		case TK_RETURN:
			node = return_stmt();
			break;
		case TK_ID:
			if (eq_oneof(2, lookahead(1)->type, '(', ';'))
				node = func_call();
			else
				node = assign_stmt();
			break;
		}
		NOT_NULL_OR_ERROR(node, error_in_stmt);
		MATCH_OR_ERROR(';', error_in_stmt);
	}
	else {
		errorf(look, "expected a statement");
		goto error_in_stmt;
	}

	if (error)
		goto error_without_skip_in_stmt;
	
	parser_demo(start, look_prev, "<语句>");
	return node;

error_in_stmt:
	skip_to_end_of_block_or_stmt();
error_without_skip_in_stmt:
	parser_demo(start, look_prev, "错误的<语句>");
	return NULL;
}

/* Pre-conditions: look->type == TK_IF.
   Post-conditions: Token look is in a possibly correct position
   (The position after end of a block or statement) whether syntax error occurs.*/
static Node * if_stmt()
{
	Token *start = look;
	Node *cond_of_if = NULL;
	Node *then_stmt = NULL;
	Node *else_stmt = NULL;

	match(TK_IF); 
	MATCH_OR_ERROR('(', error_in_if_stmt);

	cond_of_if = condition();
	NOT_NULL_OR_ERROR(cond_of_if, error_in_if_stmt);
	MATCH_OR_ERROR(')', error_in_if_stmt);

	then_stmt = stmt();	// 需要修改
	NOT_NULL_OR_ERROR(then_stmt, error_in_sub_stmt_of_if_stmt);

	if (look->type == TK_ELSE) {
		match(TK_ELSE);
		else_stmt = stmt();	// 需要修改
		NOT_NULL_OR_ERROR(else_stmt, error_in_sub_stmt_of_if_stmt);
	}

	parser_demo(start, look_prev, "<条件语句>");
	return new_if_node(cond_of_if, then_stmt, else_stmt);

error_in_if_stmt:
	skip_to_end_of_block_or_stmt();
error_in_sub_stmt_of_if_stmt:
	parser_demo(start, look_prev, "错误的<条件语句>");
	return NULL;
}

// 需要修改错误处理，出错的时候返回NULL
static Node * condition()
{
	Node *left = NULL, *right = NULL;
	Token *op = NULL, *start = look;
	Type *left_type, *right_type;

	left = expr();
	NOT_NULL_OR_ERROR(left, error_in_condition);

	if (eq_oneof(6, look->type, TK_LS, TK_LE, TK_GT, TK_GE, TK_EQ, TK_NE)) {
		// <条件> -> <表达式> <relop> <表达式>
		op = look;
		move();
		right = expr();
		NOT_NULL_OR_ERROR(right, error_in_condition);
	} // else <条件> -> <表达式>

	// type-checking
	left_type = type_of_expr(left);
	if (right != NULL) {
		right_type = type_of_expr(right);
		if (left_type->type != TYPE_INT || right_type->type != TYPE_INT)
			warningf(start, "type 'int' expected");
	}
	else if (left_type->type != TYPE_INT) {
		warningf(start, "type 'int' expected");
	}

	
	parser_demo(start, look_prev, "<条件>");
	return new_cond_node(op, left, right);

error_in_condition:
	parser_demo(start, look_prev, "错误的<条件>");
	return NULL;
}

static Node * while_stmt()
{
	Node *cond_of_while, *body;
	Token *start = look;
	match(TK_WHILE);

	MATCH_OR_ERROR('(', error_in_while_stmt);
	cond_of_while = condition();
	NOT_NULL_OR_ERROR(cond_of_while, error_in_while_stmt);
	MATCH_OR_ERROR(')', error_in_while_stmt);
	body = stmt();
	NOT_NULL_OR_ERROR(body, error_in_sub_stmt_of_while_stmt);

	parser_demo(start, look_prev, "<循环语句>");
	return new_while_node(cond_of_while, body);

error_in_while_stmt:
	skip_to_end_of_block_or_stmt();
error_in_sub_stmt_of_while_stmt:
	parser_demo(start, look_prev, "错误的<循环语句>");
	return NULL;
}

static Node * for_stmt()
{
	Node *init, *cond, *inc, *body;
	Token *start = look;
	match(TK_FOR);

	MATCH_OR_ERROR('(', error_in_for_stmt);
	init = for_init();
	NOT_NULL_OR_ERROR(init, error_in_for_stmt);
	MATCH_OR_ERROR(';', error_in_for_stmt);

	cond = condition();
	NOT_NULL_OR_ERROR(cond, error_in_for_stmt);
	MATCH_OR_ERROR(';', error_in_for_stmt);

	inc = for_inc();
	NOT_NULL_OR_ERROR(inc, error_in_for_stmt);
	MATCH_OR_ERROR(')', error_in_for_stmt);

	body = stmt();
	NOT_NULL_OR_ERROR(body, error_in_sub_stmt_of_for_stmt);

	parser_demo(start, look_prev, "<循环语句>");
	return new_for_node(init, cond, inc, body);

error_in_for_stmt:
	skip_to_end_of_block_or_stmt();
error_in_sub_stmt_of_for_stmt:
	parser_demo(start, look_prev, "错误的<循环语句>");
	return NULL;
}

static Node * for_init()
{
	Token *token = look;
	Node *e;
	MATCH_OR_RETURN_NULL(TK_ID);
	Symbol *symbol = assert_decld_id(token);
	MATCH_OR_RETURN_NULL('=');
	e = expr();
	if (e == NULL)
		return NULL;
	return new_for_init_node(symbol, e);
}

static Node * for_inc()
{
	Token *token, *op;
	Symbol *symbol;
	Node *node, *left, *right;

	token = look;
	MATCH_OR_RETURN_NULL(TK_ID);
	symbol = assert_decld_id(token);
	node = new_id_node(token, symbol);
	MATCH_OR_RETURN_NULL('=');
	token = look;
	MATCH_OR_RETURN_NULL(TK_ID);
	symbol = assert_decld_id(token);
	left = new_id_node(token, symbol);

	// more error handling need to be added.
	op = look;
	if (!eq_oneof(2, look->type, '+', '-')) {
		errorf(look, "expected '+' or '-' before %s", type2str(look->type));
		return NULL;
	}
	else {
		move();
	}

	if (look->type != TK_NUML) {
		errorf(look, "expected number literal before %s", type2str(look->type));
		return NULL;
	}

	right = new_node(ND_NUML);
	right->value = look->value;
	right->token = look;
	move();

	right = new_expr_node(op, left, right);
	return new_for_inc_node(node, right);
}

static args_list_type_checking(Vector *arg_list, Vector *param_list, Symbol *func,
	Token *tok_of_func)
{
	Node *arg;
	Type *arg_t, *param_t;
	if (!param_list && arg_list) {
		errorf(tok_of_func, "too many arguments to function '%s'", func->name);
		SEMANTIC_ERROR(error_in_program);
	}
	else if (param_list && !arg_list) {
		errorf(tok_of_func, "too few arguments to function '%s'", func->name);
		SEMANTIC_ERROR(error_in_program);
	}
	else if (param_list && arg_list) {
		for (int i = 0; i < arg_list->len && i < param_list->len; i++) {
			arg = vec_get(arg_list, i);
			arg_t = type_of_expr(arg);
			param_t = ((Node *)(vec_get(param_list, i)))->symbol->type;
			if (arg_t->type != param_t->type) {
				warningf(arg->token, "implicit conversion from '%s' to '%s'",
					arg_t->name, param_t->name);
			}
		}
		if (arg_list->len != param_list->len) {
			errorf(tok_of_func, "too %s arguments to function '%s'",
				arg_list->len < param_list->len ? "few" : "many", func->name);
			SEMANTIC_ERROR(error_in_program);
		}
	}
}

/* Pre-conditions: look is an identifier, */
static Node * func_call()
{
	Symbol *symbol;
	Token *token, *start = look;
	Type *f = NULL;
	Vector *args = NULL;
	Node *node;
	bool error = false;

	token = look;
	match(TK_ID);
	symbol = assert_decld_id(token);
	/*
	symbol = lookup_symbol(token->lexeme);

	if (symbol == NULL) {
		/* If this function hasn't been not declared, we cannot perform semantic
		   type checking. What we can do is to return NULL and let the caller stmt()
		   skip this statement.
		errorf(token, "implicit declaration of function '%s'", token->lexeme);
		SEMANTIC_ERROR(error);
	}*/

	if (!error) {
		f = symbol->type;
		if (f->type != TYPE_FUNC) {
			errorf(token, "called object '%s' is not a function", token->lexeme);
			SEMANTIC_ERROR(error);
		}
	}
	
	if (look->type == '(') {
		move();
		args = args_list();
		NOT_NULL_OR_ERROR(args, error_in_func_call);
		MATCH_OR_ERROR(')', error_in_func_call);
	}

	// type-checking
	if (!error) {
		args_list_type_checking(args, f->params_list, symbol, start);
	}

	node = new_node(ND_FUNC_CALL);
	node->symbol = symbol;
	node->args = args;
	parser_demo(start, look_prev, "<函数调用语句>");
	return node;

error_in_func_call:
	parser_demo(start, look_prev, "错误的<函数调用语句>");
	return NULL;
}

static Vector *args_list()
{
	Vector *list = new_vec();
	Node *e;
	Token *start = look;

	while (true) {
		e = expr();
		NOT_NULL_OR_ERROR(e, error_in_args_list);
		vec_put(list, e);
		if (look->type != ',')
			break;
		move();
	}
	parser_demo(start, look_prev, "<值参数表>");
	return list;

error_in_args_list:
	parser_demo(start, look_prev, "错误的<值参数表>");
	return NULL;
}

/* Pre-conditions: look is an identifier.*/
static Node * assign_stmt()
{
	Symbol *symbol;
	Token *token, *start = look;
	Node *lhs = NULL, *rhs = NULL;
	Type *lhs_type = NULL, *rhs_type = NULL;
	bool error = false;	// No need to skip if we meet semantic error.

	token = look;
	match(TK_ID);
	symbol = assert_decld_id(token);

	if (look->type == '[') {
		MATCH_OR_ERROR('[', error_in_assign_stmt);
		Node *index = expr();
		NOT_NULL_OR_ERROR(index, error_in_assign_stmt);
		MATCH_OR_ERROR(']', error_in_assign_stmt);
		if (symbol->type->type == TYPE_ARRAY) {
			lhs = new_array_access_node(symbol, index);
			lhs_type = symbol->type->array_of;
		}
		else {
			errorf(start, "'%s' is not an array", symbol->name);
			SEMANTIC_ERROR(error);
		}
	}
	else {
		lhs = new_id_node(token, symbol);
		lhs_type = symbol->type;
		if (eq_oneof(2, symbol->type->type, TYPE_CONST_INT, TYPE_CONST_CHAR)) {
			errorf(token, "assignment of read-only variable '%s'", token->lexeme);
			SEMANTIC_ERROR(error);
		}
		else if (!eq_oneof(2, symbol->type->type, TYPE_INT, TYPE_CHAR)) {
			errorf(token, "lvalue required as left operand of assignment");
			SEMANTIC_ERROR(error);
		}
	}

	MATCH_OR_ERROR('=', error_in_assign_stmt);
	
	rhs = expr();
	NOT_NULL_OR_ERROR(rhs, error_in_assign_stmt);

	rhs_type = type_of_expr(rhs);
	if (!error && lhs_type->type != rhs_type->type)
		warningf(start, "implicit conversion from '%s' to '%s'", 
			rhs_type->name, lhs_type->name);

	parser_demo(start, look_prev, "<赋值语句>");
	return new_assignment_node(lhs, rhs);

error_in_assign_stmt:
	parser_demo(start, look_prev, "错误的<赋值语句>");
	return NULL;
}

static Node * read_stmt()
{
	Token *token, *start = look;
	Vector *args = new_vec();
	Symbol *symbol;
	Node *node;

	match(TK_SCANF);
	MATCH_OR_ERROR('(', error_in_read_stmt);

	while (true) {
		token = look;
		MATCH_OR_ERROR(TK_ID, error_in_read_stmt);
		symbol = assert_decld_id(token);
		vec_put(args, new_id_node(token, symbol));
		if (look->type != ',') {
			break;
		}
		move();
	}
	MATCH_OR_ERROR(')', error_in_read_stmt);

	node = new_node(ND_READ_STMT);
	node->args = args;
	parser_demo(start, look_prev, "<读语句>");
	return node;

error_in_read_stmt:
	parser_demo(start, look_prev, "错误的<读语句>");
	return NULL;
}

static Node * write_stmt()
{
	Node *node = new_node(ND_WRITE_STMT);
	Node *e = NULL;
	Token *start = look;

	match(TK_PRINTF);
	MATCH_OR_ERROR('(', error_in_write_stmt);
	if (look->type == TK_STRL) {
		node->string = look->lexeme;
		node->string_label = enter_string(node->string);
		move();
		if (look->type == ',') {
			move();
			e = expr();
			NOT_NULL_OR_ERROR(e, error_in_write_stmt);
		}
	}
	else {	// an expr is expected
		e = expr();
		NOT_NULL_OR_ERROR(e, error_in_write_stmt);
	}
	node->left = e;
	MATCH_OR_ERROR(')', error_in_write_stmt);

	parser_demo(start, look_prev, "<写语句>");
	return node;

error_in_write_stmt:
	parser_demo(start, look_prev, "错误的<写语句>");
	return NULL;
}

static Node * return_stmt()
{
	Node *node, *e;
	Token *start = look;
	Type *expr_type;
	current_func_has_return = true;
	match(TK_RETURN);
	node = new_node(ND_RETURN_STMT);

	if (look->type == '(') {
		move();
		e = expr();
		NOT_NULL_OR_ERROR(e, error_in_return_stmt);
		MATCH_OR_ERROR(')', error_in_return_stmt);
		node->left = e;
		expr_type = type_of_expr(e);
		if (current_func->type->ret == NULL) {
			errorf(e->token, "'return' with a value, in function returning void");
			SEMANTIC_ERROR(error_in_program);
		}
		else if (current_func->type->ret->type != expr_type->type) {
			warningf(e->token, "implicit conversion from '%s' to '%s'",
				expr_type->name, current_func->type->ret->name);
		}
	}
	else if (current_func->type->ret != NULL) {
		if (look->type == ';') {
			errorf(start, "'return' with no value, in function returning '%s'",
				current_func->type->ret->name);
		}
		else {
			errorf(look, "'(' is expected.", current_func->type->ret->name);
			if (look->type == TK_ID) {
				e = expr();
				NOT_NULL_OR_ERROR(e, error_in_return_stmt);
				node->left = e;
			}
		}
		SEMANTIC_ERROR(error_in_program);
	}

	parser_demo(start, look_prev, "<返回语句>");
	return node;

error_in_return_stmt:
	parser_demo(start, look_prev, "错误的<返回语句>");
	return NULL;
}

/* Pre-conditions: look is ';' token
No error when null_stmt() is called.*/
static Node * null_stmt()
{
	Token *start = look;
	match(';');
	parser_demo(start, look_prev, "<空语句>");
	return new_node(ND_NULL_STMT);
}

/* op can be NULL*/
static Node * expr()
{
	Node *left, *right;
	Token *op, *start = look;

	if (eq_oneof(2, look->type, '+', '-')) {
		op = look;
		move();
		left = new_expr_node(op, term(), NULL);
	}
	else {
		left = term();
		NOT_NULL_OR_ERROR(left, error_in_expr);
	}

	while (eq_oneof(2, look->type, '+', '-')) {
		op = look;
		move();
		right = term();
		NOT_NULL_OR_ERROR(right, error_in_expr);
		left = new_expr_node(op, left, right);
	}
	left->token = start;
	//printf("type: %s\n", type_of_expr(left)->type == TYPE_CHAR ? "char" : "int");
	parser_demo(start, look_prev, "<表达式>");
	return left;
error_in_expr:
	parser_demo(start, look_prev, "错误的<表达式>");
	return NULL;
}

static Node * term()
{
	Node *left, *right;
	Token *op, *start = look;
	left = factor();
	NOT_NULL_OR_ERROR(left, error_in_term);
	while (eq_oneof(2, look->type, '*', '/')) {
		op = look;
		move();
		right = factor();
		NOT_NULL_OR_ERROR(right, error_in_term);
		left = new_expr_node(op, left, right);
	}
	parser_demo(start, look_prev, "<项>");
	return left;
error_in_term:
	parser_demo(start, look_prev, "错误的<项>");
	return NULL;
}

static Node * factor()
{
	Node *node = new_node(ND_NUML);	// default value when error occurs
	Symbol *symbol;
	Token *start = look;
	if (look->type == TK_ID) {
		symbol = assert_decld_id(look);
		if (eq_oneof(4, symbol->type->type, TYPE_CHAR, TYPE_INT, TYPE_CONST_CHAR,
			TYPE_CONST_INT)) {
			node = new_id_node(look, symbol);
			move();
		}
		else if (symbol->type->type == TYPE_ARRAY) {
			move();
			MATCH_OR_ERROR('[', error_in_factor);
			Node *index = expr();
			NOT_NULL_OR_ERROR(index, error_in_factor);
			MATCH_OR_ERROR(']', error_in_factor);
			node = new_array_access_node(symbol, index);
			// 数组越界检查，
			if (index->nd_type == ND_CHARL || index->nd_type == ND_NUML) {
				int i = index->value;
				int max = symbol->type->len;
				if (i < 0 || i >= max) {
					warningf(start, "index out of bound");
				}
			}

			if (index->nd_type == ND_ID && (index->symbol->type->type == TYPE_CONST_INT || index->symbol->type->type == TYPE_CONST_CHAR)) {
				int i = index->symbol->value;
				int max = symbol->type->len;
				if (i < 0 || i >= max) {
					warningf(start, "index out of bound");
				}
			}
		}
		else if (symbol->type->type == TYPE_FUNC) {
			if (symbol->type->ret == NULL) {
				errorf(look, "expression returning void");
			}
			node = func_call();
			NOT_NULL_OR_ERROR(node, error_in_factor);
		}
	}
	else if (look->type == '(') {
		move();
		node = expr();
		NOT_NULL_OR_ERROR(node, error_in_factor);
		MATCH_OR_ERROR(')', error_in_factor);
		node->with_paren = true;
	}
	else if (eq_oneof(3, look->type, TK_NUML, '+', '-')) {
		node = signed_integer();
		NOT_NULL_OR_ERROR(node, error_in_factor);
	}
	else if (look->type == TK_CHARL) {
		node = new_node(ND_CHARL);
		node->value = look->value;
		node->token = look;
		move();
	}
	else {
		errorf(look, "expected identifer, '(', int or char before %s", type2str(look->type));
	}

	parser_demo(start, look_prev, "<因子>");
	return node;

error_in_factor:
	parser_demo(start, look_prev, "错误的<因子>");
	return NULL;
}

// 需要修改
static Type *type_of_expr(Node *e)
{
	if (!e->with_paren && 
		(
			e->nd_type == ND_CHARL 
		|| (e->nd_type == ND_ARR_ACCESS && eq_oneof(2, e->symbol->type->array_of->type, TYPE_CHAR, TYPE_CONST_CHAR))
		|| (e->nd_type == ND_ID && eq_oneof(2, e->symbol->type->type, TYPE_CHAR, TYPE_CONST_CHAR))
		|| (e->nd_type == ND_FUNC_CALL && eq_oneof(2, e->symbol->type->ret->type, TYPE_CHAR, TYPE_CONST_CHAR))
		)
		)
		return new_type(TYPE_CHAR);
	return new_type(TYPE_INT);
}

Type *new_type(int type)
{
	Type *t = calloc(1, sizeof(Type));
	t->type = type;
	switch (type) {
	case TYPE_INT: case TYPE_CONST_INT:
		t->size = 4;
		t->name = "int";
		break;
	case TYPE_CHAR: case TYPE_CONST_CHAR:
		t->size = 4;
		t->name = "char";
		break;
	}
	return t;
}

Symbol *new_symbol(char *name, Type *type)
{
	Symbol *s = calloc(1, sizeof(Symbol));
	s->name = name;
	s->type = type;
	/* func_decl is after global const or var decl, offset is of no use for function id
	   Allocate 4 bytes for both char and int.*/
	s->offset = top->offset;
	if (type->type == TYPE_ARRAY)
		top->offset += WORD_SIZE * type->len;
	else
		top->offset += WORD_SIZE;
	return s;
}
  
Program_AST *new_program()
{
	return calloc(1, sizeof(Program_AST));
}