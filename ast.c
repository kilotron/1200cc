#include "1200cc.h"

Node * new_node(int nd_type)
{
	Node *node = calloc(1, sizeof(Node));
	node->nd_type = nd_type;
	return node;
}

Node * new_const_def_node(Type *type, Symbol *symbol, int value)
{
	Node *node = new_node(ND_CONST_DEF);
	node->id_type = type;
	node->symbol = symbol;
	node->value = value;
	return node;
}

/* neither of the params is null.*/
Node *new_id_node(Token *token, Symbol *symbol)
{
	Node * node = new_node(ND_ID);
	node->token = token;
	node->id_type = symbol->type;
	node->symbol = symbol;
	return node;
}

Node *new_array_access_node(Symbol *id, Node *index)
{
	Node *node = new_node(ND_ARR_ACCESS);
	node->symbol = id;
	node->left = index;
	return node;
}

Node * new_expr_node(Token *op, Node *left, Node *right) {
	Node * node = new_node(ND_EXPR);
	node->op = op;
	node->left = left;
	node->right = right;
	return node;
}

/* Pre-condtions: op == NULL && right == NULL && left != NULL or
neither of them is NULL. op is of type '>','>=','<','<=','!=','=='*/
Node *new_cond_node(Token *op, Node *left, Node *right) {
	Node *node = new_node(ND_COND);
	node->op = op;
	node->left = left;
	node->right = right;
	return node;
}

/* Pre-conditions: cond != NULL, then != NULL, els is optional(NULL or not NULL).*/
Node *new_if_node(Node *cond, Node *then, Node *els) {
	Node *node = new_node(ND_IF);
	node->cond = cond;
	node->stmt1 = then;
	node->stmt2 = els;
	return node;
}

/* Pre-condtions: cond != NULL, body != NULL*/
Node *new_while_node(Node *cond, Node *body) {
	Node *node = new_node(ND_WHILE);
	node->cond = cond;
	node->stmt1 = body;
	return node;
}

Node *new_for_node(Node *init, Node *cond, Node *inc, Node *body) {
	Node *node = new_node(ND_FOR);
	node->init = init;
	node->cond = cond;
	node->inc = inc;
	node->stmt1 = body;
	return node;
}

Node *new_for_init_node(Symbol *id, Node *expr)
{
	Node *node = new_node(ND_FOR_INIT);
	node->symbol = id;
	node->left = expr;
	return node;
}

Node *new_for_inc_node(Node *left, Node *expr)
{
	Node *node = new_node(ND_FOR_INC);
	node->left = left;
	node->right = expr;
	return node;
}

Node *new_assignment_node(Node *lhs, Node *rhs)
{
	Node *node = new_node(ND_ASSIGN);
	node->left = lhs;
	node->right = rhs;
	return node;
}

Node *new_func_decl_node(Symbol *func_id, Node *body, Env *env)
{
	Node *node = new_node(ND_FUNC_DECL);
	node->id_type = func_id->type;
	node->symbol = func_id;
	node->stmt1 = body;
	node->env = env;
	return node;
}