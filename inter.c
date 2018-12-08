// intermediate representation
#include "1200cc.h"

static int label = 1;
static int n_regs = 1;	// register number
static Vector *ir;
static Env *top;		// for temporary variable memory allocation

static void gen_const_or_var_decl(Node *node);
static void gen_func_decl(Node *node);
static Reg * gen_arr_access(Node *node);
static Reg * gen_func_call(Node *node);
static void gen_main_func(Node *node);
static Reg * gen_expr(Node *node);
static void gen_stmt(Node *node);
static void merge_labels();

Vector * gen_ir(Program_AST *prog)
{
	extern bool error_in_program;
	if (error_in_program || prog == NULL)
		return NULL;
	ir = new_vec();
	gen_const_or_var_decl(prog->const_decl);
	gen_const_or_var_decl(prog->var_decl);
	for (int i = 0; i < prog->funcs->len; i++) {
		gen_func_decl(vec_get(prog->funcs, i));
	}
	gen_main_func(prog->main_func);
	merge_labels();
	return ir;
}

static int new_label() {
	return label++;
}

static Reg *new_reg(int type)
{
	Reg *r = calloc(1, sizeof(Reg));
	Symbol *symbol;
	r->type = type;
	/* allocate reg num only for temporary variables.  */
	if (type == REG_TEMP) {
		r->vn = n_regs++;
		symbol = calloc(1, sizeof(Symbol));
		symbol->name = stringf("#t%d", r->vn);
		symbol->flag = SYMBOL_LOCAL | SYMBOL_TEMP;
		symbol->type = new_type(TYPE_INT);
		symbol->offset = top->offset;
		r->symbol = symbol;
		top->offset += WORD_SIZE;
		map_put(top->symbols, stringf("#t%d", r->vn), symbol);
	}
	return r;
}

static IR * new_ir(int op) {
	IR *i = calloc(1, sizeof(IR));
	i->op = op;
	i->is_leader = false;
	return i;
}

static IR * emit(int op, Reg *arg1, Reg *arg2, Reg *result)
{
	IR *i = new_ir(op);
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->result = result;
	vec_put(ir, i);
	return i;
}

static void emit_ir(IR *i)
{
	vec_put(ir, i);
}

static void emit_label(int label)
{
	IR *t = new_ir(IR_LABEL);
	t->b_label = t->a_label = label;
	vec_put(ir, t);
}

/*Pre-conditions: node is ND_ID, ND_ARR_ACCESS, ND_FUNC_CALL, ND_EXPR or ND_NUML*/
static Reg * gen_expr_arg(Node *node)
{
	Reg *arg = NULL;
	switch (node->nd_type) {
	case ND_ID:
		arg = new_reg(REG_VAR);
		arg->symbol = node->symbol;
		break;
	case ND_NUML: case ND_CHARL:
		arg = new_reg(REG_NUM);
		arg->value = node->value;
		break;
	case ND_ARR_ACCESS:
		arg = gen_arr_access(node);
		break;
	case ND_FUNC_CALL:
		arg = gen_func_call(node);
		break;
	case ND_EXPR:
		arg = gen_expr(node);
		break;
	}
	return arg;
}

/*Pre-conditions: node->nd_type is ND_EXPR*/
static Reg * gen_expr(Node *node)
{
	Reg *arg1 = NULL;
	Reg *arg2 = NULL;
	Reg *result;
	int op;
	if (node->op != NULL)
		op = node->op->type;	// both are ascii value.
	else { // expr is simply a var or number literal or ...
		return gen_expr_arg(node);
	}
	result = new_reg(REG_TEMP);
	arg1 = gen_expr_arg(node->left);
	if (node->right != NULL) {
		arg2 = gen_expr_arg(node->right);
	}
	emit(op, arg1, arg2, result);
	return result;
}

/* rvalue , arg1 is name of array, arg2 is index.*/
static Reg * gen_arr_access(Node *node)
{
	Reg *arg1 = new_reg(REG_VAR);
	arg1->symbol = node->symbol;
	Reg *arg2 = gen_expr(node->left);
	Reg *result = new_reg(REG_TEMP);
	emit(IR_ARR_ACCESS, arg1, arg2, result);
	return result;
}

static Reg * gen_func_call(Node *node)
{
	Reg *result, *arg;
	IR *t;

	if (node->symbol->type->ret == NULL)
		result = NULL;
	else 
		result = new_reg(REG_TEMP);
	
	t = new_ir(IR_FUNC_CALL);
	t->name = node->symbol->name;
	t->result = result;	// return value
	
	if (node->args) {
		t->num_args = node->args->len;
		t->args = new_vec();
		for (int j = 0; j < t->num_args; j++) {
			arg = gen_expr(vec_get(node->args, j));
			vec_put(t->args, arg);
		}
	}
	emit_ir(t);
	
	return result;
}

static void gen_assign_stmt(Node *node)
{
	Reg *result = new_reg(REG_VAR);
	Reg *arg1 = gen_expr(node->right);
	Reg *arg2 = NULL;
	IR *t;

	result->symbol = node->left->symbol;
	if (node->left->nd_type == ND_ARR_ACCESS) {
		arg2 = gen_expr(node->left->left);	// left of ND_ARR_ACCESS is index
		t = emit(IR_ASSIGN_ARR, arg1, arg2, result);
	}
	else {	// id
		t = emit(IR_ASSIGN, arg1, NULL, result);
	}
}

/*cond is ND_COND*/
int get_cond_op(Node *cond) {
	if (cond->op == NULL)
		return IR_EXPR_BRANCH;
	int op;
	switch (cond->op->type) {
	case TK_LS: op = IR_LS; break;
	case TK_LE: op = IR_LE; break;
	case TK_GT: op = IR_GT; break;
	case TK_GE: op = IR_GE; break;
	case TK_EQ: op = IR_EQ; break;
	case TK_NE: op = IR_NE; break;
	}
	return op;
}

static void gen_if_stmt(Node *node)
{
	int op = get_cond_op(node->cond);
	IR *code = new_ir(op);
	
	code->arg1 = gen_expr(node->cond->left);
	if (op != IR_EXPR_BRANCH)
		code->arg2 = gen_expr(node->cond->right);
	code->if_false = true;	// ifFalse arg1 op arg2 goto to_label
	emit_ir(code);
	if (node->stmt2 == NULL) {	// no else
		code->to_label = new_label();
		gen_stmt(node->stmt1);
		emit_label(code->to_label);
	}
	else {
		code->to_label = new_label();
		int if_exit_label = new_label();
		gen_stmt(node->stmt1);
		IR *t = new_ir(IR_GOTO); // goto if_exit_label
		t->to_label = if_exit_label;
		emit_ir(t);
		emit_label(code->to_label);
		gen_stmt(node->stmt2);
		emit_label(if_exit_label);
	}
}

static void gen_while_stmt(Node *node)
{
	int op = get_cond_op(node->cond);
	IR *code = new_ir(op);
	Reg *arg1 = NULL, *arg2 = NULL;
	int test_label = new_label();
	int exit_label = new_label();

	emit_label(test_label);
	code->arg1 = gen_expr(node->cond->left);
	if (op != IR_EXPR_BRANCH)
		code->arg2 = gen_expr(node->cond->right);
	code->to_label = exit_label;	// exit of while
	code->if_false = true;	// ifFalse arg1 op arg2 goto to_label
	emit_ir(code);
	gen_stmt(node->stmt1);
	IR *t = new_ir(IR_GOTO);
	t->to_label = test_label;
	emit_ir(t);
	emit_label(exit_label);
}

static void gen_for_init(Node *node)
{
	Reg *result = new_reg(REG_VAR);
	Reg *arg1 = gen_expr(node->left);
	result->symbol = node->symbol;
	IR *t = emit(IR_ASSIGN, arg1, NULL, result);
}

static void gen_for_inc(Node *node)
{
	gen_assign_stmt(node);
}

static void gen_for_stmt(Node *node)
{
	int op;
	int loop_start;
	IR *branch;

	gen_for_init(node->init);
	op = get_cond_op(node->cond);
	loop_start = new_label();
	emit_label(loop_start);
	branch = new_ir(op);
	branch->if_false = false;
	branch->to_label = loop_start;
	gen_stmt(node->stmt1);
	gen_for_inc(node->inc);
	branch->arg1 = gen_expr(node->cond->left);
	if (op != IR_EXPR_BRANCH)
		branch->arg2 = gen_expr(node->cond->right);
	emit_ir(branch);
}

static void gen_read_stmt(Node *node)
{
	IR *t = new_ir(IR_READ);
	Reg *arg;
	char *name = "scanf";
	t->name = malloc(sizeof(name + 1));
	strcpy(t->name, name);
	t->num_args = node->args->len;
	t->args = new_vec();
	for (int i = 0; i < node->args->len; i++) {
		Node *arg_node = vec_get(node->args, i);
		arg = new_reg(REG_VAR);
		arg->symbol = arg_node->symbol;
		vec_put(t->args, arg);
	}
	emit_ir(t);
}

static void gen_write_stmt(Node *node)
{
	IR *t = new_ir(IR_WRITE);
	char *name = "printf";
	t->name = malloc(sizeof(name) + 1);
	strcpy(t->name, name);
	if (node->left)
		t->arg1 = gen_expr(node->left);
	t->string_label = node->string_label;
	t->string = node->string;
	emit_ir(t);
}

static void gen_return_stmt(Node *node)
{
	IR *t;
	t = new_ir(IR_RETURN);
	if (node->left)
		t->arg1 = gen_expr(node->left);
	emit_ir(t);	// compute the expr fisrt.
}

static void gen_stmts(Node *node)
{
	Vector *stmts = node->stmts;
	if (stmts->len == 0) {
		return;
	}
	else if (stmts->len == 1) {
		gen_stmt(vec_get(stmts, 0));
		return;
	}
	else {
		gen_stmt(vec_get(stmts, 0));
	}
	for (int i = 1; i < stmts->len - 1; i++)
		gen_stmt(vec_get(stmts, i));
	gen_stmt(vec_get(stmts, stmts->len - 1));
}

static void gen_compound_stmt(Node *node) 
{
	gen_const_or_var_decl(node->const_decl);
	gen_const_or_var_decl(node->var_decl);
	gen_stmts(node->stmt1);
}

static void gen_stmt(Node *node)
{
	switch (node->nd_type) {
	case ND_IF: gen_if_stmt(node); break;
	case ND_WHILE: gen_while_stmt(node); break;
	case ND_FOR: gen_for_stmt(node); break;
	case ND_STMTS: gen_stmts(node); break;
	case ND_FUNC_CALL: gen_func_call(node); break;
	case ND_ASSIGN: gen_assign_stmt(node); break;
	case ND_READ_STMT: gen_read_stmt(node); break;
	case ND_WRITE_STMT: gen_write_stmt(node); break;
	case ND_RETURN_STMT: gen_return_stmt(node); break;
	}
}

static void gen_const_or_var_decl(Node *node)
{
	if (node == NULL)
		return;
	Vector *defs = node->defs;
	Node *def;
	Reg *result, *arg1 = NULL;
	IR *code;
	for (int i = 0; i < defs->len; i++) {
		def = vec_get(defs, i);
		result = new_reg(REG_VAR);
		result->symbol = def->symbol;
		if (def->nd_type == ND_CONST_DEF) {
			arg1 = new_reg(REG_NUM);
			arg1->value = def->value;
		}	// else ND_VAR_DEF
		code = emit(IR_DECL, arg1, NULL, result);
	}
}

bool last_ir_is_return()
{
	IR *t;
	for (int i = ir->len - 1; i >= 0; i--) {
		t = vec_get(ir, i);
		if (t->op == IR_LABEL)
			continue;
		return t->op == IR_RETURN;
	}
	return false;
}

static void gen_func_decl(Node *node)
{
	if (node == NULL)
		return;
	Reg *result = new_reg(REG_VAR);	// holds function symbol
	Reg *arg;
	IR *fd = new_ir(IR_FUNC_DECL);
	top = node->env;
	fd->env = top;
	fd->result = result;
	result->symbol = node->symbol;
	Vector *params_list = node->symbol->type->params_list;
	if (params_list) {
		fd->num_args = params_list->len;
		fd->args = new_vec();
		for (int i = 0; i < params_list->len; i++) {
			Node *n = vec_get(params_list, i);
			arg = new_reg(REG_VAR);
			arg->symbol = n->symbol;
			vec_put(fd->args, arg);
		}
	}
	fd->name = node->symbol->name;
	emit_ir(fd);
	gen_compound_stmt(node->stmt1);
	// Makes function always end with a return to make later analysis easy.
	if (!last_ir_is_return())
		emit_ir(new_ir(IR_RETURN));
	top = NULL;	// symbol table is valid in one function here, make it explicit
}

static void gen_main_func(Node *node)
{
	IR *t = new_ir(IR_FUNC_DECL);
	Reg *reg = new_reg(REG_VAR);
	top = node->env;
	t->env = top;
	reg->symbol = new_symbol("main", new_type(TYPE_FUNC));
	t->result = reg;
	t->name = reg->symbol->name;
	emit_ir(t);
	gen_compound_stmt(node->stmt1);
	top = NULL;
	// Makes function always end with a return to make later analysis easy.
	if (!last_ir_is_return())
		emit_ir(new_ir(IR_RETURN));
}

static void merge_labels()
{
	IR *l1, *l2, *t;
	for (int i = 0; i < ir->len - 1; i++) {
		l1 = vec_get(ir, i);
		l2 = vec_get(ir, i + 1);
		if (l1->op == IR_LABEL && l2->op == IR_LABEL) {
			vec_remove(ir, i + 1);
			for (int j = 0; j < ir->len; j++) {
				t = vec_get(ir, j);
				if (t->to_label == l2->b_label)
					t->to_label = l1->b_label;
			}
			i--;
		}
	}
}
