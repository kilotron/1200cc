#include "1200cc.h"

/* In a basic block, if rhs of the latter instruction is lhs of the former 
   instruction and the lhs of the former is a temporary variable, 
   i.e. t = a + b; c = t, merge them to c = a + b(Make sure t is not used elsewhere).
   Pre-conditions: No sequence like 't = a + b; c = t; d = t' is in ir.*/
static void merge_redundant_assignment(Program *prog)
{
	IR *former, *latter;
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			former = vec_get(bb->ir, 0);
			if (former == NULL)	// empty bb
				continue;

			for (int k = 1; k < bb->ir->len; k++) {
				latter = vec_get(bb->ir, k);
				if (eq_oneof(6, former->op, '+', '-', '*', '/', IR_ARR_ACCESS, IR_FUNC_CALL)
					&& latter->op == IR_ASSIGN && former->result 
					&& former->result == latter->arg1
					&& former->result->symbol->flag & SYMBOL_TEMP) {
					former->result = latter->result;
					vec_remove(bb->ir, k);	// remove the latter
					former = vec_get(bb->ir, k);
				}
				else {
					former = latter;
				}
			}
		}
	}
}

static void evaluate_constant_expressions(Program *prog)
{
	extern bool constant_folding_ON;
	if (!constant_folding_ON)
		return;
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			for (int k = 0; k < bb->ir->len; k++) {
				IR *t = vec_get(bb->ir, k);
				if (!eq_oneof(4, t->op, '+', '-', '*', '/'))
					continue;
				if (t->arg2) { // two operands
					if (t->arg1->type == REG_NUM && t->arg2->type == REG_NUM &&
						t->result->symbol->flag & SYMBOL_TEMP) {
						int value;
						switch (t->op) {
						case '+': value = t->arg1->value + t->arg2->value; break;
						case '-': value = t->arg1->value - t->arg2->value; break;
						case '*': value = t->arg1->value * t->arg2->value; break;
						case '/': value = t->arg1->value / t->arg2->value; break;
						}
						t->result->type = REG_NUM;
						t->result->value = value;
						vec_remove(bb->ir, k--);
					}
				}
				else {	// IR_ADD or IR_SUB
					if (t->arg1->type == REG_NUM && t->result->symbol->flag & SYMBOL_TEMP) {
						int value = t->arg1->value;
						if (t->op == IR_SUB)
							value = -value;
						t->result->type = REG_NUM;
						t->result->value = value;
						vec_remove(bb->ir, k--);
					}
				}
			}
		}
	}
}

typedef struct {
	Symbol *symbol;
	Vector *adj;	// vector of RIG_Node*
} RIG_Node;	// register interference graph

RIG_Node * get_node_in_graph(Vector *nodes, Symbol *s) {
	RIG_Node *node;
	for (int i = 0; i < nodes->len; i++) {
		node = vec_get(nodes, i);
		if (s == node->symbol)
			return node;
	}
	node = calloc(1, sizeof(RIG_Node));
	node->symbol = s;
	node->adj = new_vec();
	vec_put(nodes, node);
	return node;
}

static Vector * var_live_on_exit_from_bb(Vector *func_of_bb)
{
	Vector *live_var = new_vec();
	for (int i = 0; i < func_of_bb->len; i++) {
		BB *bb = vec_get(func_of_bb, i);
		vec_appendv(live_var, bb->out_regs);	// duplicates may be in live_var.
	}
	vec_remove_duplicates(live_var);
	return live_var;
}

/* Local variables or temporary variables that are live
   on exit from the block are candidates for allocating registers. Constants, 
   globals, parameters or arrays are not candidates.*/
static bool is_cand_var(Symbol *s, Vector *var_live)
{	
	return (s->flag & SYMBOL_LOCAL)
		&& !eq_oneof(3, s->type->type, TYPE_CONST_CHAR, TYPE_CONST_INT, TYPE_ARRAY)
		&& !(s->flag & SYMBOL_PARAM)
		&& vec_is_in(var_live, s);
}

static Vector * RIG(Vector *func_of_bb)
{
	Vector *nodes = new_vec();
	RIG_Node *node1, *node2;
	Vector *var_live = var_live_on_exit_from_bb(func_of_bb);
	for (int i = 0; i < func_of_bb->len; i++) {
		BB *bb = vec_get(func_of_bb, i);
		for (int j = 0; j < bb->ir->len; j++) {
			IR *t = vec_get(bb->ir, j);
			for (int k = 0; k < t->def->len; k++) {
				Symbol *def = vec_get(t->def, k);
				if (!is_cand_var(def, var_live))
					continue;
				node1 = get_node_in_graph(nodes, def);
				for (int l = 0; l < t->out->len; l++) {
					Symbol *live = vec_get(t->out, l);
					if (!is_cand_var(live, var_live)
						|| def == live)
						continue;
					node2 = get_node_in_graph(nodes, live);
					vec_put(node1->adj, node2);
					vec_put(node2->adj, node1);
					//printf("%s和%s冲突\n", def->name, live->name);
				}
			}
		}
	}
	return nodes;
}

static void RIG_remove_node(Vector *nodes, RIG_Node *node)
{
	RIG_Node *other;
	for (int i = 0; i < node->adj->len; i++) {
		other = vec_get(node->adj, i);
		vec_remove_elem(other->adj, node);
	}
	vec_remove_elem(nodes, node);
}

static void assign_reg(RIG_Node *node)
{
	bool assigned[32];
	RIG_Node *other;
	extern char *name[];

	for (int i = REG_S0; i <= REG_S7; i++) {
		assigned[i] = false;
	}
	for (int i = 0; i < node->adj->len; i++) {
		other = vec_get(node->adj, i);
		if (is_saved_reg(other->symbol->addr_des.reg_num))
			assigned[other->symbol->addr_des.reg_num] = true;
	}
	for (int i = REG_S0; i <= REG_S7; i++) {
		if (!assigned[i]) {
			node->symbol->addr_des.reg_num = i;
			printf("将寄存器%s分配给%s\n", name[i], node->symbol->name);
			return;
		}
	}
	printf("%s未分配到寄存器\n", node->symbol->name);
}

static void alloc_local_var(Vector *func_of_bb)
{
#define NUM_REGS 8
	Vector *nodes = RIG(func_of_bb);
	Vector *copy = RIG(func_of_bb);	// don't worry about efficiency at the moment
	Vector *stack = new_vec();
	bool removed;
	RIG_Node *node;

	// simplify
	while (nodes->len > 0) {
		removed = false;
		for (int i = 0; i < nodes->len; i++) {
			node = vec_get(nodes, i);
			if (node->adj->len < NUM_REGS) {
				vec_put(stack, get_node_in_graph(copy, node->symbol));
				RIG_remove_node(nodes, node);
				removed = true;
				break;
			}
		}
		if (removed)
			continue;
		// TODO(improve): Use heuristics to determine what to spill.
		node = vec_get(nodes, 0);
		vec_put(stack, get_node_in_graph(copy, node->symbol));
		RIG_remove_node(nodes, node);
	}

	// select
	for (int i = stack->len - 1; i >= 0; i--) {
		node = vec_get(stack, i);
		assign_reg(node);
	}
}

static void alloc_saved_reg(Program *prog) {
	extern bool saved_reg_alloc_ON;
	if (!saved_reg_alloc_ON)
		return;
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		//printf("函数开始\n");
		alloc_local_var(func_of_bb);
		//printf("函数结束\n");
	}
}

void test(Program *prog)
{
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			//print_regs_of_bb(bb, PRINT_DEF | PRINT_USE | PRINT_IN_REGS | PRINT_OUT_REGS);
		}
	}
}

void optimization(Program * prog)
{
	if (prog == NULL)
		return;
	merge_redundant_assignment(prog);
	data_flow_analysis(prog);
	evaluate_constant_expressions(prog);
	//test(prog);
	alloc_saved_reg(prog);
}

