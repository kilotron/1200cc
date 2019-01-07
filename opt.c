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

static void identify_return_value(Program *prog)
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
					&& latter->op == IR_RETURN && former->result && latter->arg1
					&& former->result->symbol == latter->arg1->symbol
					//&& former->result->symbol->flag & SYMBOL_LOCAL) {
					&& former->result->symbol->flag & SYMBOL_TEMP) {
					former->is_return_value = true; // remove later
					former->result->is_return_value = true;
				}
				former = latter;
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
						case '/': 
							if (t->arg2->value == 0) {
								// warningf(t->token, "division by zero");
								continue;
							}
							value = t->arg1->value / t->arg2->value; break;
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

static RIG_Node * get_node_in_graph(Vector *nodes, Symbol *s) {
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
			//printf("将寄存器%s分配给%s\n", name[i], node->symbol->name);
			return;
		}
	}
	//printf("%s未分配到寄存器\n", node->symbol->name);
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

typedef struct DAG_Node DAG_Node;
typedef struct DAG_Node {
	int op;			// 0 for leaf
	DAG_Node *left;
	DAG_Node *right;
	Vector *parents;// parent nodes, of type DAG_Node *
	Reg *leaf_reg;	// identifier or constant of leaf node
	bool is_killed;	// array access node may be killed
	Vector *regs;	// list of identifiers attached to this node
	Reg *reg;		// identifier for which TAC has been generated

	bool in_queue;
} DAG_Node;

/* Generate a string represent this identifer or constant. */
static char *get_key(Reg *r)
{
	char *key;
	if (r->type == REG_NUM) {
		key = stringf("%d", r->value);
	}
	else { // REG_VAR or REG_TEMP
		key = r->symbol->name;
	}
	return key;
}

/* Search node for identifier(or constant) in node_list, if the node is found 
   returns the node else returns NULL. */
static DAG_Node *get_leaf_node(Map *node_list, Reg *r)
{
	char *key = get_key(r);
	DAG_Node *n = map_get(node_list, key);
	return n;
}

/* Pre-conditions: node for r is not found in node_list.
   Make a node for r and add it to the graph and node_list.*/
static DAG_Node * make_leaf_node(Vector *nodes, Map *node_list, Reg *r)
{
	char *key = get_key(r);
	DAG_Node *n = calloc(1, sizeof(DAG_Node));
	n->parents = new_vec();
	n->leaf_reg = r;
	n->reg = r;
	n->is_killed = false;
	n->regs = new_vec();
	vec_put(nodes, n);
	map_put(node_list, key, n);
	return n;
}

/* Search an interior node in graph. A killed node should not be returned. */
static DAG_Node *get_interior_node(Vector *nodes, int op, DAG_Node *left, DAG_Node *right)
{
	if (op == IR_ASSIGN_ARR)
		return NULL;	// case for: a[i] = t;b[i] = t; 
	for (int i = 0; i < nodes->len; i++) {
		DAG_Node *n = vec_get(nodes, i);
		if (n->op == op && n->left == left && n->right == right && !n->is_killed)
			return n;
	}
	return NULL;
}

/* Add an interior node to DAG. If this node is an array assignment, kill nodes
   related to this node in graph.*/
static DAG_Node * make_interior_node(Vector *nodes, IR *t, DAG_Node *left, DAG_Node *right)
{
	DAG_Node *interior_node = calloc(1, sizeof(DAG_Node));

	interior_node->op = t->op;
	interior_node->parents = new_vec();
	interior_node->left = left;
	interior_node->right = right;
	interior_node->is_killed = false;
	interior_node->regs = new_vec();
	vec_put(left->parents, interior_node);
	if (right)
		vec_put(right->parents, interior_node);
	vec_put(nodes, interior_node);

	if (t->op == IR_ASSIGN_ARR) {
		for (int i = 0; i < nodes->len; i++) {
			DAG_Node *n = vec_get(nodes, i);
			if (n->op == IR_ARR_ACCESS
				&& n->left->leaf_reg->symbol == t->result->symbol)
				n->is_killed = true;
		}
	}
	return interior_node;
}

/* Pre-conditions: ir contains only arithmetic, array access or array
   assignment TAC. */
static Vector * DAG(Vector *ir)
{
	Map *node_list = new_map();
	Vector *nodes = new_vec();	// nodes in graph
	DAG_Node *left_node = NULL, *right_node = NULL, *interior_node = NULL;
	DAG_Node *node_prev;

	for (int i = 0; i < ir->len; i++) {
		IR *t = vec_get(ir, i);
		right_node = NULL;
		if ((left_node = get_leaf_node(node_list, t->arg1)) == NULL)
			left_node = make_leaf_node(nodes, node_list, t->arg1);
		if (t->arg2) {
			if ((right_node = get_leaf_node(node_list, t->arg2)) == NULL)
				right_node = make_leaf_node(nodes, node_list, t->arg2);
		}
		interior_node = get_interior_node(nodes, t->op, left_node, right_node);
		if (interior_node == NULL) {
			interior_node = make_interior_node(nodes, t, left_node, right_node);
		}
		vec_put(interior_node->regs, t->result);
		if (t->op == IR_ASSIGN_ARR) {
			continue;	// do not add array assignment to node_list
		}
		// update node_list
		node_prev = map_get(node_list, get_key(t->result));
		if (node_prev) // t->result is in node_prev->regs
			vec_remove_elem(node_prev->regs, t->result);
		map_put(node_list, get_key(t->result), interior_node);
	}

	return nodes;
}

static bool not_all_interior_nodes_in_queue(Vector *nodes)
{
	for (int i = 0; i < nodes->len; i++) {
		DAG_Node *n = vec_get(nodes, i);
		if (n->op != 0 && !n->in_queue) // op == 0: leaf node
			return true;
	}
	return false;
}

/* If all parents of n is in queue or n has no parents returns true. */
static bool all_parents_in_queue(DAG_Node *n)
{
	for (int i = 0; i < n->parents->len; i++) {
		DAG_Node *parent = vec_get(n->parents, i);
		if (!parent->in_queue)
			return false;
	}
	return true;
}

/* Returns the sequence of computing DAG_Node*. */
static Vector *get_compute_sequence(Vector *nodes)
{
	Vector *queue = new_vec(), *result = new_vec();
	DAG_Node *n = NULL;

	for (int i = 0; i < nodes->len; i++) {
		n = vec_get(nodes, i);
		if (n->op != 0) {
			vec_put(result, n);
		}
	}

	/*// Comment out the following code if the original order of TACs is important.
	for (int i = 0; i < nodes->len; i++) {
		n = vec_get(nodes, i);
		n->in_queue = false;
	}
	while (not_all_interior_nodes_in_queue(nodes)) {
		for (int i = nodes->len - 1; i >= 0; i--) { // don't change the order
			n = vec_get(nodes, i);
			if (n->op != 0 && !n->in_queue && all_parents_in_queue(n))
				break;
		}
		
		vec_put(queue, n);
		n->in_queue = true;
		while (n->left && n->left->op != 0) {// n has interior left node
			n = n->left;
			n->in_queue = true;
			vec_put(queue, n);
		}
		
	}

	for (int i = queue->len - 1; i >= 0; i--) {
		vec_put(result, vec_get(queue, i));
	}*/
	
	return result;
}

static IR * regen_ir(Vector *ir, int op, Reg *arg1, Reg *arg2, Reg *result)
{
	IR *i = new_ir(op);
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->result = result;
	vec_put(ir, i);
	return i;
}

/* Pre-conditions: regs != NULL, bb_out != NULL, len(regs) > 0
	Return a variable that is live from exit of bb if possible.*/
static Reg *var_worth_gen_code_for(Vector *regs, Vector *bb_out)
{
	Reg *r = NULL;
	for (int i = 0; i < regs->len; i++) {
		r = vec_get(regs, i);
		if (vec_is_in(bb_out, r->symbol) || !(r->symbol->flag & SYMBOL_LOCAL)
			|| r->is_return_value)
			return r;
	}
	return vec_get(regs, 0);
}

/* Pre-conditions: regs != NULL, bb_out != NULL, len(regs) > 0
	Return variables live from exit of bb.*/
static Vector *vars_need_gen_code_for(Vector *regs, Vector *bb_out)
{
	Reg *r = NULL;
	Vector *result = new_vec();
	for (int i = 0; i < regs->len; i++) {
		r = vec_get(regs, i);
		// live or globals
		if (vec_is_in(bb_out, r->symbol) || !(r->symbol->flag & SYMBOL_LOCAL)
			|| r->is_return_value)
			vec_put(result, r);
	}
	return result;
}

static Vector *vars_not_gen_code_for(Vector *regs, Vector *bb_out)
{
	return vec_except(regs, vars_need_gen_code_for(regs, bb_out));
}

/* Pre-conditions: ir contains only arithmetic, array access or array
   assignment TAC. 
   lcse: local common subexpressions*/
static Vector *regenerate(Vector *ir, BB *bb)
{
	Vector *nodes = DAG(ir);
	Vector *seq = get_compute_sequence(nodes);
	Vector *ir_result = new_vec();
	Vector *rest;
	DAG_Node *node;
	Reg *arg1, *arg2, *result;

	for (int i = 0; i < seq->len; i++) {
		node = vec_get(seq, i);
		arg1 = node->left->reg;
		arg2 = node->right ? node->right->reg : NULL;

		result = var_worth_gen_code_for(node->regs, bb->out_regs);
		regen_ir(ir_result, node->op, arg1, arg2, result);
		node->reg = result;

		// generate TAC for rest vars
		rest = vec_clone(node->regs);
		vec_remove_elem(rest, result);
		rest = vars_need_gen_code_for(rest, bb->out_regs);
		for (int j = 0; j < rest->len; j++) {
			Reg *r = vec_get(rest, j);
			regen_ir(ir_result, IR_ASSIGN, result, NULL, r);
		}

		// 替换没有生成代码的变量
		rest = vars_not_gen_code_for(rest, bb->out_regs);
		for (int j = 0; j < rest->len; j++) {
			Reg *r = vec_get(rest, j);
			for (int k = 0; k < bb->ir->len; k++) {
				IR *t = vec_get(bb->ir, k);
				if (t->arg1 && t->arg1->symbol == r->symbol)
					t->arg1 = result;
				if (t->arg2 && t->arg2->symbol == r->symbol)
					t->arg2 = result;
				if (t->op != IR_READ) {
					for (int l = 0; l < t->num_args; l++) {
						Reg *arg = vec_get(t->args, l);
						if (arg->symbol == r->symbol) {
							t->args->data[l] = result;
						}
					}
				}
			}
		}
	}
	return ir_result;
}

/* Pre-conditions: data flow analysis is done.*/
static void eliminate_lcse(Program *prog)
{
	extern bool lcse_elimination_ON;
	if (!lcse_elimination_ON)
		return;
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			for (int k = 0; k < bb->ir->len; k++) {
				IR *t = vec_get(bb->ir, k);
				if (eq_oneof(7, t->op, '+', '-', '*', '/', IR_ARR_ACCESS,
					IR_ASSIGN_ARR, IR_ASSIGN)) {

					Vector *cand = new_vec();
					Vector *regen_code;
					int start_index = k, end_index;

					for (; k < bb->ir->len; k++) {
						t = vec_get(bb->ir, k);
						if (!eq_oneof(7, t->op, '+', '-', '*', '/', IR_ARR_ACCESS,
							IR_ASSIGN_ARR, IR_ASSIGN)) {
							break;
						}
						vec_put(cand, t);
					}
					end_index = k - 1;

					while (start_index <= end_index) {
						vec_remove(bb->ir, start_index);
						end_index--;
					}

					regen_code = regenerate(cand, bb);

					for (int j = 0; j < regen_code->len; j++) {
						IR *t = vec_get(regen_code, j);
						vec_insert(bb->ir, t, start_index + j);
					}
				}
				
			}
		}
	}
	// analyze data flow again
	data_flow_analysis(prog);
}

/* Pre-conditions: data flow analysis is done.*/
static void eliminate_dead_code(Program *prog)
{
	extern bool dead_code_elimination_ON;
	if (!dead_code_elimination_ON)
		return;
	bool removed =true;
	while (removed) {
		removed = false;
		for (int i = 0; i < prog->funcs->len; i++) {
			Vector *func_of_bb = vec_get(prog->funcs, i);
			for (int j = 0; j < func_of_bb->len; j++) {
				BB *bb = vec_get(func_of_bb, j);
				for (int k = 0; k < bb->ir->len; k++) {
					IR *t = vec_get(bb->ir, k);
					if (!eq_oneof(5, t->op, '+', '-', '*', '/', IR_ASSIGN))
						continue;
					if (!vec_is_in(t->out, t->result->symbol) 
						&& (t->result->symbol->flag & SYMBOL_LOCAL)) {
						vec_remove(bb->ir, k--);
						removed = true;
					}
				}
			}
		}
		data_flow_analysis(prog);
	}
}

void optimization(Program * prog)
{
	if (prog == NULL)
		return;
	merge_redundant_assignment(prog);
	data_flow_analysis(prog);
	eliminate_dead_code(prog);
	eliminate_lcse(prog);
	evaluate_constant_expressions(prog);
	identify_return_value(prog);
	alloc_saved_reg(prog);
}

