#include "1200cc.h"

// 已经写了基本块划分

static BB *new_bb(int label)
{
	BB *bb = calloc(1, sizeof(BB));
	bb->label = label;
	bb->ir = new_vec();
	bb->pred = new_vec();
	bb->succ = new_vec();
	bb->in_regs = new_vec();
	bb->out_regs = new_vec();
	return bb;
}

static bool is_branch(IR *t)
{
	return t->op == IR_EQ || t->op == IR_NE || t->op == IR_LS || t->op == IR_GT
		|| t->op == IR_LE || t->op == IR_GE || t->op == IR_EXPR_BRANCH
		|| t->op == IR_GOTO;
}

static void mark_leaders(Vector *ir, int start, int end)
{
	// find out all leaders
	IR *t, *t2;
	// The first TAC is a leader
	t = vec_get(ir, start);
	t->is_leader = true;
	for (int j = start; j <= end; j++) {
		t = vec_get(ir, j);
		if (is_branch(t)) {
			// Any instruction that immediately follows a conditional or 
			// unconditional jump is a leader
			t2 = vec_get(ir, j + 1);
			t2->is_leader = true;
			// Any instruction that is the target of a conditional or 
			// unconditional jump is a leader.
			for (int k = start; k <= end; k++) {
				t2 = vec_get(ir, k);
				if (t2->b_label == t->to_label)
					t2->is_leader = true;
			}
		}/* I am not quite sure...
		else if (t->op == IR_FUNC_CALL) {
			// each procedure call starts a new basic block
			t2 = vec_get(ir, j + 1);
			t2->is_leader = true;
		}*/
	}
}

/* The result is put into func_of_bb */
static void partition_function(Vector *func_of_bb, Vector *ir, int start, int end)
{
	int k = start;
	BB *bb;
	IR *t;
	while (k <= end) {
		bb = new_bb(0);
		vec_put(func_of_bb, bb);
		t = vec_get(ir, k);	// assert t->is_leader
		if (t->op == IR_LABEL) {
			bb->label = t->b_label;
			k++;
		}
		if (k <= end) {
			t = vec_get(ir, k++);
			vec_put(bb->ir, t);
		}
		while (k <= end) {
			t = vec_get(ir, k);
			if (t->is_leader)
				break;
			vec_put(bb->ir, t);
			k++;
		}
	}
	vec_put(func_of_bb, new_bb(0));	// exit BB
}

/* ir is Vector of IR* */
Program * partition_program(Vector *ir)
{
	Program *prog;
	IR *t;
	BB *bb;
	int i;
	int start = 0, end = 0;	// index of func_ir
	int ir_len = ir->len;

	prog = calloc(1, sizeof(Program));
	prog->global_vars = new_bb(0);
	prog->funcs = new_vec();	// Vector of Functions (Vector of BB)

	// find out global const or var decl
	for (i = 0; i < ir->len; i++) {
		t = vec_get(ir, i);
		if (t->op != IR_DECL)
			break;
		vec_put(prog->global_vars->ir, t);
	}

	// functions
	while (i < ir_len) {
		Vector *func_of_bb = new_vec();
		vec_put(prog->funcs, func_of_bb);

		// entry bb of function
		bb = new_bb(0);
		vec_put(func_of_bb, bb);
		t = vec_get(ir, i++);	// function declaration
		vec_put(bb->ir, t);
		while (i < ir->len) {
			t = vec_get(ir, i);
			if (t->op != IR_DECL)
				break;
			vec_put(bb->ir, t);
			i++;
		}

		if (t->op == IR_FUNC_DECL) {
			continue;
		}

		// find out the beginning and end of function
		start = i;
		while (i < ir->len) {
			t = vec_get(ir, i);
			if (t->op == IR_FUNC_DECL)
				break;
			i++;
		}

		end = i - 1;

		mark_leaders(ir, start, end);
		partition_function(func_of_bb, ir, start, end);
	}

	return prog;
}