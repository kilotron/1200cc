#include "1200cc.h"

static BB *new_bb(int label)
{
	BB *bb = calloc(1, sizeof(BB));
	bb->label = label;
	bb->ir = new_vec();
	bb->pred = new_vec();
	bb->succ = new_vec();
	bb->in_regs = new_vec();
	bb->out_regs = new_vec();
	bb->def = new_vec();
	bb->use = new_vec();
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
		}/* I am not quite sure... */
		else if (t->op == IR_FUNC_CALL) {
			// each procedure call starts a new basic block
			t2 = vec_get(ir, j + 1);
			if ((t2->op == IR_ASSIGN && t2->arg1 == t->result)
				|| (t2->op == IR_RETURN && t2->arg1 && t2->arg1 == t->result))
				t2 = vec_get(ir, j + 2);	//function always ends with a ret, so t2!=NULL
			if (t2 != NULL)
				t2->is_leader = true;
		}
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
	if (ir == NULL)
		return NULL;
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

		// add declarations to the first bb of this function
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

/* Pre-conditions: label has a corresponding BB in func_of_bb.
   Post-conditions: returns the corresponding BB*. */
BB *label2BB(Vector *func_of_bb, int label)
{
	BB *bb;
	for (int i = 0; i < func_of_bb->len; i++) {
		bb = vec_get(func_of_bb, i);
		if (bb->label == label)
			return bb;
	}
	return NULL;
}

void get_pred_and_succ_of_bb(Program *prog)
{
	Vector *func_of_bb;
	BB *bb1, *bb2, *to_bb;
	IR *last_ir;
	for (int i = 0; i < prog->funcs->len; i++) {
		func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			bb1 = vec_get(func_of_bb, j);
			bb2 = vec_get(func_of_bb, j + 1);
			last_ir = vec_get(bb1->ir, bb1->ir->len - 1);
			if (last_ir == NULL)
				continue;	// empty BB
			switch (last_ir->op) {
			case IR_LS: case IR_LE: case IR_GT: case IR_GE: case IR_NE:
			case IR_EQ: case IR_EXPR_BRANCH:	// conditional branch
				if (bb2 != NULL) {
					vec_put(bb1->succ, bb2);
					vec_put(bb2->pred, bb1);
				}	// no break here	
			case IR_GOTO:	// unconditional branch
				to_bb = label2BB(func_of_bb, last_ir->to_label);
				vec_put(bb1->succ, to_bb);
				vec_put(to_bb->pred, bb1);
				break;
			default:
				if (bb2 != NULL) {
					vec_put(bb1->succ, bb2);
					vec_put(bb2->pred, bb1);
				}
				break;
			}
		}
	}
}

///// Beginning of live variable analysis

static void compute_def_use_of_bb(BB *bb)
{
	IR *t;
	for (int i = 0; i < bb->ir->len; i++) {
		t = vec_get(bb->ir, i);
		if (t->result && (t->op != IR_FUNC_DECL))
			vec_put(t->def, t->result->symbol);
		switch (t->op) {
		case IR_ASSIGN: case IR_TIMES: case IR_DIV: case IR_ADD: case IR_SUB:
		case IR_LS: case IR_LE: case IR_GT: case IR_GE: case IR_EQ: case IR_NE:
		case IR_EXPR_BRANCH: case IR_RETURN: case IR_WRITE: case IR_ASSIGN_ARR:
		case IR_ARR_ACCESS:
			if (t->arg1 && t->arg1->symbol && !vec_is_in(vec_union(bb->use, bb->def), t->arg1->symbol))
				vec_put(bb->use, t->arg1->symbol);
			if (t->arg2 && t->arg2->symbol && !vec_is_in(vec_union(bb->use, bb->def), t->arg2->symbol))
				vec_put(bb->use, t->arg2->symbol);
			if (t->result && !vec_is_in(vec_union(bb->use, bb->def), t->result->symbol)) {
				if (t->op == IR_ASSIGN_ARR)
					vec_put(bb->use, t->result->symbol);
				else
					vec_put(bb->def, t->result->symbol);
			}
			break;
		case IR_FUNC_CALL:
			if (t->args)
				for (int i = 0; i < t->num_args; i++) {
					Reg *arg = vec_get(t->args, i);
					if (arg->symbol && !vec_is_in(vec_union(bb->use, bb->def), arg->symbol))
						vec_put(bb->use, arg->symbol);
				}
			if (t->result && !vec_is_in(vec_union(bb->use, bb->def), t->result->symbol)) {
				vec_put(bb->def, t->result->symbol);
			}
			break;
		case IR_READ:
			for (int i = 0; i < t->num_args; i++) {
				Reg *arg = vec_get(t->args, i);
				if (arg->symbol && !vec_is_in(vec_union(bb->use, bb->def), arg->symbol)) {
					vec_put(bb->def, arg->symbol);
				}
				if (arg->symbol)
					vec_put(t->def, arg->symbol);
			}
			break;
		case IR_DECL:	// 常量
			if (t->arg1 && !vec_is_in(vec_union(bb->use, bb->def), t->result->symbol)) {
				vec_put(bb->def, t->result->symbol);
			}
			break;
		case IR_FUNC_DECL:	// 参数的赋值需要
			for (int i = 0; i < t->num_args; i++) {
				Reg *arg = vec_get(t->args, i);
				if (arg->symbol)
					vec_put(t->def, arg->symbol);
			}
			break;
		} // default: IR_LABEL, IR_GOTO
	}
}

static void compute_use_def(Program *prog)
{
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			compute_def_use_of_bb(bb);
		}
	}
}

/* Pre-conditions: compute_use_def() has been called.*/
static void live_variable_analysis(Program *prog)
{
	Vector *func_of_bb;
	BB *bb, *succ_of_bb;
	Vector *in_regs_prev;
	bool in_regs_changed;
	for (int i = 0; i < prog->funcs->len; i++) {
		func_of_bb = vec_get(prog->funcs, i);
		in_regs_changed = true;
		while (in_regs_changed) {
			in_regs_changed = false;
			for (int j = func_of_bb->len - 1; j >= 0; j--) {
				bb = vec_get(func_of_bb, j);
				bb->out_regs = new_vec();
				for (int k = 0; k < bb->succ->len; k++) {
					succ_of_bb = vec_get(bb->succ, k);
					bb->out_regs = vec_union(bb->out_regs, succ_of_bb->in_regs);
				}
				in_regs_prev = bb->in_regs;
				bb->in_regs = vec_union(bb->use, vec_except(bb->out_regs, bb->def));
				if (vec_is_different(bb->in_regs, in_regs_prev))
					in_regs_changed = true;
				//print_regs_of_bb(bb, PRINT_DEF | PRINT_USE | PRINT_IN_REGS | PRINT_OUT_REGS);
			}
		}
	}
}

///// End of live variable analysis

///// Beginning of computing next-use information


/* Mark all symbols in env as 'not live'. Mark symbols in out_regs as 'live'.
   This function should be called before analysis of a basic block. */
static void reset_env(Env *env, BB *globals, Vector *out_regs)
{
	Vector *symbols = env->symbols->values;
	Symbol *s;
	for (int i = 0; i < symbols->len; i++) {
		s = vec_get(symbols, i);
		if (vec_is_in(out_regs, s))
			s->live = true;
		else
			s->live = false;
	}
	for (int i = 0; i < globals->ir->len; i++) {
		IR *t = vec_get(globals->ir, i);
		s = t->result->symbol;
		if (vec_is_in(out_regs, s))
			s->live = true;
		else
			s->live = false;
	}
}

/*Excluding globals.*/
static Vector * get_live_var(Env *env)
{
	Vector *symbols = env->symbols->values;
	Symbol *s;
	Vector *live = new_vec();
	for (int i = 0; i < symbols->len; i++) {
		s = vec_get(symbols, i);
		if (s->live) {
			vec_put(live, s);
		}
	}
	return live;
}

/* Reference:
https://www2.cs.arizona.edu/~collberg/Teaching/453/2009/Handouts/Handout-21.pdf
*/
void get_next_use_info(Program *prog)
{
	Vector *func_of_bb;
	Env *env;
	BB *bb;
	IR *t;

	for (int i = 0; i < prog->funcs->len; i++) {
		func_of_bb = vec_get(prog->funcs, i);
		bb = vec_get(func_of_bb, 0);
		t = vec_get(bb->ir, 0);	// the first ir of first bb is of type IR_FUNC_DECL
		env = t->env;
		for (int j = 0; j < func_of_bb->len; j++) {
			bb = vec_get(func_of_bb, j);
			reset_env(env, prog->global_vars, bb->out_regs);
			for (int k = bb->ir->len - 1; k >= 0; k--) {
				t = vec_get(bb->ir, k);
				/* 1.Attach to statement t the information currently found in
					the symbol table. If t->argi->symbol is NULL, t->argi is a literal.
					A literal is not live.
				   2.In the symbol table, update next-use information.*/
				switch (t->op) {
				case IR_DECL: case IR_GOTO: case IR_LABEL:
					break;	// do nothing
				case IR_FUNC_DECL:
					t->out = get_live_var(env);
					break;
				case IR_FUNC_CALL:
					t->out = get_live_var(env);
					if (t->result) { // with ret value
						if (t->result->symbol->live)
							vec_put_if_not_in(t->next_use, t->result->symbol);
						t->result->symbol->live = false;
					}
					for (int i = 0; i < t->num_args; i++) {
						Reg *arg = vec_get(t->args, i);
						if (arg->symbol) {	// symbol is NULL if arg is a literal
							if (arg->symbol->live)
								vec_put_if_not_in(t->next_use, arg->symbol);
							arg->symbol->live = true;
						}
					}
					break;
				case IR_READ:
					t->out = get_live_var(env);
					for (int i = 0; i < t->args->len; i++) {
						Reg *arg = vec_get(t->args, i);
						if (arg->symbol->live)
							vec_put_if_not_in(t->next_use, arg->symbol);
						arg->symbol->live = false;
					}
					break;
				default:
					t->out = get_live_var(env);
					if (t->result) {
						if (t->result->symbol->live)
							vec_put_if_not_in(t->next_use, t->result->symbol);
						t->result->symbol->live = t->op == IR_ASSIGN_ARR ? true : false;
					}
					if (t->arg1 && t->arg1->symbol) {
						if (t->arg1->symbol->live)
							vec_put_if_not_in(t->next_use, t->arg1->symbol);
						t->arg1->symbol->live = true;
					}
					if (t->arg2 && t->arg2->symbol) {
						if (t->arg2->symbol->live)
							vec_put_if_not_in(t->next_use, t->arg2->symbol);
						t->arg2->symbol->live = true;
					}
					break;
				}
			}
		}
	}
}

///// End of computing next-use information

void print_regs_of_bb(BB *bb, int print_option)
{
	FILE *fp = stdout;
	Symbol *s;
	static int cnt = 0;
	fprintf(fp, "\nBB%d begin: ", ++cnt);
	if (bb->label)
		fprintf(fp, "L%d ", bb->label);
	fprintf(fp, "\n+------------------+\n");

	if (print_option & PRINT_USE) {
		fprintf(fp, "use:\n");
		for (int i = 0; i < bb->use->len; i++) {
			s = vec_get(bb->use, i);
			fprintf(fp, "%5s ", s->name);
		}
		fprintf(fp, "\n");
	}

	if (print_option & PRINT_DEF) {
		fprintf(fp, "\ndef:\n");
		for (int i = 0; i < bb->def->len; i++) {
			s = vec_get(bb->def, i);
			fprintf(fp, "%5s ", s->name);
		}
		fprintf(fp, "\n");
	}

	if (print_option & PRINT_IN_REGS) {
		fprintf(fp, "\nin_regs:\n");
		for (int i = 0; i < bb->in_regs->len; i++) {
			s = vec_get(bb->in_regs, i);
			fprintf(fp, "%5s ", s->name);
		}
		fprintf(fp, "\n");
	}

	if (print_option & PRINT_OUT_REGS) {
		fprintf(fp, "\nout_regs:\n");
		for (int i = 0; i < bb->out_regs->len; i++) {
			s = vec_get(bb->out_regs, i);
			fprintf(fp, "%5s ", s->name);
		}
		fprintf(fp, "\n");
	}	
	
	fprintf(fp, "\n+------------------+\nBB end\n\n");
}

void reset_data_flow(Program *prog)
{
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++) {
			BB *bb = vec_get(func_of_bb, j);
			bb->def = new_vec();
			bb->use = new_vec();
			bb->out_regs = new_vec();
			bb->in_regs = new_vec();
			for (int k = 0; k < bb->ir->len; k++) {
				IR *t = vec_get(bb->ir, k);
				t->next_use = new_vec();
				t->out = new_vec();
				t->def = new_vec();
			}
		}
	}
}

void data_flow_analysis(Program *prog)
{
	reset_data_flow(prog);
	get_pred_and_succ_of_bb(prog);
	compute_use_def(prog);
	live_variable_analysis(prog);
	get_next_use_info(prog);
}

