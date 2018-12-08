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
				if (eq_oneof(5, former->op, '+', '-', '*', '/', IR_ARR_ACCESS)
					&& latter->op == IR_ASSIGN && former->result == latter->arg1
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

void optimization(Program * prog)
{
	if (prog == NULL)
		return;
	merge_redundant_assignment(prog);
}