#include "1200cc.h"
#define OFF_FIRST_SAVED_REG 8	// first saved register's offset from fp 
#define FLUSH_LOCAL 0x1
#define FLUSH_GLOBAL 0x2
#define LOCAL_NOT_SPILL 0x4

#define SPILL_LOCAL 0x1
#define SPILL_PARAM 0x2
#define SPILL_GLOBAL 0x4
#define SPILL_ALL (SPILL_LOCAL | SPILL_PARAM | SPILL_GLOBAL)

extern bool live_variable_analysis_ON;
extern bool saved_reg_alloc_ON;
static int offset_of_first_var_in_stack_from_fp;	// always non-negative.
static int spill = REG_T0;	// temporary register to spill
static FILE *fp;
static int print_option;
static bool lhs_reg_got;

char *name[] = { 
	"$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
	"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
	"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
	"$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra" 
};
Reg_Des *reg_des;
Env *env;			// to access symbols
bool is_main_func = false;
bool is_leaf;		// to keep track of function calls

static void print_env(Env *env)
{
	for (int i = 0; i < env->symbols->keys->len; i++) {
		char *key = vec_get(env->symbols->keys, i);
		Symbol *s = map_get(env->symbols, key);
		printf("name: %s, offset: %d\n", key, s->offset);
	}
}

/* A newline is added to the end of this instruction. */
static void emitl(char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	if (print_option & PRINT_TO_CONSOLE) {
		vprintf(fmt, va);
		printf("\n");
	}
	if (print_option & PRINT_TO_FILE) {
		vfprintf(fp, fmt, va);
		fprintf(fp, "\n");
	}
	va_end(va);
}

/* A tab is added to the beginning of this instruction. A newline is added to 
   the end of this instruction. */
static void emit(char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	if (print_option & PRINT_TO_CONSOLE) {
		printf("\t");
		vprintf(fmt, va);
		printf("\n");
	}
	if (print_option & PRINT_TO_FILE) {
		fprintf(fp, "\t");
		vfprintf(fp, fmt, va);
		fprintf(fp, "\n");
	}
	va_end(va);
}

static void comment(char *fmt, ...)
{
	extern bool comment_ON;
	if (!comment_ON)
		return;
	va_list va;
	va_start(va, fmt);
	if (print_option & PRINT_TO_CONSOLE) {
		printf("\t# ");
		vprintf(fmt, va);
		printf("\n");
	}
	if (print_option & PRINT_TO_FILE) {
		fprintf(fp, "\t# ");
		vfprintf(fp, fmt, va);
		fprintf(fp, "\n");
	}
	va_end(va);
}

/* Initialize register descpritor: all saved registers and temporary registers
   are free. This function should be called before each function declaration.
   and before generating mips instructions. */
static void reg_des_init()
{
	reg_des = calloc(1, sizeof(Reg_Des));
	reg_des->saved_reg = new_map();
	reg_des->temp_reg = new_map();
}

/* Reg reg_num is temporary register.*/
static void reg_des_bind(int reg_num, Symbol *s)
{
	map_put(reg_des->temp_reg, name[reg_num], s);
}

static void reg_des_unbind(int reg_num)
{
	map_put(reg_des->temp_reg, name[reg_num], NULL);
}

static bool is_temp_reg(int reg_num)
{
	return (reg_num >= REG_T0 && reg_num <= REG_T7) || reg_num == REG_T8 || reg_num == REG_T9;
}

bool is_saved_reg(int reg_num)
{
	return (reg_num >= REG_S0 && reg_num <= REG_S7);
}

/* Pre-conditions: symbol != NULL. 
    Register reg_num has been allocated to the symbol.
	Symbol is of type TYPE_INT, TYPE_CHAR, TYPE_ARRAY, TYPE_CONST_INT
	or TYPE_CONST_CHAR. symbol cannot be of type TYPE_FUNC or TYPE_VOID.
   
   Post-condtions: Register reg_num has up-to-date value of this symbol.
   The symbol is marked as 'in-mem' in its address descriptor.
   
   symbol can be global or local. For symbols in different scope and of 
   different type, we have different ways to load them to register.
				   |--------|
				   |  ...   |
				   |--------|
				   |param 1 |
				   |--------|
				   |param 0 |
				   |--------|
			  fp-> |   fp   |
				   |--------|
				   |   ra   |
				   |--------|
				   | s0...  |
				   |--------|
			  sp-> | local  |
				   |--------|
   1.paramters: fp-relative addressing. Note that paramters are above fp,
     so offset is a positive integer. symbol->offset is offset from first
	 parameter.
   2.local constants: simply load its value to the reg.
     local variables: fp-relative addressing. offset_of_first_var_in_stack_from_fp
	 is calculated in func_decl. symbol->offset is offset from first local variable.
	 If var is a int or a char, we use the above offsets to calculate its relative
	 address. If var is an array, the size of the array is used to get the address.
   3.global variables or constants: To simplify accessing variables, we use directives
	 to allocate space for them and get them initialized. In this case, the name of
	 the identifier is sufficient to load the symbol. */
static void load_reg(int reg_num, Symbol *symbol)
{
	if (symbol->addr_des.in_reg)
		return;

	if (symbol->flag & SYMBOL_PARAM) {
		if (symbol->addr_des.in_mem)
			emit("lw %s, %d($fp)", name[reg_num], symbol->offset + WORD_SIZE);
	}
	else if (symbol->flag & SYMBOL_LOCAL) {	// local var or const
		int offset = offset_of_first_var_in_stack_from_fp - symbol->offset;
		if (symbol->type->type == TYPE_ARRAY) {
			offset = offset - symbol->type->size + WORD_SIZE;
			emit("addiu %s, $fp, %d", name[reg_num], offset);
		}
		else if (eq_oneof(2, symbol->type->type, TYPE_CONST_CHAR, TYPE_CONST_INT)) {
			emit("li %s, %d", name[reg_num], symbol->value);
		}	// else TYPE_INT or TYPE_CHAR
		else if (symbol->addr_des.in_mem) {
			emit("lw %s, %d($fp)", name[reg_num], offset);
		}
	}
	else {	// global variable or constant
		if (symbol->type->type == TYPE_ARRAY) {
			emit("la %s, g_%s", name[reg_num], symbol->name);
		}
		else {	// const int or char, int, char
			emit("lw %s, g_%s($zero)", name[reg_num], symbol->name);
		}
	}
	
	symbol->addr_des.in_reg = true;
}

/* If symbol s is variable int or char(not array or constant),
   s is in a register and has newest value in
   register but not in memory, write it back to memory.
   flag: SPILL_LOCAL, SPILL_PARAM, SPILL_GLOBAL, SPILL_ALL
   */
static void spill_reg(Symbol *s, int flag)
{
	if (s != NULL && (s->type->type == TYPE_INT || s->type->type == TYPE_CHAR)
		&& s->addr_des.reg_num && !s->addr_des.in_mem && s->addr_des.in_reg) {

		if (s->flag & SYMBOL_PARAM) {
			if (flag & SPILL_PARAM)
				emit("sw %s, %d($fp)", name[s->addr_des.reg_num], s->offset + WORD_SIZE);
		}
		else if (s->flag & SYMBOL_LOCAL) {
			if (flag & SPILL_LOCAL)
				emit("sw %s, %d($fp)", name[s->addr_des.reg_num],
					offset_of_first_var_in_stack_from_fp - s->offset);
		}
		else {	// global
			if (flag & SPILL_GLOBAL)
				emit("sw %s, g_%s($zero)", name[s->addr_des.reg_num], s->name);
		}

		s->addr_des.in_mem = true;
	}
}

/* If reg reg_num is not live(not in vector live), frees it. If reg reg_num is free,
   allocates it to symbol, and returns true else returns false.
   Update register descriptor and address descriptor if allocated successfully.*/
static bool alloc_if_free(int reg_num, Symbol *symbol)
{
	Symbol *s_prev = map_get(reg_des->temp_reg, name[reg_num]);
	
	if (s_prev == NULL) { // free
		reg_des_bind(reg_num, symbol);
		symbol->addr_des.reg_num = reg_num;
		return true;
	}
	return false;
}

/* Pre-conditions: symbol is not NULL.
   Allocate reg reg_num to symbol. If reg_num is in use at this moment,
   write it back to memory if  is a int or char(No need to write constant
   to memory since it doesn't have a place in memory. No need to store array
   address since we can calculate it at compile time).
   Update register descriptor and address descriptor. */
static void alloc_forced(int reg_num, Symbol *symbol)
{
	Symbol *s = map_get(reg_des->temp_reg, name[reg_num]);

	if (s != NULL) {
		spill_reg(s, SPILL_ALL);
		s->addr_des.in_reg = false;
		s->addr_des.reg_num = 0;
	}
	
	reg_des_bind(reg_num, symbol);
	symbol->addr_des.reg_num = reg_num;
}

/* Free register that is allocated to symbol. The register is REG_T0 ~ REG_T9.
   Freeing saved register will be supported in future.
    */
static void unalloc(Symbol *symbol)
{
	int reg_num = symbol->addr_des.reg_num;
	if (!is_temp_reg(reg_num))
		return;
	symbol->addr_des.in_reg = false;
	symbol->addr_des.reg_num = 0;
	reg_des_unbind(reg_num);
	comment("将分配给%s的寄存器%s释放，因为%s无后续使用", symbol->name, name[reg_num], symbol->name);
}

/* If next-use information has been computed, makes use of it to manage register 
   allocation. If a variable has no next-use we can reuse the register allocated
   to the variable.
   Note that globals should not be unallocated without writing it back to
   memory.
   Reference:
   https://www2.cs.arizona.edu/~collberg/Teaching/453/2009/Handouts/Handout-21.pdf
   */
static void free_no_next_use_reg(IR *t)
{
	extern bool live_variable_analysis_ON;
	if (!live_variable_analysis_ON)
		return;
	if (t->result && !vec_is_in(t->next_use, t->result->symbol) 
		&& (t->result->symbol->flag & SYMBOL_LOCAL))
		unalloc(t->result->symbol);
	if (t->arg1 && t->arg1->symbol && !vec_is_in(t->next_use, t->arg1->symbol)
		&& (t->arg1->symbol->flag & SYMBOL_LOCAL))
		unalloc(t->arg1->symbol);
	if (t->arg2 && t->arg2->symbol && !vec_is_in(t->next_use, t->arg2->symbol)
		&& (t->arg2->symbol->flag & SYMBOL_LOCAL))
		unalloc(t->arg2->symbol);
	for (int i = 0; i < t->num_args; i++) {
		Symbol *s = vec_get(t->args, i);
		if (!vec_is_in(t->next_use, s) && (s->flag & SYMBOL_LOCAL))
			unalloc(s);
	}
}

/* next-use information is used to free reg that is no longer used.*/
static void get_rhs_reg(Reg *r, bool load_constant)
{
	if (live_variable_analysis_ON && lhs_reg_got) {
		fprintf(stderr, "程序逻辑出错：应该先调用get_lhs_reg()\n");
	}
	if (r->type == REG_NUM) {
		if (!load_constant)
			return;
		for (int i = REG_T8; i <= REG_T9; i++) {
			if (map_get(reg_des->temp_reg, name[i]) == NULL) {
				map_put(reg_des->temp_reg, name[i], (void *)1);	// temporary use
				emit("li %s, %d", name[i], r->value);
				r->rn = i;
				return;
			}
		}	printf("Something Wrong.\n");// never reach here
	}	// else REG_VAR or REG_TEMP

	/* If this symbol has been allocated a register, loads content from memory 
	   if it is not in register.*/
	if (r->symbol->addr_des.reg_num) {
		r->rn = r->symbol->addr_des.reg_num;
		load_reg(r->rn, r->symbol);
		return;
	}

	/* Attempt to find a free temporary register. If found, loads content from memory.*/
	for (int i = REG_T0; i <= REG_T7; i++) {
		if (alloc_if_free(i, r->symbol)) {
			r->rn = i;
			load_reg(r->rn, r->symbol);
			return;
		}
	}

	/* All temporary registers are in use or live.*/
	r->rn = spill;
	alloc_forced(r->rn, r->symbol);
	load_reg(r->rn, r->symbol);
	spill = (spill + 1) % (REG_T7 - REG_T0 + 1) + REG_T0;
}

/* This register is going to be modified.*/
static void get_lhs_reg(Reg *r, IR *t)
{
	if (r->is_return_value) {
		r->symbol->addr_des.reg_num = REG_V0;
		r->rn = REG_V0;
		goto get_lhs_reg_exit;
	}

	// assert t->symbol->type->type is int or char 
	if (r->symbol->addr_des.reg_num) {
		r->rn = r->symbol->addr_des.reg_num;
		goto get_lhs_reg_exit;
	}

	// 优化的代码,使用此段代码要求在get_rhs_reg调用之后再调用get_lhs_reg
	if (live_variable_analysis_ON && t->op != IR_READ) {
		Symbol *s = NULL;
		int reg_num;
		bool success = false;
		if (t->arg1 && t->arg1->symbol && is_temp_reg(t->arg1->symbol->addr_des.reg_num)
			&& !vec_is_in(t->next_use, t->arg1->symbol)) {
			s = t->arg1->symbol;
			success = true;
		}
		else if (t->arg2 && t->arg2->symbol && is_temp_reg(t->arg2->symbol->addr_des.reg_num) 
			&& !vec_is_in(t->next_use, t->arg2->symbol)) {
			s = t->arg2->symbol;
			success = true;
		}
		else {
			for (int i = 0; i < t->num_args; i++) {
				Reg *arg = vec_get(t->args, i);
				if (arg->symbol && is_temp_reg(arg->symbol->addr_des.reg_num) 
					&& !vec_is_in(t->next_use, arg->symbol)) {
					s = arg->symbol;
					success = true;
					break;
				}
			}
		}
		if (success) {
			// globals should be written back before unallocated.
			if (!(s->flag & SYMBOL_LOCAL)) {
				spill_reg(s, SPILL_GLOBAL);
			}
			s->addr_des.in_reg = false;
			reg_num = s->addr_des.reg_num;
			s->addr_des.reg_num = 0;
			reg_des_bind(reg_num, r->symbol);
			r->symbol->addr_des.reg_num = reg_num;
			r->rn = reg_num;
			comment("将%s分配给%s，因为%s无后续使用", name[reg_num], r->symbol->name, s->name);
			goto get_lhs_reg_exit;
		}
	}

	/* Attempt to find a free temporary register.*/
	for (int i = REG_T0; i <= REG_T7; i++)
		if (alloc_if_free(i, r->symbol)) {
			r->rn = i;
			goto get_lhs_reg_exit;
		}

	r->rn = spill;
	alloc_forced(r->rn, r->symbol);
	spill = (spill + 1) % (REG_T7 - REG_T0 + 1) + REG_T0;

get_lhs_reg_exit:
	r->symbol->addr_des.in_reg = true;
	r->symbol->addr_des.in_mem = false;
	lhs_reg_got = true;
}

/* var includes local variable and parameters in a function.
   This function should be called after reg_des_init() is called.*/
static void alloc_local_var()
{
	Symbol *symbol;

	if (saved_reg_alloc_ON) {
		// has already been allocated. Deal with a0-a3 before return.
		for (int i = 0; i < env->symbols->values->len; i++) {
			symbol = vec_get(env->symbols->values, i);
			if (symbol->flag & SYMBOL_PARAM) {
				if (i < 4) {
					symbol->addr_des.reg_num = i + REG_A0;
					symbol->addr_des.in_mem = false;
					symbol->addr_des.in_reg = true;
				}
				else {
					symbol->addr_des.in_mem = true;
					symbol->addr_des.in_reg = false;
				}
			}
			if (is_saved_reg(symbol->addr_des.reg_num)) {//用来判断有哪些保存寄存器被占用需要保存
				map_put(reg_des->saved_reg, name[symbol->addr_des.reg_num], symbol);
			}
		}
		return;
	}

	for (int i = 0; i < env->symbols->values->len; i++) {
		symbol = vec_get(env->symbols->values, i);
		if (symbol->flag & SYMBOL_TEMP) {
			symbol->addr_des.reg_num = 0;
			symbol->addr_des.in_mem = false;
			symbol->addr_des.in_reg = false;
			continue;
		}

		if (symbol->flag & SYMBOL_PARAM) {
			if (i < 4) {
				symbol->addr_des.reg_num = i + REG_A0;
				symbol->addr_des.in_mem = false;
				symbol->addr_des.in_reg = true;
			}
			else {
				symbol->addr_des.reg_num = 0;
				symbol->addr_des.in_mem = true;
				symbol->addr_des.in_reg = false;
			}
			continue;
		}

		for (int j = REG_S0; j <= REG_S7; j++) {
			if (map_get(reg_des->saved_reg, name[j]) == NULL) {
				map_put(reg_des->saved_reg, name[j], symbol);
				symbol->addr_des.reg_num = j;
				symbol->addr_des.in_mem = false;
				symbol->addr_des.in_reg = false;
				break;
			}
		}

	}
}

/* Allocate temporary register for TAC t. */
static void get_reg(IR *t, bool load_constant)
{
	if (t->arg1)
		get_rhs_reg(t->arg1, load_constant);
	if (t->arg2)
		get_rhs_reg(t->arg2, load_constant);
	if (t->result)
		get_lhs_reg(t->result, t);
}

static char *reg(Reg *r)
{
	return name[r->rn];
}

/* Write temporary registers(except t8 and t9) back to memory if necessary
   (the register holds a variable int or char(not array), and the var has 
   newest value in register but not in memory). Those temporary registers
   are freed. The register descriptor and variable address descriptor are
   updated to keep in a consistent state.

   If live-variable analysis has been done, we know whether a variable used
   in a basic block is live-on-exit. If it is not live on exit, there is no
   need to store values kept in register back into their memory locations.
   Reference:
   https://www2.cs.arizona.edu/~collberg/Teaching/453/2009/Handouts/Handout-21.pdf

   flag can be:
   FLUSH_LOCAL: Local variables(or constants) are unallocated.
   LOCAL_NOT_SPILL: Unallocate local variables without write it back to 
					memory, this flag is valid when FLUSH_LOCAL is on.
   FLUSH_GLOBAL: Global variables(or constants) are unallocated.
   live只针对基本块出口活跃的局部变量，全局变量即使不活跃也要保存。*/
static void flush_temp_reg(Vector *live, int flag)
{
	char *key;
	Symbol *s;
	for (int i = 0; i < reg_des->temp_reg->keys->len; i++) {
		key = vec_get(reg_des->temp_reg->keys, i);
		s = map_get(reg_des->temp_reg, key);

		if (streql(key, "$t8") || streql(key, "$t9") || s == NULL 
				|| ((s->flag & SYMBOL_LOCAL) && !(flag & FLUSH_LOCAL))
				|| (!(s->flag & SYMBOL_LOCAL) && !(flag & FLUSH_GLOBAL)))
			continue;

		if (s->flag & SYMBOL_LOCAL) {
			if (!(flag & LOCAL_NOT_SPILL)) {
				if (live_variable_analysis_ON) {
					if (live && vec_is_in(live, s))
						spill_reg(s, SPILL_ALL);
				}
				else {
					spill_reg(s, SPILL_ALL);
				}
			}
		}
		else { // global
			spill_reg(s, SPILL_ALL);
		}
		
		reg_des_unbind(s->addr_des.reg_num);
		s->addr_des.reg_num = 0;
		s->addr_des.in_reg = false;
	}
}

char *arith_instr(int ir_op, bool has_imm)
{
	switch (ir_op)
	{
	case IR_ADD: return has_imm ? "addiu" : "addu";
	case IR_SUB: return has_imm ? "subiu" : "subu";
	case IR_TIMES: return "mul";
	case IR_DIV: return "div";
	default: return NULL;
	}
}

int arith_result(int ir_op, int arg1, int arg2)
{
	switch (ir_op)
	{
	case IR_ADD: return arg1 + arg2;
	case IR_SUB: return arg1 - arg2;
	case IR_TIMES: return arg1 * arg2;
	case IR_DIV: return arg1 / arg2;
	default: printf("error calling arith_result()\n"); return 0;
	}
}

/* t->op is IR_ADD, IR_SUB, IR_TIMES, IR_DIV*/
static void gen_arith(IR *t)
{
	get_reg(t, false);	// do not load constant
	if (!t->arg2) {
		if (t->arg1->type == REG_NUM)
			emit("%s %s, $zero, %d", arith_instr(t->op, true), reg(t->result), t->arg1->value);
		else
			emit("%s %s, $zero, %s", arith_instr(t->op, false), reg(t->result), reg(t->arg1));
		return;
	}
	// two operands
	if (t->arg1->type == REG_NUM && t->arg2->type == REG_NUM) {
		if (t->op == IR_DIV && t->arg2->value == 0) {
			warningf(t->token, "division by zero");
			emit("li %s, 0", reg(t->result));
		} 
		else{
			emit("li %s, %d", reg(t->result), arith_result(t->op, t->arg1->value, t->arg2->value));
		}
	}
	else if (t->arg1->type == REG_NUM && t->arg2->type != REG_NUM) {
		if (t->op == IR_DIV) {
			emit("li $t9, %d", t->arg1->value);
			emit("%s %s, $t9, %s", arith_instr(t->op, true), reg(t->result), reg(t->arg2));
		}
		else {
			emit("%s %s, %s, %d", arith_instr(t->op, true), reg(t->result), reg(t->arg2), t->arg1->value);
		}
		if (t->op == IR_SUB)
			emit("negu %s, %s", reg(t->result), reg(t->result));
	}
	else if (t->arg1->type != REG_NUM && t->arg2->type == REG_NUM) {
		if (t->op == IR_DIV && t->arg2->value == 0) {
			warningf(t->token, "division by zero");
		}
		emit("%s %s, %s, %d", arith_instr(t->op, true), reg(t->result), reg(t->arg1), t->arg2->value);
	}
	else 
		emit("%s %s, %s, %s", arith_instr(t->op, false), reg(t->result), reg(t->arg1), reg(t->arg2));
}

/* instr is blt, bgt, bge, ble*/
static char *neg_branch(const char *instr, bool neg_both)
{
	if (streql(instr, "beq") || streql(instr, "bne")) {
		if (neg_both)
			return streql(instr, "beq") ? "bne" : "beq";
		return stringf(instr);
	}
	char *p = stringf(instr);
	p[1] = (p[1] == 'g') ? 'l' : 'g';
	if (neg_both)
		p[2] = (p[2] == 'e') ? 't' : 'e';
	return p;
}

/* ir_op is one of IR_LS, IR_GT, IR_LE, IR_GE, IR_EQ, IR_NE */
char *branch_instr(int ir_op, bool if_false, bool has_zero, bool imm_first)
{
	char *p;
	switch (ir_op) {
	case IR_LS: p = "blt"; break;
	case IR_GT: p = "bgt"; break;
	case IR_GE: p = "bge"; break;
	case IR_LE: p = "ble"; break;
	case IR_EQ: p = "beq"; break;
	case IR_NE: p = "bne"; break;
	default: return NULL;
	}
	if (imm_first)
		p = neg_branch(p, false);
	if (if_false)
		p = neg_branch(p, true);
	if (has_zero)
		p = stringf("%sz", p);
	return p;
}

/* ir_op is one of IR_LS, IR_GT, IR_LE, IR_GE, IR_EQ, IR_NE */
static bool jump_directly(int ir_op, int arg1, int arg2, bool if_false)
{
	bool ls, gt, eq, ret = false;
	ls = arg1 < arg2;
	gt = arg1 > arg2;
	eq = arg1 == arg2;
	switch (ir_op) {
	case IR_LS: ret = ls; break;
	case IR_GT: ret = gt; break;
	case IR_GE: ret = gt || eq; break;
	case IR_LE: ret = ls || eq; break;
	case IR_EQ: ret = eq; break;
	case IR_NE: ret = !eq; break;
	}
	return if_false ? !ret : ret;
}


static void load_params_at_end_of_bb(Vector *outregs)
{
	Symbol *s;
	for (int i = 0; i < env->symbols->values->len; i++) {
		s = vec_get(env->symbols->values, i);
		if (s->flag & SYMBOL_PARAM
			&& s->addr_des.reg_num >= REG_A0
			&& s->addr_des.reg_num <= REG_A3) {
			if (live_variable_analysis_ON) {
				if (vec_is_in(outregs, s))
					load_reg(s->addr_des.reg_num, s);
			}
			else {
				load_reg(s->addr_des.reg_num, s);
			}
			s->addr_des.in_mem = false;
		}
	}
}

/* 1.Make sure that param 0-3 is in reg and not in memory, param n>3 is in mem
   upon entry and exit of a basic block. This function should be called
   at the end of a basic block.
   2.Store values in temporary registers back into their memory locations.*/
static void clean_up_at_end_of_bb(Vector *outregs)
{
	load_params_at_end_of_bb(outregs);
	flush_temp_reg(outregs, FLUSH_LOCAL | FLUSH_GLOBAL);
}

static void ir2mips(IR *t, BB *bb, bool end_of_bb)
{
	int offset;
	Symbol *s;
	lhs_reg_got = false;
	switch (t->op) {
	case IR_ADD: case IR_SUB: case IR_TIMES: case IR_DIV:
		gen_arith(t); break;
	case IR_ASSIGN:
		get_reg(t, false);
		if (t->arg1->type == REG_NUM)
			emit("li %s, %d", reg(t->result), t->arg1->value);
		else 
			emit("move %s, %s", reg(t->result), reg(t->arg1));
		break;
	case IR_ASSIGN_ARR:
		get_rhs_reg(t->arg1, true);
		get_rhs_reg(t->arg2, true);
		get_rhs_reg(t->result, true);
		emit("sll $t9, %s, 2", reg(t->arg2));	// t9 instead of t8
		emit("addu $t9, %s, $t9", reg(t->result));
		emit("sw %s, 0($t9)", reg(t->arg1));
		break;
	case IR_ARR_ACCESS:
		get_reg(t, true);
		emit("sll $t8, %s, 2", reg(t->arg2));
		emit("addu $t8, %s, $t8", reg(t->arg1));
		emit("lw %s, 0($t8)", reg(t->result));
		break;
	case IR_LS: case IR_GT: case IR_NE: case IR_EQ: case IR_GE: case IR_LE:
		get_reg(t, false);
		clean_up_at_end_of_bb(bb->out_regs);// after get_reg(), do this before jumping
		if (t->arg1->type == REG_NUM && t->arg2->type == REG_NUM) {
			if (jump_directly(t->op, t->arg1->value, t->arg2->value, t->if_false))
				emit("j L%d", t->to_label);
		}
		else if (t->arg1->type != REG_NUM && t->arg2->type == REG_NUM) {
			if (t->arg2->value == 0)
				emit("%s %s, L%d", branch_instr(t->op, t->if_false, true, false), reg(t->arg1), t->to_label);
			else
				emit("%s %s, %d, L%d", branch_instr(t->op, t->if_false, false, false), reg(t->arg1), t->arg2->value, t->to_label);
		}
		else if (t->arg1->type == REG_NUM && t->arg2->type != REG_NUM) {
			if (t->arg1->value == 0)
				emit("%s %s, L%d", branch_instr(t->op, t->if_false, true, true), reg(t->arg2), t->to_label);
			else
				emit("%s %s, %d, L%d", branch_instr(t->op, t->if_false, false, true), reg(t->arg2), t->arg1->value, t->to_label);
		}
		else {
			emit("%s %s, %s, L%d", branch_instr(t->op, t->if_false, false, false), reg(t->arg1), reg(t->arg2), t->to_label);
		}
		break;
	case IR_EXPR_BRANCH:
		get_reg(t, true);
		clean_up_at_end_of_bb(bb->out_regs);// after get_reg(), do this before jumping
		emit("%s %s, $zero, L%d", t->if_false ? "beq" : "bne",reg(t->arg1), t->to_label);
		break;
	case IR_GOTO:
		clean_up_at_end_of_bb(bb->out_regs); // do this before jumping
		emit("j L%d", t->to_label);
		break;
	case IR_LABEL:
		emitl("L%d", t->b_label);
		break;
	case IR_READ:
		for (int i = 0; i < t->num_args; i++) {
			Reg *r = vec_get(t->args, i);
			get_lhs_reg(r, t);
			if (r->symbol->type->type == TYPE_CHAR)
				emit("li $v0, 12");
			else // int
				emit("li $v0, 5");
			emit("syscall");
			emit("move %s, $v0", reg(r));
		}
		break;
	case IR_WRITE:
		s = vec_get(env->symbols->values, 0);
		if (s != NULL && s->flag & SYMBOL_PARAM) {
			spill_reg(s, SPILL_PARAM);
			s->addr_des.in_reg = false;
		}
		if (t->string_label) {
			emit("li $v0, 4");
			emit("la $a0, S%d", t->string_label);
			emit("syscall");
		}
		if (t->arg1) {
			get_rhs_reg(t->arg1, true);
			if (t->arg1->is_char)
				emit("li $v0, 11");
			else
				emit("li $v0, 1");
			emit("move $a0, %s", reg(t->arg1));
			emit("syscall");
		}
		emit("li $v0, 4");
		emit("la $a0, newline");
		emit("syscall");
		if (s != NULL && s->flag & SYMBOL_PARAM) {
			s->addr_des.in_reg = false;
		}
		break;
	case IR_FUNC_DECL:
		is_main_func = streql(t->name, "main");
		emitl(is_main_func ? "main:" : "f_%s:", t->name);
		env = t->env;
		//print_env(env);
		reg_des_init();
		alloc_local_var();
		offset = OFF_FIRST_SAVED_REG;
		if (!is_main_func && !is_leaf)
			emit("sw $ra, -4($fp)");
		for (int i = 0; i < reg_des->saved_reg->keys->len; i++) {
			char *key = vec_get(reg_des->saved_reg->keys, i);
			Symbol *s = map_get(reg_des->saved_reg, key);
			if (!is_main_func)
				emit("sw %s, %d($fp)", key, -offset);
			offset += 4;
		}
		offset_of_first_var_in_stack_from_fp = -offset;
		if (is_main_func)
			emit("addi $fp, $sp, -4"); // no one setup fp for main, so main has to to this itself
		emit("addiu $sp, $sp, %d", -(offset + env->offset));
		break;
	case IR_RETURN:
		if (t->arg1) {
			if (t->arg1->type == REG_NUM) {
				emit("li $v0, %d", t->arg1->value);
			}
		}
		offset = OFF_FIRST_SAVED_REG;
		load_params_at_end_of_bb(bb->out_regs);
		flush_temp_reg(bb->out_regs, FLUSH_GLOBAL | FLUSH_LOCAL | LOCAL_NOT_SPILL);
		
		if (!is_main_func && !is_leaf)
			emit("lw $ra, -4($fp)");
		for (int i = 0; i < reg_des->saved_reg->keys->len; i++) {
			char *key = vec_get(reg_des->saved_reg->keys, i);
			Symbol *s = map_get(reg_des->saved_reg, key);
			if (!is_main_func)
				emit("lw %s, %d($fp)", key, -offset);
			offset += 4;
		}
		emit("addiu $sp, $sp, %d", offset + env->offset);
		if (is_main_func) {
			emit("li $v0, 10");
			emit("syscall");
		}
		else {
			emit("jr $ra");
		}
		break;
	case IR_FUNC_CALL:
		for (int i = 0; i < env->symbols->values->len; i++) {
			Symbol *s = vec_get(env->symbols->values, i);
			if ((s->flag & SYMBOL_PARAM)) {
				//s->addr_des.in_mem = false;	// forced???
				spill_reg(s, SPILL_PARAM);
				s->addr_des.in_reg = false;
			}
		}
		
		if (t->num_args > 0)
			emit("addiu $sp, $sp, %d", -t->num_args * WORD_SIZE);

		// place arguments
		for (int i = 0; i < t->num_args; i++) {
			Reg *arg = vec_get(t->args, i);	// arg is caller's variable
			if (i < 4) {
				if (arg->symbol && arg->symbol->flag & SYMBOL_PARAM) {
					emit("lw $a%d, %d($fp)", i, arg->symbol->offset + WORD_SIZE);
				}
				else {
					get_rhs_reg(arg, true);
					emit("move $a%d, %s", i, reg(arg));
					reg_des_unbind(REG_T8);	// make sure one of t8, t9 is free.
				}
			}
			else {
				if (arg->symbol && arg->symbol->flag & SYMBOL_PARAM) {
					emit("lw $t8, %d($fp)", arg->symbol->offset + WORD_SIZE);
					emit("sw $t8, %d($sp)", i * WORD_SIZE);
				}
				else {
					get_rhs_reg(arg, true);
					emit("sw %s, %d($sp)", reg(arg), i * WORD_SIZE);
					reg_des_unbind(REG_T8);
				}
			}
		}
		// 函数调用不开始一个基本块时需要第一个参数live应该是t->next_use,
		// 否则应该是bb->out_regs
		flush_temp_reg(bb->out_regs, FLUSH_GLOBAL | FLUSH_LOCAL);

		emit("sw $fp, -4($sp)");
		emit("addi $fp, $sp, -4");
		emit("jal f_%s", t->name);
		emit("lw $fp, 0($fp)");

		if (t->num_args > 0)
			emit("addiu $sp, $sp, %d", t->num_args * WORD_SIZE);

		load_params_at_end_of_bb(bb->out_regs);

		if (t->result && !(t->result->is_return_value)) {
			get_lhs_reg(t->result, t);
			emit("move %s, $v0", reg(t->result));
		}

		break;
	}

	free_no_next_use_reg(t);

	if (end_of_bb && t->op != IR_RETURN) {
		clean_up_at_end_of_bb(bb->out_regs);
	}

	reg_des_unbind(REG_T8);
	reg_des_unbind(REG_T9);
}

static void emit_data(BB *global_vars) {
	extern String_Table string_table;
	emitl(".globl main");
	emitl(".data");
	emit("newline: .asciiz \"\\n\"");
	for (int i = 0; i < string_table.strings->len; i++) {
		char *string = vec_get(string_table.strings, i);
		emit("S%d: .asciiz \"%s\"", i + 1, string);
	}
	for (int i = 0; i < global_vars->ir->len; i++) {
		IR *t = vec_get(global_vars->ir, i);
		Symbol *s = t->result->symbol;
		s->addr_des.in_mem = true;
		if (t->arg1) {
			// constant
			emit("g_%s: .word %d", s->name, t->arg1->value);
		}
		else if (s->type->type == TYPE_ARRAY) {
			emit("g_%s: .word 0:%d", s->name, s->type->len);
		}
		else if (s->type->type == TYPE_INT || s->type->type == TYPE_CHAR) {
			emit("g_%s: .word 0", s->name);
		}
	}
}

static bool is_leaf_function(Vector *func)
{
	for (int i = 0; i < func->len; i++) {
		BB *bb = vec_get(func, i);
		for (int j = 0; j < bb->ir->len; j++) {
			IR *t = vec_get(bb->ir, j);
			if (t->op == IR_FUNC_CALL)
				return false;
		}
	}
	return true;
}

bool gen_mips(Program *prog, char *path, int flag)
{
	if (prog == NULL)
		return false;
	fp = fopen(path, "w");
	if (fp == NULL) {
		fprintf(stderr, "Cannot open file %s\n", path);
		exit(-1);
	}
	print_option = flag;
	reg_des_init();
	//basic_block_demo(prog);
	emit_data(prog->global_vars);
	emitl(".text");
	emit("j main\n");
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func = vec_get(prog->funcs, i);	// Vector of BB
		is_leaf = is_leaf_function(func);
		for (int j = 0; j < func->len; j++) {
			BB *bb = vec_get(func, j);
			if (flag & PRINT_TO_CONSOLE)
				printf("#BB begin:\n");
			if (bb->label)
				emitl("L%d:", bb->label);
			for (int k = 0; k < bb->ir->len; k++) {
				ir2mips(vec_get(bb->ir, k), bb, k == bb->ir->len - 1);
			}
			IR *t = vec_get(bb->ir, bb->ir->len - 1);
			if (flag & PRINT_TO_CONSOLE)
				printf("#BB end\n\n");
		}
	}
	return true;
}
