#include "1200cc.h"
#define OFF_FIRST_SAVED_REG 8	// first saved register's offset from fp 
#define FLUSH_LOCAL 0x1
#define FLUSH_GLOBAL 0x2
#define LOCAL_NOT_SPILL 0x4
//Program *prog;

int offset_of_first_var_in_stack_from_fp;	// always non-negative.
int spill = REG_T0;	// temporary register to spill
FILE *fp;

char *name[] = { 
	"$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
	"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
	"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
	"$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra" 
};
Reg_Des *reg_des;
Env *env;			// to access symbols
bool main_func = false;

void print_env(Env *env)
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
	vprintf(fmt, va);
	printf("\n");
	vfprintf(fp, fmt, va);
	fprintf(fp, "\n");
	va_end(va);
}

/* A tab is added to the beginning of this instruction. A newline is added to 
   the end of this instruction. */
static void emit(char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	printf("\t");
	vprintf(fmt, va);
	printf("\n");
	fprintf(fp, "\t");
	vfprintf(fp, fmt, va);
	fprintf(fp, "\n");
	va_end(va);
}

/* Initialize register descpritor: all saved registers and temporary registers
   are free. This function should be called before each function declaration.
   and before generating mips instructions. */
void reg_des_init()
{
	reg_des = calloc(1, sizeof(Reg_Des));
	reg_des->saved_reg = new_map();
	reg_des->temp_reg = new_map();
}

/* Reg reg_num is temporary register.*/
void reg_des_bind(int reg_num, Symbol *s)
{
	map_put(reg_des->temp_reg, name[reg_num], s);
}

void reg_des_unbind(int reg_num)
{
	map_put(reg_des->temp_reg, name[reg_num], NULL);
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
void load_reg(int reg_num, Symbol *symbol)
{
	if (symbol->addr_des.in_reg)
		return;

	if (symbol->flag & SYMBOL_PARAM) {
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
		else if (symbol->addr_des.in_mem && !symbol->addr_des.in_reg) {
			emit("lw %s, %d($fp)", name[reg_num], offset);
		}
	}
	else {	// global variable or constant
		if (symbol->type->type == TYPE_ARRAY) {
			emit("la %s, %s", name[reg_num], symbol->name);
		}
		else {	// const int or char, int, char
			emit("lw %s, %s($zero)", name[reg_num], symbol->name);
		}
	}
	
	symbol->addr_des.in_reg = true;
}

/* If symbol s is variable int or char(not array or constant),
   s is in a register and has newest value in
   register but not in memory, write it back to memory. */
void spill_reg(Symbol *s)
{
	if (s != NULL && (s->type->type == TYPE_INT || s->type->type == TYPE_CHAR)
		&& s->addr_des.reg_num && !s->addr_des.in_mem && s->addr_des.in_reg) {

		if (s->flag & SYMBOL_PARAM) {
			emit("sw %s, %d($fp)", name[s->addr_des.reg_num], s->offset + WORD_SIZE);
		}
		else if (s->flag & SYMBOL_LOCAL) {
			emit("sw %s, %d($fp)", name[s->addr_des.reg_num],
				offset_of_first_var_in_stack_from_fp - s->offset);
		}
		else {	// global
			emit("sw %s, %s($zero)", name[s->addr_des.reg_num], s->name);
		}

		s->addr_des.in_mem = true;
	}
}

/* If reg reg_num is free, allocates it to symbol, 
   and returns true else returns false.
   Update register descriptor and address descriptor if allocated successfully.*/
bool alloc_if_free(int reg_num, Symbol *symbol)
{
	if (map_get(reg_des->temp_reg, name[reg_num]) == NULL) {
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
void alloc_forced(int reg_num, Symbol *symbol)
{
	Symbol *s = map_get(reg_des->temp_reg, name[reg_num]);

	if (s != NULL) {
		spill_reg(s);
		s->addr_des.in_reg = false;
		s->addr_des.reg_num = 0;
	}
	
	reg_des_bind(reg_num, symbol);
	symbol->addr_des.reg_num = reg_num;
}

/**/
void get_rhs_reg(Reg *r)
{
	if (r->type == REG_NUM) {
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
	for (int i = REG_T0; i <= REG_T7; i++)
		if (alloc_if_free(i, r->symbol)) {
			r->rn = i;
			load_reg(r->rn, r->symbol);
			return;
		}

	/* All temporary registers are in use. TODO: 改用好一点的方法。*/
	r->rn = spill;
	alloc_forced(r->rn, r->symbol);
	load_reg(r->rn, r->symbol);
	spill = (spill + 1) % (REG_T7 - REG_T0 + 1) + REG_T0;
}

/* This register is going to be modified.*/
void get_lhs_reg(Reg *r)
{
	// assert t->symbol->type->type is int or char 
	if (r->symbol->addr_des.reg_num) {
		r->rn = r->symbol->addr_des.reg_num;
		goto get_lhs_reg_exit;
	}

	/* Attempt to find a free temporary register.*/
	for (int i = REG_T0; i <= REG_T7; i++)
		if (alloc_if_free(i, r->symbol)) {
			r->rn = i;
			goto get_lhs_reg_exit;
		}

	/* All temporary registers are in use. TODO: 改用好一点的方法。*/
	r->rn = spill;
	alloc_forced(r->rn, r->symbol);
	spill = (spill + 1) % (REG_T7 - REG_T0 + 1) + REG_T0;

get_lhs_reg_exit:
	r->symbol->addr_des.in_reg = true;
	r->symbol->addr_des.in_mem = false;
}

/* var includes local variable and parameters in a function.
   This function should be called after reg_des_init() is called.*/
void alloc_local_var()
{
	Symbol *symbol;
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
void get_reg(IR *t)
{
	if (t->arg1)
		get_rhs_reg(t->arg1);
	if (t->arg2)
		get_rhs_reg(t->arg2);
	if (t->result)
		get_lhs_reg(t->result);
}

char *reg(Reg *r)
{
	return name[r->rn];
}

/* Write temporary registers(excpet t8 and t9) back to memory if necessary
   (the register holds a variable int or char(not array), and the var has 
   newest value in register but not in memory). Those temporary registers
   are freed. The register descriptor and variable address descriptor are
   updated to keep in a consistent state.
   flag can be:
   FLUSH_LOCAL: Local variables(or constants) are unallocated.
   LOCAL_NOT_SPILL: Unallocate local variables without write it back to 
					memory, this flag is valid when FLUSH_LOCAL is on.
   FLUSH_GLOBAL: Global variables(or constants) are unallocated.*/
void flush_temp_reg(int flag)
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

		if (!(s->flag & SYMBOL_LOCAL && flag & LOCAL_NOT_SPILL))
			spill_reg(s);

		reg_des_unbind(s->addr_des.reg_num);
		s->addr_des.reg_num = 0;
		s->addr_des.in_reg = false;
	}
}


void ir2mips(IR *t, bool end_of_bb)
{
	int offset;
	Symbol *s;

	// special case, deal with regs on its own
	if (!eq_oneof(4, t->op, IR_FUNC_CALL, IR_DECL, IR_FUNC_DECL, IR_ASSIGN_ARR))
		get_reg(t);

	// after get_reg(), do this before jumping
	if (eq_oneof(8, t->op, IR_LS, IR_GT, IR_EQ, IR_NE, IR_LE, IR_GE, IR_EXPR_BRANCH, IR_GOTO))
		flush_temp_reg(FLUSH_LOCAL | FLUSH_GLOBAL);
	
	switch (t->op) {
	case IR_TIMES:
		emit("mul %s, %s, %s", reg(t->result), reg(t->arg1), reg(t->arg2));
		break;
	case IR_DIV:
		emit("div %s, %s, %s", reg(t->result), reg(t->arg1), reg(t->arg2));
		break;
	case IR_ADD:
		if (!t->arg2)
			emit("addu %s, $zero, %s", reg(t->result), reg(t->arg1));
		else
			emit("addu %s, %s, %s", reg(t->result), reg(t->arg1), reg(t->arg2));
		break;
	case IR_SUB:
		if (!t->arg2)
			emit("subu %s, $zero, %s", reg(t->result), reg(t->arg1));
		else
			emit("subu %s, %s, %s", reg(t->result), reg(t->arg1), reg(t->arg2));
		break;
	case IR_ASSIGN:
		emit("move %s, %s", reg(t->result), reg(t->arg1));
		break;
	case IR_ASSIGN_ARR:
		get_rhs_reg(t->arg1);
		get_rhs_reg(t->arg2);
		get_rhs_reg(t->result);
		emit("sll $t9, %s, 2", reg(t->arg2));	// t9 instead of t8
		emit("addu $t9, %s, $t9", reg(t->result));
		emit("sw %s, 0($t9)", reg(t->arg1));
		break;
	case IR_ARR_ACCESS:
		emit("sll $t8, %s, 2", reg(t->arg2));
		emit("addu $t8, %s, $t8", reg(t->arg1));
		emit("lw %s, 0($t8)", reg(t->result));
		break;
	case IR_LS:
		emit("slt $t8, %s, %s", reg(t->arg1), reg(t->arg2));
		if (t->if_false)
			emit("beq $t8, $zero, L%d", t->to_label);
		else
			emit("bne $t8, $zero, L%d", t->to_label);
		break;
	case IR_GT:
		emit("slt $t8, %s, %s", reg(t->arg2), reg(t->arg1));
		if (t->if_false)
			emit("beq $t8, $zero, L%d", t->to_label);
		else
			emit("bne $t8, $zero, L%d", t->to_label);
		break;
	case IR_NE:
		emit("subu $t8, %s, %s", reg(t->arg1), reg(t->arg2));
		if (t->if_false)
			emit("beq $t8, $zero, L%d", t->to_label);
		else
			emit("bne $t8, $zero, L%d", t->to_label);
		break;
	case IR_EQ:
		emit("subu $t8, %s, %s", reg(t->arg1), reg(t->arg2));
		if (t->if_false)
			emit("bne $t8, $zero, L%d", t->to_label);
		else
			emit("beq $t8, $zero, L%d", t->to_label);
		break;
	case IR_GE:
		emit("slt $t8, %s, %s", reg(t->arg1), reg(t->arg2));
		if (t->if_false)
			emit("bne $t8, $zero, L%d", t->to_label);
		else
			emit("beq $t8, $zero, L%d", t->to_label);
		break;
	case IR_LE:
		emit("slt $t8, %s, %s", reg(t->arg2), reg(t->arg1));
		if (t->if_false)
			emit("bne $t8, $zero, L%d", t->to_label);
		else
			emit("beq $t8, $zero, L%d", t->to_label);
		break;
	case IR_EXPR_BRANCH:
		if (t->if_false)
			emit("beq %s, $zero, L%d", reg(t->arg1), t->to_label);
		else
			emit("bne %s, $zero, L%d", reg(t->arg1), t->to_label);
		break;
	case IR_GOTO:
		emit("j L%d", t->to_label);
		break;
	case IR_LABEL:
		emitl("L%d", t->b_label);
		break;
	case IR_READ:
		s = vec_get(env->symbols->values, 0);
		if (s != NULL && s->flag & SYMBOL_PARAM) {
			spill_reg(s);
			s->addr_des.in_reg = false;
		}
		for (int i = 0; i < t->num_args; i++) {
			Reg *r = vec_get(t->args, i);
			get_lhs_reg(r);
			if (r->symbol->type->type == TYPE_CHAR)
				emit("li $v0, 12");
			else // int
				emit("li $v0, 5");
			emit("syscall");
			emit("move %s, $v0", reg(r));
		}
		emit("li $v0, 4");
		emit("la $a0, newline");
		emit("syscall");
		break;
	case IR_WRITE:
		s = vec_get(env->symbols->values, 0);
		if (s != NULL && s->flag & SYMBOL_PARAM) {
			spill_reg(s);
			s->addr_des.in_reg = false;
		}
		if (t->string_label) {
			emit("li $v0, 4");
			emit("la $a0, S%d", t->string_label);
			emit("syscall");
		}
		if (t->arg1) {
			get_rhs_reg(t->arg1);
			if (t->arg1->symbol->type->type == TYPE_CHAR)
				emit("li $v0, 11");
			else
				emit("li $v0, 1");
			emit("move $a0, %s", reg(t->arg1));
			emit("syscall");
		}
		emit("li $v0, 4");
		emit("la $a0, newline");
		emit("syscall");
		break;
	case IR_DECL:
		if (t->arg1) {	// const
			get_lhs_reg(t->result);
			emit("li %s, %d", reg(t->result), t->arg1->value);
		} // else var
		else {
			get_rhs_reg(t->result);
		}
		break;
	case IR_FUNC_DECL:
		emitl("%s:", t->name);
		env = t->env;
		//print_env(env);
		reg_des_init();
		alloc_local_var();
		offset = OFF_FIRST_SAVED_REG;
		if (!streql(t->name, "main")) {
			emit("sw $ra, -4($fp)");
			main_func = false;
		}
		else {
			main_func = true;
		}
		for (int i = 0; i < reg_des->saved_reg->keys->len; i++) {
			char *key = vec_get(reg_des->saved_reg->keys, i);
			Symbol *s = map_get(reg_des->saved_reg, key);
			if (!main_func)
				emit("sw %s, %d($fp)", key, -offset);
			offset += 4;
		}
		offset_of_first_var_in_stack_from_fp = -offset;
		emit("addiu $sp, $sp, %d", -(offset + env->offset));
		break;
	case IR_RETURN:
		offset = OFF_FIRST_SAVED_REG;
		flush_temp_reg(FLUSH_GLOBAL | FLUSH_LOCAL | LOCAL_NOT_SPILL);
		if (t->arg1)
			emit("move $v0, %s", reg(t->arg1));
		if (!main_func)
			emit("lw $ra, -4($fp)");
		for (int i = 0; i < reg_des->saved_reg->keys->len; i++) {
			char *key = vec_get(reg_des->saved_reg->keys, i);
			Symbol *s = map_get(reg_des->saved_reg, key);
			if (!main_func)
				emit("lw %s, %d($fp)", key, -offset);
			offset += 4;
		}
		emit("addiu $sp, $sp, %d", offset + env->offset);
		if (main_func) {
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
				s->addr_des.in_mem = false;	// forced???
				spill_reg(s);
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
					get_rhs_reg(arg);
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
					get_rhs_reg(arg);
					emit("sw %s, %d($sp)", reg(arg), i * WORD_SIZE);
					reg_des_unbind(REG_T8);
				}
			}
		}

		flush_temp_reg(FLUSH_GLOBAL | FLUSH_LOCAL);

		emit("sw $fp, -4($sp)");
		emit("addi $fp, $sp, -4");
		emit("jal %s", t->name);
		emit("lw $fp, 0($fp)");

		if (t->num_args > 0)
			emit("addiu $sp, $sp, %d", t->num_args * WORD_SIZE);

		for (int i = 0; i < env->symbols->values->len; i++) {
			Symbol *s = vec_get(env->symbols->values, i);
			if ((s->flag & SYMBOL_PARAM) && s->addr_des.reg_num != 0) {
				s->addr_des.in_reg = false;
				load_reg(s->addr_des.reg_num, s);
			}
		}

		if (t->result) {
			get_lhs_reg(t->result);
			emit("move %s, $v0", reg(t->result));
		}

		break;
	}

	if (end_of_bb)
		flush_temp_reg(FLUSH_GLOBAL | FLUSH_LOCAL);

	reg_des_unbind(REG_T8);
	reg_des_unbind(REG_T9);
}

void emit_data(BB *global_vars) {
	extern String_Table string_table;
	emitl(".data");
	emit("newline: .asciiz \"\\n\"");
	for (int i = 0; i < string_table.strings->len; i++) {
		char *string = vec_get(string_table.strings, i);
		emit("S%d: .asciiz \"%s\"", i + 1, string);
	}
	for (int i = 0; i < global_vars->ir->len; i++) {
		IR *t = vec_get(global_vars->ir, i);
		Symbol *s = t->result->symbol;
		if (t->arg1) {
			// constant
			emit("%s: .word %d", s->name, t->arg1->value);
		}
		else if (s->type->type == TYPE_ARRAY) {
			emit("%s: .word 0:%d", s->name, s->type->len);
		}
		else if (s->type->type == TYPE_INT || s->type->type == TYPE_CHAR) {
			emit("%s: .word 0", s->name);
		}
	}
}

void gen_mips(char *path, Vector *ir)
{
	fp = fopen(path, "w");
	if (fp == NULL) {
		fprintf(stderr, "Cannot open file %s.", path);
		exit(-1);
	}
	Program *prog = partition_program(ir);
	reg_des_init();
	//basic_block_demo(prog);
	emit_data(prog->global_vars);
	emitl(".text");
	emit("j main\n");
	for (int i = 0; i < prog->funcs->len; i++) {
		Vector *func = vec_get(prog->funcs, i);	// Vector of BB
		for (int j = 0; j < func->len; j++) {
			BB *bb = vec_get(func, j);
			// printf("#BB begin:\n");
			if (bb->label)
				emitl("L%d:", bb->label);
			for (int k = 0; k < bb->ir->len; k++) {
				ir2mips(vec_get(bb->ir, k), k == bb->ir->len - 1);
			}
			IR *t = vec_get(bb->ir, bb->ir->len - 1);
			// printf("#BB end\n\n");
		}
	}

}
