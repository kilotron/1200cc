#include "1200cc.h"

extern SrcFile *src;

void lexer_demo(char *path)
{
	lexer_init(path);
	Token *t = next_token();
#define PRINT
	for (int i = 1; t->type != TK_EOF; t = next_token(), i++) {
#ifdef PRINT
		if (t->type >= TK_ID && t->type <= TK_NUML) {
			printf("%3d, %-10s, %s\n", i, type2string(t->type), t->lexeme);
		}
		else if (t->type == TK_CHARL) {
			printf("%3d, %-10s, '%c'\n", i, type2string(t->type), t->value);
		}
		else if (t->type == TK_STRL) {
			printf("%3d, %-10s, \"%s\"\n", i, type2string(t->type), t->lexeme);
		}
		else if (t->type >= TK_LP && t->type <= TK_RBRACE) {
			printf("%3d, %-10s, %c\n", i, type2string(t->type), t->type);
		}
		else {	// multi-character symbol or bad token
			char *s = "";	// "" for bad token
			switch (t->type) {
			case TK_EQ: s = "=="; break;
			case TK_NE: s = "!="; break;
			case TK_GE: s = ">="; break;
			case TK_LE: s = "<="; break;
			}
			printf("%3d, %-10s, %s\n", i, type2string(t->type), s);
		}
#endif // PRINT
	}
}

/* 1. s->p <= e->p (Token s is e or s is before e)
 2.s->p > e->p means epsilon.*/
void parser_demo(Token *s, Token *e, char *fmt, ...)
{
#define PARSER_DEMO false
	if (!PARSER_DEMO)
		return;
	static int count = 1;
	int s_line = 1, e_line = 1;
	char *p;
	char *start = src->buf;	// beginning of the line
	char *end;
	bool empty = false;
	FILE *fp = stdout;

	//if (count > 20)
		//return;

	// for empty string: swap start and end token
	if (s->p > e->p) {
		empty = true;
		Token *t;
		t = s;
		s = e;
		e = t;
	}

	// find out the beginning of line for Token s(start token)
	for (p = src->buf; p <= s->p; p++) {
		if (*p == '\n') {
			s_line++;
			start = p + 1;
		}
	}

	// find out the end of line for Token e(end token)
	for (p = src->buf; p <= e->p; p++) {
		if (*p == '\n')
			e_line++;
	}
	for (p = e->p; *p != '\n'; p++) {
		;
	}
	end = p + 1;

	if (s_line == e_line)
		fprintf(fp, "No.%d %s: line %d: ", count++, src->path, s_line);
	else
		fprintf(fp, "No.%d %s: from line %d to %d: ", count++, src->path, s_line, e_line);
	
	va_list va;
	va_start(va, fmt);
	vfprintf(fp, fmt, va);
	va_end(va);

	if (empty) {
		fprintf(fp, "\n%.*s##", s->p + s->len - start, start);
		fprintf(fp, "%.*s##", e->p - (s->p + s->len), s->p + s->len);
		fprintf(fp, "%.*s\n", end - e->p, e->p);
	}
	else {
		fprintf(fp, "\n%.*s##", s->p - start, start);
		fprintf(fp, "%.*s##", (e->p + e->len) - s->p, s->p);
		fprintf(fp, "%.*s\n", end - (e->p + e->len), (e->p + e->len));
	}
}

static char *reg_tostr(Reg *reg)
{
	char *buf = (char *)malloc(15);
	switch (reg->type) {
	case REG_NUM: 
		_itoa(reg->value, buf, 10);
		break;
	case REG_TEMP:
		sprintf(buf, "#t%d", reg->vn);
		break;
	case REG_VAR:
		buf = reg->symbol->name;
		break;
	}
	return buf;
}

static void print_ir(FILE *fp, IR *t)
{
	Symbol *s;
	switch (t->op) {
	case IR_LABEL:
		fprintf(fp, "L%d:\n", t->a_label);
		break;
	case IR_TIMES: case IR_ADD: case IR_SUB: case IR_DIV:
		if (t->arg2) {
			fprintf(fp, "%s = %s %c %s\n", reg_tostr(t->result), reg_tostr(t->arg1),
				t->op, reg_tostr(t->arg2));
		}
		else {
			fprintf(fp, "%s = %c %s\n", reg_tostr(t->result), t->op, reg_tostr(t->arg1));
		}
		break;
	case IR_ASSIGN:
		fprintf(fp, "%s = %s\n", reg_tostr(t->result), reg_tostr(t->arg1));
		break;
	case IR_ASSIGN_ARR:
		fprintf(fp, "%s[%s] = %s\n", reg_tostr(t->result), reg_tostr(t->arg2),
			reg_tostr(t->arg1));
		break;
	case IR_ARR_ACCESS:
		fprintf(fp, "%s = %s[%s]\n", reg_tostr(t->result), reg_tostr(t->arg1),
			reg_tostr(t->arg2));
		break;
	case IR_LS: case IR_GT: case IR_EQ: case IR_NE: case IR_GE: case IR_LE: case IR_EXPR_BRANCH:
		switch (t->op) {
		case IR_LS: case IR_GT:
			fprintf(fp, "%s %c %s\n", reg_tostr(t->arg1), t->op, reg_tostr(t->arg2));
			break;
		case IR_EXPR_BRANCH:
			fprintf(fp, "%s\n", reg_tostr(t->arg1));
			break;
		default:
			fprintf(fp, "%s %s %s\n", reg_tostr(t->arg1),
				t->op == IR_EQ ? "==" :
				t->op == IR_NE ? "!=" :
				t->op == IR_GE ? ">=" : "<=", reg_tostr(t->arg2));
		}
		fprintf(fp, "%s L%d\n", t->if_false ? "BZ" : "BNZ", t->to_label);
		break;
	case IR_GOTO:
		fprintf(fp, "goto L%d\n", t->to_label);
		break;
	case IR_DECL:
		s = t->result->symbol;
		if (t->arg1 == NULL) { // var decl
			char *type = "int";
			switch (s->type->type) {
			case TYPE_INT: type = "int"; break;
			case TYPE_CHAR: type = "char"; break;
			case TYPE_ARRAY:
				if (s->type->array_of->type == TYPE_INT)
					type = "array of int";
				else
					type = "array of char";
			}
			fprintf(fp, "var %s %s\n", type, s->name);
		}
		else {	// const decl
			char *buf = (char *)malloc(15);
			char *type = "int";
			if (s->type->type == TYPE_CONST_INT) {
				_itoa(t->arg1->value, buf, 10);
			}
			else {
				buf[0] = t->arg1->value;
				buf[1] = '\0';
				type = "char";
			}
			fprintf(fp, "const %s %s = '%s'\n", type, s->name, buf);
		}
		break;
	case IR_FUNC_DECL:
	{
		Type *ret = t->result->symbol->type->ret;
		char *type = ret == NULL ? "void" :
			ret->type == TYPE_CHAR ? "char" : "int";
		Reg *reg;
		fprintf(fp, "\n%s %s()\n", type, t->result->symbol->name);
		if (t->args) {
			for (int i = 0; i < t->args->len; i++) {
				reg = vec_get(t->args, i);
				type = reg->symbol->type->type == TYPE_INT ? "int" : "char";
				fprintf(fp, "para %s %s\n", type, reg->symbol->name);
			}
		}
	}
	break;
	case IR_FUNC_CALL: case IR_READ:
		if (t->args) {
			for (int i = 0; i < t->args->len; i++)
				fprintf(fp, "push %s\n", reg_tostr(vec_get(t->args, i)));
		}
		fprintf(fp, "call %s\n", t->name);
		if (t->result != NULL)
			fprintf(fp, "%s = RET\n", reg_tostr(t->result));
		break;
	case IR_RETURN:
		fprintf(fp, "ret %s\n", t->arg1 != NULL ? reg_tostr(t->arg1) : "");
		break;
	case IR_WRITE:
		if (t->string != NULL)
			fprintf(fp, "push \"%s\"\n", t->string);
		if (t->arg1 != NULL)
			fprintf(fp, "push %s\n", reg_tostr(t->arg1));
		fprintf(fp, "call printf\n");
		break;
	}
}

void ir_demo(Vector *ir)
{
	FILE *fp = stdout;
	IR *t;
	for (int i = 0; i < ir->len; i++) {
		t = vec_get(ir, i);
		print_ir(fp, t);
	}
}

static void print_basic_block(FILE *fp, BB *bb)
{
	fprintf(fp, "\nBB begin: ");
	if (bb->label)
		fprintf(fp, "L%d", bb->label);
	fprintf(fp, "\n+------------------+\n");
	for (int i = 0; i < bb->ir->len; i++)
		print_ir(fp, vec_get(bb->ir, i));
	fprintf(fp, "+------------------+\nBB end\n\n");
}

void basic_block_demo(Program *prog)
{
	Vector *func_of_bb;
	FILE *fp = stdout;
	for (int i = 0; i < prog->funcs->len; i++) {
		func_of_bb = vec_get(prog->funcs, i);
		for (int j = 0; j < func_of_bb->len; j++)
			print_basic_block(fp, vec_get(func_of_bb, j));
	}
}
