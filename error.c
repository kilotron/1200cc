#include "1200cc.h"

extern SrcFile *src;

struct {
	int type;
	char *name;
	char *lexeme;
} type_table[] = {
	{ TK_LP, "TK_LP", "'(' token"},
	{ TK_RP, "TK_RP" , "')' token"},
	{ TK_TIMES, "TK_TIMES" , "'*' token"},
	{ TK_PLUS, "TK_PLUS" , "'-' token"},
	{ TK_COMMA, "TK_COMMA", "',' token" },
	{ TK_MINUS, "TK_MINUS" , "'-' token"},
	{ TK_DIV, "TK_DIV" , "'/' token"},
	{ TK_SC, "TK_SC" , "';' token"},
{ TK_LS, "TK_LS" , "'<' token"},
{ TK_ASSIGN, "TK_ASSIGN" , "="},
	{ TK_GT, "TK_GT" , "'>' token"},
{ TK_LBRKT, "TK_LBRKT","'[' token" },
{ TK_RBRKT, "TK_RBRKT" ,"']' token"},
	{ TK_LBRACE, "TK_LBRACE" ,"'{' token"},
{ TK_RBRACE, "TK_RBRACE" ,"'}' token"},
	{ TK_ID, "TK_ID" ,"identifier"},{ TK_CONST, "TK_CONST" ,"'const' token"},
	{ TK_INT, "TK_INT" , "int"},{ TK_CHAR, "TK_CHAR", "char"},
	{ TK_VOID, "TK_VOID", "void" },{ TK_MAIN, "TK_MAIN", "main" },
	{ TK_IF, "TK_IF", "if" },{ TK_ELSE, "TK_ELSE" , "else"},
	{ TK_WHILE, "TK_WHILE", "while" },{ TK_FOR, "TK_FOR", "for" },
	{ TK_RETURN, "TK_RETURN" ,"return"},{ TK_SCANF, "TK_SCANF" , "scanf"},
	{ TK_PRINTF, "TK_PRINTF", "printf" }, { TK_NUML, "TK_NUML" , "number"},
	{ TK_CHARL, "TK_CHARL" , "character constant"},
	{ TK_STRL, "TK_STRL" , "string constant"},
	{ TK_EQ, "TK_EQ" , "=="},{ TK_NE, "TK_NE", "!=" },{ TK_GE, "TK_GE", ">=" },
	{ TK_LE, "TK_LE", "<=" }, {TK_EOF, "TK_EOF", "end of file"},
	{ 0, "BAD_TOKEN", "BAD TOKEN" }
};

char *type2string(int type)
{
	for (int i = 0; type_table[i].type != 0; i++) {
		if (type_table[i].type == type)
			return type_table[i].name;
	}
	return "BAD_TOKEN";
}

char *type2str(int type)
{
	for (int i = 0; type_table[i].type != 0; i++) {
		if (type_table[i].type == type)
			return type_table[i].lexeme;
	}
	return "BAD_TOKEN";
}

/* Pre-conditions: t != NULL, buf <= t->p < buf + file_size. */
void errorf(Token *t, char *fmt, ...)
{
	int line = 1;
	int col = 0;
	char *p;
	char *start = src->buf;	// beginning of the line

	// find out the line and col
	for (p = src->buf; p <= t->p; p++) {
		if (*p == '\n') {
			line++;
			col = 0;
			start = p + 1;
		}
		else {
			col++;
		}
	}

	fprintf(stderr, "%s:%d:%d: error: ", src->path, line, col);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);

	// print out the line containing the error position.
	int len = strchr(start, '\n') - start;
	fprintf(stderr, "\n%.*s\n", len, start);
	for (int i = 0; i < col - 1; i++)
		fprintf(stderr, (start[i] == '\t') ? "\t" : " ");
	fprintf(stderr, "^\n");
}