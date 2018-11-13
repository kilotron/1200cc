#include "1200cc.h"

struct {
	int type;
	char *name;
} type_string_table[] = {
	{ TK_LP, "TK_LP" },{ TK_RP, "TK_RP" },{ TK_TIMES, "TK_TIMES" },{ TK_PLUS, "TK_PLUS" },
	{ TK_COMMA, "TK_COMMA" },{ TK_MINUS, "TK_MINUS" },{ TK_DIV, "TK_DIV" },
	{ TK_SC, "TK_SC" },{ TK_LS, "TK_LS" },{ TK_ASSIGN, "TK_ASSIGN" },
	{ TK_GT, "TK_GT" },{ TK_LBRKT, "TK_LBRKT" },{ TK_RBRKT, "TK_RBRKT" },
	{ TK_LBRACE, "TK_LBRACE" },{ TK_RBRACE, "TK_RBRACE" },{ TK_ID, "TK_ID" },
	{ TK_CONST, "TK_CONST" },{ TK_INT, "TK_INT" },{ TK_CHAR, "TK_CHAR" },
	{ TK_VOID, "TK_VOID" },{ TK_MAIN, "TK_MAIN" },{ TK_IF, "TK_IF" },
	{ TK_ELSE, "TK_ELSE" },{ TK_WHILE, "TK_WHILE" },{ TK_FOR, "TK_FOR" },
	{ TK_RETURN, "TK_RETURN" },{ TK_SCANF, "TK_SCANF" },{ TK_PRINTF, "TK_PRINTF" },
	{ TK_NUML, "TK_NUML" },{ TK_CHARL, "TK_CHARL" },{ TK_STRL, "TK_STRL" },
	{ TK_EQ, "TK_EQ" },{ TK_NE, "TK_NE" },{ TK_GE, "TK_GE" },{ TK_LE, "TK_LE" },
	{ 0, "BAD_TOKEN" }
};

char *type2string(int type)
{
	for (int i = 0; type_string_table[i].type != 0; i++) {
		if (type_string_table[i].type == type)
			return type_string_table[i].name;
	}
	return "BAD_TOKEN";
}

/* Pre-condition: lexer_init() is called. */
void lexer_demo()
{
	Token *t = next_token();
//#define PRINT
	for (int i = 0; t->type != TK_EOF; t = next_token(), i++) {
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
