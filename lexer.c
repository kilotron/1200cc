#include "1200cc.h"

SrcFile *src;
Map *keyword_map;

// Multi-character symbols, e.g. ==, <=, !=
static struct {
	char *name;
	int type;
} mc_symbols[] = {
	{"<=", TK_LE}, {">=", TK_GE}, {"!=", TK_NE}, {"==", TK_EQ}, {NULL, 0}
};

/* Create a Token object and initialize its type to parameter type 
   and its position in source file to parameter p.
   Other attributes are initialized to zero.*/
Token *new_token(int type, char *p)
{
	Token *t = calloc(1, sizeof(Token));
	t->type = type;
	t->p = p;
	return t;
}

void map_put_keyword(Map *map, char *keyword, int type)
{
	Token *t = new_token(type, NULL);
	t->lexeme = keyword;
	map_put(map, keyword, t);
}

Map * create_keyword_map()
{
	Map *map = new_map();
	map_put_keyword(map, "const", TK_CONST);
	map_put_keyword(map, "int", TK_INT);
	map_put_keyword(map, "char", TK_CHAR);
	map_put_keyword(map, "void", TK_VOID);
	map_put_keyword(map, "if", TK_IF);
	map_put_keyword(map, "else", TK_ELSE);
	map_put_keyword(map, "while", TK_WHILE);
	map_put_keyword(map, "for", TK_FOR);
	map_put_keyword(map, "return", TK_RETURN);
	map_put_keyword(map, "scanf", TK_SCANF);
	map_put_keyword(map, "printf", TK_PRINTF);
	map_put_keyword(map, "main", TK_MAIN);
	return map;
}

void lexer_init(char *path)
{
	src = new_srcfile(path);
	keyword_map = create_keyword_map();
}

/*Post-conditions: src->p is incremented by 1.*/
void bad_position()
{
	Token *t = new_token(TK_BAD, src->p);
	errorf(t, "stray %c in program", nextc(src));
}

/* Pre-conditions: "//" is a prefix of string src->p and source file ends with a newline. 
 Post-conditions: src->p is advanced to the end of this line(*src->p == '\n'). */
void skip_line_comment()
{
	while (peekc(src) != '\n')
		nextc(src);
}

/* Pre-conditions: "/*" is a prefix of string src->p.
Post-conditions: src->p is advanced to a character ahead of star slash.
If there is no star slash reports error and set src->p to the end of the file.*/
void skip_block_comment()
{
	Token *t = new_token(0, src->p);	// 0 for comment, this token is used to report error if any.
	while (*src->p != '\0' && !startswith(src->p, "*/"))
		src->p++;
	if (*src->p == '\0')
		errorf(t, "unterminated comment");
	else
		src->p += 2;
}

/* Skip comments and white spaces.*/
void skip_comments_spaces()
{
start:
	while (is_whitespace(peekc(src)))
		nextc(src);

	if (startswith(src->p, "//")) {
		skip_line_comment();
		goto start;
	}

	if (startswith(src->p, "/*")) {
		skip_block_comment();
		goto start;
	}
}

/* Pre-conditions: peekc(src) matches "[A-Za-z_]".
 Post-conditions: If the symbol is a keyword returns a Token t of corresponding type
 else returns a Token of type TK_ID. lexeme is the string(converted to lower case)
 that forms the symbol, len is set to the length of lexeme, value is set to zero,
 The pointer within src is avdanced to 1 character ahead of the last character
 of the symbol. */
Token *ident()
{
	StringBuilder *sb = new_sb();
	Token *token = new_token(TK_ID, src->p);

	while (is_alnum(peekc(src))) {
		char c = nextc(src);
		sb_append(sb, &c, 1);
	}

	token->lexeme = sb_get(sb);
	token->len = strlen(token->lexeme);
	str_tolower(token->lexeme);
	free_sb(sb);

	Token *tmp = map_get(keyword_map, token->lexeme);
	token->type = (tmp != NULL) ? tmp->type : token->type;
	return token;
}

/* Pre-conditions: peekc(src) matches "[0-9]".
 Post-conditions: Returns a Token of type TK_NUML. t->value holds value of the number.
 len is length of this number in source file, lexeme is set to NULL.
 The pointer within src is avdanced to 1 character ahead of the last character digit.*/
Token *number()
{
	char c;
	Token *t = new_token(TK_NUML, src->p);
	StringBuilder *sb = new_sb();

	while (isdigit(peekc(src))) {
		c = nextc(src);
		t->value = t->value * 10 + c - '0';
		sb_append(sb, &c, 1);
	}
	t->lexeme = sb_get(sb);
	t->len = strlen(t->lexeme);
	free_sb(sb);
	return t;
}

/* Pre-conditions: peekc(src) is a single quote. 
 Post-conditions: If a string matches "'[A-Za-z_0-9+/-*]'" returns a Token t
 whose type is TK_CHARL, value is the ASCII encoding of the character within the single
 quotes, lexeme is set to NULL. The pointer within src is avdanced to 1 character ahead
 of the closing quote else returns a Token of type TK_BAD and moves src->p to the end
 of the line or the end of file.*/
Token * char_literal()
{
	char c;
	Token *t = new_token(TK_CHARL, src->p);
	char *start = src->p;
	nextc(src);
	c = nextc(src);

	if (peekc(src) == '\'') {
		if (!(is_alnum(c) || is_in("+/-*", c))) {
			t->type = TK_BAD;
			errorf(t, "unexpected character constant");
		}
		t->value = c;
		nextc(src);
		t->len = src->p - start;
		return t;
	}

	// 'ab', 'a...\n: go to next line or the character after closing single quote.
	t->type = TK_BAD;
	
	while (true) {
		c = nextc(src);

		if (c == '\n' || c == '\0') {
			errorf(t, "missing terminating ' character");
			break;
		}
		else if (c == '\'') {
			errorf(t, "multi-character character constant");
			break;
		}
	}
	t->len = src->p - start;
	return t;
}

/* Pre-conditions: peekc(src) == '"'.
 Post-conditions: Returns a Token t of type TK_STRL. value is set to zero,
 lexeme is set to this string literal. The pointer within src is avdanced to the 1 
 character ahead of the closing quote else returns a Token of type TK_BAD and moves
 src->p to the end of this line or end of file.*/
Token *string_literal()
{
	Token *t = new_token(TK_STRL, src->p);
	StringBuilder *sb = new_sb();
	char c;

	nextc(src);
	for (c = nextc(src); c != '\0' && c != '\n' && c != '"'; c = nextc(src)) {
		sb_append(sb, &c, 1);
	}

	t->lexeme = sb_get(sb);
	t->len = strlen(t->lexeme) + 2;
	free_sb(sb);
	if (c == '"') {
		return t;
	}
	
	// c == '\0' or '\n'
	t->type = TK_BAD;
	errorf(t, "missing terminating \" character");
	return t;
}

bool is_mc_symbol()
{
	for (int i = 0; mc_symbols[i].name != NULL; i++) {
		if (startswith(src->p, mc_symbols[i].name))
			return true;
	}
	return false;
}

/* Pre-conditions: is_mc_symbol() == true.
 Post-conditions: Returns a Token t of corresponding type of the symbol. lexeme
 is set to the string that forms the symbol, len is set to the length of lexeme,
 value is set to zero. The pointer within src is avdanced to 1 character ahead of
 the last character of the symbol.*/
Token *mc_symbol()
{
	int i;
	for (i = 0; mc_symbols[i].name != NULL; i++) {
		if (startswith(src->p, mc_symbols[i].name))
			break;
	}
	
	Token *t = new_token(mc_symbols[i].type, src->p);
	t->lexeme = mc_symbols[i].name;
	t->len = strlen(t->lexeme);
	src->p += strlen(t->lexeme);
	return t;
}

/* Pre-conditions: peekc(src) matches "[()*+,-/;<=>[\]{}]"
 Post-conditions: Returns a Token t of corresponding type of the symbol.
 lexeme is set to NULL, len is set to 1, value is set to zero,
 The internal pointer of src is avdanced by 1.*/
Token *sc_symbol()
{
	// For single character symbol, type = ASCII encoding of the character.
	char *p = src->p;
	Token *t = new_token(nextc(src), p);
	t->len = 1;
	return t;
}

Token * next_token()
{
	char c;
	Token *t = NULL;

	while (t == NULL) {
		skip_comments_spaces();
		c = peekc(src);
		if (c == '\0')	// DO NOT advance the pointer.
			t = new_token(TK_EOF, src->p);
		else if (is_alpha(c))
			t = ident();
		else if (isdigit(c))
			t = number();
		else if (c == '\'')
			t = char_literal();
		else if (c == '"')
			t = string_literal();
		else if (is_mc_symbol())
			t = mc_symbol();
		else if (is_in("()*+,-/;<=>[]{}", c))
			t = sc_symbol();
		else 
			bad_position();
	}
	return t;
}

Vector *tokenize(char *path)
{
	lexer_init(path);
	Vector *v = new_vec();
	Token * token;
	do {
		token = next_token();
		vec_put(v, token);
	} while (token->type != TK_EOF);
	return v;
}

