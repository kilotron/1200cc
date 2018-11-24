#include "1200cc.h"

int main()
{

	// lexer_demo("test.c");
	Vector *tokens = tokenize("test.c");
	Program *prog = parse(tokens);
	Vector *ir;
	if (prog) {
		ir = gen_ir(prog);
		ir_demo(ir);
	}
		
	return 0;
}