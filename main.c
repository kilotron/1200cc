#include "1200cc.h"

int main()
{

	// lexer_demo("test.c");
	Vector *tokens = tokenize("test.c");
	Program_AST *prog = parse(tokens);
	Vector *ir = NULL;
	if (prog) {
		ir = gen_ir(prog);
	//	ir_demo(ir);
		gen_mips("test_mips.asm", ir);
	}
		
	return 0;
}