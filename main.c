#include "1200cc.h"
bool error_in_program = false;
int main()
{
	// lexer_demo("test.c");
	Vector *tokens = tokenize("test_error_2.c");
	Program_AST *prog_ast = parse(tokens);
	Vector *ir = NULL;
	Program *prog;
	if (prog_ast) {
		ir = gen_ir(prog_ast);
		//ir_demo(ir, "ir.txt", PRINT_TO_FILE);
		prog = partition_program(ir);
		basic_block_demo(prog, "basic_block_orig.txt");
		optimization(prog);
		basic_block_demo(prog, "basic_block_optd.txt");
		gen_mips(prog, "test_mips.asm", PRINT_TO_CONSOLE | PRINT_TO_FILE);
		//gen_mips(ir, "test_mips.asm", PRINT_TO_CONSOLE);
	}
		
	return 0;
}