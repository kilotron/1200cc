#include "1200cc.h"
#include <direct.h>
#define PATH_LEN 256
bool error_in_program = false;
int main()
{
	char path[PATH_LEN];
	char cwd[PATH_LEN];
	char *target_path = "test_mips.asm";
	//scanf("%s", path);
	// lexer_demo("test.c");
	Vector *tokens = tokenize("test.c");
	Program_AST *prog_ast = parse(tokens);
	Vector *ir = NULL;
	Program *prog;
	if (prog_ast) {
		ir = gen_ir(prog_ast);
		//ir_demo(ir, "ir.txt", PRINT_TO_FILE);
		prog = partition_program(ir);
		//basic_block_demo(prog, "basic_block_orig.txt");
		optimization(prog);
		//basic_block_demo(prog, "basic_block_optd.txt");
		//gen_mips(prog, "test_mips.asm", PRINT_TO_CONSOLE | PRINT_TO_FILE);
		//gen_mips(prog, "test_mips.asm", PRINT_TO_CONSOLE);
		gen_mips(prog, target_path, PRINT_TO_FILE);
		_getcwd(cwd, PATH_LEN);
		printf("目标代码输出到%s\\%s中。\n", cwd, target_path);
	}
		
	return 0;
}