#include "1200cc.h"
#include <time.h>

/* Variable error_in_program may be set to true during syntax analysis
   or semantic analysis if error exists. Its inital value is false. */
bool error_in_program = false;

/* When live_variable_analysis_ON is true, registers allocated to variables
   that are not live at a certain point of program will be freed. 
   Live variable analysis is always executed during compilation. This option
   is used for better register allocation purpose.*/
bool live_variable_analysis_ON = true;

/* Constant expressions will be evaluated at compile time if constant_folding_ON
   is true. Expressions with constants and variables(or function call) will be 
   partially evaluated.
   Note that simple constant expressions such as a = 1 + 2 will be evaluated even
   if constant_folding_ON is false.*/
bool constant_folding_ON = true;

/* If saved_reg_alloc_ON is true, saved registers are allocated applying register-
   interference-graph and graph coloring algorithm. Live-variable analysis is
   required. If saved_reg_alloc_ON is false, saved registers are allocated to the 
   first 8 variables in a function.*/
bool saved_reg_alloc_ON = true;

/* When l2r_order_of_eval is true, evaluation order of function parameters
   and arithmetic(+-/*) expressions is from left to right.

   Note that order of evaluation matters only when global variable is in
   an expression. When an expression does not consist of pure globals,
   to minimize the cost, the true order of evaluation may not be from left
   to right. However, globals are cached so that the result looks as if the
   evaluation order is from left to right.*/
bool l2r_order_of_eval = true;

/* Output comment on allocating saved registers in mips assembly.*/
bool comment_ON = false;

/* Eliminate local common subexpressions. */
bool lcse_elimination_ON = true;

/* Eliminate code where a variable is assigned a value but never used. */
bool dead_code_elimination_ON = true;

int main()
{
	//char path[PATH_LEN] = "E:\\学习\\大三上\\编译技术\\编译技术课程设计\\测试代码\\测试程序0\\test.c";
	char path[PATH_LEN];
	char *target_path;
	int id;

	scanf("%[^\n]", path);
	id = (int)time(NULL) % 65536;
	target_path = stringf("%s%s_%X.asm", get_dir(path), get_filename(path), id);
	// lexer_demo("test.c");
	Vector *tokens = tokenize(path);
	Program_AST *prog_ast = parse(tokens);
	Vector *ir = NULL;
	Program *prog;
	bool gen_success = false;
	if (prog_ast) {
		ir = gen_ir(prog_ast);

		//ir_demo(ir, "ir.txt", PRINT_TO_FILE);

		prog = partition_program(ir);

		char *p = stringf("%s%s_basic_block_orig_%X.txt", get_dir(path), get_filename(path), id);
		//basic_block_demo(prog, p);

		optimization(prog);

		p = stringf("%s%s_basic_block_optd_%X.txt", get_dir(path), get_filename(path), id);
		basic_block_demo(prog, p);

		//gen_success = gen_mips(prog, target_path, PRINT_TO_CONSOLE | PRINT_TO_FILE);
		gen_success = gen_mips(prog, target_path, PRINT_TO_FILE);

		if (gen_success) {
			printf("目标代码输出到%s中。\n", target_path);
		}
	}
		
	return 0;
}