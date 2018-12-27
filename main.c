#include "1200cc.h"
#include <direct.h>
#include <time.h>
#define PATH_LEN 256

bool error_in_program = false;
bool live_variable_analysis_ON = true;
bool constant_folding_ON = true;
bool saved_reg_alloc_ON = true;
bool comment_ON = false;

char *get_dir(const char *path)
{
	char *p = stringf("%s", path);
	char cwd[PATH_LEN];
	int i;
	for (i = strlen(p) - 1; i >= 0; i--)
		if (p[i] == '\\') {
			p[i+1] = '\0';
			return p;
		}
	// if path is a relative path
	_getcwd(cwd, PATH_LEN);
	return stringf("%s\\", cwd);
}

/* Without extension.*/
char *get_filename(const char *path)
{
	char *p1 = stringf(path), *p2;
	bool met_dot = false;
	for (p2 = p1 + strlen(p1) - 1; p2 >= p1 && *p2 != '\\'; p2--) {
		if (!met_dot && *p2 == '.') {
			met_dot = true;
			*p2 = '\0';
		}
	}
	return p2+1;
}

int main()
{
	//char path[PATH_LEN] = "E:\\学习\\大三上\\编译技术\\编译技术课程设计\\测试代码\\测试程序0\\test.c";
	char path[PATH_LEN];
	char cwd[PATH_LEN];
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
	bool gen_success;
	if (prog_ast) {
		ir = gen_ir(prog_ast);

		//ir_demo(ir, "ir.txt", PRINT_TO_FILE);

		prog = partition_program(ir);

		//basic_block_demo(prog, "basic_block_orig.txt");

		optimization(prog);

		char *p = stringf("%s%s_basic_block_optd_%X.txt", get_dir(path), get_filename(path), id);
		//basic_block_demo(prog, p);

		//gen_success = gen_mips(prog, target_path, PRINT_TO_CONSOLE | PRINT_TO_FILE);
		gen_success = gen_mips(prog, target_path, PRINT_TO_FILE);

		if (gen_success) {
			_getcwd(cwd, PATH_LEN);
			printf("目标代码输出到%s中。\n", target_path);
		}
	}
		
	return 0;
}