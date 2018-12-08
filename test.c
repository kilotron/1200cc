const int SIZE = +0020;
const int m_size = 3;
const char const_a = 'a', const_b = 'b';
const char const_y = 'y', const_z = 'z';

char char_tab[128];
int a[20];
int n, num_cmp;

void init_char_tab
{
	int n;
	for (n = ('A'); n <= ('Z'); n = n + 1) {
		char_tab[n] = 'A' + 'Z' - n;
	}
	for (n = (const_a); n <= (const_z); n = n + 1) {
		char_tab[const_a + 0] = const_z + (+2) - 1 + (-1);
		char_tab[const_b + 0] = -const_y - (-const_y) * 2;
		char_tab[const_a + 2] = const_y - 16061200 * (76 - 75) / 16061200;
		char_tab[(n)] = 'a' + 'z' - n;
	}
	return;
}


int fact(int n)
{	
	if (n == 0)
		return (1);
	else
		return (n * fact(n - 1));
}

char Atbash(char c)
{
	const char a = 'a';
	
	if (c >= a) {
		if (c <= const_z) {
			return (char_tab[(c)]);
		}
	}
	if (c >= 'A') {
		if (c <= 'Z') {
			return (char_tab[(c)]);
		}
	}
	
	return (c);	
}

void encrypt
{
	char  c1, c2, c3, c4, c5;
	printf("Input a word of 5 characters: ");
	scanf(c1, c2, c3, c4, c5);
	c1 = atbash(c1); c2 = atbash(c2); c3 = atbash(c3); c4 = atbash(c4);
	c5 = atbash(c5);
	printf("The chars: ");
	printf(c1);
	printf(c2);
	printf(c3);
	printf(c4);
	printf(c5);
}


void get_value_n(int min, int max)
{
	int num_TRIES;

	for (NUM_TRIES = 1; n > max; num_tries = Num_Tries + 001) {
		printf("Input an integer <= ", max);
		scanf(n);
		if (n < min) {
			for (num_tries = num_tries; n < min; num_TRIES = num_tries +1) {
				printf("The number you have tried = ", nUm_tRies);
				printf("Input an integer >= ", min);
				scanf(n);
			}
		}
		if (n > max) {
			printf("The number you have tried = ", num_tRies);
		}
	}
}

void swap_array_elements(int i, int j)
{
	int _tmp;
	_tmp = a[i];
	a[i] = a[j];
	a[j] = _tmp;
}

void nop
{

}

void quick(int left, int right)
{
	int i, last, tmp;
	
	if (left >= right)
		return;
	
	last = left;
	
	i = left + 1;
	while (i <= right) {
		num_cmp = num_cmp + 1;
		if (a[i] < a[left]) {
			last = + last + 1;
			swap_array_elements(last, i);
		}
		i = i + 001;
	}
	
	swap_array_elements(last, left);
	
	quick(left, last - 1);
	quick(last + 1, right);
}

int dot2(int a1, int a2, int b1, int b2)
{
	return (a1 * b1 + a2 * b2);
}

int dot3(int a1, int a2, int a3, int b1, int b2, int b3)
{
	int result;
	result = dot2(b1, b2, a1, a2) + a3 * b3;
	return (result);
}

void matrix_Mult
{
	int i, j, t;
	int b[9];
	printf("Input a 3x3 matrix");
	for (i = 0; i < m_size * m_size; i = i + 1) {
		scanf(t);
		a[i] = t;
	}
	for (i = 0; i < m_size; i = i + 1) {
		for (j = 0;j < m_size; j = j + 1) {
			b[i*m_size + j] = dot3( a[i*m_size],
									a[i*m_size+1],
									a[i*m_size+2],
									a[j], 
									a[m_size+j],
									a[m_size*2+j]);
		}
	}
	printf("result:");
	for (i = 0; i < m_size * m_size; i = i + 1)
		printf(b[i]);
}


void main()
{
	int i;
	int t;
	char order;

	printf("This is a simple sorting program using quick sort algorithm!");
	printf("Input size of the array");
	get_value_n(1, size);
	printf("and the order you prefer('a' for ascending order, 'd' and else for descending order): ");
	scanf(order);
	printf("Input the array: ");
	i = 0;
	while (i < n) {
		scanf(t);
		a[i] = t;
		i = i + 1;
	}
	
	quick(0, n - 1);
	
	;
	
	nop;

	printf("number of comparisons = ", num_cmp);
	printf("The sorted array:");
	if (order == 'a') {
		i = 0;
		while (i < n) {
			printf(a[i]);
			i = i + 1;
		}
	} else {
		i = n - 1;
		while (i > -1) {
			printf(a[i]);
			i = i + -1;
		}
	}
	
	printf("Factorial Calculator");
	get_value_n(0, 15);
	printf(fact(n));
	
	init_char_tab;
	printf("Encryption");
	encrypt;
	printf("Decryption");
	encrypt;
	
	matrix_Mult;
}

