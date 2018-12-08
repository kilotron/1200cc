const int SIZE = +0020;
const char SORT = 'Q';
const char plus = '+', times = '*', lucky = '9';

int a[20]; 
int n, num_cmp;


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
		if (c <= 'z') {
			return ('a' + 'z' - c);
		}
	}
	
	if (c >= 'A') {
		if (c <= 'Z') {
			return ('A' + 'Z' - c);
		}
	}
	
	return ('+');
}


void get_value_n
{
	int USELESS001;

	for (useless001 = 0; n < 0; uselesS001 = usEless001 + 010) {
		printf("useless001=", useless001);
		printf("Input a non-negative integer: ");
		scanf(n);
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

void array_comparison
{
	const int max = 98;
	int s1[98], s2[98];
	int i;
	for (i = 0; i < max; i = i + 1) {
		s1[i] = i + -5 * i - 12345;
		s2[i] = i + ((-5) * i)-12345;
	}
	s1[max-1] = +0;
	s2[max-1] = -0;
	s1[max - 2]=s2[max - 2]*s2[max-2];
	i = 0;
	while (s1[i]) {
		if (s1[i] != s2[i])
			printf("s1 is different from s2.");
		i = i++1;
	}
	return;
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

void main()
{
	int i;
	int t;
	char order, c;
	printf("This is a simple sorting program using quick sort algorithm!");
	printf("Input the size of the array and order you prefer('a' for ascending order, 'd' for descending order): ");
	scanf(n, order);
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
	//j = j+1;
	printf("-num_cmp/2 = ", -num_cmp/2);
	printf("The sorted array:");
	if (order == 'a') {
		i = 0;
		while (i < n) {
			printf(" ", a[i]);
			i = i + 1;
		}
	} else {
		i = n - 1;
		while (i > -1) {
			printf(" ", a[i]);
			i = i + -1;
		}
	}
	
	printf("Bonus!");
	get_value_n;
	printf(fact(n));
	printf("");
	printf("Input a letter: ");
	scanf(c);
	c = atbash(c);
	printf(c);
}

