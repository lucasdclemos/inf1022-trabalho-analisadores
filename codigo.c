#include <stdio.h>

int main(){
	int A;
	int B;
	int C;
	A = 5;
	B = A;
	printf("B: %d\n", B);
	C = B * 2;
	printf("C: %d\n", C);
	A = 0;
	B = C * A;
	printf("B: %d\n", B);

	return 0;
}