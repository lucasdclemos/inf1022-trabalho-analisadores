#include <stdio.h>

int main(){
	int X;
	int Y;
	Y = 2;
	printf("Y: %d\n", Y);
	X = 5;
	while (X > 0){
	Y = Y * 2;
	printf("Y: %d\n", Y);
	X = X - 1;
	}

	return 0;
}