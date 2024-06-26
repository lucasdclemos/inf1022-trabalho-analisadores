#include <stdio.h>

int main(){
	int X;
	int Y;
	int B;
	int Z;
	X = 5;
	Y = 2;
	B = 0;
	for (int i = X; i > Y; i--){
	if (B > 0){
	Z = Z + 2;
	printf("Z: %d\n", Z);
	}else{
	Z = Z + 1;
	printf("Z: %d\n", Z);
	}
	}

	return 0;
}