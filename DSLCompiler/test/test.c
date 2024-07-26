#include <stdio.h>
#include "../c_lib/dsl_compiler.h"
#include "out.c"

int main() {
	// both 2x2.
	float mat1[4];
	float mat2[4];
	float mat3[4];

	float value = 1.0;
	for (int i =0; i < 4; i ++) {
		mat1[i] = value;
		mat2[i] = value;
		value = value + 1.0;
	}

	array2 input1 = { mat1, 2, 2};
	array2 input2 = { mat2, 2, 2};
	array2 output = { mat3, 2, 2};

	f(input1, input2, output);

	for (int i = 0; i < 4; i ++) {
		printf("%f;", output.data[i]);
	}
	printf("\n");
}
