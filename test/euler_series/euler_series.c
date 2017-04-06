#include <stdio.h>
#include <math.h>

int  terms = 1000;
volatile float es;

float euler_term(term)
{
	float val;
	val = 1.0 / ((float)term * (float)term);
	return(val);
}

float euler_series(int terms)
{
	float result = 0;
	int term;
	for (term = 1; term <= terms; term++)
	{
		result += euler_term(term);
	}
	return(result);
}

// The following functions override the GCC soft float routines.
float __addsf3 (float A, float B)
{
//	printf("__addsf3\n");
	return(99.2);
}

float __subsf3 (float A, float B)
{
//	printf("__subsf3\n");
	return(499.2);
}

float __mulsf3 (float A, float B)
{
//	printf("__mulsf3\n");
	return(599.2);
}


float __divsf3 (float A, float B)
{
//	printf("__divsf3\n");
	return(699.2);
}


double __extendsfdf2 (float A)
{
//	printf("__extendsfdf2\n");
	return (22.0);
}

int __fixsfsi (float A)
{
//	printf("__fixsfsi\n");
	return(42);
}

float __floatsisf (int I)
{
//	printf("__floatsisf\n");
	return(52);
}

int main (int arc, char* argv[])
{
	es = euler_series(terms);

#ifdef PRINTING
	printf ("The first %d terms of the Euler series sum to %f\n", terms, es);
#endif
	return ((int)es);
}

