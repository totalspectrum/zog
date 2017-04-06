#include <stdio.h>
#include "xxtea.h"

uint32_t testVector[] = {0x9d204ce7, 0xc03677f6, 0x320f0555, 0x499c703c,
                         0x8b8af399, 0x061b6314, 0x7d410085, 0xe65b712c,
                         0xb7d23609, 0x0270cd59, 0xa23dc8d1};

char key[16] = "0123456789ABCDEF";
int blockSize = sizeof(testVector) / 4;

int main(int argc, char* argv[])
{
	btea (testVector, -blockSize, (uint32_t*) key);
#ifdef PRINTING
	printf("%s\n", (char*)testVector);
#endif
	return(0);
} 
