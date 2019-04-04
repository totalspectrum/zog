#include <stdio.h>
#include <time.h>
#include "xxtea.h"

extern int iprintf(const char *, ...);

extern unsigned int getcnt();

uint32_t testVector[] = {0x9d204ce7, 0xc03677f6, 0x320f0555, 0x499c703c,
                         0x8b8af399, 0x061b6314, 0x7d410085, 0xe65b712c,
                         0xb7d23609, 0x0270cd59, 0xa23dc8d1};

char key[16] = "0123456789ABCDEF";
int blockSize = sizeof(testVector) / 4;

int main(int argc, char* argv[])
{
    clock_t start, end;

    start = getcnt();
    btea (testVector, -blockSize, (uint32_t*) key);
    end = getcnt();

    iprintf("%s\n", (char*)testVector);
    iprintf("done in %lu cycles\n", (unsigned long)(end - start));

    return(0);
} 
