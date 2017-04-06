#include <stdio.h>
#include <string.h>
#include <malloc.h>
#ifdef __zpu__
#define printf iprintf
#endif

#define uint32_t unsigned int

#define DELTA 0x9e3779b9
#define MX ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (k[(p&3)^e] ^ z));

void btea(uint32_t *v, int n, uint32_t const k[4])
{
    uint32_t y, z, sum;
    unsigned p, rounds, e;
    if (n > 1)
    {                            /* Coding Part */
        rounds = 6 + 52/n;
        sum = 0;
        z = v[n-1];
        do
        {
            sum += DELTA;
            e = (sum >> 2) & 3;
            for (p=0; p<n-1; p++)
                y = v[p+1], z = v[p] += MX;
            y = v[0];
            z = v[n-1] += MX;
        } while (--rounds);
    }                            /* Decoding Part */
    else if (n < -1)
    {
        n = -n;
        rounds = 6 + 52/n;
        sum = rounds*DELTA;
        y = v[0];
        do
        {
            e = (sum >> 2) & 3;
            for (p=n-1; p>0; p--)
                z = v[p-1], y = v[p] -= MX;
            z = v[n-1];
            y = v[0] -= MX;
        } while ((sum -= DELTA) != 0);
    }
}


uint32_t testVector[] = {0x9d204ce7, 0xc03677f6, 0x320f0555, 0x499c703c,
                         0x8b8af399, 0x061b6314, 0x7d410085, 0xe65b712c,
                         0xb7d23609, 0x0270cd59, 0xa23dc8d1};

int main(int argc, char* argv[])
{
	char key[16] = "0123456789ABCDEF";
	char* text = "All work and no play makes Jack a dull boy.";
        uint32_t *vector;
	int blockSize = (strlen(text) + 4) / 4;

	vector = (uint32_t*)malloc (blockSize);
	memset (vector, 0, sizeof(vector));
	strncpy ((char*)vector, text, strlen(text) + 1); 

	btea (vector, blockSize, (uint32_t*) key);

	for (int i = 0; i < blockSize;  i++)
        {
		printf("%x ", vector[i]);
        }
	printf("\n");

	btea (vector, -blockSize, (uint32_t*) key);
	printf("%s\n", (char*)vector);

	btea (testVector, -blockSize, (uint32_t*) key);
	printf("%s\n", (char*)testVector);

	return(0);
} 
