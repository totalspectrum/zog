//#include <stdint.h>
 
#define uint32_t unsigned int

/* take 64 bits of data in v[0] and v[1] and 128 bits of key in k[0] - k[3] */
 
void encipher(unsigned int num_rounds, uint32_t v[2], uint32_t const k[4])
{
    unsigned int i;
    uint32_t v0=v[0], v1=v[1], sum=0, delta=0x9E3779B9;
    for (i=0; i < num_rounds; i++)
    {
        v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + k[sum & 3]);
        sum += delta;
        v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + k[(sum>>11) & 3]);
    }
    v[0]=v0; v[1]=v1;
}
 
void decipher(unsigned int num_rounds, uint32_t v[2], uint32_t const k[4])
{
    unsigned int i;
    uint32_t v0=v[0], v1=v[1], delta=0x9E3779B9, sum=delta*num_rounds;
    for (i=0; i < num_rounds; i++)
    {
        v1 -= (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + k[(sum>>11) & 3]);
        sum -= delta;
        v0 -= (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + k[sum & 3]);
    }
    v[0]=v0; v[1]=v1;
}


int main(int argc, char* argv[])
{
	char block[8] = "Hi there";            /* 64 bit (8 byte) blocks */
        char key[16] = "0123456789abcdef";     /* 128 bit (16 byte) keys */

	encipher (64, (uint32_t*) block, (uint32_t*)key);

	decipher (64, (uint32_t*)block, (uint32_t*)key);

	return (0);
}

