#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

extern int _hardware;
extern int _cpu_config;
extern int ZPU_ID;
extern int _use_syscall;

#define SYS_read  4
#define SYS_write 5

int result;
int errno;

typedef unsigned long uint32;

#define ERRDUMPCNT 0x20
#define ALLTESTS

/*
 * dump32(ptr,count);
 * call with number of longs - that is number of bytes/4.
 */
void dump32(uint32 *ptr, int count)
{
    int n;
    for(n = 0; n < count; n++) {
        if(n % 4 == 0)
            iprintf("\r\n%08x: ", (uint32*) &ptr[n]);
        iprintf("0x%08x ", (uint32) ptr[n]);
    }
    iprintf("\r\n");
}

int spitsize(int size)
{
    int mod = 0x4000;
    int spit = mod;
    if(size < mod)
        spit = size/8;
    return spit;
}

#ifdef ALLTESTS
int testinc(uint32 *ptr, int size)
{
    int n = 0;
    int val = 0;
    int spit= spitsize(size);
    int count = size/sizeof(uint32);
    iprintf("testinc %x %x\r\n",size,count);
    iprintf("0x%x - 0x%x\r\n", ptr, &ptr[count-1]);
    for(n = 0; n < count; n++) {
        ptr[n] = n;
        if(n % spit == 0)
            iprintf("Writing 0x%08x 0x%08x\r\n", &ptr[n], n);
    }
    for(n = 0; n < count; n++) {
        val = ptr[n];
        if(n % spit == 0)
            iprintf("Reading 0x%08x\r\n", &ptr[n]);
        if(val != n) {
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[n], n, n, val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
    }
    iprintf("testinc passed.\r\n\r\n");
    return 0;
}

int testdec(uint32 *ptr, int size)
{
    int n = 0;
    int val = 0;
    int spit= spitsize(size);
    int count = size/sizeof(uint32);
    iprintf("testdec %x %x\r\n",size,count);
    iprintf("0x%x - 0x%x\r\n", ptr, &ptr[count-1]);
    for(n = 0; n < count; n++) {
        ptr[n] = ~n;
        if(n % spit == 0)
            iprintf("Writing 0x%08x 0x%08x\r\n", &ptr[n], ~n);
    }
    for(n = 0; n < count; n++) {
        val = ptr[n];
        if(n % spit == 0)
            iprintf("Reading 0x%08x\r\n", &ptr[n]);
        if(val != ~n) {
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[n], n, ~n, val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
    }
    iprintf("testdec passed.\r\n\r\n");
    return 0;
}

int testmarch(uint32 *ptr, int size)
{
    /*
     * use march-x algorithm: W0^, R0^W1^, R1vW0v, R0^C^
     */
    int n = 0;
    int val = 0;
    int spit= spitsize(size);
    int count = size/sizeof(uint32);
    uint32 arr[] = { 0x55555555, 0xaaaaaaaa };
    iprintf("testmarch %x %x\r\n",size,count);
    iprintf("0x%x - 0x%x\r\n", ptr, &ptr[count-1]);
    iprintf("W0^ = Write 0 march up.\r\n");
    iprintf("W0v = Write 0 march down.\r\n");
    iprintf("R0^ = Read  0 march up.\r\n");
    iprintf("R0v = Read  0 march down.\r\n");

    // up write pattern 0
    for(n = 0; n < count; n++) {
        ptr[n] = arr[0];
        if(n % spit == 0)
            iprintf("W0^ 0x%08x 0x%08x 0x%08x\r\n", &ptr[n], n, arr[0]);
    }
    // up read pattern 0, write pattern 1
    for(n = 0; n < count; n++) {
        val = ptr[n];
        if(n % spit == 0)
            iprintf("R0^W1^ 0x%08x 0x%08x\r\n", &ptr[n], arr[1]);
        if(val != arr[0]) {
            iprintf("R0^W1^ 0x%08x\r\n", &ptr[n]);
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[n], n, arr[0], val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
        ptr[n] = arr[1];
    }
    // down read pattern 1, write pattern 0
    for(n = count-1; n > -1; n--) {
        val = ptr[n];
        if(n % spit == 0)
            iprintf("R1vW0v 0x%08x 0x%08x\r\n", &ptr[n], arr[0]);
        if(val != arr[1]) {
            iprintf("R1vW0v 0x%08x\r\n", &ptr[n]);
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[n], n, arr[1], val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
        ptr[n] = arr[0];
    }
    // up read pattern 0, clear
    for(n = 0; n < count; n++) {
        val = ptr[n];
        if(n % spit == 0)
            iprintf("R0^C0v 0x%08x\r\n", &ptr[n]);
        if(val != arr[0]) {
            iprintf("R0^C0v 0x%08x\r\n", &ptr[n]);
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[n], n, arr[0], val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
        ptr[n] = 0;
    }
    iprintf("testmarch passed.\r\n\r\n");
    return 0;
}
#endif
// ALLTESTS

#ifndef LIBRAND
uint32 msr(uint32 rseed)
{
    static uint32 rstate;
    uint32 mask = 0xffffc000;
    if(rseed != 0) {
        rstate = rseed;
    }
    else {
        rstate = (rstate >> 1) ^ (-(rstate & 1) & mask);
    }
    //iprintf("0x%08x\r\n", rstate);
    return rstate;
}
#endif

int testprand(uint32 *ptr, int size)
{
    /*
     * use march-x algorithm: W0^, R0^W1^, R1vW0v, R0^C^
     */
    int n = 0;
    int k = 0;
    int val = 0;
    int rnd = 0;
    int spit= spitsize(size);
    int count = size/sizeof(uint32);
    int seed = 0x6b8b4567;
    iprintf("testprand %x %x %08x\r\n",size,count,seed);
    iprintf("0x%x - 0x%x\r\n", ptr, &ptr[count-1]);

    // up-down write prand
#ifdef LIBRAND
    srand(seed);
#else
    msr(seed);
#endif
    for(n = 0; n < count; n++) {
        k = (n & 1) ?  count-n : n;
#ifdef LIBRAND
        rnd = rand();
#else
        rnd = msr(0);
#endif
        if(n % spit == 0)
            iprintf("Writing Pseudo-random 0x%08x 0x%08x 0x%08x\r\n", &ptr[k], k, rnd);
        ptr[k] = rnd;
    }

    // up-down write prand
#ifdef LIBRAND
    srand(seed);
#else
    msr(seed);
#endif
    for(n = 0; n < count; n++) {
        k = (n & 1) ?  count-n : n;
#ifdef LIBRAND
        rnd = rand();
#else
        rnd = msr(0);
#endif
        if(n % spit == 0)
            iprintf("Reading Pseudo-random 0x%08x 0x%08x\r\n", &ptr[k], k, rnd);
        val = ptr[k];
        if(rnd != val) {
            iprintf("Error @ 0x%x[%x] Expected 0x%08x Received 0x%08x\r\n", &ptr[k], k, rnd, val);
            dump32(&ptr[n],ERRDUMPCNT);
            return -1;
        }
    }
    iprintf("testprand passed.\r\n\r\n");
    return 0;
}

int testmem(uint32 *ptr, int size)
{
    int count = size/sizeof(uint32);
    iprintf("testmem %x %x\r\n",size,count);
#ifdef ALLTESTS
    if(testinc(ptr,size))
        return -1;
    if(testdec(ptr,size))
        return -1;
    if(testmarch(ptr,size))
        return -1;
#endif
// ALLTESTS
    if(testprand(ptr,size))
        return -1;
    return 0;
}

void malloctest(int size)
{
#ifdef DARRAY
    uint32 mp[size/4];
    testmem(mp,size);
#else
    uint32* mp;
    iprintf("Malloc buffer size: %x\r\n",size);

    //size /= sizeof(uint32);

    mp = malloc(size);
    if(0 == mp) {
        iprintf("Oops, malloc failed.\r\n");
    }
    else {
        testmem(mp,size);
        iprintf("Free buffer.\r\n");
        free(mp);
    }
#endif
}


int main (int argc,  char* argv[])
{
	// Enable using SYSCALL instruction for read(), write() printf() etc
    //_use_syscall = 1;

    iprintf("\r\nMalloc Testing.\r\n\n");
/*
    iprintf("_hardware    = %d  ",_hardware);
    iprintf("_cpu_config  = %d\r\n",_cpu_config);
    iprintf("_use_syscall = %d  ", _use_syscall);
    iprintf(" ZPU_ID      = %d\r\n\n", ZPU_ID);
*/
    malloctest(2048);
    //malloctest(1<<19);        // 1 MB test
    //malloctest(16000000);       // will it ever finish?

#if 0
    malloctest(30000000);
    malloctest((1<<24));
    {
    int n = 0;
    for(n = 16; n < 20; n++) 
        malloctest((1<<24)-(1<<n));
    }
    malloctest(16000000);
#endif

    iprintf("\r\nAll done!\r\n\n");
    iprintf("EXITCOM\r\n");

#ifdef ZOG
    for(;;)
        ;   // don't crash.
#endif
    return(0);
}

