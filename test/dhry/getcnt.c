#ifdef __propeller__
#include <propeller.h>
#else
#define TIMER_ADDRESS   0x80000100

unsigned int getcnt()
{
	unsigned int* timer = TIMER_ADDRESS;
        return *timer;
}
#endif

unsigned int getms()
{
    return getcnt() / 80000;
}

