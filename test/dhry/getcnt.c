#define TIMER_ADDRESS   0x80000100

unsigned int getcnt()
{
	unsigned int* timer = TIMER_ADDRESS;
        return *timer;
}
unsigned int getms()
{
	unsigned int* timer = TIMER_ADDRESS;
        return *timer / 80000;
}

