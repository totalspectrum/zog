#ifndef PROPELLER
#define PROPELLER

#ifdef __cplusplus
extern "C" 
{
#endif

/* ZPU's libgloss uses syscall IDs 1 to 18, we stay well away from that range */
#define SYS_cognew  8000
#define SYS_coginit 8001
#define SYS_cogstop 8002

extern int clkfreq;

extern int _syscall(int *foo, int ID, ...);

extern int errno;

void longfill(int* dst, int fill, int count);

void longmove(int* dst, int* src, int count);

int cognew(int* code, int* par);

void cogstop(int cog);

#ifdef __cplusplus
}
#endif

#endif
