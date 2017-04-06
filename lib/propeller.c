#include "propeller.h"

int clkfreq = 104857600;


int errno;

void longfill(int* dst, int fill, int count)
{
  int i;
  for (i = 0; i < count; i++)
  {
    *dst++ = fill;
  }
}

void longmove(int* dst, int* src, int count)
{
  int i;
  for (i = 0; i < count; i++)
  {
    *dst++ = *src++;
  }
}

int cognew(int* code, int* par)
{
  int cog;
  cog = _syscall(&errno, SYS_cognew, 0, code, par);
  return (cog);
}

int coginit(int cog, int* code, int* par)
{
  cog = _syscall(&errno, SYS_coginit, cog, code, par);
  return (cog);
}

void cogstop(int cog)
{
  int result;
  result = _syscall(&errno, SYS_cogstop, cog, 0, 0);
}
