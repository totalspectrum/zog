#include "stdio.h"

#ifndef ZPU
#define iprintf printf
#define print printf
#endif

char* msg = "CPU endianness test\n";

unsigned int  a_number = 0x76543210;

unsigned short b_number = 0x3210;

unsigned char bc_number[2] = {0x02, 0x01};

unsigned char c_number[4] = {0x03, 0x02, 0x01, 0x00};


int five_thousand = 5000;
int ten = 10;

void putnum(unsigned int num)
{
  char  buf[9];
  int   cnt;
  char  *ptr;
  int   digit;

  ptr = buf;
  for (cnt = 7 ; cnt >= 0 ; cnt--)
  {
    digit = (num >> (cnt * 4)) & 0xf;

    if (digit <= 9)
      *ptr++ = (char) ('0' + digit);
    else
      *ptr++ = (char) ('a' - 10 + digit);
  }

  *ptr = (char) 0;

  print (buf);
}

int main()
{
    print (msg);

    iprintf ("A message from iprintf\n");
    putnum(five_thousand % ten);
    print ("\n");

    print ("WORD constant access:\n");
    print ("The following should be: \"00003210\": ");
    putnum(b_number);
    print ("\n");

    print ("LONG constant access:\n");
    print ("The following should be: \"76543210\": ");
    putnum(a_number);
    print ("\n");

    print ("The following shows WORD endianness:\n");
    putnum(*((unsigned short*)bc_number));
    print ("\n");
    print ("The following shows LONG endianness:\n");
    int c = *((int*)c_number);
    putnum(c);
    print("\n");

    if (c == 0x03020100)
        print ("This CPU is big endian\n");
    else if (c == 0x00010203)
        print ("This CPU is little endian\n");
    else
        print ("Endian error\n");
    return 0;
}

