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

unsigned int ukrap = 100;
unsigned int umodder = 13;

int a_number = 0xdeadbeef;


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

unsigned int fibo (unsigned int n)
{
        if (n <= 1)
        {
                return (n);
        }
        else
        {
                return fibo(n - 1) + fibo(n - 2);
        }
}

int main (int argc,  char* argv[])
{
	int n;
	int result;
	char* helloMsg =  "Zog say hello\n";
	char* promptMsg =  "Please type 4 characters...\n";
	char outstr[100];

	// PROBLEM: printf does not  work
        iprintf("Hello world\n");

	// Enable using SYSCALL instruction for read(), write() printf() etc
//	_use_syscall = 1;

        siprintf(outstr, "The meaning of life = %d\n", 42);
	print(outstr);

	// print() always uses direct UART I/O
	print("Hi\n");

	// Hello message, write() uses syscall if _use_syscall = 1 
        result = write(1, helloMsg, strlen(helloMsg));
	putnum (result);
	print ("\n");

	// Print some libgloss/crt stuff
	print("argc = ");
	putnum(argc);
	print ("\n");
	
	print("Address of argv[0] = ");
	putnum((int)argv[0]);
	print ("\n");

	n = _hardware;
	print ("_hardware = ");
	putnum (n);
	print ("\n");

	n = _cpu_config;
	print ("_cpu_config = ");
	putnum (n);
	print ("\n");

	n = ZPU_ID;
	print ("ZPU_ID = ");
	putnum (n);
	print ("\n");

	print ("_use_syscall = ");
	putnum(_use_syscall);
	print ("\n");

        result = write(1, promptMsg, strlen(promptMsg));
        result = read(0, promptMsg, 4);
	promptMsg[4] = 0;
	print("You typed...\n");
	result = write(1, promptMsg, strlen(promptMsg));
	print("\n");

	print("The following should be deadbeef\n"); 
	putnum(a_number);
	print("\n");

	unsigned int z;
	z = ukrap % umodder;
	print("The following should be 00000009\n"); 
	putnum(z);
	print("\n");

        print("The following should be 0001da31\n");
        n = 26;
        result = fibo(n);
        print ("fibo(");
        putnum(n);
        print (") = ");
        putnum(result);
        print("\n");

        print("The following should be \"Bye\"\n");

//	_use_syscall = 0;

	iprintf("Bye\n");
	return(0);
}

