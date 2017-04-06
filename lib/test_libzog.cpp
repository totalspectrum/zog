// WARNING Using stdio bloats the code by 14K bytes! 
#include <stdio.h>  

#ifdef FDX_ON_HEAP    
#include <stdlib.h>
#endif

#include "FullDuplexSerialPlus.h"
#include "VMCog.h"


// Create a virtual memory driver
int mailbox[4] = {0,0,0,0};
VMCog vm;


/////////////////////// I/O Redirection to FullDulexSerial //////////////////
int baud = 115200;
FullDuplexSerialPlus uart (31, 30, 0, baud);
// These are overrides of the ZPU's libgloss in/outbyte functions.
// They are use by stdio functions, printf, getc etc.
// By overriding them here we can redirect stdio any place we like. 
extern "C"
{
    void _initIO(void)
    {
    }
    
    void outbyte(int c)
    {
	uart.tx((int)c);
    }

    int inbyte ()
    {
	int val;
	val = uart.rx();
	return(val & 0xff);
    }
}
//////////////////////////////////////////////////////////////////////////////

int main(int argc, char* argv[])
{
    char data;
    
#ifdef FDX_ON_HEAP    
    // Get some heap for FDS
    char* m = (char*)malloc(sizeof(FullDuplexSerialPlus));    

    // Construct an FDS in that heap space, we do this because "new" is not working yet.
    uart = new (m) FullDuplexSerialPlus(31, 30, 0, baud);
#endif
    
    iprintf("\nPASM Firmware in C++ test.\n");
    
    iprintf("%8x\n", 0xdeadbeef);
 
    iprintf("Press a key\n");
    while (1)
    {
	data = uart.rx();
        uart.tx(data);
	break;	
    }
        
    iprintf("Starting VMCog...\n");
    vm.start(mailbox, 0x7C00, 2);
    iprintf("OK\n");
    char gotThis;
    int address = 0;
    while (1)
    {
        iprintf("Press any key and RETURN to continue\n");
	data = getchar();
        iprintf("%c\n", data);
  
        iprintf("Writing to VMCog...\n");
        vm.wrvbyte(address, data);
        iprintf("OK\n");
    
        iprintf("Reading from VMCog...\n");
        gotThis = vm.rdvbyte(address++);
        iprintf("Got %c\n", gotThis);
        iprintf("OK\n");
    }

#ifdef FDX_ON_HEAP    
    // Explicit destructor invocation, don't use delete
    uart->~FullDuplexSerialPlus();
    free (m);
#endif
    
    return(0);
}
