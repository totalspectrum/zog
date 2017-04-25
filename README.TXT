This is an emulator for the ZPU microprocessor, running on the
Parallax Propeller. Actually it's two emulators; the original zog.spin
written by Michael Rychlik (heater) and posted on the forums, and a
just in time compiler (zog_jit.spin) written by Eric Smith. They both
have the same interfaces, although zog.spin is much more flexible in
terms of what kind of memory it can access. They're both driven by a
common test harness (debug_zog.spin), also written by Michael Rychlik.

A preliminary port to the Propeller2 is in the file zog_p2.spin. This
is a strict interpreter (not JIT) and is basically the original
zog.spin, with various changes needed for P2 support. You'll need
fastspin 3.6.2 to compile this; do "fastspin -2 debug_zog.spin" to get
a P2 debug_zog.binary. The P2 port has been enhanced over time, so it
now can use the XBYTE interpreter and the hardware multiply divide.
Both of those features are enabled by #defines in zog_p2.spin.

Unlike the original ZOG, this emulator relies on a little endian ZPU
toolchain. The GCC toolchain I used is checked in to

  https://github.com/totalspectrum/zpugccle
  

The original README.TXT is below:

                                  ZOG
                                  ----

       A ZPU Virtual machine running on the Parallax Propeller
                          micro-controller.                                      


What?    The ZPU is a 32 bit processor core architecture specified by ZyLin
         consultants for which there a number of HDL implementations
         for FPGA's.                                                

Why?     ZyLin have targeted the GCC C Compiler to the ZPU instruction set.
         ZOG makes it possible to use GCC for Propeller development.      
         All ZPU instructions are a single byte, which should work out     
         nicely when the code is placed in external RAM with an 8 bit bus. 

Credits: Based on an original idea by Toby Seckshund.                      
         Maths routines courtesy of Cluso and Parallax.                    
         Encouragement courtesy of Bill Henning and all on the Parallax    
         Propeller discussion forum.                                       

Author:  Michael Rychlik 

Change History - see end of this file.
--------------------------------------


Notes:
------

Zog can run ZPU programs from HUB memory or from external RAM.
For external RAM Zog can use Bill Henning's VMCog and hence any external memory hardware
supported my VMCog. Alternatively 32MB of external SDRAM can be used via Jazzed's  SdramCache object.
See the #defines" in debug_zog.spin for the available options and uncomment one of them.

Zog has been tested on a TriBlade board (blade #2) with VMCog and on a 32MB RAM solution via SdramCache. 

External RAM execution is about 10 times slower.

Currently Zogs executable binary is included from a "file" statement when using HUB RAM or
pulled off an SD card when using external memory. The latter allows use of ZPU binaries up to the external memory size.

debug_zog uses a serial link for user interface so should work on any Propeller board with BST or other terminal program.

To adapt Zog for your hardware, serial port, SD card pins etc use a userdefs.spin file.
This can be created by copying usedefs_template into your Spin library directory and
adjusting it's content for your hardware.


Compiling C for Zog:
--------------------

The "test.bin" file included in the ZPU memory area is created with
GCC for the ZPU.

Please see the Makefile included with Zog for how to build C programs
with GCC for the ZPU architecture that Zog can run.

Generally it goes like this:

First compile for the "phi" platform with size optimizations and "relaxed" linkage.

$ zpu-elf-gcc -Os -phi  -Wl,--relax -Wl,--gc-sections  -o test test.c

The extract the binary:

$ zpu-elf-objcopy -O binary test test.bin

Reverse the endianness of the binary file:

$ objcopy -I binary -O binary --reverse-bytes=4  test.bin

If you want to see what's in there disassemble with:

$ zpu-elf-objdump -d test


Memory Map
----------

The Zog emulator allows up to 256MB of RAM for normal ZPU code stack and data.
This is followed by a 32KB window that is mapped to the Propeller COG RAM.
Followed by a 2K window mapped into the COG running Zog itself, this enables
access to the special COG registers.
All other memory addresses are used or memory mapped I/O.


Change History:
---------------

v0.1     09-02-2010 First draft, totally untested.

v0.2      18-02-2010 Tested and fixed up initial bugs.
                     Runs first test program compiled with GCC.
                     Has no I/O yet.    

v0.3      19-02-2010 Fixed mysterious "inversion of bit four of offset" problem
                     with LOADSP and STORESP.

v0.4      20-02-2010 Added primitive UART output such that the C run time
                     routine outbyte() works. Zog now says hello!

v0.8	  18-04-2010 Rewrote all the ZPU ops using optimizations as
                     suggested by Bill Henning. 

v0.9      15-04-2010 Added some optimizations (tailored to  FIBO :))
                     Added RC4 test (Fails just now)
                     Fixed PUSHSP op.

v0.11     09-05-2010 Changes endianess of bytecodes in zpu_memory
                     Adjusted read/write byte/word and instruction fetch inline with endianness
                     Fixed ADDSP.


v0.12     11-05-2010 The ZPU_EMULATE op now works. If it's ever needed.
                     Fixed MOD and DIV instructions.
                     Inlined read/write_byte
                     read/write_word are EMULLATED for now as their endiannes is not right yet.
                     ZPU memory is 20K bytes (Until we put SD and VM COG back in).

v0.13     14-05-2010 Fixed endiannes of strings output by SYS_write syscall.
                     Return a zero result from SYS_fstat syscall.

v0.14     08-06-2010 Take Bill Henning's VMCog into use for external RAM access.
                     Added #ifdef USE_VIRTUAL_MEMORY

v0.15     15-06-2010 Fixed zpu_addsp to work correctly with virtual mem read_long.
                     Added reading of a ZPU image from SD card file.
                     Runs dhrystone by default (dstone.bin)

v1.1      17-08-2010 Zog is now broken out into a spearate spin file with minimal spin code.
                     debug_zog.spin now contains the spin support for zog I/O via ports and SYSCALLS.
                     run_zog.spin is now a minimal zog loader with no I/O.

v1.2      17-08-2010 No changes here but all make files for ZPU executables now perform
                     and endianness swap on the binaries as byte swapping is not done here anymore.

v1.3      18-08-2010 Overlaoded all Zog variables against the init code
                     freeing up many LONGs. (42 Free now).

v1.4      26-08-2010 Remove some redundant use of the nos variable and nos itself.
                     Removed decode_mask, now use self-modified jmp to select first/next IM.
                     Fixed ZPU_SWAP.
                     Fixed zpu_storeh and zu_loadh (No longer uses EMULATE)
                     Removed EMULATE handler as it is no longer required.
                     Thanks to Lonsesock who spotted errors/optimization opportunities in:
                          zpu_addsp
                          zpu_swap
                          zpu_neq
                          math_F4
v1.5      29-08-2010 Added support for Jazzed's 32MByte SDRAM external RAM solution.
                        ...define USE_JCACHED_MEMORY to use that.
                     Backed out math_F4 optimizations (reverting to v1.3 math_F4)
