#define ALWAYS
'*******************************************************************************
'*                                                                             *
'* ZOG      A ZPU Virtual machine running on the Parallax Propeller            *
'*          micro-controller.                                                  *
'*                                                                             *
'* What?    The ZPU is a 32 bit processor core architecture specified by ZyLin *
'*          consultants for which there a number of HDL implementations        *
'*          for FPGA's.                                                        *
'*                                                                             *
'* Why?     ZyLin have targeted the GNU C Compiler to the ZPU instruction set. *
'*          ZOG makes it possible to use GCC for Propeller development.        *
'*          All ZPU instructions are a single byte, which should work out      *
'*          nicely when the code is placed in external RAM with an 8 bit bus.  *
'*                                                                             *
'*          Based on an original idea by Toby Seckshund.                       *
'*          Maths routines courtesy of Cluso and Parallax.                     *
'*                                                                             *
'*          Encouragement courtesy of Bill Henning and all on the Parallax     *
'*          Propeller discussion forum.                                        *
'*                                                                             *
'* Author:  Michael Rychlik                                                    *
'* P2 Port: Eric Smith                                                         *
'*                                                                             *
'*******************************************************************************
'
' v0.1      09-02-2010 First draft, totally untested.
'
' v0.2      18-02-2010 Tested and fixed up initial bugs.
'                      Runs first test program compiled with GCC.
'                      Has no I/O yet.
'
' v0.3      19-02-2010 Fixed mysterious "inversion of bit four of offset" problem
'                      with LOADSP and STORESP.
'
' v0.4      20-02-2010 Added primitive UART output such that the C run time
'                      routine outbyte() works. Zog now says hello!
'
' v0.5      28-02-2010 The CONFIG instruction is now always emulated,
'                      see emulate code in libgloss crt0.S for why.
'                      Added handling of the SYSCALL instruction via the io_command trap mechainism.
'                      Added fsrwFemto object (inititialization calls commented out for now).
'
' v0.6      xx-03-2010 Changed UART object to FullDuplexSerialPlus.
'
' v0.8      18-04-2010 Rewrote all ops with optimizations as suggested by Bill Henning.
'
' v0.9      25-04-2010 Optimized some most used ops in the FIBO test (wink, wink:)
'                      Fixed PUSHSP.
'
' v0.11     09-05-2010 Changes endianess of bytecodes in zpu_memory
'                      Adjusted read/write byte/word and instruction fetch inline with endianness
'                      Fixed ADDSP.
'
' v0.12     11-05-2010 The ZPU_EMULATE op now works. If it's ever needed.
'                      Fixed MOD and DIV instructions.
'                      Inlined read/write_byte
'                      read/write_word are EMULLATED for now as their endiannes is not right yet.
'                      ZPU memory is 20K bytes (Until we put SD and VM COG back in).

' v0.13     14-05-2010 Fixed endiannes of strings output by SYS_write syscall.
'                      Return a zero result from SYS_fstat syscall.
'
' v0.14     08-06-2010 Take Bill Henning's VMCog into use for external RAM access.
'                      Added #ifdef USE_VIRTUAL_MEMORY
'
' v0.15     15-06-2010 Fixed zpu_addsp to work correctly with virtual mem read_long.
'                      Added reading of a ZPU image from SD card file.
'                      Runs dhrystone by default (dstone.bin)
'
' v0.16     ??-06-2010 Fixed zpu_emulate for virtual memory usage.
'                      Take VMCog v0.975 into use.
'
' v0.17     20-06-2010 Ensure still builds for HUB memory use.
'
' v0.18     23-06-2010 Test for IM instruction in execute loop and halve the dispatch table size.
'                      Moved clearing of decode_mask into execute loop, saves many LONGs
'                      Change testing of decode_mask to use tjz
'                      Removed FIBO optimization code. We don't like to cheat.
'                      Changed dispatch table entries to BYTES, halving it's size again
'                      and saving one instruction in the exececute loop.
'                      Simplified checking for I/O in read/write_long
'                      These changes bring a 13% boost to fibo performance! (Running from HUB).
'
' v0.19     23-06-2010 read/write long now have access to HUB memory at virtual address $10000
'
' v0.20     01-07-2010 Now Zog C can start a cog via SYS_cognew syscall.
'
' v0.21     03-07-2010 Now runs new "pasm_objects_test" that tests C++ versions
'                      of FDS and VMCOG.
'
' v0.22     04-07-2010 Now handle SYS_cognew in PASM rather than Spin.
'
' v1.0      08-07-2010 Zog is now split out into it's own Spin file with minimal Spin
'                      startup code.
'                      Can now totally replace Spin and take over the entire Prop.
'                      N.B. Virtual Memory opertion is broken at this point.
'
' v1.1      15-08-2010 Fixed virtual memory operation.
'
' v1.2      17-08-2010 Just bumping version in line with  other files..
'
' v1.3      18-08-2010 Overlaoded all Zog variables against the init code
'                      freeing up many LONGs. (42 Free now).
'
' v1.4      26-08-2010 Remove some redundant use of the nos variable and nos itself.
'                      Removed decode_mask, now use self-modified jmp to select first/next IM.
'                      Fixed ZPU_SWAP.
'                      Fixed zpu_storeh and zu_loadh (No longer uses EMULATE)
'                      Removed EMULATE handler as it is no longer required.
'                      Thanks to Lonesock who spotted errors/optimization opportunities in:
'                           zpu_addsp
'                           zpu_swap
'                           zpu_neq
'                           math_F4
'
' v1.5      28-08-2010 Added support for Jazzed's 32MByte SDRAM external RAM solution.
'                         ...define USE_JCACHED_MEMORY to use that.
'                      Backed out math_F4 optimizations (reverting to v1.3 math_F4)
'
' v1_6      31-08-2010 Fixed up memory map for zpu_read/write_long
'                          256MB ZPU RAM
'                           32KB HUB RAM window
'                            2KB COG RAM window
'                          All the rest for memory mapped IO
'                      Removed redundant AND from zpu_lshiftright, zpu_ashiftleft
'                          and zpu_ashiftright as suggested by Lonesock.
'                      New smaller multiply and divide routines provide by Lonesock
'                          affects zpu_mult16x16, zpu_mult, zpu_div, zpu_mod.
'                      zpu_load/storesp optimized by providing speciaö case
'                          implementations for different offset field values. Lonesock.
'                      zpu_addsp_0 special case added. Lonesock.
'
' v1_7                 Eric Smith: removed endianness swaps to work with little
'                          endian zpu toolchain
'
'
'#define SINGLE_STEP

' define USE_XBYTE to use P2 xbyte execution mechanism
'#define USE_XBYTE

' define USE_CORDIC_MULDIV to use P2 qmul and qdiv
#define USE_CORDIC_MULDIV

#ifdef USE_XBYTE
#define RET_START_ALTERNATE      _ret_ setq2  #$100
#define RET_CONTINUE_ALTERNATE   _ret_ setq2  #$100
#else
#define RET_START_ALTERNATE     _ret_ mov jmp_table_base, #$100
#define RET_CONTINUE_ALTERNATE  _ret_ mov jmp_table_base, #$100
#define RESTORE_JMP_TABLE       mov jmp_table_base, #0
#endif

CON
' These are the SPIN byte codes for mul and div
SPIN_DIV_OP     = 0 '$F6  '(divide, return quotient 32 bits)
SPIN_REM_OP     = 1 '$F7  '(divide, return remainder 32 bits)

'I/O control block commands
io_cmd_out      = $01
io_cmd_in       = $02
io_cmd_break    = $03
io_cmd_syscall  = $04

'System  call ID numbers. Taken from libgloss syscall.h
'These are required by the ANSI C part of newlib (excluding system() of course).
SYS_exit        =  1
SYS_open        =  2
SYS_close       =  3
SYS_read        =  4
SYS_write       =  5
SYS_lseek       =  6
SYS_unlink      =  7
SYS_getpid      =  8
SYS_kill        =  9
SYS_fstat       = 10
'SYS_sbrk       = 11 - not currently a system call, but reserved.
'ARGV support.
SYS_argvlen     = 12
SYS_argv        = 13
'These are extras added for one reason or another.
SYS_chdir       = 14
SYS_stat        = 15
SYS_chmod       = 16
SYS_utime       = 17
SYS_time        = 18
'Propeller specific actions
SYS_cognew      = 8000
SYS_coginit     = 8001
SYS_cogstop     = 8002

'VMCOG Commands
READVMB       = 130 'Read a byte from VMcog
READVMW       = 131 'Read a word from VMcog
READVML       = 132 'Read a long from VMcog

WRITEVMB      = 133 'Write a byte to VMcog
WRITEVMW      = 134 'Write a word to VMcog
WRITEVML      = 135 'Write a long to VMcog

VAR
  long cog

PUB start (params) | okay
  okay := cog := cognew(@enter, params) + 1   'Start emulator in a new COG

PUB stop
'FIXME we need this

PUB getdispatch_table
  return @dispatch_table

PUB getzog
  return @enter

DAT
                        org     0
enter                   jmp     #init
'------------------------------------------------------------------------------
'Opcode handlers.
PEND_zpu_breakpoint
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_breakpoint          jmp    #break

PEND_zpu_addsp_0
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_addsp_0
	_ret_		add     tos, tos                'Special case for offset = 0

PEND_zpu_addsp_N
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_addsp_N
			'' use execf mask mechanism to select just one of the below 8
			rdlong	data, ptrb[1]	       ' only one
			rdlong	data, ptrb[2]
			rdlong	data, ptrb[3]
			rdlong	data, ptrb[4]
			rdlong	data, ptrb[5]
			rdlong	data, ptrb[6]
			rdlong	data, ptrb[7]
			rdlong	data, ptrb[8]		' only one
	_ret_		add	tos, data		' always execute
	
PEND_zpu_addsp
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_addsp               and     pa, #$0F
                        shl     pa, #2
                        add     pa, ptrb
			rdlong	data, pa
        _ret_           add     tos, data

PEND_zpu_loadsp_tos
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadsp_tos
	_ret_		wrlong	tos, ptrb--

PEND_zpu_loadsp_N
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadsp_N
			wrlong	tos, ptrb--
			'' use an execf mask to select one of these 8
	_ret_		rdlong	tos, ptrb[2]
	_ret_		rdlong	tos, ptrb[3]
	_ret_		rdlong	tos, ptrb[4]
	_ret_		rdlong	tos, ptrb[5]
	
	_ret_		rdlong	tos, ptrb[6]
	_ret_		rdlong	tos, ptrb[7]
	_ret_		rdlong	tos, ptrb[8]
	_ret_		rdlong	tos, ptrb[9]
	
PEND_zpu_loadsp_hi
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadsp_hi
                        and     pa, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
                        wrlong	tos, ptrb--
        _ret_           rdlong	tos, pa

PEND_zpu_loadsp
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadsp              and     pa, #$1F
                        xor     pa, #$10           'Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
                        wrlong	tos, ptrb--
        _ret_           rdlong	tos, pa

PEND_zpu_storesp_0
	_ret_		mov	tos, PendingTos
zpu_storesp_0
	_ret_		rdlong	tos, ++ptrb

PEND_zpu_storesp_N
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_storesp_N
			'' use an EXECF mask to select just one of the wrlongs
			wrlong	tos,ptrb[1]  	' only one
			wrlong	tos,ptrb[2]	' only one
			wrlong	tos,ptrb[3]	' only one
			wrlong	tos,ptrb[4]	' only one
			wrlong	tos,ptrb[5]  	' only one
			wrlong	tos,ptrb[6]	' only one
			wrlong	tos,ptrb[7]	' only one
			wrlong	tos,ptrb[8]	' only one
	_ret_		rdlong	tos,++ptrb

PEND_zpu_storesp_hi
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_storesp_hi
                        and     pa, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
			wrlong	tos, pa
        _ret_           rdlong	tos, ++ptrb wz

PEND_zpu_storesp
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_storesp             and     pa, #$1F
                        xor     pa, #$10           'Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
			wrlong	tos, pa
        _ret_           rdlong	tos, ++ptrb wz

PEND_zpu_config
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_config              mov     cpu, tos
       _ret_            rdlong	tos, ++ptrb wz


PEND_zpu_pushpc
			wrlong	tos, ptrb--
			mov	tos, PendingTos

zpu_pushpc              wrlong	tos, ptrb--
			sub	pb, #1
                        mov     tos, pb
	_ret_		sub	tos, zpu_memory_addr

PEND_zpu_not
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_not
	_ret_		xor     tos, minus_one



PEND_zpu_load
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_load                mov     address, tos wc
	if_c		jmp	#read_zpu_tos	' special ZPU address emulation
			add	address, zpu_memory_addr
        _ret_           rdlong  tos, address

PEND_zpu_pushspadd
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_pushspadd           shl     tos, #2
                        add     tos, ptrb
	_ret_		sub	tos, zpu_memory_addr

PEND_zpu_store
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_store
			rdlong	data, ++ptrb
                        mov     address, tos wc
			rdlong	tos, ++ptrb
	if_c		jmp	#write_long_zpu		' write special address
			add	address, zpu_memory_addr
	_ret_		wrlong	data, address

PEND_zpu_poppc
			mov	pb, PendingTos
			add	pb, zpu_memory_addr
	_ret_		rdfast	#0, pb
	
zpu_poppc               mov     pb, tos
			add	pb, zpu_memory_addr
                        rdlong	tos, ++ptrb wz
	_ret_		rdfast	#0, pb		' establish new pc

PEND_zpu_poppcrel
			sub	pb, #1
			add	pb, PendingTos
	_ret_		rdfast	#0, pb		' establish new pc

zpu_poppcrel
			sub	pb, #1
			add     pb, tos
			rdlong	tos, ++ptrb wz
	_ret_		rdfast	#0, pb		' establish new pc

PEND_zpu_flip
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_flip
	_ret_		rev     tos

PEND_zpu_pushsp
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_pushsp              wrlong	tos, ptrb--
                        mov     tos, ptrb
                        add     tos, #4
	_ret_		sub	tos, zpu_memory_addr


PEND_zpu_popsp
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_popsp               mov     ptrb, tos
			add	ptrb, zpu_memory_addr
	_ret_		rdlong	tos, ptrb

PEND_zpu_nop
			wrlong	tos, ptrb--
	_ret_		mov	tos, PendingTos
zpu_nop                 ret

			'' common math routines, for SKIPF
			'' add, and can share
PEND_zpu_math
			mov	data, tos
			mov	tos, PendingTos
zpu_math
			rdlong	data, ++ptrb wz         ' SKIP this if PEND
        _ret_           add     tos, data
        _ret_           and     tos, data
        _ret_           or      tos, data
        _ret_           subr    tos, data		' tos = data - tos
        _ret_           xor     tos, data

PEND_zpu_loadb
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadb
                        add     tos, zpu_memory_addr
        _ret_           rdbyte  tos, tos

PEND_zpu_storeb
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_storeb
			rdlong	data, ++ptrb
                        add     tos, zpu_memory_addr
                        wrbyte  data, tos
	_ret_		rdlong	tos, ++ptrb

PEND_zpu_loadh
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_loadh               add     tos, zpu_memory_addr
	_ret_		rdword	tos, tos


PEND_zpu_storeh
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_storeh              rdlong	data, ++ptrb
			add	tos, zpu_memory_addr
			wrword	data, tos
        _ret_           rdlong	tos, ++ptrb

PEND_zpu_lessthan
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_lessthan            rdlong	data, ++ptrb
                        cmps    tos, data wz,wc
                        mov     tos, #0
              _ret_     muxc    tos, #1


PEND_zpu_lessthanorequal
			mov	data, tos
			mov	tos, PendingTos
			cmps	data, tos wcz
			mov	tos, #0
	_ret_		muxnc	tos, #1

zpu_lessthanorequal     rdlong	data, ++ptrb
                        cmps    data, tos wz,wc
                        mov     tos, #0
              _ret_     muxnc   tos, #1		' set to 1 if !(data < tos)


PEND_zpu_ulessthan
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_ulessthan           rdlong	data, ++ptrb
                        cmp     tos, data wz, wc
                        mov     tos, #0
              _ret_     muxc    tos, #1		' set to 1 if tos < data


PEND_zpu_ulessthanorequal
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_ulessthanorequal    rdlong	data, ++ptrb
                        cmp  	data, tos wz, wc
                        mov     tos, #0
              _ret_     muxnc   tos, #1		' set to 1 if ! (data < tos)


PEND_zpu_swap
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_swap
	_ret_		ror     tos, #16



PEND_zpu_mult16x16
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_mult16x16           rdlong	data, ++ptrb wz
                        and     data, word_mask
                        and     tos, word_mask
                        jmp     #fast_mul

PEND_zpu_eqbranch
			cmp	tos, #0 wz
			sub	pb, #1
		if_z	add	pb, PendingTos
		if_z	rdfast	#0, pb
		_ret_	rdlong	tos, ++ptrb

zpu_eqbranch            rdlong	data, ++ptrb wz
			sub	pb, #1
              if_z      add     pb, tos
	      if_z	rdfast	#0, pb		' establish new pc
              _ret_     rdlong	tos, ++ptrb

PEND_zpu_neqbranch
			cmp	tos, #0 wz
			sub	pb, #1
		if_nz	add	pb, PendingTos
		if_nz	rdfast	#0, pb
		_ret_	rdlong	tos, ++ptrb

zpu_neqbranch           rdlong	data, ++ptrb wz
			sub	pb, #1
              if_nz     add     pb, tos
	      if_nz	rdfast	#0, pb		' establish new pc
              _ret_     rdlong	tos, ++ptrb

PEND_zpu_mult
			mov	data, tos
			mov	tos, PendingTos
			jmp	#fast_mul

zpu_mult                rdlong	data, ++ptrb wz
                        jmp     #fast_mul

PEND_zpu_div
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_div                 rdlong	data, ++ptrb wz
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_DIV_OP
                        jmp     #fast_div

PEND_zpu_mod
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_mod                 rdlong	data, ++ptrb wz
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_REM_OP
                        jmp     #fast_div

			'' the shifts
			'' do lshiftright, ashiftleft, ashiftright in that order
			'' we will use an EXECF mask to select one of the three
			'' middle opcodes
PEND_zpu_shiftroutine
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_shiftroutine
		        rdlong	data, ++ptrb wz
                        shr     data, tos		' only one
                        shl     data, tos		' only one
                        sar     data, tos		' only one
         _ret_          mov     tos, data

PEND_zpu_call
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_call
			mov     temp, tos
                        mov     tos, pb
			sub	tos, zpu_memory_addr
                        mov     pb, temp
			add	pb, zpu_memory_addr
	_ret_		rdfast	#0, pb

PEND_zpu_callpcrel
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_callpcrel           mov     temp, tos
                        mov     tos, pb
			sub	tos, zpu_memory_addr
			sub	pb, #1
                        add     pb, temp
	_ret_		rdfast	#0, pb

PEND_zpu_eq
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_eq                  rdlong	data, ++ptrb
                        cmp     tos, data wz
              		mov     tos, #0
              _ret_     muxz	tos, #1

PEND_zpu_neq
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_neq                 rdlong	data, ++ptrb
                        sub     tos, data wz
              if_nz     mov     tos, #1
	      		ret

PEND_zpu_neg
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_neg
		_ret_	neg     tos, tos

PEND_zpu_syscall
			wrlong	tos, ptrb--
			mov	tos, PendingTos
zpu_syscall             jmp     #syscall

PEND_zpu_illegal
			wrlong	tos, ptrb--
			mov	tos, PendingTos
not_implemented
zpu_illegal
div_zero_error
                        jmp    #break

'------------------------------------------------------------------------------
'ZPU memory space access routines

'Read a LONG from ZPU memory at "address" into "tos"

read_zpu_tos
read_cog_long           cmp     address, zpu_cog_start wc  'Check for COG memory access
              if_c      jmp     #read_io_long

                        shr     address, #2
                        alts    address, #0
     _ret_              mov     tos, 0-0

read_io_long            cmp     address, timer_address wz 'Check for timer read
	      if_nz	jmp	#read_other
              _ret_     getct   tos

read_other                                                      'Must be other I/O address
                        wrlong  address, io_port_addr 'Set port address
                        mov     temp, #io_cmd_in      'Set I/O command to IN
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #.wait
	      _ret_     rdlong  tos, io_data_addr    'Get the port data

			
'Write a LONG from "data" to ZPU memory at "address"
write_long_zpu          cmp     address, zpu_cog_start wc
              if_c      jmp     #write_io_long

                        shr     address, #2
                        altd    address, #0
    _ret_               mov     0-0, data
   

write_io_long           wrlong  address, io_port_addr 'Set port address
                        wrlong  data, io_data_addr    'Set port data
                        mov     temp, #io_cmd_out     'Set I/O command to OUT
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #.wait
	      		ret

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
fast_mul
#ifdef USE_CORDIC_MULDIV
			setq	#0
			qmul	tos, data
	_ret_		getqx	tos
#else
			' speed up by counting via smaller item
                        abs     tos, tos        wc
                        negc    data, data
                        abs     data, data      wc
                        ' make t2 the smaller of the 2 unsigned parameters
                        mov     t2, tos
                        fle     t2, data
                        fge     data, tos
                        ' correct the sign of the adder
                        negc    data, data

                        ' my accumulator
                        mov     tos, #0
                        ' do the work
.mul_loop               shr     t2, #1          wc,wz   ' get the low bit of t2
        if_c            add     tos, data               ' if it was a 1, add adder to accumulator
                        shl     data, #1                ' shift the adder left by 1 bit
        if_nz           jmp     #.mul_loop              ' continue as long as there are no more 1's
                        ' "Run home, Jack!"
			ret
#endif

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
{{==    div_flags: xxxx_invert result_store remainder   ==}}
{{==    NOTE: Caller must not allow data == 0!!!!       ==}}
fast_div                ' tos = tos / data

#ifdef USE_CORDIC_MULDIV
			abs	tos, tos	wc
			muxc	t2,#%11		' store sign of tos
			abs	data, data	wc,wz
	if_c		xor	t2,#%10		' store sign of data
			setq	#0
			qdiv	tos, data
			test	div_flags, #1 wz
	if_nz		jmp	#do_remainder
			getqx	tos		' get quotient
			test	t2,#%10 wc	' restore sign
	_ret_		negc	tos,tos

do_remainder
			getqy	tos		' get remainder
			test	t2, #%1 wc
	_ret_		negc	tos,tos


#else
                        ' handle the signs, and check for a 0 divisor
                        and     div_flags, #1   wz      ' keep only the 0 bit, and remember if it's a 0
                        abs     t1, data        wc
             if_z_and_c or      div_flags, #2           ' data was negative, and we're looking for quotient, so set bit 1 hi
                        abs     data, tos       wc
              if_c      xor     div_flags, #2           ' tos was negative, invert bit 1 (quotient or remainder)
                        ' align the divisor to the leftmost bit
			mov	t2, #1	        wc
                        neg     t2, t2		        ' count how many times we shift (negative)
.align_loop             rcl     t1, #1          wc      ' left shift the divisior, marking when we hit a 1
              if_nc     djnz    t2, #.align_loop        ' the divisior MUST NOT BE 0
                        rcr     t1, #1                  ' restore the 1 bit we just nuked
                        neg     t2, t2                  ' how many times did we shift? (we started at -1 and counted down)
                        ' perform the division
                        mov     tos, #0
.div_loop               cmpsub  data, t1        wc      ' does the divisor fit into the dividend?
                        rcl     tos, #1                 ' if it did, store a one while shifting left
                        shr     t1, #1                  '
                        djnz    t2, #.div_loop
                        ' correct the sign
                        shr     div_flags, #1   wc,wz
              if_c      mov     tos, data               ' user wanted the remainder, not the quotient
                        negnz   tos, tos                ' need to invert the result
                        ret
#endif

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
			'' handle im 0-3f
zpu_im_pos_first
			mov	PendingTos, pa
			and	PendingTos, #$3f
			RET_START_ALTERNATE

'' handle im 40-7f
imsignextend		long	$FFFFFF80
zpu_im_neg_first
                        mov     PendingTos, pa
			or	PendingTos, imsignextend
			RET_START_ALTERNATE

zpu_im_next
			shl	PendingTos, #7
			and	pa, #$7f
			or	PendingTos, pa
			RET_CONTINUE_ALTERNATE

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Main ZPU fetch and execute loop
main_loop
#ifdef USE_XBYTE
			rdfast	#0, pb
			'' start up the xbyte loop
			push	 #$1ff
	_ret_		setq	 #$0		' 256 long execf table
			jmp	 #main_loop	' should never get here

			' padding to make the code the same size
			nop
			nop
			nop
			nop
			nop
#else
			rdfast	#0, pb
next_instruction
			rfbyte	pa			'Some opcodes contain address offsets
			getptr	pb

			push	#next_instruction
			mov	temp, pa
			add	temp, jmp_table_base
			RESTORE_JMP_TABLE
			rdlut	temp, temp
                        execf   temp                    'No # here we are jumping through temp.
#endif

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'Initialisation code.
'This is a nice mess, all Zogs variables are overlaid against this code.
init
address                 mov     address, ptra            'Pick up first 6 longs of PAR block
zpu_memory_addr         rdlong  zpu_memory_addr, address 'HUB address of the ZPU memory area
memp                    add     address, #4              'Temporary pointer into ZPU memory space
zpu_memory_sz           rdlong  zpu_memory_sz, address   'Size of ZPU memory area in bytes
temp                    add     address, #4          'For temp operands etc
                        rdlong  pb, address          'ZPU Program Counter
instruction             add     address, #4          'Opcode being executed.
                        rdlong  ptrb, address          'ZPU Stack Pointer
tos                     add     address, #4          'Top Of Stack
dispatch_tab_addr       rdlong  dispatch_tab_addr, address'HUB address of instruction dispatch table
data                    add     address, #4          'Data parameter for read, write etc
                      '  rdlong  pasm_addr, address
cpu                     add     address, #4          'The CPU type given by the CONFIG op.

' div_flags was called a
div_flags               rdlong  temp, address        '7th par item is address of ZOG I/O mailbox
io_command_addr         mov     io_command_addr, temp'HUB address of I/O command byte.
x                       add     temp, #4             'Maths var.
io_port_addr            mov     io_port_addr, temp   'HUB address of I/Ö port number
y                       add     temp, #4             'Maths var.
io_data_addr            mov     io_data_addr, temp   'HUB address of I/O data
t1                      add     temp, #4             'Maths var.
pc_addr                 mov     pc_addr, temp        'HUB address of PC
t2                      add     temp, #4             'Maths var.
sp_addr                 mov     sp_addr, temp        'HUB address of SP
coginit_dest            add     temp, #4             'Used for coginit instruction.
tos_addr                mov     tos_addr, temp       'HUB address of tos
                        add     temp, #4
dm_addr                 mov     dm_addr, temp        'HUB address of decode mask
                        add     temp, #4             'Flag for the IM instruction
debug_addr              mov     debug_addr, temp     'HUB address of debug register

			add	ptrb, zpu_memory_addr
			add	pb, zpu_memory_addr

			' set up dispatch table in LUT
			setq2   #$7f	  ' load bytecode table
			rdlong	$0, dispatch_tab_addr

			' fill the rest of the table with ZPU instructions
			mov	address, #$80
			mov	data, #zpu_im_pos_first
			mov	temp, #$40
.lp2			wrlut	data, address
			add	address, #1
			djnz	temp, #.lp2

			mov	data, #zpu_im_neg_first
			mov	temp, #$40
.lp3			wrlut	data, address
			add	address, #1
			djnz	temp, #.lp3

			' now set up the alternate dispatch table for when immediates are pending
			setq2  #$7f
			rdlong $100, dispatch_table_alternate_ptr

			' and set up the rest of the table
			mov	address, #$180
			mov	data, #zpu_im_next
			mov	temp, #$80
.lp4			wrlut	data, address
			add	address, #1
			djnz	temp, #.lp4
			jmp	#main_loop
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
break
			mov	memp, pb
			sub	memp, zpu_memory_addr
	                wrlong  memp, pc_addr             'Dump registers to HUB.
			mov	memp, ptrb
			sub	memp, zpu_memory_addr
                        wrlong  memp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  data, dm_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #.wait
break_ret               ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
syscall                 mov     address, ptrb             'Get syscall ID from stack
                        add     address, #8
			rdlong	data, address
                        cmp     data, sys_cognew_ wz    'Is it SYS_cognew?
              if_nz     jmp     #syscall_external_handler

'On SYS_cognew syscall:
'ZPU [SP +  8] is syscall ID
'    [SP + 16] is address of PASM code to load
'    [SP + 20] is address of PAR for Cog
'Cog number or error code is returned in ZPU R0 which is memory address 0.
handle_sys_cognew
			jmp	#zpu_illegal		'not implemented on P2

syscall_external_handler
			mov	memp, ptrb
			sub	memp, zpu_memory_addr
                        wrlong  memp, sp_addr
                        mov     temp, #io_cmd_syscall   'Set I/O command to SYSCALL
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz'Wait for command completion
              if_nz     jmp     #.wait
                        rdlong  ptrb, sp_addr
			add	ptrb, zpu_memory_addr
			ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Big constants
PendingTos		long 0
zero			long 0
minus_one               long $FFFFFFFF
minus_two               long $FFFFFFFE
word_mask               long $0000FFFF
timer_address           long $80000100
sys_cognew_             long SYS_cognew
jmp_table_base		long 0

zpu_io_base             long $80000000  'Start of IO access window
zpu_cog_start           long $80008000  'Start of COG access window in ZPU memory space
dispatch_table_alternate_ptr
			long @@@dispatch_table_alternate
			
'------------------------------------------------------------------------------
                        fit     $1C0
'------------------------------------------------------------------------------
' The instruction dispatch look up table (in HUB)
dispatch_table_alternate
{00}    long  PEND_zpu_breakpoint
{01}    long  PEND_zpu_illegal
{02}    long  PEND_zpu_pushsp
{03}    long  PEND_zpu_illegal
{04}    long  PEND_zpu_poppc
{05}    long  PEND_zpu_math | %0_0000_100 << 10		' add
{06}    long  PEND_zpu_math | %0_0001_100 << 10		' and
{07}    long  PEND_zpu_math | %0_0011_100 << 10		' or
{08}    long  PEND_zpu_load
{09}    long  PEND_zpu_not
{0A}    long  PEND_zpu_flip
{0B}    long  PEND_zpu_nop
{0C}    long  PEND_zpu_store
{0D}    long  PEND_zpu_popsp
{0E}    long  PEND_zpu_illegal
{0F}    long  PEND_zpu_illegal

{10}    long  PEND_zpu_addsp_0
{11}    long  PEND_zpu_addsp_N | %0_1111_1110_00 << 10
{12}    long  PEND_zpu_addsp_N | %0_1111_1101_00 << 10
{13}    long  PEND_zpu_addsp_N | %0_1111_1011_00 << 10
{14}    long  PEND_zpu_addsp_N | %0_1111_0111_00 << 10
{15}    long  PEND_zpu_addsp_N | %0_1110_1111_00 << 10
{16}    long  PEND_zpu_addsp_N | %0_1101_1111_00 << 10
{17}    long  PEND_zpu_addsp_N | %0_1011_1111_00 << 10
{18}    long  PEND_zpu_addsp_N | %0_0111_1111_00 << 10
{19}    long  PEND_zpu_addsp
{1A}    long  PEND_zpu_addsp
{1B}    long  PEND_zpu_addsp
{1C}    long  PEND_zpu_addsp
{1D}    long  PEND_zpu_addsp
{1E}    long  PEND_zpu_addsp
{1F}    long  PEND_zpu_addsp

{20}    long  PEND_zpu_illegal 'not_implemented' zpu_emulate
{21}    long  PEND_zpu_illegal 'not_implemented' zpu_emulate
{22}    long  PEND_zpu_loadh
{23}    long  PEND_zpu_storeh
{24}    long  PEND_zpu_lessthan
{25}    long  PEND_zpu_lessthanorequal
{26}    long  PEND_zpu_ulessthan
{27}    long  PEND_zpu_ulessthanorequal
{28}    long  PEND_zpu_swap
{29}    long  PEND_zpu_mult
{2A}    long  PEND_zpu_shiftroutine | %0_110_0_00 << 10	' shr
{2B}    long  PEND_zpu_shiftroutine | %0_101_0_00 << 10	' shl
{2C}    long  PEND_zpu_shiftroutine | %0_011_0_00 << 10 ' sar
{2D}    long  PEND_zpu_call
{2E}    long  PEND_zpu_eq
{2F}    long  PEND_zpu_neq

{30}    long  PEND_zpu_neg
{31}    long  PEND_zpu_math | %0_0111_1_00 << 10    ' sub
{32}    long  PEND_zpu_math | %0_1111_1_00 << 10    ' xor
{33}    long  PEND_zpu_loadb
{34}    long  PEND_zpu_storeb
{35}    long  PEND_zpu_div
{36}    long  PEND_zpu_mod
{37}    long  PEND_zpu_eqbranch
{38}    long  PEND_zpu_neqbranch
{39}    long  PEND_zpu_poppcrel
{3A}    long  PEND_zpu_config
{3B}    long  PEND_zpu_pushpc
{3C}    long  PEND_zpu_syscall
{3D}    long  PEND_zpu_pushspadd
{3E}    long  PEND_zpu_mult16x16
{3F}    long  PEND_zpu_callpcrel

{40}    long  PEND_zpu_storesp
{41}    long  PEND_zpu_storesp
{42}    long  PEND_zpu_storesp
{43}    long  PEND_zpu_storesp
{44}    long  PEND_zpu_storesp
{45}    long  PEND_zpu_storesp
{46}    long  PEND_zpu_storesp
{47}    long  PEND_zpu_storesp
{48}    long  PEND_zpu_storesp
{49}    long  PEND_zpu_storesp
{4A}    long  PEND_zpu_storesp
{4B}    long  PEND_zpu_storesp
{4C}    long  PEND_zpu_storesp
{4D}    long  PEND_zpu_storesp
{4E}    long  PEND_zpu_storesp
{4F}    long  PEND_zpu_storesp

{50}    long  PEND_zpu_storesp_0
{51}    long  PEND_zpu_storesp_N | %0_1111_1110_00 << 10
{52}    long  PEND_zpu_storesp_N | %0_1111_1101_00 << 10
{53}    long  PEND_zpu_storesp_N | %0_1111_1011_00 << 10
{54}    long  PEND_zpu_storesp_N | %0_1111_0111_00 << 10
{55}    long  PEND_zpu_storesp_N | %0_1110_1111_00 << 10
{56}    long  PEND_zpu_storesp_N | %0_1101_1111_00 << 10
{57}    long  PEND_zpu_storesp_N | %0_1011_1111_00 << 10
{58}    long  PEND_zpu_storesp_N | %0_0111_1111_00 << 10
{59}    long  PEND_zpu_storesp_hi
{5A}    long  PEND_zpu_storesp_hi
{5B}    long  PEND_zpu_storesp_hi
{5C}    long  PEND_zpu_storesp_hi
{5D}    long  PEND_zpu_storesp_hi
{5E}    long  PEND_zpu_storesp_hi
{5F}    long  PEND_zpu_storesp_hi

{60}    long  PEND_zpu_loadsp
{61}    long  PEND_zpu_loadsp
{62}    long  PEND_zpu_loadsp
{63}    long  PEND_zpu_loadsp
{64}    long  PEND_zpu_loadsp
{65}    long  PEND_zpu_loadsp
{66}    long  PEND_zpu_loadsp
{67}    long  PEND_zpu_loadsp
{68}    long  PEND_zpu_loadsp
{69}    long  PEND_zpu_loadsp
{6A}    long  PEND_zpu_loadsp
{6B}    long  PEND_zpu_loadsp
{6C}    long  PEND_zpu_loadsp
{6D}    long  PEND_zpu_loadsp
{6E}    long  PEND_zpu_loadsp
{6F}    long  PEND_zpu_loadsp

{70}    long  PEND_zpu_loadsp_tos
{71}    long  PEND_zpu_loadsp_N | %0_1111_1110_000 << 10
{72}    long  PEND_zpu_loadsp_N | %0_1111_1101_000 << 10
{73}    long  PEND_zpu_loadsp_N | %0_1111_1011_000 << 10
{74}    long  PEND_zpu_loadsp_N | %0_1111_0111_000 << 10
{75}    long  PEND_zpu_loadsp_N | %0_1110_1111_000 << 10
{76}    long  PEND_zpu_loadsp_N | %0_1101_1111_000 << 10
{77}    long  PEND_zpu_loadsp_N | %0_1011_1111_000 << 10
{78}    long  PEND_zpu_loadsp_N | %0_0111_1111_000 << 10
{79}    long  PEND_zpu_loadsp_hi
{7A}    long  PEND_zpu_loadsp_hi
{7B}    long  PEND_zpu_loadsp_hi
{7C}    long  PEND_zpu_loadsp_hi
{7D}    long  PEND_zpu_loadsp_hi
{7E}    long  PEND_zpu_loadsp_hi
{7F}    long  PEND_zpu_loadsp_hi

' Normal instruction state
dispatch_table
{00}    long  zpu_breakpoint
{01}    long  zpu_illegal
{02}    long  zpu_pushsp
{03}    long  zpu_illegal
{04}    long  zpu_poppc
{05}    long  zpu_math | %0_0000_0 << 10		' add
{06}    long  zpu_math | %0_0001_0 << 10		' and
{07}    long  zpu_math | %0_0011_0 << 10		' or
{08}    long  zpu_load
{09}    long  zpu_not
{0A}    long  zpu_flip
{0B}    long  zpu_nop
{0C}    long  zpu_store
{0D}    long  zpu_popsp
{0E}    long  zpu_illegal
{0F}    long  zpu_illegal

{10}    long  zpu_addsp_0
{11}    long  zpu_addsp_N | %0_1111_1110 << 10
{12}    long  zpu_addsp_N | %0_1111_1101 << 10
{13}    long  zpu_addsp_N | %0_1111_1011 << 10
{14}    long  zpu_addsp_N | %0_1111_0111 << 10
{15}    long  zpu_addsp_N | %0_1110_1111 << 10
{16}    long  zpu_addsp_N | %0_1101_1111 << 10
{17}    long  zpu_addsp_N | %0_1011_1111 << 10
{18}    long  zpu_addsp_N | %0_0111_1111 << 10
{19}    long  zpu_addsp
{1A}    long  zpu_addsp
{1B}    long  zpu_addsp
{1C}    long  zpu_addsp
{1D}    long  zpu_addsp
{1E}    long  zpu_addsp
{1F}    long  zpu_addsp

{20}    long  not_implemented' zpu_emulate
{21}    long  not_implemented' zpu_emulate
{22}    long  zpu_loadh
{23}    long  zpu_storeh
{24}    long  zpu_lessthan
{25}    long  zpu_lessthanorequal
{26}    long  zpu_ulessthan
{27}    long  zpu_ulessthanorequal
{28}    long  zpu_swap
{29}    long  zpu_mult
{2A}    long  zpu_shiftroutine | %0_110_0 << 10	' shr
{2B}    long  zpu_shiftroutine | %0_101_0 << 10	' shl
{2C}    long  zpu_shiftroutine | %0_011_0 << 10 ' sar
{2D}    long  zpu_call
{2E}    long  zpu_eq
{2F}    long  zpu_neq

{30}    long  zpu_neg
{31}    long  zpu_math | %0_0111_0 << 10    ' sub
{32}    long  zpu_math | %0_1111_0 << 10    ' xor
{33}    long  zpu_loadb
{34}    long  zpu_storeb
{35}    long  zpu_div
{36}    long  zpu_mod
{37}    long  zpu_eqbranch
{38}    long  zpu_neqbranch
{39}    long  zpu_poppcrel
{3A}    long  zpu_config
{3B}    long  zpu_pushpc
{3C}    long  zpu_syscall
{3D}    long  zpu_pushspadd
{3E}    long  zpu_mult16x16
{3F}    long  zpu_callpcrel

{40}    long  zpu_storesp
{41}    long  zpu_storesp
{42}    long  zpu_storesp
{43}    long  zpu_storesp
{44}    long  zpu_storesp
{45}    long  zpu_storesp
{46}    long  zpu_storesp
{47}    long  zpu_storesp
{48}    long  zpu_storesp
{49}    long  zpu_storesp
{4A}    long  zpu_storesp
{4B}    long  zpu_storesp
{4C}    long  zpu_storesp
{4D}    long  zpu_storesp
{4E}    long  zpu_storesp
{4F}    long  zpu_storesp

{50}    long  zpu_storesp_0
{51}    long  zpu_storesp_N | %0_1111_1110 << 10
{52}    long  zpu_storesp_N | %0_1111_1101 << 10
{53}    long  zpu_storesp_N | %0_1111_1011 << 10
{54}    long  zpu_storesp_N | %0_1111_0111 << 10
{55}    long  zpu_storesp_N | %0_1110_1111 << 10
{56}    long  zpu_storesp_N | %0_1101_1111 << 10
{57}    long  zpu_storesp_N | %0_1011_1111 << 10
{58}    long  zpu_storesp_N | %0_0111_1111 << 10
{59}    long  zpu_storesp_hi
{5A}    long  zpu_storesp_hi
{5B}    long  zpu_storesp_hi
{5C}    long  zpu_storesp_hi
{5D}    long  zpu_storesp_hi
{5E}    long  zpu_storesp_hi
{5F}    long  zpu_storesp_hi

{60}    long  zpu_loadsp
{61}    long  zpu_loadsp
{62}    long  zpu_loadsp
{63}    long  zpu_loadsp
{64}    long  zpu_loadsp
{65}    long  zpu_loadsp
{66}    long  zpu_loadsp
{67}    long  zpu_loadsp
{68}    long  zpu_loadsp
{69}    long  zpu_loadsp
{6A}    long  zpu_loadsp
{6B}    long  zpu_loadsp
{6C}    long  zpu_loadsp
{6D}    long  zpu_loadsp
{6E}    long  zpu_loadsp
{6F}    long  zpu_loadsp

{70}    long  zpu_loadsp_tos
{71}    long  zpu_loadsp_N | %0_1111_1110_0 << 10
{72}    long  zpu_loadsp_N | %0_1111_1101_0 << 10
{73}    long  zpu_loadsp_N | %0_1111_1011_0 << 10
{74}    long  zpu_loadsp_N | %0_1111_0111_0 << 10
{75}    long  zpu_loadsp_N | %0_1110_1111_0 << 10
{76}    long  zpu_loadsp_N | %0_1101_1111_0 << 10
{77}    long  zpu_loadsp_N | %0_1011_1111_0 << 10
{78}    long  zpu_loadsp_N | %0_0111_1111_0 << 10
{79}    long  zpu_loadsp_hi
{7A}    long  zpu_loadsp_hi
{7B}    long  zpu_loadsp_hi
{7C}    long  zpu_loadsp_hi
{7D}    long  zpu_loadsp_hi
{7E}    long  zpu_loadsp_hi
{7F}    long  zpu_loadsp_hi
'---------------------------------------------------------------------------------------------------------
'N.B. Do not add any more after the dispatch table.
'     It must be at the end such that it can be found and/or extracted by build
'     tools for use with C.
'---------------------------------------------------------------------------------------------------------
'The End.

