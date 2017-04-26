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
#define USE_XBYTE

' define USE_CORDIC_MULDIV to use P2 qmul and qdiv
'#define USE_CORDIC_MULDIV

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

zpu_breakpoint          jmp    #break


zpu_addsp_0
	_ret_		add     tos, tos                'Special case for offset = 0
zpu_addsp_4
			rdlong	data, ptrb[1]
	_ret_		add	tos, data
zpu_addsp_8
			rdlong	data, ptrb[2]
	_ret_		add	tos, data	
zpu_addsp_12
			rdlong	data, ptrb[3]
	_ret_		add	tos, data
zpu_addsp_16
			rdlong	data, ptrb[4]
	_ret_		add	tos, data
	
zpu_addsp               and     pa, #$0F
                        shl     pa, #2
                        add     pa, ptrb
			rdlong	data, pa
        _ret_           add     tos, data

zpu_loadsp_tos
	_ret_		wrlong	tos, ptrb--

zpu_loadsp_hi
                        and     pa, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
                        wrlong	tos, ptrb--
        _ret_           rdlong	tos, pa

zpu_loadsp              and     pa, #$1F
                        xor     pa, #$10           'Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
                        wrlong	tos, ptrb--
        _ret_           rdlong	tos, pa

zpu_storesp_0
	_ret_		rdlong	tos, ++ptrb

zpu_storesp_4
			wrlong	tos,ptrb[1]
	_ret_		rdlong	tos,++ptrb

zpu_storesp_8
			wrlong	tos,ptrb[2]
	_ret_		rdlong	tos,++ptrb

zpu_storesp_12
			wrlong	tos,ptrb[3]
	_ret_		rdlong	tos,++ptrb
	
zpu_storesp_16
			wrlong	tos,ptrb[4]
	_ret_		rdlong	tos,++ptrb
	
zpu_storesp_20
			wrlong	tos,ptrb[5]
	_ret_		rdlong	tos,++ptrb
	
zpu_storesp_hi
                        and     pa, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
			wrlong	tos, pa
        _ret_           rdlong	tos, ++ptrb wz

zpu_storesp             and     pa, #$1F
                        xor     pa, #$10           'Trust me, you need this.
                        shl     pa, #2
                        add     pa, ptrb
			wrlong	tos, pa
        _ret_           rdlong	tos, ++ptrb wz

zpu_config              mov     cpu, tos
       _ret_            rdlong	tos, ++ptrb wz

zpu_pushpc              wrlong	tos, ptrb--
                        mov     tos, pb
	_ret_		sub	tos, zpu_memory_addr

zpu_or                  rdlong  data, ++ptrb wz
        _ret_           or      tos, data

zpu_not
	_ret_		xor     tos, minus_one

zpu_load                mov     address, tos wc
	if_c		jmp	#read_zpu_tos	' special ZPU address emulation
			add	address, zpu_memory_addr
        _ret_           rdlong  tos, address

zpu_pushspadd           shl     tos, #2
                        add     tos, ptrb
	_ret_		sub	tos, zpu_memory_addr

zpu_store               rdlong	data, ++ptrb
                        mov     address, tos wc
			rdlong	tos, ++ptrb
	if_c		jmp	#write_long_zpu		' write special address
			add	address, zpu_memory_addr
	_ret_		wrlong	data, address

zpu_poppc               mov     pb, tos
			add	pb, zpu_memory_addr
                        rdlong	tos, ++ptrb wz
	_ret_		rdfast	zero, pb		' establish new pc

zpu_poppcrel            add     pb, tos
			rdlong	tos, ++ptrb wz
	_ret_		rdfast	zero, pb		' establish new pc

zpu_flip
	_ret_		rev     tos, #32

zpu_add                 rdlong	data, ++ptrb wz
        _ret_           add     tos, data

zpu_sub                 rdlong	data, ++ptrb wz
                        sub     data, tos
        _ret_           mov     tos, data

zpu_pushsp              wrlong	tos, ptrb--
                        mov     tos, ptrb
                        add     tos, #4
	_ret_		sub	tos, zpu_memory_addr

zpu_popsp               mov     ptrb, tos
			add	ptrb, zpu_memory_addr
	_ret_		rdlong	tos, ptrb

zpu_nop                 ret

zpu_and                 rdlong	data, ++ptrb wz
        _ret_           and     tos, data

zpu_xor                 rdlong	data, ++ptrb wz
        _ret_           xor     tos, data

zpu_loadb

                        mov     memp, tos
                        add     memp, zpu_memory_addr
        _ret_           rdbyte  tos, memp

zpu_storeb
			rdlong	data, ++ptrb wz
                        mov     memp, tos
                        add     memp, zpu_memory_addr
                        wrbyte  data, memp
	_ret_		rdlong	tos, ++ptrb wz

zpu_loadh               mov     address, tos
                        call    #read_word
        _ret_           mov     tos, data

zpu_storeh              rdlong	data, ++ptrb wz
                        mov     address, tos
                        call    #write_word
        _ret_           rdlong	tos, ++ptrb wz

zpu_lessthan            rdlong	data, ++ptrb
                        cmps    tos, data wz,wc
                        mov     tos, #0
              if_b      mov     tos, #1
	      		ret

zpu_lessthanorequal     rdlong	data, ++ptrb
                        cmps    tos, data wz,wc
                        mov     tos, #0
              if_be     mov     tos, #1
	      		ret

zpu_ulessthan           rdlong	data, ++ptrb
                        cmp     tos, data wz, wc
                        mov     tos, #0
              if_b      mov     tos, #1
	      		ret

zpu_ulessthanorequal    rdlong	data, ++ptrb
                        cmp     tos, data wz, wc
                        mov     tos, #0
              if_be     mov     tos, #1
	      		ret

zpu_swap                ror     tos, #16
			ret

zpu_mult16x16           rdlong	data, ++ptrb wz
                        and     data, word_mask
                        and     tos, word_mask
                        jmp     #fast_mul

zpu_eqbranch            rdlong	data, ++ptrb wz
              if_z      add     pb, tos
	      if_z	rdfast	zero, pb		' establish new pc
              _ret_     rdlong	tos, ++ptrb

zpu_neqbranch           rdlong	data, ++ptrb wz
              if_nz     add     pb, tos
	      if_nz	rdfast	zero, pb		' establish new pc
              _ret_     rdlong	tos, ++ptrb

zpu_mult                rdlong	data, ++ptrb wz
                        jmp     #fast_mul

zpu_div                 rdlong	data, ++ptrb wz
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_DIV_OP
                        jmp     #fast_div

zpu_mod                 rdlong	data, ++ptrb wz
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_REM_OP
                        jmp     #fast_div

zpu_lshiftright         rdlong	data, ++ptrb wz
                        shr     data, tos
         _ret_          mov     tos, data

zpu_ashiftleft          rdlong	data, ++ptrb wz
                        shl     data, tos
        _ret_           mov     tos, data

zpu_ashiftright         rdlong	data, ++ptrb wz
                        sar     data, tos
        _ret_           mov     tos, data

zpu_call                mov     temp, tos
                        mov     tos, pb
                        add     tos, #1
			sub	tos, zpu_memory_addr
                        mov     pb, temp
			add	pb, zpu_memory_addr
	_ret_		rdfast	zero, pb		' should be rdfast #0,pc but fastspin has a bug

zpu_callpcrel           mov     temp, tos
                        mov     tos, pb
                        add     tos, #1
			sub	tos, zpu_memory_addr
                        add     pb, temp
	_ret_		rdfast	zero, pb		' should be rdfast #0,pc but fastspin has a bug

zpu_eq                  rdlong	data, ++ptrb
                        cmp     tos, data wz
              if_z      mov     tos, #1
              if_nz     mov     tos, #0
	      		ret

zpu_neq                 rdlong	data, ++ptrb
                        sub     tos, data wz
              if_nz     mov     tos, #1
	      		ret

zpu_neg                 neg     tos, tos
			ret

zpu_syscall             jmp     #syscall

not_implemented
zpu_illegal
div_zero_error
                        jmp    #break

'------------------------------------------------------------------------------
'ZPU memory space access routines

'Read a LONG from ZPU memory at "address" into "tos"

read_zpu_tos
read_cog_long           cmp     address, zpu_io_start wc  'Check for COG memory access
              if_nc     jmp     #read_io_long

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
write_long_zpu          cmp     address, zpu_io_start wc
              if_nc     jmp     #write_io_long

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



'Read a WORD from ZPU memory at "address into "data"
read_word

                        mov     memp, address
                        add     memp, zpu_memory_addr
         _ret_          rdword  data, memp

'Write a WORD from "data" to ZPU memory at "address"
write_word
                        mov     memp, address
                        add     memp, zpu_memory_addr
        _ret_           wrword  data, memp

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
                        max     t2, data
                        min     data, tos
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
zpu_im_first            wrlong	tos, ptrb--
                        mov     tos, pa
                        shl     tos, #(32 - 7)          'Sign extend
                        sar     tos, #(32 - 7)

			'' fetch next byte and see if it is
			'' another im
.imloop
			getptr	pb
			rfbyte	pa
			cmpsub	pa, #$80 wc	' remove the 80 if it is present
	if_nc		jmp	#exec_non_im
			shl	tos, #7
			or	tos, pa
			jmp	#.imloop

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Main ZPU fetch and execute loop
next_instruction
			getptr	pb
			rfbyte	pa			'Some opcodes contain address offsets

#ifdef SINGLE_STEP
                        call    #break
#endif

exec_non_im
#ifndef USE_XBYTE

			push	#next_instruction
#endif
			rdlut	temp, pa
                        jmp     temp                    'No # here we are jumping through temp.

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
cpu                     add     address, #4          'The CPU type given by the CONGIG op.

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
			mov	ptra, dispatch_tab_addr
			mov	temp, #$80
			mov	address, #0
.lp1			rdbyte	data, ptra++
			wrlut	data, address
			add	address, #1
			djnz	temp, #.lp1

			' fill the rest of the table with run_zpu_im
			mov	data, #zpu_im_first
			mov	temp, #$80
.lp2			wrlut	data, address
			add	address, #1
			djnz	temp, #.lp2


#ifdef USE_XBYTE
restart_xbyte
			rdfast	zero, pb		' should be rdfast #0,pc but fastspin has a bug
			'' start up the xbyte loop
			push	 #$1f8
	_ret_		setq	 #$0		' 256 long execf table
			jmp	 #restart_xbyte
#else
			rdfast	zero, pb		' should be rdfast #0,pc but fastspin has a bug
			jmp	#next_instruction
#endif
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
zero			long 0
minus_one               long $FFFFFFFF
minus_two               long $FFFFFFFE
word_mask               long $0000FFFF
timer_address           long $80000100
sys_cognew_             long SYS_cognew

zpu_hub_start           long $10000000  'Start of HUB access window in ZPU memory space
zpu_cog_start           long $10008000  'Start of COG access window in ZPU memory space
zpu_io_start            long $10008800  'Start of IO access window
'------------------------------------------------------------------------------
                        fit     $1C0
'------------------------------------------------------------------------------
' The instruction dispatch look up table (in HUB)
dispatch_table
{00}    byte  zpu_breakpoint
{01}    byte  zpu_illegal
{02}    byte  zpu_pushsp
{03}    byte  zpu_illegal
{04}    byte  zpu_poppc
{05}    byte  zpu_add
{06}    byte  zpu_and
{07}    byte  zpu_or
{08}    byte  zpu_load
{09}    byte  zpu_not
{0A}    byte  zpu_flip
{0B}    byte  zpu_nop
{0C}    byte  zpu_store
{0D}    byte  zpu_popsp
{0E}    byte  zpu_illegal
{0F}    byte  zpu_illegal

{10}    byte  zpu_addsp_0
{11}    byte  zpu_addsp_4
{12}    byte  zpu_addsp_8
{13}    byte  zpu_addsp_12
{14}    byte  zpu_addsp_16
{15}    byte  zpu_addsp
{16}    byte  zpu_addsp
{17}    byte  zpu_addsp
{18}    byte  zpu_addsp
{19}    byte  zpu_addsp
{1A}    byte  zpu_addsp
{1B}    byte  zpu_addsp
{1C}    byte  zpu_addsp
{1D}    byte  zpu_addsp
{1E}    byte  zpu_addsp
{1F}    byte  zpu_addsp

{20}    byte  not_implemented' zpu_emulate
{21}    byte  not_implemented' zpu_emulate
{22}    byte  zpu_loadh
{23}    byte  zpu_storeh
{24}    byte  zpu_lessthan
{25}    byte  zpu_lessthanorequal
{26}    byte  zpu_ulessthan
{27}    byte  zpu_ulessthanorequal
{28}    byte  zpu_swap
{29}    byte  zpu_mult
{2A}    byte  zpu_lshiftright
{2B}    byte  zpu_ashiftleft
{2C}    byte  zpu_ashiftright
{2D}    byte  zpu_call
{2E}    byte  zpu_eq
{2F}    byte  zpu_neq

{30}    byte  zpu_neg
{31}    byte  zpu_sub
{32}    byte  zpu_xor
{33}    byte  zpu_loadb
{34}    byte  zpu_storeb
{35}    byte  zpu_div
{36}    byte  zpu_mod
{37}    byte  zpu_eqbranch
{38}    byte  zpu_neqbranch
{39}    byte  zpu_poppcrel
{3A}    byte  zpu_config
{3B}    byte  zpu_pushpc
{3C}    byte  zpu_syscall
{3D}    byte  zpu_pushspadd
{3E}    byte  zpu_mult16x16
{3F}    byte  zpu_callpcrel

{40}    byte  zpu_storesp
{41}    byte  zpu_storesp
{42}    byte  zpu_storesp
{43}    byte  zpu_storesp
{44}    byte  zpu_storesp
{45}    byte  zpu_storesp
{46}    byte  zpu_storesp
{47}    byte  zpu_storesp
{48}    byte  zpu_storesp
{49}    byte  zpu_storesp
{4A}    byte  zpu_storesp
{4B}    byte  zpu_storesp
{4C}    byte  zpu_storesp
{4D}    byte  zpu_storesp
{4E}    byte  zpu_storesp
{4F}    byte  zpu_storesp

{50}    byte  zpu_storesp_0
{51}    byte  zpu_storesp_4
{52}    byte  zpu_storesp_8
{53}    byte  zpu_storesp_12
{54}    byte  zpu_storesp_16
{55}    byte  zpu_storesp_20
{56}    byte  zpu_storesp_hi
{57}    byte  zpu_storesp_hi
{58}    byte  zpu_storesp_hi
{59}    byte  zpu_storesp_hi
{5A}    byte  zpu_storesp_hi
{5B}    byte  zpu_storesp_hi
{5C}    byte  zpu_storesp_hi
{5D}    byte  zpu_storesp_hi
{5E}    byte  zpu_storesp_hi
{5F}    byte  zpu_storesp_hi

{60}    byte  zpu_loadsp
{61}    byte  zpu_loadsp
{62}    byte  zpu_loadsp
{63}    byte  zpu_loadsp
{64}    byte  zpu_loadsp
{65}    byte  zpu_loadsp
{66}    byte  zpu_loadsp
{67}    byte  zpu_loadsp
{68}    byte  zpu_loadsp
{69}    byte  zpu_loadsp
{6A}    byte  zpu_loadsp
{6B}    byte  zpu_loadsp
{6C}    byte  zpu_loadsp
{6D}    byte  zpu_loadsp
{6E}    byte  zpu_loadsp
{6F}    byte  zpu_loadsp

{70}    byte  zpu_loadsp_tos
{71}    byte  zpu_loadsp_hi
{72}    byte  zpu_loadsp_hi
{73}    byte  zpu_loadsp_hi
{74}    byte  zpu_loadsp_hi
{75}    byte  zpu_loadsp_hi
{76}    byte  zpu_loadsp_hi
{77}    byte  zpu_loadsp_hi
{78}    byte  zpu_loadsp_hi
{79}    byte  zpu_loadsp_hi
{7A}    byte  zpu_loadsp_hi
{7B}    byte  zpu_loadsp_hi
{7C}    byte  zpu_loadsp_hi
{7D}    byte  zpu_loadsp_hi
{7E}    byte  zpu_loadsp_hi
{7F}    byte  zpu_loadsp_hi
'---------------------------------------------------------------------------------------------------------
'N.B. Do not add any more after the dispatch table.
'     It must be at the end such that it can be found and/or extracted by build
'     tools for use with C.
'---------------------------------------------------------------------------------------------------------
'The End.

