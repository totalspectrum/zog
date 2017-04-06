'********************************************************************************
'*                                                                              *
'* ZOG_SMALL A ZPU Virtual machine running on the Parallax Propeller            *
'*           micro-controller.                                                  *
'*                                                                              *
'*           ZOG_JIT is a compact version of ZOG that compiles (at run time!)   *
'*           the ZPU instructions to native Propeller instructions. It is based *
'*           on Michael Rychlik's ZOG_SMALL, but it's been extensively          *
'*           modified by me (Eric Smith) and is, alas, much uglier now          *
'*                                                                              *
'* What?     The ZPU is a 32 bit processor core architecture specified by ZyLin *
'*           consultants for which there a number of HDL implementations        *
'*           for FPGA's.                                                        *
'*                                                                              *
'* Why?      ZyLin have targeted the GNU C Compiler to the ZPU instruction set. *
'*           ZOG makes it possible to use GCC for Propeller development.        *
'*           All ZPU instructions are a single byte, which should work out      *
'*           nicely when the code is placed in external RAM with an 8 bit bus.  *
'*                                                                              *
'*           Based on an original idea by Toby Seckshund.                       *
'*           Maths routines courtesy of Cluso and Parallax.                     *
'*                                                                              *
'*           Encouragement courtesy of Bill Henning and all on the Parallax     *
'*           Propeller discussion forum.                                        *
'*                                                                              *
'* Authors:  Eric Smith, Michael Rychlik                                        *
'*                                                                              *
'********************************************************************************
'
' v0.2      2017-04-06 First zog 1.6 compatible version (original development
'                      was based on zog 1.0)
'
' Michael's original change log below:
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

' v0.13     19-05-2010 Fixed endiannes of strings output by SYS_write syscall.
'                      Return a zero result from SYS_fstat syscall.
'                      Note: ZOG_SMAL is created for the first time from v0.13 of ZOG
'
' v0.14     08-06-2010 Take Bill Henning's VMCog into use for external RAM access.
'                      Added #ifdef USE_VIRTUAL_MEMORY
'
' v0.15     08-06-2010 Fixed zpu_addsp to work correctly with virtual mem read_long.
'
' The "test.bin" file included in the ZPU memory area is created with
' GCC for the ZPU.
'
' Please see the Makefile included with Zog for how to build C programs
' with GCC for the ZPU architecture that Zog can run.
'
'#define SINGLE_STEP
'#define CYCLE_COUNT

CON
' cache configuration
' CACHE_LINE_SIZE must be a power of 2
'' define TWO_LINE_CACHE for a 2 way cache
'' otherwise we have only a single line
'#define TWO_LINE_CACHE
' CACHE_LINE_SIZE = 32

CACHE_LINE_SIZE = 64
CACHE_LINE_MASK = (CACHE_LINE_SIZE-1)

' These are the SPIN byte codes for mul and div
SPIN_MUL_OP     = $F4  '(multiply, return lower 32 bits)
SPIN_DIV_OP     = $F6  '(divide, return quotient 32 bits)
SPIN_REM_OP     = $F7  '(divide, return remainder 32 bits)

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

'Standard file decriptors
STDIN_FILENO    = 0
STDOUT_FILENO   = 1
STDERR_FILENO   = 2

CON
  READVMB       = 130 'Read a byte from VMcog
  READVMW       = 131 'Read a word from VMcog
  READVML       = 132 'Read a long from VMcog

  WRITEVMB      = 133 'Write a byte to VMcog
  WRITEVMW      = 134 'Write a word to VMcog
  WRITEVML      = 135 'Write a long to VMcog


VAR
  long cog
  
PUB start (params) : okay
  okay := cog := cognew(@enter, params) + 1                  'Start emulator in a new COG

PUB stop
'' Stop CPU simulator - frees a cog
  if cog
    cogstop(cog~ - 1)

PUB getdispatch_table
  return @dispatch_table

PUB getzog
  return @enter

DAT

                        org     0
enter
                        jmp     #init
			
zpu_nop
			nop
			nop

'------------------------------------------------------------------------------

			'' just compile the 2 instructions pointed to by the dest field
zpu_literal2
			shr	aux_opcode, #9	' extract dest field
			movs	ccopy, aux_opcode
			jmp	#ccopy_next
			
'------------------------------------------------------------------------------
'Opcode handlers.
zpu_callrelpc
			mov	data, tos
			jmpret	intern_pc, #call_pc_rel	'' uses get_next_pc, needs internal pc


'' compile zpu_im instructions
'' for the first move we have to sign extend the 7 bit immediate
'' we take advantage of the fact that the NEG and MOV instructions differ only
'' by 1 bit:
'' MOV = %101000_001_0_1111_000000000_000000000
'' NEG = %101001_001_0_1111_000000000_000000000
'' so we can mux on 1 bit to toggle between them
''
mux_mask		long	%000001_000_0_0000_000000000_000000000
first_im
			mov	im_flag, #later_im
			shl     address, #(32 - 7)
                        sar     address, #(32 - 7)
			abs	address, address wc
			muxc	im_set_impl+1, mux_mask	    ' set NEG if c, MOV if nc
			movs	im_set_impl+1, address
			movs	ccopy, #im_set_impl
			jmp	#ccopy_next

im_set_impl
			call	#push_tos
			mov	tos, #0-0	' this could become a neg

later_im
                        and     address, #$7F
                       	movs    later_set_impl+1, address
			movs	ccopy, #later_set_impl
                        jmp     #ccopy_next
later_set_impl
			shl	tos, #7
			or	tos, #0-0

zpu_breakpoint
			jmpret	intern_pc, #dummy	'' break calls get_next_pc
                        call    #break


zpu_addsp0
			add	tos, tos
			nop
zpu_addsp
			and	address, #$0F
            		shl	address, #2
	    		movs	buildasp, address
	    		movs	ccopy, #buildasp
			jmp	#ccopy_next

buildasp
			mov	address, #0-0
			call	#zpu_addsp_impl

zpu_addsp_impl
			add	address, sp
			rdlong	data, address
			add	tos, data
zpu_addsp_impl_ret
			ret


zpu_loadsp
			mov	build2b, loadsp_call
			jmp	#zpu_build2
zpu_storesp
			mov	build2b, storesp_call
zpu_build2
                        and     address, #$1F
                        xor     address, #$10           'Trust me, you need this.
                        shl     address, #2
			movs	build2, address
			movs	ccopy, #build2
			jmp	#ccopy_next
			
build2
			mov	address, #0-0
build2b
			call	#zpu_loadsp_impl	'' this is actually a dummy that will be overwritten

zpu_loadsp_impl
                        add     address, sp
                        call    #push_tos
                        rdlong	tos, address
zpu_loadsp_impl_ret
                        ret

loadsp_call		call	#zpu_loadsp_impl
storesp_call		call	#zpu_storesp_impl

zpu_storesp_impl
                        add     address, sp
                        wrlong	tos, address
			jmp	#pop_tos	'' will return correctly from there


zpu_pushpc
                        call    #push_tos
			jmpret	intern_pc, #zpu_pushpc_impl  '' set intern_pc for get_next_pc
zpu_pushpc_impl
			call	#get_next_pc
                        mov     tos, cur_pc
			sub	tos, #1
                        jmp	intern_pc

zpu_binaryop
			shr	aux_opcode, #23		' extract instruction field
			movi	build_op+1, aux_opcode
			movs	ccopy, #build_op	' set up to copy instructions
			jmp	#ccopy_next		' copy them into place

build_op
			call	#pop_tos
			add	tos, data

zpu_not
                        xor     tos, minus_one
                        nop
zpu_neg
			neg	tos,tos
			nop

zpu_dobranch
			'' replace condition in template
			and	aux_opcode, con_mask
			andn	branch_codes+1, con_mask
			or	branch_codes+1, aux_opcode
			'' and copy instructions
			movs   ccopy, #branch_codes
			jmp    #ccopy_next

branch_codes
			jmpret	intern_pc, #cmpbranch  '' set intern_pc for get_next_pc
  if_z			jmp	#set_pc_rel_data

cmpbranch
			call	#get_next_pc
			call	#pop_tos
			cmp	tos, #0 wz	'' condition code used when we return
  			call	#discard_tos
  			jmp	intern_pc
			
			' compile an unsigned comparison
			' function to call (cmp_unsigned_impl or
			' cmp_signed_impl) is in dest field of aux_opcode
zpu_cmp
			mov	data, aux_opcode
			shr	data, #9
			movs	cmp_codes, data
			
			'' replace condition
			and	aux_opcode, con_mask
			andn	cmp_codes+1, con_mask
			or	cmp_codes+1, aux_opcode
			'' and copy instructions
			movs   ccopy, #cmp_codes
			jmp    #ccopy_next
			
cmp_codes
			jmpret	cmp_ret, #0-0
	if_never	mov	tos, #1

cmp_unsigned_impl
                        call    #pop_tos
                        cmp     data, tos wc,wz
			mov	tos, #0
cmp_ret
			ret

cmp_signed_impl
                        call    #pop_tos
                        cmps    data, tos wc,wz
			mov	tos, #0
			jmp	cmp_ret

			' mask for condition codes
con_mask		long	$f<<18


zpu_load
                        mov     address, tos
			call	#read_long

zpu_store
                        call    #pop_tos
			call	#zpu_store_impl
zpu_store_impl
                        mov     address, data
                        call    #pop_tos
                        call    #write_long
zpu_store_impl_ret
                        ret

zpu_poppc
                        call    #pop_tos
                        jmp     #set_pc_data

zpu_flip
                        rev     tos, #32
			nop

zpu_pushsp
                        call    #push_tos
			call	#zpu_pushsp_impl

zpu_pushsp_impl
                        mov     tos, sp
			sub	tos, zpu_memory_addr
                        add     tos, #4
zpu_pushsp_impl_ret
			ret

zpu_pushspadd
			shl	tos, #2
			call	#zpu_pushspadd_impl

zpu_pushspadd_impl
			add	tos, sp
			sub	tos, zpu_memory_addr
zpu_pushspadd_impl_ret
			ret

zpu_popsp
                        mov     sp, tos
			call	#zpu_popsp_impl
zpu_popsp_impl
			add	sp, zpu_memory_addr
			rdlong	tos, sp
zpu_popsp_impl_ret
			ret

stub_mult16x16
                        call    #pop_tos
			call	#zpu_mult16x16

			'' compile an emulate sequence
zpu_emulate
			sub	address, #32
			movs	builde, address
			movs	ccopy, #builde
			jmp	#ccopy_next
			
builde
			mov	address, #0-0
			jmpret	intern_pc, #zpu_emulate_impl  '' get_next_pc needs internal pc

zpu_emulate_impl
			call	#get_next_pc
			call    #push_tos
                        mov     tos, cur_pc               'Push return address
			shl	address, #5
                        mov     cur_pc, address                'Op code to PC
                        jmp     #set_pc

div_zero_error
zpu_illegal
			call	#break
			nop
			
'------------------------------------------------------------------------------

' dummy routine to set up intern_pc
dummy
			jmp	intern_pc

'' COG internal PC address
intern_pc		long	icache0+1	' store return address here
'' start PC of current cache line
cur_cache_tag		long	$DEADBEEF
'' base of current cache line
cur_cache_base	   	long	icache0

#ifdef TWO_LINE_CACHE
'' last cache line
prev_cache_tag		long	$EEEEEEEE
prev_cache_base		long	icache1
#endif


' get PC of next instruction (pc+1 in ZPU documentation terms) into cur_pc
'' this relies on intern_pc being set up

get_next_pc
			mov	cur_pc, intern_pc
			add	cur_pc, #1		' 2 COG instructions per ZPU one, so round up
			sub	cur_pc, cur_cache_base	' offset from cache start
			shr	cur_pc, #1		' convert to number of instructions
			add	cur_pc, cur_cache_tag
get_next_pc_ret
			ret

' call PC relative:
' must be called with jmpret intern_pc, #call_pc_rel
' assumes that data contains the offset
call_pc_rel
			call	#get_next_pc
			mov	tos, cur_pc		' save next pc
			'' fall through
' set PC relative from data
set_pc_rel_data
			sub	cur_pc, #1		' adjust to current pc
			add	cur_pc, data
			jmp	#set_pc
' set PC from data
set_pc_data
			mov	cur_pc, data
' set PC from cur_pc
set_pc
			mov	intern_pc, cur_pc
			and	intern_pc, #CACHE_LINE_MASK
			shl	intern_pc, #1		'' 2 COG instrs per ZPU one
			andn	cur_pc, #CACHE_LINE_MASK

			'' is the desired line already in cache?
			cmp	cur_cache_tag, cur_pc wz
    	if_z		jmp	#cache_full
#ifdef TWO_LINE_CACHE
			'' is it in the previous cache line?
			'' in any case our current line is about to become
			'' the second most recently used, so swap
			mov	t1, prev_cache_tag
			mov	prev_cache_tag, cur_cache_tag
			mov	cur_cache_tag, t1

			mov	t1, prev_cache_base
			mov	prev_cache_base, cur_cache_base
			mov	cur_cache_base, t1

			cmp	cur_cache_tag, cur_pc wz
       if_z		jmp	#cache_full
#endif

			mov	cur_cache_tag, cur_pc
  			call	#fill		

cache_full
			add	intern_pc, cur_cache_base
			jmp	intern_pc
			
'------------------------------------------------------------------------------
'ZPU memory space access routines

'Push a LONG onto the stack from "tos"
push_tos

                        wrlong  tos, sp
                        sub     sp, #4
push_tos_ret            ret

'Pop a LONG from the stack into "tos", leave flags alone
'old tos gets placed in "data"
' to just throw old tos away, call discard_tos
pop_tos
			mov	data, tos
discard_tos
			add     sp, #4
			rdlong  tos, sp
discard_tos_ret
pop_tos_ret
zpu_storesp_impl_ret
			ret

'Read a LONG from ZPU memory at "address" into "tos"
read_long
			test	address, io_mask wz
	      if_nz	jmp	#special_read
                        mov     memp, address
                        add     memp, zpu_memory_addr
                        rdlong  tos, memp
read_long_ret           ret
special_read
			cmp     address, uart_address wz
              if_z      jmp     #in_long
                        cmp     address, timer_address wz
              if_z      mov     tos, cnt
	      if_nz     mov	tos, #0
                        jmp     read_long_ret
			
in_long                 mov     tos, #$100
                        jmp     #read_long_ret

'------------------------------------------------------------------------------

'Write a LONG from "data" to ZPU memory at "address"
write_long              cmp     address, zpu_hub_start wc 'Check for normal memory access
              if_nc     jmp     #write_hub_long
                        mov     memp, address
                        add     memp, zpu_memory_addr
                        wrlong  data, memp

write_long_ret		ret

write_hub_long          cmp     address, zpu_cog_start wc 'Check for HUB memory access
              if_nc     jmp     #write_cog_long

                        sub     address, zpu_hub_start
                        wrlong  data, address
                        jmp     #write_long_ret

write_cog_long          cmp     address, zpu_io_start wc
              if_nc     jmp     #write_io_long

                        shr     address, #2
                        movd    :wcog, address
                        nop
:wcog                   mov     0-0, data
                        jmp     #write_long_ret

write_io_long           wrlong  address, io_port_addr 'Set port address
                        wrlong  data, io_data_addr    'Set port data
                        mov     temp, #io_cmd_out     'Set I/O command to OUT
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #:wait
                        jmp     #write_long_ret

'------------------------------------------------------------------------------
'
' copy CNT cog longs
'
ccopy			
			mov 0-0, 0-0
			add ccopy, cstep
			djnz cnt, #ccopy
ccopy_ret
			ret
cstep			long (1<<9) + 1

'
' do ccopy then do next instruction
'
ccopy_next
			call	#ccopy
			jmp	#nexti
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'Maths routines borrowed from the Cluso Spin interpreter.
'In turn orrowed from the Parallax Spin interpreter.
'$F4..F7 = MUL/DIV lower/upper result
'
' a" holds the opcode (lower 5 bits as shown in the first block of code... e.g. MPY=$F4=%10100)
' "x" and "y" are the two numbers to be multiplied (or divided) x * y or x / y, result is in "x" (pushed).
'
' So, if you use this routine with a set to...
'  a = $F4 (multiply, return lower 32 bits)
'  a = $F5 (multiply, return upper 32 bits)
'  a = $F6 (divide, return quotient 32 bits)
'  a = $F7 (divide, return remainder 32 bits)

zpu_mult16x16
                        mov     x, tos
                        and     x, word_mask
                        mov     y, data
                        and     y, word_mask
                        mov     a, #SPIN_MUL_OP
			'' fall through

math_F4                 and     a,#%11111               '<== and mask (need in a)
                        mov     t1,#0
                        mov     t2,#32                  'multiply/divide
                        abs     x,x             wc
                        muxc    a,#%01100
                        abs     y,y             wc,wz
        if_c            xor     a,#%00100
                        test    a,#%00010       wc      'set c if divide (DIV/MOD)
        if_c_and_nz     jmp     #mdiv                   'if divide and y=0, do multiply so result=0
                        shr     x,#1            wc      'multiply
mmul    if_c            add     t1,y            wc
                        rcr     t1,#1           wc
                        rcr     x,#1            wc
                        djnz    t2,#mmul
                        test    a,#%00100       wz
        if_nz           neg     t1,t1
        if_nz           neg     x,x             wz
        if_nz           sub     t1,#1
                        test    a,#%00001       wz
        if_nz           mov     x,t1
                        mov     tos, x
                        jmp     #zpu_mult16x16_ret
mdiv                    shr     y,#1            wc,wz   'divide
                        rcr     t1,#1
        if_nz           djnz    t2,#mdiv
mdiv2                   cmpsub  x,t1            wc
                        rcl     y,#1
                        shr     t1,#1
                        djnz    t2,#mdiv2
                        test    a,#%01000       wc
                        negc    x,x
                        test    a,#%00100       wc
                        test    a,#%00001       wz
        if_z            negc    x,y
                        mov     tos, x
zpu_mult16x16_ret
			ret

'------------------------------------------------------------------------------
' fill a cache line with translated instructions
' cur_pc points to the start of the cache line we need
'------------------------------------------------------------------------------
fill
			movd	ccopy, cur_cache_base
			mov	t2, cur_cache_tag
			add	t2, zpu_memory_addr
			mov	t1, #CACHE_LINE_SIZE
			
			'' initialize im_flag for correct state, based
			'' on the previous byte
			'' if we arrived here by falling through
			'' then the high bit of im_flag is set and we
			'' should keep it
			mov	memp, t2
			sub	memp, #1
			xor	memp, #%11		'XOR for endianness
			rdbyte	opcode, memp
			mov	im_flag, #first_im
			test	opcode, #$80 wz
    if_nz		mov	im_flag, #later_im

transi
			'' translate one instruction
                        mov     memp, t2
                        xor     memp, #%11               'XOR here is an endianess fix.
                        rdbyte  opcode, memp
                        mov     address, opcode
#ifdef SINGLE_STEP
                        call    #break_ss
#endif
			add	t2, #1
			' build the instruction into icache here
			mov	cnt, #2

                        test    opcode, #$80 wz          'Check for IM instruction
              if_nz     jmp     im_flag
	      		mov	im_flag, #first_im
                        cmp     opcode, #$60 wz, wc       'Check for LOADSP instruction
        if_z_or_nc      jmp     #zpu_loadsp

                        cmp     opcode, #$40 wz, wc       'Check for STORESPinstruction
        if_z_or_nc      jmp     #zpu_storesp

			mov	aux_opcode, opcode
			shl	aux_opcode, #2			' convert to byte address
			add	aux_opcode, dispatch_tab_addr
			rdlong	aux_opcode, aux_opcode
#ifdef SINGLE_STEP
			mov	dbg_data, aux_opcode
#endif
                        jmp     aux_opcode                    'Jump through dispatch table for other ops
nexti
			djnz	t1, #transi
fill_ret
			ret

'------------------------------------------------------------------------------
icache0
			long	0[2*CACHE_LINE_SIZE]

			mov	cur_pc, cur_cache_tag
			add	cur_pc, #CACHE_LINE_SIZE
			jmp	#set_pc

#ifdef TWO_LINE_CACHE
icache1
			long	0[2*CACHE_LINE_SIZE]

			mov	cur_pc, cur_cache_tag
			add	cur_pc, #CACHE_LINE_SIZE
			jmp	#set_pc
#endif

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
#ifdef SINGLE_STEP
break_ss
			mov	data, t2
			sub	data, zpu_memory_addr
			jmp	#break_intern
#endif

break
			call	#get_next_pc
			mov     data, cur_pc wz
	if_nz  		sub	data, #1
break_intern
                        wrlong  data, pc_addr             'Dump registers to HUB.
			mov     a, sp
			sub	a, zpu_memory_addr
                        wrlong  a, sp_addr
			wrlong	tos, tos_addr
			wrlong  dbg_data, debug_addr
                        mov     a, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  a, io_command_addr
:wait                   rdlong  a, io_command_addr wz
              if_nz     jmp     #:wait
break_ss_ret
break_ret               ret
'------------------------------------------------------------------------------

#ifdef IMPL_SYSCALL
'------------------------------------------------------------------------------
syscall
	                wrlong  pc, pc_addr             'Dump registers to HUB.
                        wrlong  sp, sp_addr
                        mov     t2, #io_cmd_syscall   'Set I/O command to SYSCALL
                        wrlong  t2, io_command_addr
:wait                   rdlong  t2, io_command_addr wz'Wait for command completion
              if_nz     jmp     #:wait
                        rdlong  sp, sp_addr
                        rdlong  pc, pc_addr
syscall_ret             ret
#endif
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' ZPU registers and working variables

'This is a nice mess, all Zogs variables are overlaid against this code.
init
address                 mov     address, par             'Pick up first 6 longs of PAR block
zpu_memory_addr         rdlong  zpu_memory_addr, address 'HUB address of the ZPU memory area
memp                    add     address, #4              'Temporary pointer into ZPU memory space
zpu_memory_sz           rdlong  zpu_memory_sz, address   'Size of ZPU memory area in bytes
temp                    add     address, #4          'For temp operands etc
cur_pc                  rdlong  cur_pc, address          'ZPU Program Counter
opcode                  add     address, #4          'Opcode being executed.
sp                      rdlong  sp, address          'ZPU Stack Pointer
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
a                       add     temp, #4
dm_addr                 mov     dm_addr, temp        'HUB address of decode mask
dbg_data                add     temp, #4
debug_addr              mov     debug_addr, temp     'HUB address of debug register

			mov	cur_pc, #0
			jmp	#set_pc

im_flag			long first_im    ' last instruction was im
aux_opcode		long 0	    ' implementation data for translating current opcode byte
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Big constants
minus_one               long $FFFFFFFF
word_mask               long $0000FFFF
uart_address            long $80000024
timer_address           long $80000100
io_mask			long $ff000000

zpu_hub_start           long $10000000  'Start of HUB access window in ZPU memory space
zpu_cog_start           long $10008000  'Start of COG access window in ZPU memory space
zpu_io_start            long $10008800  'Start of IO access window
'------------------------------------------------------------------------------
                        fit     $1F0  ' $198 works, $170 would be ideal, $1F0 is whole thing

			long
dispatch_table
' this is not a traditional jmp table, it's an indirect one
' the bottom 9 bits (the source field) encodes the destination for
' the jump, but the other bits can also encode information about
' the instruction as well
' the whole 32 bits is placed in the aux_opcode register
'
{00}                    abs     zpu_breakpoint, #zpu_literal2	' compile zpu_breakpoint
{01}                    add     zpu_illegal, #zpu_literal2	' compile zpu_illegal
{02}                    adds     zpu_pushsp,  #zpu_literal2	' compile zpu_pushsp
{03}                    shl     zpu_illegal, #zpu_literal2	' compile zpu_illegal
{04}                    mov     zpu_poppc, #zpu_literal2	' compile zpu_poppc
{05}                    add     0, #zpu_binaryop		' compile zpu_add
{06}                    and     0, #zpu_binaryop		' compile zpu_and
{07}                    or      0, #zpu_binaryop		' compile zpu_or
{08}                    mov     zpu_load, #zpu_literal2		' compile zpu_load
{09}                    mov	zpu_not,  #zpu_literal2		' compile zpu_not
{0A}                    mov     zpu_flip, #zpu_literal2		' compile zpu_flip
{0B}                    rol     zpu_nop,  #zpu_literal2		' compile zpu_nop
{0C}                    mov     zpu_store, #zpu_literal2	' compile zpu_store
{0C}                    mov     zpu_popsp, #zpu_literal2	' compile zpu_popsp
{0E}                    mov     zpu_illegal, #zpu_literal2	' compile zpu_illegal
{0F}                    mov     zpu_illegal, #zpu_literal2	' compile zpu_illegal

{10}                    mov     zpu_addsp0, #zpu_literal2	' compile zpu_addsp with 0 offset
{11}                    jmp     #zpu_addsp
{12}                    jmp     #zpu_addsp
{13}                    jmp     #zpu_addsp
{14}                    jmp     #zpu_addsp
{15}                    jmp     #zpu_addsp
{16}                    jmp     #zpu_addsp
{17}                    jmp     #zpu_addsp
{18}                    jmp     #zpu_addsp
{19}                    jmp     #zpu_addsp
{1A}                    jmp     #zpu_addsp
{1B}                    jmp     #zpu_addsp
{1C}                    jmp     #zpu_addsp
{1D}                    jmp     #zpu_addsp
{1E}                    jmp     #zpu_addsp
{1F}                    jmp     #zpu_addsp

{20}                    jmp     #zpu_emulate ' reset??
{21}                    jmp     #zpu_emulate ' interrupt??
{22}                    jmp     #zpu_emulate ' loadh
{23}                    jmp     #zpu_emulate ' storeh
{24}        if_b        mov     cmp_signed_impl, #zpu_cmp ' lessthan
{25}        if_be       mov     cmp_signed_impl, #zpu_cmp ' lessthanorequal
{26}        if_b        mov     cmp_unsigned_impl, #zpu_cmp ' ulessthan
{27}        if_be       mov     cmp_unsigned_impl, #zpu_cmp ' ulessthanorequal
{28}                    jmp     #zpu_emulate ' swap
{29}                    jmp     #zpu_emulate ' slowmult
{2A}                    shr     0, #zpu_binaryop ' lshiftright
{2B}                    shl     0, #zpu_binaryop ' ashiftleft
{2C}                    sar     0, #zpu_binaryop ' ashiftright
{2D}                    jmp     #zpu_emulate ' call
{2E}        if_z        mov     cmp_unsigned_impl, #zpu_cmp ' eq
{2F}        if_nz       mov     cmp_unsigned_impl, #zpu_cmp ' neq

{30}                    mov     zpu_neg, #zpu_literal2	' compile zpu_neg
{31}                    sub     0, #zpu_binaryop ' sub
{32}                    xor     0, #zpu_binaryop ' xor
{33}                    jmp     #zpu_emulate ' loadb
{34}                    jmp     #zpu_emulate ' storeb
{35}                    jmp     #zpu_emulate ' div
{36}                    jmp     #zpu_emulate ' mod
{37}        if_z        mov     cmpbranch, #zpu_dobranch ' eqbranch
{38}        if_nz       mov     cmpbranch, #zpu_dobranch ' neqbranch
{39}                    jmp     #zpu_emulate ' poppcrel
{3A}                    jmp     #zpu_emulate ' config
{3B}                    mov     zpu_pushpc, #zpu_literal2	' compile pushpc
'''{3C}                    mov     zpu_syscall, #zpu_literal2 	' compile syscall
{3C}                    mov     zpu_breakpoint, #zpu_literal2 	' compile syscall
{3D}                    mov     zpu_pushspadd, #zpu_literal2    ' compile pushspadd
{3E}                    mov     stub_mult16x16, #zpu_literal2	' compile mult16x16
{3F}                    mov     zpu_callrelpc, #zpu_literal2 	' compile callrelpc


'---------------------------------------------------------------------------------------------------------
'The End.

