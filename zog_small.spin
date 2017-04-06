'********************************************************************************
'*                                                                              *
'* ZOG_SMALL A ZPU Virtual machine running on the Parallax Propeller            *
'*           micro-controller.                                                  *
'*                                                                              *
'*           ZOG_SMALL is a compact version of ZOG that has it's dispatch       *
'*           table in COG RAM (instead of HUB). To make room for this a lot of  *
'*           op codes are handled by the ZPUs EMULATE instruction rather than   *
'*           in PASM.                                                           *
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
'* Author:   Michael Rychlik                                                    *
'*                                                                              *
'********************************************************************************
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
#define USE_VIRTUAL_MEMORY
'#define USE_HUB_MEMORY

CON

_clkmode        = xtal1 + pll16x
_xinfreq        = 6_553_600

baud            = 115_200                                 'UART baud rate

zpu_memory_size = (16 * 1024)                             'The size of ZPU memory area

' microSD pins on TriBladeProp Blade #2
spiDO           = 9
spiClk          = 28
spiDI           = 8
spiCS           = 14

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


OBJ
  ser : "FullDuplexSerialPlus"
'  sd   : "fsrwFemto_rr001"                          'SD Software used in FemtoBASIC
#ifdef USE_VIRTUAL_MEMORY
  vm   : "vmcog.spin"                               'The MMU/virtual memory manager cog
#endif


PUB start : okay | n
  stop                                                  'Stop ZPU if it's running already

  ser.start(31,30,0,baud)                              'Start the debug Terminal
  ser.str(string("ZOG_SMALL v0.14"))
  crlf

{
  ser.str (string("Starting SD driver..."))
  crlf
  err := sd.startSD(@ioControl)                         'start the SD routines
  CheckError(not err)
  err := sd.mount_explicit(spiDO,spiClk,spiDI,spiCS)    'mount the SD
  CheckError(err)
}
#ifdef USE_VIRTUAL_MEMORY
  vm.start(@mailbox, $7C00, 16)                          'Start up VMCOG
  ser.str(string(" (VM)"))
#endif
#ifdef USE_HUB_MEMORY
  ser.str(string(" (HUB)"))
#endif
  crlf
  waitcnt(cnt+80_000_000)                               'Give serial terminal window time


  zpu_memory_addr := @zpu_memory                        'Set pointer in COG to ZPU memory area

  pc_addr := @pc_hub
  sp_addr := @sp_hub
  tos_addr := @tos_hub
  dm_addr := @decode_mask_hub
  io_command_addr:= @io_command
  io_port_addr := @io_port
  io_data_addr := @io_data

  debug_addr := @debug

  debug := 0

#ifdef USE_VIRTUAL_MEMORY
  mboxcmd := @mailbox
  mboxdat := @mailbox + 8
#endif

  load_bytecode

'  dump_virtual

  okay := cog := cognew(@enter, 0) + 1                  'Start emulator in a new COG

  service_io                                            'Go any monitor ZPU I/O

PUB stop
'' Stop CPU simulator - frees a cog
  if cog
    cogstop(cog~ - 1)

PRI load_bytecode | ad, b, c
#ifdef USE_VIRTUAL_MEMORY
  ser.str(string("Loading ZPU image..."))
  repeat ad from 0 to zpu_memory_size                     'For all BYTES in memory
    b := zpu_image[ad]                                    'Move bytes from "file"...
    vm.wrvbyte(ad ^ %11, b)                               'To virtual memory. XOR here is an endianess fix.
  ser.str(string("OK",13))
#endif
#ifdef USE_HUB_MEMORY
  repeat ad from 0 to zpu_memory_size                    'For all BYTES in memory
    b := zpu_memory[ad + 4]                              'Move bytes from "file" to correct memory location
    zpu_memory[ad ^ %11] := b                            'XOR here is an endianess fix.
#endif

#ifdef USE_VIRTUAL_MEMORY
PRI dump_virtual | ad, b, i, e
  ser.str(string("Reading virtual memory...",13))
  i := 0
  e := 0
  repeat ad from 0 to zpu_memory_size - 1 step 1
    if (i & 15) == 0
      ser.hex(ad,4)
      ser.tx(":")
      ser.tx(" ")
    b:=vm.rdvbyte(ad)
    ser.hex(b,2)
    ser.tx($20)
    i := i + 1
    if i == 16
      ser.tx(13)
      ser.tx(10)
      i := 0
#endif


PRI service_io
'ZPU I/O simulation service loop
  repeat
    case io_command
      io_cmd_break:                                     'CPU break
        on_break
      io_cmd_in:                                        'An input operation
        on_input
      io_cmd_out:                                       'An output operation
        on_output
      io_cmd_syscall:
        on_syscall
#ifdef CYCLE_COUNT
    ser.str(string("Cycles = "))
    ser.dec(debug)
    crlf
#endif

PRI on_input
' The ZPU has no I/O instructions
' Perhaps we will define some
  io_data := ser.rx
  io_command := 0

PRI on_output
' The ZPU has no I/O instructions
' Perhaps we will define some
  ser.tx(io_data)
  io_command := 0

PRI on_break
'Handle CPU break condition
  if step_count == 0
    crlf
    ser.str(string("#pc,opcode,sp,top_of_stack,next_on_stack"))
    crlf
    ser.str(string("#----------"))
    crlf
    crlf
  print_regs
  repeat while ser.rx <> $20                           'wait for 'space'
  io_command := 0
  if step_count++ == 30000
    ser.str(string("Done."))
    crlf
    repeat

PRI printStackFrame
'Labels as per read()/write() parameters.
  ser.str(string("Return address="))
  ser.hex(long[framep + 0], 8)
  crlf
  ser.str(string("errno address="))
  ser.hex(long[framep + 4], 8)
  crlf
  ser.str(string("syscall ID="))
  ser.hex(long[framep + 8], 8)
  crlf
  ser.str(string("fd="))
  ser.hex(long[framep + 12], 8)
  crlf
  ser.str(string("buff="))
  ser.hex(long[framep + 16], 8)
  crlf
  ser.str(string("length="))
  ser.hex(long[framep + 20], 8)
  crlf



PRI on_syscall | p
'Handle CPU SYSCALL instruction
'  Example C call:
'  result=_syscall(&t, SYS_write, fd, buf, nbytes);
  'Set a frame pointer into the ZPU stack (but HUB realtive)
  framep := @zpu_memory[(sp_hub {& zpu_memory_mask})]

'  printStackFrame

  'Perform system call function given by the ID param
  case long[framep + 8]
    SYS_exit:
      _exit
    SYS_open:
      _open
    SYS_close:
      _close
    SYS_read:
      _read
    SYS_write:
      _write
    SYS_lseek:
      _lseek
    SYS_unlink:
      _unlink
    SYS_getpid:
      _getpid
    SYS_kill:
      _kill
    SYS_fstat:
      _fstat
    SYS_argvlen:
      _argvlen
    SYS_argv:
      _argv
    SYS_chdir:
      _chdir
    SYS_stat:
      _stat
    SYS_chmod:
      _chmod
    SYS_utime:
      _utime
    SYS_time:
      _time
  io_command := 0

PRI _exit
  ser.str(string("_exit ?"))
  repeat

PRI _open
  ser.str(string("_open ?"))
  repeat

PRI _close
  ser.str(string("_close ?"))
  repeat


PRI _read | i, s, p
'Read nbytes of fd to buffer
'In C: result=_syscall(&t, SYS_read, fd, buf, nbytes);
  s := @zpu_memory[long[framep + 16]]                       'Get buff ptr from ZPU stack
  p := s
  case long[framep + 12]                                    'Check file descriptor
    STDIN_FILENO, STDERR_FILENO:                            'Console input?
      repeat i from 0 to long[framep + 20] - 1              'Input nbytes
        byte[p] := ser.rx
        p++
    other:
      'FIXME: Other file and device input here
  'FIXME errno
  long[@zpu_memory[0]) := p - s                             'Return value via _mreg (aka R0) at ZPU address 0 !


PRI _write | i, p, s
'Write nbytes of buffer to fd
'In C: result=_syscall(&t, SYS_write, fd, buf, nbytes);
  s := @zpu_memory[long[framep + 16]]                       'Get buff ptr from ZPU stack
  p := s
  case long[framep + 12]                                    'Check file descriptor
    STDOUT_FILENO,STDERR_FILENO:                            'Console output?
      repeat i from 0 to long[framep + 20] - 1              'Output nbytes
        ser.tx(byte[p ^ %11])                              'XOR here is an endianess fix.
        p++
    other:
      'FIXME: Other file and device output here
  'FIXME errno
  long[@zpu_memory[0]) := p - s                             'Return value via _mreg (aka R0) at ZPU address 0 !


PRI _lseek
  ser.str(string("_lseek ?"))
  repeat

PRI _unlink
  ser.str(string("_unlink ?"))
  repeat

PRI _getpid
  ser.str(string("_getpid ?"))
  repeat

PRI _kill
  ser.str(string("_kill ?"))
  repeat

PRI _fstat
  long[@zpu_memory[0]) := 0                                 'Return value via _mreg (aka R0) at ZPU address 0 !

PRI _argvlen
  ser.str(string("_argvlen ?"))
  repeat

PRI _argv
  ser.str(string("_argv ?"))
  repeat

PRI _chdir
  ser.str(string("_chdir ?"))
  repeat

PRI _stat
  ser.str(string("_stat ?"))
  repeat

PRI _chmod
  ser.str(string("_chmod ?"))
  repeat

PRI _utime
  ser.str(string("_utime ?"))
  repeat

PRI _time
  ser.str(string("_time ?"))
  repeat

PRI print_regs | i, p, op
'Print the ZPU registers
  ser.str(string("0X"))
  ser.hex(pc_hub, 7)
  ser.tx($20)
  ser.str(string("0X"))
#ifdef USE_VIRTUAL_MEMORY
  op := vm.rdvbyte(pc_hub ^ %11)                       'XOR here is ans endianess fix.
#endif
#ifdef USE_HUB_MEMORY
  op := zpu_memory[pc_hub ^ %11]                       'XOR here is ans endianess fix.
#endif
  ser.hex(op, 2)
  ser.tx($20)
  ser.str(string("0X"))
  ser.hex(sp_hub, 8)
  ser.tx($20)
  ser.str(string("0X"))
  ser.hex(tos_hub, 8)
  crlf

  case op
    $00:
      ser.str(string("BREAKPOINT"))
      crlf
      repeat
    $01,$03,$0E,$0F:
      ser.str(string("ILLEGAL OP"))
      crlf

'  p := 0 - 8*4
'  repeat i from 0 to 7
'    ser.hex(p + 4*i, 8)
'    ser.tx($20)
'    ser.hex(LONG[@zpu_memory[(p {& zpu_memory_mask})+ 4*i]], 8)
'    crlf
'  crlf


PRI crlf
  ser.tx(13)
  ser.tx(10)

PRI CheckError(r)
  if r < 0
    ser.str(string("Error: "))
    ser.hex(r,2)
    ser.str(string(" HALTED."))
    crlf
'    sd.stop
    repeat                                              '<== LOOP HERE


VAR
  long cog
  long pc_hub
  long sp_hub
  long tos_hub
  long decode_mask_hub
  long ioControl[2]               'SD parameters
  long framep
  word err                        'Error code return

#ifdef USE_VIRTUAL_MEMORY
  long mailbox[4]                 'Mail  box for Virtual Memory
#endif

  long step_count

DAT
'IO interface area. Do not change the order of these.
io_cmd_block  long
io_command    long      0
io_port       long      0
io_data       long      0

debug         long      0


DAT
                        org     0
enter
'dispatch_table
{00}                    jmp     #fix_table '#zpu_breakpoint
{01}                    jmp     #zpu_illegal
{02}                    jmp     #zpu_pushsp
{03}                    jmp     #zpu_illegal
{04}                    jmp     #zpu_poppc
{05}                    jmp     #zpu_add
{06}                    jmp     #zpu_and
{07}                    jmp     #zpu_or
{08}                    jmp     #zpu_load
{09}                    jmp     #zpu_not
{0A}                    jmp     #zpu_flip
{0B}                    jmp     #zpu_nop
{0C}                    jmp     #zpu_store
{0D}                    jmp     #zpu_popsp
{0E}                    jmp     #zpu_illegal
{0F}                    jmp     #zpu_illegal

{10}                    jmp     #zpu_addsp
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

{20}                    jmp     #zpu_emulate
{21}                    jmp     #zpu_emulate
{22}                    jmp     #zpu_emulate
{23}                    jmp     #zpu_emulate
{24}                    jmp     #zpu_emulate
{25}                    jmp     #zpu_emulate
{26}                    jmp     #zpu_emulate
{27}                    jmp     #zpu_emulate
{28}                    jmp     #zpu_swap
{29}                    jmp     #zpu_emulate
{2A}                    jmp     #zpu_emulate
{2B}                    jmp     #zpu_emulate
{2C}                    jmp     #zpu_emulate
{2D}                    jmp     #zpu_emulate
{2E}                    jmp     #zpu_emulate
{2F}                    jmp     #zpu_emulate

{30}                    jmp     #zpu_emulate
{31}                    jmp     #zpu_emulate
{32}                    jmp     #zpu_emulate
{33}                    jmp     #zpu_emulate
{34}                    jmp     #zpu_emulate
{35}                    jmp     #zpu_emulate
{36}                    jmp     #zpu_emulate
{37}                    jmp     #zpu_emulate
{38}                    jmp     #zpu_emulate
{39}                    jmp     #zpu_emulate
{3A}                    jmp     #zpu_config
{3B}                    jmp     #zpu_pushpc
{3C}                    jmp     #zpu_syscall
{3D}                    jmp     #zpu_emulate
{3E}                    jmp     #zpu_mult16x16
{3F}                    jmp     #zpu_emulate

fix_table               mov     0, first_entry
                        jmp     #execute
first_entry             jmp    #zpu_breakpoint
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'Opcode handlers.
zpu_im                  cmp     decode_mask, #0 wz
                        mov     decode_mask, #1
              if_z      jmp     #:first
                        shl     tos, #7
                        and     address, #$7F
                        or      tos, address
                        jmp     #done_and_inc_pc

:first                  call    #push_tos
                        mov     tos, address
                        shl     tos, #(32 - 7)
                        sar     tos, #(32 - 7)
                        jmp     #done_and_inc_pc

zpu_breakpoint          mov     decode_mask, #0
                        call    #break
                        jmp     #done_and_inc_pc

zpu_addsp               mov     decode_mask, #0
                        and     address, #$0F wz
              if_z      jmp     #no_offset             'Handle case where offset = 0, this DOES happen.
                        shl     address, #2
                        add     address, sp
                        call    #read_long
                        add     tos, data
                        jmp     #done_and_inc_pc
no_offset               add     tos, tos
                        jmp     #done_and_inc_pc

zpu_loadsp              mov     decode_mask, #0
                        and     address, #$1F
                        xor     address, #$10           'Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        call    #push_tos
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_storesp             mov     decode_mask, #0
                        and     address, #$1F
                        xor     address, #$10           'Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        mov     data, tos
                        call    #write_long
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_config              mov     decode_mask, #0
                        mov     cpu, tos
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_pushpc              mov     decode_mask, #0
                        call    #push_tos
                        mov     tos, pc
                        jmp     #done_and_inc_pc

zpu_or                  mov     decode_mask, #0
                        call    #pop
                        mov     nos, data
                        or      tos, nos
                        jmp     #done_and_inc_pc

zpu_not                 mov     decode_mask, #0
                        xor     tos, minus_one
                        jmp     #done_and_inc_pc

zpu_load                mov     decode_mask, #0
                        mov     address, tos
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_store               mov     decode_mask, #0
                        call    #pop
                        mov     nos, data
                        mov     address, tos
                        call    #write_long
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_poppc               mov     decode_mask, #0
                        mov     pc, tos
                        call    #pop
                        mov     tos, data
                        jmp     #done

zpu_flip                mov     decode_mask, #0
                        rev     tos, #32
                        jmp     #done_and_inc_pc

zpu_add                 mov     decode_mask, #0
                        call    #pop
                        mov     nos, data
                        add     tos, nos
                        jmp     #done_and_inc_pc

zpu_pushsp              mov     decode_mask, #0
                        call    #push_tos
                        mov     tos, sp
                        add     tos, #4
                        jmp     #done_and_inc_pc

zpu_popsp               mov     decode_mask, #0
                        mov     sp, tos
                        mov     address, sp
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_nop                 mov     decode_mask, #0
                        jmp     #done_and_inc_pc

zpu_and                 mov     decode_mask, #0
                        call    #pop
                        mov     nos, data
                        and     tos, nos
                        jmp     #done_and_inc_pc

zpu_swap                mov     decode_mask, #0
                        mov     data, tos
                        shr     data, #16
                        shl     tos, #16
                        or      data, tos
                        jmp     #done_and_inc_pc

zpu_mult16x16           mov     decode_mask, #0
                        call    #pop
                        mov     x, data
                        and     x, word_mask
                        mov     y, tos
                        and     y, word_mask
                        mov     a, #SPIN_MUL_OP
                        jmp     #math_F4

zpu_syscall             mov     decode_mask, #0
                        call    #syscall
                        jmp     #done_and_inc_pc

zpu_emulate             call    #push_tos
                        mov     tos, pc                 'Push return address
                        add     tos, #1
                        mov     pc, address                'Op code to PC
                        sub     pc, #32                 'Vector to emulate code
                        shl     pc, #5
                        jmp     #done

zpu_illegal             mov     decode_mask, #0
div_zero_error          call    #break
                        jmp     #done
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'ZPU memory space access routines

'Push a LONG onto the stack from "tos"
push_tos

#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong    temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp       #:waitmbox              'instructions are not necc
                        mov       addr, sp
                        wrlong    tos, mboxdat
                        shl       addr, #9
                        movs      addr, #WRITEVML
                        wrlong    addr, mboxcmd

:waitdone               rdlong    vdata, mboxcmd wz
                  if_nz jmp       #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, sp
'                        and     memp, zpu_memory_mask
                        add     memp, zpu_memory_addr
                        wrlong  tos, memp
#endif
                        sub     sp, #4
push_tos_ret            ret

'Pop a LONG from the stack into "data", set Z according to data.
pop                     add     sp, #4
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, sp
                        shl     addr, #9
                        movs    addr, #READVML
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdlong  data, mboxdat wz
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, sp
'                        and     memp, zpu_memory_mask
                        add     memp, zpu_memory_addr
                        rdlong  data, memp wz           'Must set Z for caller
#endif
pop_ret                 ret

'Read a LONG from ZPU memory at "address" into "data"
read_long               cmp     address, uart_address wz
              if_z      jmp     #in_long
                        cmp     address, timer_address wz
              if_z      mov     data, cnt
              if_z      jmp     #read_long_ret
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, address
                        shl     addr, #9
                        movs    addr, #READVML
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdlong  data, mboxdat
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, address
'                        and     memp, zpu_memory_mask
                        add     memp, zpu_memory_addr
                        rdlong  data, memp
#endif
read_long_ret           ret

'Write a LONG from "data" to ZPU memory at "address"
write_long              cmp     address, uart_address nr,wz
              if_z      jmp     #out_long
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        wrlong  data, mboxdat
                        mov     addr, address
                        shl     addr, #9
                        movs    addr, #WRITEVML
                        wrlong  addr, mboxcmd

:waitdone               rdlong  vdata, mboxcmd wz
                  if_nz jmp     #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, address
'                        and     memp, zpu_memory_mask
                        add     memp, zpu_memory_addr
                        wrlong  data, memp
#endif
write_long_ret          ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
in_long                 mov     data, #$100
                        jmp     #read_long_ret

out_long                wrlong  data, io_data_addr
                        mov     temp, #io_cmd_out     'Set I/O command to OUT
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #:wait
                        jmp     #write_long_ret
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
                        jmp     #done_and_inc_pc
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
                        jmp     #done_and_inc_pc

a                       long    0
x                       long    0
y                       long    0
t1                      long    0
t2                      long    0
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Main ZPU fetch and execute loop
done_and_inc_pc         add     pc, #1
done
execute
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, pc
                        xor     addr, #%11              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #READVMB
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdbyte  data, mboxdat
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, pc
'                        and     memp, zpu_memory_mask
                        xor     memp, #%11               'XOR here is an endianess fix.
                        add     memp, zpu_memory_addr
                        rdbyte  data, memp
#endif
                        mov     address, data
#ifdef SINGLE_STEP
                        call    #break
#endif
                        test     data, #$80 wz          'Check for IM instruction
              if_nz     jmp     #zpu_im

                        cmp     data, #$60 wz, wc       'Check for LOADSP instruction
        if_z_or_nc      jmp     #zpu_loadsp

                        cmp     data, #$40 wz, wc       'Check for STORESPinstruction
        if_z_or_nc      jmp     #zpu_storesp

                        jmp     data                    'Jump through dispatch table for other ops
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
break                   wrlong  pc, pc_addr             'Dump registers to HUB.
                        wrlong  sp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  debug, debug_addr
                        wrlong  decode_mask, dm_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #:wait
break_ret               ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
syscall                 wrlong  pc, pc_addr             'Dump registers to HUB.
                        wrlong  sp, sp_addr
                        wrlong  decode_mask, dm_addr
                        mov     temp, #io_cmd_syscall   'Set I/O command to SYSCALL
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz'Wait for command completion
              if_nz     jmp     #:wait
                        rdlong  decode_mask, dm_addr    'Retrive registers from HUB.
                        rdlong  sp, sp_addr
                        rdlong  pc, pc_addr
syscall_ret             ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' ZPU registers and working variables
pc                      long 0      'ZPU Program Counter
sp                      long zpu_memory_size - 8  'ZPU Stack Pointer
tos                     long 0      'Top Of Stack
nos                     long 0      'Next on Stack
decode_mask             long 0      'Flag for the IM instruction
data                    long 0      'Data parameter for read, write etc
address                 long 0      'Address parameter for read write etc
memp                    long 0      'Temporary pointer into ZPU memory space
zpu_ram_base            long 0      'HUB address of ZPU RAM space
temp                    long 0      'For temp operands etc
cpu                     long 0      'The CPU type given by the CONGIG op.
op_counter              long 0      'Counts number of executed ops.

zpu_memory_addr         long 0      'HUB address of the ZPU memory area
pc_addr                 long 0      'HUB address of PC
sp_addr                 long 0      'HUB address of SP
tos_addr                long 0      'HUB address of tos
dm_addr                 long 0      'HUB address of decode mask
io_command_addr         long 0      'HUB address of I/O command byte.
io_port_addr            long 0      'HUB address of I/Ö port number
io_data_addr            long 0      'HUB address of I/O data
debug_addr              long 0      'HUB address of debug register
'------------------------------------------------------------------------------

#ifdef USE_VIRTUAL_MEMORY
'--------------------------------------------------------------------------------------------------
' Variables used to support VM readbyte/writebyte
'--------------------------------------------------------------------------------------------------
addr                    long 0      'Address we want to read, can be PC or whatever
vdata                    long 0      'Data read/written
mboxcmd                 long 0-0    'Pointer to first long of VMCOG mailbox (+0 offset)
mboxdat                 long 0-0    'Pointer to second long of VMCOG mailbox (+4 offset)
'--------------------------------------------------------------------------------------------------
#endif

'------------------------------------------------------------------------------
' Big constants
minus_one               long $FFFFFFFF
minus_two               long $FFFFFFFE
word_mask               long $0000FFFF
zpu_memory_mask         long zpu_memory_size - 1
uart_address            long $80000024
'timer_address           long $080a0014
timer_address           long $80000100
'------------------------------------------------------------------------------
                        fit     $1F0
'---------------------------------------------------------------------------------------------------------
DAT                     org 0
zpu_memory byte         'Force zpu_memory to have type BYTE
                        'Four free bytes, code is "loaded" from file to here fixing endianness
                        byte 0,0,0,0
zpu_image               file "test.bin"
padding                 byte 0[(zpu_memory_size) - (@padding - @zpu_memory)]
zpu_memory_end
                        fit (zpu_memory_size / 4)
'---------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------
'The End.

