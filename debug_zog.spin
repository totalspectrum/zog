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
' v1.1      17-08-2010 Fixed virtual memory operation.
'
' v1.2      17-08-2010 No changes here but all make files for ZPU executables now perform
'                      and endianness swap on the binaries as byte swapping is not done here anymore.
'
' v1.3      18-08-2010 No change but bump version inline with ZOG.
'
' v1.4      26-08-2010 Added display of "NOT IMPLEMENTED" ops in print_regs.
'
' v1.5      28-08-2010 Added support for Jazzed's SDRAM external RAM solution.
'                         ...define USE_JCACHED_MEMORY to use that.
'                      Adopted Jazzed's use of userdefs.spin for platform configuration.
'
' v1.6      31-08-2010 No change but bump version inline with ZOG.
'
' The "*.bin" file included in the ZPU memory area is created with
' GCC for the ZPU.
'
' Please see the Makefile included with Zog for how to build C programs
' with GCC for the ZPU architecture that Zog can run.
'
'#define SINGLE_STEP
'#define CYCLE_COUNT

'Select use of HUB memory or external memory here
'#define USE_JCACHED_MEMORY
'#define USE_VIRTUAL_MEMORY
#define USE_HUB_MEMORY

OBJ
'Set your platform configuration in userdef.spin
'Ideally, your userdefs.spin can live in the library path,
'and you can simply remove a new package's userdefs.spin
'to enable your own definitions.
    def : "userdefs.spin"

CON
#ifndef __P2__
    _clkmode        = def#_clkmode
    _xinfreq        = def#_xinfreq
#endif    
    ' user baud rate
    baud            = def#conBaud
    ' user microSD pins
    spiDO           = def#spiDO
    spiClk          = def#spiCLK
    spiDI           = def#spiDI
    spiCS           = def#spiCS

    ENDIAN_FIX      = %00 '' or %11 for big endian swapped
    
DAT                     org 0
zpu_memory              byte ' Force zpu_memory to be BYTE type.
#ifdef USE_HUB_MEMORY
zpu_image	file "program.bin"
'zpu_image               file "dhry.bin"
'zpu_image               file "fibo.bin"
'zpu_image               file "xxtea.bin"
'zpu_image               file "fftbench.bin"
'zpu_image		file "toggle.bin"
padding                 byte 0[(zpu_memory_size) - (@padding - @zpu_memory)]
zpu_memory_end
                        fit (zpu_memory_size / 4)
#endif
#ifdef USE_JCACHED_MEMORY
DAT
filename byte "fibo.bin",0
'filename byte "mall.bin",0
'filename byte "hello.bin",0
#endif
#ifdef USE_VIRTUAL_MEMORY
DAT
filename byte "fibo.bin",0
'filename byte "mall.bin",0
'filename byte "hello.bin",0
#endif

CON
#ifdef USE_JCACHED_MEMORY
zpu_memory_size = (1 << 25)                              'The size of ZPU memory area
#endif
#ifdef USE_VIRTUAL_MEMORY
zpu_memory_size = (64 * 1024)                            'The size of ZPU memory area
#endif
#ifdef USE_HUB_MEMORY
#ifdef __P2__
zpu_memory_size = (64 * 1024)                            'The size of ZPU memory area
#else
zpu_memory_size = (16 * 1024)                            'The size of ZPU memory area
#endif
#endif
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
'Propeller specific actions
SYS_cognew      = 8000
SYS_coginit     = 8001
SYS_cogstop     = 8002

'Standard file decriptors
STDIN_FILENO    = 0
STDOUT_FILENO   = 1
STDERR_FILENO   = 2

UART_TX_PORT  = $80000024 'ZPU virtual UART I/O ports
UART_RX_PORT  = $80000028

OBJ
#ifdef __P2__
#ifdef USE_JIT
  zog  : "jit_p2"
#else  
  zog  : "zog_p2"
#endif  
  ser  : "spin/SmartSerial"
#else
#ifdef USE_JIT
  zog  : "zog_jit"
#else
  zog  : "zog"
#endif  
  ser  : "FullDuplexSerialPlus"
#endif
#ifdef USE_JCACHED_MEMORY
  sd   : "fsrwFemto_rr001"                          'SD Software used in FemtoBASIC
  cm   : "SdramCache"                               'sdram cache interface
#endif
#ifdef USE_VIRTUAL_MEMORY
  sd   : "fsrwFemto_rr001"                          'SD Software used in FemtoBASIC
  vm   : "vmcog_v0_975.spin"                        'The MMU/virtual memory manager cog
#endif

VAR
  '8 consecutive longs for Zog's par block.
  long par_zpu_memory_addr
  long par_zpu_memory_sz
  long par_initial_pc
  long par_initial_sp
  long par_dispatch_tab_addr
  long par_pasm_addr
  long par_zog_mbox_addr
  long par_vm_mbox_addr

  '8 consecutive longs for Zog's I/O mailbox
  'Need not follow on from par block above
  long zog_mbox_command
  long zog_mbox_port
  long zog_mbox_data
  long zog_mbox_pc
  long zog_mbox_sp
  long zog_mbox_tos
  long zog_mbox_dm
  long zog_mbox_debug

#ifdef USE_JCACHED_MEMORY
  'Mail box for Cache
  'Need not follow on from zog_mbox above
  long vm_mbox[4]
#endif

#ifdef USE_VIRTUAL_MEMORY
  'Mail box for Virtual Memory
  'Need not follow on from zog_mbox above
  long vm_mbox[4]
#endif

  'Misc vars
  long cog
  long ioControl[2]               'SD parameters
  long framep
  word err                        'Error code return
  long step_count
  byte diskbuff[512]

PUB start : okay | n
#ifdef __P2__
  clkset(def#p2_clkmode, def#p2_freq)
#endif
  ser.start(def#conRxPin, def#conTxPin, def#conMode, def#conBaud) 'Start the debug Terminal
  waitcnt(cnt+40_000_000)                               'Give serial terminal time
  ser.str(string("ZOG v1.7"))

#ifdef USE_JCACHED_MEMORY
  cm.start(@vm_mbox)                                    'Start up cache
  ser.str(string(" (CACHE)"))
#endif

#ifdef USE_VIRTUAL_MEMORY
  vm.start(@vm_mbox, $7C00, 10)                          'Start up VMCOG
  ser.str(string(" (VM)"))
#endif

#ifdef USE_HUB_MEMORY
  ser.str(string(" (HUB)", 13))
  ser.str(string("zpu memory at "))
  ser.hex(@zpu_memory, 8)
#endif
  crlf
  waitcnt(cnt+20_000_000)                               'Give serial terminal window time

#ifdef USE_JCACHED_MEMORY
  ser.str (string("Starting SD driver...",))
  err := sd.startSD(@ioControl)                         'Start the SD routines
  ser.hex(err, 8)
  crlf

  ser.str (string("Mounting SD...",))
  err := \sd.mount_explicit(spiDO,spiClk,spiDI,spiCS)    'Mount the SD
  ser.hex(err, 8)
  crlf

  load_bytecode
  'check_bytecode
  'dump_virtual

  crlf
  ser.str(string("Running Program!"))
  crlf
#endif

#ifdef USE_VIRTUAL_MEMORY
  ser.str (string("Starting SD driver...",))
  err := sd.startSD(@ioControl)                         'Start the SD routines
  ser.hex(err, 8)
  crlf

  ser.str (string("Mounting SD...",))
  err := \sd.mount_explicit(spiDO,spiClk,spiDI,spiCS)    'Mount the SD
  ser.hex(err, 8)
  crlf

  load_bytecode
  'dump_virtual
  'prime_vm
#endif

  'Set up Zog's parameter block
  par_zpu_memory_addr := @zpu_memory
  par_zpu_memory_sz := zpu_memory_size
  par_initial_pc := 0
  par_initial_sp := zpu_memory_size - 8
  par_dispatch_tab_addr := zog.getdispatch_table
  par_pasm_addr := zog.getzog
  par_zog_mbox_addr := @zog_mbox_command
#ifdef USE_JCACHED_MEMORY
  par_vm_mbox_addr := @vm_mbox
#endif
#ifdef USE_VIRTUAL_MEMORY
  par_vm_mbox_addr := @vm_mbox
#endif

  cog := zog.start(@par_zpu_memory_addr)                    'Start ZOG with parameter block
  ser.str(string("Started cog "))
  ser.dec(cog)
  ser.str(string(13, 10))
  
  service_io                                                'Go any monitor ZPU I/O

#ifdef USE_JCACHED_MEMORY
PUB load_bytecode | bytes, i, ramaddr, mbyte, error, sbytes
  ser.str (string("Booting ",))
  ser.str (@filename)
  ser.str (string($d,$a))
  err := \sd.popen(@filename, "r")
  ser.hex(err, 8)
  crlf
{
  ser.str (string($d,$a,"Filling memory with garbage ",))
  repeat ramaddr from 0 to constant(1 << 18) step 4
      cm.writelong(ramaddr, bytes?)
    ifnot ramaddr & ($10000-1)
      ser.tx(".")
'}
  crlf
  ser.str (string("Reading image...",))

  ramaddr := 0
  bytes := 1
  sbytes~
  repeat while bytes > 0
    bytes := sd.pread(@diskbuff, 512)
    sbytes+= bytes
    err := sd.stopSDCard                                'stop SD card, but do not stop cog
    repeat i from 0 to bytes - 1
      cm.writebyte(ramaddr, diskbuff[i])                'To virtual memory.
      ramaddr++
    err := \sd.initSDCard(spiDO,spiClk,spiDI,spiCS)     'Init the SD

  ser.str(string(" "))
  ser.dec(sbytes)
  ser.str(string(" Bytes Loaded."))
  crlf
  ser.str (string("Done",))
  crlf

  ser.str (string($d,$a,"Clearing bss: ",))
  repeat i from 0 to constant(1 << 15) step 4
      cm.writelong(ramaddr++, 0)
    ifnot ramaddr & ($800-1)
      ser.tx(".")

  err := sd.stopSDCard                                  'stop SD card, but do not stop cog
  CheckError(err)

pub check_bytecode | bytes, i, ramaddr, mbyte, error, sbytes, secs

  err := sd.stop

  secs := 2
  crlf
  ser.str (string("Waiting "))
  ser.dec(secs)
  ser.str (string(" seconds before program check...",))
  waitcnt(clkfreq*secs+cnt)
  crlf
  crlf

  ser.str (string("Restarting SD driver...",))
  err := sd.startSD(@ioControl)                         'Start the SD routines
  ser.hex(err, 8)
  crlf

  ser.str (string("Remounting SD...",))
  err := \sd.mount_explicit(spiDO,spiClk,spiDI,spiCS)    'Mount the SD
  ser.hex(err, 8)
  crlf

  ser.str (string("Checking image...",))

  err := \sd.popen(@filename, "r")
  error := 0
  ramaddr := 0
  bytes := 1
  sbytes~
  repeat while bytes > 0
    bytes := sd.pread(@diskbuff, 512)
    sbytes+= bytes
    err := sd.stopSDCard                                'stop SD card, but do not stop cog
    repeat i from 0 to bytes - 1
      mbyte := cm.readbyte(ramaddr)
      if mbyte <> diskbuff[i]                           'Check virtual memory.
        ser.str(string($d,$a,"Program load error: "))
        ser.hex(ramaddr,8)
        ser.str(string(" expected : "))
        ser.hex(diskbuff[i],2)
        ser.str(string(" received : "))
        ser.hex(mbyte,2)
        ser.str(string(" ("))
        ser.dec(ramaddr)
        ser.str(string(")"))
        error++
      ramaddr++
    err := \sd.initSDCard(spiDO,spiClk,spiDI,spiCS)     'Init the SD

  ser.str(string(" "))
  ser.dec(sbytes)
  ser.str(string(" Bytes Checked."))
  if error
    ser.str (string($d,$a,"Program Load Failed."))
  else
    ser.str (string($d,$a,"Program Load OK."))
  crlf

  err := sd.stopSDCard                                  'stop SD card, but do not stop cog
  CheckError(err)
{
PUB prime_vm | vbyte, count, ramaddr
  ser.str (string("Priming VM...", 13))
  repeat count from 1 to 2
    repeat ramaddr from $FC00 to $FFFF
      vbyte := cm.readbyte(ramaddr ^ ENDIAN_FIX)
  repeat count from 1 to 1000
    repeat ramaddr from $05b9 to $05d5
      vbyte := cm.readbyte(ramaddr ^ ENDIAN_FIX)
  ser.str (string("Done", 13))
}
PRI dump_virtual | ad, b, i, e
  ser.str(string("Reading virtual memory...",13))
  i := 0
  e := 0
  repeat ad from 0 to 255  'zpu_memory_size - 1 step 1
    if (i & 15) == 0
      ser.hex(ad,4)
      ser.tx(":")
      ser.tx(" ")
    b:=cm.readbyte(ad ^ ENDIAN_FIX)                            'XOR here is an endiannes fix
    ser.hex(b,2)
    ser.tx($20)
    i := i + 1
    if i == 16
      ser.tx(13)
      ser.tx(10)
      i := 0
#endif

#ifdef USE_VIRTUAL_MEMORY
PUB prime_vm | vbyte, count, ramaddr
  ser.str (string("Priming VM...", 13))
  repeat count from 1 to 2
    repeat ramaddr from $FC00 to $FFFF
      vbyte := vm.rdvbyte(ramaddr ^ ENDIAN_FIX)
  repeat count from 1 to 1000
    repeat ramaddr from $05b9 to $05d5
      vbyte := vm.rdvbyte(ramaddr ^ ENDIAN_FIX)
  ser.str (string("Done", 13))
#endif

#ifdef USE_VIRTUAL_MEMORY
PUB load_bytecode | bytes, i, ramaddr
  ser.str (string("Opening ZPU image...",))
  err := \sd.popen(@filename, "r")
  ser.hex(err, 8)
  crlf

  ser.str (string("Reading image...",))

  bytes := 1
  repeat while bytes > 0
    bytes := sd.pread(@diskbuff, 512)
    err := sd.stopSDCard                                'Stop SD card, but do not stop cog
    repeat i from 0 to bytes - 1
      vm.wrvbyte(ramaddr, diskbuff[i])                  'To virtual memory.
      ramaddr++
    err := \sd.initSDCard(spiDO,spiClk,spiDI,spiCS)     'Init the SD
  ser.str (string("Done",))
  crlf

  err := sd.stopSDCard                                  'stop SD card, but do not stop cog
  CheckError(err)

PRI dump_virtual | ad, b, i, e
  ser.str(string("Reading virtual memory...",13))
  i := 0
  e := 0
  repeat ad from 0 to 255  'zpu_memory_size - 1 step 1
    if (i & 15) == 0
      ser.hex(ad,4)
      ser.tx(":")
      ser.tx(" ")
    b:=vm.rdvbyte(ad ^ ENDIAN_FIX)                             'XOR here is an endiannes fix
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
    case zog_mbox_command
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
  case zog_mbox_port
    UART_TX_PORT:
      zog_mbox_data := $100
    UART_RX_PORT:
      zog_mbox_data := ser.rx + $100
    other:
      ser.str(string("Read from unknown input port "))
      ser.hex(zog_mbox_port, 8)
      crlf
  zog_mbox_command := 0

PRI on_output
  case zog_mbox_port
    UART_TX_PORT:
      'ser.tx("[")
      if zog_mbox_data == 10
        ser.tx(13)
      ser.tx(zog_mbox_data)
      'ser.tx("]")
    other:
      ser.str(string("Write to unknown output port "))
      ser.hex(zog_mbox_port, 8)
      ser.str(string(" : "))
      ser.hex(zog_mbox_data, 8)
      crlf
  zog_mbox_command := 0

PRI on_break | cmd
'Handle CPU break condition
  if step_count == 0
    crlf
    ser.str(string("#pc,opcode,sp,top_of_stack,next_on_stack"))
    crlf
    ser.str(string("#----------"))
    crlf
    crlf
  print_regs

  if step_count > 1 and zog_mbox_pc <> step_count
    zog_mbox_command := 0
    return
    
  repeat
    cmd := ser.rx
    if cmd == " "
      step_count := 1
      quit
    if cmd == "g"
      step_count := $6c4
      quit
    ser.tx(string("Press space to step once, g to step 100 times", 13, 10))

  zog_mbox_command := 0

PRI printStackFrame | buff, i
'Labels as per C read()/write() parameters.
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
  framep := zog_mbox_sp + @zpu_memory
  'printStackFrame
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
  zog_mbox_command := 0

PRI _exit
  ser.str(string("_exit ?"))
  repeat

PRI _open
  ser.str(string("_open ?"))
  repeat

PRI _close
  ser.str(string("_close ?"))
  repeat


PRI _read | i, f, p
'Read nbytes of fd to buffer
'In C, result=_syscall(&t, SYS_read, fd, buf, nbytes);
  'FIXME why not just add an offset to sp?
  f := @zpu_memory[long[framep + 16]]                   'Get buff ptr from ZPU stack
  p := f
  case long[framep + 12]                                'Check file descriptor
    STDIN_FILENO, STDERR_FILENO:                        'Console input?
      repeat i from 0 to long[framep + 20] - 1          'Input nbytes
        byte[p ^ ENDIAN_FIX] := ser.rx                         'XOR here is an endianess fix.
        p++
    other:
      'FIXME: Other file and device input here
  'FIXME errno
  long[@zpu_memory[0]] := p - f                         'Return value via _mreg (aka R0) at ZPU address 0 !


PRI _write | i, p, f
'Write nbytes of buffer to fd
'In C, result=_syscall(&t, SYS_write, fd, buf, nbytes);
  'FIXME why not just add an offset to sp?
  f := @zpu_memory[long[framep + 16]]                   'Get buff ptr from ZPU stack
  p := f
  case long[framep + 12]                                'Check file descriptor
    STDOUT_FILENO,STDERR_FILENO:                        'Console output?
      repeat i from 0 to long[framep + 20] - 1          'Output nbytes
        ser.tx(byte[p ^ ENDIAN_FIX])                           'XOR here is an endianess fix.
        p++
    other:
      'FIXME: Other file and device output here
  'FIXME errno
  long[@zpu_memory[0]] := p - f                         'Return value via _mreg (aka R0) at ZPU address 0 !

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
  long[@zpu_memory[0]] := 0                             'Return value via _mreg (aka R0) at ZPU address 0 !

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

PRI print_regs | i, p, op, d
'Print the ZPU registers
#ifdef PRINT_HUB_PC
  ser.str(string("("))
  ser.hex(@zpu_memory[zog_mbox_pc^ENDIAN_FIX], 5)
  ser.str(string(") 0x"))
#else
  ser.str(string("0x"))
#endif
  ser.hex(zog_mbox_pc, 7)
  ser.tx($20)
  ser.str(string("0x"))
#ifdef USE_JCACHED_MEMORY
  op := cm.readbyte(zog_mbox_pc ^ ENDIAN_FIX)                  'XOR here is an endianess fix.
#endif
#ifdef USE_VIRTUAL_MEMORY
  op := vm.rdvbyte(zog_mbox_pc ^ ENDIAN_FIX)                   'XOR here is an endianess fix.
#endif
#ifdef USE_HUB_MEMORY
  op := zpu_memory[(zog_mbox_pc ^ ENDIAN_FIX)]                   'XOR here is an endianess fix.
#endif
  ser.hex(op, 2)
  ser.tx($20)
  ser.str(string("0x"))
  ser.hex(zog_mbox_sp, 8)
  ser.tx($20)
  ser.str(string("0x"))
  ser.hex(zog_mbox_tos, 8)
  ser.tx($20)
  '' pop increments sp and then reads from it
  d := zpu_memory[4+(zog_mbox_sp ^ ENDIAN_FIX)]  
  ser.str(string("0x"))
  ser.hex(d, 8)
  ser.tx($20)
  ser.hex(zog_mbox_debug, 8)
  crlf

  case op
    $00:
      ser.str(string("BREAKPOINT"))
      crlf
    $01,$03,$0E,$0F:
      ser.str(string("ILLEGAL OP"))
      crlf
    $20,$21:
      ser.str(string("NOT_IMPLEMENTED"))
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
    repeat                                             '<== LOOP HERE

'---------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------
'The End.

