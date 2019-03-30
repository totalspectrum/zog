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
#define SINGLE_STEP
'#define CYCLE_COUNT
'#define USE_JCACHED_MEMORY
'#define USE_VIRTUAL_MEMORY
#define USE_HUB_MEMORY

' comment these out to save a bit of space
#define SPEED_ADD_LOAD_STORE_SP
#define USE_FASTER_MULT

CON
' These are the SPIN byte codes for mul and div
'SPIN_MUL_OP     = $F4  '(multiply, return lower 32 bits)
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

#ifdef USE_JCACHED_MEMORY
OBJ cache : "SdramCache"             
#endif

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
'Opcode handlers. This section MUST fit in 256 LONGS.

zpu_breakpoint          call    #break
                        jmp     #done_and_inc_pc

zpu_addsp_0             add     tos, tos                'Special case for offset = 0
                        jmp     #done_and_inc_pc

zpu_addsp               and     address, #$0F
                        shl     address, #2
                        add     address, sp
                        call    #read_long
                        add     tos, data
                        jmp     #done_and_inc_pc

zpu_loadsp_tos          call    #push_tos
                        jmp     #done_and_inc_pc

zpu_loadsp_hi           ' this will fall through if we're saving space
#ifdef SPEED_ADD_LOAD_STORE_SP
                        and     address, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        call    #push_tos
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc
#endif
zpu_loadsp              and     address, #$1F
                        xor     address, #$10           'Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        call    #push_tos
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_storesp_tos         call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_storesp_hi          ' this will fall through if we're saving space
#ifdef SPEED_ADD_LOAD_STORE_SP
                        and     address, #$0F           'bit 4 was 1...Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        mov     data, tos
                        call    #write_long
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc
#endif
zpu_storesp             and     address, #$1F
                        xor     address, #$10           'Trust me, you need this.
                        shl     address, #2
                        add     address, sp
                        mov     data, tos
                        call    #write_long
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_config              mov     cpu, tos
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_pushpc              call    #push_tos
                        mov     tos, pc
                        jmp     #done_and_inc_pc

zpu_or                  call    #pop
                        or      tos, data
                        jmp     #done_and_inc_pc

zpu_not                 xor     tos, minus_one
                        jmp     #done_and_inc_pc

zpu_load                mov     address, tos
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_pushspadd           shl     tos, #2
                        add     tos, sp
                        jmp     #done_and_inc_pc

zpu_store               call    #pop
                        mov     address, tos
                        call    #write_long
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_poppc               mov     pc, tos
                        call    #pop
                        mov     tos, data
                        jmp     #done

zpu_poppcrel            add     pc, tos
                        call    #pop
                        mov     tos, data
                        jmp     #done

zpu_flip                rev     tos, #32
                        jmp     #done_and_inc_pc

zpu_add                 call    #pop
                        add     tos, data
                        jmp     #done_and_inc_pc

zpu_sub                 call    #pop
                        sub     data, tos
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_pushsp              call    #push_tos
                        mov     tos, sp
                        add     tos, #4
                        jmp     #done_and_inc_pc

zpu_popsp               mov     sp, tos
                        mov     address, sp
                        call    #read_long
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_nop                 jmp     #done_and_inc_pc

zpu_and                 call    #pop
                        and     tos, data
                        jmp     #done_and_inc_pc

zpu_xor                 call    #pop
                        xor     tos, data
                        jmp     #done_and_inc_pc

zpu_loadb

#ifdef USE_JCACHED_MEMORY
                        mov     addr, tos
''                        xor     addr, #%11              'XOR here is an endianess fix.
                        call    #zpu_cache              'z flag is set if address is in current buffer
            if_ne       call    #cache_read             'If address not in current buffer, read new buffer
                        rdbyte  tos, memp               'zpu_cache / cache_read leaves address in memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, tos
''                        xor     addr, #%11              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #READVMB
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdbyte  tos, mboxdat
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, tos
''                        xor     memp, #%11              'XOR here is an endianess fix.
                        add     memp, zpu_memory_addr
                        rdbyte  tos, memp
#endif
                        jmp     #done_and_inc_pc

zpu_storeb              call    #pop

#ifdef USE_JCACHED_MEMORY
                        mov     addr, tos
''                        xor     addr, #%11              'XOR here is an endianess fix.
                        call    #zpu_cache
            if_ne       call    #cache_write
                        wrbyte  data, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        wrbyte  data, mboxdat
                        mov     addr, tos
''                        xor     addr, #%11              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #WRITEVMB
                        wrlong  addr, mboxcmd

:waitdone               rdlong  data, mboxcmd wz
                  if_nz jmp     #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, tos
''                        xor     memp, #%11              'XOR here is an endianess fix.
                        add     memp, zpu_memory_addr
                        wrbyte  data, memp
#endif
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_loadh               mov     address, tos
                        call    #read_word
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_storeh              call    #pop
                        mov     address, tos
                        call    #write_word
                        call    #pop
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_lessthan            call    #pop
                        cmps    tos, data wz,wc
                        mov     tos, #0
              if_b      mov     tos, #1
                        jmp     #done_and_inc_pc

zpu_lessthanorequal     call    #pop
                        cmps    tos, data wz,wc
                        mov     tos, #0
              if_be     mov     tos, #1
                        jmp     #done_and_inc_pc

zpu_ulessthan           call    #pop
                        cmp     tos, data wz, wc
                        mov     tos, #0
              if_b      mov     tos, #1
                        jmp     #done_and_inc_pc

zpu_ulessthanorequal    call    #pop
                        cmp     tos, data wz, wc
                        mov     tos, #0
              if_be     mov     tos, #1
                        jmp     #done_and_inc_pc

zpu_swap                ror     tos, #16
                        jmp     #done_and_inc_pc

zpu_mult16x16           call    #pop
                        and     data, word_mask
                        and     tos, word_mask
                        jmp     #fast_mul

zpu_eqbranch            call    #pop
              if_z      add     pc, tos
              if_nz     add     pc, #1
                        call    #pop
                        mov     tos, data
                        jmp     #done

zpu_neqbranch           call    #pop
              if_nz     add     pc, tos
              if_z      add     pc, #1
                        call    #pop
                        mov     tos, data
                        jmp     #done

zpu_mult                call    #pop
                        jmp     #fast_mul

zpu_div                 call    #pop            ' pop sets z for me
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_DIV_OP
                        jmp     #fast_div

zpu_mod                 call    #pop            ' pop sets z for me
              if_z      jmp     #div_zero_error
                        mov     div_flags, #SPIN_REM_OP
                        jmp     #fast_div

zpu_lshiftright         call    #pop
                        shr     data, tos
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_ashiftleft          call    #pop
                        shl     data, tos
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_ashiftright         call    #pop
                        sar     data, tos
                        mov     tos, data
                        jmp     #done_and_inc_pc

zpu_call                mov     temp, tos
                        mov     tos, pc
                        add     tos, #1
                        mov     pc, temp
                        jmp     #done

zpu_callpcrel           mov     temp, tos
                        mov     tos, pc
                        add     tos, #1
                        add     pc, temp
                        jmp     #done

zpu_eq                  call    #pop
                        cmp     tos, data wz
              if_z      mov     tos, #1
              if_nz     mov     tos, #0
                        jmp     #done_and_inc_pc

zpu_neq                 call    #pop
                        sub     tos, data wz
              if_nz     mov     tos, #1
                        jmp     #done_and_inc_pc

zpu_neg                 neg     tos, tos
                        jmp     #done_and_inc_pc

zpu_syscall             jmp     #syscall

not_implemented
zpu_illegal
div_zero_error
                        call    #break
                        jmp     #done
'------------------------------------------------------------------------------
                        fit $FF                         'Opcode handlers must fit in 256 LONGS
'------------------------------------------------------------------------------
'ZPU memory space access routines

'Push a LONG onto the stack from "tos"
push_tos

#ifdef USE_JCACHED_MEMORY
                        mov       addr, sp
'                       call      #zpu_cache              'in-lining gives ity-bity speed improvement
                        mov       memp, addr              ' ptr + mboxdat = hub address of byte to load
                        and       memp, #(cache#LINELEN-1)
                        mov       temp, addr              'ptr + mboxdat = hub address of byte to load
                        andn      temp, #(cache#LINELEN-1)
                        cmp       cacheaddr,temp wz       'If cacheaddr == addr, just pull form cache
                        add       memp, mboxptr           'add ptr to memp to get data address
                                                          'memp gets overwriteen on a miss
'                       end call      #zpu_cache
            if_ne       call      #cache_write
                        wrlong    tos, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong    temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp       #:waitmbox              'instructions are not necc
                        mov       addr, sp
                        wrlong    tos, mboxdat
                        shl       addr, #9
                        movs      addr, #WRITEVML
                        wrlong    addr, mboxcmd

:waitdone               rdlong    addr, mboxcmd wz
                  if_nz jmp       #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, sp
                        add     memp, zpu_memory_addr
                        wrlong  tos, memp
#endif
                        sub     sp, #4
push_tos_ret            ret

'Pop a LONG from the stack into "data", set Z according to data.
pop                     add     sp, #4

#ifdef USE_JCACHED_MEMORY
                        mov     addr, sp
'                       call    #zpu_cache              'in-lining gives ity-bity speed improvement
                        mov     memp, addr              'ptr + mboxdat = hub address of byte to load
                        and     memp, #(cache#LINELEN-1)
                        mov     temp, addr              'ptr + mboxdat = hub address of byte to load
                        andn    temp, #(cache#LINELEN-1)
                        cmp     cacheaddr,temp wz       'if cacheaddr == addr, just pull form cache
                        add     memp, mboxptr           'add ptr to memp to get data address
                                                        'memp gets overwriteen on a miss
'                       end call    #zpu_cache
            if_ne       call    #cache_read
                        rdlong  data, memp wz
#endif
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
                        add     memp, zpu_memory_addr
                        rdlong  data, memp wz           'Must set Z for caller
#endif
pop_ret                 ret

'Read a LONG from ZPU memory at "address" into "data"
read_long               cmp     address, zpu_hub_start wc 'Check for normal memory access
              if_nc     jmp     #read_hub_long

#ifdef USE_JCACHED_MEMORY
                        mov     addr, address
                        call    #zpu_cache
            if_ne       call    #cache_read
                        rdlong  data, memp
#endif
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
                        add     memp, zpu_memory_addr
                        rdlong  data, memp
#endif
read_long_ret           ret

read_hub_long           cmp     address, zpu_cog_start wc 'Check for HUB memory access
              if_nc     jmp     #read_cog_long

                        sub     address, zpu_hub_start
                        rdlong  data, address
                        jmp     #read_long_ret

read_cog_long           cmp     address, zpu_io_start wc  'Check for COG memory access
              if_nc     jmp     #read_io_long

                        shr     address, #2
                        movs    :rcog, address
                        nop
:rcog                   mov     data, 0-0
                        jmp     #read_long_ret

read_io_long            cmp     address, timer_address wz 'Check for timer read
              if_z      mov     data, cnt
              if_z      jmp     #read_long_ret
                                                      'Must be other I/O address
                        wrlong  address, io_port_addr 'Set port address
                        mov     temp, #io_cmd_in      'Set I/O command to IN
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #:wait
                        rdlong  data, io_data_addr    'Get the port data
                        jmp     #read_long_ret

'Write a LONG from "data" to ZPU memory at "address"
write_long              cmp     address, zpu_hub_start wc 'Check for normal memory access
              if_nc     jmp     #write_hub_long

#ifdef USE_JCACHED_MEMORY
                        mov     addr, address
                        call    #zpu_cache
            if_ne       call    #cache_write
                        wrlong  data, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        wrlong  data, mboxdat
                        mov     addr, address
                        shl     addr, #9
                        movs    addr, #WRITEVML
                        wrlong  addr, mboxcmd

:waitdone               rdlong  addr, mboxcmd wz
                  if_nz jmp     #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, address
                        add     memp, zpu_memory_addr
                        wrlong  data, memp
#endif
write_long_ret          ret


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



'Read a WORD from ZPU memory at "address into "data"
read_word

#ifdef USE_JCACHED_MEMORY
                        mov     addr, address
''                        xor     addr, #%10              'XOR here is an endianess fix.
                        call    #zpu_cache
            if_ne       call    #cache_read
                        rdword  data, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, address
''                        xor     addr, #%10              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #READVMW
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdlong  data, mboxdat
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, address
                        add     memp, zpu_memory_addr
''                        xor     memp, #%10              'XOR here is an endianess fix.
                        rdword  data, memp
#endif
read_word_ret           ret

'Write a WORD from "data" to ZPU memory at "address"
write_word
#ifdef USE_JCACHED_MEMORY
                        mov     addr, address
''                        xor     addr, #%10              'XOR here is an endianess fix.
                        call    #zpu_cache
            if_ne       call    #cache_write
                        wrword  data, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        wrlong  data, mboxdat
                        mov     addr, address
''                        xor     addr, #%10              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #WRITEVMW
                        wrlong  addr, mboxcmd

:waitdone               rdlong  addr, mboxcmd wz
                  if_nz jmp     #:waitdone
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, address
                        add     memp, zpu_memory_addr
''                        xor     memp, #%10              'XOR here is an endianess fix.
                        wrword  data, memp
#endif
write_word_ret          ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
fast_mul                ' account for sign
#ifdef USE_FASTER_MULT
                        abs     tos, tos        wc
                        negc    data, data
                        abs     data, data      wc
                        ' make t2 the smaller of the 2 unsigned parameters
                        mov     t2, tos
                        max     t2, data
                        min     data, tos
                        ' corrct the sign of the adder
                        negc    data, data
#else
                        abs     t2, tos         wc
                        negc    data, data
#endif
                        ' my accumulator
                        mov     tos, #0
                        ' do the work
:mul_loop               shr     t2, #1          wc,wz   ' get the low bit of t2
        if_c            add     tos, data               ' if it was a 1, add adder to accumulator
                        shl     data, #1                ' shift the adder left by 1 bit
        if_nz           jmp     #:mul_loop              ' continue as long as there are no more 1's
                        ' "Run home, Jack!"
                        jmp     #done_and_inc_pc

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
{{==    div_flags: xxxx_invert result_store remainder   ==}}
{{==    NOTE: Caller must not allow data == 0!!!!       ==}}
fast_div                ' tos = tos / data
                        ' handle the signs, and check for a 0 divisor
                        and     div_flags, #1   wz      ' keep only the 0 bit, and remember if it's a 0
                        abs     t1, data        wc
             if_z_and_c or      div_flags, #2           ' data was negative, and we're looking for quotient, so set bit 1 hi
                        abs     data, tos       wc
              if_c      xor     div_flags, #2           ' tos was negative, invert bit 1 (quotient or remainder)
                        ' align the divisor to the leftmost bit
                        neg     t2, #1          wc      ' count how many times we shift (negative)
:align_loop             rcl     t1, #1          wc      ' left shift the divisior, marking when we hit a 1
              if_nc     djnz    t2, #:align_loop        ' the divisior MUST NOT BE 0
                        rcr     t1, #1                  ' restore the 1 bit we just nuked
                        neg     t2, t2                  ' how many times did we shift? (we started at -1 and counted down)
                        ' perform the division
                        mov     tos, #0
:div_loop               cmpsub  data, t1        wc      ' does the divisor fit into the dividend?
                        rcl     tos, #1                 ' if it did, store a one while shifting left
                        shr     t1, #1                  '
                        djnz    t2, #:div_loop
                        ' correct the sign
                        shr     div_flags, #1   wc,wz
              if_c      mov     tos, data               ' user wanted the remainder, not the quotient
                        negnz   tos, tos                ' need to invert the result
                        jmp     #done_and_inc_pc
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
zpu_im_next             shl     tos, #7
                        or      tos, data
                        jmp     #done_and_inc_pc

zpu_im_first            call    #push_tos
                        mov     tos, data
                        shl     tos, #(32 - 7)          'Sign extend
                        sar     tos, #(32 - 7)
                        mov     which_im, #zpu_im_next  'N.B. Drops through to done_and_inc_pc
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Main ZPU fetch and execute loop
done_and_inc_pc         add     pc, #1
done
execute
#ifdef USE_JCACHED_MEMORY
                        mov     addr, pc
''                        xor     addr, #%11              'XOR here is an endianess fix.

'                       call    #zpu_cache              'in-lining gives ity-bity speed improvement
                        mov     memp, addr              'ptr + mboxdat = hub address of byte to load
                        and     memp, #(cache#LINELEN-1)
                        mov     temp, addr              'ptr + mboxdat = hub address of byte to load
                        andn    temp, #(cache#LINELEN-1)
                        cmp     cacheaddr,temp wz       'if cacheaddr == addr, just pull form cache
                        add     memp, mboxptr           'add ptr to memp to get data address
                                                        'memp is overwriteen on a miss
'                       end call    #zpu_cache
            if_ne       call    #cache_read
                        rdbyte  data, memp
#endif
#ifdef USE_VIRTUAL_MEMORY
':waitmbox              rdlong  temp, mboxcmd wz        'If only one client to VMCOG, these first two
'              if_nz    jmp     #:waitmbox              'instructions are not necc
                        mov     addr, pc
''                        xor     addr, #%11              'XOR here is an endianess fix.
                        shl     addr, #9
                        movs    addr, #READVMB
                        wrlong  addr, mboxcmd
:waitres                rdlong  data, mboxcmd wz
              if_nz     jmp     #:waitres
                        rdbyte  data, mboxdat
#endif
#ifdef USE_HUB_MEMORY
                        mov     memp, pc
''                        xor     memp, #%11              'XOR here is an endianess fix.
                        add     memp, zpu_memory_addr
                        rdbyte  data, memp
#endif
#ifdef SINGLE_STEP
                        call    #break
#endif
                        cmpsub  data, #$80 wc           'Check for IM instruction. This saves table lookup
              if_c      jmp     which_im           'for the most common op. 7% fibo speed gain!
                        mov     which_im, #zpu_im_first 'select state
                        mov     address, data           'Some opcodes contains address offsets.

                        add     data, dispatch_tab_addr
                        rdbyte  temp, data
                        jmp     temp                    'No # here we are jumping through temp.
which_im		long	zpu_im_first
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
#ifdef USE_JCACHED_MEMORY
zpu_cache
                        mov     memp, addr                  'ptr + mboxdat = hub address of byte to load
                        and     memp, #(cache#LINELEN-1)
                        mov     temp, addr                  'ptr + mboxdat = hub address of byte to load
                        andn    temp, #(cache#LINELEN-1)
                        cmp     cacheaddr,temp wz           'if cacheaddr == addr, just pull form cache
                        add     memp, mboxptr               'add ptr to memp to get data address
                                                            'memp gets overwriteen on a miss
zpu_cache_ret           ret

cache_write             ' if cacheaddr <> addr, load new cache line
                        mov     memp, addr                  'save address for index
                        or      addr, #cache#WRITE_CMD
                        call    #cache_access
cache_write_ret         ret

cache_read              ' if cacheaddr <> addr, load new cache line
                        mov     memp, addr                  'save address for index
                        or      addr, #cache#READ_CMD
cache_access
                        wrlong  addr, mboxcmd
                        mov     cacheaddr,addr              'Save new cache address. it's free time here
                        andn    cacheaddr,#cache#LINELEN-1  'Kill command bits in free time
:waitres                rdlong  temp, mboxcmd
                        tjnz    temp, #:waitres             'We have room for tjnz 4 or 8 cycles
                        rdlong  mboxptr, mboxdat            'Get new buffer
                        and     memp, #(cache#LINELEN-1)    'memp is index into buffer
                        add     memp, mboxptr               'memp is now HUB buf address of data to read
cache_read_ret
cache_access_ret        ret

#endif
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'Initialisation code.
'This is a nice mess, all Zogs variables are overlaid against this code.
init
address                 mov     address, par             'Pick up first 6 longs of PAR block
zpu_memory_addr         rdlong  zpu_memory_addr, address 'HUB address of the ZPU memory area
memp                    add     address, #4              'Temporary pointer into ZPU memory space
zpu_memory_sz           rdlong  zpu_memory_sz, address   'Size of ZPU memory area in bytes
temp                    add     address, #4          'For temp operands etc
pc                      rdlong  pc, address          'ZPU Program Counter
instruction             add     address, #4          'Opcode being executed.
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
                        add     temp, #4
dm_addr                 mov     dm_addr, temp        'HUB address of decode mask
                        add     temp, #4             'Flag for the IM instruction
debug_addr              mov     debug_addr, temp     'HUB address of debug register

#ifdef USE_JCACHED_MEMORY
' Initialization and Variables used to support CACHE interface
cacheaddr               add     address, #4          'cache block address
mboxptr                 rdlong  temp, address        '8th par item is address of VMCog mailbox
addr                    mov     mboxcmd, temp
mboxcmd                 add     temp, #4             'next long is data for cached memory
mboxdat                 mov     mboxdat, temp
#endif

#ifdef USE_VIRTUAL_MEMORY
' Variables used to support VM readbyte/writebyte
                        add     address, #4
                        rdlong  temp, address        '8th par item is address of VMCog mailbox
addr                    mov     mboxcmd, temp        'Address we want to read, can be PC or whatever
mboxcmd                 add     temp, #8             'Pointer to first long of VMCOG mailbox (+0 offset)
mboxdat                 mov     mboxdat, temp        'Pointer to second long of VMCOG mailbox (+4 offset)
#endif

			mov	tos, #0
                        jmp     #execute
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
break                   wrlong  pc, pc_addr             'Dump registers to HUB.
                        wrlong  sp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  which_im, dm_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #:wait
break_ret               ret
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
syscall                 mov     address, sp             'Get syscall ID from stack
                        add     address, #8
                        call    #read_long
                        cmp     data, sys_cognew_ wz    'Is it SYS_cognew?
              if_nz     jmp     #syscall_external_handler

'On SYS_cognew syscall:
'ZPU [SP +  8] is syscall ID
'    [SP + 16] is address of PASM code to load
'    [SP + 20] is address of PAR for Cog
'Cog number or error code is returned in ZPU R0 which is memory address 0.
handle_sys_cognew
                        mov     coginit_dest, #%1000    'Set cognew "any cog will do" bit
                        add     address, #8             'Get HUB address of PASM code to run
                        call    #read_long
                        add     data, zpu_memory_addr   'Map to HUB address

                        and     data, word_mask         'Only 16 bit address
                        shl     data, #2                'Shift into bits 17:4 (only high 14 bits required)
                        or      coginit_dest, data      'Place in dest for coginit

                        add     address, #4             'Now offset 20 from SP
                        call    #read_long              'Get address of PAR data
                        add     data, zpu_memory_addr   'Map to HUB address

                        and     data, word_mask         'Only 16 bit address
                        shl     data, #16               'Move to bits 31:18 (only high 14 bits required)

                        or      coginit_dest, data      'Combine PASM addr and PAR addr
                        coginit coginit_dest wc,wz,wr
              if_c      mov     coginit_dest, minus_one 'Return -1 if no free cog
                        mov     address, #0             'Error is returned in ZPU reg 0, which is address 0
                        mov     data, coginit_dest
                        call    #write_long
                        jmp     #done_and_inc_pc

syscall_external_handler
                        wrlong  sp, sp_addr
                        mov     temp, #io_cmd_syscall   'Set I/O command to SYSCALL
                        wrlong  temp, io_command_addr
:wait                   rdlong  temp, io_command_addr wz'Wait for command completion
              if_nz     jmp     #:wait
                        rdlong  sp, sp_addr
                        jmp     #done_and_inc_pc
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Big constants
minus_one               long $FFFFFFFF
minus_two               long $FFFFFFFE
word_mask               long $0000FFFF
timer_address           long $80000100
sys_cognew_             long SYS_cognew

zpu_hub_start           long $10000000  'Start of HUB access window in ZPU memory space
zpu_cog_start           long $10008000  'Start of COG access window in ZPU memory space
zpu_io_start            long $10008800  'Start of IO access window
'------------------------------------------------------------------------------
                        fit     $1F0
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
{11}    byte  zpu_addsp
{12}    byte  zpu_addsp
{13}    byte  zpu_addsp
{14}    byte  zpu_addsp
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

{50}    byte  zpu_storesp_tos
{51}    byte  zpu_storesp_hi
{52}    byte  zpu_storesp_hi
{53}    byte  zpu_storesp_hi
{54}    byte  zpu_storesp_hi
{55}    byte  zpu_storesp_hi
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

