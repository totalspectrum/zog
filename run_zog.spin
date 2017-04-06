'*******************************************************************************
'*                                                                             *
'* run_zog.spin                                                                *
'*                                                                             *
'* Configures and runs the Zog ZPU bytecode interpreter.                       *
'* Optionally totally replaces all Spin in the Prop.                           *
'*                                                                             *
'* Author:  Michael Rychlik                                                    *
'*                                                                             *
'*******************************************************************************
'
' Loads a ZPU byte code executable from a "file" statement.
' Starts a Cog to run the ZPU interpreter.
' This COG then stops leaving the entire Propeller to ZPU code and it's COG PASM firmware.
' If it is required to run other Spin/PASM as well then add your code after
' the cog start. Decrease the size of zpu memory space to accomodate it.
'
' If the "Cuckoo Mode" is enabled, see below, all Spin is displaced from the Prop
' and Zog takes over all HUB memory.
' The final Zog memory map is defined at the end of this file.
'
' N.B. #define USE_HUB_MEMORY must be defined in Zog.spin.
'
' v1.0      08-07-2010 First draft but works!
'                      The Zog VM is now split out into the separate zog.spin
'
' v1.1      17-08-2010 Added some new PAR parameters for Zog.
'
' v1.2      17-08-2010 No changes here but all make files for ZPU executables now perform
'                      and endianness swap on the binaries as byte swapping is not done here anymore.
'
' v1.3      18-08-2010 No change but bump version inline with ZOG.
'
' v1.4      16-08-2010 No change but bump version inline with ZOG.
'
CON

_clkmode        = xtal1 + pll16x
_xinfreq        = 5_000_000

'In Cuckoo mode Zog takes over the entire Propeller displacing Spin entirely.
'This is done my staring Zog from a PASM boot loader.
'Without Cuckoo Mode Zog runs in it's defined space in HUB and can coexist with Spin.
'#define CUCKOO_MODE

'The size of ZPU memory area in HUB
zpu_memory_size = (24 * 1024)

OBJ
  zog  : "zog.spin"

DAT                     org 0
zpu_memory              byte ' Force zpu_memory to be BYTE type.
zpu_image               file "test_libzog.bin"
padding                 byte 0[(zpu_memory_size) - (@padding - @zpu_memory)]
zpu_memory_end
                        fit (zpu_memory_size / 4)

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

#ifdef USE_VIRTUAL_MEMORY
  'Mail box for Virtual Memory
  'Need not follow on from zog_mbox above
  long vm_mbox[4]
#endif

PUB start | b, ad
  'Set up Zog's parameter block
  par_zpu_memory_addr := @zpu_memory
  par_zpu_memory_sz := zpu_memory_size
  par_initial_pc := 0
  par_initial_sp := zpu_memory_size - 8
  par_dispatch_tab_addr := zog.getdispatch_table
  par_pasm_addr := zog.getzog
  par_zog_mbox_addr := @zog_mbox_command
#ifdef USE_VIRTUAL_MEMORY
  par_vm_mbox_addr := @vm_mbox
#endif

#ifdef CUCKOO_MODE
#warn Zog is taking over the Propeller, all Spin code will be overwritten!
  'Replace this Spin COG, probably Cog 0, with the Zog bootloader PASM
  coginit (cogid, @boot_zog, @par_zpu_memory_addr)
#else
  zog.start(@par_zpu_memory_addr)
#endif
  'N.B. This spin COG terminates at this point.
  '     From now on there is only Zog running C code and PASM firmware!

#ifdef CUCKOO_MODE
DAT
                        org     0
'Copy PAR block into COG
boot_zog                mov     address, par
                        rdlong  zpu_memory_addr, address
                        add     address, #4
                        rdlong  zpu_memory_sz, address
                        add     address, #4
                        rdlong  initial_pc, address
                        add     address, #4
                        rdlong  initial_sp, address
                        add     address, #4
                        rdlong  dispatch_tab_addr, address
                        add     address, #4
                        rdlong  pasm_addr, address

'Move ZPU bytecode image to it's final run time position in HUB RAM
move_image              mov     src, zpu_memory_addr
                        mov     dest, zpu_memory_final
                        mov     count, zpu_memory_sz
:loop                   rdbyte  temp, src
                        wrbyte  temp, dest
                        add     src, #1
                        add     dest, #1
                        djnz    count, #:loop
                        mov     zpu_memory_addr, zpu_memory_final  'Update param block with new address

'Move ZOG dispatch table runtime position in HUB RAM
move_dispatch           mov     src, dispatch_tab_addr
                        mov     dest, dispatch_tab_final
                        mov     count, #$80
:loop                   rdbyte  temp, src
                        wrbyte  temp, dest
                        add     src, #1
                        add     dest, #1
                        djnz    count, #:loop
                        mov     dispatch_tab_addr, dispatch_tab_final 'Update param block with new address

'Set Zog's inital runtime PC and SP
set_zog_regs            mov     initial_pc, #0
                        mov     initial_sp, zpu_stack_final

'Copy updated PAR block back to HUB in new location
write_par_block         mov     address, params
                        wrlong  zpu_memory_addr, address
                        add     address, #4
                        wrlong  zpu_memory_sz, address
                        add     address, #4
                        wrlong  initial_pc, address
                        add     address, #4
                        wrlong  initial_sp, address
                        add     address, #4
                        wrlong  dispatch_tab_addr, address
                        add     address, #4
                        wrlong  pasm_addr, address      'Update param block with new address

'Launch Zog in this COG.
go_zog                  cogid   coginit_dest            'Get ID of this COG for restarting
                        mov     temp, pasm_addr         'Set ZOG PASM address for coginit
                        shl     temp, #2                'Shift into bits 17:4 (only high 14 bits required)
                        or      coginit_dest, temp      'Place in dest for coginit

                        mov     temp, params            'Set PAR block address for coginit
                        shl     temp, #16               'Move to bits 31:18 (only high 14 bits required)
                        or      coginit_dest, temp      'Combine PASM addr and PAR addr

                        coginit coginit_dest


'Hub address of Zog's par block
params                  long    $7C00  'Somewhere in high memory that is not currently used.

'6 consecutive longs for Zog's par block
zpu_memory_addr         long    0
zpu_memory_sz           long    0
initial_pc              long    0
initial_sp              long    0
dispatch_tab_addr       long    0
pasm_addr               long    0

address                 long    0
dest                    long    0
src                     long    0
count                   long    0
temp                    long    0
this_cog                long    0
coginit_dest            long    0

'Zog runtime memory map (If Cuckoo bootloader used).
'The final run time position of Zog's dispatch table and ZPU memory area.
dispatch_tab_final      long    $8000 - $80 'Last 128 bytes of HUB RAM
zpu_memory_final        long    0           'Let's use all HUB
zpu_stack_final         long    $7A00       'Must leave room for VMCog working set.

                        fit     $01ff
#endif
'The End.

