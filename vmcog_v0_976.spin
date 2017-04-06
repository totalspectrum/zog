'----------------------------------------------------------------------------------------------------
'
' VMCOG v0.976 - virtual memory server for the Propeller
'
' Copyright February 3, 2010 by William Henning
'
' This code is released under the terms of the MIT license, "Atribution Required", and this
' Copyright notice may not be removed. Basically this means that as long as you leave this
' copyright notice in any code that uses this file, and provide credit to the authors in
' the source and documentation for your product, you may use this code. Please note that
' this does NOT give you any license to the hardware the code may be written for - for example,
' if there is an XMM driver for Acme board's XMM interface, it does not give you the right to
' duplicate the Acme board, or its XMM interface.
'
' At this point this code is just a skeleton of what will come, but it is enough to illustrate
' the concept.
'
' Feb3  v0.10 - initial upload
' Feb3  v0.11 - fixes dumb bug in mailbox layout
' Feb4  v0.12 - added "vmdump", saves page table to hub for analysis
' Feb4  v0.13 - added constants for SPI RAM pins, GETVMPAGE and PUTVMPAGE
' Feb4  v0.14 - removed "chipsel", auto-selecting CS based on bit 15 of virtual address in BSTART
' Feb5  v0.15 - added "firstpage","lastpage","nextpage"
' Feb5  v0.16 - added start of Spin API
' Feb5  v0.17 - removed InPhys, just use GetPhysVirt, returns 0 if not in memory
' Feb6  v0.18 - changed to separate messages for BYTE/WORD/LONG read and write
' Feb8  v0.20 - VMDUMP almost works, still SPI RAM issues
' Feb9  v0.21 - VMDUMP works, debugger renamed
' Feb9  v0.23 - rdvbyte, wrvbyte, vmflush, vmdump, getvirtphys all work :)
' Feb9  v0.24 - removed vminc, adding #1
' Feb10 v0.25 - added aligned VMREADW and VMREADL
' Feb10 v0.26 - added aligned VMWRITEW and VMWRITEL
' Feb10 v0.27 - refactored writes, saved many longs :)
' Feb10 v0.28 - added FINDLRU, some code to vmgetpage and vmputpage
' Feb11 v0.29 - code refactoring, was running out of cog space for SPI code
' Feb11 v0.30 - recovered 11 longs by moving init routine into TLB space
' Feb11 v0.31 - recovered more space by refactoring reads, removing BEND and FINDLRU
' Feb11 v0.32 - eliminated vmgetpage and vmputpage messages
' Feb12 v0.33 - combined vmaddr & command to reduce hub accesses, reads 1 instruction faster, saved 3 longs
' Feb28 v0.50 - started change to 512 byte pages
' Mar10 v0.51 - changed TLB long definition, added Lock bit, increased counter to 23 bits
' Apr15 v0.60 - added memory hit/miss global counters for calculating hit rates and flushes, minor fixes
' Apr15 v0.70 - added PropCade 23K256 code
' Apr16 v0.75 - implemented BSTART, BREAD, BWRITE using 23K256 PropCade code
' Apr16 v0.80 - started implementing "find page to sacrifice"
' Apr16 v0.85 - init SPI ram's, VMFLUSH and VMDUMP seem to work, rest not tested yet
' Apr16 v0.86 - added constants PAGESIZE, TLBENTRIES, MEMSIZE, rest of messages that are implemented tested OK
' Apr17 v0.87 - added initializing working set from SPI ram, finished implementing "find page to sacrifice"
' Apr18 v0.88 - fixed IC1 initialization issue
' Jun3  v0.89 - reversed order of working set allocation to make lock() easier, fixed write word / long busted in .85
' Jun3  v0.90 - minor optimizations on invoking BUSERR, fixed find sacrifical page
' Jun4  v0.91 - added 'ifdef EXTRAM' for heater, and conditional compilation based on platform
' Jun4  v0.92 - flushes dirty pages, handles TLB correctly - appears to work! NEEDS THOROUGH TESTING
' Jun4  v0.93 - Michael Rychlik (aka heater) added support for external RAM on TriBlade blade #2
' Jun5  v0.940 - implemented shr_hits, cleanup
' Jun5  v0.941 - clear low vmaddr bits in TriBlade_2 BSTART
' Jun5  v0.942 - disabled shr_hits for now, looking for bug in writing out pages
' Jun6  v0.950 - fixed PropCade multi-chip initialization bug in ini_lp
' Jun7  v0.950+Triblade_0.4 - Heater - Fixed TriBladeProp latch loading on start up (BINIT),
'                             no longer requires the TriBladeProp Driver.
' Jun7  v0.950+Triblade_0.5 - Heater - A little TriBlade optimization.
' Jun7  v0.960 - fixed "lost first write" bug, was clobbering temp in BUSERR
' Jun7  v0.961 - merged jazzed's XEDODRAM platform
' Jun10 v0.965 - fixed XEDODRAM merge (initially I left a few lines out)
' Jun10 v0.966 - merged latest TriBlade_2_0.9 code from heater
' Jun10 v0.967 - merged HYDRA HX512 support by Antoine Doinel, moved DAT to platform DAT area
' Jun11 v0.968 - start now presets cmd to $FFFF0000, end of init can now be detected (thanks steve)
' Jun15 v0.970 - now sets new pages access count to average of all page access count - see 'hitavg'
' Jun15 v0.971 - fixed hitavg
' Jun16 v0.973 - FIXED THRASHING BUG!
' Jun16 v0.974 - fixed shr_hits bug!
' Jul21 v0.975 - added SPI RAM status read command
' Jul31 v0.976 - found problem! BINIT fixed for SPI rams on new PropCade
' Aug17 v.0976+TriBlade_0.11 - Heater - Moved TriBlade 2 RAM init code out of BININT
'                              and start code out of BSTART. Added tristating of the bus
'                              after BREAD or BWRITE. Now shares bus with SD card.
'                              All TriBlade optimizations are included here.
{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}

'----------------------------------------------------------------------------------------------------
' Conditional compilation controls - use these to configure VMCOG for your platform
'----------------------------------------------------------------------------------------------------

' Currently PROPCADE, TRIBLADE_2, XEDODRAM and HYDRA are supported. More support soon!

#define EXTRAM
#ifdef EXTRAM
'#define PROPCADE
'#define FLEXMEM
'#define MORPHEUS_SPI
'#define MORPHEUS_XMM
'#define MORPHEUS3_SPI
'#define PLCG
'#define DRACBLADE
'#define TRIBLADE
'#define RAMBLADE
#define TRIBLADE_2
'#define XEDODRAM
'#define HYDRA
#else
#warn External Ram access disabled - only use memory up to WS_SIZE
#endif

'----------------------------------------------------------------------------------------------------
' Constants defined for VMCOG use
'----------------------------------------------------------------------------------------------------

CON

  '--------------------------------------------------------------------------------------------------
  ' VMCOG exported constants
  '--------------------------------------------------------------------------------------------------

  PAGESIZE      = 512
  TLBENTRIES    = 128
  MEMSIZE       = PAGESIZE * TLBENTRIES

  '--------------------------------------------------------------------------------------------------
  ' VMCOG commands
  '--------------------------------------------------------------------------------------------------

  FLUSHVM       = 128 ' clear all page table entries, resets VMCOG
  DUMPVM        = 129 ' dump all page table entries to specified hub address (NOTE: needs 1KB ie 256 LONG's)

  READVMB       = 130 ' read a byte/word/long from VM
  READVMW       = 131 ' read a byte/word/long from VM
  READVML       = 132 ' read a byte/word/long from VM

  WRITEVMB      = 133 ' write a byte/word/long to VM
  WRITEVMW      = 134 ' write a byte/word/long to VM
  WRITEVML      = 135 ' write a byte/word/long to VM

  VIRTPHYS      = 136 ' get what page a virtual location is mapped to, returns 0 if not in working set
  PHYSVIRT      = 137 ' get what virtual page a physical page represents, return 0 if not in working set

  LOCKPAGE      = 138 ' lock N pages so they can't be swapped, takes vaddr and num pages as argument, returns vaddr
  UNLOCKPAGE    = 139 ' unlocked a page locked into memory, takes vaddr and page count

  READSTATUS    = 140 ' Read SPI RAM status register

#ifdef PROPCADE
  '--------------------------------------------------------------------------------------------------
  ' SPI RAM Driver constants
  '--------------------------------------------------------------------------------------------------

  ' PropCade version - bit masks for the pins

  CS            = 1
  CLK           = 2
  MOSI          = 4
  MISO          = 8
#endif

#ifdef HYDRA
  '--------------------------------------------------------------------------------------------------
  ' HYDRA HX512 Driver constants
  '--------------------------------------------------------------------------------------------------

  ' Choose one:
  '   HYDRA_FAST for 10ns SRAM chip (like mine ;^)
  '   HYDRA_SAFE for 55ns SRAM chip (? not sure about speed of other existing variants)

'#define HYDRA_FAST
#define HYDRA_SAFE
#endif


'----------------------------------------------------------------------------------------------------
' Start VMCOG
'
' You must reserve 4 longs for the mailbox, and pass its address to Start in order to run VMCOG.
' The reason you must provide your own mailbox is to allow more than VMCOG to run on the same
' Propeller.
'
'----------------------------------------------------------------------------------------------------

VAR

  long cmdptr, vaddrptr, dataptr, fakebox[4], fdataptr, fcmdptr

' when command is done, VMCOG will write 0 to cmd, and 1 to bytes (one long write)

#ifdef XEDODRAM
' xmailbox is a command and data word
' command is %CCCC_LLLL_LLLL_AAAA_AAAA_AAAA_AAAA_AAAA
' C is Command bits up to 16 commands
' L is Length bits up to 256 bytes
' A is Address bits up to 1MB address range
' data is interpreter based on command word context
' data is a pointer in case of buffer read/write
' data is a long/word/byte in other command cases
'
  long xmailbox

OBJ
  xm : "XEDODRAM_1MB"
#endif

PUB start(mailbox,lastp,nump)

  cmdptr   := mailbox
  vaddrptr := mailbox+4
  dataptr  := mailbox+8

  long[vaddrptr] := lastp-((nump-1)*512) ' singe byte read/writes are the default
  long[dataptr]  := nump        ' singe byte read/writes are the default
  word[cmdptr]   := $FFFF0000

#ifdef XEDODRAM
  longfill(@xmailbox,0,2)       ' ensure command starts as 0
  long[mailbox+12]:= @xmailbox  ' 3rd vmcog mailbox word used for xm interface
  xm.start(@xmailbox)           ' start up inter-cog xmem
#endif

  cognew(@vmcog,mailbox)

  fcmdptr  := @fakebox
  fdataptr := fcmdptr+4
  repeat while long[cmdptr]     ' should fix startup bug heater found - it was the delay to load/init the cog

PUB rdvbyte(adr)
  repeat while long[cmdptr]
  long[cmdptr] := (adr<<9)|READVMB
  repeat while long[cmdptr]
  return long[dataptr]

PUB rdvword(adr)
  repeat while long[cmdptr]
  long[cmdptr] := (adr<<9)|READVMW
  repeat while long[cmdptr]
  return long[dataptr]

PUB rdvlong(adr)
  repeat while long[cmdptr]
  long[cmdptr] := (adr<<9)|READVML
  repeat while long[cmdptr]
  return long[dataptr]

PUB wrvbyte(adr,dt)
  repeat while long[cmdptr]
  long[dataptr]  := dt
  long[cmdptr] := (adr<<9)|WRITEVMB
  repeat while long[cmdptr]

PUB wrvword(adr,dt)
  repeat while long[cmdptr]
  long[dataptr]  := dt
  long[cmdptr] := (adr<<9)|WRITEVMW
  repeat while long[cmdptr]

PUB wrvlong(adr,dt)
  repeat while long[cmdptr]
  long[dataptr]  := dt
  long[cmdptr] := (adr<<9)|WRITEVML
  repeat while long[cmdptr]

PUB rdfbyte(adr)
  repeat while 0
  long[fcmdptr] := (adr<<9)|READVMB
  repeat while 0
  return 0

PUB wrfbyte(adr,dt)
  repeat while 0
  long[fdataptr]  := dt
  long[fcmdptr] := (adr<<9)|WRITEVMB
  repeat while 0

PUB Flush
  repeat while long[cmdptr]
  word[cmdptr]   := FLUSHVM
  repeat while long[cmdptr]
  return

PUB Look(adr)
  repeat while long[cmdptr]
  long[dataptr]   := adr
  long[cmdptr]    := DUMPVM
  repeat while long[cmdptr]
  return

PUB GetPhysVirt(vaddr)
  repeat while long[cmdptr]
  long[cmdptr] := (vaddr<<9)|VIRTPHYS
  repeat while long[cmdptr]
  return long[dataptr]

PUB GetVirtLoadAddr(vaddr)|va
  va:= vaddr&$7FFE00         ' 23 bit VM address - force start of page
  wrvbyte(vaddr,rdvbyte(va)) ' force page into working set, set dirty bit
  return GetPhysVirt(va)     ' note returned pointer only valid until next vm call

PUB GetVirtPhys(adr)
'  waitcnt(cnt+80_000_000)
  return 0

PUB Lock(vaddr,pages) ' should be called after Flush, before any other access
  return 0

PUB Unlock(vaddr,page)
  return 0

PUB Read_Status(vaddr)
  repeat while long[cmdptr]
  long[cmdptr] := (vaddr<<9)|READSTATUS
  repeat while long[cmdptr]
  return long[dataptr]

DAT

'----------------------------------------------------------------------------------------------------
' Start at COG address 0
'----------------------------------------------------------------------------------------------------

        org   $0

'----------------------------------------------------------------------------------------------------
' VM LUT for virtual to physical address translation, 256 pages of 256 bytes --> 64KB virtual memory
'
' LUT Entry values take the following form:
'
' if 0, page is not present in memory
'
' if not 0, the 32 bits are interpreted as:
'
' CCCCCCCC CCCCCCCC CCCCCLDP PPPPPPPP
'
' where
'
' PPPPPPPPPP = hub address, upper 9 bits of up to 18 bit address, 000xxxxxx on Prop1
'
' This allows using up to 256KB of the hub as the working set on the future Prop2
'
' The hub address is stored here so that the MOVS instruction can be used to update it without
' disturbing the rest of the bits in the page table entry
'
' D = Dirty bit
'
' This bit is set whenever a write is performed to any byte(s) in the page
'
' L = Locked bit
'
' This bit is set whenever the physical memory page is locked into the hub and may not be swapped out
'
' CCCCCCCC CCCCCCCC CCCCC = 21 bit read access counter
'
' Every time a read or write is performed to this page, this count is incremented
' If the count overflows into carry, every page count in the address translation table will
' will be divided by two in order to ensure that the LRU page replacement algorithm will work well
'
'
'----------------------------------------------------------------------------------------------------

vmcog
'----------------------------------------------------------------------------------------------------
' VMCOG initialization code - gets overwritten by vmflush, so no wasted space :)
'----------------------------------------------------------------------------------------------------

init_vm mov   pvmcmd,  par       ' save pointer to mailbox, first word is command
        add   pvmcmd,  #4
        mov   pvmaddr, pvmcmd    ' pvmaddr = pointer to long in hub that holds the vmaddr
        add   pvmcmd,  #4
        mov   pvmdata, pvmcmd    ' pdata = pointer to long in hub for byte/word/long to be read/written to/from vm
        sub   pvmcmd,  #8

        rdlong lastpage, pvmaddr ' on startup vmaddr contains the hub address of theLAST page in the working set
        shr    lastpage, #9      ' adjust for 512 byte pages
        rdlong numpages, pvmdata ' on startup vmdata contains number of pages in working set

#ifdef EXTRAM
'----------------------------------------------------------------------------------------------------
' BINIT SECTION - place your platform specific run-once (on startup) code here
'
' NOTE: There is at most 108 instructions available for initialization code - this area will be
'       over-written by the actual TLB
'----------------------------------------------------------------------------------------------------
#ifdef PROPCADE
        mov   dira,spidir
        ' Initialize up to six SPI RAM's on PropCade to sequential mode
        mov  count2,#6
ini_lp  mov  outa,#0
        mov  addr,ramseq
        mov  bits,#16
        andn outa,#CS|CLK
        call #send
        or   outa,#CS
        add outa,dstinc
        djnz count2,#ini_lp
#endif
#ifdef FLEXMEM
        'End of FlexMem code
#endif
#ifdef MORPHEUS_SPI
        'End of Morpheus SPI code
#endif
#ifdef MORPHEUS_XMM
        'End of Morpheus XMM code
#endif
#ifdef MORPHEUS3_SPI
        'End of Morpheus SPI code
#endif
#ifdef PLCG
        'End of PLCG code
#endif
#ifdef DRACBLADE
        'End of DracBlade code
#endif
#ifdef TRIBLADE_2
        'Nothing required here.
	'End of TriBlade code
#endif
#ifdef XEDODRAM
        mov     XDRAM_cmd,par
        add     XDRAM_cmd,#12        ' par+n+4 for dat
        rdlong  XDRAM_cmd,XDRAM_cmd
        mov     XDRAM_dat,XDRAM_cmd
        add     XDRAM_dat,#4        ' par+n+4 for dat
#endif
#ifdef HYDRA
        mov   outa, #0          ' clear all pins
        mov   dira, dira_mask   ' RST/CLK/DBUS/CTRL as outputs (implicit reset, active low)
        or    outa, reset_mask  ' RST=1 (not needed on HYDRA and HYBRID, should be harmless)
        andn  dira, reset_mask  ' RST=in (and leave it so from now on, pull-up will keep it high)
        mov   data, #%001111    ' auto-increment on both reads and writes
        shl   data, #16         ' shift in place
        or    outa, data        ' put it to DBUS
        or    outa, clock_mask  ' \ load auto-increment program...
        andn  outa, clock_mask  ' / ...into the CPLD internal register
        andn  outa, data_mask   ' DBUS=$00 setup for clearing address
        or    outa, ctrl_mask   ' CTRL %00->%11 (ADDR_HI)
        or    outa, clock_mask  ' \ will load A8..A15 and clear A16..A18 (std firmware)...
        andn  outa, clock_mask  ' / ...or load A8..A15 and advance internal state (ehn firmware)
        or    outa, clock_mask  ' \ redundant, same as above (std firmware)...
        andn  outa, clock_mask  ' / ...or load A16..A18 (ehn firmware)
        andn  outa, ctl0_mask   ' CTRL %11->%10 (ADDR_LO)
        or    outa, clock_mask  ' \ will load A0..A7 (any firmware)...
        andn  outa, clock_mask  ' / ...as we only R/W in pages, we will not need to set it anymore
        andn  dira, data_mask   ' leave DBUS as high-Z
        'End of Hydra code
#endif
'----------------------------------------------------------------------------------------------------
' END OF BINIT SECTION
'----------------------------------------------------------------------------------------------------
#endif

        jmp   #vmflush

fillme  long  0[128-fillme]     ' first 128 cog locations are used for a direct mapped page table

        fit   128

'----------------------------------------------------------------------------------------------------
' VMCOG command vectors
'----------------------------------------------------------------------------------------------------

flushv  jmp   #vmflush  ' due to above, this is at cog address 256
        jmp   #vmdump

        jmp   #vmreadb
        jmp   #vmreadw
        jmp   #vmreadl

        jmp   #vmwriteb
        jmp   #vmwritew
        jmp   #vmwritel

        jmp   #vmvirtphys
        jmp   #vmphysvirt

        jmp   #vmlock
        jmp   #vmunlock

'        jmp   #read_stat

'----------------------------------------------------------------------------------------------------
'
' VMFLUSH
'
' Flush the page translation table, automatic on initialization
'
' re-initialize page table so first <numpages> entries point to working set pages
' load the first <numpages> into the working set
' set dirty flag = 0, count = 0, access_count = 1 for all working set pages
'
'----------------------------------------------------------------------------------------------------

'              _____access_count______ L D _hubpage_
'accinc  long  %00000_00000000_00000001_0_0_000000000 ' used to init TLB, and to inc access counter
dmask   long  %0_111111111_000000000
count2  long  0

vmflush movd  vflush,#0
        mov   count,#128

vflush  mov   0, #0
        add   vflush,dstinc
        djnz  count,#vflush

        movs  vset, lastpage
        movd  vset, #0
        mov   count2, numpages

vset    movs  0,#0
        mov   vmaddr,vset       ' destination of vset instruction is VM page address!
        and   vmaddr,dmask      ' no need, masked in BSTART
        mov   hubaddr,vset      ' low 9 bits of vset is upper bits of hub addr!
        shl   hubaddr,#9
        and   hubaddr,dmask     ' no need, done in BSTART clear low bits to make hubptr correct
        call  #BREAD            ' page in the data
        add   vset,#1
        add   vset,dstinc
        djnz  count2,#vset

'----------------------------------------------------------------------------------------------------
' Wait for a command
'----------------------------------------------------------------------------------------------------

waitcmd wrlong zero,pvmcmd

wl	rdlong	vminst, pvmcmd wz ' top 23 bits = address, bottom 9 = command
	mov	vmaddr, vminst
 if_z	jmp	#wl

	shr	vmaddr, #9
        mov     vmpage, vmaddr
	shr	vmpage, #9
	movs	present, vmpage

zero    LONG 0                    ' used as a zero constant, plus as NOP for delay slot

present mov     vmentry,0-0 wz    'Z=1 not present, Z=0 present
        and     vmentry, #511
        shl     vmentry, #9       ' vmentry now points to correct hub page
        movs    vmentry, vmaddr   ' now put in offset - vmentry now is correct hub addr!

vector	jmp	vminst            ' ignores top 23 bits


vminst  long  0                   ' place holder for handler address
vmentry long  0
rhitcount long 1 << 11
whitcount long 1 << 11

'----------------------------------------------------------------------------------------------------
'
' VMVIRTPHYS
'
' return address of physical page that a virtual memory loc is mapped to, 0 if not in working set
'
'----------------------------------------------------------------------------------------------------

vmvirtphys
        wrlong vmentry, pvmdata
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMPHYSVIRT
'
' return address of physical page that a virtual memory loc is mapped to, 0 if not in working set
'
'----------------------------------------------------------------------------------------------------

vmphysvirt
        ' look up vmdata to see if it matches a real page address in TLB
        ' if yes, return vm address of the page
        ' if no, return 0
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMDUMP
'
' Save a snapshot of the current page table at the hub address pointed to by vmdata
' so that usage can be analyzed
'
'----------------------------------------------------------------------------------------------------

vmdump  movd  dloop,#0
        mov   count,#128

        rdlong vmaddr, pvmdata

dloop   wrlong 0,vmaddr
        add    vmaddr,#4
        add    dloop,dstinc
        djnz   count,#dloop

        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMREADB
'
' Read byte from the working set, page in/out as needed
'
'----------------------------------------------------------------------------------------------------

VMREADB
 if_z   call  #BUSERR   ' page in missing vm page

READM1  rdbyte temp,vmentry

' RDONE is shared by VMREADB / VMREADW / VMREADL

RDONE   movd  usec1,vmpage
        wrlong temp,pvmdata
usec1   add   0-0,rhitcount wc
  if_c  call  #shr_hits ' divide all hit counts by two, for fairness, if any overflow
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' SHR_HITS - divide all valid hit counts by two, called when a hit count would wrap around
'
' NOTE: shr_hits is not debugged yet!
'
'----------------------------------------------------------------------------------------------------


shr_hits      ' walk through TLB, divide all non-zero hit counts by two
        movs  jx,#0             ' finding candidate page to sacrifice

forj

jx      mov   tlbi,0-0 wz
 if_z   jmp   #nextj
        movd  updtc,jx
        mov   temp,tlbi
        andn  temp,elevenbits
        shr   tlbi,#1
        andn  tlbi,elevenbits
        or    tlbi,temp

updtc   mov   0-0,tlbi

        ' next ix
nextj   add   jx, #1
        and   jx, #128 nr, wz
 if_z   jmp   #forj
shr_hits_ret  ret

elevenbits long $07FF

'----------------------------------------------------------------------------------------------------
'
' BUSERR - requested virtual page not resident in the working set
'
'----------------------------------------------------------------------------------------------------

' OK, if we get here, we know:
'
' vmaddr = full VM address
' vmpage = (vmaddr >> 9) ie index into TLB
' vmentry is 0 if we got here - that is, TLB[vmpage]==0 or we would not be here!

BUSERR  ' location not mapped to physical memory
#ifdef EXTRAM

        movs  ix,#0             ' finding candidate page to sacrifice
        mov   minent,#511       ' minent = 511
        mov   minacc,bigacc     ' minacc = $FFFFFFFF

fori    ' for ix=0 to 127
        '   if TLB[ix]
ix      mov   tlbi,0-0 wz
 if_z   jmp   #nexti
        '     if TLB[ix].locked == 0
        and   tlbi,VM_LOCK nr, wz
 if_nz  jmp   #nexti
        '       if TLB[ix].accesses < minacc
        mov   temp,tlbi
        shr   temp,#11  ' temp is now TLB[ix].accesses
        cmp   temp,minacc wc
 if_nc  jmp   #nexti    ' nope, not smaller
        '         minacc = TLB
        mov   minacc,temp
        '         minent = ix
        movs  minent,ix
        ' next ix
nexti   add   ix, #1
        and   ix, #128 nr, wz
 if_z   jmp   #fori

        ' above verified, found sacrificial page, move its entry into tlbi

        movs  gettlbi,minent
        movd  clrold,minent
gettlbi mov   tlbi,0-0

        mov   vmentry,tlbi
        shl   vmentry,#9
        movs  vmentry,vmaddr    ' make it into a physical address

        '   if TLB[minent] & VM_DIRTY
        and   tlbi,VM_DIRTY nr,wz
        '     flush page out to disk
        mov   hubaddr,tlbi
        shl   hubaddr,#9
        if_z  jmp #clrold       ' page was clean, just clear old entry

        ' flush out dirty page
        mov   vmaddr,minent
        shl   vmaddr,#9
        ' **** NEED TO SET UP vmentry pointer so reads/writes work *****!!!!!
        call  #BWRITE

clrold  mov   0-0,#0            ' clear old version
        movd  setent,vmpage
' method #1 - leave access count alone
        andn  tlbi,VM_FLAGS    ' does not work - cannot keep count
' method #2 - semi-average
'        call  #hitavg

setent  mov   0-0,tlbi

rdin    '   fill TLB[minent].hubaddr with BREAD
        mov   vmaddr,vmpage
        shl   vmaddr,#9
        call  #BREAD 'fetch page
#endif
BUSERR_RET ret

minacc  LONG  $0        ' current lowest non-locked hit count
minent  LONG  0         ' if not 511, then it is the TLB entry whose physical page we will re-use
bigacc  LONG  $00400000 ' impossibly large hit count
tlbi    LONG  0         ' copy of the sacrificial entry

'----------------------------------------------------------------------------------------------------
' hitavg
'----------------------------------------------------------------------------------------------------


hitavg  mov   temp,#0
        movs  hitloop,#0
        mov   count,#TLBENTRIES

hitloop mov   count2,0-0
        shr   count2,#11
        add   temp,count2
        add   hitloop,#1
        djnz  count,#hitloop

        shr   temp,#7
        shl   temp,#13

        or    tlbi,temp ' $$$ NEW
hitavg_ret ret

xtemp   long  0
xcount  long  0
xcount2 long  0

'----------------------------------------------------------------------------------------------------
'
' VMREADW
'
' Read word from the working set, page in/out as needed
'
'----------------------------------------------------------------------------------------------------

VMREADW
 if_z   call  #BUSERR   ' page not present

READM2  and   vmaddr,#1 nr, wz
' if_nz  jmp   #ALIGWE   ' alignment error

        ' do the aligned read
        rdword temp,vmentry
        jmp   #RDONE

        ' handle unaligned read - only tough if vmaddr will roll over to next page
'ALIGWE  mov   temp,vmaddr
'        add   temp,#1
'        AND   temp,#$ff nr,wz  ' if zero, rolls onto next page
' if_nz  jmp   #read2

        ' page fault - fetch second virtual page
        ' return *(temp)<<8+*(vaddr) from two diff pages

'read2  ' return *(temp)<<8+*(vaddr) from same page
'        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMREADL
'
' Read long from the working set, page in/out as needed
'
'----------------------------------------------------------------------------------------------------

VMREADL
 if_z   call  #BUSERR   ' page not present

READM4  and   vmaddr,#3 nr, wz
' if_nz  jmp   #ALIGLE   ' alignment error

        ' handle the aligned read
        rdlong temp,vmentry
        jmp   #RDONE

        ' handle unaligned read - fix this code!
'ALIGLE  mov   temp,vmaddr
'        and   temp,#$ff
'        add   temp,#3
'        and   temp,#$100 nr,wz ' if not zero, spans pages
' if_z   jmp   #readu4
        ' page fault - fetch second virtual page
        ' return long value spanning two pages

'readu4  ' return long value from same page
'        jmp   #waitcmd


'----------------------------------------------------------------------------------------------------
'
' VMWRCOM
'
' Code common to all VMWRITEB/W/L
'
' Marks page dirty, increments use count
'
'----------------------------------------------------------------------------------------------------

VMWRCOM
 if_z   call  #BUSERR   ' page in missing, load vm page

        rdlong temp,pvmdata
        movd  wd4,vmpage
        movd  usec4,vmpage
wd4     or    0-0,VM_DIRTY ' mark page as dirty
usec4   add   0-0,whitcount wc
 if_c   call  #shr_hits ' divide all hit counts by two, for fairness, if any overflow
VMWRCOM_RET ret

'----------------------------------------------------------------------------------------------------
'
' VMWRITEB
'
' Write byte(s)
'
' 1) If page is present in the working set, write it to the working set
' 2) If present, or not, always write it to the backing store
'
' This is called a "write through" strategy, and reduces reading in case written value will not
' need to be read in the near future
'
'----------------------------------------------------------------------------------------------------

VMWRITEB call #VMWRCOM
        wrbyte temp,vmentry
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMWRITEW
'
' Write WORD
'
' 1) If page is present in the working set, write it to the working set
' 2) If present, or not, always write it to the backing store
'
' This is called a "write through" strategy, and reduces reading in case written value will not
' need to be read in the near future
'
'----------------------------------------------------------------------------------------------------

VMWRITEW call #VMWRCOM
        wrword temp,vmentry
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMWRITEL
'
' Write LONG
'
' 1) If page is present in the working set, write it to the working set
' 2) If present, or not, always write it to the backing store
'
' This is called a "write through" strategy, and reduces reading in case written value will not
' need to be read in the near future
'
'----------------------------------------------------------------------------------------------------

VMWRITEL call #VMWRCOM
        wrlong temp,vmentry
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMLOCK - finish later, two args - vmaddr & numpages - lock consecutive numpages
'
'----------------------------------------------------------------------------------------------------

vmlock  movd  setl,vmpage
        nop
setl    or    0-0,VM_LOCK
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' VMLOCK - finish later, two args - vmaddr & numpages - lock consecutive numpages
'
'----------------------------------------------------------------------------------------------------

vmunlock movd  clrl,vmpage
        nop
clrl    or    0-0,VM_LOCK
        jmp   #waitcmd

'----------------------------------------------------------------------------------------------------
'
' READ_STATUS - return status register of specified ram chip
'
'----------------------------------------------------------------------------------------------------

{
read_stat shr  vminst,#9
        movd  outa,vminst ' select ram chip
        mov  addr,readstat
        mov  bits,#8
        andn outa,#CS|CLK
        call #send

        mov   data,#0
        mov   bits,#8

gr      or    outa,#CLK
        test  miso_mask,ina wc
        rcl   data,#1
        andn  outa,#CLK
        djnz  bits,#gr

        or   outa,#CS
        wrlong data,pvmdata
        jmp   #waitcmd
}
'----------------------------------------------------------------------------------------------------
'
' BSTART
'
' Assert /CS for the device specified by "vmaddr", initially 2 pins are used to select RAM0 or RAM1
' (the MCP23K256's are 32KB devices), later can choose between different XMM's on same prop :)
'
'----------------------------------------------------------------------------------------------------

BSTART
#ifdef EXTRAM
#ifdef HOMEBREW
' if using two chip selects for two chips, uncomment this block
        ror  vmaddr, #16 nr, wz
 if_z   andn outa, CS0_PIN
 if_nz  andn outa, CS1_PIN
#endif
#ifdef PROPCADE
        ' PropCade device selection logic
        mov   addr, vmaddr
        and   addr, addr_mask   ' highest addr = $1FFFF
        mov   dv, addr
        shr   dv,#15            ' dv = 0..3, ie the chip select
        movd  outa,dv           ' select SPI device
        ' prepare address for SPI chips
        and   addr, offs_mask   ' limit offset to $7FFF (32k-1)
        andn  addr, #511        ' clear low bits
        shl   addr,#8           ' move it into position for transmission
        or    addr, fn

        mov   ptr,hubaddr       ' hubaddr = hub page address
        and   ptr,dmask

        mov   bits,#24
        andn  outa,#CS|CLK      ' drop /CS, drop CLK

        call  #send

        mov   count,#128
        ' end of PropCade code
fn      long  0
#endif
#ifdef FLEXMEM
        'End of FlexMem code
#endif
#ifdef MORPHEUS_SPI
        'End of Morpheus SPI code
#endif
#ifdef MORPHEUS_XMM
        'End of Morpheus XMM code
#endif
#ifdef MORPHEUS3_SPI
        'End of Morpheus SPI code
#endif
#ifdef PLCG
        'End of PLCG code
#endif
#ifdef DRACBLADE
        'End of DracBlade code
#endif
#ifdef TRIBLADE_2
        mov   addr, vmaddr        'addr = external RAM address
        and   addr, addr_mask     'Highest addr = $1FFFF
        andn  addr, #$1FF         'Clear low bits
        mov   ptr, hubaddr        'ptr = hub page address
        and   ptr, dmask
        mov   count,dstinc        '512 bytes to read/write
	'End of TriBlade #2 code
#endif
#ifdef RAMBLADE
        'End of RamBlade code
#endif
#ifdef HYDRA
        mov   addr, vmaddr      ' addr = external RAM address
        'and   addr, addr_mask  ' highest addr = $1FFFF
        'andn  addr, #$1FF      ' clear low bits
        and   addr, haddr_mask  ' merged in one single and and reduced mask to 64K total ($FE00)
        shl   addr, #8          ' shift in place
        or    outa, ctrl_mask   ' CTRL %XX->%11 = ADDR_HI
        andn  outa, data_mask   ' make hole for byte
        or    outa, addr        ' put byte on bus
        or    dira, data_mask   ' DBUS as output
        or    outa, clock_mask  ' stobe clock
        andn  outa, clock_mask  ' release clock
        mov   ptr,  hubaddr     ' ptr = hub page address
        and   ptr,  dmask       ' ???
        mov   count,dstinc      ' 512 bytes to read/write
        'End of Hydra code
#endif
#endif
BSTART_RET    ret


'----------------------------------------------------------------------------------------------------
'
' BREAD
'
' read 512 bytes from virtual (extended) address "vmaddr" to the hub
' starting at address "hubaddr"
'
'----------------------------------------------------------------------------------------------------

BREAD
#ifdef EXTRAM
#ifdef PROPCADE
        mov   fn,read
        call #BSTART

pr      mov   bits,#32

gloop   or    outa,#CLK
        test  miso_mask,ina wc
        rcl   data,#1
        andn  outa,#CLK
        djnz  bits,#gloop

        wrlong data,ptr
        add  ptr,#4
        djnz  count,#pr

        or    outa,#CS
#endif
#ifdef FLEXMEM
        'End of FlexMem code
#endif
#ifdef MORPHEUS_SPI
        'End of Morpheus SPI code
#endif
#ifdef MORPHEUS_XMM
        'End of Morpheus XMM code
#endif
#ifdef MORPHEUS3_SPI
        'End of Morpheus SPI code
#endif
#ifdef PLCG
        'End of PLCG code
#endif
#ifdef DRACBLADE
        'End of DracBlade code
#endif
#ifdef TRIBLADE_2
        call   #BSTART

        'From tbp2 driver init
        mov    outa, ram_enable_lo  'Put latch back to SRAM=U23
        mov    dira, ram_dir_read   'Enable bits (for init & wrblock)
        xor    outa, ram_LE_bit     'LE= 1 -> 0 (latch)
        mov    dira, ram_dir_read   'Setup direction to read

        shl    addr,#8              'Shift address 8 bits
        or     addr, ram_read       'Add -WE=1 -OE=0
:bloop  mov    outa, addr           'pc (shifted 8 bits + -WE=1 -OE=0)
        add    addr, #$100          'Next byte in ext RAM AND delay for sram access time
        mov    data, ina            'Read SRAM
        wrbyte data,ptr             'Save the byte
        add    ptr, #1              'Next byte in HUB
        djnz   count,#:bloop        'Block done?

        mov   dira, #0              'Tristate all
        'End of TriBlade #2 code
#endif
#ifdef RAMBLADE
        'End of RamBlade code
#endif
#ifdef XEDODRAM
' get 256*2 bytes
        mov    xcmd,vmaddr       ' get physical address
        and    xcmd,XDRAM_amsk
        or     xcmd,XDRAM_rbuf   ' set readbuf command
        mov    count,#2
        mov    ptr,hubaddr       ' set hub pointer
        and    ptr,dmask
:next
        wrlong ptr,XDRAM_dat     ' send new hub pointer
        wrlong xcmd,XDRAM_cmd    ' send command
        rdlong temp,XDRAM_cmd wz
 if_nz  jmp    #$-1              ' wait for command complete
        add    ptr,#256          ' incr pointers
        add    xcmd,#256
        djnz   count,#:next      ' do next 256
#endif
' endif XEDORAM
#ifdef HYDRA
        call  #BSTART
        andn  dira, data_mask   ' DBUS as input
        andn  outa, ctl1_mask   ' CTRL %11->%01 READ
:bloop  ' HX512 read memory byte with implicit address increment
        or    outa, clock_mask  ' strobe clock
#ifdef HYDRA_SAFE
        nop                     ' added for safety, may be removed if card has faster SRAM (10ns)
#endif
        mov   data, ina         ' read SRAM
        andn  outa, clock_mask  ' release clock
        shr   data, #16         ' shift in place
        'and   data, #$FF        ' extract 8 bits - NOT NEEDED - wrbyte will chop it anyway
        wrbyte data, ptr        ' save the byte
        add   ptr,  #1          ' next byte in HUB
        'add   addr, #1          ' next byte in ext RAM - NOT NEEDED - SRAM will auto-increment
        djnz  count, #:bloop    ' block done?
        add   addr, dstinc      ' add the whole page at once
        'End of Hydra code
#endif
#endif
BREAD_RET     ret

'----------------------------------------------------------------------------------------------------
'
' BWRITE
'
' write 512 bytes to virtual (extended) address "vmaddr" from the hub
' starting at address "hubaddr"
'
'----------------------------------------------------------------------------------------------------

BWRITE
#ifdef EXTRAM
#ifdef PROPCADE
        mov   fn,write
        call #BSTART

pw      rdlong addr,ptr
        mov   bits,#32
        call  #send
        add   ptr,#4
        djnz  count,#pw

        or    outa,#CS
#endif
#ifdef FLEXMEM
        'End of FlexMem code
#endif
#ifdef MORPHEUS_SPI
        'End of Morpheus SPI code
#endif
#ifdef MORPHEUS_XMM
        'End of Morpheus XMM code
#endif
#ifdef MORPHEUS3_SPI
        'End of Morheus3 SPI code
#endif
#ifdef PLCG
        'End of PLCG code
#endif
#ifdef DRACBLADE
        'End of DracBlade code
#endif
#ifdef TRIBLADE_2
        call   #BSTART

        mov     outa, ram_enable_lo  'Put latch back to SRAM=U23
        mov     dira, ram_dir_read   'Enable bits (for init & wrblock)
        xor     outa, ram_LE_bit     'LE= 1 -> 0 (latch)

        shl    addr, #8              'Shift address 8 bits
        or     addr, ram_write       'Add -WE=0 -OE=1
        mov    dira, ram_dir_write   'Set data bits to outputs

:bloop  rdbyte data, ptr             'Get a byte from HUB
        andn   addr, #$FF
        or     addr, data            'Add data bits to be written
        mov    outa, addr            'Output -WE=0 -OE=1, address bits, data bits
        add    addr, #$100           'Next byte in ext RAM AND 25nS min delay
        or     outa, ram_latched     'Set -WE=0->1 -OE=1->1  (end write)
        add    ptr,#1                'Next byte in HUB
        djnz   count,#:bloop         'Block done?
        mov    dira, ram_dir_read    'Set data bits to inputs

        mov   dira, #0               'Tristate all
	'End of TriBlade #2 code
#endif
#ifdef RAMBLADE
        'End of RamBlade code
#endif
#ifdef XEDODRAM
' write 256*2 bytes
        mov    xcmd,vmaddr       ' get physical address
        and    xcmd,XDRAM_amsk
        or     xcmd,XDRAM_wbuf   ' set writebuf command
        mov    count,#2
        mov    ptr,hubaddr       ' set hub pointer
        and    ptr,dmask
:next
        wrlong ptr,XDRAM_dat     ' send new hub pointer
        wrlong xcmd,XDRAM_cmd    ' send command
        rdlong temp,XDRAM_cmd wz
 if_nz  jmp    #$-1              ' wait for command complete
        add    ptr,#256          ' incr pointers
        add    xcmd,#256
        djnz   count,#:next      ' do next 256
#endif
' endif XEDODRAM
#ifdef HYDRA
        call  #BSTART
        andn  outa, ctrl_mask   ' CTRL %11->%00 WRITE
:bloop  'HX512 write memory byte with implicit address increment
        rdbyte data, ptr        ' get a byte from HUB
        'and   data, #$FF        ' mask a single byte - NOT NEEDED - rdbyte will zero extend
        shl   data, #16         ' shift in place
        andn  outa, data_mask   ' make hole for data
        or    outa, data        ' put data on DBUS
#ifdef HYDRA_SAFE
        nop                     ' added for safety, may be removed if card has faster SRAM (10ns)
#endif
        or    outa, clock_mask  ' strobe clock
        andn  outa, clock_mask  ' release clock
        add   ptr,  #1          ' next byte in HUB
        'add   addr, #1         ' next byte in ext RAM - NOT NEEDED - SRAM will auto-increment
        djnz  count, #:bloop    ' block done?
        add   addr, dstinc      ' add the whole page at once
        andn  dira, data_mask   ' leave DBUS as high-Z
        'End of Hydra code
#endif
#endif
BWRITE_RET    ret


#ifdef EXTRAM
'----------------------------------------------------------------------------------------------------
' EXTERNAL SUPPORT ROUTINES FOR MEMORY PLATFORMS
'----------------------------------------------------------------------------------------------------

#ifdef PROPCADE

'----------------------------------------------------------------------------------------------------
' SPI send routine - shared by BREAD and BWRITE
'----------------------------------------------------------------------------------------------------

send    andn  outa,#CLK
        nop
        rol   addr, #1 wc
        muxc  outa,#MOSI
        or    outa,#CLK
        nop
        djnz  bits,#send
        andn  outa,#CLK|MOSI
send_ret ret
#endif
#endif

'----------------------------------------------------------------------------------------------------
' COG variables
'----------------------------------------------------------------------------------------------------

'----------------------------------------------------------------------------------------------------
'
' VMCOG mailbox variables
'
'----------------------------------------------------------------------------------------------------

pvmcmd    LONG 0                ' used to hold value of PAR, points at mailbox for VMCOG
pvmaddr   LONG 0                ' pointer to hub location containing vmaddr
pvmdata   LONG 0                ' pointer to hub location containing byte/word/long to be read/written

vmaddr    LONG 0                ' virtual address, initially $0000-$FFFF, later 24 bit address space
vmdata    LONG 0                ' byte/word/long data read or to be written
vmpage    LONG 0

'----------------------------------------------------------------------------------------------------
'
' General VMCOG variables
'
'----------------------------------------------------------------------------------------------------

count     LONG 0                ' used for DJNZ
temp      LONG 0                ' generic temporary variable

'----------------------------------------------------------------------------------------------------
'
' Generic Backing Store support variables
'
'----------------------------------------------------------------------------------------------------

lastpage  LONG $7C00            ' last page in the hub to be used for the working set
numpages  LONG 0                ' total number of pages in the working set

hubaddr   LONG 0                ' hub memory address to read from or write to

totreads  LONG 0,0              ' 64 bit total reads counter
totwrites LONG 0,0              ' 64 bit total writes counter
pagereads LONG 0                ' total number of page reads
pagewrites LONG 0               ' total number of page writes

VM_DIRTY  LONG %0000_0000_0000_0000_0000_0010_0000_0000
VM_LOCK   LONG %0000_0000_0000_0000_0000_0100_0000_0000
VM_FLAGS  LONG %0000_0000_0000_0000_0000_0110_0000_0000

'----------------------------------------------------------------------------------------------------
'
' Support variables
'
'----------------------------------------------------------------------------------------------------

addr      long  0               ' byte address, between 0 and $7FFF
data      long  0
ptr       long  0

addr_mask long $1FFFF

'----------------------------------------------------------------------------------------------------
'
' Platform-Specific support variables
'
'----------------------------------------------------------------------------------------------------

#ifdef EXTRAM
#ifdef PROPCADE
dv        long  0               ' device address, between 0 and 7, however 6&7 are not valid
bits      long  0

read      long  $03000000       ' read command
write     long  $02000000       ' write command
ramseq    long  $01400000       ' %00000001_01000000 << 16 ' set sequetial mode
readstat  long  $05000000       ' read status

pagesiz   long 128              ' in longs

spidir    long  %1110_00000111

pdata     long  0

offs_mask long $7FFF
miso_mask long MISO             ' required due to ina not being usable in destination field
#endif
#ifdef TRIBLADE_2
'
'
'                                          +-------- -CS (U26 FLASH)   -+
'                                          |+------- -CS (J22 microSD)  +- Latched pins...
'                                          ||+------ -RST3              |    passes when LE=1
'             (P30) SO ---+                |||+----- -RST1              |    latches on  LE low going edge
'             (P31) SI --+|                ||||+---- -CE (U24 SRAM-hi)  |    latched on  LE=0
'                        ||+---- -WE(SDA)  |||||+--- -CE (U23 SRAM-lo)  |
'                        |||+--- -OE(SCL)  ||||||+-- A20                |
'                        ||||+--  LE       |||||||+- A19               -+
'                        |||||             ||||||||
'          P31...P0 --> %00xx0_xxxxxxxxxxx_xxxxxxxx_xxxxxxxx
ram_LE_bit      long    %00001_00000000000_00000000_00000000 ' LE bit
ram_latched     long    %00110_00000000000_00000000_00000000 ' -WE=1 -OE=1 (LE=0)
ram_read        long    %00100_00000000000_00000000_00000000 ' -WE=1 -OE=0 (LE=0)
ram_write       long    %00010_00000000000_00000000_00000000 ' -WE=0 -OE=1 (LE=0)
ram_enable_lo   long    %00111_00000000000_11111000_00000000 ' -WE=1 -OE=1 LE=1 + -CE(U23 SRAM) = 0 (A19-20=0)
ram_dir_read    long    %00111_11111111111_11111111_00000000 ' outputs WE, OE, A0-18, inputs D0-7
ram_dir_write   long    %00111_11111111111_11111111_11111111 ' outputs WE, OE, A0-18, D0-7
'SD card pins           %00010_00000000000_01000011_00000000
'ram_dir_input   long    %00000_00000000000_00000000_00000000 ' all inputs (for Ram Disk access)
'                              +---------+ +------+ +------+
'                              A18......A8 A7....A0 D7....D0
'                                          Q7....Q0
	'End of TriBlade #2 data
#endif
#ifdef XEDODRAM
xcmd       long 0
XDRAM_cmd  long 0
XDRAM_dat  long 0
XDRAM_rbuf long $7ff0_0000      ' always read  256 at a time ... loops 2 times
XDRAM_wbuf long $8ff0_0000      ' always write 256 at a time ... loops 2 times
XDRAM_amsk long $000f_ffff      ' address mask up to 1MB
#endif
' endif XEDODRAM
#ifdef HYDRA
ctl0_mask     long      %00000000_00000000_00000000_00000010
ctl1_mask     long      %00000000_00000000_00000000_00000100
ctrl_mask     long      %00000000_00000000_00000000_00000110
data_mask     long      %00000000_11111111_00000000_00000000
clock_mask    long      %01000000_00000000_00000000_00000000
reset_mask    long      %00100000_00000000_00000000_00000000
dira_mask     long      %01100000_11111111_00000000_00000110
haddr_mask    long      %00000000_00000000_11111110_00000000
#endif
#endif

'----------------------------------------------------------------------------------------------------
' Generic helpful constants too big to be embedded into instructions
'----------------------------------------------------------------------------------------------------

dstinc    LONG 512

          FIT 496               ' out of 496

