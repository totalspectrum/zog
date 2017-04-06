'----------------------------------------------------------------------------------------------------
'
' VMDEBUG v0.961+Triblade_0.7 - virtual memory server debugger for the Propeller
'
' Copyright February 5, 2010 by William Henning
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
' Feb5  v0.10 - initial upload
' Feb5  v0.20 - VMDUMP almost works, SPI RAM issues still present
' Feb9  v0.21 - VMDUMP works, debugger renamed
' Feb9  v0.22 - added "Show VM Page", local echo & backspace to getstr based SerialPlus routines
' Feb9  v0.23 - commands 1,2,3,6,9 now work :) ... except for backing store read/write, that will come after 4,5,7,8
' Feb9  v0.24 - fixed page access count increment
' Feb10 v0.25 - added aligned VMREADW and VMREADL
' Feb11 v0.32 - with a slimmed down VMCOG :-)
' Mar10 v0.51 - modified for 512 byte pages and new TLB data structure
' Apr16 v0.85 - now uses 0.85 VMCOG
' Apr17 v0.87 - added VMADDR display to TBL dump, synced latest VMCOG
' Jun3  v0.89 - fixed dirty bit display, fixed wrword/wrlong busted by .85 changes
' Jun4  v0.92 - flushes dirty pages, handles TLB correctly - appears to work! NEEDS THOROUGH TESTING
' Jun5  v0.940 - integrated TriBlade_2 support from Michael Rychlik (aka heater)
' Jun5  v0.942 - improved show_page, added Fill test pattern command
' Jun6  v0.943 - added heater's test, and made a new version of it as 'h' command
' Jun6  v0.950 - improved full TLB display, added dumb benchmark
' Jun6  v0.950 - simplified TLB display, added more complex fill test
' Jun7  v0.950+Triblade_0.3 - Heater - took into use the TriBladeProp RAM driver which makes everything work on TriBlade now.
'                             N.B. This is only a temporary fix as we don't want to waste a COG on it.
'                             Added a more sophisticated RAM test "heater3"
' Jun7  v0.950+Triblade_0.4 - Heater - Fixed TriBladeProp latch loading on start up,
'                             no longer requires the TriBladeProp Driver.
' Jun7  v0.950+Triblade_0.5 - Heater - No changes here just bumped version..
' Jun7  v0.951 - integrated heater 3 into main tree
' Jun8  v0.961+Triblade_0.8 - No change here just bumped version in line with vmcog.
' Jun8  v0.961+TriBlade_0.9 - Heater - Added coutup, coutdown and random fill/read tests to heater3 test.
' Jun9  v0.961+TriBlade_0.10 - Replaced lfsr object with Spin ? operator (Duh!).
' Jun11 v0.961+TriBlade_0.11 - Heater - No change, just bumped version inline with vmcog.
'----------------------------------------------------------------------------------------------------

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

CON

'----------------------------------------------------------------------------------------------------
' Constants defined for VMDEBUG use
'----------------------------------------------------------------------------------------------------

'  _clkmode          = xtal1 + pll16x
'  _xinfreq          = 5_000_000
'   baud             = 38400

  _clkmode          = xtal1 + pll16x
  _xinfreq          = 6_553_600
   baud             = 115_200

'----------------------------------------------------------------------------------------------------
' Objects needed for VMCOG_DEBUGGER
'----------------------------------------------------------------------------------------------------

OBJ

  ser    : "FullDuplexSerialPlus"   ' so we can talk to a terminal program over SerPlug
  vm     : "vmcog_v0_976"           ' the MMU/virtual memory manager cog

VAR

  long command, addr, val

'----------------------------------------------------------------------------------------------------
' Variables defined for VMDEBUG use
'----------------------------------------------------------------------------------------------------

  long mailbox[4]

  long tlb[256]

  byte testbyte

'----------------------------------------------------------------------------------------------------
' Main
'
'----------------------------------------------------------------------------------------------------

PUB main | ch

  ser.start(31,30,0,baud)
  vm.start(@mailbox,$7C00, 10) ' start up VMCOG
  waitcnt(cnt+80_000_000) ' give serial terminal window time

  repeat
    show(@menu)
    ch := ser.rx
    ser.tx(13)
    case ch
      "1":
        vm_show_tlb
      "2":
        vm.flush
      "3":
        vm_read_byte
      "4":
        vm_read_word
      "5":
        vm_read_long
      "6":
        vm_write_byte
      "7":
        vm_write_word
      "8":
        vm_write_long
      "9":
        vm_show_page(get_addr)
      "0":
        vm_fill
      "h":
        heater3
      "b":
        benchmark
      "f":
        fill2
      other:
        ser.str(@unknown)


PUB heater3 | ad, e, b, s, rnd, seed
repeat
  ser.str(string("Testing $"))
  ser.hex(vm#MEMSIZE, 8)
  ser.str(string(" bytes",13))

  ser.str(string("Up count fill...",13))
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    vm.wrvbyte(ad, ad)

  ser.str(string("Up count check...",13))
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    b := vm.rdvbyte(ad)
    if b <> (ad &  $FF)
      ser.str(string("Error at "))
      ser.hex(ad, 4)
      ser.str(string(" expected "))
      ser.hex(ad, 2)
      ser.str(string(" got "))
      ser.hex(b, 2)
      ser.tx(13)
      ser.tx(10)
      return

  ser.str(string("Down count fill...",13))
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    vm.wrvbyte(ad, -ad)

  ser.str(string("Down count check...",13))
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    b := vm.rdvbyte(ad)
    if b <> (-ad &  $FF)
      ser.str(string("Error at "))
      ser.hex(ad, 4)
      ser.str(string(" expected "))
      ser.hex(-ad, 2)
      ser.str(string(" got "))
      ser.hex(b, 2)
      ser.tx(13)
      ser.tx(10)
      return

  ser.str(string("Random fill...",13))
  seed := ?cnt
  'seed := $12345678

  rnd := seed
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    vm.wrvbyte(ad, ?rnd)

  ser.str(string("Random check...",13))
  rnd := seed
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    b := vm.rdvbyte(ad)
    if b <> ((?rnd) & $FF)
      ser.str(string("Error at "))
      ser.hex(ad, 4)
      ser.str(string(" expected "))
      ser.hex(rnd, 2)
      ser.str(string(" got "))
      ser.hex(b, 2)
      ser.tx(13)
      ser.tx(10)
      return

  ser.str(string("Zero fill...",13))
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    vm.wrvbyte(ad, 0)

  'Force an error
'  vm.wrvbyte($13, 2)

  e := checkandwrite($00, $FF)
  if e <> 0
    return
  e := checkandwrite($FF, $AA)
  if e <> 0
    return
  e := checkandwrite($AA, $55)
  if e <> 0
    return
  e := checkandwrite($55, $00)
  if e == 0
    ser.str(string("OK",13))


PRI checkandwrite(oldbyte, newbyte) | ad, b
  ser.str(string("Checking "))
  ser.hex(oldbyte, 2)
  ser.str(string(" writing "))
  ser.hex(newbyte, 2)
  ser.str(string("..."))
  ser.tx(13)
  ser.tx(10)
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
    b := vm.rdvbyte(ad)
    if b <> oldbyte
      ser.str(string("Error at "))
      ser.hex(ad, 4)
      ser.str(string(" expected "))
      ser.hex(oldbyte, 2)
      ser.str(string(" got "))
      ser.hex(b, 2)
      ser.tx(13)
      ser.tx(10)
      return 1
    vm.wrvbyte(ad, newbyte)
  return 0


PRI dump | ad, b, i, e
  ser.str(string("Reading memory...",13))
  i := 0
  e := 0
  repeat ad from 0 to vm#MEMSIZE - 1 step 1
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


PUB fill2|ad, i, b,e,t1,t2,w
  ser.str(string("Filling memory...",13))
  t1:=cnt
  repeat ad from 0 to (1024 * 64) - 1 step 2
    w:=((ad>>9)<<8)+((ad>>1)&255)
    vm.wrvword(ad,w)

  t2:=cnt
  ser.str(string("Reading memory...",13))
  i := 0
  e := 0
  repeat ad from 0 to (1024 * 64) - 1 step 2
    if (i & 15) == 0
      ser.hex(ad,4)
      ser.tx(":")
      ser.tx(" ")
    b:=vm.rdvword(ad)
    if b<>(((ad>>9)<<8)+((ad>>1)&255))
      ser.str(string("Error:",13))
      e++
    ser.hex(b,4)
    ser.tx($20)
    i := i + 1
    if i == 16
      ser.tx(13)
      ser.tx(10)
      i := 0
      if e > 255
        return ' stop after 256 errors

PUB benchmark|ad, i, b,e,t1,t2

  '----- fake

  ser.str(string("Approx. Spin overhead for Filling memory..."))
  t1:=cnt
  repeat ad from 0 to 65535
    vm.wrfbyte(ad, ad)
  t2:=cnt

  ser.dec((t2-t1)/(80_000))
  ser.str(string(" ms.",13))

  ser.str(string("Approx. Spin overhead for Reading memory..."))
  t1:=cnt
  repeat ad from 0 to 65535
    b:=vm.rdfbyte(ad)
  t2:=cnt

  ser.dec((t2-t1)/(80_000))
  ser.str(string(" ms.",13,13))

  '----- real

  ser.str(string("Filling memory..."))
  t1:=cnt
  repeat ad from 0 to 65535
    vm.wrvbyte(ad, ad)
  t2:=cnt

  ser.str(string("Spin Fill took "))
  ser.dec((t2-t1)/(80_000))
  ser.str(string(" ms.",13))

  ser.str(string("Reading memory..."))
  t1:=cnt
  repeat ad from 0 to 65535
    b:=vm.rdvbyte(ad)
  t2:=cnt

  ser.str(string("Spin Read took "))
  ser.dec((t2-t1)/(80_000))
  ser.str(string(" ms.",13))

PUB vm_fill|page,ix
  repeat page from 0 to 15
    repeat ix from 0 to 511
      vm.wrvbyte(page*512+ix,ix) 'page+$A0)
    vm.wrvbyte(page*512,page+$A0)

PUB vm_read_byte|ad
  ad := get_addr
  ser.str(string("Read Byte $"))
  ser.hex(vm.rdvbyte(ad),2)
  ser.tx(13)

PUB vm_read_word|ad
  ad := get_addr
  ser.str(string("Read Word $"))
  ser.hex(vm.rdvword(ad),4)
  ser.tx(13)

PUB vm_read_long|ad
  ad := get_addr
  ser.str(string("Read Long $"))
  ser.hex(vm.rdvlong(ad),8)
  ser.tx(13)

PUB vm_write_byte|ad,v
  ad := get_addr
  v := get_value
  vm.wrvbyte(ad,v)
  ser.str(string("Wrote Byte $"))
  ser.hex(v,2)
  ser.str(string("to virtual address $"))
  ser.hex(ad,8)
  ser.tx(13)

PUB vm_write_word|ad,v
  ad := get_addr
  v := get_value
  vm.wrvword(ad,v)
  ser.str(string("Wrote Word $"))
  ser.hex(v,4)
  ser.str(string("to virtual address $"))
  ser.hex(ad,8)
  ser.tx(13)

PUB vm_write_long|ad,v
  ad := get_addr
  v := get_value
  vm.wrvlong(ad,v)
  ser.str(string("Wrote Long $"))
  ser.hex(v,8)
  ser.str(string("to virtual address $"))
  ser.hex(ad,8)
  ser.tx(13)


PUB vm_show_tlb|i,hub
  ser.str(string("TLB Contents:",13,13))
  vm.Look(@tlb)
  ser.str(string("    Acesses L D  Hub  (Raw Data) VM_ADDR",13))
  ser.str(string("    ------- - - ----- ---------- -------",13))
  repeat i from 0 to 127
    if tlb[i]
      ser.hex(i,2)
      ser.str(string(": $"))
      ser.hex(tlb[i]>>11,6)
      if (tlb[i]& %10_000000000)
        ser.str(string(" L"))
      else
        ser.str(string(" -"))
      if (tlb[i]& %1_000000000)
        ser.str(string(" D $"))
      else
        ser.str(string(" - $"))
      hub := (tlb[i] & $1FF)<<9
      ser.hex(hub,4)
      ser.tx(" ")
      ser.tx("(")
      ser.hex(tlb[i],8)
      ser.str(string(") $"))
      ser.hex(i<<9,6)
      ser.tx(13)

{
PUB vm_show_tlb|i,j,hub
  ser.str(string("TLB Contents:",13,13))
  vm.Look(@tlb)
  ser.str(string("    Acesses L D  Hub  VM_ADDR         Acesses L D  Hub  VM_ADDR",13))
  ser.str(string("    ------- - - ----- -------         ------- - - ----- -------",13))
  repeat i from 0 to 63
    ser.hex(i,2)
    ser.str(string(": $"))
    ser.hex(tlb[i]>>11,6)
    if (tlb[i]& %10_000000000)
      ser.str(string(" L"))
    else
      ser.str(string(" -"))
    if (tlb[i]& %1_000000000)
      ser.str(string(" D $"))
    else
      ser.str(string(" - $"))
    hub := (tlb[i] & $1FF)<<9
    if hub
      ser.hex(hub,4)
    else
      ser.str(string("----"))
    ser.str(string(" $"))
    ser.hex(i<<9,6)
    ' right hand side column
    j:=i+64
    ser.str(string("     "))
    ser.hex(j,2)
    ser.str(string(": $"))
    ser.hex(tlb[j]>>11,6)
    if (tlb[j]& %10_000000000)
      ser.str(string(" L"))
    else
      ser.str(string(" -"))
    if (tlb[j]& %1_000000000)
      ser.str(string(" D $"))
    else
      ser.str(string(" - $"))
    hub := (tlb[j] & $1FF)<<9
    if hub
      ser.hex(hub,4)
    else
      ser.str(string("----"))
    ser.str(string(" $"))
    ser.hex(j<<9,6)
    ser.tx(13)
}

PUB vm_show_page(va)|p,i,hub,pa
  i:=vm.rdvbyte(va) ' force the page in
  pa := vm.GetPhysVirt(va)
  if pa == 0
    ser.str(string("Sorry, Virtual Address $"))
    ser.hex(va,8)
    ser.str(string(" is not resident in the working set.",13))
    return
  ser.str(string("     Virtual Addr: $"))
  ser.hex(va,8)
  ser.str(string("    Physical Addr: $"))
  ser.hex(pa,4)
  ser.str(string(13,13,"     "))
  repeat i from 0 to 15
    ser.hex(i,2)
    ser.tx(" ")
  ser.tx(13)
  ser.str(string("     "))
  repeat i from 0 to 15
    ser.str(string("-- "))
  ser.tx(13)
  repeat p from 0 to 496 step 16
    ser.hex(p,3)
    ser.str(string(": "))
    repeat i from 0 to 15
      ser.hex(byte[pa+p+i],2)
'      ser.hex(vm.rdvbyte(va+p+i),2)
      ser.tx(" ")
    ser.tx(13)

PUB get_addr|t
  ser.str(string(13,"Enter HEX Address: "))
  t:= ser.GetHex
  ser.tx(13)
  return t

PUB get_value|t
  ser.str(string(13,"Enter HEX Value: "))
  t:= ser.GetHex
  ser.tx(13)
  return t


pub show(ptr)
  repeat until (byte[ptr]==0)
    ser.str(ptr)
    ser.tx(13)
    ptr+=strsize(ptr)+1

DAT

menu    byte 13,"VMDebug Main Menu",13,0
        byte "1) View Page Table",0
        byte "2) Flush Page Table",0
        byte "3) VMREADB",0
        byte "4) VMREADW",0
        byte "5) VMREADL",0
        byte "6) VMWRITEB",0
        byte "7) VMWRITEW",0
        byte "8) VMWRITEL",0
        byte "9) Show VM Page",0
        byte "0) Fill test pattern",0
        byte "h) Heater's test",0
        byte "b) Benchmark (slow, in spin)",0
        byte "f) Fill memory with Word pattern",0
        byte 13,"READY>",0,0

unknown byte "ERROR: Unknown Command",0
