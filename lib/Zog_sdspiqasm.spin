'   sdspi:  SPI interface to a Secure Digital card.
'
'   Modified for Zog by Michael Rychlik to allow operation with
'   external memory on the TriBladeProp and RamBlade.
'
'   Modified for Catalina by Ross Higson (to allow the use of
'   non-consecutive pins, which is required on the TriBladeProp).
'
'   Copyright 2008   Radical Eye Software
'
'   See end of file for terms of use.
'
'   You probably never want to call this; you want to use fsrw
'   instead (which calls this); this is only the lowest layer.
'
'   Assumes SD card is interfaced using four consecutive Propeller
'   pins, as follows (assuming the base pin is pin 0):
'                3.3v
'                   
'                    20k
'   p0 ────────┻─┼─┼─┼─┼─┼────── do
'   p1 ──────────┻─┼─┼─┼─┼────── clk
'   p2 ────────────┻─┼─┼─┼────── di
'   p3 ──────────────┻─┼─┼────── cs (dat3)
'         150          └─┼────── irq (dat1)
'                        └────── p9 (dat2)
'
'   The 20k resistors
'   are pullups, and should be there on all six lines (even
'   the ones we don't drive).
'
'   This code is not general-purpose SPI code; it's very specific
'   to reading SD cards, although it can be used as an example.
'
'   The code does not use CRC at the moment (this is the default).
'   With some additional effort we can probe the card to see if it
'   supports CRC, and if so, turn it on.   
'
'   All operations are guarded by a watchdog timer, just in case
'   no card is plugged in or something else is wrong.  If an
'   operation does not complete in one second it is aborted.
'

#define TRIBLADEPROP
'#define RAMBLADE

con
   sectorsize = 512
   sectorshift = 9

   SD_Init   = 1
   SD_Read   = 2
   SD_Write  = 3
   SD_ByteIO = 4
   SD_StopIO = 5

var
   long cog
   long command, param, blockno ' rendezvous between spin and assembly
pub stop
   if cog
      cogstop(cog~ - 1)      
pub start(do_pin, clk_pin, di_pin, cs_pin)
'
'   Initialize the card!  Send a whole bunch of
'   clocks (in case the previous program crashed
'   in the middle of a read command or something),
'   then a reset command, and then wait until the
'   card goes idle.
'

   param  := 0
   do := do_pin
   clk := clk_pin 
   di := di_pin
   cs := cs_pin
   stop
   command := SD_Init
   cog := 1 + cognew(@entry, @command)
   repeat while command
   if param
      abort param
   return 0
pub readblock(n, b)
'
'   Read a single block.  The "n" passed in is the
'   block number (blocks are 512 bytes); the b passed
'   in is the address of 512 blocks to fill with the
'   data.
'                              \
   param := b
   blockno := n
   command := SD_Read
   repeat while command
   if param
      abort param
   return 0
pub writeblock(n, b)
'
'   Write a single block.  Mirrors the read above.
'
   param := b
   blockno := n
   command := SD_Write
   repeat while command
   if param
      abort param
   return 0
dat
        org 0
entry   mov comptr,par
        mov parptr,par
        add parptr,#4
        mov parptr2,parptr
        add parptr2,#4

#ifdef TRIBLADEPROP
#else
        call    #SD_activate
#endif

        neg phsb,#1
        mov frqb,#0
        mov acca,nco
        add acca,clk
        mov ctra,acca
        mov acca,nco
        add acca,di
        mov ctrb,acca
        mov ctr2,onek
oneloop
        call #sendiohi
        djnz ctr2,#oneloop
        mov starttime,cnt
        mov cmdo,#0
        mov cmdp,#0
        call #cmd
        or outa,csmask
        call #sendiohi
initloop
        mov cmdo,#55
        call #cmd
        mov cmdo,#41
        call #cmd
        or outa,csmask
        cmp accb,#1 wz
   if_z jmp #initloop
        wrlong accb,parptr
' reset frqa and the clock
finished
#ifdef TRIBLADEPROP
        mov frqa,#0
        or outa,csmask
        neg phsb,#1
        call #sendiohi
        call #SD_tristate
        mov acca,#511                           ' on the TriBlade, we must not let the
        add acca,cnt                            ' kernel resume until we have paused
        waitcnt acca,#0                         ' for a short period after disabling the
        mov acca,#0                             ' SD card, or the XMM access goes haywire.
        wrlong acca,comptr                      ' (this should do no harm on other cards).
        jmp #waitloop
#elseifdef RAMBLADE
        mov frqa,#0
        or outa,csmask
        neg phsb,#1
        call #sendiohi
        call #SD_tristate
        mov acca,#511                           ' on the RamBlade, we must not let the
        add acca,cnt                            ' kernel resume until we have paused
        waitcnt acca,#0                         ' for a short period after disabling the
        mov acca,#0                             ' SD card, or the XMM access goes haywire.
        wrlong acca,comptr                     ' (this should do no harm on other cards).
        jmp #waitloop
#else
        mov frqa,#0
        wrlong frqa,comptr
        or outa,csmask
        neg phsb,#1
        call #sendiohi
#endif

pause
        mov acca,#511
        add acca,cnt
        waitcnt acca,#0

waitloop
        mov starttime,cnt
        rdlong acca,comptr wz
   if_z jmp #pause

        cmp acca,#SD_ByteIO wz
   if_z jmp #byteio
        mov ctr2,sector
        cmp acca,#SD_Read wz
   if_z jmp #rblock
        cmp acca,#SD_Write wz
#ifdef TRIBLADEPROP
   if_z jmp #wblock
        cmp acca,#SD_StopIO wz
  if_nz jmp #pause
        jmp  #finished
#elseifdef RAMBLADE
   if_z jmp #wblock
        cmp acca,#SD_StopIO wz
  if_nz jmp #pause
        jmp  #finished
#else
  if_nz jmp #pause
#endif

wblock
#ifdef TRIBLADEPROP
        call #SD_activate
#elseifdef RAMBLADE
        call #SD_activate
#endif
        mov starttime,cnt
        mov cmdo,#24
        rdlong cmdp,parptr2
        call #cmd
        mov phsb,#$fe
        call #sendio
        rdlong accb,parptr
        neg frqa,#1
wbyte
        rdbyte phsb,accb
        shl phsb,#23
        add accb,#1
        mov ctr,#8
wbit    mov phsa,#8
        shl phsb,#1
        djnz ctr,#wbit
        djnz ctr2,#wbyte        
        neg phsb,#1
        call #sendiohi
        call #sendiohi
        call #readresp
        and accb,#$1f
        sub accb,#5
        wrlong accb,parptr
        call #busy
        jmp #finished
rblock
#ifdef TRIBLADEPROP
        call #SD_activate
#elseifdef RAMBLADE
        call #SD_activate
#endif
        mov starttime,cnt
        mov cmdo,#17
        rdlong cmdp,parptr2
        call #cmd
        call #readresp
        rdlong accb,parptr
        sub accb,#1
rbyte
        mov phsa,hifreq
        mov frqa,freq
        add accb,#1
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        mov frqa,#0
        test domask,ina wc
        addx acca,acca
        wrbyte acca,accb
        djnz ctr2,#rbyte        
        mov frqa,#0
        neg phsb,#1
        call #sendiohi
        call #sendiohi
        or outa,csmask
        wrlong ctr2,parptr
        jmp #finished
byteio     
#ifdef TRIBLADEPROP
        call #SD_activate
#elseifdef RAMBLADE
        call #SD_activate
#endif
        rdlong phsb,parptr
        call #sendio
        wrlong accb,parptr
        jmp #finished
sendio
        rol phsb,#24
sendiohi
        mov ctr,#8
        neg frqa,#1
        mov accb,#0
bit     mov phsa,#8
        test domask,ina wc
        addx accb,accb        
        rol phsb,#1
        djnz ctr,#bit
sendio_ret
sendiohi_ret
        ret
checktime
        mov duration,cnt
        sub duration,starttime
        cmp duration,clockfreq wc
checktime_ret
  if_c  ret
        neg duration,#13
        wrlong duration,parptr
        jmp #finished
cmd
        andn outa,csmask
        neg phsb,#1
        call #sendiohi
        mov phsb,cmdo
        add phsb,#$40
        call #sendio
        mov phsb,cmdp
        shl phsb,#9
        call #sendiohi
        call #sendiohi
        call #sendiohi
        call #sendiohi
        mov phsb,#$95
        call #sendio
readresp
        neg phsb,#1
        call #sendiohi
        call #checktime
        cmp accb,#$ff wz
   if_z jmp #readresp 
cmd_ret
readresp_ret
        ret
busy
        neg phsb,#1
        call #sendiohi
        call #checktime
        cmp accb,#$0 wz
   if_z jmp #busy
busy_ret
        ret

SD_activate
#ifdef TRIBLADEPROP
        mov  acca,ram_disable
        or   acca,ram_LE_bit
        mov  outa,acca
        mov  dira,acca
#endif
        mov acca,#1
        shl acca,di
        or dira,acca
        mov acca,#1
        shl acca,clk
        or dira,acca
        mov acca,#1
        shl acca,do
        mov domask,acca
        mov acca,#1
        shl acca,cs
        or dira,acca                            ' drive CS low
#ifdef TRIBLADEPROP
        andn outa,acca
#elseifdef RAMBLADE
'        andn outa,acca
#endif
        mov csmask,acca
SD_activate_ret
        ret

#ifdef TRIBLADEPROP
'
'SD_Tristate : Give up the control lines shared between SD and XMM
'
SD_tristate
              mov     outa,#0
              mov     dira,#0
SD_tristate_ret
        ret
'
' TriBladeProp Blade #2 Hardware Access:
'
'                                        +-------- -CS (U26 FLASH)   -+
'                                        |+------- -CS (J22 microSD)  +- Latched pins...
'                                        ||+------ -RST3              |    passes when LE=1
'           (P30) SO ---+                |||+----- -RST1              |    latches on  LE low going edge
'           (P31) SI --+|                ||||+---- -CE (U24 SRAM-hi)  |    latched on  LE=0
'                      ||+---- -WE(SDA)  |||||+--- -CE (U23 SRAM-lo)  |
'                      |||+--- -OE(SCL)  ||||||+-- A20                |
'                      ||||+--  LE       |||||||+- A19               -+
'                      |||||             ||||||||
ram_LE_bit    long    %00001_00000000000_00000000_00000000 ' LE bit
ram_disable   long    %00000_00000000000_00111100_00000000 ' -RST1=1, -RST3=1, -CE(U24)=1, -CE(U23)=1
'                            +---------+ +------+ +------+
'                            A18......A8 A7....A0 D7....D0
'                                        Q7....Q0
#elseifdef RAMBLADE
'
'SD_Tristate : Give up the control lines shared between SD and XMM
'
SD_tristate
              mov     outa,#0
              mov     dira,#0
SD_tristate_ret
        ret
#endif

di      long 0
do      long 0
clk     long 0
cs      long 0
nco     long $1000_0000
hifreq  long $e0_00_00_00
freq    long $26_214_400
clockfreq long 104_857_600
onek    long 1000
sector  long 512
domask  res 1
csmask  res 1
acca    res 1
accb    res 1
cmdo    res 1
cmdp    res 1
comptr  res 1
parptr  res 1
parptr2 res 1
ctr     res 1
ctr2    res 1
starttime res 1
duration res 1
{{
'  Permission is hereby granted, free of charge, to any person obtaining
'  a copy of this software and associated documentation files
'  (the "Software"), to deal in the Software without restriction,
'  including without limitation the rights to use, copy, modify, merge,
'  publish, distribute, sublicense, and/or sell copies of the Software,
'  and to permit persons to whom the Software is furnished to do so,
'  subject to the following conditions:
'
'  The above copyright notice and this permission notice shall be included
'  in all copies or substantial portions of the Software.
'
'  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
'  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
'  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
'  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
'  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
'  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
'  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}}
