{{
sdram.spin - defines a device driver for ISSI/Micron 32Mx8+ SDRAM

Any memory interface can be used given enough pins; a parallel
bus interface is challenging since Propeller only has 32 pins.
An 8 bit data bus is a reasonable trade for pin use/performance.
With a cache, the a bus width > 8 bits is best if possible.

This SDRAM solution uses 20 propeller pins (A8-15 are latched)
for A0-12, BA0-1, CS*, CLK, RAS*, CAS*, D0-7 (CKE is pulled high).

The CS* pin is permanently pulled low for a 32Mx8 memory.

It is natural for a multiplexed address to total 16 pins and so
9 pins are used to allow a 32Mx8 memory space (8 pins + ALE).
A0-7     : A0-7
A0-7 ALE : A8-15 (ALE on P24)
SDRAM A0-7 on P8-15
SDRAM CLK  on P23
SDRAM ALE  on P24 ... using inverted SDRAM CLK for ALE is difficult.

An alternative design is to provide address/data on P0-15. That is
A0-7 shared with D0-7. This is possible because of the SDRAM cycles.
There is some concern that SDRAM output could step on latched address.
}}

'not using ALWAYSWRITE seems to break things as programs start printing in French! ... fix it later!
#define ALWAYSWRITE     ' using this is faster for cache hits, slower for miss
'#define ALWAYSFLUSH    ' using this would force cache miss for worst case performance

{

con EOL      = $a
con TESTSIZE = $2_0000  ' test size

{{
=====================================================================
Test clock setup
}}
con
  _CLKMODE      = XTAL1 + PLL16x
  _XINFREQ      = 5_000_000
    
{{
=====================================================================
Debug objects
}}
obj
    sx  : "FullDuplexSingleton"
#ifdef BMAD
    bu  : "BMAutility"
#endif
{
'    bu  : "BMAutility"
'#define BMAD
'}

{{
=====================================================================
test code
}}
var
    long    command             ' command pointer
    long    address             ' address pointer
    long    databuf             ' data or buffer pointer

{{
=====================================================================
main test
}}
pub main | n, a
    sx.start(31,30,0,57600)     ' start serial interface
    waitcnt(clkfreq/10+cnt)     ' wait for user terminal to start

    sx.str(string(EOL,"33 MB SDRAM Test Startup. Cache size: "))
    sx.dec(CACHESIZE)
    sx.str(string(" Tag count: "))
    sx.dec(TAGCOUNT)
    sx.str(string(" Line size: "))
    sx.dec(LINELEN)
    sx.out(EOL)

    start(@command)             ' start sdram pasm cog

#ifdef BMAD
    cognew(tmptest,@stack)
    bu.start
#endif


    repeat
        writeLong(0,$55aa33cc)
'        'readLong(0)
'        dumpCache

    'tmptest
    'test6(TESTSIZE)
    'repeat

    'timetest
    repeat
        tmptest
        test6(TESTSIZE)
        sx.str(string("EXITCOM",sx#EOL))
{
        test7(TESTSIZE)
        test8(TESTSIZE)
        'test7($200_0000)
}
    repeat
        writeBuffer(0,@mybuffer)
        'readBuffer(0,@mybuffer)
{
    repeat
        test1
        test2
        test3
        test4
        test5
}

var long stack[80]
var long mybuffer[LINELEN]

pub testloop | n

    repeat
        repeat n from 0 to 255*LINELEN step LINELEN
            readBuffer(n,@mybuffer)


pub tmptest | n, wval, rval, madr, orval
    repeat while long[mbcmd]

    n       := 260
    madr    := $01ff_ffff
    orval   := $1000_0000

    'repeat
    '    writeLong(madr,$11224488 | orval)
    '    rval := readLong(madr)

    'sx.str(string(EOL,EOL,"writeLong dump",EOL))
    sx.str(string("Write / Read Value",sx#EOL))
    'repeat 
    repeat madr from 0 to n*LINELEN-1 step 4
        writeLong(madr,madr | orval)
{
        ifnot madr & $1f
            sx.out(EOL)
        sx.out(" ")
        sx.hex(madr,8)

    dumpCache

    sx.str(string(EOL,EOL,"readLong dump",EOL))
'}
    repeat madr from 0 to n*LINELEN-1 step 4
        rval := readLong(madr)
{
'       if madr > LINELEN<<2-1
'           repeat
        ifnot madr & $1f
            sx.out(EOL)
        sx.out(" ")
        sx.hex(rval,8)
'}
        if rval <> madr | orval
            sx.str(string(sx#EOL,"Error: expected "))
            sx.hex(madr | orval,8)
            sx.str(string(" received "))
            sx.out(" ")
            sx.hex(rval,8)
            dumpCache
'            repeat until sx.rxready
'                rval := readLong(madr)
            'sx.rx
        'dumpCache
    'dumpCache
    'sx.rx


pub dumpCache | n, mod

    sx.str(string(EOL,EOL,"Cache Dump",EOL))
    sx.str(string("TAG TAGVAL  : Cache Line"))
    mod := 8
    repeat n from 0 to CACHESIZE>>2-1
        ifnot n // mod
            sx.out(EOL)
            sx.hex(n/constant(LINELEN>>2),3)
            sx.out(" ")
            sx.hex(tagram[n/constant(LINELEN>>2)],8)
            sx.out(":")
        sx.out(" ")
        sx.hex(cache[n],8)
    sx.out(EOL)

pub timetest | t0, t1, n, len, spinover, bps

    len := 50000

    t0  := cnt
    repeat n from 0 to len
        spincall(0,@mybuffer)
        spincall(0,@mybuffer)
    t1 := cnt
    spinover := (t1-t0)
    sx.str(string(EOL,"SPIN Overhead Calls/s: "))
    sx.dec((len*2)/((t1-t0)/clkfreq))

    t0  := cnt
    t0  := cnt
    repeat n from 0 to len
        writeBuffer(0,@mybuffer)
        readBuffer(0,@mybuffer)
    t1 := cnt
    bps := (len*2)/((t1-t0-spinover)/clkfreq)
    sx.str(string(EOL,"Cached Buffers/s: "))
    sx.dec(bps)
    sx.str(string(" about "))
    sx.dec(LINELEN*bps/1000)
    sx.str(string(" KB/s minus SPIN overhead."))

    t0  := cnt
    repeat n from 0 to len
        writeBuffer(n*LINELEN,@mybuffer)
        readBuffer(n*LINELEN,@mybuffer)
    t1 := cnt
    bps := (len*2)/((t1-t0-spinover)/clkfreq)
    sx.str(string(EOL,"Uncached Buffers/s: "))
    sx.dec(bps)
    sx.str(string(" about "))
    sx.dec(LINELEN*bps/1000)
    sx.str(string(" KB/s minus SPIN overhead."))
    sx.out(EOL)

pub spincall(x,datp)
    long[mbcmd] := 0 ' nop
    repeat while 0
    longmove(datp,long[mbdata],LINELEN>>2)

pub test1 | n, wval,rval,madr
    repeat while long[mbcmd]

    madr := 4
    wval := $88442211

    sx.str(string(EOL,EOL,"Write / Read Value",EOL))
    sx.hex(wval,8)
    sx.out(" ")

    writelong(madr,wval)
    rval := readlong(madr)
    sx.hex(rval,8)

    n := 1
    writelong(madr,wval)

    repeat 2

        rval := readlong(madr)

        if rval <> wval
           sx.str(string(EOL,"Read error. Expected: "))
           sx.hex(wval,8)
           sx.str(string(" Received: "))
           sx.hex(rval,8)
           repeat
                readlong(madr)
        else
            sx.out(" ")
            sx.hex(rval,8)
            ifnot n++ & 7
                sx.out(EOL)

pub test2 | n,wval,rval

    repeat while long[mbcmd]

    sx.str(string(EOL,EOL,"Long Write/Read Cache Buffer",EOL))

    wval := $88442211

    n := 0
    repeat 1
        writelong(0,wval)
        writelong(4,$11223344)
        writelong(8,$55555555)
        writelong(12,$AAAAAAAA)

        sx.hex(readLong(0),8)
        sx.out(" ")
        sx.hex(readLong(4),8)
        sx.out(" ")
        sx.hex(readLong(8),8)
        sx.out(" ")
        sx.hex(readLong(12),8)
        sx.out(" ")

        ifnot ++n & 7
            sx.out(EOL)

    repeat 1
        readBuffer(0,@mybuffer)
        repeat n from 0 to 3
            sx.hex(long[@mybuffer][n],8)
            sx.out(" ")

pub test3 | j,n,wval,rval

    repeat while long[mbcmd]

    sx.str(string(EOL,EOL,"Buffer Write/Read Cache"))

    repeat j from 0 to 1*LINELEN step LINELEN

        repeat n from 0 to LINELEN-1
            byte[@mybuffer][n] := $55
{
            ifnot n & 15
                sx.out(EOL)
            sx.out(" ")
            sx.hex(byte[@mybuffer][n],2)
}

        writeBuffer(j,@mybuffer)

        readBuffer(j,@mybuffer)

        repeat n from 0 to LINELEN-1
            ifnot n & 15
                sx.out(EOL)
            sx.out(" ")
            sx.hex(byte[@mybuffer][n],2)

        repeat n from 0 to LINELEN-1
            byte[@mybuffer][n] := (n+1) & $ff
{
            ifnot n & 15
                sx.out(EOL)
            sx.out(" ")
            sx.hex(byte[@mybuffer][n],2)
}
        writeBuffer(j,@mybuffer)

        readBuffer(j,@mybuffer)

        repeat n from 0 to LINELEN-1
            ifnot n & 15
                sx.out(EOL)
            sx.out(" ")
            sx.hex(byte[@mybuffer][n],2)


pub test4 | j,k,n,wval,rval,pages

    repeat while long[mbcmd]

    writelong(0,0)
    sx.str(string(EOL,EOL,"Buffer Write/Read Cache Dump",EOL))

    pages := 130
    k := 0

    repeat j from 0 to pages*LINELEN step LINELEN

        repeat n from 0 to LINELEN>>2-1

            mybuffer[n] := ++k
            {
            ifnot n & $7
                sx.out(EOL)
            sx.out(" ")
            sx.hex(mybuffer[n],8)
            '}

        writeBuffer(j,@mybuffer)

    sx.out(EOL)

    k := 0
    repeat j from 0 to pages*LINELEN step LINELEN

        readBuffer(j,@mybuffer)

        repeat n from 0 to LINELEN>>2-1

            ifnot n & $7
                sx.out(EOL)
            sx.out(" ")
            sx.hex(mybuffer[n],8)

    dumpCache


pub test5 | n, a
    repeat n from 0 to LINELEN-1
        byte[@mybuffer][n] := n & $ff

    a := 0
    writeBuffer(a,@mybuffer)
    readBuffer(a,@mybuffer)

    sx.str(string(EOL,"Address "))
    sx.hex(a,8)
    sx.out(" ")
    sx.dec(a/1000000)
    sx.str(string("M Buffer at "))
    sx.hex(long[mbdata],8)

    repeat n from 0 to LINELEN-1
        ifnot n & 15
            sx.out(EOL)
        sx.out(" ")
        sx.hex(byte[@mybuffer][n],2)


pub test6(size) | n, a, base, d, j, end, seed, spit

    repeat while long[mbcmd]
    sx.str(string(sx#EOL,"Incremental Pattern Test "))

    end  := size-LINELEN-1
    spit := (end >> 6) - 1
    base := 0

    sx.dec(end/1000)
    sx.str(string(" KB",sx#EOL))

    sx.str(string("----------------------------------------------------------------"))
    sx.out(sx#EOL)
    seed := newseed
    repeat a from 0 to end step LINELEN
        repeat n from 0 to LINELEN>>2-1
            long[@mybuffer][n] := ++seed
{
            ifnot n & 31
                'sx.out(sx#EOL)
                sx.str(string(sx#EOL,"$"))
                sx.hex(a+base,8)
                sx.out(":")
            sx.out(" ")
            sx.hex(long[@mybuffer][n],8)
'}
        writeBuffer(a+base,@mybuffer)
        ifnot a & spit
            sx.out("w")
    sx.out(EOL)

    seed := newseed
    repeat a from 0 to end step LINELEN
        readBuffer(a+base,@mybuffer)
        ifnot a & spit
            sx.out("r")
        repeat n from 0 to LINELEN>>2-1
            d := long[@mybuffer][n]
            if ++seed <> d
                sx.str(string(EOL,EOL,"Error at $"))
                sx.hex(a,8)
                sx.str(string(" Expected $"))
                sx.hex(seed,8)
                sx.str(string(" Received $"))
                sx.hex(d,8)
                sx.str(string(EOL,"Address  $"))
                sx.hex(a,8)
                sx.str(string(" Buffer at $"))
                sx.hex(long[mbdata],8)
                sx.out(" ")
                sx.dec(a/1000)
                sx.str(string("K Page"))
                dumpCache
                return

    sx.str(string(EOL,"Test Complete!",EOL))
    newseed := seed

pub test7(size) | n, a, base, d, j, end, seed, spit

    repeat while long[mbcmd]
    sx.str(string(EOL,"Pseudo-Random Pattern Test "))

    end  := size-LINELEN
    spit := (end >> 6) - 1
    base := 0

    sx.dec(end/1000)
    sx.str(string(" KB",EOL))

    sx.str(string("----------------------------------------------------------------"))
    sx.out(EOL)
    seed := newseed
    repeat a from 0 to end step LINELEN
        repeat n from 0 to LINELEN>>2-1
            long[@mybuffer][n] := ?seed
{
            ifnot n & 31
                sx.out(EOL)
            sx.out(" ")
            sx.hex(long[@mybuffer][n],8)
'}
        writeBuffer(a+base,@mybuffer)
        ifnot a & spit
            sx.out("w")
    sx.out(EOL)

    seed := newseed
    repeat a from 0 to end step LINELEN
        readBuffer(a+base,@mybuffer)
        ifnot a & spit
            sx.out("r")
        repeat n from 0 to LINELEN>>2-1
            d := long[@mybuffer][n]
            if ?seed <> d
                sx.str(string(EOL,EOL,"Error at $"))
                sx.hex(a,8)
                sx.str(string(" Expected $"))
                sx.hex(seed,8)
                sx.str(string(" Received $"))
                sx.hex(d,8)
                sx.str(string(EOL,"Address  $"))
                sx.hex(a,8)
                sx.str(string(" Buffer at $"))
                sx.hex(long[mbdata],8)
                sx.out(" ")
                sx.dec(a/1000)
                sx.str(string("K Page"))
                dumpCache
                return

    newseed := seed ' use different seed every test pass

    sx.str(string(EOL,"Test Complete!",EOL))


pub test8(size) | n, a, mask, d, j, end, seed, spit, madr, key

    repeat while long[mbcmd]
    sx.str(string(EOL,"Random Addr Pattern Test "))

    end  := size-LINELEN
    spit := (end >> 6) - 1
    mask := (size-1) >> 1

    sx.dec(end/1000)
    sx.str(string(" KB",EOL))

    sx.str(string("----------------------------------------------------------------"))
    sx.out(EOL)

    repeat a from 0 to end step LINELEN
        writeLong(a,0)
        ifnot a & spit
            sx.out("c")
    sx.out(EOL)

    seed := newseed | 1
    madr := newseed
    repeat a from 0 to end step LINELEN
        madr?
        madr &= mask & !(LINELEN-1)

        if readLong(a+madr)    ' already have a key?
            next

        long[@mybuffer][0] := seed++
        repeat n from 1 to LINELEN>>2-1
            long[@mybuffer][n] := seed++
            writeBuffer(a+madr,@mybuffer)
{
            ifnot n & 31
                sx.out(sx#EOL)
                sx.hex(madr,8)
                sx.out(":")
            sx.out(" ")
            sx.hex(long[@mybuffer][n],8)
'}
        ifnot a & spit
            sx.out("w")
    sx.out(EOL)

    seed := newseed | 1
    madr := newseed
    repeat a from 0 to end step LINELEN
        madr?
        madr &= mask & !(LINELEN-1)
        readBuffer(a+madr,@mybuffer)
        if seed <> readLong(a+madr)
            next
        ifnot a & spit
            sx.out("r")
        repeat n from 0 to LINELEN>>2-1
            d := long[@mybuffer][n]
            if seed++ <> d
                sx.str(string(EOL,EOL,"Error at $"))
                sx.hex(a+madr,8)
                sx.str(string(" Expected $"))
                sx.hex(seed,8)
                sx.str(string(" Received $"))
                sx.hex(d,8)
                sx.str(string(EOL,"Address  $"))
                sx.hex(a+madr,8)
                sx.str(string(EOL,"Tag "))
                sx.hex(((a+madr)>>LINESHFT) & (TAGCOUNT-1),3)
                sx.str(string(" TagValue $"))
                sx.hex((a+madr)>>LINESHFT,8)
                sx.str(string(" Buffer at $"))
                sx.hex(long[mbdata],8)
                sx.out(" ")
                sx.dec((a+madr)/1000)
                sx.str(string("K Page"))
                dumpCache
                sx.str(string("Press any key ..."))
                sx.rx
                quit

    sx.str(string(EOL,"Test Complete!",EOL))
    newseed := madr

    'dumpCache

var long newseed

'}

{{
=====================================================================
driver code
=====================================================================
}}

{{
=====================================================================
These constants define the pin functions:
}}
con
    DBPIN_MASK  = $FF
    ABPIN_MASK  = $FF00
    DQM_PIN     = 23
    ALE_PIN     = 24
    RASN_PIN    = 25
    CASN_PIN    = 26
    WEN_PIN     = 27
    CLK_PIN     = 23

    FREE_MASK   = $00FF0000     ' unused/free pin mask

    ADDR_MASK   = (1 << 26)-1   ' 64MB
    ROW_MASK    = (1 << 13)-1   ' A0-12
    COL_MASK    = (1 << 10)-1   ' A0-9
    CASN_PREPIN = (1 << 10)     ' A10 is precharge pin
    RASN_MASK   = (1 << RASN_PIN)
    CASN_MASK   = (1 << CASN_PIN)
    WEN_MASK    = (1 << WEN_PIN)
    DQM_MASK    = (1 << DQM_PIN)
    DCMD_MASK   = RASN_MASK | CASN_MASK | WEN_MASK

    MODE_REG    = $37           ' CAS latency 3, full burst

{{
=====================================================================
These constants define the driver commands
}}
con
    READ_CMD    = $3
    WRITE_CMD   = $2
    READ_BIT    = $1

{{
=====================================================================
Driver mailbox interface
}}
var long    mbcmd, mbdata, mbtags, mbcache

' cog store
var long    cog

{{
=====================================================================
Cache is organized as a table. The tagline table contains pointers to cache entries.
Each tagline has the following definition:

    TAGLINE:    ccmx_xxxx
    such that:  c=counter, d=dirty, m=c+d+xx, and x=match address

One small problem with this is where a program constantly swaps between
address $01ff_f000 and $0000_0000 as in the case of ZOG function calls.
Is it possible to quickl redirect tags for top and bottom addresses?
}}

#define LINELEN_64
con
'#define CACHE_8K ' if loader is too big ... can't get the friggin ifdefs to work right with homespun
#ifdef  CACHE_8K
    CACHESIZE   = $2000>>2          ' power of 2 cache size ... 64B to 16KB
#elseifdef CACHE_4K
    CACHESIZE   = $1000>>2          ' power of 2 cache size ... 64B to 16KB
#elseifdef CACHE_2K
    CACHESIZE   =  $800>>2          ' power of 2 cache size ... 64B to 16KB
#else
    CACHESIZE   = $4000>>2          ' power of 2 cache size ... 64B to 16KB
#endif

con
    TAGCOUNT    = CACHESIZE/LINELEN ' cache with linelen bytes
#ifdef LINELEN_128
    LINESHFT    = 7                 ' this must match the size of the buffer operations
#elseifdef LINELEN_64
    LINESHFT    = 6                 ' this must match the size of the buffer operations
#else
    LINESHFT    = 5                 ' this must match the size of the buffer operations
#endif
    LINELEN     = 1<<LINESHFT       ' CACHELINE SIZE

    TAGMASK     = $003F_FFFF        ' $3FFFFF*512 = 2GB $3FFFFF*32 = 128MB
    DIRTYSHFT   = 30                ' shift by N to get dirty bit   test tag,DIRTYMASK wc
    DIRTYMASK   = 1 << DIRTYSHFT    ' dirty bit mask                muxc tag,DIRTYSHFT

{{
=====================================================================
start(mbptr)
mbptr = start address of command interface
}}
pub start(mbptr)
    cache  := $AAAAAAAA
    mbcmd  := mbptr
    mbdata := mbptr+4
    mbtags := @tagram           ' tags are in-cog now
    mbcache:= @cache
    long[mbcmd]~~               ' set now so we know when startup is done
'#ifdef BMAD
'   bu.taskstart(@pasm,@mbcmd,0)
'#else
    cog := cognew(@pasm,@mbcmd)+1
    repeat while long[mbcmd]    ' wait for startup to finish
'#endif
    return cog                  ' start failed if return is 0

{{
=====================================================================
stop
}}
pub stop
    if cog                      ' if cog is zero, don't stop
        cogstop(cog~ - 1)       ' stop cog and set cog var to zero

{{
=====================================================================
read a long
}}
pub readLong(madr)
    long[mbcmd] := madr | READ_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    return long[long[mbdata]+madr]

{{
=====================================================================
write a long
}}
pub writeLong(madr,val)
    long[mbcmd] := madr | WRITE_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    long[long[mbdata]+madr] := val

{{
=====================================================================
read a word
}}
pub readWord(madr)
    long[mbcmd] := madr | READ_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    return word[long[mbdata]+madr]

{{
=====================================================================
write a word
}}
pub writeWord(madr,val)
    long[mbcmd] := madr | WRITE_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    word[long[mbdata]+madr] := val

{{
=====================================================================
read a byte
}}
pub readByte(madr)
    long[mbcmd] := madr | READ_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    return byte[long[mbdata]+madr]

{{
=====================================================================
write a byte
}}
pub writeByte(madr,val)
    long[mbcmd] := madr | WRITE_CMD
    repeat while long[mbcmd]
    madr &= LINELEN-1
    byte[long[mbdata]+madr] := val

{{
=====================================================================
read buffer
}}
pub readBuffer(madr,datp) | n
    long[mbcmd] := madr | READ_CMD
    repeat while long[mbcmd]
    longmove(datp,long[mbdata],LINELEN>>2)

{{
=====================================================================
write buffer
}}
pub writeBuffer(madr,datp) | n
    long[mbcmd] := madr | WRITE_CMD
    repeat while long[mbcmd]
    ' sending write command says this buffer will be written on next flush
    longmove(long[mbdata],datp,LINELEN>>2)

{{
=====================================================================
cache
}}
dat ' cache
tagram          long 0 [TAGCOUNT]
cache           long 0 [CACHESIZE-$1e0]     ' reclaim PASM code for cache

{{
=====================================================================
PASM Driver
=====================================================================
}}
dat                 org 0

pasm
long    0 [8]                               ' debugger stub space

'--------------------------------------------------------------------
' startup and some variables
'
_Setup              jmp #sdram_setup        ' 0

cmdptr              long 0                  ' cache command pointer
datptr              long 0                  ' cache data pointer
tagptr              long 0                  ' tag pointer
cacheptr            long 0                  ' cache pointer

index               long 0                  ' index into cache
tagline             long 0                  ' tag line pointer
clineptr            long 0                  ' cache line pointer
tags                long 0

tag                 long 0
tagset              long 0
tagaddr             long 0
addr                long 0
saveaddr            long 0
dirty               long 0

bank                long 0
ndx                 long 0
tmp                 long 0
bufp                long 0

'====================================================================
' mailbox command spinner loop
' user will get a block pointer back and access the block with a mod index such as:
'
'                    mov     index,addr          ' user must do this for return pointer array index
'                    and     index,#(LINELEN-1)  ' index can be only up to LINELEN-1
'

sdramDone           'wrlong  zero,cmdptr        ' waste of time to do it here ....

cmdloop             djnz    refresh,#norefresh  ' check refresh every time ... djnz here keeps window
                    call    #sdram_refresh      ' if refresh timer expired, reload and refresh
norefresh           rdlong  addr,cmdptr    wz   ' get command/address ... command is in bit 0..1
    if_z            jmp     #cmdloop            ' if zero, do nothing

                    mov     clineptr,addr       ' get the cache line pointer offset from address
                    and     clineptr,_CLP_MOD   ' user sends full address. we only load blocks
                    add     clineptr,cacheptr   ' get cache line

                    shr     addr,#LINESHFT wc   ' carry set (READ) else (WRITE) align address to page
                    mov     tags,addr           ' (addr / LINESIZE) mod TAGCOUNT is tag / cache line offset
                    and     tags,_TAG_MOD       ' user sends full address. we only load blocks
                    shl     tags,#2             ' adjust for long hub storage
                    add     tags,tagptr         ' add tagptr base to get address of tag

readtag             rdlong  tag,tags wz         ' get tag line contents
#ifdef ALWAYSWRITE
                    mov     tagaddr,tag         ' get tag
                    and     tagaddr,_TAG_MASK
                    wrlong  clineptr,datptr     ' send cache buffer to user
#else
                    muxnc   dirty,_TAG_DIRTY    ' save dirty bit for any write. C clear if write
                    mov     tagaddr,tag         ' get tag
                    wrlong  clineptr,datptr     ' send cache buffer to user
                    and     tagaddr,_TAG_MASK
#endif
                    cmp     addr,tagaddr wz
#ifdef ALWAYSFLUSH
                    call    #flush              ' always flush means no cache
#else
    if_nz           call    #flush              ' use to enable cache
#endif

CacheDone
                    ' the user doesn't have to wait for us to update our tag table :)
                    wrlong  zero,cmdptr         ' let user know we're ready ...

#ifndef ALWAYSWRITE
                    or      tag,dirty           ' save dirty bit
#endif
                    or      tag,_TAG_MSB        ' if add overflow set MSB ...
writetag            wrlong  tag,tags            ' write the new tag info

                    jmp     #sdramDone

'--------------------------------------------------------------------
' flush method saves dirty page and loads page from backstore
'
flush               ' if dirty bit set, write old buffer before read
#ifndef ALWAYSWRITE
                    test    tag,_TAG_DIRTY wc   ' test for write on any tag ... not just the last access
    if_nc           jmp     #flush_rdonly       ' if dirty page, save it, else just read another one
#endif

flush_write         mov     saveaddr,addr       ' save addr for read
                    mov     addr,tagaddr        ' get cacheline tag addr
                    shl     addr,#LINESHFT      ' physical address to write cacheline
                    call    #sdramBuffWrite     ' save cacheline
                    mov     addr,saveaddr       ' restore addr for read
flush_rdonly        
                    mov     tag,addr            ' move address clears dirty/count
                    shl     addr,#LINESHFT      ' physical address to load cacheline
                    call    #sdramBuffRead      ' if zero, load cache line
flush_ret           ret



'====================================================================

refresh_wait        mov     ndx,#1              ' _FRQA_FASTER gives 2 clocks per instruction 
:loop               nop                         ' the call and these instructions gives 10 tRP clocks
                    djnz    ndx,#:loop
refresh_wait_ret    ret

'--------------------------------------------------------------------
' refresh
'
sdram_refresh       ' if we get here refresh is 0
                    mov     refresh, refreshtime
                    mov     addr,#0             ' refresh 1st chip

                    or      outa,_DCMD_NOP      ' set NOP
                    or      addr,_SET_A10       ' set A10 for precharge all bank select
                    call    #setAddress

                    mov     frqa,_FRQA_FASTER
                    mov     phsa,_PHSA2         ' edge offset each enable
                    andn    outa,_CLK_MASK

                    andn    outa,_DCMD_PRECHGNOT' send PRECHARG
                    call    #delay2
                    or      outa,_DCMD_NOP      ' set NOP
                    call    #delay2
                    andn    outa,_DCMD_RFRSH_NOT' set REFRESH
                    call    #delay2
                    or      outa,_DCMD_NOP      ' set NOP
                    call    #refresh_wait
                    andn    outa,_DCMD_RFRSH_NOT' set REFRESH
                    call    #delay2
                    or      outa,_DCMD_NOP      ' set NOP
                    call    #refresh_wait
                    mov     frqa,_FRQA_FAST
                    or      outa,_CLK_MASK      ' disable clock
sdram_refresh_ret   ret

refresh         long  625     ' execute 8192 times each 64ms = 8 times/ms
refreshtime     long  625     ' 625*200ns = 125us = 8 times/ms


'====================================================================

'_FREE_MASK      long FREE_MASK   ' $00FF0000   ' unused/free pin mask

_TAG_MASK       long TAGMASK                    ' greatest address for cache
_TAG_DIRTY      long DIRTYMASK
_TAG_MOD        long (TAGCOUNT-1)               ' use modulo for finding tag address
_TAG_LINE_MASK  long (TAGCOUNT-1)<<LINESHFT     ' use for modulo
_TAG_MSB        long $8000_0000
_TAG_INC        long $0080_0000
_TAG_CNT        long $FF80_0000
_TAG_CMD        long $C000_0000                 ' used for decoding command
_CLP_MOD        long (TAGCOUNT-1)<<LINESHFT     ' use modulo for finding tag address

'_DBPIN_MASK     long DBPIN_MASK
_ABPIN_MASK     long ABPIN_MASK
_ALE_ABPIN_MASK long ABPIN_MASK | (1 << ALE_PIN)
_ALE_MASK       long 1 << ALE_PIN
_CLK_MASK       long 1 << CLK_PIN
_WEN_MASK       long 1 << WEN_PIN
_BANK_MASK      long $6000
_ADDR_MASK      long ADDR_MASK
_RASN_MASK      long RASN_MASK
_CASN_MASK      long CASN_MASK
_CPRE_MASK      long CASN_PREPIN
_DCMD_MASK      long DCMD_MASK                  ' SDRAM COMMAND bits
_DCMD_CLK_MASK  long DCMD_MASK | (1 << CLK_PIN)

_SET_A10        long $0400  ' A10 used for prechareg all banks A7..0 A15..8
_LOW_ADDR_MASK  long $03ff
'_32MB           long $0200_0000
'_MSB16          long $8000
_MSB32          long $8000_0000

_DCMD_NOP       long RASN_MASK | CASN_MASK | WEN_MASK   ' 111 RAS,CAS,WEN
'_DCMD_BTERM     long RASN_MASK | CASN_MASK             ' 110
_DCMD_BTERM_NOT long WEN_MASK                           ' 001
'_DCMD_READ      long RASN_MASK | WEN_MASK              ' 101
_DCMD_READ_NOT  long CASN_MASK                          ' 010
'_DCMD_WRITE     long RASN_MASK                         ' 100
_DCMD_WRITE_NOT long CASN_MASK | WEN_MASK               ' 011
'_DCMD_ACTIVE    long CASN_MASK | WEN_MASK              ' 011
_DCMD_ACTIVENOT long RASN_MASK                          ' 100
'_DCMD_PRECHG    long CASN_MASK                         ' 010
_DCMD_PRECHGNOT long RASN_MASK | WEN_MASK               ' 101
'_DCMD_REFRESH   long WEN_MASK                          ' 001 RAS,CAS,WEN
_DCMD_RFRSH_NOT long RASN_MASK | CASN_MASK              ' 110 RAS,CAS,WEN
_DCMD_MODE      long 0                                  ' 000

_MODE_REG       long MODE_REG << 8

OUTPUT_MASK     long DQM_MASK| ABPIN_MASK| RASN_MASK| CASN_MASK| (1<<ALE_PIN)| (1<<WEN_PIN)

_FRQA_FASTER    long $8000_0000
_FRQA_FAST      long $2000_0000
_PHSA           long $8000_0000
_PHSA8          long $8000_0000
_PHSA4          long $4000_0000
_PHSA2          long $2000_0000
_PHSA1          long $1000_0000
'_PHSA08         long $0800_0000
'_PHSA0          long $8000_0000

_CTRA           long (%100_000 << 23) | CLK_PIN

zero            long 0

data            long 0
dat1            long 0
dat2            long 0
dat3            long 0
dat4            long 0
dat5            long 0
dat6            long 0
dat7            long 0

'--------------------------------------------------------------------
' setup hardware and mailbox interface
'
sdram_setup

' get mailbox pointers
                    mov     tmp,par
                    rdlong  cmdptr,tmp
                    add     tmp,#4
                    rdlong  datptr,tmp
                    add     tmp,#4
                    rdlong  tagptr,tmp
                    add     tmp,#4
                    rdlong  cacheptr,tmp
sdram_enable

' enable outputs
                    or      outa,_DCMD_NOP      ' set NOP for startup
                    andn    outa,_ABPIN_MASK    ' clear address bits
                    mov     dira, OUTPUT_MASK   ' start with read

' setup sdram clock
                    or      outa,_CLK_MASK      ' clock starts "disabled"
                    or      dira,_CLK_MASK      ' turn on clock output forever
                    mov     frqa,_FRQA_FAST     ' set access frequency
                    mov     ctra,_CTRA

' write SDRAM mode bits.
' DQM is pulled high on the board; it should be low - rework.
'

' send mode command sequence

                    mov     addr,#0             ' init 1st chip
send_initmode
                    ' necessary to stabalize SDRAM before first precharge
                    andn    outa,_CLK_MASK      ' turn on clock
                    mov     ndx,#$100
                    shl     ndx,#7              ' wait 100us
                    add     ndx,cnt
                    waitcnt ndx,ndx
                    or      outa,_CLK_MASK      ' turn off clock

                    call    #sdcmd_PRECHALL
                    call    #sdcmd_NOP
                    call    #sdcmd_NOP
                    call    #sdcmd_NOP
                    call    #sdcmd_NOP
                    mov     ndx,#12             ' at least 8 REFRESH to startup for ISSI
:initloop           call    #sdcmd_REFRESH
                    call    #sdcmd_NOP
                    djnz    ndx,#:initloop

                    call    #sdcmd_MODE
                    call    #sdcmd_NOP
                    call    #sdcmd_NOP
                    call    #sdcmd_NOP

' preload cache here saves an instruction during cache fetch
                    mov     addr,#0
                    mov     ndx,#TAGCOUNT
                    mov     tags,tagptr         ' tagstart
                    mov     clineptr,cacheptr   ' cachestart
:loop               call    #sdramBuffRead      ' if zero, load cache line ... changes carry
                    mov     tag,addr            ' tag address
                    or      tag,_MSB32          ' always active
                    wrlong  tag,tags            ' save tag
                    add     tags,#4             ' next tag
                    add     addr,#LINELEN       ' next block
                    add     clineptr,#LINELEN   ' next cache line
                    djnz    ndx,#:loop

'                   jmp     #sdram_enable       ' debug startup
' all done
                    wrlong  zero,cmdptr         ' let user know we're ready ...
                    jmp     #sdramDone

'--------------------------------------------------------------------
' SDRAM commands
' use movd later to set commands ... can save andn _DCMD_MASK and toggleClock
'
toggleClock         mov     phsa,_PHSA          ' edge offset each enable
                    andn    outa,_CLK_MASK      ' turn clock on
                    or      outa,_CLK_MASK      ' turn clock off
toggleClock_ret     ret

sdcmd_ACTIVE        andn    outa,_DCMD_ACTIVENOT' send ACTIVE
                    call    #toggleClock
sdcmd_ACTIVE_ret    ret

sdcmd_MODE          call    #setAddress         ' clear A10
                    or      outa,_MODE_REG
                    andn    outa,_DCMD_MASK     ' mode CMD is 0
                    call    #toggleClock
                    or      outa,_DCMD_NOP      ' send NOP
sdcmd_MODE_ret      ret

sdcmd_NOP           or      outa,_DCMD_NOP      ' send NOP
                    call    #toggleClock
sdcmd_NOP_ret       ret

sdcmd_PRECHALL
                    or      addr,_SET_A10       ' set A10 for precharge all bank select
                    call    #setAddress
                    call    #sdcmd_PRECHARG     ' precharge
                    andn    addr,_SET_A10       ' clear A10 when done
sdcmd_PRECHALL_ret  ret

sdcmd_PRECHARG
                    or      outa,_DCMD_NOP      ' send NOP
                    call    #toggleClock
                    andn    outa,_DCMD_PRECHGNOT' send PRECHARG
                    call    #toggleClock
                    or      outa,_DCMD_NOP      ' send NOP
                    call    #toggleClock
                    call    #toggleClock
sdcmd_PRECHARG_ret  ret

sdcmd_REFRESH       andn    outa,_DCMD_RFRSH_NOT' send REFRESH
                    call    #toggleClock
sdcmd_REFRESH_ret   ret

delay2              nop
delay2_ret          ret

'--------------------------------------------------------------------
' utilities
'
setAddress          mov     tmp, addr
                    andn    outa,_ABPIN_MASK
                    or      outa,tmp
                    or      outa,_ALE_MASK
                    andn    outa,_ALE_MASK      ' hold upper address bits
                    and     tmp,_ABPIN_MASK
                    shl     tmp,#8
                    andn    outa,_ABPIN_MASK
                    or      outa,tmp
setAddress_ret      ret

'--------------------------------------------------------------------
' Send address sets address into sdram with upper address ACTIVE
' and leaves lower address on the bus for a READ or WRITE to take over.
' This is a burst setup
'
' SDRAM addressing architecture is a little strange. 25 bits for 32MB.
' BA0:1+A0:12 make up the 15 RAS bits. A0:9 make up 10 CAS bits.
' A10 is precharge control, and A11:12 are ignored during CAS cycle.
'
' a full address looks like this B_BHHH_HHhh_hhhh_hhLL_llll_llll
' CBBH_HHHH_hhhh_hhhh shr 10
'
'oldaddr             long 0  ' save last addressed accessed for new precharge

send_ADDRESS
                   ' first send precharge 
                    andn    outa,_DCMD_PRECHGNOT' set PRECHARGD
                    andn    outa,_CLK_MASK      ' turn on clock
                    mov     phsa,_PHSA          ' clock edge offset
                    nop
                    or      outa,_DCMD_NOP      ' send NOP
                    call    #delay2
                    or      outa,_CLK_MASK      ' turn off clock

                    ' then setup ROW address
                    mov     tmp,addr            ' $03FF_FFFF
                    shr     tmp,#10             ' $0000_FFFF put upper addr bits on P8-15
                    mov     bank,tmp            ' get bank address bits A13-14
                    and     bank,_BANK_MASK     ' use only bank bits
                    andn    outa,_ABPIN_MASK    ' clear address bits
                    or      outa,tmp            ' send high byte

                    ' could be a subroutine
                    or      outa,_ALE_MASK      ' enable latch to save hiadr,ba,CS bits
                    andn    outa,_ALE_MASK      ' high latched
                    andn    outa,_ABPIN_MASK    ' clear for next byte
                    shl     tmp,#8              ' $00FF_FF00 put lower bits on P8-15
                    or      outa,tmp            ' set low bits

                    ' send RAS ACTIVE - could call #sdcmd_ACTIVE
                    andn    outa,_DCMD_ACTIVENOT' send ACTIVE
                    mov     phsa,_PHSA          ' clock edge offset
                    andn    outa,_CLK_MASK      ' turn on clock
                    nop
                    or      outa,_DCMD_NOP      ' send NOP
                    call    #delay2
                    or      outa,_CLK_MASK      ' turn off clock

                    mov     tmp,addr            ' get lower 10 address bits A0..9
                    and     tmp,_LOW_ADDR_MASK  ' clear bits A10..15
                    or      tmp,bank            ' set bank select for CAS 
                    andn    outa,_ABPIN_MASK
'                    test    addr,_32MB wc
'    if_c            or      outa,_MSB16         ' >= 32MB set 2nd chip select
                    or      outa,tmp            ' put A8..9 and 2nd chip select on P8-15

                    ' don't use autoprecharge with burst terminate
                    andn    outa,_SET_A10       ' A10 low for normal precharge

                    ' could be a subroutine
                    or      outa,_ALE_MASK      ' enable upper bits
                    andn    outa,_ALE_MASK      ' high bits latched
                    andn    outa,_ABPIN_MASK    ' clear for next byte
                    shl     tmp,#8              ' put A0..7 on P8-15
                    or      outa,tmp            ' set low order address

send_ADDRESS_ret    ret


'--------------------------------------------------------------------
' read Burst ... 32 bytes ...
'
sdramBuffRead       'init and write set dbus to input
                    call    #send_ADDRESS
                    mov     bufp, clineptr      ' load cacheline

                    mov     phsa,_PHSA1         ' edge offset each enable
                    andn    outa,_DCMD_READ_NOT ' set READ from NOP
                    andn    outa,_CLK_MASK      ' turn on clock now for CAS delay time
                    nop
                    or      outa,_CLK_MASK      ' turn on clock now for CAS delay time
                    or      outa,_DCMD_NOP      ' set nops
                    mov     phsa,_PHSA4         ' edge offset each enable
                    andn    outa,_CLK_MASK      ' turn on clock now for CAS delay time
                    nop

                    call    #readLong8
#ifdef LINELEN_128
                    call    #readLong8
                    call    #readLong8
                    call    #readLong8
#elseifdef LINELEN_64
                    call    #readLong8
#endif

                    call    #burstTerminate

sdramBuffRead_ret   ret

readLong8
                    mov     phsa,_PHSA4         ' edge offset each enable
                    andn    outa,_CLK_MASK
                    movs    data,ina            ' $0000_0aAA
                    ror     data,#8     wc      ' $AA00_0001 C = LSB
                    movs    data,ina            ' $AA00_0bBB
                    ror     data,#8             ' $BBAA_000b
                    movs    data,ina            ' $BBAA_0cCC
                    ror     data,#17            ' $00cC_BbAa shifted right x 1
                    movi    data,ina            ' $DDcC_BBAa
                    rcl     data,#1             ' $DDCC_BBAA restore LSB
                    movs    dat1,ina            ' $0000_0aAA
                    ror     dat1,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat1,ina            ' $AA00_0bBB
                    ror     dat1,#8             ' $BBAA_000b
                    movs    dat1,ina            ' $BBAA_0cCC
                    ror     dat1,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat1,ina            ' $DDcC_BBAa
                    rcl     dat1,#1             ' $DDCC_BBAA restore LSB
                    movs    dat2,ina            ' $0000_0aAA
                    ror     dat2,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat2,ina            ' $AA00_0bBB
                    ror     dat2,#8             ' $BBAA_000b
                    movs    dat2,ina            ' $BBAA_0cCC
                    ror     dat2,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat2,ina            ' $DDcC_BBAa
                    rcl     dat2,#1             ' $DDCC_BBAA restore LSB
                    movs    dat3,ina            ' $0000_0aAA
                    ror     dat3,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat3,ina            ' $AA00_0bBB
                    ror     dat3,#8             ' $BBAA_000b
                    movs    dat3,ina            ' $BBAA_0cCC
                    ror     dat3,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat3,ina            ' $DDcC_BBAa
                    rcl     dat3,#1             ' $DDCC_BBAA restore LSB
                    movs    dat4,ina            ' $0000_0aAA
                    ror     dat4,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat4,ina            ' $AA00_0bBB
                    ror     dat4,#8             ' $BBAA_000b
                    movs    dat4,ina            ' $BBAA_0cCC
                    ror     dat4,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat4,ina            ' $DDcC_BBAa
                    rcl     dat4,#1             ' $DDCC_BBAA restore LSB
                    movs    dat5,ina            ' $0000_0aAA
                    ror     dat5,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat5,ina            ' $AA00_0bBB
                    ror     dat5,#8             ' $BBAA_000b
                    movs    dat5,ina            ' $BBAA_0cCC
                    ror     dat5,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat5,ina            ' $DDcC_BBAa
                    rcl     dat5,#1             ' $DDCC_BBAA restore LSB
                    movs    dat6,ina            ' $0000_0aAA
                    ror     dat6,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat6,ina            ' $AA00_0bBB
                    ror     dat6,#8             ' $BBAA_000b
                    movs    dat6,ina            ' $BBAA_0cCC
                    ror     dat6,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat6,ina            ' $DDcC_BBAa
                    rcl     dat6,#1             ' $DDCC_BBAA restore LSB
                    movs    dat7,ina            ' $0000_0aAA
                    ror     dat7,#8     wc      ' $AA00_0001 C = LSB
                    movs    dat7,ina            ' $AA00_0bBB
                    ror     dat7,#8             ' $BBAA_000b
                    movs    dat7,ina            ' $BBAA_0cCC
                    ror     dat7,#17            ' $00cC_BbAa shifted right x 1
                    movi    dat7,ina            ' $DDcC_BBAa
                    or      outa,_CLK_MASK
                    rcl     dat7,#1             ' $DDCC_BBAA restore LSB

                    wrlong  data,bufp
                    add     bufp,#4
                    wrlong  dat1,bufp
                    add     bufp,#4
                    wrlong  dat2,bufp
                    add     bufp,#4
                    wrlong  dat3,bufp
                    add     bufp,#4
                    wrlong  dat4,bufp
                    add     bufp,#4
                    wrlong  dat5,bufp
                    add     bufp,#4
                    wrlong  dat6,bufp
                    add     bufp,#4
                    wrlong  dat7,bufp
                    add     bufp,#4

readLong8_ret       ret

'--------------------------------------------------------------------
' write Burst ... 32 bytes
'
sdramBuffWrite
                    mov     bufp, clineptr      ' get all 32 bytes now

                    rdlong  data,bufp
                    add     bufp,#4

                    call    #send_ADDRESS       ' activate address/bank

                    movs    outa,data           ' First byte for write
                    shr     data,#8
                    andn    outa,#$100          ' ensure A0 clear for write command?

                    or      dira,#DBPIN_MASK    ' dbus output for write

                    andn    outa,_DCMD_WRITE_NOT' set WRITE command from NOP
                    mov     phsa,_PHSA8         ' edge offset each enable
                    andn    outa,_CLK_MASK      ' turn on clock now for CAS delay time
                    mov     phsa,_PHSA4         ' clock edge offset
                    or      outa,_DCMD_NOP      ' set NOP after write
                    movs    outa,data           ' A0 has no impact after WRITE cmd
                    shr     data,#8
                    movs    outa,data           
                    shr     data,#8
                    movs    outa,data           
                    or      outa,_CLK_MASK

                    call    #sendLong1
                    call    #sendLong2
                    call    #sendLong4

#ifdef LINELEN_128
                    call    #sendLong4
                    call    #sendLong4

                    call    #sendLong4
                    call    #sendLong4
                    call    #sendLong4
                    call    #sendLong4
#elseifdef LINELEN_64
                    call    #sendLong4
                    call    #sendLong4
#endif

                    call    #burstTerminate

                    andn    dira,#DBPIN_MASK    ' leave dbus as input for read

sdramBuffWrite_ret  ret

sendLong2
                    rdlong  data,bufp
                    add     bufp,#4
                    mov     phsa,_PHSA2         ' clock edge offset
                    andn    outa,_CLK_MASK
                    movs    outa,data           ' next byte for write
                    shr     data,#8
                    movs    outa,data           
                    shr     data,#8
                    movs    outa,data           
                    shr     data,#8
                    movs    outa,data           
                    or      outa,_CLK_MASK
sendLong1
                    rdlong  dat1,bufp
                    add     bufp,#4
                    mov     phsa,_PHSA2         ' clock edge offset
                    andn    outa,_CLK_MASK
                    movs    outa,dat1           ' next byte for write
                    shr     dat1,#8
                    movs    outa,dat1           
                    shr     dat1,#8
                    movs    outa,dat1           
                    shr     dat1,#8
                    movs    outa,dat1           
                    or      outa,_CLK_MASK
sendLong2_ret
sendLong1_ret       ret

sendLong4
                    rdlong  data,bufp
                    add     bufp,#4
                    rdlong  dat1,bufp
                    add     bufp,#4
                    rdlong  dat2,bufp
                    add     bufp,#4
                    rdlong  dat3,bufp
                    add     bufp,#4

                    mov     phsa,_PHSA2         ' clock edge offset
                    andn    outa,_CLK_MASK

                    movs    outa,data           ' next byte for write
                    shr     data,#8
                    movs    outa,data           
                    shr     data,#8
                    movs    outa,data           
                    shr     data,#8
                    movs    outa,data           
                    nop
                    movs    outa,dat1           ' next byte for write
                    shr     dat1,#8
                    movs    outa,dat1           
                    shr     dat1,#8
                    movs    outa,dat1           
                    shr     dat1,#8
                    movs    outa,dat1           
                    nop
                    movs    outa,dat2           ' next byte for write
                    shr     dat2,#8
                    movs    outa,dat2           
                    shr     dat2,#8
                    movs    outa,dat2           
                    shr     dat2,#8
                    movs    outa,dat2           
                    nop
                    movs    outa,dat3           ' next byte for write
                    shr     dat3,#8
                    movs    outa,dat3           
                    shr     dat3,#8
                    movs    outa,dat3           
                    shr     dat3,#8
                    movs    outa,dat3           
                    or      outa,_CLK_MASK
sendLong4_ret       ret

burstTerminate
                    andn    outa,_DCMD_BTERM_NOT' andn with NOP burst terminate
                    andn    outa,_CLK_MASK      ' turn on clock
                    mov     phsa,_PHSA4         ' clock offset guarantees clock for BURST TERM
                    or      outa,_DCMD_NOP      ' send NOP
                    call    #delay2
                    or      outa,_CLK_MASK      ' turn off clock
burstTerminate_ret  ret

'====================================================================

fillpasm            long 0 [$1ef-$]
endpasm             fit $1ef

