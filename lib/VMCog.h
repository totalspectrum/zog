#ifndef VMCOG
#define VMCOG

// Conditional compilation controls - use these to configure VMCOG for your platform
// Currently PROPCADE, TRIBLADE_2, XEDODRAM and HYDRA are supported. More support soon!
#define EXTRAM
#ifdef EXTRAM
//#define PROPCADE
//#define FLEXMEM
//#define MORPHEUS_SPI
//#define MORPHEUS_XMM
//#define MORPHEUS3_SPI
//#define PLCG
//#define DRACBLADE
//#define RAMBLADE
#define TRIBLADE_2
//#define XEDODRAM
//#define HYDRA
#else
#warning External Ram access disabled - only use memory up to WS_SIZE
#endif

// VMCOG exported constants
#define PAGESIZE   512
#define TLBENTRIES 128
#define MEMSIZE    PAGESIZE * TLBENTRIES

// VMCOG commands
#define FLUSHVM    128 // Clear all page table entries, resets VMCOG
#define DUMPVM     129 // Dump all page table entries to specified hub address
                       // (NOTE: needs 1KB ie 256 LONG's)

#define READVMB    130 // Read a byte/word/long from VM
#define READVMW    131 // Read a byte/word/long from VM
#define READVML    132 // Read a byte/word/long from VM

#define WRITEVMB   133 // Write a byte/word/long to VM
#define WRITEVMW   134 // Write a byte/word/long to VM
#define WRITEVML   135 // Write a byte/word/long to VM

#define VIRTPHYS   136 // Get what page a virtual location is mapped to, returns 0 if not in working set
#define PHYSVIRT   137 // Get what virtual page a physical page represents, return 0 if not in working set

#define LOCKPAGE   138 // Lock N pages so they can't be swapped, takes vaddr and num pages as argument, returns vaddr
#define UNLOCKPAGE 139 // Unlocked a page locked into memory, takes vaddr and page count

#ifdef PROPCADE
// SPI RAM Driver constants
// PropCade version - bit masks for the pins
#define CS          1
#define CLK         2
#define MOSI        4
#define MISO        8
#endif

#ifdef HYDRA
// HYDRA HX512 Driver constants
// Choose one:
//#define HYDRA_FAST // For 10ns SRAM chip (like mine ;^)
#define HYDRA_SAFE   // For 55ns SRAM chip (? not sure about speed of other existing variants)
#endif


// Start of firmware blob for vmcog
extern int _binary_vmcog_dat_start;

class VMCog
{
  public:
    void start(int* mailbox, int lastp, int nump);

    int rdvbyte(int adr);

    int rdvword(int adr);

    int rdvlong(int adr);

    void wrvbyte(int adr, int dt);

    void wrvword(int adr, int dt);

    void wrvlong(int adr, int dt);

    int rdfbyte(int adr);

    void wrfbyte(int adr, int dt);

    void flush();

    void look(int adr);

    int getPhysVirt(int vaddr);

    int getVirtLoadAddr(int vaddr);

    int getVirtPhys(int adr);

    int lock(int vaddr, int pages);

    int Unlock(int vaddr, int page);

  private:
    volatile int* cmdptr;         // Volatile,  pointers into a
    volatile int* vaddrptr;       // mailbox shared wuth a 
    volatile int* dataptr;        // different CPU.
    int  fakebox[4];
    int* fdataptr;
    int* fcmdptr;

#ifdef XEDODRAM
    // xmailbox is a command and data word
    // command is %CCCC_LLLL_LLLL_AAAA_AAAA_AAAA_AAAA_AAAA
    // C is Command bits up to 16 commands
    // L is Length bits up to 256 bytes
    // A is Address bits up to 1MB address range
    // data is interpreter based on command word context
    // data is a pointer in case of buffer read/write
    // data is a long/word/byte in other command cases
    int* xmailbox;

/*  FIXME What to do about this?
OBJ
  xm : "XEDODRAM_1MB"
*/
#endif

};
#endif

