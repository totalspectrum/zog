#ifndef FULLDUPLEX_SERIAL_PLUS
#define FULLDUPLEX_SERIAL_PLUS


// The firmware blob for FullDuplexSerialPlus.
extern int _binary_Zog_FullDuplexSerialPlus_dat_start;

class FullDuplexSerialPlus
{
    public:
        FullDuplexSerialPlus();
        FullDuplexSerialPlus(int rxpin, int txpin, int mode, int baudrate);
        void stop();
        void tx(int txbyte);
        int rx();
        void rxflush();
        int rxcheck();
        int rxtime(int ms);

    private:
        // ID of the Cog that runs FullDuplexSerial firmware.
        int cog;                   

        // This stuct data is shared with the Cog that runs the
	// FullDuplexSerial firmware (PASM code).
	// Do not change the layout of this struct without
	// appropriate changes to the FullDupexSerial PASM code.
        struct                   
        {
	    // 9 contiguous LONGS MUST be followed by tx/rx buffers
	    int   rx_head;           
            int   rx_tail;
            int   tx_head;
            int   tx_tail;
            int   rx_pin;
            int   tx_pin;
            int   rxtx_mode;
            int   bit_ticks;
            char  rx_buffer[16];     // Transmit and receive buffers.
            char  tx_buffer[16];
        } shared;

  protected:
	int start(int rxpin, int txpin, int mode, int baudrate);
};
#endif
