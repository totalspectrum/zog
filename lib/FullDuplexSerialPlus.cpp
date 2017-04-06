/****************************************************************************************

FullDuplexSerialPlus.cpp

A C++ implementation of the Spin language parts of FullDuplexSerialPlus.spin a
serial driver for the Parallax Inc. Propeller micro-controller.

This implements the configuration and loading of the FDS firmware into a COG and subsequent
interaction with the FDS COG through shared memory.

The FDS driver firmware (PASM code) is compiled and extracted from 
FullDuplexSerialPlus.spin source using the BSTC compiler (See accompanying Makefile).


See end of file for terms of use.
****************************************************************************************/
#include <stdio.h>

#include "propeller.h"

#include "FullDuplexSerialPlus.h"

FullDuplexSerialPlus::FullDuplexSerialPlus()
{
}

FullDuplexSerialPlus::FullDuplexSerialPlus(int rxpin, int txpin, int mode, int baudrate)
{
    start(rxpin, txpin, mode, baudrate);
}

int FullDuplexSerialPlus::start(int rxpin, int txpin, int mode, int baudrate)
{
    /*
    Starts serial driver in a new cog

      rxpin - input receives signals from peripheral's TX pin
      txpin - output sends signals to peripheral's RX pin
      mode  - bits in this variable configure signaling
                 bit 0 inverts rx
                 bit 1 inverts tx
                 bit 2 open drain/source tx
                 bit 3 ignore tx echo on rx
      baudrate - bits per second

      Returns false if no cog is available.
    */
    //stop();
    shared.rx_head = 0;
    shared.rx_tail = 0;
    shared.tx_head = 0;
    shared.tx_tail = 0;
    shared.rx_pin = rxpin;
    shared.tx_pin = txpin;
    shared.rxtx_mode = mode;
    shared.bit_ticks = clkfreq / baudrate;
    int* entry = &_binary_Zog_FullDuplexSerialPlus_dat_start;
    cog = cognew(entry, (int*)&shared) + 1;
    return(cog);
}

void FullDuplexSerialPlus::stop()
{
    // Stops serial driver - frees a cog
    if (cog)
    {
        cogstop(cog - 1);
        cog = 0;
    }
    longfill(&shared.rx_head, 0, 9);
}

void FullDuplexSerialPlus::tx(int txbyte)
{
    // Sends byte (may wait for room in buffer)

    // Do nothing while waiting
    while (shared.tx_tail == ((shared.tx_head + 1) & 0x0F));

    //tx_buffer[tx_head] = txbyte;   // We want to do this but...
    char *p = &shared.tx_buffer[shared.tx_head];   // ...this mess is because our COG firmware lives in 
    p = (char*)((int)p ^ 0x03);                    // Propeller HUB land with opposite endianness
    *p = txbyte;
    shared.tx_head = (shared.tx_head + 1) & 0x0F;
    if (shared.rxtx_mode & 0x08)
    {
        rx();
    }
}

int FullDuplexSerialPlus::rx()
{
    // Receives byte (may wait for byte)
    // rxbyte returns $00..$FF
    int rxbyte;
    while ((rxbyte = rxcheck()) < 0);
    return(rxbyte);
}

void FullDuplexSerialPlus::rxflush()
{
    // Flush receive buffer
    while (rxcheck() >= 0);
}

int FullDuplexSerialPlus::rxcheck()
{
    // Check if byte received (never waits)
    // rxbyte returns -1 if no byte received, $00..$FF if byte
    int rxbyte;
    rxbyte = -1;
    if (shared.rx_tail != shared.rx_head)
    {
        // rxbyte = rx_buffer[rx_tail];   // We want to do this but...
	char *p = &shared.rx_buffer[shared.rx_tail];   // ...this mess is because our COG firmware lives in 
        p = (char*)((int)p ^ 0x03);      // Propeller HUB land with opposite endianness
        rxbyte = *p;
        shared.rx_tail = (shared.rx_tail + 1) & 0x0F;
    }
    return(rxbyte);
}

int FullDuplexSerialPlus::rxtime(int ms)
{
    // Wait ms milliseconds for a byte to be received
    // returns -1 if no byte received, $00..$FF if byte
    int rxbyte;
    rxbyte = -1;
    /*
      t = cnt;
      repeat until (rxbyte := rxcheck) => 0 or (cnt - t) / (clkfreq / 1000) > ms;
    */
    return(rxbyte);
}

/****************************************************************************************
                           TERMS OF USE: MIT License
Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
****************************************************************************************/
