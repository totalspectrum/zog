' A simple test of sdspiqasm.asm

CON

_clkmode        = xtal1 + pll16x
_xinfreq        = 6_553_600

baud            = 115_200                                 'UART baud rate

' microSD pins on TriBladeProp Blade #2
spiDO           = 9
spiClk          = 28
spiDI           = 8
spiCS           = 14

DAT
buffer byte 0[512]


OBJ
   sd  : "zog_sdspiqasm.spin"
   ser : "FullDuplexSerialPlus.spin"

PUB start | r

  ser.start(31,30,0,baud)                               'Start the debug Terminal
  ser.str(string("Testing zog_sdspiqasm.spin"))
  ser.crlf

  ser.str(string("Starting zog_sdspiqasm..."))
  ser.crlf
  r := \sd.start(spiDo, spiClk, spiDi, spiCS)
  ser.str(string("result = "))
  ser.hex(r, 8)

  ser.str(string("Writing block..."))
  ser.crlf
  r := \sd.writeblock(0, @buffer)
  ser.str(string("result = "))
  ser.hex(r, 8)









