'Example of two tasks in Spin.
'
CON
  _clkmode        = xtal1 + pll16x
  _xinfreq        = 6_553_600

  OFF = 0
  ON = 1

VAR
  'Task state variables
  byte state0
  byte state1

  'Millisecond counter
  long ms

PUB start
  'Outputs, P0 for task0, P1 for task1
  OUTA := %00
  DIRA := %11

  'Repeatedly run each tasks method every 1ms
  repeat
    task0
    task1
    waitcnt(clkfreq / 1000 + cnt) '1ms delay
    ms++

'N.B. Tasks may not hang up for long periods in repeat loops
'     Rather they would peform one iteration of a loop on each call
'     remembering the task state and iteration count ready for the next call.

'Task 0 flashes LED 0 every second
PRI task0
  case state0
    OFF:
      if (ms // 500) == 0
        DIRA := %01
        state0 := ON
    ON:
      if (ms // 500) == 0
        DIRA := %00
        state0 := OFF
    OTHER:
      'WTF?

'Task 1 flashes LED 1 twice every second
PRI task1
  case state1
    OFF:
      if (ms // 250) == 0
        DIRA := %10
        state1 := ON
    ON:
      if (ms // 250) == 0
        DIRA := %00
        state1 := OFF
    OTHER:
      'WTF?


