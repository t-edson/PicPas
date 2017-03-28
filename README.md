PicPas 0.4.3
=============
Pascal compiler for PIC microcontrollers.

NOTE: This compiler is still in alpha phase and has limited functionality.

PicPas is a simple compiler, written in Lazarus, which generates executable code for midrange PIC microcontrollers (the 16F series).

Currently, it only supports types byte, boolean and word (limited support).

No additional libraries or software required to compile. PicPas generates the * .hex file directly.

It includes a simple IDE to facilitate the development of programs.

The code optimization options are not well developed but the code obtained is fairly compact, as that could generate any commercial compiler.

As an example the following code, is to blink an LED on port B:

```
{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 4 MHZ }
{$PROCESSOR PIC16F84}
program BlinkLed;
const
  HIGH = true;
  LOW = false;
var
  STATUS: BYTE absolute $03;
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  RP0 : boolean @STATUS.5;
  RP1 : boolean @STATUS.6;
  pin: boolean absolute PORTB.4;
begin                          
  RP0 := HIGH;
  TRISB := 0;   //all outputs
  RP0 := LOW;
  PORTB := 0;   //init
  delay_ms(1000);  //wait
  while true do begin
    delay_ms(300);
    pin := HIGH;
    delay_ms(300);
    pin := LOW;
  end;
end.
```

