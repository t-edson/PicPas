PicPas 0.5.4
=============
Pascal compiler for PIC microcontrollers.

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/04/Sin-título.png "Título de la imagen")

NOTE: This compiler is still in alpha phase and has limited functionality.

PicPas is a simple compiler, written in Lazarus, which generates executable code for midrange PIC microcontrollers (the 16F series).

No additional libraries or software required to compile. PicPas generates the *.hex file directly.

PicPas works with a simplified version of the Pascal language, that has been adapted to work with limited resources small devices.

Currently, it only supports types bit, byte, boolean and word (limited support).

It includes a simple IDE to facilitate the development of programs.

The compiler includes optimization options so the code obtained is fairly compact, as that could generate any commercial compiler.

As an example the following code, is to blink an LED on port B:

```
{Sample program to blink a Led on PORTB.7}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: bit absolute PORTB.7;
begin                          
  TRISB := 0;   //all outputs
  while true do begin
    delay_ms(1000);
    pin := not pin;
  end;
end.```
```

## Source Code

The source code is in the folder /Source.

To compile PicPas, it's needed to have the following libraries:

* SynFacilUtils
* MisUtils
* ConfigFrame 
* PicUtils 
* Xpres 

All of them, must be availables on the Web. Check the versions used.

PicPas has been compiled, using the version 1.6.2 of Lazarus. Tested in Windows and Ubuntu.

To have more information about the compiler, check the Technical Documentation (Only in spanish by now).

## Development

PicPas is a free software (GPL license) and it's opened for the collaboration of anyone who is interested. 

There is still, much work for development or documentation, so any help will be ap	preciated.