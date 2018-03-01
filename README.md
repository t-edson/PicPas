## Donate to the project

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=7LKYWG9LXNQ9C&lc=ES&item_name=Tito%20Hinostroza&item_number=2153&no_note=0&cn=Dar%20instrucciones%20especiales%20al%20vendedor%3a&no_shipping=2&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted)

PicPas 0.8.4
============

Multi-platform Pascal cross-compiler for Microchip PIC16F microcontrollers.

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/04/PicPas.png "PicPas IDE")

PicPas is a Pascal compiler, written in Lazarus, which generates executable code for midrange PIC microcontrollers (the 16F series).

No additional libraries or software required to compile. PicPas generates the *.hex file directly.

PicPas works with a simplified version of the Pascal language, that has been adapted to work with limited resources small devices.

Currently, it only supports basic types. 

It includes a very complete IDE to facilitate the development.

The compiler includes optimization options so the code obtained is fairly compact, as that could generate any commercial compiler.

## Installation

PicPas doesn't need installation, and have not dependencies, except the commons of the operative system, where it's runnig.

To run, it's only needed to download the folder from GitHub. There is a compiled  Windows-32 version (PicPas-win32.exe) and a Ubuntu version (PicPas-linux).

If it's required other platform, it need to be compiled from the source code.

When starting, PicPas could generate warning messsages, if not needed folders exist.

## Hello World

As an example the following code, is to blink a LED on port B:

```
{Sample program to blink a Led on PORTB.7}
program BlinkLed;
{$PROCESSOR PIC16F84A}
{$FREQUENCY 8MHZ}
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: bit absolute PORTB.7;
begin                          
  TRISB := 0;   //all outputs
  while true do 
    delay_ms(1000);
    pin := not pin;
  end;
end.
```

PicPas has not special libraries yet, so the special register names, must be defined in the program, or a unit containing this definitions can be created.

## IDE

PicPas includes an IDE integrated to the compiler, to help on developing the programs.

Some features of the IDE are:

•	Cross-platform.

•	Multiple editors windows.

•	Syntax highlighting, code folding, word, and line highlighting for Pascal and ASM.

•	Code completion, and templates for the common structures IF, REPEAT, WHILE, …

•	Shows the assembler code and the resources used.

•	Support for themes (skins).

•	Code tools for completion and navigation.

•	Check syntax in ¡¡¡REAL TIME!!!.

•	Several setting options.

•	Translated to english/spanish and german.

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/06/PicPas-0.7_en.png "PicPas with dark skin")

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/11/PicPas-Linux.jpg "PicPas for Ubuntu")


## Debugger/Simulator

PicPas includes a graphical debugger/simulator for instructions of the Mid-Range core:

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/11/PicPas-simulator.png "PicPas debugger-simulator")

To control the execution, the following keys can be used:

F5 -> Set a breakpoint in the current position of  the assembler code.
F6 -> Reste the device.
F7 -> Step by step into subroutine.
F8 -> Step by step over subroutine.
F9 -> Run the program in real time.


## Optimization Comparison

PisPas has been compared in two code optimization competition against the best profesional compilers for PIC microcontrollers, obtaining the first place in both.


Firts competition

Compiling a simple light sequence:
https://github.com/AguHDz/PicPas-Librerias_y_Programas/tree/master/Comparacion%20PicPas-Otros%20Compiladores

Result:

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/08/21106414_961808527291234_7715586770614634466_n.jpg "PicPas comparison 1")


Second competition

Compiling a digital clock using I2C and the DS1307:
https://www.facebook.com/groups/electronicaymicrocontroladores/permalink/1812269192135162/

Result 

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/11/23172519_996093467196073_6412663861585566494_n.jpg "PicPas comparison 2")



## Language Reference

### Program structure

```
program <Name>;  //optional
uses
  //Units declarations

const
  //Constants declarations

var
  //Variables declarations

//<Procedures declaration>

begin
  //Main program body
end.
```

### Unit structure

```
unit <name>;
interface
uses
  //units declaration
const
  //Constant declaration
var
  //Variable declaration

//Procedures declaration

implementation

uses
  //units declaration
const
  //Constant declaration
var
  //Variable declaration

//Procedures implementation

end.
```

### Operators

```
Operator            Precedence
=================== ==========
 NOT, sign “-“         6
 *, DIV, MOD, AND      5
 +, -, OR, XOR         4
 =, <>, <, <=, >, >=   3
 := +=                 2
 ```

### Types

```
Type           Size
============== ==========
 bit           1 bit
 boolean       1 bit
 byte          1 byte
 char          1 byte
 word          2 bytes
 dword         4 bytes
 ```
Numerical types are all unsigned.

### Variables

Variables are defined with the VAR keyword:

```
var
  var1 : byte;
  var2 : bit;
  large_name_variable: boolean;
```

Variables can be defined too, at an absolute memory address:

```
var
  PORTB: BYTE absolute $06;
  pin0: bit; absolute $06.0;
  pin1: boolean; absolute PORTB.bit1;
```

Bit access can be performed too, using fields:

```
  var_byte.bit0 := 1;
  var_byte.bit7 := 0;
```

Specific byte of a word, can be access using fields:

```
  word_var.Low := $ff;
  word_var.High := $ff;
```

### Control structures

PicPas doens't follow the common Pascal syntax. Instead, a new Modula-2, style syntax is implemented.

The common control structures have the following forms:

```
IF <condition> THEN 
  <block of code>
END;

IF <condition> THEN 
  <block of code>
ELSE
  <block of code>
END;

IF <condition> THEN 
  <block of code>
ELSIF <condition> THEN 
  <block of code>
ELSE
  <block of code>
END;

WHILE <condition> DO
  <block of code>
END;

REPEAT
  <block of code>
UNTIL <condition>;

FOR  <variable> := <start-value> TO <end-value> DO 
  <block of code>
END;
```

### System Functions

System functions are always available in code. They don't need to be defined or included in a unit.

```
FUNCTION       DESCRIPTION
============== =================================================
delay_ms()	   Generate a time delay in miliseconds, from 0 to 65536.
Inc()          Increase a variable.
Dec()          Decrease a varaible.
SetBank()      Set the current RAM bank.
Exit()         Exit from a procedure or end the program.
Ord()          Convert a char to a byte.
Chr()          Convert a byte to a char.
Bit()          Convert an expression to a bit expression.
Byte()         Convert an expression to a byte expression.
Word()         Convert an expression to a word expression.
DWord()        Convert an expression to a dword expression.
SetAsInput()   Set a 8-bits port or a pin as an input.
SetAsOutput()  Set a 8-bits port or a pin as an output.
```

### Procedure and Functions

PicPas use the Modula-2 syntax for procedures and functions:

Proedures are declared in the common Pascal syntax:

```
  procedure proc2(par1: byte);
  begin
    if par1 = 0 then 
      exit;
    else
      par1 := 5;
    end;  
  end;
```

Functions are declared the same, but indicating the type to return:

```
procedure TheNext(par1: byte): byte;
begin
  exit(par1 + 1);
end;
```

The return value is indicated with the exit() instruction.

When using in procedures parameters, a REGISTER parameter can be included:

```
procedure QuickParameterProc(register regvar: byte);
begin
  //Be carefull if put some code here
  PORTB := regvar;
end;
```

REGISTER parameters are fast, because they use the W register, so only one REGISTER parameter can be used. 
As REGISTER parameter is stored in W register, any operation using the W register, could lose its value, so the first operation in a procedure, using a REGISTER parameter must be read this parameter.

### Interrupts

To manage interrupts, PicPas let us to define a special kind of Procedure:

```
  procedure My_ISR; interrupt;
  begin

    //ISR code

  end;
```

The name of the procedure is not important, but the declaration must be followed but the reserved word INTERRUPT.

Only one INTERRUPT procedure is allowed in a program.

When PicPas compile an INTERRUPT procedure, some special criterias are considered:

1. Are always compiled starting in the address 0x0004.
2. A RETFIE instruction is added to the end of the routine.
3. No additional bank switching instructions are generated at the beginning of the procedure. It is the responsibility of the programmer to properly handle the banks within the routine.

INTERRUPT procedures don't save the value of registers or the control flags. This should be done manually.


### ASM blocks

PicPas have a complete support for inserting ASM code inside the Pascal source. 

ASM blocks must be included between the delimiters ASM and END:

```
procedure DoSomething;
begin
  x := 10;
  asm
    ;Add 2 to the address $20 
    MOVLW 2
    ADDWF $20, F
  end
end;
```

ASM blocks are not instructions, that's why they are not finished with ";". It lets the ASM block, to be included in almost any place of the source code, like a comment.

WARNING: Changing the RAM bank, inside an ASM block, can generate errors in compilation or in the code compiled. PicPas know always the current RAM bank, when compiling, but is not aware of the changes can be made inside ASM blocks.

Absolute and relative Labels can be used too:

```
asm 
  GOTO $+1   ;jump one position forward
end
```

```
asm 
  ;infinite loop
label:
  NOP
  GOTO label
end
```

Program variables can be accessed, using his common name:

```
var 
 byte1: byte; 
 car1: char; 
 bit1: bit;
 bol1: boolean; 
 word1: word;
 dword1: dword;
begin
  //Low level clear
  asm 
    CLRF byte1
    CLRF car1
    BCF bit1
    BCF bol1
    CLRF word1.Low
    BCF word1.high.bit1
	CLRF dword1.low
	CLRF dword1.high
	CLRF dword1.extra
	CLRF dword1.ultra
  end
end.
```

Constant can be accessed too, using the same way. 

It's possible to use the directive ORG inside a ASM block, too:

```
  asm 
    org $-2
  end
  vbit := 1;
```

The address in ORG, can be absolute or relative. 

WARNING: Changing the PC pointer with ORG, can generate errors in the compilation or in the code compiled.

## Pointers

Pointers are supported in PicPas, only for addresses going from $00 to $FF (1 byte size), thus they can cover only the RAM memory in banks 0 and 1.

Pointers must be declared usin first, a type declaration in the common Pascal style:

```
type
  ptrByte: ^Byte;
  ptrByte: ^Word;
var
  pbyte: ptrByte;
  pword: ptrWord;
```

Pointers can be assigned like variables or using addresses form others varaibles:

```
type
  ptrByte: ^Byte;
var
  pbyte: ptrByte;
  m    : byte;
begin
  pbyte := @m;    //Assign address
  pbyte^ := $ff;  //Write value
  //Now “m” is  $ff
end.
```
The operator "@" return the address of a variable.

Pointers support some basic operations:

Assign   :	p1 := p2;
Compare  : 	if p1 = p2 then ...
Increment:	Inc(p);
Decrement:	Dec(p);
Add      :	p1 + p2 + 1
Subtrac  :	p1 - 5

## Directives

The next directives are supported by PicPas:

#### $PROCESSOR

Specify the target device model of the microcontroller. Example:

```
{$PROCESSOR PIC16F628A}
```

The devices supported by now are: 

 PIC12F629
 PIC12F675
 PIC12F629A
 PIC12F675A
 PIC16C63
 PIC16CR63
 PIC16C65
 PIC16C65A
 PIC16CR65
 PIC16F72
 PIC16F83
 PIC16CR83
 PIC16F84
 PIC16CR84
 PIC16F84A
 PIC16F870
 PIC16F871
 PIC16F872
 PIC16F873
 PIC16F873A
 PIC16F874
 PIC16F874A
 PIC16F876
 PIC16F876A
 PIC16F877
 PIC16F877A
 PIC16F887
 PIC16F627A
 PIC16F628A
 PIC16F648A

 New devices can be configured, using directives.
 
#### $FREQUENCY

Specify the clock frequency, in MHz or KHz. Example:

```
{$FREQUENCY 10Mhz}
```

Frequency information is used for:

* The compiler, when needed to generate delays.
* The simulator, for Real Time simulation.

If delays are used in the program, only some frequencies are supported. They are:

1MHz, 2Mhz, 4Mhz, 8MHz, 10MHz, 12MHz, 16MHz or 20MHz.

If frequency is not specified, the default value is 4MHz.

#### $MODE

Specify the syntax mode, used by the compiler. The allowed values are:

{$MODE PICPAS} -> Default mode. Use the new syntax for the control structures.

{$MODE PASCAL} -> Clasic Pascal mode. Use the common Pascal syntax for the control structures.

#### $MSGBOX

Shows a text message in the screen:

{$MSGBOX 'Hello World'} -> Shows the message 'Hello World' in the screen.

{$MSGBOX PIC_MODEL} -> Shows the system variable PIC_MODEL, that is the PIC model defined.

{$MSGBOX PIC_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX 'clock=' + PIC_FREQUEN}  -> Shows the message: "clock=8000000" (if the Frequency was set to 8MHz).

#### $MSGERR

Shows a text message in the screen, with an error icon.

#### $MSGWAR

Shows a text message in the screen, with a warning icon.

#### $CONFIG

Sets the configuration bits of the device.

```
{$CONFIG $3FFD}

{$define _CP_ON       =     0x000F}
{$define _CP_OFF      =     0x3FFF}
{$define _WDT_OFF     =     0x3FFB}
{$define _LP_OSC      =     0x3FFC}
{$define _XT_OSC      =     $3FFD}

{$CONFIG _CP_OFF, _XT_OSC, _WDT_OFF }

{$CONFIG _CP_OFF _XT_OSC _WDT_OFF }
```

#### $INCLUDE

Includes the contents of a external file, into de source code:

```
{$INCLUDE aaa.pas}
{$INCLUDE d:\temp\aaa.txt}
x := {$INCLUDE expression.txt};
```

#### $OUTPUTHEX

Defines the name of the output binary file *.hex.

```
{$OUTPUTHEX myoutput.hex}  // Relative path
{$OUTPUTHEX d:\temp\myoutput.hex}  //Absolute path
```

When relative path is used, the file will be created in the same folder the Pascal program is.

#### $DEFINE

Define symbols or macros

To define a symbol we can do:

```
{$define MY_SYMBOL}
```

Once defined, it can be tested using $IFDEF directive.

To define a macro we can do:

```
{$DEFINE macro=Hello}
```

Then we can expand a macro, in the code, using the way:

{$macro}

Following, there a sample code:

```
{$DEFINE pin_out=PORTB.0}
uses PIC16F84A;
begin
  SetAsOutput({$pin_out});
  {$pin_out} := 1;
end.
```

#### $SET

Set a value for a variable. If variables doesn't exist, it will be created.

```
{$SET pin_out='PORTB.0'}
uses PIC16F84A;
begin
  SetAsOutput({$pin_out});
  {$pin_out} := 1;
end.
```

Variables can be numbers or string.

Variables supports expresions:

```
{$SET a_var = 1 + 2 * another_var + 2 ^ sin(0.5)}
```

Unlike macros, variables values are solved when assigned. Macros values, are solved when macro is referenced.

#### $IFDEF, $IFNDEF, $ELSE, $ENDIF

This directives let us to define conditional compilation blocks:

```
{$DEFINE MyPinOut=PORTB.0}
uses PIC16F84A;
begin
{$IFDEF MyPinOut}
{$ELSE}
  {$DEFINE MyPinOut=PORTB.1}
{$ENDIF}
  SetAsOutput({$MyPinOut});
  {$MyPinOut} := 1;
end.
```

```
{$DEFINE MyPinOut=PORTB.0}
uses PIC16F84A;
begin
{$IFNDEF MyPinOut}
  {$DEFINE MyPinOut=PORTB.1}
{$ENDIF}
  SetAsOutput({$MyPinOut});
  {$MyPinOut} := 1;
end.
```

#### $IF, $IFNOT

This directives let us to define conditional compilation blocks, using expressions:

```
{$IF valor>255}
var x: word;
{$ELSE}
var x: byte;
{$ENDIF}
```

#### $SET_STATE_RAM, $SET_MAPPED_RAM, $CLEAR_STATE_RAM

These directives let us to define the RAM memory hardware state. In conjunction with system variables, they can define custom microcontroller hardware:

```
//Define hardware
{$SET PIC_MODEL='MY_PIC'}
{$SET PIC_MAXFREQ = 1000000}
{$SET PIC_NPINS = 18}
{$SET PIC_NUMBANKS = 2}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}
//Clear memory state
{$SET_STATE_RAM '000-1FF:NIM'}
//Define RAM state
{$SET_STATE_RAM '000-00B:SFR, 00C-04F:GPR'}
{$SET_STATE_RAM '080-08B:SFR, 08C-0CF:GPR'}
//Define mapped RAM
{$SET_MAPPED_RAM '080-080:bnk0, 082-084:bnk0, 08A-08B:bnk0'}
{$SET_MAPPED_RAM '08C-0CF:bnk0'}
```

(*) For more information, check the User Manual.

#### SYSTEM VARIABLES

There are some system variables, accessible from the directives language. They are:
 
{$MSGBOX PIC_MODEL} -> Shows the PIC model defined.

{$MSGBOX PIC_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX PIC_MAXFREQ} -> Shows the Max Clock frequency for the device.

{$MSGBOX PIC_NUMBANKS} -> Shows the RAM banks number for the device.

{$MSGBOX SYN_MODE} -> Shows the syntax Mode of the compiler.

{$MSGBOX CURRBANK} -> Shows the current RAM bank.

(*) To see the complete list, check the User Manual.

## Limitations

•	Only basic types are implemented: bit, byte, char, boolean, word an dword(limited support).

•	Cannot declare arrays or records.

•	No recursion implemented, Because of the limited hardware resources, available in PIC devices.

•	No float point implemented.

Some of these limitations must be solved in next versions.

## Development

PicPas is a free software (GPL license) and it's opened for the collaboration of anyone who is interested. 

There is still, much work for development or documentation, so any help will be appreciated.

## Source Code

The source code of the compiler is in the folder /Source.

To compile PicPas, it's needed to have the following libraries:

* SynFacilUtils
* MisUtils
* MiConfig
* PicUtils 
* t-Xpres 
* UtilsGrilla
* ogEditGraf

All of them, must be availables on the GitHub. Check the versions used.

These libraries don't include package. They are only files in folders that need to be included when compiling PicPas.

PicPas has been compiled, using the version 1.6.2 of Lazarus. Tested in Windows and Ubuntu.

To have more information about the compiler, check the Technical Documentation (Only in spanish by now).

## Libraries

PicPas is a new project and it's still in development and there are not dedicated libraries for the compiler. 

The best repository for libraries and useful code is in: https://github.com/AguHDz/PicPas-Librerias_y_Programas

