PicPas 0.6.9
=============
Multi-platform Pascal cross-compiler for Microchip PIC16F microcontrollers.

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/04/PicPas.png "Título de la imagen")

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
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
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
 :=                    2
 ```

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
Bit()          Convert a byte to a bit.
Word()         Convert a byte to a word.
SetAsInput()   Set a 8-bits port or a pin as an input.
SetAsOutput()  Set a 8-bits port or a pin as an output.
```

### Procedure and Functions

PicPas use de Modula-2 syntax for procedure and fucntions:

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
begin
  //Low level clear
  asm 
    CLRF byte1
    CLRF car1
    BCF bit1
    BCF bol1
    BCF word1.Low
    BCF word1.high
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

#### $FREQUENCY

Specify the clock frequency, in MHz or KHz. It's used for the delay routines Example:

```
{$FREQUENCY 1000Mhz}
```

The frequencies supported by now are:

1MHz, 2Mhz, 4Mhz, 8MHz, 10MHz o 12MHz.


#### $MODE

Specify the syntax mode, used by the compiler. The allowed values are:

{$MODE PICPAS} -> Default mode. Use the new syntax for the control structures.

{$MODE PASCAL} -> Clasic Pascal mode. Use the common Pascal syntax for the control structures.


## Limitations

•	Only basic types are implemented: bit, byte, char, boolean and word (limited support).

•	Cannot declare arrays or records.

•	No recursion implemented, Because of the limited hardware resources, available in PIC devices.

•	No float point implemented.

•	No interruption support.

Some of these limitations must be solved in next versions.

## Source Code

The source code is in the folder /Source.

To compile PicPas, it's needed to have the following libraries:

* SynFacilUtils
* MisUtils
* MiConfig
* PicUtils 
* Xpres 

All of them, must be availables on the Web. Check the versions used.

PicPas has been compiled, using the version 1.6.2 of Lazarus. Tested in Windows and Ubuntu.

To have more information about the compiler, check the Technical Documentation (Only in spanish by now).

## Development

PicPas is a free software (GPL license) and it's opened for the collaboration of anyone who is interested. 

There is still, much work for development or documentation, so any help will be appreciated.
