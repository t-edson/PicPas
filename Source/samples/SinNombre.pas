{programa de demostración de uso}
{$PROCESSOR PIC16F690 }
{$FREQUENCY 8 MHZ }
{$OPTIMIZE SPEED }
{$POINTERS LARGE }
{$CONFIG FCMEN = OFF, IESO = OFF, OSC = INTOSCIO, WDT = OFF }
{$DEFINE PK2} // Generate code for testing on PicKit2 evaluation board, else it is for the target board.
{$DEFINE USE_SERIALPORT_OUTPUT} // Generate code for Serial Output only

program aaa;
var a: byte;
 b,c: word;
 r,s: boolean;
begin
while true do begin
a:=1;
end;
b:=$123;
b:=b+$100;
r := false;
s := true;
//delay_ms(100);
//b:=(1+b)+a+3;
end;

