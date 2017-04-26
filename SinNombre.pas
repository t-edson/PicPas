{Description of the program.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program nombre;
var 
PORTB: BYTE absolute $06;
TRISB: BYTE absolute $86;
c: bit absolute POrTB.0;
a: bit absolute POrTB.1;
b: bit absolute POrTB.2;
begin
	TRISB  := %00001110;
	 while  true do  begin
//	  c  :=  a  and b;
  c := a xor b;
//    c := (not a and b) or (a and not b);
    delay_ms(100);
  end;
end.
