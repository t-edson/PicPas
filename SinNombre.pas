 {Prueba Puertos de Entrada y Salida.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program LedBlink;
uses prueba;
const 
  zz, bb = $20;
  ddd = %100 and zz;
var
  PORTB: BYTE absolute $06;
	a, 
  b,
  c: byte;
begin
  b > ddd + 1;
  if true  then begin
	  a  := 1;
  end else begin
	  b  := 0;
  end;

end.

