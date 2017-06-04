{$PROCESSOR PIC16F84}
//{$FREQUENCY 1000Khz}
{Description of the program.}
program nombre;
var 
   x: byte;
  n: bit;
  w: word;
begin
  x := 1; 
//  delay_ms(1);
  asm 
    MOVLW 0
	  MOVWF w.high
    MOVLW 1
	  MOVWF w.low
  end
end.
