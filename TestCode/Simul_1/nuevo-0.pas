{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses PIC16F877A;
var  //simple type declarations
	bit1: bit absolute $20.0;
  bit2: bit absolute $A0.0;
begin
//  SetAsOutput(pinLed);
  //Access to bytes of word
  bit1 := 1;
  bit2 := bit1;
//	if bit1 = bit2 then bien else mal end;
end.

