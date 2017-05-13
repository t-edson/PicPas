{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program LCD8bit; 
uses PIC16F84A;  
var
  pin1	: bit absolute PORTB.1;
  pin2	: bit absolute $0.0;
var 
  n: byte;
begin
  while true do begin
	  n  := 8;
  end;
//  SetAsOutput(pin); 
  pin1 := 1;
	mapVarTo(pin2, pin1);
  pin2 := 1;
  SetAsInput(pin1);
  SetAsInput(pin2);
end.

///***************************************************************************//
