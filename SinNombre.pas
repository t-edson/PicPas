{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program LCD8bit;
uses PIC16F84A; 
var
  LCDData : byte absolute PORTB;       // 8 BIT DATA BUS.
  RS      : boolean absolute PORTA.0;  // SELECT REGISTER.
  EN      : boolean absolute PORTA.1;  // ENABLE STARTS DATA READ/WRITE.
  Counter : byte;
procedure proc1;
var
aa: byte;
begin
  
end; 
var n: byte;
begin
  while true do
  begin
	  n  := 8;
    chr(n + ord('0')); 
  end;
end.
///***************************************************************************//
