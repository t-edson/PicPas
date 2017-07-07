program aaa;
var
  PORTB: byte absolute $06; 
  TRISB: byte absolute $86;
	out1: bit absolute PORTB.0;
	in1: bit absolute PORTB.1;
	in2: bit absolute PORTB.6;
begin
  TRISB := %00001110;
  while true do 
	 out1 := in1 xor in2; 
   delay_ms(100);
  end;
end.


