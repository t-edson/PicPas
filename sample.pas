program aaa;
var
	out1: bit absolute PORTB.0;
	in1: bit absolute PORTB.1;
	in2: bit absolute PORTB.2;
begin
  while  do begin
	 out1 := in1 and in2; 
   delay_ms(100);
  end;
end.


