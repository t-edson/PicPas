{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
var  //simple type declarations
	vbyte: byte;
  vword: word;
  aword: word absolute vword;
  procedure proc1;
  begin
    
  end; 
begin
  aword := $2010;
//  vword.low.bit0 := 1;
  proc1;
end.

