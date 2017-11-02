{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
uses PIC16F877A;
type  //tipos equivalentes
	byte1 = byte;
var
  m: byte;
  w: word;
  procedure proc1(x, y: byte1);
  begin  x+y; end;
  
  procedure proc2(w: word);
  begin  w := 1; end; 
  
begin
//  m.bit2;
//  proc1(1,2);
//  proc2(1); 
end.
