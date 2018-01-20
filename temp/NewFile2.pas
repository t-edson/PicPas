{$PROCESSOR PIC16F877A}
{$FREQUENCY 20MHZ }
{$OUTPUTHEX "aaa.hex"}
procedure proc1;
var x: dword;
begin
  x := 0;
end; 

type tarr = array[95] of byte;
var
  a, b, c: tarr;
  x: dword;
begin
  a[0] := 1;
  b[0] := 1;
  c[0] := 1;
  x := 1;
end.

