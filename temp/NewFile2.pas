{Description of the program.}
{$PROCESSOR PIC16F84}
program nombre;
var a, b, d: byte;
begin
  //a div b
  d := 0;
  while a>0 do
    a := a - b;
    inc(d);
  end;
end.
