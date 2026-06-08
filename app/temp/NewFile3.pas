{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$MODE PICPAS}
 
program NestedCycle;
uses PIC16F877A;
var
  a, b, c, d: byte;
 
begin
  d := 0;
  for a := 1 to 4 do
    for b := a + 1 to 5 do
      for c := b + 1 to 6 do
        inc(d);
      end;
    end;
  end;
end.
