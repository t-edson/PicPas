{Sample of using ASM}
program aaa;
var
 a,b: byte;
 c: word @123;

begin
  repeat
    asm
      MOVLW %0101
      ADDWF $0C, f
    end
    inc(a);
  until a=0;
  a := (a xor %1);
end.
