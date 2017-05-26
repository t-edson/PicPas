procedure LimpiarByte(b: byte);
begin
  asm
    CLRF b  
  end
end; 
procedure LimpiarBit(n: bit);
begin
  asm
    BCF n  
  end
end; 

var 
 byte1: byte; 
 bit1: bit;
begin
  LimpiarBit(bit1);
  if byte1 <> 0 then
    LimpiarByte(byte1);
  end; 
end.
