{Description of the program.}
program nombre;
type   //Tipos punteros
  byteptr = ^byte;
  wordptr = ^word;
var  //Variables para punteros
  ptrByte1, ptrByte2: byteptr;
  ptrWord1: wordptr;
  w: word;
begin
  ptrWord1:= @w;
  ptrWord1^ := word($FF);
//  if x = word($ff) then good else bad end;
  
end.
