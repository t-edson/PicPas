{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
uses UnitTest, PIC16F877A;
var  //simple type declarations
	vbyte: byte;
  a : dword;
begin
  SetAsOutput(pinLed);
  pinLed := 0;
 
  //coConst_Expres
//  {$INFO 'Hello'}
  a := $FF; 
  if dword(1) + (a+dword(1)) = dword($101) then
    good;
  else 
    bad;
  end;

  //coExpres_Const
  a := $FF; 
  if (a+dword(1)) + dword(1) = dword($101)   then good else bad end; 
  (a+dword(1)) + dword(1); 
//¿Hay forma de verificar (¿y se cumple siempre?) que Los registros de pila, se están poniendo en "Libre", al final de una expresión?  
end.
