{Rutina de verificación para operaciones básicas con datos de 
tipo BIT. 
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var
  pinLed: bit absolute GPIO.0;
procedure bien;
begin
  pinLed := 1;
  delay_ms(20);
  pinLed := 0;
  delay_ms(40);
end;
procedure Mal;
begin
  pinLed := 1;
  delay_ms(2000);
  pinLed := 0;
  asm SLEEP end
end;
var 
  a, b: bit;
  vbyte: byte;
begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica XOR ///////////////////
  //////////////////////////////////////////////////////////
	//Constantes
  if 0 xor 0 = 0 then bien else mal end;
  if 0 xor 1 = 1 then bien else mal end;
  if 1 xor 0 = 1 then bien else mal end;
  if 1 xor 1 = 0 then bien else mal end;

	//Variables
  a := 0; b := 0;
  if a xor b = 0 then bien else mal end;
  a := 0; b := 1;
  if a xor b = 1 then bien else mal end;
  a := 1; b := 0;
  if a xor b = 1 then bien else mal end;
  a := 1; b := 1;
  if a xor b = 0 then bien else mal end;

	//Variables y constantes
  a := 0; 
  if a xor 0 = 0 then bien else mal end;
  if 0 xor a = 0 then bien else mal end;
  if a xor 1 = 1 then bien else mal end;
  if 1 xor a = 1 then bien else mal end;
  a := 1; 
  if a xor 0 = 1 then bien else mal end;
  if 0 xor a = 1 then bien else mal end;
  if a xor 1 = 0 then bien else mal end;
  if 1 xor a = 0 then bien else mal end;

	//Lógica invertida
  a := 0; b := 0;
  if a xor not b = 1 then bien else mal end;  //lógica invertida
  if not a xor b = 1 then bien else mal end;  //lógica invertida
  if (not a xor not b) = 0 then bien  else mal end;  //lógica invertida

  //////////////////////////////////////////////////////////
	//////////////// Operaciones Mixtas // //////////////////
  //////////////////////////////////////////////////////////
  //Operaciones con la misma variable
  a := 0;
  a := a;
  if (a=a) and (a=0) then bien else mal end;
  a := 1;
  a := a;
  if (a=a) and (a=1) then bien else mal end;
   
  a := 0;
  a := not a;
  if (a=1) then bien else mal end;

  vbyte.bit7 := 0; 
  vbyte.bit7 := vbyte.bit7;
  if (vbyte.bit7=vbyte.bit7) and (vbyte.bit7=0) then bien else mal end;

  vbyte.bit7 := 1; 
  vbyte.bit7 := vbyte.bit7;
  if (vbyte.bit7=1) then bien else mal end;

  vbyte.bit7 := 0; 
  vbyte.bit7 := not vbyte.bit7;
  if vbyte.bit7 = 1 then bien else mal end;
  
  //Oepraciones con dos variables    
  a := 0; b := 0;
  if (a = 0) and (b = 0) then bien else mal end;
  if (not a and not b) = 1 then bien else mal end;

  a := 1; b := 0;
  if (a = 1) and (b = 0) then bien else mal end;
  if (a and not b) = 1 then bien else mal end;


  a := 0; b := 0;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  a := 1; b := 0;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  a := 1; b := 1;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;

end.

