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

	//Pruebas con IF
	if true then bien else mal end;
	if false then mal else bien end;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////

	//Pruebas con BIT - constante
  if bit(0) = bit(0) then bien else mal end;
  if bit(0) = bit(1) then mal else bien end;
  if bit(1) = bit(1) then bien else mal end;
  if bit(1) = bit(0) then mal else bien end;

  if bit(0) <> bit(0) then mal else bien end;
  if bit(1) <> bit(0) then bien else mal end;

	//Pruebas con BIT - variable
  a := 1;
  b := 0;
  if a = a then bien else mal end;
  if a = b then mal else bien end;
  if a <> a then mal else bien end;
  if a <> b then bien else mal end;

	//Pruebas con BIT - variable - constante
  a := 1;
  b := 0;
  if a = 1 then bien else mal end;
  if a = 0 then mal else bien end;
  if 0 <> b then mal else bien end;
  if 1 <> b then bien else mal end;

	//Pruebas con BIT - expresión
  a := 1;
  b := 0;
  if a = (b and a) then mal else bien end;
  if (b and a) = a then mal else bien end;
  if (a and 1) = 1 then bien else mal end;
  if (a and 1) = 0 then mal else bien end;

  if (a and b) = (b and a) then bien else mal end;
  if (a and b) <> (b and a) then mal else bien end;
	
	//Operación lógica NOT
  if not bit(0) = 1 then bien else mal end;
  if not bit(1) = 0 then bien else mal end;
  a := 0; 
  if not a = 1 then bien else mal end;
  if not a = 0 then mal  else bien end;
  if not a = not a then bien else mal end;

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica AND ///////////////////
  //////////////////////////////////////////////////////////

	//Operación lógica AND - constantes
  if 0 and 0 = 0 then bien else mal end;
  if 0 and 1 = 0 then bien else mal end;
  if 1 and 0 = 0 then bien else mal end;
  if 1 and 1 = 1 then bien else mal end;

	//Operación lógica AND - variables
  a := 0; b := 0;
  if a and b = 0 then bien else mal end;
  a := 0; b := 1;
  if a and b = 0 then bien else mal end;
  a := 1; b := 0;
  if a and b = 0 then bien else mal end;
  a := 1; b := 1;
  if a and b = 1 then bien else mal end;

	//Operación lógica AND - variables y constantes
  a := 0; 
  if a and 0 = 0 then bien else mal end;
  if 0 and a = 0 then bien else mal end;
  if a and 1 = 0 then bien else mal end;
  if 1 and a = 0 then bien else mal end;
  a := 1; 
  if a and 0 = 0 then bien else mal end;
  if 0 and a = 0 then bien else mal end;
  if a and 1 = 1 then bien else mal end;
  if 1 and a = 1 then bien else mal end;

	//Operación lógica AND - lógica invertida
  a := 0; b := 0;
  if a and not b = 0 then bien else mal end;  //lógica invertida
  if not a and b = 0 then bien else mal end;  //lógica invertida
  if (not a and not b) = 1 then bien  else mal end;  //lógica invertida

  if a and a = a then bien  else mal end;  //lógica invertida
  if a and not a = 0 then bien  else mal end;  //lógica invertida

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica OR ///////////////////
  //////////////////////////////////////////////////////////
	//Operación lógica OR - constantes
  if 0 or 0 = 0 then bien else mal end;
  if 0 or 1 = 1 then bien else mal end;
  if 1 or 0 = 1 then bien else mal end;
  if 1 or 1 = 1 then bien else mal end;

	//Operación lógica OR - variables
  a := 0; b := 0;
  if a or b = 0 then bien else mal end;
  a := 0; b := 1;
  if a or b = 1 then bien else mal end;
  a := 1; b := 0;
  if a or b = 1 then bien else mal end;
  a := 1; b := 1;
  if a or b = 1 then bien else mal end;

	//Operación lógica OR - variables y constantes
  a := 0; 
  if a or 0 = 0 then bien else mal end;
  if 0 or a = 0 then bien else mal end;
  if a or 1 = 1 then bien else mal end;
  if 1 or a = 1 then bien else mal end;
  a := 1; 
  if a or 0 = 1 then bien else mal end;
  if 0 or a = 1 then bien else mal end;
  if a or 1 = 1 then bien else mal end;
  if 1 or a = 1 then bien else mal end;

	//Operación lógica OR - lógica invertida
  a := 0; b := 0;
  if a or not b = 1 then bien else mal end;  //lógica invertida
  if not a or b = 1 then bien else mal end;  //lógica invertida
  if (not a or not b) = 1 then bien  else mal end;  //lógica invertida

  if a or a = a then bien  else mal end;  //lógica invertida
  if a or not a = 1 then bien  else mal end;  //lógica invertida

//  //////////////////////////////////////////////////////////
//	//////////////  Operación lógica XOR ///////////////////
//  //////////////////////////////////////////////////////////
//	//Constantes
//  if 0 xor 0 = 0 then bien else mal end;
//  if 0 xor 1 = 1 then bien else mal end;
//  if 1 xor 0 = 1 then bien else mal end;
//  if 1 xor 1 = 0 then bien else mal end;
//
//	//Variables
//  a := 0; b := 0;
//  if a xor b = 0 then bien else mal end;
//  a := 0; b := 1;
//  if a xor b = 1 then bien else mal end;
//  a := 1; b := 0;
//  if a xor b = 1 then bien else mal end;
//  a := 1; b := 1;
//  if a xor b = 0 then bien else mal end;
//
//	//Variables y constantes
//  a := 0; 
//  if a xor 0 = 0 then bien else mal end;
//  if 0 xor a = 0 then bien else mal end;
//  if a xor 1 = 1 then bien else mal end;
//  if 1 xor a = 1 then bien else mal end;
//  a := 1; 
//  if a xor 0 = 1 then bien else mal end;
//  if 0 xor a = 1 then bien else mal end;
//  if a xor 1 = 0 then bien else mal end;
//  if 1 xor a = 0 then bien else mal end;
//
//	//Lógica invertida
//  a := 0; b := 0;
//  if a xor not b = 1 then bien else mal end;  //lógica invertida
//  if not a xor b = 1 then bien else mal end;  //lógica invertida
//  if (not a xor not b) = 0 then bien  else mal end;  //lógica invertida
//
//  //////////////////////////////////////////////////////////
//	//////////////// Operaciones Mixtas // //////////////////
//  //////////////////////////////////////////////////////////
//  //Operaciones con la misma variable
//  a := 0;
//  a := a;
//  if (a=a) and (a=0) then bien else mal end;
//  a := 1;
//  a := a;
//  if (a=a) and (a=1) then bien else mal end;
//   
//  a := 0;
//  a := not a;
//  if (a=1) then bien else mal end;
//
//  vbyte.bit7 := 0; 
//  vbyte.bit7 := vbyte.bit7;
//  if (vbyte.bit7=vbyte.bit7) and (vbyte.bit7=0) then bien else mal end;
//
//  vbyte.bit7 := 1; 
//  vbyte.bit7 := vbyte.bit7;
//  if (vbyte.bit7=1) then bien else mal end;
//
//  vbyte.bit7 := 0; 
//  vbyte.bit7 := not vbyte.bit7;
//  if vbyte.bit7 = 1 then bien else mal end;
//  
//  //Oepraciones con dos variables    
//  a := 0; b := 0;
//  if (a = 0) and (b = 0) then bien else mal end;
//  if (not a and not b) = 1 then bien else mal end;
//
//  a := 1; b := 0;
//  if (a = 1) and (b = 0) then bien else mal end;
//  if (a and not b) = 1 then bien else mal end;
//
//
//  a := 0; b := 0;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;
//  a := 1; b := 0;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;
//  a := 1; b := 1;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;

end.

