{Rutina de verificación para operaciones con datos de tipo byte.
Se debe simular el programa en el circuito "Test2.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.
Por: Tito Hinostroza
Modificado: 01/03/2018}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var
  a, b: byte;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	////////////////////////  Suma  /////////////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 + 0 = 0 then good else bad end;
  if 0 + 1  = 0 then bad else good end;
  if 255 + 0 = 255 then good else bad end;

	//Variables
  a := 0;
  b := 255;
  if a + a = 0 then good else bad end;
  if a + b = 255 then good else bad end;
  if b + a = 255 then good else bad end;
  if a + b = b then good else bad end;

  a := 1;
  b := 254;
  if a + b = 255 then good else bad end;

	//Variables - constantes
  a := 0;
  if a + 0 = 0 then good else bad end;
  if 0 + a = 0 then good else bad end;
  if a + 1 = 1 then good else bad end;
  if 1 + a = 1 then good else bad end;
  if a + 255 = 255 then good else bad end;
  if 255 + a = 255 then good else bad end;

  a := 1;
  if a + 0 = 1 then good else bad end;
  if 0 + a = 1 then good else bad end;
  if a + 1 = 2 then good else bad end;
  if 1 + a = 2 then good else bad end;
  if a + 254 = 255 then good else bad end;
  if 254 + a = 255 then good else bad end;

	//Expresiones
  a := 10; b := 5;
  if a = b+5 then good else bad end;
  if a + b = 15 then good else bad end;
  if (a + b) + 1 = 16 then good else bad end;
  if b + a = 15 then good else bad end;
  if 15 = b + a then good else bad end;
  if a + b = a + 5 then good else bad end;
  if a + (b+a) = a + (a + 5) then good else bad end;
  if a + (b+ (a+b)) = a + (a + a) then good else bad end;

  //////////////////////////////////////////////////////////
	////////////////////////  Resta  /////////////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 - 0 = 0 then good else bad end;
  if 1 - 0  = 0 then bad else good end;
  if 255 - 0 = 255 then good else bad end;

	//Variables
  a := 255; b := 0;
  if a - a = 0 then good else bad end;
  if a - a = b then good else bad end;
  if b - b = b then good else bad end;
  if a - b = 255 then good else bad end;
  if a - b = a then good else bad end;

  a := 255; b := 1;
  if b - b = 0 then good else bad end;
  if a - b = 254 then good else bad end;

  a := 1; b := 0;
  if a - b = 1 then good else bad end;
  if a - b = a then good else bad end;

  a := 10; b := 5;
  if a - b = b then good else bad end;
  if a - b = 5 then good else bad end;

	//Variables - constantes
  a := 0;
  if a - 0 = 0 then good else bad end;
  if 0 - a = 0 then good else bad end;
  if a - 0 = a then good else bad end;
  a := 1;
  if a - 1 = 0 then good else bad end;
  if 1 - a = 0 then good else bad end;
  if 255 - a = 254 then good else bad end;
  if 10 - a = 9 then good else bad end;
  a := 255;
  if a - 1 = 254 then good else bad end;
  if 255 - a = 0 then good else bad end;
  if a - 10 = 245 then good else bad end;

	//Expresiones
  a := 10; b := 5;
  if 10 - (b+1) = 4 then good else bad end;  //constante - expresión  
  if a - (b+1) = 4 then good else bad end;   //variable - expresión  
  if (a+1) - 1 = 10 then good else bad end;   //expresión - constante 
  if (a+1) - 0 = 11 then good else bad end;   //expresión - constante 
  if (a+1) - a = 1 then good else bad end;   //expresión - variable
  //Pruebas eliminadas por falta de espacio
//  if (a+1) - (b+1) = 5 then good else bad end;   //expresión - expresión
//  if a - b = a - b then good else bad end;
//  if a - b - 1 = 4 then good else bad end;
//  if a - b - 1 = b - 1 then good else bad end;
//  if 5 = a - b then good else bad end;
//  if a - 5 = a - b then good else bad end;
//  if b + (a-b) = a  then good else bad end;
//  if a = b + (a-b) then good else bad end;
//  if a + (b+ (a-b)) = a + (a - b + 5) then good else bad end;
//

end.
