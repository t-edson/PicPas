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

	//Pruebas con IF
	if true then good else bad end;
	if false then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if 0 = 0 then good else bad end;
  if 0 = 1 then bad else good end;
  if 255 = 255 then good else bad end;
  if 0 <> 0 then bad else good end;
  if 255 <> 0 then good else bad end;

	//coConst_Variab
  a := 10;
  if 10 = a then good else bad end;
  if 0 = a then bad else good end;
  if 10 <> a then bad else good end;
  a := 255;
  if 255 = a then good else bad end;

  //coVariab_Const
  a := 10;
  if a = 10 then good else bad end;
  if a = 0 then bad else good end;
  if a <> 10 then bad else good end;
  if a = 5+5 then good else bad end;

	//coVariab_Variab
  a := 0;
  b := 255;
  if a = a then good else bad end;
  if a = b then bad else good end;
  if a <> a then bad else good end;
  a := 255;
  if a = a then good else bad end;
  if a = b then good else bad end;
  if a <> a then bad else good end;

  //coConst_Expres
  a := 0;
  if 11 = a+11 then good else bad end;

  //coExpres_Const
  if a+255 = 255 then good else bad end;
  
  //coVariab_Variab
  a:= 255; b := 255;
  if a = b then good else bad end;
  
	//coExpres_Expres
  a := 10;
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 > 0 then bad else good end;
  if 0 > 1 then bad else good end;
  if 255 > 0 then good else bad end;
  if 255 > 15 then good else bad end;
  if 255 > 255 then bad else good end;

	//Variables
  a := 1;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 1;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 0;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then bad else good end;

  a := 255;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then bad else good end;

	//Variables - constantes
  a := 1;
  if a > 0 then good else bad end;
  if a > 1 then bad else good end;
  if a > 255 then bad else good end;
  if 0 > a then bad else good end;
  if 1 > a then bad else good end;
  if 2 > a then good else bad end;
  if 255 > a then good else bad end;

	//Expresiones
  a := 10;
  if 0 > a-10 then bad else good end;
  if 0 > a+1 then bad else good end;
  if 11 > a+1 then bad else good end;
  if 12 > a+1 then good else bad end;
  if 255 > a+1 then good else bad end;

  a := 10;
  if a+1 > 0 then good else bad end;
  if a+1 > 10 then good else bad end;
  if a+1 > 11 then bad else good end;
  if a+1 > 12 then bad else good end;
  if a+1 > 255 then bad else good end;

  if a > a+1 then bad else good end;
  if a+1 > a then good else bad end;

  if a+1 > a+2 then bad else good end;

//Código comentado por falta de espacio
//  if a+1 > a+1 then bad else good end;
//  if a+2 > a+1 then good else bad end;
//
//  //////////////////////////////////////////////////////////
//	/////////////////////  FIN DE PRUEBAS  ///////////////////
//  //////////////////////////////////////////////////////////
//  pinLed := 0;
//  delay_ms(50);
//  pinLed := 1;
//  delay_ms(70);
//  pinLed := 0;
end.
