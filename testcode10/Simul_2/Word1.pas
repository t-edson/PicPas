{Rutina de verificación para operaciones de comparación con datos 
de tipo WORD. 
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var

  a, b: word;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if word(0) = word(0) then good else bad end;
  if word(0) = word(1) then bad else good end;
  if word(255) = word(255) then good else bad end;
  if 65535 = 65535 then good else bad end;

  if word(0) <> word(0) then bad else good end;
  if word(255) <> word(0) then good else bad end;
  if 65535 <> 65535 then bad else good end;

  //coConst_Variab
  //Se prueba más detalladamente, porque hay varios casos que se optimizan
  a := 10;
  if word(10) = a then good else bad end;
  if word(0) = a then bad else good end;
  if word(10) <> a then bad else good end;
  if word(255) <> a then good else bad end;
  if 256 <> a then good else bad end;
  a := 1000;
  if 1000 = a then good else bad end;
  if word(0) = a then bad else good end;
  if word(1000) <> a then bad else good end;
  if $FFFF <> a then good else bad end;
  if 256 <> a then good else bad end;
  a := 257;  //caso para optimizar
  if 257 = a then good else bad end;
  
  //coConst_Expres
  a := 10; 
  if word(11) = a+1 then good else bad end;
  a := 1000; 
  if 2000 = a+1000 then good else bad end;
  
  //coVariab_Const
  a := 10;
  if a = word(10) then good else bad end;
  a := 1000;
  if a = 1000 then good else bad end;
  
  //coVariab_Variab
  a := 0;
  b := 255;
  if a = a then good else bad end;
  if a = b then bad else good end;
  if a <> a then bad else good end;
  a := 65535;
  b := 65535;
  if a = a then good else bad end;
  if a = b then good else bad end;
  if b = a then good else bad end;
  if a <> a then bad else good end;
  a := 65535;
  b := 1;
  if a = b then bad else good end;
  if b = a then bad else good end;
  if a <> a then bad else good end;

  //coVariab_Expres
  a := 10; b := 5; 
  if a = b+5 then good else bad end;
  a := 1000; 
  if a = b+995 then good else bad end;

  //coExpres_Const
  a := 10; 
  if a+1 = word(11) then good else bad end;
  a := 1000; 
  if a+1 = 1001 then good else bad end;
  
  //coExpres_Variab 
  a := 1000; b := 5; 
  if b+995 = a then good else bad end;

	//coExpres_Expres
  a := 10; b := 5; 
  if a+1 = a+1 then good else bad end;
//Operaciones comentadas por falta de espacio
//  if a+1 = a+2 then bad else good end;
//  a := 1000; b := 5; 
//  if a+1 = a+1 then good else bad end;
//  if a+1 = a+2 then bad else good end;
//  if a+1 = (a+1)+(b+2) then bad else good end;

end.
