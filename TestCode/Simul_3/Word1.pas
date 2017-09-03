{Rutina de verificación para operaciones de comparación con datos 
de tipo WORD. 
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
uses UnitTest, PIC16F877A;
var

  a, b: word;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////
	//Constante
  if word(0) = word(0) then good else bad end;
  if word(0) = word(1) then bad else good end;
  if word(255) = word(255) then good else bad end;
  if 65535 = 65535 then good else bad end;

  if word(0) <> word(0) then bad else good end;
  if word(255) <> word(0) then good else bad end;
  if 65535 <> 65535 then bad else good end;

	//Variables
  a := 0;
  b := 255;
  if a = a then good else bad end;
  if a = b then bad else good end;
  if a <> a then bad else good end;
  if a <> b then good else bad end;

  a := 65535;
  b := 65535;
  if a = a then good else bad end;
  if a = b then good else bad end;
  if b = a then good else bad end;
  if a <> a then bad else good end;
  if a <> b then bad else good end;

  a := 65535;
  b := 1;
  if a = b then bad else good end;
  if b = a then bad else good end;
  if a <> a then bad else good end;
  if a <> b then good else bad end;

	//Variables - constantes
  a := 10;
  if a = word(10) then good else bad end;
  if word(10) = a then good else bad end;
  if a = word(0) then bad else good end;
  if word(0) = a then bad else good end;
  if a <> word(10) then bad else good end;
  if word(10) <> a then bad else good end;
  if a <> word(255) then good else bad end;
  if word(255) <> a then good else bad end;
  if 256 <> a then good else bad end;
  a := 1000;
  if a = 1000 then good else bad end;
  if 1000 = a then good else bad end;
  if a = word(0) then bad else good end;
  if word(0) = a then bad else good end;
  if a <> word(1000) then bad else good end;
  if word(1000) <> a then bad else good end;
  if a <> $FFFF then good else bad end;
  if $FFFF <> a then good else bad end;
  if 256 <> a then good else bad end;

	//Expresiones
  a := 10; b := 5; 
  if a = b+5 then good else bad end;
  if word(11) = a+1 then good else bad end;
  if a+1 = word(11) then good else bad end;
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;
  a := 1000; b := 5; 
  if a = b+995 then good else bad end;
  if 1001 = a+1 then good else bad end;
  if a+1 = 1001 then good else bad end;
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;
  if a+1 = (a+1)+(b+2) then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if word(0) > word(0) then bad else good end;
  if word(10) > word(9) then good else bad end;

	//coConst_Variab
  a := 0;  
  if word(0) > a then bad else good end;
  a := 1000;  
  if 1000 > a then bad else good end;
  if 1001 > a then good else bad end;
  //coConst_Expres
  a := 0;  
  if word(1) > (a+1) then bad else good end;
  a := 999;  
  if 1000 > (a+1) then bad else good end;
  if 1001 > (a+1) then good else bad end;
  //coVariab_Const
  //<<No implemented>>

  //coVariab_Variab
  a := 0; b := 0; 
  if a > b then bad else good end;
  a := 256; b := 255; 
  if a > b then good else bad end;
  if b > a then bad else good end;
  a := 1000; b := 1000; 
  if a > b then bad else good end;
  a := 1001; b := 1000; 
  if a > b then good else bad end;
  //coVariab_Expres
  a := 1; b := 0; 
  if a > (b+1) then bad else good end;
  a := 256; b := 254; 
  if a > (b+1) then good else bad end;
  if a > (b+2) then bad else good end;
  a := 1001; b := 1000; 
  if a > (b+1) then bad else good end;
  a := 1001; b := 999; 
  if a > (b+1) then good else bad end;
  //coExpres_Const
  //<<No implemented>>

  //coExpres_Variab
  //<<No implemented>>

  //coExpres_Expres
  a := 0; b := 0; 
  if (a+1) > (b+1) then bad else good end;
  a := 254; b := 254; 
  if (a+1) > (b+1) then bad else good end;
  if (a+2) > (b+1) then good else bad end;
  a := 1000; b := 1000; 
  if (a+1) > (b+2) then bad else good end;
  if (a+1) > (b+1) then bad else good end;
  if (a+2) > (b+1) then good else bad end;
  
end.
