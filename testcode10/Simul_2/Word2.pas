{Rutina de verificación para operaciones aritméticas con datos 
de tipo WORD. 
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var
  a, b: word;
	vbyte: byte;
	vword: word;
begin
  SetAsOutput(pinLed);
  pinLed := 0;

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
  a := 0;  
  if a > word(0) then bad else good end;
  a := 1;  
  if a > word(0) then good else bad end;
  a := 255;
  if a > word(254) then good else bad end;
  a := 1000;  
  if 1000 > a then bad else good end;
  if 1001 > a then good else bad end;

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
  a := 0;  
  if (a+1) > word(1)   then bad else good end;
  if (a+2) > word(1)   then good else bad end;
  a := 999;  
  if (a+1) > 1000 then bad else good end;
  if (a+2) > 1000 then good else bad end;

//Lineas comentadas por falta de espacio
//  //coExpres_Variab
//  a := 0; b := 1;
//  if (a+1) > b   then bad else good end;
//  if (a+2) > b   then good else bad end;
//  a := 999; b := 1000;
//  if (a+1) > b then bad else good end;
//  if (a+2) > b then good else bad end;
//
//  //coExpres_Expres
//  a := 0; b := 0; 
//  if (a+1) > (b+1) then bad else good end;
//  a := 254; b := 254; 
//  if (a+1) > (b+1) then bad else good end;
//  if (a+2) > (b+1) then good else bad end;
//  a := 1000; b := 1000; 
//  if (a+1) > (b+2) then bad else good end;
//  if (a+1) > (b+1) then bad else good end;
//  if (a+2) > (b+1) then good else bad end;
  
end.

