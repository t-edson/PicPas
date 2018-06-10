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
	///////////////////  Desplaz. Derecha ////////////////////
  //////////////////////////////////////////////////////////
  //coConst_Const
  if 1>>0 = 1 then good else bad end;  
  if 2>>2 = 0 then good else bad end;  
  //coVariab_Const
  a := $ff;
  if a>>0 = $ff then good else bad end;
  if a>>1 = $7f then good else bad end;
  if a>>2 = $3f then good else bad end;
  if a>>3 = $1f then good else bad end;
  if a>>7 = $1 then good else bad end;
  if a>>8 = $0 then good else bad end;
  //coVariab_Variab
  a := $ff; 
  b := 0;
  if a>>b = $ff then good else bad end;
  b := 1;
  if a>>b = $7f then good else bad end;
  b := 8;
  if a>>b = $0 then good else bad end;
  //coExpres_Const
  a := $80;
  if (a+8)>>1 = $44 then good else bad end;
  if (a+8)>>2 = $22 then good else bad end;
  if (a+8)>>3 = $11 then good else bad end;
  if (a+8)>>4 = $08 then good else bad end;
  if (a+8)>>5 = $04 then good else bad end;
    
  //////////////////////////////////////////////////////////
	//////////////////  Desplaz. Izquierda ///////////////////
  //////////////////////////////////////////////////////////
  //coConst_Const
  if 1<<0 = 1 then good else bad end;  
  if 2<<2 = 8 then good else bad end;  
  
  //coVariab_Const
  a := $01;
  if a<<0 = $01 then good else bad end;
  if a<<1 = $02 then good else bad end;
  if a<<2 = $04 then good else bad end;
  if a<<3 = $08 then good else bad end;
  if a<<7 = $80 then good else bad end;
  if a<<8 = $00 then good else bad end;
    
  //coVariab_Variab
  a := $01;
  b := 0;
  if a<<b = $01 then good else bad end;
  b := 1;
  if a<<b = $02 then good else bad end;
  b := 8;
  if a<<b = $00 then good else bad end;
  
  //coExpres_Const
  a := $02;
  if (a+1)<<1 = $06 then good else bad end;
  if (a+1)<<2 = $0C then good else bad end;
  if (a+1)<<3 = $18 then good else bad end;
  if (a+1)<<4 = $30 then good else bad end;
//Prueba eliminada por falta de espacio
//  if (a+1)<<5 = $60 then good else bad end;
 
end.
