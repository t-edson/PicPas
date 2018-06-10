{Rutina de verificación para operaciones de comparación con datos 
de tipo DWORD.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var

  a, b: Dword;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if dword(0) = dword(0) then good else bad end;
  if dword(0) = dword(1) then bad else good end;
  if dword($FFFF) = dword($FFFF) then good else bad end;
  if 1265535 = 1265535 then good else bad end;

  if dword(0) <> dword(0) then bad else good end;
  if dword(255) <> dword(0) then good else bad end;
  if $12345678 <> $12345678 then bad else good end;

	//coConst_Variab
  a := $123456;
  if $123456 = a then good else bad end;
  a := 0;
  if dword(0) = a then good else bad end;
  a := $01020304;
  if dword($01020304) = a then good else bad end;
  a := $FFFFFFFF;
  if $FFFFFFFF = a then good else bad end;
  
  //coConst_Expres
  a := $123456;
  if $123457 = a + dword(1) then good else bad end;
  
  //coVariab_Const
  a := $123456;
  if a = $123456 then good else bad end;
  a := $1234;
  if a = dword($1234) then good else bad end;

  //coVariab_Variab
  a := $123456;
  b := $123456;
  if a = b then good else bad end;
  a := $1234;
  b := $1234;
  if a = b then good else bad end;
  
  //coVariab_Expres
  a := $123456;
  b := $123457;
  if b = dword(1) + a then good else bad end;
  
  //coExpres_Const
  a := $FFFFFFF;
  if dword(1) + a = $10000000 then good else bad end;

  //coExpres_Variab
  b := $10000000;
  if dword(1) + a = b then good else bad end;

  //coExpres_Expres
//  a := 3141592654;
//  b := 3141592654;
//  if dword(1) + b = dword(1) + a then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////
  //<<No implementado>>


end.
