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
	//////////////  Operación suma ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if word(0) + word(1) = 1 then good else bad end;
  if 32768 + 1000 = 33768 then good else bad end;
	//coConst_Variab
  a := 100; 
  if 257 + a = 357 then good else bad end;
  //coConst_Expres
  a := 100; 
  if 256 + (a+1) = 357 then good else bad end;
  //coVariab_Const
  a := 100; 
  if a + 257 = 357 then good else bad end;
	//coVariab_Variab
  a := 0; b := 256;
  if a + b = 256 then good else bad end;
  a := 1000; b := 256;
  if a + b = 1256 then good else bad end;
  //coVariab_Expres
  a := 300; 
  if a + (a+1) = 601 then good else bad end;
  //coExpres_Const
  a := 100; 
  if (a+1) + 256 = 357 then good else bad end;
  //coExpres_Variab
  a := 300; b := 1 ;
  if (a+1) + b = 302 then good else bad end;
  a := 100; b := 500; 
  if a + b + 5 = 605 then good else bad end;
	//coExpres_Expres
  a := 100; b := 500; 
  if (a+500) + (b + 500) = 1600 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Operación suma word-byte ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if word(0) + 1 = 1 then good else bad end;
  if 65534 + 1 = 65535 then good else bad end;
	//coConst_Variab
  vbyte := 100; 
  if 257 + vbyte = 357 then good else bad end;
  //coConst_Expres
  vbyte := 100; 
  if 256 + (vbyte+1) = 357 then good else bad end;
  //coVariab_Const
  a := 100; 
  if a + 25 = word(125) then good else bad end;
	//coVariab_Variab
  a := 0; vbyte := 1;
  if a + vbyte = word(1) then good else bad end;
  a := 1000; vbyte := 1;
  if a + vbyte = 1001 then good else bad end;
  //coVariab_Expres
  a := 300; vbyte := 5; 
  if a + (vbyte+1) = 306 then good else bad end;
  //coExpres_Const
  a := 100; 
  if (a+1) + 5 = word(106) then good else bad end;
  //coExpres_Variab
  a := 300; vbyte := 1 ;
  if (a+1) + vbyte = 302 then good else bad end;
	//coExpres_Expres
//  a := 100; vbyte := 100; 
//  if (a+500) + (vbyte + 1) = 701 then good else bad end;

end.

