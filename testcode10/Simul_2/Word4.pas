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
	//////////////  Operación suma byte-word ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if 1 + word(0) = 1 then good else bad end;
  if 1 + 65534  = 65535 then good else bad end;
	//coConst_Variab
  vword := 257; 
  if 100 + vword = 357 then good else bad end;
  //coConst_Expres
  vword := 256; 
  if 100 + (vword + 1) = 357 then good else bad end;
  //coVariab_Const
  vbyte := 100; 
  if vbyte + 350 = 450 then good else bad end;
	//coVariab_Variab
  vbyte := 0; vword := 1;
  if vbyte + vword = word(1) then good else bad end;
  vbyte := 1; vword := 1000;
  if vbyte + vword = 1001 then good else bad end;
  //coVariab_Expres
  vbyte := 5; vword := 300; 
  if vbyte + (vword+1) = 306 then good else bad end;
  //coExpres_Const
  vbyte := 100; 
  if (vbyte+1) + 500 = 601 then good else bad end;
  //coExpres_Variab
  vbyte := 1; vword := 300;
  if (vbyte+1) + vword = 302 then good else bad end;
	//coExpres_Expres
  vbyte := 100; vword := 100; 
  if (vbyte+100) + (vword + 500) = 800 then good else bad end;

//  //////////////////////////////////////////////////////////
//	//////////////  Resta word-word ///////////////////
//  //////////////////////////////////////////////////////////
//	//coConst_Const
//  if word(2) - word(1) = 1 then good else bad end;
//  if word(1000) - word(500) = 500 then good else bad end;
//  //coConst_Variab
//  a := 2; 
//  if word(10) - a = word(8) then good else bad end;
//  if word(1000) - a = 998 then good else bad end;
//  //coConst_Expres
//  if word(10) - (a+1) = word(7) then good else bad end;
//  if word(1000) - (498 + a) = 500 then good else bad end;
//  //coVariab_Const
//  if a - word(1) = word(1) then good else bad end;
//  a := 1000;
//  if a - 300 = 700 then good else bad end;
//	//coVariab_Variab
//  a := 2; b := 1;
//  if a - b = word(1) then good else bad end;
//  a := 1000; b := 999;
//  if a - b = word(1) then good else bad end;
//  a := 1000; b := 0;
//  if a - b = 1000 then good else bad end;
//  //coVariab_Expres  
//  if a - (b+1) = 999 then good else bad end;
//  //coExpres_Const  
//  if (a+1000) - 1250 = 750 then good else bad end;
//  //coExpres_Variab  
//  if (a+1000) - b = 2000 then good else bad end;
//  //coExpres_Expres  
//  if (a+1000) - (b+1000) = 1000 then good else bad end;
//
//  //////////////////////////////////////////////////////////
//	//////////////  Multip word-word->word ///////////////////
//  //////////////////////////////////////////////////////////
	//coConst_Const
  if word(2) umulword word(1) = word(2) then good else bad end;
  if word(100) umulword 500 = 50000 then good else bad end;
  //coConst_Variab
  a := 2; 
  if word(10) - a = word(8) then good else bad end;
  if word(1000) - a = 998 then good else bad end;
  //coConst_Expres
  if word(10) - (a+1) = word(7) then good else bad end;
  if word(1000) - (498 + a) = 500 then good else bad end;
  //coVariab_Const
  if a - word(1) = word(1) then good else bad end;
  a := 1000;
  if a - 300 = 700 then good else bad end;
	//coVariab_Variab
  a := 2; b := 1;
  if a - b = word(1) then good else bad end;
  a := 1000; b := 999;
  if a - b = word(1) then good else bad end;
  a := 1000; b := 0;
  if a - b = 1000 then good else bad end;
//  //coVariab_Expres  
//  if a - (b+1) = 999 then good else bad end;
//  //coExpres_Const  
//  if (a+1000) - 1250 = 750 then good else bad end;
//  //coExpres_Variab  
//  if (a+1000) - b = 2000 then good else bad end;
//  //coExpres_Expres  
//  if (a+1000) - (b+1000) = 1000 then good else bad end;
  
 
end.
 
