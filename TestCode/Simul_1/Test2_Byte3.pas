{Rutina de verificación para operaciones aritméticas con datos 
de tipo BYTE. 
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
uses PIC16F84A;
var
  pinLed: bit absolute PORTB.0;
  a, b: byte;

  procedure bien;
  begin
    pinLed := 1;
    delay_ms(25);
    pinLed := 0;
    delay_ms(40);
  end;
  procedure Mal;
  begin
    pinLed := 1;
    delay_ms(1500);
    pinLed := 0;
    asm SLEEP end
  end;
begin
  SetAsOutput(pinLed);
  pinLed := 0;

//  //////////////////////////////////////////////////////////
//	////////////////////  Mult 8 bits->16  ///////////////////
//  //////////////////////////////////////////////////////////
//	//coConst_Const
//  if 0 * 0 = word(0) then bien else mal end;
//  if 0 * 1  = word(0) then bien else mal end;
//  if 255 * 0 = word(0) then bien else mal end;
//  if 5 * 5 = word(25) then bien else mal end;
//
//	//coConst_Variab
//  a := 0;
//  if 5 * a = word(0) then bien else mal end;
//  a := 5;
//  if word(0 * a) = word(0) then bien else mal end;
//  if 5 * a = word(25) then bien else mal end;
//  //coConst_Expres
//  a := 5;
//  if 0 * (a+1) = word(0) then bien else mal end;
//  if 5 * (a+1) = word(30) then bien else mal end;
//  //coVariab_Const
//  a := 5;
//  if a * 0 = word(0) then bien else mal end;
//  if a * 5 = word(25) then bien else mal end;
//  //coVariab_Variab  
//  a := 5; b := 0;
//  if a * b = word(0) then bien else mal end;
//  a := 5; b := 100;
//  if a * b = 500 then bien else mal end;
//  //coVariab_Expres
//  a := 10; b := 10;
//  if a * (b+1) = word(110) then bien else mal end;
//  //coExpres_Const
//  a := 19;
//  if (a+1) * 5 = word(100) then bien else mal end;
//  //coExpres_Variab
//  a := 19; b := 100;
//  if (a+1) * b = 2000 then bien else mal end;
//  //coExpres_Expres
//  a := 10; b := 10;
//  if (a+1) * (b+1) = word(121) then bien else mal end;
//
//  //////////////////////////////////////////////////////////
	////////////////////  Div 8 bits->8  ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if 0 div 1  = word(0) then bien else mal end;
  if 5 div 255 = word(0) then bien else mal end;
  if 25 div 5 = word(5) then bien else mal end;

	//coConst_Variab
  a := 0;
  if 0 div a = 0 then bien else mal end;
//  a := 1;
//  if 1 div a = 1 then bien else mal end;
//  a := 2;
//  if 1 div a = 0 then bien else mal end;
  a := 50;
  if 100 div a = 2 then bien else mal end;

//  //coConst_Expres
//  a := 5;
//  if 0 * (a+1) = word(0) then bien else mal end;
//  if 5 * (a+1) = word(30) then bien else mal end;
//  //coVariab_Const
//  a := 5;
//  if a * 0 = word(0) then bien else mal end;
//  if a * 5 = word(25) then bien else mal end;
//  //coVariab_Variab  
//  a := 5; b := 0;
//  if a * b = word(0) then bien else mal end;
//  a := 5; b := 100;
//  if a * b = 500 then bien else mal end;
//  //coVariab_Expres
//  a := 10; b := 10;
//  if a * (b+1) = word(110) then bien else mal end;
//  //coExpres_Const
//  a := 19;
//  if (a+1) * 5 = word(100) then bien else mal end;
//  //coExpres_Variab
//  a := 19; b := 100;
//  if (a+1) * b = 2000 then bien else mal end;
//  //coExpres_Expres
//  a := 10; b := 10;
//  if (a+1) * (b+1) = word(121) then bien else mal end;
  
end.
