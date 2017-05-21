{Rutina de verificación para operaciones básicas con datos de 
tipo BYTE. 
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
    delay_ms(30);
    pinLed := 0;
    delay_ms(70);
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

	//Pruebas con IF
	if true then bien else mal end;
	if false then mal else bien end;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 = 0 then bien else mal end;
  if 0 = 1 then mal else bien end;
  if 255 = 255 then bien else mal end;
  if 1 = 0 then mal else bien end;

  if 0 <> 0 then mal else bien end;
  if 255 <> 0 then bien else mal end;
  if 255 <> 255 then mal else bien end;

	//Variables
  a := 0;
  b := 255;
  if a = a then bien else mal end;
  if a = b then mal else bien end;
  if a <> a then mal else bien end;
  if a <> b then bien else mal end;

  a := 255;
  b := 255;
  if a = a then bien else mal end;
  if a = b then bien else mal end;
  if b = a then bien else mal end;
  if a <> a then mal else bien end;
  if a <> b then mal else bien end;

	//Variables - constantes
  a := 10;
  if a = 10 then bien else mal end;
  if 10 = a then bien else mal end;
  if a = 0 then mal else bien end;
  if 0 = a then mal else bien end;
  if a <> 10 then mal else bien end;
  if 10 <> a then mal else bien end;
  if a <> 255 then bien else mal end;
  if 255 <> a then bien else mal end;

	//Expresiones
  a := 10;
  if a = 5+5 then bien else mal end;
  if 11 = a+1 then bien else mal end;
  if a+1 = a+1 then bien else mal end;
  if a+1 = a+2 then mal else bien end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 > 0 then mal else bien end;
  if 0 > 1 then mal else bien end;
  if 255 > 0 then bien else mal end;
  if 255 > 15 then bien else mal end;
  if 255 > 255 then mal else bien end;

	//Variables
  a := 1;  b := 255;
  if a > a then mal else bien end;
  if a > b then mal else bien end;
  if b > b then mal else bien end;
  if b > a then bien else mal end;

  a := 0;  b := 255;
  if a > a then mal else bien end;
  if a > b then mal else bien end;
  if b > b then mal else bien end;
  if b > a then bien else mal end;

  a := 0;  b := 1;
  if a > a then mal else bien end;
  if a > b then mal else bien end;
  if b > b then mal else bien end;
  if b > a then bien else mal end;

  a := 0;  b := 0;
  if a > a then mal else bien end;
  if a > b then mal else bien end;
  if b > b then mal else bien end;
  if b > a then mal else bien end;

  a := 255;  b := 255;
  if a > a then mal else bien end;
  if a > b then mal else bien end;
  if b > b then mal else bien end;
  if b > a then mal else bien end;

	//Variables - constantes
  a := 1;
  if a > 0 then bien else mal end;
  if a > 1 then mal else bien end;
  if a > 255 then mal else bien end;
  if 0 > a then mal else bien end;
  if 1 > a then mal else bien end;
  if 2 > a then bien else mal end;
  if 255 > a then bien else mal end;

	//Expresiones
  a := 10;
  if 0 > a-10 then mal else bien end;
  if 0 > a+1 then mal else bien end;
  if 11 > a+1 then mal else bien end;
  if 12 > a+1 then bien else mal end;
  if 255 > a+1 then bien else mal end;

  a := 10;
  if a+1 > 0 then bien else mal end;
  if a+1 > 10 then bien else mal end;
  if a+1 > 11 then mal else bien end;
  if a+1 > 12 then mal else bien end;
  if a+1 > 255 then mal else bien end;

  if a > a+1 then mal else bien end;
  if a+1 > a then bien else mal end;

  if a+1 > a+2 then mal else bien end;
  if a+1 > a+1 then mal else bien end;
  if a+2 > a+1 then bien else mal end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Menor /////////////////////
  //////////////////////////////////////////////////////////
	{Las pruebas para las operaciones "menor que", no son tan exigentes porque 
   se generan a partir de "mayor que"}

	//Constante
  if 0 < 0 then mal else bien end;
  if 0 < 1 then bien else mal end;
  if 255 < 0 then mal else bien end;

	//Variables
  a := 0;  b := 0;
  if a < a then mal else bien end;
  if a < b then mal else bien end;
  if b < a then mal else bien end;

  a := 255;  b := 255;
  if a < a then mal else bien end;
  if a < b then mal else bien end;
  if b < a then mal else bien end;

	//Variables - constantes
  a := 1;
  if a < 0 then mal else bien end;
  if a < 1 then mal else bien end;
  if a < 255 then bien else mal end;
  if 0 < a then bien else mal end;
  if 255 < a then mal else bien end;

	//Expresiones
  a := 10;
  if a < a+1 then bien else mal end;
  if a+1 < a then mal else bien end;

  if a+1 < a+2 then bien else mal end;
  if a+1 < a+1 then mal else bien end;
  if a+2 < a+1 then mal else bien end;

  //////////////////////////////////////////////////////////
	///////////////  Operación Mayor/Menor o igual ///////////
  //////////////////////////////////////////////////////////
	{Estas operaciones, son aún menos exigentes, porque son negaciones 
  de las operaciones base}

	if 5>=4 then bien else mal end;
	if 5>=5 then bien else mal end;
	if 5>=6 then mal else bien end;

  a := 5;  b := 6;
  if a >= a then bien else mal end;
  if a >= b then mal else bien end;
  if b >= a then bien else mal end;

	if a <= a then bien else mal end;
	if a <= 6 then bien else mal end;
	if a <= 4 then mal else bien end;

end.

