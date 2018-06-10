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
	//////////////////  Operación Menor /////////////////////
  //////////////////////////////////////////////////////////
	{Las pruebas para las operaciones "menor que", no son tan exigentes porque 
   se generan a partir de "mayor que"}

	//Constante
  if 0 < 0 then bad else good end;
  if 0 < 1 then good else bad end;
  if 255 < 0 then bad else good end;

	//Variables
  a := 0;  b := 0;
  if a < a then bad else good end;
  if a < b then bad else good end;
  if b < a then bad else good end;

  a := 255;  b := 255;
  if a < a then bad else good end;
  if a < b then bad else good end;
  if b < a then bad else good end;

	//Variables - constantes
  a := 1;
  if a < 0 then bad else good end;
  if a < 1 then bad else good end;
  if a < 255 then good else bad end;
  if 0 < a then good else bad end;
  if 255 < a then bad else good end;

	//Expresiones
  a := 10;
  if a < a+1 then good else bad end;
  if a+1 < a then bad else good end;

  if a+1 < a+2 then good else bad end;
  if a+1 < a+1 then bad else good end;
  if a+2 < a+1 then bad else good end;

  //////////////////////////////////////////////////////////
	///////////////  Operación Mayor/Menor o igual ///////////
  //////////////////////////////////////////////////////////
	{Estas operaciones, son aún menos exigentes, porque son negaciones 
  de las operaciones base}

	if 5>=4 then good else bad end;
	if 5>=5 then good else bad end;
	if 5>=6 then bad else good end;

  a := 5;  b := 6;
  if a >= a then good else bad end;
  if a >= b then bad else good end;
  if b >= a then good else bad end;

	if a <= a then good else bad end;
	if a <= 6 then good else bad end;
	if a <= 4 then bad else good end;

  //////////////////////////////////////////////////////////
	/////////////////////  Operación AND /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 and a = 0 then good else bad end;
  if $FF and b = $0F then good else bad end;
  //coConst_Expres
  if $FF and (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b and $01 = $01 then good else bad end;
  //coVariab_Variab
  if a and b = $0F then good else bad end;
  //coVariab_Expres
  if a and (b+1) = $10 then good else bad end;
  //coExpres_Const    
  if (b+1) and $0F = $0 then good else bad end;
  //coExpres_Variab
  if (b+1) and a = $10 then good else bad end;
  //coExpres_Expres
  if (b+1) and (a+1) = $00 then good else bad end;

  //////////////////////////////////////////////////////////
	/////////////////////  Operación OR /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 or a = $FF then good else bad end;
  if $00 or b = $0F then good else bad end;
  //coConst_Expres
  if $00 or (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b or $01 = $0F then good else bad end;
  //coVariab_Variab
  if a or b = $FF then good else bad end;
  //coVariab_Expres
  if a or (b+1) = $FF then good else bad end;
  //coExpres_Const    
  if (b+1) or $0F = $1F then good else bad end;
  //coExpres_Variab
  if (b+1) or b = $1F then good else bad end;
  //coExpres_Expres
  if (b+1) or (a+1) = $10 then good else bad end;
  
  //////////////////////////////////////////////////////////
	/////////////////////  Operación XOR /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 xor a = $FF then good else bad end;
  if $00 xor b = $0F then good else bad end;
  //coConst_Expres
  if $00 xor (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b xor $01 = $0E then good else bad end;
  //coVariab_Variab
  if a xor b = $F0 then good else bad end;
  //coVariab_Expres
  if a xor (b+1) = $EF then good else bad end;
  //coExpres_Const    
  if (b+1) xor $0F = $1F then good else bad end;
  //coExpres_Variab
  if (b+1) xor b = $1F then good else bad end;
  //coExpres_Expres
  if (b+1) xor (a+1) = $10 then good else bad end;
    
  //////////////////////////////////////////////////////////
	/////////////////////  Operación NOT /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; 
  //coConst
  if not $00 = $FF then good else bad end;
  if not $0F = $F0 then good else bad end;
  //coConst
  if not a = $00 then good else bad end;

  
  //////////////////////////////////////////////////////////
	/////////////////////  FIN DE PRUEBAS  ///////////////////
  //////////////////////////////////////////////////////////
  pinLed := 0;
  delay_ms(50);
  pinLed := 1;
  delay_ms(70);
  pinLed := 0;
end.
