{Rutina de verificación del funcionamiento de los procedimientos y
funciones.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var
  vbyte: byte;

  pinLed: bit absolute GPIO.0;

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
  //Procedimientos de prueba
  procedure proc1;
  begin
    vbyte := 5;
  end;

  procedure proc2(par1: byte);
  begin
    if par1 = 0 then 
      exit;
    else
      vbyte := 10;
    end;  
  end;

var 
  xbyte : byte;
  xword : word;
  xbit  : bit;
  xbool : boolean;
begin
  SetAsOutput(pinLed);
  pinLed := 0;
  //Prueba de procedimiento
  vbyte := 0;
  Proc1;
  if vbyte = 5 then bien else mal end;

  vbyte := 1;
	proc2(0);
  if vbyte = 1 then bien else mal end;

	proc2(1);
  if vbyte = 10 then bien else mal end;

  //Falta implementar soporte para funciones
  
end.
