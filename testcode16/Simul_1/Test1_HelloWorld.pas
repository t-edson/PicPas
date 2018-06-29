{Rutina de verificación del programa Hola Mundo. 
Lo que se prueba aquí, es el funcionamiento báscio y la capacidad de llamar
a la rutina delay_ms(), definiendo el uso del registro H.}
{$FREQUENCY 10Mhz}
{$OUTPUTHEX 'output.hex'}
uses PIC16F84A;
var
  pinLed: bit absolute PORTB.0;
begin
  SetAsOutput(pinLed);
  while true do
    pinLed := not pinLed;
    delay_ms(1000);
  end; 
end.

