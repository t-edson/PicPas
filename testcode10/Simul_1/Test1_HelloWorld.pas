{Rutina de verificación del programa Hola Mundo. 
Lo que se prueba aquí, es el funcionamiento báscio y la capacidad de llamar
a la rutina delay_ms(), definiendo el uso del registro H.}
uses PIC10F202;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
begin
  SetAsOutput(GPIO_GP0);
  while true do
    GPIO_GP0 := not GPIO_GP0;
    delay_ms(1000);
  end; 
end.

