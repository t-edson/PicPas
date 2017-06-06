{Unit used to test operations of the compiler. 
Include simple functions to generate tones, using a buzzer or led in the pin PORTB.0,
like a way of detect errors on the results of operations.
                                                  By TIto Hinostroza 05/06/2017}
unit UnitTest;
interface
var
  PORTB   : byte absolute $06; 
  pinLed  : bit absolute PORTB.0;
 
//***************************************************************************//
// Rutina de inicializacion del display LCD 16x2.
// Resetea el LCD y lo inicializa en modo 8 bit mode, 2 columnas y cursor off.
//***************************************************************************//
  procedure good;
  procedure bad;
  
implementation

  procedure good;
  begin
    pinLed := 1;
    delay_ms(30);
    pinLed := 0;
    delay_ms(30);
  end;

  procedure bad;
  begin
    pinLed := 1;
    delay_ms(1500);
    pinLed := 0;
    asm SLEEP end
  end;

end.

