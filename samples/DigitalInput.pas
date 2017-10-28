{Sample program to read a digital input
in PORTB.4 and put the value in PORTB.5}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: bit absolute PORTB.7;
begin                          
  SetAsInput(PORTB.4);
  SetAsOutput(PORTB.5);
  while true do
    delay_ms(100);
    PORTB.5 := PORTB.4;
  end;
end.
