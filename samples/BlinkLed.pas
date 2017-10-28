{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: bit absolute PORTB.7;
begin
  TRISB := 0;   //all outputs
  while true do
    delay_ms(1000);
    pin := not pin;
  end;
end.

