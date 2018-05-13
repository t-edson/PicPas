{Sample program to blink a Led on PORTB.4}
program BlinkLed;
uses PIC16F84A;
{$FREQUENCY 8 MHZ }
var
  pin: bit absolute PORTB.7;
begin
  TRISB := 0;   //all outputs
  while true do
    delay_ms(1000);
    pin := not pin;
  end;
end.

