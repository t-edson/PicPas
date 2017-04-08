{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
const
  HIGH = true;
  LOW = false;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: boolean absolute PORTB.7;
begin                          
  TRISB := 0;   //all outputs
//  PORTB := 0;   //init
  delay_ms(1000);  //wait
  while true do begin
    delay_ms(1000);
    pin := HIGH;
    delay_ms(1000);
    pin := LOW;
  end;
end.
