{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 4 MHZ }
{$PROCESSOR PIC16F84}
program BlinkLed;
const
  HIGH = true;
  LOW = false;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: boolean absolute PORTB.4;
begin                          
  TRISB := 0;   //all outputs
//  PORTB := 0;   //init
  delay_ms(1000);  //wait
  while true do begin
    delay_ms(300);
    pin := HIGH;
    delay_ms(200);
    pin := LOW;
  end;
end.
