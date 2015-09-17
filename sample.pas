{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 4 MHZ }
{$PROCESSOR PIC16F84}
program BlinkLed;
const
  HIGH = true;
  LOW = false;
var
  STATUS: BYTE absolute $03;
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  RP0 : boolean @STATUS.5;
  RP1 : boolean @STATUS.6;
  pin: boolean absolute PORTB.4;
begin                          
  RP0 := HIGH;
  TRISB := 0;   //all outputs
  RP0 := LOW;
  PORTB := 0;   //init
  delay_ms(1000);  //wait
  while true do begin
    delay_ms(300);
    pin := HIGH;
    delay_ms(300);
    pin := LOW;
  end;
end.
