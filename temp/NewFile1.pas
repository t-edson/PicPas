{Sample program to read a digital input
in PORTB.4 and put the value in PORTB.5}
{$FREQUENCY 4 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
uses PIC16F84A;
var
  TRISB : BYTE absolute $86;
  PORTB : BYTE absolute $06;
//  STATUS: BYTE absolute $03;
  //pin: bit absolute PORTB.7;
begin                          
//  TRISB := 1;
//  SetAsInput(PORTB.4);
//  SetAsOutput(PORTB.5);
//  while true do
//      PORTB.4 := 1;
//    PORTB.5 := PORTB.4;
//    if PORTB.4 = 1 then PORTB.5 := 1 end;
//    if PORTB.4 = 0 then PORTB.5 := 0 end;
//  end;
end.
    
