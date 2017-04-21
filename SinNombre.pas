{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
//  pin: bit absolute PORTB.7;
  //Define output
  out1: boolean absolute PORTB.0;
  //Define inputs 
  in1: boolean absolute PORTB.1;
  in2: boolean absolute PORTB.2;
  in3: boolean absolute PORTB.3;
begin                   
  //Set port direction       
  TRISB := %00000110; 
  
  while true do begin
    in1 and in2;
    if in1 and in2 then 
      out1 := true
    else 
 	    out1 := false;
    delay_ms(500);
//    out1 := not out1;
  end;
end.
