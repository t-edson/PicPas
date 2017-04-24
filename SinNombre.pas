{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  GPIO : BYTE absolute $06;
  TRISIO : BYTE absolute $86;
  //Define output
  out1: boolean absolute GPIO.0;
  //Define inputs
  in1: boolean absolute GPIO.1;
  in2: boolean absolute GPIO.2;
begin
  //Set port direction
  TRISIO := %00000110;
  while true do begin
	  out1 := in1 and not in2; 
//    out1 := not out1;
    delay_ms(200);
  end;
end.

