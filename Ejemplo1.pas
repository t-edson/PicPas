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
  in3: boolean absolute GPIO.3;
begin
  //Set port direction
  TRISIO := %00000110;
  //Main loop
  while true do begin
    if in1 and in2 then begin
      out1 := true;
	    delay_ms(2000);
		  out1 := false;
			//wait until release
		  while in1 or in2 do begin
      end;
    end else if in1 and in3 then begin
      out1 := true;
	    delay_ms(1000);
		  out1 := false;
	    delay_ms(1000);
      out1 := true;
	    delay_ms(1000);
		  out1 := false;
			//wait until release
		  while in1 or in3 do begin
      end;
    end else begin
 	    out1 := false;
		end;
    delay_ms(100);
  end;
end.
