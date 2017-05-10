unit PIC16F84A;
interface
var
  TMR0     :byte absolute $01;
	PCL      :byte absolute $02;
	STATUS   :byte absolute $03;
	FSR 	   :byte absolute $04;
  PORTA    :byte absolute $05;
	PORTB    :byte absolute $06; 
	EDATA    :byte absolute $08;	
	EEADR    :byte absolute $09;
	PCLATH   :byte absolute $0A;
	INTCON   :byte absolute $0B;
OPTION_REG :byte absolute $81;
	TRISA    :byte absolute $85;
	TRISB    :byte absolute $86;
	EECON1   :byte absolute $88;
	EECON2   :byte absolute $89;

implementation
end.
