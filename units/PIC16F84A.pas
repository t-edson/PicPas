{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F84A
*
*  (C) AguHDz 05-MAY-2017
*  Ultima Actualizacion: 05-MAY-2017
}
unit PIC16F84A;
 
interface
var
  INDF             : byte absolute $0000;
  TMR0             : byte absolute $0001;
  PCL              : byte absolute $0002;
  STATUS           : byte absolute $0003;
  FSR              : byte absolute $0004;
  PORTA            : byte absolute $0005;
  PORTB            : byte absolute $0006;
  EEDATA           : byte absolute $0008;  
  EEADR            : byte absolute $0009;
  PCLATH           : byte absolute $000A;
  INTCON           : byte absolute $000B;
  OPTION_REG       : byte absolute $0081;
  TRISA            : byte absolute $0085;
  TRISB            : byte absolute $0086;
  EECON1           : byte absolute $0088;
  EECON2           : byte absolute $0089;
implementation
end.