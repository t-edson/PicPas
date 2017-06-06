{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F877A
*
*  (C) AguHDz 04-MAY-2017
*  Ultima Actualizacion: 05-MAY-2017
}
 
unit PIC16F877A;
interface
var
  INDF             : byte absolute $0000;
  TMR0             : byte absolute $0001;
  PCL              : byte absolute $0002;
  STATUS           : byte absolute $0003;
  FSR              : byte absolute $0004;
  PORTA            : byte absolute $0005;
  PORTB            : byte absolute $0006;
  PORTC            : byte absolute $0007;
  PORTD            : byte absolute $0008;
  PORTE            : byte absolute $0009;
  PCLATH           : byte absolute $000A;
  INTCON           : byte absolute $000B;
  PIR1             : byte absolute $000C;
  PIR2             : byte absolute $000D;
  TMR1L            : byte absolute $000E;
  TMR1H            : byte absolute $000F;
  T1CON            : byte absolute $0010;
  TMR2             : byte absolute $0011;
  T2CON            : byte absolute $0012;
  SSPBUF           : byte absolute $0013;
  SSPCON           : byte absolute $0014;
  CCPR1            : word absolute $0015;
  CCPR1L           : byte absolute CCPR1.LOW;   // $0015
  CCPR1H           : byte absolute CCPR1.HIGh;  // $0016
  CCP1CON          : byte absolute $0017;
  RCSTA            : byte absolute $0018;
  TXREG            : byte absolute $0019;
  RCREG            : byte absolute $001A;
  CCPR2            : word absolute $001B;
  CCPR2L           : byte absolute CCPR2.LOW;   // $001B
  CCPR2H           : byte absolute CCPR2.HIGH;  // $001C
  CCP2CON          : byte absolute $001D;
  ADRESH           : byte absolute $001E;
  ADCON0           : byte absolute $001F;
  OPTION_REG       : byte absolute $0081;
  TRISA            : byte absolute $0085;
  TRISB            : byte absolute $0086;
  TRISC            : byte absolute $0087;
  TRISD            : byte absolute $0088;
  TRISE            : byte absolute $0089;
  PIE1             : byte absolute $008C;
  PIE2             : byte absolute $008D;
  PCON             : byte absolute $008E;
  SSPCON2          : byte absolute $0091;
  PR2              : byte absolute $0092;
  SSPADD           : byte absolute $0093;
  SSPSTAT          : byte absolute $0094;
  TXSTA            : byte absolute $0098;
  SPBRG            : byte absolute $0099;
  CMCON            : byte absolute $009C;
  CVRCON           : byte absolute $009D;
  ADRESL           : byte absolute $009E;
  ADCON1           : byte absolute $009F;
  EEDATA           : byte absolute $010C;
  EEADR            : byte absolute $010D;
  EEDATH           : byte absolute $010E;
  EEADRH           : byte absolute $010F;
  EECON1           : byte absolute $018C;
  EECON2           : byte absolute $018D;
implementation
end.