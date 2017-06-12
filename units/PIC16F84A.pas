{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F84A
*
*  (C) AguHDz 05-MAY-2017
*  Ultima Actualizacion: 08-MAY-2017
}
unit PIC16F84A;
 
interface
var
  INDF          : byte absolute $0000;
  TMR0          : byte absolute $0001;
  PCL           : byte absolute $0002;
  STATUS        : byte absolute $0003;
  FSR           : byte absolute $0004;
  PORTA         : byte absolute $0005;
  PORTB         : byte absolute $0006;
  EEDATA        : byte absolute $0008;  
  EEADR         : byte absolute $0009;
  PCLATH        : byte absolute $000A;
  INTCON        : byte absolute $000B;
  OPTION_REG    : byte absolute $0081;
  TRISA         : byte absolute $0085;
  TRISB         : byte absolute $0086;
  EECON1        : byte absolute $0088;
  EECON2        : byte absolute $0089;

const
// Bits del registro STATUS
  STATUS_IRP    = 7;
  STATUS_RP1    = 6;
  STATUS_RP0    = 5;
  STATUS_TO     = 4;
  STATUS_PD     = 3;
  STATUS_Z      = 2;
  STATUS_DC     = 1;
  STATUS_C      = 0;
// Bits del registro PORTA
  PORTA_T0CKI   = 4;
  PORTA_RBA4    = 4;
  PORTA_RBA3    = 3;
  PORTA_RBA2    = 2;
  PORTA_RBA1    = 1;
  PORTA_RBA0    = 0;
// Bits del registro PORTB
  PORTB_RBB7    = 7;
  PORTB_RBB6    = 6;
  PORTB_RBB5    = 5;
  PORTB_RBB4    = 4;
  PORTB_RBB3    = 3;
  PORTB_RBB2    = 2;
  PORTB_RBB1    = 1;
  PORTB_RBB0    = 0;
  PORTB_INT     = 0;
// Bits del registro INTCON
  INTCON_GI1    = 7;
  INTCON_EEIE   = 6;
  INTCON_T0IE   = 5;
  INTCON_INTE   = 4;
  INTCON_RBIE   = 3;
  INTCON_T0IF   = 2;
  INTCON_INTF   = 1;
  INTCON_RBIF   = 0;
// Bits del registro OPTION
  OPTION_RBPU   = 7;
  OPTION_INTEDG = 6;
  OPTION_TOCS   = 5;
  OPTION_T0SE   = 4;
  OPTION_PSA    = 3;
  OPTION_PS2    = 2;
  OPTION_PS1    = 1;
  OPTION_PS0    = 0;
// Bits del registro EECON1
  EECON1_EEIF   = 4;
  EECON1_WRERR  = 3;
  EECON1_WREN   = 2;
  EECON1_WR     = 1;
  EECON1_RD     = 0;
implementation
end.
