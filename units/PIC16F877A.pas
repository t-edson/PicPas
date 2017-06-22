{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F877A
*
*  (C) AguHDz 04-JUN-2017
*  Ultima Actualizacion: 14-JUN-2017
}
 
unit PIC16F877A;
interface
var
  INDF              : byte absolute $0000;
  TMR0              : byte absolute $0001;
  PCL               : byte absolute $0002;
  STATUS            : byte absolute $0003;
  STATUS_IRP        : bit  absolute STATUS.7;
  STATUS_RP1        : bit  absolute STATUS.6;
  STATUS_RP0        : bit  absolute STATUS.5;
  STATUS_TO         : bit  absolute STATUS.4;
  STATUS_PD         : bit  absolute STATUS.3;
  STATUS_Z          : bit  absolute STATUS.2;
  STATUS_DC         : bit  absolute STATUS.1;
  STATUS_C          : bit  absolute STATUS.0;
  FSR               : byte absolute $0004;
  PORTA             : byte absolute $0005;
  PORTB             : byte absolute $0006;
  PORTC             : byte absolute $0007;
  PORTD             : byte absolute $0008;
  PORTE             : byte absolute $0009;
  PCLATH            : byte absolute $000A;
  INTCON            : byte absolute $000B;
  INTCON_GI1        : bit  absolute INTCON.7;
  INTCON_EEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR2              : byte absolute $000D;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  SSPBUF            : byte absolute $0013;
  SSPCON            : byte absolute $0014;
  CCPR1             : word absolute $0015;
  CCPR1L            : byte absolute CCPR1.LOW;   // $0015
  CCPR1H            : byte absolute CCPR1.HIGh;  // $0016
  CCP1CON           : byte absolute $0017;
  RCSTA             : byte absolute $0018;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001A;
  CCPR2             : word absolute $001B;
  CCPR2L            : byte absolute CCPR2.LOW;   // $001B
  CCPR2H            : byte absolute CCPR2.HIGH;  // $001C
  CCP2CON           : byte absolute $001D;
  ADRESH            : byte absolute $001E;
  ADCON0            : byte absolute $001F;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RBPU   : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_TOCS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
  TRISB             : byte absolute $0086;
  TRISC             : byte absolute $0087;
  TRISD             : byte absolute $0088;
  TRISE             : byte absolute $0089;
  PIE1              : byte absolute $008C;
  PIE2              : byte absolute $008D;
  PCON              : byte absolute $008E;
  SSPCON2           : byte absolute $0091;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPSTAT           : byte absolute $0094;
  TXSTA             : byte absolute $0098;
  SPBRG             : byte absolute $0099;
  CMCON             : byte absolute $009C;
  CVRCON            : byte absolute $009D;
  ADRESL            : byte absolute $009E;
  ADCON1            : byte absolute $009F;
  EEDATA            : byte absolute $010C;
  EEADR             : byte absolute $010D;
  EEDATH            : byte absolute $010E;
  EEADRH            : byte absolute $010F;
  EECON1            : byte absolute $018C;
  EECON1_EEIF       : bit  absolute EECON1.4;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $018D;
 
const
// PARA SU USO EN SECCIONES DE PROGRAMA ESCRITAS EN ENSAMBLADOR.
// Segun los nombres y direcciones en datasheet de Microchip.
// Bits del registro STATUS
  bit_IRP    = 7;
  bit_RP1    = 6;
  bit_RP0    = 5;
  bit_TO     = 4;
  bit_PD     = 3;
  bit_Z      = 2;
  bit_DC     = 1;
  bit_C      = 0;
// Bits del registro INTCON
  bit_GI1    = 7;
  bit_EEIE   = 6;
  bit_T0IE   = 5;
  bit_INTE   = 4;
  bit_RBIE   = 3;
  bit_T0IF   = 2;
  bit_INTF   = 1;
  bit_RBIF   = 0;
// Bits del registro OPTION_REG
  bit_RBPU   = 7;
  bit_INTEDG = 6;
  bit_TOCS   = 5;
  bit_T0SE   = 4;
  bit_PSA    = 3;
  bit_PS2    = 2;
  bit_PS1    = 1;
  bit_PS0    = 0;
// Bits del registro EECON1
  bit_EEIF   = 4;
  bit_WRERR  = 3;
  bit_WREN   = 2;
  bit_WR     = 1;
  bit_RD     = 0;
implementation
end.