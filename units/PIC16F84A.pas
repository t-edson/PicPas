{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F84A
*
*  (C) AguHDz 05-JUN-2017
*  Ultima Actualizacion: 14-JUN-2017
}
unit PIC16F84A;
 
interface
var
// DEFINICION DE BYTES Y BITS DE ZONA MEMORIA SFR.
// Segun los nombres y direcciones en datasheet de Microchip.
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
  PORTA_T0CKI       : bit  absolute PORTA.4;
  PORTA_RBA4        : bit  absolute PORTA.4;
  PORTA_RBA3        : bit  absolute PORTA.3;
  PORTA_RBA2        : bit  absolute PORTA.2;
  PORTA_RBA1        : bit  absolute PORTA.1;
  PORTA_RBA0        : bit  absolute PORTA.0;
  PORTB             : byte absolute $0006;
  PORTB_RBB7        : bit  absolute PORTB.7;
  PORTB_RBB6        : bit  absolute PORTB.6;
  PORTB_RBB5        : bit  absolute PORTB.5;
  PORTB_RBB4        : bit  absolute PORTB.4;
  PORTB_RBB3        : bit  absolute PORTB.3;
  PORTB_RBB2        : bit  absolute PORTB.2;
  PORTB_RBB1        : bit  absolute PORTB.1;
  PORTB_RBB0        : bit  absolute PORTB.0;
  PORTB_INT         : bit  absolute PORTB.0;
  EEDATA            : byte absolute $0008;  
  EEADR             : byte absolute $0009;
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
  EECON1            : byte absolute $0088;
  EECON1_EEIF       : bit  absolute EECON1.4;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $0089;
 
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
// Bits del registro PORTA
  bit_T0CKI  = 4;
  bit_RBA4   = 4;
  bit_RBA3   = 3;
  bit_RBA2   = 2;
  bit_RBA1   = 1;
  bit_RBA0   = 0;
// Bits del registro PORTB
  bit_RBB7   = 7;
  bit_RBB6   = 6;
  bit_RBB5   = 5;
  bit_RBB4   = 4;
  bit_RBB3   = 3;
  bit_RBB2   = 2;
  bit_RBB1   = 1;
  bit_RBB0   = 0;
  bit_INT    = 0;
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