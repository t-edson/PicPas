{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F84A
*
*  (C) AguHDz 05-JUN-2017
*  Ultima Actualizacion: 23-JUN-2017
}
{$PROCESSOR PIC16F84A}
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
  PORTA_RA4         : bit  absolute PORTA.4;
  PORTA_RA3         : bit  absolute PORTA.3;
  PORTA_RA2         : bit  absolute PORTA.2;
  PORTA_RA1         : bit  absolute PORTA.1;
  PORTA_RA0         : bit  absolute PORTA.0;
  PORTB             : byte absolute $0006;
  PORTB_RB7         : bit  absolute PORTB.7;
  PORTB_RB6         : bit  absolute PORTB.6;
  PORTB_RB5         : bit  absolute PORTB.5;
  PORTB_RB4         : bit  absolute PORTB.4;
  PORTB_RB3         : bit  absolute PORTB.3;
  PORTB_RB2         : bit  absolute PORTB.2;
  PORTB_RB1         : bit  absolute PORTB.1;
  PORTB_RB0         : bit  absolute PORTB.0;
  PORTB_INT         : bit  absolute PORTB.0;
  EEDATA            : byte absolute $0008;  
  EEADR             : byte absolute $0009;
  PCLATH            : byte absolute $000A;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
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
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
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

  //Bits Configuration 
  {$define _CP_ON       =     0x000F}
  {$define _CP_OFF      =     0x3FFF}
  {$define _PWRTE_ON    =     0x3FF7}
  {$define _PWRTE_OFF   =     0x3FFF}
  {$define _WDT_ON      =     0x3FFF}
  {$define _WDT_OFF     =     0x3FFB}
  {$define _LP_OSC      =     0x3FFC}
  {$define _XT_OSC      =     0x3FFD}
  {$define _HS_OSC      =     0x3FFE}
  {$define _RC_OSC      =     0x3FFF}
 

implementation

procedure EEPROM_Read(addr:byte):byte;
begin
   EEADR:=addr;
   EECON1_RD:=1;
   exit(EEDATA);   
end; 

procedure WriteEEPROM(direccion , valor: byte);
begin
  EEADR       := direccion;
  EEDATA      := valor;
  EECON1_WREN := 1;
  EECON2      := $55;
  EECON2      := $AA;
  EECON1_WR   := 1;
  EECON1_WREN := 0;
  repeat until (EECON1_WR = 0);
end;
 
end.
