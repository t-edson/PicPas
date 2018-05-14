{$PROCESSOR PIC12F675A}
unit PIC12F675A;
 
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
  PORTA_RA5         : bit  absolute PORTA.5;
  PORTA_RA4         : bit  absolute PORTA.4;
  PORTA_RA3         : bit  absolute PORTA.3;
  PORTA_RA2         : bit  absolute PORTA.2;
  PORTA_RA1         : bit  absolute PORTA.1;
  PORTA_RA0         : bit  absolute PORTA.0;
  PCLATH            : byte absolute $000A;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_GPIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_GPIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_CMIF         : bit  absolute PIR1.3;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  T1CON             : byte absolute $0010;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  CMCON             : byte absolute $0019;
  CMCON_COUT        : bit  absolute CMCON.6;
  CMCON_CINV        : bit  absolute CMCON.4;
  CMCON_CIS         : bit  absolute CMCON.3;
  CMCON_CM2         : bit  absolute CMCON.2;
  CMCON_CM1         : bit  absolute CMCON.1;
  CMCON_CM0         : bit  absolute CMCON.0;
  ADRESH            : byte absolute $001E;
  ADCON0            : byte absolute $001F;
  ADCON0_ADFM       : bit  absolute ADCON0.7;
  ADCON0_VCFG       : bit  absolute ADCON0.6;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_DONE    : bit  absolute ADCON0.1;
  ADCON0_ASON       : bit  absolute ADCON0.0;
  
  EEDATA            : byte absolute $009A;
  EEADR             : byte absolute $009B;
  EECON1            : byte absolute $009C;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $009D;
  
  ADRESL            : byte absolute $009E;
  ANSEL             : byte absolute $009F;
  ANSEL_ADCS2       : bit  absolute ANSEL.6;
  ANSEL_ADCS1       : bit  absolute ANSEL.5;
  ANSEL_ADCS0       : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;
  
  
  /////////////////////////////////////////////////
// Config Register
/////////////////////////////////////////////////
//#define _CONFIG			   0x2007

////////////////////////////////////////////////////////////////////////////
//
//	   Configuration Bits
//
////////////////////////////////////////////////////////////////////////////

{$define _CPD_ON       =     0x3EFF}
{$define _CPD_OFF      =     0x3FFF}
{$define _CP_ON        =     0x3F7F}
{$define _CP_OFF       =     0x3FFF} 
{$define _BODEN_ON       =     0x3FFF}
{$define _BODEN_OFF       =     0x3FBF}
{$define _MCLRE_ON       =     0x3FFF}
{$define _MCLRE_OFF       =     0x3FDF}
{$define _PWRTE_OFF       =     0x3FFF}
{$define _PWRTE_ON       =     0x3FEF}
{$define _WDT_ON       =     0x3FFF} 
{$define _WDT_OFF       =     0x3FF7} 
{$define _LP_OSC       =     0x3FF8} 
{$define _XT_OSC       =     0x3FF9} 
{$define _HS_OSC       =     0x3FFA}
{$define _EC_OSC       =     0x3FFB}
{$define _INTRC_OSC_NOCLKOUT     =     0x3FFC}
{$define _INTRC_OSC_CLKOUT       =     0x3FFD}
{$define _EXTRC_OSC_NOCLKOUT     =     0x3FFE}
{$define _EXTRC_OSC_CLKOUT       =     0x3FFF} 
 
 
implementation
end.
