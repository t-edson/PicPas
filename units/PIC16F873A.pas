{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F877A
*
*  (C) AguHDz 04-JUN-2017
*  Ultima Actualizacion: 23-JUN-2017
}
{$PROCESSOR PIC16F873A}
unit PIC16F873A;
 
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
  PORTB             : byte absolute $0006;
  PORTB_RB7         : bit  absolute PORTB.7;
  PORTB_RB6         : bit  absolute PORTB.6;
  PORTB_RB5         : bit  absolute PORTB.5;
  PORTB_RB4         : bit  absolute PORTB.4;
  PORTB_RB3         : bit  absolute PORTB.3;
  PORTB_RB2         : bit  absolute PORTB.2;
  PORTB_RB1         : bit  absolute PORTB.1;
  PORTB_RB0         : bit  absolute PORTB.0;
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_RC0         : bit  absolute PORTC.0;
  PCLATH            : byte absolute $000A;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_PSPIF        : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000D;
  PIR2_CMIF         : bit  absolute PIR2.6;
  PIR2_EEIF         : bit  absolute PIR2.4;
  PIR2_BCLIF        : bit  absolute PIR2.3;
  PIR2_CCP2IF       : bit  absolute PIR2.0;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute TMR2.6;
  T2CON_TOUTPS2     : bit  absolute TMR2.5;
  T2CON_TOUTPS1     : bit  absolute TMR2.4;
  T2CON_TOUTPS0     : bit  absolute TMR2.3;
  T2CON_TMR2ON      : bit  absolute TMR2.2;
  T2CON_T2CKPS1     : bit  absolute TMR2.1;
  T2CON_T2CKPS0     : bit  absolute TMR2.0;
  SSPBUF            : byte absolute $0013;
  SSPCON            : byte absolute $0014;
  SSPCON_WCOL       : bit  absolute SSPCON.7;
  SSPCON_SSPOV      : bit  absolute SSPCON.6;
  SSPCON_SSPEN      : bit  absolute SSPCON.5;
  SSPCON_CKP        : bit  absolute SSPCON.4;
  SSPCON_SSPM3      : bit  absolute SSPCON.3;
  SSPCON_SSPM2      : bit  absolute SSPCON.2;
  SSPCON_SSPM1      : bit  absolute SSPCON.1;
  SSPCON_SSPM0      : bit  absolute SSPCON.0;
  CCPR1             : word absolute $0015;
  CCPR1L            : byte absolute CCPR1.LOW;   // $0015
  CCPR1H            : byte absolute CCPR1.HIGh;  // $0016
  CCP1CON           : byte absolute $0017;
  CCP1CON_CCP1X     : bit  absolute CCP1CON.5;
  CCP1CON_CCP1Y     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  RCSTA             : byte absolute $0018;
  RCSTA_SPEN        : bit  absolute RCSTA.7;
  RCSTA_RX9         : bit  absolute RCSTA.6;
  RCSTA_SREN        : bit  absolute RCSTA.5;
  RCSTA_CREN        : bit  absolute RCSTA.4;
  RCSTA_ADDEN       : bit  absolute RCSTA.3;
  RCSTA_FERR        : bit  absolute RCSTA.2;
  RCSTA_OERR        : bit  absolute RCSTA.1;
  RCSTA_RX9D        : bit  absolute RCSTA.0;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001A;
  CCPR2             : word absolute $001B;
  CCPR2L            : byte absolute CCPR2.LOW;   // $001B
  CCPR2H            : byte absolute CCPR2.HIGH;  // $001C
  CCP2CON           : byte absolute $001D;
  CCP2CON_CCP2X     : bit  absolute CCP2CON.5;
  CCP2CON_CCP2Y     : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3    : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2    : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1    : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0    : bit  absolute CCP2CON.0;
  ADRESH            : byte absolute $001E;
  ADCON0            : byte absolute $001F;
  ADCON0_ADCS1      : bit  absolute ADCON0.7;
  ADCON0_ADCS0      : bit  absolute ADCON0.6;
  ADCON0_CHS2       : bit  absolute ADCON0.5;
  ADCON0_CHS1       : bit  absolute ADCON0.4;
  ADCON0_CHS0       : bit  absolute ADCON0.3;
  ADCON0_GO_DONE    : bit  absolute ADCON0.2;
  ADCON0_ASON       : bit  absolute ADCON0.0;
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
  TRISC             : byte absolute $0087;
  PIE1              : byte absolute $008C;
  PIE1_PSPIE        : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008D;
  PIE2_CMIE         : bit  absolute PIE2.6;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PIE2_BCLIE        : bit  absolute PIE2.3;
  PIE2_CCP2IE       : bit  absolute PIE2.0;
  PCON              : byte absolute $008E;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  SSPCON2           : byte absolute $0091;
  SSPCON2_GCEN      : bit  absolute SSPCON2.7;
  SSPCON2_ACKSTAT   : bit  absolute SSPCON2.6;
  SSPCON2_ACKDT     : bit  absolute SSPCON2.5;
  SSPCON2_ACKEN     : bit  absolute SSPCON2.4;
  SSPCON2_RCEN      : bit  absolute SSPCON2.3;
  SSPCON2_PEN       : bit  absolute SSPCON2.2;
  SSPCON2_RSEN      : bit  absolute SSPCON2.1;
  SSPCON2_SEN       : bit  absolute SSPCON2.0;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPSTAT           : byte absolute $0094;
  SSPSTAT_SMP       : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE       : bit  absolute SSPSTAT.6;
  SSPSTAT_DA        : bit  absolute SSPSTAT.5;
  SSPSTAT_P         : bit  absolute SSPSTAT.4;
  SSPSTAT_S         : bit  absolute SSPSTAT.3;
  SSPSTAT_RW        : bit  absolute SSPSTAT.2;
  SSPSTAT_UA        : bit  absolute SSPSTAT.1;
  SSPSTAT_BF        : bit  absolute SSPSTAT.0;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.2;
  TXSTA_TRMT        : bit  absolute TXSTA.1;
  TXSTA_TX9D        : bit  absolute TXSTA.0;
  SPBRG             : byte absolute $0099;
  CMCON             : byte absolute $009C;
  CMCON_C2OUT       : bit  absolute CMCON.7;
  CMCON_C1OUT       : bit  absolute CMCON.6;
  CMCON_C2INV       : bit  absolute CMCON.5;
  CMCON_C1INV       : bit  absolute CMCON.4;
  CMCON_CIS         : bit  absolute CMCON.3;
  CMCON_CM2         : bit  absolute CMCON.2;
  CMCON_CM1         : bit  absolute CMCON.1;
  CMCON_CM0         : bit  absolute CMCON.0;
  CVRCON            : byte absolute $009D;
  CVRCON_CVREN      : bit  absolute CVRCON.7;
  CVRCON_CVROE      : bit  absolute CVRCON.6;
  CVRCON_CVRR       : bit  absolute CVRCON.5;
  CVRCON_CVR3       : bit  absolute CVRCON.3;
  CVRCON_CVR2       : bit  absolute CVRCON.2;
  CVRCON_CVR1       : bit  absolute CVRCON.1;
  CVRCON_CVR0       : bit  absolute CVRCON.0;
  ADRESL            : byte absolute $009E;
  ADCON1            : byte absolute $009F;
  ADCON1_ADFM       : bit  absolute ADCON1.7;
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
  ADCON1_PCFG3      : bit  absolute ADCON1.3;
  ADCON1_PCFG2      : bit  absolute ADCON1.2;
  ADCON1_PCFG1      : bit  absolute ADCON1.1;
  ADCON1_PCFG0      : bit  absolute ADCON1.0;
  EEDATA            : byte absolute $010C;
  EEADR             : byte absolute $010D;
  EEDATH            : byte absolute $010E;
  EEADRH            : byte absolute $010F;
  EECON1            : byte absolute $018C;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $018D;
 
 // CONFIGURATION WORD PIC16F87XA
// PIC16F873A
// PIC16F874A
// PIC16F876A
// PIC16F877A
// =======================================
// CP : FLASH Program Memory Code Protection bit.
{$DEFINE _CP_ON         =    $1FFF}      // All program memory code-protected
{$DEFINE _CP_ALL        =    $1FFF}      // All program memory code-protected
{$DEFINE _CP_OFF        =    $3FFF}      // Code protection off
// DEBUG : In-Circuit Debugger Mode bit
// RB6-RB7 are dedicaded to the debugger.
{$DEFINE _DEBUG_ON      =    $37FF}      // In-Circuit Debugger enabled, RB6 and RB7 are dedicated to the debugger
{$DEFINE _DEBUG_OFF     =    $3FFF}      // In-Circuit Debugger disabled, RB6 and RB7 are general purpose I/O pins
// WRT1:WRT0 : Flash Program Memory Write Enable bits.
{$DEFINE _WRT_HALF      =    $39FF}      // 0000h to 0FFFh write-protected; 1000h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_1FOURTH   =    $3BFF}      // 0000h to 07FFh write-protected; 0800h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_256       =    $3DFF}      // 0000h to 00FFh write-protected; 0100h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_OFF       =    $3FFF}      // Write protection off; all program memory may be written to by EECON control
// CPD : Data EEPROM Memory Code Protection bit.
{$DEFINE _CPD_ON        =    $3EFF}      // Data EEPROM code-protected
{$DEFINE _CPD_OFF       =    $3FFF}      // Data EEPROM code protection off
// LVP : Low-Voltage (Single-Supply) In-Circuit Serial Programming Enable bit.
{$DEFINE _LVP_OFF       =    $3F7F}      // RB3 is digital I/O, HV on MCLR must be used for programming
{$DEFINE _LVP_ON        =    $3FFF}      // RB3/PGM pin has PGM function; low-voltage programming enabled
// BOREN : Brown-out Reset Enable bit.
// Enable BOREN automatically enable PWRTEN, regardless of the
// value of bit PWRTEN. Ensure the PWRTEN is enable any time
// BOREN is enable.
{$DEFINE _BOREN_OFF     =    $3FBF}      // BOR disabled
{$DEFINE _BOREN_ON      =    $3FFF}      // BOR enabled
// /PWRTEN : Power-up Timer Enable bit.
{$DEFINE _PWRTEN_ON     =    $3FF7}      // PWRT enabled
{$DEFINE _PWRTEN_OFF    =    $3FFF}      // PWRT disabled
// WDTEN : Watchdog Timer Eneble bit.
{$DEFINE _WDT_OFF       =    $3FFB}      // WDT disabled
{$DEFINE _WDT_ON        =    $3FFF}      // WDT enabled
// FOSC1:FOSC2 : Oscilator Seleccion bits.
{$DEFINE _LP_OSC        =    $3FFC}      // LP oscillator
{$DEFINE _XT_OSC        =    $3FFD}      // XT oscillator
{$DEFINE _HS_OSC        =    $3FFE}      // HS oscillator
{$DEFINE _RC_OSC        =    $3FFF}      // RC oscillator
// =======================================
// The erased (unprogrammed) value of the configuration word is 3FFFFh.
// Configuration Word Address : 2007h.


implementation


procedure EEPROM_Read(addr:byte):byte;
begin
   EEADR:=addr;
   EECON1_EEPGD:=0;
   EECON1_RD:=1;
   exit(EEDATA);
end;

procedure EEPROM_Write(addr,data:byte);
begin
   EEADR:=addr;
   EEDATA:=data;
   EECON1_EEPGD:= 0;
   EECON1_WREN := 1;
   INTCON_GIE := 0;
   EECON2:=$55;
   EECON2:=$AA;
   EECON1_WR:= 1;
   INTCON_GIE := 1;
   While EECON1_WR =1 do end;
   EECON1_WREN:= 0;
end;

end.
