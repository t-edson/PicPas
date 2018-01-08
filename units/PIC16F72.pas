{
*  UNIT para compilador PicPas
*  SFR del microcontrolador PIC 16F84A
*
*  (C) AguHDz 05-JUL-2017
*  Ultima Actualizacion: 10-JUL-2017
}

{$PROCESSOR PIC16F72}

unit PIC16F72;
 
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
  PORTA_AN4         : bit  absolute PORTA.5;
  PORTA_RA5         : bit  absolute PORTA.5;
  PORTA_T0CKI       : bit  absolute PORTA.4;
  PORTA_RA4         : bit  absolute PORTA.4;
  PORTA_AN3         : bit  absolute PORTA.3;
  PORTA_RA3         : bit  absolute PORTA.3;
  PORTA_AN2         : bit  absolute PORTA.2;
  PORTA_RA2         : bit  absolute PORTA.2;
  PORTA_AN1         : bit  absolute PORTA.1;
  PORTA_RA1         : bit  absolute PORTA.1;
  PORTA_AN0         : bit  absolute PORTA.0;
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
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_SDO         : bit  absolute PORTC.5;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_SCA         : bit  absolute PORTC.4;
  PORTC_SDI         : bit  absolute PORTC.4;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_SCL         : bit  absolute PORTC.3;
  PORTC_SCK         : bit  absolute PORTC.3;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_CCP1        : bit  absolute PORTC.2;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_T1OSI       : bit  absolute PORTC.1;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_T1CKI       : bit  absolute PORTC.0;
  PORTC_T1OSC       : bit  absolute PORTC.0;
  PORTC_RC0         : bit  absolute PORTC.0;  
  PCLATH            : byte absolute $000A;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;  
  PIR1              : byte absolute $000C;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
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
  CCPR1H            : byte absolute CCPR1.HIGH;  // $0016
  CCP1CON           : byte absolute $0017;
  CCP1CON_CCP1X     : bit  absolute CCP1CON.5;
  CCP1CON_CCP1Y     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  ADRES             : byte absolute $001E;  
  ADCON0            : byte absolute $001F;
  ADCON0_ADCS1      : bit  absolute ADCON0.7;
  ADCON0_ADCS2      : bit  absolute ADCON0.6;
  ADCON0_CHS2       : bit  absolute ADCON0.5;
  ADCON0_CHS1       : bit  absolute ADCON0.4;
  ADCON0_CHS0       : bit  absolute ADCON0.3;
  ADCON0_GO_DONE    : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
  OPTION            : byte absolute $0081;
  OPTION_RBPU       : bit  absolute OPTION.7;
  OPTION_INTEDG     : bit  absolute OPTION.6;
  OPTION_T0CS       : bit  absolute OPTION.5;
  OPTION_T0SE       : bit  absolute OPTION.4;
  OPTION_PSA        : bit  absolute OPTION.3;
  OPTION_PS2        : bit  absolute OPTION.2;
  OPTION_PS1        : bit  absolute OPTION.1;
  OPTION_PS0        : bit  absolute OPTION.0;
//---- Por compatibilidad de nombres con versiones anteriores -----
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RBPU   : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
//-----------------------------------------------------------------
  TRISA             : byte absolute $0085;
  TRISB             : byte absolute $0086;
  TRISC             : byte absolute $0087;
  PIE1              : byte absolute $008C;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TME2IE       : bit  absolute PIE1.1;
  PIE1_TME1IE       : bit  absolute PIE1.0;
  PCON              : byte absolute $008E;
  PCON_POR          : bit absolute PCON.1;
  PCON_BOR          : bit absolute PCON.0;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPSTAT           : byte absolute $0094;
  SSPSTAT_SMP       : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE       : bit  absolute SSPSTAT.6;
  SSPSTAT_D_A       : bit  absolute SSPSTAT.5;
  SSPSTAT_P         : bit  absolute SSPSTAT.4;
  SSPSTAT_S         : bit  absolute SSPSTAT.3;
  SSPSTAT_R_W       : bit  absolute SSPSTAT.2;
  SSPSTAT_UA        : bit  absolute SSPSTAT.1;
  SSPSTAT_BF        : bit  absolute SSPSTAT.0;
  ADCON1            : byte absolute $009F;
  ADCON1_PCFG2      : bit  absolute ADCON1.2;
  ADCON1_PCFG1      : bit  absolute ADCON1.1;
  ADCON1_PCFG0      : bit  absolute ADCON1.0;
  PMDATL            : byte absolute $010C;
  PMADRL            : byte absolute $010D;
  PMDATH            : byte absolute $010E;
  PMADRH            : byte absolute $010F;
  PMCON1            : byte absolute $018C;
  PMCON1_RD         : bit absolute PMCON1.0;
   
   
  // CONFIGURATION WORD PIC16F72
// =======================================
// BOREN : Brown-out Reset Enable bit.
// Enable BOREN automatically enable PWRTEN, regardless of the
// value of bit PWRTEN. Ensure the PWRTEN is enable any time
// BOREN is enable.
{$DEFINE _BOREN_OFF    =     $3FBF}    // BOR disabled
{$DEFINE _BOREN_ON     =     $3FFF}    // BOR enabled
// CP : FLASH Program Memory Code Protection bit.
{$DEFINE _CP_ON        =     $3FEF}    // All Memory locations code protected
{$DEFINE _CP_ALL       =     $3FEF}    // All Memory locations code protected
{$DEFINE _CP_OFF       =     $3FFF}    // Code protection off
// PWRTEN : Power-up Timer Enable bit.
{$DEFINE _PWRTEN_ON    =     $3FF7}    // PWRT enabled
{$DEFINE _PWRTEN_OFF   =     $3FFF}    // PWRT disabled
// WDTEN : Watchdog Timer Eneble bit.
{$DEFINE _WDT_OFF      =     $3FFB}    // WDT disabled
{$DEFINE _WDT_ON       =     $3FFF}    // WDT enabled
// FOSC1:FOSC2 : Oscilator Seleccion bits.
{$DEFINE _LP_OSC       =     $3FFC}    // LP oscillator
{$DEFINE _XT_OSC       =     $3FFD}    // XT oscillator
{$DEFINE _HS_OSC       =     $3FFE}    // HS oscillator
{$DEFINE _RC_OSC       =     $3FFF}    // RC oscillator
// =======================================
// The erased (unprogrammed) value of the configuration word is 3FFFFh.
// Configuration Word Address : 2007h. 
   
   
   
   
implementation
end.
