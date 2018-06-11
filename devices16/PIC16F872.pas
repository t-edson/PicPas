unit PIC16F872;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F872'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

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
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_SSPIF        : bit  absolute PIR1.5;
  PIR1_CCP1IF       : bit  absolute PIR1.4;
  PIR1_TMR2IF       : bit  absolute PIR1.3;
  PIR1_TMR1IF       : bit  absolute PIR1.2;
  PIR2              : byte absolute $000d;
  PIR2_EEIF         : bit  absolute PIR2.4;
  PIR2_BCLIF        : bit  absolute PIR2.3;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS0     : bit  absolute T2CON.1;
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
  CCPR1L            : byte absolute $0015;
  CCPR1H            : byte absolute $0016;
  CCP1CON           : byte absolute $0017;
  CCP1CON_CCP1X     : bit  absolute CCP1CON.5;
  CCP1CON_CCP1Y     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  ADRESH            : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADCS1      : bit  absolute ADCON0.7;
  ADCON0_ADCS0      : bit  absolute ADCON0.6;
  ADCON0_CHS2       : bit  absolute ADCON0.5;
  ADCON0_CHS1       : bit  absolute ADCON0.4;
  ADCON0_CHS0       : bit  absolute ADCON0.3;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.2;
  ADCON0_ADON       : bit  absolute ADCON0.1;
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
  TRISA_TRISA5      : bit  absolute TRISA.5;
  TRISA_TRISA4      : bit  absolute TRISA.4;
  TRISA_TRISA3      : bit  absolute TRISA.3;
  TRISA_TRISA2      : bit  absolute TRISA.2;
  TRISA_TRISA1      : bit  absolute TRISA.1;
  TRISA_TRISA0      : bit  absolute TRISA.0;
  TRISB             : byte absolute $0086;
  TRISB_TRISB7      : bit  absolute TRISB.7;
  TRISB_TRISB6      : bit  absolute TRISB.6;
  TRISB_TRISB5      : bit  absolute TRISB.5;
  TRISB_TRISB4      : bit  absolute TRISB.4;
  TRISB_TRISB3      : bit  absolute TRISB.3;
  TRISB_TRISB2      : bit  absolute TRISB.2;
  TRISB_TRISB1      : bit  absolute TRISB.1;
  TRISB_TRISB0      : bit  absolute TRISB.0;
  TRISC             : byte absolute $0087;
  TRISC_TRISC7      : bit  absolute TRISC.7;
  TRISC_TRISC6      : bit  absolute TRISC.6;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  PIE1              : byte absolute $008c;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_SSPIE        : bit  absolute PIE1.5;
  PIE1_CCP1IE       : bit  absolute PIE1.4;
  PIE1_TMR2IE       : bit  absolute PIE1.3;
  PIE1_TMR1IE       : bit  absolute PIE1.2;
  PIE2              : byte absolute $008d;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PIE2_BCLIE        : bit  absolute PIE2.3;
  PCON              : byte absolute $008e;
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
  SSPSTAT_D_nA      : bit  absolute SSPSTAT.5;
  SSPSTAT_P         : bit  absolute SSPSTAT.4;
  SSPSTAT_S         : bit  absolute SSPSTAT.3;
  SSPSTAT_R_nW      : bit  absolute SSPSTAT.2;
  SSPSTAT_UA        : bit  absolute SSPSTAT.1;
  SSPSTAT_BF        : bit  absolute SSPSTAT.0;
  ADRESL            : byte absolute $009e;
  ADCON1            : byte absolute $009f;
  ADCON1_ADFM       : bit  absolute ADCON1.7;
  ADCON1_PCFG3      : bit  absolute ADCON1.3;
  ADCON1_PCFG2      : bit  absolute ADCON1.2;
  ADCON1_PCFG1      : bit  absolute ADCON1.1;
  ADCON1_PCFG0      : bit  absolute ADCON1.0;
  EEDATA            : byte absolute $010c;
  EEADR             : byte absolute $010d;
  EEDATH            : byte absolute $010e;
  EEADRH            : byte absolute $010f;
  EECON1            : byte absolute $018c;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_WRERR      : bit  absolute EECON1.6;
  EECON1_WREN       : bit  absolute EECON1.5;
  EECON1_WR         : bit  absolute EECON1.4;
  EECON1_RD         : bit  absolute EECON1.3;
  EECON2            : byte absolute $018d;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '00A-017:SFR'}  // PCLATH, INTCON, PIR1, PIR2, TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-080:SFR'}  // mapped to INDF
  {$SET_STATE_RAM '081-081:SFR'}  // OPTION_REG
  {$SET_STATE_RAM '082-082:SFR'}  // mapped to PCL
  {$SET_STATE_RAM '085-087:SFR'}  // TRISA, TRISB, TRISC
  {$SET_STATE_RAM '08A-08A:SFR'}  // mapped to PCLATH
  {$SET_STATE_RAM '08C-08E:SFR'}  // PIE1, PIE2, PCON
  {$SET_STATE_RAM '091-094:SFR'}  // SSPCON2, PR2, SSPADD, SSPSTAT
  {$SET_STATE_RAM '09E-09F:SFR'}  // ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-102:SFR'}  // mapped to INDF, TMR0, PCL
  {$SET_STATE_RAM '106-106:SFR'}  // mapped to PORTB
  {$SET_STATE_RAM '10A-10A:SFR'}  // mapped to PCLATH
  {$SET_STATE_RAM '10C-10F:SFR'}  // EEDATA, EEADR, EEDATH, EEADRH
  {$SET_STATE_RAM '120-16F:GPR'} 
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-182:SFR'}  // mapped to INDF, OPTION_REG, PCL
  {$SET_STATE_RAM '186-186:SFR'}  // mapped to TRISB
  {$SET_STATE_RAM '18A-18A:SFR'}  // mapped to PCLATH
  {$SET_STATE_RAM '18C-18D:SFR'}  // EECON1, EECON2
  {$SET_STATE_RAM '1A0-1BF:GPR'} 
  {$SET_STATE_RAM '1F0-1FF:GPR'} 


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '080-080:bnk0'} // maps to INDF (bank 0)
  {$SET_MAPPED_RAM '082-084:bnk0'} // maps to PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // maps to PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '0F0-0FF:bnk1'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '100-100:bnk0'} // maps to INDF (bank 0)
  {$SET_MAPPED_RAM '101-101:bnk0'} // maps to TMR0 (bank 0)
  {$SET_MAPPED_RAM '102-104:bnk0'} // maps to PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '106-106:bnk0'} // maps to PORTB (bank 0)
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // maps to PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '120-16F:bnk2'} // maps to area 020-06F (bank 0)
  {$SET_MAPPED_RAM '170-17F:bnk2'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '180-180:bnk0'} // maps to INDF (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)
  {$SET_MAPPED_RAM '182-184:bnk0'} // maps to PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '186-186:bnk1'} // maps to TRISB (bank 1)
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // maps to PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1A0-1BF:bnk3'} // maps to area 0A0-0BF (bank 1)
  {$SET_MAPPED_RAM '1F0-1FF:bnk3'} // maps to area 070-07F (bank 0)


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:3F'} // PORTA
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00D:59'} // PIR2
  {$SET_UNIMP_BITS '010:3F'} // T1CON
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON
  {$SET_UNIMP_BITS '01F:FD'} // ADCON0
  {$SET_UNIMP_BITS '085:3F'} // TRISA
  {$SET_UNIMP_BITS '08D:59'} // PIE2
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '09F:8F'} // ADCON1
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH
  {$SET_UNIMP_BITS '10F:1F'} // EEADRH
  {$SET_UNIMP_BITS '18C:8F'} // EECON1


  // -- PIN mapping --

  // Pin  1 : MCLR/Vpp
  // Pin  2 : RA0/AN0
  // Pin  3 : RA1/AN1
  // Pin  4 : RA2/AN2/Vref-
  // Pin  5 : RA3/AN3/Vref+
  // Pin  6 : RA4/T0CKI
  // Pin  7 : RA5/SS/AN4
  // Pin  8 : Vss
  // Pin  9 : OSC1/CLKI
  // Pin 10 : OSC2/CLKO
  // Pin 11 : RC0/T1OSO/T1CKI
  // Pin 12 : RC1/T1OSI
  // Pin 13 : RC2/CCP1
  // Pin 14 : RC3/SCK/SCL
  // Pin 15 : RC4/SDI/SDA
  // Pin 16 : RC5/SDO
  // Pin 17 : RC6
  // Pin 18 : RC7
  // Pin 19 : Vss
  // Pin 20 : Vdd
  // Pin 21 : RB0/INT
  // Pin 22 : RB1
  // Pin 23 : RB2
  // Pin 24 : RB3/PGM
  // Pin 25 : RB4
  // Pin 26 : RB5
  // Pin 27 : RB6/PGC
  // Pin 28 : RB7/PGD


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-2,1-3,2-4,3-5,4-6,5-7'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-21,1-22,2-23,3-24,4-25,5-26,6-27,7-28'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-11,1-12,2-13,3-14,4-15,5-16,6-17,7-18'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRC = $3FFF}  // RC oscillator
  {$define _FOSC_HS    = $3FFE}  // HS oscillator
  {$define _FOSC_XT    = $3FFD}  // XT oscillator
  {$define _FOSC_LP    = $3FFC}  // LP oscillator

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON    = $3FFF}  // WDT enabled
  {$define _WDTE_OFF   = $3FFB}  // WDT disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF  = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON   = $3FF7}  // PWRT enabled

  // CP : FLASH Program Memory Code Protection bits
  {$define _CP_OFF     = $3FFF}  // Code protection off
  {$define _CP_ON      = $0FCF}  // All memory code protected

  // BOREN : Brown-out Reset Enable bit
  {$define _BOREN_ON   = $3FFF}  // BOR enabled
  {$define _BOREN_OFF  = $3FBF}  // BOR disabled

  // LVP : Low Voltage In-Circuit Serial Programming Enable bit
  {$define _LVP_ON     = $3FFF}  // RB3/PGM pin has PGM function; low-voltage programming enabled
  {$define _LVP_OFF    = $3F7F}  // RB3 is digital I/O, HV on MCLR must be used for programming

  // CPD : Data EE Memory Code Protection
  {$define _CPD_OFF    = $3FFF}  // Code Protection off
  {$define _CPD_ON     = $3EFF}  // Data EEPROM memory code-protected

  // WRT : FLASH Program Memory Write Enable
  {$define _WRT_ALL    = $3FFF}  // Unprotected program memory may be written to by EECON control
  {$define _WRT_OFF    = $3DFF}  // Unprotected program memory may not be written to by EECON control

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF  = $3FFF}  // In-Circuit Debugger disabled, RB6 and RB7 are general purpose I/O pins
  {$define _DEBUG_ON   = $37FF}  // In-Circuit Debugger enabled, RB6 and RB7 are dedicated to the debugger

implementation
end.
