unit PIC16F819;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F819'}
{$SET PIC_MAXFREQ  = 10000000}
{$SET PIC_NPINS    = 18}
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
  PORTA_RA7         : bit  absolute PORTA.7;
  PORTA_RA6         : bit  absolute PORTA.6;
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
  TRISA_TRISA7      : bit  absolute TRISA.7;
  TRISA_TRISA6      : bit  absolute TRISA.6;
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
  PIE1              : byte absolute $008c;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_SSPIE        : bit  absolute PIE1.5;
  PIE1_CCP1IE       : bit  absolute PIE1.4;
  PIE1_TMR2IE       : bit  absolute PIE1.3;
  PIE1_TMR1IE       : bit  absolute PIE1.2;
  PIE2              : byte absolute $008d;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PCON              : byte absolute $008e;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  OSCCON            : byte absolute $008f;
  OSCCON_IRCF2      : bit  absolute OSCCON.6;
  OSCCON_IRCF1      : bit  absolute OSCCON.5;
  OSCCON_IRCF0      : bit  absolute OSCCON.4;
  OSCCON_IOFS       : bit  absolute OSCCON.2;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN5      : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
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
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
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
  EECON1_FREE       : bit  absolute EECON1.6;
  EECON1_WRERR      : bit  absolute EECON1.5;
  EECON1_WREN       : bit  absolute EECON1.4;
  EECON1_WR         : bit  absolute EECON1.3;
  EECON1_RD         : bit  absolute EECON1.2;
  EECON2            : byte absolute $018d;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-006:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB
  {$SET_STATE_RAM '00A-017:SFR'}  // PCLATH, INTCON, PIR1, PIR2, TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-086:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB
  {$SET_STATE_RAM '08A-090:SFR'}  // PCLATH, INTCON, PIE1, PIE2, PCON, OSCCON, OSCTUNE
  {$SET_STATE_RAM '092-094:SFR'}  // PR2, SSPADD, SSPSTAT
  {$SET_STATE_RAM '09E-09F:SFR'}  // ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0FF:GPR'} 
  {$SET_STATE_RAM '100-104:SFR'}  // INDF, TMR0, PCL, STATUS, FSR
  {$SET_STATE_RAM '106-106:SFR'}  // PORTB
  {$SET_STATE_RAM '10A-10F:SFR'}  // PCLATH, INTCON, EEDATA, EEADR, EEDATH, EEADRH
  {$SET_STATE_RAM '120-17F:GPR'} 
  {$SET_STATE_RAM '180-184:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR
  {$SET_STATE_RAM '186-186:SFR'}  // TRISB
  {$SET_STATE_RAM '18A-18D:SFR'}  // PCLATH, INTCON, EECON1, EECON2
  {$SET_STATE_RAM '1A0-1FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '100-104:bnk0'} // INDF, TMR0, PCL, STATUS, FSR
  {$SET_MAPPED_RAM '106-106:bnk0'} // PORTB
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '180-180:bnk0'} // INDF
  {$SET_MAPPED_RAM '181-181:bnk1'} // OPTION_REG
  {$SET_MAPPED_RAM '182-184:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '186-186:bnk1'} // TRISB
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:4F'} // PIR1
  {$SET_UNIMP_BITS '00D:10'} // PIR2
  {$SET_UNIMP_BITS '010:3F'} // T1CON
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON
  {$SET_UNIMP_BITS '01F:FD'} // ADCON0
  {$SET_UNIMP_BITS '08C:4F'} // PIE1
  {$SET_UNIMP_BITS '08D:10'} // PIE2
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '08F:74'} // OSCCON
  {$SET_UNIMP_BITS '090:3F'} // OSCTUNE
  {$SET_UNIMP_BITS '09F:CF'} // ADCON1
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH
  {$SET_UNIMP_BITS '10F:07'} // EEADRH
  {$SET_UNIMP_BITS '18C:9F'} // EECON1


  // -- PIN mapping --

  // Pin  1 : RA2/AN2/Vref-
  // Pin  2 : RA3/AN3/Vref+
  // Pin  3 : RA4/AN4/T0CKI
  // Pin  4 : RA5/MCLR/Vpp
  // Pin  5 : Vss
  // Pin  6 : RB0/INT
  // Pin  7 : RB1/SDI/SDA
  // Pin  8 : RB2/SDO/CCP1
  // Pin  9 : RB3/CCP1/PGM
  // Pin 10 : RB4/SCK/SCL
  // Pin 11 : RB5/SS
  // Pin 12 : RB6/T1OSO/T1CKI/PGC
  // Pin 13 : RB7/T1OSI/PGD
  // Pin 14 : Vdd
  // Pin 15 : RA6/OSC2/CLKO
  // Pin 16 : RA7/OSC1/CLKI
  // Pin 17 : RA0/AN0
  // Pin 18 : RA1/AN1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3,5-4,6-15,7-16'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // CP : Flash Program Memory Code Protection bit
  {$define _CP_OFF         = $3FFF}  // Code protection off
  {$define _CP_ON          = $3FFE}  // All memory locations code-protected

  // CCPMX : CCP1 Pin Selection bit
  {$define _CCPMX_RB2      = $3FFF}  // CCP1 function on RB2
  {$define _CCPMX_RB3      = $3FFD}  // CCP1 function on RB3

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF      = $3FFF}  // In-Circuit Debugger disabled, RB6 and RB7 are general purpose I/O pins
  {$define _DEBUG_ON       = $3FFB}  // In-Circuit Debugger enabled, RB6 and RB7 are dedicated to the debugger

  // WRT : Flash Program Memory Write Enable bits
  {$define _WRT_OFF        = $3FFF}  // Write protection off
  {$define _WRT_512        = $3FF7}  // 0000h to 01FFh write-protected, 0200h to 07FFh may be modified by EECON control
  {$define _WRT_1024       = $3FEF}  // 0000h to 03FFh write-protected, 0400h to 07FFh may be modified by EECON control
  {$define _WRT_1536       = $3FE7}  // 0000h to 05FFh write-protected, 0600h to 07FFh may be modified by EECON control

  // CPD : Data EE Memory Code Protection bit
  {$define _CPD_OFF        = $3FFF}  // Code protection off
  {$define _CPD_ON         = $3FDF}  // Data EE memory locations code-protected

  // LVP : Low-Voltage Programming Enable bit
  {$define _LVP_ON         = $3FFF}  // RB3/PGM pin has PGM function, Low-Voltage Programming enabled
  {$define _LVP_OFF        = $3FBF}  // RB3/PGM pin has digital I/O function, HV on MCLR must be used for programming

  // BOREN : Brown-out Reset Enable bit
  {$define _BOREN_ON       = $3FFF}  // BOR enabled
  {$define _BOREN_OFF      = $3F7F}  // BOR disabled

  // MCLRE : RA5/MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON       = $3FFF}  // RA5/MCLR/VPP pin function is MCLR
  {$define _MCLRE_OFF      = $3EFF}  // RA5/MCLR/VPP pin function is digital I/O, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $3DFF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $3FFF}  // WDT enabled
  {$define _WDTE_OFF       = $3BFF}  // WDT disabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $9FFF}  // EXTRC oscillator; CLKO function on RA6/OSC2/CLKO pin
  {$define _FOSC_EXTRCIO   = $97FF}  // EXTRC oscillator; port I/O function on RA6/OSC2/CLKO pin
  {$define _FOSC_INTOSCCLK = $8FFF}  // INTRC oscillator; CLKO function on RA6/OSC2/CLKO pin and port I/O function on RA7/OSC1/CLKI pin
  {$define _FOSC_INTOSCIO  = $87FF}  // INTRC oscillator; port I/O function on both RA6/OSC2/CLKO pin and RA7/OSC1/CLKI pin
  {$define _FOSC_EC        = $1FFF}  // EXTCLK; port I/O function on RA6/OSC2/CLKO pin
  {$define _FOSC_HS        = $17FF}  // HS oscillator
  {$define _FOSC_XT        = $0FFF}  // XT oscillator
  {$define _FOSC_LP        = $07FF}  // LP oscillator

implementation
end.
