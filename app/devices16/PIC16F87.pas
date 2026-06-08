unit PIC16F87;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F87'}
{$SET PIC_MAXFREQ  = 10000000}
{$SET PIC_NPINS    = 18}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 4096}

interface
var
  INDF              : byte absolute $0000;
  TMR0              : byte absolute $0001;
  PCL               : byte absolute $0002;
  STATUS            : byte absolute $0003;
  STATUS_IRP        : bit  absolute STATUS.7;
  STATUS_RP1        : bit  absolute STATUS.6;
  STATUS_RP0        : bit  absolute STATUS.5;
  STATUS_nTO        : bit  absolute STATUS.4;
  STATUS_nPD        : bit  absolute STATUS.3;
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
  PCLATH            : byte absolute $000A;
  PCLATH_PCLATH4    : bit  absolute PCLATH.4;
  PCLATH_PCLATH3    : bit  absolute PCLATH.3;
  PCLATH_PCLATH2    : bit  absolute PCLATH.2;
  PCLATH_PCLATH1    : bit  absolute PCLATH.1;
  PCLATH_PCLATH0    : bit  absolute PCLATH.0;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INT0IE     : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INT0IF     : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000D;
  PIR2_OSFIF        : bit  absolute PIR2.7;
  PIR2_CMIF         : bit  absolute PIR2.6;
  PIR2_EEIF         : bit  absolute PIR2.4;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_T1RUN       : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_nT1SYNC     : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS1     : bit  absolute T2CON.1;
  T2CON_T2CKPS0     : bit  absolute T2CON.0;
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
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_nRBPU  : bit  absolute OPTION_REG.7;
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
  PIE1              : byte absolute $008C;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008D;
  PIE2_OSFIE        : bit  absolute PIE2.7;
  PIE2_CMIE         : bit  absolute PIE2.6;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PCON              : byte absolute $008E;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  OSCCON            : byte absolute $008F;
  OSCCON_IRCF2      : bit  absolute OSCCON.6;
  OSCCON_IRCF1      : bit  absolute OSCCON.5;
  OSCCON_IRCF0      : bit  absolute OSCCON.4;
  OSCCON_OSTS       : bit  absolute OSCCON.3;
  OSCCON_IOFS       : bit  absolute OSCCON.2;
  OSCCON_SCS1       : bit  absolute OSCCON.1;
  OSCCON_SCS0       : bit  absolute OSCCON.0;
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
  WDTCON            : byte absolute $0105;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  EEDATA            : byte absolute $010C;
  EEADR             : byte absolute $010D;
  EEDATH            : byte absolute $010E;
  EEDATH_EEDATH5    : bit  absolute EEDATH.5;
  EEDATH_EEDATH4    : bit  absolute EEDATH.4;
  EEDATH_EEDATH3    : bit  absolute EEDATH.3;
  EEDATH_EEDATH2    : bit  absolute EEDATH.2;
  EEDATH_EEDATH1    : bit  absolute EEDATH.1;
  EEDATH_EEDATH0    : bit  absolute EEDATH.0;
  EEADRH            : byte absolute $010F;
  EEADRH_EEADRH3    : bit  absolute EEADRH.3;
  EEADRH_EEADRH2    : bit  absolute EEADRH.2;
  EEADRH_EEADRH1    : bit  absolute EEADRH.1;
  EEADRH_EEADRH0    : bit  absolute EEADRH.0;
  EECON1            : byte absolute $018C;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_FREE       : bit  absolute EECON1.4;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $018D;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-3 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
                                            // Bank 2 : TMR0
                                            // Bank 3 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-3 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-005:SFR'}            // Bank 0 : PORTA
  {$SET_STATE_RAM '006-006:SFR:ALL'}        // Bank 0 : PORTB
                                            // Bank 1 : TRISB
                                            // Bank 2 : PORTB
                                            // Bank 3 : TRISB
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00D:SFR:ALL'}        // Bank 0 : PIR1, PIR2
                                            // Bank 1 : PIE1, PIE2
                                            // Bank 2 : EEDATA, EEADR
                                            // Bank 3 : EECON1, EECON2
  {$SET_STATE_RAM '00E-01A:SFR'}            // Bank 0 : TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG
  {$SET_STATE_RAM '020-06F:GPR:ALL'}       
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '085-085:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '08E-090:SFR'}            // Bank 1 : PCON, OSCCON, OSCTUNE
  {$SET_STATE_RAM '092-094:SFR'}            // Bank 1 : PR2, SSPADD, SSPSTAT
  {$SET_STATE_RAM '098-099:SFR'}            // Bank 1 : TXSTA, SPBRG
  {$SET_STATE_RAM '09C-09D:SFR'}            // Bank 1 : CMCON, CVRCON
  {$SET_STATE_RAM '105-105:SFR'}            // Bank 2 : WDTCON
  {$SET_STATE_RAM '10E-10F:SFR'}            // Bank 2 : EEDATH, EEADRH
  {$SET_STATE_RAM '110-11F:GPR'}           
  {$SET_STATE_RAM '190-19F:GPR'}           


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '101-101:bnk0'} // maps to TMR0 (bank 0)
  {$SET_MAPPED_RAM '106-106:bnk0'} // maps to PORTB (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)
  {$SET_MAPPED_RAM '186-186:bnk1'} // maps to TRISB (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PIR1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:D0'} // PIR2 bits 5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:7F'} // T1CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // PIE1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:D0'} // PIE2 bits 5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:F7'} // TXSTA bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:EF'} // CVRCON bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '105:1F'} // WDTCON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:0F'} // EEADRH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:9F'} // EECON1 bits 6,5 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RA2/AN2/CVref/Vref-
  // Pin  2 : RA3/AN3/Vref+/C1OUT
  // Pin  3 : RA4/AN4/T0CKI/C2OUT
  // Pin  4 : RA5/MCLR/Vpp
  // Pin  5 : Vss
  // Pin  6 : RB0/INT/CCP1
  // Pin  7 : RB1/SDI/SDA
  // Pin  8 : RB2/SDO/RX/DT
  // Pin  9 : RB3/PGM/CCP1
  // Pin 10 : RB4/SCK/SCL
  // Pin 11 : RB5/SS/TX/CK
  // Pin 12 : RB6/AN5/PGC/T1OSO/T1CKI
  // Pin 13 : RB7/AN6/PGD/T1OSI
  // Pin 14 : Vdd
  // Pin 15 : RA6/OSC2/CLKO
  // Pin 16 : RA7/OSC1/CLKI
  // Pin 17 : RA0/AN0
  // Pin 18 : RA1/AN1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3,5-4,6-15,7-16'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $3FFF}  // EXTRC oscillator; CLKO function on RA6/OSC2/CLKO
  {$define _FOSC_EXTRCIO   = $3FFE}  // EXTRC oscillator; port I/O function on RA6/OSC2/CLKO
  {$define _FOSC_INTOSCCLK = $3FFD}  // INTRC oscillator; CLKO function on RA6/OSC2/CLKO pin and port I/O function on RA7/OSC1/CLKI pin
  {$define _FOSC_INTOSCIO  = $3FFC}  // INTRC oscillator; port I/O function on both RA6/OSC2/CLKO pin and RA7/OSC1/CLKI pin
  {$define _FOSC_EC        = $3FEF}  // ECIO; port I/O function on RA6/OSC2/CLKO
  {$define _FOSC_HS        = $3FEE}  // HS oscillator
  {$define _FOSC_XT        = $3FED}  // XT oscillator
  {$define _FOSC_LP        = $3FEC}  // LP oscillator

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $3FFF}  // WDT enabled
  {$define _WDTE_OFF       = $3FFB}  // WDT disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $3FF7}  // PWRT enabled

  // MCLRE : RA5/MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON       = $3FFF}  // RA5/MCLR/VPP pin function is MCLR
  {$define _MCLRE_OFF      = $3FDF}  // RA5/MCLR/VPP pin function is digital I/O, MCLR internally tied to VDD

  // BOREN : Brown-out Reset Enable bit
  {$define _BOREN_ON       = $3FFF}  // BOR enabled
  {$define _BOREN_OFF      = $3FBF}  // BOR disabled

  // LVP : Low-Voltage Programming Enable bit
  {$define _LVP_ON         = $3FFF}  // RB3/PGM pin has PGM function, Low-Voltage Programming enabled
  {$define _LVP_OFF        = $3F7F}  // RB3 is digital I/O, HV on MCLR must be used for programming

  // CPD : Data EE Memory Code Protection bit
  {$define _CPD_OFF        = $3FFF}  // Code protection off
  {$define _CPD_ON         = $3EFF}  // Data EE memory code-protected

  // WRT : Flash Program Memory Write Enable bits
  {$define _WRT_OFF        = $3FFF}  // Write protection off
  {$define _WRT_256        = $3DFF}  // 0000h to 00FFh write-protected, 0100h to 0FFFh may be modified by EECON control
  {$define _WRT_2048       = $3BFF}  // 0000h to 07FFh write-protected, 0800h to 0FFFh may be modified by EECON control
  {$define _WRT_ALL        = $39FF}  // 0000h to 0FFFh write-protected

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF      = $3FFF}  // In-Circuit Debugger disabled, RB6 and RB7 are general purpose I/O pins
  {$define _DEBUG_ON       = $37FF}  // In-Circuit Debugger enabled, RB6 and RB7 are dedicated to the debugger

  // CCPMX : CCP1 Pin Selection bit
  {$define _CCPMX_RB0      = $3FFF}  // CCP1 function on RB0
  {$define _CCPMX_RB3      = $2FFF}  // CCP1 function on RB3

  // CP : Flash Program Memory Code Protection bit
  {$define _CP_OFF         = $3FFF}  // Code protection off
  {$define _CP_ON          = $1FFF}  // 0000h to 0FFFh code-protected (all protected)

  // FCMEN : Fail-Safe Clock Monitor Enable bit
  {$define _FCMEN_ON       = $3FFF}  // Fail-Safe Clock Monitor enabled
  {$define _FCMEN_OFF      = $3FFE}  // Fail-Safe Clock Monitor disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON        = $3FFF}  // Internal External Switchover mode enabled
  {$define _IESO_OFF       = $3FFD}  // Internal External Switchover mode disabled

implementation
end.
