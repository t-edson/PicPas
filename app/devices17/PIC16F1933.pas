unit PIC16F1933;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1933'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 4096}

interface
var
  INDF0                    : byte absolute $0000;
  INDF1                    : byte absolute $0001;
  PCL                      : byte absolute $0002;
  STATUS                   : byte absolute $0003;
  STATUS_nTO               : bit  absolute STATUS.4;
  STATUS_nPD               : bit  absolute STATUS.3;
  STATUS_Z                 : bit  absolute STATUS.2;
  STATUS_DC                : bit  absolute STATUS.1;
  STATUS_C                 : bit  absolute STATUS.0;
  FSR0L                    : byte absolute $0004;
  FSR0H                    : byte absolute $0005;
  FSR1L                    : byte absolute $0006;
  FSR1H                    : byte absolute $0007;
  BSR                      : byte absolute $0008;
  BSR_BSR4                 : bit  absolute BSR.4;
  BSR_BSR3                 : bit  absolute BSR.3;
  BSR_BSR2                 : bit  absolute BSR.2;
  BSR_BSR1                 : bit  absolute BSR.1;
  BSR_BSR0                 : bit  absolute BSR.0;
  WREG                     : byte absolute $0009;
  PCLATH                   : byte absolute $000A;
  PCLATH_PCLATH6           : bit  absolute PCLATH.6;
  PCLATH_PCLATH5           : bit  absolute PCLATH.5;
  PCLATH_PCLATH4           : bit  absolute PCLATH.4;
  PCLATH_PCLATH3           : bit  absolute PCLATH.3;
  PCLATH_PCLATH2           : bit  absolute PCLATH.2;
  PCLATH_PCLATH1           : bit  absolute PCLATH.1;
  PCLATH_PCLATH0           : bit  absolute PCLATH.0;
  INTCON                   : byte absolute $000B;
  INTCON_GIE               : bit  absolute INTCON.7;
  INTCON_PEIE              : bit  absolute INTCON.6;
  INTCON_TMR0IE            : bit  absolute INTCON.5;
  INTCON_INTE              : bit  absolute INTCON.4;
  INTCON_IOCIE             : bit  absolute INTCON.3;
  INTCON_TMR0IF            : bit  absolute INTCON.2;
  INTCON_INTF              : bit  absolute INTCON.1;
  INTCON_IOCIF             : bit  absolute INTCON.0;
  PORTA                    : byte absolute $000C;
  PORTA_RA7                : bit  absolute PORTA.7;
  PORTA_RA6                : bit  absolute PORTA.6;
  PORTA_RA5                : bit  absolute PORTA.5;
  PORTA_RA4                : bit  absolute PORTA.4;
  PORTA_RA3                : bit  absolute PORTA.3;
  PORTA_RA2                : bit  absolute PORTA.2;
  PORTA_RA1                : bit  absolute PORTA.1;
  PORTA_RA0                : bit  absolute PORTA.0;
  PORTB                    : byte absolute $000D;
  PORTB_RB7                : bit  absolute PORTB.7;
  PORTB_RB6                : bit  absolute PORTB.6;
  PORTB_RB5                : bit  absolute PORTB.5;
  PORTB_RB4                : bit  absolute PORTB.4;
  PORTB_RB3                : bit  absolute PORTB.3;
  PORTB_RB2                : bit  absolute PORTB.2;
  PORTB_RB1                : bit  absolute PORTB.1;
  PORTB_RB0                : bit  absolute PORTB.0;
  PORTC                    : byte absolute $000E;
  PORTC_RC7                : bit  absolute PORTC.7;
  PORTC_RC6                : bit  absolute PORTC.6;
  PORTC_RC5                : bit  absolute PORTC.5;
  PORTC_RC4                : bit  absolute PORTC.4;
  PORTC_RC3                : bit  absolute PORTC.3;
  PORTC_RC2                : bit  absolute PORTC.2;
  PORTC_RC1                : bit  absolute PORTC.1;
  PORTC_RC0                : bit  absolute PORTC.0;
  PORTE                    : byte absolute $0010;
  PORTE_RE3                : bit  absolute PORTE.3;
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_RCIF                : bit  absolute PIR1.5;
  PIR1_TXIF                : bit  absolute PIR1.4;
  PIR1_SSPIF               : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_EEIF                : bit  absolute PIR2.4;
  PIR2_BCLIF               : bit  absolute PIR2.3;
  PIR2_LCDIF               : bit  absolute PIR2.2;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_CCP5IF              : bit  absolute PIR3.6;
  PIR3_CCP4IF              : bit  absolute PIR3.5;
  PIR3_CCP3IF              : bit  absolute PIR3.4;
  PIR3_TMR6IF              : bit  absolute PIR3.3;
  PIR3_TMR4IF              : bit  absolute PIR3.1;
  TMR0                     : byte absolute $0015;
  TMR1L                    : byte absolute $0016;
  TMR1H                    : byte absolute $0017;
  T1CON                    : byte absolute $0018;
  T1CON_TMR1CS1            : bit  absolute T1CON.7;
  T1CON_TMR1CS0            : bit  absolute T1CON.6;
  T1CON_T1CKPS1            : bit  absolute T1CON.5;
  T1CON_T1CKPS0            : bit  absolute T1CON.4;
  T1CON_T1OSCEN            : bit  absolute T1CON.3;
  T1CON_nT1SYNC            : bit  absolute T1CON.2;
  T1CON_TMR1ON             : bit  absolute T1CON.0;
  T1GCON                   : byte absolute $0019;
  T1GCON_TMR1GE            : bit  absolute T1GCON.7;
  T1GCON_T1GPOL            : bit  absolute T1GCON.6;
  T1GCON_T1GTM             : bit  absolute T1GCON.5;
  T1GCON_T1GSPM            : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE       : bit  absolute T1GCON.3;
  T1GCON_T1GVAL            : bit  absolute T1GCON.2;
  T1GCON_T1GSS1            : bit  absolute T1GCON.1;
  T1GCON_T1GSS0            : bit  absolute T1GCON.0;
  TMR2                     : byte absolute $001A;
  PR2                      : byte absolute $001B;
  T2CON                    : byte absolute $001C;
  T2CON_T2OUTPS3           : bit  absolute T2CON.6;
  T2CON_T2OUTPS2           : bit  absolute T2CON.5;
  T2CON_T2OUTPS1           : bit  absolute T2CON.4;
  T2CON_T2OUTPS0           : bit  absolute T2CON.3;
  T2CON_TMR2ON             : bit  absolute T2CON.2;
  T2CON_T2CKPS1            : bit  absolute T2CON.1;
  T2CON_T2CKPS0            : bit  absolute T2CON.0;
  CPSCON0                  : byte absolute $001E;
  CPSCON0_CPSON            : bit  absolute CPSCON0.7;
  CPSCON0_CPSRM            : bit  absolute CPSCON0.6;
  CPSCON0_CPSRNG1          : bit  absolute CPSCON0.3;
  CPSCON0_CPSRNG0          : bit  absolute CPSCON0.2;
  CPSCON0_CPSOUT           : bit  absolute CPSCON0.1;
  CPSCON0_T0XCS            : bit  absolute CPSCON0.0;
  CPSCON1                  : byte absolute $001F;
  CPSCON1_CPSCH2           : bit  absolute CPSCON1.2;
  CPSCON1_CPSCH1           : bit  absolute CPSCON1.1;
  CPSCON1_CPSCH0           : bit  absolute CPSCON1.0;
  TRISA                    : byte absolute $008C;
  TRISA_TRISA7             : bit  absolute TRISA.7;
  TRISA_TRISA6             : bit  absolute TRISA.6;
  TRISA_TRISA5             : bit  absolute TRISA.5;
  TRISA_TRISA4             : bit  absolute TRISA.4;
  TRISA_TRISA3             : bit  absolute TRISA.3;
  TRISA_TRISA2             : bit  absolute TRISA.2;
  TRISA_TRISA1             : bit  absolute TRISA.1;
  TRISA_TRISA0             : bit  absolute TRISA.0;
  TRISB                    : byte absolute $008D;
  TRISB_TRISB7             : bit  absolute TRISB.7;
  TRISB_TRISB6             : bit  absolute TRISB.6;
  TRISB_TRISB5             : bit  absolute TRISB.5;
  TRISB_TRISB4             : bit  absolute TRISB.4;
  TRISB_TRISB3             : bit  absolute TRISB.3;
  TRISB_TRISB2             : bit  absolute TRISB.2;
  TRISB_TRISB1             : bit  absolute TRISB.1;
  TRISB_TRISB0             : bit  absolute TRISB.0;
  TRISC                    : byte absolute $008E;
  TRISC_TRISC7             : bit  absolute TRISC.7;
  TRISC_TRISC6             : bit  absolute TRISC.6;
  TRISC_TRISC5             : bit  absolute TRISC.5;
  TRISC_TRISC4             : bit  absolute TRISC.4;
  TRISC_TRISC3             : bit  absolute TRISC.3;
  TRISC_TRISC2             : bit  absolute TRISC.2;
  TRISC_TRISC1             : bit  absolute TRISC.1;
  TRISC_TRISC0             : bit  absolute TRISC.0;
  TRISE                    : byte absolute $0090;
  TRISE_TRISE3             : bit  absolute TRISE.3;
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_RCIE                : bit  absolute PIE1.5;
  PIE1_TXIE                : bit  absolute PIE1.4;
  PIE1_SSPIE               : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_OSFIE               : bit  absolute PIE2.7;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_EEIE                : bit  absolute PIE2.4;
  PIE2_BCLIE               : bit  absolute PIE2.3;
  PIE2_LCDIE               : bit  absolute PIE2.2;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_CCP5IE              : bit  absolute PIE3.6;
  PIE3_CCP4IE              : bit  absolute PIE3.5;
  PIE3_CCP3IE              : bit  absolute PIE3.4;
  PIE3_TMR6IE              : bit  absolute PIE3.3;
  PIE3_TMR4IE              : bit  absolute PIE3.1;
  OPTION_REG               : byte absolute $0095;
  OPTION_REG_nWPUEN        : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG        : bit  absolute OPTION_REG.6;
  OPTION_REG_TMR0CS        : bit  absolute OPTION_REG.5;
  OPTION_REG_TMR0SE        : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA           : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2           : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1           : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0           : bit  absolute OPTION_REG.0;
  PCON                     : byte absolute $0096;
  PCON_STKOVF              : bit  absolute PCON.7;
  PCON_STKUNF              : bit  absolute PCON.6;
  PCON_nRMCLR              : bit  absolute PCON.3;
  PCON_nRI                 : bit  absolute PCON.2;
  PCON_nPOR                : bit  absolute PCON.1;
  PCON_nBOR                : bit  absolute PCON.0;
  WDTCON                   : byte absolute $0097;
  WDTCON_WDTPS4            : bit  absolute WDTCON.5;
  WDTCON_WDTPS3            : bit  absolute WDTCON.4;
  WDTCON_WDTPS2            : bit  absolute WDTCON.3;
  WDTCON_WDTPS1            : bit  absolute WDTCON.2;
  WDTCON_WDTPS0            : bit  absolute WDTCON.1;
  WDTCON_SWDTEN            : bit  absolute WDTCON.0;
  OSCTUNE                  : byte absolute $0098;
  OSCTUNE_TUN5             : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4             : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3             : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2             : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1             : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0             : bit  absolute OSCTUNE.0;
  OSCCON                   : byte absolute $0099;
  OSCCON_SPLLEN            : bit  absolute OSCCON.7;
  OSCCON_IRCF3             : bit  absolute OSCCON.6;
  OSCCON_IRCF2             : bit  absolute OSCCON.5;
  OSCCON_IRCF1             : bit  absolute OSCCON.4;
  OSCCON_IRCF0             : bit  absolute OSCCON.3;
  OSCCON_SCS1              : bit  absolute OSCCON.1;
  OSCCON_SCS0              : bit  absolute OSCCON.0;
  OSCSTAT                  : byte absolute $009A;
  OSCSTAT_T1OSCR           : bit  absolute OSCSTAT.7;
  OSCSTAT_PLLR             : bit  absolute OSCSTAT.6;
  OSCSTAT_OSTS             : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFR           : bit  absolute OSCSTAT.4;
  OSCSTAT_HFIOFL           : bit  absolute OSCSTAT.3;
  OSCSTAT_MFIOFR           : bit  absolute OSCSTAT.2;
  OSCSTAT_LFIOFR           : bit  absolute OSCSTAT.1;
  OSCSTAT_HFIOFS           : bit  absolute OSCSTAT.0;
  ADRESL                   : byte absolute $009B;
  ADRESH                   : byte absolute $009C;
  ADCON0                   : byte absolute $009D;
  ADCON0_CHS4              : bit  absolute ADCON0.6;
  ADCON0_CHS3              : bit  absolute ADCON0.5;
  ADCON0_CHS2              : bit  absolute ADCON0.4;
  ADCON0_CHS1              : bit  absolute ADCON0.3;
  ADCON0_CHS0              : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE          : bit  absolute ADCON0.1;
  ADCON0_ADON              : bit  absolute ADCON0.0;
  ADCON1                   : byte absolute $009E;
  ADCON1_ADFM              : bit  absolute ADCON1.7;
  ADCON1_ADCS2             : bit  absolute ADCON1.6;
  ADCON1_ADCS1             : bit  absolute ADCON1.5;
  ADCON1_ADCS0             : bit  absolute ADCON1.4;
  ADCON1_ADNREF            : bit  absolute ADCON1.2;
  ADCON1_ADPREF1           : bit  absolute ADCON1.1;
  ADCON1_ADPREF0           : bit  absolute ADCON1.0;
  LATA                     : byte absolute $010C;
  LATA_LATA7               : bit  absolute LATA.7;
  LATA_LATA6               : bit  absolute LATA.6;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
  LATA_LATA3               : bit  absolute LATA.3;
  LATA_LATA2               : bit  absolute LATA.2;
  LATA_LATA1               : bit  absolute LATA.1;
  LATA_LATA0               : bit  absolute LATA.0;
  LATB                     : byte absolute $010D;
  LATB_LATB7               : bit  absolute LATB.7;
  LATB_LATB6               : bit  absolute LATB.6;
  LATB_LATB5               : bit  absolute LATB.5;
  LATB_LATB4               : bit  absolute LATB.4;
  LATB_LATB3               : bit  absolute LATB.3;
  LATB_LATB2               : bit  absolute LATB.2;
  LATB_LATB1               : bit  absolute LATB.1;
  LATB_LATB0               : bit  absolute LATB.0;
  LATC                     : byte absolute $010E;
  LATC_LATC7               : bit  absolute LATC.7;
  LATC_LATC6               : bit  absolute LATC.6;
  LATC_LATC5               : bit  absolute LATC.5;
  LATC_LATC4               : bit  absolute LATC.4;
  LATC_LATC3               : bit  absolute LATC.3;
  LATC_LATC2               : bit  absolute LATC.2;
  LATC_LATC1               : bit  absolute LATC.1;
  LATC_LATC0               : bit  absolute LATC.0;
  LATE                     : byte absolute $0110;
  LATE_LATE3               : bit  absolute LATE.3;
  CM1CON0                  : byte absolute $0111;
  CM1CON0_C1ON             : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT            : bit  absolute CM1CON0.6;
  CM1CON0_C1OE             : bit  absolute CM1CON0.5;
  CM1CON0_C1POL            : bit  absolute CM1CON0.4;
  CM1CON0_C1SP             : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS            : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC           : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0112;
  CM1CON1_C1INTP           : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN           : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH1           : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH0           : bit  absolute CM1CON1.4;
  CM1CON1_C1NCH1           : bit  absolute CM1CON1.1;
  CM1CON1_C1NCH0           : bit  absolute CM1CON1.0;
  CM2CON0                  : byte absolute $0113;
  CM2CON0_C2ON             : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT            : bit  absolute CM2CON0.6;
  CM2CON0_C2OE             : bit  absolute CM2CON0.5;
  CM2CON0_C2POL            : bit  absolute CM2CON0.4;
  CM2CON0_C2SP             : bit  absolute CM2CON0.2;
  CM2CON0_C2HYS            : bit  absolute CM2CON0.1;
  CM2CON0_C2SYNC           : bit  absolute CM2CON0.0;
  CM2CON1                  : byte absolute $0114;
  CM2CON1_C2INTP           : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN           : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH1           : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH0           : bit  absolute CM2CON1.4;
  CM2CON1_C2NCH1           : bit  absolute CM2CON1.1;
  CM2CON1_C2NCH0           : bit  absolute CM2CON1.0;
  CMOUT                    : byte absolute $0115;
  CMOUT_MC2OUT             : bit  absolute CMOUT.1;
  CMOUT_MC1OUT             : bit  absolute CMOUT.0;
  BORCON                   : byte absolute $0116;
  BORCON_SBOREN            : bit  absolute BORCON.7;
  BORCON_BORRDY            : bit  absolute BORCON.0;
  FVRCON                   : byte absolute $0117;
  FVRCON_FVREN             : bit  absolute FVRCON.7;
  FVRCON_FVRRDY            : bit  absolute FVRCON.6;
  FVRCON_TSEN              : bit  absolute FVRCON.5;
  FVRCON_TSRNG             : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1           : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0           : bit  absolute FVRCON.2;
  FVRCON_ADFVR1            : bit  absolute FVRCON.1;
  FVRCON_ADFVR0            : bit  absolute FVRCON.0;
  DACCON0                  : byte absolute $0118;
  DACCON0_DACEN            : bit  absolute DACCON0.7;
  DACCON0_DACLPS           : bit  absolute DACCON0.6;
  DACCON0_DACOE            : bit  absolute DACCON0.5;
  DACCON0_DACPSS1          : bit  absolute DACCON0.3;
  DACCON0_DACPSS0          : bit  absolute DACCON0.2;
  DACCON0_DACNSS           : bit  absolute DACCON0.0;
  DACCON1                  : byte absolute $0119;
  DACCON1_DACR4            : bit  absolute DACCON1.4;
  DACCON1_DACR3            : bit  absolute DACCON1.3;
  DACCON1_DACR2            : bit  absolute DACCON1.2;
  DACCON1_DACR1            : bit  absolute DACCON1.1;
  DACCON1_DACR0            : bit  absolute DACCON1.0;
  SRCON0                   : byte absolute $011A;
  SRCON0_SRLEN             : bit  absolute SRCON0.7;
  SRCON0_SRCLK2            : bit  absolute SRCON0.6;
  SRCON0_SRCLK1            : bit  absolute SRCON0.5;
  SRCON0_SRCLK0            : bit  absolute SRCON0.4;
  SRCON0_SRQEN             : bit  absolute SRCON0.3;
  SRCON0_SRNQEN            : bit  absolute SRCON0.2;
  SRCON0_SRPS              : bit  absolute SRCON0.1;
  SRCON0_SRPR              : bit  absolute SRCON0.0;
  SRCON1                   : byte absolute $011B;
  SRCON1_SRSPE             : bit  absolute SRCON1.7;
  SRCON1_SRSCKE            : bit  absolute SRCON1.6;
  SRCON1_SRSC2E            : bit  absolute SRCON1.5;
  SRCON1_SRSC1E            : bit  absolute SRCON1.4;
  SRCON1_SRRPE             : bit  absolute SRCON1.3;
  SRCON1_SRRCKE            : bit  absolute SRCON1.2;
  SRCON1_SRRC2E            : bit  absolute SRCON1.1;
  SRCON1_SRRC1E            : bit  absolute SRCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_CCP3SEL           : bit  absolute APFCON.6;
  APFCON_T1GSEL            : bit  absolute APFCON.5;
  APFCON_P2BSEL            : bit  absolute APFCON.4;
  APFCON_SRNQSEL           : bit  absolute APFCON.3;
  APFCON_C2OUTSEL          : bit  absolute APFCON.2;
  APFCON_SSSEL             : bit  absolute APFCON.1;
  APFCON_CCP2SEL           : bit  absolute APFCON.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA5             : bit  absolute ANSELA.5;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA3             : bit  absolute ANSELA.3;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
  ANSELB                   : byte absolute $018D;
  ANSELB_ANSB5             : bit  absolute ANSELB.5;
  ANSELB_ANSB4             : bit  absolute ANSELB.4;
  ANSELB_ANSB3             : bit  absolute ANSELB.3;
  ANSELB_ANSB2             : bit  absolute ANSELB.2;
  ANSELB_ANSB1             : bit  absolute ANSELB.1;
  ANSELB_ANSB0             : bit  absolute ANSELB.0;
  EEADRL                   : byte absolute $0191;
  EEADRH                   : byte absolute $0192;
  EEADRH_EEADRH6           : bit  absolute EEADRH.6;
  EEADRH_EEADRH5           : bit  absolute EEADRH.5;
  EEADRH_EEADRH4           : bit  absolute EEADRH.4;
  EEADRH_EEADRH3           : bit  absolute EEADRH.3;
  EEADRH_EEADRH2           : bit  absolute EEADRH.2;
  EEADRH_EEADRH1           : bit  absolute EEADRH.1;
  EEADRH_EEADRH0           : bit  absolute EEADRH.0;
  EEDATL                   : byte absolute $0193;
  EEDATH                   : byte absolute $0194;
  EEDATH_EEDATH5           : bit  absolute EEDATH.5;
  EEDATH_EEDATH4           : bit  absolute EEDATH.4;
  EEDATH_EEDATH3           : bit  absolute EEDATH.3;
  EEDATH_EEDATH2           : bit  absolute EEDATH.2;
  EEDATH_EEDATH1           : bit  absolute EEDATH.1;
  EEDATH_EEDATH0           : bit  absolute EEDATH.0;
  EECON1                   : byte absolute $0195;
  EECON1_EEPGD             : bit  absolute EECON1.7;
  EECON1_CFGS              : bit  absolute EECON1.6;
  EECON1_LWLO              : bit  absolute EECON1.5;
  EECON1_FREE              : bit  absolute EECON1.4;
  EECON1_WRERR             : bit  absolute EECON1.3;
  EECON1_WREN              : bit  absolute EECON1.2;
  EECON1_WR                : bit  absolute EECON1.1;
  EECON1_RD                : bit  absolute EECON1.0;
  EECON2                   : byte absolute $0196;
  RCREG                    : byte absolute $0199;
  TXREG                    : byte absolute $019A;
  SP1BRGL                  : byte absolute $019B;
  SP1BRGH                  : byte absolute $019C;
  RCSTA                    : byte absolute $019D;
  RCSTA_SPEN               : bit  absolute RCSTA.7;
  RCSTA_RX9                : bit  absolute RCSTA.6;
  RCSTA_SREN               : bit  absolute RCSTA.5;
  RCSTA_CREN               : bit  absolute RCSTA.4;
  RCSTA_ADDEN              : bit  absolute RCSTA.3;
  RCSTA_FERR               : bit  absolute RCSTA.2;
  RCSTA_OERR               : bit  absolute RCSTA.1;
  RCSTA_RX9D               : bit  absolute RCSTA.0;
  TXSTA                    : byte absolute $019E;
  TXSTA_CSRC               : bit  absolute TXSTA.7;
  TXSTA_TX9                : bit  absolute TXSTA.6;
  TXSTA_TXEN               : bit  absolute TXSTA.5;
  TXSTA_SYNC               : bit  absolute TXSTA.4;
  TXSTA_SENDB              : bit  absolute TXSTA.3;
  TXSTA_BRGH               : bit  absolute TXSTA.2;
  TXSTA_TRMT               : bit  absolute TXSTA.1;
  TXSTA_TX9D               : bit  absolute TXSTA.0;
  BAUDCON                  : byte absolute $019F;
  BAUDCON_ABDOVF           : bit  absolute BAUDCON.7;
  BAUDCON_RCIDL            : bit  absolute BAUDCON.6;
  BAUDCON_SCKP             : bit  absolute BAUDCON.4;
  BAUDCON_BRG16            : bit  absolute BAUDCON.3;
  BAUDCON_WUE              : bit  absolute BAUDCON.1;
  BAUDCON_ABDEN            : bit  absolute BAUDCON.0;
  WPUB                     : byte absolute $020D;
  WPUB_WPUB7               : bit  absolute WPUB.7;
  WPUB_WPUB6               : bit  absolute WPUB.6;
  WPUB_WPUB5               : bit  absolute WPUB.5;
  WPUB_WPUB4               : bit  absolute WPUB.4;
  WPUB_WPUB3               : bit  absolute WPUB.3;
  WPUB_WPUB2               : bit  absolute WPUB.2;
  WPUB_WPUB1               : bit  absolute WPUB.1;
  WPUB_WPUB0               : bit  absolute WPUB.0;
  WPUE                     : byte absolute $0210;
  WPUE_WPUE3               : bit  absolute WPUE.3;
  SSPBUF                   : byte absolute $0211;
  SSPADD                   : byte absolute $0212;
  SSPMSK                   : byte absolute $0213;
  SSPSTAT                  : byte absolute $0214;
  SSPSTAT_SMP              : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE              : bit  absolute SSPSTAT.6;
  SSPSTAT_D_nA             : bit  absolute SSPSTAT.5;
  SSPSTAT_P                : bit  absolute SSPSTAT.4;
  SSPSTAT_S                : bit  absolute SSPSTAT.3;
  SSPSTAT_R_nW             : bit  absolute SSPSTAT.2;
  SSPSTAT_UA               : bit  absolute SSPSTAT.1;
  SSPSTAT_BF               : bit  absolute SSPSTAT.0;
  SSPCON1                  : byte absolute $0215;
  SSPCON1_WCOL             : bit  absolute SSPCON1.7;
  SSPCON1_SSPOV            : bit  absolute SSPCON1.6;
  SSPCON1_SSPEN            : bit  absolute SSPCON1.5;
  SSPCON1_CKP              : bit  absolute SSPCON1.4;
  SSPCON1_SSPM3            : bit  absolute SSPCON1.3;
  SSPCON1_SSPM2            : bit  absolute SSPCON1.2;
  SSPCON1_SSPM1            : bit  absolute SSPCON1.1;
  SSPCON1_SSPM0            : bit  absolute SSPCON1.0;
  SSPCON2                  : byte absolute $0216;
  SSPCON2_GCEN             : bit  absolute SSPCON2.7;
  SSPCON2_ACKSTAT          : bit  absolute SSPCON2.6;
  SSPCON2_ACKDT            : bit  absolute SSPCON2.5;
  SSPCON2_ACKEN            : bit  absolute SSPCON2.4;
  SSPCON2_RCEN             : bit  absolute SSPCON2.3;
  SSPCON2_PEN              : bit  absolute SSPCON2.2;
  SSPCON2_RSEN             : bit  absolute SSPCON2.1;
  SSPCON2_SEN              : bit  absolute SSPCON2.0;
  SSPCON3                  : byte absolute $0217;
  SSPCON3_ACKTIM           : bit  absolute SSPCON3.7;
  SSPCON3_PCIE             : bit  absolute SSPCON3.6;
  SSPCON3_SCIE             : bit  absolute SSPCON3.5;
  SSPCON3_BOEN             : bit  absolute SSPCON3.4;
  SSPCON3_SDAHT            : bit  absolute SSPCON3.3;
  SSPCON3_SBCDE            : bit  absolute SSPCON3.2;
  SSPCON3_AHEN             : bit  absolute SSPCON3.1;
  SSPCON3_DHEN             : bit  absolute SSPCON3.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_P1M1             : bit  absolute CCP1CON.7;
  CCP1CON_P1M0             : bit  absolute CCP1CON.6;
  CCP1CON_DC1B1            : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0            : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3           : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2           : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1           : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0           : bit  absolute CCP1CON.0;
  PWM1CON                  : byte absolute $0294;
  PWM1CON_P1RSEN           : bit  absolute PWM1CON.7;
  PWM1CON_P1DC6            : bit  absolute PWM1CON.6;
  PWM1CON_P1DC5            : bit  absolute PWM1CON.5;
  PWM1CON_P1DC4            : bit  absolute PWM1CON.4;
  PWM1CON_P1DC3            : bit  absolute PWM1CON.3;
  PWM1CON_P1DC2            : bit  absolute PWM1CON.2;
  PWM1CON_P1DC1            : bit  absolute PWM1CON.1;
  PWM1CON_P1DC0            : bit  absolute PWM1CON.0;
  CCP1AS                   : byte absolute $0295;
  CCP1AS_CCP1ASE           : bit  absolute CCP1AS.7;
  CCP1AS_CCP1AS2           : bit  absolute CCP1AS.6;
  CCP1AS_CCP1AS1           : bit  absolute CCP1AS.5;
  CCP1AS_CCP1AS0           : bit  absolute CCP1AS.4;
  CCP1AS_PSS1AC1           : bit  absolute CCP1AS.3;
  CCP1AS_PSS1AC0           : bit  absolute CCP1AS.2;
  CCP1AS_PSS1BD1           : bit  absolute CCP1AS.1;
  CCP1AS_PSS1BD0           : bit  absolute CCP1AS.0;
  PSTR1CON                 : byte absolute $0296;
  PSTR1CON_STR1SYNC        : bit  absolute PSTR1CON.4;
  PSTR1CON_STR1D           : bit  absolute PSTR1CON.3;
  PSTR1CON_STR1C           : bit  absolute PSTR1CON.2;
  PSTR1CON_STR1B           : bit  absolute PSTR1CON.1;
  PSTR1CON_STR1A           : bit  absolute PSTR1CON.0;
  CCPR2L                   : byte absolute $0298;
  CCPR2H                   : byte absolute $0299;
  CCP2CON                  : byte absolute $029A;
  CCP2CON_P2M1             : bit  absolute CCP2CON.7;
  CCP2CON_P2M0             : bit  absolute CCP2CON.6;
  CCP2CON_DC2B1            : bit  absolute CCP2CON.5;
  CCP2CON_DC2B0            : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3           : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2           : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1           : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0           : bit  absolute CCP2CON.0;
  PWM2CON                  : byte absolute $029B;
  PWM2CON_P2RSEN           : bit  absolute PWM2CON.7;
  PWM2CON_P2DC6            : bit  absolute PWM2CON.6;
  PWM2CON_P2DC5            : bit  absolute PWM2CON.5;
  PWM2CON_P2DC4            : bit  absolute PWM2CON.4;
  PWM2CON_P2DC3            : bit  absolute PWM2CON.3;
  PWM2CON_P2DC2            : bit  absolute PWM2CON.2;
  PWM2CON_P2DC1            : bit  absolute PWM2CON.1;
  PWM2CON_P2DC0            : bit  absolute PWM2CON.0;
  CCP2AS                   : byte absolute $029C;
  CCP2AS_CCP2ASE           : bit  absolute CCP2AS.7;
  CCP2AS_CCP2AS2           : bit  absolute CCP2AS.6;
  CCP2AS_CCP2AS1           : bit  absolute CCP2AS.5;
  CCP2AS_CCP2AS0           : bit  absolute CCP2AS.4;
  CCP2AS_PSS2AC1           : bit  absolute CCP2AS.3;
  CCP2AS_PSS2AC0           : bit  absolute CCP2AS.2;
  CCP2AS_PSS2BD1           : bit  absolute CCP2AS.1;
  CCP2AS_PSS2BD0           : bit  absolute CCP2AS.0;
  PSTR2CON                 : byte absolute $029D;
  PSTR2CON_STR2SYNC        : bit  absolute PSTR2CON.4;
  PSTR2CON_STR2D           : bit  absolute PSTR2CON.3;
  PSTR2CON_STR2C           : bit  absolute PSTR2CON.2;
  PSTR2CON_STR2B           : bit  absolute PSTR2CON.1;
  PSTR2CON_STR2A           : bit  absolute PSTR2CON.0;
  CCPTMRS0                 : byte absolute $029E;
  CCPTMRS0_C4TSEL1         : bit  absolute CCPTMRS0.7;
  CCPTMRS0_C4TSEL0         : bit  absolute CCPTMRS0.6;
  CCPTMRS0_C3TSEL1         : bit  absolute CCPTMRS0.5;
  CCPTMRS0_C3TSEL0         : bit  absolute CCPTMRS0.4;
  CCPTMRS0_C2TSEL1         : bit  absolute CCPTMRS0.3;
  CCPTMRS0_C2TSEL0         : bit  absolute CCPTMRS0.2;
  CCPTMRS0_C1TSEL1         : bit  absolute CCPTMRS0.1;
  CCPTMRS0_C1TSEL0         : bit  absolute CCPTMRS0.0;
  CCPTMRS1                 : byte absolute $029F;
  CCPTMRS1_C5TSEL1         : bit  absolute CCPTMRS1.1;
  CCPTMRS1_C5TSEL0         : bit  absolute CCPTMRS1.0;
  CCPR3L                   : byte absolute $0311;
  CCPR3H                   : byte absolute $0312;
  CCP3CON                  : byte absolute $0313;
  CCP3CON_P3M1             : bit  absolute CCP3CON.7;
  CCP3CON_P3M0             : bit  absolute CCP3CON.6;
  CCP3CON_DC3B1            : bit  absolute CCP3CON.5;
  CCP3CON_DC3B0            : bit  absolute CCP3CON.4;
  CCP3CON_CCP3M3           : bit  absolute CCP3CON.3;
  CCP3CON_CCP3M2           : bit  absolute CCP3CON.2;
  CCP3CON_CCP3M1           : bit  absolute CCP3CON.1;
  CCP3CON_CCP3M0           : bit  absolute CCP3CON.0;
  PWM3CON                  : byte absolute $0314;
  PWM3CON_P3RSEN           : bit  absolute PWM3CON.7;
  PWM3CON_P3DC6            : bit  absolute PWM3CON.6;
  PWM3CON_P3DC5            : bit  absolute PWM3CON.5;
  PWM3CON_P3DC4            : bit  absolute PWM3CON.4;
  PWM3CON_P3DC3            : bit  absolute PWM3CON.3;
  PWM3CON_P3DC2            : bit  absolute PWM3CON.2;
  PWM3CON_P3DC1            : bit  absolute PWM3CON.1;
  PWM3CON_P3DC0            : bit  absolute PWM3CON.0;
  CCP3AS                   : byte absolute $0315;
  CCP3AS_CCP3ASE           : bit  absolute CCP3AS.7;
  CCP3AS_CCP3AS2           : bit  absolute CCP3AS.6;
  CCP3AS_CCP3AS1           : bit  absolute CCP3AS.5;
  CCP3AS_CCP3AS0           : bit  absolute CCP3AS.4;
  CCP3AS_PSS3AC1           : bit  absolute CCP3AS.3;
  CCP3AS_PSS3AC0           : bit  absolute CCP3AS.2;
  CCP3AS_PSS3BD1           : bit  absolute CCP3AS.1;
  CCP3AS_PSS3BD0           : bit  absolute CCP3AS.0;
  PSTR3CON                 : byte absolute $0316;
  PSTR3CON_STR3SYNC        : bit  absolute PSTR3CON.4;
  PSTR3CON_STR3D           : bit  absolute PSTR3CON.3;
  PSTR3CON_STR3C           : bit  absolute PSTR3CON.2;
  PSTR3CON_STR3B           : bit  absolute PSTR3CON.1;
  PSTR3CON_STR3A           : bit  absolute PSTR3CON.0;
  CCPR4L                   : byte absolute $0318;
  CCPR4H                   : byte absolute $0319;
  CCP4CON                  : byte absolute $031A;
  CCP4CON_DC4B1            : bit  absolute CCP4CON.5;
  CCP4CON_DC4B0            : bit  absolute CCP4CON.4;
  CCP4CON_CCP4M3           : bit  absolute CCP4CON.3;
  CCP4CON_CCP4M2           : bit  absolute CCP4CON.2;
  CCP4CON_CCP4M1           : bit  absolute CCP4CON.1;
  CCP4CON_CCP4M0           : bit  absolute CCP4CON.0;
  CCPR5L                   : byte absolute $031C;
  CCPR5H                   : byte absolute $031D;
  CCP5CON                  : byte absolute $031E;
  CCP5CON_DC5B1            : bit  absolute CCP5CON.5;
  CCP5CON_DC5B0            : bit  absolute CCP5CON.4;
  CCP5CON_CCP5M3           : bit  absolute CCP5CON.3;
  CCP5CON_CCP5M2           : bit  absolute CCP5CON.2;
  CCP5CON_CCP5M1           : bit  absolute CCP5CON.1;
  CCP5CON_CCP5M0           : bit  absolute CCP5CON.0;
  IOCBP                    : byte absolute $0394;
  IOCBP_IOCBP7             : bit  absolute IOCBP.7;
  IOCBP_IOCBP6             : bit  absolute IOCBP.6;
  IOCBP_IOCBP5             : bit  absolute IOCBP.5;
  IOCBP_IOCBP4             : bit  absolute IOCBP.4;
  IOCBP_IOCBP3             : bit  absolute IOCBP.3;
  IOCBP_IOCBP2             : bit  absolute IOCBP.2;
  IOCBP_IOCBP1             : bit  absolute IOCBP.1;
  IOCBP_IOCBP0             : bit  absolute IOCBP.0;
  IOCBN                    : byte absolute $0395;
  IOCBN_IOCBN7             : bit  absolute IOCBN.7;
  IOCBN_IOCBN6             : bit  absolute IOCBN.6;
  IOCBN_IOCBN5             : bit  absolute IOCBN.5;
  IOCBN_IOCBN4             : bit  absolute IOCBN.4;
  IOCBN_IOCBN3             : bit  absolute IOCBN.3;
  IOCBN_IOCBN2             : bit  absolute IOCBN.2;
  IOCBN_IOCBN1             : bit  absolute IOCBN.1;
  IOCBN_IOCBN0             : bit  absolute IOCBN.0;
  IOCBF                    : byte absolute $0396;
  IOCBF_IOCBF7             : bit  absolute IOCBF.7;
  IOCBF_IOCBF6             : bit  absolute IOCBF.6;
  IOCBF_IOCBF5             : bit  absolute IOCBF.5;
  IOCBF_IOCBF4             : bit  absolute IOCBF.4;
  IOCBF_IOCBF3             : bit  absolute IOCBF.3;
  IOCBF_IOCBF2             : bit  absolute IOCBF.2;
  IOCBF_IOCBF1             : bit  absolute IOCBF.1;
  IOCBF_IOCBF0             : bit  absolute IOCBF.0;
  TMR4                     : byte absolute $0415;
  PR4                      : byte absolute $0416;
  T4CON                    : byte absolute $0417;
  T4CON_T4OUTPS3           : bit  absolute T4CON.6;
  T4CON_T4OUTPS2           : bit  absolute T4CON.5;
  T4CON_T4OUTPS1           : bit  absolute T4CON.4;
  T4CON_T4OUTPS0           : bit  absolute T4CON.3;
  T4CON_TMR4ON             : bit  absolute T4CON.2;
  T4CON_T4CKPS1            : bit  absolute T4CON.1;
  T4CON_T4CKPS0            : bit  absolute T4CON.0;
  TMR6                     : byte absolute $041C;
  PR6                      : byte absolute $041D;
  T6CON                    : byte absolute $041E;
  T6CON_T6OUTPS3           : bit  absolute T6CON.6;
  T6CON_T6OUTPS2           : bit  absolute T6CON.5;
  T6CON_T6OUTPS1           : bit  absolute T6CON.4;
  T6CON_T6OUTPS0           : bit  absolute T6CON.3;
  T6CON_TMR6ON             : bit  absolute T6CON.2;
  T6CON_T6CKPS1            : bit  absolute T6CON.1;
  T6CON_T6CKPS0            : bit  absolute T6CON.0;
  LCDCON                   : byte absolute $0791;
  LCDCON_LCDEN             : bit  absolute LCDCON.7;
  LCDCON_SLPEN             : bit  absolute LCDCON.6;
  LCDCON_WERR              : bit  absolute LCDCON.5;
  LCDCON_CS1               : bit  absolute LCDCON.3;
  LCDCON_CS0               : bit  absolute LCDCON.2;
  LCDCON_LMUX1             : bit  absolute LCDCON.1;
  LCDCON_LMUX0             : bit  absolute LCDCON.0;
  LCDPS                    : byte absolute $0792;
  LCDPS_WFT                : bit  absolute LCDPS.7;
  LCDPS_BIASMD             : bit  absolute LCDPS.6;
  LCDPS_LCDA               : bit  absolute LCDPS.5;
  LCDPS_WA                 : bit  absolute LCDPS.4;
  LCDPS_LP3                : bit  absolute LCDPS.3;
  LCDPS_LP2                : bit  absolute LCDPS.2;
  LCDPS_LP1                : bit  absolute LCDPS.1;
  LCDPS_LP0                : bit  absolute LCDPS.0;
  LCDREF                   : byte absolute $0793;
  LCDREF_LCDIRE            : bit  absolute LCDREF.7;
  LCDREF_LCDIRS            : bit  absolute LCDREF.6;
  LCDREF_LCDIRI            : bit  absolute LCDREF.5;
  LCDREF_VLCD3PE           : bit  absolute LCDREF.3;
  LCDREF_VLCD2PE           : bit  absolute LCDREF.2;
  LCDREF_VLCD1PE           : bit  absolute LCDREF.1;
  LCDCST                   : byte absolute $0794;
  LCDCST_LCDCST2           : bit  absolute LCDCST.2;
  LCDCST_LCDCST1           : bit  absolute LCDCST.1;
  LCDCST_LCDCST0           : bit  absolute LCDCST.0;
  LCDRL                    : byte absolute $0795;
  LCDRL_LRLAP1             : bit  absolute LCDRL.7;
  LCDRL_LRLAP0             : bit  absolute LCDRL.6;
  LCDRL_LRLBP1             : bit  absolute LCDRL.5;
  LCDRL_LRLBP0             : bit  absolute LCDRL.4;
  LCDRL_LRLAT2             : bit  absolute LCDRL.2;
  LCDRL_LRLAT1             : bit  absolute LCDRL.1;
  LCDRL_LRLAT0             : bit  absolute LCDRL.0;
  LCDSE0                   : byte absolute $0798;
  LCDSE0_SE7               : bit  absolute LCDSE0.7;
  LCDSE0_SE6               : bit  absolute LCDSE0.6;
  LCDSE0_SE5               : bit  absolute LCDSE0.5;
  LCDSE0_SE4               : bit  absolute LCDSE0.4;
  LCDSE0_SE3               : bit  absolute LCDSE0.3;
  LCDSE0_SE2               : bit  absolute LCDSE0.2;
  LCDSE0_SE1               : bit  absolute LCDSE0.1;
  LCDSE0_SE0               : bit  absolute LCDSE0.0;
  LCDSE1                   : byte absolute $0799;
  LCDSE1_SE15              : bit  absolute LCDSE1.7;
  LCDSE1_SE14              : bit  absolute LCDSE1.6;
  LCDSE1_SE13              : bit  absolute LCDSE1.5;
  LCDSE1_SE12              : bit  absolute LCDSE1.4;
  LCDSE1_SE11              : bit  absolute LCDSE1.3;
  LCDSE1_SE10              : bit  absolute LCDSE1.2;
  LCDSE1_SE9               : bit  absolute LCDSE1.1;
  LCDSE1_SE8               : bit  absolute LCDSE1.0;
  LCDDATA0                 : byte absolute $07A0;
  LCDDATA0_SEG7COM0        : bit  absolute LCDDATA0.7;
  LCDDATA0_SEG6COM0        : bit  absolute LCDDATA0.6;
  LCDDATA0_SEG5COM0        : bit  absolute LCDDATA0.5;
  LCDDATA0_SEG4COM0        : bit  absolute LCDDATA0.4;
  LCDDATA0_SEG3COM0        : bit  absolute LCDDATA0.3;
  LCDDATA0_SEG2COM0        : bit  absolute LCDDATA0.2;
  LCDDATA0_SEG1COM0        : bit  absolute LCDDATA0.1;
  LCDDATA0_SEG0COM0        : bit  absolute LCDDATA0.0;
  LCDDATA1                 : byte absolute $07A1;
  LCDDATA1_SEG15COM0       : bit  absolute LCDDATA1.7;
  LCDDATA1_SEG14COM0       : bit  absolute LCDDATA1.6;
  LCDDATA1_SEG13COM0       : bit  absolute LCDDATA1.5;
  LCDDATA1_SEG12COM0       : bit  absolute LCDDATA1.4;
  LCDDATA1_SEG11COM0       : bit  absolute LCDDATA1.3;
  LCDDATA1_SEG10COM0       : bit  absolute LCDDATA1.2;
  LCDDATA1_SEG9COM0        : bit  absolute LCDDATA1.1;
  LCDDATA1_SEG8COM0        : bit  absolute LCDDATA1.0;
  LCDDATA3                 : byte absolute $07A3;
  LCDDATA3_SEG7COM1        : bit  absolute LCDDATA3.7;
  LCDDATA3_SEG6COM1        : bit  absolute LCDDATA3.6;
  LCDDATA3_SEG5COM1        : bit  absolute LCDDATA3.5;
  LCDDATA3_SEG4COM1        : bit  absolute LCDDATA3.4;
  LCDDATA3_SEG3COM1        : bit  absolute LCDDATA3.3;
  LCDDATA3_SEG2COM1        : bit  absolute LCDDATA3.2;
  LCDDATA3_SEG1COM1        : bit  absolute LCDDATA3.1;
  LCDDATA3_SEG0COM1        : bit  absolute LCDDATA3.0;
  LCDDATA4                 : byte absolute $07A4;
  LCDDATA4_SEG15COM1       : bit  absolute LCDDATA4.7;
  LCDDATA4_SEG14COM1       : bit  absolute LCDDATA4.6;
  LCDDATA4_SEG13COM1       : bit  absolute LCDDATA4.5;
  LCDDATA4_SEG12COM1       : bit  absolute LCDDATA4.4;
  LCDDATA4_SEG11COM1       : bit  absolute LCDDATA4.3;
  LCDDATA4_SEG10COM1       : bit  absolute LCDDATA4.2;
  LCDDATA4_SEG9COM1        : bit  absolute LCDDATA4.1;
  LCDDATA4_SEG8COM1        : bit  absolute LCDDATA4.0;
  LCDDATA6                 : byte absolute $07A6;
  LCDDATA6_SEG7COM2        : bit  absolute LCDDATA6.7;
  LCDDATA6_SEG6COM2        : bit  absolute LCDDATA6.6;
  LCDDATA6_SEG5COM2        : bit  absolute LCDDATA6.5;
  LCDDATA6_SEG4COM2        : bit  absolute LCDDATA6.4;
  LCDDATA6_SEG3COM2        : bit  absolute LCDDATA6.3;
  LCDDATA6_SEG2COM2        : bit  absolute LCDDATA6.2;
  LCDDATA6_SEG1COM2        : bit  absolute LCDDATA6.1;
  LCDDATA6_SEG0COM2        : bit  absolute LCDDATA6.0;
  LCDDATA7                 : byte absolute $07A7;
  LCDDATA7_SEG15COM2       : bit  absolute LCDDATA7.7;
  LCDDATA7_SEG14COM2       : bit  absolute LCDDATA7.6;
  LCDDATA7_SEG13COM2       : bit  absolute LCDDATA7.5;
  LCDDATA7_SEG12COM2       : bit  absolute LCDDATA7.4;
  LCDDATA7_SEG11COM2       : bit  absolute LCDDATA7.3;
  LCDDATA7_SEG10COM2       : bit  absolute LCDDATA7.2;
  LCDDATA7_SEG9COM2        : bit  absolute LCDDATA7.1;
  LCDDATA7_SEG8COM2        : bit  absolute LCDDATA7.0;
  LCDDATA9                 : byte absolute $07A9;
  LCDDATA9_SEG7COM3        : bit  absolute LCDDATA9.7;
  LCDDATA9_SEG6COM3        : bit  absolute LCDDATA9.6;
  LCDDATA9_SEG5COM3        : bit  absolute LCDDATA9.5;
  LCDDATA9_SEG4COM3        : bit  absolute LCDDATA9.4;
  LCDDATA9_SEG3COM3        : bit  absolute LCDDATA9.3;
  LCDDATA9_SEG2COM3        : bit  absolute LCDDATA9.2;
  LCDDATA9_SEG1COM3        : bit  absolute LCDDATA9.1;
  LCDDATA9_SEG0COM3        : bit  absolute LCDDATA9.0;
  LCDDATA10                : byte absolute $07AA;
  LCDDATA10_SEG15COM3      : bit  absolute LCDDATA10.7;
  LCDDATA10_SEG14COM3      : bit  absolute LCDDATA10.6;
  LCDDATA10_SEG13COM3      : bit  absolute LCDDATA10.5;
  LCDDATA10_SEG12COM3      : bit  absolute LCDDATA10.4;
  LCDDATA10_SEG11COM3      : bit  absolute LCDDATA10.3;
  LCDDATA10_SEG10COM3      : bit  absolute LCDDATA10.2;
  LCDDATA10_SEG9COM3       : bit  absolute LCDDATA10.1;
  LCDDATA10_SEG8COM3       : bit  absolute LCDDATA10.0;
  STATUS_SHAD              : byte absolute $0FE4;
  STATUS_SHAD_Z_SHAD       : bit  absolute STATUS_SHAD.2;
  STATUS_SHAD_DC_SHAD      : bit  absolute STATUS_SHAD.1;
  STATUS_SHAD_C_SHAD       : bit  absolute STATUS_SHAD.0;
  WREG_SHAD                : byte absolute $0FE5;
  BSR_SHAD                 : byte absolute $0FE6;
  BSR_SHAD_BSR_SHAD4       : bit  absolute BSR_SHAD.4;
  BSR_SHAD_BSR_SHAD3       : bit  absolute BSR_SHAD.3;
  BSR_SHAD_BSR_SHAD2       : bit  absolute BSR_SHAD.2;
  BSR_SHAD_BSR_SHAD1       : bit  absolute BSR_SHAD.1;
  BSR_SHAD_BSR_SHAD0       : bit  absolute BSR_SHAD.0;
  PCLATH_SHAD              : byte absolute $0FE7;
  PCLATH_SHAD_PCLATH_SHAD6 : bit  absolute PCLATH_SHAD.6;
  PCLATH_SHAD_PCLATH_SHAD5 : bit  absolute PCLATH_SHAD.5;
  PCLATH_SHAD_PCLATH_SHAD4 : bit  absolute PCLATH_SHAD.4;
  PCLATH_SHAD_PCLATH_SHAD3 : bit  absolute PCLATH_SHAD.3;
  PCLATH_SHAD_PCLATH_SHAD2 : bit  absolute PCLATH_SHAD.2;
  PCLATH_SHAD_PCLATH_SHAD1 : bit  absolute PCLATH_SHAD.1;
  PCLATH_SHAD_PCLATH_SHAD0 : bit  absolute PCLATH_SHAD.0;
  FSR0L_SHAD               : byte absolute $0FE8;
  FSR0H_SHAD               : byte absolute $0FE9;
  FSR1L_SHAD               : byte absolute $0FEA;
  FSR1H_SHAD               : byte absolute $0FEB;
  STKPTR                   : byte absolute $0FED;
  STKPTR_STKPTR4           : bit  absolute STKPTR.4;
  STKPTR_STKPTR3           : bit  absolute STKPTR.3;
  STKPTR_STKPTR2           : bit  absolute STKPTR.2;
  STKPTR_STKPTR1           : bit  absolute STKPTR.1;
  STKPTR_STKPTR0           : bit  absolute STKPTR.0;
  TOSL                     : byte absolute $0FEE;
  TOSH                     : byte absolute $0FEF;
  TOSH_TOSH6               : bit  absolute TOSH.6;
  TOSH_TOSH5               : bit  absolute TOSH.5;
  TOSH_TOSH4               : bit  absolute TOSH.4;
  TOSH_TOSH3               : bit  absolute TOSH.3;
  TOSH_TOSH2               : bit  absolute TOSH.2;
  TOSH_TOSH1               : bit  absolute TOSH.1;
  TOSH_TOSH0               : bit  absolute TOSH.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-00B:SFR:ALLMAPPED'}  // Banks 0-31 : INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON
  {$SET_STATE_RAM '00C-00E:SFR'}            // Bank 0 : PORTA, PORTB, PORTC
  {$SET_STATE_RAM '010-013:SFR'}            // Bank 0 : PORTE, PIR1, PIR2, PIR3
  {$SET_STATE_RAM '015-01C:SFR'}            // Bank 0 : TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '01E-01F:SFR'}            // Bank 0 : CPSCON0, CPSCON1
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08E:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '090-093:SFR'}            // Bank 1 : TRISE, PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-09E:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10E:SFR'}            // Bank 2 : LATA, LATB, LATC
  {$SET_STATE_RAM '110-11B:SFR'}            // Bank 2 : LATE, CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DACCON0, DACCON1, SRCON0, SRCON1
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18D:SFR'}            // Bank 3 : ANSELA, ANSELB
  {$SET_STATE_RAM '191-196:SFR'}            // Bank 3 : EEADRL, EEADRH, EEDATL, EEDATH, EECON1, EECON2
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RCREG, TXREG, SP1BRGL, SP1BRGH, RCSTA, TXSTA, BAUDCON
  {$SET_STATE_RAM '20D-20D:SFR'}            // Bank 4 : WPUB
  {$SET_STATE_RAM '210-217:SFR'}            // Bank 4 : WPUE, SSPBUF, SSPADD, SSPMSK, SSPSTAT, SSPCON1, SSPCON2, SSPCON3
  {$SET_STATE_RAM '291-296:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, PWM1CON, CCP1AS, PSTR1CON
  {$SET_STATE_RAM '298-29F:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON, PWM2CON, CCP2AS, PSTR2CON, CCPTMRS0, CCPTMRS1
  {$SET_STATE_RAM '311-316:SFR'}            // Bank 6 : CCPR3L, CCPR3H, CCP3CON, PWM3CON, CCP3AS, PSTR3CON
  {$SET_STATE_RAM '318-31A:SFR'}            // Bank 6 : CCPR4L, CCPR4H, CCP4CON
  {$SET_STATE_RAM '31C-31E:SFR'}            // Bank 6 : CCPR5L, CCPR5H, CCP5CON
  {$SET_STATE_RAM '394-396:SFR'}            // Bank 7 : IOCBP, IOCBN, IOCBF
  {$SET_STATE_RAM '415-417:SFR'}            // Bank 8 : TMR4, PR4, T4CON
  {$SET_STATE_RAM '41C-41E:SFR'}            // Bank 8 : TMR6, PR6, T6CON
  {$SET_STATE_RAM '791-795:SFR'}            // Bank 15 : LCDCON, LCDPS, LCDREF, LCDCST, LCDRL
  {$SET_STATE_RAM '798-799:SFR'}            // Bank 15 : LCDSE0, LCDSE1
  {$SET_STATE_RAM '7A0-7A1:SFR'}            // Bank 15 : LCDDATA0, LCDDATA1
  {$SET_STATE_RAM '7A3-7A4:SFR'}            // Bank 15 : LCDDATA3, LCDDATA4
  {$SET_STATE_RAM '7A6-7A7:SFR'}            // Bank 15 : LCDDATA6, LCDDATA7
  {$SET_STATE_RAM '7A9-7AA:SFR'}            // Bank 15 : LCDDATA9, LCDDATA10
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:08'} // PORTE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:FD'} // PIR2 bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:7A'} // PIR3 bits 7,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:CF'} // CPSCON0 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:07'} // CPSCON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:08'} // TRISE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:FD'} // PIE2 bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:7A'} // PIE3 bits 7,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:CF'} // PCON bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '110:08'} // LATE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F3'} // CM1CON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:F7'} // CM2CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:F3'} // CM2CON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:81'} // BORCON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:ED'} // DACCON0 bits 4,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:7F'} // APFCON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:3F'} // ANSELA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:3F'} // ANSELB bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUDCON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:08'} // WPUE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '296:1F'} // PSTR1CON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29D:1F'} // PSTR2CON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29F:03'} // CCPTMRS1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '316:1F'} // PSTR3CON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31A:3F'} // CCP4CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31E:3F'} // CCP5CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:7F'} // T4CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:7F'} // T6CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '791:EF'} // LCDCON bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '793:EE'} // LCDREF bits 4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '794:07'} // LCDCST bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '795:F7'} // LCDRL bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '090:08'} // TRISE bit 3 un-implemented (read as 1)
  {$SET_UNIMP_BITS1 '192:80'} // EEADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : RE3/MCLR/Vpp
  // Pin  2 : RA0/AN0/C12IN0-/C2OUT/SRNQ/SS/Vcap/SEG12
  // Pin  3 : RA1/AN1/C12IN1-/SEG7
  // Pin  4 : RA2/AN2/C2IN+/Vref-/DACOUT/COM2
  // Pin  5 : RA3/AN3/C1IN+/Vref+/COM3/SEG15
  // Pin  6 : RA4/C1OUT/CPS6/T0CKI/SRQ/CCP5/SEG4
  // Pin  7 : RA5/AN4/C2OUT/CPS7/SRNQ/SS/Vcap/SEG5
  // Pin  8 : Vss
  // Pin  9 : RA7/OSC1/CLKIN/SEG2
  // Pin 10 : RA6/OSC2/CLKOUT/Vcap/SEG1
  // Pin 11 : RC0/T1OSO/T1CKI/P2B
  // Pin 12 : RC1/T1OSI/CCP2/P2A
  // Pin 13 : RC2/CCP1/P1A/SEG3
  // Pin 14 : RC3/SCK/SCL/SEG6
  // Pin 15 : RC4/SDI/SDA/T1G/SEG11
  // Pin 16 : RC5/SDO/SEG10
  // Pin 17 : RC6/TX/CK/CCP3/P3A/SEG9
  // Pin 18 : RC7/RX/DT/P3B/SEG8
  // Pin 19 : Vss
  // Pin 20 : Vdd
  // Pin 21 : RB0/AN12/CPS0/CCP4/SRI/INT/SEG0
  // Pin 22 : RB1/AN10/C12IN3-/CPS1/P1C/VLCD1
  // Pin 23 : RB2/AN8/CPS2/P1B/VLCD2
  // Pin 24 : RB3/AN9/C12IN2-/CPS3/CCP2/P2A/VLCD3
  // Pin 25 : RB4/AN11/CPS4/P1D/COM0
  // Pin 26 : RB5/AN13/CPS5/P2B/CCP3/P3A/T1G/COM1
  // Pin 27 : RB6/ICSPCLK/ICDCLK/SEG14
  // Pin 28 : RB7/ICSPDAT/ICDDAT/SEG13


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-2,1-3,2-4,3-5,4-6,5-7,6-10,7-9'} // PORTA
  {$MAP_RAM_TO_PIN '00D:0-21,1-22,2-23,3-24,4-25,5-26,6-27,7-28'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-11,1-12,2-13,3-14,4-15,5-16,6-17,7-18'} // PORTC
  {$MAP_RAM_TO_PIN '010:3-1'} // PORTE


  // -- Bits Configuration --

  // FOSC : Oscillator Selection
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-32 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_ECM     = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_ECL     = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_INTOSC  = $3FFC}  // INTOSC oscillator: I/O function on CLKIN pin
  {$define _FOSC_EXTRC   = $3FFB}  // EXTRC oscillator: External RC circuit connected to CLKIN pin
  {$define _FOSC_HS      = $3FFA}  // HS Oscillator, High-speed crystal/resonator connected between OSC1 and OSC2 pins
  {$define _FOSC_XT      = $3FF9}  // XT Oscillator, Crystal/resonator connected between OSC1 and OSC2 pins
  {$define _FOSC_LP      = $3FF8}  // LP Oscillator, Low-power crystal connected between OSC1 and OSC2 pins

  // WDTE : Watchdog Timer Enable
  {$define _WDTE_ON      = $3FFF}  // WDT enabled
  {$define _WDTE_NSLEEP  = $3FF7}  // WDT enabled while running and disabled in Sleep
  {$define _WDTE_SWDTEN  = $3FEF}  // WDT controlled by the SWDTEN bit in the WDTCON register
  {$define _WDTE_OFF     = $3FE7}  // WDT disabled

  // PWRTE : Power-up Timer Enable
  {$define _PWRTE_OFF    = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON     = $3FDF}  // PWRT enabled

  // MCLRE : MCLR Pin Function Select
  {$define _MCLRE_ON     = $3FFF}  // MCLR/VPP pin function is MCLR
  {$define _MCLRE_OFF    = $3FBF}  // MCLR/VPP pin function is digital input

  // CP : Flash Program Memory Code Protection
  {$define _CP_OFF       = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON        = $3F7F}  // Program memory code protection is enabled

  // CPD : Data Memory Code Protection
  {$define _CPD_OFF      = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON       = $3EFF}  // Data memory code protection is enabled

  // BOREN : Brown-out Reset Enable
  {$define _BOREN_ON     = $3FFF}  // Brown-out Reset enabled
  {$define _BOREN_NSLEEP = $3DFF}  // Brown-out Reset enabled while running and disabled in Sleep
  {$define _BOREN_SBODEN = $3BFF}  // Brown-out Reset controlled by the SBOREN bit in the BORCON register
  {$define _BOREN_OFF    = $39FF}  // Brown-out Reset disabled

  // CLKOUTEN : Clock Out Enable
  {$define _CLKOUTEN_OFF = $3FFF}  // CLKOUT function is disabled. I/O or oscillator function on the CLKOUT pin
  {$define _CLKOUTEN_ON  = $37FF}  // CLKOUT function is enabled on the CLKOUT pin

  // IESO : Internal/External Switchover
  {$define _IESO_ON      = $3FFF}  // Internal/External Switchover mode is enabled
  {$define _IESO_OFF     = $2FFF}  // Internal/External Switchover mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enable
  {$define _FCMEN_ON     = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF    = $1FFF}  // Fail-Safe Clock Monitor is disabled

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF      = $3FFF}  // Write protection off
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to FFFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to 7FFh write protected, 800h to FFFh may be modified by EECON control
  {$define _WRT_ALL      = $3FFC}  // 000h to FFFh write protected, no addresses may be modified by EECON control

  // VCAPEN : Voltage Regulator Capacitor Enable
  {$define _VCAPEN_OFF   = $3FFF}  // All VCAP pin functionality is disabled
  {$define _VCAPEN_RA6   = $3FEF}  // VCAP functionality is enabled on RA6
  {$define _VCAPEN_RA5   = $3FDF}  // VCAP functionality is enabled on RA5
  {$define _VCAPEN_RA0   = $3FCF}  // VCAP functionality is enabled on RA0

  // PLLEN : PLL Enable
  {$define _PLLEN_ON     = $3FFF}  // 4x PLL enabled
  {$define _PLLEN_OFF    = $3EFF}  // 4x PLL disabled

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON    = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF   = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO      = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI      = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
