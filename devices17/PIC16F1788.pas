unit PIC16F1788;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1788'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 8}
{$SET PIC_MAXFLASH = 16384}

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
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_EEIF                : bit  absolute PIR2.4;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_C4IF                : bit  absolute PIR2.2;
  PIR2_C3IF                : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_CCP3IF              : bit  absolute PIR3.4;
  PIR4                     : byte absolute $0014;
  PIR4_PSMC4TIF            : bit  absolute PIR4.7;
  PIR4_PSMC3TIF            : bit  absolute PIR4.6;
  PIR4_PSMC2TIF            : bit  absolute PIR4.5;
  PIR4_PSMC1TIF            : bit  absolute PIR4.4;
  PIR4_PSMC4SIF            : bit  absolute PIR4.3;
  PIR4_PSMC3SIF            : bit  absolute PIR4.2;
  PIR4_PSMC2SIF            : bit  absolute PIR4.1;
  PIR4_PSMC1SIF            : bit  absolute PIR4.0;
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
  T1GCON_T1GGO             : bit  absolute T1GCON.3;
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
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_OSFIE               : bit  absolute PIE2.7;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_EEIE                : bit  absolute PIE2.4;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_C4IE                : bit  absolute PIE2.2;
  PIE2_C3IE                : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_CCP3IE              : bit  absolute PIE3.4;
  PIE4                     : byte absolute $0094;
  PIE4_PSMC4TIE            : bit  absolute PIE4.7;
  PIE4_PSMC3TIE            : bit  absolute PIE4.6;
  PIE4_PSMC2TIE            : bit  absolute PIE4.5;
  PIE4_PSMC1TIE            : bit  absolute PIE4.4;
  PIE4_PSMC4SIE            : bit  absolute PIE4.3;
  PIE4_PSMC3SIE            : bit  absolute PIE4.2;
  PIE4_PSMC2SIE            : bit  absolute PIE4.1;
  PIE4_PSMC1SIE            : bit  absolute PIE4.0;
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
  PCON_nRWDT               : bit  absolute PCON.4;
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
  ADCON0_ADRMD             : bit  absolute ADCON0.7;
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
  ADCON2                   : byte absolute $009F;
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.4;
  ADCON2_CHSN3             : bit  absolute ADCON2.3;
  ADCON2_CHSN2             : bit  absolute ADCON2.2;
  ADCON2_CHSN1             : bit  absolute ADCON2.1;
  ADCON2_CHSN0             : bit  absolute ADCON2.0;
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
  CM1CON0                  : byte absolute $0111;
  CM1CON0_C1ON             : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT            : bit  absolute CM1CON0.6;
  CM1CON0_C1OE             : bit  absolute CM1CON0.5;
  CM1CON0_C1POL            : bit  absolute CM1CON0.4;
  CM1CON0_C1ZLF            : bit  absolute CM1CON0.3;
  CM1CON0_C1SP             : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS            : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC           : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0112;
  CM1CON1_C1INTP           : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN           : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH2           : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH1           : bit  absolute CM1CON1.4;
  CM1CON1_C1PCH0           : bit  absolute CM1CON1.3;
  CM1CON1_C1NCH2           : bit  absolute CM1CON1.2;
  CM1CON1_C1NCH1           : bit  absolute CM1CON1.1;
  CM1CON1_C1NCH0           : bit  absolute CM1CON1.0;
  CM2CON0                  : byte absolute $0113;
  CM2CON0_C2ON             : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT            : bit  absolute CM2CON0.6;
  CM2CON0_C2OE             : bit  absolute CM2CON0.5;
  CM2CON0_C2POL            : bit  absolute CM2CON0.4;
  CM2CON0_C2ZLF            : bit  absolute CM2CON0.3;
  CM2CON0_C2SP             : bit  absolute CM2CON0.2;
  CM2CON0_C2HYS            : bit  absolute CM2CON0.1;
  CM2CON0_C2SYNC           : bit  absolute CM2CON0.0;
  CM2CON1                  : byte absolute $0114;
  CM2CON1_C2INTP           : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN           : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH2           : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH1           : bit  absolute CM2CON1.4;
  CM2CON1_C2PCH0           : bit  absolute CM2CON1.3;
  CM2CON1_C2NCH2           : bit  absolute CM2CON1.2;
  CM2CON1_C2NCH1           : bit  absolute CM2CON1.1;
  CM2CON1_C2NCH0           : bit  absolute CM2CON1.0;
  CMOUT                    : byte absolute $0115;
  CMOUT_MC4OUT             : bit  absolute CMOUT.3;
  CMOUT_MC3OUT             : bit  absolute CMOUT.2;
  CMOUT_MC2OUT             : bit  absolute CMOUT.1;
  CMOUT_MC1OUT             : bit  absolute CMOUT.0;
  BORCON                   : byte absolute $0116;
  BORCON_SBOREN            : bit  absolute BORCON.7;
  BORCON_BORFS             : bit  absolute BORCON.6;
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
  DAC1CON0                 : byte absolute $0118;
  DAC1CON0_DAC1EN          : bit  absolute DAC1CON0.7;
  DAC1CON0_DAC1OE1         : bit  absolute DAC1CON0.5;
  DAC1CON0_DAC1OE2         : bit  absolute DAC1CON0.4;
  DAC1CON0_DAC1PSS1        : bit  absolute DAC1CON0.3;
  DAC1CON0_DAC1PSS0        : bit  absolute DAC1CON0.2;
  DAC1CON0_DAC1NSS         : bit  absolute DAC1CON0.0;
  DAC1CON1                 : byte absolute $0119;
  CM4CON0                  : byte absolute $011A;
  CM4CON0_C4ON             : bit  absolute CM4CON0.7;
  CM4CON0_C4OUT            : bit  absolute CM4CON0.6;
  CM4CON0_C4OE             : bit  absolute CM4CON0.5;
  CM4CON0_C4POL            : bit  absolute CM4CON0.4;
  CM4CON0_C4ZLF            : bit  absolute CM4CON0.3;
  CM4CON0_C4SP             : bit  absolute CM4CON0.2;
  CM4CON0_C4HYS            : bit  absolute CM4CON0.1;
  CM4CON0_C4SYNC           : bit  absolute CM4CON0.0;
  CM4CON1                  : byte absolute $011B;
  CM4CON1_C4INTP           : bit  absolute CM4CON1.7;
  CM4CON1_C4INTN           : bit  absolute CM4CON1.6;
  CM4CON1_C4PCH2           : bit  absolute CM4CON1.5;
  CM4CON1_C4PCH1           : bit  absolute CM4CON1.4;
  CM4CON1_C4PCH0           : bit  absolute CM4CON1.3;
  CM4CON1_C4NCH2           : bit  absolute CM4CON1.2;
  CM4CON1_C4NCH1           : bit  absolute CM4CON1.1;
  CM4CON1_C4NCH0           : bit  absolute CM4CON1.0;
  APFCON2                  : byte absolute $011C;
  APFCON2_SSSEL1           : bit  absolute APFCON2.2;
  APFCON2_SSSEL0           : bit  absolute APFCON2.1;
  APFCON2_CCP3SEL          : bit  absolute APFCON2.0;
  APFCON1                  : byte absolute $011D;
  APFCON1_C2OUTSEL         : bit  absolute APFCON1.7;
  APFCON1_CCP1SEL          : bit  absolute APFCON1.6;
  APFCON1_SDOSEL           : bit  absolute APFCON1.5;
  APFCON1_SCKSEL           : bit  absolute APFCON1.4;
  APFCON1_SDISEL           : bit  absolute APFCON1.3;
  APFCON1_TXSEL            : bit  absolute APFCON1.2;
  APFCON1_RXSEL            : bit  absolute APFCON1.1;
  APFCON1_CCP2SEL          : bit  absolute APFCON1.0;
  CM3CON0                  : byte absolute $011E;
  CM3CON0_C3ON             : bit  absolute CM3CON0.7;
  CM3CON0_C3OUT            : bit  absolute CM3CON0.6;
  CM3CON0_C3OE             : bit  absolute CM3CON0.5;
  CM3CON0_C3POL            : bit  absolute CM3CON0.4;
  CM3CON0_C3ZLF            : bit  absolute CM3CON0.3;
  CM3CON0_C3SP             : bit  absolute CM3CON0.2;
  CM3CON0_C3HYS            : bit  absolute CM3CON0.1;
  CM3CON0_C3SYNC           : bit  absolute CM3CON0.0;
  CM3CON1                  : byte absolute $011F;
  CM3CON1_C3INTP           : bit  absolute CM3CON1.7;
  CM3CON1_C3INTN           : bit  absolute CM3CON1.6;
  CM3CON1_C3PCH2           : bit  absolute CM3CON1.5;
  CM3CON1_C3PCH1           : bit  absolute CM3CON1.4;
  CM3CON1_C3PCH0           : bit  absolute CM3CON1.3;
  CM3CON1_C3NCH2           : bit  absolute CM3CON1.2;
  CM3CON1_C3NCH1           : bit  absolute CM3CON1.1;
  CM3CON1_C3NCH0           : bit  absolute CM3CON1.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA7             : bit  absolute ANSELA.7;
  ANSELA_ANSA5             : bit  absolute ANSELA.5;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA3             : bit  absolute ANSELA.3;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
  ANSELB                   : byte absolute $018D;
  ANSELB_ANSB6             : bit  absolute ANSELB.6;
  ANSELB_ANSB5             : bit  absolute ANSELB.5;
  ANSELB_ANSB4             : bit  absolute ANSELB.4;
  ANSELB_ANSB3             : bit  absolute ANSELB.3;
  ANSELB_ANSB2             : bit  absolute ANSELB.2;
  ANSELB_ANSB1             : bit  absolute ANSELB.1;
  ANSELB_ANSB0             : bit  absolute ANSELB.0;
  ANSELC                   : byte absolute $018E;
  ANSELC_ANSC7             : bit  absolute ANSELC.7;
  ANSELC_ANSC6             : bit  absolute ANSELC.6;
  ANSELC_ANSC5             : bit  absolute ANSELC.5;
  ANSELC_ANSC4             : bit  absolute ANSELC.4;
  ANSELC_ANSC3             : bit  absolute ANSELC.3;
  ANSELC_ANSC2             : bit  absolute ANSELC.2;
  ANSELC_ANSC1             : bit  absolute ANSELC.1;
  ANSELC_ANSC0             : bit  absolute ANSELC.0;
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
  VREGCON                  : byte absolute $0197;
  VREGCON_VREGPM           : bit  absolute VREGCON.1;
  VREGCON_VREGPM0          : bit  absolute VREGCON.0;
  RC1REG                   : byte absolute $0199;
  TX1REG                   : byte absolute $019A;
  SP1BRGL                  : byte absolute $019B;
  SP1BRGH                  : byte absolute $019C;
  RC1STA                   : byte absolute $019D;
  RC1STA_SPEN              : bit  absolute RC1STA.7;
  RC1STA_RX9               : bit  absolute RC1STA.6;
  RC1STA_SREN              : bit  absolute RC1STA.5;
  RC1STA_CREN              : bit  absolute RC1STA.4;
  RC1STA_ADDEN             : bit  absolute RC1STA.3;
  RC1STA_FERR              : bit  absolute RC1STA.2;
  RC1STA_OERR              : bit  absolute RC1STA.1;
  RC1STA_RX9D              : bit  absolute RC1STA.0;
  TX1STA                   : byte absolute $019E;
  TX1STA_CSRC              : bit  absolute TX1STA.7;
  TX1STA_TX9               : bit  absolute TX1STA.6;
  TX1STA_TXEN              : bit  absolute TX1STA.5;
  TX1STA_SYNC              : bit  absolute TX1STA.4;
  TX1STA_SENDB             : bit  absolute TX1STA.3;
  TX1STA_BRGH              : bit  absolute TX1STA.2;
  TX1STA_TRMT              : bit  absolute TX1STA.1;
  TX1STA_TX9D              : bit  absolute TX1STA.0;
  BAUD1CON                 : byte absolute $019F;
  BAUD1CON_ABDOVF          : bit  absolute BAUD1CON.7;
  BAUD1CON_RCIDL           : bit  absolute BAUD1CON.6;
  BAUD1CON_SCKP            : bit  absolute BAUD1CON.4;
  BAUD1CON_BRG16           : bit  absolute BAUD1CON.3;
  BAUD1CON_WUE             : bit  absolute BAUD1CON.1;
  BAUD1CON_ABDEN           : bit  absolute BAUD1CON.0;
  WPUA                     : byte absolute $020C;
  WPUA_WPUA7               : bit  absolute WPUA.7;
  WPUA_WPUA6               : bit  absolute WPUA.6;
  WPUA_WPUA5               : bit  absolute WPUA.5;
  WPUA_WPUA4               : bit  absolute WPUA.4;
  WPUA_WPUA3               : bit  absolute WPUA.3;
  WPUA_WPUA2               : bit  absolute WPUA.2;
  WPUA_WPUA1               : bit  absolute WPUA.1;
  WPUA_WPUA0               : bit  absolute WPUA.0;
  WPUB                     : byte absolute $020D;
  WPUB_WPUB7               : bit  absolute WPUB.7;
  WPUB_WPUB6               : bit  absolute WPUB.6;
  WPUB_WPUB5               : bit  absolute WPUB.5;
  WPUB_WPUB4               : bit  absolute WPUB.4;
  WPUB_WPUB3               : bit  absolute WPUB.3;
  WPUB_WPUB2               : bit  absolute WPUB.2;
  WPUB_WPUB1               : bit  absolute WPUB.1;
  WPUB_WPUB0               : bit  absolute WPUB.0;
  WPUC                     : byte absolute $020E;
  WPUC_WPUC7               : bit  absolute WPUC.7;
  WPUC_WPUC6               : bit  absolute WPUC.6;
  WPUC_WPUC5               : bit  absolute WPUC.5;
  WPUC_WPUC4               : bit  absolute WPUC.4;
  WPUC_WPUC3               : bit  absolute WPUC.3;
  WPUC_WPUC2               : bit  absolute WPUC.2;
  WPUC_WPUC1               : bit  absolute WPUC.1;
  WPUC_WPUC0               : bit  absolute WPUC.0;
  WPUE                     : byte absolute $0210;
  WPUE_WPUE3               : bit  absolute WPUE.3;
  SSP1BUF                  : byte absolute $0211;
  SSP1BUF_SSP1BUF7         : bit  absolute SSP1BUF.7;
  SSP1BUF_SSP1BUF6         : bit  absolute SSP1BUF.6;
  SSP1BUF_SSP1BUF5         : bit  absolute SSP1BUF.5;
  SSP1BUF_SSP1BUF4         : bit  absolute SSP1BUF.4;
  SSP1BUF_SSP1BUF3         : bit  absolute SSP1BUF.3;
  SSP1BUF_SSP1BUF2         : bit  absolute SSP1BUF.2;
  SSP1BUF_SSP1BUF1         : bit  absolute SSP1BUF.1;
  SSP1BUF_SSP1BUF0         : bit  absolute SSP1BUF.0;
  SSP1ADD                  : byte absolute $0212;
  SSP1ADD_SSP1ADD7         : bit  absolute SSP1ADD.7;
  SSP1ADD_SSP1ADD6         : bit  absolute SSP1ADD.6;
  SSP1ADD_SSP1ADD5         : bit  absolute SSP1ADD.5;
  SSP1ADD_SSP1ADD4         : bit  absolute SSP1ADD.4;
  SSP1ADD_SSP1ADD3         : bit  absolute SSP1ADD.3;
  SSP1ADD_SSP1ADD2         : bit  absolute SSP1ADD.2;
  SSP1ADD_SSP1ADD1         : bit  absolute SSP1ADD.1;
  SSP1ADD_SSP1ADD0         : bit  absolute SSP1ADD.0;
  SSP1MSK                  : byte absolute $0213;
  SSP1MSK_SSP1MSK7         : bit  absolute SSP1MSK.7;
  SSP1MSK_SSP1MSK6         : bit  absolute SSP1MSK.6;
  SSP1MSK_SSP1MSK5         : bit  absolute SSP1MSK.5;
  SSP1MSK_SSP1MSK4         : bit  absolute SSP1MSK.4;
  SSP1MSK_SSP1MSK3         : bit  absolute SSP1MSK.3;
  SSP1MSK_SSP1MSK2         : bit  absolute SSP1MSK.2;
  SSP1MSK_SSP1MSK1         : bit  absolute SSP1MSK.1;
  SSP1MSK_SSP1MSK0         : bit  absolute SSP1MSK.0;
  SSP1STAT                 : byte absolute $0214;
  SSP1STAT_SMP             : bit  absolute SSP1STAT.7;
  SSP1STAT_CKE             : bit  absolute SSP1STAT.6;
  SSP1STAT_D_nA            : bit  absolute SSP1STAT.5;
  SSP1STAT_P               : bit  absolute SSP1STAT.4;
  SSP1STAT_S               : bit  absolute SSP1STAT.3;
  SSP1STAT_R_nW            : bit  absolute SSP1STAT.2;
  SSP1STAT_UA              : bit  absolute SSP1STAT.1;
  SSP1STAT_BF              : bit  absolute SSP1STAT.0;
  SSP1CON1                 : byte absolute $0215;
  SSP1CON1_WCOL            : bit  absolute SSP1CON1.7;
  SSP1CON1_SSPOV           : bit  absolute SSP1CON1.6;
  SSP1CON1_SSPEN           : bit  absolute SSP1CON1.5;
  SSP1CON1_CKP             : bit  absolute SSP1CON1.4;
  SSP1CON1_SSPM3           : bit  absolute SSP1CON1.3;
  SSP1CON1_SSPM2           : bit  absolute SSP1CON1.2;
  SSP1CON1_SSPM1           : bit  absolute SSP1CON1.1;
  SSP1CON1_SSPM0           : bit  absolute SSP1CON1.0;
  SSP1CON2                 : byte absolute $0216;
  SSP1CON2_GCEN            : bit  absolute SSP1CON2.7;
  SSP1CON2_ACKSTAT         : bit  absolute SSP1CON2.6;
  SSP1CON2_ACKDT           : bit  absolute SSP1CON2.5;
  SSP1CON2_ACKEN           : bit  absolute SSP1CON2.4;
  SSP1CON2_RCEN            : bit  absolute SSP1CON2.3;
  SSP1CON2_PEN             : bit  absolute SSP1CON2.2;
  SSP1CON2_RSEN            : bit  absolute SSP1CON2.1;
  SSP1CON2_SEN             : bit  absolute SSP1CON2.0;
  SSP1CON3                 : byte absolute $0217;
  SSP1CON3_ACKTIM          : bit  absolute SSP1CON3.7;
  SSP1CON3_PCIE            : bit  absolute SSP1CON3.6;
  SSP1CON3_SCIE            : bit  absolute SSP1CON3.5;
  SSP1CON3_BOEN            : bit  absolute SSP1CON3.4;
  SSP1CON3_SDAHT           : bit  absolute SSP1CON3.3;
  SSP1CON3_SBCDE           : bit  absolute SSP1CON3.2;
  SSP1CON3_AHEN            : bit  absolute SSP1CON3.1;
  SSP1CON3_DHEN            : bit  absolute SSP1CON3.0;
  ODCONA                   : byte absolute $028C;
  ODCONA_ODCONA7           : bit  absolute ODCONA.7;
  ODCONA_ODCONA6           : bit  absolute ODCONA.6;
  ODCONA_ODCONA5           : bit  absolute ODCONA.5;
  ODCONA_ODCONA4           : bit  absolute ODCONA.4;
  ODCONA_ODCONA3           : bit  absolute ODCONA.3;
  ODCONA_ODCONA2           : bit  absolute ODCONA.2;
  ODCONA_ODCONA1           : bit  absolute ODCONA.1;
  ODCONA_ODCONA0           : bit  absolute ODCONA.0;
  ODCONB                   : byte absolute $028D;
  ODCONB_ODCONB7           : bit  absolute ODCONB.7;
  ODCONB_ODCONB6           : bit  absolute ODCONB.6;
  ODCONB_ODCONB5           : bit  absolute ODCONB.5;
  ODCONB_ODCONB4           : bit  absolute ODCONB.4;
  ODCONB_ODCONB3           : bit  absolute ODCONB.3;
  ODCONB_ODCONB2           : bit  absolute ODCONB.2;
  ODCONB_ODCONB1           : bit  absolute ODCONB.1;
  ODCONB_ODCONB0           : bit  absolute ODCONB.0;
  ODCONC                   : byte absolute $028E;
  ODCONC_ODCONC7           : bit  absolute ODCONC.7;
  ODCONC_ODCONC6           : bit  absolute ODCONC.6;
  ODCONC_ODCONC5           : bit  absolute ODCONC.5;
  ODCONC_ODCONC4           : bit  absolute ODCONC.4;
  ODCONC_ODCONC3           : bit  absolute ODCONC.3;
  ODCONC_ODCONC2           : bit  absolute ODCONC.2;
  ODCONC_ODCONC1           : bit  absolute ODCONC.1;
  ODCONC_ODCONC0           : bit  absolute ODCONC.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_DC1B1            : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0            : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3           : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2           : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1           : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0           : bit  absolute CCP1CON.0;
  CCPR2L                   : byte absolute $0298;
  CCPR2H                   : byte absolute $0299;
  CCP2CON                  : byte absolute $029A;
  CCP2CON_DC2B1            : bit  absolute CCP2CON.5;
  CCP2CON_DC2B0            : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3           : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2           : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1           : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0           : bit  absolute CCP2CON.0;
  SLRCONA                  : byte absolute $030C;
  SLRCONA_SLRCONA7         : bit  absolute SLRCONA.7;
  SLRCONA_SLRCONA6         : bit  absolute SLRCONA.6;
  SLRCONA_SLRCONA5         : bit  absolute SLRCONA.5;
  SLRCONA_SLRCONA4         : bit  absolute SLRCONA.4;
  SLRCONA_SLRCONA3         : bit  absolute SLRCONA.3;
  SLRCONA_SLRCONA2         : bit  absolute SLRCONA.2;
  SLRCONA_SLRCONA1         : bit  absolute SLRCONA.1;
  SLRCONA_SLRCONA0         : bit  absolute SLRCONA.0;
  SLRCONB                  : byte absolute $030D;
  SLRCONB_SLRCONB7         : bit  absolute SLRCONB.7;
  SLRCONB_SLRCONB6         : bit  absolute SLRCONB.6;
  SLRCONB_SLRCONB5         : bit  absolute SLRCONB.5;
  SLRCONB_SLRCONB4         : bit  absolute SLRCONB.4;
  SLRCONB_SLRCONB3         : bit  absolute SLRCONB.3;
  SLRCONB_SLRCONB2         : bit  absolute SLRCONB.2;
  SLRCONB_SLRCONB1         : bit  absolute SLRCONB.1;
  SLRCONB_SLRCONB0         : bit  absolute SLRCONB.0;
  SLRCONC                  : byte absolute $030E;
  SLRCONC_SLRCONC7         : bit  absolute SLRCONC.7;
  SLRCONC_SLRCONC6         : bit  absolute SLRCONC.6;
  SLRCONC_SLRCONC5         : bit  absolute SLRCONC.5;
  SLRCONC_SLRCONC4         : bit  absolute SLRCONC.4;
  SLRCONC_SLRCONC3         : bit  absolute SLRCONC.3;
  SLRCONC_SLRCONC2         : bit  absolute SLRCONC.2;
  SLRCONC_SLRCONC1         : bit  absolute SLRCONC.1;
  SLRCONC_SLRCONC0         : bit  absolute SLRCONC.0;
  CCPR3L                   : byte absolute $0311;
  CCPR3H                   : byte absolute $0312;
  CCP3CON                  : byte absolute $0313;
  CCP3CON_DC3B1            : bit  absolute CCP3CON.5;
  CCP3CON_DC3B0            : bit  absolute CCP3CON.4;
  CCP3CON_CCP3M3           : bit  absolute CCP3CON.3;
  CCP3CON_CCP3M2           : bit  absolute CCP3CON.2;
  CCP3CON_CCP3M1           : bit  absolute CCP3CON.1;
  CCP3CON_CCP3M0           : bit  absolute CCP3CON.0;
  INLVLA                   : byte absolute $038C;
  INLVLA_INLVLA7           : bit  absolute INLVLA.7;
  INLVLA_INLVLA6           : bit  absolute INLVLA.6;
  INLVLA_INLVLA5           : bit  absolute INLVLA.5;
  INLVLA_INLVLA4           : bit  absolute INLVLA.4;
  INLVLA_INLVLA3           : bit  absolute INLVLA.3;
  INLVLA_INLVLA2           : bit  absolute INLVLA.2;
  INLVLA_INLVLA1           : bit  absolute INLVLA.1;
  INLVLA_INLVLA0           : bit  absolute INLVLA.0;
  INLVLB                   : byte absolute $038D;
  INLVLB_INLVLB7           : bit  absolute INLVLB.7;
  INLVLB_INLVLB6           : bit  absolute INLVLB.6;
  INLVLB_INLVLB5           : bit  absolute INLVLB.5;
  INLVLB_INLVLB4           : bit  absolute INLVLB.4;
  INLVLB_INLVLB3           : bit  absolute INLVLB.3;
  INLVLB_INLVLB2           : bit  absolute INLVLB.2;
  INLVLB_INLVLB1           : bit  absolute INLVLB.1;
  INLVLB_INLVLB0           : bit  absolute INLVLB.0;
  INLVLC                   : byte absolute $038E;
  INLVLC_INLVLC7           : bit  absolute INLVLC.7;
  INLVLC_INLVLC6           : bit  absolute INLVLC.6;
  INLVLC_INLVLC5           : bit  absolute INLVLC.5;
  INLVLC_INLVLC4           : bit  absolute INLVLC.4;
  INLVLC_INLVLC3           : bit  absolute INLVLC.3;
  INLVLC_INLVLC2           : bit  absolute INLVLC.2;
  INLVLC_INLVLC1           : bit  absolute INLVLC.1;
  INLVLC_INLVLC0           : bit  absolute INLVLC.0;
  INLVLE                   : byte absolute $0390;
  INLVLE_INLVLE3           : bit  absolute INLVLE.3;
  IOCAP                    : byte absolute $0391;
  IOCAP_IOCAP7             : bit  absolute IOCAP.7;
  IOCAP_IOCAP6             : bit  absolute IOCAP.6;
  IOCAP_IOCAP5             : bit  absolute IOCAP.5;
  IOCAP_IOCAP4             : bit  absolute IOCAP.4;
  IOCAP_IOCAP3             : bit  absolute IOCAP.3;
  IOCAP_IOCAP2             : bit  absolute IOCAP.2;
  IOCAP_IOCAP1             : bit  absolute IOCAP.1;
  IOCAP_IOCAP0             : bit  absolute IOCAP.0;
  IOCAN                    : byte absolute $0392;
  IOCAN_IOCAN7             : bit  absolute IOCAN.7;
  IOCAN_IOCAN6             : bit  absolute IOCAN.6;
  IOCAN_IOCAN5             : bit  absolute IOCAN.5;
  IOCAN_IOCAN4             : bit  absolute IOCAN.4;
  IOCAN_IOCAN3             : bit  absolute IOCAN.3;
  IOCAN_IOCAN2             : bit  absolute IOCAN.2;
  IOCAN_IOCAN1             : bit  absolute IOCAN.1;
  IOCAN_IOCAN0             : bit  absolute IOCAN.0;
  IOCAF                    : byte absolute $0393;
  IOCAF_IOCAF7             : bit  absolute IOCAF.7;
  IOCAF_IOCAF6             : bit  absolute IOCAF.6;
  IOCAF_IOCAF5             : bit  absolute IOCAF.5;
  IOCAF_IOCAF4             : bit  absolute IOCAF.4;
  IOCAF_IOCAF3             : bit  absolute IOCAF.3;
  IOCAF_IOCAF2             : bit  absolute IOCAF.2;
  IOCAF_IOCAF1             : bit  absolute IOCAF.1;
  IOCAF_IOCAF0             : bit  absolute IOCAF.0;
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
  IOCCP                    : byte absolute $0397;
  IOCCP_IOCCP7             : bit  absolute IOCCP.7;
  IOCCP_IOCCP6             : bit  absolute IOCCP.6;
  IOCCP_IOCCP5             : bit  absolute IOCCP.5;
  IOCCP_IOCCP4             : bit  absolute IOCCP.4;
  IOCCP_IOCCP3             : bit  absolute IOCCP.3;
  IOCCP_IOCCP2             : bit  absolute IOCCP.2;
  IOCCP_IOCCP1             : bit  absolute IOCCP.1;
  IOCCP_IOCCP0             : bit  absolute IOCCP.0;
  IOCCN                    : byte absolute $0398;
  IOCCN_IOCCN7             : bit  absolute IOCCN.7;
  IOCCN_IOCCN6             : bit  absolute IOCCN.6;
  IOCCN_IOCCN5             : bit  absolute IOCCN.5;
  IOCCN_IOCCN4             : bit  absolute IOCCN.4;
  IOCCN_IOCCN3             : bit  absolute IOCCN.3;
  IOCCN_IOCCN2             : bit  absolute IOCCN.2;
  IOCCN_IOCCN1             : bit  absolute IOCCN.1;
  IOCCN_IOCCN0             : bit  absolute IOCCN.0;
  IOCCF                    : byte absolute $0399;
  IOCCF_IOCCF7             : bit  absolute IOCCF.7;
  IOCCF_IOCCF6             : bit  absolute IOCCF.6;
  IOCCF_IOCCF5             : bit  absolute IOCCF.5;
  IOCCF_IOCCF4             : bit  absolute IOCCF.4;
  IOCCF_IOCCF3             : bit  absolute IOCCF.3;
  IOCCF_IOCCF2             : bit  absolute IOCCF.2;
  IOCCF_IOCCF1             : bit  absolute IOCCF.1;
  IOCCF_IOCCF0             : bit  absolute IOCCF.0;
  IOCEP                    : byte absolute $039D;
  IOCEP_IOCEP3             : bit  absolute IOCEP.3;
  IOCEN                    : byte absolute $039E;
  IOCEN_IOCEN3             : bit  absolute IOCEN.3;
  IOCEF                    : byte absolute $039F;
  IOCEF_IOCEF3             : bit  absolute IOCEF.3;
  OPA1CON                  : byte absolute $0511;
  OPA1CON_OPA1PCH2         : bit  absolute OPA1CON.2;
  OPA1CON_OPA1PCH1         : bit  absolute OPA1CON.1;
  OPA1CON_OPA1PCH0         : bit  absolute OPA1CON.0;
  OPA2CON                  : byte absolute $0513;
  OPA2CON_OPA2PCH2         : bit  absolute OPA2CON.2;
  OPA2CON_OPA2PCH1         : bit  absolute OPA2CON.1;
  OPA2CON_OPA2PCH0         : bit  absolute OPA2CON.0;
  CLKRCON                  : byte absolute $051A;
  CLKRCON_CLKREN           : bit  absolute CLKRCON.7;
  CLKRCON_CLKROE           : bit  absolute CLKRCON.6;
  CLKRCON_CLKRSLR          : bit  absolute CLKRCON.5;
  CLKRCON_CLKRDC1          : bit  absolute CLKRCON.4;
  CLKRCON_CLKRDC0          : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2         : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1         : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0         : bit  absolute CLKRCON.0;
  DAC2CON0                 : byte absolute $0591;
  DAC2CON0_DAC2EN          : bit  absolute DAC2CON0.7;
  DAC2CON0_DAC2OE1         : bit  absolute DAC2CON0.5;
  DAC2CON0_DAC2OE2         : bit  absolute DAC2CON0.4;
  DAC2CON0_DAC2PSS1        : bit  absolute DAC2CON0.3;
  DAC2CON0_DAC2PSS0        : bit  absolute DAC2CON0.2;
  DAC2REF                  : byte absolute $0592;
  DAC2REF_DAC2R4           : bit  absolute DAC2REF.4;
  DAC2REF_DAC2R3           : bit  absolute DAC2REF.3;
  DAC2REF_DAC2R2           : bit  absolute DAC2REF.2;
  DAC2REF_DAC2R1           : bit  absolute DAC2REF.1;
  DAC2REF_DAC2R0           : bit  absolute DAC2REF.0;
  DAC3CON0                 : byte absolute $0593;
  DAC3CON0_DAC3EN          : bit  absolute DAC3CON0.7;
  DAC3CON0_DAC3OE1         : bit  absolute DAC3CON0.5;
  DAC3CON0_DAC3OE2         : bit  absolute DAC3CON0.4;
  DAC3CON0_DAC3PSS1        : bit  absolute DAC3CON0.3;
  DAC3CON0_DAC3PSS0        : bit  absolute DAC3CON0.2;
  DAC3REF                  : byte absolute $0594;
  DAC3REF_DAC3R4           : bit  absolute DAC3REF.4;
  DAC3REF_DAC3R3           : bit  absolute DAC3REF.3;
  DAC3REF_DAC3R2           : bit  absolute DAC3REF.2;
  DAC3REF_DAC3R1           : bit  absolute DAC3REF.1;
  DAC3REF_DAC3R0           : bit  absolute DAC3REF.0;
  DAC4CON0                 : byte absolute $0595;
  DAC4CON0_DAC4EN          : bit  absolute DAC4CON0.7;
  DAC4CON0_DAC4OE1         : bit  absolute DAC4CON0.5;
  DAC4CON0_DAC4OE2         : bit  absolute DAC4CON0.4;
  DAC4CON0_DAC4PSS1        : bit  absolute DAC4CON0.3;
  DAC4CON0_DAC4PSS0        : bit  absolute DAC4CON0.2;
  DAC4REF                  : byte absolute $0596;
  DAC4REF_DAC4R4           : bit  absolute DAC4REF.4;
  DAC4REF_DAC4R3           : bit  absolute DAC4REF.3;
  DAC4REF_DAC4R2           : bit  absolute DAC4REF.2;
  DAC4REF_DAC4R1           : bit  absolute DAC4REF.1;
  DAC4REF_DAC4R0           : bit  absolute DAC4REF.0;
  PSMC1CON                 : byte absolute $0E91;
  PSMC1CON_PSMC1EN         : bit  absolute PSMC1CON.7;
  PSMC1CON_PSMC1LD         : bit  absolute PSMC1CON.6;
  PSMC1CON_P1DBFE          : bit  absolute PSMC1CON.5;
  PSMC1CON_P1DBRE          : bit  absolute PSMC1CON.4;
  PSMC1CON_P1MODE3         : bit  absolute PSMC1CON.3;
  PSMC1CON_P1MODE2         : bit  absolute PSMC1CON.2;
  PSMC1CON_P1MODE1         : bit  absolute PSMC1CON.1;
  PSMC1CON_P1MODE0         : bit  absolute PSMC1CON.0;
  PSMC1MDL                 : byte absolute $0E92;
  PSMC1MDL_P1MDLEN         : bit  absolute PSMC1MDL.7;
  PSMC1MDL_P1MDLPOL        : bit  absolute PSMC1MDL.6;
  PSMC1MDL_P1MDLBIT        : bit  absolute PSMC1MDL.5;
  PSMC1MDL_P1MSRC3         : bit  absolute PSMC1MDL.3;
  PSMC1MDL_P1MSRC2         : bit  absolute PSMC1MDL.2;
  PSMC1MDL_P1MSRC1         : bit  absolute PSMC1MDL.1;
  PSMC1MDL_P1MSRC0         : bit  absolute PSMC1MDL.0;
  PSMC1SYNC                : byte absolute $0E93;
  PSMC1SYNC_P1POFST        : bit  absolute PSMC1SYNC.7;
  PSMC1SYNC_P1PRPOL        : bit  absolute PSMC1SYNC.6;
  PSMC1SYNC_P1DCPOL        : bit  absolute PSMC1SYNC.5;
  PSMC1SYNC_P1SYNC2        : bit  absolute PSMC1SYNC.2;
  PSMC1SYNC_P1SYNC1        : bit  absolute PSMC1SYNC.1;
  PSMC1SYNC_P1SYNC0        : bit  absolute PSMC1SYNC.0;
  PSMC1CLK                 : byte absolute $0E94;
  PSMC1CLK_P1CPRE1         : bit  absolute PSMC1CLK.5;
  PSMC1CLK_P1CPRE0         : bit  absolute PSMC1CLK.4;
  PSMC1CLK_P1CSRC1         : bit  absolute PSMC1CLK.1;
  PSMC1CLK_P1CSRC0         : bit  absolute PSMC1CLK.0;
  PSMC1OEN                 : byte absolute $0E95;
  PSMC1OEN_P1OEF           : bit  absolute PSMC1OEN.5;
  PSMC1OEN_P1OEE           : bit  absolute PSMC1OEN.4;
  PSMC1OEN_P1OED           : bit  absolute PSMC1OEN.3;
  PSMC1OEN_P1OEC           : bit  absolute PSMC1OEN.2;
  PSMC1OEN_P1OEB           : bit  absolute PSMC1OEN.1;
  PSMC1OEN_P1OEA           : bit  absolute PSMC1OEN.0;
  PSMC1POL                 : byte absolute $0E96;
  PSMC1POL_P1INPOL         : bit  absolute PSMC1POL.6;
  PSMC1POL_P1POLF          : bit  absolute PSMC1POL.5;
  PSMC1POL_P1POLE          : bit  absolute PSMC1POL.4;
  PSMC1POL_P1POLD          : bit  absolute PSMC1POL.3;
  PSMC1POL_P1POLC          : bit  absolute PSMC1POL.2;
  PSMC1POL_P1POLB          : bit  absolute PSMC1POL.1;
  PSMC1POL_P1POLA          : bit  absolute PSMC1POL.0;
  PSMC1BLNK                : byte absolute $0E97;
  PSMC1BLNK_P1FEBM1        : bit  absolute PSMC1BLNK.5;
  PSMC1BLNK_P1FEBM0        : bit  absolute PSMC1BLNK.4;
  PSMC1BLNK_P1REBM1        : bit  absolute PSMC1BLNK.1;
  PSMC1BLNK_P1REBM0        : bit  absolute PSMC1BLNK.0;
  PSMC1REBS                : byte absolute $0E98;
  PSMC1REBS_P1REBSIN       : bit  absolute PSMC1REBS.7;
  PSMC1REBS_P1REBSC4       : bit  absolute PSMC1REBS.4;
  PSMC1REBS_P1REBSC3       : bit  absolute PSMC1REBS.3;
  PSMC1REBS_P1REBSC2       : bit  absolute PSMC1REBS.2;
  PSMC1REBS_P1REBSC1       : bit  absolute PSMC1REBS.1;
  PSMC1FEBS                : byte absolute $0E99;
  PSMC1FEBS_P1FEBSIN       : bit  absolute PSMC1FEBS.7;
  PSMC1FEBS_P1FEBSC4       : bit  absolute PSMC1FEBS.4;
  PSMC1FEBS_P1FEBSC3       : bit  absolute PSMC1FEBS.3;
  PSMC1FEBS_P1FEBSC2       : bit  absolute PSMC1FEBS.2;
  PSMC1FEBS_P1FEBSC1       : bit  absolute PSMC1FEBS.1;
  PSMC1PHS                 : byte absolute $0E9A;
  PSMC1PHS_P1PHSIN         : bit  absolute PSMC1PHS.7;
  PSMC1PHS_P1PHSC4         : bit  absolute PSMC1PHS.4;
  PSMC1PHS_P1PHSC3         : bit  absolute PSMC1PHS.3;
  PSMC1PHS_P1PHSC2         : bit  absolute PSMC1PHS.2;
  PSMC1PHS_P1PHSC1         : bit  absolute PSMC1PHS.1;
  PSMC1PHS_P1PHST          : bit  absolute PSMC1PHS.0;
  PSMC1DCS                 : byte absolute $0E9B;
  PSMC1DCS_P1DCSIN         : bit  absolute PSMC1DCS.7;
  PSMC1DCS_P1DCSC4         : bit  absolute PSMC1DCS.4;
  PSMC1DCS_P1DCSC3         : bit  absolute PSMC1DCS.3;
  PSMC1DCS_P1DCSC2         : bit  absolute PSMC1DCS.2;
  PSMC1DCS_P1DCSC1         : bit  absolute PSMC1DCS.1;
  PSMC1DCS_P1DCST          : bit  absolute PSMC1DCS.0;
  PSMC1PRS                 : byte absolute $0E9C;
  PSMC1PRS_P1PRSIN         : bit  absolute PSMC1PRS.7;
  PSMC1PRS_P1PRSC4         : bit  absolute PSMC1PRS.4;
  PSMC1PRS_P1PRSC3         : bit  absolute PSMC1PRS.3;
  PSMC1PRS_P1PRSC2         : bit  absolute PSMC1PRS.2;
  PSMC1PRS_P1PRSC1         : bit  absolute PSMC1PRS.1;
  PSMC1PRS_P1PRST          : bit  absolute PSMC1PRS.0;
  PSMC1ASDC                : byte absolute $0E9D;
  PSMC1ASDC_P1ASE          : bit  absolute PSMC1ASDC.7;
  PSMC1ASDC_P1ASDEN        : bit  absolute PSMC1ASDC.6;
  PSMC1ASDC_P1ARSEN        : bit  absolute PSMC1ASDC.5;
  PSMC1ASDC_P1ASDOV        : bit  absolute PSMC1ASDC.0;
  PSMC1ASDL                : byte absolute $0E9E;
  PSMC1ASDL_P1ASDLF        : bit  absolute PSMC1ASDL.5;
  PSMC1ASDL_P1ASDLE        : bit  absolute PSMC1ASDL.4;
  PSMC1ASDL_P1ASDLD        : bit  absolute PSMC1ASDL.3;
  PSMC1ASDL_P1ASDLC        : bit  absolute PSMC1ASDL.2;
  PSMC1ASDL_P1ASDLB        : bit  absolute PSMC1ASDL.1;
  PSMC1ASDL_P1ASDLA        : bit  absolute PSMC1ASDL.0;
  PSMC1ASDS                : byte absolute $0E9F;
  PSMC1ASDS_P1ASDSIN       : bit  absolute PSMC1ASDS.7;
  PSMC1ASDS_P1ASDSC4       : bit  absolute PSMC1ASDS.4;
  PSMC1ASDS_P1ASDSC3       : bit  absolute PSMC1ASDS.3;
  PSMC1ASDS_P1ASDSC2       : bit  absolute PSMC1ASDS.2;
  PSMC1ASDS_P1ASDSC1       : bit  absolute PSMC1ASDS.1;
  PSMC1INT                 : byte absolute $0EA0;
  PSMC1INT_P1TOVIE         : bit  absolute PSMC1INT.7;
  PSMC1INT_P1TPHIE         : bit  absolute PSMC1INT.6;
  PSMC1INT_P1TDCIE         : bit  absolute PSMC1INT.5;
  PSMC1INT_P1TPRIE         : bit  absolute PSMC1INT.4;
  PSMC1INT_P1TOVIF         : bit  absolute PSMC1INT.3;
  PSMC1INT_P1TPHIF         : bit  absolute PSMC1INT.2;
  PSMC1INT_P1TDCIF         : bit  absolute PSMC1INT.1;
  PSMC1INT_P1TPRIF         : bit  absolute PSMC1INT.0;
  PSMC1PHL                 : byte absolute $0EA1;
  PSMC1PHL_PSMC1PH7        : bit  absolute PSMC1PHL.7;
  PSMC1PHL_PSMC1PH6        : bit  absolute PSMC1PHL.6;
  PSMC1PHL_PSMC1PH5        : bit  absolute PSMC1PHL.5;
  PSMC1PHL_PSMC1PH4        : bit  absolute PSMC1PHL.4;
  PSMC1PHL_PSMC1PH3        : bit  absolute PSMC1PHL.3;
  PSMC1PHL_PSMC1PH2        : bit  absolute PSMC1PHL.2;
  PSMC1PHL_PSMC1PH1        : bit  absolute PSMC1PHL.1;
  PSMC1PHL_PSMC1PH0        : bit  absolute PSMC1PHL.0;
  PSMC1PHH                 : byte absolute $0EA2;
  PSMC1PHH_PSMC1PH15       : bit  absolute PSMC1PHH.7;
  PSMC1PHH_PSMC1PH14       : bit  absolute PSMC1PHH.6;
  PSMC1PHH_PSMC1PH13       : bit  absolute PSMC1PHH.5;
  PSMC1PHH_PSMC1PH12       : bit  absolute PSMC1PHH.4;
  PSMC1PHH_PSMC1PH11       : bit  absolute PSMC1PHH.3;
  PSMC1PHH_PSMC1PH10       : bit  absolute PSMC1PHH.2;
  PSMC1PHH_PSMC1PH9        : bit  absolute PSMC1PHH.1;
  PSMC1PHH_PSMC1PH8        : bit  absolute PSMC1PHH.0;
  PSMC1DCL                 : byte absolute $0EA3;
  PSMC1DCL_PSMC1DC7        : bit  absolute PSMC1DCL.7;
  PSMC1DCL_PSMC1DC6        : bit  absolute PSMC1DCL.6;
  PSMC1DCL_PSMC1DC5        : bit  absolute PSMC1DCL.5;
  PSMC1DCL_PSMC1DC4        : bit  absolute PSMC1DCL.4;
  PSMC1DCL_PSMC1DC3        : bit  absolute PSMC1DCL.3;
  PSMC1DCL_PSMC1DC2        : bit  absolute PSMC1DCL.2;
  PSMC1DCL_PSMC1DC1        : bit  absolute PSMC1DCL.1;
  PSMC1DCL_PSMC1DC0        : bit  absolute PSMC1DCL.0;
  PSMC1DCH                 : byte absolute $0EA4;
  PSMC1DCH_PSMC1DC15       : bit  absolute PSMC1DCH.7;
  PSMC1DCH_PSMC1DC14       : bit  absolute PSMC1DCH.6;
  PSMC1DCH_PSMC1DC13       : bit  absolute PSMC1DCH.5;
  PSMC1DCH_PSMC1DC12       : bit  absolute PSMC1DCH.4;
  PSMC1DCH_PSMC1DC11       : bit  absolute PSMC1DCH.3;
  PSMC1DCH_PSMC1DC10       : bit  absolute PSMC1DCH.2;
  PSMC1DCH_PSMC1DC9        : bit  absolute PSMC1DCH.1;
  PSMC1DCH_PSMC1DC8        : bit  absolute PSMC1DCH.0;
  PSMC1PRL                 : byte absolute $0EA5;
  PSMC1PRL_PSMC1PR7        : bit  absolute PSMC1PRL.7;
  PSMC1PRL_PSMC1PR6        : bit  absolute PSMC1PRL.6;
  PSMC1PRL_PSMC1PR5        : bit  absolute PSMC1PRL.5;
  PSMC1PRL_PSMC1PR4        : bit  absolute PSMC1PRL.4;
  PSMC1PRL_PSMC1PR3        : bit  absolute PSMC1PRL.3;
  PSMC1PRL_PSMC1PR2        : bit  absolute PSMC1PRL.2;
  PSMC1PRL_PSMC1PR1        : bit  absolute PSMC1PRL.1;
  PSMC1PRL_PSMC1PR0        : bit  absolute PSMC1PRL.0;
  PSMC1PRH                 : byte absolute $0EA6;
  PSMC1PRH_PSMC1PR15       : bit  absolute PSMC1PRH.7;
  PSMC1PRH_PSMC1PR14       : bit  absolute PSMC1PRH.6;
  PSMC1PRH_PSMC1PR13       : bit  absolute PSMC1PRH.5;
  PSMC1PRH_PSMC1PR12       : bit  absolute PSMC1PRH.4;
  PSMC1PRH_PSMC1PR11       : bit  absolute PSMC1PRH.3;
  PSMC1PRH_PSMC1PR10       : bit  absolute PSMC1PRH.2;
  PSMC1PRH_PSMC1PR9        : bit  absolute PSMC1PRH.1;
  PSMC1PRH_PSMC1PR8        : bit  absolute PSMC1PRH.0;
  PSMC1TMRL                : byte absolute $0EA7;
  PSMC1TMRL_PSMC1TMR7      : bit  absolute PSMC1TMRL.7;
  PSMC1TMRL_PSMC1TMR6      : bit  absolute PSMC1TMRL.6;
  PSMC1TMRL_PSMC1TMR5      : bit  absolute PSMC1TMRL.5;
  PSMC1TMRL_PSMC1TMR4      : bit  absolute PSMC1TMRL.4;
  PSMC1TMRL_PSMC1TMR3      : bit  absolute PSMC1TMRL.3;
  PSMC1TMRL_PSMC1TMR2      : bit  absolute PSMC1TMRL.2;
  PSMC1TMRL_PSMC1TMR1      : bit  absolute PSMC1TMRL.1;
  PSMC1TMRL_PSMC1TMR0      : bit  absolute PSMC1TMRL.0;
  PSMC1TMRH                : byte absolute $0EA8;
  PSMC1TMRH_PSMC1TMR15     : bit  absolute PSMC1TMRH.7;
  PSMC1TMRH_PSMC1TMR14     : bit  absolute PSMC1TMRH.6;
  PSMC1TMRH_PSMC1TMR13     : bit  absolute PSMC1TMRH.5;
  PSMC1TMRH_PSMC1TMR12     : bit  absolute PSMC1TMRH.4;
  PSMC1TMRH_PSMC1TMR11     : bit  absolute PSMC1TMRH.3;
  PSMC1TMRH_PSMC1TMR10     : bit  absolute PSMC1TMRH.2;
  PSMC1TMRH_PSMC1TMR9      : bit  absolute PSMC1TMRH.1;
  PSMC1TMRH_PSMC1TMR8      : bit  absolute PSMC1TMRH.0;
  PSMC1DBR                 : byte absolute $0EA9;
  PSMC1DBR_PSMC1DBR7       : bit  absolute PSMC1DBR.7;
  PSMC1DBR_PSMC1DBR6       : bit  absolute PSMC1DBR.6;
  PSMC1DBR_PSMC1DBR5       : bit  absolute PSMC1DBR.5;
  PSMC1DBR_PSMC1DBR4       : bit  absolute PSMC1DBR.4;
  PSMC1DBR_PSMC1DBR3       : bit  absolute PSMC1DBR.3;
  PSMC1DBR_PSMC1DBR2       : bit  absolute PSMC1DBR.2;
  PSMC1DBR_PSMC1DBR1       : bit  absolute PSMC1DBR.1;
  PSMC1DBR_PSMC1DBR0       : bit  absolute PSMC1DBR.0;
  PSMC1DBF                 : byte absolute $0EAA;
  PSMC1DBF_PSMC1DBF7       : bit  absolute PSMC1DBF.7;
  PSMC1DBF_PSMC1DBF6       : bit  absolute PSMC1DBF.6;
  PSMC1DBF_PSMC1DBF5       : bit  absolute PSMC1DBF.5;
  PSMC1DBF_PSMC1DBF4       : bit  absolute PSMC1DBF.4;
  PSMC1DBF_PSMC1DBF3       : bit  absolute PSMC1DBF.3;
  PSMC1DBF_PSMC1DBF2       : bit  absolute PSMC1DBF.2;
  PSMC1DBF_PSMC1DBF1       : bit  absolute PSMC1DBF.1;
  PSMC1DBF_PSMC1DBF0       : bit  absolute PSMC1DBF.0;
  PSMC1BLKR                : byte absolute $0EAB;
  PSMC1BLKR_PSMC1BLKR7     : bit  absolute PSMC1BLKR.7;
  PSMC1BLKR_PSMC1BLKR6     : bit  absolute PSMC1BLKR.6;
  PSMC1BLKR_PSMC1BLKR5     : bit  absolute PSMC1BLKR.5;
  PSMC1BLKR_PSMC1BLKR4     : bit  absolute PSMC1BLKR.4;
  PSMC1BLKR_PSMC1BLKR3     : bit  absolute PSMC1BLKR.3;
  PSMC1BLKR_PSMC1BLKR2     : bit  absolute PSMC1BLKR.2;
  PSMC1BLKR_PSMC1BLKR1     : bit  absolute PSMC1BLKR.1;
  PSMC1BLKR_PSMC1BLKR0     : bit  absolute PSMC1BLKR.0;
  PSMC1BLKF                : byte absolute $0EAC;
  PSMC1BLKF_PSMC1BLKF7     : bit  absolute PSMC1BLKF.7;
  PSMC1BLKF_PSMC1BLKF6     : bit  absolute PSMC1BLKF.6;
  PSMC1BLKF_PSMC1BLKF5     : bit  absolute PSMC1BLKF.5;
  PSMC1BLKF_PSMC1BLKF4     : bit  absolute PSMC1BLKF.4;
  PSMC1BLKF_PSMC1BLKF3     : bit  absolute PSMC1BLKF.3;
  PSMC1BLKF_PSMC1BLKF2     : bit  absolute PSMC1BLKF.2;
  PSMC1BLKF_PSMC1BLKF1     : bit  absolute PSMC1BLKF.1;
  PSMC1BLKF_PSMC1BLKF0     : bit  absolute PSMC1BLKF.0;
  PSMC1FFA                 : byte absolute $0EAD;
  PSMC1FFA_PSMC1FFA3       : bit  absolute PSMC1FFA.3;
  PSMC1FFA_PSMC1FFA2       : bit  absolute PSMC1FFA.2;
  PSMC1FFA_PSMC1FFA1       : bit  absolute PSMC1FFA.1;
  PSMC1FFA_PSMC1FFA0       : bit  absolute PSMC1FFA.0;
  PSMC1STR0                : byte absolute $0EAE;
  PSMC1STR0_P1STRF         : bit  absolute PSMC1STR0.5;
  PSMC1STR0_P1STRE         : bit  absolute PSMC1STR0.4;
  PSMC1STR0_P1STRD         : bit  absolute PSMC1STR0.3;
  PSMC1STR0_P1STRC         : bit  absolute PSMC1STR0.2;
  PSMC1STR0_P1STRB         : bit  absolute PSMC1STR0.1;
  PSMC1STR0_P1STRA         : bit  absolute PSMC1STR0.0;
  PSMC1STR1                : byte absolute $0EAF;
  PSMC1STR1_P1SSYNC        : bit  absolute PSMC1STR1.7;
  PSMC1STR1_P1LSMEN        : bit  absolute PSMC1STR1.1;
  PSMC1STR1_P1HSMEN        : bit  absolute PSMC1STR1.0;
  PSMC2CON                 : byte absolute $0EB1;
  PSMC2CON_PSMC2EN         : bit  absolute PSMC2CON.7;
  PSMC2CON_PSMC2LD         : bit  absolute PSMC2CON.6;
  PSMC2CON_P2DBFE          : bit  absolute PSMC2CON.5;
  PSMC2CON_P2DBRE          : bit  absolute PSMC2CON.4;
  PSMC2CON_P2MODE3         : bit  absolute PSMC2CON.3;
  PSMC2CON_P2MODE2         : bit  absolute PSMC2CON.2;
  PSMC2CON_P2MODE1         : bit  absolute PSMC2CON.1;
  PSMC2CON_P2MODE0         : bit  absolute PSMC2CON.0;
  PSMC2MDL                 : byte absolute $0EB2;
  PSMC2MDL_P2MDLEN         : bit  absolute PSMC2MDL.7;
  PSMC2MDL_P2MDLPOL        : bit  absolute PSMC2MDL.6;
  PSMC2MDL_P2MDLBIT        : bit  absolute PSMC2MDL.5;
  PSMC2MDL_P2MSRC3         : bit  absolute PSMC2MDL.3;
  PSMC2MDL_P2MSRC2         : bit  absolute PSMC2MDL.2;
  PSMC2MDL_P2MSRC1         : bit  absolute PSMC2MDL.1;
  PSMC2MDL_P2MSRC0         : bit  absolute PSMC2MDL.0;
  PSMC2SYNC                : byte absolute $0EB3;
  PSMC2SYNC_P2POFST        : bit  absolute PSMC2SYNC.7;
  PSMC2SYNC_P2PRPOL        : bit  absolute PSMC2SYNC.6;
  PSMC2SYNC_P2DCPOL        : bit  absolute PSMC2SYNC.5;
  PSMC2SYNC_P2SYNC2        : bit  absolute PSMC2SYNC.2;
  PSMC2SYNC_P2SYNC1        : bit  absolute PSMC2SYNC.1;
  PSMC2SYNC_P2SYNC0        : bit  absolute PSMC2SYNC.0;
  PSMC2CLK                 : byte absolute $0EB4;
  PSMC2CLK_P2CPRE1         : bit  absolute PSMC2CLK.5;
  PSMC2CLK_P2CPRE0         : bit  absolute PSMC2CLK.4;
  PSMC2CLK_P2CSRC1         : bit  absolute PSMC2CLK.1;
  PSMC2CLK_P2CSRC0         : bit  absolute PSMC2CLK.0;
  PSMC2OEN                 : byte absolute $0EB5;
  PSMC2OEN_P2OEB           : bit  absolute PSMC2OEN.1;
  PSMC2OEN_P2OEA           : bit  absolute PSMC2OEN.0;
  PSMC2POL                 : byte absolute $0EB6;
  PSMC2POL_P2INPOL         : bit  absolute PSMC2POL.6;
  PSMC2POL_P2POLB          : bit  absolute PSMC2POL.1;
  PSMC2POL_P2POLA          : bit  absolute PSMC2POL.0;
  PSMC2BLNK                : byte absolute $0EB7;
  PSMC2BLNK_P2FEBM1        : bit  absolute PSMC2BLNK.5;
  PSMC2BLNK_P2FEBM0        : bit  absolute PSMC2BLNK.4;
  PSMC2BLNK_P2REBM1        : bit  absolute PSMC2BLNK.1;
  PSMC2BLNK_P2REBM0        : bit  absolute PSMC2BLNK.0;
  PSMC2REBS                : byte absolute $0EB8;
  PSMC2REBS_P2REBSIN       : bit  absolute PSMC2REBS.7;
  PSMC2REBS_P2REBSC4       : bit  absolute PSMC2REBS.4;
  PSMC2REBS_P2REBSC3       : bit  absolute PSMC2REBS.3;
  PSMC2REBS_P2REBSC2       : bit  absolute PSMC2REBS.2;
  PSMC2REBS_P2REBSC1       : bit  absolute PSMC2REBS.1;
  PSMC2FEBS                : byte absolute $0EB9;
  PSMC2FEBS_P2FEBSIN       : bit  absolute PSMC2FEBS.7;
  PSMC2FEBS_P2FEBSC4       : bit  absolute PSMC2FEBS.4;
  PSMC2FEBS_P2FEBSC3       : bit  absolute PSMC2FEBS.3;
  PSMC2FEBS_P2FEBSC2       : bit  absolute PSMC2FEBS.2;
  PSMC2FEBS_P2FEBSC1       : bit  absolute PSMC2FEBS.1;
  PSMC2PHS                 : byte absolute $0EBA;
  PSMC2PHS_P2PHSIN         : bit  absolute PSMC2PHS.7;
  PSMC2PHS_P2PHSC4         : bit  absolute PSMC2PHS.4;
  PSMC2PHS_P2PHSC3         : bit  absolute PSMC2PHS.3;
  PSMC2PHS_P2PHSC2         : bit  absolute PSMC2PHS.2;
  PSMC2PHS_P2PHSC1         : bit  absolute PSMC2PHS.1;
  PSMC2PHS_P2PHST          : bit  absolute PSMC2PHS.0;
  PSMC2DCS                 : byte absolute $0EBB;
  PSMC2DCS_P2DCSIN         : bit  absolute PSMC2DCS.7;
  PSMC2DCS_P2DCSC4         : bit  absolute PSMC2DCS.4;
  PSMC2DCS_P2DCSC3         : bit  absolute PSMC2DCS.3;
  PSMC2DCS_P2DCSC2         : bit  absolute PSMC2DCS.2;
  PSMC2DCS_P2DCSC1         : bit  absolute PSMC2DCS.1;
  PSMC2DCS_P2DCST          : bit  absolute PSMC2DCS.0;
  PSMC2PRS                 : byte absolute $0EBC;
  PSMC2PRS_P2PRSIN         : bit  absolute PSMC2PRS.7;
  PSMC2PRS_P2PRSC4         : bit  absolute PSMC2PRS.4;
  PSMC2PRS_P2PRSC3         : bit  absolute PSMC2PRS.3;
  PSMC2PRS_P2PRSC2         : bit  absolute PSMC2PRS.2;
  PSMC2PRS_P2PRSC1         : bit  absolute PSMC2PRS.1;
  PSMC2PRS_P2PRST          : bit  absolute PSMC2PRS.0;
  PSMC2ASDC                : byte absolute $0EBD;
  PSMC2ASDC_P2ASE          : bit  absolute PSMC2ASDC.7;
  PSMC2ASDC_P2ASDEN        : bit  absolute PSMC2ASDC.6;
  PSMC2ASDC_P2ARSEN        : bit  absolute PSMC2ASDC.5;
  PSMC2ASDC_P2ASDOV        : bit  absolute PSMC2ASDC.0;
  PSMC2ASDL                : byte absolute $0EBE;
  PSMC2ASDL_P2ASDLB        : bit  absolute PSMC2ASDL.1;
  PSMC2ASDL_P2ASDLA        : bit  absolute PSMC2ASDL.0;
  PSMC2ASDS                : byte absolute $0EBF;
  PSMC2ASDS_P2ASDSIN       : bit  absolute PSMC2ASDS.7;
  PSMC2ASDS_P2ASDSC4       : bit  absolute PSMC2ASDS.4;
  PSMC2ASDS_P2ASDSC3       : bit  absolute PSMC2ASDS.3;
  PSMC2ASDS_P2ASDSC2       : bit  absolute PSMC2ASDS.2;
  PSMC2ASDS_P2ASDSC1       : bit  absolute PSMC2ASDS.1;
  PSMC2INT                 : byte absolute $0EC0;
  PSMC2INT_P2TOVIE         : bit  absolute PSMC2INT.7;
  PSMC2INT_P2TPHIE         : bit  absolute PSMC2INT.6;
  PSMC2INT_P2TDCIE         : bit  absolute PSMC2INT.5;
  PSMC2INT_P2TPRIE         : bit  absolute PSMC2INT.4;
  PSMC2INT_P2TOVIF         : bit  absolute PSMC2INT.3;
  PSMC2INT_P2TPHIF         : bit  absolute PSMC2INT.2;
  PSMC2INT_P2TDCIF         : bit  absolute PSMC2INT.1;
  PSMC2INT_P2TPRIF         : bit  absolute PSMC2INT.0;
  PSMC2PHL                 : byte absolute $0EC1;
  PSMC2PHL_PSMC2PH7        : bit  absolute PSMC2PHL.7;
  PSMC2PHL_PSMC2PH6        : bit  absolute PSMC2PHL.6;
  PSMC2PHL_PSMC2PH5        : bit  absolute PSMC2PHL.5;
  PSMC2PHL_PSMC2PH4        : bit  absolute PSMC2PHL.4;
  PSMC2PHL_PSMC2PH3        : bit  absolute PSMC2PHL.3;
  PSMC2PHL_PSMC2PH2        : bit  absolute PSMC2PHL.2;
  PSMC2PHL_PSMC2PH1        : bit  absolute PSMC2PHL.1;
  PSMC2PHL_PSMC2PH0        : bit  absolute PSMC2PHL.0;
  PSMC2PHH                 : byte absolute $0EC2;
  PSMC2PHH_PSMC2PH15       : bit  absolute PSMC2PHH.7;
  PSMC2PHH_PSMC2PH14       : bit  absolute PSMC2PHH.6;
  PSMC2PHH_PSMC2PH13       : bit  absolute PSMC2PHH.5;
  PSMC2PHH_PSMC2PH12       : bit  absolute PSMC2PHH.4;
  PSMC2PHH_PSMC2PH11       : bit  absolute PSMC2PHH.3;
  PSMC2PHH_PSMC2PH10       : bit  absolute PSMC2PHH.2;
  PSMC2PHH_PSMC2PH9        : bit  absolute PSMC2PHH.1;
  PSMC2PHH_PSMC2PH8        : bit  absolute PSMC2PHH.0;
  PSMC2DCL                 : byte absolute $0EC3;
  PSMC2DCL_PSMC2DC7        : bit  absolute PSMC2DCL.7;
  PSMC2DCL_PSMC2DC6        : bit  absolute PSMC2DCL.6;
  PSMC2DCL_PSMC2DC5        : bit  absolute PSMC2DCL.5;
  PSMC2DCL_PSMC2DC4        : bit  absolute PSMC2DCL.4;
  PSMC2DCL_PSMC2DC3        : bit  absolute PSMC2DCL.3;
  PSMC2DCL_PSMC2DC2        : bit  absolute PSMC2DCL.2;
  PSMC2DCL_PSMC2DC1        : bit  absolute PSMC2DCL.1;
  PSMC2DCL_PSMC2DC0        : bit  absolute PSMC2DCL.0;
  PSMC2DCH                 : byte absolute $0EC4;
  PSMC2DCH_PSMC2DC15       : bit  absolute PSMC2DCH.7;
  PSMC2DCH_PSMC2DC14       : bit  absolute PSMC2DCH.6;
  PSMC2DCH_PSMC2DC13       : bit  absolute PSMC2DCH.5;
  PSMC2DCH_PSMC2DC12       : bit  absolute PSMC2DCH.4;
  PSMC2DCH_PSMC2DC11       : bit  absolute PSMC2DCH.3;
  PSMC2DCH_PSMC2DC10       : bit  absolute PSMC2DCH.2;
  PSMC2DCH_PSMC2DC9        : bit  absolute PSMC2DCH.1;
  PSMC2DCH_PSMC2DC8        : bit  absolute PSMC2DCH.0;
  PSMC2PRL                 : byte absolute $0EC5;
  PSMC2PRL_PSMC2PR7        : bit  absolute PSMC2PRL.7;
  PSMC2PRL_PSMC2PR6        : bit  absolute PSMC2PRL.6;
  PSMC2PRL_PSMC2PR5        : bit  absolute PSMC2PRL.5;
  PSMC2PRL_PSMC2PR4        : bit  absolute PSMC2PRL.4;
  PSMC2PRL_PSMC2PR3        : bit  absolute PSMC2PRL.3;
  PSMC2PRL_PSMC2PR2        : bit  absolute PSMC2PRL.2;
  PSMC2PRL_PSMC2PR1        : bit  absolute PSMC2PRL.1;
  PSMC2PRL_PSMC2PR0        : bit  absolute PSMC2PRL.0;
  PSMC2PRH                 : byte absolute $0EC6;
  PSMC2PRH_PSMC2PR15       : bit  absolute PSMC2PRH.7;
  PSMC2PRH_PSMC2PR14       : bit  absolute PSMC2PRH.6;
  PSMC2PRH_PSMC2PR13       : bit  absolute PSMC2PRH.5;
  PSMC2PRH_PSMC2PR12       : bit  absolute PSMC2PRH.4;
  PSMC2PRH_PSMC2PR11       : bit  absolute PSMC2PRH.3;
  PSMC2PRH_PSMC2PR10       : bit  absolute PSMC2PRH.2;
  PSMC2PRH_PSMC2PR9        : bit  absolute PSMC2PRH.1;
  PSMC2PRH_PSMC2PR8        : bit  absolute PSMC2PRH.0;
  PSMC2TMRL                : byte absolute $0EC7;
  PSMC2TMRL_PSMC2TMR7      : bit  absolute PSMC2TMRL.7;
  PSMC2TMRL_PSMC2TMR6      : bit  absolute PSMC2TMRL.6;
  PSMC2TMRL_PSMC2TMR5      : bit  absolute PSMC2TMRL.5;
  PSMC2TMRL_PSMC2TMR4      : bit  absolute PSMC2TMRL.4;
  PSMC2TMRL_PSMC2TMR3      : bit  absolute PSMC2TMRL.3;
  PSMC2TMRL_PSMC2TMR2      : bit  absolute PSMC2TMRL.2;
  PSMC2TMRL_PSMC2TMR1      : bit  absolute PSMC2TMRL.1;
  PSMC2TMRL_PSMC2TMR0      : bit  absolute PSMC2TMRL.0;
  PSMC2TMRH                : byte absolute $0EC8;
  PSMC2TMRH_PSMC2TMR15     : bit  absolute PSMC2TMRH.7;
  PSMC2TMRH_PSMC2TMR14     : bit  absolute PSMC2TMRH.6;
  PSMC2TMRH_PSMC2TMR13     : bit  absolute PSMC2TMRH.5;
  PSMC2TMRH_PSMC2TMR12     : bit  absolute PSMC2TMRH.4;
  PSMC2TMRH_PSMC2TMR11     : bit  absolute PSMC2TMRH.3;
  PSMC2TMRH_PSMC2TMR10     : bit  absolute PSMC2TMRH.2;
  PSMC2TMRH_PSMC2TMR9      : bit  absolute PSMC2TMRH.1;
  PSMC2TMRH_PSMC2TMR8      : bit  absolute PSMC2TMRH.0;
  PSMC2DBR                 : byte absolute $0EC9;
  PSMC2DBR_PSMC2DBR7       : bit  absolute PSMC2DBR.7;
  PSMC2DBR_PSMC2DBR6       : bit  absolute PSMC2DBR.6;
  PSMC2DBR_PSMC2DBR5       : bit  absolute PSMC2DBR.5;
  PSMC2DBR_PSMC2DBR4       : bit  absolute PSMC2DBR.4;
  PSMC2DBR_PSMC2DBR3       : bit  absolute PSMC2DBR.3;
  PSMC2DBR_PSMC2DBR2       : bit  absolute PSMC2DBR.2;
  PSMC2DBR_PSMC2DBR1       : bit  absolute PSMC2DBR.1;
  PSMC2DBR_PSMC2DBR0       : bit  absolute PSMC2DBR.0;
  PSMC2DBF                 : byte absolute $0ECA;
  PSMC2DBF_PSMC2DBF7       : bit  absolute PSMC2DBF.7;
  PSMC2DBF_PSMC2DBF6       : bit  absolute PSMC2DBF.6;
  PSMC2DBF_PSMC2DBF5       : bit  absolute PSMC2DBF.5;
  PSMC2DBF_PSMC2DBF4       : bit  absolute PSMC2DBF.4;
  PSMC2DBF_PSMC2DBF3       : bit  absolute PSMC2DBF.3;
  PSMC2DBF_PSMC2DBF2       : bit  absolute PSMC2DBF.2;
  PSMC2DBF_PSMC2DBF1       : bit  absolute PSMC2DBF.1;
  PSMC2DBF_PSMC2DBF0       : bit  absolute PSMC2DBF.0;
  PSMC2BLKR                : byte absolute $0ECB;
  PSMC2BLKR_PSMC2BLKR7     : bit  absolute PSMC2BLKR.7;
  PSMC2BLKR_PSMC2BLKR6     : bit  absolute PSMC2BLKR.6;
  PSMC2BLKR_PSMC2BLKR5     : bit  absolute PSMC2BLKR.5;
  PSMC2BLKR_PSMC2BLKR4     : bit  absolute PSMC2BLKR.4;
  PSMC2BLKR_PSMC2BLKR3     : bit  absolute PSMC2BLKR.3;
  PSMC2BLKR_PSMC2BLKR2     : bit  absolute PSMC2BLKR.2;
  PSMC2BLKR_PSMC2BLKR1     : bit  absolute PSMC2BLKR.1;
  PSMC2BLKR_PSMC2BLKR0     : bit  absolute PSMC2BLKR.0;
  PSMC2BLKF                : byte absolute $0ECC;
  PSMC2BLKF_PSMC2BLKF7     : bit  absolute PSMC2BLKF.7;
  PSMC2BLKF_PSMC2BLKF6     : bit  absolute PSMC2BLKF.6;
  PSMC2BLKF_PSMC2BLKF5     : bit  absolute PSMC2BLKF.5;
  PSMC2BLKF_PSMC2BLKF4     : bit  absolute PSMC2BLKF.4;
  PSMC2BLKF_PSMC2BLKF3     : bit  absolute PSMC2BLKF.3;
  PSMC2BLKF_PSMC2BLKF2     : bit  absolute PSMC2BLKF.2;
  PSMC2BLKF_PSMC2BLKF1     : bit  absolute PSMC2BLKF.1;
  PSMC2BLKF_PSMC2BLKF0     : bit  absolute PSMC2BLKF.0;
  PSMC2FFA                 : byte absolute $0ECD;
  PSMC2FFA_PSMC2FFA3       : bit  absolute PSMC2FFA.3;
  PSMC2FFA_PSMC2FFA2       : bit  absolute PSMC2FFA.2;
  PSMC2FFA_PSMC2FFA1       : bit  absolute PSMC2FFA.1;
  PSMC2FFA_PSMC2FFA0       : bit  absolute PSMC2FFA.0;
  PSMC2STR0                : byte absolute $0ECE;
  PSMC2STR0_P2STRB         : bit  absolute PSMC2STR0.1;
  PSMC2STR0_P2STRA         : bit  absolute PSMC2STR0.0;
  PSMC2STR1                : byte absolute $0ECF;
  PSMC2STR1_P2SSYNC        : bit  absolute PSMC2STR1.7;
  PSMC2STR1_P2LSMEN        : bit  absolute PSMC2STR1.1;
  PSMC2STR1_P2HSMEN        : bit  absolute PSMC2STR1.0;
  PSMC3CON                 : byte absolute $0ED1;
  PSMC3CON_PSMC3EN         : bit  absolute PSMC3CON.7;
  PSMC3CON_PSMC3LD         : bit  absolute PSMC3CON.6;
  PSMC3CON_P3DBFE          : bit  absolute PSMC3CON.5;
  PSMC3CON_P3DBRE          : bit  absolute PSMC3CON.4;
  PSMC3CON_P3MODE3         : bit  absolute PSMC3CON.3;
  PSMC3CON_P3MODE2         : bit  absolute PSMC3CON.2;
  PSMC3CON_P3MODE1         : bit  absolute PSMC3CON.1;
  PSMC3CON_P3MODE0         : bit  absolute PSMC3CON.0;
  PSMC3MDL                 : byte absolute $0ED2;
  PSMC3MDL_P3MDLEN         : bit  absolute PSMC3MDL.7;
  PSMC3MDL_P3MDLPOL        : bit  absolute PSMC3MDL.6;
  PSMC3MDL_P3MDLBIT        : bit  absolute PSMC3MDL.5;
  PSMC3MDL_P3MSRC3         : bit  absolute PSMC3MDL.3;
  PSMC3MDL_P3MSRC2         : bit  absolute PSMC3MDL.2;
  PSMC3MDL_P3MSRC1         : bit  absolute PSMC3MDL.1;
  PSMC3MDL_P3MSRC0         : bit  absolute PSMC3MDL.0;
  PSMC3SYNC                : byte absolute $0ED3;
  PSMC3SYNC_P3POFST        : bit  absolute PSMC3SYNC.7;
  PSMC3SYNC_P3PRPOL        : bit  absolute PSMC3SYNC.6;
  PSMC3SYNC_P3DCPOL        : bit  absolute PSMC3SYNC.5;
  PSMC3SYNC_P3SYNC2        : bit  absolute PSMC3SYNC.2;
  PSMC3SYNC_P3SYNC1        : bit  absolute PSMC3SYNC.1;
  PSMC3SYNC_P3SYNC0        : bit  absolute PSMC3SYNC.0;
  PSMC3CLK                 : byte absolute $0ED4;
  PSMC3CLK_P3CPRE1         : bit  absolute PSMC3CLK.5;
  PSMC3CLK_P3CPRE0         : bit  absolute PSMC3CLK.4;
  PSMC3CLK_P3CSRC1         : bit  absolute PSMC3CLK.1;
  PSMC3CLK_P3CSRC0         : bit  absolute PSMC3CLK.0;
  PSMC3OEN                 : byte absolute $0ED5;
  PSMC3OEN_P3OEB           : bit  absolute PSMC3OEN.1;
  PSMC3OEN_P3OEA           : bit  absolute PSMC3OEN.0;
  PSMC3POL                 : byte absolute $0ED6;
  PSMC3POL_P3INPOL         : bit  absolute PSMC3POL.6;
  PSMC3POL_P3POLB          : bit  absolute PSMC3POL.1;
  PSMC3POL_P3POLA          : bit  absolute PSMC3POL.0;
  PSMC3BLNK                : byte absolute $0ED7;
  PSMC3BLNK_P3FEBM1        : bit  absolute PSMC3BLNK.5;
  PSMC3BLNK_P3FEBM0        : bit  absolute PSMC3BLNK.4;
  PSMC3BLNK_P3REBM1        : bit  absolute PSMC3BLNK.1;
  PSMC3BLNK_P3REBM0        : bit  absolute PSMC3BLNK.0;
  PSMC3REBS                : byte absolute $0ED8;
  PSMC3REBS_P3REBSIN       : bit  absolute PSMC3REBS.7;
  PSMC3REBS_P3REBSC4       : bit  absolute PSMC3REBS.4;
  PSMC3REBS_P3REBSC3       : bit  absolute PSMC3REBS.3;
  PSMC3REBS_P3REBSC2       : bit  absolute PSMC3REBS.2;
  PSMC3REBS_P3REBSC1       : bit  absolute PSMC3REBS.1;
  PSMC3FEBS                : byte absolute $0ED9;
  PSMC3FEBS_P3FEBSIN       : bit  absolute PSMC3FEBS.7;
  PSMC3FEBS_P3FEBSC4       : bit  absolute PSMC3FEBS.4;
  PSMC3FEBS_P3FEBSC3       : bit  absolute PSMC3FEBS.3;
  PSMC3FEBS_P3FEBSC2       : bit  absolute PSMC3FEBS.2;
  PSMC3FEBS_P3FEBSC1       : bit  absolute PSMC3FEBS.1;
  PSMC3PHS                 : byte absolute $0EDA;
  PSMC3PHS_P3PHSIN         : bit  absolute PSMC3PHS.7;
  PSMC3PHS_P3PHSC4         : bit  absolute PSMC3PHS.4;
  PSMC3PHS_P3PHSC3         : bit  absolute PSMC3PHS.3;
  PSMC3PHS_P3PHSC2         : bit  absolute PSMC3PHS.2;
  PSMC3PHS_P3PHSC1         : bit  absolute PSMC3PHS.1;
  PSMC3PHS_P3PHST          : bit  absolute PSMC3PHS.0;
  PSMC3DCS                 : byte absolute $0EDB;
  PSMC3DCS_P3DCSIN         : bit  absolute PSMC3DCS.7;
  PSMC3DCS_P3DCSC4         : bit  absolute PSMC3DCS.4;
  PSMC3DCS_P3DCSC3         : bit  absolute PSMC3DCS.3;
  PSMC3DCS_P3DCSC2         : bit  absolute PSMC3DCS.2;
  PSMC3DCS_P3DCSC1         : bit  absolute PSMC3DCS.1;
  PSMC3DCS_P3DCST          : bit  absolute PSMC3DCS.0;
  PSMC3PRS                 : byte absolute $0EDC;
  PSMC3PRS_P3PRSIN         : bit  absolute PSMC3PRS.7;
  PSMC3PRS_P3PRSC4         : bit  absolute PSMC3PRS.4;
  PSMC3PRS_P3PRSC3         : bit  absolute PSMC3PRS.3;
  PSMC3PRS_P3PRSC2         : bit  absolute PSMC3PRS.2;
  PSMC3PRS_P3PRSC1         : bit  absolute PSMC3PRS.1;
  PSMC3PRS_P3PRST          : bit  absolute PSMC3PRS.0;
  PSMC3ASDC                : byte absolute $0EDD;
  PSMC3ASDC_P3ASE          : bit  absolute PSMC3ASDC.7;
  PSMC3ASDC_P3ASDEN        : bit  absolute PSMC3ASDC.6;
  PSMC3ASDC_P3ARSEN        : bit  absolute PSMC3ASDC.5;
  PSMC3ASDC_P3ASDOV        : bit  absolute PSMC3ASDC.0;
  PSMC3ASDL                : byte absolute $0EDE;
  PSMC3ASDL_P3ASDLB        : bit  absolute PSMC3ASDL.1;
  PSMC3ASDL_P3ASDLA        : bit  absolute PSMC3ASDL.0;
  PSMC3ASDS                : byte absolute $0EDF;
  PSMC3ASDS_P3ASDSIN       : bit  absolute PSMC3ASDS.7;
  PSMC3ASDS_P3ASDSC4       : bit  absolute PSMC3ASDS.4;
  PSMC3ASDS_P3ASDSC3       : bit  absolute PSMC3ASDS.3;
  PSMC3ASDS_P3ASDSC2       : bit  absolute PSMC3ASDS.2;
  PSMC3ASDS_P3ASDSC1       : bit  absolute PSMC3ASDS.1;
  PSMC3INT                 : byte absolute $0EE0;
  PSMC3INT_P3TOVIE         : bit  absolute PSMC3INT.7;
  PSMC3INT_P3TPHIE         : bit  absolute PSMC3INT.6;
  PSMC3INT_P3TDCIE         : bit  absolute PSMC3INT.5;
  PSMC3INT_P3TPRIE         : bit  absolute PSMC3INT.4;
  PSMC3INT_P3TOVIF         : bit  absolute PSMC3INT.3;
  PSMC3INT_P3TPHIF         : bit  absolute PSMC3INT.2;
  PSMC3INT_P3TDCIF         : bit  absolute PSMC3INT.1;
  PSMC3INT_P3TPRIF         : bit  absolute PSMC3INT.0;
  PSMC3PHL                 : byte absolute $0EE1;
  PSMC3PHL_PSMC3PH7        : bit  absolute PSMC3PHL.7;
  PSMC3PHL_PSMC3PH6        : bit  absolute PSMC3PHL.6;
  PSMC3PHL_PSMC3PH5        : bit  absolute PSMC3PHL.5;
  PSMC3PHL_PSMC3PH4        : bit  absolute PSMC3PHL.4;
  PSMC3PHL_PSMC3PH3        : bit  absolute PSMC3PHL.3;
  PSMC3PHL_PSMC3PH2        : bit  absolute PSMC3PHL.2;
  PSMC3PHL_PSMC3PH1        : bit  absolute PSMC3PHL.1;
  PSMC3PHL_PSMC3PH0        : bit  absolute PSMC3PHL.0;
  PSMC3PHH                 : byte absolute $0EE2;
  PSMC3PHH_PSMC3PH15       : bit  absolute PSMC3PHH.7;
  PSMC3PHH_PSMC3PH14       : bit  absolute PSMC3PHH.6;
  PSMC3PHH_PSMC3PH13       : bit  absolute PSMC3PHH.5;
  PSMC3PHH_PSMC3PH12       : bit  absolute PSMC3PHH.4;
  PSMC3PHH_PSMC3PH11       : bit  absolute PSMC3PHH.3;
  PSMC3PHH_PSMC3PH10       : bit  absolute PSMC3PHH.2;
  PSMC3PHH_PSMC3PH9        : bit  absolute PSMC3PHH.1;
  PSMC3PHH_PSMC3PH8        : bit  absolute PSMC3PHH.0;
  PSMC3DCL                 : byte absolute $0EE3;
  PSMC3DCL_PSMC3DC7        : bit  absolute PSMC3DCL.7;
  PSMC3DCL_PSMC3DC6        : bit  absolute PSMC3DCL.6;
  PSMC3DCL_PSMC3DC5        : bit  absolute PSMC3DCL.5;
  PSMC3DCL_PSMC3DC4        : bit  absolute PSMC3DCL.4;
  PSMC3DCL_PSMC3DC3        : bit  absolute PSMC3DCL.3;
  PSMC3DCL_PSMC3DC2        : bit  absolute PSMC3DCL.2;
  PSMC3DCL_PSMC3DC1        : bit  absolute PSMC3DCL.1;
  PSMC3DCL_PSMC3DC0        : bit  absolute PSMC3DCL.0;
  PSMC3DCH                 : byte absolute $0EE4;
  PSMC3DCH_PSMC3DC15       : bit  absolute PSMC3DCH.7;
  PSMC3DCH_PSMC3DC14       : bit  absolute PSMC3DCH.6;
  PSMC3DCH_PSMC3DC13       : bit  absolute PSMC3DCH.5;
  PSMC3DCH_PSMC3DC12       : bit  absolute PSMC3DCH.4;
  PSMC3DCH_PSMC3DC11       : bit  absolute PSMC3DCH.3;
  PSMC3DCH_PSMC3DC10       : bit  absolute PSMC3DCH.2;
  PSMC3DCH_PSMC3DC9        : bit  absolute PSMC3DCH.1;
  PSMC3DCH_PSMC3DC8        : bit  absolute PSMC3DCH.0;
  PSMC3PRL                 : byte absolute $0EE5;
  PSMC3PRL_PSMC3PR7        : bit  absolute PSMC3PRL.7;
  PSMC3PRL_PSMC3PR6        : bit  absolute PSMC3PRL.6;
  PSMC3PRL_PSMC3PR5        : bit  absolute PSMC3PRL.5;
  PSMC3PRL_PSMC3PR4        : bit  absolute PSMC3PRL.4;
  PSMC3PRL_PSMC3PR3        : bit  absolute PSMC3PRL.3;
  PSMC3PRL_PSMC3PR2        : bit  absolute PSMC3PRL.2;
  PSMC3PRL_PSMC3PR1        : bit  absolute PSMC3PRL.1;
  PSMC3PRL_PSMC3PR0        : bit  absolute PSMC3PRL.0;
  PSMC3PRH                 : byte absolute $0EE6;
  PSMC3PRH_PSMC3PR15       : bit  absolute PSMC3PRH.7;
  PSMC3PRH_PSMC3PR14       : bit  absolute PSMC3PRH.6;
  PSMC3PRH_PSMC3PR13       : bit  absolute PSMC3PRH.5;
  PSMC3PRH_PSMC3PR12       : bit  absolute PSMC3PRH.4;
  PSMC3PRH_PSMC3PR11       : bit  absolute PSMC3PRH.3;
  PSMC3PRH_PSMC3PR10       : bit  absolute PSMC3PRH.2;
  PSMC3PRH_PSMC3PR9        : bit  absolute PSMC3PRH.1;
  PSMC3PRH_PSMC3PR8        : bit  absolute PSMC3PRH.0;
  PSMC3TMRL                : byte absolute $0EE7;
  PSMC3TMRL_PSMC3TMR7      : bit  absolute PSMC3TMRL.7;
  PSMC3TMRL_PSMC3TMR6      : bit  absolute PSMC3TMRL.6;
  PSMC3TMRL_PSMC3TMR5      : bit  absolute PSMC3TMRL.5;
  PSMC3TMRL_PSMC3TMR4      : bit  absolute PSMC3TMRL.4;
  PSMC3TMRL_PSMC3TMR3      : bit  absolute PSMC3TMRL.3;
  PSMC3TMRL_PSMC3TMR2      : bit  absolute PSMC3TMRL.2;
  PSMC3TMRL_PSMC3TMR1      : bit  absolute PSMC3TMRL.1;
  PSMC3TMRL_PSMC3TMR0      : bit  absolute PSMC3TMRL.0;
  PSMC3TMRH                : byte absolute $0EE8;
  PSMC3TMRH_PSMC3TMR15     : bit  absolute PSMC3TMRH.7;
  PSMC3TMRH_PSMC3TMR14     : bit  absolute PSMC3TMRH.6;
  PSMC3TMRH_PSMC3TMR13     : bit  absolute PSMC3TMRH.5;
  PSMC3TMRH_PSMC3TMR12     : bit  absolute PSMC3TMRH.4;
  PSMC3TMRH_PSMC3TMR11     : bit  absolute PSMC3TMRH.3;
  PSMC3TMRH_PSMC3TMR10     : bit  absolute PSMC3TMRH.2;
  PSMC3TMRH_PSMC3TMR9      : bit  absolute PSMC3TMRH.1;
  PSMC3TMRH_PSMC3TMR8      : bit  absolute PSMC3TMRH.0;
  PSMC3DBR                 : byte absolute $0EE9;
  PSMC3DBR_PSMC3DBR7       : bit  absolute PSMC3DBR.7;
  PSMC3DBR_PSMC3DBR6       : bit  absolute PSMC3DBR.6;
  PSMC3DBR_PSMC3DBR5       : bit  absolute PSMC3DBR.5;
  PSMC3DBR_PSMC3DBR4       : bit  absolute PSMC3DBR.4;
  PSMC3DBR_PSMC3DBR3       : bit  absolute PSMC3DBR.3;
  PSMC3DBR_PSMC3DBR2       : bit  absolute PSMC3DBR.2;
  PSMC3DBR_PSMC3DBR1       : bit  absolute PSMC3DBR.1;
  PSMC3DBR_PSMC3DBR0       : bit  absolute PSMC3DBR.0;
  PSMC3DBF                 : byte absolute $0EEA;
  PSMC3DBF_PSMC3DBF7       : bit  absolute PSMC3DBF.7;
  PSMC3DBF_PSMC3DBF6       : bit  absolute PSMC3DBF.6;
  PSMC3DBF_PSMC3DBF5       : bit  absolute PSMC3DBF.5;
  PSMC3DBF_PSMC3DBF4       : bit  absolute PSMC3DBF.4;
  PSMC3DBF_PSMC3DBF3       : bit  absolute PSMC3DBF.3;
  PSMC3DBF_PSMC3DBF2       : bit  absolute PSMC3DBF.2;
  PSMC3DBF_PSMC3DBF1       : bit  absolute PSMC3DBF.1;
  PSMC3DBF_PSMC3DBF0       : bit  absolute PSMC3DBF.0;
  PSMC3BLKR                : byte absolute $0EEB;
  PSMC3BLKR_PSMC3BLKR7     : bit  absolute PSMC3BLKR.7;
  PSMC3BLKR_PSMC3BLKR6     : bit  absolute PSMC3BLKR.6;
  PSMC3BLKR_PSMC3BLKR5     : bit  absolute PSMC3BLKR.5;
  PSMC3BLKR_PSMC3BLKR4     : bit  absolute PSMC3BLKR.4;
  PSMC3BLKR_PSMC3BLKR3     : bit  absolute PSMC3BLKR.3;
  PSMC3BLKR_PSMC3BLKR2     : bit  absolute PSMC3BLKR.2;
  PSMC3BLKR_PSMC3BLKR1     : bit  absolute PSMC3BLKR.1;
  PSMC3BLKR_PSMC3BLKR0     : bit  absolute PSMC3BLKR.0;
  PSMC3BLKF                : byte absolute $0EEC;
  PSMC3BLKF_PSMC3BLKF7     : bit  absolute PSMC3BLKF.7;
  PSMC3BLKF_PSMC3BLKF6     : bit  absolute PSMC3BLKF.6;
  PSMC3BLKF_PSMC3BLKF5     : bit  absolute PSMC3BLKF.5;
  PSMC3BLKF_PSMC3BLKF4     : bit  absolute PSMC3BLKF.4;
  PSMC3BLKF_PSMC3BLKF3     : bit  absolute PSMC3BLKF.3;
  PSMC3BLKF_PSMC3BLKF2     : bit  absolute PSMC3BLKF.2;
  PSMC3BLKF_PSMC3BLKF1     : bit  absolute PSMC3BLKF.1;
  PSMC3BLKF_PSMC3BLKF0     : bit  absolute PSMC3BLKF.0;
  PSMC3FFA                 : byte absolute $0EED;
  PSMC3FFA_PSMC3FFA3       : bit  absolute PSMC3FFA.3;
  PSMC3FFA_PSMC3FFA2       : bit  absolute PSMC3FFA.2;
  PSMC3FFA_PSMC3FFA1       : bit  absolute PSMC3FFA.1;
  PSMC3FFA_PSMC3FFA0       : bit  absolute PSMC3FFA.0;
  PSMC3STR0                : byte absolute $0EEE;
  PSMC3STR0_P3STRB         : bit  absolute PSMC3STR0.1;
  PSMC3STR0_P3STRA         : bit  absolute PSMC3STR0.0;
  PSMC3STR1                : byte absolute $0EEF;
  PSMC3STR1_P3SSYNC        : bit  absolute PSMC3STR1.7;
  PSMC3STR1_P3LSMEN        : bit  absolute PSMC3STR1.1;
  PSMC3STR1_P3HSMEN        : bit  absolute PSMC3STR1.0;
  PSMC4CON                 : byte absolute $0F11;
  PSMC4CON_PSMC4EN         : bit  absolute PSMC4CON.7;
  PSMC4CON_PSMC4LD         : bit  absolute PSMC4CON.6;
  PSMC4CON_P4DBFE          : bit  absolute PSMC4CON.5;
  PSMC4CON_P4DBRE          : bit  absolute PSMC4CON.4;
  PSMC4CON_P4MODE3         : bit  absolute PSMC4CON.3;
  PSMC4CON_P4MODE2         : bit  absolute PSMC4CON.2;
  PSMC4CON_P4MODE1         : bit  absolute PSMC4CON.1;
  PSMC4CON_P4MODE0         : bit  absolute PSMC4CON.0;
  PSMC4MDL                 : byte absolute $0F12;
  PSMC4MDL_P4MDLEN         : bit  absolute PSMC4MDL.7;
  PSMC4MDL_P4MDLPOL        : bit  absolute PSMC4MDL.6;
  PSMC4MDL_P4MDLBIT        : bit  absolute PSMC4MDL.5;
  PSMC4MDL_P4MSRC3         : bit  absolute PSMC4MDL.3;
  PSMC4MDL_P4MSRC2         : bit  absolute PSMC4MDL.2;
  PSMC4MDL_P4MSRC1         : bit  absolute PSMC4MDL.1;
  PSMC4MDL_P4MSRC0         : bit  absolute PSMC4MDL.0;
  PSMC4SYNC                : byte absolute $0F13;
  PSMC4SYNC_P4POFST        : bit  absolute PSMC4SYNC.7;
  PSMC4SYNC_P4PRPOL        : bit  absolute PSMC4SYNC.6;
  PSMC4SYNC_P4DCPOL        : bit  absolute PSMC4SYNC.5;
  PSMC4SYNC_P4SYNC2        : bit  absolute PSMC4SYNC.2;
  PSMC4SYNC_P4SYNC1        : bit  absolute PSMC4SYNC.1;
  PSMC4SYNC_P4SYNC0        : bit  absolute PSMC4SYNC.0;
  PSMC4CLK                 : byte absolute $0F14;
  PSMC4CLK_P4CPRE1         : bit  absolute PSMC4CLK.5;
  PSMC4CLK_P4CPRE0         : bit  absolute PSMC4CLK.4;
  PSMC4CLK_P4CSRC1         : bit  absolute PSMC4CLK.1;
  PSMC4CLK_P4CSRC0         : bit  absolute PSMC4CLK.0;
  PSMC4OEN                 : byte absolute $0F15;
  PSMC4OEN_P4OEB           : bit  absolute PSMC4OEN.1;
  PSMC4OEN_P4OEA           : bit  absolute PSMC4OEN.0;
  PSMC4POL                 : byte absolute $0F16;
  PSMC4POL_P4INPOL         : bit  absolute PSMC4POL.6;
  PSMC4POL_P4POLB          : bit  absolute PSMC4POL.1;
  PSMC4POL_P4POLA          : bit  absolute PSMC4POL.0;
  PSMC4BLNK                : byte absolute $0F17;
  PSMC4BLNK_P4FEBM1        : bit  absolute PSMC4BLNK.5;
  PSMC4BLNK_P4FEBM0        : bit  absolute PSMC4BLNK.4;
  PSMC4BLNK_P4REBM1        : bit  absolute PSMC4BLNK.1;
  PSMC4BLNK_P4REBM0        : bit  absolute PSMC4BLNK.0;
  PSMC4REBS                : byte absolute $0F18;
  PSMC4REBS_P4REBSIN       : bit  absolute PSMC4REBS.7;
  PSMC4REBS_P4REBSC4       : bit  absolute PSMC4REBS.4;
  PSMC4REBS_P4REBSC3       : bit  absolute PSMC4REBS.3;
  PSMC4REBS_P4REBSC2       : bit  absolute PSMC4REBS.2;
  PSMC4REBS_P4REBSC1       : bit  absolute PSMC4REBS.1;
  PSMC4FEBS                : byte absolute $0F19;
  PSMC4FEBS_P4FEBSIN       : bit  absolute PSMC4FEBS.7;
  PSMC4FEBS_P4FEBSC4       : bit  absolute PSMC4FEBS.4;
  PSMC4FEBS_P4FEBSC3       : bit  absolute PSMC4FEBS.3;
  PSMC4FEBS_P4FEBSC2       : bit  absolute PSMC4FEBS.2;
  PSMC4FEBS_P4FEBSC1       : bit  absolute PSMC4FEBS.1;
  PSMC4PHS                 : byte absolute $0F1A;
  PSMC4PHS_P4PHSIN         : bit  absolute PSMC4PHS.7;
  PSMC4PHS_P4PHSC4         : bit  absolute PSMC4PHS.4;
  PSMC4PHS_P4PHSC3         : bit  absolute PSMC4PHS.3;
  PSMC4PHS_P4PHSC2         : bit  absolute PSMC4PHS.2;
  PSMC4PHS_P4PHSC1         : bit  absolute PSMC4PHS.1;
  PSMC4PHS_P4PHST          : bit  absolute PSMC4PHS.0;
  PSMC4DCS                 : byte absolute $0F1B;
  PSMC4DCS_P4DCSIN         : bit  absolute PSMC4DCS.7;
  PSMC4DCS_P4DCSC4         : bit  absolute PSMC4DCS.4;
  PSMC4DCS_P4DCSC3         : bit  absolute PSMC4DCS.3;
  PSMC4DCS_P4DCSC2         : bit  absolute PSMC4DCS.2;
  PSMC4DCS_P4DCSC1         : bit  absolute PSMC4DCS.1;
  PSMC4DCS_P4DCST          : bit  absolute PSMC4DCS.0;
  PSMC4PRS                 : byte absolute $0F1C;
  PSMC4PRS_P4PRSIN         : bit  absolute PSMC4PRS.7;
  PSMC4PRS_P4PRSC4         : bit  absolute PSMC4PRS.4;
  PSMC4PRS_P4PRSC3         : bit  absolute PSMC4PRS.3;
  PSMC4PRS_P4PRSC2         : bit  absolute PSMC4PRS.2;
  PSMC4PRS_P4PRSC1         : bit  absolute PSMC4PRS.1;
  PSMC4PRS_P4PRST          : bit  absolute PSMC4PRS.0;
  PSMC4ASDC                : byte absolute $0F1D;
  PSMC4ASDC_P4ASE          : bit  absolute PSMC4ASDC.7;
  PSMC4ASDC_P4ASDEN        : bit  absolute PSMC4ASDC.6;
  PSMC4ASDC_P4ARSEN        : bit  absolute PSMC4ASDC.5;
  PSMC4ASDC_P4ASDOV        : bit  absolute PSMC4ASDC.0;
  PSMC4ASDL                : byte absolute $0F1E;
  PSMC4ASDL_P4ASDLB        : bit  absolute PSMC4ASDL.1;
  PSMC4ASDL_P4ASDLA        : bit  absolute PSMC4ASDL.0;
  PSMC4ASDS                : byte absolute $0F1F;
  PSMC4ASDS_P4ASDSIN       : bit  absolute PSMC4ASDS.7;
  PSMC4ASDS_P4ASDSC4       : bit  absolute PSMC4ASDS.4;
  PSMC4ASDS_P4ASDSC3       : bit  absolute PSMC4ASDS.3;
  PSMC4ASDS_P4ASDSC2       : bit  absolute PSMC4ASDS.2;
  PSMC4ASDS_P4ASDSC1       : bit  absolute PSMC4ASDS.1;
  PSMC4INT                 : byte absolute $0F20;
  PSMC4INT_P4TOVIE         : bit  absolute PSMC4INT.7;
  PSMC4INT_P4TPHIE         : bit  absolute PSMC4INT.6;
  PSMC4INT_P4TDCIE         : bit  absolute PSMC4INT.5;
  PSMC4INT_P4TPRIE         : bit  absolute PSMC4INT.4;
  PSMC4INT_P4TOVIF         : bit  absolute PSMC4INT.3;
  PSMC4INT_P4TPHIF         : bit  absolute PSMC4INT.2;
  PSMC4INT_P4TDCIF         : bit  absolute PSMC4INT.1;
  PSMC4INT_P4TPRIF         : bit  absolute PSMC4INT.0;
  PSMC4PHL                 : byte absolute $0F21;
  PSMC4PHL_PSMC4PH7        : bit  absolute PSMC4PHL.7;
  PSMC4PHL_PSMC4PH6        : bit  absolute PSMC4PHL.6;
  PSMC4PHL_PSMC4PH5        : bit  absolute PSMC4PHL.5;
  PSMC4PHL_PSMC4PH4        : bit  absolute PSMC4PHL.4;
  PSMC4PHL_PSMC4PH3        : bit  absolute PSMC4PHL.3;
  PSMC4PHL_PSMC4PH2        : bit  absolute PSMC4PHL.2;
  PSMC4PHL_PSMC4PH1        : bit  absolute PSMC4PHL.1;
  PSMC4PHL_PSMC4PH0        : bit  absolute PSMC4PHL.0;
  PSMC4PHH                 : byte absolute $0F22;
  PSMC4PHH_PSMC4PH15       : bit  absolute PSMC4PHH.7;
  PSMC4PHH_PSMC4PH14       : bit  absolute PSMC4PHH.6;
  PSMC4PHH_PSMC4PH13       : bit  absolute PSMC4PHH.5;
  PSMC4PHH_PSMC4PH12       : bit  absolute PSMC4PHH.4;
  PSMC4PHH_PSMC4PH11       : bit  absolute PSMC4PHH.3;
  PSMC4PHH_PSMC4PH10       : bit  absolute PSMC4PHH.2;
  PSMC4PHH_PSMC4PH9        : bit  absolute PSMC4PHH.1;
  PSMC4PHH_PSMC4PH8        : bit  absolute PSMC4PHH.0;
  PSMC4DCL                 : byte absolute $0F23;
  PSMC4DCL_PSMC4DC7        : bit  absolute PSMC4DCL.7;
  PSMC4DCL_PSMC4DC6        : bit  absolute PSMC4DCL.6;
  PSMC4DCL_PSMC4DC5        : bit  absolute PSMC4DCL.5;
  PSMC4DCL_PSMC4DC4        : bit  absolute PSMC4DCL.4;
  PSMC4DCL_PSMC4DC3        : bit  absolute PSMC4DCL.3;
  PSMC4DCL_PSMC4DC2        : bit  absolute PSMC4DCL.2;
  PSMC4DCL_PSMC4DC1        : bit  absolute PSMC4DCL.1;
  PSMC4DCL_PSMC4DC0        : bit  absolute PSMC4DCL.0;
  PSMC4DCH                 : byte absolute $0F24;
  PSMC4DCH_PSMC4DC15       : bit  absolute PSMC4DCH.7;
  PSMC4DCH_PSMC4DC14       : bit  absolute PSMC4DCH.6;
  PSMC4DCH_PSMC4DC13       : bit  absolute PSMC4DCH.5;
  PSMC4DCH_PSMC4DC12       : bit  absolute PSMC4DCH.4;
  PSMC4DCH_PSMC4DC11       : bit  absolute PSMC4DCH.3;
  PSMC4DCH_PSMC4DC10       : bit  absolute PSMC4DCH.2;
  PSMC4DCH_PSMC4DC9        : bit  absolute PSMC4DCH.1;
  PSMC4DCH_PSMC4DC8        : bit  absolute PSMC4DCH.0;
  PSMC4PRL                 : byte absolute $0F25;
  PSMC4PRL_PSMC4PR7        : bit  absolute PSMC4PRL.7;
  PSMC4PRL_PSMC4PR6        : bit  absolute PSMC4PRL.6;
  PSMC4PRL_PSMC4PR5        : bit  absolute PSMC4PRL.5;
  PSMC4PRL_PSMC4PR4        : bit  absolute PSMC4PRL.4;
  PSMC4PRL_PSMC4PR3        : bit  absolute PSMC4PRL.3;
  PSMC4PRL_PSMC4PR2        : bit  absolute PSMC4PRL.2;
  PSMC4PRL_PSMC4PR1        : bit  absolute PSMC4PRL.1;
  PSMC4PRL_PSMC4PR0        : bit  absolute PSMC4PRL.0;
  PSMC4PRH                 : byte absolute $0F26;
  PSMC4PRH_PSMC4PR15       : bit  absolute PSMC4PRH.7;
  PSMC4PRH_PSMC4PR14       : bit  absolute PSMC4PRH.6;
  PSMC4PRH_PSMC4PR13       : bit  absolute PSMC4PRH.5;
  PSMC4PRH_PSMC4PR12       : bit  absolute PSMC4PRH.4;
  PSMC4PRH_PSMC4PR11       : bit  absolute PSMC4PRH.3;
  PSMC4PRH_PSMC4PR10       : bit  absolute PSMC4PRH.2;
  PSMC4PRH_PSMC4PR9        : bit  absolute PSMC4PRH.1;
  PSMC4PRH_PSMC4PR8        : bit  absolute PSMC4PRH.0;
  PSMC4TMRL                : byte absolute $0F27;
  PSMC4TMRL_PSMC4TMR7      : bit  absolute PSMC4TMRL.7;
  PSMC4TMRL_PSMC4TMR6      : bit  absolute PSMC4TMRL.6;
  PSMC4TMRL_PSMC4TMR5      : bit  absolute PSMC4TMRL.5;
  PSMC4TMRL_PSMC4TMR4      : bit  absolute PSMC4TMRL.4;
  PSMC4TMRL_PSMC4TMR3      : bit  absolute PSMC4TMRL.3;
  PSMC4TMRL_PSMC4TMR2      : bit  absolute PSMC4TMRL.2;
  PSMC4TMRL_PSMC4TMR1      : bit  absolute PSMC4TMRL.1;
  PSMC4TMRL_PSMC4TMR0      : bit  absolute PSMC4TMRL.0;
  PSMC4TMRH                : byte absolute $0F28;
  PSMC4TMRH_PSMC4TMR15     : bit  absolute PSMC4TMRH.7;
  PSMC4TMRH_PSMC4TMR14     : bit  absolute PSMC4TMRH.6;
  PSMC4TMRH_PSMC4TMR13     : bit  absolute PSMC4TMRH.5;
  PSMC4TMRH_PSMC4TMR12     : bit  absolute PSMC4TMRH.4;
  PSMC4TMRH_PSMC4TMR11     : bit  absolute PSMC4TMRH.3;
  PSMC4TMRH_PSMC4TMR10     : bit  absolute PSMC4TMRH.2;
  PSMC4TMRH_PSMC4TMR9      : bit  absolute PSMC4TMRH.1;
  PSMC4TMRH_PSMC4TMR8      : bit  absolute PSMC4TMRH.0;
  PSMC4DBR                 : byte absolute $0F29;
  PSMC4DBR_PSMC4DBR7       : bit  absolute PSMC4DBR.7;
  PSMC4DBR_PSMC4DBR6       : bit  absolute PSMC4DBR.6;
  PSMC4DBR_PSMC4DBR5       : bit  absolute PSMC4DBR.5;
  PSMC4DBR_PSMC4DBR4       : bit  absolute PSMC4DBR.4;
  PSMC4DBR_PSMC4DBR3       : bit  absolute PSMC4DBR.3;
  PSMC4DBR_PSMC4DBR2       : bit  absolute PSMC4DBR.2;
  PSMC4DBR_PSMC4DBR1       : bit  absolute PSMC4DBR.1;
  PSMC4DBR_PSMC4DBR0       : bit  absolute PSMC4DBR.0;
  PSMC4DBF                 : byte absolute $0F2A;
  PSMC4DBF_PSMC4DBF7       : bit  absolute PSMC4DBF.7;
  PSMC4DBF_PSMC4DBF6       : bit  absolute PSMC4DBF.6;
  PSMC4DBF_PSMC4DBF5       : bit  absolute PSMC4DBF.5;
  PSMC4DBF_PSMC4DBF4       : bit  absolute PSMC4DBF.4;
  PSMC4DBF_PSMC4DBF3       : bit  absolute PSMC4DBF.3;
  PSMC4DBF_PSMC4DBF2       : bit  absolute PSMC4DBF.2;
  PSMC4DBF_PSMC4DBF1       : bit  absolute PSMC4DBF.1;
  PSMC4DBF_PSMC4DBF0       : bit  absolute PSMC4DBF.0;
  PSMC4BLKR                : byte absolute $0F2B;
  PSMC4BLKR_PSMC4BLKR7     : bit  absolute PSMC4BLKR.7;
  PSMC4BLKR_PSMC4BLKR6     : bit  absolute PSMC4BLKR.6;
  PSMC4BLKR_PSMC4BLKR5     : bit  absolute PSMC4BLKR.5;
  PSMC4BLKR_PSMC4BLKR4     : bit  absolute PSMC4BLKR.4;
  PSMC4BLKR_PSMC4BLKR3     : bit  absolute PSMC4BLKR.3;
  PSMC4BLKR_PSMC4BLKR2     : bit  absolute PSMC4BLKR.2;
  PSMC4BLKR_PSMC4BLKR1     : bit  absolute PSMC4BLKR.1;
  PSMC4BLKR_PSMC4BLKR0     : bit  absolute PSMC4BLKR.0;
  PSMC4BLKF                : byte absolute $0F2C;
  PSMC4BLKF_PSMC4BLKF7     : bit  absolute PSMC4BLKF.7;
  PSMC4BLKF_PSMC4BLKF6     : bit  absolute PSMC4BLKF.6;
  PSMC4BLKF_PSMC4BLKF5     : bit  absolute PSMC4BLKF.5;
  PSMC4BLKF_PSMC4BLKF4     : bit  absolute PSMC4BLKF.4;
  PSMC4BLKF_PSMC4BLKF3     : bit  absolute PSMC4BLKF.3;
  PSMC4BLKF_PSMC4BLKF2     : bit  absolute PSMC4BLKF.2;
  PSMC4BLKF_PSMC4BLKF1     : bit  absolute PSMC4BLKF.1;
  PSMC4BLKF_PSMC4BLKF0     : bit  absolute PSMC4BLKF.0;
  PSMC4FFA                 : byte absolute $0F2D;
  PSMC4FFA_PSMC4FFA3       : bit  absolute PSMC4FFA.3;
  PSMC4FFA_PSMC4FFA2       : bit  absolute PSMC4FFA.2;
  PSMC4FFA_PSMC4FFA1       : bit  absolute PSMC4FFA.1;
  PSMC4FFA_PSMC4FFA0       : bit  absolute PSMC4FFA.0;
  PSMC4STR0                : byte absolute $0F2E;
  PSMC4STR0_P4STRB         : bit  absolute PSMC4STR0.1;
  PSMC4STR0_P4STRA         : bit  absolute PSMC4STR0.0;
  PSMC4STR1                : byte absolute $0F2F;
  PSMC4STR1_P4SSYNC        : bit  absolute PSMC4STR1.7;
  PSMC4STR1_P4LSMEN        : bit  absolute PSMC4STR1.1;
  PSMC4STR1_P4HSMEN        : bit  absolute PSMC4STR1.0;
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
  {$SET_STATE_RAM '010-01C:SFR'}            // Bank 0 : PORTE, PIR1, PIR2, PIR3, PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08E:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '090-09F:SFR'}            // Bank 1 : TRISE, PIE1, PIE2, PIE3, PIE4, OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10E:SFR'}            // Bank 2 : LATA, LATB, LATC
  {$SET_STATE_RAM '111-11F:SFR'}            // Bank 2 : CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DAC1CON0, DAC1CON1, CM4CON0, CM4CON1, APFCON2, APFCON1, CM3CON0, CM3CON1
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18E:SFR'}            // Bank 3 : ANSELA, ANSELB, ANSELC
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : EEADRL, EEADRH, EEDATL, EEDATH, EECON1, EECON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20E:SFR'}            // Bank 4 : WPUA, WPUB, WPUC
  {$SET_STATE_RAM '210-217:SFR'}            // Bank 4 : WPUE, SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28E:SFR'}            // Bank 5 : ODCONA, ODCONB, ODCONC
  {$SET_STATE_RAM '291-293:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '298-29A:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30E:SFR'}            // Bank 6 : SLRCONA, SLRCONB, SLRCONC
  {$SET_STATE_RAM '311-313:SFR'}            // Bank 6 : CCPR3L, CCPR3H, CCP3CON
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-38E:SFR'}            // Bank 7 : INLVLA, INLVLB, INLVLC
  {$SET_STATE_RAM '390-399:SFR'}            // Bank 7 : INLVLE, IOCAP, IOCAN, IOCAF, IOCBP, IOCBN, IOCBF, IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '39D-39F:SFR'}            // Bank 7 : IOCEP, IOCEN, IOCEF
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '511-511:SFR'}            // Bank 10 : OPA1CON
  {$SET_STATE_RAM '513-513:SFR'}            // Bank 10 : OPA2CON
  {$SET_STATE_RAM '51A-51A:SFR'}            // Bank 10 : CLKRCON
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '591-596:SFR'}            // Bank 11 : DAC2CON0, DAC2REF, DAC3CON0, DAC3REF, DAC4CON0, DAC4REF
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '620-66F:GPR'}           
  {$SET_STATE_RAM '6A0-6EF:GPR'}           
  {$SET_STATE_RAM '720-76F:GPR'}           
  {$SET_STATE_RAM '7A0-7EF:GPR'}           
  {$SET_STATE_RAM '820-86F:GPR'}           
  {$SET_STATE_RAM '8A0-8EF:GPR'}           
  {$SET_STATE_RAM '920-96F:GPR'}           
  {$SET_STATE_RAM '9A0-9EF:GPR'}           
  {$SET_STATE_RAM 'A20-A6F:GPR'}           
  {$SET_STATE_RAM 'AA0-AEF:GPR'}           
  {$SET_STATE_RAM 'B20-B6F:GPR'}           
  {$SET_STATE_RAM 'BA0-BEF:GPR'}           
  {$SET_STATE_RAM 'C20-C6F:GPR'}           
  {$SET_STATE_RAM 'CA0-CBF:GPR'}           
  {$SET_STATE_RAM 'E91-EAF:SFR'}            // Bank 29 : PSMC1CON, PSMC1MDL, PSMC1SYNC, PSMC1CLK, PSMC1OEN, PSMC1POL, PSMC1BLNK, PSMC1REBS, PSMC1FEBS, PSMC1PHS, PSMC1DCS, PSMC1PRS, PSMC1ASDC, PSMC1ASDL, PSMC1ASDS, PSMC1INT, PSMC1PHL, PSMC1PHH, PSMC1DCL, PSMC1DCH, PSMC1PRL, PSMC1PRH, PSMC1TMRL, PSMC1TMRH, PSMC1DBR, PSMC1DBF, PSMC1BLKR, PSMC1BLKF, PSMC1FFA, PSMC1STR0, PSMC1STR1
  {$SET_STATE_RAM 'EB1-ECF:SFR'}            // Bank 29 : PSMC2CON, PSMC2MDL, PSMC2SYNC, PSMC2CLK, PSMC2OEN, PSMC2POL, PSMC2BLNK, PSMC2REBS, PSMC2FEBS, PSMC2PHS, PSMC2DCS, PSMC2PRS, PSMC2ASDC, PSMC2ASDL, PSMC2ASDS, PSMC2INT, PSMC2PHL, PSMC2PHH, PSMC2DCL, PSMC2DCH, PSMC2PRL, PSMC2PRH, PSMC2TMRL, PSMC2TMRH, PSMC2DBR, PSMC2DBF, PSMC2BLKR, PSMC2BLKF, PSMC2FFA, PSMC2STR0, PSMC2STR1
  {$SET_STATE_RAM 'ED1-EEF:SFR'}            // Bank 29 : PSMC3CON, PSMC3MDL, PSMC3SYNC, PSMC3CLK, PSMC3OEN, PSMC3POL, PSMC3BLNK, PSMC3REBS, PSMC3FEBS, PSMC3PHS, PSMC3DCS, PSMC3PRS, PSMC3ASDC, PSMC3ASDL, PSMC3ASDS, PSMC3INT, PSMC3PHL, PSMC3PHH, PSMC3DCL, PSMC3DCH, PSMC3PRL, PSMC3PRH, PSMC3TMRL, PSMC3TMRH, PSMC3DBR, PSMC3DBF, PSMC3BLKR, PSMC3BLKF, PSMC3FFA, PSMC3STR0, PSMC3STR1
  {$SET_STATE_RAM 'F11-F2F:SFR'}            // Bank 30 : PSMC4CON, PSMC4MDL, PSMC4SYNC, PSMC4CLK, PSMC4OEN, PSMC4POL, PSMC4BLNK, PSMC4REBS, PSMC4FEBS, PSMC4PHS, PSMC4DCS, PSMC4PRS, PSMC4ASDC, PSMC4ASDL, PSMC4ASDS, PSMC4INT, PSMC4PHL, PSMC4PHH, PSMC4DCL, PSMC4DCH, PSMC4PRL, PSMC4PRH, PSMC4TMRL, PSMC4TMRH, PSMC4DBR, PSMC4DBF, PSMC4BLKR, PSMC4BLKF, PSMC4FFA, PSMC4STR0, PSMC4STR1
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:08'} // PORTE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:10'} // PIR3 bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:08'} // TRISE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:10'} // PIE3 bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:0F'} // CMOUT bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:BD'} // DAC1CON0 bits 6,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:07'} // APFCON2 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:BF'} // ANSELA bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:7F'} // ANSELB bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:08'} // WPUE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29A:3F'} // CCP2CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:3F'} // CCP3CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '390:08'} // INLVLE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:08'} // IOCEP bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:08'} // IOCEN bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:08'} // IOCEF bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '511:C7'} // OPA1CON bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '513:C7'} // OPA2CON bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '591:BC'} // DAC2CON0 bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '592:1F'} // DAC2REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '593:BC'} // DAC3CON0 bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '594:1F'} // DAC3REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '595:BC'} // DAC4CON0 bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '596:1F'} // DAC4REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E92:EF'} // PSMC1MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E93:E7'} // PSMC1SYNC bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E94:33'} // PSMC1CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E95:3F'} // PSMC1OEN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E96:7F'} // PSMC1POL bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E97:33'} // PSMC1BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E98:9E'} // PSMC1REBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E99:9E'} // PSMC1FEBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9A:9F'} // PSMC1PHS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9B:9F'} // PSMC1DCS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9C:9F'} // PSMC1PRS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9D:E1'} // PSMC1ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9E:3F'} // PSMC1ASDL bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9F:9E'} // PSMC1ASDS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAD:0F'} // PSMC1FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAE:3F'} // PSMC1STR0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAF:83'} // PSMC1STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB2:EF'} // PSMC2MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB3:E7'} // PSMC2SYNC bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB4:33'} // PSMC2CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB5:03'} // PSMC2OEN bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB6:43'} // PSMC2POL bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB7:33'} // PSMC2BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB8:9E'} // PSMC2REBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB9:9E'} // PSMC2FEBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBA:9F'} // PSMC2PHS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBB:9F'} // PSMC2DCS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBC:9F'} // PSMC2PRS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBD:E1'} // PSMC2ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBE:03'} // PSMC2ASDL bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EBF:9E'} // PSMC2ASDS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ECD:0F'} // PSMC2FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ECE:03'} // PSMC2STR0 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ECF:83'} // PSMC2STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED2:EF'} // PSMC3MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED3:E7'} // PSMC3SYNC bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED4:33'} // PSMC3CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED5:03'} // PSMC3OEN bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED6:43'} // PSMC3POL bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED7:33'} // PSMC3BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED8:9E'} // PSMC3REBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'ED9:9E'} // PSMC3FEBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDA:9F'} // PSMC3PHS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDB:9F'} // PSMC3DCS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDC:9F'} // PSMC3PRS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDD:E1'} // PSMC3ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDE:03'} // PSMC3ASDL bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EDF:9E'} // PSMC3ASDS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EED:0F'} // PSMC3FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EEE:03'} // PSMC3STR0 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EEF:83'} // PSMC3STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F12:EF'} // PSMC4MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F13:E7'} // PSMC4SYNC bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F14:33'} // PSMC4CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F15:03'} // PSMC4OEN bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F16:43'} // PSMC4POL bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F17:33'} // PSMC4BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F18:9E'} // PSMC4REBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F19:9E'} // PSMC4FEBS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1A:9F'} // PSMC4PHS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1B:9F'} // PSMC4DCS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1C:9F'} // PSMC4PRS bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1D:E1'} // PSMC4ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1E:03'} // PSMC4ASDL bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1F:9E'} // PSMC4ASDS bits 6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2D:0F'} // PSMC4FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2E:03'} // PSMC4STR0 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2F:83'} // PSMC4STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RE3/MCLR/Vpp
  // Pin  2 : RA0/AN0/C1IN0-/C2IN0-/C3IN0-/C4IN0-/SS
  // Pin  3 : RA1/AN1/C1IN1-/C2IN1-/C3IN1-/C4IN1-/OPA1OUT
  // Pin  4 : RA2/AN2/C1IN0+/C2IN0+/C3IN0+/C4IN0+/DAC1OUT1/Vref-/DAC1Vref-
  // Pin  5 : RA3/AN3/Vref+/C1IN1+/DAC1Vref+n/DAC2Vref+/DAC3Vref+/DAC4Vref+
  // Pin  6 : RA4/C1OUT/OPA1IN+/T0CKI/DAC4OUT1
  // Pin  7 : RA5/AN4/C2OUT/OPA1IN-/SS/DA2COUT1
  // Pin  8 : Vss
  // Pin  9 : RA7/Vref+/PSMC1CLK/PSMC2CLK/PSMC3CLK/PSMC4CLK/OSC1/CLKIN
  // Pin 10 : RA6/C2OUT/OSC2/CLKOUT/VCAP
  // Pin 11 : RC0/SOSCO/T1CKI/PSMC1A
  // Pin 12 : RC1/SOSCI/CCP2/PSMC1B
  // Pin 13 : RC2/CCP1/PSMC3B/PSMC1C
  // Pin 14 : RC3/SCK/SCL/PSMC4A/PSMC1D
  // Pin 15 : RC4/SDI/SDA/PSMC4B/PSMC1E
  // Pin 16 : RC5/SDO/PSMC3A/PSMC1F
  // Pin 17 : RC6/CCP3/TX/CK/PSMC2A
  // Pin 18 : RC7/C4OUT/RX/DT/PSMC2B
  // Pin 19 : Vss
  // Pin 20 : Vdd
  // Pin 21 : RB0/AN12/C2IN1+/INT/CCP1/PSMC1IN/PSMC2IN/PSMC3IN/PSMC4IN
  // Pin 22 : RB1/AN10/C1IN3-/C2IN3-/C3IN3-/C4IN3-/OPA2OUT
  // Pin 23 : RB2/AN8/OPA2IN-/CLKR/DAC3OUT1
  // Pin 24 : RB3/AN9/C1IN2-/C2IN2-/C3IN2-/OPA2IN+/CCP2
  // Pin 25 : RB4/AN11/C3IN1+/SS
  // Pin 26 : RB5/AN13/CCP3/C3OUT/T1G/SDO/C4IN2-
  // Pin 27 : RB6/SDI/TX/CK/SDA/C4IN1+/ICSPCLK/ICDCLK
  // Pin 28 : RB7/SCK/RX/DT/SCL/DAC4OUT2/DAC3OUT2/DAC2OUT2/DAC1OUT2/ICSPDAT/ICDDAT


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
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to FFFh write protected, 1000h to 1FFFh may be modified by EECON control
  {$define _WRT_ALL      = $3FFC}  // 000h to 7FFh write protected, no addresses may be modified by EECON control

  // VCAPEN : Voltage Regulator Capacitor Enable bit
  {$define _VCAPEN_OFF   = $3FFF}  // Vcap functionality is disabled on RA6.
  {$define _VCAPEN_ON    = $3FDF}  // Vcap functionality is enabled on RA6 (Vddcore is connected to the pad)

  // PLLEN : PLL Enable
  {$define _PLLEN_ON     = $3FFF}  // 4x PLL enabled
  {$define _PLLEN_OFF    = $3EFF}  // 4x PLL disabled

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON    = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF   = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO      = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI      = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LPBOR : Low Power Brown-Out Reset Enable Bit
  {$define _LPBOR_OFF    = $3FFF}  // Low power brown-out is disabled
  {$define _LPBOR_ON     = $37FF}  // Low power brown-out is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
