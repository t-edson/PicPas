unit PIC16F1783;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1783'}
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
  PIR2_C3IF                : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR4                     : byte absolute $0014;
  PIR4_PSMC2TIF            : bit  absolute PIR4.5;
  PIR4_PSMC1TIF            : bit  absolute PIR4.4;
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
  PIE2_C3IE                : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE4                     : byte absolute $0094;
  PIE4_PSMC2TIE            : bit  absolute PIE4.5;
  PIE4_PSMC1TIE            : bit  absolute PIE4.4;
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
  DACCON0                  : byte absolute $0118;
  DACCON0_DACEN            : bit  absolute DACCON0.7;
  DACCON0_DACOE1           : bit  absolute DACCON0.5;
  DACCON0_DACOE2           : bit  absolute DACCON0.4;
  DACCON0_DACPSS1          : bit  absolute DACCON0.3;
  DACCON0_DACPSS0          : bit  absolute DACCON0.2;
  DACCON0_DACNSS           : bit  absolute DACCON0.0;
  DACCON1                  : byte absolute $0119;
  DACCON1_DACR7            : bit  absolute DACCON1.7;
  DACCON1_DACR6            : bit  absolute DACCON1.6;
  DACCON1_DACR5            : bit  absolute DACCON1.5;
  DACCON1_DACR4            : bit  absolute DACCON1.4;
  DACCON1_DACR3            : bit  absolute DACCON1.3;
  DACCON1_DACR2            : bit  absolute DACCON1.2;
  DACCON1_DACR1            : bit  absolute DACCON1.1;
  DACCON1_DACR0            : bit  absolute DACCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_C2OUTSEL          : bit  absolute APFCON.7;
  APFCON_CCP1SEL           : bit  absolute APFCON.6;
  APFCON_SDOSEL            : bit  absolute APFCON.5;
  APFCON_SCKSEL            : bit  absolute APFCON.4;
  APFCON_SDISEL            : bit  absolute APFCON.3;
  APFCON_TXSEL             : bit  absolute APFCON.2;
  APFCON_RXSEL             : bit  absolute APFCON.1;
  APFCON_CCP2SEL           : bit  absolute APFCON.0;
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
  CCP1CON_P1M1             : bit  absolute CCP1CON.7;
  CCP1CON_P1M0             : bit  absolute CCP1CON.6;
  CCP1CON_DC1B1            : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0            : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3           : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2           : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1           : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0           : bit  absolute CCP1CON.0;
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
  OPA1CON_OPA1EN           : bit  absolute OPA1CON.7;
  OPA1CON_OPA1SP           : bit  absolute OPA1CON.6;
  OPA1CON_OPA1CH1          : bit  absolute OPA1CON.1;
  OPA1CON_OPA1CH0          : bit  absolute OPA1CON.0;
  OPA2CON                  : byte absolute $0513;
  OPA2CON_OPA2EN           : bit  absolute OPA2CON.7;
  OPA2CON_OPA2SP           : bit  absolute OPA2CON.6;
  OPA2CON_OPA2CH1          : bit  absolute OPA2CON.1;
  OPA2CON_OPA2CH0          : bit  absolute OPA2CON.0;
  CLKRCON                  : byte absolute $051A;
  CLKRCON_CLKREN           : bit  absolute CLKRCON.7;
  CLKRCON_CLKROE           : bit  absolute CLKRCON.6;
  CLKRCON_CLKRSLR          : bit  absolute CLKRCON.5;
  CLKRCON_CLKRDC1          : bit  absolute CLKRCON.4;
  CLKRCON_CLKRDC0          : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2         : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1         : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0         : bit  absolute CLKRCON.0;
  PSMC1CON                 : byte absolute $0811;
  PSMC1CON_PSMC1EN         : bit  absolute PSMC1CON.7;
  PSMC1CON_PSMC1LD         : bit  absolute PSMC1CON.6;
  PSMC1CON_P1DBFE          : bit  absolute PSMC1CON.5;
  PSMC1CON_P1DBRE          : bit  absolute PSMC1CON.4;
  PSMC1CON_P1MODE3         : bit  absolute PSMC1CON.3;
  PSMC1CON_P1MODE2         : bit  absolute PSMC1CON.2;
  PSMC1CON_P1MODE1         : bit  absolute PSMC1CON.1;
  PSMC1CON_P1MODE0         : bit  absolute PSMC1CON.0;
  PSMC1MDL                 : byte absolute $0812;
  PSMC1MDL_P1MDLEN         : bit  absolute PSMC1MDL.7;
  PSMC1MDL_P1MDLPOL        : bit  absolute PSMC1MDL.6;
  PSMC1MDL_P1MDLBIT        : bit  absolute PSMC1MDL.5;
  PSMC1MDL_P1MSRC3         : bit  absolute PSMC1MDL.3;
  PSMC1MDL_P1MSRC2         : bit  absolute PSMC1MDL.2;
  PSMC1MDL_P1MSRC1         : bit  absolute PSMC1MDL.1;
  PSMC1MDL_P1MSRC0         : bit  absolute PSMC1MDL.0;
  PSMC1SYNC                : byte absolute $0813;
  PSMC1SYNC_P1SYNC1        : bit  absolute PSMC1SYNC.1;
  PSMC1SYNC_P1SYNC0        : bit  absolute PSMC1SYNC.0;
  PSMC1CLK                 : byte absolute $0814;
  PSMC1CLK_P1CPRE1         : bit  absolute PSMC1CLK.5;
  PSMC1CLK_P1CPRE0         : bit  absolute PSMC1CLK.4;
  PSMC1CLK_P1CSRC1         : bit  absolute PSMC1CLK.1;
  PSMC1CLK_P1CSRC0         : bit  absolute PSMC1CLK.0;
  PSMC1OEN                 : byte absolute $0815;
  PSMC1OEN_P1OEF           : bit  absolute PSMC1OEN.5;
  PSMC1OEN_P1OEE           : bit  absolute PSMC1OEN.4;
  PSMC1OEN_P1OED           : bit  absolute PSMC1OEN.3;
  PSMC1OEN_P1OEC           : bit  absolute PSMC1OEN.2;
  PSMC1OEN_P1OEB           : bit  absolute PSMC1OEN.1;
  PSMC1OEN_P1OEA           : bit  absolute PSMC1OEN.0;
  PSMC1POL                 : byte absolute $0816;
  PSMC1POL_P1INPOL         : bit  absolute PSMC1POL.6;
  PSMC1POL_P1POLF          : bit  absolute PSMC1POL.5;
  PSMC1POL_P1POLE          : bit  absolute PSMC1POL.4;
  PSMC1POL_P1POLD          : bit  absolute PSMC1POL.3;
  PSMC1POL_P1POLC          : bit  absolute PSMC1POL.2;
  PSMC1POL_P1POLB          : bit  absolute PSMC1POL.1;
  PSMC1POL_P1POLA          : bit  absolute PSMC1POL.0;
  PSMC1BLNK                : byte absolute $0817;
  PSMC1BLNK_P1FEBM1        : bit  absolute PSMC1BLNK.5;
  PSMC1BLNK_P1FEBM0        : bit  absolute PSMC1BLNK.4;
  PSMC1BLNK_P1REBM1        : bit  absolute PSMC1BLNK.1;
  PSMC1BLNK_P1REBM0        : bit  absolute PSMC1BLNK.0;
  PSMC1REBS                : byte absolute $0818;
  PSMC1REBS_P1REBSIN       : bit  absolute PSMC1REBS.7;
  PSMC1REBS_P1REBSC3       : bit  absolute PSMC1REBS.3;
  PSMC1REBS_P1REBSC2       : bit  absolute PSMC1REBS.2;
  PSMC1REBS_P1REBSC1       : bit  absolute PSMC1REBS.1;
  PSMC1FEBS                : byte absolute $0819;
  PSMC1FEBS_P1FEBSIN       : bit  absolute PSMC1FEBS.7;
  PSMC1FEBS_P1FEBSC3       : bit  absolute PSMC1FEBS.3;
  PSMC1FEBS_P1FEBSC2       : bit  absolute PSMC1FEBS.2;
  PSMC1FEBS_P1FEBSC1       : bit  absolute PSMC1FEBS.1;
  PSMC1PHS                 : byte absolute $081A;
  PSMC1PHS_P1PHSIN         : bit  absolute PSMC1PHS.7;
  PSMC1PHS_P1PHSC3         : bit  absolute PSMC1PHS.3;
  PSMC1PHS_P1PHSC2         : bit  absolute PSMC1PHS.2;
  PSMC1PHS_P1PHSC1         : bit  absolute PSMC1PHS.1;
  PSMC1PHS_P1PHST          : bit  absolute PSMC1PHS.0;
  PSMC1DCS                 : byte absolute $081B;
  PSMC1DCS_P1DCSIN         : bit  absolute PSMC1DCS.7;
  PSMC1DCS_P1DCSC3         : bit  absolute PSMC1DCS.3;
  PSMC1DCS_P1DCSC2         : bit  absolute PSMC1DCS.2;
  PSMC1DCS_P1DCSC1         : bit  absolute PSMC1DCS.1;
  PSMC1DCS_P1DCST          : bit  absolute PSMC1DCS.0;
  PSMC1PRS                 : byte absolute $081C;
  PSMC1PRS_P1PRSIN         : bit  absolute PSMC1PRS.7;
  PSMC1PRS_P1PRSC3         : bit  absolute PSMC1PRS.3;
  PSMC1PRS_P1PRSC2         : bit  absolute PSMC1PRS.2;
  PSMC1PRS_P1PRSC1         : bit  absolute PSMC1PRS.1;
  PSMC1PRS_P1PRST          : bit  absolute PSMC1PRS.0;
  PSMC1ASDC                : byte absolute $081D;
  PSMC1ASDC_P1ASE          : bit  absolute PSMC1ASDC.7;
  PSMC1ASDC_P1ASDEN        : bit  absolute PSMC1ASDC.6;
  PSMC1ASDC_P1ARSEN        : bit  absolute PSMC1ASDC.5;
  PSMC1ASDC_P1ASDOV        : bit  absolute PSMC1ASDC.0;
  PSMC1ASDL                : byte absolute $081E;
  PSMC1ASDL_P1ASDLF        : bit  absolute PSMC1ASDL.5;
  PSMC1ASDL_P1ASDLE        : bit  absolute PSMC1ASDL.4;
  PSMC1ASDL_P1ASDLD        : bit  absolute PSMC1ASDL.3;
  PSMC1ASDL_P1ASDLC        : bit  absolute PSMC1ASDL.2;
  PSMC1ASDL_P1ASDLB        : bit  absolute PSMC1ASDL.1;
  PSMC1ASDL_P1ASDLA        : bit  absolute PSMC1ASDL.0;
  PSMC1ASDS                : byte absolute $081F;
  PSMC1ASDS_P1ASDSIN       : bit  absolute PSMC1ASDS.7;
  PSMC1ASDS_P1ASDSC3       : bit  absolute PSMC1ASDS.3;
  PSMC1ASDS_P1ASDSC2       : bit  absolute PSMC1ASDS.2;
  PSMC1ASDS_P1ASDSC1       : bit  absolute PSMC1ASDS.1;
  PSMC1INT                 : byte absolute $0820;
  PSMC1INT_P1TOVIE         : bit  absolute PSMC1INT.7;
  PSMC1INT_P1TPHIE         : bit  absolute PSMC1INT.6;
  PSMC1INT_P1TDCIE         : bit  absolute PSMC1INT.5;
  PSMC1INT_P1TPRIE         : bit  absolute PSMC1INT.4;
  PSMC1INT_P1TOVIF         : bit  absolute PSMC1INT.3;
  PSMC1INT_P1TPHIF         : bit  absolute PSMC1INT.2;
  PSMC1INT_P1TDCIF         : bit  absolute PSMC1INT.1;
  PSMC1INT_P1TPRIF         : bit  absolute PSMC1INT.0;
  PSMC1PHL                 : byte absolute $0821;
  PSMC1PHL_PSMC1PH7        : bit  absolute PSMC1PHL.7;
  PSMC1PHL_PSMC1PH6        : bit  absolute PSMC1PHL.6;
  PSMC1PHL_PSMC1PH5        : bit  absolute PSMC1PHL.5;
  PSMC1PHL_PSMC1PH4        : bit  absolute PSMC1PHL.4;
  PSMC1PHL_PSMC1PH3        : bit  absolute PSMC1PHL.3;
  PSMC1PHL_PSMC1PH2        : bit  absolute PSMC1PHL.2;
  PSMC1PHL_PSMC1PH1        : bit  absolute PSMC1PHL.1;
  PSMC1PHL_PSMC1PH0        : bit  absolute PSMC1PHL.0;
  PSMC1PHH                 : byte absolute $0822;
  PSMC1PHH_PSMC1PH15       : bit  absolute PSMC1PHH.7;
  PSMC1PHH_PSMC1PH14       : bit  absolute PSMC1PHH.6;
  PSMC1PHH_PSMC1PH13       : bit  absolute PSMC1PHH.5;
  PSMC1PHH_PSMC1PH12       : bit  absolute PSMC1PHH.4;
  PSMC1PHH_PSMC1PH11       : bit  absolute PSMC1PHH.3;
  PSMC1PHH_PSMC1PH10       : bit  absolute PSMC1PHH.2;
  PSMC1PHH_PSMC1PH9        : bit  absolute PSMC1PHH.1;
  PSMC1PHH_PSMC1PH8        : bit  absolute PSMC1PHH.0;
  PSMC1DCL                 : byte absolute $0823;
  PSMC1DCL_PSMC1DC7        : bit  absolute PSMC1DCL.7;
  PSMC1DCL_PSMC1DC6        : bit  absolute PSMC1DCL.6;
  PSMC1DCL_PSMC1DC5        : bit  absolute PSMC1DCL.5;
  PSMC1DCL_PSMC1DC4        : bit  absolute PSMC1DCL.4;
  PSMC1DCL_PSMC1DC3        : bit  absolute PSMC1DCL.3;
  PSMC1DCL_PSMC1DC2        : bit  absolute PSMC1DCL.2;
  PSMC1DCL_PSMC1DC1        : bit  absolute PSMC1DCL.1;
  PSMC1DCL_PSMC1DC0        : bit  absolute PSMC1DCL.0;
  PSMC1DCH                 : byte absolute $0824;
  PSMC1DCH_PSMC1DC15       : bit  absolute PSMC1DCH.7;
  PSMC1DCH_PSMC1DC14       : bit  absolute PSMC1DCH.6;
  PSMC1DCH_PSMC1DC13       : bit  absolute PSMC1DCH.5;
  PSMC1DCH_PSMC1DC12       : bit  absolute PSMC1DCH.4;
  PSMC1DCH_PSMC1DC11       : bit  absolute PSMC1DCH.3;
  PSMC1DCH_PSMC1DC10       : bit  absolute PSMC1DCH.2;
  PSMC1DCH_PSMC1DC9        : bit  absolute PSMC1DCH.1;
  PSMC1DCH_PSMC1DC8        : bit  absolute PSMC1DCH.0;
  PSMC1PRL                 : byte absolute $0825;
  PSMC1PRL_PSMC1PR7        : bit  absolute PSMC1PRL.7;
  PSMC1PRL_PSMC1PR6        : bit  absolute PSMC1PRL.6;
  PSMC1PRL_PSMC1PR5        : bit  absolute PSMC1PRL.5;
  PSMC1PRL_PSMC1PR4        : bit  absolute PSMC1PRL.4;
  PSMC1PRL_PSMC1PR3        : bit  absolute PSMC1PRL.3;
  PSMC1PRL_PSMC1PR2        : bit  absolute PSMC1PRL.2;
  PSMC1PRL_PSMC1PR1        : bit  absolute PSMC1PRL.1;
  PSMC1PRL_PSMC1PR0        : bit  absolute PSMC1PRL.0;
  PSMC1PRH                 : byte absolute $0826;
  PSMC1PRH_PSMC1PR15       : bit  absolute PSMC1PRH.7;
  PSMC1PRH_PSMC1PR14       : bit  absolute PSMC1PRH.6;
  PSMC1PRH_PSMC1PR13       : bit  absolute PSMC1PRH.5;
  PSMC1PRH_PSMC1PR12       : bit  absolute PSMC1PRH.4;
  PSMC1PRH_PSMC1PR11       : bit  absolute PSMC1PRH.3;
  PSMC1PRH_PSMC1PR10       : bit  absolute PSMC1PRH.2;
  PSMC1PRH_PSMC1PR9        : bit  absolute PSMC1PRH.1;
  PSMC1PRH_PSMC1PR8        : bit  absolute PSMC1PRH.0;
  PSMC1TMRL                : byte absolute $0827;
  PSMC1TMRL_PSMC1TMR7      : bit  absolute PSMC1TMRL.7;
  PSMC1TMRL_PSMC1TMR6      : bit  absolute PSMC1TMRL.6;
  PSMC1TMRL_PSMC1TMR5      : bit  absolute PSMC1TMRL.5;
  PSMC1TMRL_PSMC1TMR4      : bit  absolute PSMC1TMRL.4;
  PSMC1TMRL_PSMC1TMR3      : bit  absolute PSMC1TMRL.3;
  PSMC1TMRL_PSMC1TMR2      : bit  absolute PSMC1TMRL.2;
  PSMC1TMRL_PSMC1TMR1      : bit  absolute PSMC1TMRL.1;
  PSMC1TMRL_PSMC1TMR0      : bit  absolute PSMC1TMRL.0;
  PSMC1TMRH                : byte absolute $0828;
  PSMC1TMRH_PSMC1TMR15     : bit  absolute PSMC1TMRH.7;
  PSMC1TMRH_PSMC1TMR14     : bit  absolute PSMC1TMRH.6;
  PSMC1TMRH_PSMC1TMR13     : bit  absolute PSMC1TMRH.5;
  PSMC1TMRH_PSMC1TMR12     : bit  absolute PSMC1TMRH.4;
  PSMC1TMRH_PSMC1TMR11     : bit  absolute PSMC1TMRH.3;
  PSMC1TMRH_PSMC1TMR10     : bit  absolute PSMC1TMRH.2;
  PSMC1TMRH_PSMC1TMR9      : bit  absolute PSMC1TMRH.1;
  PSMC1TMRH_PSMC1TMR8      : bit  absolute PSMC1TMRH.0;
  PSMC1DBR                 : byte absolute $0829;
  PSMC1DBR_PSMC1DBR7       : bit  absolute PSMC1DBR.7;
  PSMC1DBR_PSMC1DBR6       : bit  absolute PSMC1DBR.6;
  PSMC1DBR_PSMC1DBR5       : bit  absolute PSMC1DBR.5;
  PSMC1DBR_PSMC1DBR4       : bit  absolute PSMC1DBR.4;
  PSMC1DBR_PSMC1DBR3       : bit  absolute PSMC1DBR.3;
  PSMC1DBR_PSMC1DBR2       : bit  absolute PSMC1DBR.2;
  PSMC1DBR_PSMC1DBR1       : bit  absolute PSMC1DBR.1;
  PSMC1DBR_PSMC1DBR0       : bit  absolute PSMC1DBR.0;
  PSMC1DBF                 : byte absolute $082A;
  PSMC1DBF_PSMC1DBF7       : bit  absolute PSMC1DBF.7;
  PSMC1DBF_PSMC1DBF6       : bit  absolute PSMC1DBF.6;
  PSMC1DBF_PSMC1DBF5       : bit  absolute PSMC1DBF.5;
  PSMC1DBF_PSMC1DBF4       : bit  absolute PSMC1DBF.4;
  PSMC1DBF_PSMC1DBF3       : bit  absolute PSMC1DBF.3;
  PSMC1DBF_PSMC1DBF2       : bit  absolute PSMC1DBF.2;
  PSMC1DBF_PSMC1DBF1       : bit  absolute PSMC1DBF.1;
  PSMC1DBF_PSMC1DBF0       : bit  absolute PSMC1DBF.0;
  PSMC1BLKR                : byte absolute $082B;
  PSMC1BLKR_PSMC1BLKR7     : bit  absolute PSMC1BLKR.7;
  PSMC1BLKR_PSMC1BLKR6     : bit  absolute PSMC1BLKR.6;
  PSMC1BLKR_PSMC1BLKR5     : bit  absolute PSMC1BLKR.5;
  PSMC1BLKR_PSMC1BLKR4     : bit  absolute PSMC1BLKR.4;
  PSMC1BLKR_PSMC1BLKR3     : bit  absolute PSMC1BLKR.3;
  PSMC1BLKR_PSMC1BLKR2     : bit  absolute PSMC1BLKR.2;
  PSMC1BLKR_PSMC1BLKR1     : bit  absolute PSMC1BLKR.1;
  PSMC1BLKR_PSMC1BLKR0     : bit  absolute PSMC1BLKR.0;
  PSMC1BLKF                : byte absolute $082C;
  PSMC1BLKF_PSMC1BLKF7     : bit  absolute PSMC1BLKF.7;
  PSMC1BLKF_PSMC1BLKF6     : bit  absolute PSMC1BLKF.6;
  PSMC1BLKF_PSMC1BLKF5     : bit  absolute PSMC1BLKF.5;
  PSMC1BLKF_PSMC1BLKF4     : bit  absolute PSMC1BLKF.4;
  PSMC1BLKF_PSMC1BLKF3     : bit  absolute PSMC1BLKF.3;
  PSMC1BLKF_PSMC1BLKF2     : bit  absolute PSMC1BLKF.2;
  PSMC1BLKF_PSMC1BLKF1     : bit  absolute PSMC1BLKF.1;
  PSMC1BLKF_PSMC1BLKF0     : bit  absolute PSMC1BLKF.0;
  PSMC1FFA                 : byte absolute $082D;
  PSMC1FFA_PSMC1FFA3       : bit  absolute PSMC1FFA.3;
  PSMC1FFA_PSMC1FFA2       : bit  absolute PSMC1FFA.2;
  PSMC1FFA_PSMC1FFA1       : bit  absolute PSMC1FFA.1;
  PSMC1FFA_PSMC1FFA0       : bit  absolute PSMC1FFA.0;
  PSMC1STR0                : byte absolute $082E;
  PSMC1STR0_P1STRF         : bit  absolute PSMC1STR0.5;
  PSMC1STR0_P1STRE         : bit  absolute PSMC1STR0.4;
  PSMC1STR0_P1STRD         : bit  absolute PSMC1STR0.3;
  PSMC1STR0_P1STRC         : bit  absolute PSMC1STR0.2;
  PSMC1STR0_P1STRB         : bit  absolute PSMC1STR0.1;
  PSMC1STR0_P1STRA         : bit  absolute PSMC1STR0.0;
  PSMC1STR1                : byte absolute $082F;
  PSMC1STR1_P1SSYNC        : bit  absolute PSMC1STR1.7;
  PSMC1STR1_P1LSMEN        : bit  absolute PSMC1STR1.1;
  PSMC1STR1_P1HSMEN        : bit  absolute PSMC1STR1.0;
  PSMC2CON                 : byte absolute $0831;
  PSMC2CON_PSMC2EN         : bit  absolute PSMC2CON.7;
  PSMC2CON_PSMC2LD         : bit  absolute PSMC2CON.6;
  PSMC2CON_P2DBFE          : bit  absolute PSMC2CON.5;
  PSMC2CON_P2DBRE          : bit  absolute PSMC2CON.4;
  PSMC2CON_P2MODE3         : bit  absolute PSMC2CON.3;
  PSMC2CON_P2MODE2         : bit  absolute PSMC2CON.2;
  PSMC2CON_P2MODE1         : bit  absolute PSMC2CON.1;
  PSMC2CON_P2MODE0         : bit  absolute PSMC2CON.0;
  PSMC2MDL                 : byte absolute $0832;
  PSMC2MDL_P2MDLEN         : bit  absolute PSMC2MDL.7;
  PSMC2MDL_P2MDLPOL        : bit  absolute PSMC2MDL.6;
  PSMC2MDL_P2MDLBIT        : bit  absolute PSMC2MDL.5;
  PSMC2MDL_P2MSRC3         : bit  absolute PSMC2MDL.3;
  PSMC2MDL_P2MSRC2         : bit  absolute PSMC2MDL.2;
  PSMC2MDL_P2MSRC1         : bit  absolute PSMC2MDL.1;
  PSMC2MDL_P2MSRC0         : bit  absolute PSMC2MDL.0;
  PSMC2SYNC                : byte absolute $0833;
  PSMC2SYNC_P2SYNC1        : bit  absolute PSMC2SYNC.1;
  PSMC2SYNC_P2SYNC0        : bit  absolute PSMC2SYNC.0;
  PSMC2CLK                 : byte absolute $0834;
  PSMC2CLK_P2CPRE1         : bit  absolute PSMC2CLK.5;
  PSMC2CLK_P2CPRE0         : bit  absolute PSMC2CLK.4;
  PSMC2CLK_P2CSRC1         : bit  absolute PSMC2CLK.1;
  PSMC2CLK_P2CSRC0         : bit  absolute PSMC2CLK.0;
  PSMC2OEN                 : byte absolute $0835;
  PSMC2OEN_P2OEB           : bit  absolute PSMC2OEN.1;
  PSMC2OEN_P2OEA           : bit  absolute PSMC2OEN.0;
  PSMC2POL                 : byte absolute $0836;
  PSMC2POL_P2INPOL         : bit  absolute PSMC2POL.6;
  PSMC2POL_P2POLB          : bit  absolute PSMC2POL.1;
  PSMC2POL_P2POLA          : bit  absolute PSMC2POL.0;
  PSMC2BLNK                : byte absolute $0837;
  PSMC2BLNK_P2FEBM1        : bit  absolute PSMC2BLNK.5;
  PSMC2BLNK_P2FEBM0        : bit  absolute PSMC2BLNK.4;
  PSMC2BLNK_P2REBM1        : bit  absolute PSMC2BLNK.1;
  PSMC2BLNK_P2REBM0        : bit  absolute PSMC2BLNK.0;
  PSMC2REBS                : byte absolute $0838;
  PSMC2REBS_P2REBSIN       : bit  absolute PSMC2REBS.7;
  PSMC2REBS_P2REBSC3       : bit  absolute PSMC2REBS.3;
  PSMC2REBS_P2REBSC2       : bit  absolute PSMC2REBS.2;
  PSMC2REBS_P2REBSC1       : bit  absolute PSMC2REBS.1;
  PSMC2FEBS                : byte absolute $0839;
  PSMC2FEBS_P2FEBSIN       : bit  absolute PSMC2FEBS.7;
  PSMC2FEBS_P2FEBSC3       : bit  absolute PSMC2FEBS.3;
  PSMC2FEBS_P2FEBSC2       : bit  absolute PSMC2FEBS.2;
  PSMC2FEBS_P2FEBSC1       : bit  absolute PSMC2FEBS.1;
  PSMC2PHS                 : byte absolute $083A;
  PSMC2PHS_P2PHSIN         : bit  absolute PSMC2PHS.7;
  PSMC2PHS_P2PHSC3         : bit  absolute PSMC2PHS.3;
  PSMC2PHS_P2PHSC2         : bit  absolute PSMC2PHS.2;
  PSMC2PHS_P2PHSC1         : bit  absolute PSMC2PHS.1;
  PSMC2PHS_P2PHST          : bit  absolute PSMC2PHS.0;
  PSMC2DCS                 : byte absolute $083B;
  PSMC2DCS_P2DCSIN         : bit  absolute PSMC2DCS.7;
  PSMC2DCS_P2DCSC3         : bit  absolute PSMC2DCS.3;
  PSMC2DCS_P2DCSC2         : bit  absolute PSMC2DCS.2;
  PSMC2DCS_P2DCSC1         : bit  absolute PSMC2DCS.1;
  PSMC2DCS_P2DCST          : bit  absolute PSMC2DCS.0;
  PSMC2PRS                 : byte absolute $083C;
  PSMC2PRS_P2PRSIN         : bit  absolute PSMC2PRS.7;
  PSMC2PRS_P2PRSC3         : bit  absolute PSMC2PRS.3;
  PSMC2PRS_P2PRSC2         : bit  absolute PSMC2PRS.2;
  PSMC2PRS_P2PRSC1         : bit  absolute PSMC2PRS.1;
  PSMC2PRS_P2PRST          : bit  absolute PSMC2PRS.0;
  PSMC2ASDC                : byte absolute $083D;
  PSMC2ASDC_P2ASE          : bit  absolute PSMC2ASDC.7;
  PSMC2ASDC_P2ASDEN        : bit  absolute PSMC2ASDC.6;
  PSMC2ASDC_P2ARSEN        : bit  absolute PSMC2ASDC.5;
  PSMC2ASDC_P2ASDOV        : bit  absolute PSMC2ASDC.0;
  PSMC2ASDL                : byte absolute $083E;
  PSMC2ASDL_P2ASDLB        : bit  absolute PSMC2ASDL.1;
  PSMC2ASDL_P2ASDLA        : bit  absolute PSMC2ASDL.0;
  PSMC2ASDS                : byte absolute $083F;
  PSMC2ASDS_P2ASDSIN       : bit  absolute PSMC2ASDS.7;
  PSMC2ASDS_P2ASDSC3       : bit  absolute PSMC2ASDS.3;
  PSMC2ASDS_P2ASDSC2       : bit  absolute PSMC2ASDS.2;
  PSMC2ASDS_P2ASDSC1       : bit  absolute PSMC2ASDS.1;
  PSMC2INT                 : byte absolute $0840;
  PSMC2INT_P2TOVIE         : bit  absolute PSMC2INT.7;
  PSMC2INT_P2TPHIE         : bit  absolute PSMC2INT.6;
  PSMC2INT_P2TDCIE         : bit  absolute PSMC2INT.5;
  PSMC2INT_P2TPRIE         : bit  absolute PSMC2INT.4;
  PSMC2INT_P2TOVIF         : bit  absolute PSMC2INT.3;
  PSMC2INT_P2TPHIF         : bit  absolute PSMC2INT.2;
  PSMC2INT_P2TDCIF         : bit  absolute PSMC2INT.1;
  PSMC2INT_P2TPRIF         : bit  absolute PSMC2INT.0;
  PSMC2PHL                 : byte absolute $0841;
  PSMC2PHL_PSMC2PH7        : bit  absolute PSMC2PHL.7;
  PSMC2PHL_PSMC2PH6        : bit  absolute PSMC2PHL.6;
  PSMC2PHL_PSMC2PH5        : bit  absolute PSMC2PHL.5;
  PSMC2PHL_PSMC2PH4        : bit  absolute PSMC2PHL.4;
  PSMC2PHL_PSMC2PH3        : bit  absolute PSMC2PHL.3;
  PSMC2PHL_PSMC2PH2        : bit  absolute PSMC2PHL.2;
  PSMC2PHL_PSMC2PH1        : bit  absolute PSMC2PHL.1;
  PSMC2PHL_PSMC2PH0        : bit  absolute PSMC2PHL.0;
  PSMC2PHH                 : byte absolute $0842;
  PSMC2PHH_PSMC2PH15       : bit  absolute PSMC2PHH.7;
  PSMC2PHH_PSMC2PH14       : bit  absolute PSMC2PHH.6;
  PSMC2PHH_PSMC2PH13       : bit  absolute PSMC2PHH.5;
  PSMC2PHH_PSMC2PH12       : bit  absolute PSMC2PHH.4;
  PSMC2PHH_PSMC2PH11       : bit  absolute PSMC2PHH.3;
  PSMC2PHH_PSMC2PH10       : bit  absolute PSMC2PHH.2;
  PSMC2PHH_PSMC2PH9        : bit  absolute PSMC2PHH.1;
  PSMC2PHH_PSMC2PH8        : bit  absolute PSMC2PHH.0;
  PSMC2DCL                 : byte absolute $0843;
  PSMC2DCL_PSMC2DC7        : bit  absolute PSMC2DCL.7;
  PSMC2DCL_PSMC2DC6        : bit  absolute PSMC2DCL.6;
  PSMC2DCL_PSMC2DC5        : bit  absolute PSMC2DCL.5;
  PSMC2DCL_PSMC2DC4        : bit  absolute PSMC2DCL.4;
  PSMC2DCL_PSMC2DC3        : bit  absolute PSMC2DCL.3;
  PSMC2DCL_PSMC2DC2        : bit  absolute PSMC2DCL.2;
  PSMC2DCL_PSMC2DC1        : bit  absolute PSMC2DCL.1;
  PSMC2DCL_PSMC2DC0        : bit  absolute PSMC2DCL.0;
  PSMC2DCH                 : byte absolute $0844;
  PSMC2DCH_PSMC2DC15       : bit  absolute PSMC2DCH.7;
  PSMC2DCH_PSMC2DC14       : bit  absolute PSMC2DCH.6;
  PSMC2DCH_PSMC2DC13       : bit  absolute PSMC2DCH.5;
  PSMC2DCH_PSMC2DC12       : bit  absolute PSMC2DCH.4;
  PSMC2DCH_PSMC2DC11       : bit  absolute PSMC2DCH.3;
  PSMC2DCH_PSMC2DC10       : bit  absolute PSMC2DCH.2;
  PSMC2DCH_PSMC2DC9        : bit  absolute PSMC2DCH.1;
  PSMC2DCH_PSMC2DC8        : bit  absolute PSMC2DCH.0;
  PSMC2PRL                 : byte absolute $0845;
  PSMC2PRL_PSMC2PR7        : bit  absolute PSMC2PRL.7;
  PSMC2PRL_PSMC2PR6        : bit  absolute PSMC2PRL.6;
  PSMC2PRL_PSMC2PR5        : bit  absolute PSMC2PRL.5;
  PSMC2PRL_PSMC2PR4        : bit  absolute PSMC2PRL.4;
  PSMC2PRL_PSMC2PR3        : bit  absolute PSMC2PRL.3;
  PSMC2PRL_PSMC2PR2        : bit  absolute PSMC2PRL.2;
  PSMC2PRL_PSMC2PR1        : bit  absolute PSMC2PRL.1;
  PSMC2PRL_PSMC2PR0        : bit  absolute PSMC2PRL.0;
  PSMC2PRH                 : byte absolute $0846;
  PSMC2PRH_PSMC2PR15       : bit  absolute PSMC2PRH.7;
  PSMC2PRH_PSMC2PR14       : bit  absolute PSMC2PRH.6;
  PSMC2PRH_PSMC2PR13       : bit  absolute PSMC2PRH.5;
  PSMC2PRH_PSMC2PR12       : bit  absolute PSMC2PRH.4;
  PSMC2PRH_PSMC2PR11       : bit  absolute PSMC2PRH.3;
  PSMC2PRH_PSMC2PR10       : bit  absolute PSMC2PRH.2;
  PSMC2PRH_PSMC2PR9        : bit  absolute PSMC2PRH.1;
  PSMC2PRH_PSMC2PR8        : bit  absolute PSMC2PRH.0;
  PSMC2TMRL                : byte absolute $0847;
  PSMC2TMRL_PSMC2TMR7      : bit  absolute PSMC2TMRL.7;
  PSMC2TMRL_PSMC2TMR6      : bit  absolute PSMC2TMRL.6;
  PSMC2TMRL_PSMC2TMR5      : bit  absolute PSMC2TMRL.5;
  PSMC2TMRL_PSMC2TMR4      : bit  absolute PSMC2TMRL.4;
  PSMC2TMRL_PSMC2TMR3      : bit  absolute PSMC2TMRL.3;
  PSMC2TMRL_PSMC2TMR2      : bit  absolute PSMC2TMRL.2;
  PSMC2TMRL_PSMC2TMR1      : bit  absolute PSMC2TMRL.1;
  PSMC2TMRL_PSMC2TMR0      : bit  absolute PSMC2TMRL.0;
  PSMC2TMRH                : byte absolute $0848;
  PSMC2TMRH_PSMC2TMR15     : bit  absolute PSMC2TMRH.7;
  PSMC2TMRH_PSMC2TMR14     : bit  absolute PSMC2TMRH.6;
  PSMC2TMRH_PSMC2TMR13     : bit  absolute PSMC2TMRH.5;
  PSMC2TMRH_PSMC2TMR12     : bit  absolute PSMC2TMRH.4;
  PSMC2TMRH_PSMC2TMR11     : bit  absolute PSMC2TMRH.3;
  PSMC2TMRH_PSMC2TMR10     : bit  absolute PSMC2TMRH.2;
  PSMC2TMRH_PSMC2TMR9      : bit  absolute PSMC2TMRH.1;
  PSMC2TMRH_PSMC2TMR8      : bit  absolute PSMC2TMRH.0;
  PSMC2DBR                 : byte absolute $0849;
  PSMC2DBR_PSMC2DBR7       : bit  absolute PSMC2DBR.7;
  PSMC2DBR_PSMC2DBR6       : bit  absolute PSMC2DBR.6;
  PSMC2DBR_PSMC2DBR5       : bit  absolute PSMC2DBR.5;
  PSMC2DBR_PSMC2DBR4       : bit  absolute PSMC2DBR.4;
  PSMC2DBR_PSMC2DBR3       : bit  absolute PSMC2DBR.3;
  PSMC2DBR_PSMC2DBR2       : bit  absolute PSMC2DBR.2;
  PSMC2DBR_PSMC2DBR1       : bit  absolute PSMC2DBR.1;
  PSMC2DBR_PSMC2DBR0       : bit  absolute PSMC2DBR.0;
  PSMC2DBF                 : byte absolute $084A;
  PSMC2DBF_PSMC2DBF7       : bit  absolute PSMC2DBF.7;
  PSMC2DBF_PSMC2DBF6       : bit  absolute PSMC2DBF.6;
  PSMC2DBF_PSMC2DBF5       : bit  absolute PSMC2DBF.5;
  PSMC2DBF_PSMC2DBF4       : bit  absolute PSMC2DBF.4;
  PSMC2DBF_PSMC2DBF3       : bit  absolute PSMC2DBF.3;
  PSMC2DBF_PSMC2DBF2       : bit  absolute PSMC2DBF.2;
  PSMC2DBF_PSMC2DBF1       : bit  absolute PSMC2DBF.1;
  PSMC2DBF_PSMC2DBF0       : bit  absolute PSMC2DBF.0;
  PSMC2BLKR                : byte absolute $084B;
  PSMC2BLKR_PSMC2BLKR7     : bit  absolute PSMC2BLKR.7;
  PSMC2BLKR_PSMC2BLKR6     : bit  absolute PSMC2BLKR.6;
  PSMC2BLKR_PSMC2BLKR5     : bit  absolute PSMC2BLKR.5;
  PSMC2BLKR_PSMC2BLKR4     : bit  absolute PSMC2BLKR.4;
  PSMC2BLKR_PSMC2BLKR3     : bit  absolute PSMC2BLKR.3;
  PSMC2BLKR_PSMC2BLKR2     : bit  absolute PSMC2BLKR.2;
  PSMC2BLKR_PSMC2BLKR1     : bit  absolute PSMC2BLKR.1;
  PSMC2BLKR_PSMC2BLKR0     : bit  absolute PSMC2BLKR.0;
  PSMC2BLKF                : byte absolute $084C;
  PSMC2BLKF_PSMC2BLKF7     : bit  absolute PSMC2BLKF.7;
  PSMC2BLKF_PSMC2BLKF6     : bit  absolute PSMC2BLKF.6;
  PSMC2BLKF_PSMC2BLKF5     : bit  absolute PSMC2BLKF.5;
  PSMC2BLKF_PSMC2BLKF4     : bit  absolute PSMC2BLKF.4;
  PSMC2BLKF_PSMC2BLKF3     : bit  absolute PSMC2BLKF.3;
  PSMC2BLKF_PSMC2BLKF2     : bit  absolute PSMC2BLKF.2;
  PSMC2BLKF_PSMC2BLKF1     : bit  absolute PSMC2BLKF.1;
  PSMC2BLKF_PSMC2BLKF0     : bit  absolute PSMC2BLKF.0;
  PSMC2FFA                 : byte absolute $084D;
  PSMC2FFA_PSMC2FFA3       : bit  absolute PSMC2FFA.3;
  PSMC2FFA_PSMC2FFA2       : bit  absolute PSMC2FFA.2;
  PSMC2FFA_PSMC2FFA1       : bit  absolute PSMC2FFA.1;
  PSMC2FFA_PSMC2FFA0       : bit  absolute PSMC2FFA.0;
  PSMC2STR0                : byte absolute $084E;
  PSMC2STR0_P2STRB         : bit  absolute PSMC2STR0.1;
  PSMC2STR0_P2STRA         : bit  absolute PSMC2STR0.0;
  PSMC2STR1                : byte absolute $084F;
  PSMC2STR1_P2SSYNC        : bit  absolute PSMC2STR1.7;
  PSMC2STR1_P2LSMEN        : bit  absolute PSMC2STR1.1;
  PSMC2STR1_P2HSMEN        : bit  absolute PSMC2STR1.0;
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
  {$SET_STATE_RAM '010-012:SFR'}            // Bank 0 : PORTE, PIR1, PIR2
  {$SET_STATE_RAM '014-01C:SFR'}            // Bank 0 : PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08E:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '090-092:SFR'}            // Bank 1 : TRISE, PIE1, PIE2
  {$SET_STATE_RAM '094-09F:SFR'}            // Bank 1 : PIE4, OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10E:SFR'}            // Bank 2 : LATA, LATB, LATC
  {$SET_STATE_RAM '111-119:SFR'}            // Bank 2 : CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '11D-11F:SFR'}            // Bank 2 : APFCON, CM3CON0, CM3CON1
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18D:SFR'}            // Bank 3 : ANSELA, ANSELB
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : EEADRL, EEADRH, EEDATL, EEDATH, EECON1, EECON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20E:SFR'}            // Bank 4 : WPUA, WPUB, WPUC
  {$SET_STATE_RAM '210-217:SFR'}            // Bank 4 : WPUE, SSPBUF, SSPADD, SSPMSK, SSPSTAT, SSPCON1, SSPCON2, SSPCON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28E:SFR'}            // Bank 5 : ODCONA, ODCONB, ODCONC
  {$SET_STATE_RAM '291-293:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '298-29A:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30E:SFR'}            // Bank 6 : SLRCONA, SLRCONB, SLRCONC
  {$SET_STATE_RAM '320-32F:GPR'}           
  {$SET_STATE_RAM '38C-38E:SFR'}            // Bank 7 : INLVLA, INLVLB, INLVLC
  {$SET_STATE_RAM '390-399:SFR'}            // Bank 7 : INLVLE, IOCAP, IOCAN, IOCAF, IOCBP, IOCBN, IOCBF, IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '39D-39F:SFR'}            // Bank 7 : IOCEP, IOCEN, IOCEF
  {$SET_STATE_RAM '511-511:SFR'}            // Bank 10 : OPA1CON
  {$SET_STATE_RAM '513-513:SFR'}            // Bank 10 : OPA2CON
  {$SET_STATE_RAM '51A-51A:SFR'}            // Bank 10 : CLKRCON
  {$SET_STATE_RAM '811-82F:SFR'}            // Bank 16 : PSMC1CON, PSMC1MDL, PSMC1SYNC, PSMC1CLK, PSMC1OEN, PSMC1POL, PSMC1BLNK, PSMC1REBS, PSMC1FEBS, PSMC1PHS, PSMC1DCS, PSMC1PRS, PSMC1ASDC, PSMC1ASDL, PSMC1ASDS, PSMC1INT, PSMC1PHL, PSMC1PHH, PSMC1DCL, PSMC1DCH, PSMC1PRL, PSMC1PRH, PSMC1TMRL, PSMC1TMRH, PSMC1DBR, PSMC1DBF, PSMC1BLKR, PSMC1BLKF, PSMC1FFA, PSMC1STR0, PSMC1STR1
  {$SET_STATE_RAM '831-84F:SFR'}            // Bank 16 : PSMC2CON, PSMC2MDL, PSMC2SYNC, PSMC2CLK, PSMC2OEN, PSMC2POL, PSMC2BLNK, PSMC2REBS, PSMC2FEBS, PSMC2PHS, PSMC2DCS, PSMC2PRS, PSMC2ASDC, PSMC2ASDL, PSMC2ASDS, PSMC2INT, PSMC2PHL, PSMC2PHH, PSMC2DCL, PSMC2DCH, PSMC2PRL, PSMC2PRH, PSMC2TMRL, PSMC2TMRH, PSMC2DBR, PSMC2DBF, PSMC2BLKR, PSMC2BLKF, PSMC2FFA, PSMC2STR0, PSMC2STR1
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:08'} // PORTE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:FB'} // PIR2 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '014:33'} // PIR4 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:08'} // TRISE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:FB'} // PIE2 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '094:33'} // PIE4 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:07'} // CMOUT bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:BD'} // DACCON0 bits 6,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:BF'} // ANSELA bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:3F'} // ANSELB bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:08'} // WPUE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '390:08'} // INLVLE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:08'} // IOCEP bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:08'} // IOCEN bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:08'} // IOCEF bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '511:C3'} // OPA1CON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '513:C3'} // OPA2CON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '812:EF'} // PSMC1MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '813:03'} // PSMC1SYNC bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '814:33'} // PSMC1CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '815:3F'} // PSMC1OEN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '816:7F'} // PSMC1POL bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '817:33'} // PSMC1BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '818:8E'} // PSMC1REBS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '819:8E'} // PSMC1FEBS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81A:8F'} // PSMC1PHS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81B:8F'} // PSMC1DCS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81C:8F'} // PSMC1PRS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81D:E1'} // PSMC1ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81E:3F'} // PSMC1ASDL bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81F:8E'} // PSMC1ASDS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '82D:0F'} // PSMC1FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '82E:3F'} // PSMC1STR0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '82F:83'} // PSMC1STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '832:EF'} // PSMC2MDL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '833:03'} // PSMC2SYNC bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '834:33'} // PSMC2CLK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '835:03'} // PSMC2OEN bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '836:43'} // PSMC2POL bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '837:33'} // PSMC2BLNK bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '838:8E'} // PSMC2REBS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '839:8E'} // PSMC2FEBS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83A:8F'} // PSMC2PHS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83B:8F'} // PSMC2DCS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83C:8F'} // PSMC2PRS bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83D:E1'} // PSMC2ASDC bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83E:03'} // PSMC2ASDL bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '83F:8E'} // PSMC2ASDS bits 6,5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '84D:0F'} // PSMC2FFA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '84E:03'} // PSMC2STR0 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '84F:83'} // PSMC2STR1 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RE3/MCLR/Vpp
  // Pin  2 : RA0/AN0/C1IN0-/C2IN0-/C3IN0-
  // Pin  3 : RA1/AN1/C1IN1-/C2IN1-/C3IN1-/OPA1OUT
  // Pin  4 : RA2/AN2/C1IN0+/C2IN0+/C3IN0+/DAC1OUT1/Vref-/DAC1Vref-
  // Pin  5 : RA3/AN3/Vref+/C1IN1+/DAC1Vref+
  // Pin  6 : RA4/C1OUT/OPA1IN+/T0CKI
  // Pin  7 : RA5/AN4/C2OUT/OPA1IN-/SS
  // Pin  8 : Vss
  // Pin  9 : RA7/PSMC1CLK/PSMC2CLK/OSC1/CLKIN
  // Pin 10 : RA6/C2OUT/OSC2/CLKOUT/VCAP
  // Pin 11 : RC0/T1OSO/T1CKI/PSMC1A
  // Pin 12 : RC1/T1OSI/PSMC1B/CCP2
  // Pin 13 : RC2/CCP1/PSMC1C
  // Pin 14 : RC3/SCK/SCL/PSMC1D
  // Pin 15 : RC4/SDI/SDA/PSMC1E
  // Pin 16 : RC5/SDO/PSMC1F
  // Pin 17 : RC6/TX/CK/PSMC2A
  // Pin 18 : RC7/RX/DT/PSMC2B
  // Pin 19 : Vss
  // Pin 20 : Vdd
  // Pin 21 : RB0/AN12/C2IN1+/CCP1/PSMC1IN/PSMC2IN/INT
  // Pin 22 : RB1/AN10/C1IN3-/C2IN3-/C3IN3-/OPA2OUT
  // Pin 23 : RB2/AN8/OPA2IN-/CLKR
  // Pin 24 : RB3/AN9/C1IN2-/C2IN2-/C3IN2-/OPA2IN+/CCP2
  // Pin 25 : RB4/AN11/C3IN1+
  // Pin 26 : RB5/AN13/C3OUT/T1G/SDO
  // Pin 27 : RB6/TX/CK/SDI/SDA/ICSPCLK/ICDCLK
  // Pin 28 : RB7/SCK/RX/DT/SCL/DAC1OUT2/ICSPDAT/ICDDAT


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
