unit PIC16F1459;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1459'}
{$SET PIC_MAXFREQ  = 48000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 8192}

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
  PORTA_RA5                : bit  absolute PORTA.5;
  PORTA_RA4                : bit  absolute PORTA.4;
  PORTA_RA3                : bit  absolute PORTA.3;
  PORTA_RA1                : bit  absolute PORTA.1;
  PORTA_RA0                : bit  absolute PORTA.0;
  PORTB                    : byte absolute $000D;
  PORTB_RB7                : bit  absolute PORTB.7;
  PORTB_RB6                : bit  absolute PORTB.6;
  PORTB_RB5                : bit  absolute PORTB.5;
  PORTB_RB4                : bit  absolute PORTB.4;
  PORTC                    : byte absolute $000E;
  PORTC_RC7                : bit  absolute PORTC.7;
  PORTC_RC6                : bit  absolute PORTC.6;
  PORTC_RC5                : bit  absolute PORTC.5;
  PORTC_RC4                : bit  absolute PORTC.4;
  PORTC_RC3                : bit  absolute PORTC.3;
  PORTC_RC2                : bit  absolute PORTC.2;
  PORTC_RC1                : bit  absolute PORTC.1;
  PORTC_RC0                : bit  absolute PORTC.0;
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_RCIF                : bit  absolute PIR1.5;
  PIR1_TXIF                : bit  absolute PIR1.4;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_USBIF               : bit  absolute PIR2.2;
  PIR2_ACTIF               : bit  absolute PIR2.1;
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
  TRISA                    : byte absolute $008C;
  TRISA_TRISA5             : bit  absolute TRISA.5;
  TRISA_TRISA4             : bit  absolute TRISA.4;
  TRISB                    : byte absolute $008D;
  TRISB_TRISB7             : bit  absolute TRISB.7;
  TRISB_TRISB6             : bit  absolute TRISB.6;
  TRISB_TRISB5             : bit  absolute TRISB.5;
  TRISB_TRISB4             : bit  absolute TRISB.4;
  TRISC                    : byte absolute $008E;
  TRISC_TRISC7             : bit  absolute TRISC.7;
  TRISC_TRISC6             : bit  absolute TRISC.6;
  TRISC_TRISC5             : bit  absolute TRISC.5;
  TRISC_TRISC4             : bit  absolute TRISC.4;
  TRISC_TRISC3             : bit  absolute TRISC.3;
  TRISC_TRISC2             : bit  absolute TRISC.2;
  TRISC_TRISC1             : bit  absolute TRISC.1;
  TRISC_TRISC0             : bit  absolute TRISC.0;
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_RCIE                : bit  absolute PIE1.5;
  PIE1_TXIE                : bit  absolute PIE1.4;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_OSFIE               : bit  absolute PIE2.7;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_USBIE               : bit  absolute PIE2.2;
  PIE2_ACTIE               : bit  absolute PIE2.1;
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
  OSCTUNE_TUN6             : bit  absolute OSCTUNE.6;
  OSCTUNE_TUN5             : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4             : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3             : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2             : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1             : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0             : bit  absolute OSCTUNE.0;
  OSCCON                   : byte absolute $0099;
  OSCCON_SPLLEN            : bit  absolute OSCCON.7;
  OSCCON_SPLLMULT          : bit  absolute OSCCON.6;
  OSCCON_IRCF3             : bit  absolute OSCCON.5;
  OSCCON_IRCF2             : bit  absolute OSCCON.4;
  OSCCON_IRCF1             : bit  absolute OSCCON.3;
  OSCCON_IRCF0             : bit  absolute OSCCON.2;
  OSCCON_SCS1              : bit  absolute OSCCON.1;
  OSCCON_SCS0              : bit  absolute OSCCON.0;
  OSCSTAT                  : byte absolute $009A;
  OSCSTAT_SOSCR            : bit  absolute OSCSTAT.7;
  OSCSTAT_PLLRDY           : bit  absolute OSCSTAT.6;
  OSCSTAT_OSTS             : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFR           : bit  absolute OSCSTAT.4;
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
  ADCON1_ADPREF1           : bit  absolute ADCON1.1;
  ADCON1_ADPREF0           : bit  absolute ADCON1.0;
  ADCON2                   : byte absolute $009F;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.4;
  LATA                     : byte absolute $010C;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
  LATB                     : byte absolute $010D;
  LATB_LATB7               : bit  absolute LATB.7;
  LATB_LATB6               : bit  absolute LATB.6;
  LATB_LATB5               : bit  absolute LATB.5;
  LATB_LATB4               : bit  absolute LATB.4;
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
  CM1CON0_C1SP             : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS            : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC           : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0112;
  CM1CON1_C1INTP           : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN           : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH1           : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH0           : bit  absolute CM1CON1.4;
  CM1CON1_C1NCH2           : bit  absolute CM1CON1.2;
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
  CM2CON1_C2NCH2           : bit  absolute CM2CON1.2;
  CM2CON1_C2NCH1           : bit  absolute CM2CON1.1;
  CM2CON1_C2NCH0           : bit  absolute CM2CON1.0;
  CMOUT                    : byte absolute $0115;
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
  DACCON0_D1PSS1           : bit  absolute DACCON0.3;
  DACCON0_D1PSS0           : bit  absolute DACCON0.2;
  DACCON1                  : byte absolute $0119;
  DACCON1_DACR4            : bit  absolute DACCON1.4;
  DACCON1_DACR3            : bit  absolute DACCON1.3;
  DACCON1_DACR2            : bit  absolute DACCON1.2;
  DACCON1_DACR1            : bit  absolute DACCON1.1;
  DACCON1_DACR0            : bit  absolute DACCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_CLKRSEL           : bit  absolute APFCON.7;
  APFCON_SSSEL             : bit  absolute APFCON.5;
  APFCON_T1GSEL            : bit  absolute APFCON.3;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELB                   : byte absolute $018D;
  ANSELB_ANSB5             : bit  absolute ANSELB.5;
  ANSELB_ANSB4             : bit  absolute ANSELB.4;
  ANSELC                   : byte absolute $018E;
  ANSELC_ANSC7             : bit  absolute ANSELC.7;
  ANSELC_ANSC6             : bit  absolute ANSELC.6;
  ANSELC_ANSC3             : bit  absolute ANSELC.3;
  ANSELC_ANSC2             : bit  absolute ANSELC.2;
  ANSELC_ANSC1             : bit  absolute ANSELC.1;
  ANSELC_ANSC0             : bit  absolute ANSELC.0;
  PMADRL                   : byte absolute $0191;
  PMADRH                   : byte absolute $0192;
  PMADRH_PMADRH6           : bit  absolute PMADRH.6;
  PMADRH_PMADRH5           : bit  absolute PMADRH.5;
  PMADRH_PMADRH4           : bit  absolute PMADRH.4;
  PMADRH_PMADRH3           : bit  absolute PMADRH.3;
  PMADRH_PMADRH2           : bit  absolute PMADRH.2;
  PMADRH_PMADRH1           : bit  absolute PMADRH.1;
  PMADRH_PMADRH0           : bit  absolute PMADRH.0;
  PMDATL                   : byte absolute $0193;
  PMDATH                   : byte absolute $0194;
  PMDATH_PMDATH5           : bit  absolute PMDATH.5;
  PMDATH_PMDATH4           : bit  absolute PMDATH.4;
  PMDATH_PMDATH3           : bit  absolute PMDATH.3;
  PMDATH_PMDATH2           : bit  absolute PMDATH.2;
  PMDATH_PMDATH1           : bit  absolute PMDATH.1;
  PMDATH_PMDATH0           : bit  absolute PMDATH.0;
  PMCON1                   : byte absolute $0195;
  PMCON1_CFGS              : bit  absolute PMCON1.6;
  PMCON1_LWLO              : bit  absolute PMCON1.5;
  PMCON1_FREE              : bit  absolute PMCON1.4;
  PMCON1_WRERR             : bit  absolute PMCON1.3;
  PMCON1_WREN              : bit  absolute PMCON1.2;
  PMCON1_WR                : bit  absolute PMCON1.1;
  PMCON1_RD                : bit  absolute PMCON1.0;
  PMCON2                   : byte absolute $0196;
  VREGCON                  : byte absolute $0197;
  VREGCON_VREGPM1          : bit  absolute VREGCON.1;
  VREGCON_VREGPM0          : bit  absolute VREGCON.0;
  RCREG                    : byte absolute $0199;
  TXREG                    : byte absolute $019A;
  SPBRGL                   : byte absolute $019B;
  SPBRGH                   : byte absolute $019C;
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
  WPUA                     : byte absolute $020C;
  WPUA_WPUA5               : bit  absolute WPUA.5;
  WPUA_WPUA4               : bit  absolute WPUA.4;
  WPUA_WPUA3               : bit  absolute WPUA.3;
  WPUB                     : byte absolute $020D;
  WPUB_WPUB7               : bit  absolute WPUB.7;
  WPUB_WPUB6               : bit  absolute WPUB.6;
  WPUB_WPUB5               : bit  absolute WPUB.5;
  WPUB_WPUB4               : bit  absolute WPUB.4;
  SSP1BUF                  : byte absolute $0211;
  SSP1ADD                  : byte absolute $0212;
  SSP1MSK                  : byte absolute $0213;
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
  SSP1CON1_SSP1M3          : bit  absolute SSP1CON1.3;
  SSP1CON1_SSP1M2          : bit  absolute SSP1CON1.2;
  SSP1CON1_SSP1M1          : bit  absolute SSP1CON1.1;
  SSP1CON1_SSP1M0          : bit  absolute SSP1CON1.0;
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
  IOCAP                    : byte absolute $0391;
  IOCAP_IOCAP5             : bit  absolute IOCAP.5;
  IOCAP_IOCAP4             : bit  absolute IOCAP.4;
  IOCAP_IOCAP3             : bit  absolute IOCAP.3;
  IOCAP_IOCAP1             : bit  absolute IOCAP.1;
  IOCAP_IOCAP0             : bit  absolute IOCAP.0;
  IOCAN                    : byte absolute $0392;
  IOCAN_IOCAN5             : bit  absolute IOCAN.5;
  IOCAN_IOCAN4             : bit  absolute IOCAN.4;
  IOCAN_IOCAN3             : bit  absolute IOCAN.3;
  IOCAN_IOCAN1             : bit  absolute IOCAN.1;
  IOCAN_IOCAN0             : bit  absolute IOCAN.0;
  IOCAF                    : byte absolute $0393;
  IOCAF_IOCAF5             : bit  absolute IOCAF.5;
  IOCAF_IOCAF4             : bit  absolute IOCAF.4;
  IOCAF_IOCAF3             : bit  absolute IOCAF.3;
  IOCAF_IOCAF1             : bit  absolute IOCAF.1;
  IOCAF_IOCAF0             : bit  absolute IOCAF.0;
  IOCBP                    : byte absolute $0394;
  IOCBP_IOCBP7             : bit  absolute IOCBP.7;
  IOCBP_IOCBP6             : bit  absolute IOCBP.6;
  IOCBP_IOCBP5             : bit  absolute IOCBP.5;
  IOCBP_IOCBP4             : bit  absolute IOCBP.4;
  IOCBN                    : byte absolute $0395;
  IOCBN_IOCBN7             : bit  absolute IOCBN.7;
  IOCBN_IOCBN6             : bit  absolute IOCBN.6;
  IOCBN_IOCBN5             : bit  absolute IOCBN.5;
  IOCBN_IOCBN4             : bit  absolute IOCBN.4;
  IOCBF                    : byte absolute $0396;
  IOCBF_IOCBF7             : bit  absolute IOCBF.7;
  IOCBF_IOCBF6             : bit  absolute IOCBF.6;
  IOCBF_IOCBF5             : bit  absolute IOCBF.5;
  IOCBF_IOCBF4             : bit  absolute IOCBF.4;
  CLKRCON                  : byte absolute $039A;
  CLKRCON_CLKREN           : bit  absolute CLKRCON.7;
  CLKRCON_CLKROE           : bit  absolute CLKRCON.6;
  CLKRCON_CLKRSLR          : bit  absolute CLKRCON.5;
  CLKRCON_CLKRCD1          : bit  absolute CLKRCON.4;
  CLKRCON_CLKRCD0          : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2         : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1         : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0         : bit  absolute CLKRCON.0;
  ACTCON                   : byte absolute $039B;
  ACTCON_ACTEN             : bit  absolute ACTCON.7;
  ACTCON_ACTUD             : bit  absolute ACTCON.6;
  ACTCON_ACTSRC            : bit  absolute ACTCON.4;
  ACTCON_ACTLOCK           : bit  absolute ACTCON.3;
  ACTCON_ACTORS            : bit  absolute ACTCON.1;
  PWM1DCL                  : byte absolute $0611;
  PWM1DCL_PWM1DCL1         : bit  absolute PWM1DCL.7;
  PWM1DCL_PWM1DCL0         : bit  absolute PWM1DCL.6;
  PWM1DCH                  : byte absolute $0612;
  PWM1CON                  : byte absolute $0613;
  PWM1CON_PWM1EN           : bit  absolute PWM1CON.7;
  PWM1CON_PWM1OE           : bit  absolute PWM1CON.6;
  PWM1CON_PWM1OUT          : bit  absolute PWM1CON.5;
  PWM1CON_PWM1POL          : bit  absolute PWM1CON.4;
  PWM2DCL                  : byte absolute $0614;
  PWM2DCL_PWM2DCL1         : bit  absolute PWM2DCL.7;
  PWM2DCL_PWM2DCL0         : bit  absolute PWM2DCL.6;
  PWM2DCH                  : byte absolute $0615;
  PWM2CON                  : byte absolute $0616;
  PWM2CON_PWM2EN           : bit  absolute PWM2CON.7;
  PWM2CON_PWM2OE           : bit  absolute PWM2CON.6;
  PWM2CON_PWM2OUT          : bit  absolute PWM2CON.5;
  PWM2CON_PWM2POL          : bit  absolute PWM2CON.4;
  CWG1DBR                  : byte absolute $0691;
  CWG1DBR_CWG1DBR5         : bit  absolute CWG1DBR.5;
  CWG1DBR_CWG1DBR4         : bit  absolute CWG1DBR.4;
  CWG1DBR_CWG1DBR3         : bit  absolute CWG1DBR.3;
  CWG1DBR_CWG1DBR2         : bit  absolute CWG1DBR.2;
  CWG1DBR_CWG1DBR1         : bit  absolute CWG1DBR.1;
  CWG1DBR_CWG1DBR0         : bit  absolute CWG1DBR.0;
  CWG1DBF                  : byte absolute $0692;
  CWG1DBF_CWG1DBF5         : bit  absolute CWG1DBF.5;
  CWG1DBF_CWG1DBF4         : bit  absolute CWG1DBF.4;
  CWG1DBF_CWG1DBF3         : bit  absolute CWG1DBF.3;
  CWG1DBF_CWG1DBF2         : bit  absolute CWG1DBF.2;
  CWG1DBF_CWG1DBF1         : bit  absolute CWG1DBF.1;
  CWG1DBF_CWG1DBF0         : bit  absolute CWG1DBF.0;
  CWG1CON0                 : byte absolute $0693;
  CWG1CON0_G1EN            : bit  absolute CWG1CON0.7;
  CWG1CON0_G1OEB           : bit  absolute CWG1CON0.6;
  CWG1CON0_G1OEA           : bit  absolute CWG1CON0.5;
  CWG1CON0_G1POLB          : bit  absolute CWG1CON0.4;
  CWG1CON0_G1POLA          : bit  absolute CWG1CON0.3;
  CWG1CON0_G1CS0           : bit  absolute CWG1CON0.0;
  CWG1CON1                 : byte absolute $0694;
  CWG1CON1_G1ASDLB1        : bit  absolute CWG1CON1.7;
  CWG1CON1_G1ASDLB0        : bit  absolute CWG1CON1.6;
  CWG1CON1_G1ASDLA1        : bit  absolute CWG1CON1.5;
  CWG1CON1_G1ASDLA0        : bit  absolute CWG1CON1.4;
  CWG1CON1_G1IS1           : bit  absolute CWG1CON1.1;
  CWG1CON1_G1IS0           : bit  absolute CWG1CON1.0;
  CWG1CON2                 : byte absolute $0695;
  CWG1CON2_G1ASE           : bit  absolute CWG1CON2.7;
  CWG1CON2_G1ARSEN         : bit  absolute CWG1CON2.6;
  CWG1CON2_G1ASDSC2        : bit  absolute CWG1CON2.3;
  CWG1CON2_G1ASDSC1        : bit  absolute CWG1CON2.2;
  CWG1CON2_G1ASDSFLT       : bit  absolute CWG1CON2.1;
  UCON                     : byte absolute $0E8E;
  UCON_PPBRST              : bit  absolute UCON.6;
  UCON_SE0                 : bit  absolute UCON.5;
  UCON_PKTDIS              : bit  absolute UCON.4;
  UCON_USBEN               : bit  absolute UCON.3;
  UCON_RESUME              : bit  absolute UCON.2;
  UCON_SUSPND              : bit  absolute UCON.1;
  USTAT                    : byte absolute $0E8F;
  USTAT_ENDP3              : bit  absolute USTAT.6;
  USTAT_ENDP2              : bit  absolute USTAT.5;
  USTAT_ENDP1              : bit  absolute USTAT.4;
  USTAT_ENDP0              : bit  absolute USTAT.3;
  USTAT_DIR                : bit  absolute USTAT.2;
  USTAT_PPBI               : bit  absolute USTAT.1;
  UIR                      : byte absolute $0E90;
  UIR_SOFIF                : bit  absolute UIR.6;
  UIR_STALLIF              : bit  absolute UIR.5;
  UIR_IDLEIF               : bit  absolute UIR.4;
  UIR_TRNIF                : bit  absolute UIR.3;
  UIR_ACTVIF               : bit  absolute UIR.2;
  UIR_UERRIF               : bit  absolute UIR.1;
  UIR_URSTIF               : bit  absolute UIR.0;
  UCFG                     : byte absolute $0E91;
  UCFG_UTEYE               : bit  absolute UCFG.7;
  UCFG_UPUEN               : bit  absolute UCFG.4;
  UCFG_FSEN                : bit  absolute UCFG.2;
  UCFG_PPB1                : bit  absolute UCFG.1;
  UCFG_PPB0                : bit  absolute UCFG.0;
  UIE                      : byte absolute $0E92;
  UIE_SOFIE                : bit  absolute UIE.6;
  UIE_STALLIE              : bit  absolute UIE.5;
  UIE_IDLEIE               : bit  absolute UIE.4;
  UIE_TRNIE                : bit  absolute UIE.3;
  UIE_ACTVIE               : bit  absolute UIE.2;
  UIE_UERRIE               : bit  absolute UIE.1;
  UIE_URSTIE               : bit  absolute UIE.0;
  UEIR                     : byte absolute $0E93;
  UEIR_BTSEF               : bit  absolute UEIR.7;
  UEIR_BTOEF               : bit  absolute UEIR.4;
  UEIR_DFN8EF              : bit  absolute UEIR.3;
  UEIR_CRC16EF             : bit  absolute UEIR.2;
  UEIR_CRC5EF              : bit  absolute UEIR.1;
  UEIR_PIDEF               : bit  absolute UEIR.0;
  UFRMH                    : byte absolute $0E94;
  UFRMH_FRM10              : bit  absolute UFRMH.2;
  UFRMH_FRM9               : bit  absolute UFRMH.1;
  UFRMH_FRM8               : bit  absolute UFRMH.0;
  UFRML                    : byte absolute $0E95;
  UFRML_FRM7               : bit  absolute UFRML.7;
  UFRML_FRM6               : bit  absolute UFRML.6;
  UFRML_FRM5               : bit  absolute UFRML.5;
  UFRML_FRM4               : bit  absolute UFRML.4;
  UFRML_FRM3               : bit  absolute UFRML.3;
  UFRML_FRM2               : bit  absolute UFRML.2;
  UFRML_FRM1               : bit  absolute UFRML.1;
  UFRML_FRM0               : bit  absolute UFRML.0;
  UADDR                    : byte absolute $0E96;
  UADDR_ADDR6              : bit  absolute UADDR.6;
  UADDR_ADDR5              : bit  absolute UADDR.5;
  UADDR_ADDR4              : bit  absolute UADDR.4;
  UADDR_ADDR3              : bit  absolute UADDR.3;
  UADDR_ADDR2              : bit  absolute UADDR.2;
  UADDR_ADDR1              : bit  absolute UADDR.1;
  UADDR_ADDR0              : bit  absolute UADDR.0;
  UEIE                     : byte absolute $0E97;
  UEIE_BTSEE               : bit  absolute UEIE.7;
  UEIE_BTOEE               : bit  absolute UEIE.4;
  UEIE_DFN8EE              : bit  absolute UEIE.3;
  UEIE_CRC16EE             : bit  absolute UEIE.2;
  UEIE_CRC5EE              : bit  absolute UEIE.1;
  UEIE_PIDEE               : bit  absolute UEIE.0;
  UEP0                     : byte absolute $0E98;
  UEP0_EPHSHK              : bit  absolute UEP0.4;
  UEP0_EPCONDIS            : bit  absolute UEP0.3;
  UEP0_EPOUTEN             : bit  absolute UEP0.2;
  UEP0_EPINEN              : bit  absolute UEP0.1;
  UEP0_EPSTALL             : bit  absolute UEP0.0;
  UEP1                     : byte absolute $0E99;
  UEP2                     : byte absolute $0E9A;
  UEP3                     : byte absolute $0E9B;
  UEP4                     : byte absolute $0E9C;
  UEP5                     : byte absolute $0E9D;
  UEP6                     : byte absolute $0E9E;
  UEP7                     : byte absolute $0E9F;
  STATUS_SHAD              : byte absolute $0FE4;
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
  {$SET_STATE_RAM '011-012:SFR'}            // Bank 0 : PIR1, PIR2
  {$SET_STATE_RAM '015-01C:SFR'}            // Bank 0 : TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08E:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '091-092:SFR'}            // Bank 1 : PIE1, PIE2
  {$SET_STATE_RAM '095-09F:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10E:SFR'}            // Bank 2 : LATA, LATB, LATC
  {$SET_STATE_RAM '111-119:SFR'}            // Bank 2 : CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18E:SFR'}            // Bank 3 : ANSELA, ANSELB, ANSELC
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RCREG, TXREG, SPBRGL, SPBRGH, RCSTA, TXSTA, BAUDCON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20D:SFR'}            // Bank 4 : WPUA, WPUB
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '320-32F:GPR'}           
  {$SET_STATE_RAM '330-36F:GPR'}           
  {$SET_STATE_RAM '391-396:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF, IOCBP, IOCBN, IOCBF
  {$SET_STATE_RAM '39A-39B:SFR'}            // Bank 7 : CLKRCON, ACTCON
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '611-616:SFR'}            // Bank 12 : PWM1DCL, PWM1DCH, PWM1CON, PWM2DCL, PWM2DCH, PWM2CON
  {$SET_STATE_RAM '620-64F:GPR'}           
  {$SET_STATE_RAM '691-695:SFR'}            // Bank 13 : CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1CON2
  {$SET_STATE_RAM 'E8E-E9F:SFR'}            // Bank 29 : UCON, USTAT, UIR, UCFG, UIE, UEIR, UFRMH, UFRML, UADDR, UEIE, UEP0, UEP1, UEP2, UEP3, UEP4, UEP5, UEP6, UEP7
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3B'} // PORTA bits 7,6,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:F0'} // PORTB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:FB'} // PIR1 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:EE'} // PIR2 bits 4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:30'} // TRISA bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:F0'} // TRISB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:FB'} // PIE1 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:EE'} // PIE2 bits 4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:7F'} // OSCTUNE bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:F3'} // OSCSTAT bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:70'} // ADCON2 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:30'} // LATA bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10D:F0'} // LATB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:F7'} // CM2CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:F7'} // CM2CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:BC'} // DACCON0 bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:A8'} // APFCON bits 6,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:10'} // ANSELA bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:30'} // ANSELB bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:CF'} // ANSELC bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUDCON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:38'} // WPUA bits 7,6,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20D:F0'} // WPUB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3B'} // IOCAP bits 7,6,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3B'} // IOCAN bits 7,6,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3B'} // IOCAF bits 7,6,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '394:F0'} // IOCBP bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '395:F0'} // IOCBN bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '396:F0'} // IOCBF bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39B:DA'} // ACTCON bits 5,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '611:C0'} // PWM1DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:F0'} // PWM1CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '614:C0'} // PWM2DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '616:F0'} // PWM2CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:F9'} // CWG1CON0 bits 2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:F3'} // CWG1CON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '695:CE'} // CWG1CON2 bits 5,4,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E8E:7E'} // UCON bits 7,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E8F:7E'} // USTAT bits 7,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E90:7F'} // UIR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E91:97'} // UCFG bits 6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E92:7F'} // UIE bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E93:9F'} // UEIR bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E94:07'} // UFRMH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E96:7F'} // UADDR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E97:9F'} // UEIE bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E98:1F'} // UEP0 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E99:1F'} // UEP1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9A:1F'} // UEP2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9B:1F'} // UEP3 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9C:1F'} // UEP4 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9D:1F'} // UEP5 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9E:1F'} // UEP6 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9F:1F'} // UEP7 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : RA5/SOSCI/T1CKI/OSC1/CLKIN
  // Pin  3 : RA4/AN3/SOSCO/T1G/OSC2/CLKOUT/CLKR
  // Pin  4 : RA3/SS/T1G/MCLR/VPP
  // Pin  5 : RC5/T0CKI/PWM1/CWG1A
  // Pin  6 : RC4/C2OUT/C1OUT/CWG1B
  // Pin  7 : RC3/AN7/DACOUT2/C2IN3-/C1IN3-/CLKR
  // Pin  8 : RC6/AN8/SS/PWM2
  // Pin  9 : RC7/AN9/SDO
  // Pin 10 : RB7/TX_CK
  // Pin 11 : RB6/SCK/SCL
  // Pin 12 : RB5/AN11/RX_DT
  // Pin 13 : RB4/AN10/SDI/SDA
  // Pin 14 : RC2/AN6/C2IN2-/C1IN2-/DACOUT1
  // Pin 15 : RC1/INT/CWGFLT_n/C2IN1-/C1IN1-/AN5/ICDCLK/ICSPCLK
  // Pin 16 : RC0/AN4/C2IN+/C1IN+/VREF+/ICSPDAT
  // Pin 17 : VUSB3V3
  // Pin 18 : RA1/D-/ICSPCLK
  // Pin 19 : RA0/D+/ICSPDAT
  // Pin 20 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-19,1-18,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '00D:4-13,5-12,6-11,7-10'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_ECH        = $3FFF}  // ECH, External Clock, High Power Mode (4-20 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECM        = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECL        = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_INTOSC     = $3FFC}  // INTOSC oscillator: I/O function on CLKIN pin
  {$define _FOSC_EXTRC      = $3FFB}  // EXTRC oscillator: External RC circuit connected to CLKIN pin
  {$define _FOSC_HS         = $3FFA}  // HS Oscillator, High-speed crystal/resonator connected between OSC1 and OSC2 pins
  {$define _FOSC_XT         = $3FF9}  // XT Oscillator, Crystal/resonator connected between OSC1 and OSC2 pins
  {$define _FOSC_LP         = $3FF8}  // LP Oscillator, Low-power crystal connected between OSC1 and OSC2 pins

  // WDTE : Watchdog Timer Enable
  {$define _WDTE_ON         = $3FFF}  // WDT enabled
  {$define _WDTE_NSLEEP     = $3FF7}  // WDT enabled while running and disabled in Sleep
  {$define _WDTE_SWDTEN     = $3FEF}  // WDT controlled by the SWDTEN bit in the WDTCON register
  {$define _WDTE_OFF        = $3FE7}  // WDT disabled

  // PWRTE : Power-up Timer Enable
  {$define _PWRTE_OFF       = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON        = $3FDF}  // PWRT enabled

  // MCLRE : MCLR Pin Function Select
  {$define _MCLRE_ON        = $3FFF}  // MCLR/VPP pin function is MCLR
  {$define _MCLRE_OFF       = $3FBF}  // MCLR/VPP pin function is digital input

  // CP : Flash Program Memory Code Protection
  {$define _CP_OFF          = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON           = $3F7F}  // Program memory code protection is enabled

  // BOREN : Brown-out Reset Enable
  {$define _BOREN_ON        = $3FFF}  // Brown-out Reset enabled
  {$define _BOREN_NSLEEP    = $3DFF}  // Brown-out Reset enabled while running and disabled in Sleep
  {$define _BOREN_SBODEN    = $3BFF}  // Brown-out Reset controlled by the SBOREN bit in the BORCON register
  {$define _BOREN_OFF       = $39FF}  // Brown-out Reset disabled

  // CLKOUTEN : Clock Out Enable
  {$define _CLKOUTEN_OFF    = $3FFF}  // CLKOUT function is disabled. I/O or oscillator function on the CLKOUT pin
  {$define _CLKOUTEN_ON     = $37FF}  // CLKOUT function is enabled on the CLKOUT pin

  // IESO : Internal/External Switchover Mode
  {$define _IESO_ON         = $3FFF}  // Internal/External Switchover Mode is enabled
  {$define _IESO_OFF        = $2FFF}  // Internal/External Switchover Mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enable
  {$define _FCMEN_ON        = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF       = $1FFF}  // Fail-Safe Clock Monitor is disabled

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF         = $3FFF}  // Write protection off
  {$define _WRT_BOOT        = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by PMCON control
  {$define _WRT_HALF        = $3FFD}  // 000h to 0FFFh write protected, 1000h to 1FFFh may be modified by PMCON control
  {$define _WRT_ALL         = $3FFC}  // 000h to 1FFFh write protected, no addresses may be modified by PMCON control

  // CPUDIV : CPU System Clock Selection Bit
  {$define _CPUDIV_CLKDIV6  = $3FFF}  // CPU system clock divided by 6
  {$define _CPUDIV_CLKDIV3  = $3FEF}  // CPU system clock divided by 3
  {$define _CPUDIV_CLKDIV2  = $3FDF}  // CPU system clock divided by 2
  {$define _CPUDIV_NOCLKDIV = $3FCF}  // NO CPU system divide

  // USBLSCLK : USB Low SPeed Clock Selection bit
  {$define _USBLSCLK_48MHz  = $3FFF}  // System clock expects 48 MHz, FS/LS USB CLKENs divide-by is set to 8.
  {$define _USBLSCLK_24MHz  = $3FBF}  // System clock expects 24 MHz, FS/LS USB CLKENs divide-by is set to 4.

  // PLLMULT : PLL Multipler Selection Bit
  {$define _PLLMULT_3x      = $3FFF}  // 3x Output Frequency Selected
  {$define _PLLMULT_4x      = $3F7F}  // 4x Output Frequency Selected

  // PLLEN : PLL Enable Bit
  {$define _PLLEN_ENABLED   = $3FFF}  // 3x or 4x PLL Enabled
  {$define _PLLEN_DISABLED  = $3EFF}  // 3x or 4x PLL Disabled

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON       = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF      = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO         = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI         = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LPBOR : Low-Power Brown Out Reset
  {$define _LPBOR_OFF       = $3FFF}  // Low-Power BOR is disabled
  {$define _LPBOR_ON        = $37FF}  // Low-Power BOR is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON          = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF         = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
