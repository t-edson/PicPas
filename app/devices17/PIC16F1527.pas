unit PIC16F1527;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1527'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 64}
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
  PORTD                    : byte absolute $000F;
  PORTD_RD7                : bit  absolute PORTD.7;
  PORTD_RD6                : bit  absolute PORTD.6;
  PORTD_RD5                : bit  absolute PORTD.5;
  PORTD_RD4                : bit  absolute PORTD.4;
  PORTD_RD3                : bit  absolute PORTD.3;
  PORTD_RD2                : bit  absolute PORTD.2;
  PORTD_RD1                : bit  absolute PORTD.1;
  PORTD_RD0                : bit  absolute PORTD.0;
  PORTE                    : byte absolute $0010;
  PORTE_RE7                : bit  absolute PORTE.7;
  PORTE_RE6                : bit  absolute PORTE.6;
  PORTE_RE5                : bit  absolute PORTE.5;
  PORTE_RE4                : bit  absolute PORTE.4;
  PORTE_RE3                : bit  absolute PORTE.3;
  PORTE_RE2                : bit  absolute PORTE.2;
  PORTE_RE1                : bit  absolute PORTE.1;
  PORTE_RE0                : bit  absolute PORTE.0;
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_RC1IF               : bit  absolute PIR1.5;
  PIR1_TX1IF               : bit  absolute PIR1.4;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_TMR5GIF             : bit  absolute PIR2.6;
  PIR2_TMR3GIF             : bit  absolute PIR2.5;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_TMR10IF             : bit  absolute PIR2.2;
  PIR2_TMR8IF              : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_CCP6IF              : bit  absolute PIR3.7;
  PIR3_CCP5IF              : bit  absolute PIR3.6;
  PIR3_CCP4IF              : bit  absolute PIR3.5;
  PIR3_CCP3IF              : bit  absolute PIR3.4;
  PIR3_TMR6IF              : bit  absolute PIR3.3;
  PIR3_TMR5IF              : bit  absolute PIR3.2;
  PIR3_TMR4IF              : bit  absolute PIR3.1;
  PIR3_TMR3IF              : bit  absolute PIR3.0;
  PIR4                     : byte absolute $0014;
  PIR4_CCP10IF             : bit  absolute PIR4.7;
  PIR4_CCP9IF              : bit  absolute PIR4.6;
  PIR4_RC2IF               : bit  absolute PIR4.5;
  PIR4_TX2IF               : bit  absolute PIR4.4;
  PIR4_CCP8IF              : bit  absolute PIR4.3;
  PIR4_CCP7IF              : bit  absolute PIR4.2;
  PIR4_BCL2IF              : bit  absolute PIR4.1;
  PIR4_SSP2IF              : bit  absolute PIR4.0;
  TMR0                     : byte absolute $0015;
  TMR1L                    : byte absolute $0016;
  TMR1H                    : byte absolute $0017;
  T1CON                    : byte absolute $0018;
  T1CON_TMR1CS1            : bit  absolute T1CON.7;
  T1CON_TMR1CS0            : bit  absolute T1CON.6;
  T1CON_T1CKPS1            : bit  absolute T1CON.5;
  T1CON_T1CKPS0            : bit  absolute T1CON.4;
  T1CON_SOSCEN             : bit  absolute T1CON.3;
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
  TRISD                    : byte absolute $008F;
  TRISD_TRISD7             : bit  absolute TRISD.7;
  TRISD_TRISD6             : bit  absolute TRISD.6;
  TRISD_TRISD5             : bit  absolute TRISD.5;
  TRISD_TRISD4             : bit  absolute TRISD.4;
  TRISD_TRISD3             : bit  absolute TRISD.3;
  TRISD_TRISD2             : bit  absolute TRISD.2;
  TRISD_TRISD1             : bit  absolute TRISD.1;
  TRISD_TRISD0             : bit  absolute TRISD.0;
  TRISE                    : byte absolute $0090;
  TRISE_TRISE7             : bit  absolute TRISE.7;
  TRISE_TRISE6             : bit  absolute TRISE.6;
  TRISE_TRISE5             : bit  absolute TRISE.5;
  TRISE_TRISE4             : bit  absolute TRISE.4;
  TRISE_TRISE3             : bit  absolute TRISE.3;
  TRISE_TRISE2             : bit  absolute TRISE.2;
  TRISE_TRISE1             : bit  absolute TRISE.1;
  TRISE_TRISE0             : bit  absolute TRISE.0;
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_RC1IE               : bit  absolute PIE1.5;
  PIE1_TX1IE               : bit  absolute PIE1.4;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_OSFIE               : bit  absolute PIE2.7;
  PIE2_TMR5GIE             : bit  absolute PIE2.6;
  PIE2_TMR3GIE             : bit  absolute PIE2.5;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_TMR10IE             : bit  absolute PIE2.2;
  PIE2_TMR8IE              : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_CCP6IE              : bit  absolute PIE3.7;
  PIE3_CCP5IE              : bit  absolute PIE3.6;
  PIE3_CCP4IE              : bit  absolute PIE3.5;
  PIE3_CCP3IE              : bit  absolute PIE3.4;
  PIE3_TMR6IE              : bit  absolute PIE3.3;
  PIE3_TMR5IE              : bit  absolute PIE3.2;
  PIE3_TMR4IE              : bit  absolute PIE3.1;
  PIE3_TMR3IE              : bit  absolute PIE3.0;
  PIE4                     : byte absolute $0094;
  PIE4_CCP10IE             : bit  absolute PIE4.7;
  PIE4_CCP9IE              : bit  absolute PIE4.6;
  PIE4_RC2IE               : bit  absolute PIE4.5;
  PIE4_TX2IE               : bit  absolute PIE4.4;
  PIE4_CCP8IE              : bit  absolute PIE4.3;
  PIE4_CCP7IE              : bit  absolute PIE4.2;
  PIE4_BCL2IE              : bit  absolute PIE4.1;
  PIE4_SSP2IE              : bit  absolute PIE4.0;
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
  OSCCON                   : byte absolute $0099;
  OSCCON_IRCF3             : bit  absolute OSCCON.6;
  OSCCON_IRCF2             : bit  absolute OSCCON.5;
  OSCCON_IRCF1             : bit  absolute OSCCON.4;
  OSCCON_IRCF0             : bit  absolute OSCCON.3;
  OSCCON_SCS1              : bit  absolute OSCCON.1;
  OSCCON_SCS0              : bit  absolute OSCCON.0;
  OSCSTAT                  : byte absolute $009A;
  OSCSTAT_SOSCR            : bit  absolute OSCSTAT.7;
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
  LATD                     : byte absolute $010F;
  LATD_LATD7               : bit  absolute LATD.7;
  LATD_LATD6               : bit  absolute LATD.6;
  LATD_LATD5               : bit  absolute LATD.5;
  LATD_LATD4               : bit  absolute LATD.4;
  LATD_LATD3               : bit  absolute LATD.3;
  LATD_LATD2               : bit  absolute LATD.2;
  LATD_LATD1               : bit  absolute LATD.1;
  LATD_LATD0               : bit  absolute LATD.0;
  LATE                     : byte absolute $0110;
  LATE_LATE7               : bit  absolute LATE.7;
  LATE_LATE6               : bit  absolute LATE.6;
  LATE_LATE5               : bit  absolute LATE.5;
  LATE_LATE4               : bit  absolute LATE.4;
  LATE_LATE3               : bit  absolute LATE.3;
  LATE_LATE2               : bit  absolute LATE.2;
  LATE_LATE1               : bit  absolute LATE.1;
  LATE_LATE0               : bit  absolute LATE.0;
  BORCON                   : byte absolute $0116;
  BORCON_SBOREN            : bit  absolute BORCON.7;
  BORCON_BORFS             : bit  absolute BORCON.6;
  BORCON_BORRDY            : bit  absolute BORCON.0;
  FVRCON                   : byte absolute $0117;
  FVRCON_FVREN             : bit  absolute FVRCON.7;
  FVRCON_FVRRDY            : bit  absolute FVRCON.6;
  FVRCON_TSEN              : bit  absolute FVRCON.5;
  FVRCON_TSRNG             : bit  absolute FVRCON.4;
  FVRCON_ADFVR1            : bit  absolute FVRCON.1;
  FVRCON_ADFVR0            : bit  absolute FVRCON.0;
  APFCON                   : byte absolute $011D;
  APFCON_T3CKISEL          : bit  absolute APFCON.1;
  APFCON_CCP2SEL           : bit  absolute APFCON.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA5             : bit  absolute ANSELA.5;
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
  ANSELD                   : byte absolute $018F;
  ANSELD_ANSD3             : bit  absolute ANSELD.3;
  ANSELD_ANSD2             : bit  absolute ANSELD.2;
  ANSELD_ANSD1             : bit  absolute ANSELD.1;
  ANSELD_ANSD0             : bit  absolute ANSELD.0;
  ANSELE                   : byte absolute $0190;
  ANSELE_ANSE2             : bit  absolute ANSELE.2;
  ANSELE_ANSE1             : bit  absolute ANSELE.1;
  ANSELE_ANSE0             : bit  absolute ANSELE.0;
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
  VREGCON_VREGPM           : bit  absolute VREGCON.1;
  VREGCON_reserved         : bit  absolute VREGCON.0;
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
  WPUB                     : byte absolute $020D;
  WPUB_WPUB7               : bit  absolute WPUB.7;
  WPUB_WPUB6               : bit  absolute WPUB.6;
  WPUB_WPUB5               : bit  absolute WPUB.5;
  WPUB_WPUB4               : bit  absolute WPUB.4;
  WPUB_WPUB3               : bit  absolute WPUB.3;
  WPUB_WPUB2               : bit  absolute WPUB.2;
  WPUB_WPUB1               : bit  absolute WPUB.1;
  WPUB_WPUB0               : bit  absolute WPUB.0;
  WPUD                     : byte absolute $020F;
  WPUD_WPUD7               : bit  absolute WPUD.7;
  WPUD_WPUD6               : bit  absolute WPUD.6;
  WPUD_WPUD5               : bit  absolute WPUD.5;
  WPUD_WPUD4               : bit  absolute WPUD.4;
  WPUD_WPUD3               : bit  absolute WPUD.3;
  WPUD_WPUD2               : bit  absolute WPUD.2;
  WPUD_WPUD1               : bit  absolute WPUD.1;
  WPUD_WPUD0               : bit  absolute WPUD.0;
  WPUE                     : byte absolute $0210;
  WPUE_WPUE7               : bit  absolute WPUE.7;
  WPUE_WPUE6               : bit  absolute WPUE.6;
  WPUE_WPUE5               : bit  absolute WPUE.5;
  WPUE_WPUE4               : bit  absolute WPUE.4;
  WPUE_WPUE3               : bit  absolute WPUE.3;
  WPUE_WPUE2               : bit  absolute WPUE.2;
  WPUE_WPUE1               : bit  absolute WPUE.1;
  WPUE_WPUE0               : bit  absolute WPUE.0;
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
  SSP2BUF                  : byte absolute $0219;
  SSP2ADD                  : byte absolute $021A;
  SSP2MSK                  : byte absolute $021B;
  SSP2STAT                 : byte absolute $021C;
  SSP2CON1                 : byte absolute $021D;
  SSP2CON2                 : byte absolute $021E;
  SSP2CON3                 : byte absolute $021F;
  PORTF                    : byte absolute $028C;
  PORTF_RF7                : bit  absolute PORTF.7;
  PORTF_RF6                : bit  absolute PORTF.6;
  PORTF_RF5                : bit  absolute PORTF.5;
  PORTF_RF4                : bit  absolute PORTF.4;
  PORTF_RF3                : bit  absolute PORTF.3;
  PORTF_RF2                : bit  absolute PORTF.2;
  PORTF_RF1                : bit  absolute PORTF.1;
  PORTF_RF0                : bit  absolute PORTF.0;
  PORTG                    : byte absolute $028D;
  PORTG_RG5                : bit  absolute PORTG.5;
  PORTG_RG4                : bit  absolute PORTG.4;
  PORTG_RG3                : bit  absolute PORTG.3;
  PORTG_RG2                : bit  absolute PORTG.2;
  PORTG_RG1                : bit  absolute PORTG.1;
  PORTG_RG0                : bit  absolute PORTG.0;
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
  CCPTMRS0                 : byte absolute $029D;
  CCPTMRS0_C4TSEL1         : bit  absolute CCPTMRS0.7;
  CCPTMRS0_C4TSEL0         : bit  absolute CCPTMRS0.6;
  CCPTMRS0_C3TSEL1         : bit  absolute CCPTMRS0.5;
  CCPTMRS0_C3TSEL0         : bit  absolute CCPTMRS0.4;
  CCPTMRS0_C2TSEL1         : bit  absolute CCPTMRS0.3;
  CCPTMRS0_C2TSEL0         : bit  absolute CCPTMRS0.2;
  CCPTMRS0_C1TSEL1         : bit  absolute CCPTMRS0.1;
  CCPTMRS0_C1TSEL0         : bit  absolute CCPTMRS0.0;
  CCPTMRS1                 : byte absolute $029E;
  CCPTMRS1_C8TSEL1         : bit  absolute CCPTMRS1.7;
  CCPTMRS1_C8TSEL0         : bit  absolute CCPTMRS1.6;
  CCPTMRS1_C7TSEL1         : bit  absolute CCPTMRS1.5;
  CCPTMRS1_C7TSEL0         : bit  absolute CCPTMRS1.4;
  CCPTMRS1_C6TSEL1         : bit  absolute CCPTMRS1.3;
  CCPTMRS1_C6TSEL0         : bit  absolute CCPTMRS1.2;
  CCPTMRS1_C5TSEL1         : bit  absolute CCPTMRS1.1;
  CCPTMRS1_C5TSEL0         : bit  absolute CCPTMRS1.0;
  CCPTMRS2                 : byte absolute $029F;
  CCPTMRS2_C10TSEL1        : bit  absolute CCPTMRS2.3;
  CCPTMRS2_C10TSEL0        : bit  absolute CCPTMRS2.2;
  CCPTMRS2_C9TSEL1         : bit  absolute CCPTMRS2.1;
  CCPTMRS2_C9TSEL0         : bit  absolute CCPTMRS2.0;
  TRISF                    : byte absolute $030C;
  TRISF_TRISF7             : bit  absolute TRISF.7;
  TRISF_TRISF6             : bit  absolute TRISF.6;
  TRISF_TRISF5             : bit  absolute TRISF.5;
  TRISF_TRISF4             : bit  absolute TRISF.4;
  TRISF_TRISF3             : bit  absolute TRISF.3;
  TRISF_TRISF2             : bit  absolute TRISF.2;
  TRISF_TRISF1             : bit  absolute TRISF.1;
  TRISF_TRISF0             : bit  absolute TRISF.0;
  TRISG                    : byte absolute $030D;
  TRISG_TRISG4             : bit  absolute TRISG.4;
  TRISG_TRISG3             : bit  absolute TRISG.3;
  TRISG_TRISG2             : bit  absolute TRISG.2;
  TRISG_TRISG1             : bit  absolute TRISG.1;
  TRISG_TRISG0             : bit  absolute TRISG.0;
  CCPR3L                   : byte absolute $0311;
  CCPR3H                   : byte absolute $0312;
  CCP3CON                  : byte absolute $0313;
  CCP3CON_DC3B1            : bit  absolute CCP3CON.5;
  CCP3CON_DC3B0            : bit  absolute CCP3CON.4;
  CCP3CON_CCP3M3           : bit  absolute CCP3CON.3;
  CCP3CON_CCP3M2           : bit  absolute CCP3CON.2;
  CCP3CON_CCP3M1           : bit  absolute CCP3CON.1;
  CCP3CON_CCP3M0           : bit  absolute CCP3CON.0;
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
  LATF                     : byte absolute $038C;
  LATF_LATF7               : bit  absolute LATF.7;
  LATF_LATF6               : bit  absolute LATF.6;
  LATF_LATF5               : bit  absolute LATF.5;
  LATF_LATF4               : bit  absolute LATF.4;
  LATF_LATF3               : bit  absolute LATF.3;
  LATF_LATF2               : bit  absolute LATF.2;
  LATF_LATF1               : bit  absolute LATF.1;
  LATF_LATF0               : bit  absolute LATF.0;
  LATG                     : byte absolute $038D;
  LATG_LATG4               : bit  absolute LATG.4;
  LATG_LATG3               : bit  absolute LATG.3;
  LATG_LATG2               : bit  absolute LATG.2;
  LATG_LATG1               : bit  absolute LATG.1;
  LATG_LATG0               : bit  absolute LATG.0;
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
  ANSELF                   : byte absolute $040C;
  ANSELF_ANSF7             : bit  absolute ANSELF.7;
  ANSELF_ANSF6             : bit  absolute ANSELF.6;
  ANSELF_ANSF5             : bit  absolute ANSELF.5;
  ANSELF_ANSF4             : bit  absolute ANSELF.4;
  ANSELF_ANSF3             : bit  absolute ANSELF.3;
  ANSELF_ANSF2             : bit  absolute ANSELF.2;
  ANSELF_ANSF1             : bit  absolute ANSELF.1;
  ANSELF_ANSF0             : bit  absolute ANSELF.0;
  ANSELG                   : byte absolute $040D;
  ANSELG_ANSG4             : bit  absolute ANSELG.4;
  ANSELG_ANSG3             : bit  absolute ANSELG.3;
  ANSELG_ANSG2             : bit  absolute ANSELG.2;
  ANSELG_ANSG1             : bit  absolute ANSELG.1;
  TMR3L                    : byte absolute $0411;
  TMR3H                    : byte absolute $0412;
  T3CON                    : byte absolute $0413;
  T3CON_TMR3CS1            : bit  absolute T3CON.7;
  T3CON_TMR3CS0            : bit  absolute T3CON.6;
  T3CON_T3CKPS1            : bit  absolute T3CON.5;
  T3CON_T3CKPS0            : bit  absolute T3CON.4;
  T3CON_nT3SYNC            : bit  absolute T3CON.2;
  T3CON_TMR3ON             : bit  absolute T3CON.0;
  T3GCON                   : byte absolute $0414;
  T3GCON_TMR3GE            : bit  absolute T3GCON.7;
  T3GCON_T3GPOL            : bit  absolute T3GCON.6;
  T3GCON_T3GTM             : bit  absolute T3GCON.5;
  T3GCON_T3GSPM            : bit  absolute T3GCON.4;
  T3GCON_T3GGO_nDONE       : bit  absolute T3GCON.3;
  T3GCON_T3GVAL            : bit  absolute T3GCON.2;
  T3GCON_T3GSS1            : bit  absolute T3GCON.1;
  T3GCON_T3GSS0            : bit  absolute T3GCON.0;
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
  TMR5L                    : byte absolute $0418;
  TMR5H                    : byte absolute $0419;
  T5CON                    : byte absolute $041A;
  T5CON_TMR5CS1            : bit  absolute T5CON.7;
  T5CON_TMR5CS0            : bit  absolute T5CON.6;
  T5CON_T5CKPS1            : bit  absolute T5CON.5;
  T5CON_T5CKPS0            : bit  absolute T5CON.4;
  T5CON_nT5SYNC            : bit  absolute T5CON.2;
  T5CON_TMR5ON             : bit  absolute T5CON.0;
  T5GCON                   : byte absolute $041B;
  T5GCON_TMR5GE            : bit  absolute T5GCON.7;
  T5GCON_T5GPOL            : bit  absolute T5GCON.6;
  T5GCON_T5GTM             : bit  absolute T5GCON.5;
  T5GCON_T5GSPM            : bit  absolute T5GCON.4;
  T5GCON_T5GGO_nDONE       : bit  absolute T5GCON.3;
  T5GCON_T5GVAL            : bit  absolute T5GCON.2;
  T5GCON_T5GSS1            : bit  absolute T5GCON.1;
  T5GCON_T5GSS0            : bit  absolute T5GCON.0;
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
  WPUG                     : byte absolute $048D;
  WPUG_WPUG5               : bit  absolute WPUG.5;
  RC2REG                   : byte absolute $0491;
  TX2REG                   : byte absolute $0492;
  SP2BRGL                  : byte absolute $0493;
  SP2BRGH                  : byte absolute $0494;
  RC2STA                   : byte absolute $0495;
  TX2STA                   : byte absolute $0496;
  BAUD2CON                 : byte absolute $0497;
  TMR8                     : byte absolute $0595;
  PR8                      : byte absolute $0596;
  T8CON                    : byte absolute $0597;
  T8CON_T8OUTPS3           : bit  absolute T8CON.6;
  T8CON_T8OUTPS2           : bit  absolute T8CON.5;
  T8CON_T8OUTPS1           : bit  absolute T8CON.4;
  T8CON_T8OUTPS0           : bit  absolute T8CON.3;
  T8CON_TMR8ON             : bit  absolute T8CON.2;
  T8CON_T8CKPS1            : bit  absolute T8CON.1;
  T8CON_T8CKPS0            : bit  absolute T8CON.0;
  TMR10                    : byte absolute $059C;
  PR10                     : byte absolute $059D;
  T10CON                   : byte absolute $059E;
  T10CON_T10OUTPS3         : bit  absolute T10CON.6;
  T10CON_T10OUTPS2         : bit  absolute T10CON.5;
  T10CON_T10OUTPS1         : bit  absolute T10CON.4;
  T10CON_T10OUTPS0         : bit  absolute T10CON.3;
  T10CON_TMR10ON           : bit  absolute T10CON.2;
  T10CON_T10CKPS1          : bit  absolute T10CON.1;
  T10CON_T10CKPS0          : bit  absolute T10CON.0;
  CCPR6L                   : byte absolute $0611;
  CCPR6H                   : byte absolute $0612;
  CCP6CON                  : byte absolute $0613;
  CCP6CON_DC6B1            : bit  absolute CCP6CON.5;
  CCP6CON_DC6B0            : bit  absolute CCP6CON.4;
  CCP6CON_CCP6M3           : bit  absolute CCP6CON.3;
  CCP6CON_CCP6M2           : bit  absolute CCP6CON.2;
  CCP6CON_CCP6M1           : bit  absolute CCP6CON.1;
  CCP6CON_CCP6M0           : bit  absolute CCP6CON.0;
  CCPR7L                   : byte absolute $0614;
  CCPR7H                   : byte absolute $0615;
  CCP7CON                  : byte absolute $0616;
  CCP7CON_DC7B1            : bit  absolute CCP7CON.5;
  CCP7CON_DC7B0            : bit  absolute CCP7CON.4;
  CCP7CON_CCP7M3           : bit  absolute CCP7CON.3;
  CCP7CON_CCP7M2           : bit  absolute CCP7CON.2;
  CCP7CON_CCP7M1           : bit  absolute CCP7CON.1;
  CCP7CON_CCP7M0           : bit  absolute CCP7CON.0;
  CCPR8L                   : byte absolute $0617;
  CCPR8H                   : byte absolute $0618;
  CCP8CON                  : byte absolute $0619;
  CCP8CON_DC8B1            : bit  absolute CCP8CON.5;
  CCP8CON_DC8B0            : bit  absolute CCP8CON.4;
  CCP8CON_CCP8M3           : bit  absolute CCP8CON.3;
  CCP8CON_CCP8M2           : bit  absolute CCP8CON.2;
  CCP8CON_CCP8M1           : bit  absolute CCP8CON.1;
  CCP8CON_CCP8M0           : bit  absolute CCP8CON.0;
  CCPR9L                   : byte absolute $061A;
  CCPR9H                   : byte absolute $061B;
  CCP9CON                  : byte absolute $061C;
  CCP9CON_DC9B1            : bit  absolute CCP9CON.5;
  CCP9CON_DC9B0            : bit  absolute CCP9CON.4;
  CCP9CON_CCP9M3           : bit  absolute CCP9CON.3;
  CCP9CON_CCP9M2           : bit  absolute CCP9CON.2;
  CCP9CON_CCP9M1           : bit  absolute CCP9CON.1;
  CCP9CON_CCP9M0           : bit  absolute CCP9CON.0;
  CCPR10L                  : byte absolute $061D;
  CCPR10H                  : byte absolute $061E;
  CCP10CON                 : byte absolute $061F;
  CCP10CON_DC10B1          : bit  absolute CCP10CON.5;
  CCP10CON_DC10B0          : bit  absolute CCP10CON.4;
  CCP10CON_CCP10M3         : bit  absolute CCP10CON.3;
  CCP10CON_CCP10M2         : bit  absolute CCP10CON.2;
  CCP10CON_CCP10M1         : bit  absolute CCP10CON.1;
  CCP10CON_CCP10M0         : bit  absolute CCP10CON.0;
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
  {$SET_STATE_RAM '00C-01C:SFR'}            // Bank 0 : PORTA, PORTB, PORTC, PORTD, PORTE, PIR1, PIR2, PIR3, PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-097:SFR'}            // Bank 1 : TRISA, TRISB, TRISC, TRISD, TRISE, PIE1, PIE2, PIE3, PIE4, OPTION_REG, PCON, WDTCON
  {$SET_STATE_RAM '099-09E:SFR'}            // Bank 1 : OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-110:SFR'}            // Bank 2 : LATA, LATB, LATC, LATD, LATE
  {$SET_STATE_RAM '116-117:SFR'}            // Bank 2 : BORCON, FVRCON
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18D:SFR'}            // Bank 3 : ANSELA, ANSELB
  {$SET_STATE_RAM '18F-197:SFR'}            // Bank 3 : ANSELD, ANSELE, PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20D-20D:SFR'}            // Bank 4 : WPUB
  {$SET_STATE_RAM '20F-217:SFR'}            // Bank 4 : WPUD, WPUE, SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '219-21F:SFR'}            // Bank 4 : SSP2BUF, SSP2ADD, SSP2MSK, SSP2STAT, SSP2CON1, SSP2CON2, SSP2CON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28D:SFR'}            // Bank 5 : PORTF, PORTG
  {$SET_STATE_RAM '291-293:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '298-29A:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON
  {$SET_STATE_RAM '29D-29F:SFR'}            // Bank 5 : CCPTMRS0, CCPTMRS1, CCPTMRS2
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30D:SFR'}            // Bank 6 : TRISF, TRISG
  {$SET_STATE_RAM '311-313:SFR'}            // Bank 6 : CCPR3L, CCPR3H, CCP3CON
  {$SET_STATE_RAM '318-31A:SFR'}            // Bank 6 : CCPR4L, CCPR4H, CCP4CON
  {$SET_STATE_RAM '31C-31E:SFR'}            // Bank 6 : CCPR5L, CCPR5H, CCP5CON
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-38D:SFR'}            // Bank 7 : LATF, LATG
  {$SET_STATE_RAM '394-396:SFR'}            // Bank 7 : IOCBP, IOCBN, IOCBF
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '40C-40D:SFR'}            // Bank 8 : ANSELF, ANSELG
  {$SET_STATE_RAM '411-41E:SFR'}            // Bank 8 : TMR3L, TMR3H, T3CON, T3GCON, TMR4, PR4, T4CON, TMR5L, TMR5H, T5CON, T5GCON, TMR6, PR6, T6CON
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '48D-48D:SFR'}            // Bank 9 : WPUG
  {$SET_STATE_RAM '491-497:SFR'}            // Bank 9 : RC2REG, TX2REG, SP2BRGL, SP2BRGH, RC2STA, TX2STA, BAUD2CON
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '595-597:SFR'}            // Bank 11 : TMR8, PR8, T8CON
  {$SET_STATE_RAM '59C-59E:SFR'}            // Bank 11 : TMR10, PR10, T10CON
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '611-61F:SFR'}            // Bank 12 : CCPR6L, CCPR6H, CCP6CON, CCPR7L, CCPR7H, CCP7CON, CCPR8L, CCPR8H, CCP8CON, CCPR9L, CCPR9H, CCP9CON, CCPR10L, CCPR10H, CCP10CON
  {$SET_STATE_RAM '620-66F:GPR'}           
  {$SET_STATE_RAM '6A0-6EF:GPR'}           
  {$SET_STATE_RAM '720-76F:GPR'}           
  {$SET_STATE_RAM '7A0-7EF:GPR'}           
  {$SET_STATE_RAM '820-86F:GPR'}           
  {$SET_STATE_RAM '8A0-8EF:GPR'}           
  {$SET_STATE_RAM '920-96F:GPR'}           
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:EF'} // PIR2 bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:EF'} // PIE2 bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:7B'} // OSCCON bits 7,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:B3'} // OSCSTAT bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '117:F3'} // FVRCON bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:03'} // APFCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:2F'} // ANSELA bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:3F'} // ANSELB bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18F:0F'} // ANSELD bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '190:07'} // ANSELE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28D:3F'} // PORTG bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29A:3F'} // CCP2CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29F:0F'} // CCPTMRS2 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30D:3F'} // TRISG bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:3F'} // CCP3CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31A:3F'} // CCP4CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31E:3F'} // CCP5CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38D:1F'} // LATG bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '40D:1E'} // ANSELG bits 7,6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '413:FD'} // T3CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:7F'} // T4CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41A:FD'} // T5CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:7F'} // T6CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '48D:20'} // WPUG bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '497:DB'} // BAUD2CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '597:7F'} // T8CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59E:7F'} // T10CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:3F'} // CCP6CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '616:3F'} // CCP7CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:3F'} // CCP8CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61C:3F'} // CCP9CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61F:3F'} // CCP10CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)
  {$SET_UNIMP_BITS1 '30D:20'} // TRISG bit 5 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : RE1/AN28
  // Pin  2 : RE0/AN27
  // Pin  3 : RG0/CCP3
  // Pin  4 : RG1/AN15/TX2/CK2
  // Pin  5 : RG2/AN14/RX2/DT2
  // Pin  6 : RG3/AN13/CCP4
  // Pin  7 : RG5/MCLR/Vpp
  // Pin  8 : RG4/AN12/T5G/CCP5
  // Pin  9 : Vss
  // Pin 10 : Vdd
  // Pin 11 : RF7/AN5/SS1
  // Pin 12 : RF6/AN11
  // Pin 13 : RF5/AN10
  // Pin 14 : RF4/AN9
  // Pin 15 : RF3/AN8
  // Pin 16 : RF2/AN7
  // Pin 17 : RF1/AN6
  // Pin 18 : RF0/AN16/Vcap
  // Pin 19 : AVdd
  // Pin 20 : AVss
  // Pin 21 : RA3/AN3/Vref+
  // Pin 22 : RA2/AN2
  // Pin 23 : RA1/AN1
  // Pin 24 : RA0/AN0
  // Pin 25 : Vss
  // Pin 26 : Vdd
  // Pin 27 : RA5/AN4/T3G
  // Pin 28 : RA4/T0CKI
  // Pin 29 : RC1/SOSCI/CCP2
  // Pin 30 : RC0/SOSCO/T1CKI
  // Pin 31 : RC6/TX1/CK1
  // Pin 32 : RC7/RX1/DT1
  // Pin 33 : RC2/CCP1
  // Pin 34 : RC3/SCK1/SCL1
  // Pin 35 : RC4/SDI1/SDA1
  // Pin 36 : RC5/SDO1
  // Pin 37 : RB7/ICSPDAT/ICDDAT
  // Pin 38 : Vdd
  // Pin 39 : RA7/OSC1/CLKIN
  // Pin 40 : RA6/OSC2/CLKOUT
  // Pin 41 : Vss
  // Pin 42 : RB6/ICSPCLK/ICDCLK
  // Pin 43 : RB5/AN22/T1G/T3CKI
  // Pin 44 : RB4/AN21/T3CKI
  // Pin 45 : RB3/AN20
  // Pin 46 : RB2/AN19
  // Pin 47 : RB1/AN18
  // Pin 48 : RB0/AN17/INT
  // Pin 49 : RD7/SS2
  // Pin 50 : RD6/SCK2/SCL2
  // Pin 51 : RD5/SDI2/SDA2
  // Pin 52 : RD4/SDO2
  // Pin 53 : RD3/AN26
  // Pin 54 : RD2/AN25
  // Pin 55 : RD1/AN24/T5CKI
  // Pin 56 : Vss
  // Pin 57 : Vdd
  // Pin 58 : RD0/AN23
  // Pin 59 : RE7/CCP2
  // Pin 60 : RE6/CCP6
  // Pin 61 : RE5/CCP7
  // Pin 62 : RE4/CCP8
  // Pin 63 : RE3/CCP9
  // Pin 64 : RE2/AN29/CCP10


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-24,1-23,2-22,3-21,4-28,5-27,6-40,7-39'} // PORTA
  {$MAP_RAM_TO_PIN '00D:0-48,1-47,2-46,3-45,4-44,5-43,6-42,7-37'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-30,1-29,2-33,3-34,4-35,5-36,6-31,7-32'} // PORTC
  {$MAP_RAM_TO_PIN '00F:0-58,1-55,2-54,3-53,4-52,5-51,6-50,7-49'} // PORTD
  {$MAP_RAM_TO_PIN '010:0-2,1-1,2-64,3-63,4-62,5-61,6-60,7-59'} // PORTE
  {$MAP_RAM_TO_PIN '28C:0-18,1-17,2-16,3-15,4-14,5-13,6-12,7-11'} // PORTF
  {$MAP_RAM_TO_PIN '28D:0-3,1-4,2-5,3-6,4-8,5-7'} // PORTG


  // -- Bits Configuration --

  // FOSC : Oscillator Selection
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-20 MHz): device clock supplied to CLKIN pin
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
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to 3FFFh may be modified by PMCON control
  {$define _WRT_HALF     = $3FFD}  // 000h to FFFh write protected, 2000h to 3FFFh may be modified by PMCON control
  {$define _WRT_ALL      = $3FFC}  // 000h to 3FFFh write protected, no addresses may be modified by PMCON control

  // VCAPEN : Voltage Regulator Capacitor Enable bit
  {$define _VCAPEN_OFF   = $3FFF}  // VCAP pin function disabled
  {$define _VCAPEN_ON    = $3FEF}  // VCAP pin function enabled

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON    = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF   = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO      = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI      = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LPBOR : Low-Power Brown Out Reset
  {$define _LPBOR_OFF    = $3FFF}  // Low-Power BOR is disabled
  {$define _LPBOR_ON     = $37FF}  // Low-Power BOR is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
