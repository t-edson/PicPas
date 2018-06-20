unit PIC16F1779;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1779'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 40}
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
  PORTE_RE3                : bit  absolute PORTE.3;
  PORTE_RE2                : bit  absolute PORTE.2;
  PORTE_RE1                : bit  absolute PORTE.1;
  PORTE_RE0                : bit  absolute PORTE.0;
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
  PIR2_COG1IF              : bit  absolute PIR2.4;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_C4IF                : bit  absolute PIR2.2;
  PIR2_C3IF                : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_COG2IF              : bit  absolute PIR3.5;
  PIR3_ZCDIF               : bit  absolute PIR3.4;
  PIR3_CLC4IF              : bit  absolute PIR3.3;
  PIR3_CLC3IF              : bit  absolute PIR3.2;
  PIR3_CLC2IF              : bit  absolute PIR3.1;
  PIR3_CLC1IF              : bit  absolute PIR3.0;
  PIR4                     : byte absolute $0014;
  PIR4_TMR8IF              : bit  absolute PIR4.6;
  PIR4_TMR5GIF             : bit  absolute PIR4.5;
  PIR4_TMR5IF              : bit  absolute PIR4.4;
  PIR4_TMR3GIF             : bit  absolute PIR4.3;
  PIR4_TMR3IF              : bit  absolute PIR4.2;
  PIR4_TMR6IF              : bit  absolute PIR4.1;
  PIR4_TMR4IF              : bit  absolute PIR4.0;
  PIR5                     : byte absolute $0015;
  PIR5_CCP8IF              : bit  absolute PIR5.7;
  PIR5_CCP7IF              : bit  absolute PIR5.6;
  PIR5_COG4IF              : bit  absolute PIR5.5;
  PIR5_COG3IF              : bit  absolute PIR5.4;
  PIR5_C8IF                : bit  absolute PIR5.3;
  PIR5_C7IF                : bit  absolute PIR5.2;
  PIR5_C6IF                : bit  absolute PIR5.1;
  PIR5_C5IF                : bit  absolute PIR5.0;
  PIR6                     : byte absolute $0016;
  PIR6_PWM12IF             : bit  absolute PIR6.3;
  PIR6_PWM11IF             : bit  absolute PIR6.2;
  PIR6_PWM6IF              : bit  absolute PIR6.1;
  PIR6_PWM5IF              : bit  absolute PIR6.0;
  TMR0                     : byte absolute $0017;
  TMR1L                    : byte absolute $0018;
  TMR1H                    : byte absolute $0019;
  T1CON                    : byte absolute $001A;
  T1CON_CS1                : bit  absolute T1CON.7;
  T1CON_CS0                : bit  absolute T1CON.6;
  T1CON_CKPS1              : bit  absolute T1CON.5;
  T1CON_CKPS0              : bit  absolute T1CON.4;
  T1CON_OSCEN              : bit  absolute T1CON.3;
  T1CON_nSYNC              : bit  absolute T1CON.2;
  T1CON_ON                 : bit  absolute T1CON.0;
  T1GCON                   : byte absolute $001B;
  T1GCON_GE                : bit  absolute T1GCON.7;
  T1GCON_GPOL              : bit  absolute T1GCON.6;
  T1GCON_GTM               : bit  absolute T1GCON.5;
  T1GCON_GSPM              : bit  absolute T1GCON.4;
  T1GCON_GGO_nDONE         : bit  absolute T1GCON.3;
  T1GCON_GVAL              : bit  absolute T1GCON.2;
  T1GCON_GSS1              : bit  absolute T1GCON.1;
  T1GCON_GSS0              : bit  absolute T1GCON.0;
  TMR3L                    : byte absolute $001C;
  TMR3H                    : byte absolute $001D;
  T3CON                    : byte absolute $001E;
  T3GCON                   : byte absolute $001F;
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
  TRISE_TRISE3             : bit  absolute TRISE.3;
  TRISE_TRISE2             : bit  absolute TRISE.2;
  TRISE_TRISE1             : bit  absolute TRISE.1;
  TRISE_TRISE0             : bit  absolute TRISE.0;
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
  PIE2_COGIE               : bit  absolute PIE2.4;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_C4IE                : bit  absolute PIE2.2;
  PIE2_C3IE                : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_COG2IE              : bit  absolute PIE3.5;
  PIE3_ZCDIE               : bit  absolute PIE3.4;
  PIE3_CLC4IE              : bit  absolute PIE3.3;
  PIE3_CLC3IE              : bit  absolute PIE3.2;
  PIE3_CLC2IE              : bit  absolute PIE3.1;
  PIE3_CLC1IE              : bit  absolute PIE3.0;
  PIE4                     : byte absolute $0094;
  PIE4_TMR8IE              : bit  absolute PIE4.6;
  PIE4_TMR5GIE             : bit  absolute PIE4.5;
  PIE4_TMR5IE              : bit  absolute PIE4.4;
  PIE4_TMR3GIE             : bit  absolute PIE4.3;
  PIE4_TMR3IE              : bit  absolute PIE4.2;
  PIE4_TMR6IE              : bit  absolute PIE4.1;
  PIE4_TMR4IE              : bit  absolute PIE4.0;
  PIE5                     : byte absolute $0095;
  PIE5_CCP8IE              : bit  absolute PIE5.7;
  PIE5_CCP7IE              : bit  absolute PIE5.6;
  PIE5_COG4IE              : bit  absolute PIE5.5;
  PIE5_COG3IE              : bit  absolute PIE5.4;
  PIE5_C8IE                : bit  absolute PIE5.3;
  PIE5_C7IE                : bit  absolute PIE5.2;
  PIE5_C6IE                : bit  absolute PIE5.1;
  PIE5_C5IE                : bit  absolute PIE5.0;
  PIE6                     : byte absolute $0096;
  PIE6_PWM12IE             : bit  absolute PIE6.3;
  PIE6_PWM11IE             : bit  absolute PIE6.2;
  PIE6_PWM6IE              : bit  absolute PIE6.1;
  PIE6_PWM5IE              : bit  absolute PIE6.0;
  OPTION_REG               : byte absolute $0097;
  OPTION_REG_nWPUEN        : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG        : bit  absolute OPTION_REG.6;
  OPTION_REG_TMR0CS        : bit  absolute OPTION_REG.5;
  OPTION_REG_TMR0SE        : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA           : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2           : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1           : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0           : bit  absolute OPTION_REG.0;
  PCON                     : byte absolute $0098;
  PCON_STKOVF              : bit  absolute PCON.7;
  PCON_STKUNF              : bit  absolute PCON.6;
  PCON_nRWDT               : bit  absolute PCON.4;
  PCON_nRMCLR              : bit  absolute PCON.3;
  PCON_nRI                 : bit  absolute PCON.2;
  PCON_nPOR                : bit  absolute PCON.1;
  PCON_nBOR                : bit  absolute PCON.0;
  WDTCON                   : byte absolute $0099;
  WDTCON_WDTPS4            : bit  absolute WDTCON.5;
  WDTCON_WDTPS3            : bit  absolute WDTCON.4;
  WDTCON_WDTPS2            : bit  absolute WDTCON.3;
  WDTCON_WDTPS1            : bit  absolute WDTCON.2;
  WDTCON_WDTPS0            : bit  absolute WDTCON.1;
  WDTCON_SWDTEN            : bit  absolute WDTCON.0;
  OSCTUNE                  : byte absolute $009A;
  OSCTUNE_TUN5             : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4             : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3             : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2             : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1             : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0             : bit  absolute OSCTUNE.0;
  OSCCON                   : byte absolute $009B;
  OSCCON_SPLLEN            : bit  absolute OSCCON.7;
  OSCCON_IRCF3             : bit  absolute OSCCON.6;
  OSCCON_IRCF2             : bit  absolute OSCCON.5;
  OSCCON_IRCF1             : bit  absolute OSCCON.4;
  OSCCON_IRCF0             : bit  absolute OSCCON.3;
  OSCCON_SCS1              : bit  absolute OSCCON.1;
  OSCCON_SCS0              : bit  absolute OSCCON.0;
  OSCSTAT                  : byte absolute $009C;
  OSCSTAT_SOSCR            : bit  absolute OSCSTAT.7;
  OSCSTAT_PLLR             : bit  absolute OSCSTAT.6;
  OSCSTAT_OSTS             : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFR           : bit  absolute OSCSTAT.4;
  OSCSTAT_HFIOFL           : bit  absolute OSCSTAT.3;
  OSCSTAT_MFIOFR           : bit  absolute OSCSTAT.2;
  OSCSTAT_LFIOFR           : bit  absolute OSCSTAT.1;
  OSCSTAT_HFIOFS           : bit  absolute OSCSTAT.0;
  BORCON                   : byte absolute $009D;
  BORCON_SBOREN            : bit  absolute BORCON.7;
  BORCON_BORFS             : bit  absolute BORCON.6;
  BORCON_BORRDY            : bit  absolute BORCON.0;
  FVRCON                   : byte absolute $009E;
  FVRCON_FVREN             : bit  absolute FVRCON.7;
  FVRCON_FVRRDY            : bit  absolute FVRCON.6;
  FVRCON_TSEN              : bit  absolute FVRCON.5;
  FVRCON_TSRNG             : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1           : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0           : bit  absolute FVRCON.2;
  FVRCON_ADFVR1            : bit  absolute FVRCON.1;
  FVRCON_ADFVR0            : bit  absolute FVRCON.0;
  ZCD1CON                  : byte absolute $009F;
  ZCD1CON_ZCD1EN           : bit  absolute ZCD1CON.7;
  ZCD1CON_ZCD1OUT          : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL          : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP         : bit  absolute ZCD1CON.1;
  ZCD1CON_ZCD1INTN         : bit  absolute ZCD1CON.0;
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
  LATE_LATE2               : bit  absolute LATE.2;
  LATE_LATE1               : bit  absolute LATE.1;
  LATE_LATE0               : bit  absolute LATE.0;
  CMOUT                    : byte absolute $0111;
  CMOUT_MC8OUT             : bit  absolute CMOUT.7;
  CMOUT_MC7OUT             : bit  absolute CMOUT.6;
  CMOUT_MC6OUT             : bit  absolute CMOUT.5;
  CMOUT_MC5OUT             : bit  absolute CMOUT.4;
  CMOUT_MC4OUT             : bit  absolute CMOUT.3;
  CMOUT_MC3OUT             : bit  absolute CMOUT.2;
  CMOUT_MC2OUT             : bit  absolute CMOUT.1;
  CMOUT_MC1OUT             : bit  absolute CMOUT.0;
  CM1CON0                  : byte absolute $0112;
  CM1CON0_OUT              : bit  absolute CM1CON0.6;
  CM1CON0_POL              : bit  absolute CM1CON0.4;
  CM1CON0_ZLF              : bit  absolute CM1CON0.3;
  CM1CON0_Reserved         : bit  absolute CM1CON0.2;
  CM1CON0_HYS              : bit  absolute CM1CON0.1;
  CM1CON0_SYNC             : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0113;
  CM1CON1_INTP             : bit  absolute CM1CON1.1;
  CM1CON1_INTN             : bit  absolute CM1CON1.0;
  CM1NSEL                  : byte absolute $0114;
  CM1NSEL_NCH3             : bit  absolute CM1NSEL.3;
  CM1NSEL_NCH2             : bit  absolute CM1NSEL.2;
  CM1NSEL_NCH1             : bit  absolute CM1NSEL.1;
  CM1NSEL_NCH0             : bit  absolute CM1NSEL.0;
  CM1PSEL                  : byte absolute $0115;
  CM1PSEL_PCH3             : bit  absolute CM1PSEL.3;
  CM1PSEL_PCH2             : bit  absolute CM1PSEL.2;
  CM1PSEL_PCH1             : bit  absolute CM1PSEL.1;
  CM1PSEL_PCH0             : bit  absolute CM1PSEL.0;
  CM2CON0                  : byte absolute $0116;
  CM2CON1                  : byte absolute $0117;
  CM2NSEL                  : byte absolute $0118;
  CM2PSEL                  : byte absolute $0119;
  CM3CON0                  : byte absolute $011A;
  CM3CON1                  : byte absolute $011B;
  CM3NSEL                  : byte absolute $011C;
  CM3PSEL                  : byte absolute $011D;
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
  ANSELC                   : byte absolute $018E;
  ANSELC_ANSC7             : bit  absolute ANSELC.7;
  ANSELC_ANSC6             : bit  absolute ANSELC.6;
  ANSELC_ANSC5             : bit  absolute ANSELC.5;
  ANSELC_ANSC4             : bit  absolute ANSELC.4;
  ANSELC_ANSC3             : bit  absolute ANSELC.3;
  ANSELC_ANSC2             : bit  absolute ANSELC.2;
  ANSELD                   : byte absolute $018F;
  ANSELD_ANSD7             : bit  absolute ANSELD.7;
  ANSELD_ANSD6             : bit  absolute ANSELD.6;
  ANSELD_ANSD5             : bit  absolute ANSELD.5;
  ANSELD_ANSD4             : bit  absolute ANSELD.4;
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
  WPUE_WPUE3               : bit  absolute WPUE.3;
  WPUE_WPUE2               : bit  absolute WPUE.2;
  WPUE_WPUE1               : bit  absolute WPUE.1;
  WPUE_WPUE0               : bit  absolute WPUE.0;
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
  MD3CON0                  : byte absolute $021B;
  MD3CON0_EN               : bit  absolute MD3CON0.7;
  MD3CON0_OPOL             : bit  absolute MD3CON0.4;
  MD3CON0_BIT              : bit  absolute MD3CON0.0;
  MD3CON1                  : byte absolute $021C;
  MD3CON1_CHPOL            : bit  absolute MD3CON1.5;
  MD3CON1_CHSYNC           : bit  absolute MD3CON1.4;
  MD3CON1_CLPOL            : bit  absolute MD3CON1.1;
  MD3CON1_CLSYNC           : bit  absolute MD3CON1.0;
  MD3SRC                   : byte absolute $021D;
  MD3SRC_MS4               : bit  absolute MD3SRC.4;
  MD3SRC_MS3               : bit  absolute MD3SRC.3;
  MD3SRC_MS2               : bit  absolute MD3SRC.2;
  MD3SRC_MS1               : bit  absolute MD3SRC.1;
  MD3SRC_MS0               : bit  absolute MD3SRC.0;
  MD3CARL                  : byte absolute $021E;
  MD3CARL_CL4              : bit  absolute MD3CARL.4;
  MD3CARL_CL3              : bit  absolute MD3CARL.3;
  MD3CARL_CL2              : bit  absolute MD3CARL.2;
  MD3CARL_CL1              : bit  absolute MD3CARL.1;
  MD3CARL_CL0              : bit  absolute MD3CARL.0;
  MD3CARH                  : byte absolute $021F;
  MD3CARH_CH4              : bit  absolute MD3CARH.4;
  MD3CARH_CH3              : bit  absolute MD3CARH.3;
  MD3CARH_CH2              : bit  absolute MD3CARH.2;
  MD3CARH_CH1              : bit  absolute MD3CARH.1;
  MD3CARH_CH0              : bit  absolute MD3CARH.0;
  ODCONA                   : byte absolute $028C;
  ODCONA_ODA7              : bit  absolute ODCONA.7;
  ODCONA_ODA6              : bit  absolute ODCONA.6;
  ODCONA_ODA5              : bit  absolute ODCONA.5;
  ODCONA_ODA4              : bit  absolute ODCONA.4;
  ODCONA_ODA3              : bit  absolute ODCONA.3;
  ODCONA_ODA2              : bit  absolute ODCONA.2;
  ODCONA_ODA1              : bit  absolute ODCONA.1;
  ODCONA_ODA0              : bit  absolute ODCONA.0;
  ODCONB                   : byte absolute $028D;
  ODCONB_ODB7              : bit  absolute ODCONB.7;
  ODCONB_ODB6              : bit  absolute ODCONB.6;
  ODCONB_ODB5              : bit  absolute ODCONB.5;
  ODCONB_ODB4              : bit  absolute ODCONB.4;
  ODCONB_ODB3              : bit  absolute ODCONB.3;
  ODCONB_ODB2              : bit  absolute ODCONB.2;
  ODCONB_ODB1              : bit  absolute ODCONB.1;
  ODCONB_ODB0              : bit  absolute ODCONB.0;
  ODCONC                   : byte absolute $028E;
  ODCONC_ODC7              : bit  absolute ODCONC.7;
  ODCONC_ODC6              : bit  absolute ODCONC.6;
  ODCONC_ODC5              : bit  absolute ODCONC.5;
  ODCONC_ODC4              : bit  absolute ODCONC.4;
  ODCONC_ODC3              : bit  absolute ODCONC.3;
  ODCONC_ODC2              : bit  absolute ODCONC.2;
  ODCONC_ODC1              : bit  absolute ODCONC.1;
  ODCONC_ODC0              : bit  absolute ODCONC.0;
  ODCOND                   : byte absolute $028F;
  ODCOND_ODD7              : bit  absolute ODCOND.7;
  ODCOND_ODD6              : bit  absolute ODCOND.6;
  ODCOND_ODD5              : bit  absolute ODCOND.5;
  ODCOND_ODD4              : bit  absolute ODCOND.4;
  ODCOND_ODD3              : bit  absolute ODCOND.3;
  ODCOND_ODD2              : bit  absolute ODCOND.2;
  ODCOND_ODD1              : bit  absolute ODCOND.1;
  ODCOND_ODD0              : bit  absolute ODCOND.0;
  ODCONE                   : byte absolute $0290;
  ODCONE_ODE2              : bit  absolute ODCONE.2;
  ODCONE_ODE1              : bit  absolute ODCONE.1;
  ODCONE_ODE0              : bit  absolute ODCONE.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_FMT              : bit  absolute CCP1CON.4;
  CCP1CON_MODE3            : bit  absolute CCP1CON.3;
  CCP1CON_MODE2            : bit  absolute CCP1CON.2;
  CCP1CON_MODE1            : bit  absolute CCP1CON.1;
  CCP1CON_MODE0            : bit  absolute CCP1CON.0;
  CCP1CAP                  : byte absolute $0294;
  CCPR2L                   : byte absolute $0295;
  CCPR2H                   : byte absolute $0296;
  CCP2CON                  : byte absolute $0297;
  CCP2CAP                  : byte absolute $0298;
  CCPR7L                   : byte absolute $0299;
  CCPR7H                   : byte absolute $029A;
  CCP7CON                  : byte absolute $029B;
  CCP7CAP                  : byte absolute $029C;
  CCPTMRS1                 : byte absolute $029E;
  CCPTMRS1_C8TSEL1         : bit  absolute CCPTMRS1.7;
  CCPTMRS1_C8TSEL0         : bit  absolute CCPTMRS1.6;
  CCPTMRS1_C7TSEL1         : bit  absolute CCPTMRS1.5;
  CCPTMRS1_C7TSEL0         : bit  absolute CCPTMRS1.4;
  CCPTMRS1_C2TSEL1         : bit  absolute CCPTMRS1.3;
  CCPTMRS1_C2TSEL0         : bit  absolute CCPTMRS1.2;
  CCPTMRS1_C1TSEL1         : bit  absolute CCPTMRS1.1;
  CCPTMRS1_C1TSEL0         : bit  absolute CCPTMRS1.0;
  CCPTMRS2                 : byte absolute $029F;
  CCPTMRS2_P10TSEL1        : bit  absolute CCPTMRS2.7;
  CCPTMRS2_P10TSEL0        : bit  absolute CCPTMRS2.6;
  CCPTMRS2_P9TSEL1         : bit  absolute CCPTMRS2.5;
  CCPTMRS2_P9TSEL0         : bit  absolute CCPTMRS2.4;
  CCPTMRS2_P4TSEL1         : bit  absolute CCPTMRS2.3;
  CCPTMRS2_P4TSEL0         : bit  absolute CCPTMRS2.2;
  CCPTMRS2_P3TSEL1         : bit  absolute CCPTMRS2.1;
  CCPTMRS2_P3TSEL0         : bit  absolute CCPTMRS2.0;
  SLRCONA                  : byte absolute $030C;
  SLRCONA_SLRA7            : bit  absolute SLRCONA.7;
  SLRCONA_SLRA6            : bit  absolute SLRCONA.6;
  SLRCONA_SLRA5            : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4            : bit  absolute SLRCONA.4;
  SLRCONA_SLRA3            : bit  absolute SLRCONA.3;
  SLRCONA_SLRA2            : bit  absolute SLRCONA.2;
  SLRCONA_SLRA1            : bit  absolute SLRCONA.1;
  SLRCONA_SLRA0            : bit  absolute SLRCONA.0;
  SLRCONB                  : byte absolute $030D;
  SLRCONB_SLRB7            : bit  absolute SLRCONB.7;
  SLRCONB_SLRB6            : bit  absolute SLRCONB.6;
  SLRCONB_SLRB5            : bit  absolute SLRCONB.5;
  SLRCONB_SLRB4            : bit  absolute SLRCONB.4;
  SLRCONB_SLRB3            : bit  absolute SLRCONB.3;
  SLRCONB_SLRB2            : bit  absolute SLRCONB.2;
  SLRCONB_SLRB1            : bit  absolute SLRCONB.1;
  SLRCONB_SLRB0            : bit  absolute SLRCONB.0;
  SLRCONC                  : byte absolute $030E;
  SLRCONC_SLRC7            : bit  absolute SLRCONC.7;
  SLRCONC_SLRC6            : bit  absolute SLRCONC.6;
  SLRCONC_SLRC5            : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4            : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3            : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2            : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1            : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0            : bit  absolute SLRCONC.0;
  SLRCOND                  : byte absolute $030F;
  SLRCOND_SLRD7            : bit  absolute SLRCOND.7;
  SLRCOND_SLRD6            : bit  absolute SLRCOND.6;
  SLRCOND_SLRD5            : bit  absolute SLRCOND.5;
  SLRCOND_SLRD4            : bit  absolute SLRCOND.4;
  SLRCOND_SLRD3            : bit  absolute SLRCOND.3;
  SLRCOND_SLRD2            : bit  absolute SLRCOND.2;
  SLRCOND_SLRD1            : bit  absolute SLRCOND.1;
  SLRCOND_SLRD0            : bit  absolute SLRCOND.0;
  SLRCONE                  : byte absolute $0310;
  SLRCONE_SLRE2            : bit  absolute SLRCONE.2;
  SLRCONE_SLRE1            : bit  absolute SLRCONE.1;
  SLRCONE_SLRE0            : bit  absolute SLRCONE.0;
  CCPR8L                   : byte absolute $0311;
  CCPR8H                   : byte absolute $0312;
  CCP8CON                  : byte absolute $0313;
  CCP8CAP                  : byte absolute $0314;
  MD1CON0                  : byte absolute $0315;
  MD1CON1                  : byte absolute $0316;
  MD1SRC                   : byte absolute $0317;
  MD1CARL                  : byte absolute $0318;
  MD1CARH                  : byte absolute $0319;
  MD2CON0                  : byte absolute $031B;
  MD2CON1                  : byte absolute $031C;
  MD2SRC                   : byte absolute $031D;
  MD2CARL                  : byte absolute $031E;
  MD2CARH                  : byte absolute $031F;
  INLVLA                   : byte absolute $038C;
  INLVLA_INLVA7            : bit  absolute INLVLA.7;
  INLVLA_INLVA6            : bit  absolute INLVLA.6;
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
  INLVLB_INLVB3            : bit  absolute INLVLB.3;
  INLVLB_INLVB2            : bit  absolute INLVLB.2;
  INLVLB_INLVB1            : bit  absolute INLVLB.1;
  INLVLB_INLVB0            : bit  absolute INLVLB.0;
  INLVLC                   : byte absolute $038E;
  INLVLC_INLVLC7           : bit  absolute INLVLC.7;
  INLVLC_INLVLC6           : bit  absolute INLVLC.6;
  INLVLC_INLVLC5           : bit  absolute INLVLC.5;
  INLVLC_INLVLC4           : bit  absolute INLVLC.4;
  INLVLC_INLVLC3           : bit  absolute INLVLC.3;
  INLVLC_INLVLC2           : bit  absolute INLVLC.2;
  INLVLC_INLVLC1           : bit  absolute INLVLC.1;
  INLVLC_INLVLC0           : bit  absolute INLVLC.0;
  INLVLD                   : byte absolute $038F;
  INLVLD_INLVLD7           : bit  absolute INLVLD.7;
  INLVLD_INLVLD6           : bit  absolute INLVLD.6;
  INLVLD_INLVLD5           : bit  absolute INLVLD.5;
  INLVLD_INLVLD4           : bit  absolute INLVLD.4;
  INLVLD_INLVLD3           : bit  absolute INLVLD.3;
  INLVLD_INLVLD2           : bit  absolute INLVLD.2;
  INLVLD_INLVLD1           : bit  absolute INLVLD.1;
  INLVLD_INLVLD0           : bit  absolute INLVLD.0;
  INLVE                    : byte absolute $0390;
  INLVE_INLVE3             : bit  absolute INLVE.3;
  INLVE_INLVE2             : bit  absolute INLVE.2;
  INLVE_INLVE1             : bit  absolute INLVE.1;
  INLVE_INLVE0             : bit  absolute INLVE.0;
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
  HIDRVB                   : byte absolute $040D;
  HIDRVB_HIDB1             : bit  absolute HIDRVB.1;
  HIDRVB_HIDB0             : bit  absolute HIDRVB.0;
  TMR5L                    : byte absolute $040F;
  TMR5H                    : byte absolute $0410;
  T5CON                    : byte absolute $0411;
  T5GCON                   : byte absolute $0412;
  T4TMR                    : byte absolute $0413;
  T4PR                     : byte absolute $0414;
  T4CON                    : byte absolute $0415;
  T4CON_CKPS2              : bit  absolute T4CON.6;
  T4CON_OUTPS3             : bit  absolute T4CON.3;
  T4CON_OUTPS2             : bit  absolute T4CON.2;
  T4CON_OUTPS1             : bit  absolute T4CON.1;
  T4CON_OUTPS0             : bit  absolute T4CON.0;
  T4HLT                    : byte absolute $0416;
  T4HLT_PSYNC              : bit  absolute T4HLT.7;
  T4HLT_CKPOL              : bit  absolute T4HLT.6;
  T4HLT_CKSYNC             : bit  absolute T4HLT.5;
  T4HLT_MODE4              : bit  absolute T4HLT.4;
  T4CLKCON                 : byte absolute $0417;
  T4CLKCON_CS3             : bit  absolute T4CLKCON.3;
  T4CLKCON_CS2             : bit  absolute T4CLKCON.2;
  T4RST                    : byte absolute $0418;
  T4RST_RSEL4              : bit  absolute T4RST.4;
  T4RST_RSEL3              : bit  absolute T4RST.3;
  T4RST_RSEL2              : bit  absolute T4RST.2;
  T4RST_RSEL1              : bit  absolute T4RST.1;
  T4RST_RSEL0              : bit  absolute T4RST.0;
  T6TMR                    : byte absolute $041A;
  T6PR                     : byte absolute $041B;
  T6CON                    : byte absolute $041C;
  T6HLT                    : byte absolute $041D;
  T6CLKCON                 : byte absolute $041E;
  T6RST                    : byte absolute $041F;
  ADRESL                   : byte absolute $048E;
  ADRESH                   : byte absolute $048F;
  ADCON0                   : byte absolute $0490;
  ADCON0_CHS5              : bit  absolute ADCON0.7;
  ADCON0_CHS4              : bit  absolute ADCON0.6;
  ADCON0_CHS3              : bit  absolute ADCON0.5;
  ADCON0_CHS2              : bit  absolute ADCON0.4;
  ADCON0_CHS1              : bit  absolute ADCON0.3;
  ADCON0_CHS0              : bit  absolute ADCON0.2;
  ADCON0_GO                : bit  absolute ADCON0.1;
  ADCON0_ADON              : bit  absolute ADCON0.0;
  ADCON1                   : byte absolute $0491;
  ADCON1_ADFM              : bit  absolute ADCON1.7;
  ADCON1_ADCS2             : bit  absolute ADCON1.6;
  ADCON1_ADCS1             : bit  absolute ADCON1.5;
  ADCON1_ADCS0             : bit  absolute ADCON1.4;
  ADCON1_ADNREF            : bit  absolute ADCON1.2;
  ADCON1_ADPREF1           : bit  absolute ADCON1.1;
  ADCON1_ADPREF0           : bit  absolute ADCON1.0;
  ADCON2                   : byte absolute $0492;
  ADCON2_TRIGSEL5          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL4          : bit  absolute ADCON2.4;
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.3;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.2;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.1;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.0;
  T2TMR                    : byte absolute $0493;
  T2PR                     : byte absolute $0494;
  T2CON                    : byte absolute $0495;
  T2HLT                    : byte absolute $0496;
  T2CLKCON                 : byte absolute $0497;
  T2RST                    : byte absolute $0498;
  T8TMR                    : byte absolute $049A;
  T8PR                     : byte absolute $049B;
  T8CON                    : byte absolute $049C;
  T8HLT                    : byte absolute $049D;
  T8CLKCON                 : byte absolute $049E;
  T8RST                    : byte absolute $049F;
  OPA1NCHS                 : byte absolute $050F;
  OPA1PCHS                 : byte absolute $0510;
  OPA1CON                  : byte absolute $0511;
  OPA1CON_UG               : bit  absolute OPA1CON.4;
  OPA1CON_ORPOL            : bit  absolute OPA1CON.2;
  OPA1CON_ORM1             : bit  absolute OPA1CON.1;
  OPA1CON_ORM0             : bit  absolute OPA1CON.0;
  OPA1ORS                  : byte absolute $0512;
  OPA2NCHS                 : byte absolute $0513;
  OPA2PCHS                 : byte absolute $0514;
  OPA2CON                  : byte absolute $0515;
  OPA2ORS                  : byte absolute $0516;
  OPA3NCHS                 : byte absolute $0517;
  OPA3PCHS                 : byte absolute $0518;
  OPA3CON                  : byte absolute $0519;
  OPA3CON_SP               : bit  absolute OPA3CON.6;
  OPA3ORS                  : byte absolute $051A;
  OPA4NCHS                 : byte absolute $051B;
  OPA4PCHS                 : byte absolute $051C;
  OPA4CON                  : byte absolute $051D;
  OPA4ORS                  : byte absolute $051E;
  DACLD                    : byte absolute $058D;
  DACLD_DAC6LD             : bit  absolute DACLD.5;
  DACLD_DAC5LD             : bit  absolute DACLD.4;
  DACLD_DAC2LD             : bit  absolute DACLD.1;
  DACLD_DAC1LD             : bit  absolute DACLD.0;
  DAC1CON0                 : byte absolute $058E;
  DAC1CON0_FM              : bit  absolute DAC1CON0.6;
  DAC1CON0_OE1             : bit  absolute DAC1CON0.5;
  DAC1CON0_OE2             : bit  absolute DAC1CON0.4;
  DAC1CON0_PSS1            : bit  absolute DAC1CON0.3;
  DAC1CON0_PSS0            : bit  absolute DAC1CON0.2;
  DAC1CON0_NSS1            : bit  absolute DAC1CON0.1;
  DAC1CON0_NSS0            : bit  absolute DAC1CON0.0;
  DAC1REFL                 : byte absolute $058F;
  DAC1REFL_REF7            : bit  absolute DAC1REFL.7;
  DAC1REFL_REF6            : bit  absolute DAC1REFL.6;
  DAC1REFL_REF5            : bit  absolute DAC1REFL.5;
  DAC1REFL_REF4            : bit  absolute DAC1REFL.4;
  DAC1REFL_REF3            : bit  absolute DAC1REFL.3;
  DAC1REFL_REF2            : bit  absolute DAC1REFL.2;
  DAC1REFL_REF1            : bit  absolute DAC1REFL.1;
  DAC1REFL_REF0            : bit  absolute DAC1REFL.0;
  DAC1REFH                 : byte absolute $0590;
  DAC1REFH_REF15           : bit  absolute DAC1REFH.7;
  DAC1REFH_REF14           : bit  absolute DAC1REFH.6;
  DAC1REFH_REF13           : bit  absolute DAC1REFH.5;
  DAC1REFH_REF12           : bit  absolute DAC1REFH.4;
  DAC1REFH_REF11           : bit  absolute DAC1REFH.3;
  DAC1REFH_REF10           : bit  absolute DAC1REFH.2;
  DAC1REFH_REF9            : bit  absolute DAC1REFH.1;
  DAC1REFH_REF8            : bit  absolute DAC1REFH.0;
  DAC2CON0                 : byte absolute $0591;
  DAC2REFL                 : byte absolute $0592;
  DAC2REFH                 : byte absolute $0593;
  DAC3CON0                 : byte absolute $0594;
  DAC3REF                  : byte absolute $0595;
  DAC3REF_DACR4            : bit  absolute DAC3REF.4;
  DAC3REF_DACR3            : bit  absolute DAC3REF.3;
  DAC3REF_DACR2            : bit  absolute DAC3REF.2;
  DAC3REF_DACR1            : bit  absolute DAC3REF.1;
  DAC3REF_DACR0            : bit  absolute DAC3REF.0;
  DAC4CON0                 : byte absolute $0596;
  DAC4REF                  : byte absolute $0597;
  DAC5CON0                 : byte absolute $0598;
  DAC5REFL                 : byte absolute $0599;
  DAC5REFH                 : byte absolute $059A;
  DAC6CON0                 : byte absolute $059B;
  DAC6REFL                 : byte absolute $059C;
  DAC6REFH                 : byte absolute $059D;
  DAC7CON0                 : byte absolute $059E;
  DAC7REF                  : byte absolute $059F;
  DAC8CON0                 : byte absolute $060C;
  DAC8REF                  : byte absolute $060D;
  PRG4RTSS                 : byte absolute $060E;
  PRG4FTSS                 : byte absolute $060F;
  PRG4INS                  : byte absolute $0610;
  PRG4CON0                 : byte absolute $0611;
  PRG4CON0_FEDG            : bit  absolute PRG4CON0.5;
  PRG4CON0_REDG            : bit  absolute PRG4CON0.4;
  PRG4CON0_OS              : bit  absolute PRG4CON0.1;
  PRG4CON1                 : byte absolute $0612;
  PRG4CON1_RDY             : bit  absolute PRG4CON1.2;
  PRG4CON1_FPOL            : bit  absolute PRG4CON1.1;
  PRG4CON1_RPOL            : bit  absolute PRG4CON1.0;
  PRG4CON2                 : byte absolute $0613;
  PRG4CON2_ISET4           : bit  absolute PRG4CON2.4;
  PRG4CON2_ISET3           : bit  absolute PRG4CON2.3;
  PRG4CON2_ISET2           : bit  absolute PRG4CON2.2;
  PRG4CON2_ISET1           : bit  absolute PRG4CON2.1;
  PRG4CON2_ISET0           : bit  absolute PRG4CON2.0;
  PWM3DCL                  : byte absolute $0614;
  PWM3DCL_DC1              : bit  absolute PWM3DCL.7;
  PWM3DCL_DC0              : bit  absolute PWM3DCL.6;
  PWM3DCH                  : byte absolute $0615;
  PWM3CON                  : byte absolute $0616;
  PWM4DCL                  : byte absolute $0617;
  PWM4DCH                  : byte absolute $0618;
  PWM4CON                  : byte absolute $0619;
  PWM9DCL                  : byte absolute $061A;
  PWM9DCH                  : byte absolute $061B;
  PWM9CON                  : byte absolute $061C;
  PWM10DCL                 : byte absolute $061D;
  PWM10DCH                 : byte absolute $061E;
  PWM10CON                 : byte absolute $061F;
  COG1PHR                  : byte absolute $068D;
  COG1PHR_PHR5             : bit  absolute COG1PHR.5;
  COG1PHR_PHR4             : bit  absolute COG1PHR.4;
  COG1PHR_PHR3             : bit  absolute COG1PHR.3;
  COG1PHR_PHR2             : bit  absolute COG1PHR.2;
  COG1PHR_PHR1             : bit  absolute COG1PHR.1;
  COG1PHR_PHR0             : bit  absolute COG1PHR.0;
  COG1PHF                  : byte absolute $068E;
  COG1PHF_PHF5             : bit  absolute COG1PHF.5;
  COG1PHF_PHF4             : bit  absolute COG1PHF.4;
  COG1PHF_PHF3             : bit  absolute COG1PHF.3;
  COG1PHF_PHF2             : bit  absolute COG1PHF.2;
  COG1PHF_PHF1             : bit  absolute COG1PHF.1;
  COG1PHF_PHF0             : bit  absolute COG1PHF.0;
  COG1BLKR                 : byte absolute $068F;
  COG1BLKR_BLKR5           : bit  absolute COG1BLKR.5;
  COG1BLKR_BLKR4           : bit  absolute COG1BLKR.4;
  COG1BLKR_BLKR3           : bit  absolute COG1BLKR.3;
  COG1BLKR_BLKR2           : bit  absolute COG1BLKR.2;
  COG1BLKR_BLKR1           : bit  absolute COG1BLKR.1;
  COG1BLKR_BLKR0           : bit  absolute COG1BLKR.0;
  COG1BLKF                 : byte absolute $0690;
  COG1BLKF_BLKF5           : bit  absolute COG1BLKF.5;
  COG1BLKF_BLKF4           : bit  absolute COG1BLKF.4;
  COG1BLKF_BLKF3           : bit  absolute COG1BLKF.3;
  COG1BLKF_BLKF2           : bit  absolute COG1BLKF.2;
  COG1BLKF_BLKF1           : bit  absolute COG1BLKF.1;
  COG1BLKF_BLKF0           : bit  absolute COG1BLKF.0;
  COG1DBR                  : byte absolute $0691;
  COG1DBR_DBR5             : bit  absolute COG1DBR.5;
  COG1DBR_DBR4             : bit  absolute COG1DBR.4;
  COG1DBR_DBR3             : bit  absolute COG1DBR.3;
  COG1DBR_DBR2             : bit  absolute COG1DBR.2;
  COG1DBR_DBR1             : bit  absolute COG1DBR.1;
  COG1DBR_DBR0             : bit  absolute COG1DBR.0;
  COG1DBF                  : byte absolute $0692;
  COG1DBF_DBF5             : bit  absolute COG1DBF.5;
  COG1DBF_DBF4             : bit  absolute COG1DBF.4;
  COG1DBF_DBF3             : bit  absolute COG1DBF.3;
  COG1DBF_DBF2             : bit  absolute COG1DBF.2;
  COG1DBF_DBF1             : bit  absolute COG1DBF.1;
  COG1DBF_DBF0             : bit  absolute COG1DBF.0;
  COG1CON0                 : byte absolute $0693;
  COG1CON0_LD              : bit  absolute COG1CON0.6;
  COG1CON0_MD2             : bit  absolute COG1CON0.2;
  COG1CON0_MD1             : bit  absolute COG1CON0.1;
  COG1CON0_MD0             : bit  absolute COG1CON0.0;
  COG1CON1                 : byte absolute $0694;
  COG1CON1_RDBS            : bit  absolute COG1CON1.7;
  COG1CON1_FDBS            : bit  absolute COG1CON1.6;
  COG1CON1_POLD            : bit  absolute COG1CON1.3;
  COG1CON1_POLC            : bit  absolute COG1CON1.2;
  COG1CON1_POLB            : bit  absolute COG1CON1.1;
  COG1CON1_POLA            : bit  absolute COG1CON1.0;
  COG1RIS0                 : byte absolute $0695;
  COG1RIS0_RIS7            : bit  absolute COG1RIS0.7;
  COG1RIS0_RIS6            : bit  absolute COG1RIS0.6;
  COG1RIS0_RIS5            : bit  absolute COG1RIS0.5;
  COG1RIS0_RIS4            : bit  absolute COG1RIS0.4;
  COG1RIS0_RIS3            : bit  absolute COG1RIS0.3;
  COG1RIS0_RIS2            : bit  absolute COG1RIS0.2;
  COG1RIS0_RIS1            : bit  absolute COG1RIS0.1;
  COG1RIS0_RIS0            : bit  absolute COG1RIS0.0;
  COG1RIS1                 : byte absolute $0696;
  COG1RIS1_RIS15           : bit  absolute COG1RIS1.7;
  COG1RIS1_RIS14           : bit  absolute COG1RIS1.6;
  COG1RIS1_RIS13           : bit  absolute COG1RIS1.5;
  COG1RIS1_RIS12           : bit  absolute COG1RIS1.4;
  COG1RIS1_RIS11           : bit  absolute COG1RIS1.3;
  COG1RIS1_RIS10           : bit  absolute COG1RIS1.2;
  COG1RIS1_RIS9            : bit  absolute COG1RIS1.1;
  COG1RIS1_RIS8            : bit  absolute COG1RIS1.0;
  COG1RSIM0                : byte absolute $0697;
  COG1RSIM0_RSIM7          : bit  absolute COG1RSIM0.7;
  COG1RSIM0_RSIM6          : bit  absolute COG1RSIM0.6;
  COG1RSIM0_RSIM5          : bit  absolute COG1RSIM0.5;
  COG1RSIM0_RSIM4          : bit  absolute COG1RSIM0.4;
  COG1RSIM0_RSIM3          : bit  absolute COG1RSIM0.3;
  COG1RSIM0_RSIM2          : bit  absolute COG1RSIM0.2;
  COG1RSIM0_RSIM1          : bit  absolute COG1RSIM0.1;
  COG1RSIM0_RSIM0          : bit  absolute COG1RSIM0.0;
  COG1RSIM1                : byte absolute $0698;
  COG1RSIM1_RSIM15         : bit  absolute COG1RSIM1.7;
  COG1RSIM1_RSIM14         : bit  absolute COG1RSIM1.6;
  COG1RSIM1_RSIM13         : bit  absolute COG1RSIM1.5;
  COG1RSIM1_RSIM12         : bit  absolute COG1RSIM1.4;
  COG1RSIM1_RSIM11         : bit  absolute COG1RSIM1.3;
  COG1RSIM1_RSIM10         : bit  absolute COG1RSIM1.2;
  COG1RSIM1_RSIM9          : bit  absolute COG1RSIM1.1;
  COG1RSIM1_RSIM8          : bit  absolute COG1RSIM1.0;
  COG1FIS0                 : byte absolute $0699;
  COG1FIS0_FIS7            : bit  absolute COG1FIS0.7;
  COG1FIS0_FIS6            : bit  absolute COG1FIS0.6;
  COG1FIS0_FIS5            : bit  absolute COG1FIS0.5;
  COG1FIS0_FIS4            : bit  absolute COG1FIS0.4;
  COG1FIS0_FIS3            : bit  absolute COG1FIS0.3;
  COG1FIS0_FIS2            : bit  absolute COG1FIS0.2;
  COG1FIS0_FIS1            : bit  absolute COG1FIS0.1;
  COG1FIS0_FIS0            : bit  absolute COG1FIS0.0;
  COG1FIS1                 : byte absolute $069A;
  COG1FIS1_FIS15           : bit  absolute COG1FIS1.7;
  COG1FIS1_FIS14           : bit  absolute COG1FIS1.6;
  COG1FIS1_FIS13           : bit  absolute COG1FIS1.5;
  COG1FIS1_FIS12           : bit  absolute COG1FIS1.4;
  COG1FIS1_FIS11           : bit  absolute COG1FIS1.3;
  COG1FIS1_FIS10           : bit  absolute COG1FIS1.2;
  COG1FIS1_FIS9            : bit  absolute COG1FIS1.1;
  COG1FIS1_FIS8            : bit  absolute COG1FIS1.0;
  COG1FSIM0                : byte absolute $069B;
  COG1FSIM0_FSIM7          : bit  absolute COG1FSIM0.7;
  COG1FSIM0_FSIM6          : bit  absolute COG1FSIM0.6;
  COG1FSIM0_FSIM5          : bit  absolute COG1FSIM0.5;
  COG1FSIM0_FSIM4          : bit  absolute COG1FSIM0.4;
  COG1FSIM0_FSIM3          : bit  absolute COG1FSIM0.3;
  COG1FSIM0_FSIM2          : bit  absolute COG1FSIM0.2;
  COG1FSIM0_FSIM1          : bit  absolute COG1FSIM0.1;
  COG1FSIM0_FSIM0          : bit  absolute COG1FSIM0.0;
  COG1FSIM1                : byte absolute $069C;
  COG1FSIM1_FSIM15         : bit  absolute COG1FSIM1.7;
  COG1FSIM1_FSIM14         : bit  absolute COG1FSIM1.6;
  COG1FSIM1_FSIM13         : bit  absolute COG1FSIM1.5;
  COG1FSIM1_FSIM12         : bit  absolute COG1FSIM1.4;
  COG1FSIM1_FSIM11         : bit  absolute COG1FSIM1.3;
  COG1FSIM1_FSIM10         : bit  absolute COG1FSIM1.2;
  COG1FSIM1_FSIM9          : bit  absolute COG1FSIM1.1;
  COG1FSIM1_FSIM8          : bit  absolute COG1FSIM1.0;
  COG1ASD0                 : byte absolute $069D;
  COG1ASD0_ASE             : bit  absolute COG1ASD0.7;
  COG1ASD0_ASREN           : bit  absolute COG1ASD0.6;
  COG1ASD0_ASDBD1          : bit  absolute COG1ASD0.5;
  COG1ASD0_ASDBD0          : bit  absolute COG1ASD0.4;
  COG1ASD0_ASDAC1          : bit  absolute COG1ASD0.3;
  COG1ASD0_ASDAC0          : bit  absolute COG1ASD0.2;
  COG1ASD1                 : byte absolute $069E;
  COG1ASD1_AS7E            : bit  absolute COG1ASD1.7;
  COG1ASD1_AS6E            : bit  absolute COG1ASD1.6;
  COG1ASD1_AS5E            : bit  absolute COG1ASD1.5;
  COG1ASD1_AS4E            : bit  absolute COG1ASD1.4;
  COG1ASD1_AS3E            : bit  absolute COG1ASD1.3;
  COG1ASD1_AS2E            : bit  absolute COG1ASD1.2;
  COG1ASD1_AS1E            : bit  absolute COG1ASD1.1;
  COG1ASD1_AS0E            : bit  absolute COG1ASD1.0;
  COG1STR                  : byte absolute $069F;
  COG1STR_SDATD            : bit  absolute COG1STR.7;
  COG1STR_SDATC            : bit  absolute COG1STR.6;
  COG1STR_SDATB            : bit  absolute COG1STR.5;
  COG1STR_SDATA            : bit  absolute COG1STR.4;
  COG1STR_STRD             : bit  absolute COG1STR.3;
  COG1STR_STRC             : bit  absolute COG1STR.2;
  COG1STR_STRB             : bit  absolute COG1STR.1;
  COG1STR_STRA             : bit  absolute COG1STR.0;
  COG2PHR                  : byte absolute $070D;
  COG2PHF                  : byte absolute $070E;
  COG2BLKR                 : byte absolute $070F;
  COG2BLKF                 : byte absolute $0710;
  COG2DBR                  : byte absolute $0711;
  COG2DBF                  : byte absolute $0712;
  COG2CON0                 : byte absolute $0713;
  COG2CON1                 : byte absolute $0714;
  COG2RIS0                 : byte absolute $0715;
  COG2RIS1                 : byte absolute $0716;
  COG2RSIM0                : byte absolute $0717;
  COG2RSIM1                : byte absolute $0718;
  COG2FIS0                 : byte absolute $0719;
  COG2FIS1                 : byte absolute $071A;
  COG2FSIM0                : byte absolute $071B;
  COG2FSIM1                : byte absolute $071C;
  COG2ASD0                 : byte absolute $071D;
  COG2ASD1                 : byte absolute $071E;
  COG2STR                  : byte absolute $071F;
  PRG1RTSS                 : byte absolute $078E;
  PRG1FTSS                 : byte absolute $078F;
  PRG1INS                  : byte absolute $0790;
  PRG1CON0                 : byte absolute $0791;
  PRG1CON1                 : byte absolute $0792;
  PRG1CON2                 : byte absolute $0793;
  PRG2RTSS                 : byte absolute $0794;
  PRG2FTSS                 : byte absolute $0795;
  PRG2INS                  : byte absolute $0796;
  PRG2CON0                 : byte absolute $0797;
  PRG2CON1                 : byte absolute $0798;
  PRG2CON2                 : byte absolute $0799;
  PRG3RTSS                 : byte absolute $079A;
  PRG3FTSS                 : byte absolute $079B;
  PRG3INS                  : byte absolute $079C;
  PRG3CON0                 : byte absolute $079D;
  PRG3CON1                 : byte absolute $079E;
  PRG3CON2                 : byte absolute $079F;
  COG3PHR                  : byte absolute $080D;
  COG3PHF                  : byte absolute $080E;
  COG3BLKR                 : byte absolute $080F;
  COG3BLKF                 : byte absolute $0810;
  COG3DBR                  : byte absolute $0811;
  COG3DBF                  : byte absolute $0812;
  COG3CON0                 : byte absolute $0813;
  COG3CON1                 : byte absolute $0814;
  COG3RIS0                 : byte absolute $0815;
  COG3RIS1                 : byte absolute $0816;
  COG3RSIM0                : byte absolute $0817;
  COG3RSIM1                : byte absolute $0818;
  COG3FIS0                 : byte absolute $0819;
  COG3FIS1                 : byte absolute $081A;
  COG3FSIM0                : byte absolute $081B;
  COG3FSIM1                : byte absolute $081C;
  COG3ASD0                 : byte absolute $081D;
  COG3ASD1                 : byte absolute $081E;
  COG3STR                  : byte absolute $081F;
  COG4PHR                  : byte absolute $088D;
  COG4PHF                  : byte absolute $088E;
  COG4BLKR                 : byte absolute $088F;
  COG4BLKF                 : byte absolute $0890;
  COG4DBR                  : byte absolute $0891;
  COG4DBF                  : byte absolute $0892;
  COG4CON0                 : byte absolute $0893;
  COG4CON1                 : byte absolute $0894;
  COG4RIS0                 : byte absolute $0895;
  COG4RIS1                 : byte absolute $0896;
  COG4RSIM0                : byte absolute $0897;
  COG4RSIM1                : byte absolute $0898;
  COG4FIS0                 : byte absolute $0899;
  COG4FIS1                 : byte absolute $089A;
  COG4FSIM0                : byte absolute $089B;
  COG4FSIM1                : byte absolute $089C;
  COG4ASD0                 : byte absolute $089D;
  COG4ASD1                 : byte absolute $089E;
  COG4STR                  : byte absolute $089F;
  CM4CON0                  : byte absolute $090C;
  CM4CON1                  : byte absolute $090D;
  CM4NSEL                  : byte absolute $090E;
  CM4PSEL                  : byte absolute $090F;
  CM5CON0                  : byte absolute $0910;
  CM5CON1                  : byte absolute $0911;
  CM5NSEL                  : byte absolute $0912;
  CM5PSEL                  : byte absolute $0913;
  CM6CON0                  : byte absolute $0914;
  CM6CON1                  : byte absolute $0915;
  CM6NSEL                  : byte absolute $0916;
  CM6PSEL                  : byte absolute $0917;
  CM7CON0                  : byte absolute $0918;
  CM7CON1                  : byte absolute $0919;
  CM7NSEL                  : byte absolute $091A;
  CM7PSEL                  : byte absolute $091B;
  CM8CON0                  : byte absolute $091C;
  CM8CON1                  : byte absolute $091D;
  CM8NSEL                  : byte absolute $091E;
  CM8PSEL                  : byte absolute $091F;
  MD4CON0                  : byte absolute $0D1B;
  MD4CON1                  : byte absolute $0D1C;
  MD4SRC                   : byte absolute $0D1D;
  MD4CARL                  : byte absolute $0D1E;
  MD4CARH                  : byte absolute $0D1F;
  PWMEN                    : byte absolute $0D8E;
  PWMEN_MPWM12EN           : bit  absolute PWMEN.3;
  PWMEN_MPWM11EN           : bit  absolute PWMEN.2;
  PWMEN_MPWM6EN            : bit  absolute PWMEN.1;
  PWMEN_MPWM5EN            : bit  absolute PWMEN.0;
  PWMLD                    : byte absolute $0D8F;
  PWMLD_MPWM12LD           : bit  absolute PWMLD.3;
  PWMLD_MPWM11LD           : bit  absolute PWMLD.2;
  PWMLD_MPWM6LD            : bit  absolute PWMLD.1;
  PWMLD_MPWM5LD            : bit  absolute PWMLD.0;
  PWMOUT                   : byte absolute $0D90;
  PWMOUT_MPWM12OUT         : bit  absolute PWMOUT.3;
  PWMOUT_MPWM11OUT         : bit  absolute PWMOUT.2;
  PWMOUT_MPWM6OUT          : bit  absolute PWMOUT.1;
  PWMOUT_MPWM5OUT          : bit  absolute PWMOUT.0;
  PWM5PHL                  : byte absolute $0D91;
  PWM5PHH                  : byte absolute $0D92;
  PWM5DCL                  : byte absolute $0D93;
  PWM5DCH                  : byte absolute $0D94;
  PWM5PRL                  : byte absolute $0D95;
  PWM5PRH                  : byte absolute $0D96;
  PWM5OFL                  : byte absolute $0D97;
  PWM5OFH                  : byte absolute $0D98;
  PWM5TMRL                 : byte absolute $0D99;
  PWM5TMRH                 : byte absolute $0D9A;
  PWM5CON                  : byte absolute $0D9B;
  PWM5INTE                 : byte absolute $0D9C;
  PWM5INTE_OFIE            : bit  absolute PWM5INTE.3;
  PWM5INTE_PHIE            : bit  absolute PWM5INTE.2;
  PWM5INTE_DCIE            : bit  absolute PWM5INTE.1;
  PWM5INTE_PRIE            : bit  absolute PWM5INTE.0;
  PWM5INTF                 : byte absolute $0D9D;
  PWM5INTF_OFIF            : bit  absolute PWM5INTF.3;
  PWM5INTF_PHIF            : bit  absolute PWM5INTF.2;
  PWM5INTF_DCIF            : bit  absolute PWM5INTF.1;
  PWM5INTF_PRIF            : bit  absolute PWM5INTF.0;
  PWM5CLKCON               : byte absolute $0D9E;
  PWM5LDCON                : byte absolute $0D9F;
  PWM5LDCON_LDA            : bit  absolute PWM5LDCON.7;
  PWM5LDCON_LDT            : bit  absolute PWM5LDCON.6;
  PWM5LDCON_LDS1           : bit  absolute PWM5LDCON.1;
  PWM5LDCON_LDS0           : bit  absolute PWM5LDCON.0;
  PWM5OFCON                : byte absolute $0DA0;
  PWM5OFCON_OFM1           : bit  absolute PWM5OFCON.6;
  PWM5OFCON_OFM0           : bit  absolute PWM5OFCON.5;
  PWM5OFCON_OFO            : bit  absolute PWM5OFCON.4;
  PWM5OFCON_OFS1           : bit  absolute PWM5OFCON.1;
  PWM5OFCON_OFS0           : bit  absolute PWM5OFCON.0;
  PWM6PHL                  : byte absolute $0DA1;
  PWM6PHH                  : byte absolute $0DA2;
  PWM6DCL                  : byte absolute $0DA3;
  PWM6DCH                  : byte absolute $0DA4;
  PWM6PRL                  : byte absolute $0DA5;
  PWM6PRH                  : byte absolute $0DA6;
  PWM6OFL                  : byte absolute $0DA7;
  PWM6OFH                  : byte absolute $0DA8;
  PWM6TMRL                 : byte absolute $0DA9;
  PWM6TMRH                 : byte absolute $0DAA;
  PWM6CON                  : byte absolute $0DAB;
  PWM6INTE                 : byte absolute $0DAC;
  PWM6INTF                 : byte absolute $0DAD;
  PWM6CLKCON               : byte absolute $0DAE;
  PWM6LDCON                : byte absolute $0DAF;
  PWM6OFCON                : byte absolute $0DB0;
  PWM11PHL                 : byte absolute $0DB1;
  PWM11PHH                 : byte absolute $0DB2;
  PWM11DCL                 : byte absolute $0DB3;
  PWM11DCH                 : byte absolute $0DB4;
  PWM11PRL                 : byte absolute $0DB5;
  PWM11PRH                 : byte absolute $0DB6;
  PWM11OFL                 : byte absolute $0DB7;
  PWM11OFH                 : byte absolute $0DB8;
  PWM11TMRL                : byte absolute $0DB9;
  PWM11TMRH                : byte absolute $0DBA;
  PWM11CON                 : byte absolute $0DBB;
  PWM11INTE                : byte absolute $0DBC;
  PWM11INTF                : byte absolute $0DBD;
  PWM11CLKCON              : byte absolute $0DBE;
  PWM11LDCON               : byte absolute $0DBF;
  PWM11OFCON               : byte absolute $0DC0;
  PWM12PHL                 : byte absolute $0DC1;
  PWM12PHH                 : byte absolute $0DC2;
  PWM12DCL                 : byte absolute $0DC3;
  PWM12DCH                 : byte absolute $0DC4;
  PWM12PRL                 : byte absolute $0DC5;
  PWM12PRH                 : byte absolute $0DC6;
  PWM12OFL                 : byte absolute $0DC7;
  PWM12OFH                 : byte absolute $0DC8;
  PWM12TMRL                : byte absolute $0DC9;
  PWM12TMRH                : byte absolute $0DCA;
  PWM12CON                 : byte absolute $0DCB;
  PWM12INTE                : byte absolute $0DCC;
  PWM12INTF                : byte absolute $0DCD;
  PWM12CLKCON              : byte absolute $0DCE;
  PWM12LDCON               : byte absolute $0DCF;
  PWM12OFCON               : byte absolute $0DD0;
  PPSLOCK                  : byte absolute $0E0C;
  PPSLOCK_PPSLOCKED        : bit  absolute PPSLOCK.0;
  INTPPS                   : byte absolute $0E0D;
  INTPPS_INTPPS5           : bit  absolute INTPPS.5;
  INTPPS_INTPPS4           : bit  absolute INTPPS.4;
  INTPPS_INTPPS3           : bit  absolute INTPPS.3;
  INTPPS_INTPPS2           : bit  absolute INTPPS.2;
  INTPPS_INTPPS1           : bit  absolute INTPPS.1;
  INTPPS_INTPPS0           : bit  absolute INTPPS.0;
  T0CKIPPS                 : byte absolute $0E0E;
  T0CKIPPS_T0CKIPPS5       : bit  absolute T0CKIPPS.5;
  T0CKIPPS_T0CKIPPS4       : bit  absolute T0CKIPPS.4;
  T0CKIPPS_T0CKIPPS3       : bit  absolute T0CKIPPS.3;
  T0CKIPPS_T0CKIPPS2       : bit  absolute T0CKIPPS.2;
  T0CKIPPS_T0CKIPPS1       : bit  absolute T0CKIPPS.1;
  T0CKIPPS_T0CKIPPS0       : bit  absolute T0CKIPPS.0;
  T1CKIPPS                 : byte absolute $0E0F;
  T1CKIPPS_T1CKIPPS5       : bit  absolute T1CKIPPS.5;
  T1CKIPPS_T1CKIPPS4       : bit  absolute T1CKIPPS.4;
  T1CKIPPS_T1CKIPPS3       : bit  absolute T1CKIPPS.3;
  T1CKIPPS_T1CKIPPS2       : bit  absolute T1CKIPPS.2;
  T1CKIPPS_T1CKIPPS1       : bit  absolute T1CKIPPS.1;
  T1CKIPPS_T1CKIPPS0       : bit  absolute T1CKIPPS.0;
  T1GPPS                   : byte absolute $0E10;
  T1GPPS_T1GPPS5           : bit  absolute T1GPPS.5;
  T1GPPS_T1GPPS4           : bit  absolute T1GPPS.4;
  T1GPPS_T1GPPS3           : bit  absolute T1GPPS.3;
  T1GPPS_T1GPPS2           : bit  absolute T1GPPS.2;
  T1GPPS_T1GPPS1           : bit  absolute T1GPPS.1;
  T1GPPS_T1GPPS0           : bit  absolute T1GPPS.0;
  T3CKIPPS                 : byte absolute $0E11;
  T3CKIPPS_T3CKIPPS5       : bit  absolute T3CKIPPS.5;
  T3CKIPPS_T3CKIPPS4       : bit  absolute T3CKIPPS.4;
  T3CKIPPS_T3CKIPPS3       : bit  absolute T3CKIPPS.3;
  T3CKIPPS_T3CKIPPS2       : bit  absolute T3CKIPPS.2;
  T3CKIPPS_T3CKIPPS1       : bit  absolute T3CKIPPS.1;
  T3CKIPPS_T3CKIPPS0       : bit  absolute T3CKIPPS.0;
  T3GPPS                   : byte absolute $0E12;
  T3GPPS_T3GPPS5           : bit  absolute T3GPPS.5;
  T3GPPS_T3GPPS4           : bit  absolute T3GPPS.4;
  T3GPPS_T3GPPS3           : bit  absolute T3GPPS.3;
  T3GPPS_T3GPPS2           : bit  absolute T3GPPS.2;
  T3GPPS_T3GPPS1           : bit  absolute T3GPPS.1;
  T3GPPS_T3GPPS0           : bit  absolute T3GPPS.0;
  T5CKIPPS                 : byte absolute $0E13;
  T5CKIPPS_T5CKIPPS5       : bit  absolute T5CKIPPS.5;
  T5CKIPPS_T5CKIPPS4       : bit  absolute T5CKIPPS.4;
  T5CKIPPS_T5CKIPPS3       : bit  absolute T5CKIPPS.3;
  T5CKIPPS_T5CKIPPS2       : bit  absolute T5CKIPPS.2;
  T5CKIPPS_T5CKIPPS1       : bit  absolute T5CKIPPS.1;
  T5CKIPPS_T5CKIPPS0       : bit  absolute T5CKIPPS.0;
  T5GPPS                   : byte absolute $0E14;
  T5GPPS_T5GPPS5           : bit  absolute T5GPPS.5;
  T5GPPS_T5GPPS4           : bit  absolute T5GPPS.4;
  T5GPPS_T5GPPS3           : bit  absolute T5GPPS.3;
  T5GPPS_T5GPPS2           : bit  absolute T5GPPS.2;
  T5GPPS_T5GPPS1           : bit  absolute T5GPPS.1;
  T5GPPS_T5GPPS0           : bit  absolute T5GPPS.0;
  T2INPPS                  : byte absolute $0E15;
  T2INPPS_T2INPPS5         : bit  absolute T2INPPS.5;
  T2INPPS_T2INPPS4         : bit  absolute T2INPPS.4;
  T2INPPS_T2INPPS3         : bit  absolute T2INPPS.3;
  T2INPPS_T2INPPS2         : bit  absolute T2INPPS.2;
  T2INPPS_T2INPPS1         : bit  absolute T2INPPS.1;
  T2INPPS_T2INPPS0         : bit  absolute T2INPPS.0;
  T4INPPS                  : byte absolute $0E16;
  T4INPPS_T4INPPS5         : bit  absolute T4INPPS.5;
  T4INPPS_T4INPPS4         : bit  absolute T4INPPS.4;
  T4INPPS_T4INPPS3         : bit  absolute T4INPPS.3;
  T4INPPS_T4INPPS2         : bit  absolute T4INPPS.2;
  T4INPPS_T4INPPS1         : bit  absolute T4INPPS.1;
  T4INPPS_T4INPPS0         : bit  absolute T4INPPS.0;
  T6INPPS                  : byte absolute $0E17;
  T6INPPS_T6INPPS5         : bit  absolute T6INPPS.5;
  T6INPPS_T6INPPS4         : bit  absolute T6INPPS.4;
  T6INPPS_T6INPPS3         : bit  absolute T6INPPS.3;
  T6INPPS_T6INPPS2         : bit  absolute T6INPPS.2;
  T6INPPS_T6INPPS1         : bit  absolute T6INPPS.1;
  T6INPPS_T6INPPS0         : bit  absolute T6INPPS.0;
  T8INPPS                  : byte absolute $0E18;
  T8INPPS_T8INPPS5         : bit  absolute T8INPPS.5;
  T8INPPS_T8INPPS4         : bit  absolute T8INPPS.4;
  T8INPPS_T8INPPS3         : bit  absolute T8INPPS.3;
  T8INPPS_T8INPPS2         : bit  absolute T8INPPS.2;
  T8INPPS_T8INPPS1         : bit  absolute T8INPPS.1;
  T8INPPS_T8INPPS0         : bit  absolute T8INPPS.0;
  CCP1PPS                  : byte absolute $0E19;
  CCP1PPS_CCP1PPS5         : bit  absolute CCP1PPS.5;
  CCP1PPS_CCP1PPS4         : bit  absolute CCP1PPS.4;
  CCP1PPS_CCP1PPS3         : bit  absolute CCP1PPS.3;
  CCP1PPS_CCP1PPS2         : bit  absolute CCP1PPS.2;
  CCP1PPS_CCP1PPS1         : bit  absolute CCP1PPS.1;
  CCP1PPS_CCP1PPS0         : bit  absolute CCP1PPS.0;
  CCP2PPS                  : byte absolute $0E1A;
  CCP2PPS_CCP2PPS5         : bit  absolute CCP2PPS.5;
  CCP2PPS_CCP2PPS4         : bit  absolute CCP2PPS.4;
  CCP2PPS_CCP2PPS3         : bit  absolute CCP2PPS.3;
  CCP2PPS_CCP2PPS2         : bit  absolute CCP2PPS.2;
  CCP2PPS_CCP2PPS1         : bit  absolute CCP2PPS.1;
  CCP2PPS_CCP2PPS0         : bit  absolute CCP2PPS.0;
  CCP7PPS                  : byte absolute $0E1B;
  CCP7PPS_CCP7PPS5         : bit  absolute CCP7PPS.5;
  CCP7PPS_CCP7PPS4         : bit  absolute CCP7PPS.4;
  CCP7PPS_CCP7PPS3         : bit  absolute CCP7PPS.3;
  CCP7PPS_CCP7PPS2         : bit  absolute CCP7PPS.2;
  CCP7PPS_CCP7PPS1         : bit  absolute CCP7PPS.1;
  CCP7PPS_CCP7PPS0         : bit  absolute CCP7PPS.0;
  CCP8PPS                  : byte absolute $0E1C;
  CCP8PPS_CCP8PPS5         : bit  absolute CCP8PPS.5;
  CCP8PPS_CCP8PPS4         : bit  absolute CCP8PPS.4;
  CCP8PPS_CCP8PPS3         : bit  absolute CCP8PPS.3;
  CCP8PPS_CCP8PPS2         : bit  absolute CCP8PPS.2;
  CCP8PPS_CCP8PPS1         : bit  absolute CCP8PPS.1;
  CCP8PPS_CCP8PPS0         : bit  absolute CCP8PPS.0;
  COG1INPPS                : byte absolute $0E1D;
  COG1INPPS_COG1INPPS5     : bit  absolute COG1INPPS.5;
  COG1INPPS_COG1INPPS4     : bit  absolute COG1INPPS.4;
  COG1INPPS_COG1INPPS3     : bit  absolute COG1INPPS.3;
  COG1INPPS_COG1INPPS2     : bit  absolute COG1INPPS.2;
  COG1INPPS_COG1INPPS1     : bit  absolute COG1INPPS.1;
  COG1INPPS_COG1INPPS0     : bit  absolute COG1INPPS.0;
  COG2INPPS                : byte absolute $0E1E;
  COG2INPPS_COG2INPPS5     : bit  absolute COG2INPPS.5;
  COG2INPPS_COG2INPPS4     : bit  absolute COG2INPPS.4;
  COG2INPPS_COG2INPPS3     : bit  absolute COG2INPPS.3;
  COG2INPPS_COG2INPPS2     : bit  absolute COG2INPPS.2;
  COG2INPPS_COG2INPPS1     : bit  absolute COG2INPPS.1;
  COG2INPPS_COG2INPPS0     : bit  absolute COG2INPPS.0;
  COG3INPPS                : byte absolute $0E1F;
  COG3INPPS_COG3INPPS5     : bit  absolute COG3INPPS.5;
  COG3INPPS_COG3INPPS4     : bit  absolute COG3INPPS.4;
  COG3INPPS_COG3INPPS3     : bit  absolute COG3INPPS.3;
  COG3INPPS_COG3INPPS2     : bit  absolute COG3INPPS.2;
  COG3INPPS_COG3INPPS1     : bit  absolute COG3INPPS.1;
  COG3INPPS_COG3INPPS0     : bit  absolute COG3INPPS.0;
  COG4INPPS                : byte absolute $0E20;
  COG4INPPS_COG4INPPS5     : bit  absolute COG4INPPS.5;
  COG4INPPS_COG4INPPS4     : bit  absolute COG4INPPS.4;
  COG4INPPS_COG4INPPS3     : bit  absolute COG4INPPS.3;
  COG4INPPS_COG4INPPS2     : bit  absolute COG4INPPS.2;
  COG4INPPS_COG4INPPS1     : bit  absolute COG4INPPS.1;
  COG4INPPS_COG4INPPS0     : bit  absolute COG4INPPS.0;
  MD1CLPPS                 : byte absolute $0E21;
  MD1CLPPS_MD1CLPPS5       : bit  absolute MD1CLPPS.5;
  MD1CLPPS_MD1CLPPS4       : bit  absolute MD1CLPPS.4;
  MD1CLPPS_MD1CLPPS3       : bit  absolute MD1CLPPS.3;
  MD1CLPPS_MD1CLPPS2       : bit  absolute MD1CLPPS.2;
  MD1CLPPS_MD1CLPPS1       : bit  absolute MD1CLPPS.1;
  MD1CLPPS_MD1CLPPS0       : bit  absolute MD1CLPPS.0;
  MD1CHPPS                 : byte absolute $0E22;
  MD1CHPPS_MD1CHPPS5       : bit  absolute MD1CHPPS.5;
  MD1CHPPS_MD1CHPPS4       : bit  absolute MD1CHPPS.4;
  MD1CHPPS_MD1CHPPS3       : bit  absolute MD1CHPPS.3;
  MD1CHPPS_MD1CHPPS2       : bit  absolute MD1CHPPS.2;
  MD1CHPPS_MD1CHPPS1       : bit  absolute MD1CHPPS.1;
  MD1CHPPS_MD1CHPPS0       : bit  absolute MD1CHPPS.0;
  MD1MODPPS                : byte absolute $0E23;
  MD1MODPPS_MD1MODPPS5     : bit  absolute MD1MODPPS.5;
  MD1MODPPS_MD1MODPPS4     : bit  absolute MD1MODPPS.4;
  MD1MODPPS_MD1MODPPS3     : bit  absolute MD1MODPPS.3;
  MD1MODPPS_MD1MODPPS2     : bit  absolute MD1MODPPS.2;
  MD1MODPPS_MD1MODPPS1     : bit  absolute MD1MODPPS.1;
  MD1MODPPS_MD1MODPPS0     : bit  absolute MD1MODPPS.0;
  MD2CLPPS                 : byte absolute $0E24;
  MD2CLPPS_MD2CLPPS5       : bit  absolute MD2CLPPS.5;
  MD2CLPPS_MD2CLPPS4       : bit  absolute MD2CLPPS.4;
  MD2CLPPS_MD2CLPPS3       : bit  absolute MD2CLPPS.3;
  MD2CLPPS_MD2CLPPS2       : bit  absolute MD2CLPPS.2;
  MD2CLPPS_MD2CLPPS1       : bit  absolute MD2CLPPS.1;
  MD2CLPPS_MD2CLPPS0       : bit  absolute MD2CLPPS.0;
  MD2CHPPS                 : byte absolute $0E25;
  MD2CHPPS_MD2CHPPS5       : bit  absolute MD2CHPPS.5;
  MD2CHPPS_MD2CHPPS4       : bit  absolute MD2CHPPS.4;
  MD2CHPPS_MD2CHPPS3       : bit  absolute MD2CHPPS.3;
  MD2CHPPS_MD2CHPPS2       : bit  absolute MD2CHPPS.2;
  MD2CHPPS_MD2CHPPS1       : bit  absolute MD2CHPPS.1;
  MD2CHPPS_MD2CHPPS0       : bit  absolute MD2CHPPS.0;
  MD2MODPPS                : byte absolute $0E26;
  MD2MODPPS_MD2MODPPS5     : bit  absolute MD2MODPPS.5;
  MD2MODPPS_MD2MODPPS4     : bit  absolute MD2MODPPS.4;
  MD2MODPPS_MD2MODPPS3     : bit  absolute MD2MODPPS.3;
  MD2MODPPS_MD2MODPPS2     : bit  absolute MD2MODPPS.2;
  MD2MODPPS_MD2MODPPS1     : bit  absolute MD2MODPPS.1;
  MD2MODPPS_MD2MODPPS0     : bit  absolute MD2MODPPS.0;
  MD3CLPPS                 : byte absolute $0E27;
  MD3CLPPS_MD3CLPPS5       : bit  absolute MD3CLPPS.5;
  MD3CLPPS_MD3CLPPS4       : bit  absolute MD3CLPPS.4;
  MD3CLPPS_MD3CLPPS3       : bit  absolute MD3CLPPS.3;
  MD3CLPPS_MD3CLPPS2       : bit  absolute MD3CLPPS.2;
  MD3CLPPS_MD3CLPPS1       : bit  absolute MD3CLPPS.1;
  MD3CLPPS_MD3CLPPS0       : bit  absolute MD3CLPPS.0;
  MD3CHPPS                 : byte absolute $0E28;
  MD3CHPPS_MD3CHPPS5       : bit  absolute MD3CHPPS.5;
  MD3CHPPS_MD3CHPPS4       : bit  absolute MD3CHPPS.4;
  MD3CHPPS_MD3CHPPS3       : bit  absolute MD3CHPPS.3;
  MD3CHPPS_MD3CHPPS2       : bit  absolute MD3CHPPS.2;
  MD3CHPPS_MD3CHPPS1       : bit  absolute MD3CHPPS.1;
  MD3CHPPS_MD3CHPPS0       : bit  absolute MD3CHPPS.0;
  MD3MODPPS                : byte absolute $0E29;
  MD3MODPPS_MD3MODPPS5     : bit  absolute MD3MODPPS.5;
  MD3MODPPS_MD3MODPPS4     : bit  absolute MD3MODPPS.4;
  MD3MODPPS_MD3MODPPS3     : bit  absolute MD3MODPPS.3;
  MD3MODPPS_MD3MODPPS2     : bit  absolute MD3MODPPS.2;
  MD3MODPPS_MD3MODPPS1     : bit  absolute MD3MODPPS.1;
  MD3MODPPS_MD3MODPPS0     : bit  absolute MD3MODPPS.0;
  MD4CLPPS                 : byte absolute $0E2A;
  MD4CLPPS_MD4CLPPS5       : bit  absolute MD4CLPPS.5;
  MD4CLPPS_MD4CLPPS4       : bit  absolute MD4CLPPS.4;
  MD4CLPPS_MD4CLPPS3       : bit  absolute MD4CLPPS.3;
  MD4CLPPS_MD4CLPPS2       : bit  absolute MD4CLPPS.2;
  MD4CLPPS_MD4CLPPS1       : bit  absolute MD4CLPPS.1;
  MD4CLPPS_MD4CLPPS0       : bit  absolute MD4CLPPS.0;
  MD4CHPPS                 : byte absolute $0E2B;
  MD4CHPPS_MD4CHPPS5       : bit  absolute MD4CHPPS.5;
  MD4CHPPS_MD4CHPPS4       : bit  absolute MD4CHPPS.4;
  MD4CHPPS_MD4CHPPS3       : bit  absolute MD4CHPPS.3;
  MD4CHPPS_MD4CHPPS2       : bit  absolute MD4CHPPS.2;
  MD4CHPPS_MD4CHPPS1       : bit  absolute MD4CHPPS.1;
  MD4CHPPS_MD4CHPPS0       : bit  absolute MD4CHPPS.0;
  MD4MODPPS                : byte absolute $0E2C;
  MD4MODPPS_MD4MODPPS5     : bit  absolute MD4MODPPS.5;
  MD4MODPPS_MD4MODPPS4     : bit  absolute MD4MODPPS.4;
  MD4MODPPS_MD4MODPPS3     : bit  absolute MD4MODPPS.3;
  MD4MODPPS_MD4MODPPS2     : bit  absolute MD4MODPPS.2;
  MD4MODPPS_MD4MODPPS1     : bit  absolute MD4MODPPS.1;
  MD4MODPPS_MD4MODPPS0     : bit  absolute MD4MODPPS.0;
  PRG1RPPS                 : byte absolute $0E2D;
  PRG1RPPS_PRG1RPPS5       : bit  absolute PRG1RPPS.5;
  PRG1RPPS_PRG1RPPS4       : bit  absolute PRG1RPPS.4;
  PRG1RPPS_PRG1RPPS3       : bit  absolute PRG1RPPS.3;
  PRG1RPPS_PRG1RPPS2       : bit  absolute PRG1RPPS.2;
  PRG1RPPS_PRG1RPPS1       : bit  absolute PRG1RPPS.1;
  PRG1RPPS_PRG1RPPS0       : bit  absolute PRG1RPPS.0;
  PRG1FPPS                 : byte absolute $0E2E;
  PRG1FPPS_PRG1FPPS5       : bit  absolute PRG1FPPS.5;
  PRG1FPPS_PRG1FPPS4       : bit  absolute PRG1FPPS.4;
  PRG1FPPS_PRG1FPPS3       : bit  absolute PRG1FPPS.3;
  PRG1FPPS_PRG1FPPS2       : bit  absolute PRG1FPPS.2;
  PRG1FPPS_PRG1FPPS1       : bit  absolute PRG1FPPS.1;
  PRG1FPPS_PRG1FPPS0       : bit  absolute PRG1FPPS.0;
  PRG2RPPS                 : byte absolute $0E2F;
  PRG2RPPS_PRG2RPPS5       : bit  absolute PRG2RPPS.5;
  PRG2RPPS_PRG2RPPS4       : bit  absolute PRG2RPPS.4;
  PRG2RPPS_PRG2RPPS3       : bit  absolute PRG2RPPS.3;
  PRG2RPPS_PRG2RPPS2       : bit  absolute PRG2RPPS.2;
  PRG2RPPS_PRG2RPPS1       : bit  absolute PRG2RPPS.1;
  PRG2RPPS_PRG2RPPS0       : bit  absolute PRG2RPPS.0;
  PRG2FPPS                 : byte absolute $0E30;
  PRG2FPPS_PRG2FPPS5       : bit  absolute PRG2FPPS.5;
  PRG2FPPS_PRG2FPPS4       : bit  absolute PRG2FPPS.4;
  PRG2FPPS_PRG2FPPS3       : bit  absolute PRG2FPPS.3;
  PRG2FPPS_PRG2FPPS2       : bit  absolute PRG2FPPS.2;
  PRG2FPPS_PRG2FPPS1       : bit  absolute PRG2FPPS.1;
  PRG2FPPS_PRG2FPPS0       : bit  absolute PRG2FPPS.0;
  PRG3RPPS                 : byte absolute $0E31;
  PRG3RPPS_PRG3RPPS5       : bit  absolute PRG3RPPS.5;
  PRG3RPPS_PRG3RPPS4       : bit  absolute PRG3RPPS.4;
  PRG3RPPS_PRG3RPPS3       : bit  absolute PRG3RPPS.3;
  PRG3RPPS_PRG3RPPS2       : bit  absolute PRG3RPPS.2;
  PRG3RPPS_PRG3RPPS1       : bit  absolute PRG3RPPS.1;
  PRG3RPPS_PRG3RPPS0       : bit  absolute PRG3RPPS.0;
  PRG3FPPS                 : byte absolute $0E32;
  PRG3FPPS_PRG3FPPS5       : bit  absolute PRG3FPPS.5;
  PRG3FPPS_PRG3FPPS4       : bit  absolute PRG3FPPS.4;
  PRG3FPPS_PRG3FPPS3       : bit  absolute PRG3FPPS.3;
  PRG3FPPS_PRG3FPPS2       : bit  absolute PRG3FPPS.2;
  PRG3FPPS_PRG3FPPS1       : bit  absolute PRG3FPPS.1;
  PRG3FPPS_PRG3FPPS0       : bit  absolute PRG3FPPS.0;
  PRG4RPPS                 : byte absolute $0E33;
  PRG4RPPS_PRG4RPPS5       : bit  absolute PRG4RPPS.5;
  PRG4RPPS_PRG4RPPS4       : bit  absolute PRG4RPPS.4;
  PRG4RPPS_PRG4RPPS3       : bit  absolute PRG4RPPS.3;
  PRG4RPPS_PRG4RPPS2       : bit  absolute PRG4RPPS.2;
  PRG4RPPS_PRG4RPPS1       : bit  absolute PRG4RPPS.1;
  PRG4RPPS_PRG4RPPS0       : bit  absolute PRG4RPPS.0;
  PRG4FPPS                 : byte absolute $0E34;
  PRG4FPPS_PRG4FPPS5       : bit  absolute PRG4FPPS.5;
  PRG4FPPS_PRG4FPPS4       : bit  absolute PRG4FPPS.4;
  PRG4FPPS_PRG4FPPS3       : bit  absolute PRG4FPPS.3;
  PRG4FPPS_PRG4FPPS2       : bit  absolute PRG4FPPS.2;
  PRG4FPPS_PRG4FPPS1       : bit  absolute PRG4FPPS.1;
  PRG4FPPS_PRG4FPPS0       : bit  absolute PRG4FPPS.0;
  CLCIN0PPS                : byte absolute $0E35;
  CLCIN0PPS_CLCIN0PPS5     : bit  absolute CLCIN0PPS.5;
  CLCIN0PPS_CLCIN0PPS4     : bit  absolute CLCIN0PPS.4;
  CLCIN0PPS_CLCIN0PPS3     : bit  absolute CLCIN0PPS.3;
  CLCIN0PPS_CLCIN0PPS2     : bit  absolute CLCIN0PPS.2;
  CLCIN0PPS_CLCIN0PPS1     : bit  absolute CLCIN0PPS.1;
  CLCIN0PPS_CLCIN0PPS0     : bit  absolute CLCIN0PPS.0;
  CLCIN1PPS                : byte absolute $0E36;
  CLCIN1PPS_CLCIN1PPS5     : bit  absolute CLCIN1PPS.5;
  CLCIN1PPS_CLCIN1PPS4     : bit  absolute CLCIN1PPS.4;
  CLCIN1PPS_CLCIN1PPS3     : bit  absolute CLCIN1PPS.3;
  CLCIN1PPS_CLCIN1PPS2     : bit  absolute CLCIN1PPS.2;
  CLCIN1PPS_CLCIN1PPS1     : bit  absolute CLCIN1PPS.1;
  CLCIN1PPS_CLCIN1PPS0     : bit  absolute CLCIN1PPS.0;
  CLCIN2PPS                : byte absolute $0E37;
  CLCIN2PPS_CLCIN2PPS5     : bit  absolute CLCIN2PPS.5;
  CLCIN2PPS_CLCIN2PPS4     : bit  absolute CLCIN2PPS.4;
  CLCIN2PPS_CLCIN2PPS3     : bit  absolute CLCIN2PPS.3;
  CLCIN2PPS_CLCIN2PPS2     : bit  absolute CLCIN2PPS.2;
  CLCIN2PPS_CLCIN2PPS1     : bit  absolute CLCIN2PPS.1;
  CLCIN2PPS_CLCIN2PPS0     : bit  absolute CLCIN2PPS.0;
  CLCIN3PPS                : byte absolute $0E38;
  CLCIN3PPS_CLCIN3PPS5     : bit  absolute CLCIN3PPS.5;
  CLCIN3PPS_CLCIN3PPS4     : bit  absolute CLCIN3PPS.4;
  CLCIN3PPS_CLCIN3PPS3     : bit  absolute CLCIN3PPS.3;
  CLCIN3PPS_CLCIN3PPS2     : bit  absolute CLCIN3PPS.2;
  CLCIN3PPS_CLCIN3PPS1     : bit  absolute CLCIN3PPS.1;
  CLCIN3PPS_CLCIN3PPS0     : bit  absolute CLCIN3PPS.0;
  ADCACTPPS                : byte absolute $0E39;
  ADCACTPPS_ADCACTPPS5     : bit  absolute ADCACTPPS.5;
  ADCACTPPS_ADCACTPPS4     : bit  absolute ADCACTPPS.4;
  ADCACTPPS_ADCACTPPS3     : bit  absolute ADCACTPPS.3;
  ADCACTPPS_ADCACTPPS2     : bit  absolute ADCACTPPS.2;
  ADCACTPPS_ADCACTPPS1     : bit  absolute ADCACTPPS.1;
  ADCACTPPS_ADCACTPPS0     : bit  absolute ADCACTPPS.0;
  SSPCLKPPS                : byte absolute $0E3A;
  SSPCLKPPS_SSPCLKPPS5     : bit  absolute SSPCLKPPS.5;
  SSPCLKPPS_SSPCLKPPS4     : bit  absolute SSPCLKPPS.4;
  SSPCLKPPS_SSPCLKPPS3     : bit  absolute SSPCLKPPS.3;
  SSPCLKPPS_SSPCLKPPS2     : bit  absolute SSPCLKPPS.2;
  SSPCLKPPS_SSPCLKPPS1     : bit  absolute SSPCLKPPS.1;
  SSPCLKPPS_SSPCLKPPS0     : bit  absolute SSPCLKPPS.0;
  SSPDATPPS                : byte absolute $0E3B;
  SSPDATPPS_SSPDATPPS5     : bit  absolute SSPDATPPS.5;
  SSPDATPPS_SSPDATPPS4     : bit  absolute SSPDATPPS.4;
  SSPDATPPS_SSPDATPPS3     : bit  absolute SSPDATPPS.3;
  SSPDATPPS_SSPDATPPS2     : bit  absolute SSPDATPPS.2;
  SSPDATPPS_SSPDATPPS1     : bit  absolute SSPDATPPS.1;
  SSPDATPPS_SSPDATPPS0     : bit  absolute SSPDATPPS.0;
  SSPSSPPS                 : byte absolute $0E3C;
  SSPSSPPS_SSPSSPPS5       : bit  absolute SSPSSPPS.5;
  SSPSSPPS_SSPSSPPS4       : bit  absolute SSPSSPPS.4;
  SSPSSPPS_SSPSSPPS3       : bit  absolute SSPSSPPS.3;
  SSPSSPPS_SSPSSPPS2       : bit  absolute SSPSSPPS.2;
  SSPSSPPS_SSPSSPPS1       : bit  absolute SSPSSPPS.1;
  SSPSSPPS_SSPSSPPS0       : bit  absolute SSPSSPPS.0;
  RXPPS                    : byte absolute $0E3D;
  RXPPS_RXPPS5             : bit  absolute RXPPS.5;
  RXPPS_RXPPS4             : bit  absolute RXPPS.4;
  RXPPS_RXPPS3             : bit  absolute RXPPS.3;
  RXPPS_RXPPS2             : bit  absolute RXPPS.2;
  RXPPS_RXPPS1             : bit  absolute RXPPS.1;
  RXPPS_RXPPS0             : bit  absolute RXPPS.0;
  CKPPS                    : byte absolute $0E3E;
  CKPPS_CKPPS5             : bit  absolute CKPPS.5;
  CKPPS_CKPPS4             : bit  absolute CKPPS.4;
  CKPPS_CKPPS3             : bit  absolute CKPPS.3;
  CKPPS_CKPPS2             : bit  absolute CKPPS.2;
  CKPPS_CKPPS1             : bit  absolute CKPPS.1;
  CKPPS_CKPPS0             : bit  absolute CKPPS.0;
  RA0PPS                   : byte absolute $0E90;
  RA0PPS_RA0PPS5           : bit  absolute RA0PPS.5;
  RA0PPS_RA0PPS4           : bit  absolute RA0PPS.4;
  RA0PPS_RA0PPS3           : bit  absolute RA0PPS.3;
  RA0PPS_RA0PPS2           : bit  absolute RA0PPS.2;
  RA0PPS_RA0PPS1           : bit  absolute RA0PPS.1;
  RA0PPS_RA0PPS0           : bit  absolute RA0PPS.0;
  RA1PPS                   : byte absolute $0E91;
  RA1PPS_RA1PPS5           : bit  absolute RA1PPS.5;
  RA1PPS_RA1PPS4           : bit  absolute RA1PPS.4;
  RA1PPS_RA1PPS3           : bit  absolute RA1PPS.3;
  RA1PPS_RA1PPS2           : bit  absolute RA1PPS.2;
  RA1PPS_RA1PPS1           : bit  absolute RA1PPS.1;
  RA1PPS_RA1PPS0           : bit  absolute RA1PPS.0;
  RA2PPS                   : byte absolute $0E92;
  RA2PPS_RA2PPS5           : bit  absolute RA2PPS.5;
  RA2PPS_RA2PPS4           : bit  absolute RA2PPS.4;
  RA2PPS_RA2PPS3           : bit  absolute RA2PPS.3;
  RA2PPS_RA2PPS2           : bit  absolute RA2PPS.2;
  RA2PPS_RA2PPS1           : bit  absolute RA2PPS.1;
  RA2PPS_RA2PPS0           : bit  absolute RA2PPS.0;
  RA3PPS                   : byte absolute $0E93;
  RA3PPS_RA3PPS5           : bit  absolute RA3PPS.5;
  RA3PPS_RA3PPS4           : bit  absolute RA3PPS.4;
  RA3PPS_RA3PPS3           : bit  absolute RA3PPS.3;
  RA3PPS_RA3PPS2           : bit  absolute RA3PPS.2;
  RA3PPS_RA3PPS1           : bit  absolute RA3PPS.1;
  RA3PPS_RA3PPS0           : bit  absolute RA3PPS.0;
  RA4PPS                   : byte absolute $0E94;
  RA4PPS_RA4PPS5           : bit  absolute RA4PPS.5;
  RA4PPS_RA4PPS4           : bit  absolute RA4PPS.4;
  RA4PPS_RA4PPS3           : bit  absolute RA4PPS.3;
  RA4PPS_RA4PPS2           : bit  absolute RA4PPS.2;
  RA4PPS_RA4PPS1           : bit  absolute RA4PPS.1;
  RA4PPS_RA4PPS0           : bit  absolute RA4PPS.0;
  RA5PPS                   : byte absolute $0E95;
  RA5PPS_RA5PPS5           : bit  absolute RA5PPS.5;
  RA5PPS_RA5PPS4           : bit  absolute RA5PPS.4;
  RA5PPS_RA5PPS3           : bit  absolute RA5PPS.3;
  RA5PPS_RA5PPS2           : bit  absolute RA5PPS.2;
  RA5PPS_RA5PPS1           : bit  absolute RA5PPS.1;
  RA5PPS_RA5PPS0           : bit  absolute RA5PPS.0;
  RA6PPS                   : byte absolute $0E96;
  RA6PPS_RA6PPS5           : bit  absolute RA6PPS.5;
  RA6PPS_RA6PPS4           : bit  absolute RA6PPS.4;
  RA6PPS_RA6PPS3           : bit  absolute RA6PPS.3;
  RA6PPS_RA6PPS2           : bit  absolute RA6PPS.2;
  RA6PPS_RA6PPS1           : bit  absolute RA6PPS.1;
  RA6PPS_RA6PPS0           : bit  absolute RA6PPS.0;
  RA7PPS                   : byte absolute $0E97;
  RA7PPS_RA7PPS5           : bit  absolute RA7PPS.5;
  RA7PPS_RA7PPS4           : bit  absolute RA7PPS.4;
  RA7PPS_RA7PPS3           : bit  absolute RA7PPS.3;
  RA7PPS_RA7PPS2           : bit  absolute RA7PPS.2;
  RA7PPS_RA7PPS1           : bit  absolute RA7PPS.1;
  RA7PPS_RA7PPS0           : bit  absolute RA7PPS.0;
  RB0PPS                   : byte absolute $0E98;
  RB0PPS_RB0PPS5           : bit  absolute RB0PPS.5;
  RB0PPS_RB0PPS4           : bit  absolute RB0PPS.4;
  RB0PPS_RB0PPS3           : bit  absolute RB0PPS.3;
  RB0PPS_RB0PPS2           : bit  absolute RB0PPS.2;
  RB0PPS_RB0PPS1           : bit  absolute RB0PPS.1;
  RB0PPS_RB0PPS0           : bit  absolute RB0PPS.0;
  RB1PPS                   : byte absolute $0E99;
  RB1PPS_RB1PPS5           : bit  absolute RB1PPS.5;
  RB1PPS_RB1PPS4           : bit  absolute RB1PPS.4;
  RB1PPS_RB1PPS3           : bit  absolute RB1PPS.3;
  RB1PPS_RB1PPS2           : bit  absolute RB1PPS.2;
  RB1PPS_RB1PPS1           : bit  absolute RB1PPS.1;
  RB1PPS_RB1PPS0           : bit  absolute RB1PPS.0;
  RB2PPS                   : byte absolute $0E9A;
  RB2PPS_RB2PPS5           : bit  absolute RB2PPS.5;
  RB2PPS_RB2PPS4           : bit  absolute RB2PPS.4;
  RB2PPS_RB2PPS3           : bit  absolute RB2PPS.3;
  RB2PPS_RB2PPS2           : bit  absolute RB2PPS.2;
  RB2PPS_RB2PPS1           : bit  absolute RB2PPS.1;
  RB2PPS_RB2PPS0           : bit  absolute RB2PPS.0;
  RB3PPS                   : byte absolute $0E9B;
  RB3PPS_RB3PPS5           : bit  absolute RB3PPS.5;
  RB3PPS_RB3PPS4           : bit  absolute RB3PPS.4;
  RB3PPS_RB3PPS3           : bit  absolute RB3PPS.3;
  RB3PPS_RB3PPS2           : bit  absolute RB3PPS.2;
  RB3PPS_RB3PPS1           : bit  absolute RB3PPS.1;
  RB3PPS_RB3PPS0           : bit  absolute RB3PPS.0;
  RB4PPS                   : byte absolute $0E9C;
  RB4PPS_RB4PPS5           : bit  absolute RB4PPS.5;
  RB4PPS_RB4PPS4           : bit  absolute RB4PPS.4;
  RB4PPS_RB4PPS3           : bit  absolute RB4PPS.3;
  RB4PPS_RB4PPS2           : bit  absolute RB4PPS.2;
  RB4PPS_RB4PPS1           : bit  absolute RB4PPS.1;
  RB4PPS_RB4PPS0           : bit  absolute RB4PPS.0;
  RB5PPS                   : byte absolute $0E9D;
  RB5PPS_RB5PPS5           : bit  absolute RB5PPS.5;
  RB5PPS_RB5PPS4           : bit  absolute RB5PPS.4;
  RB5PPS_RB5PPS3           : bit  absolute RB5PPS.3;
  RB5PPS_RB5PPS2           : bit  absolute RB5PPS.2;
  RB5PPS_RB5PPS1           : bit  absolute RB5PPS.1;
  RB5PPS_RB5PPS0           : bit  absolute RB5PPS.0;
  RB6PPS                   : byte absolute $0E9E;
  RB6PPS_RB6PPS5           : bit  absolute RB6PPS.5;
  RB6PPS_RB6PPS4           : bit  absolute RB6PPS.4;
  RB6PPS_RB6PPS3           : bit  absolute RB6PPS.3;
  RB6PPS_RB6PPS2           : bit  absolute RB6PPS.2;
  RB6PPS_RB6PPS1           : bit  absolute RB6PPS.1;
  RB6PPS_RB6PPS0           : bit  absolute RB6PPS.0;
  RB7PPS                   : byte absolute $0E9F;
  RB7PPS_RB7PPS5           : bit  absolute RB7PPS.5;
  RB7PPS_RB7PPS4           : bit  absolute RB7PPS.4;
  RB7PPS_RB7PPS3           : bit  absolute RB7PPS.3;
  RB7PPS_RB7PPS2           : bit  absolute RB7PPS.2;
  RB7PPS_RB7PPS1           : bit  absolute RB7PPS.1;
  RB7PPS_RB7PPS0           : bit  absolute RB7PPS.0;
  RC0PPS                   : byte absolute $0EA0;
  RC0PPS_RC0PPS5           : bit  absolute RC0PPS.5;
  RC0PPS_RC0PPS4           : bit  absolute RC0PPS.4;
  RC0PPS_RC0PPS3           : bit  absolute RC0PPS.3;
  RC0PPS_RC0PPS2           : bit  absolute RC0PPS.2;
  RC0PPS_RC0PPS1           : bit  absolute RC0PPS.1;
  RC0PPS_RC0PPS0           : bit  absolute RC0PPS.0;
  RC1PPS                   : byte absolute $0EA1;
  RC1PPS_RC1PPS5           : bit  absolute RC1PPS.5;
  RC1PPS_RC1PPS4           : bit  absolute RC1PPS.4;
  RC1PPS_RC1PPS3           : bit  absolute RC1PPS.3;
  RC1PPS_RC1PPS2           : bit  absolute RC1PPS.2;
  RC1PPS_RC1PPS1           : bit  absolute RC1PPS.1;
  RC1PPS_RC1PPS0           : bit  absolute RC1PPS.0;
  RC2PPS                   : byte absolute $0EA2;
  RC2PPS_RC2PPS5           : bit  absolute RC2PPS.5;
  RC2PPS_RC2PPS4           : bit  absolute RC2PPS.4;
  RC2PPS_RC2PPS3           : bit  absolute RC2PPS.3;
  RC2PPS_RC2PPS2           : bit  absolute RC2PPS.2;
  RC2PPS_RC2PPS1           : bit  absolute RC2PPS.1;
  RC2PPS_RC2PPS0           : bit  absolute RC2PPS.0;
  RC3PPS                   : byte absolute $0EA3;
  RC3PPS_RC3PPS5           : bit  absolute RC3PPS.5;
  RC3PPS_RC3PPS4           : bit  absolute RC3PPS.4;
  RC3PPS_RC3PPS3           : bit  absolute RC3PPS.3;
  RC3PPS_RC3PPS2           : bit  absolute RC3PPS.2;
  RC3PPS_RC3PPS1           : bit  absolute RC3PPS.1;
  RC3PPS_RC3PPS0           : bit  absolute RC3PPS.0;
  RC4PPS                   : byte absolute $0EA4;
  RC4PPS_RC4PPS5           : bit  absolute RC4PPS.5;
  RC4PPS_RC4PPS4           : bit  absolute RC4PPS.4;
  RC4PPS_RC4PPS3           : bit  absolute RC4PPS.3;
  RC4PPS_RC4PPS2           : bit  absolute RC4PPS.2;
  RC4PPS_RC4PPS1           : bit  absolute RC4PPS.1;
  RC4PPS_RC4PPS0           : bit  absolute RC4PPS.0;
  RC5PPS                   : byte absolute $0EA5;
  RC5PPS_RC5PPS5           : bit  absolute RC5PPS.5;
  RC5PPS_RC5PPS4           : bit  absolute RC5PPS.4;
  RC5PPS_RC5PPS3           : bit  absolute RC5PPS.3;
  RC5PPS_RC5PPS2           : bit  absolute RC5PPS.2;
  RC5PPS_RC5PPS1           : bit  absolute RC5PPS.1;
  RC5PPS_RC5PPS0           : bit  absolute RC5PPS.0;
  RC6PPS                   : byte absolute $0EA6;
  RC6PPS_RC6PPS5           : bit  absolute RC6PPS.5;
  RC6PPS_RC6PPS4           : bit  absolute RC6PPS.4;
  RC6PPS_RC6PPS3           : bit  absolute RC6PPS.3;
  RC6PPS_RC6PPS2           : bit  absolute RC6PPS.2;
  RC6PPS_RC6PPS1           : bit  absolute RC6PPS.1;
  RC6PPS_RC6PPS0           : bit  absolute RC6PPS.0;
  RC7PPS                   : byte absolute $0EA7;
  RC7PPS_RC7PPS5           : bit  absolute RC7PPS.5;
  RC7PPS_RC7PPS4           : bit  absolute RC7PPS.4;
  RC7PPS_RC7PPS3           : bit  absolute RC7PPS.3;
  RC7PPS_RC7PPS2           : bit  absolute RC7PPS.2;
  RC7PPS_RC7PPS1           : bit  absolute RC7PPS.1;
  RC7PPS_RC7PPS0           : bit  absolute RC7PPS.0;
  RD0PPS                   : byte absolute $0EA8;
  RD0PPS_RD0PPS5           : bit  absolute RD0PPS.5;
  RD0PPS_RD0PPS4           : bit  absolute RD0PPS.4;
  RD0PPS_RD0PPS3           : bit  absolute RD0PPS.3;
  RD0PPS_RD0PPS2           : bit  absolute RD0PPS.2;
  RD0PPS_RD0PPS1           : bit  absolute RD0PPS.1;
  RD0PPS_RD0PPS0           : bit  absolute RD0PPS.0;
  RD1PPS                   : byte absolute $0EA9;
  RD1PPS_RD1PPS5           : bit  absolute RD1PPS.5;
  RD1PPS_RD1PPS4           : bit  absolute RD1PPS.4;
  RD1PPS_RD1PPS3           : bit  absolute RD1PPS.3;
  RD1PPS_RD1PPS2           : bit  absolute RD1PPS.2;
  RD1PPS_RD1PPS1           : bit  absolute RD1PPS.1;
  RD1PPS_RD1PPS0           : bit  absolute RD1PPS.0;
  RD2PPS                   : byte absolute $0EAA;
  RD2PPS_RD2PPS5           : bit  absolute RD2PPS.5;
  RD2PPS_RD2PPS4           : bit  absolute RD2PPS.4;
  RD2PPS_RD2PPS3           : bit  absolute RD2PPS.3;
  RD2PPS_RD2PPS2           : bit  absolute RD2PPS.2;
  RD2PPS_RD2PPS1           : bit  absolute RD2PPS.1;
  RD2PPS_RD2PPS0           : bit  absolute RD2PPS.0;
  RD3PPS                   : byte absolute $0EAB;
  RD3PPS_RD3PPS5           : bit  absolute RD3PPS.5;
  RD3PPS_RD3PPS4           : bit  absolute RD3PPS.4;
  RD3PPS_RD3PPS3           : bit  absolute RD3PPS.3;
  RD3PPS_RD3PPS2           : bit  absolute RD3PPS.2;
  RD3PPS_RD3PPS1           : bit  absolute RD3PPS.1;
  RD3PPS_RD3PPS0           : bit  absolute RD3PPS.0;
  RD4PPS                   : byte absolute $0EAC;
  RD4PPS_RD4PPS5           : bit  absolute RD4PPS.5;
  RD4PPS_RD4PPS4           : bit  absolute RD4PPS.4;
  RD4PPS_RD4PPS3           : bit  absolute RD4PPS.3;
  RD4PPS_RD4PPS2           : bit  absolute RD4PPS.2;
  RD4PPS_RD4PPS1           : bit  absolute RD4PPS.1;
  RD4PPS_RD4PPS0           : bit  absolute RD4PPS.0;
  RD5PPS                   : byte absolute $0EAD;
  RD5PPS_RD5PPS5           : bit  absolute RD5PPS.5;
  RD5PPS_RD5PPS4           : bit  absolute RD5PPS.4;
  RD5PPS_RD5PPS3           : bit  absolute RD5PPS.3;
  RD5PPS_RD5PPS2           : bit  absolute RD5PPS.2;
  RD5PPS_RD5PPS1           : bit  absolute RD5PPS.1;
  RD5PPS_RD5PPS0           : bit  absolute RD5PPS.0;
  RD6PPS                   : byte absolute $0EAE;
  RD6PPS_RD6PPS5           : bit  absolute RD6PPS.5;
  RD6PPS_RD6PPS4           : bit  absolute RD6PPS.4;
  RD6PPS_RD6PPS3           : bit  absolute RD6PPS.3;
  RD6PPS_RD6PPS2           : bit  absolute RD6PPS.2;
  RD6PPS_RD6PPS1           : bit  absolute RD6PPS.1;
  RD6PPS_RD6PPS0           : bit  absolute RD6PPS.0;
  RD7PPS                   : byte absolute $0EAF;
  RD7PPS_RD7PPS5           : bit  absolute RD7PPS.5;
  RD7PPS_RD7PPS4           : bit  absolute RD7PPS.4;
  RD7PPS_RD7PPS3           : bit  absolute RD7PPS.3;
  RD7PPS_RD7PPS2           : bit  absolute RD7PPS.2;
  RD7PPS_RD7PPS1           : bit  absolute RD7PPS.1;
  RD7PPS_RD7PPS0           : bit  absolute RD7PPS.0;
  RE0PPS                   : byte absolute $0EB0;
  RE0PPS_RE0PPS5           : bit  absolute RE0PPS.5;
  RE0PPS_RE0PPS4           : bit  absolute RE0PPS.4;
  RE0PPS_RE0PPS3           : bit  absolute RE0PPS.3;
  RE0PPS_RE0PPS2           : bit  absolute RE0PPS.2;
  RE0PPS_RE0PPS1           : bit  absolute RE0PPS.1;
  RE0PPS_RE0PPS0           : bit  absolute RE0PPS.0;
  RE1PPS                   : byte absolute $0EB1;
  RE1PPS_RE1PPS5           : bit  absolute RE1PPS.5;
  RE1PPS_RE1PPS4           : bit  absolute RE1PPS.4;
  RE1PPS_RE1PPS3           : bit  absolute RE1PPS.3;
  RE1PPS_RE1PPS2           : bit  absolute RE1PPS.2;
  RE1PPS_RE1PPS1           : bit  absolute RE1PPS.1;
  RE1PPS_RE1PPS0           : bit  absolute RE1PPS.0;
  RE2PPS                   : byte absolute $0EB2;
  RE2PPS_RE2PPS5           : bit  absolute RE2PPS.5;
  RE2PPS_RE2PPS4           : bit  absolute RE2PPS.4;
  RE2PPS_RE2PPS3           : bit  absolute RE2PPS.3;
  RE2PPS_RE2PPS2           : bit  absolute RE2PPS.2;
  RE2PPS_RE2PPS1           : bit  absolute RE2PPS.1;
  RE2PPS_RE2PPS0           : bit  absolute RE2PPS.0;
  CLCDATA                  : byte absolute $0F0F;
  CLCDATA_MLC4OUT          : bit  absolute CLCDATA.3;
  CLCDATA_MCLC3OUT         : bit  absolute CLCDATA.2;
  CLCDATA_MCLC2OUT         : bit  absolute CLCDATA.1;
  CLCDATA_MCLC1OUT         : bit  absolute CLCDATA.0;
  CLC1CON                  : byte absolute $0F10;
  CLC1CON_LC1EN            : bit  absolute CLC1CON.7;
  CLC1CON_LC1OUT           : bit  absolute CLC1CON.5;
  CLC1CON_LC1INTP          : bit  absolute CLC1CON.4;
  CLC1CON_LC1INTN          : bit  absolute CLC1CON.3;
  CLC1CON_LC1MODE2         : bit  absolute CLC1CON.2;
  CLC1CON_LC1MODE1         : bit  absolute CLC1CON.1;
  CLC1CON_LC1MODE0         : bit  absolute CLC1CON.0;
  CLC1POL                  : byte absolute $0F11;
  CLC1POL_LC1POL           : bit  absolute CLC1POL.7;
  CLC1POL_LC1G4POL         : bit  absolute CLC1POL.3;
  CLC1POL_LC1G3POL         : bit  absolute CLC1POL.2;
  CLC1POL_LC1G2POL         : bit  absolute CLC1POL.1;
  CLC1POL_LC1G1POL         : bit  absolute CLC1POL.0;
  CLC1SEL0                 : byte absolute $0F12;
  CLC1SEL0_LC1D1S5         : bit  absolute CLC1SEL0.5;
  CLC1SEL0_LC1D1S4         : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S3         : bit  absolute CLC1SEL0.3;
  CLC1SEL0_LC1D1S2         : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1         : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0         : bit  absolute CLC1SEL0.0;
  CLC1SEL1                 : byte absolute $0F13;
  CLC1SEL1_LC1D2S5         : bit  absolute CLC1SEL1.5;
  CLC1SEL1_LC1D2S4         : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D2S3         : bit  absolute CLC1SEL1.3;
  CLC1SEL1_LC1D2S2         : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D2S1         : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D2S0         : bit  absolute CLC1SEL1.0;
  CLC1SEL2                 : byte absolute $0F14;
  CLC1SEL2_LC1D3S5         : bit  absolute CLC1SEL2.5;
  CLC1SEL2_LC1D3S4         : bit  absolute CLC1SEL2.4;
  CLC1SEL2_LC1D3S3         : bit  absolute CLC1SEL2.3;
  CLC1SEL2_LC1D3S2         : bit  absolute CLC1SEL2.2;
  CLC1SEL2_LC1D3S1         : bit  absolute CLC1SEL2.1;
  CLC1SEL2_LC1D3S0         : bit  absolute CLC1SEL2.0;
  CLC1SEL3                 : byte absolute $0F15;
  CLC1SEL3_LC1D4S5         : bit  absolute CLC1SEL3.5;
  CLC1SEL3_LC1D4S4         : bit  absolute CLC1SEL3.4;
  CLC1SEL3_LC1D4S3         : bit  absolute CLC1SEL3.3;
  CLC1SEL3_LC1D4S2         : bit  absolute CLC1SEL3.2;
  CLC1SEL3_LC1D4S1         : bit  absolute CLC1SEL3.1;
  CLC1SEL3_LC1D4S0         : bit  absolute CLC1SEL3.0;
  CLC1GLS0                 : byte absolute $0F16;
  CLC1GLS0_LC1G1D4T        : bit  absolute CLC1GLS0.7;
  CLC1GLS0_LC1G1D4N        : bit  absolute CLC1GLS0.6;
  CLC1GLS0_LC1G1D3T        : bit  absolute CLC1GLS0.5;
  CLC1GLS0_LC1G1D3N        : bit  absolute CLC1GLS0.4;
  CLC1GLS0_LC1G1D2T        : bit  absolute CLC1GLS0.3;
  CLC1GLS0_LC1G1D2N        : bit  absolute CLC1GLS0.2;
  CLC1GLS0_LC1G1D1T        : bit  absolute CLC1GLS0.1;
  CLC1GLS0_LC1G1D1N        : bit  absolute CLC1GLS0.0;
  CLC1GLS1                 : byte absolute $0F17;
  CLC1GLS1_LC1G2D4T        : bit  absolute CLC1GLS1.7;
  CLC1GLS1_LC1G2D4N        : bit  absolute CLC1GLS1.6;
  CLC1GLS1_LC1G2D3T        : bit  absolute CLC1GLS1.5;
  CLC1GLS1_LC1G2D3N        : bit  absolute CLC1GLS1.4;
  CLC1GLS1_LC1G2D2T        : bit  absolute CLC1GLS1.3;
  CLC1GLS1_LC1G2D2N        : bit  absolute CLC1GLS1.2;
  CLC1GLS1_LC1G2D1T        : bit  absolute CLC1GLS1.1;
  CLC1GLS1_LC1G2D1N        : bit  absolute CLC1GLS1.0;
  CLC1GLS2                 : byte absolute $0F18;
  CLC1GLS2_LC1G3D4T        : bit  absolute CLC1GLS2.7;
  CLC1GLS2_LC1G3D4N        : bit  absolute CLC1GLS2.6;
  CLC1GLS2_LC1G3D3T        : bit  absolute CLC1GLS2.5;
  CLC1GLS2_LC1G3D3N        : bit  absolute CLC1GLS2.4;
  CLC1GLS2_LC1G3D2T        : bit  absolute CLC1GLS2.3;
  CLC1GLS2_LC1G3D2N        : bit  absolute CLC1GLS2.2;
  CLC1GLS2_LC1G3D1T        : bit  absolute CLC1GLS2.1;
  CLC1GLS2_LC1G3D1N        : bit  absolute CLC1GLS2.0;
  CLC1GLS3                 : byte absolute $0F19;
  CLC1GLS3_LC1G4D4T        : bit  absolute CLC1GLS3.7;
  CLC1GLS3_LC1G4D4N        : bit  absolute CLC1GLS3.6;
  CLC1GLS3_LC1G4D3T        : bit  absolute CLC1GLS3.5;
  CLC1GLS3_LC1G4D3N        : bit  absolute CLC1GLS3.4;
  CLC1GLS3_LC1G4D2T        : bit  absolute CLC1GLS3.3;
  CLC1GLS3_LC1G4D2N        : bit  absolute CLC1GLS3.2;
  CLC1GLS3_LC1G4D1T        : bit  absolute CLC1GLS3.1;
  CLC1GLS3_LC1G4D1N        : bit  absolute CLC1GLS3.0;
  CLC2CON                  : byte absolute $0F1A;
  CLC2CON_LC2EN            : bit  absolute CLC2CON.7;
  CLC2CON_LC2OUT           : bit  absolute CLC2CON.5;
  CLC2CON_LC2INTP          : bit  absolute CLC2CON.4;
  CLC2CON_LC2INTN          : bit  absolute CLC2CON.3;
  CLC2CON_LC2MODE2         : bit  absolute CLC2CON.2;
  CLC2CON_LC2MODE1         : bit  absolute CLC2CON.1;
  CLC2CON_LC2MODE0         : bit  absolute CLC2CON.0;
  CLC2POL                  : byte absolute $0F1B;
  CLC2POL_LC2POL           : bit  absolute CLC2POL.7;
  CLC2POL_LC2G4POL         : bit  absolute CLC2POL.3;
  CLC2POL_LC2G3POL         : bit  absolute CLC2POL.2;
  CLC2POL_LC2G2POL         : bit  absolute CLC2POL.1;
  CLC2POL_LC2G1POL         : bit  absolute CLC2POL.0;
  CLC2SEL0                 : byte absolute $0F1C;
  CLC2SEL0_LC2D1S5         : bit  absolute CLC2SEL0.5;
  CLC2SEL0_LC2D1S4         : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S3         : bit  absolute CLC2SEL0.3;
  CLC2SEL0_LC2D1S2         : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1         : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0         : bit  absolute CLC2SEL0.0;
  CLC2SEL1                 : byte absolute $0F1D;
  CLC2SEL1_LC2D2S5         : bit  absolute CLC2SEL1.5;
  CLC2SEL1_LC2D2S4         : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D2S3         : bit  absolute CLC2SEL1.3;
  CLC2SEL1_LC2D2S2         : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D2S1         : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D2S0         : bit  absolute CLC2SEL1.0;
  CLC2SEL2                 : byte absolute $0F1E;
  CLC2SEL2_LC2D3S5         : bit  absolute CLC2SEL2.5;
  CLC2SEL2_LC2D3S4         : bit  absolute CLC2SEL2.4;
  CLC2SEL2_LC2D3S3         : bit  absolute CLC2SEL2.3;
  CLC2SEL2_LC2D3S2         : bit  absolute CLC2SEL2.2;
  CLC2SEL2_LC2D3S1         : bit  absolute CLC2SEL2.1;
  CLC2SEL2_LC2D3S0         : bit  absolute CLC2SEL2.0;
  CLC2SEL3                 : byte absolute $0F1F;
  CLC2SEL3_LC2D4S5         : bit  absolute CLC2SEL3.5;
  CLC2SEL3_LC2D4S4         : bit  absolute CLC2SEL3.4;
  CLC2SEL3_LC2D4S3         : bit  absolute CLC2SEL3.3;
  CLC2SEL3_LC2D4S2         : bit  absolute CLC2SEL3.2;
  CLC2SEL3_LC2D4S1         : bit  absolute CLC2SEL3.1;
  CLC2SEL3_LC2D4S0         : bit  absolute CLC2SEL3.0;
  CLC2GLS0                 : byte absolute $0F20;
  CLC2GLS0_LC2G1D4T        : bit  absolute CLC2GLS0.7;
  CLC2GLS0_LC2G1D4N        : bit  absolute CLC2GLS0.6;
  CLC2GLS0_LC2G1D3T        : bit  absolute CLC2GLS0.5;
  CLC2GLS0_LC2G1D3N        : bit  absolute CLC2GLS0.4;
  CLC2GLS0_LC2G1D2T        : bit  absolute CLC2GLS0.3;
  CLC2GLS0_LC2G1D2N        : bit  absolute CLC2GLS0.2;
  CLC2GLS0_LC2G1D1T        : bit  absolute CLC2GLS0.1;
  CLC2GLS0_LC2G1D1N        : bit  absolute CLC2GLS0.0;
  CLC2GLS1                 : byte absolute $0F21;
  CLC2GLS1_LC2G2D4T        : bit  absolute CLC2GLS1.7;
  CLC2GLS1_LC2G2D4N        : bit  absolute CLC2GLS1.6;
  CLC2GLS1_LC2G2D3T        : bit  absolute CLC2GLS1.5;
  CLC2GLS1_LC2G2D3N        : bit  absolute CLC2GLS1.4;
  CLC2GLS1_LC2G2D2T        : bit  absolute CLC2GLS1.3;
  CLC2GLS1_LC2G2D2N        : bit  absolute CLC2GLS1.2;
  CLC2GLS1_LC2G2D1T        : bit  absolute CLC2GLS1.1;
  CLC2GLS1_LC2G2D1N        : bit  absolute CLC2GLS1.0;
  CLC2GLS2                 : byte absolute $0F22;
  CLC2GLS2_LC2G3D4T        : bit  absolute CLC2GLS2.7;
  CLC2GLS2_LC2G3D4N        : bit  absolute CLC2GLS2.6;
  CLC2GLS2_LC2G3D3T        : bit  absolute CLC2GLS2.5;
  CLC2GLS2_LC2G3D3N        : bit  absolute CLC2GLS2.4;
  CLC2GLS2_LC2G3D2T        : bit  absolute CLC2GLS2.3;
  CLC2GLS2_LC2G3D2N        : bit  absolute CLC2GLS2.2;
  CLC2GLS2_LC2G3D1T        : bit  absolute CLC2GLS2.1;
  CLC2GLS2_LC2G3D1N        : bit  absolute CLC2GLS2.0;
  CLC2GLS3                 : byte absolute $0F23;
  CLC2GLS3_LC2G4D4T        : bit  absolute CLC2GLS3.7;
  CLC2GLS3_LC2G4D4N        : bit  absolute CLC2GLS3.6;
  CLC2GLS3_LC2G4D3T        : bit  absolute CLC2GLS3.5;
  CLC2GLS3_LC2G4D3N        : bit  absolute CLC2GLS3.4;
  CLC2GLS3_LC2G4D2T        : bit  absolute CLC2GLS3.3;
  CLC2GLS3_LC2G4D2N        : bit  absolute CLC2GLS3.2;
  CLC2GLS3_LC2G4D1T        : bit  absolute CLC2GLS3.1;
  CLC2GLS3_LC2G4D1N        : bit  absolute CLC2GLS3.0;
  CLC3CON                  : byte absolute $0F24;
  CLC3CON_LC3EN            : bit  absolute CLC3CON.7;
  CLC3CON_LC3OUT           : bit  absolute CLC3CON.5;
  CLC3CON_LC3INTP          : bit  absolute CLC3CON.4;
  CLC3CON_LC3INTN          : bit  absolute CLC3CON.3;
  CLC3CON_LC3MODE2         : bit  absolute CLC3CON.2;
  CLC3CON_LC3MODE1         : bit  absolute CLC3CON.1;
  CLC3CON_LC3MODE0         : bit  absolute CLC3CON.0;
  CLC3POL                  : byte absolute $0F25;
  CLC3POL_LC3POL           : bit  absolute CLC3POL.7;
  CLC3POL_LC3G4POL         : bit  absolute CLC3POL.3;
  CLC3POL_LC3G3POL         : bit  absolute CLC3POL.2;
  CLC3POL_LC3G2POL         : bit  absolute CLC3POL.1;
  CLC3POL_LC3G1POL         : bit  absolute CLC3POL.0;
  CLC3SEL0                 : byte absolute $0F26;
  CLC3SEL0_LC3D1S5         : bit  absolute CLC3SEL0.5;
  CLC3SEL0_LC3D1S4         : bit  absolute CLC3SEL0.4;
  CLC3SEL0_LC3D1S3         : bit  absolute CLC3SEL0.3;
  CLC3SEL0_LC3D1S2         : bit  absolute CLC3SEL0.2;
  CLC3SEL0_LC3D1S1         : bit  absolute CLC3SEL0.1;
  CLC3SEL0_LC3D1S0         : bit  absolute CLC3SEL0.0;
  CLC3SEL1                 : byte absolute $0F27;
  CLC3SEL1_LC3D2S5         : bit  absolute CLC3SEL1.5;
  CLC3SEL1_LC3D2S4         : bit  absolute CLC3SEL1.4;
  CLC3SEL1_LC3D2S3         : bit  absolute CLC3SEL1.3;
  CLC3SEL1_LC3D2S2         : bit  absolute CLC3SEL1.2;
  CLC3SEL1_LC3D2S1         : bit  absolute CLC3SEL1.1;
  CLC3SEL1_LC3D2S0         : bit  absolute CLC3SEL1.0;
  CLC3SEL2                 : byte absolute $0F28;
  CLC3SEL2_LC3D3S5         : bit  absolute CLC3SEL2.5;
  CLC3SEL2_LC3D3S4         : bit  absolute CLC3SEL2.4;
  CLC3SEL2_LC3D3S3         : bit  absolute CLC3SEL2.3;
  CLC3SEL2_LC3D3S2         : bit  absolute CLC3SEL2.2;
  CLC3SEL2_LC3D3S1         : bit  absolute CLC3SEL2.1;
  CLC3SEL2_LC3D3S0         : bit  absolute CLC3SEL2.0;
  CLC3SEL3                 : byte absolute $0F29;
  CLC3SEL3_LC3D4S5         : bit  absolute CLC3SEL3.5;
  CLC3SEL3_LC3D4S4         : bit  absolute CLC3SEL3.4;
  CLC3SEL3_LC3D4S3         : bit  absolute CLC3SEL3.3;
  CLC3SEL3_LC3D4S2         : bit  absolute CLC3SEL3.2;
  CLC3SEL3_LC3D4S1         : bit  absolute CLC3SEL3.1;
  CLC3SEL3_LC3D4S0         : bit  absolute CLC3SEL3.0;
  CLC3GLS0                 : byte absolute $0F2A;
  CLC3GLS0_LC3G1D4T        : bit  absolute CLC3GLS0.7;
  CLC3GLS0_LC3G1D4N        : bit  absolute CLC3GLS0.6;
  CLC3GLS0_LC3G1D3T        : bit  absolute CLC3GLS0.5;
  CLC3GLS0_LC3G1D3N        : bit  absolute CLC3GLS0.4;
  CLC3GLS0_LC3G1D2T        : bit  absolute CLC3GLS0.3;
  CLC3GLS0_LC3G1D2N        : bit  absolute CLC3GLS0.2;
  CLC3GLS0_LC3G1D1T        : bit  absolute CLC3GLS0.1;
  CLC3GLS0_LC3G1D1N        : bit  absolute CLC3GLS0.0;
  CLC3GLS1                 : byte absolute $0F2B;
  CLC3GLS1_LC3G2D4T        : bit  absolute CLC3GLS1.7;
  CLC3GLS1_LC3G2D4N        : bit  absolute CLC3GLS1.6;
  CLC3GLS1_LC3G2D3T        : bit  absolute CLC3GLS1.5;
  CLC3GLS1_LC3G2D3N        : bit  absolute CLC3GLS1.4;
  CLC3GLS1_LC3G2D2T        : bit  absolute CLC3GLS1.3;
  CLC3GLS1_LC3G2D2N        : bit  absolute CLC3GLS1.2;
  CLC3GLS1_LC3G2D1T        : bit  absolute CLC3GLS1.1;
  CLC3GLS1_LC3G2D1N        : bit  absolute CLC3GLS1.0;
  CLC3GLS2                 : byte absolute $0F2C;
  CLC3GLS2_LC3G3D4T        : bit  absolute CLC3GLS2.7;
  CLC3GLS2_LC3G3D4N        : bit  absolute CLC3GLS2.6;
  CLC3GLS2_LC3G3D3T        : bit  absolute CLC3GLS2.5;
  CLC3GLS2_LC3G3D3N        : bit  absolute CLC3GLS2.4;
  CLC3GLS2_LC3G3D2T        : bit  absolute CLC3GLS2.3;
  CLC3GLS2_LC3G3D2N        : bit  absolute CLC3GLS2.2;
  CLC3GLS2_LC3G3D1T        : bit  absolute CLC3GLS2.1;
  CLC3GLS2_LC3G3D1N        : bit  absolute CLC3GLS2.0;
  CLC3GLS3                 : byte absolute $0F2D;
  CLC3GLS3_LC3G4D4T        : bit  absolute CLC3GLS3.7;
  CLC3GLS3_LC3G4D4N        : bit  absolute CLC3GLS3.6;
  CLC3GLS3_LC3G4D3T        : bit  absolute CLC3GLS3.5;
  CLC3GLS3_LC3G4D3N        : bit  absolute CLC3GLS3.4;
  CLC3GLS3_LC3G4D2T        : bit  absolute CLC3GLS3.3;
  CLC3GLS3_LC3G4D2N        : bit  absolute CLC3GLS3.2;
  CLC3GLS3_LC3G4D1T        : bit  absolute CLC3GLS3.1;
  CLC3GLS3_LC3G4D1N        : bit  absolute CLC3GLS3.0;
  CLC4CON                  : byte absolute $0F2E;
  CLC4CON_LC4EN            : bit  absolute CLC4CON.7;
  CLC4CON_LC4OUT           : bit  absolute CLC4CON.5;
  CLC4CON_LC4INTP          : bit  absolute CLC4CON.4;
  CLC4CON_LC4INTN          : bit  absolute CLC4CON.3;
  CLC4CON_LC4MODE2         : bit  absolute CLC4CON.2;
  CLC4CON_LC4MODE1         : bit  absolute CLC4CON.1;
  CLC4CON_LC4MODE0         : bit  absolute CLC4CON.0;
  CLC4POL                  : byte absolute $0F2F;
  CLC4POL_LC4POL           : bit  absolute CLC4POL.7;
  CLC4POL_LC4G4POL         : bit  absolute CLC4POL.3;
  CLC4POL_LC4G3POL         : bit  absolute CLC4POL.2;
  CLC4POL_LC4G2POL         : bit  absolute CLC4POL.1;
  CLC4POL_LC4G1POL         : bit  absolute CLC4POL.0;
  CLC4SEL0                 : byte absolute $0F30;
  CLC4SEL0_LC4D1S5         : bit  absolute CLC4SEL0.5;
  CLC4SEL0_LC4D1S4         : bit  absolute CLC4SEL0.4;
  CLC4SEL0_LC4D1S3         : bit  absolute CLC4SEL0.3;
  CLC4SEL0_LC4D1S2         : bit  absolute CLC4SEL0.2;
  CLC4SEL0_LC4D1S1         : bit  absolute CLC4SEL0.1;
  CLC4SEL0_LC4D1S0         : bit  absolute CLC4SEL0.0;
  CLC4SEL1                 : byte absolute $0F31;
  CLC4SEL1_LC4D2S5         : bit  absolute CLC4SEL1.5;
  CLC4SEL1_LC4D2S4         : bit  absolute CLC4SEL1.4;
  CLC4SEL1_LC4D2S3         : bit  absolute CLC4SEL1.3;
  CLC4SEL1_LC4D2S2         : bit  absolute CLC4SEL1.2;
  CLC4SEL1_LC4D2S1         : bit  absolute CLC4SEL1.1;
  CLC4SEL1_LC4D2S0         : bit  absolute CLC4SEL1.0;
  CLC4SEL2                 : byte absolute $0F32;
  CLC4SEL2_LC4D3S5         : bit  absolute CLC4SEL2.5;
  CLC4SEL2_LC4D3S4         : bit  absolute CLC4SEL2.4;
  CLC4SEL2_LC4D3S3         : bit  absolute CLC4SEL2.3;
  CLC4SEL2_LC4D3S2         : bit  absolute CLC4SEL2.2;
  CLC4SEL2_LC4D3S1         : bit  absolute CLC4SEL2.1;
  CLC4SEL2_LC4D3S0         : bit  absolute CLC4SEL2.0;
  CLC4SEL3                 : byte absolute $0F33;
  CLC4SEL3_LC4D4S5         : bit  absolute CLC4SEL3.5;
  CLC4SEL3_LC4D4S4         : bit  absolute CLC4SEL3.4;
  CLC4SEL3_LC4D4S3         : bit  absolute CLC4SEL3.3;
  CLC4SEL3_LC4D4S2         : bit  absolute CLC4SEL3.2;
  CLC4SEL3_LC4D4S1         : bit  absolute CLC4SEL3.1;
  CLC4SEL3_LC4D4S0         : bit  absolute CLC4SEL3.0;
  CLC4GLS0                 : byte absolute $0F34;
  CLC4GLS0_LC4G1D4T        : bit  absolute CLC4GLS0.7;
  CLC4GLS0_LC4G1D4N        : bit  absolute CLC4GLS0.6;
  CLC4GLS0_LC4G1D3T        : bit  absolute CLC4GLS0.5;
  CLC4GLS0_LC4G1D3N        : bit  absolute CLC4GLS0.4;
  CLC4GLS0_LC4G1D2T        : bit  absolute CLC4GLS0.3;
  CLC4GLS0_LC4G1D2N        : bit  absolute CLC4GLS0.2;
  CLC4GLS0_LC4G1D1T        : bit  absolute CLC4GLS0.1;
  CLC4GLS0_LC4G1D1N        : bit  absolute CLC4GLS0.0;
  CLC4GLS1                 : byte absolute $0F35;
  CLC4GLS1_LC4G2D4T        : bit  absolute CLC4GLS1.7;
  CLC4GLS1_LC4G2D4N        : bit  absolute CLC4GLS1.6;
  CLC4GLS1_LC4G2D3T        : bit  absolute CLC4GLS1.5;
  CLC4GLS1_LC4G2D3N        : bit  absolute CLC4GLS1.4;
  CLC4GLS1_LC4G2D2T        : bit  absolute CLC4GLS1.3;
  CLC4GLS1_LC4G2D2N        : bit  absolute CLC4GLS1.2;
  CLC4GLS1_LC4G2D1T        : bit  absolute CLC4GLS1.1;
  CLC4GLS1_LC4G2D1N        : bit  absolute CLC4GLS1.0;
  CLC4GLS2                 : byte absolute $0F36;
  CLC4GLS2_LC4G3D4T        : bit  absolute CLC4GLS2.7;
  CLC4GLS2_LC4G3D4N        : bit  absolute CLC4GLS2.6;
  CLC4GLS2_LC4G3D3T        : bit  absolute CLC4GLS2.5;
  CLC4GLS2_LC4G3D3N        : bit  absolute CLC4GLS2.4;
  CLC4GLS2_LC4G3D2T        : bit  absolute CLC4GLS2.3;
  CLC4GLS2_LC4G3D2N        : bit  absolute CLC4GLS2.2;
  CLC4GLS2_LC4G3D1T        : bit  absolute CLC4GLS2.1;
  CLC4GLS2_LC4G3D1N        : bit  absolute CLC4GLS2.0;
  CLC4GLS3                 : byte absolute $0F37;
  CLC4GLS3_LC4G4D4T        : bit  absolute CLC4GLS3.7;
  CLC4GLS3_LC4G4D4N        : bit  absolute CLC4GLS3.6;
  CLC4GLS3_LC4G4D3T        : bit  absolute CLC4GLS3.5;
  CLC4GLS3_LC4G4D3N        : bit  absolute CLC4GLS3.4;
  CLC4GLS3_LC4G4D2T        : bit  absolute CLC4GLS3.3;
  CLC4GLS3_LC4G4D2N        : bit  absolute CLC4GLS3.2;
  CLC4GLS3_LC4G4D1T        : bit  absolute CLC4GLS3.1;
  CLC4GLS3_LC4G4D1N        : bit  absolute CLC4GLS3.0;
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
  {$SET_STATE_RAM '00C-01F:SFR'}            // Bank 0 : PORTA, PORTB, PORTC, PORTD, PORTE, PIR1, PIR2, PIR3, PIR4, PIR5, PIR6, TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR3L, TMR3H, T3CON, T3GCON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-09F:SFR'}            // Bank 1 : TRISA, TRISB, TRISC, TRISD, TRISE, PIE1, PIE2, PIE3, PIE4, PIE5, PIE6, OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, BORCON, FVRCON, ZCD1CON
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-11D:SFR'}            // Bank 2 : LATA, LATB, LATC, LATD, LATE, CMOUT, CM1CON0, CM1CON1, CM1NSEL, CM1PSEL, CM2CON0, CM2CON1, CM2NSEL, CM2PSEL, CM3CON0, CM3CON1, CM3NSEL, CM3PSEL
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-197:SFR'}            // Bank 3 : ANSELA, ANSELB, ANSELC, ANSELD, ANSELE, PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-217:SFR'}            // Bank 4 : WPUA, WPUB, WPUC, WPUD, WPUE, SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '21B-21F:SFR'}            // Bank 4 : MD3CON0, MD3CON1, MD3SRC, MD3CARL, MD3CARH
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-29C:SFR'}            // Bank 5 : ODCONA, ODCONB, ODCONC, ODCOND, ODCONE, CCPR1L, CCPR1H, CCP1CON, CCP1CAP, CCPR2L, CCPR2H, CCP2CON, CCP2CAP, CCPR7L, CCPR7H, CCP7CON, CCP7CAP
  {$SET_STATE_RAM '29E-29F:SFR'}            // Bank 5 : CCPTMRS1, CCPTMRS2
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-319:SFR'}            // Bank 6 : SLRCONA, SLRCONB, SLRCONC, SLRCOND, SLRCONE, CCPR8L, CCPR8H, CCP8CON, CCP8CAP, MD1CON0, MD1CON1, MD1SRC, MD1CARL, MD1CARH
  {$SET_STATE_RAM '31B-31F:SFR'}            // Bank 6 : MD2CON0, MD2CON1, MD2SRC, MD2CARL, MD2CARH
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-399:SFR'}            // Bank 7 : INLVLA, INLVLB, INLVLC, INLVLD, INLVE, IOCAP, IOCAN, IOCAF, IOCBP, IOCBN, IOCBF, IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '39D-39F:SFR'}            // Bank 7 : IOCEP, IOCEN, IOCEF
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '40D-40D:SFR'}            // Bank 8 : HIDRVB
  {$SET_STATE_RAM '40F-418:SFR'}            // Bank 8 : TMR5L, TMR5H, T5CON, T5GCON, T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '41A-41F:SFR'}            // Bank 8 : T6TMR, T6PR, T6CON, T6HLT, T6CLKCON, T6RST
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '48E-498:SFR'}            // Bank 9 : ADRESL, ADRESH, ADCON0, ADCON1, ADCON2, T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '49A-49F:SFR'}            // Bank 9 : T8TMR, T8PR, T8CON, T8HLT, T8CLKCON, T8RST
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '50F-51E:SFR'}            // Bank 10 : OPA1NCHS, OPA1PCHS, OPA1CON, OPA1ORS, OPA2NCHS, OPA2PCHS, OPA2CON, OPA2ORS, OPA3NCHS, OPA3PCHS, OPA3CON, OPA3ORS, OPA4NCHS, OPA4PCHS, OPA4CON, OPA4ORS
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '58D-59F:SFR'}            // Bank 11 : DACLD, DAC1CON0, DAC1REFL, DAC1REFH, DAC2CON0, DAC2REFL, DAC2REFH, DAC3CON0, DAC3REF, DAC4CON0, DAC4REF, DAC5CON0, DAC5REFL, DAC5REFH, DAC6CON0, DAC6REFL, DAC6REFH, DAC7CON0, DAC7REF
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '60C-61F:SFR'}            // Bank 12 : DAC8CON0, DAC8REF, PRG4RTSS, PRG4FTSS, PRG4INS, PRG4CON0, PRG4CON1, PRG4CON2, PWM3DCL, PWM3DCH, PWM3CON, PWM4DCL, PWM4DCH, PWM4CON, PWM9DCL, PWM9DCH, PWM9CON, PWM10DCL, PWM10DCH, PWM10CON
  {$SET_STATE_RAM '620-66F:GPR'}           
  {$SET_STATE_RAM '68D-69F:SFR'}            // Bank 13 : COG1PHR, COG1PHF, COG1BLKR, COG1BLKF, COG1DBR, COG1DBF, COG1CON0, COG1CON1, COG1RIS0, COG1RIS1, COG1RSIM0, COG1RSIM1, COG1FIS0, COG1FIS1, COG1FSIM0, COG1FSIM1, COG1ASD0, COG1ASD1, COG1STR
  {$SET_STATE_RAM '6A0-6EF:GPR'}           
  {$SET_STATE_RAM '70D-71F:SFR'}            // Bank 14 : COG2PHR, COG2PHF, COG2BLKR, COG2BLKF, COG2DBR, COG2DBF, COG2CON0, COG2CON1, COG2RIS0, COG2RIS1, COG2RSIM0, COG2RSIM1, COG2FIS0, COG2FIS1, COG2FSIM0, COG2FSIM1, COG2ASD0, COG2ASD1, COG2STR
  {$SET_STATE_RAM '720-76F:GPR'}           
  {$SET_STATE_RAM '78E-79F:SFR'}            // Bank 15 : PRG1RTSS, PRG1FTSS, PRG1INS, PRG1CON0, PRG1CON1, PRG1CON2, PRG2RTSS, PRG2FTSS, PRG2INS, PRG2CON0, PRG2CON1, PRG2CON2, PRG3RTSS, PRG3FTSS, PRG3INS, PRG3CON0, PRG3CON1, PRG3CON2
  {$SET_STATE_RAM '7A0-7EF:GPR'}           
  {$SET_STATE_RAM '80D-81F:SFR'}            // Bank 16 : COG3PHR, COG3PHF, COG3BLKR, COG3BLKF, COG3DBR, COG3DBF, COG3CON0, COG3CON1, COG3RIS0, COG3RIS1, COG3RSIM0, COG3RSIM1, COG3FIS0, COG3FIS1, COG3FSIM0, COG3FSIM1, COG3ASD0, COG3ASD1, COG3STR
  {$SET_STATE_RAM '820-86F:GPR'}           
  {$SET_STATE_RAM '88D-89F:SFR'}            // Bank 17 : COG4PHR, COG4PHF, COG4BLKR, COG4BLKF, COG4DBR, COG4DBF, COG4CON0, COG4CON1, COG4RIS0, COG4RIS1, COG4RSIM0, COG4RSIM1, COG4FIS0, COG4FIS1, COG4FSIM0, COG4FSIM1, COG4ASD0, COG4ASD1, COG4STR
  {$SET_STATE_RAM '8A0-8EF:GPR'}           
  {$SET_STATE_RAM '90C-91F:SFR'}            // Bank 18 : CM4CON0, CM4CON1, CM4NSEL, CM4PSEL, CM5CON0, CM5CON1, CM5NSEL, CM5PSEL, CM6CON0, CM6CON1, CM6NSEL, CM6PSEL, CM7CON0, CM7CON1, CM7NSEL, CM7PSEL, CM8CON0, CM8CON1, CM8NSEL, CM8PSEL
  {$SET_STATE_RAM '920-96F:GPR'}           
  {$SET_STATE_RAM '9A0-9EF:GPR'}           
  {$SET_STATE_RAM 'A20-A6F:GPR'}           
  {$SET_STATE_RAM 'AA0-AEF:GPR'}           
  {$SET_STATE_RAM 'B20-B6F:GPR'}           
  {$SET_STATE_RAM 'BA0-BEF:GPR'}           
  {$SET_STATE_RAM 'C20-C6F:GPR'}           
  {$SET_STATE_RAM 'CA0-CBF:GPR'}           
  {$SET_STATE_RAM 'D1B-D1F:SFR'}            // Bank 26 : MD4CON0, MD4CON1, MD4SRC, MD4CARL, MD4CARH
  {$SET_STATE_RAM 'D8E-DD0:SFR'}            // Bank 27 : PWMEN, PWMLD, PWMOUT, PWM5PHL, PWM5PHH, PWM5DCL, PWM5DCH, PWM5PRL, PWM5PRH, PWM5OFL, PWM5OFH, PWM5TMRL, PWM5TMRH, PWM5CON, PWM5INTE, PWM5INTF, PWM5CLKCON, PWM5LDCON, PWM5OFCON, PWM6PHL, PWM6PHH, PWM6DCL, PWM6DCH, PWM6PRL, PWM6PRH, PWM6OFL, PWM6OFH, PWM6TMRL, PWM6TMRH, PWM6CON, PWM6INTE, PWM6INTF, PWM6CLKCON, PWM6LDCON, PWM6OFCON, PWM11PHL, PWM11PHH, PWM11DCL, PWM11DCH, PWM11PRL, PWM11PRH, PWM11OFL, PWM11OFH, PWM11TMRL, PWM11TMRH, PWM11CON, PWM11INTE, PWM11INTF, PWM11CLKCON, PWM11LDCON, PWM11OFCON, PWM12PHL, PWM12PHH, PWM12DCL, PWM12DCH, PWM12PRL, PWM12PRH, PWM12OFL, PWM12OFH, PWM12TMRL, PWM12TMRH, PWM12CON, PWM12INTE, PWM12INTF, PWM12CLKCON, PWM12LDCON, PWM12OFCON
  {$SET_STATE_RAM 'E0C-E3E:SFR'}            // Bank 28 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, T3CKIPPS, T3GPPS, T5CKIPPS, T5GPPS, T2INPPS, T4INPPS, T6INPPS, T8INPPS, CCP1PPS, CCP2PPS, CCP7PPS, CCP8PPS, COG1INPPS, COG2INPPS, COG3INPPS, COG4INPPS, MD1CLPPS, MD1CHPPS, MD1MODPPS, MD2CLPPS, MD2CHPPS, MD2MODPPS, MD3CLPPS, MD3CHPPS, MD3MODPPS, MD4CLPPS, MD4CHPPS, MD4MODPPS, PRG1RPPS, PRG1FPPS, PRG2RPPS, PRG2FPPS, PRG3RPPS, PRG3FPPS, PRG4RPPS, PRG4FPPS, CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS, ADCACTPPS, SSPCLKPPS, SSPDATPPS, SSPSSPPS, RXPPS, CKPPS
  {$SET_STATE_RAM 'E90-EB2:SFR'}            // Bank 29 : RA0PPS, RA1PPS, RA2PPS, RA3PPS, RA4PPS, RA5PPS, RA6PPS, RA7PPS, RB0PPS, RB1PPS, RB2PPS, RB3PPS, RB4PPS, RB5PPS, RB6PPS, RB7PPS, RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS, RC6PPS, RC7PPS, RD0PPS, RD1PPS, RD2PPS, RD3PPS, RD4PPS, RD5PPS, RD6PPS, RD7PPS, RE0PPS, RE1PPS, RE2PPS
  {$SET_STATE_RAM 'F0F-F37:SFR'}            // Bank 30 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3, CLC4CON, CLC4POL, CLC4SEL0, CLC4SEL1, CLC4SEL2, CLC4SEL3, CLC4GLS0, CLC4GLS1, CLC4GLS2, CLC4GLS3
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:0F'} // PORTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:3F'} // PIR3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '014:7F'} // PIR4 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '016:0F'} // PIR6 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01A:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:FD'} // T3CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:0F'} // TRISE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:3F'} // PIE3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '094:7F'} // PIE4 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:0F'} // PIE6 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09B:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:B3'} // ZCD1CON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '110:07'} // LATE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:DF'} // CM1CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:03'} // CM1CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:0F'} // CM1NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:0F'} // CM1PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:DF'} // CM2CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '117:03'} // CM2CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:0F'} // CM2NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:0F'} // CM2PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11A:DF'} // CM3CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11B:03'} // CM3CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:0F'} // CM3NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:0F'} // CM3PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:3F'} // ANSELA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:3F'} // ANSELB bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:FC'} // ANSELC bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '190:07'} // ANSELE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:0F'} // WPUE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21B:B1'} // MD3CON0 bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21C:33'} // MD3CON1 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21D:1F'} // MD3SRC bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21E:1F'} // MD3CARL bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21F:1F'} // MD3CARH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '290:07'} // ODCONE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:BF'} // CCP1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:0F'} // CCP1CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '297:BF'} // CCP2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '298:0F'} // CCP2CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29B:BF'} // CCP7CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29C:0F'} // CCP7CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '310:07'} // SLRCONE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:BF'} // CCP8CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '314:0F'} // CCP8CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '315:B1'} // MD1CON0 bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '316:33'} // MD1CON1 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '317:1F'} // MD1SRC bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '318:1F'} // MD1CARL bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '319:1F'} // MD1CARH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31B:B1'} // MD2CON0 bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31C:33'} // MD2CON1 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31D:1F'} // MD2SRC bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31E:1F'} // MD2CARL bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31F:1F'} // MD2CARH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '390:0F'} // INLVE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:08'} // IOCEP bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:08'} // IOCEN bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:08'} // IOCEF bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '40D:03'} // HIDRVB bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '411:FD'} // T5CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:0F'} // T4CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '418:1F'} // T4RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:0F'} // T6CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41F:1F'} // T6RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '491:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '492:3F'} // ADCON2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '497:0F'} // T2CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '498:1F'} // T2RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49E:0F'} // T8CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49F:1F'} // T8RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '50F:0F'} // OPA1NCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '510:0F'} // OPA1PCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '511:97'} // OPA1CON bits 6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '512:1F'} // OPA1ORS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '513:0F'} // OPA2NCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '514:0F'} // OPA2PCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '515:97'} // OPA2CON bits 6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '516:1F'} // OPA2ORS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '517:0F'} // OPA3NCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '518:0F'} // OPA3PCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '519:D7'} // OPA3CON bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '51A:1F'} // OPA3ORS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '51B:0F'} // OPA4NCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '51C:0F'} // OPA4PCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '51D:D7'} // OPA4CON bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '58D:33'} // DACLD bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '594:BF'} // DAC3CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '595:1F'} // DAC3REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '596:BF'} // DAC4CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '597:1F'} // DAC4REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59E:BF'} // DAC7CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59F:1F'} // DAC7REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60C:BF'} // DAC8CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60D:1F'} // DAC8REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60E:0F'} // PRG4RTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60F:0F'} // PRG4FTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '610:0F'} // PRG4INS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '611:BF'} // PRG4CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '612:07'} // PRG4CON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:1F'} // PRG4CON2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '614:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '616:B0'} // PWM3CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '617:C0'} // PWM4DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:B0'} // PWM4CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61A:C0'} // PWM9DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61C:B0'} // PWM9CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61D:C0'} // PWM10DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61F:B0'} // PWM10CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68D:3F'} // COG1PHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68E:3F'} // COG1PHF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68F:3F'} // COG1BLKR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '690:3F'} // COG1BLKF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // COG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // COG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:DF'} // COG1CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:CF'} // COG1CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69D:FC'} // COG1ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70D:3F'} // COG2PHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70E:3F'} // COG2PHF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70F:3F'} // COG2BLKR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '710:3F'} // COG2BLKF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:3F'} // COG2DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:3F'} // COG2DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '713:DF'} // COG2CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '714:CF'} // COG2CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71D:FC'} // COG2ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '78E:0F'} // PRG1RTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '78F:0F'} // PRG1FTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '790:0F'} // PRG1INS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '791:BF'} // PRG1CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '792:07'} // PRG1CON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '793:1F'} // PRG1CON2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '794:0F'} // PRG2RTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '795:0F'} // PRG2FTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '796:0F'} // PRG2INS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:BF'} // PRG2CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '798:07'} // PRG2CON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:1F'} // PRG2CON2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79A:0F'} // PRG3RTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79B:0F'} // PRG3FTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79C:0F'} // PRG3INS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79D:BF'} // PRG3CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79E:07'} // PRG3CON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79F:1F'} // PRG3CON2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80D:3F'} // COG3PHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80E:3F'} // COG3PHF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80F:3F'} // COG3BLKR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '810:3F'} // COG3BLKF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '811:3F'} // COG3DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '812:3F'} // COG3DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '813:DF'} // COG3CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '814:CF'} // COG3CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '816:FA'} // COG3RIS1 bits 2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '818:FA'} // COG3RSIM1 bits 2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81A:FA'} // COG3FIS1 bits 2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81C:FA'} // COG3FSIM1 bits 2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81D:FC'} // COG3ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88D:3F'} // COG4PHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88E:3F'} // COG4PHF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88F:3F'} // COG4BLKR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '890:3F'} // COG4BLKF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '891:3F'} // COG4DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '892:3F'} // COG4DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '893:DF'} // COG4CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '894:CF'} // COG4CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '89D:FC'} // COG4ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90C:DF'} // CM4CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90D:03'} // CM4CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90E:0F'} // CM4NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90F:0F'} // CM4PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '910:DF'} // CM5CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '911:03'} // CM5CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '912:0F'} // CM5NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '913:0F'} // CM5PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '914:DF'} // CM6CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '915:03'} // CM6CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '916:0F'} // CM6NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '917:0F'} // CM6PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '918:DF'} // CM7CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '919:03'} // CM7CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91A:0F'} // CM7NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91B:0F'} // CM7PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91C:DF'} // CM8CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91D:03'} // CM8CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91E:0F'} // CM8NSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91F:0F'} // CM8PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D1B:B1'} // MD4CON0 bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D1C:33'} // MD4CON1 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D1D:1F'} // MD4SRC bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D1E:1F'} // MD4CARL bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D1F:1F'} // MD4CARH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8E:0F'} // PWMEN bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8F:0F'} // PWMLD bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D90:0F'} // PWMOUT bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9B:BC'} // PWM5CON bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9C:0F'} // PWM5INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9D:0F'} // PWM5INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9E:73'} // PWM5CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9F:C3'} // PWM5LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DA0:73'} // PWM5OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAB:BC'} // PWM6CON bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAC:0F'} // PWM6INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAD:0F'} // PWM6INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAE:73'} // PWM6CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAF:C3'} // PWM6LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DB0:73'} // PWM6OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBB:BC'} // PWM11CON bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBC:0F'} // PWM11INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBD:0F'} // PWM11INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBE:73'} // PWM11CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBF:C3'} // PWM11LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DC0:73'} // PWM11OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DCB:BC'} // PWM12CON bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DCC:0F'} // PWM12INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DCD:0F'} // PWM12INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DCE:73'} // PWM12CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DCF:C3'} // PWM12LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DD0:73'} // PWM12OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0C:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0D:3F'} // INTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0E:3F'} // T0CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0F:3F'} // T1CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E10:3F'} // T1GPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E11:3F'} // T3CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E12:3F'} // T3GPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E13:3F'} // T5CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E14:3F'} // T5GPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E15:3F'} // T2INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E16:3F'} // T4INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E17:3F'} // T6INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E18:3F'} // T8INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E19:3F'} // CCP1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1A:3F'} // CCP2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1B:3F'} // CCP7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1C:3F'} // CCP8PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1D:3F'} // COG1INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1E:3F'} // COG2INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1F:3F'} // COG3INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E20:3F'} // COG4INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E21:3F'} // MD1CLPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E22:3F'} // MD1CHPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E23:3F'} // MD1MODPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E24:3F'} // MD2CLPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E25:3F'} // MD2CHPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E26:3F'} // MD2MODPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E27:3F'} // MD3CLPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E28:3F'} // MD3CHPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E29:3F'} // MD3MODPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2A:3F'} // MD4CLPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2B:3F'} // MD4CHPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2C:3F'} // MD4MODPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2D:3F'} // PRG1RPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2E:3F'} // PRG1FPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2F:3F'} // PRG2RPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E30:3F'} // PRG2FPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E31:3F'} // PRG3RPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E32:3F'} // PRG3FPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E33:3F'} // PRG4RPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E34:3F'} // PRG4FPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E35:3F'} // CLCIN0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E36:3F'} // CLCIN1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E37:3F'} // CLCIN2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E38:3F'} // CLCIN3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E39:3F'} // ADCACTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E3A:3F'} // SSPCLKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E3B:3F'} // SSPDATPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E3C:3F'} // SSPSSPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E3D:3F'} // RXPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E3E:3F'} // CKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E90:3F'} // RA0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E91:3F'} // RA1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E92:3F'} // RA2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E93:3F'} // RA3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E94:3F'} // RA4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E95:3F'} // RA5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E96:3F'} // RA6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E97:3F'} // RA7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E98:3F'} // RB0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E99:3F'} // RB1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9A:3F'} // RB2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9B:3F'} // RB3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9C:3F'} // RB4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9D:3F'} // RB5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9E:3F'} // RB6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9F:3F'} // RB7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA0:3F'} // RC0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA1:3F'} // RC1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA2:3F'} // RC2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA3:3F'} // RC3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA4:3F'} // RC4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA5:3F'} // RC5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA6:3F'} // RC6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA7:3F'} // RC7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA8:3F'} // RD0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA9:3F'} // RD1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAA:3F'} // RD2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAB:3F'} // RD3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAC:3F'} // RD4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAD:3F'} // RD5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAE:3F'} // RD6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EAF:3F'} // RD7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB0:3F'} // RE0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB1:3F'} // RE1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EB2:3F'} // RE2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F0F:0F'} // CLCDATA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F10:BF'} // CLC1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F12:3F'} // CLC1SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F13:3F'} // CLC1SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F14:3F'} // CLC1SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F15:3F'} // CLC1SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1A:BF'} // CLC2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1B:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1C:3F'} // CLC2SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1D:3F'} // CLC2SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1E:3F'} // CLC2SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1F:3F'} // CLC2SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F24:BF'} // CLC3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F25:8F'} // CLC3POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F26:3F'} // CLC3SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F27:3F'} // CLC3SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F28:3F'} // CLC3SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F29:3F'} // CLC3SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2E:BF'} // CLC4CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2F:8F'} // CLC4POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F30:3F'} // CLC4SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F31:3F'} // CLC4SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F32:3F'} // CLC4SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F33:3F'} // CLC4SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : VPP/MCLR_n/ICD_n/IOCE3/RE3
  // Pin  2 : AN0/C1IN0-/C2IN0-/C3IN0-/C4IN0-/C5IN0-/C6IN0-/C7IN0-/C8IN0-/IOCA0/RA0
  // Pin  3 : OPA1OUT/PRG1IN0/PRG2IN1/OPA2IN1+/OPA2IN1-/AN1/C1IN1-/C2IN1-/C3IN1-/C4IN1-/IOCA1/RA1
  // Pin  4 : DAC1OUT1/DAC1REF0-/DAC2REF0-/DAC3REF0-/DAC4REF0-/DAC5REF0-/DAC6REF0-/DAC7REF0-/DAC8REF0-/VREF-/AN2/C1IN0+/C2IN0+/C3IN0+/C4IN0+/C5IN0+/C6IN0+/C7IN0+/C8IN0+/IOCA2/RA2
  // Pin  5 : DAC1REF0+/DAC2REF0+/DAC3REF0+/DAC4REF0+/DAC5REF0+/DAC6REF0+/DAC7REF0+/DAC8REF0+/VREF+/AN3/C1IN1+/IOCA3/RA3
  // Pin  6 : OPA1IN0+/IOCA4/RA4
  // Pin  7 : DAC2OUT1/OPA1IN0-/AN4/IOCA5/RA5
  // Pin  8 : DAC6REF1+/DAC8REF1+/AN5/RE0
  // Pin  9 : DAC6OUT1/DAC6REF1-/DAC8REF1-/AN6/RE1
  // Pin 10 : DAC8OUT1/AN7/RE2
  // Pin 11 : AVDD
  // Pin 12 : AVSS
  // Pin 13 : OSC1/CLKIN/IOCA7/RA7
  // Pin 14 : OSC2/CLKOUT/C6IN1+/IOCA6/RA6
  // Pin 15 : SOSCO/DAC5OUT1/IOCC0/RC0
  // Pin 16 : SOSCI/DAC7OUT1/IOCC1/RC1
  // Pin 17 : AN14/C5IN2-/C6IN2-/IOCC2/RC2
  // Pin 18 : AN15/SCL/IOCC3/RC3
  // Pin 19 : OPA4IN0+/AN20/RD0
  // Pin 20 : OPA4OUT/OPA3IN1+/OPA3IN1-/PRG3IN1/PRG4IN0/AN21/C1IN4-/C2IN4-/C3IN4-/C4IN4-/C5IN4-/C6IN4-/C7IN4-/C8IN4-/RD1
  // Pin 21 : DAC4OUT1/OPA4IN0-/AN22/RD2
  // Pin 22 : AN23/C8IN2-/RD3
  // Pin 23 : AN16/C5IN3-/C6IN3-/SDA/IOCC4/RC4
  // Pin 24 : OPA3IN0+/AN17/IOCC5/RC5
  // Pin 25 : OPA3OUT/PRG3IN0/OPA4IN1+/OPA4IN1-/PRG4IN1/AN18/C5IN1-/C6IN1-/C7IN1-/C8IN1-/IOCC6/RC6
  // Pin 26 : OPA3IN0-/AN19/IOCC7/RC7
  // Pin 27 : AN24/C7IN2-/RD4
  // Pin 28 : AN25/C7IN3-/C8IN3-/RD5
  // Pin 29 : AN26/C7IN1+/RD6
  // Pin 30 : AN27/C8IN1+/RD7
  // Pin 31 : VSS
  // Pin 32 : VDD
  // Pin 33 : AN12/ZCD/C2IN1+/IOCB0/HIB0/RB0
  // Pin 34 : OPA2OUT/PRG1IN1/OPA1IN1+/OPA1IN1-/PRG2IN0/AN10/C1IN3-/C2IN3-/C3IN3-/C4IN3-/IOCB1/HIB1/RB1
  // Pin 35 : DAC3OUT1/OPA2IN0-/AN8/IOCB2/RB2
  // Pin 36 : OPA2IN0+/AN9/C1IN2-/C2IN2-/C3IN2-/IOCB3/RB3
  // Pin 37 : AN11/C3IN1+/IOCB4/RB4
  // Pin 38 : DAC5REF1-/DAC7REF1-/AN13/C4IN2-/IOCB5/RB5
  // Pin 39 : ICSPCLK/ICDCLK/DAC5REF1+/DAC7REF1+/C4IN1+/IOCB6/RB6
  // Pin 40 : ICSPDAT/ICDDAT/DAC1OUT2/DAC2OUT2/DAC5OUT2/DAC6OUT2/DAC3OUT2/DAC4OUT2/DAC7OUT2/DAC8OUT2/C5IN1+/IOCB7/RB7


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-2,1-3,2-4,3-5,4-6,5-7,6-14,7-13'} // PORTA
  {$MAP_RAM_TO_PIN '00D:0-33,1-34,2-35,3-36,4-37,5-38,6-39,7-40'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-15,1-16,2-17,3-18,4-23,5-24,6-25,7-26'} // PORTC
  {$MAP_RAM_TO_PIN '00F:0-19,1-20,2-21,3-22,4-27,5-28,6-29,7-30'} // PORTD
  {$MAP_RAM_TO_PIN '010:0-8,1-9,2-10,3-1'} // PORTE


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-20 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECM     = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECL     = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz): device clock supplied to CLKIN pins
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

  // IESO : Internal/External Switchover Mode
  {$define _IESO_ON      = $3FFF}  // Internal/External Switchover Mode is enabled
  {$define _IESO_OFF     = $2FFF}  // Internal/External Switchover Mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enable
  {$define _FCMEN_ON     = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF    = $1FFF}  // Fail-Safe Clock Monitor is disabled

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF      = $3FFF}  // Write protection off
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to FFFh write protected, 1000h to 1FFFh may be modified by EECON control
  {$define _WRT_ALL      = $3FFC}  // 000h to 1FFFh write protected, no addresses may be modified by EECON control

  // PPS1WAY : Peripheral Pin Select one-way control
  {$define _PPS1WAY_ON   = $3FFF}  // The PPSLOCK bit cannot be cleared once it is set by software
  {$define _PPS1WAY_OFF  = $3FFB}  // The PPSLOCK bit can be set and cleared repeatedly by software

  // ZCD : Zero-cross detect disable
  {$define _ZCD_OFF      = $3FFF}  // Zero-cross detect circuit is disabled at POR
  {$define _ZCD_ON       = $3F7F}  // Zero-cross detect circuit is enabled at POR

  // PLLEN : Phase Lock Loop enable
  {$define _PLLEN_ON     = $3FFF}  // 4x PLL is always enabled
  {$define _PLLEN_OFF    = $3EFF}  // 4x PLL is enabled when software sets the SPLLEN bit

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
