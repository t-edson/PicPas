unit PIC16F1615;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1615'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 14}
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
  PORTA_RA2                : bit  absolute PORTA.2;
  PORTA_RA1                : bit  absolute PORTA.1;
  PORTA_RA0                : bit  absolute PORTA.0;
  PORTC                    : byte absolute $000E;
  PORTC_RC5                : bit  absolute PORTC.5;
  PORTC_RC4                : bit  absolute PORTC.4;
  PORTC_RC3                : bit  absolute PORTC.3;
  PORTC_RC2                : bit  absolute PORTC.2;
  PORTC_RC1                : bit  absolute PORTC.1;
  PORTC_RC0                : bit  absolute PORTC.0;
  PIR1                     : byte absolute $0010;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_RCIF                : bit  absolute PIR1.5;
  PIR1_TXIF                : bit  absolute PIR1.4;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0011;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_TMR6IF              : bit  absolute PIR2.2;
  PIR2_TMR4IF              : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0012;
  PIR3_CWGIF               : bit  absolute PIR3.5;
  PIR3_ZCDIF               : bit  absolute PIR3.4;
  PIR3_CLC4IF              : bit  absolute PIR3.3;
  PIR3_CLC3IF              : bit  absolute PIR3.2;
  PIR3_CLC2IF              : bit  absolute PIR3.1;
  PIR3_CLC1IF              : bit  absolute PIR3.0;
  PIR4                     : byte absolute $0013;
  PIR4_SCANIF              : bit  absolute PIR4.7;
  PIR4_CRCIF               : bit  absolute PIR4.6;
  PIR4_SMT2PWAIF           : bit  absolute PIR4.5;
  PIR4_SMT2PRAIF           : bit  absolute PIR4.4;
  PIR4_SMT2IF              : bit  absolute PIR4.3;
  PIR4_SMT1PWAIF           : bit  absolute PIR4.2;
  PIR4_SMT1PRAIF           : bit  absolute PIR4.1;
  PIR4_SMT1IF              : bit  absolute PIR4.0;
  PIR5                     : byte absolute $0014;
  PIR5_TMR3GIF             : bit  absolute PIR5.7;
  PIR5_TMR3IF              : bit  absolute PIR5.6;
  PIR5_TMR5GIF             : bit  absolute PIR5.5;
  PIR5_TMR5IF              : bit  absolute PIR5.4;
  PIR5_AT1IF               : bit  absolute PIR5.2;
  PIR5_PID1EIF             : bit  absolute PIR5.1;
  PIR5_PID1DIF             : bit  absolute PIR5.0;
  TMR0                     : byte absolute $0015;
  TMR1L                    : byte absolute $0016;
  TMR1H                    : byte absolute $0017;
  T1CON                    : byte absolute $0018;
  T1CON_TMR1CS1            : bit  absolute T1CON.7;
  T1CON_TMR1CS0            : bit  absolute T1CON.6;
  T1CON_T1CKPS1            : bit  absolute T1CON.5;
  T1CON_T1CKPS0            : bit  absolute T1CON.4;
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
  T2TMR                    : byte absolute $001A;
  T2PR                     : byte absolute $001B;
  T2CON                    : byte absolute $001C;
  T2CON_ON                 : bit  absolute T2CON.7;
  T2CON_CKPS2              : bit  absolute T2CON.6;
  T2CON_CKPS1              : bit  absolute T2CON.5;
  T2CON_CKPS0              : bit  absolute T2CON.4;
  T2CON_OUTPS3             : bit  absolute T2CON.3;
  T2CON_OUTPS2             : bit  absolute T2CON.2;
  T2CON_OUTPS1             : bit  absolute T2CON.1;
  T2CON_OUTPS0             : bit  absolute T2CON.0;
  T2HLT                    : byte absolute $001D;
  T2HLT_PSYNC              : bit  absolute T2HLT.7;
  T2HLT_CKPOL              : bit  absolute T2HLT.6;
  T2HLT_CKSYNC             : bit  absolute T2HLT.5;
  T2HLT_MODE4              : bit  absolute T2HLT.4;
  T2HLT_MODE3              : bit  absolute T2HLT.3;
  T2HLT_MODE2              : bit  absolute T2HLT.2;
  T2HLT_MODE1              : bit  absolute T2HLT.1;
  T2HLT_MODE0              : bit  absolute T2HLT.0;
  T2CLKCON                 : byte absolute $001E;
  T2CLKCON_CS3             : bit  absolute T2CLKCON.3;
  T2CLKCON_CS2             : bit  absolute T2CLKCON.2;
  T2CLKCON_CS1             : bit  absolute T2CLKCON.1;
  T2CLKCON_CS0             : bit  absolute T2CLKCON.0;
  T2RST                    : byte absolute $001F;
  T2RST_RSEL3              : bit  absolute T2RST.3;
  T2RST_RSEL2              : bit  absolute T2RST.2;
  T2RST_RSEL1              : bit  absolute T2RST.1;
  T2RST_RSEL0              : bit  absolute T2RST.0;
  TRISA                    : byte absolute $008C;
  TRISA_TRISA5             : bit  absolute TRISA.5;
  TRISA_TRISA4             : bit  absolute TRISA.4;
  TRISA_TRISA3             : bit  absolute TRISA.3;
  TRISA_TRISA2             : bit  absolute TRISA.2;
  TRISA_TRISA1             : bit  absolute TRISA.1;
  TRISA_TRISA0             : bit  absolute TRISA.0;
  TRISC                    : byte absolute $008E;
  TRISC_TRISC5             : bit  absolute TRISC.5;
  TRISC_TRISC4             : bit  absolute TRISC.4;
  TRISC_TRISC3             : bit  absolute TRISC.3;
  TRISC_TRISC2             : bit  absolute TRISC.2;
  TRISC_TRISC1             : bit  absolute TRISC.1;
  TRISC_TRISC0             : bit  absolute TRISC.0;
  PIE1                     : byte absolute $0090;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_RCIE                : bit  absolute PIE1.5;
  PIE1_TXIE                : bit  absolute PIE1.4;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0091;
  PIE2_OSCFIE              : bit  absolute PIE2.7;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_TMR6IE              : bit  absolute PIE2.2;
  PIE2_TMR4IE              : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0092;
  PIE3_CWGIE               : bit  absolute PIE3.5;
  PIE3_ZCDIE               : bit  absolute PIE3.4;
  PIE3_CLC4IE              : bit  absolute PIE3.3;
  PIE3_CLC3IE              : bit  absolute PIE3.2;
  PIE3_CLC2IE              : bit  absolute PIE3.1;
  PIE3_CLC1IE              : bit  absolute PIE3.0;
  PIE4                     : byte absolute $0093;
  PIE4_SCANIE              : bit  absolute PIE4.7;
  PIE4_CRCIE               : bit  absolute PIE4.6;
  PIE4_SMT2PWAIE           : bit  absolute PIE4.5;
  PIE4_SMT2PRAIE           : bit  absolute PIE4.4;
  PIE4_SMT2IE              : bit  absolute PIE4.3;
  PIE4_SMT1PWAIE           : bit  absolute PIE4.2;
  PIE4_SMT1PRAIE           : bit  absolute PIE4.1;
  PIE4_SMT1IE              : bit  absolute PIE4.0;
  PIE5                     : byte absolute $0094;
  PIE5_TMR3GIE             : bit  absolute PIE5.7;
  PIE5_TMR3IE              : bit  absolute PIE5.6;
  PIE5_TMR5GIE             : bit  absolute PIE5.5;
  PIE5_TMR5IE              : bit  absolute PIE5.4;
  PIE5_AT1IE               : bit  absolute PIE5.2;
  PIE5_PID1EIE             : bit  absolute PIE5.1;
  PIE5_PID1DIE             : bit  absolute PIE5.0;
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
  PCON_nWDTWV              : bit  absolute PCON.5;
  PCON_nRWDT               : bit  absolute PCON.4;
  PCON_nRMCLR              : bit  absolute PCON.3;
  PCON_nRI                 : bit  absolute PCON.2;
  PCON_nPOR                : bit  absolute PCON.1;
  PCON_nBOR                : bit  absolute PCON.0;
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
  ADCON1_ADPREF1           : bit  absolute ADCON1.1;
  ADCON1_ADPREF0           : bit  absolute ADCON1.0;
  ADCON2                   : byte absolute $009F;
  ADCON2_TRIGSEL4          : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.4;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.3;
  LATA                     : byte absolute $010C;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
  LATA_LATA3               : bit  absolute LATA.3;
  LATA_LATA2               : bit  absolute LATA.2;
  LATA_LATA1               : bit  absolute LATA.1;
  LATA_LATA0               : bit  absolute LATA.0;
  LATC                     : byte absolute $010E;
  LATC_LATC5               : bit  absolute LATC.5;
  LATC_LATC4               : bit  absolute LATC.4;
  LATC_LATC3               : bit  absolute LATC.3;
  LATC_LATC2               : bit  absolute LATC.2;
  LATC_LATC1               : bit  absolute LATC.1;
  LATC_LATC0               : bit  absolute LATC.0;
  CM1CON0                  : byte absolute $0111;
  CM1CON0_C1ON             : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT            : bit  absolute CM1CON0.6;
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
  DAC1CON0                 : byte absolute $0118;
  DAC1CON0_DAC1EN          : bit  absolute DAC1CON0.7;
  DAC1CON0_DAC1OE          : bit  absolute DAC1CON0.5;
  DAC1CON0_DAC1PSS1        : bit  absolute DAC1CON0.3;
  DAC1CON0_DAC1PSS0        : bit  absolute DAC1CON0.2;
  DAC1CON1                 : byte absolute $0119;
  ZCD1CON                  : byte absolute $011C;
  ZCD1CON_ZCD1EN           : bit  absolute ZCD1CON.7;
  ZCD1CON_ZCD1OUT          : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL          : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP         : bit  absolute ZCD1CON.1;
  ZCD1CON_ZCD1INTN         : bit  absolute ZCD1CON.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
  ANSELC                   : byte absolute $018E;
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
  WPUA_WPUA5               : bit  absolute WPUA.5;
  WPUA_WPUA4               : bit  absolute WPUA.4;
  WPUA_WPUA3               : bit  absolute WPUA.3;
  WPUA_WPUA2               : bit  absolute WPUA.2;
  WPUA_WPUA1               : bit  absolute WPUA.1;
  WPUA_WPUA0               : bit  absolute WPUA.0;
  WPUC                     : byte absolute $020E;
  WPUC_WPUC5               : bit  absolute WPUC.5;
  WPUC_WPUC4               : bit  absolute WPUC.4;
  WPUC_WPUC3               : bit  absolute WPUC.3;
  WPUC_WPUC2               : bit  absolute WPUC.2;
  WPUC_WPUC1               : bit  absolute WPUC.1;
  WPUC_WPUC0               : bit  absolute WPUC.0;
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
  ODCONA_ODA5              : bit  absolute ODCONA.5;
  ODCONA_ODA4              : bit  absolute ODCONA.4;
  ODCONA_ODA2              : bit  absolute ODCONA.2;
  ODCONA_ODA1              : bit  absolute ODCONA.1;
  ODCONA_ODA0              : bit  absolute ODCONA.0;
  ODCONC                   : byte absolute $028E;
  ODCONC_ODC5              : bit  absolute ODCONC.5;
  ODCONC_ODC4              : bit  absolute ODCONC.4;
  ODCONC_ODC3              : bit  absolute ODCONC.3;
  ODCONC_ODC2              : bit  absolute ODCONC.2;
  ODCONC_ODC1              : bit  absolute ODCONC.1;
  ODCONC_ODC0              : bit  absolute ODCONC.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_EN               : bit  absolute CCP1CON.7;
  CCP1CON_OUT              : bit  absolute CCP1CON.5;
  CCP1CON_FMT              : bit  absolute CCP1CON.4;
  CCP1CAP                  : byte absolute $0294;
  CCPR2L                   : byte absolute $0298;
  CCPR2H                   : byte absolute $0299;
  CCP2CON                  : byte absolute $029A;
  CCP2CAP                  : byte absolute $029B;
  CCPTMRS                  : byte absolute $029E;
  CCPTMRS_P4TSEL1          : bit  absolute CCPTMRS.7;
  CCPTMRS_P4TSEL0          : bit  absolute CCPTMRS.6;
  CCPTMRS_P3TSEL1          : bit  absolute CCPTMRS.5;
  CCPTMRS_P3TSEL0          : bit  absolute CCPTMRS.4;
  CCPTMRS_CCP2TSEL1        : bit  absolute CCPTMRS.3;
  CCPTMRS_CCP2TSEL0        : bit  absolute CCPTMRS.2;
  CCPTMRS_CCP1TSEL1        : bit  absolute CCPTMRS.1;
  CCPTMRS_CCP1TSEL0        : bit  absolute CCPTMRS.0;
  SLRCONA                  : byte absolute $030C;
  SLRCONA_SLRA5            : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4            : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2            : bit  absolute SLRCONA.2;
  SLRCONA_SLRA1            : bit  absolute SLRCONA.1;
  SLRCONA_SLRA0            : bit  absolute SLRCONA.0;
  SLRCONC                  : byte absolute $030E;
  SLRCONC_SLRC5            : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4            : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3            : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2            : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1            : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0            : bit  absolute SLRCONC.0;
  INLVLA                   : byte absolute $038C;
  INLVLA_INLVLA5           : bit  absolute INLVLA.5;
  INLVLA_INLVLA4           : bit  absolute INLVLA.4;
  INLVLA_INLVLA3           : bit  absolute INLVLA.3;
  INLVLA_INLVLA2           : bit  absolute INLVLA.2;
  INLVLA_INLVLA1           : bit  absolute INLVLA.1;
  INLVLA_INLVLA0           : bit  absolute INLVLA.0;
  INLVLC                   : byte absolute $038E;
  INLVLC_INLVLC5           : bit  absolute INLVLC.5;
  INLVLC_INLVLC4           : bit  absolute INLVLC.4;
  INLVLC_INLVLC3           : bit  absolute INLVLC.3;
  INLVLC_INLVLC2           : bit  absolute INLVLC.2;
  INLVLC_INLVLC1           : bit  absolute INLVLC.1;
  INLVLC_INLVLC0           : bit  absolute INLVLC.0;
  IOCAP                    : byte absolute $0391;
  IOCAP_IOCAP5             : bit  absolute IOCAP.5;
  IOCAP_IOCAP4             : bit  absolute IOCAP.4;
  IOCAP_IOCAP3             : bit  absolute IOCAP.3;
  IOCAP_IOCAP2             : bit  absolute IOCAP.2;
  IOCAP_IOCAP1             : bit  absolute IOCAP.1;
  IOCAP_IOCAP0             : bit  absolute IOCAP.0;
  IOCAN                    : byte absolute $0392;
  IOCAN_IOCAN5             : bit  absolute IOCAN.5;
  IOCAN_IOCAN4             : bit  absolute IOCAN.4;
  IOCAN_IOCAN3             : bit  absolute IOCAN.3;
  IOCAN_IOCAN2             : bit  absolute IOCAN.2;
  IOCAN_IOCAN1             : bit  absolute IOCAN.1;
  IOCAN_IOCAN0             : bit  absolute IOCAN.0;
  IOCAF                    : byte absolute $0393;
  IOCAF_IOCAF5             : bit  absolute IOCAF.5;
  IOCAF_IOCAF4             : bit  absolute IOCAF.4;
  IOCAF_IOCAF3             : bit  absolute IOCAF.3;
  IOCAF_IOCAF2             : bit  absolute IOCAF.2;
  IOCAF_IOCAF1             : bit  absolute IOCAF.1;
  IOCAF_IOCAF0             : bit  absolute IOCAF.0;
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
  HIDRVC                   : byte absolute $040E;
  HIDRVC_HIDC5             : bit  absolute HIDRVC.5;
  HIDRVC_HIDC4             : bit  absolute HIDRVC.4;
  T4TMR                    : byte absolute $0413;
  T4PR                     : byte absolute $0414;
  T4CON                    : byte absolute $0415;
  T4HLT                    : byte absolute $0416;
  T4CLKCON                 : byte absolute $0417;
  T4RST                    : byte absolute $0418;
  T6TMR                    : byte absolute $041A;
  T6PR                     : byte absolute $041B;
  T6CON                    : byte absolute $041C;
  T6HLT                    : byte absolute $041D;
  T6CLKCON                 : byte absolute $041E;
  T6RST                    : byte absolute $041F;
  TMR3L                    : byte absolute $0493;
  TMR3H                    : byte absolute $0494;
  T3CON                    : byte absolute $0495;
  T3CON_TMR3CS1            : bit  absolute T3CON.7;
  T3CON_TMR3CS0            : bit  absolute T3CON.6;
  T3CON_T3CKPS1            : bit  absolute T3CON.5;
  T3CON_T3CKPS0            : bit  absolute T3CON.4;
  T3CON_T3SYNC             : bit  absolute T3CON.2;
  T3CON_TMR3ON             : bit  absolute T3CON.0;
  T3GCON                   : byte absolute $0496;
  T3GCON_TMR3GE            : bit  absolute T3GCON.7;
  T3GCON_T3GPOL            : bit  absolute T3GCON.6;
  T3GCON_T3GTM             : bit  absolute T3GCON.5;
  T3GCON_T3GSPM            : bit  absolute T3GCON.4;
  T3GCON_T3GGO_nDONE       : bit  absolute T3GCON.3;
  T3GCON_T3GVAL            : bit  absolute T3GCON.2;
  T3GCON_T3GSS1            : bit  absolute T3GCON.1;
  T3GCON_T3GSS0            : bit  absolute T3GCON.0;
  TMR5L                    : byte absolute $049A;
  TMR5H                    : byte absolute $049B;
  T5CON                    : byte absolute $049C;
  T5CON_TMR5CS1            : bit  absolute T5CON.7;
  T5CON_TMR5CS0            : bit  absolute T5CON.6;
  T5CON_T5CKPS1            : bit  absolute T5CON.5;
  T5CON_T5CKPS0            : bit  absolute T5CON.4;
  T5CON_T5SYNC             : bit  absolute T5CON.2;
  T5CON_TMR5ON             : bit  absolute T5CON.0;
  T5GCON                   : byte absolute $049D;
  T5GCON_TMR5GE            : bit  absolute T5GCON.7;
  T5GCON_T5GPOL            : bit  absolute T5GCON.6;
  T5GCON_T5GTM             : bit  absolute T5GCON.5;
  T5GCON_T5GSPM            : bit  absolute T5GCON.4;
  T5GCON_T5GGO_nDONE       : bit  absolute T5GCON.3;
  T5GCON_T5GVAL            : bit  absolute T5GCON.2;
  T5GCON_T5GSS1            : bit  absolute T5GCON.1;
  T5GCON_T5GSS0            : bit  absolute T5GCON.0;
  PID1SETL                 : byte absolute $058C;
  PID1SETL_PID1SET7        : bit  absolute PID1SETL.7;
  PID1SETL_PID1SET6        : bit  absolute PID1SETL.6;
  PID1SETL_PID1SET5        : bit  absolute PID1SETL.5;
  PID1SETL_PID1SET4        : bit  absolute PID1SETL.4;
  PID1SETL_PID1SET3        : bit  absolute PID1SETL.3;
  PID1SETL_PID1SET2        : bit  absolute PID1SETL.2;
  PID1SETL_PID1SET1        : bit  absolute PID1SETL.1;
  PID1SETL_PID1SET0        : bit  absolute PID1SETL.0;
  PID1SETH                 : byte absolute $058D;
  PID1SETH_PID1SET15       : bit  absolute PID1SETH.7;
  PID1SETH_PID1SET14       : bit  absolute PID1SETH.6;
  PID1SETH_PID1SET13       : bit  absolute PID1SETH.5;
  PID1SETH_PID1SET12       : bit  absolute PID1SETH.4;
  PID1SETH_PID1SET11       : bit  absolute PID1SETH.3;
  PID1SETH_PID1SET10       : bit  absolute PID1SETH.2;
  PID1SETH_PID1SET9        : bit  absolute PID1SETH.1;
  PID1SETH_PID1SET8        : bit  absolute PID1SETH.0;
  PID1INL                  : byte absolute $058E;
  PID1INL_PID1IN7          : bit  absolute PID1INL.7;
  PID1INL_PID1IN6          : bit  absolute PID1INL.6;
  PID1INL_PID1IN5          : bit  absolute PID1INL.5;
  PID1INL_PID1IN4          : bit  absolute PID1INL.4;
  PID1INL_PID1IN3          : bit  absolute PID1INL.3;
  PID1INL_PID1IN2          : bit  absolute PID1INL.2;
  PID1INL_PID1IN1          : bit  absolute PID1INL.1;
  PID1INL_PID1IN0          : bit  absolute PID1INL.0;
  PID1INH                  : byte absolute $058F;
  PID1INH_PID1IN15         : bit  absolute PID1INH.7;
  PID1INH_PID1IN14         : bit  absolute PID1INH.6;
  PID1INH_PID1IN13         : bit  absolute PID1INH.5;
  PID1INH_PID1IN12         : bit  absolute PID1INH.4;
  PID1INH_PID1IN11         : bit  absolute PID1INH.3;
  PID1INH_PID1IN10         : bit  absolute PID1INH.2;
  PID1INH_PID1IN9          : bit  absolute PID1INH.1;
  PID1INH_PID1IN8          : bit  absolute PID1INH.0;
  PID1K1L                  : byte absolute $0590;
  PID1K1L_PID1K17          : bit  absolute PID1K1L.7;
  PID1K1L_PID1K16          : bit  absolute PID1K1L.6;
  PID1K1L_PID1K15          : bit  absolute PID1K1L.5;
  PID1K1L_PID1K14          : bit  absolute PID1K1L.4;
  PID1K1L_PID1K13          : bit  absolute PID1K1L.3;
  PID1K1L_PID1K12          : bit  absolute PID1K1L.2;
  PID1K1L_PID1K11          : bit  absolute PID1K1L.1;
  PID1K1L_PID1K10          : bit  absolute PID1K1L.0;
  PID1K1H                  : byte absolute $0591;
  PID1K1H_PID1K115         : bit  absolute PID1K1H.7;
  PID1K1H_PID1K114         : bit  absolute PID1K1H.6;
  PID1K1H_PID1K113         : bit  absolute PID1K1H.5;
  PID1K1H_PID1K112         : bit  absolute PID1K1H.4;
  PID1K1H_PID1K111         : bit  absolute PID1K1H.3;
  PID1K1H_PID1K110         : bit  absolute PID1K1H.2;
  PID1K1H_PID1K19          : bit  absolute PID1K1H.1;
  PID1K1H_PID1K18          : bit  absolute PID1K1H.0;
  PID1K2L                  : byte absolute $0592;
  PID1K2L_PID1K27          : bit  absolute PID1K2L.7;
  PID1K2L_PID1K26          : bit  absolute PID1K2L.6;
  PID1K2L_PID1K25          : bit  absolute PID1K2L.5;
  PID1K2L_PID1K24          : bit  absolute PID1K2L.4;
  PID1K2L_PID1K23          : bit  absolute PID1K2L.3;
  PID1K2L_PID1K22          : bit  absolute PID1K2L.2;
  PID1K2L_PID1K21          : bit  absolute PID1K2L.1;
  PID1K2L_PID1K20          : bit  absolute PID1K2L.0;
  PID1K2H                  : byte absolute $0593;
  PID1K2H_PID1K215         : bit  absolute PID1K2H.7;
  PID1K2H_PID1K214         : bit  absolute PID1K2H.6;
  PID1K2H_PID1K213         : bit  absolute PID1K2H.5;
  PID1K2H_PID1K212         : bit  absolute PID1K2H.4;
  PID1K2H_PID1K211         : bit  absolute PID1K2H.3;
  PID1K2H_PID1K210         : bit  absolute PID1K2H.2;
  PID1K2H_PID1K29          : bit  absolute PID1K2H.1;
  PID1K2H_PID1K28          : bit  absolute PID1K2H.0;
  PID1K3L                  : byte absolute $0594;
  PID1K3L_PID1K37          : bit  absolute PID1K3L.7;
  PID1K3L_PID1K36          : bit  absolute PID1K3L.6;
  PID1K3L_PID1K35          : bit  absolute PID1K3L.5;
  PID1K3L_PID1K34          : bit  absolute PID1K3L.4;
  PID1K3L_PID1K33          : bit  absolute PID1K3L.3;
  PID1K3L_PID1K32          : bit  absolute PID1K3L.2;
  PID1K3L_PID1K31          : bit  absolute PID1K3L.1;
  PID1K3L_PID1K30          : bit  absolute PID1K3L.0;
  PID1K3H                  : byte absolute $0595;
  PID1K3H_PID1K315         : bit  absolute PID1K3H.7;
  PID1K3H_PID1K314         : bit  absolute PID1K3H.6;
  PID1K3H_PID1K313         : bit  absolute PID1K3H.5;
  PID1K3H_PID1K312         : bit  absolute PID1K3H.4;
  PID1K3H_PID1K311         : bit  absolute PID1K3H.3;
  PID1K3H_PID1K310         : bit  absolute PID1K3H.2;
  PID1K3H_PID1K39          : bit  absolute PID1K3H.1;
  PID1K3H_PID1K38          : bit  absolute PID1K3H.0;
  PID1OUTLL                : byte absolute $0596;
  PID1OUTLL_PID1OUT7       : bit  absolute PID1OUTLL.7;
  PID1OUTLL_PID1OUT6       : bit  absolute PID1OUTLL.6;
  PID1OUTLL_PID1OUT5       : bit  absolute PID1OUTLL.5;
  PID1OUTLL_PID1OUT4       : bit  absolute PID1OUTLL.4;
  PID1OUTLL_PID1OUT3       : bit  absolute PID1OUTLL.3;
  PID1OUTLL_PID1OUT2       : bit  absolute PID1OUTLL.2;
  PID1OUTLL_PID1OUT1       : bit  absolute PID1OUTLL.1;
  PID1OUTLL_PID1OUT0       : bit  absolute PID1OUTLL.0;
  PID1OUTLH                : byte absolute $0597;
  PID1OUTLH_PID1OUT15      : bit  absolute PID1OUTLH.7;
  PID1OUTLH_PID1OUT14      : bit  absolute PID1OUTLH.6;
  PID1OUTLH_PID1OUT13      : bit  absolute PID1OUTLH.5;
  PID1OUTLH_PID1OUT12      : bit  absolute PID1OUTLH.4;
  PID1OUTLH_PID1OUT11      : bit  absolute PID1OUTLH.3;
  PID1OUTLH_PID1OUT10      : bit  absolute PID1OUTLH.2;
  PID1OUTLH_PID1OUT9       : bit  absolute PID1OUTLH.1;
  PID1OUTLH_PID1OUT8       : bit  absolute PID1OUTLH.0;
  PID1OUTHL                : byte absolute $0598;
  PID1OUTHL_PID1OUT23      : bit  absolute PID1OUTHL.7;
  PID1OUTHL_PID1OUT22      : bit  absolute PID1OUTHL.6;
  PID1OUTHL_PID1OUT21      : bit  absolute PID1OUTHL.5;
  PID1OUTHL_PID1OUT20      : bit  absolute PID1OUTHL.4;
  PID1OUTHL_PID1OUT19      : bit  absolute PID1OUTHL.3;
  PID1OUTHL_PID1OUT18      : bit  absolute PID1OUTHL.2;
  PID1OUTHL_PID1OUT17      : bit  absolute PID1OUTHL.1;
  PID1OUTHL_PID1OUT16      : bit  absolute PID1OUTHL.0;
  PID1OUTHH                : byte absolute $0599;
  PID1OUTHH_PID1OUT31      : bit  absolute PID1OUTHH.7;
  PID1OUTHH_PID1OUT30      : bit  absolute PID1OUTHH.6;
  PID1OUTHH_PID1OUT29      : bit  absolute PID1OUTHH.5;
  PID1OUTHH_PID1OUT28      : bit  absolute PID1OUTHH.4;
  PID1OUTHH_PID1OUT27      : bit  absolute PID1OUTHH.3;
  PID1OUTHH_PID1OUT26      : bit  absolute PID1OUTHH.2;
  PID1OUTHH_PID1OUT25      : bit  absolute PID1OUTHH.1;
  PID1OUTHH_PID1OUT24      : bit  absolute PID1OUTHH.0;
  PID1OUTU                 : byte absolute $059A;
  PID1OUTU_PID1OUT35       : bit  absolute PID1OUTU.3;
  PID1OUTU_PID1OUT34       : bit  absolute PID1OUTU.2;
  PID1OUTU_PID1OUT33       : bit  absolute PID1OUTU.1;
  PID1OUTU_PID1OUT32       : bit  absolute PID1OUTU.0;
  PID1Z1L                  : byte absolute $059B;
  PID1Z1L_PID1Z17          : bit  absolute PID1Z1L.7;
  PID1Z1L_PID1Z16          : bit  absolute PID1Z1L.6;
  PID1Z1L_PID1Z15          : bit  absolute PID1Z1L.5;
  PID1Z1L_PID1Z14          : bit  absolute PID1Z1L.4;
  PID1Z1L_PID1Z13          : bit  absolute PID1Z1L.3;
  PID1Z1L_PID1Z12          : bit  absolute PID1Z1L.2;
  PID1Z1L_PID1Z11          : bit  absolute PID1Z1L.1;
  PID1Z1L_PID1Z10          : bit  absolute PID1Z1L.0;
  PID1Z1H                  : byte absolute $059C;
  PID1Z1H_PID1Z115         : bit  absolute PID1Z1H.7;
  PID1Z1H_PID1Z114         : bit  absolute PID1Z1H.6;
  PID1Z1H_PID1Z113         : bit  absolute PID1Z1H.5;
  PID1Z1H_PID1Z112         : bit  absolute PID1Z1H.4;
  PID1Z1H_PID1Z111         : bit  absolute PID1Z1H.3;
  PID1Z1H_PID1Z110         : bit  absolute PID1Z1H.2;
  PID1Z1H_PID1Z19          : bit  absolute PID1Z1H.1;
  PID1Z1H_PID1Z18          : bit  absolute PID1Z1H.0;
  PID1Z1U                  : byte absolute $059D;
  PID1Z1U_PID1Z116         : bit  absolute PID1Z1U.0;
  PID1Z2L                  : byte absolute $060C;
  PID1Z2L_PID1Z27          : bit  absolute PID1Z2L.7;
  PID1Z2L_PID1Z26          : bit  absolute PID1Z2L.6;
  PID1Z2L_PID1Z25          : bit  absolute PID1Z2L.5;
  PID1Z2L_PID1Z24          : bit  absolute PID1Z2L.4;
  PID1Z2L_PID1Z23          : bit  absolute PID1Z2L.3;
  PID1Z2L_PID1Z22          : bit  absolute PID1Z2L.2;
  PID1Z2L_PID1Z21          : bit  absolute PID1Z2L.1;
  PID1Z2L_PID1Z20          : bit  absolute PID1Z2L.0;
  PID1Z2H                  : byte absolute $060D;
  PID1Z2H_PID1Z215         : bit  absolute PID1Z2H.7;
  PID1Z2H_PID1Z214         : bit  absolute PID1Z2H.6;
  PID1Z2H_PID1Z213         : bit  absolute PID1Z2H.5;
  PID1Z2H_PID1Z212         : bit  absolute PID1Z2H.4;
  PID1Z2H_PID1Z211         : bit  absolute PID1Z2H.3;
  PID1Z2H_PID1Z210         : bit  absolute PID1Z2H.2;
  PID1Z2H_PID1Z29          : bit  absolute PID1Z2H.1;
  PID1Z2H_PID1Z28          : bit  absolute PID1Z2H.0;
  PID1Z2U                  : byte absolute $060E;
  PID1Z2U_PID1Z216         : bit  absolute PID1Z2U.0;
  PID1ACCLL                : byte absolute $060F;
  PID1ACCLL_PID1ACC7       : bit  absolute PID1ACCLL.7;
  PID1ACCLL_PID1ACC6       : bit  absolute PID1ACCLL.6;
  PID1ACCLL_PID1ACC5       : bit  absolute PID1ACCLL.5;
  PID1ACCLL_PID1ACC4       : bit  absolute PID1ACCLL.4;
  PID1ACCLL_PID1ACC3       : bit  absolute PID1ACCLL.3;
  PID1ACCLL_PID1ACC2       : bit  absolute PID1ACCLL.2;
  PID1ACCLL_PID1ACC1       : bit  absolute PID1ACCLL.1;
  PID1ACCLL_PID1ACC0       : bit  absolute PID1ACCLL.0;
  PID1ACCLH                : byte absolute $0610;
  PID1ACCLH_PID1ACC15      : bit  absolute PID1ACCLH.7;
  PID1ACCLH_PID1ACC14      : bit  absolute PID1ACCLH.6;
  PID1ACCLH_PID1ACC13      : bit  absolute PID1ACCLH.5;
  PID1ACCLH_PID1ACC12      : bit  absolute PID1ACCLH.4;
  PID1ACCLH_PID1ACC11      : bit  absolute PID1ACCLH.3;
  PID1ACCLH_PID1ACC10      : bit  absolute PID1ACCLH.2;
  PID1ACCLH_PID1ACC9       : bit  absolute PID1ACCLH.1;
  PID1ACCLH_PID1ACC8       : bit  absolute PID1ACCLH.0;
  PID1ACCHL                : byte absolute $0611;
  PID1ACCHL_PID1ACC23      : bit  absolute PID1ACCHL.7;
  PID1ACCHL_PID1ACC22      : bit  absolute PID1ACCHL.6;
  PID1ACCHL_PID1ACC21      : bit  absolute PID1ACCHL.5;
  PID1ACCHL_PID1ACC20      : bit  absolute PID1ACCHL.4;
  PID1ACCHL_PID1ACC19      : bit  absolute PID1ACCHL.3;
  PID1ACCHL_PID1ACC18      : bit  absolute PID1ACCHL.2;
  PID1ACCHL_PID1ACC17      : bit  absolute PID1ACCHL.1;
  PID1ACCHL_PID1ACC16      : bit  absolute PID1ACCHL.0;
  PID1ACCHH                : byte absolute $0612;
  PID1ACCHH_PID1ACC31      : bit  absolute PID1ACCHH.7;
  PID1ACCHH_PID1ACC30      : bit  absolute PID1ACCHH.6;
  PID1ACCHH_PID1ACC29      : bit  absolute PID1ACCHH.5;
  PID1ACCHH_PID1ACC28      : bit  absolute PID1ACCHH.4;
  PID1ACCHH_PID1ACC27      : bit  absolute PID1ACCHH.3;
  PID1ACCHH_PID1ACC26      : bit  absolute PID1ACCHH.2;
  PID1ACCHH_PID1ACC25      : bit  absolute PID1ACCHH.1;
  PID1ACCHH_PID1ACC24      : bit  absolute PID1ACCHH.0;
  PID1ACCU                 : byte absolute $0613;
  PID1ACCU_PID1ACC34       : bit  absolute PID1ACCU.2;
  PID1ACCU_PID1ACC33       : bit  absolute PID1ACCU.1;
  PID1ACCU_PID1ACC32       : bit  absolute PID1ACCU.0;
  PID1CON                  : byte absolute $0614;
  PID1CON_PID1EN           : bit  absolute PID1CON.7;
  PID1CON_PID1BUSY         : bit  absolute PID1CON.6;
  PID1CON_PID1MODE2        : bit  absolute PID1CON.2;
  PID1CON_PID1MODE1        : bit  absolute PID1CON.1;
  PID1CON_PID1MODE0        : bit  absolute PID1CON.0;
  PWM3DCL                  : byte absolute $0617;
  PWM3DCL_DC1              : bit  absolute PWM3DCL.7;
  PWM3DCL_DC0              : bit  absolute PWM3DCL.6;
  PWM3DCH                  : byte absolute $0618;
  PWM3CON                  : byte absolute $0619;
  PWM3CON_POL              : bit  absolute PWM3CON.4;
  PWM4DCL                  : byte absolute $061A;
  PWM4DCH                  : byte absolute $061B;
  PWM4CON                  : byte absolute $061C;
  CWG1DBR                  : byte absolute $0691;
  CWG1DBR_DBR5             : bit  absolute CWG1DBR.5;
  CWG1DBR_DBR4             : bit  absolute CWG1DBR.4;
  CWG1DBR_DBR3             : bit  absolute CWG1DBR.3;
  CWG1DBR_DBR2             : bit  absolute CWG1DBR.2;
  CWG1DBR_DBR1             : bit  absolute CWG1DBR.1;
  CWG1DBR_DBR0             : bit  absolute CWG1DBR.0;
  CWG1DBF                  : byte absolute $0692;
  CWG1DBF_DBF5             : bit  absolute CWG1DBF.5;
  CWG1DBF_DBF4             : bit  absolute CWG1DBF.4;
  CWG1DBF_DBF3             : bit  absolute CWG1DBF.3;
  CWG1DBF_DBF2             : bit  absolute CWG1DBF.2;
  CWG1DBF_DBF1             : bit  absolute CWG1DBF.1;
  CWG1DBF_DBF0             : bit  absolute CWG1DBF.0;
  CWG1AS0                  : byte absolute $0693;
  CWG1AS0_SHUTDOWN         : bit  absolute CWG1AS0.7;
  CWG1AS0_REN              : bit  absolute CWG1AS0.6;
  CWG1AS0_LSBD1            : bit  absolute CWG1AS0.5;
  CWG1AS0_LSBD0            : bit  absolute CWG1AS0.4;
  CWG1AS0_LSAC1            : bit  absolute CWG1AS0.3;
  CWG1AS0_LSAC0            : bit  absolute CWG1AS0.2;
  CWG1AS1                  : byte absolute $0694;
  CWG1AS1_TMR6AS           : bit  absolute CWG1AS1.6;
  CWG1AS1_TMR4AS           : bit  absolute CWG1AS1.5;
  CWG1AS1_TMR2AS           : bit  absolute CWG1AS1.4;
  CWG1AS1_C2AS             : bit  absolute CWG1AS1.2;
  CWG1AS1_C1AS             : bit  absolute CWG1AS1.1;
  CWG1AS1_INAS             : bit  absolute CWG1AS1.0;
  CWG1OCON0                : byte absolute $0695;
  CWG1OCON0_OVRD           : bit  absolute CWG1OCON0.7;
  CWG1OCON0_OVRC           : bit  absolute CWG1OCON0.6;
  CWG1OCON0_OVRB           : bit  absolute CWG1OCON0.5;
  CWG1OCON0_OVRA           : bit  absolute CWG1OCON0.4;
  CWG1OCON0_STRD           : bit  absolute CWG1OCON0.3;
  CWG1OCON0_STRC           : bit  absolute CWG1OCON0.2;
  CWG1OCON0_STRB           : bit  absolute CWG1OCON0.1;
  CWG1OCON0_STRA           : bit  absolute CWG1OCON0.0;
  CWG1CON0                 : byte absolute $0696;
  CWG1CON0_LD              : bit  absolute CWG1CON0.6;
  CWG1CON1                 : byte absolute $0697;
  CWG1CON1_IN              : bit  absolute CWG1CON1.5;
  CWG1CON1_POLD            : bit  absolute CWG1CON1.3;
  CWG1CON1_POLC            : bit  absolute CWG1CON1.2;
  CWG1CON1_POLB            : bit  absolute CWG1CON1.1;
  CWG1CON1_POLA            : bit  absolute CWG1CON1.0;
  CWG1CLKCON               : byte absolute $0699;
  CWG1CLKCON_CS            : bit  absolute CWG1CLKCON.0;
  CWG1ISM                  : byte absolute $069A;
  WDTCON0                  : byte absolute $0711;
  WDTCON0_WDTPS4           : bit  absolute WDTCON0.5;
  WDTCON0_WDTPS3           : bit  absolute WDTCON0.4;
  WDTCON0_WDTPS2           : bit  absolute WDTCON0.3;
  WDTCON0_WDTPS1           : bit  absolute WDTCON0.2;
  WDTCON0_WDTPS0           : bit  absolute WDTCON0.1;
  WDTCON1                  : byte absolute $0712;
  WDTCON1_WDTCS2           : bit  absolute WDTCON1.6;
  WDTCON1_WDTCS1           : bit  absolute WDTCON1.5;
  WDTCON1_WDTCS0           : bit  absolute WDTCON1.4;
  WDTCON1_WINDOW2          : bit  absolute WDTCON1.2;
  WDTCON1_WINDOW1          : bit  absolute WDTCON1.1;
  WDTCON1_WINDOW0          : bit  absolute WDTCON1.0;
  WDTPSL                   : byte absolute $0713;
  WDTPSH                   : byte absolute $0714;
  WDTTMR                   : byte absolute $0715;
  WDTTMR_WDTTMR4           : bit  absolute WDTTMR.7;
  WDTTMR_WDTTMR3           : bit  absolute WDTTMR.6;
  WDTTMR_WDTTMR2           : bit  absolute WDTTMR.5;
  WDTTMR_WDTTMR1           : bit  absolute WDTTMR.4;
  WDTTMR_WDTTMR0           : bit  absolute WDTTMR.3;
  WDTTMR_STATE             : bit  absolute WDTTMR.2;
  WDTTMR_PSCNT17           : bit  absolute WDTTMR.1;
  WDTTMR_PSCNT16           : bit  absolute WDTTMR.0;
  SCANLADRL                : byte absolute $0718;
  SCANLADRH                : byte absolute $0719;
  SCANHADRL                : byte absolute $071A;
  SCANHADRH                : byte absolute $071B;
  SCANCON0                 : byte absolute $071C;
  SCANCON0_SCANGO          : bit  absolute SCANCON0.6;
  SCANCON0_BUSY            : bit  absolute SCANCON0.5;
  SCANCON0_INVALID         : bit  absolute SCANCON0.4;
  SCANCON0_INTM            : bit  absolute SCANCON0.3;
  SCANTRIG                 : byte absolute $071D;
  CRCDATL                  : byte absolute $0791;
  CRCDATH                  : byte absolute $0792;
  CRCACCL                  : byte absolute $0793;
  CRCACCH                  : byte absolute $0794;
  CRCSHIFTL                : byte absolute $0795;
  CRCSHIFTH                : byte absolute $0796;
  CRCXORL                  : byte absolute $0797;
  CRCXORL_XOR7             : bit  absolute CRCXORL.7;
  CRCXORL_XOR6             : bit  absolute CRCXORL.6;
  CRCXORL_XOR5             : bit  absolute CRCXORL.5;
  CRCXORL_XOR4             : bit  absolute CRCXORL.4;
  CRCXORL_XOR3             : bit  absolute CRCXORL.3;
  CRCXORL_XOR2             : bit  absolute CRCXORL.2;
  CRCXORL_XOR1             : bit  absolute CRCXORL.1;
  CRCXORH                  : byte absolute $0798;
  CRCXORH_XOR15            : bit  absolute CRCXORH.7;
  CRCXORH_XOR14            : bit  absolute CRCXORH.6;
  CRCXORH_XOR13            : bit  absolute CRCXORH.5;
  CRCXORH_XOR12            : bit  absolute CRCXORH.4;
  CRCXORH_XOR11            : bit  absolute CRCXORH.3;
  CRCXORH_XOR10            : bit  absolute CRCXORH.2;
  CRCXORH_XOR9             : bit  absolute CRCXORH.1;
  CRCXORH_XOR8             : bit  absolute CRCXORH.0;
  CRCCON0                  : byte absolute $0799;
  CRCCON0_CRCGO            : bit  absolute CRCCON0.6;
  CRCCON0_ACCM             : bit  absolute CRCCON0.4;
  CRCCON0_SHIFTM           : bit  absolute CRCCON0.1;
  CRCCON0_FULL             : bit  absolute CRCCON0.0;
  CRCCON1                  : byte absolute $079A;
  CRCCON1_DLEN3            : bit  absolute CRCCON1.7;
  CRCCON1_DLEN2            : bit  absolute CRCCON1.6;
  CRCCON1_DLEN1            : bit  absolute CRCCON1.5;
  CRCCON1_DLEN0            : bit  absolute CRCCON1.4;
  CRCCON1_PLEN3            : bit  absolute CRCCON1.3;
  CRCCON1_PLEN2            : bit  absolute CRCCON1.2;
  CRCCON1_PLEN1            : bit  absolute CRCCON1.1;
  CRCCON1_PLEN0            : bit  absolute CRCCON1.0;
  AT1RESL                  : byte absolute $080C;
  AT1RESL_AT1RES7          : bit  absolute AT1RESL.7;
  AT1RESL_AT1RES6          : bit  absolute AT1RESL.6;
  AT1RESL_AT1RES5          : bit  absolute AT1RESL.5;
  AT1RESL_AT1RES4          : bit  absolute AT1RESL.4;
  AT1RESL_AT1RES3          : bit  absolute AT1RESL.3;
  AT1RESL_AT1RES2          : bit  absolute AT1RESL.2;
  AT1RESL_AT1RES1          : bit  absolute AT1RESL.1;
  AT1RESL_AT1RES0          : bit  absolute AT1RESL.0;
  AT1RESH                  : byte absolute $080D;
  AT1RESH_AT1RES9          : bit  absolute AT1RESH.1;
  AT1RESH_AT1RES8          : bit  absolute AT1RESH.0;
  AT1MISSL                 : byte absolute $080E;
  AT1MISSL_AT1MISS7        : bit  absolute AT1MISSL.7;
  AT1MISSL_AT1MISS6        : bit  absolute AT1MISSL.6;
  AT1MISSL_AT1MISS5        : bit  absolute AT1MISSL.5;
  AT1MISSL_AT1MISS4        : bit  absolute AT1MISSL.4;
  AT1MISSL_AT1MISS3        : bit  absolute AT1MISSL.3;
  AT1MISSL_AT1MISS2        : bit  absolute AT1MISSL.2;
  AT1MISSL_AT1MISS1        : bit  absolute AT1MISSL.1;
  AT1MISSL_AT1MISS0        : bit  absolute AT1MISSL.0;
  AT1MISSH                 : byte absolute $080F;
  AT1MISSH_AT1MISS15       : bit  absolute AT1MISSH.7;
  AT1MISSH_AT1MISS14       : bit  absolute AT1MISSH.6;
  AT1MISSH_AT1MISS13       : bit  absolute AT1MISSH.5;
  AT1MISSH_AT1MISS12       : bit  absolute AT1MISSH.4;
  AT1MISSH_AT1MISS11       : bit  absolute AT1MISSH.3;
  AT1MISSH_AT1MISS10       : bit  absolute AT1MISSH.2;
  AT1MISSH_AT1MISS9        : bit  absolute AT1MISSH.1;
  AT1MISSH_AT1MISS8        : bit  absolute AT1MISSH.0;
  AT1PERL                  : byte absolute $0810;
  AT1PERL_AT1PER7          : bit  absolute AT1PERL.7;
  AT1PERL_AT1PER6          : bit  absolute AT1PERL.6;
  AT1PERL_AT1PER5          : bit  absolute AT1PERL.5;
  AT1PERL_AT1PER4          : bit  absolute AT1PERL.4;
  AT1PERL_AT1PER3          : bit  absolute AT1PERL.3;
  AT1PERL_AT1PER2          : bit  absolute AT1PERL.2;
  AT1PERL_AT1PER1          : bit  absolute AT1PERL.1;
  AT1PERL_AT1PER0          : bit  absolute AT1PERL.0;
  AT1PERH                  : byte absolute $0811;
  AT1PERH_AT1POV           : bit  absolute AT1PERH.7;
  AT1PERH_AT1PER14         : bit  absolute AT1PERH.6;
  AT1PERH_AT1PER13         : bit  absolute AT1PERH.5;
  AT1PERH_AT1PER12         : bit  absolute AT1PERH.4;
  AT1PERH_AT1PER11         : bit  absolute AT1PERH.3;
  AT1PERH_AT1PER10         : bit  absolute AT1PERH.2;
  AT1PERH_AT1PER9          : bit  absolute AT1PERH.1;
  AT1PERH_AT1PER8          : bit  absolute AT1PERH.0;
  AT1PHSL                  : byte absolute $0812;
  AT1PHSL_AT1PHS7          : bit  absolute AT1PHSL.7;
  AT1PHSL_AT1PHS6          : bit  absolute AT1PHSL.6;
  AT1PHSL_AT1PHS5          : bit  absolute AT1PHSL.5;
  AT1PHSL_AT1PHS4          : bit  absolute AT1PHSL.4;
  AT1PHSL_AT1PHS3          : bit  absolute AT1PHSL.3;
  AT1PHSL_AT1PHS2          : bit  absolute AT1PHSL.2;
  AT1PHSL_AT1PHS1          : bit  absolute AT1PHSL.1;
  AT1PHSL_AT1PHS0          : bit  absolute AT1PHSL.0;
  AT1PHSH                  : byte absolute $0813;
  AT1PHSH_AT1PHS9          : bit  absolute AT1PHSH.1;
  AT1PHSH_AT1PHS8          : bit  absolute AT1PHSH.0;
  AT1CON0                  : byte absolute $0814;
  AT1CON0_PREC             : bit  absolute AT1CON0.6;
  AT1CON0_APMOD            : bit  absolute AT1CON0.1;
  AT1CON0_MODE             : bit  absolute AT1CON0.0;
  AT1CON1                  : byte absolute $0815;
  AT1CON1_PHP              : bit  absolute AT1CON1.6;
  AT1CON1_PRP              : bit  absolute AT1CON1.4;
  AT1CON1_MPP              : bit  absolute AT1CON1.2;
  AT1CON1_ACCS             : bit  absolute AT1CON1.1;
  AT1CON1_VALID            : bit  absolute AT1CON1.0;
  AT1IR0                   : byte absolute $0816;
  AT1IR0_AT1PHSIF          : bit  absolute AT1IR0.2;
  AT1IR0_AT1MISSIF         : bit  absolute AT1IR0.1;
  AT1IR0_AT1PERIF          : bit  absolute AT1IR0.0;
  AT1IE0                   : byte absolute $0817;
  AT1IE0_AT1PHSIE          : bit  absolute AT1IE0.2;
  AT1IE0_AT1MISSIE         : bit  absolute AT1IE0.1;
  AT1IE0_AT1PERIE          : bit  absolute AT1IE0.0;
  AT1IR1                   : byte absolute $0818;
  AT1IR1_AT1CC3IF          : bit  absolute AT1IR1.2;
  AT1IR1_AT1CC2IF          : bit  absolute AT1IR1.1;
  AT1IR1_AT1CC1IF          : bit  absolute AT1IR1.0;
  AT1IE1                   : byte absolute $0819;
  AT1IE1_AT1CC3IE          : bit  absolute AT1IE1.2;
  AT1IE1_AT1CC2IE          : bit  absolute AT1IE1.1;
  AT1IE1_AT1CC1IE          : bit  absolute AT1IE1.0;
  AT1STPTL                 : byte absolute $081A;
  AT1STPTL_AT1STPT7        : bit  absolute AT1STPTL.7;
  AT1STPTL_AT1STPT6        : bit  absolute AT1STPTL.6;
  AT1STPTL_AT1STPT5        : bit  absolute AT1STPTL.5;
  AT1STPTL_AT1STPT4        : bit  absolute AT1STPTL.4;
  AT1STPTL_AT1STPT3        : bit  absolute AT1STPTL.3;
  AT1STPTL_AT1STPT2        : bit  absolute AT1STPTL.2;
  AT1STPTL_AT1STPT1        : bit  absolute AT1STPTL.1;
  AT1STPTL_AT1STPT0        : bit  absolute AT1STPTL.0;
  AT1STPTH                 : byte absolute $081B;
  AT1STPTH_AT1STPT14       : bit  absolute AT1STPTH.6;
  AT1STPTH_AT1STPT13       : bit  absolute AT1STPTH.5;
  AT1STPTH_AT1STPT12       : bit  absolute AT1STPTH.4;
  AT1STPTH_AT1STPT11       : bit  absolute AT1STPTH.3;
  AT1STPTH_AT1STPT10       : bit  absolute AT1STPTH.2;
  AT1STPTH_AT1STPT9        : bit  absolute AT1STPTH.1;
  AT1STPTH_AT1STPT8        : bit  absolute AT1STPTH.0;
  AT1ERRL                  : byte absolute $081C;
  AT1ERRL_AT1ERR7          : bit  absolute AT1ERRL.7;
  AT1ERRL_AT1ERR6          : bit  absolute AT1ERRL.6;
  AT1ERRL_AT1ERR5          : bit  absolute AT1ERRL.5;
  AT1ERRL_AT1ERR4          : bit  absolute AT1ERRL.4;
  AT1ERRL_AT1ERR3          : bit  absolute AT1ERRL.3;
  AT1ERRL_AT1ERR2          : bit  absolute AT1ERRL.2;
  AT1ERRL_AT1ERR1          : bit  absolute AT1ERRL.1;
  AT1ERRL_AT1ERR0          : bit  absolute AT1ERRL.0;
  AT1ERRH                  : byte absolute $081D;
  AT1ERRH_AT1ERR15         : bit  absolute AT1ERRH.7;
  AT1ERRH_AT1ERR14         : bit  absolute AT1ERRH.6;
  AT1ERRH_AT1ERR13         : bit  absolute AT1ERRH.5;
  AT1ERRH_AT1ERR12         : bit  absolute AT1ERRH.4;
  AT1ERRH_AT1ERR11         : bit  absolute AT1ERRH.3;
  AT1ERRH_AT1ERR10         : bit  absolute AT1ERRH.2;
  AT1ERRH_AT1ERR9          : bit  absolute AT1ERRH.1;
  AT1ERRH_AT1ERR8          : bit  absolute AT1ERRH.0;
  AT1CLK                   : byte absolute $088C;
  AT1SIG                   : byte absolute $088D;
  AT1SIG_SSEL2             : bit  absolute AT1SIG.2;
  AT1SIG_SSEL1             : bit  absolute AT1SIG.1;
  AT1SIG_SSEL0             : bit  absolute AT1SIG.0;
  AT1CSEL1                 : byte absolute $088E;
  AT1CSEL1_CP1S2           : bit  absolute AT1CSEL1.2;
  AT1CSEL1_CP1S1           : bit  absolute AT1CSEL1.1;
  AT1CSEL1_CP1S0           : bit  absolute AT1CSEL1.0;
  AT1CC1L                  : byte absolute $088F;
  AT1CC1L_AT1CC17          : bit  absolute AT1CC1L.7;
  AT1CC1L_AT1CC16          : bit  absolute AT1CC1L.6;
  AT1CC1L_AT1CC15          : bit  absolute AT1CC1L.5;
  AT1CC1L_AT1CC14          : bit  absolute AT1CC1L.4;
  AT1CC1L_AT1CC13          : bit  absolute AT1CC1L.3;
  AT1CC1L_AT1CC12          : bit  absolute AT1CC1L.2;
  AT1CC1L_AT1CC11          : bit  absolute AT1CC1L.1;
  AT1CC1L_AT1CC10          : bit  absolute AT1CC1L.0;
  AT1CC1H                  : byte absolute $0890;
  AT1CC1H_AT1CC19          : bit  absolute AT1CC1H.1;
  AT1CC1H_AT1CC18          : bit  absolute AT1CC1H.0;
  AT1CCON1                 : byte absolute $0891;
  AT1CCON1_AT1CC1EN        : bit  absolute AT1CCON1.7;
  AT1CCON1_AT1CC1POL       : bit  absolute AT1CCON1.4;
  AT1CCON1_AT1CAP1P        : bit  absolute AT1CCON1.3;
  AT1CCON1_AT1CC1MODE      : bit  absolute AT1CCON1.0;
  AT1CSEL2                 : byte absolute $0892;
  AT1CSEL2_AT1CP2S2        : bit  absolute AT1CSEL2.2;
  AT1CSEL2_AT1CP2S1        : bit  absolute AT1CSEL2.1;
  AT1CSEL2_AT1CP2S0        : bit  absolute AT1CSEL2.0;
  AT1CC2L                  : byte absolute $0893;
  AT1CC2L_AT1CC27          : bit  absolute AT1CC2L.7;
  AT1CC2L_AT1CC26          : bit  absolute AT1CC2L.6;
  AT1CC2L_AT1CC25          : bit  absolute AT1CC2L.5;
  AT1CC2L_AT1CC24          : bit  absolute AT1CC2L.4;
  AT1CC2L_AT1CC23          : bit  absolute AT1CC2L.3;
  AT1CC2L_AT1CC22          : bit  absolute AT1CC2L.2;
  AT1CC2L_AT1CC21          : bit  absolute AT1CC2L.1;
  AT1CC2L_AT1CC20          : bit  absolute AT1CC2L.0;
  AT1CC2H                  : byte absolute $0894;
  AT1CC2H_AT1CC29          : bit  absolute AT1CC2H.1;
  AT1CC2H_AT1CC28          : bit  absolute AT1CC2H.0;
  AT1CCON2                 : byte absolute $0895;
  AT1CCON2_AT1CC2EN        : bit  absolute AT1CCON2.7;
  AT1CCON2_AT1CC2POL       : bit  absolute AT1CCON2.4;
  AT1CCON2_AT1CAP2P        : bit  absolute AT1CCON2.3;
  AT1CCON2_AT1CC2MODE      : bit  absolute AT1CCON2.0;
  AT1CSEL3                 : byte absolute $0896;
  AT1CSEL3_AT1CP3S2        : bit  absolute AT1CSEL3.2;
  AT1CSEL3_AT1CP3S1        : bit  absolute AT1CSEL3.1;
  AT1CSEL3_AT1CP3S0        : bit  absolute AT1CSEL3.0;
  AT1CC3L                  : byte absolute $0897;
  AT1CC3L_AT1CC37          : bit  absolute AT1CC3L.7;
  AT1CC3L_AT1CC36          : bit  absolute AT1CC3L.6;
  AT1CC3L_AT1CC35          : bit  absolute AT1CC3L.5;
  AT1CC3L_AT1CC34          : bit  absolute AT1CC3L.4;
  AT1CC3L_AT1CC33          : bit  absolute AT1CC3L.3;
  AT1CC3L_AT1CC32          : bit  absolute AT1CC3L.2;
  AT1CC3L_AT1CC31          : bit  absolute AT1CC3L.1;
  AT1CC3L_AT1CC30          : bit  absolute AT1CC3L.0;
  AT1CC3H                  : byte absolute $0898;
  AT1CC3H_AT1CC39          : bit  absolute AT1CC3H.1;
  AT1CC3H_AT1CC38          : bit  absolute AT1CC3H.0;
  AT1CCON3                 : byte absolute $0899;
  AT1CCON3_AT1CC3EN        : bit  absolute AT1CCON3.7;
  AT1CCON3_AT1CC3POL       : bit  absolute AT1CCON3.4;
  AT1CCON3_AT1CAP3P        : bit  absolute AT1CCON3.3;
  AT1CCON3_AT1CC3MODE      : bit  absolute AT1CCON3.0;
  SMT1TMRL                 : byte absolute $0D8C;
  SMT1TMRH                 : byte absolute $0D8D;
  SMT1TMRU                 : byte absolute $0D8E;
  SMT1CPRL                 : byte absolute $0D8F;
  SMT1CPRH                 : byte absolute $0D90;
  SMT1CPRU                 : byte absolute $0D91;
  SMT1CPWL                 : byte absolute $0D92;
  SMT1CPWH                 : byte absolute $0D93;
  SMT1CPWU                 : byte absolute $0D94;
  SMT1PRL                  : byte absolute $0D95;
  SMT1PRH                  : byte absolute $0D96;
  SMT1PRU                  : byte absolute $0D97;
  SMT1CON0                 : byte absolute $0D98;
  SMT1CON0_STP             : bit  absolute SMT1CON0.5;
  SMT1CON0_WPOL            : bit  absolute SMT1CON0.4;
  SMT1CON0_SPOL            : bit  absolute SMT1CON0.3;
  SMT1CON0_CPOL            : bit  absolute SMT1CON0.2;
  SMT1CON0_SMT1PS1         : bit  absolute SMT1CON0.1;
  SMT1CON0_SMT1PS0         : bit  absolute SMT1CON0.0;
  SMT1CON1                 : byte absolute $0D99;
  SMT1CON1_SMT1GO          : bit  absolute SMT1CON1.7;
  SMT1CON1_REPEAT          : bit  absolute SMT1CON1.6;
  SMT1STAT                 : byte absolute $0D9A;
  SMT1STAT_CPRUP           : bit  absolute SMT1STAT.7;
  SMT1STAT_CPWUP           : bit  absolute SMT1STAT.6;
  SMT1STAT_RST             : bit  absolute SMT1STAT.5;
  SMT1STAT_TS              : bit  absolute SMT1STAT.2;
  SMT1STAT_WS              : bit  absolute SMT1STAT.1;
  SMT1STAT_AS              : bit  absolute SMT1STAT.0;
  SMT1CLK                  : byte absolute $0D9B;
  SMT1SIG                  : byte absolute $0D9C;
  SMT1WIN                  : byte absolute $0D9D;
  SMT2TMRL                 : byte absolute $0D9E;
  SMT2TMRH                 : byte absolute $0D9F;
  SMT2TMRU                 : byte absolute $0DA0;
  SMT2CPRL                 : byte absolute $0DA1;
  SMT2CPRH                 : byte absolute $0DA2;
  SMT2CPRU                 : byte absolute $0DA3;
  SMT2CPWL                 : byte absolute $0DA4;
  SMT2CPWH                 : byte absolute $0DA5;
  SMT2CPWU                 : byte absolute $0DA6;
  SMT2PRL                  : byte absolute $0DA7;
  SMT2PRH                  : byte absolute $0DA8;
  SMT2PRU                  : byte absolute $0DA9;
  SMT2CON0                 : byte absolute $0DAA;
  SMT2CON0_SMT2PS1         : bit  absolute SMT2CON0.1;
  SMT2CON0_SMT2PS0         : bit  absolute SMT2CON0.0;
  SMT2CON1                 : byte absolute $0DAB;
  SMT2CON1_SMT2GO          : bit  absolute SMT2CON1.7;
  SMT2STAT                 : byte absolute $0DAC;
  SMT2CLK                  : byte absolute $0DAD;
  SMT2SIG                  : byte absolute $0DAE;
  SMT2WIN                  : byte absolute $0DAF;
  PPSLOCK                  : byte absolute $0E0F;
  PPSLOCK_PPSLOCKED        : bit  absolute PPSLOCK.0;
  INTPPS                   : byte absolute $0E10;
  INTPPS_INTPPS4           : bit  absolute INTPPS.4;
  INTPPS_INTPPS3           : bit  absolute INTPPS.3;
  INTPPS_INTPPS2           : bit  absolute INTPPS.2;
  INTPPS_INTPPS1           : bit  absolute INTPPS.1;
  INTPPS_INTPPS0           : bit  absolute INTPPS.0;
  T0CKIPPS                 : byte absolute $0E11;
  T0CKIPPS_T0CKIPPS4       : bit  absolute T0CKIPPS.4;
  T0CKIPPS_T0CKIPPS3       : bit  absolute T0CKIPPS.3;
  T0CKIPPS_T0CKIPPS2       : bit  absolute T0CKIPPS.2;
  T0CKIPPS_T0CKIPPS1       : bit  absolute T0CKIPPS.1;
  T0CKIPPS_T0CKIPPS0       : bit  absolute T0CKIPPS.0;
  T1CKIPPS                 : byte absolute $0E12;
  T1CKIPPS_T1CKIPPS4       : bit  absolute T1CKIPPS.4;
  T1CKIPPS_T1CKIPPS3       : bit  absolute T1CKIPPS.3;
  T1CKIPPS_T1CKIPPS2       : bit  absolute T1CKIPPS.2;
  T1CKIPPS_T1CKIPPS1       : bit  absolute T1CKIPPS.1;
  T1CKIPPS_T1CKIPPS0       : bit  absolute T1CKIPPS.0;
  T1GPPS                   : byte absolute $0E13;
  T1GPPS_T1GPPS4           : bit  absolute T1GPPS.4;
  T1GPPS_T1GPPS3           : bit  absolute T1GPPS.3;
  T1GPPS_T1GPPS2           : bit  absolute T1GPPS.2;
  T1GPPS_T1GPPS1           : bit  absolute T1GPPS.1;
  T1GPPS_T1GPPS0           : bit  absolute T1GPPS.0;
  CCP1PPS                  : byte absolute $0E14;
  CCP1PPS_CCP1PPS4         : bit  absolute CCP1PPS.4;
  CCP1PPS_CCP1PPS3         : bit  absolute CCP1PPS.3;
  CCP1PPS_CCP1PPS2         : bit  absolute CCP1PPS.2;
  CCP1PPS_CCP1PPS1         : bit  absolute CCP1PPS.1;
  CCP1PPS_CCP1PPS0         : bit  absolute CCP1PPS.0;
  CCP2PPS                  : byte absolute $0E15;
  CCP2PPS_CCP2PPS4         : bit  absolute CCP2PPS.4;
  CCP2PPS_CCP2PPS3         : bit  absolute CCP2PPS.3;
  CCP2PPS_CCP2PPS2         : bit  absolute CCP2PPS.2;
  CCP2PPS_CCP2PPS1         : bit  absolute CCP2PPS.1;
  CCP2PPS_CCP2PPS0         : bit  absolute CCP2PPS.0;
  ATINPPS                  : byte absolute $0E16;
  ATINPPS_ATINPPS4         : bit  absolute ATINPPS.4;
  ATINPPS_ATINPPS3         : bit  absolute ATINPPS.3;
  ATINPPS_ATINPPS2         : bit  absolute ATINPPS.2;
  ATINPPS_ATINPPS1         : bit  absolute ATINPPS.1;
  ATINPPS_ATINPPS0         : bit  absolute ATINPPS.0;
  CWGINPPS                 : byte absolute $0E17;
  CWGINPPS_CWGINPPS4       : bit  absolute CWGINPPS.4;
  CWGINPPS_CWGINPPS3       : bit  absolute CWGINPPS.3;
  CWGINPPS_CWGINPPS2       : bit  absolute CWGINPPS.2;
  CWGINPPS_CWGINPPS1       : bit  absolute CWGINPPS.1;
  CWGINPPS_CWGINPPS0       : bit  absolute CWGINPPS.0;
  T2PPS                    : byte absolute $0E18;
  T2PPS_T2PPS4             : bit  absolute T2PPS.4;
  T2PPS_T2PPS3             : bit  absolute T2PPS.3;
  T2PPS_T2PPS2             : bit  absolute T2PPS.2;
  T2PPS_T2PPS1             : bit  absolute T2PPS.1;
  T2PPS_T2PPS0             : bit  absolute T2PPS.0;
  T3CKIPPS                 : byte absolute $0E19;
  T3CKIPPS_T3CKIPPS4       : bit  absolute T3CKIPPS.4;
  T3CKIPPS_T3CKIPPS3       : bit  absolute T3CKIPPS.3;
  T3CKIPPS_T3CKIPPS2       : bit  absolute T3CKIPPS.2;
  T3CKIPPS_T3CKIPPS1       : bit  absolute T3CKIPPS.1;
  T3CKIPPS_T3CKIPPS0       : bit  absolute T3CKIPPS.0;
  T3GPPS                   : byte absolute $0E1A;
  T3GPPS_T3GPPS4           : bit  absolute T3GPPS.4;
  T3GPPS_T3GPPS3           : bit  absolute T3GPPS.3;
  T3GPPS_T3GPPS2           : bit  absolute T3GPPS.2;
  T3GPPS_T3GPPS1           : bit  absolute T3GPPS.1;
  T3GPPS_T3GPPS0           : bit  absolute T3GPPS.0;
  T4PPS                    : byte absolute $0E1B;
  T4PPS_T4PPS4             : bit  absolute T4PPS.4;
  T4PPS_T4PPS3             : bit  absolute T4PPS.3;
  T4PPS_T4PPS2             : bit  absolute T4PPS.2;
  T4PPS_T4PPS1             : bit  absolute T4PPS.1;
  T4PPS_T4PPS0             : bit  absolute T4PPS.0;
  T5CKIPPS                 : byte absolute $0E1C;
  T5CKIPPS_T5CKIPPS4       : bit  absolute T5CKIPPS.4;
  T5CKIPPS_T5CKIPPS3       : bit  absolute T5CKIPPS.3;
  T5CKIPPS_T5CKIPPS2       : bit  absolute T5CKIPPS.2;
  T5CKIPPS_T5CKIPPS1       : bit  absolute T5CKIPPS.1;
  T5CKIPPS_T5CKIPPS0       : bit  absolute T5CKIPPS.0;
  T5GPPS                   : byte absolute $0E1D;
  T5GPPS_T5GPPS4           : bit  absolute T5GPPS.4;
  T5GPPS_T5GPPS3           : bit  absolute T5GPPS.3;
  T5GPPS_T5GPPS2           : bit  absolute T5GPPS.2;
  T5GPPS_T5GPPS1           : bit  absolute T5GPPS.1;
  T5GPPS_T5GPPS0           : bit  absolute T5GPPS.0;
  T6PPS                    : byte absolute $0E1E;
  T6PPS_T6PPS4             : bit  absolute T6PPS.4;
  T6PPS_T6PPS3             : bit  absolute T6PPS.3;
  T6PPS_T6PPS2             : bit  absolute T6PPS.2;
  T6PPS_T6PPS1             : bit  absolute T6PPS.1;
  T6PPS_T6PPS0             : bit  absolute T6PPS.0;
  ATCC1PPS                 : byte absolute $0E1F;
  ATCC1PPS_ATCC1PPS4       : bit  absolute ATCC1PPS.4;
  ATCC1PPS_ATCC1PPS3       : bit  absolute ATCC1PPS.3;
  ATCC1PPS_ATCC1PPS2       : bit  absolute ATCC1PPS.2;
  ATCC1PPS_ATCC1PPS1       : bit  absolute ATCC1PPS.1;
  ATCC1PPS_ATCC1PPS0       : bit  absolute ATCC1PPS.0;
  SSPCLKPPS                : byte absolute $0E20;
  SSPCLKPPS_SSPCLKPPS4     : bit  absolute SSPCLKPPS.4;
  SSPCLKPPS_SSPCLKPPS3     : bit  absolute SSPCLKPPS.3;
  SSPCLKPPS_SSPCLKPPS2     : bit  absolute SSPCLKPPS.2;
  SSPCLKPPS_SSPCLKPPS1     : bit  absolute SSPCLKPPS.1;
  SSPCLKPPS_SSPCLKPPS0     : bit  absolute SSPCLKPPS.0;
  SSPDATPPS                : byte absolute $0E21;
  SSPDATPPS_SSPDATPPS4     : bit  absolute SSPDATPPS.4;
  SSPDATPPS_SSPDATPPS3     : bit  absolute SSPDATPPS.3;
  SSPDATPPS_SSPDATPPS2     : bit  absolute SSPDATPPS.2;
  SSPDATPPS_SSPDATPPS1     : bit  absolute SSPDATPPS.1;
  SSPDATPPS_SSPDATPPS0     : bit  absolute SSPDATPPS.0;
  SSPSSPPS                 : byte absolute $0E22;
  SSPSSPPS_SSPSSPPS4       : bit  absolute SSPSSPPS.4;
  SSPSSPPS_SSPSSPPS3       : bit  absolute SSPSSPPS.3;
  SSPSSPPS_SSPSSPPS2       : bit  absolute SSPSSPPS.2;
  SSPSSPPS_SSPSSPPS1       : bit  absolute SSPSSPPS.1;
  SSPSSPPS_SSPSSPPS0       : bit  absolute SSPSSPPS.0;
  ATCC2PPS                 : byte absolute $0E23;
  ATCC2PPS_ATCC2PPS4       : bit  absolute ATCC2PPS.4;
  ATCC2PPS_ATCC2PPS3       : bit  absolute ATCC2PPS.3;
  ATCC2PPS_ATCC2PPS2       : bit  absolute ATCC2PPS.2;
  ATCC2PPS_ATCC2PPS1       : bit  absolute ATCC2PPS.1;
  ATCC2PPS_ATCC2PPS0       : bit  absolute ATCC2PPS.0;
  RXPPS                    : byte absolute $0E24;
  RXPPS_RXPPS4             : bit  absolute RXPPS.4;
  RXPPS_RXPPS3             : bit  absolute RXPPS.3;
  RXPPS_RXPPS2             : bit  absolute RXPPS.2;
  RXPPS_RXPPS1             : bit  absolute RXPPS.1;
  RXPPS_RXPPS0             : bit  absolute RXPPS.0;
  CKPPS                    : byte absolute $0E25;
  CKPPS_CKPPS4             : bit  absolute CKPPS.4;
  CKPPS_CKPPS3             : bit  absolute CKPPS.3;
  CKPPS_CKPPS2             : bit  absolute CKPPS.2;
  CKPPS_CKPPS1             : bit  absolute CKPPS.1;
  CKPPS_CKPPS0             : bit  absolute CKPPS.0;
  SMT1SIGPPS               : byte absolute $0E26;
  SMT1SIGPPS_SMT1SIGPPS4   : bit  absolute SMT1SIGPPS.4;
  SMT1SIGPPS_SMT1SIGPPS3   : bit  absolute SMT1SIGPPS.3;
  SMT1SIGPPS_SMT1SIGPPS2   : bit  absolute SMT1SIGPPS.2;
  SMT1SIGPPS_SMT1SIGPPS1   : bit  absolute SMT1SIGPPS.1;
  SMT1SIGPPS_SMT1SIGPPS0   : bit  absolute SMT1SIGPPS.0;
  SMT1WINPPS               : byte absolute $0E27;
  SMT1WINPPS_SMTWINPPS4    : bit  absolute SMT1WINPPS.4;
  SMT1WINPPS_SMTWINPPS3    : bit  absolute SMT1WINPPS.3;
  SMT1WINPPS_SMTWINPPS2    : bit  absolute SMT1WINPPS.2;
  SMT1WINPPS_SMTWINPPS1    : bit  absolute SMT1WINPPS.1;
  SMT1WINPPS_SMTWINPPS0    : bit  absolute SMT1WINPPS.0;
  CLCIN0PPS                : byte absolute $0E28;
  CLCIN0PPS_CLCIN0PPS4     : bit  absolute CLCIN0PPS.4;
  CLCIN0PPS_CLCIN0PPS3     : bit  absolute CLCIN0PPS.3;
  CLCIN0PPS_CLCIN0PPS2     : bit  absolute CLCIN0PPS.2;
  CLCIN0PPS_CLCIN0PPS1     : bit  absolute CLCIN0PPS.1;
  CLCIN0PPS_CLCIN0PPS0     : bit  absolute CLCIN0PPS.0;
  CLCIN1PPS                : byte absolute $0E29;
  CLCIN1PPS_CLCIN1PPS4     : bit  absolute CLCIN1PPS.4;
  CLCIN1PPS_CLCIN1PPS3     : bit  absolute CLCIN1PPS.3;
  CLCIN1PPS_CLCIN1PPS2     : bit  absolute CLCIN1PPS.2;
  CLCIN1PPS_CLCIN1PPS1     : bit  absolute CLCIN1PPS.1;
  CLCIN1PPS_CLCIN1PPS0     : bit  absolute CLCIN1PPS.0;
  CLCIN2PPS                : byte absolute $0E2A;
  CLCIN2PPS_CLCIN2PPS4     : bit  absolute CLCIN2PPS.4;
  CLCIN2PPS_CLCIN2PPS3     : bit  absolute CLCIN2PPS.3;
  CLCIN2PPS_CLCIN2PPS2     : bit  absolute CLCIN2PPS.2;
  CLCIN2PPS_CLCIN2PPS1     : bit  absolute CLCIN2PPS.1;
  CLCIN2PPS_CLCIN2PPS0     : bit  absolute CLCIN2PPS.0;
  CLCIN3PPS                : byte absolute $0E2B;
  CLCIN3PPS_CLCIN3PPS4     : bit  absolute CLCIN3PPS.4;
  CLCIN3PPS_CLCIN3PPS3     : bit  absolute CLCIN3PPS.3;
  CLCIN3PPS_CLCIN3PPS2     : bit  absolute CLCIN3PPS.2;
  CLCIN3PPS_CLCIN3PPS1     : bit  absolute CLCIN3PPS.1;
  CLCIN3PPS_CLCIN3PPS0     : bit  absolute CLCIN3PPS.0;
  SMT2SIGPPS               : byte absolute $0E2C;
  SMT2SIGPPS_SMT2SIGPPS4   : bit  absolute SMT2SIGPPS.4;
  SMT2SIGPPS_SMT2SIGPPS3   : bit  absolute SMT2SIGPPS.3;
  SMT2SIGPPS_SMT2SIGPPS2   : bit  absolute SMT2SIGPPS.2;
  SMT2SIGPPS_SMT2SIGPPS1   : bit  absolute SMT2SIGPPS.1;
  SMT2SIGPPS_SMT2SIGPPS0   : bit  absolute SMT2SIGPPS.0;
  SMT2WINPPS               : byte absolute $0E2D;
  SMT2WINPPS_SMT2WINPPS4   : bit  absolute SMT2WINPPS.4;
  SMT2WINPPS_SMT2WINPPS3   : bit  absolute SMT2WINPPS.3;
  SMT2WINPPS_SMT2WINPPS2   : bit  absolute SMT2WINPPS.2;
  SMT2WINPPS_SMT2WINPPS1   : bit  absolute SMT2WINPPS.1;
  SMT2WINPPS_SMT2WINPPS0   : bit  absolute SMT2WINPPS.0;
  ATCC3PPS                 : byte absolute $0E2E;
  ATCC3PPS_ATCC3PPS4       : bit  absolute ATCC3PPS.4;
  ATCC3PPS_ATCC3PPS3       : bit  absolute ATCC3PPS.3;
  ATCC3PPS_ATCC3PPS2       : bit  absolute ATCC3PPS.2;
  ATCC3PPS_ATCC3PPS1       : bit  absolute ATCC3PPS.1;
  ATCC3PPS_ATCC3PPS0       : bit  absolute ATCC3PPS.0;
  RA0PPS                   : byte absolute $0E90;
  RA0PPS_RA0PPS4           : bit  absolute RA0PPS.4;
  RA0PPS_RA0PPS3           : bit  absolute RA0PPS.3;
  RA0PPS_RA0PPS2           : bit  absolute RA0PPS.2;
  RA0PPS_RA0PPS1           : bit  absolute RA0PPS.1;
  RA0PPS_RA0PPS0           : bit  absolute RA0PPS.0;
  RA1PPS                   : byte absolute $0E91;
  RA1PPS_RA1PPS4           : bit  absolute RA1PPS.4;
  RA1PPS_RA1PPS3           : bit  absolute RA1PPS.3;
  RA1PPS_RA1PPS2           : bit  absolute RA1PPS.2;
  RA1PPS_RA1PPS1           : bit  absolute RA1PPS.1;
  RA1PPS_RA1PPS0           : bit  absolute RA1PPS.0;
  RA2PPS                   : byte absolute $0E92;
  RA2PPS_RA2PPS4           : bit  absolute RA2PPS.4;
  RA2PPS_RA2PPS3           : bit  absolute RA2PPS.3;
  RA2PPS_RA2PPS2           : bit  absolute RA2PPS.2;
  RA2PPS_RA2PPS1           : bit  absolute RA2PPS.1;
  RA2PPS_RA2PPS0           : bit  absolute RA2PPS.0;
  RA4PPS                   : byte absolute $0E94;
  RA4PPS_RA4PPS4           : bit  absolute RA4PPS.4;
  RA4PPS_RA4PPS3           : bit  absolute RA4PPS.3;
  RA4PPS_RA4PPS2           : bit  absolute RA4PPS.2;
  RA4PPS_RA4PPS1           : bit  absolute RA4PPS.1;
  RA4PPS_RA4PPS0           : bit  absolute RA4PPS.0;
  RA5PPS                   : byte absolute $0E95;
  RA5PPS_RA5PPS4           : bit  absolute RA5PPS.4;
  RA5PPS_RA5PPS3           : bit  absolute RA5PPS.3;
  RA5PPS_RA5PPS2           : bit  absolute RA5PPS.2;
  RA5PPS_RA5PPS1           : bit  absolute RA5PPS.1;
  RA5PPS_RA5PPS0           : bit  absolute RA5PPS.0;
  RC0PPS                   : byte absolute $0EA0;
  RC0PPS_RC0PPS4           : bit  absolute RC0PPS.4;
  RC0PPS_RC0PPS3           : bit  absolute RC0PPS.3;
  RC0PPS_RC0PPS2           : bit  absolute RC0PPS.2;
  RC0PPS_RC0PPS1           : bit  absolute RC0PPS.1;
  RC0PPS_RC0PPS0           : bit  absolute RC0PPS.0;
  RC1PPS                   : byte absolute $0EA1;
  RC1PPS_RC1PPS4           : bit  absolute RC1PPS.4;
  RC1PPS_RC1PPS3           : bit  absolute RC1PPS.3;
  RC1PPS_RC1PPS2           : bit  absolute RC1PPS.2;
  RC1PPS_RC1PPS1           : bit  absolute RC1PPS.1;
  RC1PPS_RC1PPS0           : bit  absolute RC1PPS.0;
  RC2PPS                   : byte absolute $0EA2;
  RC2PPS_RC2PPS4           : bit  absolute RC2PPS.4;
  RC2PPS_RC2PPS3           : bit  absolute RC2PPS.3;
  RC2PPS_RC2PPS2           : bit  absolute RC2PPS.2;
  RC2PPS_RC2PPS1           : bit  absolute RC2PPS.1;
  RC2PPS_RC2PPS0           : bit  absolute RC2PPS.0;
  RC3PPS                   : byte absolute $0EA3;
  RC3PPS_RC3PPS4           : bit  absolute RC3PPS.4;
  RC3PPS_RC3PPS3           : bit  absolute RC3PPS.3;
  RC3PPS_RC3PPS2           : bit  absolute RC3PPS.2;
  RC3PPS_RC3PPS1           : bit  absolute RC3PPS.1;
  RC3PPS_RC3PPS0           : bit  absolute RC3PPS.0;
  RC4PPS                   : byte absolute $0EA4;
  RC4PPS_RC4PPS4           : bit  absolute RC4PPS.4;
  RC4PPS_RC4PPS3           : bit  absolute RC4PPS.3;
  RC4PPS_RC4PPS2           : bit  absolute RC4PPS.2;
  RC4PPS_RC4PPS1           : bit  absolute RC4PPS.1;
  RC4PPS_RC4PPS0           : bit  absolute RC4PPS.0;
  RC5PPS                   : byte absolute $0EA5;
  RC5PPS_RC5PPS4           : bit  absolute RC5PPS.4;
  RC5PPS_RC5PPS3           : bit  absolute RC5PPS.3;
  RC5PPS_RC5PPS2           : bit  absolute RC5PPS.2;
  RC5PPS_RC5PPS1           : bit  absolute RC5PPS.1;
  RC5PPS_RC5PPS0           : bit  absolute RC5PPS.0;
  CLCDATA                  : byte absolute $0F0F;
  CLCDATA_MLC4OUT          : bit  absolute CLCDATA.3;
  CLCDATA_MLC3OUT          : bit  absolute CLCDATA.2;
  CLCDATA_MLC2OUT          : bit  absolute CLCDATA.1;
  CLCDATA_MLC1OUT          : bit  absolute CLCDATA.0;
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
  {$SET_STATE_RAM '00C-00C:SFR'}            // Bank 0 : PORTA
  {$SET_STATE_RAM '00E-00E:SFR'}            // Bank 0 : PORTC
  {$SET_STATE_RAM '010-01F:SFR'}            // Bank 0 : PIR1, PIR2, PIR3, PIR4, PIR5, TMR0, TMR1L, TMR1H, T1CON, T1GCON, T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '08E-08E:SFR'}            // Bank 1 : TRISC
  {$SET_STATE_RAM '090-096:SFR'}            // Bank 1 : PIE1, PIE2, PIE3, PIE4, PIE5, OPTION_REG, PCON
  {$SET_STATE_RAM '098-09F:SFR'}            // Bank 1 : OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '10E-10E:SFR'}            // Bank 2 : LATC
  {$SET_STATE_RAM '111-119:SFR'}            // Bank 2 : CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DAC1CON0, DAC1CON1
  {$SET_STATE_RAM '11C-11C:SFR'}            // Bank 2 : ZCD1CON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '18E-18E:SFR'}            // Bank 3 : ANSELC
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '20E-20E:SFR'}            // Bank 4 : WPUC
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28C:SFR'}            // Bank 5 : ODCONA
  {$SET_STATE_RAM '28E-28E:SFR'}            // Bank 5 : ODCONC
  {$SET_STATE_RAM '291-294:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, CCP1CAP
  {$SET_STATE_RAM '298-29B:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON, CCP2CAP
  {$SET_STATE_RAM '29E-29E:SFR'}            // Bank 5 : CCPTMRS
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30C:SFR'}            // Bank 6 : SLRCONA
  {$SET_STATE_RAM '30E-30E:SFR'}            // Bank 6 : SLRCONC
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-38C:SFR'}            // Bank 7 : INLVLA
  {$SET_STATE_RAM '38E-38E:SFR'}            // Bank 7 : INLVLC
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '397-399:SFR'}            // Bank 7 : IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '40E-40E:SFR'}            // Bank 8 : HIDRVC
  {$SET_STATE_RAM '413-418:SFR'}            // Bank 8 : T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '41A-41F:SFR'}            // Bank 8 : T6TMR, T6PR, T6CON, T6HLT, T6CLKCON, T6RST
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '493-496:SFR'}            // Bank 9 : TMR3L, TMR3H, T3CON, T3GCON
  {$SET_STATE_RAM '49A-49D:SFR'}            // Bank 9 : TMR5L, TMR5H, T5CON, T5GCON
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '58C-59D:SFR'}            // Bank 11 : PID1SETL, PID1SETH, PID1INL, PID1INH, PID1K1L, PID1K1H, PID1K2L, PID1K2H, PID1K3L, PID1K3H, PID1OUTLL, PID1OUTLH, PID1OUTHL, PID1OUTHH, PID1OUTU, PID1Z1L, PID1Z1H, PID1Z1U
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '60C-614:SFR'}            // Bank 12 : PID1Z2L, PID1Z2H, PID1Z2U, PID1ACCLL, PID1ACCLH, PID1ACCHL, PID1ACCHH, PID1ACCU, PID1CON
  {$SET_STATE_RAM '617-61C:SFR'}            // Bank 12 : PWM3DCL, PWM3DCH, PWM3CON, PWM4DCL, PWM4DCH, PWM4CON
  {$SET_STATE_RAM '620-64F:GPR'}           
  {$SET_STATE_RAM '691-697:SFR'}            // Bank 13 : CWG1DBR, CWG1DBF, CWG1AS0, CWG1AS1, CWG1OCON0, CWG1CON0, CWG1CON1
  {$SET_STATE_RAM '699-69A:SFR'}            // Bank 13 : CWG1CLKCON, CWG1ISM
  {$SET_STATE_RAM '711-715:SFR'}            // Bank 14 : WDTCON0, WDTCON1, WDTPSL, WDTPSH, WDTTMR
  {$SET_STATE_RAM '718-71D:SFR'}            // Bank 14 : SCANLADRL, SCANLADRH, SCANHADRL, SCANHADRH, SCANCON0, SCANTRIG
  {$SET_STATE_RAM '791-79A:SFR'}            // Bank 15 : CRCDATL, CRCDATH, CRCACCL, CRCACCH, CRCSHIFTL, CRCSHIFTH, CRCXORL, CRCXORH, CRCCON0, CRCCON1
  {$SET_STATE_RAM '80C-81D:SFR'}            // Bank 16 : AT1RESL, AT1RESH, AT1MISSL, AT1MISSH, AT1PERL, AT1PERH, AT1PHSL, AT1PHSH, AT1CON0, AT1CON1, AT1IR0, AT1IE0, AT1IR1, AT1IE1, AT1STPTL, AT1STPTH, AT1ERRL, AT1ERRH
  {$SET_STATE_RAM '88C-899:SFR'}            // Bank 17 : AT1CLK, AT1SIG, AT1CSEL1, AT1CC1L, AT1CC1H, AT1CCON1, AT1CSEL2, AT1CC2L, AT1CC2H, AT1CCON2, AT1CSEL3, AT1CC3L, AT1CC3H, AT1CCON3
  {$SET_STATE_RAM 'D8C-DAF:SFR'}            // Bank 27 : SMT1TMRL, SMT1TMRH, SMT1TMRU, SMT1CPRL, SMT1CPRH, SMT1CPRU, SMT1CPWL, SMT1CPWH, SMT1CPWU, SMT1PRL, SMT1PRH, SMT1PRU, SMT1CON0, SMT1CON1, SMT1STAT, SMT1CLK, SMT1SIG, SMT1WIN, SMT2TMRL, SMT2TMRH, SMT2TMRU, SMT2CPRL, SMT2CPRH, SMT2CPRU, SMT2CPWL, SMT2CPWH, SMT2CPWU, SMT2PRL, SMT2PRH, SMT2PRU, SMT2CON0, SMT2CON1, SMT2STAT, SMT2CLK, SMT2SIG, SMT2WIN
  {$SET_STATE_RAM 'E0F-E2E:SFR'}            // Bank 28 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, CCP1PPS, CCP2PPS, ATINPPS, CWGINPPS, T2PPS, T3CKIPPS, T3GPPS, T4PPS, T5CKIPPS, T5GPPS, T6PPS, ATCC1PPS, SSPCLKPPS, SSPDATPPS, SSPSSPPS, ATCC2PPS, RXPPS, CKPPS, SMT1SIGPPS, SMT1WINPPS, CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS, SMT2SIGPPS, SMT2WINPPS, ATCC3PPS
  {$SET_STATE_RAM 'E90-E92:SFR'}            // Bank 29 : RA0PPS, RA1PPS, RA2PPS
  {$SET_STATE_RAM 'E94-E95:SFR'}            // Bank 29 : RA4PPS, RA5PPS
  {$SET_STATE_RAM 'EA0-EA5:SFR'}            // Bank 29 : RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS
  {$SET_STATE_RAM 'F0F-F37:SFR'}            // Bank 30 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3, CLC4CON, CLC4POL, CLC4SEL0, CLC4SEL1, CLC4SEL2, CLC4SEL3, CLC4GLS0, CLC4GLS1, CLC4GLS2, CLC4GLS3
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00E:3F'} // PORTC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:EF'} // PIR2 bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:3F'} // PIR3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '014:F7'} // PIR5 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:0F'} // T2CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:0F'} // T2RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:3F'} // TRISC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:EF'} // PIE2 bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:3F'} // PIE3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '094:F7'} // PIE5 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:7F'} // OSCSTAT bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F8'} // ADCON2 bits 2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:3F'} // LATA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // LATC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:D7'} // CM1CON0 bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:D7'} // CM2CON0 bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:F7'} // CM2CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:AC'} // DAC1CON0 bits 6,4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:B3'} // ZCD1CON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:0F'} // ANSELC bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:3F'} // WPUC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28E:3F'} // ODCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:BF'} // CCP1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:07'} // CCP1CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29A:BF'} // CCP2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29B:07'} // CCP2CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30C:37'} // SLRCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30E:3F'} // SLRCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:3F'} // INLVLA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38E:3F'} // INLVLC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '40E:30'} // HIDRVC bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:0F'} // T4CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '418:0F'} // T4RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:0F'} // T6CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41F:0F'} // T6RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '495:F5'} // T3CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49C:F5'} // T5CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59A:0F'} // PID1OUTU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59D:01'} // PID1Z1U bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60E:01'} // PID1Z2U bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:07'} // PID1ACCU bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '614:C7'} // PID1CON bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '617:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:B0'} // PWM3CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61A:C0'} // PWM4DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61C:B0'} // PWM4CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:FC'} // CWG1AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:77'} // CWG1AS1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '696:C7'} // CWG1CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '697:2F'} // CWG1CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '699:01'} // CWG1CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69A:0F'} // CWG1ISM bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:3F'} // WDTCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:77'} // WDTCON1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71C:FB'} // SCANCON0 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71D:0F'} // SCANTRIG bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:FE'} // CRCXORL bit 0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:F3'} // CRCCON0 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80D:03'} // AT1RESH bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '813:03'} // AT1PHSH bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '814:FB'} // AT1CON0 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '815:57'} // AT1CON1 bits 7,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '816:07'} // AT1IR0 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '817:07'} // AT1IE0 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '818:07'} // AT1IR1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '819:07'} // AT1IE1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81B:7F'} // AT1STPTH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88C:01'} // AT1CLK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88D:07'} // AT1SIG bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88E:07'} // AT1CSEL1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '890:03'} // AT1CC1H bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '891:99'} // AT1CCON1 bits 6,5,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '892:07'} // AT1CSEL2 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '894:03'} // AT1CC2H bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '895:99'} // AT1CCON2 bits 6,5,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '896:07'} // AT1CSEL3 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '898:03'} // AT1CC3H bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '899:99'} // AT1CCON3 bits 6,5,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D98:BF'} // SMT1CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D99:CF'} // SMT1CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9A:E7'} // SMT1STAT bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9B:07'} // SMT1CLK bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9C:1F'} // SMT1SIG bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9D:1F'} // SMT1WIN bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAA:BF'} // SMT2CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAB:CF'} // SMT2CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAC:E7'} // SMT2STAT bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAD:07'} // SMT2CLK bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAE:1F'} // SMT2SIG bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAF:1F'} // SMT2WIN bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E10:1F'} // INTPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E11:1F'} // T0CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E12:1F'} // T1CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E13:1F'} // T1GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E14:1F'} // CCP1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E15:1F'} // CCP2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E16:1F'} // ATINPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E17:1F'} // CWGINPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E18:1F'} // T2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E19:1F'} // T3CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1A:1F'} // T3GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1B:1F'} // T4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1C:1F'} // T5CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1D:1F'} // T5GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1E:1F'} // T6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1F:1F'} // ATCC1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E20:1F'} // SSPCLKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E21:1F'} // SSPDATPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E22:1F'} // SSPSSPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E23:1F'} // ATCC2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E24:1F'} // RXPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E25:1F'} // CKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E26:1F'} // SMT1SIGPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E27:1F'} // SMT1WINPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E28:1F'} // CLCIN0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E29:1F'} // CLCIN1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2A:1F'} // CLCIN2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2B:1F'} // CLCIN3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2C:1F'} // SMT2SIGPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2D:1F'} // SMT2WINPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2E:1F'} // ATCC3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E90:1F'} // RA0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E91:1F'} // RA1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E92:1F'} // RA2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E94:1F'} // RA4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E95:1F'} // RA5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA0:1F'} // RC0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA1:1F'} // RC1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA2:1F'} // RC2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA3:1F'} // RC3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA4:1F'} // RC4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA5:1F'} // RC5PPS bits 7,6,5 un-implemented (read as 0)
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

  {$SET_UNIMP_BITS1 '08C:08'} // TRISA bit 3 un-implemented (read as 1)
  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : RA5/CLKIN
  // Pin  3 : RA4/AN3/CLKOUT
  // Pin  4 : RA3/VPP/nMCLR/MCLR
  // Pin  5 : RC5/HIDC5
  // Pin  6 : RC4/HIDC4
  // Pin  7 : RC3/AN7/C1IN3-/C2IN3-
  // Pin  8 : RC2/AN6/C1IN2-/C2IN2-
  // Pin  9 : RC1/AN5/C1IN1-/C2IN1-
  // Pin 10 : RC0/AN4/C2IN+
  // Pin 11 : RA2/AN2
  // Pin 12 : RA1/AN1/VREF+/C1IN0-/C2IN0-/ICSPCLK
  // Pin 13 : RA0/AN0/DACOUT/C1IN+/ICSPDAT
  // Pin 14 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '00E:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_HS          = $3FFA}  // HS Oscillator, High-speed crystal/resonator connected between OSC1 and OSC2 pins
  {$define _FOSC_ECH         = $3FFF}  // ECH, External Clock, High Power Mode (4-20 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECM         = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_ECL         = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz): device clock supplied to CLKIN pins
  {$define _FOSC_INTOSC      = $3FFC}  // INTOSC oscillator: I/O function on CLKIN pin

  // PWRTE : Power-up Timer Enable
  {$define _PWRTE_OFF        = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON         = $3FDF}  // PWRT enabled

  // MCLRE : MCLR Pin Function Select
  {$define _MCLRE_ON         = $3FFF}  // MCLR/VPP pin function is MCLR
  {$define _MCLRE_OFF        = $3FBF}  // MCLR/VPP pin function is digital input

  // CP : Flash Program Memory Code Protection
  {$define _CP_OFF           = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON            = $3F7F}  // Program memory code protection is enabled

  // BOREN : Brown-out Reset Enable
  {$define _BOREN_ON         = $3FFF}  // Brown-out Reset enabled
  {$define _BOREN_NSLEEP     = $3DFF}  // Brown-out Reset enabled while running and disabled in Sleep
  {$define _BOREN_SBODEN     = $3BFF}  // Brown-out Reset controlled by the SBOREN bit in the BORCON register
  {$define _BOREN_OFF        = $39FF}  // Brown-out Reset disabled

  // CLKOUTEN : Clock Out Enable
  {$define _CLKOUTEN_OFF     = $3FFF}  // CLKOUT function is disabled. I/O or oscillator function on the CLKOUT pin
  {$define _CLKOUTEN_ON      = $37FF}  // CLKOUT function is enabled on the CLKOUT pin

  // IESO : Internal/External Switch Over
  {$define _IESO_ON          = $3FFF}  // Internal External Switch Over mode is enabled
  {$define _IESO_OFF         = $2FFF}  // Internal External Switch Over mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enable
  {$define _FCMEN_ON         = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF        = $1FFF}  // Fail-Safe Clock Monitor is disabled

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF          = $3FFF}  // Write protection off
  {$define _WRT_BOOT         = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by PMCON control
  {$define _WRT_HALF         = $3FFD}  // 000h to FFFh write protected, 1000h to 1FFFh may be modified by PMCON control
  {$define _WRT_ALL          = $3FFC}  // 000h to 1FFFh write protected, no addresses may be modified by PMCON control

  // PPS1WAY : Peripheral Pin Select one-way control
  {$define _PPS1WAY_ON       = $3FFF}  // The PPSLOCK bit cannot be cleared once it is set by software
  {$define _PPS1WAY_OFF      = $3FFB}  // The PPSLOCK bit can be set and cleared repeatedly by software

  // ZCD : Zero Cross Detect Disable Bit
  {$define _ZCD_OFF          = $3FFF}  // ZCD disable.  ZCD can be enabled by setting the ZCDSEN bit of ZCDCON
  {$define _ZCD_ON           = $3F7F}  // ZCD always enabled

  // PLLEN : PLL Enable Bit
  {$define _PLLEN_ON         = $3FFF}  // 4x PLL is always enabled
  {$define _PLLEN_OFF        = $3EFF}  // 4x PLL is enabled when software sets the SPLLEN bit

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON        = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF       = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO          = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI          = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LPBOR : Low-Power Brown Out Reset
  {$define _LPBOR_OFF        = $3FFF}  // Low-Power BOR is disabled
  {$define _LPBOR_ON         = $37FF}  // Low-Power BOR is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON           = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF          = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

  // WDTCPS : WDT Period Select
  {$define _WDTCPS_WDTCPS0   = $3FE0}  // 1:32 (1 ms period)
  {$define _WDTCPS_WDTCPS1   = $3FE1}  // 1:64 (2 ms period)
  {$define _WDTCPS_WDTCPS2   = $3FE2}  // 1:128 (4 ms period)
  {$define _WDTCPS_WDTCPS3   = $3FE3}  // 1:256 (8 ms period)
  {$define _WDTCPS_WDTCPS4   = $3FE4}  // 1:512 (16 ms period)
  {$define _WDTCPS_WDTCPS5   = $3FE5}  // 1:1024 (32 ms period)
  {$define _WDTCPS_WDTCPS6   = $3FE6}  // 1:2048 (64 ms period)
  {$define _WDTCPS_WDTCPS7   = $3FE7}  // 1:4096 (128 ms period)
  {$define _WDTCPS_WDTCPS8   = $3FE8}  // 1:8192 (256 ms period)
  {$define _WDTCPS_WDTCPS9   = $3FE9}  // 1:16384 (512 ms period)
  {$define _WDTCPS_WDTCPSA   = $3FEA}  // 1:32768 (1 s period)
  {$define _WDTCPS_WDTCPSB   = $3FEB}  // 1:65536 (2 s period)
  {$define _WDTCPS_WDTCPSC   = $3FEC}  // 1:131072 (4 s period)
  {$define _WDTCPS_WDTCPSD   = $3FED}  // 1:262144 (8 s period)
  {$define _WDTCPS_WDTCPSE   = $3FEE}  // 1:524299 (16 s period)
  {$define _WDTCPS_WDTCPSF   = $3FEF}  // 1:1048576 (32 s period)
  {$define _WDTCPS_WDTCPS10  = $3FF0}  // 1:2097152 (64 s period)
  {$define _WDTCPS_WDTCPS11  = $3FF1}  // 1:4194304 (128 s period)
  {$define _WDTCPS_WDTCPS12  = $3FF2}  // 1:8388608 (256 s period)
  {$define _WDTCPS_WDTCPS1F  = $3FFF}  // Software Control (WDTPS)

  // WDTE : Watchdog Timer Enable
  {$define _WDTE_ON          = $3FFF}  // WDT enabled
  {$define _WDTE_NSLEEP      = $3FDF}  // WDT enabled while running and disabled in Sleep
  {$define _WDTE_SWDTEN      = $3FBF}  // WDT controlled by the SWDTEN bit in the WDTCON register
  {$define _WDTE_OFF         = $3F9F}  // WDT disabled

  // WDTCWS : WDT Window Select
  {$define _WDTCWS_WDTCWS125 = $38FF}  // 12.5 percent window open time
  {$define _WDTCWS_WDTCWS25  = $39FF}  // 25 percent window open time
  {$define _WDTCWS_WDTCWS375 = $3AFF}  // 37.5 percent window open time
  {$define _WDTCWS_WDTCWS50  = $3BFF}  // 50 percent window open time
  {$define _WDTCWS_WDTCWS625 = $3CFF}  // 62.5 percent window open time
  {$define _WDTCWS_WDTCWS75  = $3DFF}  // 75 percent window open time
  {$define _WDTCWS_WDTCWS100 = $3EFF}  // 100 percent window open time (Legacy WDT) 
  {$define _WDTCWS_WDTCWSSW  = $3FFF}  // Software WDT window size control (WDTWS bits)

  // WDTCCS : WDT Input Clock Selector
  {$define _WDTCCS_LFINTOSC  = $07FF}  // 31.0kHz LFINTOSC
  {$define _WDTCCS_MFINTOSC  = $0FFF}  // 31.25 kHz HFINTOSC (MFINTOSC)
  {$define _WDTCCS_SWC       = $3FFF}  // Software control, controlled by WDTCS bits

implementation
end.
