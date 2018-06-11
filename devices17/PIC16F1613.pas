unit PIC16F1613;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1613'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

interface
var
  INDF0               : byte absolute $0000;
  INDF1               : byte absolute $0001;
  PCL                 : byte absolute $0002;
  STATUS              : byte absolute $0003;
  STATUS_TO           : bit  absolute STATUS.6;
  STATUS_PD           : bit  absolute STATUS.5;
  STATUS_Z            : bit  absolute STATUS.4;
  STATUS_DC           : bit  absolute STATUS.3;
  STATUS_C            : bit  absolute STATUS.2;
  FSR0L               : byte absolute $0004;
  FSR0H               : byte absolute $0005;
  FSR1L               : byte absolute $0006;
  FSR1H               : byte absolute $0007;
  BSR                 : byte absolute $0008;
  BSR_BSR4            : bit  absolute BSR.4;
  BSR_BSR3            : bit  absolute BSR.3;
  BSR_BSR2            : bit  absolute BSR.2;
  BSR_BSR1            : bit  absolute BSR.1;
  BSR_BSR0            : bit  absolute BSR.0;
  WREG                : byte absolute $0009;
  PCLATH              : byte absolute $000A;
  INTCON              : byte absolute $000B;
  INTCON_GIE          : bit  absolute INTCON.7;
  INTCON_PEIE         : bit  absolute INTCON.6;
  INTCON_TMR0IE       : bit  absolute INTCON.5;
  INTCON_INTE         : bit  absolute INTCON.4;
  INTCON_IOCIE        : bit  absolute INTCON.3;
  INTCON_TMR0IF       : bit  absolute INTCON.2;
  INTCON_INTF         : bit  absolute INTCON.1;
  INTCON_IOCIF        : bit  absolute INTCON.0;
  PORTA               : byte absolute $000C;
  PORTA_RA5           : bit  absolute PORTA.6;
  PORTA_RA4           : bit  absolute PORTA.5;
  PORTA_RA3           : bit  absolute PORTA.4;
  PORTA_RA2           : bit  absolute PORTA.3;
  PORTA_RA1           : bit  absolute PORTA.2;
  PORTA_RA0           : bit  absolute PORTA.1;
  PORTC               : byte absolute $000E;
  PORTC_RC5           : bit  absolute PORTC.6;
  PORTC_RC4           : bit  absolute PORTC.5;
  PORTC_RC3           : bit  absolute PORTC.4;
  PORTC_RC2           : bit  absolute PORTC.3;
  PORTC_RC1           : bit  absolute PORTC.2;
  PORTC_RC0           : bit  absolute PORTC.1;
  PIR1                : byte absolute $0011;
  PIR1_TMR1GIF        : bit  absolute PIR1.5;
  PIR1_ADIF           : bit  absolute PIR1.4;
  PIR1_CCP1IF         : bit  absolute PIR1.3;
  PIR1_TMR2IF         : bit  absolute PIR1.2;
  PIR1_TMR1IF         : bit  absolute PIR1.1;
  PIR2                : byte absolute $0012;
  PIR2_C2IF           : bit  absolute PIR2.5;
  PIR2_C1IF           : bit  absolute PIR2.4;
  PIR2_TMR6IF         : bit  absolute PIR2.3;
  PIR2_TMR4IF         : bit  absolute PIR2.2;
  PIR2_CCP2IF         : bit  absolute PIR2.1;
  PIR3                : byte absolute $0013;
  PIR3_CWGIF          : bit  absolute PIR3.2;
  PIR3_ZCDIF          : bit  absolute PIR3.1;
  PIR4                : byte absolute $0014;
  PIR4_SCANIF         : bit  absolute PIR4.7;
  PIR4_CRCIF          : bit  absolute PIR4.6;
  PIR4_SMT2PWAIF      : bit  absolute PIR4.5;
  PIR4_SMT2PRAIF      : bit  absolute PIR4.4;
  PIR4_SMT2IF         : bit  absolute PIR4.3;
  PIR4_SMT1PWAIF      : bit  absolute PIR4.2;
  PIR4_SMT1PRAIF      : bit  absolute PIR4.1;
  PIR4_SMT1IF         : bit  absolute PIR4.0;
  TMR0                : byte absolute $0015;
  TMR1L               : byte absolute $0016;
  TMR1H               : byte absolute $0017;
  T1CON               : byte absolute $0018;
  T1CON_TMR1CS1       : bit  absolute T1CON.7;
  T1CON_TMR1CS0       : bit  absolute T1CON.6;
  T1CON_T1CKPS1       : bit  absolute T1CON.5;
  T1CON_T1CKPS0       : bit  absolute T1CON.4;
  T1CON_T1SYNC        : bit  absolute T1CON.2;
  T1CON_TMR1ON        : bit  absolute T1CON.1;
  T1GCON              : byte absolute $0019;
  T1GCON_TMR1GE       : bit  absolute T1GCON.7;
  T1GCON_T1GPOL       : bit  absolute T1GCON.6;
  T1GCON_T1GTM        : bit  absolute T1GCON.5;
  T1GCON_T1GSPM       : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE  : bit  absolute T1GCON.3;
  T1GCON_T1GVAL       : bit  absolute T1GCON.2;
  T1GCON_T1GSS1       : bit  absolute T1GCON.1;
  T1GCON_T1GSS0       : bit  absolute T1GCON.0;
  T2TMR               : byte absolute $001A;
  T2PR                : byte absolute $001B;
  T2CON               : byte absolute $001C;
  T2CON_ON            : bit  absolute T2CON.7;
  T2CON_T2CKPS2       : bit  absolute T2CON.6;
  T2CON_T2CKPS1       : bit  absolute T2CON.5;
  T2CON_T2CKPS0       : bit  absolute T2CON.4;
  T2CON_T2OUTPS3      : bit  absolute T2CON.3;
  T2CON_T2OUTPS2      : bit  absolute T2CON.2;
  T2CON_T2OUTPS1      : bit  absolute T2CON.1;
  T2CON_T2OUTPS0      : bit  absolute T2CON.0;
  T2HLT               : byte absolute $001D;
  T2HLT_PSYNC         : bit  absolute T2HLT.7;
  T2HLT_CKPOL         : bit  absolute T2HLT.6;
  T2HLT_CKSYNC        : bit  absolute T2HLT.5;
  T2HLT_MODE3         : bit  absolute T2HLT.3;
  T2HLT_MODE2         : bit  absolute T2HLT.2;
  T2HLT_MODE1         : bit  absolute T2HLT.1;
  T2HLT_MODE0         : bit  absolute T2HLT.0;
  T2CLKCON            : byte absolute $001E;
  T2CLKCON_T2CS2      : bit  absolute T2CLKCON.2;
  T2CLKCON_T2CS1      : bit  absolute T2CLKCON.1;
  T2CLKCON_T2CS0      : bit  absolute T2CLKCON.0;
  T2RST               : byte absolute $001F;
  T2RST_RSEL3         : bit  absolute T2RST.3;
  T2RST_RSEL2         : bit  absolute T2RST.2;
  T2RST_RSEL1         : bit  absolute T2RST.1;
  T2RST_RSEL0         : bit  absolute T2RST.0;
  TRISA               : byte absolute $008C;
  TRISA_TRISA5        : bit  absolute TRISA.6;
  TRISA_TRISA4        : bit  absolute TRISA.5;
  TRISA_TRISA3        : bit  absolute TRISA.4;
  TRISA_TRISA2        : bit  absolute TRISA.3;
  TRISA_TRISA1        : bit  absolute TRISA.2;
  TRISA_TRISA0        : bit  absolute TRISA.1;
  TRISC               : byte absolute $008E;
  TRISC_TRISC5        : bit  absolute TRISC.6;
  TRISC_TRISC4        : bit  absolute TRISC.5;
  TRISC_TRISC3        : bit  absolute TRISC.4;
  TRISC_TRISC2        : bit  absolute TRISC.3;
  TRISC_TRISC1        : bit  absolute TRISC.2;
  TRISC_TRISC0        : bit  absolute TRISC.1;
  PIE1                : byte absolute $0091;
  PIE1_TMR1GIE        : bit  absolute PIE1.5;
  PIE1_ADIE           : bit  absolute PIE1.4;
  PIE1_CCP1IE         : bit  absolute PIE1.3;
  PIE1_TMR2IE         : bit  absolute PIE1.2;
  PIE1_TMR1IE         : bit  absolute PIE1.1;
  PIE2                : byte absolute $0092;
  PIE2_C2IE           : bit  absolute PIE2.5;
  PIE2_C1IE           : bit  absolute PIE2.4;
  PIE2_TMR6IE         : bit  absolute PIE2.3;
  PIE2_TMR4IE         : bit  absolute PIE2.2;
  PIE2_CCP2IE         : bit  absolute PIE2.1;
  PIE3                : byte absolute $0093;
  PIE3_CWGIE          : bit  absolute PIE3.2;
  PIE3_ZCDIE          : bit  absolute PIE3.1;
  PIE4                : byte absolute $0094;
  PIE4_SCANIE         : bit  absolute PIE4.7;
  PIE4_CRCIE          : bit  absolute PIE4.6;
  PIE4_SMT2PWAIE      : bit  absolute PIE4.5;
  PIE4_SMT2PRAIE      : bit  absolute PIE4.4;
  PIE4_SMT2IE         : bit  absolute PIE4.3;
  PIE4_SMT1PWAIE      : bit  absolute PIE4.2;
  PIE4_SMT1PRAIE      : bit  absolute PIE4.1;
  PIE4_SMT1IE         : bit  absolute PIE4.0;
  OPTION_REG          : byte absolute $0095;
  OPTION_REG_WPUEN    : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG   : bit  absolute OPTION_REG.6;
  OPTION_REG_TMR0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_TMR0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA      : bit  absolute OPTION_REG.3;
  OPTION_REG_PS1      : bit  absolute OPTION_REG.2;
  OPTION_REG_PS0      : bit  absolute OPTION_REG.1;
  PCON                : byte absolute $0096;
  PCON_STKOVF         : bit  absolute PCON.7;
  PCON_STKUNF         : bit  absolute PCON.6;
  PCON_WDTWV          : bit  absolute PCON.5;
  PCON_RWDT           : bit  absolute PCON.4;
  PCON_RMCLR          : bit  absolute PCON.3;
  PCON_RI             : bit  absolute PCON.2;
  PCON_POR            : bit  absolute PCON.1;
  PCON_BOR            : bit  absolute PCON.0;
  OSCTUNE             : byte absolute $0098;
  OSCTUNE_TUN5        : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4        : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3        : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2        : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1        : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0        : bit  absolute OSCTUNE.0;
  OSCCON              : byte absolute $0099;
  OSCCON_SPLLEN       : bit  absolute OSCCON.7;
  OSCCON_IRCF3        : bit  absolute OSCCON.6;
  OSCCON_IRCF2        : bit  absolute OSCCON.5;
  OSCCON_IRCF1        : bit  absolute OSCCON.4;
  OSCCON_IRCF0        : bit  absolute OSCCON.3;
  OSCCON_SCS1         : bit  absolute OSCCON.2;
  OSCCON_SCS0         : bit  absolute OSCCON.1;
  OSCSTAT             : byte absolute $009A;
  OSCSTAT_PLLR        : bit  absolute OSCSTAT.6;
  OSCSTAT_HFIOFR      : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFL      : bit  absolute OSCSTAT.4;
  OSCSTAT_MFIOFR      : bit  absolute OSCSTAT.3;
  OSCSTAT_LFIOFR      : bit  absolute OSCSTAT.2;
  OSCSTAT_HFIOFS      : bit  absolute OSCSTAT.1;
  ADRESL              : byte absolute $009B;
  ADRESH              : byte absolute $009C;
  ADCON0              : byte absolute $009D;
  ADCON0_CHS4         : bit  absolute ADCON0.6;
  ADCON0_CHS3         : bit  absolute ADCON0.5;
  ADCON0_CHS2         : bit  absolute ADCON0.4;
  ADCON0_CHS1         : bit  absolute ADCON0.3;
  ADCON0_GO_nDONE     : bit  absolute ADCON0.2;
  ADCON0_ADON         : bit  absolute ADCON0.1;
  ADCON1              : byte absolute $009E;
  ADCON1_ADFM         : bit  absolute ADCON1.6;
  ADCON1_ADCS1        : bit  absolute ADCON1.5;
  ADCON1_ADCS0        : bit  absolute ADCON1.4;
  ADCON1_ADPREF1      : bit  absolute ADCON1.3;
  ADCON1_ADPREF0      : bit  absolute ADCON1.2;
  ADCON2              : byte absolute $009F;
  ADCON2_TRIGSEL3     : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL2     : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1     : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0     : bit  absolute ADCON2.4;
  LATA                : byte absolute $010C;
  LATA_LATA5          : bit  absolute LATA.6;
  LATA_LATA4          : bit  absolute LATA.5;
  LATA_LATA3          : bit  absolute LATA.4;
  LATA_LATA2          : bit  absolute LATA.3;
  LATA_LATA1          : bit  absolute LATA.2;
  LATA_LATA0          : bit  absolute LATA.1;
  LATC                : byte absolute $010E;
  LATC_LATC5          : bit  absolute LATC.6;
  LATC_LATC4          : bit  absolute LATC.5;
  LATC_LATC3          : bit  absolute LATC.4;
  LATC_LATC2          : bit  absolute LATC.3;
  LATC_LATC1          : bit  absolute LATC.2;
  LATC_LATC0          : bit  absolute LATC.1;
  CM1CON0             : byte absolute $0111;
  CM1CON0_C1ON        : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT       : bit  absolute CM1CON0.6;
  CM1CON0_C1OE        : bit  absolute CM1CON0.5;
  CM1CON0_C1POL       : bit  absolute CM1CON0.4;
  CM1CON0_C1SP        : bit  absolute CM1CON0.3;
  CM1CON0_C1HYS       : bit  absolute CM1CON0.2;
  CM1CON0_C1SYNC      : bit  absolute CM1CON0.1;
  CM1CON1             : byte absolute $0112;
  CM1CON1_C1INTP      : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN      : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH1      : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH0      : bit  absolute CM1CON1.4;
  CM1CON1_C1NCH2      : bit  absolute CM1CON1.3;
  CM1CON1_C1NCH1      : bit  absolute CM1CON1.2;
  CM1CON1_C1NCH0      : bit  absolute CM1CON1.1;
  CM2CON0             : byte absolute $0113;
  CM2CON0_C2ON        : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT       : bit  absolute CM2CON0.6;
  CM2CON0_C2OE        : bit  absolute CM2CON0.5;
  CM2CON0_C2POL       : bit  absolute CM2CON0.4;
  CM2CON0_C2SP        : bit  absolute CM2CON0.3;
  CM2CON0_C2HYS       : bit  absolute CM2CON0.2;
  CM2CON0_C2SYNC      : bit  absolute CM2CON0.1;
  CM2CON1             : byte absolute $0114;
  CM2CON1_C2INTP      : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN      : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH1      : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH0      : bit  absolute CM2CON1.4;
  CM2CON1_C2NCH2      : bit  absolute CM2CON1.3;
  CM2CON1_C2NCH1      : bit  absolute CM2CON1.2;
  CM2CON1_C2NCH0      : bit  absolute CM2CON1.1;
  CMOUT               : byte absolute $0115;
  CMOUT_MC2OUT        : bit  absolute CMOUT.1;
  CMOUT_MC1OUT        : bit  absolute CMOUT.0;
  BORCON              : byte absolute $0116;
  BORCON_SBOREN       : bit  absolute BORCON.7;
  BORCON_BORFS        : bit  absolute BORCON.6;
  BORCON_BORRDY       : bit  absolute BORCON.5;
  FVRCON              : byte absolute $0117;
  FVRCON_FVREN        : bit  absolute FVRCON.7;
  FVRCON_FVRRDY       : bit  absolute FVRCON.6;
  FVRCON_TSEN         : bit  absolute FVRCON.5;
  FVRCON_TSRNG        : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1      : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0      : bit  absolute FVRCON.2;
  FVRCON_ADFVR1       : bit  absolute FVRCON.1;
  FVRCON_ADFVR0       : bit  absolute FVRCON.0;
  DAC1CON0            : byte absolute $0118;
  DAC1CON0_DAC1EN     : bit  absolute DAC1CON0.4;
  DAC1CON0_DAC1OE     : bit  absolute DAC1CON0.3;
  DAC1CON0_D1PSS0     : bit  absolute DAC1CON0.2;
  DAC1CON1            : byte absolute $0119;
  DAC1CON1_DAC1R7     : bit  absolute DAC1CON1.7;
  DAC1CON1_DAC1R6     : bit  absolute DAC1CON1.6;
  DAC1CON1_DAC1R5     : bit  absolute DAC1CON1.5;
  DAC1CON1_DAC1R4     : bit  absolute DAC1CON1.4;
  DAC1CON1_DAC1R3     : bit  absolute DAC1CON1.3;
  DAC1CON1_DAC1R2     : bit  absolute DAC1CON1.2;
  DAC1CON1_DAC1R1     : bit  absolute DAC1CON1.1;
  DAC1CON1_DAC1R0     : bit  absolute DAC1CON1.0;
  ZCD1CON             : byte absolute $011C;
  ZCD1CON_ZCD1EN      : bit  absolute ZCD1CON.7;
  ZCD1CON_ZCD1OE      : bit  absolute ZCD1CON.6;
  ZCD1CON_ZCD1OUT     : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL     : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP    : bit  absolute ZCD1CON.3;
  ZCD1CON_ZCD1INTN    : bit  absolute ZCD1CON.2;
  APFCON              : byte absolute $011D;
  APFCON_T1GSEL       : bit  absolute APFCON.2;
  APFCON_CCP2SEL      : bit  absolute APFCON.1;
  ANSELA              : byte absolute $018C;
  ANSELA_ANSA4        : bit  absolute ANSELA.4;
  ANSELA_ANSA2        : bit  absolute ANSELA.3;
  ANSELA_ANSA1        : bit  absolute ANSELA.2;
  ANSELA_ANSA0        : bit  absolute ANSELA.1;
  ANSELC              : byte absolute $018E;
  ANSELC_ANSC3        : bit  absolute ANSELC.4;
  ANSELC_ANSC2        : bit  absolute ANSELC.3;
  ANSELC_ANSC1        : bit  absolute ANSELC.2;
  ANSELC_ANSC0        : bit  absolute ANSELC.1;
  PMADRL              : byte absolute $0191;
  PMADRH              : byte absolute $0192;
  PMDATL              : byte absolute $0193;
  PMDATH              : byte absolute $0194;
  PMCON1              : byte absolute $0195;
  PMCON1_CFGS         : bit  absolute PMCON1.7;
  PMCON1_LWLO         : bit  absolute PMCON1.6;
  PMCON1_FREE         : bit  absolute PMCON1.5;
  PMCON1_WRERR        : bit  absolute PMCON1.4;
  PMCON1_WREN         : bit  absolute PMCON1.3;
  PMCON1_WR           : bit  absolute PMCON1.2;
  PMCON1_RD           : bit  absolute PMCON1.1;
  PMCON2              : byte absolute $0196;
  VREGCON             : byte absolute $0197;
  VREGCON_VREGPM1     : bit  absolute VREGCON.1;
  VREGCON_VREGPM0     : bit  absolute VREGCON.0;
  WPUA                : byte absolute $020C;
  WPUA_WPUA5          : bit  absolute WPUA.5;
  WPUA_WPUA4          : bit  absolute WPUA.4;
  WPUA_WPUA3          : bit  absolute WPUA.3;
  WPUA_WPUA2          : bit  absolute WPUA.2;
  WPUA_WPUA1          : bit  absolute WPUA.1;
  WPUA_WPUA0          : bit  absolute WPUA.0;
  WPUC                : byte absolute $020E;
  WPUC_WPUC5          : bit  absolute WPUC.5;
  WPUC_WPUC4          : bit  absolute WPUC.4;
  WPUC_WPUC3          : bit  absolute WPUC.3;
  WPUC_WPUC2          : bit  absolute WPUC.2;
  WPUC_WPUC1          : bit  absolute WPUC.1;
  WPUC_WPUC0          : bit  absolute WPUC.0;
  ODCONA              : byte absolute $028C;
  ODCONA_ODA5         : bit  absolute ODCONA.5;
  ODCONA_ODA4         : bit  absolute ODCONA.4;
  ODCONA_ODA2         : bit  absolute ODCONA.3;
  ODCONA_ODA1         : bit  absolute ODCONA.2;
  ODCONA_ODA0         : bit  absolute ODCONA.1;
  ODCONC              : byte absolute $028E;
  ODCONC_ODC5         : bit  absolute ODCONC.5;
  ODCONC_ODC4         : bit  absolute ODCONC.4;
  ODCONC_ODC3         : bit  absolute ODCONC.3;
  ODCONC_ODC2         : bit  absolute ODCONC.2;
  ODCONC_ODC1         : bit  absolute ODCONC.1;
  ODCONC_ODC0         : bit  absolute ODCONC.0;
  CCPR1L              : byte absolute $0291;
  CCPR1H              : byte absolute $0292;
  CCP1CON             : byte absolute $0293;
  CCP1CON_EN          : bit  absolute CCP1CON.7;
  CCP1CON_OE          : bit  absolute CCP1CON.6;
  CCP1CON_OUT         : bit  absolute CCP1CON.5;
  CCP1CON_FMT         : bit  absolute CCP1CON.4;
  CCP1CAP             : byte absolute $0294;
  CCP1CAP_CTS1        : bit  absolute CCP1CAP.2;
  CCP1CAP_CTS0        : bit  absolute CCP1CAP.1;
  CCPR2L              : byte absolute $0298;
  CCPR2H              : byte absolute $0299;
  CCP2CON             : byte absolute $029A;
  CCP2CAP             : byte absolute $029B;
  CCPTMRS             : byte absolute $029E;
  CCPTMRS_CCP2TSEL1   : bit  absolute CCPTMRS.3;
  CCPTMRS_CCP2TSEL0   : bit  absolute CCPTMRS.2;
  CCPTMRS_CCP1TSEL1   : bit  absolute CCPTMRS.1;
  CCPTMRS_CCP1TSEL0   : bit  absolute CCPTMRS.0;
  SLRCONA             : byte absolute $030C;
  SLRCONA_SLRA5       : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4       : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2       : bit  absolute SLRCONA.3;
  SLRCONA_SLRA1       : bit  absolute SLRCONA.2;
  SLRCONA_SLRA0       : bit  absolute SLRCONA.1;
  SLRCONC             : byte absolute $030E;
  SLRCONC_SLRC5       : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4       : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3       : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2       : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1       : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0       : bit  absolute SLRCONC.0;
  INLVLA              : byte absolute $038C;
  INLVLA_INLVLA5      : bit  absolute INLVLA.5;
  INLVLA_INLVLA4      : bit  absolute INLVLA.4;
  INLVLA_INLVLA3      : bit  absolute INLVLA.3;
  INLVLA_INLVLA2      : bit  absolute INLVLA.2;
  INLVLA_INLVLA1      : bit  absolute INLVLA.1;
  INLVLA_INLVLA0      : bit  absolute INLVLA.0;
  INLVLC              : byte absolute $038E;
  INLVLC_INLVLC5      : bit  absolute INLVLC.5;
  INLVLC_INLVLC4      : bit  absolute INLVLC.4;
  INLVLC_INLVLC3      : bit  absolute INLVLC.3;
  INLVLC_INLVLC2      : bit  absolute INLVLC.2;
  INLVLC_INLVLC1      : bit  absolute INLVLC.1;
  INLVLC_INLVLC0      : bit  absolute INLVLC.0;
  IOCAP               : byte absolute $0391;
  IOCAP_IOCAP5        : bit  absolute IOCAP.5;
  IOCAP_IOCAP4        : bit  absolute IOCAP.4;
  IOCAP_IOCAP3        : bit  absolute IOCAP.3;
  IOCAP_IOCAP2        : bit  absolute IOCAP.2;
  IOCAP_IOCAP1        : bit  absolute IOCAP.1;
  IOCAP_IOCAP0        : bit  absolute IOCAP.0;
  IOCAN               : byte absolute $0392;
  IOCAN_IOCAN5        : bit  absolute IOCAN.5;
  IOCAN_IOCAN4        : bit  absolute IOCAN.4;
  IOCAN_IOCAN3        : bit  absolute IOCAN.3;
  IOCAN_IOCAN2        : bit  absolute IOCAN.2;
  IOCAN_IOCAN1        : bit  absolute IOCAN.1;
  IOCAN_IOCAN0        : bit  absolute IOCAN.0;
  IOCAF               : byte absolute $0393;
  IOCAF_IOCAF5        : bit  absolute IOCAF.5;
  IOCAF_IOCAF4        : bit  absolute IOCAF.4;
  IOCAF_IOCAF3        : bit  absolute IOCAF.3;
  IOCAF_IOCAF2        : bit  absolute IOCAF.2;
  IOCAF_IOCAF1        : bit  absolute IOCAF.1;
  IOCAF_IOCAF0        : bit  absolute IOCAF.0;
  IOCCP               : byte absolute $0397;
  IOCCP_IOCCP5        : bit  absolute IOCCP.5;
  IOCCP_IOCCP4        : bit  absolute IOCCP.4;
  IOCCP_IOCCP3        : bit  absolute IOCCP.3;
  IOCCP_IOCCP2        : bit  absolute IOCCP.2;
  IOCCP_IOCCP1        : bit  absolute IOCCP.1;
  IOCCP_IOCCP0        : bit  absolute IOCCP.0;
  IOCCN               : byte absolute $0398;
  IOCCN_IOCCN5        : bit  absolute IOCCN.5;
  IOCCN_IOCCN4        : bit  absolute IOCCN.4;
  IOCCN_IOCCN3        : bit  absolute IOCCN.3;
  IOCCN_IOCCN2        : bit  absolute IOCCN.2;
  IOCCN_IOCCN1        : bit  absolute IOCCN.1;
  IOCCN_IOCCN0        : bit  absolute IOCCN.0;
  IOCCF               : byte absolute $0399;
  IOCCF_IOCCF5        : bit  absolute IOCCF.5;
  IOCCF_IOCCF4        : bit  absolute IOCCF.4;
  IOCCF_IOCCF3        : bit  absolute IOCCF.3;
  IOCCF_IOCCF2        : bit  absolute IOCCF.2;
  IOCCF_IOCCF1        : bit  absolute IOCCF.1;
  IOCCF_IOCCF0        : bit  absolute IOCCF.0;
  T4TMR               : byte absolute $0413;
  T4PR                : byte absolute $0414;
  T4CON               : byte absolute $0415;
  T4CON_T4CKPS2       : bit  absolute T4CON.6;
  T4CON_T4CKPS1       : bit  absolute T4CON.5;
  T4CON_T4CKPS0       : bit  absolute T4CON.4;
  T4CON_T4OUTPS3      : bit  absolute T4CON.3;
  T4CON_T4OUTPS2      : bit  absolute T4CON.2;
  T4CON_T4OUTPS1      : bit  absolute T4CON.1;
  T4CON_T4OUTPS0      : bit  absolute T4CON.0;
  T4HLT               : byte absolute $0416;
  T4CLKCON            : byte absolute $0417;
  T4CLKCON_T4CS2      : bit  absolute T4CLKCON.2;
  T4CLKCON_T4CS1      : bit  absolute T4CLKCON.1;
  T4CLKCON_T4CS0      : bit  absolute T4CLKCON.0;
  T4RST               : byte absolute $0418;
  T6TMR               : byte absolute $041A;
  T6PR                : byte absolute $041B;
  T6CON               : byte absolute $041C;
  T6CON_T6CKPS2       : bit  absolute T6CON.6;
  T6CON_T6CKPS1       : bit  absolute T6CON.5;
  T6CON_T6CKPS0       : bit  absolute T6CON.4;
  T6CON_T6OUTPS3      : bit  absolute T6CON.3;
  T6CON_T6OUTPS2      : bit  absolute T6CON.2;
  T6CON_T6OUTPS1      : bit  absolute T6CON.1;
  T6CON_T6OUTPS0      : bit  absolute T6CON.0;
  T6HLT               : byte absolute $041D;
  T6CLKCON            : byte absolute $041E;
  T6CLKCON_T6CS2      : bit  absolute T6CLKCON.2;
  T6CLKCON_T6CS1      : bit  absolute T6CLKCON.1;
  T6CLKCON_T6CS0      : bit  absolute T6CLKCON.0;
  T6RST               : byte absolute $041F;
  CWG1DBR             : byte absolute $0691;
  CWG1DBR_DBR5        : bit  absolute CWG1DBR.5;
  CWG1DBR_DBR4        : bit  absolute CWG1DBR.4;
  CWG1DBR_DBR3        : bit  absolute CWG1DBR.3;
  CWG1DBR_DBR2        : bit  absolute CWG1DBR.2;
  CWG1DBR_DBR1        : bit  absolute CWG1DBR.1;
  CWG1DBR_DBR0        : bit  absolute CWG1DBR.0;
  CWG1DBF             : byte absolute $0692;
  CWG1DBF_DBF5        : bit  absolute CWG1DBF.5;
  CWG1DBF_DBF4        : bit  absolute CWG1DBF.4;
  CWG1DBF_DBF3        : bit  absolute CWG1DBF.3;
  CWG1DBF_DBF2        : bit  absolute CWG1DBF.2;
  CWG1DBF_DBF1        : bit  absolute CWG1DBF.1;
  CWG1DBF_DBF0        : bit  absolute CWG1DBF.0;
  CWG1AS0             : byte absolute $0693;
  CWG1AS0_SHUTDOWN    : bit  absolute CWG1AS0.7;
  CWG1AS0_REN         : bit  absolute CWG1AS0.6;
  CWG1AS0_LSBD1       : bit  absolute CWG1AS0.4;
  CWG1AS0_LSBD0       : bit  absolute CWG1AS0.3;
  CWG1AS0_LSAC1       : bit  absolute CWG1AS0.2;
  CWG1AS0_LSAC0       : bit  absolute CWG1AS0.1;
  CWG1AS1             : byte absolute $0694;
  CWG1AS1_TMR6AS      : bit  absolute CWG1AS1.6;
  CWG1AS1_TMR4AS      : bit  absolute CWG1AS1.5;
  CWG1AS1_TMR2AS      : bit  absolute CWG1AS1.4;
  CWG1AS1_C2AS        : bit  absolute CWG1AS1.3;
  CWG1AS1_C1AS        : bit  absolute CWG1AS1.2;
  CWG1AS1_INAS        : bit  absolute CWG1AS1.1;
  CWG1OCON0           : byte absolute $0695;
  CWG1OCON0_OVRD      : bit  absolute CWG1OCON0.7;
  CWG1OCON0_OVRC      : bit  absolute CWG1OCON0.6;
  CWG1OCON0_OVRB      : bit  absolute CWG1OCON0.5;
  CWG1OCON0_OVRA      : bit  absolute CWG1OCON0.4;
  CWG1OCON0_STRD      : bit  absolute CWG1OCON0.3;
  CWG1OCON0_STRC      : bit  absolute CWG1OCON0.2;
  CWG1OCON0_STRB      : bit  absolute CWG1OCON0.1;
  CWG1OCON0_STRA      : bit  absolute CWG1OCON0.0;
  CWG1CON0            : byte absolute $0696;
  CWG1CON0_LD         : bit  absolute CWG1CON0.6;
  CWG1CON0_CWG1MODE2  : bit  absolute CWG1CON0.2;
  CWG1CON0_CWG1MODE1  : bit  absolute CWG1CON0.1;
  CWG1CON0_CWG1MODE0  : bit  absolute CWG1CON0.0;
  CWG1CON1            : byte absolute $0697;
  CWG1CON1_IN         : bit  absolute CWG1CON1.5;
  CWG1CON1_POLD       : bit  absolute CWG1CON1.4;
  CWG1CON1_POLC       : bit  absolute CWG1CON1.3;
  CWG1CON1_POLB       : bit  absolute CWG1CON1.2;
  CWG1CON1_POLA       : bit  absolute CWG1CON1.1;
  CWG1OCON1           : byte absolute $0698;
  CWG1OCON1_OED       : bit  absolute CWG1OCON1.5;
  CWG1OCON1_OEC       : bit  absolute CWG1OCON1.4;
  CWG1OCON1_OEB       : bit  absolute CWG1OCON1.3;
  CWG1OCON1_OEA       : bit  absolute CWG1OCON1.2;
  CWG1CLKCON          : byte absolute $0699;
  CWG1CLKCON_CS       : bit  absolute CWG1CLKCON.1;
  CWG1ISM             : byte absolute $069A;
  CWG1ISM_IS2         : bit  absolute CWG1ISM.3;
  CWG1ISM_IS1         : bit  absolute CWG1ISM.2;
  CWG1ISM_IS0         : bit  absolute CWG1ISM.1;
  WDTCON0             : byte absolute $0711;
  WDTCON0_WDTSEN      : bit  absolute WDTCON0.5;
  WDTCON0_WDTPS3      : bit  absolute WDTCON0.4;
  WDTCON0_WDTPS2      : bit  absolute WDTCON0.3;
  WDTCON0_SEN         : bit  absolute WDTCON0.2;
  WDTCON0_SWDTEN      : bit  absolute WDTCON0.1;
  WDTCON1             : byte absolute $0712;
  WDTCON1_WDTCS2      : bit  absolute WDTCON1.6;
  WDTCON1_WDTCS1      : bit  absolute WDTCON1.5;
  WDTCON1_WDTCS0      : bit  absolute WDTCON1.4;
  WDTCON1_WINDOW2     : bit  absolute WDTCON1.3;
  WDTCON1_WINDOW1     : bit  absolute WDTCON1.2;
  WDTCON1_WINDOW0     : bit  absolute WDTCON1.1;
  WDTPSL              : byte absolute $0713;
  WDTPSL_PSCNT7       : bit  absolute WDTPSL.7;
  WDTPSL_PSCNT6       : bit  absolute WDTPSL.6;
  WDTPSL_PSCNT5       : bit  absolute WDTPSL.5;
  WDTPSL_PSCNT4       : bit  absolute WDTPSL.4;
  WDTPSL_PSCNT3       : bit  absolute WDTPSL.3;
  WDTPSL_PSCNT2       : bit  absolute WDTPSL.2;
  WDTPSL_PSCNT1       : bit  absolute WDTPSL.1;
  WDTPSL_PSCNT0       : bit  absolute WDTPSL.0;
  WDTPSH              : byte absolute $0714;
  WDTPSH_PSCNT15      : bit  absolute WDTPSH.7;
  WDTPSH_PSCNT14      : bit  absolute WDTPSH.6;
  WDTPSH_PSCNT13      : bit  absolute WDTPSH.5;
  WDTPSH_PSCNT12      : bit  absolute WDTPSH.4;
  WDTPSH_PSCNT11      : bit  absolute WDTPSH.3;
  WDTPSH_PSCNT10      : bit  absolute WDTPSH.2;
  WDTPSH_PSCNT9       : bit  absolute WDTPSH.1;
  WDTPSH_PSCNT8       : bit  absolute WDTPSH.0;
  WDTTMR              : byte absolute $0715;
  WDTTMR_WDTTMR4      : bit  absolute WDTTMR.7;
  WDTTMR_WDTTMR3      : bit  absolute WDTTMR.6;
  WDTTMR_WDTTMR2      : bit  absolute WDTTMR.5;
  WDTTMR_WDTTMR1      : bit  absolute WDTTMR.4;
  WDTTMR_WDTTMR0      : bit  absolute WDTTMR.3;
  WDTTMR_STATE        : bit  absolute WDTTMR.2;
  WDTTMR_PSCNT17      : bit  absolute WDTTMR.1;
  WDTTMR_PSCNT16      : bit  absolute WDTTMR.0;
  SCANLADRL           : byte absolute $0718;
  SCANLADRL_LADR7     : bit  absolute SCANLADRL.7;
  SCANLADRL_LADR6     : bit  absolute SCANLADRL.6;
  SCANLADRL_LADR5     : bit  absolute SCANLADRL.5;
  SCANLADRL_LADR4     : bit  absolute SCANLADRL.4;
  SCANLADRL_LADR3     : bit  absolute SCANLADRL.3;
  SCANLADRL_LADR2     : bit  absolute SCANLADRL.2;
  SCANLADRL_LDAR1     : bit  absolute SCANLADRL.1;
  SCANLADRL_LDAR0     : bit  absolute SCANLADRL.0;
  SCANLADRH           : byte absolute $0719;
  SCANLADRH_LADR15    : bit  absolute SCANLADRH.7;
  SCANLADRH_LADR14    : bit  absolute SCANLADRH.6;
  SCANLADRH_LADR13    : bit  absolute SCANLADRH.5;
  SCANLADRH_LADR12    : bit  absolute SCANLADRH.4;
  SCANLADRH_LADR11    : bit  absolute SCANLADRH.3;
  SCANLADRH_LADR10    : bit  absolute SCANLADRH.2;
  SCANLADRH_LADR9     : bit  absolute SCANLADRH.1;
  SCANLADRH_LADR8     : bit  absolute SCANLADRH.0;
  SCANHADRL           : byte absolute $071A;
  SCANHADRL_HADR7     : bit  absolute SCANHADRL.7;
  SCANHADRL_HADR6     : bit  absolute SCANHADRL.6;
  SCANHADRL_HADR5     : bit  absolute SCANHADRL.5;
  SCANHADRL_HADR4     : bit  absolute SCANHADRL.4;
  SCANHADRL_HADR3     : bit  absolute SCANHADRL.3;
  SCANHADRL_HARD2     : bit  absolute SCANHADRL.2;
  SCANHADRL_HADR1     : bit  absolute SCANHADRL.1;
  SCANHADRL_HADR0     : bit  absolute SCANHADRL.0;
  SCANHADRH           : byte absolute $071B;
  SCANHADRH_HADR15    : bit  absolute SCANHADRH.7;
  SCANHADRH_HADR14    : bit  absolute SCANHADRH.6;
  SCANHADRH_HADR13    : bit  absolute SCANHADRH.5;
  SCANHADRH_HADR12    : bit  absolute SCANHADRH.4;
  SCANHADRH_HADR11    : bit  absolute SCANHADRH.3;
  SCANHADRH_HADR10    : bit  absolute SCANHADRH.2;
  SCANHADRH_HADR9     : bit  absolute SCANHADRH.1;
  SCANHADRH_HADR8     : bit  absolute SCANHADRH.0;
  SCANCON0            : byte absolute $071C;
  SCANCON0_SCANGO     : bit  absolute SCANCON0.6;
  SCANCON0_BUSY       : bit  absolute SCANCON0.5;
  SCANCON0_INVALID    : bit  absolute SCANCON0.4;
  SCANCON0_INTM       : bit  absolute SCANCON0.3;
  SCANCON0_SCANMODE0  : bit  absolute SCANCON0.0;
  SCANTRIG            : byte absolute $071D;
  SCANTRIG_TSEL1      : bit  absolute SCANTRIG.2;
  SCANTRIG_TSEL0      : bit  absolute SCANTRIG.1;
  CRCDATL             : byte absolute $0791;
  CRCDATL_DAT7        : bit  absolute CRCDATL.7;
  CRCDATL_DAT6        : bit  absolute CRCDATL.6;
  CRCDATL_DAT5        : bit  absolute CRCDATL.5;
  CRCDATL_DAT4        : bit  absolute CRCDATL.4;
  CRCDATL_DAT3        : bit  absolute CRCDATL.3;
  CRCDATL_DAT2        : bit  absolute CRCDATL.2;
  CRCDATL_DAT1        : bit  absolute CRCDATL.1;
  CRCDATL_DAT0        : bit  absolute CRCDATL.0;
  CRCDATH             : byte absolute $0792;
  CRCDATH_DAT15       : bit  absolute CRCDATH.7;
  CRCDATH_DAT14       : bit  absolute CRCDATH.6;
  CRCDATH_DAT13       : bit  absolute CRCDATH.5;
  CRCDATH_DAT12       : bit  absolute CRCDATH.4;
  CRCDATH_DAT11       : bit  absolute CRCDATH.3;
  CRCDATH_DAT10       : bit  absolute CRCDATH.2;
  CRCDATH_DAT9        : bit  absolute CRCDATH.1;
  CRCDATH_DAT8        : bit  absolute CRCDATH.0;
  CRCACCL             : byte absolute $0793;
  CRCACCL_ACC7        : bit  absolute CRCACCL.7;
  CRCACCL_ACC6        : bit  absolute CRCACCL.6;
  CRCACCL_ACC5        : bit  absolute CRCACCL.5;
  CRCACCL_ACC4        : bit  absolute CRCACCL.4;
  CRCACCL_ACC3        : bit  absolute CRCACCL.3;
  CRCACCL_ACC2        : bit  absolute CRCACCL.2;
  CRCACCL_ACC1        : bit  absolute CRCACCL.1;
  CRCACCL_ACC0        : bit  absolute CRCACCL.0;
  CRCACCH             : byte absolute $0794;
  CRCACCH_ACC15       : bit  absolute CRCACCH.7;
  CRCACCH_ACC14       : bit  absolute CRCACCH.6;
  CRCACCH_ACC13       : bit  absolute CRCACCH.5;
  CRCACCH_ACC12       : bit  absolute CRCACCH.4;
  CRCACCH_ACC11       : bit  absolute CRCACCH.3;
  CRCACCH_ACC10       : bit  absolute CRCACCH.2;
  CRCACCH_ACC9        : bit  absolute CRCACCH.1;
  CRCACCH_ACC8        : bit  absolute CRCACCH.0;
  CRCSHIFTL           : byte absolute $0795;
  CRCSHIFTL_SHIFT7    : bit  absolute CRCSHIFTL.7;
  CRCSHIFTL_SHIFT6    : bit  absolute CRCSHIFTL.6;
  CRCSHIFTL_SHIFT5    : bit  absolute CRCSHIFTL.5;
  CRCSHIFTL_SHIFT4    : bit  absolute CRCSHIFTL.4;
  CRCSHIFTL_SHIFT3    : bit  absolute CRCSHIFTL.3;
  CRCSHIFTL_SHIFT2    : bit  absolute CRCSHIFTL.2;
  CRCSHIFTL_SHIFT1    : bit  absolute CRCSHIFTL.1;
  CRCSHIFTL_SHIFT0    : bit  absolute CRCSHIFTL.0;
  CRCSHIFTH           : byte absolute $0796;
  CRCSHIFTH_SHIFT15   : bit  absolute CRCSHIFTH.7;
  CRCSHIFTH_SHIFT14   : bit  absolute CRCSHIFTH.6;
  CRCSHIFTH_SHIFT13   : bit  absolute CRCSHIFTH.5;
  CRCSHIFTH_SHIFT12   : bit  absolute CRCSHIFTH.4;
  CRCSHIFTH_SHIFT11   : bit  absolute CRCSHIFTH.3;
  CRCSHIFTH_SHIFT10   : bit  absolute CRCSHIFTH.2;
  CRCSHIFTH_SHIFT9    : bit  absolute CRCSHIFTH.1;
  CRCSHIFTH_SHIFT8    : bit  absolute CRCSHIFTH.0;
  CRCXORL             : byte absolute $0797;
  CRCXORL_XOR7        : bit  absolute CRCXORL.7;
  CRCXORL_XOR6        : bit  absolute CRCXORL.6;
  CRCXORL_XOR5        : bit  absolute CRCXORL.5;
  CRCXORL_XOR4        : bit  absolute CRCXORL.4;
  CRCXORL_XOR3        : bit  absolute CRCXORL.3;
  CRCXORL_XOR2        : bit  absolute CRCXORL.2;
  CRCXORL_XOR1        : bit  absolute CRCXORL.1;
  CRCXORH             : byte absolute $0798;
  CRCXORH_XOR15       : bit  absolute CRCXORH.7;
  CRCXORH_XOR14       : bit  absolute CRCXORH.6;
  CRCXORH_XOR13       : bit  absolute CRCXORH.5;
  CRCXORH_XOR12       : bit  absolute CRCXORH.4;
  CRCXORH_XOR11       : bit  absolute CRCXORH.3;
  CRCXORH_XOR10       : bit  absolute CRCXORH.2;
  CRCXORH_XOR9        : bit  absolute CRCXORH.1;
  CRCXORH_XOR8        : bit  absolute CRCXORH.0;
  CRCCON0             : byte absolute $0799;
  CRCCON0_CRCGO       : bit  absolute CRCCON0.5;
  CRCCON0_ACCM        : bit  absolute CRCCON0.3;
  CRCCON0_SHIFTM      : bit  absolute CRCCON0.2;
  CRCCON0_FULL        : bit  absolute CRCCON0.1;
  CRCCON1             : byte absolute $079A;
  CRCCON1_DLEN3       : bit  absolute CRCCON1.7;
  CRCCON1_DLEN2       : bit  absolute CRCCON1.6;
  CRCCON1_DLEN1       : bit  absolute CRCCON1.5;
  CRCCON1_DLEN0       : bit  absolute CRCCON1.4;
  CRCCON1_PLEN3       : bit  absolute CRCCON1.3;
  CRCCON1_PLEN2       : bit  absolute CRCCON1.2;
  CRCCON1_PLEN1       : bit  absolute CRCCON1.1;
  CRCCON1_PLEN0       : bit  absolute CRCCON1.0;
  SMT1TMRL            : byte absolute $0D8C;
  SMT1TMRL_SMT1TMR7   : bit  absolute SMT1TMRL.7;
  SMT1TMRL_SMT1TMR6   : bit  absolute SMT1TMRL.6;
  SMT1TMRL_SMT1TMR5   : bit  absolute SMT1TMRL.5;
  SMT1TMRL_SMT1TMR4   : bit  absolute SMT1TMRL.4;
  SMT1TMRL_SMT1TMR3   : bit  absolute SMT1TMRL.3;
  SMT1TMRL_SMT1TMR2   : bit  absolute SMT1TMRL.2;
  SMT1TMRL_SMT1TMR1   : bit  absolute SMT1TMRL.1;
  SMT1TMRL_SMT1TMR0   : bit  absolute SMT1TMRL.0;
  SMT1TMRH            : byte absolute $0D8D;
  SMT1TMRH_SMT1TMR15  : bit  absolute SMT1TMRH.7;
  SMT1TMRH_SMT1TMR14  : bit  absolute SMT1TMRH.6;
  SMT1TMRH_SMT1TMR13  : bit  absolute SMT1TMRH.5;
  SMT1TMRH_SMT1TMR12  : bit  absolute SMT1TMRH.4;
  SMT1TMRH_SMT1TMR11  : bit  absolute SMT1TMRH.3;
  SMT1TMRH_SMT1TMR10  : bit  absolute SMT1TMRH.2;
  SMT1TMRH_SMT1TMR9   : bit  absolute SMT1TMRH.1;
  SMT1TMRH_SMT1TMR8   : bit  absolute SMT1TMRH.0;
  SMT1TMRU            : byte absolute $0D8E;
  SMT1TMRU_SMT1TMR23  : bit  absolute SMT1TMRU.7;
  SMT1TMRU_SMT1TMR22  : bit  absolute SMT1TMRU.6;
  SMT1TMRU_SMT1TMR21  : bit  absolute SMT1TMRU.5;
  SMT1TMRU_SMT1TMR20  : bit  absolute SMT1TMRU.4;
  SMT1TMRU_SMT1TMR19  : bit  absolute SMT1TMRU.3;
  SMT1TMRU_SMT1TMR18  : bit  absolute SMT1TMRU.2;
  SMT1TMRU_SMT1TMR17  : bit  absolute SMT1TMRU.1;
  SMT1TMRU_SMT1TMR16  : bit  absolute SMT1TMRU.0;
  SMT1CPRL            : byte absolute $0D8F;
  SMT1CPRL_SMT1CPR7   : bit  absolute SMT1CPRL.7;
  SMT1CPRL_SMT1CPR6   : bit  absolute SMT1CPRL.6;
  SMT1CPRL_SMT1CPR5   : bit  absolute SMT1CPRL.5;
  SMT1CPRL_SMT1CPR4   : bit  absolute SMT1CPRL.4;
  SMT1CPRL_SMT1CPR3   : bit  absolute SMT1CPRL.3;
  SMT1CPRL_SMT1CPR2   : bit  absolute SMT1CPRL.2;
  SMT1CPRL_SMT1CPR1   : bit  absolute SMT1CPRL.1;
  SMT1CPRL_SMT1CPR0   : bit  absolute SMT1CPRL.0;
  SMT1CPRH            : byte absolute $0D90;
  SMT1CPRH_SMT1CPR15  : bit  absolute SMT1CPRH.7;
  SMT1CPRH_SMT1CPR14  : bit  absolute SMT1CPRH.6;
  SMT1CPRH_SMT1CPR13  : bit  absolute SMT1CPRH.5;
  SMT1CPRH_SMT1CPR12  : bit  absolute SMT1CPRH.4;
  SMT1CPRH_SMT1CPR11  : bit  absolute SMT1CPRH.3;
  SMT1CPRH_SMT1CPR10  : bit  absolute SMT1CPRH.2;
  SMT1CPRH_SMT1CPR9   : bit  absolute SMT1CPRH.1;
  SMT1CPRH_SMT1CPR8   : bit  absolute SMT1CPRH.0;
  SMT1CPRU            : byte absolute $0D91;
  SMT1CPRU_SMT1CPR23  : bit  absolute SMT1CPRU.7;
  SMT1CPRU_SMT1CPR22  : bit  absolute SMT1CPRU.6;
  SMT1CPRU_SMT1CPR21  : bit  absolute SMT1CPRU.5;
  SMT1CPRU_SMT1CPR20  : bit  absolute SMT1CPRU.4;
  SMT1CPRU_SMT1CPR19  : bit  absolute SMT1CPRU.3;
  SMT1CPRU_SMT1CPR18  : bit  absolute SMT1CPRU.2;
  SMT1CPRU_SMT1CPR17  : bit  absolute SMT1CPRU.1;
  SMT1CPRU_SMT1CPR16  : bit  absolute SMT1CPRU.0;
  SMT1CPWL            : byte absolute $0D92;
  SMT1CPWL_SMT1CPW7   : bit  absolute SMT1CPWL.7;
  SMT1CPWL_SMT1CPW6   : bit  absolute SMT1CPWL.6;
  SMT1CPWL_SMT1CPW5   : bit  absolute SMT1CPWL.5;
  SMT1CPWL_SMT1CPW4   : bit  absolute SMT1CPWL.4;
  SMT1CPWL_SMT1CPW3   : bit  absolute SMT1CPWL.3;
  SMT1CPWL_SMT1CPW2   : bit  absolute SMT1CPWL.2;
  SMT1CPWL_SMT1CPW1   : bit  absolute SMT1CPWL.1;
  SMT1CPWL_SMT1CPW0   : bit  absolute SMT1CPWL.0;
  SMT1CPWH            : byte absolute $0D93;
  SMT1CPWH_SMT1CPW15  : bit  absolute SMT1CPWH.7;
  SMT1CPWH_SMT1CPW14  : bit  absolute SMT1CPWH.6;
  SMT1CPWH_SMT1CPW13  : bit  absolute SMT1CPWH.5;
  SMT1CPWH_SMT1CPW12  : bit  absolute SMT1CPWH.4;
  SMT1CPWH_SMT1CPW11  : bit  absolute SMT1CPWH.3;
  SMT1CPWH_SMT1CPW10  : bit  absolute SMT1CPWH.2;
  SMT1CPWH_SMT1CPW9   : bit  absolute SMT1CPWH.1;
  SMT1CPWH_SMT1CPW8   : bit  absolute SMT1CPWH.0;
  SMT1CPWU            : byte absolute $0D94;
  SMT1CPWU_SMT1CPW23  : bit  absolute SMT1CPWU.7;
  SMT1CPWU_SMT1CPW22  : bit  absolute SMT1CPWU.6;
  SMT1CPWU_SMT1CPW21  : bit  absolute SMT1CPWU.5;
  SMT1CPWU_SMT1CPW20  : bit  absolute SMT1CPWU.4;
  SMT1CPWU_SMT1CPW19  : bit  absolute SMT1CPWU.3;
  SMT1CPWU_SMT1CPW18  : bit  absolute SMT1CPWU.2;
  SMT1CPWU_SMT1CPW17  : bit  absolute SMT1CPWU.1;
  SMT1CPWU_SMT1CPW16  : bit  absolute SMT1CPWU.0;
  SMT1PRL             : byte absolute $0D95;
  SMT1PRL_SMT1PR7     : bit  absolute SMT1PRL.7;
  SMT1PRL_SMT1PR6     : bit  absolute SMT1PRL.6;
  SMT1PRL_SMT1PR5     : bit  absolute SMT1PRL.5;
  SMT1PRL_SMT1PR4     : bit  absolute SMT1PRL.4;
  SMT1PRL_SMT1PR3     : bit  absolute SMT1PRL.3;
  SMT1PRL_SMT1PR2     : bit  absolute SMT1PRL.2;
  SMT1PRL_SMT1PR1     : bit  absolute SMT1PRL.1;
  SMT1PRL_SMT1PR0     : bit  absolute SMT1PRL.0;
  SMT1PRH             : byte absolute $0D96;
  SMT1PRH_SMT1PR15    : bit  absolute SMT1PRH.7;
  SMT1PRH_SMT1PR14    : bit  absolute SMT1PRH.6;
  SMT1PRH_SMT1PR13    : bit  absolute SMT1PRH.5;
  SMT1PRH_SMT1PR12    : bit  absolute SMT1PRH.4;
  SMT1PRH_SMT1PR11    : bit  absolute SMT1PRH.3;
  SMT1PRH_SMT1PR10    : bit  absolute SMT1PRH.2;
  SMT1PRH_SMT1PR9     : bit  absolute SMT1PRH.1;
  SMT1PRH_SMT1PR8     : bit  absolute SMT1PRH.0;
  SMT1PRU             : byte absolute $0D97;
  SMT1PRU_SMT1PR23    : bit  absolute SMT1PRU.7;
  SMT1PRU_SMT1PR22    : bit  absolute SMT1PRU.6;
  SMT1PRU_SMT1PR21    : bit  absolute SMT1PRU.5;
  SMT1PRU_SMT1PR20    : bit  absolute SMT1PRU.4;
  SMT1PRU_SMT1PR19    : bit  absolute SMT1PRU.3;
  SMT1PRU_SMT1PR18    : bit  absolute SMT1PRU.2;
  SMT1PRU_SMT1PR17    : bit  absolute SMT1PRU.1;
  SMT1PRU_SMT1PR16    : bit  absolute SMT1PRU.0;
  SMT1CON0            : byte absolute $0D98;
  SMT1CON0_STP        : bit  absolute SMT1CON0.6;
  SMT1CON0_WPOL       : bit  absolute SMT1CON0.5;
  SMT1CON0_SPOL       : bit  absolute SMT1CON0.4;
  SMT1CON0_CPOL       : bit  absolute SMT1CON0.3;
  SMT1CON0_SMT1PS1    : bit  absolute SMT1CON0.1;
  SMT1CON0_SMT1PS0    : bit  absolute SMT1CON0.0;
  SMT1CON1            : byte absolute $0D99;
  SMT1CON1_SMT1GO     : bit  absolute SMT1CON1.7;
  SMT1CON1_REPEAT     : bit  absolute SMT1CON1.6;
  SMT1STAT            : byte absolute $0D9A;
  SMT1STAT_CPRUP      : bit  absolute SMT1STAT.7;
  SMT1STAT_CPWUP      : bit  absolute SMT1STAT.6;
  SMT1STAT_RST        : bit  absolute SMT1STAT.5;
  SMT1STAT_TS         : bit  absolute SMT1STAT.4;
  SMT1STAT_WS         : bit  absolute SMT1STAT.3;
  SMT1STAT_AS         : bit  absolute SMT1STAT.2;
  SMT1CLK             : byte absolute $0D9B;
  SMT1CLK_CSEL2       : bit  absolute SMT1CLK.3;
  SMT1CLK_CSEL1       : bit  absolute SMT1CLK.2;
  SMT1CLK_CSEL0       : bit  absolute SMT1CLK.1;
  SMT1SIG             : byte absolute $0D9C;
  SMT1SIG_SSEL2       : bit  absolute SMT1SIG.3;
  SMT1SIG_SSEL1       : bit  absolute SMT1SIG.2;
  SMT1SIG_SSEL0       : bit  absolute SMT1SIG.1;
  SMT1WIN             : byte absolute $0D9D;
  SMT1WIN_WSEL3       : bit  absolute SMT1WIN.4;
  SMT1WIN_WSEL2       : bit  absolute SMT1WIN.3;
  SMT1WIN_WSEL1       : bit  absolute SMT1WIN.2;
  SMT1WIN_WSEL0       : bit  absolute SMT1WIN.1;
  SMT2TMRL            : byte absolute $0D9E;
  SMT2TMRL_SMT2TMR7   : bit  absolute SMT2TMRL.7;
  SMT2TMRL_SMT2TMR6   : bit  absolute SMT2TMRL.6;
  SMT2TMRL_SMT2TMR5   : bit  absolute SMT2TMRL.5;
  SMT2TMRL_SMT2TMR4   : bit  absolute SMT2TMRL.4;
  SMT2TMRL_SMT2TMR3   : bit  absolute SMT2TMRL.3;
  SMT2TMRL_SMT2TMR2   : bit  absolute SMT2TMRL.2;
  SMT2TMRL_SMT2TMR1   : bit  absolute SMT2TMRL.1;
  SMT2TMRL_SMT2TMR0   : bit  absolute SMT2TMRL.0;
  SMT2TMRH            : byte absolute $0D9F;
  SMT2TMRH_SMT2TMR15  : bit  absolute SMT2TMRH.7;
  SMT2TMRH_SMT2TMR14  : bit  absolute SMT2TMRH.6;
  SMT2TMRH_SMT2TMR13  : bit  absolute SMT2TMRH.5;
  SMT2TMRH_SMT2TMR12  : bit  absolute SMT2TMRH.4;
  SMT2TMRH_SMT2TMR11  : bit  absolute SMT2TMRH.3;
  SMT2TMRH_SMT2TMR10  : bit  absolute SMT2TMRH.2;
  SMT2TMRH_SMT2TMR9   : bit  absolute SMT2TMRH.1;
  SMT2TMRH_SMT2TMR8   : bit  absolute SMT2TMRH.0;
  SMT2TMRU            : byte absolute $0DA0;
  SMT2TMRU_SMT2TMR23  : bit  absolute SMT2TMRU.7;
  SMT2TMRU_SMT2TMR22  : bit  absolute SMT2TMRU.6;
  SMT2TMRU_SMT2TMR21  : bit  absolute SMT2TMRU.5;
  SMT2TMRU_SMT2TMR20  : bit  absolute SMT2TMRU.4;
  SMT2TMRU_SMT2TMR19  : bit  absolute SMT2TMRU.3;
  SMT2TMRU_SMT2TMR18  : bit  absolute SMT2TMRU.2;
  SMT2TMRU_SMT2TMR17  : bit  absolute SMT2TMRU.1;
  SMT2TMRU_SMT2TMR16  : bit  absolute SMT2TMRU.0;
  SMT2CPRL            : byte absolute $0DA1;
  SMT2CPRL_SMT2CPR7   : bit  absolute SMT2CPRL.7;
  SMT2CPRL_SMT2CPR6   : bit  absolute SMT2CPRL.6;
  SMT2CPRL_SMT2CPR5   : bit  absolute SMT2CPRL.5;
  SMT2CPRL_SMT2CPR4   : bit  absolute SMT2CPRL.4;
  SMT2CPRL_SMT2CPR3   : bit  absolute SMT2CPRL.3;
  SMT2CPRL_SMT2CPR2   : bit  absolute SMT2CPRL.2;
  SMT2CPRL_SMT2CPR1   : bit  absolute SMT2CPRL.1;
  SMT2CPRL_SMT2CPR0   : bit  absolute SMT2CPRL.0;
  SMT2CPRH            : byte absolute $0DA2;
  SMT2CPRH_SMT2CPR15  : bit  absolute SMT2CPRH.7;
  SMT2CPRH_SMT2CPR14  : bit  absolute SMT2CPRH.6;
  SMT2CPRH_SMT2CPR13  : bit  absolute SMT2CPRH.5;
  SMT2CPRH_SMT2CPR12  : bit  absolute SMT2CPRH.4;
  SMT2CPRH_SMT2CPR11  : bit  absolute SMT2CPRH.3;
  SMT2CPRH_SMT2CPR10  : bit  absolute SMT2CPRH.2;
  SMT2CPRH_SMT2CPR9   : bit  absolute SMT2CPRH.1;
  SMT2CPRH_SMT2CPR8   : bit  absolute SMT2CPRH.0;
  SMT2CPRU            : byte absolute $0DA3;
  SMT2CPRU_SMT2CPR23  : bit  absolute SMT2CPRU.7;
  SMT2CPRU_SMT2CPR22  : bit  absolute SMT2CPRU.6;
  SMT2CPRU_SMT2CPR21  : bit  absolute SMT2CPRU.5;
  SMT2CPRU_SMT2CPR20  : bit  absolute SMT2CPRU.4;
  SMT2CPRU_SMT2CPR19  : bit  absolute SMT2CPRU.3;
  SMT2CPRU_SMT2CPR18  : bit  absolute SMT2CPRU.2;
  SMT2CPRU_SMT2CPR17  : bit  absolute SMT2CPRU.1;
  SMT2CPRU_SMT2CPR16  : bit  absolute SMT2CPRU.0;
  SMT2CPWL            : byte absolute $0DA4;
  SMT2CPWL_SMT2CPW7   : bit  absolute SMT2CPWL.7;
  SMT2CPWL_SMT2CPW6   : bit  absolute SMT2CPWL.6;
  SMT2CPWL_SMT2CPW5   : bit  absolute SMT2CPWL.5;
  SMT2CPWL_SMT2CPW4   : bit  absolute SMT2CPWL.4;
  SMT2CPWL_SMT2CPW3   : bit  absolute SMT2CPWL.3;
  SMT2CPWL_SMT2CPW2   : bit  absolute SMT2CPWL.2;
  SMT2CPWL_SMT2CPW1   : bit  absolute SMT2CPWL.1;
  SMT2CPWL_SMT2CPW0   : bit  absolute SMT2CPWL.0;
  SMT2CPWH            : byte absolute $0DA5;
  SMT2CPWH_SMT2CPW15  : bit  absolute SMT2CPWH.7;
  SMT2CPWH_SMT2CPW14  : bit  absolute SMT2CPWH.6;
  SMT2CPWH_SMT2CPW13  : bit  absolute SMT2CPWH.5;
  SMT2CPWH_SMT2CPW12  : bit  absolute SMT2CPWH.4;
  SMT2CPWH_SMT2CPW11  : bit  absolute SMT2CPWH.3;
  SMT2CPWH_SMT2CPW10  : bit  absolute SMT2CPWH.2;
  SMT2CPWH_SMT2CPW9   : bit  absolute SMT2CPWH.1;
  SMT2CPWH_SMT2CPW8   : bit  absolute SMT2CPWH.0;
  SMT2CPWU            : byte absolute $0DA6;
  SMT2CPWU_SMT2CPW23  : bit  absolute SMT2CPWU.7;
  SMT2CPWU_SMT2CPW22  : bit  absolute SMT2CPWU.6;
  SMT2CPWU_SMT2CPW21  : bit  absolute SMT2CPWU.5;
  SMT2CPWU_SMT2CPW20  : bit  absolute SMT2CPWU.4;
  SMT2CPWU_SMT2CPW19  : bit  absolute SMT2CPWU.3;
  SMT2CPWU_SMT2CPW18  : bit  absolute SMT2CPWU.2;
  SMT2CPWU_SMT2CPW17  : bit  absolute SMT2CPWU.1;
  SMT2CPWU_SMT2CPW16  : bit  absolute SMT2CPWU.0;
  SMT2PRL             : byte absolute $0DA7;
  SMT2PRL_SMT2PR7     : bit  absolute SMT2PRL.7;
  SMT2PRL_SMT2PR6     : bit  absolute SMT2PRL.6;
  SMT2PRL_SMT2PR5     : bit  absolute SMT2PRL.5;
  SMT2PRL_SMT2PR4     : bit  absolute SMT2PRL.4;
  SMT2PRL_SMT2PR3     : bit  absolute SMT2PRL.3;
  SMT2PRL_SMT2PR2     : bit  absolute SMT2PRL.2;
  SMT2PRL_SMT2PR1     : bit  absolute SMT2PRL.1;
  SMT2PRL_SMT2PR0     : bit  absolute SMT2PRL.0;
  SMT2PRH             : byte absolute $0DA8;
  SMT2PRH_SMT2PR15    : bit  absolute SMT2PRH.7;
  SMT2PRH_SMT2PR14    : bit  absolute SMT2PRH.6;
  SMT2PRH_SMT2PR13    : bit  absolute SMT2PRH.5;
  SMT2PRH_SMT2PR12    : bit  absolute SMT2PRH.4;
  SMT2PRH_SMT2PR11    : bit  absolute SMT2PRH.3;
  SMT2PRH_SMT2PR10    : bit  absolute SMT2PRH.2;
  SMT2PRH_SMT2PR9     : bit  absolute SMT2PRH.1;
  SMT2PRH_SMT2PR8     : bit  absolute SMT2PRH.0;
  SMT2PRU             : byte absolute $0DA9;
  SMT2PRU_SMT2PR23    : bit  absolute SMT2PRU.7;
  SMT2PRU_SMT2PR22    : bit  absolute SMT2PRU.6;
  SMT2PRU_SMT2PR21    : bit  absolute SMT2PRU.5;
  SMT2PRU_SMT2PR20    : bit  absolute SMT2PRU.4;
  SMT2PRU_SMT2PR19    : bit  absolute SMT2PRU.3;
  SMT2PRU_SMT2PR18    : bit  absolute SMT2PRU.2;
  SMT2PRU_SMT2PR17    : bit  absolute SMT2PRU.1;
  SMT2PRU_SMT2PR16    : bit  absolute SMT2PRU.0;
  SMT2CON0            : byte absolute $0DAA;
  SMT2CON0_SMT2PS1    : bit  absolute SMT2CON0.1;
  SMT2CON0_SMT2PS0    : bit  absolute SMT2CON0.0;
  SMT2CON1            : byte absolute $0DAB;
  SMT2CON1_SMT2GO     : bit  absolute SMT2CON1.7;
  SMT2STAT            : byte absolute $0DAC;
  SMT2CLK             : byte absolute $0DAD;
  SMT2SIG             : byte absolute $0DAE;
  SMT2WIN             : byte absolute $0DAF;
  STATUS_SHAD         : byte absolute $0FE4;
  STATUS_SHAD_Z_SHAD  : bit  absolute STATUS_SHAD.2;
  STATUS_SHAD_DC_SHAD : bit  absolute STATUS_SHAD.1;
  STATUS_SHAD_C_SHAD  : bit  absolute STATUS_SHAD.0;
  WREG_SHAD           : byte absolute $0FE5;
  BSR_SHAD            : byte absolute $0FE6;
  PCLATH_SHAD         : byte absolute $0FE7;
  FSR0L_SHAD          : byte absolute $0FE8;
  FSR0H_SHAD          : byte absolute $0FE9;
  FSR1L_SHAD          : byte absolute $0FEA;
  FSR1H_SHAD          : byte absolute $0FEB;
  STKPTR              : byte absolute $0FED;
  TOSL                : byte absolute $0FEE;
  TOSH                : byte absolute $0FEF;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-00C:SFR'}  // INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON, PORTA
  {$SET_STATE_RAM '00E-00E:SFR'}  // PORTC
  {$SET_STATE_RAM '011-01F:SFR'}  // PIR1, PIR2, PIR3, PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '020-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-080:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '08C-08C:SFR'}  // TRISA
  {$SET_STATE_RAM '08E-08E:SFR'}  // TRISC
  {$SET_STATE_RAM '091-096:SFR'}  // PIE1, PIE2, PIE3, PIE4, OPTION_REG, PCON
  {$SET_STATE_RAM '098-09F:SFR'}  // OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-100:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '10C-10C:SFR'}  // LATA
  {$SET_STATE_RAM '10E-10E:SFR'}  // LATC
  {$SET_STATE_RAM '111-119:SFR'}  // CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DAC1CON0, DAC1CON1
  {$SET_STATE_RAM '11C-11D:SFR'}  // ZCD1CON, APFCON
  {$SET_STATE_RAM '120-16F:GPR'} 
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-180:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '18C-18C:SFR'}  // ANSELA
  {$SET_STATE_RAM '18E-18E:SFR'}  // ANSELC
  {$SET_STATE_RAM '191-197:SFR'}  // PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '1F0-1FF:GPR'} 
  {$SET_STATE_RAM '200-200:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '20C-20C:SFR'}  // WPUA
  {$SET_STATE_RAM '20E-20E:SFR'}  // WPUC
  {$SET_STATE_RAM '270-27F:GPR'} 
  {$SET_STATE_RAM '280-280:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '28C-28C:SFR'}  // ODCONA
  {$SET_STATE_RAM '28E-28E:SFR'}  // ODCONC
  {$SET_STATE_RAM '291-294:SFR'}  // CCPR1L, CCPR1H, CCP1CON, CCP1CAP
  {$SET_STATE_RAM '298-29B:SFR'}  // CCPR2L, CCPR2H, CCP2CON, CCP2CAP
  {$SET_STATE_RAM '29E-29E:SFR'}  // CCPTMRS
  {$SET_STATE_RAM '2F0-2FF:GPR'} 
  {$SET_STATE_RAM '300-300:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '30C-30C:SFR'}  // SLRCONA
  {$SET_STATE_RAM '30E-30E:SFR'}  // SLRCONC
  {$SET_STATE_RAM '370-37F:GPR'} 
  {$SET_STATE_RAM '380-380:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '38C-38C:SFR'}  // INLVLA
  {$SET_STATE_RAM '38E-38E:SFR'}  // INLVLC
  {$SET_STATE_RAM '391-393:SFR'}  // IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '397-399:SFR'}  // IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '3F0-3FF:GPR'} 
  {$SET_STATE_RAM '400-400:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '413-418:SFR'}  // T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '41A-41F:SFR'}  // T6TMR, T6PR, T6CON, T6HLT, T6CLKCON, T6RST
  {$SET_STATE_RAM '470-47F:GPR'} 
  {$SET_STATE_RAM '480-480:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '4F0-4FF:GPR'} 
  {$SET_STATE_RAM '500-500:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '570-57F:GPR'} 
  {$SET_STATE_RAM '580-580:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '5F0-5FF:GPR'} 
  {$SET_STATE_RAM '600-600:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '670-67F:GPR'} 
  {$SET_STATE_RAM '680-680:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '691-69A:SFR'}  // CWG1DBR, CWG1DBF, CWG1AS0, CWG1AS1, CWG1OCON0, CWG1CON0, CWG1CON1, CWG1OCON1, CWG1CLKCON, CWG1ISM
  {$SET_STATE_RAM '6F0-6FF:GPR'} 
  {$SET_STATE_RAM '700-700:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '711-715:SFR'}  // WDTCON0, WDTCON1, WDTPSL, WDTPSH, WDTTMR
  {$SET_STATE_RAM '718-71D:SFR'}  // SCANLADRL, SCANLADRH, SCANHADRL, SCANHADRH, SCANCON0, SCANTRIG
  {$SET_STATE_RAM '770-77F:GPR'} 
  {$SET_STATE_RAM '780-780:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '791-79A:SFR'}  // CRCDATL, CRCDATH, CRCACCL, CRCACCH, CRCSHIFTL, CRCSHIFTH, CRCXORL, CRCXORH, CRCCON0, CRCCON1
  {$SET_STATE_RAM '7F0-7FF:GPR'} 
  {$SET_STATE_RAM '800-800:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '870-87F:GPR'} 
  {$SET_STATE_RAM '880-880:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '8F0-8FF:GPR'} 
  {$SET_STATE_RAM '900-900:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '970-97F:GPR'} 
  {$SET_STATE_RAM '980-980:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '9F0-9FF:GPR'} 
  {$SET_STATE_RAM 'A00-A00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'A70-A7F:GPR'} 
  {$SET_STATE_RAM 'A80-A80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'AF0-AFF:GPR'} 
  {$SET_STATE_RAM 'B00-B00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'B70-B7F:GPR'} 
  {$SET_STATE_RAM 'B80-B80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'BF0-BFF:GPR'} 
  {$SET_STATE_RAM 'C00-C00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'C70-C7F:GPR'} 
  {$SET_STATE_RAM 'C80-C80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'CF0-CFF:GPR'} 
  {$SET_STATE_RAM 'D00-D00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'D70-D7F:GPR'} 
  {$SET_STATE_RAM 'D80-D80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'D8C-DAF:SFR'}  // SMT1TMRL, SMT1TMRH, SMT1TMRU, SMT1CPRL, SMT1CPRH, SMT1CPRU, SMT1CPWL, SMT1CPWH, SMT1CPWU, SMT1PRL, SMT1PRH, SMT1PRU, SMT1CON0, SMT1CON1, SMT1STAT, SMT1CLK, SMT1SIG, SMT1WIN, SMT2TMRL, SMT2TMRH, SMT2TMRU, SMT2CPRL, SMT2CPRH, SMT2CPRU, SMT2CPWL, SMT2CPWH, SMT2CPWU, SMT2PRL, SMT2PRH, SMT2PRU, SMT2CON0, SMT2CON1, SMT2STAT, SMT2CLK, SMT2SIG, SMT2WIN
  {$SET_STATE_RAM 'DF0-DFF:GPR'} 
  {$SET_STATE_RAM 'E00-E00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'E70-E7F:GPR'} 
  {$SET_STATE_RAM 'E80-E80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'EF0-EFF:GPR'} 
  {$SET_STATE_RAM 'F00-F00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'F70-F7F:GPR'} 
  {$SET_STATE_RAM 'F80-F80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'FE4-FEB:SFR'}  // STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}  // STKPTR, TOSL, TOSH
  {$SET_STATE_RAM 'FF0-FFF:GPR'} 


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '080-08B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '0F0-0FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '100-10B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '170-17F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '180-18B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1F0-1FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '200-20B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '270-27F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '280-28B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '2F0-2FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '300-30B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '370-37F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '380-38B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '3F0-3FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '400-40B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '470-47F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '480-48B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '4F0-4FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '500-50B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '570-57F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '580-58B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '5F0-5FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '600-60B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '670-67F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '680-68B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '6F0-6FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '700-70B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '770-77F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '780-78B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '7F0-7FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '800-80B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '870-87F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '880-88B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '8F0-8FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '900-90B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '970-97F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '980-98B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '9F0-9FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'A00-A0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'A70-A7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'A80-A8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'AF0-AFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'B00-B0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'B70-B7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'B80-B8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'BF0-BFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'C00-C0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'C70-C7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'C80-C8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'CF0-CFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'D00-D0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'D70-D7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'D80-D8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'DF0-DFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'E00-E0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'E70-E7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'E80-E8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'EF0-EFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'F00-F0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'F70-F7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'F80-F8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'FF0-FFF:bnk0'} // maps to area 070-07F (bank 0)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00E:3F'} // PORTC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:C7'} // PIR1 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:67'} // PIR2 bits 7,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:30'} // PIR3 bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01D:EF'} // T2HLT bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:07'} // T2CLKCON bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:0F'} // T2RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:37'} // TRISA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:3F'} // TRISC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:C7'} // PIE1 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:67'} // PIE2 bits 7,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:30'} // PIE3 bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:5F'} // OSCSTAT bits 7,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F0'} // ADCON2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:3F'} // LATA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // LATC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:F7'} // CM2CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:F7'} // CM2CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:AC'} // DAC1CON0 bits 6,4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:F3'} // ZCD1CON bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:0A'} // APFCON bits 7,6,5,4,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:0F'} // ANSELC bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:3F'} // WPUC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28E:3F'} // ODCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:03'} // CCP1CAP bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29B:03'} // CCP2CAP bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29E:0F'} // CCPTMRS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30C:37'} // SLRCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30E:3F'} // SLRCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:3F'} // INLVLA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38E:3F'} // INLVLC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '397:3F'} // IOCCP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '398:3F'} // IOCCN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '399:3F'} // IOCCF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '416:EF'} // T4HLT bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:07'} // T4CLKCON bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '418:0F'} // T4RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41D:EF'} // T6HLT bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:07'} // T6CLKCON bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41F:0F'} // T6RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:FC'} // CWG1AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:77'} // CWG1AS1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '696:C7'} // CWG1CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '697:2F'} // CWG1CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '698:0F'} // CWG1OCON1 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '699:01'} // CWG1CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69A:07'} // CWG1ISM bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:3F'} // WDTCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:77'} // WDTCON1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71C:FB'} // SCANCON0 bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71D:03'} // SCANTRIG bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:FE'} // CRCXORL bit 0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:F3'} // CRCCON0 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D98:BF'} // SMT1CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D99:CF'} // SMT1CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9A:E7'} // SMT1STAT bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9B:07'} // SMT1CLK bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9C:07'} // SMT1SIG bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9D:0F'} // SMT1WIN bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAA:BF'} // SMT2CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAB:CF'} // SMT2CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAC:E7'} // SMT2STAT bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAD:07'} // SMT2CLK bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAE:07'} // SMT2SIG bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAF:0F'} // SMT2WIN bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/CLKIN/SMT1WIN/T1CKI/T2AIN/CCP2/IOCA5
  // Pin  3 : RA4/CLKOUT/SMT1SIG/AN3/T1G/IOCA4
  // Pin  4 : RA3/SMT2WIN/T1G/T6AIN/IOCA3/VPP/nMCLR/MCLR
  // Pin  5 : RC5/CWG1A/CCP1/IOCC5
  // Pin  6 : RC4/CWG1B/C2OUT/IOCC4
  // Pin  7 : RC3/AN7/C1IN3-/C2IN3-/CWG1C/CCP2/IOCC3
  // Pin  8 : RC2/AN6/C1IN2-/C2IN2-/CWG1D/IOCC2
  // Pin  9 : RC1/SMT2SIG/AN5/C1IN1-/C2IN1-/T4AIN/IOCC1
  // Pin 10 : RC0/AN4/C2IN+/IOCC0
  // Pin 11 : RA2/ZCD1IN/AN2/CWG1IN/C1OUT/T0CKI/INT/IOCA2
  // Pin 12 : RA1/ZCD1OUT/VREF+DAC/VREF+ADC/AN1/C1IN0-/IOCA
  // Pin 13 : RA0/DAC1OUT/AN0/C1IN+/IOCA0
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:1-13,2-12,3-11,4-4,5-3,6-2'} // PORTA
  {$MAP_RAM_TO_PIN '00E:1-10,2-9,3-8,4-7,5-6,6-5'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_ECH         = $3FFF}  // ECH, External Clock, High Power Mode (4-32 MHz): device clock supplied to CLKIN pins
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

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF          = $3FFF}  // Write protection off
  {$define _WRT_BOOT         = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by EECON control
  {$define _WRT_HALF         = $3FFD}  // 000h to FFFh write protected, 1000h to 1FFFh may be modified by EECON control
  {$define _WRT_ALL          = $3FFC}  // 000h to 1FFFh write protected, no addresses may be modified by EECON control

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
