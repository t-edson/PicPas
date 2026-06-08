unit PIC12F1612;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F1612'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

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
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_TMR6IF              : bit  absolute PIR2.2;
  PIR2_TMR4IF              : bit  absolute PIR2.1;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_CWGIF               : bit  absolute PIR3.5;
  PIR3_ZCDIF               : bit  absolute PIR3.4;
  PIR4                     : byte absolute $0014;
  PIR4_SCANIF              : bit  absolute PIR4.7;
  PIR4_CRCIF               : bit  absolute PIR4.6;
  PIR4_SMT2PWAIF           : bit  absolute PIR4.5;
  PIR4_SMT2PRAIF           : bit  absolute PIR4.4;
  PIR4_SMT2IF              : bit  absolute PIR4.3;
  PIR4_SMT1PWAIF           : bit  absolute PIR4.2;
  PIR4_SMT1PRAIF           : bit  absolute PIR4.1;
  PIR4_SMT1IF              : bit  absolute PIR4.0;
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
  T2HLT_MODE3              : bit  absolute T2HLT.3;
  T2HLT_MODE2              : bit  absolute T2HLT.2;
  T2HLT_MODE1              : bit  absolute T2HLT.1;
  T2HLT_MODE0              : bit  absolute T2HLT.0;
  T2CLKCON                 : byte absolute $001E;
  T2CLKCON_T2CS2           : bit  absolute T2CLKCON.2;
  T2CLKCON_T2CS1           : bit  absolute T2CLKCON.1;
  T2CLKCON_T2CS0           : bit  absolute T2CLKCON.0;
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
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_TMR6IE              : bit  absolute PIE2.2;
  PIE2_TMR4IE              : bit  absolute PIE2.1;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_CWGIE               : bit  absolute PIE3.5;
  PIE3_ZCDIE               : bit  absolute PIE3.4;
  PIE4                     : byte absolute $0094;
  PIE4_SCANIE              : bit  absolute PIE4.7;
  PIE4_CRCIE               : bit  absolute PIE4.6;
  PIE4_SMT2PWAIE           : bit  absolute PIE4.5;
  PIE4_SMT2PRAIE           : bit  absolute PIE4.4;
  PIE4_SMT2IE              : bit  absolute PIE4.3;
  PIE4_SMT1PWAIE           : bit  absolute PIE4.2;
  PIE4_SMT1PRAIE           : bit  absolute PIE4.1;
  PIE4_SMT1IE              : bit  absolute PIE4.0;
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
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.4;
  LATA                     : byte absolute $010C;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
  LATA_LATA3               : bit  absolute LATA.3;
  LATA_LATA2               : bit  absolute LATA.2;
  LATA_LATA1               : bit  absolute LATA.1;
  LATA_LATA0               : bit  absolute LATA.0;
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
  CMOUT                    : byte absolute $0115;
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
  ZCD1CON_ZCD1OE           : bit  absolute ZCD1CON.6;
  ZCD1CON_ZCD1OUT          : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL          : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP         : bit  absolute ZCD1CON.1;
  ZCD1CON_ZCD1INTN         : bit  absolute ZCD1CON.0;
  APFCON                   : byte absolute $011D;
  APFCON_CWGASEL           : bit  absolute APFCON.6;
  APFCON_CWGBSEL           : bit  absolute APFCON.5;
  APFCON_T1GSEL            : bit  absolute APFCON.3;
  APFCON_CCP1SEL           : bit  absolute APFCON.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
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
  WPUA                     : byte absolute $020C;
  WPUA_WPUA5               : bit  absolute WPUA.5;
  WPUA_WPUA4               : bit  absolute WPUA.4;
  WPUA_WPUA3               : bit  absolute WPUA.3;
  WPUA_WPUA2               : bit  absolute WPUA.2;
  WPUA_WPUA1               : bit  absolute WPUA.1;
  WPUA_WPUA0               : bit  absolute WPUA.0;
  ODCONA                   : byte absolute $028C;
  ODCONA_ODA5              : bit  absolute ODCONA.5;
  ODCONA_ODA4              : bit  absolute ODCONA.4;
  ODCONA_ODA2              : bit  absolute ODCONA.2;
  ODCONA_ODA1              : bit  absolute ODCONA.1;
  ODCONA_ODA0              : bit  absolute ODCONA.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_EN               : bit  absolute CCP1CON.7;
  CCP1CON_OE               : bit  absolute CCP1CON.6;
  CCP1CON_OUT              : bit  absolute CCP1CON.5;
  CCP1CON_FMT              : bit  absolute CCP1CON.4;
  CCP1CAP                  : byte absolute $0294;
  CCPR2L                   : byte absolute $0298;
  CCPR2H                   : byte absolute $0299;
  CCP2CON                  : byte absolute $029A;
  CCP2CAP                  : byte absolute $029B;
  CCPTMRS                  : byte absolute $029E;
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
  INLVLA                   : byte absolute $038C;
  INLVLA_INLVLA5           : bit  absolute INLVLA.5;
  INLVLA_INLVLA4           : bit  absolute INLVLA.4;
  INLVLA_INLVLA3           : bit  absolute INLVLA.3;
  INLVLA_INLVLA2           : bit  absolute INLVLA.2;
  INLVLA_INLVLA1           : bit  absolute INLVLA.1;
  INLVLA_INLVLA0           : bit  absolute INLVLA.0;
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
  T4TMR                    : byte absolute $0413;
  T4PR                     : byte absolute $0414;
  T4CON                    : byte absolute $0415;
  T4HLT                    : byte absolute $0416;
  T4CLKCON                 : byte absolute $0417;
  T4CLKCON_T4CS2           : bit  absolute T4CLKCON.2;
  T4CLKCON_T4CS1           : bit  absolute T4CLKCON.1;
  T4CLKCON_T4CS0           : bit  absolute T4CLKCON.0;
  T4RST                    : byte absolute $0418;
  T6TMR                    : byte absolute $041A;
  T6PR                     : byte absolute $041B;
  T6CON                    : byte absolute $041C;
  T6HLT                    : byte absolute $041D;
  T6CLKCON                 : byte absolute $041E;
  T6CLKCON_T6CS2           : bit  absolute T6CLKCON.2;
  T6CLKCON_T6CS1           : bit  absolute T6CLKCON.1;
  T6CLKCON_T6CS0           : bit  absolute T6CLKCON.0;
  T6RST                    : byte absolute $041F;
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
  CWG1OCON1                : byte absolute $0698;
  CWG1OCON1_OED            : bit  absolute CWG1OCON1.3;
  CWG1OCON1_OEC            : bit  absolute CWG1OCON1.2;
  CWG1OCON1_OEB            : bit  absolute CWG1OCON1.1;
  CWG1OCON1_OEA            : bit  absolute CWG1OCON1.0;
  CWG1CLKCON               : byte absolute $0699;
  CWG1CLKCON_CS            : bit  absolute CWG1CLKCON.0;
  CWG1ISM                  : byte absolute $069A;
  WDTCON0                  : byte absolute $0711;
  WDTCON0_WDTPS4           : bit  absolute WDTCON0.5;
  WDTCON0_WDTPS3           : bit  absolute WDTCON0.4;
  WDTCON0_WDTPS2           : bit  absolute WDTCON0.3;
  WDTCON0_WDTPS1           : bit  absolute WDTCON0.2;
  WDTCON0_WDTPS0           : bit  absolute WDTCON0.1;
  WDTCON0_SEN              : bit  absolute WDTCON0.0;
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
  {$SET_STATE_RAM '011-01F:SFR'}            // Bank 0 : PIR1, PIR2, PIR3, PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '091-096:SFR'}            // Bank 1 : PIE1, PIE2, PIE3, PIE4, OPTION_REG, PCON
  {$SET_STATE_RAM '098-09F:SFR'}            // Bank 1 : OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '111-112:SFR'}            // Bank 2 : CM1CON0, CM1CON1
  {$SET_STATE_RAM '115-119:SFR'}            // Bank 2 : CMOUT, BORCON, FVRCON, DAC1CON0, DAC1CON1
  {$SET_STATE_RAM '11C-11D:SFR'}            // Bank 2 : ZCD1CON, APFCON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '28C-28C:SFR'}            // Bank 5 : ODCONA
  {$SET_STATE_RAM '291-294:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, CCP1CAP
  {$SET_STATE_RAM '298-29B:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON, CCP2CAP
  {$SET_STATE_RAM '29E-29E:SFR'}            // Bank 5 : CCPTMRS
  {$SET_STATE_RAM '30C-30C:SFR'}            // Bank 6 : SLRCONA
  {$SET_STATE_RAM '38C-38C:SFR'}            // Bank 7 : INLVLA
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '413-418:SFR'}            // Bank 8 : T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '41A-41F:SFR'}            // Bank 8 : T6TMR, T6PR, T6CON, T6HLT, T6CLKCON, T6RST
  {$SET_STATE_RAM '691-69A:SFR'}            // Bank 13 : CWG1DBR, CWG1DBF, CWG1AS0, CWG1AS1, CWG1OCON0, CWG1CON0, CWG1CON1, CWG1OCON1, CWG1CLKCON, CWG1ISM
  {$SET_STATE_RAM '711-715:SFR'}            // Bank 14 : WDTCON0, WDTCON1, WDTPSL, WDTPSH, WDTTMR
  {$SET_STATE_RAM '718-71D:SFR'}            // Bank 14 : SCANLADRL, SCANLADRH, SCANHADRL, SCANHADRH, SCANCON0, SCANTRIG
  {$SET_STATE_RAM '791-79A:SFR'}            // Bank 15 : CRCDATL, CRCDATH, CRCACCL, CRCACCH, CRCSHIFTL, CRCSHIFTH, CRCXORL, CRCXORH, CRCCON0, CRCCON1
  {$SET_STATE_RAM 'D8C-DAF:SFR'}            // Bank 27 : SMT1TMRL, SMT1TMRH, SMT1TMRU, SMT1CPRL, SMT1CPRH, SMT1CPRU, SMT1CPWL, SMT1CPWH, SMT1CPWU, SMT1PRL, SMT1PRH, SMT1PRU, SMT1CON0, SMT1CON1, SMT1STAT, SMT1CLK, SMT1SIG, SMT1WIN, SMT2TMRL, SMT2TMRH, SMT2TMRU, SMT2CPRL, SMT2CPRH, SMT2CPRU, SMT2CPWL, SMT2CPWH, SMT2CPWU, SMT2PRL, SMT2PRH, SMT2PRU, SMT2CON0, SMT2CON1, SMT2STAT, SMT2CLK, SMT2SIG, SMT2WIN
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:C7'} // PIR1 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:67'} // PIR2 bits 7,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:30'} // PIR3 bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01D:EF'} // T2HLT bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:07'} // T2CLKCON bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:0F'} // T2RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:01'} // CMOUT bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:AC'} // DAC1CON0 bits 6,4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:F3'} // ZCD1CON bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:69'} // APFCON bits 7,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:03'} // CCP1CAP bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29B:03'} // CCP2CAP bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29E:0F'} // CCPTMRS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30C:37'} // SLRCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:3F'} // INLVLA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
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
  // Pin  2 : RA5/CLKIN/SMT1WIN/CWG1A/T1CKI/T2IN/CCP1/IOCA5
  // Pin  3 : CLKOUT/SMT1SIG/AN3/C1IN1-/CWG1B/T1G/IOCA4/RA4
  // Pin  4 : RA3/SMT2WIN/T1G/T6IN/IOCA3/VPP/nMCLR/MCLR
  // Pin  5 : RA2/SMT2SIG/ZCD1IN/AN2/CWG1A/CWG1IN/C1OUT/T0CKI/T4IN/CCP1/INT/IOCA2
  // Pin  6 : RA1/ZCD1OUT/VREF+DAC/VREF+ADC/AN1/C1IN0-/IOCA1
  // Pin  7 : RA0/DAC1OUT/AN0/C1IN+/CWG1B/CCP2/IOCA0
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-7,1-6,2-5,3-4,4-3,5-2'} // PORTA


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
  {$define _WDTCCS_LFINTOSC  = $07FF}  // 31.0 kHz LFINTOSC
  {$define _WDTCCS_MFINTOSC  = $0FFF}  // 31.25 kHz HFINTOSC (MFINTOSC)
  {$define _WDTCCS_SWC       = $3FFF}  // Software control, controlled by WDTCS bits

implementation
end.
