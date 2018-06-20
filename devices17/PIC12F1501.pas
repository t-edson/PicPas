unit PIC12F1501;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F1501'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}

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
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_NCO1IF              : bit  absolute PIR2.2;
  PIR3                     : byte absolute $0013;
  PIR3_CLC2IF              : bit  absolute PIR3.1;
  PIR3_CLC1IF              : bit  absolute PIR3.0;
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
  TRISA_TRISA3             : bit  absolute TRISA.3;
  TRISA_TRISA2             : bit  absolute TRISA.2;
  TRISA_TRISA1             : bit  absolute TRISA.1;
  TRISA_TRISA0             : bit  absolute TRISA.0;
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_NCO1IE              : bit  absolute PIE2.2;
  PIE3                     : byte absolute $0093;
  PIE3_CLC2IE              : bit  absolute PIE3.1;
  PIE3_CLC1IE              : bit  absolute PIE3.0;
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
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.4;
  LATA                     : byte absolute $010C;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
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
  DACCON0                  : byte absolute $0118;
  DACCON0_DACEN            : bit  absolute DACCON0.7;
  DACCON0_DACOE1           : bit  absolute DACCON0.5;
  DACCON0_DACOE2           : bit  absolute DACCON0.4;
  DACCON0_DACPSS           : bit  absolute DACCON0.2;
  DACCON1                  : byte absolute $0119;
  DACCON1_DACR4            : bit  absolute DACCON1.4;
  DACCON1_DACR3            : bit  absolute DACCON1.3;
  DACCON1_DACR2            : bit  absolute DACCON1.2;
  DACCON1_DACR1            : bit  absolute DACCON1.1;
  DACCON1_DACR0            : bit  absolute DACCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_CWG1BSEL          : bit  absolute APFCON.7;
  APFCON_CWG1ASEL          : bit  absolute APFCON.6;
  APFCON_T1GSEL            : bit  absolute APFCON.3;
  APFCON_CLC1SEL           : bit  absolute APFCON.1;
  APFCON_NCO1SEL           : bit  absolute APFCON.0;
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
  VREGCON_VREGPM           : bit  absolute VREGCON.1;
  WPUA                     : byte absolute $020C;
  WPUA_WPUA5               : bit  absolute WPUA.5;
  WPUA_WPUA4               : bit  absolute WPUA.4;
  WPUA_WPUA3               : bit  absolute WPUA.3;
  WPUA_WPUA2               : bit  absolute WPUA.2;
  WPUA_WPUA1               : bit  absolute WPUA.1;
  WPUA_WPUA0               : bit  absolute WPUA.0;
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
  NCO1ACCL                 : byte absolute $0498;
  NCO1ACCL_NCO1ACC7        : bit  absolute NCO1ACCL.7;
  NCO1ACCL_NCO1ACC6        : bit  absolute NCO1ACCL.6;
  NCO1ACCL_NCO1ACC5        : bit  absolute NCO1ACCL.5;
  NCO1ACCL_NCO1ACC4        : bit  absolute NCO1ACCL.4;
  NCO1ACCL_NCO1ACC3        : bit  absolute NCO1ACCL.3;
  NCO1ACCL_NCO1ACC2        : bit  absolute NCO1ACCL.2;
  NCO1ACCL_NCO1ACC1        : bit  absolute NCO1ACCL.1;
  NCO1ACCL_NCO1ACC0        : bit  absolute NCO1ACCL.0;
  NCO1ACCH                 : byte absolute $0499;
  NCO1ACCH_NCO1ACC15       : bit  absolute NCO1ACCH.7;
  NCO1ACCH_NCO1ACC14       : bit  absolute NCO1ACCH.6;
  NCO1ACCH_NCO1ACC13       : bit  absolute NCO1ACCH.5;
  NCO1ACCH_NCO1ACC12       : bit  absolute NCO1ACCH.4;
  NCO1ACCH_NCO1ACC11       : bit  absolute NCO1ACCH.3;
  NCO1ACCH_NCO1ACC10       : bit  absolute NCO1ACCH.2;
  NCO1ACCH_NCO1ACC9        : bit  absolute NCO1ACCH.1;
  NCO1ACCH_NCO1ACC8        : bit  absolute NCO1ACCH.0;
  NCO1ACCU                 : byte absolute $049A;
  NCO1ACCU_NCO1ACC19       : bit  absolute NCO1ACCU.3;
  NCO1ACCU_NCO1ACC18       : bit  absolute NCO1ACCU.2;
  NCO1ACCU_NCO1ACC17       : bit  absolute NCO1ACCU.1;
  NCO1ACCU_NCO1ACC16       : bit  absolute NCO1ACCU.0;
  NCO1INCL                 : byte absolute $049B;
  NCO1INCL_NCO1INC7        : bit  absolute NCO1INCL.7;
  NCO1INCL_NCO1INC6        : bit  absolute NCO1INCL.6;
  NCO1INCL_NCO1INC5        : bit  absolute NCO1INCL.5;
  NCO1INCL_NCO1INC4        : bit  absolute NCO1INCL.4;
  NCO1INCL_NCO1INC3        : bit  absolute NCO1INCL.3;
  NCO1INCL_NCO1INC2        : bit  absolute NCO1INCL.2;
  NCO1INCL_NCO1INC1        : bit  absolute NCO1INCL.1;
  NCO1INCL_NCO1INC0        : bit  absolute NCO1INCL.0;
  NCO1INCH                 : byte absolute $049C;
  NCO1INCH_NCO1INC15       : bit  absolute NCO1INCH.7;
  NCO1INCH_NCO1INC14       : bit  absolute NCO1INCH.6;
  NCO1INCH_NCO1INC13       : bit  absolute NCO1INCH.5;
  NCO1INCH_NCO1INC12       : bit  absolute NCO1INCH.4;
  NCO1INCH_NCO1INC11       : bit  absolute NCO1INCH.3;
  NCO1INCH_NCO1INC10       : bit  absolute NCO1INCH.2;
  NCO1INCH_NCO1INC9        : bit  absolute NCO1INCH.1;
  NCO1INCH_NCO1INC8        : bit  absolute NCO1INCH.0;
  NCO1INCU                 : byte absolute $049D;
  NCO1CON                  : byte absolute $049E;
  NCO1CON_N1EN             : bit  absolute NCO1CON.7;
  NCO1CON_N1OE             : bit  absolute NCO1CON.6;
  NCO1CON_N1OUT            : bit  absolute NCO1CON.5;
  NCO1CON_N1POL            : bit  absolute NCO1CON.4;
  NCO1CON_N1PFM            : bit  absolute NCO1CON.0;
  NCO1CLK                  : byte absolute $049F;
  NCO1CLK_N1PWS2           : bit  absolute NCO1CLK.7;
  NCO1CLK_N1PWS1           : bit  absolute NCO1CLK.6;
  NCO1CLK_N1PWS0           : bit  absolute NCO1CLK.5;
  NCO1CLK_N1CKS3           : bit  absolute NCO1CLK.3;
  NCO1CLK_N1CKS2           : bit  absolute NCO1CLK.2;
  NCO1CLK_N1CKS1           : bit  absolute NCO1CLK.1;
  NCO1CLK_N1CKS0           : bit  absolute NCO1CLK.0;
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
  PWM3DCL                  : byte absolute $0617;
  PWM3DCL_PWM3DCL1         : bit  absolute PWM3DCL.7;
  PWM3DCL_PWM3DCL0         : bit  absolute PWM3DCL.6;
  PWM3DCH                  : byte absolute $0618;
  PWM3CON                  : byte absolute $0619;
  PWM3CON_PWM3EN           : bit  absolute PWM3CON.7;
  PWM3CON_PWM3OE           : bit  absolute PWM3CON.6;
  PWM3CON_PWM3OUT          : bit  absolute PWM3CON.5;
  PWM3CON_PWM3POL          : bit  absolute PWM3CON.4;
  PWM4DCL                  : byte absolute $061A;
  PWM4DCL_PWM4DCL1         : bit  absolute PWM4DCL.7;
  PWM4DCL_PWM4DCL0         : bit  absolute PWM4DCL.6;
  PWM4DCH                  : byte absolute $061B;
  PWM4CON                  : byte absolute $061C;
  PWM4CON_PWM4EN           : bit  absolute PWM4CON.7;
  PWM4CON_PWM4OE           : bit  absolute PWM4CON.6;
  PWM4CON_PWM4OUT          : bit  absolute PWM4CON.5;
  PWM4CON_PWM4POL          : bit  absolute PWM4CON.4;
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
  CWG1CON1_G1IS2           : bit  absolute CWG1CON1.2;
  CWG1CON1_G1IS1           : bit  absolute CWG1CON1.1;
  CWG1CON1_G1IS0           : bit  absolute CWG1CON1.0;
  CWG1CON2                 : byte absolute $0695;
  CWG1CON2_G1ASE           : bit  absolute CWG1CON2.7;
  CWG1CON2_G1ARSEN         : bit  absolute CWG1CON2.6;
  CWG1CON2_G1ASDSC1        : bit  absolute CWG1CON2.2;
  CWG1CON2_G1ASDSFLT       : bit  absolute CWG1CON2.1;
  CWG1CON2_G1ASDSCLC2      : bit  absolute CWG1CON2.0;
  CLCDATA                  : byte absolute $0F0F;
  CLCDATA_MCLC2OUT         : bit  absolute CLCDATA.1;
  CLCDATA_MCLC1OUT         : bit  absolute CLCDATA.0;
  CLC1CON                  : byte absolute $0F10;
  CLC1CON_LC1EN            : bit  absolute CLC1CON.7;
  CLC1CON_LC1OE            : bit  absolute CLC1CON.6;
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
  CLC1SEL0_LC1D2S2         : bit  absolute CLC1SEL0.6;
  CLC1SEL0_LC1D2S1         : bit  absolute CLC1SEL0.5;
  CLC1SEL0_LC1D2S0         : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S2         : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1         : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0         : bit  absolute CLC1SEL0.0;
  CLC1SEL1                 : byte absolute $0F13;
  CLC1SEL1_LC1D4S2         : bit  absolute CLC1SEL1.6;
  CLC1SEL1_LC1D4S1         : bit  absolute CLC1SEL1.5;
  CLC1SEL1_LC1D4S0         : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D3S2         : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D3S1         : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D3S0         : bit  absolute CLC1SEL1.0;
  CLC1GLS0                 : byte absolute $0F14;
  CLC1GLS0_LC1G1D4T        : bit  absolute CLC1GLS0.7;
  CLC1GLS0_LC1G1D4N        : bit  absolute CLC1GLS0.6;
  CLC1GLS0_LC1G1D3T        : bit  absolute CLC1GLS0.5;
  CLC1GLS0_LC1G1D3N        : bit  absolute CLC1GLS0.4;
  CLC1GLS0_LC1G1D2T        : bit  absolute CLC1GLS0.3;
  CLC1GLS0_LC1G1D2N        : bit  absolute CLC1GLS0.2;
  CLC1GLS0_LC1G1D1T        : bit  absolute CLC1GLS0.1;
  CLC1GLS0_LC1G1D1N        : bit  absolute CLC1GLS0.0;
  CLC1GLS1                 : byte absolute $0F15;
  CLC1GLS1_LC1G2D4T        : bit  absolute CLC1GLS1.7;
  CLC1GLS1_LC1G2D4N        : bit  absolute CLC1GLS1.6;
  CLC1GLS1_LC1G2D3T        : bit  absolute CLC1GLS1.5;
  CLC1GLS1_LC1G2D3N        : bit  absolute CLC1GLS1.4;
  CLC1GLS1_LC1G2D2T        : bit  absolute CLC1GLS1.3;
  CLC1GLS1_LC1G2D2N        : bit  absolute CLC1GLS1.2;
  CLC1GLS1_LC1G2D1T        : bit  absolute CLC1GLS1.1;
  CLC1GLS1_LC1G2D1N        : bit  absolute CLC1GLS1.0;
  CLC1GLS2                 : byte absolute $0F16;
  CLC1GLS2_LC1G3D4T        : bit  absolute CLC1GLS2.7;
  CLC1GLS2_LC1G3D4N        : bit  absolute CLC1GLS2.6;
  CLC1GLS2_LC1G3D3T        : bit  absolute CLC1GLS2.5;
  CLC1GLS2_LC1G3D3N        : bit  absolute CLC1GLS2.4;
  CLC1GLS2_LC1G3D2T        : bit  absolute CLC1GLS2.3;
  CLC1GLS2_LC1G3D2N        : bit  absolute CLC1GLS2.2;
  CLC1GLS2_LC1G3D1T        : bit  absolute CLC1GLS2.1;
  CLC1GLS2_LC1G3D1N        : bit  absolute CLC1GLS2.0;
  CLC1GLS3                 : byte absolute $0F17;
  CLC1GLS3_LC1G4D4T        : bit  absolute CLC1GLS3.7;
  CLC1GLS3_LC1G4D4N        : bit  absolute CLC1GLS3.6;
  CLC1GLS3_LC1G4D3T        : bit  absolute CLC1GLS3.5;
  CLC1GLS3_LC1G4D3N        : bit  absolute CLC1GLS3.4;
  CLC1GLS3_LC1G4D2T        : bit  absolute CLC1GLS3.3;
  CLC1GLS3_LC1G4D2N        : bit  absolute CLC1GLS3.2;
  CLC1GLS3_LC1G4D1T        : bit  absolute CLC1GLS3.1;
  CLC1GLS3_LC1G4D1N        : bit  absolute CLC1GLS3.0;
  CLC2CON                  : byte absolute $0F18;
  CLC2CON_LC2EN            : bit  absolute CLC2CON.7;
  CLC2CON_LC2OE            : bit  absolute CLC2CON.6;
  CLC2CON_LC2OUT           : bit  absolute CLC2CON.5;
  CLC2CON_LC2INTP          : bit  absolute CLC2CON.4;
  CLC2CON_LC2INTN          : bit  absolute CLC2CON.3;
  CLC2CON_LC2MODE2         : bit  absolute CLC2CON.2;
  CLC2CON_LC2MODE1         : bit  absolute CLC2CON.1;
  CLC2CON_LC2MODE0         : bit  absolute CLC2CON.0;
  CLC2POL                  : byte absolute $0F19;
  CLC2POL_LC2POL           : bit  absolute CLC2POL.7;
  CLC2POL_LC2G4POL         : bit  absolute CLC2POL.3;
  CLC2POL_LC2G3POL         : bit  absolute CLC2POL.2;
  CLC2POL_LC2G2POL         : bit  absolute CLC2POL.1;
  CLC2POL_LC2G1POL         : bit  absolute CLC2POL.0;
  CLC2SEL0                 : byte absolute $0F1A;
  CLC2SEL0_LC2D2S2         : bit  absolute CLC2SEL0.6;
  CLC2SEL0_LC2D2S1         : bit  absolute CLC2SEL0.5;
  CLC2SEL0_LC2D2S0         : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S2         : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1         : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0         : bit  absolute CLC2SEL0.0;
  CLC2SEL1                 : byte absolute $0F1B;
  CLC2SEL1_LC2D4S2         : bit  absolute CLC2SEL1.6;
  CLC2SEL1_LC2D4S1         : bit  absolute CLC2SEL1.5;
  CLC2SEL1_LC2D4S0         : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D3S2         : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D3S1         : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D3S0         : bit  absolute CLC2SEL1.0;
  CLC2GLS0                 : byte absolute $0F1C;
  CLC2GLS0_LC2G1D4T        : bit  absolute CLC2GLS0.7;
  CLC2GLS0_LC2G1D4N        : bit  absolute CLC2GLS0.6;
  CLC2GLS0_LC2G1D3T        : bit  absolute CLC2GLS0.5;
  CLC2GLS0_LC2G1D3N        : bit  absolute CLC2GLS0.4;
  CLC2GLS0_LC2G1D2T        : bit  absolute CLC2GLS0.3;
  CLC2GLS0_LC2G1D2N        : bit  absolute CLC2GLS0.2;
  CLC2GLS0_LC2G1D1T        : bit  absolute CLC2GLS0.1;
  CLC2GLS0_LC2G1D1N        : bit  absolute CLC2GLS0.0;
  CLC2GLS1                 : byte absolute $0F1D;
  CLC2GLS1_LC2G2D4T        : bit  absolute CLC2GLS1.7;
  CLC2GLS1_LC2G2D4N        : bit  absolute CLC2GLS1.6;
  CLC2GLS1_LC2G2D3T        : bit  absolute CLC2GLS1.5;
  CLC2GLS1_LC2G2D3N        : bit  absolute CLC2GLS1.4;
  CLC2GLS1_LC2G2D2T        : bit  absolute CLC2GLS1.3;
  CLC2GLS1_LC2G2D2N        : bit  absolute CLC2GLS1.2;
  CLC2GLS1_LC2G2D1T        : bit  absolute CLC2GLS1.1;
  CLC2GLS1_LC2G2D1N        : bit  absolute CLC2GLS1.0;
  CLC2GLS2                 : byte absolute $0F1E;
  CLC2GLS2_LC2G3D4T        : bit  absolute CLC2GLS2.7;
  CLC2GLS2_LC2G3D4N        : bit  absolute CLC2GLS2.6;
  CLC2GLS2_LC2G3D3T        : bit  absolute CLC2GLS2.5;
  CLC2GLS2_LC2G3D3N        : bit  absolute CLC2GLS2.4;
  CLC2GLS2_LC2G3D2T        : bit  absolute CLC2GLS2.3;
  CLC2GLS2_LC2G3D2N        : bit  absolute CLC2GLS2.2;
  CLC2GLS2_LC2G3D1T        : bit  absolute CLC2GLS2.1;
  CLC2GLS2_LC2G3D1N        : bit  absolute CLC2GLS2.0;
  CLC2GLS3                 : byte absolute $0F1F;
  CLC2GLS3_LC2G4D4T        : bit  absolute CLC2GLS3.7;
  CLC2GLS3_LC2G4D4N        : bit  absolute CLC2GLS3.6;
  CLC2GLS3_LC2G4D3T        : bit  absolute CLC2GLS3.5;
  CLC2GLS3_LC2G4D3N        : bit  absolute CLC2GLS3.4;
  CLC2GLS3_LC2G4D2T        : bit  absolute CLC2GLS3.3;
  CLC2GLS3_LC2G4D2N        : bit  absolute CLC2GLS3.2;
  CLC2GLS3_LC2G4D1T        : bit  absolute CLC2GLS3.1;
  CLC2GLS3_LC2G4D1N        : bit  absolute CLC2GLS3.0;
  BSR_ICDSHAD              : byte absolute $0FE3;
  BSR_ICDSHAD_BSR_ICDSHAD4 : bit  absolute BSR_ICDSHAD.4;
  BSR_ICDSHAD_BSR_ICDSHAD3 : bit  absolute BSR_ICDSHAD.3;
  BSR_ICDSHAD_BSR_ICDSHAD2 : bit  absolute BSR_ICDSHAD.2;
  BSR_ICDSHAD_BSR_ICDSHAD1 : bit  absolute BSR_ICDSHAD.1;
  BSR_ICDSHAD_BSR_ICDSHAD0 : bit  absolute BSR_ICDSHAD.0;
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
  {$SET_STATE_RAM '011-013:SFR'}            // Bank 0 : PIR1, PIR2, PIR3
  {$SET_STATE_RAM '015-01C:SFR'}            // Bank 0 : TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-04F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '091-093:SFR'}            // Bank 1 : PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-097:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON
  {$SET_STATE_RAM '099-09F:SFR'}            // Bank 1 : OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '111-112:SFR'}            // Bank 2 : CM1CON0, CM1CON1
  {$SET_STATE_RAM '115-119:SFR'}            // Bank 2 : CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '498-49F:SFR'}            // Bank 9 : NCO1ACCL, NCO1ACCH, NCO1ACCU, NCO1INCL, NCO1INCH, NCO1INCU, NCO1CON, NCO1CLK
  {$SET_STATE_RAM '611-61C:SFR'}            // Bank 12 : PWM1DCL, PWM1DCH, PWM1CON, PWM2DCL, PWM2DCH, PWM2CON, PWM3DCL, PWM3DCH, PWM3CON, PWM4DCL, PWM4DCH, PWM4CON
  {$SET_STATE_RAM '691-695:SFR'}            // Bank 13 : CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1CON2
  {$SET_STATE_RAM 'F0F-F1F:SFR'}            // Bank 30 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3
  {$SET_STATE_RAM 'FE3-FEB:SFR'}            // Bank 31 : BSR_ICDSHAD, STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:C3'} // PIR1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:24'} // PIR2 bits 7,6,4,3,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:03'} // PIR3 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:C3'} // PIE1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:24'} // PIE2 bits 7,6,4,3,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:03'} // PIE3 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:7B'} // OSCCON bits 7,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:13'} // OSCSTAT bits 7,6,5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F0'} // ADCON2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:01'} // CMOUT bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:B4'} // DACCON0 bits 6,3,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:CB'} // APFCON bits 5,4,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:02'} // VREGCON bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49A:0F'} // NCO1ACCU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49D:00'} // NCO1INCU bits 7,6,5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49E:F1'} // NCO1CON bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49F:E3'} // NCO1CLK bits 4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '611:C0'} // PWM1DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:F0'} // PWM1CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '614:C0'} // PWM2DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '616:F0'} // PWM2CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '617:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:F0'} // PWM3CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61A:C0'} // PWM4DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61C:F0'} // PWM4CON bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:F9'} // CWG1CON0 bits 2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:F7'} // CWG1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '695:C7'} // CWG1CON2 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F0F:03'} // CLCDATA bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F12:77'} // CLC1SEL0 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F13:77'} // CLC1SEL1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F19:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1A:77'} // CLC2SEL0 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1B:77'} // CLC2SEL1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE3:1F'} // BSR_ICDSHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : RA5/CLKIN/T1CKI/CWG1A/NCO1/NCO1CLK/CLC1IN1/CLC2/PWM4
  // Pin  3 : RA4/AN3/C1IN1-/CWG1B/CLC1/PWM3/CLKOUT/T1G
  // Pin  4 : RA3/CLC1IN0/VPP/T1G/MCLR
  // Pin  5 : RA2/AN2/C1OUT/DACOUT2/T0CKI/INT/PWM1/CLC1/CWG1A/CWG1FLT
  // Pin  6 : RA1/AN1/VREF+/C1IN0-/NCO1/CLC2IN0/ICSPCLK
  // Pin  7 : RA0/AN0/C1IN+/DACOUT1/CWG1B/CLC2IN1/PWM2/ICSPDAT
  // Pin  8 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-7,1-6,2-5,3-4,4-3,5-2'} // PORTA


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-20 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_ECM     = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_ECL     = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz): device clock supplied to CLKIN pin
  {$define _FOSC_INTOSC  = $3FFC}  // INTOSC oscillator: I/O function on CLKIN pin

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

  // WRT : Flash Memory Self-Write Protection
  {$define _WRT_OFF      = $3FFF}  // Write protection off
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to 1FFFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to FFFh write protected, 1000h to 1FFFh may be modified by EECON control
  {$define _WRT_ALL      = $3FFC}  // 000h to 1FFFh write protected, no addresses may be modified by EECON control

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
