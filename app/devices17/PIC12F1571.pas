unit PIC12F1571;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F1571'}
{$SET PIC_MAXFREQ  = 32000000}
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
  PIR3                     : byte absolute $0013;
  PIR3_PWM3IF              : bit  absolute PIR3.6;
  PIR3_PWM2IF              : bit  absolute PIR3.5;
  PIR3_PWM1IF              : bit  absolute PIR3.4;
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
  PIE3                     : byte absolute $0093;
  PIE3_PWM3IE              : bit  absolute PIE3.6;
  PIE3_PWM2IE              : bit  absolute PIE3.5;
  PIE3_PWM1IE              : bit  absolute PIE3.4;
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
  DACCON0_DACLPS           : bit  absolute DACCON0.6;
  DACCON0_DACOE            : bit  absolute DACCON0.5;
  DACCON0_DACPSS1          : bit  absolute DACCON0.3;
  DACCON0_DACPSS0          : bit  absolute DACCON0.2;
  DACCON1                  : byte absolute $0119;
  DACCON1_DACR4            : bit  absolute DACCON1.4;
  DACCON1_DACR3            : bit  absolute DACCON1.3;
  DACCON1_DACR2            : bit  absolute DACCON1.2;
  DACCON1_DACR1            : bit  absolute DACCON1.1;
  DACCON1_DACR0            : bit  absolute DACCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_CWGASEL           : bit  absolute APFCON.6;
  APFCON_CWGBSEL           : bit  absolute APFCON.5;
  APFCON_T1GSEL            : bit  absolute APFCON.3;
  APFCON_P2SEL             : bit  absolute APFCON.1;
  APFCON_P1SEL             : bit  absolute APFCON.0;
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
  ODCONA                   : byte absolute $028C;
  ODCONA_ODA5              : bit  absolute ODCONA.5;
  ODCONA_ODA4              : bit  absolute ODCONA.4;
  ODCONA_ODA2              : bit  absolute ODCONA.2;
  ODCONA_ODA1              : bit  absolute ODCONA.1;
  ODCONA_ODA0              : bit  absolute ODCONA.0;
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
  PWMEN                    : byte absolute $0D8E;
  PWMEN_PWM3EN_A           : bit  absolute PWMEN.2;
  PWMEN_PWM2EN_A           : bit  absolute PWMEN.1;
  PWMEN_PWM1EN_A           : bit  absolute PWMEN.0;
  PWMLD                    : byte absolute $0D8F;
  PWMLD_PWM3LDA_A          : bit  absolute PWMLD.2;
  PWMLD_PWM2LDA_A          : bit  absolute PWMLD.1;
  PWMLD_PWM1LDA_A          : bit  absolute PWMLD.0;
  PWMOUT                   : byte absolute $0D90;
  PWMOUT_PWM3OUT_A         : bit  absolute PWMOUT.2;
  PWMOUT_PWM2OUT_A         : bit  absolute PWMOUT.1;
  PWMOUT_PWM1OUT_A         : bit  absolute PWMOUT.0;
  PWM1PHL                  : byte absolute $0D91;
  PWM1PHH                  : byte absolute $0D92;
  PWM1DCL                  : byte absolute $0D93;
  PWM1DCH                  : byte absolute $0D94;
  PWM1PRL                  : byte absolute $0D95;
  PWM1PRH                  : byte absolute $0D96;
  PWM1OFL                  : byte absolute $0D97;
  PWM1OFH                  : byte absolute $0D98;
  PWM1TMRL                 : byte absolute $0D99;
  PWM1TMRH                 : byte absolute $0D9A;
  PWM1CON                  : byte absolute $0D9B;
  PWM1CON_EN               : bit  absolute PWM1CON.7;
  PWM1CON_OE               : bit  absolute PWM1CON.6;
  PWM1CON_OUT              : bit  absolute PWM1CON.5;
  PWM1CON_POL              : bit  absolute PWM1CON.4;
  PWM1CON_MODE1            : bit  absolute PWM1CON.3;
  PWM1CON_MODE0            : bit  absolute PWM1CON.2;
  PWM1INTE                 : byte absolute $0D9C;
  PWM1INTE_OFIE            : bit  absolute PWM1INTE.3;
  PWM1INTE_PHIE            : bit  absolute PWM1INTE.2;
  PWM1INTE_DCIE            : bit  absolute PWM1INTE.1;
  PWM1INTE_PRIE            : bit  absolute PWM1INTE.0;
  PWM1INTF                 : byte absolute $0D9D;
  PWM1INTF_OFIF            : bit  absolute PWM1INTF.3;
  PWM1INTF_PHIF            : bit  absolute PWM1INTF.2;
  PWM1INTF_DCIF            : bit  absolute PWM1INTF.1;
  PWM1INTF_PRIF            : bit  absolute PWM1INTF.0;
  PWM1CLKCON               : byte absolute $0D9E;
  PWM1CLKCON_CS1           : bit  absolute PWM1CLKCON.1;
  PWM1CLKCON_CS0           : bit  absolute PWM1CLKCON.0;
  PWM1LDCON                : byte absolute $0D9F;
  PWM1LDCON_LDA            : bit  absolute PWM1LDCON.7;
  PWM1LDCON_LDT            : bit  absolute PWM1LDCON.6;
  PWM1LDCON_LDS1           : bit  absolute PWM1LDCON.1;
  PWM1LDCON_LDS0           : bit  absolute PWM1LDCON.0;
  PWM1OFCON                : byte absolute $0DA0;
  PWM1OFCON_OFM1           : bit  absolute PWM1OFCON.6;
  PWM1OFCON_OFM0           : bit  absolute PWM1OFCON.5;
  PWM1OFCON_OFO            : bit  absolute PWM1OFCON.4;
  PWM1OFCON_OFS1           : bit  absolute PWM1OFCON.1;
  PWM1OFCON_OFS0           : bit  absolute PWM1OFCON.0;
  PWM2PHL                  : byte absolute $0DA1;
  PWM2PHH                  : byte absolute $0DA2;
  PWM2DCL                  : byte absolute $0DA3;
  PWM2DCH                  : byte absolute $0DA4;
  PWM2PRL                  : byte absolute $0DA5;
  PWM2PRH                  : byte absolute $0DA6;
  PWM2OFL                  : byte absolute $0DA7;
  PWM2OFH                  : byte absolute $0DA8;
  PWM2TMRL                 : byte absolute $0DA9;
  PWM2TMRH                 : byte absolute $0DAA;
  PWM2CON                  : byte absolute $0DAB;
  PWM2INTE                 : byte absolute $0DAC;
  PWM2INTF                 : byte absolute $0DAD;
  PWM2CLKCON               : byte absolute $0DAE;
  PWM2LDCON                : byte absolute $0DAF;
  PWM2OFCON                : byte absolute $0DB0;
  PWM3PHL                  : byte absolute $0DB1;
  PWM3PHH                  : byte absolute $0DB2;
  PWM3DCL                  : byte absolute $0DB3;
  PWM3DCH                  : byte absolute $0DB4;
  PWM3PRL                  : byte absolute $0DB5;
  PWM3PRH                  : byte absolute $0DB6;
  PWM3OFL                  : byte absolute $0DB7;
  PWM3OFH                  : byte absolute $0DB8;
  PWM3TMRL                 : byte absolute $0DB9;
  PWM3TMRH                 : byte absolute $0DBA;
  PWM3CON                  : byte absolute $0DBB;
  PWM3INTE                 : byte absolute $0DBC;
  PWM3INTF                 : byte absolute $0DBD;
  PWM3CLKCON               : byte absolute $0DBE;
  PWM3LDCON                : byte absolute $0DBF;
  PWM3OFCON                : byte absolute $0DC0;
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
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '091-093:SFR'}            // Bank 1 : PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-09F:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0BF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '111-112:SFR'}            // Bank 2 : CM1CON0, CM1CON1
  {$SET_STATE_RAM '115-119:SFR'}            // Bank 2 : CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '28C-28C:SFR'}            // Bank 5 : ODCONA
  {$SET_STATE_RAM '30C-30C:SFR'}            // Bank 6 : SLRCONA
  {$SET_STATE_RAM '38C-38C:SFR'}            // Bank 7 : INLVLA
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '691-695:SFR'}            // Bank 13 : CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1CON2
  {$SET_STATE_RAM 'D8E-DC0:SFR'}            // Bank 27 : PWMEN, PWMLD, PWMOUT, PWM1PHL, PWM1PHH, PWM1DCL, PWM1DCH, PWM1PRL, PWM1PRH, PWM1OFL, PWM1OFH, PWM1TMRL, PWM1TMRH, PWM1CON, PWM1INTE, PWM1INTF, PWM1CLKCON, PWM1LDCON, PWM1OFCON, PWM2PHL, PWM2PHH, PWM2DCL, PWM2DCH, PWM2PRL, PWM2PRH, PWM2OFL, PWM2OFH, PWM2TMRL, PWM2TMRH, PWM2CON, PWM2INTE, PWM2INTF, PWM2CLKCON, PWM2LDCON, PWM2OFCON, PWM3PHL, PWM3PHH, PWM3DCL, PWM3DCH, PWM3PRL, PWM3PRH, PWM3OFL, PWM3OFH, PWM3TMRL, PWM3TMRH, PWM3CON, PWM3INTE, PWM3INTF, PWM3CLKCON, PWM3LDCON, PWM3OFCON
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:C3'} // PIR1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:20'} // PIR2 bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:70'} // PIR3 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:C3'} // PIE1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:20'} // PIE2 bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:70'} // PIE3 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09A:7F'} // OSCSTAT bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F0'} // ADCON2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F7'} // CM1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:01'} // CMOUT bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:EC'} // DACCON0 bits 4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:6B'} // APFCON bits 7,4,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '192:7F'} // PMADRH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:02'} // VREGCON bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30C:37'} // SLRCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:3F'} // INLVLA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:F9'} // CWG1CON0 bits 2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:F7'} // CWG1CON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '695:C6'} // CWG1CON2 bits 5,4,3,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8E:07'} // PWMEN bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8F:07'} // PWMLD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D90:07'} // PWMOUT bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9B:FC'} // PWM1CON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9C:0F'} // PWM1INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9D:0F'} // PWM1INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9E:73'} // PWM1CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9F:C3'} // PWM1LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DA0:73'} // PWM1OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAB:FC'} // PWM2CON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAC:0F'} // PWM2INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAD:0F'} // PWM2INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAE:73'} // PWM2CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DAF:C3'} // PWM2LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DB0:73'} // PWM2OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBB:FC'} // PWM3CON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBC:0F'} // PWM3INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBD:0F'} // PWM3INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBE:73'} // PWM3CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DBF:C3'} // PWM3LDCON bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DC0:73'} // PWM3OFCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/PWM1/CWG1A/CLKIN
  // Pin  3 : RA4/AN3/C1IN1-/PWM2/CWG1B/CLKOUT/T1G
  // Pin  4 : RA3/T1G/MCLR/Vpp
  // Pin  5 : RA2/AN2/T0CKI/PWM3/C1OUT/CWG1FLT/CWG1A/INT
  // Pin  6 : RA1/AN1/VREF+/C1IN0-/PWM1/ICSPCLK/ICDCLK
  // Pin  7 : RA0/AN0/C1IN+/PWM2/DAC1OUT/CWG1B/ICSPDAT/ICDDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-7,1-6,2-5,3-4,4-3,5-2'} // PORTA


  // -- Bits Configuration --

  // FOSC : 
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-32 MHz); device clock supplied to CLKIN pin
  {$define _FOSC_ECM     = $3FFE}  // ECM, External Clock, Medium Power Mode (0.5-4 MHz); device clock supplied to CLKIN pin
  {$define _FOSC_ECL     = $3FFD}  // ECL, External Clock, Low Power Mode (0-0.5 MHz); device clock supplied to CLKIN pin
  {$define _FOSC_INTOSC  = $3FFC}  // INTOSC oscillator; I/O function on CLKIN pin

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
  {$define _WRT_BOOT     = $3FFE}  // 000h to 0FFh write protected, 100h to 3FFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to 1FFh write protected, 200h to 3FFh may be modified by EECON control
  {$define _WRT_ALL      = $3FFC}  // 000h to 7FFh write protected, no addresses may be modified by EECON control

  // PLLEN : PLL Enable
  {$define _PLLEN_ON     = $3FFF}  // 4x PLL enabled
  {$define _PLLEN_OFF    = $3EFF}  // 4x PLL disabled

  // STVREN : Stack Overflow/Underflow Reset Enable
  {$define _STVREN_ON    = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF   = $3DFF}  // Stack Overflow or Underflow will not cause a Reset

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO      = $3FFF}  // Brown-out Reset Voltage (Vbor), low trip point selected.
  {$define _BORV_HI      = $3BFF}  // Brown-out Reset Voltage (Vbor), high trip point selected.

  // LPBOREN : Low Power Brown-out Reset enable bit
  {$define _LPBOREN_OFF  = $3FFF}  // LPBOR is disabled
  {$define _LPBOREN_ON   = $37FF}  // LPBOR is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
