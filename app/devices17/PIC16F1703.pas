unit PIC16F1703;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1703'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 14}
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
  PORTC                    : byte absolute $000E;
  PORTC_RC5                : bit  absolute PORTC.5;
  PORTC_RC4                : bit  absolute PORTC.4;
  PORTC_RC3                : bit  absolute PORTC.3;
  PORTC_RC2                : bit  absolute PORTC.2;
  PORTC_RC1                : bit  absolute PORTC.1;
  PORTC_RC0                : bit  absolute PORTC.0;
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR2_CCP2IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_ZCDIF               : bit  absolute PIR3.4;
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
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE2_CCP2IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_ZCDIE               : bit  absolute PIE3.4;
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
  OSCSTAT_SOSCR            : bit  absolute OSCSTAT.7;
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
  ADCON1_ADNREF            : bit  absolute ADCON1.2;
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
  LATC                     : byte absolute $010E;
  LATC_LATC5               : bit  absolute LATC.5;
  LATC_LATC4               : bit  absolute LATC.4;
  LATC_LATC3               : bit  absolute LATC.3;
  LATC_LATC2               : bit  absolute LATC.2;
  LATC_LATC1               : bit  absolute LATC.1;
  LATC_LATC0               : bit  absolute LATC.0;
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
  ANSELC_ANSC5             : bit  absolute ANSELC.5;
  ANSELC_ANSC4             : bit  absolute ANSELC.4;
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
  VREGCON_VREGPM           : bit  absolute VREGCON.1;
  VREGCON_Reserved         : bit  absolute VREGCON.0;
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
  IOCCP_IOCCP5             : bit  absolute IOCCP.5;
  IOCCP_IOCCP4             : bit  absolute IOCCP.4;
  IOCCP_IOCCP3             : bit  absolute IOCCP.3;
  IOCCP_IOCCP2             : bit  absolute IOCCP.2;
  IOCCP_IOCCP1             : bit  absolute IOCCP.1;
  IOCCP_IOCCP0             : bit  absolute IOCCP.0;
  IOCCN                    : byte absolute $0398;
  IOCCN_IOCCN5             : bit  absolute IOCCN.5;
  IOCCN_IOCCN4             : bit  absolute IOCCN.4;
  IOCCN_IOCCN3             : bit  absolute IOCCN.3;
  IOCCN_IOCCN2             : bit  absolute IOCCN.2;
  IOCCN_IOCCN1             : bit  absolute IOCCN.1;
  IOCCN_IOCCN0             : bit  absolute IOCCN.0;
  IOCCF                    : byte absolute $0399;
  IOCCF_IOCCF5             : bit  absolute IOCCF.5;
  IOCCF_IOCCF4             : bit  absolute IOCCF.4;
  IOCCF_IOCCF3             : bit  absolute IOCCF.3;
  IOCCF_IOCCF2             : bit  absolute IOCCF.2;
  IOCCF_IOCCF1             : bit  absolute IOCCF.1;
  IOCCF_IOCCF0             : bit  absolute IOCCF.0;
  OPA1CON                  : byte absolute $0511;
  OPA1CON_OPA1EN           : bit  absolute OPA1CON.7;
  OPA1CON_OPA1SP           : bit  absolute OPA1CON.6;
  OPA1CON_OPA1UG           : bit  absolute OPA1CON.4;
  OPA1CON_OPA1PCH1         : bit  absolute OPA1CON.1;
  OPA1CON_OPA1PCH0         : bit  absolute OPA1CON.0;
  OPA2CON                  : byte absolute $0515;
  OPA2CON_OPA2EN           : bit  absolute OPA2CON.7;
  OPA2CON_OPA2SP           : bit  absolute OPA2CON.6;
  OPA2CON_OPA2UG           : bit  absolute OPA2CON.4;
  OPA2CON_OPA2PCH1         : bit  absolute OPA2CON.1;
  OPA2CON_OPA2PCH0         : bit  absolute OPA2CON.0;
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
  ICDIO                    : byte absolute $0F8C;
  ICDIO_PORT_ICDDAT        : bit  absolute ICDIO.7;
  ICDIO_PORT_ICDCLK        : bit  absolute ICDIO.6;
  ICDIO_LAT_ICDDAT         : bit  absolute ICDIO.5;
  ICDIO_LAT_ICDCLK         : bit  absolute ICDIO.4;
  ICDIO_TRIS_ICDDAT        : bit  absolute ICDIO.3;
  ICDIO_TRIS_ICDCLK        : bit  absolute ICDIO.2;
  ICDCON0                  : byte absolute $0F8D;
  ICDCON0_INBUG            : bit  absolute ICDCON0.7;
  ICDCON0_FREEZ            : bit  absolute ICDCON0.6;
  ICDCON0_SSTEP            : bit  absolute ICDCON0.5;
  ICDCON0_DBGINEX          : bit  absolute ICDCON0.3;
  ICDCON0_RSTVEC           : bit  absolute ICDCON0.0;
  ICDSTAT                  : byte absolute $0F91;
  ICDSTAT_TRP1HLTF         : bit  absolute ICDSTAT.7;
  ICDSTAT_TRP0HLTF         : bit  absolute ICDSTAT.6;
  ICDSTAT_USRHLTF          : bit  absolute ICDSTAT.1;
  CLKRFRZ                  : byte absolute $0F92;
  DEVSEL                   : byte absolute $0F95;
  DEVSEL_DEVSEL2           : bit  absolute DEVSEL.2;
  DEVSEL_DEVSEL1           : bit  absolute DEVSEL.1;
  DEVSEL_DEVSEL0           : bit  absolute DEVSEL.0;
  ICDINSTL                 : byte absolute $0F96;
  ICDINSTL_DBGIN7          : bit  absolute ICDINSTL.7;
  ICDINSTL_DBGIN6          : bit  absolute ICDINSTL.6;
  ICDINSTL_DBGIN5          : bit  absolute ICDINSTL.5;
  ICDINSTL_DBGIN4          : bit  absolute ICDINSTL.4;
  ICDINSTL_DBGIN3          : bit  absolute ICDINSTL.3;
  ICDINSTL_DBGIN2          : bit  absolute ICDINSTL.2;
  ICDINSTL_DBGIN1          : bit  absolute ICDINSTL.1;
  ICDINSTL_DBGIN0          : bit  absolute ICDINSTL.0;
  ICDINSTH                 : byte absolute $0F97;
  ICDINSTH_DBGIN13         : bit  absolute ICDINSTH.5;
  ICDINSTH_DBGIN12         : bit  absolute ICDINSTH.4;
  ICDINSTH_DBGIN11         : bit  absolute ICDINSTH.3;
  ICDINSTH_DBGIN10         : bit  absolute ICDINSTH.2;
  ICDINSTH_DBGIN9          : bit  absolute ICDINSTH.1;
  ICDINSTH_DBGIN8          : bit  absolute ICDINSTH.0;
  ICDBK0CON                : byte absolute $0F9C;
  ICDBK0CON_BKEN           : bit  absolute ICDBK0CON.7;
  ICDBK0CON_BKHLT          : bit  absolute ICDBK0CON.0;
  ICDBK0L                  : byte absolute $0F9D;
  ICDBK0L_BKA7             : bit  absolute ICDBK0L.7;
  ICDBK0L_BKA6             : bit  absolute ICDBK0L.6;
  ICDBK0L_BKA5             : bit  absolute ICDBK0L.5;
  ICDBK0L_BKA4             : bit  absolute ICDBK0L.4;
  ICDBK0L_BKA3             : bit  absolute ICDBK0L.3;
  ICDBK0L_BKA2             : bit  absolute ICDBK0L.2;
  ICDBK0L_BKA1             : bit  absolute ICDBK0L.1;
  ICDBK0L_BKA0             : bit  absolute ICDBK0L.0;
  ICDBK0H                  : byte absolute $0F9E;
  ICDBK0H_BKA14            : bit  absolute ICDBK0H.6;
  ICDBK0H_BKA13            : bit  absolute ICDBK0H.5;
  ICDBK0H_BKA12            : bit  absolute ICDBK0H.4;
  ICDBK0H_BKA11            : bit  absolute ICDBK0H.3;
  ICDBK0H_BKA10            : bit  absolute ICDBK0H.2;
  ICDBK0H_BKA9             : bit  absolute ICDBK0H.1;
  ICDBK0H_BKA8             : bit  absolute ICDBK0H.0;
  BSRICDSHAD               : byte absolute $0FE3;
  BSRICDSHAD_BSR_ICDSHAD4  : bit  absolute BSRICDSHAD.4;
  BSRICDSHAD_BSR_ICDSHAD3  : bit  absolute BSRICDSHAD.3;
  BSRICDSHAD_BSR_ICDSHAD2  : bit  absolute BSRICDSHAD.2;
  BSRICDSHAD_BSR_ICDSHAD1  : bit  absolute BSRICDSHAD.1;
  BSRICDSHAD_BSR_ICDSHAD0  : bit  absolute BSRICDSHAD.0;
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
  {$SET_STATE_RAM '011-013:SFR'}            // Bank 0 : PIR1, PIR2, PIR3
  {$SET_STATE_RAM '015-01C:SFR'}            // Bank 0 : TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '08E-08E:SFR'}            // Bank 1 : TRISC
  {$SET_STATE_RAM '091-093:SFR'}            // Bank 1 : PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-09F:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '10E-10E:SFR'}            // Bank 2 : LATC
  {$SET_STATE_RAM '116-117:SFR'}            // Bank 2 : BORCON, FVRCON
  {$SET_STATE_RAM '11C-11C:SFR'}            // Bank 2 : ZCD1CON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '18E-18E:SFR'}            // Bank 3 : ANSELC
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '20E-20E:SFR'}            // Bank 4 : WPUC
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '28C-28C:SFR'}            // Bank 5 : ODCONA
  {$SET_STATE_RAM '28E-28E:SFR'}            // Bank 5 : ODCONC
  {$SET_STATE_RAM '291-293:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '298-29A:SFR'}            // Bank 5 : CCPR2L, CCPR2H, CCP2CON
  {$SET_STATE_RAM '30C-30C:SFR'}            // Bank 6 : SLRCONA
  {$SET_STATE_RAM '30E-30E:SFR'}            // Bank 6 : SLRCONC
  {$SET_STATE_RAM '38C-38C:SFR'}            // Bank 7 : INLVLA
  {$SET_STATE_RAM '38E-38E:SFR'}            // Bank 7 : INLVLC
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '397-399:SFR'}            // Bank 7 : IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '511-511:SFR'}            // Bank 10 : OPA1CON
  {$SET_STATE_RAM '515-515:SFR'}            // Bank 10 : OPA2CON
  {$SET_STATE_RAM 'E0F-E15:SFR'}            // Bank 28 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, CCP1PPS, CCP2PPS
  {$SET_STATE_RAM 'E20-E22:SFR'}            // Bank 28 : SSPCLKPPS, SSPDATPPS, SSPSSPPS
  {$SET_STATE_RAM 'E90-E92:SFR'}            // Bank 29 : RA0PPS, RA1PPS, RA2PPS
  {$SET_STATE_RAM 'E94-E95:SFR'}            // Bank 29 : RA4PPS, RA5PPS
  {$SET_STATE_RAM 'EA0-EA5:SFR'}            // Bank 29 : RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS
  {$SET_STATE_RAM 'F8C-F8D:SFR'}            // Bank 31 : ICDIO, ICDCON0
  {$SET_STATE_RAM 'F91-F92:SFR'}            // Bank 31 : ICDSTAT, CLKRFRZ
  {$SET_STATE_RAM 'F95-F97:SFR'}            // Bank 31 : DEVSEL, ICDINSTL, ICDINSTH
  {$SET_STATE_RAM 'F9C-F9E:SFR'}            // Bank 31 : ICDBK0CON, ICDBK0L, ICDBK0H
  {$SET_STATE_RAM 'FE3-FEB:SFR'}            // Bank 31 : BSRICDSHAD, STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00E:3F'} // PORTC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:CF'} // PIR1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:09'} // PIR2 bits 7,6,5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:10'} // PIR3 bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:3F'} // TRISC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:CF'} // PIE1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:09'} // PIE2 bits 7,6,5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:10'} // PIE3 bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F0'} // ADCON2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // LATC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11C:B3'} // ZCD1CON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:3F'} // ANSELC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:3F'} // WPUC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28E:3F'} // ODCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29A:3F'} // CCP2CON bits 7,6 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS '511:D3'} // OPA1CON bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '515:D3'} // OPA2CON bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E10:1F'} // INTPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E11:1F'} // T0CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E12:1F'} // T1CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E13:1F'} // T1GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E14:1F'} // CCP1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E15:1F'} // CCP2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E20:1F'} // SSPCLKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E21:1F'} // SSPDATPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E22:1F'} // SSPSSPPS bits 7,6,5 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS 'F8C:FC'} // ICDIO bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F8D:E9'} // ICDCON0 bits 4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F91:C2'} // ICDSTAT bits 5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F92:00'} // CLKRFRZ bits 7,6,5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F95:07'} // DEVSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F97:3F'} // ICDINSTH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F9C:81'} // ICDBK0CON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F9E:7F'} // ICDBK0H bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE3:1F'} // BSRICDSHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // PMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/SOSCI/CLKIN/OSC1
  // Pin  3 : RA4/AN3/SOSCO/CLKOUT/OSC2
  // Pin  4 : RA3/MCLR/Vpp
  // Pin  5 : RC5/OPA2IN+
  // Pin  6 : RC4/OPA2IN-
  // Pin  7 : RC3/AN7/OPA2OUT
  // Pin  8 : RC2/AN6/OPA1OUT
  // Pin  9 : RC1/AN5/OPA1IN-
  // Pin 10 : RC0/AN4/OPA1IN+
  // Pin 11 : RA2/AN2/ZCD
  // Pin 12 : RA1/AN1/Vref+/ICSPCLK
  // Pin 13 : RA0/AN0/Vref-/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '00E:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection Bits
  {$define _FOSC_ECH     = $3FFF}  // External oscillator, high power. I/O function on RA4
  {$define _FOSC_ECM     = $3FFE}  // External oscillator, medium power. I/O function on RA4
  {$define _FOSC_ECL     = $3FFD}  // External oscillator, low power. I/O function on RA4
  {$define _FOSC_INTOSC  = $3FFC}  // Internal HFINTOSC. I/O function on RA4 and RA5.

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

  // PPS1WAY : Peripheral Pin Select one-way control
  {$define _PPS1WAY_ON   = $3FFF}  // The PPSLOCK bit cannot be cleared once it is set by software
  {$define _PPS1WAY_OFF  = $3FFB}  // The PPSLOCK bit can be set and cleared repeatedly by software

  // ZCDDIS : Zero-cross detect disable
  {$define _ZCDDIS_ON    = $3FFF}  // Zero-cross detect circuit is disabled at POR
  {$define _ZCDDIS_OFF   = $3F7F}  // Zero-cross detect circuit is enabled at POR

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
