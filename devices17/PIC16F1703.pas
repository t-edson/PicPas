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
  PIR1_TMR1GIF        : bit  absolute PIR1.6;
  PIR1_ADIF           : bit  absolute PIR1.5;
  PIR1_SSP1IF         : bit  absolute PIR1.4;
  PIR1_CCP1IF         : bit  absolute PIR1.3;
  PIR1_TMR2IF         : bit  absolute PIR1.2;
  PIR1_TMR1IF         : bit  absolute PIR1.1;
  PIR2                : byte absolute $0012;
  PIR2_BCL1IF         : bit  absolute PIR2.2;
  PIR2_CCP2IF         : bit  absolute PIR2.1;
  PIR3                : byte absolute $0013;
  PIR3_ZCDIF          : bit  absolute PIR3.1;
  TMR0                : byte absolute $0015;
  TMR1L               : byte absolute $0016;
  TMR1H               : byte absolute $0017;
  T1CON               : byte absolute $0018;
  T1CON_TMR1CS1       : bit  absolute T1CON.7;
  T1CON_TMR1CS0       : bit  absolute T1CON.6;
  T1CON_T1CKPS1       : bit  absolute T1CON.5;
  T1CON_T1CKPS0       : bit  absolute T1CON.4;
  T1CON_T1OSCEN       : bit  absolute T1CON.3;
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
  TMR2                : byte absolute $001A;
  PR2                 : byte absolute $001B;
  T2CON               : byte absolute $001C;
  T2CON_T2OUTPS3      : bit  absolute T2CON.6;
  T2CON_T2OUTPS2      : bit  absolute T2CON.5;
  T2CON_T2OUTPS1      : bit  absolute T2CON.4;
  T2CON_T2OUTPS0      : bit  absolute T2CON.3;
  T2CON_TMR2ON        : bit  absolute T2CON.2;
  T2CON_T2CKPS0       : bit  absolute T2CON.1;
  TRISA               : byte absolute $008C;
  TRISA_TRISA5        : bit  absolute TRISA.5;
  TRISA_TRISA4        : bit  absolute TRISA.4;
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
  PIE1_TMR1GIE        : bit  absolute PIE1.6;
  PIE1_ADIE           : bit  absolute PIE1.5;
  PIE1_SSP1IE         : bit  absolute PIE1.4;
  PIE1_CCP1IE         : bit  absolute PIE1.3;
  PIE1_TMR2IE         : bit  absolute PIE1.2;
  PIE1_TMR1IE         : bit  absolute PIE1.1;
  PIE2                : byte absolute $0092;
  PIE2_BCL1IE         : bit  absolute PIE2.2;
  PIE2_CCP2IE         : bit  absolute PIE2.1;
  PIE3                : byte absolute $0093;
  PIE3_ZCDIE          : bit  absolute PIE3.1;
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
  PCON_RWDT           : bit  absolute PCON.5;
  PCON_RMCLR          : bit  absolute PCON.4;
  PCON_RI             : bit  absolute PCON.3;
  PCON_POR            : bit  absolute PCON.2;
  PCON_BOR            : bit  absolute PCON.1;
  WDTCON              : byte absolute $0097;
  WDTCON_WDTPS4       : bit  absolute WDTCON.5;
  WDTCON_WDTPS3       : bit  absolute WDTCON.4;
  WDTCON_WDTPS2       : bit  absolute WDTCON.3;
  WDTCON_WDTPS1       : bit  absolute WDTCON.2;
  WDTCON_WDTPS0       : bit  absolute WDTCON.1;
  WDTCON_SWDTEN       : bit  absolute WDTCON.0;
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
  OSCSTAT_SOSCR       : bit  absolute OSCSTAT.7;
  OSCSTAT_PLLR        : bit  absolute OSCSTAT.6;
  OSCSTAT_OSTS        : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFR      : bit  absolute OSCSTAT.4;
  OSCSTAT_HFIOFL      : bit  absolute OSCSTAT.3;
  OSCSTAT_MFIOFR      : bit  absolute OSCSTAT.2;
  OSCSTAT_LFIOFR      : bit  absolute OSCSTAT.1;
  OSCSTAT_HFIOFS      : bit  absolute OSCSTAT.0;
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
  ADCON1_ADFM         : bit  absolute ADCON1.7;
  ADCON1_ADNREF       : bit  absolute ADCON1.3;
  ADCON1_ADPREF0      : bit  absolute ADCON1.2;
  ADCON2              : byte absolute $009F;
  ADCON2_TRIGSEL3     : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL2     : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL1     : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL0     : bit  absolute ADCON2.4;
  LATA                : byte absolute $010C;
  LATA_LATA5          : bit  absolute LATA.5;
  LATA_LATA4          : bit  absolute LATA.4;
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
  ZCD1CON             : byte absolute $011C;
  ZCD1CON_ZCD1EN      : bit  absolute ZCD1CON.6;
  ZCD1CON_ZCD1OUT     : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL     : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP    : bit  absolute ZCD1CON.3;
  ZCD1CON_ZCD1INTN    : bit  absolute ZCD1CON.2;
  ANSELA              : byte absolute $018C;
  ANSELA_ANSA4        : bit  absolute ANSELA.4;
  ANSELA_ANSA2        : bit  absolute ANSELA.3;
  ANSELA_ANSA1        : bit  absolute ANSELA.2;
  ANSELA_ANSA0        : bit  absolute ANSELA.1;
  ANSELC              : byte absolute $018E;
  ANSELC_ANSC5        : bit  absolute ANSELC.6;
  ANSELC_ANSC4        : bit  absolute ANSELC.5;
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
  VREGCON_VREGPM      : bit  absolute VREGCON.1;
  VREGCON_Reserved    : bit  absolute VREGCON.0;
  WPUA                : byte absolute $020C;
  WPUA_WPUA5          : bit  absolute WPUA.5;
  WPUA_WPUA4          : bit  absolute WPUA.4;
  WPUA_WPUA3          : bit  absolute WPUA.3;
  WPUA_WPUA2          : bit  absolute WPUA.2;
  WPUA_WPUA1          : bit  absolute WPUA.1;
  WPUA_WPUA0          : bit  absolute WPUA.0;
  WPUC                : byte absolute $020E;
  WPUC_WPUC5          : bit  absolute WPUC.6;
  WPUC_WPUC4          : bit  absolute WPUC.5;
  WPUC_WPUC3          : bit  absolute WPUC.4;
  WPUC_WPUC2          : bit  absolute WPUC.3;
  WPUC_WPUC1          : bit  absolute WPUC.2;
  WPUC_WPUC0          : bit  absolute WPUC.1;
  SSP1BUF             : byte absolute $0211;
  SSP1BUF_SSP1BUF7    : bit  absolute SSP1BUF.7;
  SSP1BUF_SSP1BUF6    : bit  absolute SSP1BUF.6;
  SSP1BUF_SSP1BUF5    : bit  absolute SSP1BUF.5;
  SSP1BUF_SSP1BUF4    : bit  absolute SSP1BUF.4;
  SSP1BUF_SSP1BUF3    : bit  absolute SSP1BUF.3;
  SSP1BUF_SSP1BUF2    : bit  absolute SSP1BUF.2;
  SSP1BUF_SSP1BUF1    : bit  absolute SSP1BUF.1;
  SSP1BUF_SSP1BUF0    : bit  absolute SSP1BUF.0;
  SSP1ADD             : byte absolute $0212;
  SSP1ADD_SSP1ADD7    : bit  absolute SSP1ADD.7;
  SSP1ADD_SSP1ADD6    : bit  absolute SSP1ADD.6;
  SSP1ADD_SSP1ADD5    : bit  absolute SSP1ADD.5;
  SSP1ADD_SSP1ADD4    : bit  absolute SSP1ADD.4;
  SSP1ADD_SSP1ADD3    : bit  absolute SSP1ADD.3;
  SSP1ADD_SSP1ADD2    : bit  absolute SSP1ADD.2;
  SSP1ADD_SSP1ADD1    : bit  absolute SSP1ADD.1;
  SSP1ADD_SSP1ADD0    : bit  absolute SSP1ADD.0;
  SSP1MSK             : byte absolute $0213;
  SSP1MSK_SSP1MSK7    : bit  absolute SSP1MSK.7;
  SSP1MSK_SSP1MSK6    : bit  absolute SSP1MSK.6;
  SSP1MSK_SSP1MSK5    : bit  absolute SSP1MSK.5;
  SSP1MSK_SSP1MSK4    : bit  absolute SSP1MSK.4;
  SSP1MSK_SSP1MSK3    : bit  absolute SSP1MSK.3;
  SSP1MSK_SSP1MSK2    : bit  absolute SSP1MSK.2;
  SSP1MSK_SSP1MSK1    : bit  absolute SSP1MSK.1;
  SSP1MSK_SSP1MSK0    : bit  absolute SSP1MSK.0;
  SSP1STAT            : byte absolute $0214;
  SSP1STAT_SMP        : bit  absolute SSP1STAT.7;
  SSP1STAT_CKE        : bit  absolute SSP1STAT.6;
  SSP1STAT_D_nA       : bit  absolute SSP1STAT.5;
  SSP1STAT_P          : bit  absolute SSP1STAT.4;
  SSP1STAT_S          : bit  absolute SSP1STAT.3;
  SSP1STAT_R_nW       : bit  absolute SSP1STAT.2;
  SSP1STAT_UA         : bit  absolute SSP1STAT.1;
  SSP1STAT_BF         : bit  absolute SSP1STAT.0;
  SSP1CON1            : byte absolute $0215;
  SSP1CON1_WCOL       : bit  absolute SSP1CON1.7;
  SSP1CON1_SSPOV      : bit  absolute SSP1CON1.6;
  SSP1CON1_SSPEN      : bit  absolute SSP1CON1.5;
  SSP1CON1_CKP        : bit  absolute SSP1CON1.4;
  SSP1CON1_SSPM3      : bit  absolute SSP1CON1.3;
  SSP1CON1_SSPM2      : bit  absolute SSP1CON1.2;
  SSP1CON1_SSPM1      : bit  absolute SSP1CON1.1;
  SSP1CON1_SSPM0      : bit  absolute SSP1CON1.0;
  SSP1CON2            : byte absolute $0216;
  SSP1CON2_GCEN       : bit  absolute SSP1CON2.7;
  SSP1CON2_ACKSTAT    : bit  absolute SSP1CON2.6;
  SSP1CON2_ACKDT      : bit  absolute SSP1CON2.5;
  SSP1CON2_ACKEN      : bit  absolute SSP1CON2.4;
  SSP1CON2_RCEN       : bit  absolute SSP1CON2.3;
  SSP1CON2_PEN        : bit  absolute SSP1CON2.2;
  SSP1CON2_RSEN       : bit  absolute SSP1CON2.1;
  SSP1CON2_SEN        : bit  absolute SSP1CON2.0;
  SSP1CON3            : byte absolute $0217;
  SSP1CON3_ACKTIM     : bit  absolute SSP1CON3.7;
  SSP1CON3_PCIE       : bit  absolute SSP1CON3.6;
  SSP1CON3_SCIE       : bit  absolute SSP1CON3.5;
  SSP1CON3_BOEN       : bit  absolute SSP1CON3.4;
  SSP1CON3_SDAHT      : bit  absolute SSP1CON3.3;
  SSP1CON3_SBCDE      : bit  absolute SSP1CON3.2;
  SSP1CON3_AHEN       : bit  absolute SSP1CON3.1;
  SSP1CON3_DHEN       : bit  absolute SSP1CON3.0;
  ODCONA              : byte absolute $028C;
  ODCONA_ODA5         : bit  absolute ODCONA.5;
  ODCONA_ODA4         : bit  absolute ODCONA.4;
  ODCONA_ODA2         : bit  absolute ODCONA.3;
  ODCONA_ODA1         : bit  absolute ODCONA.2;
  ODCONA_ODA0         : bit  absolute ODCONA.1;
  ODCONC              : byte absolute $028E;
  ODCONC_ODC5         : bit  absolute ODCONC.6;
  ODCONC_ODC4         : bit  absolute ODCONC.5;
  ODCONC_ODC3         : bit  absolute ODCONC.4;
  ODCONC_ODC2         : bit  absolute ODCONC.3;
  ODCONC_ODC1         : bit  absolute ODCONC.2;
  ODCONC_ODC0         : bit  absolute ODCONC.1;
  CCPR1L              : byte absolute $0291;
  CCPR1H              : byte absolute $0292;
  CCP1CON             : byte absolute $0293;
  CCP1CON_DC1B1       : bit  absolute CCP1CON.6;
  CCP1CON_DC1B0       : bit  absolute CCP1CON.5;
  CCP1CON_CCP1M3      : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M2      : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M1      : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M0      : bit  absolute CCP1CON.1;
  CCPR2L              : byte absolute $0298;
  CCPR2H              : byte absolute $0299;
  CCP2CON             : byte absolute $029A;
  CCP2CON_DC2B1       : bit  absolute CCP2CON.6;
  CCP2CON_DC2B0       : bit  absolute CCP2CON.5;
  CCP2CON_CCP2M3      : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M2      : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M1      : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M0      : bit  absolute CCP2CON.1;
  SLRCONA             : byte absolute $030C;
  SLRCONA_SLRA5       : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4       : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2       : bit  absolute SLRCONA.3;
  SLRCONA_SLRA1       : bit  absolute SLRCONA.2;
  SLRCONA_SLRA0       : bit  absolute SLRCONA.1;
  SLRCONC             : byte absolute $030E;
  SLRCONC_SLRC5       : bit  absolute SLRCONC.6;
  SLRCONC_SLRC4       : bit  absolute SLRCONC.5;
  SLRCONC_SLRC3       : bit  absolute SLRCONC.4;
  SLRCONC_SLRC2       : bit  absolute SLRCONC.3;
  SLRCONC_SLRC1       : bit  absolute SLRCONC.2;
  SLRCONC_SLRC0       : bit  absolute SLRCONC.1;
  INLVLA              : byte absolute $038C;
  INLVLA_INLVLA5      : bit  absolute INLVLA.5;
  INLVLA_INLVLA4      : bit  absolute INLVLA.4;
  INLVLA_INLVLA3      : bit  absolute INLVLA.3;
  INLVLA_INLVLA2      : bit  absolute INLVLA.2;
  INLVLA_INLVLA1      : bit  absolute INLVLA.1;
  INLVLA_INLVLA0      : bit  absolute INLVLA.0;
  INLVLC              : byte absolute $038E;
  INLVLC_INLVLC5      : bit  absolute INLVLC.6;
  INLVLC_INLVLC4      : bit  absolute INLVLC.5;
  INLVLC_INLVLC3      : bit  absolute INLVLC.4;
  INLVLC_INLVLC2      : bit  absolute INLVLC.3;
  INLVLC_INLVLC1      : bit  absolute INLVLC.2;
  INLVLC_INLVLC0      : bit  absolute INLVLC.1;
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
  IOCCP_IOCCP5        : bit  absolute IOCCP.6;
  IOCCP_IOCCP4        : bit  absolute IOCCP.5;
  IOCCP_IOCCP3        : bit  absolute IOCCP.4;
  IOCCP_IOCCP2        : bit  absolute IOCCP.3;
  IOCCP_IOCCP1        : bit  absolute IOCCP.2;
  IOCCP_IOCCP0        : bit  absolute IOCCP.1;
  IOCCN               : byte absolute $0398;
  IOCCN_IOCCN5        : bit  absolute IOCCN.6;
  IOCCN_IOCCN4        : bit  absolute IOCCN.5;
  IOCCN_IOCCN3        : bit  absolute IOCCN.4;
  IOCCN_IOCCN2        : bit  absolute IOCCN.3;
  IOCCN_IOCCN1        : bit  absolute IOCCN.2;
  IOCCN_IOCCN0        : bit  absolute IOCCN.1;
  IOCCF               : byte absolute $0399;
  IOCCF_IOCCF5        : bit  absolute IOCCF.6;
  IOCCF_IOCCF4        : bit  absolute IOCCF.5;
  IOCCF_IOCCF3        : bit  absolute IOCCF.4;
  IOCCF_IOCCF2        : bit  absolute IOCCF.3;
  IOCCF_IOCCF1        : bit  absolute IOCCF.2;
  IOCCF_IOCCF0        : bit  absolute IOCCF.1;
  OPA1CON             : byte absolute $0511;
  OPA1CON_OPA1EN      : bit  absolute OPA1CON.6;
  OPA1CON_OPA1SP      : bit  absolute OPA1CON.5;
  OPA1CON_OPA1UG      : bit  absolute OPA1CON.4;
  OPA1CON_OPA1PCH1    : bit  absolute OPA1CON.1;
  OPA1CON_OPA1PCH0    : bit  absolute OPA1CON.0;
  OPA2CON             : byte absolute $0515;
  OPA2CON_OPA2EN      : bit  absolute OPA2CON.6;
  OPA2CON_OPA2SP      : bit  absolute OPA2CON.5;
  OPA2CON_OPA2UG      : bit  absolute OPA2CON.4;
  OPA2CON_OPA2PCH1    : bit  absolute OPA2CON.1;
  OPA2CON_OPA2PCH0    : bit  absolute OPA2CON.0;
  PPSLOCK             : byte absolute $0E0F;
  PPSLOCK_PPSLOCKED   : bit  absolute PPSLOCK.0;
  INTPPS              : byte absolute $0E10;
  T0CKIPPS            : byte absolute $0E11;
  T1CKIPPS            : byte absolute $0E12;
  T1GPPS              : byte absolute $0E13;
  CCP1PPS             : byte absolute $0E14;
  CCP2PPS             : byte absolute $0E15;
  SSPCLKPPS           : byte absolute $0E20;
  SSPDATPPS           : byte absolute $0E21;
  SSPSSPPS            : byte absolute $0E22;
  RA0PPS              : byte absolute $0E90;
  RA1PPS              : byte absolute $0E91;
  RA2PPS              : byte absolute $0E92;
  RA4PPS              : byte absolute $0E94;
  RA5PPS              : byte absolute $0E95;
  RC0PPS              : byte absolute $0EA0;
  RC1PPS              : byte absolute $0EA1;
  RC2PPS              : byte absolute $0EA2;
  RC3PPS              : byte absolute $0EA3;
  RC4PPS              : byte absolute $0EA4;
  RC5PPS              : byte absolute $0EA5;
  ICDIO               : byte absolute $0F8C;
  ICDIO_PORT_ICDDAT   : bit  absolute ICDIO.7;
  ICDIO_PORT_ICDCLK   : bit  absolute ICDIO.6;
  ICDIO_LAT_ICDDAT    : bit  absolute ICDIO.5;
  ICDIO_LAT_ICDCLK    : bit  absolute ICDIO.4;
  ICDIO_TRIS_ICDDAT   : bit  absolute ICDIO.3;
  ICDIO_TRIS_ICDCLK   : bit  absolute ICDIO.2;
  ICDCON0             : byte absolute $0F8D;
  ICDCON0_INBUG       : bit  absolute ICDCON0.6;
  ICDCON0_FREEZ       : bit  absolute ICDCON0.5;
  ICDCON0_SSTEP       : bit  absolute ICDCON0.4;
  ICDCON0_DBGINEX     : bit  absolute ICDCON0.3;
  ICDCON0_RSTVEC      : bit  absolute ICDCON0.2;
  ICDSTAT             : byte absolute $0F91;
  ICDSTAT_TRP1HLTF    : bit  absolute ICDSTAT.3;
  ICDSTAT_TRP0HLTF    : bit  absolute ICDSTAT.2;
  ICDSTAT_USRHLTF     : bit  absolute ICDSTAT.1;
  CLKRFRZ             : byte absolute $0F92;
  DEVSEL              : byte absolute $0F95;
  DEVSEL_DEVSEL2      : bit  absolute DEVSEL.2;
  DEVSEL_DEVSEL1      : bit  absolute DEVSEL.1;
  DEVSEL_DEVSEL0      : bit  absolute DEVSEL.0;
  ICDINSTL            : byte absolute $0F96;
  ICDINSTL_DBGIN7     : bit  absolute ICDINSTL.7;
  ICDINSTL_DBGIN6     : bit  absolute ICDINSTL.6;
  ICDINSTL_DBGIN5     : bit  absolute ICDINSTL.5;
  ICDINSTL_DBGIN4     : bit  absolute ICDINSTL.4;
  ICDINSTL_DBGIN3     : bit  absolute ICDINSTL.3;
  ICDINSTL_DBGIN2     : bit  absolute ICDINSTL.2;
  ICDINSTL_DBGIN1     : bit  absolute ICDINSTL.1;
  ICDINSTL_DBGIN0     : bit  absolute ICDINSTL.0;
  ICDINSTH            : byte absolute $0F97;
  ICDINSTH_DBGIN13    : bit  absolute ICDINSTH.5;
  ICDINSTH_DBGIN12    : bit  absolute ICDINSTH.4;
  ICDINSTH_DBGIN11    : bit  absolute ICDINSTH.3;
  ICDINSTH_DBGIN10    : bit  absolute ICDINSTH.2;
  ICDINSTH_DBGIN9     : bit  absolute ICDINSTH.1;
  ICDINSTH_DBGIN8     : bit  absolute ICDINSTH.0;
  ICDBK0CON           : byte absolute $0F9C;
  ICDBK0CON_BKEN      : bit  absolute ICDBK0CON.7;
  ICDBK0CON_BKHLT     : bit  absolute ICDBK0CON.6;
  ICDBK0L             : byte absolute $0F9D;
  ICDBK0L_BKA7        : bit  absolute ICDBK0L.7;
  ICDBK0L_BKA6        : bit  absolute ICDBK0L.6;
  ICDBK0L_BKA5        : bit  absolute ICDBK0L.5;
  ICDBK0L_BKA4        : bit  absolute ICDBK0L.4;
  ICDBK0L_BKA3        : bit  absolute ICDBK0L.3;
  ICDBK0L_BKA2        : bit  absolute ICDBK0L.2;
  ICDBK0L_BKA1        : bit  absolute ICDBK0L.1;
  ICDBK0L_BKA0        : bit  absolute ICDBK0L.0;
  ICDBK0H             : byte absolute $0F9E;
  ICDBK0H_BKA14       : bit  absolute ICDBK0H.6;
  ICDBK0H_BKA13       : bit  absolute ICDBK0H.5;
  ICDBK0H_BKA12       : bit  absolute ICDBK0H.4;
  ICDBK0H_BKA11       : bit  absolute ICDBK0H.3;
  ICDBK0H_BKA10       : bit  absolute ICDBK0H.2;
  ICDBK0H_BKA9        : bit  absolute ICDBK0H.1;
  ICDBK0H_BKA8        : bit  absolute ICDBK0H.0;
  BSRICDSHAD          : byte absolute $0FE3;
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
  {$SET_STATE_RAM '011-013:SFR'}  // PIR1, PIR2, PIR3
  {$SET_STATE_RAM '015-01C:SFR'}  // TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-080:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '08C-08C:SFR'}  // TRISA
  {$SET_STATE_RAM '08E-08E:SFR'}  // TRISC
  {$SET_STATE_RAM '091-093:SFR'}  // PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-09F:SFR'}  // OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-100:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '10C-10C:SFR'}  // LATA
  {$SET_STATE_RAM '10E-10E:SFR'}  // LATC
  {$SET_STATE_RAM '116-117:SFR'}  // BORCON, FVRCON
  {$SET_STATE_RAM '11C-11C:SFR'}  // ZCD1CON
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
  {$SET_STATE_RAM '211-217:SFR'}  // SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '270-27F:GPR'} 
  {$SET_STATE_RAM '280-280:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '28C-28C:SFR'}  // ODCONA
  {$SET_STATE_RAM '28E-28E:SFR'}  // ODCONC
  {$SET_STATE_RAM '291-293:SFR'}  // CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '298-29A:SFR'}  // CCPR2L, CCPR2H, CCP2CON
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
  {$SET_STATE_RAM '470-47F:GPR'} 
  {$SET_STATE_RAM '480-480:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '4F0-4FF:GPR'} 
  {$SET_STATE_RAM '500-500:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '511-511:SFR'}  // OPA1CON
  {$SET_STATE_RAM '515-515:SFR'}  // OPA2CON
  {$SET_STATE_RAM '570-57F:GPR'} 
  {$SET_STATE_RAM '580-580:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '5F0-5FF:GPR'} 
  {$SET_STATE_RAM '600-600:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '670-67F:GPR'} 
  {$SET_STATE_RAM '680-680:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '6F0-6FF:GPR'} 
  {$SET_STATE_RAM '700-700:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '770-77F:GPR'} 
  {$SET_STATE_RAM '780-780:SFR'}  // mapped to INDF0
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
  {$SET_STATE_RAM 'DF0-DFF:GPR'} 
  {$SET_STATE_RAM 'E00-E00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'E0F-E15:SFR'}  // PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, CCP1PPS, CCP2PPS
  {$SET_STATE_RAM 'E20-E22:SFR'}  // SSPCLKPPS, SSPDATPPS, SSPSSPPS
  {$SET_STATE_RAM 'E70-E7F:GPR'} 
  {$SET_STATE_RAM 'E80-E80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'E90-E92:SFR'}  // RA0PPS, RA1PPS, RA2PPS
  {$SET_STATE_RAM 'E94-E95:SFR'}  // RA4PPS, RA5PPS
  {$SET_STATE_RAM 'EA0-EA5:SFR'}  // RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS
  {$SET_STATE_RAM 'EF0-EFF:GPR'} 
  {$SET_STATE_RAM 'F00-F00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'F70-F7F:GPR'} 
  {$SET_STATE_RAM 'F80-F80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'F8C-F8D:SFR'}  // ICDIO, ICDCON0
  {$SET_STATE_RAM 'F91-F92:SFR'}  // ICDSTAT, CLKRFRZ
  {$SET_STATE_RAM 'F95-F97:SFR'}  // DEVSEL, ICDINSTL, ICDINSTH
  {$SET_STATE_RAM 'F9C-F9E:SFR'}  // ICDBK0CON, ICDBK0L, ICDBK0H
  {$SET_STATE_RAM 'FE3-FEB:SFR'}  // BSRICDSHAD, STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
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

  {$MAP_RAM_TO_PIN '00C:1-13,2-12,3-11,4-4,5-3,6-2'} // PORTA
  {$MAP_RAM_TO_PIN '00E:1-10,2-9,3-8,4-7,5-6,6-5'} // PORTC


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
