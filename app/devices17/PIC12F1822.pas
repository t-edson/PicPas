unit PIC12F1822;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F1822'}
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
  PIR1_RCIF                : bit  absolute PIR1.5;
  PIR1_TXIF                : bit  absolute PIR1.4;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_CCP1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_OSFIF               : bit  absolute PIR2.7;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_EEIF                : bit  absolute PIR2.4;
  PIR2_BCL1IF              : bit  absolute PIR2.3;
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
  CPSCON0                  : byte absolute $001E;
  CPSCON0_CPSON            : bit  absolute CPSCON0.7;
  CPSCON0_CPSRM            : bit  absolute CPSCON0.6;
  CPSCON0_CPSRNG1          : bit  absolute CPSCON0.3;
  CPSCON0_CPSRNG0          : bit  absolute CPSCON0.2;
  CPSCON0_CPSOUT           : bit  absolute CPSCON0.1;
  CPSCON0_T0XCS            : bit  absolute CPSCON0.0;
  CPSCON1                  : byte absolute $001F;
  CPSCON1_CPSCH1           : bit  absolute CPSCON1.1;
  CPSCON1_CPSCH0           : bit  absolute CPSCON1.0;
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
  PIE1_RCIE                : bit  absolute PIE1.5;
  PIE1_TXIE                : bit  absolute PIE1.4;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_CCP1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_OSFIE               : bit  absolute PIE2.7;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_EEIE                : bit  absolute PIE2.4;
  PIE2_BCL1IE              : bit  absolute PIE2.3;
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
  OSCSTAT_T1OSCR           : bit  absolute OSCSTAT.7;
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
  CM1CON1_C1NCH0           : bit  absolute CM1CON1.0;
  CMOUT                    : byte absolute $0115;
  CMOUT_MC1OUT             : bit  absolute CMOUT.0;
  BORCON                   : byte absolute $0116;
  BORCON_SBOREN            : bit  absolute BORCON.7;
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
  SRCON0                   : byte absolute $011A;
  SRCON0_SRLEN             : bit  absolute SRCON0.7;
  SRCON0_SRCLK2            : bit  absolute SRCON0.6;
  SRCON0_SRCLK1            : bit  absolute SRCON0.5;
  SRCON0_SRCLK0            : bit  absolute SRCON0.4;
  SRCON0_SRQEN             : bit  absolute SRCON0.3;
  SRCON0_SRNQEN            : bit  absolute SRCON0.2;
  SRCON0_SRPS              : bit  absolute SRCON0.1;
  SRCON0_SRPR              : bit  absolute SRCON0.0;
  SRCON1                   : byte absolute $011B;
  SRCON1_SRSPE             : bit  absolute SRCON1.7;
  SRCON1_SRSCKE            : bit  absolute SRCON1.6;
  SRCON1_SRSC1E            : bit  absolute SRCON1.4;
  SRCON1_SRRPE             : bit  absolute SRCON1.3;
  SRCON1_SRRCKE            : bit  absolute SRCON1.2;
  SRCON1_SRRC1E            : bit  absolute SRCON1.0;
  APFCON                   : byte absolute $011D;
  APFCON_RXDTSEL           : bit  absolute APFCON.7;
  APFCON_SDOSEL            : bit  absolute APFCON.6;
  APFCON_SSSEL             : bit  absolute APFCON.5;
  APFCON_T1GSEL            : bit  absolute APFCON.3;
  APFCON_TXCKSEL           : bit  absolute APFCON.2;
  APFCON_P1BSEL            : bit  absolute APFCON.1;
  APFCON_CCP1SEL           : bit  absolute APFCON.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
  EEADRL                   : byte absolute $0191;
  EEADRH                   : byte absolute $0192;
  EEADRH_EEADRH6           : bit  absolute EEADRH.6;
  EEADRH_EEADRH5           : bit  absolute EEADRH.5;
  EEADRH_EEADRH4           : bit  absolute EEADRH.4;
  EEADRH_EEADRH3           : bit  absolute EEADRH.3;
  EEADRH_EEADRH2           : bit  absolute EEADRH.2;
  EEADRH_EEADRH1           : bit  absolute EEADRH.1;
  EEADRH_EEADRH0           : bit  absolute EEADRH.0;
  EEDATL                   : byte absolute $0193;
  EEDATH                   : byte absolute $0194;
  EEDATH_EEDATH5           : bit  absolute EEDATH.5;
  EEDATH_EEDATH4           : bit  absolute EEDATH.4;
  EEDATH_EEDATH3           : bit  absolute EEDATH.3;
  EEDATH_EEDATH2           : bit  absolute EEDATH.2;
  EEDATH_EEDATH1           : bit  absolute EEDATH.1;
  EEDATH_EEDATH0           : bit  absolute EEDATH.0;
  EECON1                   : byte absolute $0195;
  EECON1_EEPGD             : bit  absolute EECON1.7;
  EECON1_CFGS              : bit  absolute EECON1.6;
  EECON1_LWLO              : bit  absolute EECON1.5;
  EECON1_FREE              : bit  absolute EECON1.4;
  EECON1_WRERR             : bit  absolute EECON1.3;
  EECON1_WREN              : bit  absolute EECON1.2;
  EECON1_WR                : bit  absolute EECON1.1;
  EECON1_RD                : bit  absolute EECON1.0;
  EECON2                   : byte absolute $0196;
  RCREG                    : byte absolute $0199;
  TXREG                    : byte absolute $019A;
  SP1BRGL                  : byte absolute $019B;
  SP1BRGH                  : byte absolute $019C;
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
  WPUA_WPUA2               : bit  absolute WPUA.2;
  WPUA_WPUA1               : bit  absolute WPUA.1;
  WPUA_WPUA0               : bit  absolute WPUA.0;
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
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_P1M1             : bit  absolute CCP1CON.7;
  CCP1CON_P1M0             : bit  absolute CCP1CON.6;
  CCP1CON_DC1B1            : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0            : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3           : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2           : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1           : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0           : bit  absolute CCP1CON.0;
  PWM1CON                  : byte absolute $0294;
  PWM1CON_P1RSEN           : bit  absolute PWM1CON.7;
  PWM1CON_P1DC6            : bit  absolute PWM1CON.6;
  PWM1CON_P1DC5            : bit  absolute PWM1CON.5;
  PWM1CON_P1DC4            : bit  absolute PWM1CON.4;
  PWM1CON_P1DC3            : bit  absolute PWM1CON.3;
  PWM1CON_P1DC2            : bit  absolute PWM1CON.2;
  PWM1CON_P1DC1            : bit  absolute PWM1CON.1;
  PWM1CON_P1DC0            : bit  absolute PWM1CON.0;
  CCP1AS                   : byte absolute $0295;
  CCP1AS_CCP1ASE           : bit  absolute CCP1AS.7;
  CCP1AS_CCP1AS2           : bit  absolute CCP1AS.6;
  CCP1AS_CCP1AS1           : bit  absolute CCP1AS.5;
  CCP1AS_CCP1AS0           : bit  absolute CCP1AS.4;
  CCP1AS_PSS1AC1           : bit  absolute CCP1AS.3;
  CCP1AS_PSS1AC0           : bit  absolute CCP1AS.2;
  CCP1AS_PSS1BD1           : bit  absolute CCP1AS.1;
  CCP1AS_PSS1BD0           : bit  absolute CCP1AS.0;
  PSTR1CON                 : byte absolute $0296;
  PSTR1CON_STR1SYNC        : bit  absolute PSTR1CON.4;
  PSTR1CON_STR1D           : bit  absolute PSTR1CON.3;
  PSTR1CON_STR1C           : bit  absolute PSTR1CON.2;
  PSTR1CON_STR1B           : bit  absolute PSTR1CON.1;
  PSTR1CON_STR1A           : bit  absolute PSTR1CON.0;
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
  CLKRCON                  : byte absolute $039A;
  CLKRCON_CLKREN           : bit  absolute CLKRCON.7;
  CLKRCON_CLKROE           : bit  absolute CLKRCON.6;
  CLKRCON_CLKRSLR          : bit  absolute CLKRCON.5;
  CLKRCON_CLKRDC1          : bit  absolute CLKRCON.4;
  CLKRCON_CLKRDC0          : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2         : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1         : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0         : bit  absolute CLKRCON.0;
  MDCON                    : byte absolute $039C;
  MDCON_MDEN               : bit  absolute MDCON.7;
  MDCON_MDOE               : bit  absolute MDCON.6;
  MDCON_MDSLR              : bit  absolute MDCON.5;
  MDCON_MDOPOL             : bit  absolute MDCON.4;
  MDCON_MDOUT              : bit  absolute MDCON.3;
  MDCON_MDBIT              : bit  absolute MDCON.0;
  MDSRC                    : byte absolute $039D;
  MDSRC_MDMSODIS           : bit  absolute MDSRC.7;
  MDSRC_MDMS3              : bit  absolute MDSRC.3;
  MDSRC_MDMS2              : bit  absolute MDSRC.2;
  MDSRC_MDMS1              : bit  absolute MDSRC.1;
  MDSRC_MDMS0              : bit  absolute MDSRC.0;
  MDCARL                   : byte absolute $039E;
  MDCARL_MDCLODIS          : bit  absolute MDCARL.7;
  MDCARL_MDCLPOL           : bit  absolute MDCARL.6;
  MDCARL_MDCLSYNC          : bit  absolute MDCARL.5;
  MDCARL_MDCL3             : bit  absolute MDCARL.3;
  MDCARL_MDCL2             : bit  absolute MDCARL.2;
  MDCARL_MDCL1             : bit  absolute MDCARL.1;
  MDCARL_MDCL0             : bit  absolute MDCARL.0;
  MDCARH                   : byte absolute $039F;
  MDCARH_MDCHODIS          : bit  absolute MDCARH.7;
  MDCARH_MDCHPOL           : bit  absolute MDCARH.6;
  MDCARH_MDCHSYNC          : bit  absolute MDCARH.5;
  MDCARH_MDCH3             : bit  absolute MDCARH.3;
  MDCARH_MDCH2             : bit  absolute MDCARH.2;
  MDCARH_MDCH1             : bit  absolute MDCARH.1;
  MDCARH_MDCH0             : bit  absolute MDCARH.0;
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
  {$SET_STATE_RAM '011-012:SFR'}            // Bank 0 : PIR1, PIR2
  {$SET_STATE_RAM '015-01C:SFR'}            // Bank 0 : TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '01E-01F:SFR'}            // Bank 0 : CPSCON0, CPSCON1
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '091-092:SFR'}            // Bank 1 : PIE1, PIE2
  {$SET_STATE_RAM '095-09E:SFR'}            // Bank 1 : OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '111-112:SFR'}            // Bank 2 : CM1CON0, CM1CON1
  {$SET_STATE_RAM '115-11B:SFR'}            // Bank 2 : CMOUT, BORCON, FVRCON, DACCON0, DACCON1, SRCON0, SRCON1
  {$SET_STATE_RAM '11D-11D:SFR'}            // Bank 2 : APFCON
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '191-196:SFR'}            // Bank 3 : EEADRL, EEADRH, EEDATL, EEDATH, EECON1, EECON2
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RCREG, TXREG, SP1BRGL, SP1BRGH, RCSTA, TXSTA, BAUDCON
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '291-296:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, PWM1CON, CCP1AS, PSTR1CON
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '39A-39A:SFR'}            // Bank 7 : CLKRCON
  {$SET_STATE_RAM '39C-39F:SFR'}            // Bank 7 : MDCON, MDSRC, MDCARL, MDCARH
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:B8'} // PIR2 bits 6,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:8F'} // CPSCON0 bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:03'} // CPSCON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:B8'} // PIE2 bits 6,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:CF'} // PCON bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:F1'} // CM1CON1 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:01'} // CMOUT bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:81'} // BORCON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:EC'} // DACCON0 bits 4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11B:DD'} // SRCON1 bits 5,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11D:EF'} // APFCON bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUDCON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '296:1F'} // PSTR1CON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39C:F9'} // MDCON bits 2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:8F'} // MDSRC bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:EF'} // MDCARL bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:EF'} // MDCARH bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '192:80'} // EEADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : RA5/CLKIN/OSC1/T1OSI/T1CKI/SRNQ/P1A/CCP1/DT/RX
  // Pin  3 : RA4/AN3/CPS3/OSC2/CLKOUT/T1OSO/C1IN1-/CLKR/SDO/CK/TX/P1B/T1G/MDCIN2
  // Pin  4 : RA3/SS/T1G/VPP/MCLR
  // Pin  5 : RA2/AN2/CPS2/C1OUT/SRQ/T0CKI/CCP1/P1A/FLT0/SDA/SDI/INT/MDCIN1
  // Pin  6 : RA1/AN1/CPS1/VREF/C1IN0-/SRI/RX/DT/SCL/SCK/MDMIN/ICSPCLK
  // Pin  7 : RA0/AN0/CPS0/C1IN+/DACOUT/TX/CK/SDO/SS/P1B/MDOUT/ICSPDAT
  // Pin  8 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-7,1-6,2-5,3-4,4-3,5-2'} // PORTA


  // -- Bits Configuration --

  // FOSC : Oscillator Selection
  {$define _FOSC_ECH     = $3FFF}  // ECH, External Clock, High Power Mode (4-32 MHz): device clock supplied to CLKIN pin
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

  // CPD : Data Memory Code Protection
  {$define _CPD_OFF      = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON       = $3EFF}  // Data memory code protection is enabled

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
  {$define _WRT_BOOT     = $3FFE}  // 000h to 1FFh write protected, 200h to 7FFh may be modified by EECON control
  {$define _WRT_HALF     = $3FFD}  // 000h to 3FFh write protected, 400h to 7FFh may be modified by EECON control
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

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
