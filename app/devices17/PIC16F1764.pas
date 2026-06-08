unit PIC16F1764;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F1764'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 4096}

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
  PIR2_BCL1IF              : bit  absolute PIR2.3;
  PIR3                     : byte absolute $0013;
  PIR3_PWM5IF              : bit  absolute PIR3.6;
  PIR3_COG1IF              : bit  absolute PIR3.5;
  PIR3_ZCDIF               : bit  absolute PIR3.4;
  PIR3_CLC3IF              : bit  absolute PIR3.2;
  PIR3_CLC2IF              : bit  absolute PIR3.1;
  PIR3_CLC1IF              : bit  absolute PIR3.0;
  PIR4                     : byte absolute $0014;
  PIR4_TMR5GIF             : bit  absolute PIR4.5;
  PIR4_TMR5IF              : bit  absolute PIR4.4;
  PIR4_TMR3GIF             : bit  absolute PIR4.3;
  PIR4_TMR3IF              : bit  absolute PIR4.2;
  PIR4_TMR6IF              : bit  absolute PIR4.1;
  PIR4_TMR4IF              : bit  absolute PIR4.0;
  TMR0                     : byte absolute $0015;
  TMR1L                    : byte absolute $0016;
  TMR1H                    : byte absolute $0017;
  T1CON                    : byte absolute $0018;
  T1CON_CS1                : bit  absolute T1CON.7;
  T1CON_CS0                : bit  absolute T1CON.6;
  T1CON_CKPS1              : bit  absolute T1CON.5;
  T1CON_CKPS0              : bit  absolute T1CON.4;
  T1CON_OSCEN              : bit  absolute T1CON.3;
  T1CON_nSYNC              : bit  absolute T1CON.2;
  T1CON_ON                 : bit  absolute T1CON.0;
  T1GCON                   : byte absolute $0019;
  T1GCON_GE                : bit  absolute T1GCON.7;
  T1GCON_GPOL              : bit  absolute T1GCON.6;
  T1GCON_GTM               : bit  absolute T1GCON.5;
  T1GCON_GSPM              : bit  absolute T1GCON.4;
  T1GCON_GGO_nDONE         : bit  absolute T1GCON.3;
  T1GCON_GVAL              : bit  absolute T1GCON.2;
  T1GCON_GSS1              : bit  absolute T1GCON.1;
  T1GCON_GSS0              : bit  absolute T1GCON.0;
  T2TMR                    : byte absolute $001A;
  T2PR                     : byte absolute $001B;
  T2CON                    : byte absolute $001C;
  T2CON_CKPS2              : bit  absolute T2CON.6;
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
  T2RST                    : byte absolute $001F;
  T2RST_RSEL3              : bit  absolute T2RST.3;
  T2RST_RSEL2              : bit  absolute T2RST.2;
  T2RST_RSEL1              : bit  absolute T2RST.1;
  T2RST_RSEL0              : bit  absolute T2RST.0;
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
  PIE2_BCL1IE              : bit  absolute PIE2.3;
  PIE3                     : byte absolute $0093;
  PIE3_PWM5IE              : bit  absolute PIE3.6;
  PIE3_COGIE               : bit  absolute PIE3.5;
  PIE3_ZCDIE               : bit  absolute PIE3.4;
  PIE3_CLC3IE              : bit  absolute PIE3.2;
  PIE3_CLC2IE              : bit  absolute PIE3.1;
  PIE3_CLC1IE              : bit  absolute PIE3.0;
  PIE4                     : byte absolute $0094;
  PIE4_TMR5GIE             : bit  absolute PIE4.5;
  PIE4_TMR5IE              : bit  absolute PIE4.4;
  PIE4_TMR3GIE             : bit  absolute PIE4.3;
  PIE4_TMR3IE              : bit  absolute PIE4.2;
  PIE4_TMR6IE              : bit  absolute PIE4.1;
  PIE4_TMR4IE              : bit  absolute PIE4.0;
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
  ADCON2_TRIGSEL4          : bit  absolute ADCON2.7;
  ADCON2_TRIGSEL3          : bit  absolute ADCON2.6;
  ADCON2_TRIGSEL2          : bit  absolute ADCON2.5;
  ADCON2_TRIGSEL1          : bit  absolute ADCON2.4;
  ADCON2_TRIGSEL0          : bit  absolute ADCON2.3;
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
  CMOUT                    : byte absolute $010F;
  CMOUT_MC2OUT             : bit  absolute CMOUT.1;
  CMOUT_MC1OUT             : bit  absolute CMOUT.0;
  CM1CON0                  : byte absolute $0110;
  CM1CON0_OUT              : bit  absolute CM1CON0.6;
  CM1CON0_POL              : bit  absolute CM1CON0.4;
  CM1CON0_ZLF              : bit  absolute CM1CON0.3;
  CM1CON0_Reserved         : bit  absolute CM1CON0.2;
  CM1CON0_HYS              : bit  absolute CM1CON0.1;
  CM1CON0_SYNC             : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0111;
  CM1CON1_INTP             : bit  absolute CM1CON1.1;
  CM1CON1_INTN             : bit  absolute CM1CON1.0;
  CM1NSEL                  : byte absolute $0112;
  CM1NSEL_NCH2             : bit  absolute CM1NSEL.2;
  CM1NSEL_NCH1             : bit  absolute CM1NSEL.1;
  CM1NSEL_NCH0             : bit  absolute CM1NSEL.0;
  CM1PSEL                  : byte absolute $0113;
  CM1PSEL_PCH3             : bit  absolute CM1PSEL.3;
  CM1PSEL_PCH2             : bit  absolute CM1PSEL.2;
  CM1PSEL_PCH1             : bit  absolute CM1PSEL.1;
  CM1PSEL_PCH0             : bit  absolute CM1PSEL.0;
  CM2CON0                  : byte absolute $0114;
  CM2CON1                  : byte absolute $0115;
  CM2NSEL                  : byte absolute $0116;
  CM2PSEL                  : byte absolute $0117;
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
  BORCON                   : byte absolute $021D;
  BORCON_SBOREN            : bit  absolute BORCON.7;
  BORCON_BORFS             : bit  absolute BORCON.6;
  BORCON_BORRDY            : bit  absolute BORCON.0;
  FVRCON                   : byte absolute $021E;
  FVRCON_FVREN             : bit  absolute FVRCON.7;
  FVRCON_FVRRDY            : bit  absolute FVRCON.6;
  FVRCON_TSEN              : bit  absolute FVRCON.5;
  FVRCON_TSRNG             : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1           : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0           : bit  absolute FVRCON.2;
  FVRCON_ADFVR1            : bit  absolute FVRCON.1;
  FVRCON_ADFVR0            : bit  absolute FVRCON.0;
  ZCD1CON                  : byte absolute $021F;
  ZCD1CON_ZCD1EN           : bit  absolute ZCD1CON.7;
  ZCD1CON_ZCD1OUT          : bit  absolute ZCD1CON.5;
  ZCD1CON_ZCD1POL          : bit  absolute ZCD1CON.4;
  ZCD1CON_ZCD1INTP         : bit  absolute ZCD1CON.1;
  ZCD1CON_ZCD1INTN         : bit  absolute ZCD1CON.0;
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
  CCP1CON_FMT              : bit  absolute CCP1CON.4;
  CCP1CAP                  : byte absolute $0294;
  CCPTMRS                  : byte absolute $029E;
  CCPTMRS_P3TSEL1          : bit  absolute CCPTMRS.5;
  CCPTMRS_P3TSEL0          : bit  absolute CCPTMRS.4;
  CCPTMRS_C1TSEL1          : bit  absolute CCPTMRS.1;
  CCPTMRS_C1TSEL0          : bit  absolute CCPTMRS.0;
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
  MD1CON0                  : byte absolute $039B;
  MD1CON0_OPOL             : bit  absolute MD1CON0.4;
  MD1CON0_BIT              : bit  absolute MD1CON0.0;
  MD1CON1                  : byte absolute $039C;
  MD1CON1_CHPOL            : bit  absolute MD1CON1.5;
  MD1CON1_CHSYNC           : bit  absolute MD1CON1.4;
  MD1CON1_CLPOL            : bit  absolute MD1CON1.1;
  MD1CON1_CLSYNC           : bit  absolute MD1CON1.0;
  MD1SRC                   : byte absolute $039D;
  MD1SRC_MS4               : bit  absolute MD1SRC.4;
  MD1SRC_MS3               : bit  absolute MD1SRC.3;
  MD1SRC_MS2               : bit  absolute MD1SRC.2;
  MD1SRC_MS1               : bit  absolute MD1SRC.1;
  MD1SRC_MS0               : bit  absolute MD1SRC.0;
  MD1CARL                  : byte absolute $039E;
  MD1CARL_CL3              : bit  absolute MD1CARL.3;
  MD1CARL_CL2              : bit  absolute MD1CARL.2;
  MD1CARL_CL1              : bit  absolute MD1CARL.1;
  MD1CARL_CL0              : bit  absolute MD1CARL.0;
  MD1CARH                  : byte absolute $039F;
  MD1CARH_CH3              : bit  absolute MD1CARH.3;
  MD1CARH_CH2              : bit  absolute MD1CARH.2;
  MD1CARH_CH1              : bit  absolute MD1CARH.1;
  MD1CARH_CH0              : bit  absolute MD1CARH.0;
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
  T3GCON                   : byte absolute $0496;
  TMR5L                    : byte absolute $049A;
  TMR5H                    : byte absolute $049B;
  T5CON                    : byte absolute $049C;
  T5GCON                   : byte absolute $049D;
  OPA1NCHS                 : byte absolute $050F;
  OPA1PCHS                 : byte absolute $0510;
  OPA1CON                  : byte absolute $0511;
  OPA1CON_UG               : bit  absolute OPA1CON.4;
  OPA1CON_ORPOL            : bit  absolute OPA1CON.2;
  OPA1CON_ORM1             : bit  absolute OPA1CON.1;
  OPA1CON_ORM0             : bit  absolute OPA1CON.0;
  OPA1ORS                  : byte absolute $0512;
  DACLD                    : byte absolute $0590;
  DACLD_DAC1LD             : bit  absolute DACLD.0;
  DAC1CON0                 : byte absolute $0591;
  DAC1CON0_FM              : bit  absolute DAC1CON0.6;
  DAC1CON0_OE1             : bit  absolute DAC1CON0.5;
  DAC1CON0_PSS1            : bit  absolute DAC1CON0.3;
  DAC1CON0_PSS0            : bit  absolute DAC1CON0.2;
  DAC1CON0_NSS1            : bit  absolute DAC1CON0.1;
  DAC1CON0_NSS0            : bit  absolute DAC1CON0.0;
  DAC1REFL                 : byte absolute $0592;
  DAC1REFL_REF7            : bit  absolute DAC1REFL.7;
  DAC1REFL_REF6            : bit  absolute DAC1REFL.6;
  DAC1REFL_REF5            : bit  absolute DAC1REFL.5;
  DAC1REFL_REF4            : bit  absolute DAC1REFL.4;
  DAC1REFL_REF3            : bit  absolute DAC1REFL.3;
  DAC1REFL_REF2            : bit  absolute DAC1REFL.2;
  DAC1REFL_REF1            : bit  absolute DAC1REFL.1;
  DAC1REFL_REF0            : bit  absolute DAC1REFL.0;
  DAC1REFH                 : byte absolute $0593;
  DAC1REFH_REF15           : bit  absolute DAC1REFH.7;
  DAC1REFH_REF14           : bit  absolute DAC1REFH.6;
  DAC1REFH_REF13           : bit  absolute DAC1REFH.5;
  DAC1REFH_REF12           : bit  absolute DAC1REFH.4;
  DAC1REFH_REF11           : bit  absolute DAC1REFH.3;
  DAC1REFH_REF10           : bit  absolute DAC1REFH.2;
  DAC1REFH_REF9            : bit  absolute DAC1REFH.1;
  DAC1REFH_REF8            : bit  absolute DAC1REFH.0;
  DAC3CON0                 : byte absolute $0597;
  DAC3CON0_NSS             : bit  absolute DAC3CON0.0;
  DAC3REF                  : byte absolute $0598;
  DAC3REF_DACR4            : bit  absolute DAC3REF.4;
  DAC3REF_DACR3            : bit  absolute DAC3REF.3;
  DAC3REF_DACR2            : bit  absolute DAC3REF.2;
  DAC3REF_DACR1            : bit  absolute DAC3REF.1;
  DAC3REF_DACR0            : bit  absolute DAC3REF.0;
  PWM3DCL                  : byte absolute $0617;
  PWM3DCL_DC1              : bit  absolute PWM3DCL.7;
  PWM3DCL_DC0              : bit  absolute PWM3DCL.6;
  PWM3DCH                  : byte absolute $0618;
  PWM3CON                  : byte absolute $0619;
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
  PRG1RTSS                 : byte absolute $0794;
  PRG1FTSS                 : byte absolute $0795;
  PRG1INS                  : byte absolute $0796;
  PRG1CON0                 : byte absolute $0797;
  PRG1CON0_FEDG            : bit  absolute PRG1CON0.5;
  PRG1CON0_REDG            : bit  absolute PRG1CON0.4;
  PRG1CON0_OS              : bit  absolute PRG1CON0.1;
  PRG1CON0_GO              : bit  absolute PRG1CON0.0;
  PRG1CON1                 : byte absolute $0798;
  PRG1CON1_RDY             : bit  absolute PRG1CON1.2;
  PRG1CON1_FPOL            : bit  absolute PRG1CON1.1;
  PRG1CON1_RPOL            : bit  absolute PRG1CON1.0;
  PRG1CON2                 : byte absolute $0799;
  PRG1CON2_ISET4           : bit  absolute PRG1CON2.4;
  PRG1CON2_ISET3           : bit  absolute PRG1CON2.3;
  PRG1CON2_ISET2           : bit  absolute PRG1CON2.2;
  PRG1CON2_ISET1           : bit  absolute PRG1CON2.1;
  PRG1CON2_ISET0           : bit  absolute PRG1CON2.0;
  PWMEN                    : byte absolute $0D8E;
  PWMEN_MPWM5EN            : bit  absolute PWMEN.4;
  PWMLD                    : byte absolute $0D8F;
  PWMLD_MPWM5LD            : bit  absolute PWMLD.4;
  PWMOUT                   : byte absolute $0D90;
  PWMOUT_MPWM5OUT          : bit  absolute PWMOUT.4;
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
  PWM5OFCON                : byte absolute $0DA0;
  PWM5OFCON_OFO            : bit  absolute PWM5OFCON.4;
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
  COG1INPPS                : byte absolute $0E16;
  COG1INPPS_COG1INPPS4     : bit  absolute COG1INPPS.4;
  COG1INPPS_COG1INPPS3     : bit  absolute COG1INPPS.3;
  COG1INPPS_COG1INPPS2     : bit  absolute COG1INPPS.2;
  COG1INPPS_COG1INPPS1     : bit  absolute COG1INPPS.1;
  COG1INPPS_COG1INPPS0     : bit  absolute COG1INPPS.0;
  T2INPPS                  : byte absolute $0E19;
  T2INPPS_T2INPPS4         : bit  absolute T2INPPS.4;
  T2INPPS_T2INPPS3         : bit  absolute T2INPPS.3;
  T2INPPS_T2INPPS2         : bit  absolute T2INPPS.2;
  T2INPPS_T2INPPS1         : bit  absolute T2INPPS.1;
  T2INPPS_T2INPPS0         : bit  absolute T2INPPS.0;
  T3CKIPPS                 : byte absolute $0E1A;
  T3CKIPPS_T3CKIPPS4       : bit  absolute T3CKIPPS.4;
  T3CKIPPS_T3CKIPPS3       : bit  absolute T3CKIPPS.3;
  T3CKIPPS_T3CKIPPS2       : bit  absolute T3CKIPPS.2;
  T3CKIPPS_T3CKIPPS1       : bit  absolute T3CKIPPS.1;
  T3CKIPPS_T3CKIPPS0       : bit  absolute T3CKIPPS.0;
  T3GPPS                   : byte absolute $0E1B;
  T3GPPS_T3GPPS4           : bit  absolute T3GPPS.4;
  T3GPPS_T3GPPS3           : bit  absolute T3GPPS.3;
  T3GPPS_T3GPPS2           : bit  absolute T3GPPS.2;
  T3GPPS_T3GPPS1           : bit  absolute T3GPPS.1;
  T3GPPS_T3GPPS0           : bit  absolute T3GPPS.0;
  T4INPPS                  : byte absolute $0E1C;
  T4INPPS_T4INPPS4         : bit  absolute T4INPPS.4;
  T4INPPS_T4INPPS3         : bit  absolute T4INPPS.3;
  T4INPPS_T4INPPS2         : bit  absolute T4INPPS.2;
  T4INPPS_T4INPPS1         : bit  absolute T4INPPS.1;
  T4INPPS_T4INPPS0         : bit  absolute T4INPPS.0;
  T5CKIPPS                 : byte absolute $0E1D;
  T5CKIPPS_T5CKIPPS4       : bit  absolute T5CKIPPS.4;
  T5CKIPPS_T5CKIPPS3       : bit  absolute T5CKIPPS.3;
  T5CKIPPS_T5CKIPPS2       : bit  absolute T5CKIPPS.2;
  T5CKIPPS_T5CKIPPS1       : bit  absolute T5CKIPPS.1;
  T5CKIPPS_T5CKIPPS0       : bit  absolute T5CKIPPS.0;
  T5GPPS                   : byte absolute $0E1E;
  T5GPPS_T5GPPS4           : bit  absolute T5GPPS.4;
  T5GPPS_T5GPPS3           : bit  absolute T5GPPS.3;
  T5GPPS_T5GPPS2           : bit  absolute T5GPPS.2;
  T5GPPS_T5GPPS1           : bit  absolute T5GPPS.1;
  T5GPPS_T5GPPS0           : bit  absolute T5GPPS.0;
  T6INPPS                  : byte absolute $0E1F;
  T6INPPS_T6INPPS4         : bit  absolute T6INPPS.4;
  T6INPPS_T6INPPS3         : bit  absolute T6INPPS.3;
  T6INPPS_T6INPPS2         : bit  absolute T6INPPS.2;
  T6INPPS_T6INPPS1         : bit  absolute T6INPPS.1;
  T6INPPS_T6INPPS0         : bit  absolute T6INPPS.0;
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
  PRG1RPPS                 : byte absolute $0E2C;
  PRG1RPPS_PRG1RPPS4       : bit  absolute PRG1RPPS.4;
  PRG1RPPS_PRG1RPPS3       : bit  absolute PRG1RPPS.3;
  PRG1RPPS_PRG1RPPS2       : bit  absolute PRG1RPPS.2;
  PRG1RPPS_PRG1RPPS1       : bit  absolute PRG1RPPS.1;
  PRG1RPPS_PRG1RPPS0       : bit  absolute PRG1RPPS.0;
  PRG1FPPS                 : byte absolute $0E2D;
  PRG1FPPS_PRG1FPPS4       : bit  absolute PRG1FPPS.4;
  PRG1FPPS_PRG1FPPS3       : bit  absolute PRG1FPPS.3;
  PRG1FPPS_PRG1FPPS2       : bit  absolute PRG1FPPS.2;
  PRG1FPPS_PRG1FPPS1       : bit  absolute PRG1FPPS.1;
  PRG1FPPS_PRG1FPPS0       : bit  absolute PRG1FPPS.0;
  MD1CHPPS                 : byte absolute $0E30;
  MD1CHPPS_MD1CHPPS4       : bit  absolute MD1CHPPS.4;
  MD1CHPPS_MD1CHPPS3       : bit  absolute MD1CHPPS.3;
  MD1CHPPS_MD1CHPPS2       : bit  absolute MD1CHPPS.2;
  MD1CHPPS_MD1CHPPS1       : bit  absolute MD1CHPPS.1;
  MD1CHPPS_MD1CHPPS0       : bit  absolute MD1CHPPS.0;
  MD1CLPPS                 : byte absolute $0E31;
  MD1CLPPS_MD1CLPPS4       : bit  absolute MD1CLPPS.4;
  MD1CLPPS_MD1CLPPS3       : bit  absolute MD1CLPPS.3;
  MD1CLPPS_MD1CLPPS2       : bit  absolute MD1CLPPS.2;
  MD1CLPPS_MD1CLPPS1       : bit  absolute MD1CLPPS.1;
  MD1CLPPS_MD1CLPPS0       : bit  absolute MD1CLPPS.0;
  MD1MODPPS                : byte absolute $0E32;
  MD1MODPPS_MD1MODPPS4     : bit  absolute MD1MODPPS.4;
  MD1MODPPS_MD1MODPPS3     : bit  absolute MD1MODPPS.3;
  MD1MODPPS_MD1MODPPS2     : bit  absolute MD1MODPPS.2;
  MD1MODPPS_MD1MODPPS1     : bit  absolute MD1MODPPS.1;
  MD1MODPPS_MD1MODPPS0     : bit  absolute MD1MODPPS.0;
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
  CLC1SEL0_LC1D1S4         : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S3         : bit  absolute CLC1SEL0.3;
  CLC1SEL0_LC1D1S2         : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1         : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0         : bit  absolute CLC1SEL0.0;
  CLC1SEL1                 : byte absolute $0F13;
  CLC1SEL1_LC1D2S4         : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D2S3         : bit  absolute CLC1SEL1.3;
  CLC1SEL1_LC1D2S2         : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D2S1         : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D2S0         : bit  absolute CLC1SEL1.0;
  CLC1SEL2                 : byte absolute $0F14;
  CLC1SEL2_LC1D3S4         : bit  absolute CLC1SEL2.4;
  CLC1SEL2_LC1D3S3         : bit  absolute CLC1SEL2.3;
  CLC1SEL2_LC1D3S2         : bit  absolute CLC1SEL2.2;
  CLC1SEL2_LC1D3S1         : bit  absolute CLC1SEL2.1;
  CLC1SEL2_LC1D3S0         : bit  absolute CLC1SEL2.0;
  CLC1SEL3                 : byte absolute $0F15;
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
  CLC2SEL0_LC2D1S4         : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S3         : bit  absolute CLC2SEL0.3;
  CLC2SEL0_LC2D1S2         : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1         : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0         : bit  absolute CLC2SEL0.0;
  CLC2SEL1                 : byte absolute $0F1D;
  CLC2SEL1_LC2D2S4         : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D2S3         : bit  absolute CLC2SEL1.3;
  CLC2SEL1_LC2D2S2         : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D2S1         : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D2S0         : bit  absolute CLC2SEL1.0;
  CLC2SEL2                 : byte absolute $0F1E;
  CLC2SEL2_LC2D3S4         : bit  absolute CLC2SEL2.4;
  CLC2SEL2_LC2D3S3         : bit  absolute CLC2SEL2.3;
  CLC2SEL2_LC2D3S2         : bit  absolute CLC2SEL2.2;
  CLC2SEL2_LC2D3S1         : bit  absolute CLC2SEL2.1;
  CLC2SEL2_LC2D3S0         : bit  absolute CLC2SEL2.0;
  CLC2SEL3                 : byte absolute $0F1F;
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
  CLC3SEL0_LC3D1S4         : bit  absolute CLC3SEL0.4;
  CLC3SEL0_LC3D1S3         : bit  absolute CLC3SEL0.3;
  CLC3SEL0_LC3D1S2         : bit  absolute CLC3SEL0.2;
  CLC3SEL0_LC3D1S1         : bit  absolute CLC3SEL0.1;
  CLC3SEL0_LC3D1S0         : bit  absolute CLC3SEL0.0;
  CLC3SEL1                 : byte absolute $0F27;
  CLC3SEL1_LC3D2S4         : bit  absolute CLC3SEL1.4;
  CLC3SEL1_LC3D2S3         : bit  absolute CLC3SEL1.3;
  CLC3SEL1_LC3D2S2         : bit  absolute CLC3SEL1.2;
  CLC3SEL1_LC3D2S1         : bit  absolute CLC3SEL1.1;
  CLC3SEL1_LC3D2S0         : bit  absolute CLC3SEL1.0;
  CLC3SEL2                 : byte absolute $0F28;
  CLC3SEL2_LC3D3S4         : bit  absolute CLC3SEL2.4;
  CLC3SEL2_LC3D3S3         : bit  absolute CLC3SEL2.3;
  CLC3SEL2_LC3D3S2         : bit  absolute CLC3SEL2.2;
  CLC3SEL2_LC3D3S1         : bit  absolute CLC3SEL2.1;
  CLC3SEL2_LC3D3S0         : bit  absolute CLC3SEL2.0;
  CLC3SEL3                 : byte absolute $0F29;
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
  {$SET_STATE_RAM '011-01F:SFR'}            // Bank 0 : PIR1, PIR2, PIR3, PIR4, TMR0, TMR1L, TMR1H, T1CON, T1GCON, T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08C:SFR'}            // Bank 1 : TRISA
  {$SET_STATE_RAM '08E-08E:SFR'}            // Bank 1 : TRISC
  {$SET_STATE_RAM '091-09F:SFR'}            // Bank 1 : PIE1, PIE2, PIE3, PIE4, OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10C:SFR'}            // Bank 2 : LATA
  {$SET_STATE_RAM '10E-117:SFR'}            // Bank 2 : LATC, CMOUT, CM1CON0, CM1CON1, CM1NSEL, CM1PSEL, CM2CON0, CM2CON1, CM2NSEL, CM2PSEL
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18C:SFR'}            // Bank 3 : ANSELA
  {$SET_STATE_RAM '18E-18E:SFR'}            // Bank 3 : ANSELC
  {$SET_STATE_RAM '191-197:SFR'}            // Bank 3 : PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20C:SFR'}            // Bank 4 : WPUA
  {$SET_STATE_RAM '20E-20E:SFR'}            // Bank 4 : WPUC
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '21D-21F:SFR'}            // Bank 4 : BORCON, FVRCON, ZCD1CON
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28C:SFR'}            // Bank 5 : ODCONA
  {$SET_STATE_RAM '28E-28E:SFR'}            // Bank 5 : ODCONC
  {$SET_STATE_RAM '291-294:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, CCP1CAP
  {$SET_STATE_RAM '29E-29E:SFR'}            // Bank 5 : CCPTMRS
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30C:SFR'}            // Bank 6 : SLRCONA
  {$SET_STATE_RAM '30E-30E:SFR'}            // Bank 6 : SLRCONC
  {$SET_STATE_RAM '320-32F:GPR'}           
  {$SET_STATE_RAM '38C-38C:SFR'}            // Bank 7 : INLVLA
  {$SET_STATE_RAM '38E-38E:SFR'}            // Bank 7 : INLVLC
  {$SET_STATE_RAM '391-393:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '397-399:SFR'}            // Bank 7 : IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '39B-39F:SFR'}            // Bank 7 : MD1CON0, MD1CON1, MD1SRC, MD1CARL, MD1CARH
  {$SET_STATE_RAM '40E-40E:SFR'}            // Bank 8 : HIDRVC
  {$SET_STATE_RAM '413-418:SFR'}            // Bank 8 : T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '41A-41F:SFR'}            // Bank 8 : T6TMR, T6PR, T6CON, T6HLT, T6CLKCON, T6RST
  {$SET_STATE_RAM '493-496:SFR'}            // Bank 9 : TMR3L, TMR3H, T3CON, T3GCON
  {$SET_STATE_RAM '49A-49D:SFR'}            // Bank 9 : TMR5L, TMR5H, T5CON, T5GCON
  {$SET_STATE_RAM '50F-512:SFR'}            // Bank 10 : OPA1NCHS, OPA1PCHS, OPA1CON, OPA1ORS
  {$SET_STATE_RAM '590-593:SFR'}            // Bank 11 : DACLD, DAC1CON0, DAC1REFL, DAC1REFH
  {$SET_STATE_RAM '597-598:SFR'}            // Bank 11 : DAC3CON0, DAC3REF
  {$SET_STATE_RAM '617-619:SFR'}            // Bank 12 : PWM3DCL, PWM3DCH, PWM3CON
  {$SET_STATE_RAM '68D-69F:SFR'}            // Bank 13 : COG1PHR, COG1PHF, COG1BLKR, COG1BLKF, COG1DBR, COG1DBF, COG1CON0, COG1CON1, COG1RIS0, COG1RIS1, COG1RSIM0, COG1RSIM1, COG1FIS0, COG1FIS1, COG1FSIM0, COG1FSIM1, COG1ASD0, COG1ASD1, COG1STR
  {$SET_STATE_RAM '794-799:SFR'}            // Bank 15 : PRG1RTSS, PRG1FTSS, PRG1INS, PRG1CON0, PRG1CON1, PRG1CON2
  {$SET_STATE_RAM 'D8E-DA0:SFR'}            // Bank 27 : PWMEN, PWMLD, PWMOUT, PWM5PHL, PWM5PHH, PWM5DCL, PWM5DCH, PWM5PRL, PWM5PRH, PWM5OFL, PWM5OFH, PWM5TMRL, PWM5TMRH, PWM5CON, PWM5INTE, PWM5INTF, PWM5CLKCON, PWM5LDCON, PWM5OFCON
  {$SET_STATE_RAM 'E0F-E14:SFR'}            // Bank 28 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, CCP1PPS
  {$SET_STATE_RAM 'E16-E16:SFR'}            // Bank 28 : COG1INPPS
  {$SET_STATE_RAM 'E19-E22:SFR'}            // Bank 28 : T2INPPS, T3CKIPPS, T3GPPS, T4INPPS, T5CKIPPS, T5GPPS, T6INPPS, SSPCLKPPS, SSPDATPPS, SSPSSPPS
  {$SET_STATE_RAM 'E24-E25:SFR'}            // Bank 28 : RXPPS, CKPPS
  {$SET_STATE_RAM 'E28-E2D:SFR'}            // Bank 28 : CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS, PRG1RPPS, PRG1FPPS
  {$SET_STATE_RAM 'E30-E32:SFR'}            // Bank 28 : MD1CHPPS, MD1CLPPS, MD1MODPPS
  {$SET_STATE_RAM 'E90-E92:SFR'}            // Bank 29 : RA0PPS, RA1PPS, RA2PPS
  {$SET_STATE_RAM 'E94-E95:SFR'}            // Bank 29 : RA4PPS, RA5PPS
  {$SET_STATE_RAM 'EA0-EA5:SFR'}            // Bank 29 : RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS
  {$SET_STATE_RAM 'F0F-F2D:SFR'}            // Bank 30 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00E:3F'} // PORTC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:E8'} // PIR2 bits 4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:77'} // PIR3 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '014:3F'} // PIR4 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:0F'} // T2CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:0F'} // T2RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:3F'} // TRISC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '092:E8'} // PIE2 bits 4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '093:77'} // PIE3 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '094:3F'} // PIE4 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:DF'} // PCON bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:FB'} // OSCCON bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:7F'} // ADCON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:F8'} // ADCON2 bits 2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // LATC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '110:DF'} // CM1CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:03'} // CM1CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:07'} // CM1NSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:0F'} // CM1PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:DF'} // CM2CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CM2CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:07'} // CM2NSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '117:0F'} // CM2PSEL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18E:0F'} // ANSELC bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:3F'} // WPUC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21D:C1'} // BORCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '21F:B3'} // ZCD1CON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28E:3F'} // ODCONC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:BF'} // CCP1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:07'} // CCP1CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '29E:33'} // CCPTMRS bits 7,6,3,2 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS '39B:B1'} // MD1CON0 bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39C:33'} // MD1CON1 bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:1F'} // MD1SRC bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:0F'} // MD1CARL bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:0F'} // MD1CARH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '40E:30'} // HIDRVC bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:0F'} // T4CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '418:0F'} // T4RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:0F'} // T6CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41F:0F'} // T6RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '495:F5'} // T3CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49C:F5'} // T5CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '50F:0F'} // OPA1NCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '510:0F'} // OPA1PCHS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '511:97'} // OPA1CON bits 6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '512:1F'} // OPA1ORS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '590:01'} // DACLD bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '591:ED'} // DAC1CON0 bits 4,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '597:AD'} // DAC3CON0 bits 6,4,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '598:1F'} // DAC3REF bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '617:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:B0'} // PWM3CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68D:3F'} // COG1PHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68E:3F'} // COG1PHF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '68F:3F'} // COG1BLKR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '690:3F'} // COG1BLKF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:3F'} // COG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:3F'} // COG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:DF'} // COG1CON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:CF'} // COG1CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '696:7F'} // COG1RIS1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '698:7F'} // COG1RSIM1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69A:7F'} // COG1FIS1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69C:7F'} // COG1FSIM1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '69D:FC'} // COG1ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '794:0F'} // PRG1RTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '795:0F'} // PRG1FTSS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '796:0F'} // PRG1INS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:BF'} // PRG1CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '798:07'} // PRG1CON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:1F'} // PRG1CON2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8E:10'} // PWMEN bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D8F:10'} // PWMLD bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D90:10'} // PWMOUT bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9B:BC'} // PWM5CON bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9C:0F'} // PWM5INTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9D:0F'} // PWM5INTF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9E:73'} // PWM5CLKCON bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'D9F:80'} // PWM5LDCON bits 6,5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'DA0:10'} // PWM5OFCON bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E10:1F'} // INTPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E11:1F'} // T0CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E12:1F'} // T1CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E13:1F'} // T1GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E14:1F'} // CCP1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E16:1F'} // COG1INPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E19:1F'} // T2INPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1A:1F'} // T3CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1B:1F'} // T3GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1C:1F'} // T4INPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1D:1F'} // T5CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1E:1F'} // T5GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1F:1F'} // T6INPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E20:1F'} // SSPCLKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E21:1F'} // SSPDATPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E22:1F'} // SSPSSPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E24:1F'} // RXPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E25:1F'} // CKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E28:1F'} // CLCIN0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E29:1F'} // CLCIN1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2A:1F'} // CLCIN2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2B:1F'} // CLCIN3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2C:1F'} // PRG1RPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2D:1F'} // PRG1FPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E30:1F'} // MD1CHPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E31:1F'} // MD1CLPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E32:1F'} // MD1MODPPS bits 7,6,5 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS 'F0F:07'} // CLCDATA bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F10:BF'} // CLC1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F12:1F'} // CLC1SEL0 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F13:1F'} // CLC1SEL1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F14:1F'} // CLC1SEL2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F15:1F'} // CLC1SEL3 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1A:BF'} // CLC2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1B:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1C:1F'} // CLC2SEL0 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1D:1F'} // CLC2SEL1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1E:1F'} // CLC2SEL2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1F:1F'} // CLC2SEL3 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F24:BF'} // CLC3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F25:8F'} // CLC3POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F26:1F'} // CLC3SEL0 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F27:1F'} // CLC3SEL1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F28:1F'} // CLC3SEL2 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F29:1F'} // CLC3SEL3 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/SOSCI/OSC1/CLKIN
  // Pin  3 : RA4/AN3/SOSCO/OSC2/CLKOUT
  // Pin  4 : RA3/Vpp/nMCLR/nICD/MCLR
  // Pin  5 : RC5
  // Pin  6 : RC4
  // Pin  7 : RC3/AN7/C1IN3-/C2IN3-
  // Pin  8 : RC2/AN6/OPA1OUT/C1IN2-/C2IN2-/PRG1IN0
  // Pin  9 : RC1/AN5/OPA1IN-/C1IN1-/C2IN1-
  // Pin 10 : RC0/AN4/OPA1IN+/C2IN0+
  // Pin 11 : RA2/AN2/ZCD
  // Pin 12 : RA1/AN1/Vref+/DAC1Ref+/DAC3Ref+/C1IN0-/C2IN0-/ICSPCLK
  // Pin 13 : RA0/AN0/Vref-/DAC1Ref-/DAC3Ref-/DAC1OUT1/DAC3OUT1/C1IN0+/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '00E:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


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
