unit PIC16F753;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F753'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

interface
var
  INDF                : byte absolute $0000;
  TMR0                : byte absolute $0001;
  PCL                 : byte absolute $0002;
  STATUS              : byte absolute $0003;
  STATUS_IRP          : bit  absolute STATUS.7;
  STATUS_RP1          : bit  absolute STATUS.6;
  STATUS_RP0          : bit  absolute STATUS.5;
  STATUS_nTO          : bit  absolute STATUS.4;
  STATUS_nPD          : bit  absolute STATUS.3;
  STATUS_Z            : bit  absolute STATUS.2;
  STATUS_DC           : bit  absolute STATUS.1;
  STATUS_C            : bit  absolute STATUS.0;
  FSR                 : byte absolute $0004;
  PORTA               : byte absolute $0005;
  PORTA_RA5           : bit  absolute PORTA.5;
  PORTA_RA4           : bit  absolute PORTA.4;
  PORTA_RA3           : bit  absolute PORTA.3;
  PORTA_RA2           : bit  absolute PORTA.2;
  PORTA_RA1           : bit  absolute PORTA.1;
  PORTA_RA0           : bit  absolute PORTA.0;
  PORTC               : byte absolute $0007;
  PORTC_RC5           : bit  absolute PORTC.5;
  PORTC_RC4           : bit  absolute PORTC.4;
  PORTC_RC3           : bit  absolute PORTC.3;
  PORTC_RC2           : bit  absolute PORTC.2;
  PORTC_RC1           : bit  absolute PORTC.1;
  PORTC_RC0           : bit  absolute PORTC.0;
  IOCAF               : byte absolute $0008;
  IOCAF_IOCAF5        : bit  absolute IOCAF.5;
  IOCAF_IOCAF4        : bit  absolute IOCAF.4;
  IOCAF_IOCAF3        : bit  absolute IOCAF.3;
  IOCAF_IOCAF2        : bit  absolute IOCAF.2;
  IOCAF_IOCAF1        : bit  absolute IOCAF.1;
  IOCAF_IOCAF0        : bit  absolute IOCAF.0;
  IOCCF               : byte absolute $0009;
  IOCCF_IOCCF5        : bit  absolute IOCCF.5;
  IOCCF_IOCCF4        : bit  absolute IOCCF.4;
  IOCCF_IOCCF3        : bit  absolute IOCCF.3;
  IOCCF_IOCCF2        : bit  absolute IOCCF.2;
  IOCCF_IOCCF1        : bit  absolute IOCCF.1;
  IOCCF_IOCCF0        : bit  absolute IOCCF.0;
  PCLATH              : byte absolute $000A;
  PCLATH_PCLATH4      : bit  absolute PCLATH.4;
  PCLATH_PCLATH3      : bit  absolute PCLATH.3;
  PCLATH_PCLATH2      : bit  absolute PCLATH.2;
  PCLATH_PCLATH1      : bit  absolute PCLATH.1;
  PCLATH_PCLATH0      : bit  absolute PCLATH.0;
  INTCON              : byte absolute $000B;
  INTCON_GIE          : bit  absolute INTCON.7;
  INTCON_PEIE         : bit  absolute INTCON.6;
  INTCON_T0IE         : bit  absolute INTCON.5;
  INTCON_INTE         : bit  absolute INTCON.4;
  INTCON_IOCIE        : bit  absolute INTCON.3;
  INTCON_T0IF         : bit  absolute INTCON.2;
  INTCON_INTF         : bit  absolute INTCON.1;
  INTCON_IOCIF        : bit  absolute INTCON.0;
  PIR1                : byte absolute $000C;
  PIR1_TMR1GIF        : bit  absolute PIR1.7;
  PIR1_ADIF           : bit  absolute PIR1.6;
  PIR1_HLTMR2IF       : bit  absolute PIR1.3;
  PIR1_HLTMR1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF         : bit  absolute PIR1.1;
  PIR1_TMR1IF         : bit  absolute PIR1.0;
  PIR2                : byte absolute $000D;
  PIR2_C2IF           : bit  absolute PIR2.5;
  PIR2_C1IF           : bit  absolute PIR2.4;
  PIR2_COG1IF         : bit  absolute PIR2.2;
  PIR2_CCP1IF         : bit  absolute PIR2.0;
  TMR1L               : byte absolute $000F;
  TMR1H               : byte absolute $0010;
  T1CON               : byte absolute $0011;
  T1CON_TMR1CS1       : bit  absolute T1CON.7;
  T1CON_TMR1CS0       : bit  absolute T1CON.6;
  T1CON_T1CKPS1       : bit  absolute T1CON.5;
  T1CON_T1CKPS0       : bit  absolute T1CON.4;
  T1CON_T1OSCEN       : bit  absolute T1CON.3;
  T1CON_nT1SYNC       : bit  absolute T1CON.2;
  T1CON_TMR1ON        : bit  absolute T1CON.0;
  T1GCON              : byte absolute $0012;
  T1GCON_TMR1GE       : bit  absolute T1GCON.7;
  T1GCON_T1GPOL       : bit  absolute T1GCON.6;
  T1GCON_T1GTM        : bit  absolute T1GCON.5;
  T1GCON_T1GSPM       : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE  : bit  absolute T1GCON.3;
  T1GCON_T1GVAL       : bit  absolute T1GCON.2;
  T1GCON_T1GSS1       : bit  absolute T1GCON.1;
  T1GCON_T1GSS0       : bit  absolute T1GCON.0;
  CCPR1L              : byte absolute $0013;
  CCPR1H              : byte absolute $0014;
  CCP1CON             : byte absolute $0015;
  CCP1CON_DC1B1       : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0       : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3      : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2      : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1      : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0      : bit  absolute CCP1CON.0;
  ADRESL              : byte absolute $001C;
  ADRESH              : byte absolute $001D;
  ADCON0              : byte absolute $001E;
  ADCON0_ADFM         : bit  absolute ADCON0.7;
  ADCON0_CHS3         : bit  absolute ADCON0.5;
  ADCON0_CHS2         : bit  absolute ADCON0.4;
  ADCON0_CHS1         : bit  absolute ADCON0.3;
  ADCON0_CHS0         : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE     : bit  absolute ADCON0.1;
  ADCON0_ADON         : bit  absolute ADCON0.0;
  ADCON1              : byte absolute $001F;
  ADCON1_ADCS2        : bit  absolute ADCON1.6;
  ADCON1_ADCS1        : bit  absolute ADCON1.5;
  ADCON1_ADCS0        : bit  absolute ADCON1.4;
  ADCON1_ADPREF1      : bit  absolute ADCON1.0;
  OPTION_REG          : byte absolute $0081;
  OPTION_REG_nRAPU    : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG   : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS     : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE     : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA      : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2      : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1      : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0      : bit  absolute OPTION_REG.0;
  TRISA               : byte absolute $0085;
  TRISA_TRISA5        : bit  absolute TRISA.5;
  TRISA_TRISA4        : bit  absolute TRISA.4;
  TRISA_TRISA3        : bit  absolute TRISA.3;
  TRISA_TRISA2        : bit  absolute TRISA.2;
  TRISA_TRISA1        : bit  absolute TRISA.1;
  TRISA_TRISA0        : bit  absolute TRISA.0;
  TRISC               : byte absolute $0087;
  TRISC_TRISC5        : bit  absolute TRISC.5;
  TRISC_TRISC4        : bit  absolute TRISC.4;
  TRISC_TRISC3        : bit  absolute TRISC.3;
  TRISC_TRISC2        : bit  absolute TRISC.2;
  TRISC_TRISC1        : bit  absolute TRISC.1;
  TRISC_TRISC0        : bit  absolute TRISC.0;
  IOCAP               : byte absolute $0088;
  IOCAP_IOCAP5        : bit  absolute IOCAP.5;
  IOCAP_IOCAP4        : bit  absolute IOCAP.4;
  IOCAP_IOCAP3        : bit  absolute IOCAP.3;
  IOCAP_IOCAP2        : bit  absolute IOCAP.2;
  IOCAP_IOCAP1        : bit  absolute IOCAP.1;
  IOCAP_IOCAP0        : bit  absolute IOCAP.0;
  IOCCP               : byte absolute $0089;
  IOCCP_IOCCP5        : bit  absolute IOCCP.5;
  IOCCP_IOCCP4        : bit  absolute IOCCP.4;
  IOCCP_IOCCP3        : bit  absolute IOCCP.3;
  IOCCP_IOCCP2        : bit  absolute IOCCP.2;
  IOCCP_IOCCP1        : bit  absolute IOCCP.1;
  IOCCP_IOCCP0        : bit  absolute IOCCP.0;
  PIE1                : byte absolute $008C;
  PIE1_TMR1GIE        : bit  absolute PIE1.7;
  PIE1_ADIE           : bit  absolute PIE1.6;
  PIE1_HLTMR2IE       : bit  absolute PIE1.3;
  PIE1_HLTMR1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE         : bit  absolute PIE1.1;
  PIE1_TMR1IE         : bit  absolute PIE1.0;
  PIE2                : byte absolute $008D;
  PIE2_C2IE           : bit  absolute PIE2.5;
  PIE2_C1IE           : bit  absolute PIE2.4;
  PIE2_COG1IE         : bit  absolute PIE2.2;
  PIE2_CCP1IE         : bit  absolute PIE2.0;
  OSCCON              : byte absolute $008F;
  OSCCON_IRCF1        : bit  absolute OSCCON.5;
  OSCCON_IRCF0        : bit  absolute OSCCON.4;
  OSCCON_HTS          : bit  absolute OSCCON.2;
  OSCCON_LTS          : bit  absolute OSCCON.1;
  FVR1CON0            : byte absolute $0090;
  FVR1CON0_FVREN      : bit  absolute FVR1CON0.7;
  FVR1CON0_FVRRDY     : bit  absolute FVR1CON0.6;
  FVR1CON0_FVROE      : bit  absolute FVR1CON0.5;
  FVR1CON0_FVRBUFSS01 : bit  absolute FVR1CON0.4;
  FVR1CON0_FVRBUFSS00 : bit  absolute FVR1CON0.3;
  FVR1CON0_FVRBUFEN   : bit  absolute FVR1CON0.0;
  DAC1CON0            : byte absolute $0091;
  DAC1CON0_DACEN      : bit  absolute DAC1CON0.7;
  DAC1CON0_DACFM      : bit  absolute DAC1CON0.6;
  DAC1CON0_DACOE      : bit  absolute DAC1CON0.5;
  DAC1CON0_DACPSS1    : bit  absolute DAC1CON0.3;
  DAC1CON0_DACPSS0    : bit  absolute DAC1CON0.2;
  DAC1REFL            : byte absolute $0092;
  DAC1REFH            : byte absolute $0093;
  OPA1CON             : byte absolute $0096;
  OPA1CON_OPAEN       : bit  absolute OPA1CON.7;
  OPA1CON_OPAUGM      : bit  absolute OPA1CON.4;
  OPA1CON_OPA1NCH1    : bit  absolute OPA1CON.3;
  OPA1CON_OPA1NCH0    : bit  absolute OPA1CON.2;
  OPA1CON_OPA1PCH1    : bit  absolute OPA1CON.1;
  OPA1CON_OPA1PCH0    : bit  absolute OPA1CON.0;
  CM2CON0             : byte absolute $009B;
  CM2CON0_C2ON        : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT       : bit  absolute CM2CON0.6;
  CM2CON0_C2OE        : bit  absolute CM2CON0.5;
  CM2CON0_C2POL       : bit  absolute CM2CON0.4;
  CM2CON0_C2ZLF       : bit  absolute CM2CON0.3;
  CM2CON0_C2SP        : bit  absolute CM2CON0.2;
  CM2CON0_C2HYS       : bit  absolute CM2CON0.1;
  CM2CON0_C2SYNC      : bit  absolute CM2CON0.0;
  CM2CON1             : byte absolute $009C;
  CM2CON1_C2INTP      : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN      : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH2      : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH1      : bit  absolute CM2CON1.4;
  CM2CON1_C2PCH0      : bit  absolute CM2CON1.3;
  CM2CON1_C2NCH2      : bit  absolute CM2CON1.2;
  CM2CON1_C2NCH1      : bit  absolute CM2CON1.1;
  CM2CON1_C2NCH0      : bit  absolute CM2CON1.0;
  CM1CON0             : byte absolute $009D;
  CM1CON0_C1ON        : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT       : bit  absolute CM1CON0.6;
  CM1CON0_C1OE        : bit  absolute CM1CON0.5;
  CM1CON0_C1POL       : bit  absolute CM1CON0.4;
  CM1CON0_C1ZLF       : bit  absolute CM1CON0.3;
  CM1CON0_C1SP        : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS       : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC      : bit  absolute CM1CON0.0;
  CM1CON1             : byte absolute $009E;
  CM1CON1_C1INTP      : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN      : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH2      : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH1      : bit  absolute CM1CON1.4;
  CM1CON1_C1PCH0      : bit  absolute CM1CON1.3;
  CM1CON1_C1NCH2      : bit  absolute CM1CON1.2;
  CM1CON1_C1NCH1      : bit  absolute CM1CON1.1;
  CM1CON1_C1NCH0      : bit  absolute CM1CON1.0;
  CMOUT               : byte absolute $009F;
  CMOUT_MCOUT2        : bit  absolute CMOUT.1;
  CMOUT_MCOUT1        : bit  absolute CMOUT.0;
  LATA                : byte absolute $0105;
  LATA_LATA5          : bit  absolute LATA.5;
  LATA_LATA4          : bit  absolute LATA.4;
  LATA_LATA2          : bit  absolute LATA.2;
  LATA_LATA1          : bit  absolute LATA.1;
  LATA_LATA0          : bit  absolute LATA.0;
  LATC                : byte absolute $0107;
  LATC_LATC5          : bit  absolute LATC.5;
  LATC_LATC4          : bit  absolute LATC.4;
  LATC_LATC3          : bit  absolute LATC.3;
  LATC_LATC2          : bit  absolute LATC.2;
  LATC_LATC1          : bit  absolute LATC.1;
  LATC_LATC0          : bit  absolute LATC.0;
  IOCAN               : byte absolute $0108;
  IOCAN_IOCAN5        : bit  absolute IOCAN.5;
  IOCAN_IOCAN4        : bit  absolute IOCAN.4;
  IOCAN_IOCAN3        : bit  absolute IOCAN.3;
  IOCAN_IOCAN2        : bit  absolute IOCAN.2;
  IOCAN_IOCAN1        : bit  absolute IOCAN.1;
  IOCAN_IOCAN0        : bit  absolute IOCAN.0;
  IOCCN               : byte absolute $0109;
  IOCCN_IOCCN5        : bit  absolute IOCCN.5;
  IOCCN_IOCCN4        : bit  absolute IOCCN.4;
  IOCCN_IOCCN3        : bit  absolute IOCCN.3;
  IOCCN_IOCCN2        : bit  absolute IOCCN.2;
  IOCCN_IOCCN1        : bit  absolute IOCCN.1;
  IOCCN_IOCCN0        : bit  absolute IOCCN.0;
  WPUA                : byte absolute $010C;
  WPUA_WPUA5          : bit  absolute WPUA.5;
  WPUA_WPUA4          : bit  absolute WPUA.4;
  WPUA_WPUA3          : bit  absolute WPUA.3;
  WPUA_WPUA2          : bit  absolute WPUA.2;
  WPUA_WPUA1          : bit  absolute WPUA.1;
  WPUA_WPUA0          : bit  absolute WPUA.0;
  WPUC                : byte absolute $010D;
  WPUC_WPUC5          : bit  absolute WPUC.5;
  WPUC_WPUC4          : bit  absolute WPUC.4;
  WPUC_WPUC3          : bit  absolute WPUC.3;
  WPUC_WPUC2          : bit  absolute WPUC.2;
  WPUC_WPUC1          : bit  absolute WPUC.1;
  WPUC_WPUC0          : bit  absolute WPUC.0;
  SLRCONC             : byte absolute $010E;
  SLRCONC_SLRC5       : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4       : bit  absolute SLRCONC.4;
  PCON                : byte absolute $010F;
  PCON_nPOR           : bit  absolute PCON.1;
  PCON_nBOR           : bit  absolute PCON.0;
  TMR2                : byte absolute $0110;
  PR2                 : byte absolute $0111;
  T2CON               : byte absolute $0112;
  T2CON_T2OUTPS3      : bit  absolute T2CON.6;
  T2CON_T2OUTPS2      : bit  absolute T2CON.5;
  T2CON_T2OUTPS1      : bit  absolute T2CON.4;
  T2CON_T2OUTPS0      : bit  absolute T2CON.3;
  T2CON_TMR2ON        : bit  absolute T2CON.2;
  T2CON_T2CKPS1       : bit  absolute T2CON.1;
  T2CON_T2CKPS0       : bit  absolute T2CON.0;
  HLTMR1              : byte absolute $0113;
  HLTPR1              : byte absolute $0114;
  HLT1CON0            : byte absolute $0115;
  HLT1CON0_H1OUTPS3   : bit  absolute HLT1CON0.6;
  HLT1CON0_H1OUTPS2   : bit  absolute HLT1CON0.5;
  HLT1CON0_H1OUTPS1   : bit  absolute HLT1CON0.4;
  HLT1CON0_H1OUTPS0   : bit  absolute HLT1CON0.3;
  HLT1CON0_H1ON       : bit  absolute HLT1CON0.2;
  HLT1CON0_H1CKPS1    : bit  absolute HLT1CON0.1;
  HLT1CON0_H1CKPS0    : bit  absolute HLT1CON0.0;
  HLT1CON1            : byte absolute $0116;
  HLT1CON1_H1FES      : bit  absolute HLT1CON1.7;
  HLT1CON1_H1RES      : bit  absolute HLT1CON1.6;
  HLT1CON1_H1ERS2     : bit  absolute HLT1CON1.4;
  HLT1CON1_H1ERS1     : bit  absolute HLT1CON1.3;
  HLT1CON1_H1ERS0     : bit  absolute HLT1CON1.2;
  HLT1CON1_H1FEREN    : bit  absolute HLT1CON1.1;
  HLT1CON1_H1REREN    : bit  absolute HLT1CON1.0;
  HLTMR2              : byte absolute $0117;
  HLTPR2              : byte absolute $0118;
  HLT2CON0            : byte absolute $0119;
  HLT2CON0_H2OUTPS3   : bit  absolute HLT2CON0.6;
  HLT2CON0_H2OUTPS2   : bit  absolute HLT2CON0.5;
  HLT2CON0_H2OUTPS1   : bit  absolute HLT2CON0.4;
  HLT2CON0_H2OUTPS0   : bit  absolute HLT2CON0.3;
  HLT2CON0_H2ON       : bit  absolute HLT2CON0.2;
  HLT2CON0_H2CKPS1    : bit  absolute HLT2CON0.1;
  HLT2CON0_H2CKPS0    : bit  absolute HLT2CON0.0;
  HLT2CON1            : byte absolute $011A;
  HLT2CON1_H2FES      : bit  absolute HLT2CON1.7;
  HLT2CON1_H2RES      : bit  absolute HLT2CON1.6;
  HLT2CON1_H2ERS2     : bit  absolute HLT2CON1.4;
  HLT2CON1_H2ERS1     : bit  absolute HLT2CON1.3;
  HLT2CON1_H2ERS0     : bit  absolute HLT2CON1.2;
  HLT2CON1_H2FEREN    : bit  absolute HLT2CON1.1;
  HLT2CON1_H2REREN    : bit  absolute HLT2CON1.0;
  SLPC1CON0           : byte absolute $011E;
  SLPC1CON0_SC1EN     : bit  absolute SLPC1CON0.7;
  SLPC1CON0_SC1MRPE   : bit  absolute SLPC1CON0.5;
  SLPC1CON0_SC1POL    : bit  absolute SLPC1CON0.4;
  SLPC1CON0_SC1TSS1   : bit  absolute SLPC1CON0.3;
  SLPC1CON0_SC1TSS0   : bit  absolute SLPC1CON0.2;
  SLPC1CON0_SC1INS    : bit  absolute SLPC1CON0.0;
  SLPC1CON1           : byte absolute $011F;
  SLPC1CON1_SC1RNG    : bit  absolute SLPC1CON1.4;
  SLPC1CON1_SC1ISET3  : bit  absolute SLPC1CON1.3;
  SLPC1CON1_SC1ISET2  : bit  absolute SLPC1CON1.2;
  SLPC1CON1_SC1ISET1  : bit  absolute SLPC1CON1.1;
  SLPC1CON1_SC1ISET0  : bit  absolute SLPC1CON1.0;
  ANSELA              : byte absolute $0185;
  ANSELA_ANSA4        : bit  absolute ANSELA.4;
  ANSELA_ANSA2        : bit  absolute ANSELA.2;
  ANSELA_ANSA1        : bit  absolute ANSELA.1;
  ANSELA_ANSA0        : bit  absolute ANSELA.0;
  ANSELC              : byte absolute $0187;
  ANSELC_ANSC3        : bit  absolute ANSELC.3;
  ANSELC_ANSC2        : bit  absolute ANSELC.2;
  ANSELC_ANSC1        : bit  absolute ANSELC.1;
  ANSELC_ANSC0        : bit  absolute ANSELC.0;
  APFCON              : byte absolute $0188;
  APFCON_T1GSEL       : bit  absolute APFCON.4;
  OSCTUNE             : byte absolute $0189;
  OSCTUNE_TUN4        : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3        : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2        : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1        : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0        : bit  absolute OSCTUNE.0;
  PMCON1              : byte absolute $018C;
  PMCON1_WREN         : bit  absolute PMCON1.2;
  PMCON1_WR           : bit  absolute PMCON1.1;
  PMCON1_RD           : bit  absolute PMCON1.0;
  PMCON2              : byte absolute $018D;
  PMADRL              : byte absolute $018E;
  PMADRH              : byte absolute $018F;
  PMADRH_PMADRH2      : bit  absolute PMADRH.2;
  PMADRH_PMADRH1      : bit  absolute PMADRH.1;
  PMADRH_PMADRH0      : bit  absolute PMADRH.0;
  PMDATL              : byte absolute $0190;
  PMDATH              : byte absolute $0191;
  PMDATH_PMDATH5      : bit  absolute PMDATH.5;
  PMDATH_PMDATH4      : bit  absolute PMDATH.4;
  PMDATH_PMDATH3      : bit  absolute PMDATH.3;
  PMDATH_PMDATH2      : bit  absolute PMDATH.2;
  PMDATH_PMDATH1      : bit  absolute PMDATH.1;
  PMDATH_PMDATH0      : bit  absolute PMDATH.0;
  COG1PHR             : byte absolute $0192;
  COG1PHR_G1PHR3      : bit  absolute COG1PHR.3;
  COG1PHR_G1PHR2      : bit  absolute COG1PHR.2;
  COG1PHR_G1PHR1      : bit  absolute COG1PHR.1;
  COG1PHR_G1PHR0      : bit  absolute COG1PHR.0;
  COG1PHF             : byte absolute $0193;
  COG1PHF_G1PHF3      : bit  absolute COG1PHF.3;
  COG1PHF_G1PHF2      : bit  absolute COG1PHF.2;
  COG1PHF_G1PHF1      : bit  absolute COG1PHF.1;
  COG1PHF_G1PHF0      : bit  absolute COG1PHF.0;
  COG1BKR             : byte absolute $0194;
  COG1BKR_G1BKR3      : bit  absolute COG1BKR.3;
  COG1BKR_G1BKR2      : bit  absolute COG1BKR.2;
  COG1BKR_G1BKR1      : bit  absolute COG1BKR.1;
  COG1BKR_G1BKR0      : bit  absolute COG1BKR.0;
  COG1BKF             : byte absolute $0195;
  COG1BKF_G1BKF3      : bit  absolute COG1BKF.3;
  COG1BKF_G1BKF2      : bit  absolute COG1BKF.2;
  COG1BKF_G1BKF1      : bit  absolute COG1BKF.1;
  COG1BKF_G1BKF0      : bit  absolute COG1BKF.0;
  COG1DBR             : byte absolute $0196;
  COG1DBR_G1DBR3      : bit  absolute COG1DBR.3;
  COG1DBR_G1DBR2      : bit  absolute COG1DBR.2;
  COG1DBR_G1DBR1      : bit  absolute COG1DBR.1;
  COG1DBR_G1DBR0      : bit  absolute COG1DBR.0;
  COG1DBF             : byte absolute $0197;
  COG1DBF_G1DBF3      : bit  absolute COG1DBF.3;
  COG1DBF_G1DBF2      : bit  absolute COG1DBF.2;
  COG1DBF_G1DBF1      : bit  absolute COG1DBF.1;
  COG1DBF_G1DBF0      : bit  absolute COG1DBF.0;
  COG1CON0            : byte absolute $0198;
  COG1CON0_G1EN       : bit  absolute COG1CON0.7;
  COG1CON0_G1OE1      : bit  absolute COG1CON0.6;
  COG1CON0_G1OE0      : bit  absolute COG1CON0.5;
  COG1CON0_G1POL1     : bit  absolute COG1CON0.4;
  COG1CON0_G1POL0     : bit  absolute COG1CON0.3;
  COG1CON0_G1LD       : bit  absolute COG1CON0.2;
  COG1CON0_G1MD       : bit  absolute COG1CON0.0;
  COG1CON1            : byte absolute $0199;
  COG1CON1_G1RDBTS    : bit  absolute COG1CON1.7;
  COG1CON1_G1FDBTS    : bit  absolute COG1CON1.6;
  COG1CON1_G1CS1      : bit  absolute COG1CON1.1;
  COG1CON1_G1CS0      : bit  absolute COG1CON1.0;
  COG1RIS             : byte absolute $019A;
  COG1RIS_G1RIHLT2    : bit  absolute COG1RIS.6;
  COG1RIS_G1R1HLT1    : bit  absolute COG1RIS.5;
  COG1RIS_G1RIT2M     : bit  absolute COG1RIS.4;
  COG1RIS_G1RIFLT     : bit  absolute COG1RIS.3;
  COG1RIS_C1RICCP1    : bit  absolute COG1RIS.2;
  COG1RIS_G1RIC2      : bit  absolute COG1RIS.1;
  COG1RIS_G1RIC1      : bit  absolute COG1RIS.0;
  COG1RSIM            : byte absolute $019B;
  COG1RSIM_G1RMHLT2   : bit  absolute COG1RSIM.6;
  COG1RSIM_G1RMHLT1   : bit  absolute COG1RSIM.5;
  COG1RSIM_G1RTM2M    : bit  absolute COG1RSIM.4;
  COG1RSIM_G1RMFLT    : bit  absolute COG1RSIM.3;
  COG1RSIM_G1RMCCP1   : bit  absolute COG1RSIM.2;
  COG1RSIM_G1RMC2     : bit  absolute COG1RSIM.1;
  COG1RSIM_G1RMC1     : bit  absolute COG1RSIM.0;
  COG1FIS             : byte absolute $019C;
  COG1FIS_G1FIHLT2    : bit  absolute COG1FIS.6;
  COG1FIS_G1FIHLT1    : bit  absolute COG1FIS.5;
  COG1FIS_G1FIT2M     : bit  absolute COG1FIS.4;
  COG1FIS_G1FIFLT     : bit  absolute COG1FIS.3;
  COG1FIS_G1FICCP1    : bit  absolute COG1FIS.2;
  COG1FIS_G1FIC2      : bit  absolute COG1FIS.1;
  COG1FIS_G1FIC1      : bit  absolute COG1FIS.0;
  COG1FSIM            : byte absolute $019D;
  COG1FSIM_G1FMHLT2   : bit  absolute COG1FSIM.6;
  COG1FSIM_G1FMHLT1   : bit  absolute COG1FSIM.5;
  COG1FSIM_G1FMT2M    : bit  absolute COG1FSIM.4;
  COG1FSIM_G1FMFLT    : bit  absolute COG1FSIM.3;
  COG1FSIM_G1FMCCP1   : bit  absolute COG1FSIM.2;
  COG1FSIM_G1FMC2     : bit  absolute COG1FSIM.1;
  COG1FSIM_G1FMC1     : bit  absolute COG1FSIM.0;
  COG1ASD0            : byte absolute $019E;
  COG1ASD0_G1ASDE     : bit  absolute COG1ASD0.7;
  COG1ASD0_G1ARSEN    : bit  absolute COG1ASD0.6;
  COG1ASD0_G1ASD1L1   : bit  absolute COG1ASD0.5;
  COG1ASD0_G1ASD1L0   : bit  absolute COG1ASD0.4;
  COG1ASD0_G1ASD0L1   : bit  absolute COG1ASD0.3;
  COG1ASD0_G1ASD0L0   : bit  absolute COG1ASD0.2;
  COG1ASD1            : byte absolute $019F;
  COG1ASD1_G1ASDSHLT2 : bit  absolute COG1ASD1.4;
  COG1ASD1_G1ASDSHLT1 : bit  absolute COG1ASD1.3;
  COG1ASD1_G1ASDSC2   : bit  absolute COG1ASD1.2;
  COG1ASD1_G1ASDSC1   : bit  absolute COG1ASD1.1;
  COG1ASD1_G1ASDSFLT  : bit  absolute COG1ASD1.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-3 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
                                            // Bank 2 : TMR0
                                            // Bank 3 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-3 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-005:SFR:ALL'}        // Bank 0 : PORTA
                                            // Bank 1 : TRISA
                                            // Bank 2 : LATA
                                            // Bank 3 : ANSELA
  {$SET_STATE_RAM '007-009:SFR:ALL'}        // Bank 0 : PORTC, IOCAF, IOCCF
                                            // Bank 1 : TRISC, IOCAP, IOCCP
                                            // Bank 2 : LATC, IOCAN, IOCCN
                                            // Bank 3 : ANSELC, APFCON, OSCTUNE
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00D:SFR:ALL'}        // Bank 0 : PIR1, PIR2
                                            // Bank 1 : PIE1, PIE2
                                            // Bank 2 : WPUA, WPUC
                                            // Bank 3 : PMCON1, PMCON2
  {$SET_STATE_RAM '00F-013:SFR:ALL'}        // Bank 0 : TMR1L, TMR1H, T1CON, T1GCON, CCPR1L
                                            // Bank 1 : OSCCON, FVR1CON0, DAC1CON0, DAC1REFL, DAC1REFH
                                            // Bank 2 : PCON, TMR2, PR2, T2CON, HLTMR1
                                            // Bank 3 : PMADRH, PMDATL, PMDATH, COG1PHR, COG1PHF
  {$SET_STATE_RAM '014-015:SFR'}            // Bank 0 : CCPR1H, CCP1CON
  {$SET_STATE_RAM '01C-01D:SFR'}            // Bank 0 : ADRESL, ADRESH
  {$SET_STATE_RAM '01E-01F:SFR:ALL'}        // Bank 0 : ADCON0, ADCON1
                                            // Bank 1 : CM1CON1, CMOUT
                                            // Bank 2 : SLPC1CON0, SLPC1CON1
                                            // Bank 3 : COG1ASD0, COG1ASD1
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '096-096:SFR'}            // Bank 1 : OPA1CON
  {$SET_STATE_RAM '09B-09D:SFR'}            // Bank 1 : CM2CON0, CM2CON1, CM1CON0
  {$SET_STATE_RAM '0A0-0BF:GPR'}           
  {$SET_STATE_RAM '10E-10E:SFR'}            // Bank 2 : SLRCONC
  {$SET_STATE_RAM '114-11A:SFR'}            // Bank 2 : HLTPR1, HLT1CON0, HLT1CON1, HLTMR2, HLTPR2, HLT2CON0, HLT2CON1
  {$SET_STATE_RAM '18E-18E:SFR'}            // Bank 3 : PMADRL
  {$SET_STATE_RAM '194-19D:SFR'}            // Bank 3 : COG1BKR, COG1BKF, COG1DBR, COG1DBF, COG1CON0, COG1CON1, COG1RIS, COG1RSIM, COG1FIS, COG1FSIM


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '100-104:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '007:3F'} // PORTC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '009:3F'} // IOCCF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:CF'} // PIR1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:35'} // PIR2 bits 7,6,3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '011:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '015:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01E:BF'} // ADCON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:71'} // ADCON1 bits 7,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '087:3F'} // TRISC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '088:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '089:3F'} // IOCCP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:CF'} // PIE1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:35'} // PIE2 bits 7,6,3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08F:36'} // OSCCON bits 7,6,3,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:F9'} // FVR1CON0 bits 2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:EC'} // DAC1CON0 bits 4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:9F'} // OPA1CON bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '105:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '107:3F'} // LATC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '108:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '109:3F'} // IOCCN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10D:3F'} // WPUC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:30'} // SLRCONC bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:7F'} // HLT1CON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:DF'} // HLT1CON1 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:7F'} // HLT2CON0 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11A:DF'} // HLT2CON1 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11E:BD'} // SLPC1CON0 bits 6,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11F:1F'} // SLPC1CON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '185:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '187:0F'} // ANSELC bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '188:10'} // APFCON bits 7,6,5,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '189:1F'} // OSCTUNE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:07'} // PMCON1 bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18F:07'} // PMADRH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '191:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '192:0F'} // COG1PHR bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '193:0F'} // COG1PHF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:0F'} // COG1BKR bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:0F'} // COG1BKF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '196:0F'} // COG1DBR bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:0F'} // COG1DBF bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '198:FD'} // COG1CON0 bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '199:C3'} // COG1CON1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19A:7F'} // COG1RIS bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19B:7F'} // COG1RSIM bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19C:7F'} // COG1FIS bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19D:7F'} // COG1FSIM bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19E:FC'} // COG1ASD0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:1F'} // COG1ASD1 bits 7,6,5 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/CLKIN
  // Pin  3 : RA4/T1G/AN3/CLKOUT
  // Pin  4 : RA3/T1G/Vpp/MCLR
  // Pin  5 : RC5/COG1OUT0/CCP1
  // Pin  6 : RC4/COG1OUT1/C2OUT
  // Pin  7 : RC3/AN7/C1IN3-/C2IN3-
  // Pin  8 : RC2/SLPCIN/AN6/OPA1OUT/C1IN2-/C2IN2-
  // Pin  9 : RC1/OPA1IN-/AN5/C1IN1-/C2IN1-
  // Pin 10 : RC0/OPA1IN+/AN4/C2IN0+
  // Pin 11 : RA2/INT/C1OUT/T0CKI/AN2/COG1FLT
  // Pin 12 : RA1/C1IN0-/C2IN0-/AN1/VREF+/FVRIN/ICSPCLK
  // Pin 13 : RA0/C1IN0+/AN0/DACOUT/FVROUT/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '007:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // FOSC0 : FOSC: Oscillator Selection bit
  {$define _FOSC0_EC        = $3FFF}  // EC oscillator mode.  CLKIN function on RA5/CLKIN
  {$define _FOSC0_INT       = $3FFE}  // Internal oscillator mode.  I/O function on RA5/CLKIN

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON         = $3FFF}  // Watchdog Timer enabled
  {$define _WDTE_OFF        = $3FF7}  // Watchdog Timer disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF       = $3FFF}  // Power-up Timer disabled
  {$define _PWRTE_ON        = $3FEF}  // Power-up Timer enabled

  // MCLRE : MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON        = $3FFF}  // MCLR pin is MCLR function with internal weak pullup
  {$define _MCLRE_OFF       = $3FDF}  // MCLR pin is alternate function

  // CP : Code Protection bit
  {$define _CP_OFF          = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON           = $3FBF}  // Program memory code protection is enabled

  // BOREN : Brown-out Reset Enable bits
  {$define _BOREN_EN        = $3FFF}  // BOR enabled
  {$define _BOREN_SLEEP_DIS = $3EFF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_DIS       = $3CFF}  // BOR disabled

  // WRT : Flash Program Memory Self Write Enable bit
  {$define _WRT_OFF         = $3FFF}  // Flash self-write protection off
  {$define _WRT_FOURTH      = $3BFF}  // 000h to 0FFh self-write protected
  {$define _WRT_HALF        = $37FF}  // 000h to 1FFh self-write protected
  {$define _WRT_ALL         = $33FF}  // 000h to 3FFh self-write protected

  // CLKOUTEN : Clock Out Enable bit
  {$define _CLKOUTEN_OFF    = $3FFF}  // CLKOUT function disabled.  CLKOUT pin acts as I/O
  {$define _CLKOUTEN_ON     = $2FFF}  // CLKOUT function enabled.  CLKOUT pin is CLKOUT

  // DEBUG : Debug Mode Enable bit
  {$define _DEBUG_OFF       = $3FFF}  // Debug mode disabled
  {$define _DEBUG_ON        = $1FFF}  // Debug mode enabled

implementation
end.
