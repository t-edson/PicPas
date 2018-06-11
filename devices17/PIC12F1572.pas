unit PIC12F1572;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F1572'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 8}
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
  PIR1                : byte absolute $0011;
  PIR1_TMR1GIF        : bit  absolute PIR1.6;
  PIR1_ADIF           : bit  absolute PIR1.5;
  PIR1_RCIF           : bit  absolute PIR1.4;
  PIR1_TXIF           : bit  absolute PIR1.3;
  PIR1_TMR2IF         : bit  absolute PIR1.2;
  PIR1_TMR1IF         : bit  absolute PIR1.1;
  PIR2                : byte absolute $0012;
  PIR2_C1IF           : bit  absolute PIR2.1;
  PIR3                : byte absolute $0013;
  PIR3_PWM3IF         : bit  absolute PIR3.6;
  PIR3_PWM2IF         : bit  absolute PIR3.5;
  PIR3_PWM1IF         : bit  absolute PIR3.4;
  TMR0                : byte absolute $0015;
  TMR1L               : byte absolute $0016;
  TMR1H               : byte absolute $0017;
  T1CON               : byte absolute $0018;
  T1CON_TMR1CS1       : bit  absolute T1CON.6;
  T1CON_TMR1CS0       : bit  absolute T1CON.5;
  T1CON_T1CKPS1       : bit  absolute T1CON.4;
  T1CON_T1CKPS0       : bit  absolute T1CON.3;
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
  T2CON_T2CKPS1       : bit  absolute T2CON.2;
  T2CON_T2CKPS0       : bit  absolute T2CON.1;
  TRISA               : byte absolute $008C;
  TRISA_TRISA5        : bit  absolute TRISA.6;
  TRISA_TRISA4        : bit  absolute TRISA.5;
  TRISA_TRISA3        : bit  absolute TRISA.4;
  TRISA_TRISA2        : bit  absolute TRISA.3;
  TRISA_TRISA1        : bit  absolute TRISA.2;
  TRISA_TRISA0        : bit  absolute TRISA.1;
  PIE1                : byte absolute $0091;
  PIE1_TMR1GIE        : bit  absolute PIE1.6;
  PIE1_ADIE           : bit  absolute PIE1.5;
  PIE1_RCIE           : bit  absolute PIE1.4;
  PIE1_TXIE           : bit  absolute PIE1.3;
  PIE1_TMR2IE         : bit  absolute PIE1.2;
  PIE1_TMR1IE         : bit  absolute PIE1.1;
  PIE2                : byte absolute $0092;
  PIE2_C1IE           : bit  absolute PIE2.1;
  PIE3                : byte absolute $0093;
  PIE3_PWM3IE         : bit  absolute PIE3.6;
  PIE3_PWM2IE         : bit  absolute PIE3.5;
  PIE3_PWM1IE         : bit  absolute PIE3.4;
  OPTION_REG          : byte absolute $0095;
  OPTION_REG_WPUEN    : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG   : bit  absolute OPTION_REG.6;
  OPTION_REG_TMR0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_TMR0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA      : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2      : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1      : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0      : bit  absolute OPTION_REG.0;
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
  OSCSTAT_PLLR        : bit  absolute OSCSTAT.7;
  OSCSTAT_OSTS        : bit  absolute OSCSTAT.6;
  OSCSTAT_HFIOFR      : bit  absolute OSCSTAT.5;
  OSCSTAT_HFIOFL      : bit  absolute OSCSTAT.4;
  OSCSTAT_MFIOFR      : bit  absolute OSCSTAT.3;
  OSCSTAT_LFIOFR      : bit  absolute OSCSTAT.2;
  OSCSTAT_HFIOFS      : bit  absolute OSCSTAT.1;
  ADRESL              : byte absolute $009B;
  ADRESH              : byte absolute $009C;
  ADCON0              : byte absolute $009D;
  ADCON0_CHS4         : bit  absolute ADCON0.7;
  ADCON0_CHS3         : bit  absolute ADCON0.6;
  ADCON0_CHS2         : bit  absolute ADCON0.5;
  ADCON0_CHS1         : bit  absolute ADCON0.4;
  ADCON0_CHS0         : bit  absolute ADCON0.3;
  ADCON0_GO_nDONE     : bit  absolute ADCON0.2;
  ADCON0_ADON         : bit  absolute ADCON0.1;
  ADCON1              : byte absolute $009E;
  ADCON1_ADFM         : bit  absolute ADCON1.7;
  ADCON1_ADCS2        : bit  absolute ADCON1.6;
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
  LATA_LATA5          : bit  absolute LATA.5;
  LATA_LATA4          : bit  absolute LATA.4;
  LATA_LATA2          : bit  absolute LATA.3;
  LATA_LATA1          : bit  absolute LATA.2;
  LATA_LATA0          : bit  absolute LATA.1;
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
  CMOUT               : byte absolute $0115;
  CMOUT_MC1OUT        : bit  absolute CMOUT.1;
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
  DACCON0             : byte absolute $0118;
  DACCON0_DACEN       : bit  absolute DACCON0.5;
  DACCON0_DACLPS      : bit  absolute DACCON0.4;
  DACCON0_DACOE       : bit  absolute DACCON0.3;
  DACCON0_DACPSS1     : bit  absolute DACCON0.2;
  DACCON0_DACPSS0     : bit  absolute DACCON0.1;
  DACCON1             : byte absolute $0119;
  DACCON1_DACR4       : bit  absolute DACCON1.5;
  DACCON1_DACR3       : bit  absolute DACCON1.4;
  DACCON1_DACR2       : bit  absolute DACCON1.3;
  DACCON1_DACR1       : bit  absolute DACCON1.2;
  DACCON1_DACR0       : bit  absolute DACCON1.1;
  APFCON              : byte absolute $011D;
  APFCON_RXDTSEL      : bit  absolute APFCON.7;
  APFCON_CWGASEL      : bit  absolute APFCON.6;
  APFCON_CWGBSEL      : bit  absolute APFCON.5;
  APFCON_T1GSEL       : bit  absolute APFCON.4;
  APFCON_TXCKSEL      : bit  absolute APFCON.3;
  APFCON_P2SEL        : bit  absolute APFCON.2;
  APFCON_P1SEL        : bit  absolute APFCON.1;
  ANSELA              : byte absolute $018C;
  ANSELA_ANSA4        : bit  absolute ANSELA.4;
  ANSELA_ANSA2        : bit  absolute ANSELA.3;
  ANSELA_ANSA1        : bit  absolute ANSELA.2;
  ANSELA_ANSA0        : bit  absolute ANSELA.1;
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
  RCREG               : byte absolute $0199;
  TXREG               : byte absolute $019A;
  SPBRGL              : byte absolute $019B;
  SPBRGH              : byte absolute $019C;
  RCSTA               : byte absolute $019D;
  RCSTA_SPEN          : bit  absolute RCSTA.7;
  RCSTA_RX9           : bit  absolute RCSTA.6;
  RCSTA_SREN          : bit  absolute RCSTA.5;
  RCSTA_CREN          : bit  absolute RCSTA.4;
  RCSTA_ADDEN         : bit  absolute RCSTA.3;
  RCSTA_FERR          : bit  absolute RCSTA.2;
  RCSTA_OERR          : bit  absolute RCSTA.1;
  RCSTA_RX9D          : bit  absolute RCSTA.0;
  TXSTA               : byte absolute $019E;
  TXSTA_CSRC          : bit  absolute TXSTA.7;
  TXSTA_TX9           : bit  absolute TXSTA.6;
  TXSTA_TXEN          : bit  absolute TXSTA.5;
  TXSTA_SYNC          : bit  absolute TXSTA.4;
  TXSTA_SENDB         : bit  absolute TXSTA.3;
  TXSTA_BRGH          : bit  absolute TXSTA.2;
  TXSTA_TRMT          : bit  absolute TXSTA.1;
  TXSTA_TX9D          : bit  absolute TXSTA.0;
  BAUDCON             : byte absolute $019F;
  BAUDCON_ABDOVF      : bit  absolute BAUDCON.6;
  BAUDCON_RCIDL       : bit  absolute BAUDCON.5;
  BAUDCON_SCKP        : bit  absolute BAUDCON.4;
  BAUDCON_BRG16       : bit  absolute BAUDCON.3;
  BAUDCON_WUE         : bit  absolute BAUDCON.2;
  BAUDCON_ABDEN       : bit  absolute BAUDCON.1;
  WPUA                : byte absolute $020C;
  WPUA_WPUA5          : bit  absolute WPUA.5;
  WPUA_WPUA4          : bit  absolute WPUA.4;
  WPUA_WPUA3          : bit  absolute WPUA.3;
  WPUA_WPUA2          : bit  absolute WPUA.2;
  WPUA_WPUA1          : bit  absolute WPUA.1;
  WPUA_WPUA0          : bit  absolute WPUA.0;
  ODCONA              : byte absolute $028C;
  ODCONA_ODA5         : bit  absolute ODCONA.5;
  ODCONA_ODA4         : bit  absolute ODCONA.4;
  ODCONA_ODA2         : bit  absolute ODCONA.3;
  ODCONA_ODA1         : bit  absolute ODCONA.2;
  ODCONA_ODA0         : bit  absolute ODCONA.1;
  SLRCONA             : byte absolute $030C;
  SLRCONA_SLRA5       : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4       : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2       : bit  absolute SLRCONA.3;
  SLRCONA_SLRA1       : bit  absolute SLRCONA.2;
  SLRCONA_SLRA0       : bit  absolute SLRCONA.1;
  INLVLA              : byte absolute $038C;
  INLVLA_INLVLA5      : bit  absolute INLVLA.5;
  INLVLA_INLVLA4      : bit  absolute INLVLA.4;
  INLVLA_INLVLA3      : bit  absolute INLVLA.3;
  INLVLA_INLVLA2      : bit  absolute INLVLA.2;
  INLVLA_INLVLA1      : bit  absolute INLVLA.1;
  INLVLA_INLVLA0      : bit  absolute INLVLA.0;
  IOCAP               : byte absolute $0391;
  IOCAP_IOCAP5        : bit  absolute IOCAP.6;
  IOCAP_IOCAP4        : bit  absolute IOCAP.5;
  IOCAP_IOCAP3        : bit  absolute IOCAP.4;
  IOCAP_IOCAP2        : bit  absolute IOCAP.3;
  IOCAP_IOCAP1        : bit  absolute IOCAP.2;
  IOCAP_IOCAP0        : bit  absolute IOCAP.1;
  IOCAN               : byte absolute $0392;
  IOCAN_IOCAN5        : bit  absolute IOCAN.6;
  IOCAN_IOCAN4        : bit  absolute IOCAN.5;
  IOCAN_IOCAN3        : bit  absolute IOCAN.4;
  IOCAN_IOCAN2        : bit  absolute IOCAN.3;
  IOCAN_IOCAN1        : bit  absolute IOCAN.2;
  IOCAN_IOCAN0        : bit  absolute IOCAN.1;
  IOCAF               : byte absolute $0393;
  IOCAF_IOCAF5        : bit  absolute IOCAF.6;
  IOCAF_IOCAF4        : bit  absolute IOCAF.5;
  IOCAF_IOCAF3        : bit  absolute IOCAF.4;
  IOCAF_IOCAF2        : bit  absolute IOCAF.3;
  IOCAF_IOCAF1        : bit  absolute IOCAF.2;
  IOCAF_IOCAF0        : bit  absolute IOCAF.1;
  CWG1DBR             : byte absolute $0691;
  CWG1DBR_CWG1DBR5    : bit  absolute CWG1DBR.5;
  CWG1DBR_CWG1DBR4    : bit  absolute CWG1DBR.4;
  CWG1DBR_CWG1DBR3    : bit  absolute CWG1DBR.3;
  CWG1DBR_CWG1DBR2    : bit  absolute CWG1DBR.2;
  CWG1DBR_CWG1DBR1    : bit  absolute CWG1DBR.1;
  CWG1DBR_CWG1DBR0    : bit  absolute CWG1DBR.0;
  CWG1DBF             : byte absolute $0692;
  CWG1DBF_CWG1DBF5    : bit  absolute CWG1DBF.5;
  CWG1DBF_CWG1DBF4    : bit  absolute CWG1DBF.4;
  CWG1DBF_CWG1DBF3    : bit  absolute CWG1DBF.3;
  CWG1DBF_CWG1DBF2    : bit  absolute CWG1DBF.2;
  CWG1DBF_CWG1DBF1    : bit  absolute CWG1DBF.1;
  CWG1DBF_CWG1DBF0    : bit  absolute CWG1DBF.0;
  CWG1CON0            : byte absolute $0693;
  CWG1CON0_G1EN       : bit  absolute CWG1CON0.6;
  CWG1CON0_G1OEB      : bit  absolute CWG1CON0.5;
  CWG1CON0_G1OEA      : bit  absolute CWG1CON0.4;
  CWG1CON0_G1POLB     : bit  absolute CWG1CON0.3;
  CWG1CON0_G1POLA     : bit  absolute CWG1CON0.2;
  CWG1CON0_G1CS0      : bit  absolute CWG1CON0.1;
  CWG1CON1            : byte absolute $0694;
  CWG1CON1_G1ASDLB1   : bit  absolute CWG1CON1.7;
  CWG1CON1_G1ASDLB0   : bit  absolute CWG1CON1.6;
  CWG1CON1_G1ASDLA1   : bit  absolute CWG1CON1.5;
  CWG1CON1_G1ASDLA0   : bit  absolute CWG1CON1.4;
  CWG1CON1_G1IS2      : bit  absolute CWG1CON1.3;
  CWG1CON1_G1IS1      : bit  absolute CWG1CON1.2;
  CWG1CON1_G1IS0      : bit  absolute CWG1CON1.1;
  CWG1CON2            : byte absolute $0695;
  CWG1CON2_G1ASE      : bit  absolute CWG1CON2.4;
  CWG1CON2_G1ARSEN    : bit  absolute CWG1CON2.3;
  CWG1CON2_G1ASDSC1   : bit  absolute CWG1CON2.2;
  CWG1CON2_G1ASDSFLT  : bit  absolute CWG1CON2.1;
  PWMEN               : byte absolute $0D8E;
  PWMEN_PWM3EN_A      : bit  absolute PWMEN.2;
  PWMEN_PWM2EN_A      : bit  absolute PWMEN.1;
  PWMEN_PWM1EN_A      : bit  absolute PWMEN.0;
  PWMLD               : byte absolute $0D8F;
  PWMLD_PWM3LDA_A     : bit  absolute PWMLD.2;
  PWMLD_PWM2LDA_A     : bit  absolute PWMLD.1;
  PWMLD_PWM1LDA_A     : bit  absolute PWMLD.0;
  PWMOUT              : byte absolute $0D90;
  PWMOUT_PWM3OUT_A    : bit  absolute PWMOUT.2;
  PWMOUT_PWM2OUT_A    : bit  absolute PWMOUT.1;
  PWMOUT_PWM1OUT_A    : bit  absolute PWMOUT.0;
  PWM1PHL             : byte absolute $0D91;
  PWM1PHL_PWM1PHL7    : bit  absolute PWM1PHL.7;
  PWM1PHL_PWM1PHL6    : bit  absolute PWM1PHL.6;
  PWM1PHL_PWM1PHL5    : bit  absolute PWM1PHL.5;
  PWM1PHL_PWM1PHL4    : bit  absolute PWM1PHL.4;
  PWM1PHL_PWM1PHL3    : bit  absolute PWM1PHL.3;
  PWM1PHL_PWM1PHL2    : bit  absolute PWM1PHL.2;
  PWM1PHL_PWM1PHL1    : bit  absolute PWM1PHL.1;
  PWM1PHL_PWM1PHL0    : bit  absolute PWM1PHL.0;
  PWM1PHH             : byte absolute $0D92;
  PWM1PHH_PWM1PHH7    : bit  absolute PWM1PHH.7;
  PWM1PHH_PWM1PHH6    : bit  absolute PWM1PHH.6;
  PWM1PHH_PWM1PHH5    : bit  absolute PWM1PHH.5;
  PWM1PHH_PWM1PHH4    : bit  absolute PWM1PHH.4;
  PWM1PHH_PWM1PHH3    : bit  absolute PWM1PHH.3;
  PWM1PHH_PWM1PHH2    : bit  absolute PWM1PHH.2;
  PWM1PHH_PWM1PHH1    : bit  absolute PWM1PHH.1;
  PWM1PHH_PWM1PHH0    : bit  absolute PWM1PHH.0;
  PWM1DCL             : byte absolute $0D93;
  PWM1DCL_PWM1DCL7    : bit  absolute PWM1DCL.7;
  PWM1DCL_PWM1DCL6    : bit  absolute PWM1DCL.6;
  PWM1DCL_PWM1DCL5    : bit  absolute PWM1DCL.5;
  PWM1DCL_PWM1DCL4    : bit  absolute PWM1DCL.4;
  PWM1DCL_PWM1DCL3    : bit  absolute PWM1DCL.3;
  PWM1DCL_PWM1DCL2    : bit  absolute PWM1DCL.2;
  PWM1DCL_PWM1DCL1    : bit  absolute PWM1DCL.1;
  PWM1DCL_PWM1DCL0    : bit  absolute PWM1DCL.0;
  PWM1DCH             : byte absolute $0D94;
  PWM1DCH_PWM1DCH7    : bit  absolute PWM1DCH.7;
  PWM1DCH_PWM1DCH6    : bit  absolute PWM1DCH.6;
  PWM1DCH_PWM1DCH5    : bit  absolute PWM1DCH.5;
  PWM1DCH_PWM1DCH4    : bit  absolute PWM1DCH.4;
  PWM1DCH_PWM1DCH3    : bit  absolute PWM1DCH.3;
  PWM1DCH_PWM1DCH2    : bit  absolute PWM1DCH.2;
  PWM1DCH_PWM1DCH1    : bit  absolute PWM1DCH.1;
  PWM1DCH_PWM1DCH0    : bit  absolute PWM1DCH.0;
  PWM1PRL             : byte absolute $0D95;
  PWM1PRL_PWM1PRL7    : bit  absolute PWM1PRL.7;
  PWM1PRL_PWM1PRL6    : bit  absolute PWM1PRL.6;
  PWM1PRL_PWM1PRL5    : bit  absolute PWM1PRL.5;
  PWM1PRL_PWM1PRL4    : bit  absolute PWM1PRL.4;
  PWM1PRL_PWM1PRL3    : bit  absolute PWM1PRL.3;
  PWM1PRL_PWM1PRL2    : bit  absolute PWM1PRL.2;
  PWM1PRL_PWM1PRL1    : bit  absolute PWM1PRL.1;
  PWM1PRL_PWM1PRL0    : bit  absolute PWM1PRL.0;
  PWM1PRH             : byte absolute $0D96;
  PWM1PRH_PWM1PRH7    : bit  absolute PWM1PRH.7;
  PWM1PRH_PWM1PRH6    : bit  absolute PWM1PRH.6;
  PWM1PRH_PWM1PRH5    : bit  absolute PWM1PRH.5;
  PWM1PRH_PWM1PRH4    : bit  absolute PWM1PRH.4;
  PWM1PRH_PWM1PRH3    : bit  absolute PWM1PRH.3;
  PWM1PRH_PWM1PRH2    : bit  absolute PWM1PRH.2;
  PWM1PRH_PWM1PRH1    : bit  absolute PWM1PRH.1;
  PWM1PRH_PWM1PRH0    : bit  absolute PWM1PRH.0;
  PWM1OFL             : byte absolute $0D97;
  PWM1OFL_PWM1OFL7    : bit  absolute PWM1OFL.7;
  PWM1OFL_PWM1OFL6    : bit  absolute PWM1OFL.6;
  PWM1OFL_PWM1OFL5    : bit  absolute PWM1OFL.5;
  PWM1OFL_PWM1OFL4    : bit  absolute PWM1OFL.4;
  PWM1OFL_PWM1OFL3    : bit  absolute PWM1OFL.3;
  PWM1OFL_PWM1OFL2    : bit  absolute PWM1OFL.2;
  PWM1OFL_PWM1OFL1    : bit  absolute PWM1OFL.1;
  PWM1OFL_PWM1OFL0    : bit  absolute PWM1OFL.0;
  PWM1OFH             : byte absolute $0D98;
  PWM1OFH_PWM1OFH7    : bit  absolute PWM1OFH.7;
  PWM1OFH_PWM1OFH6    : bit  absolute PWM1OFH.6;
  PWM1OFH_PWM1OFH5    : bit  absolute PWM1OFH.5;
  PWM1OFH_PWM1OFH4    : bit  absolute PWM1OFH.4;
  PWM1OFH_PWM1OFH3    : bit  absolute PWM1OFH.3;
  PWM1OFH_PWM1OFH2    : bit  absolute PWM1OFH.2;
  PWM1OFH_PWM1OFH1    : bit  absolute PWM1OFH.1;
  PWM1OFH_PWM1OFH0    : bit  absolute PWM1OFH.0;
  PWM1TMRL            : byte absolute $0D99;
  PWM1TMRL_PWM1TMRL7  : bit  absolute PWM1TMRL.7;
  PWM1TMRL_PWM1TMRL6  : bit  absolute PWM1TMRL.6;
  PWM1TMRL_PWM1TMRL5  : bit  absolute PWM1TMRL.5;
  PWM1TMRL_PWM1TMRL4  : bit  absolute PWM1TMRL.4;
  PWM1TMRL_PWM1TMRL3  : bit  absolute PWM1TMRL.3;
  PWM1TMRL_PWM1TMRL2  : bit  absolute PWM1TMRL.2;
  PWM1TMRL_PWM1TMRL1  : bit  absolute PWM1TMRL.1;
  PWM1TMRL_PWM1TMRL0  : bit  absolute PWM1TMRL.0;
  PWM1TMRH            : byte absolute $0D9A;
  PWM1TMRH_PWM1TMRH7  : bit  absolute PWM1TMRH.7;
  PWM1TMRH_PWM1TMRH6  : bit  absolute PWM1TMRH.6;
  PWM1TMRH_PWM1TMRH5  : bit  absolute PWM1TMRH.5;
  PWM1TMRH_PWM1TMRH4  : bit  absolute PWM1TMRH.4;
  PWM1TMRH_PWM1TMRH3  : bit  absolute PWM1TMRH.3;
  PWM1TMRH_PWM1TMRH2  : bit  absolute PWM1TMRH.2;
  PWM1TMRH_PWM1TMRH1  : bit  absolute PWM1TMRH.1;
  PWM1TMRH_PWM1TMRH0  : bit  absolute PWM1TMRH.0;
  PWM1CON             : byte absolute $0D9B;
  PWM1CON_EN          : bit  absolute PWM1CON.7;
  PWM1CON_OE          : bit  absolute PWM1CON.6;
  PWM1CON_OUT         : bit  absolute PWM1CON.5;
  PWM1CON_POL         : bit  absolute PWM1CON.4;
  PWM1CON_MODE1       : bit  absolute PWM1CON.3;
  PWM1CON_PWM1MODE1   : bit  absolute PWM1CON.2;
  PWM1CON_PWM1MODE0   : bit  absolute PWM1CON.1;
  PWM1INTE            : byte absolute $0D9C;
  PWM1INTE_OFIE       : bit  absolute PWM1INTE.3;
  PWM1INTE_PHIE       : bit  absolute PWM1INTE.2;
  PWM1INTE_DCIE       : bit  absolute PWM1INTE.1;
  PWM1INTE_PRIE       : bit  absolute PWM1INTE.0;
  PWM1INTF            : byte absolute $0D9D;
  PWM1INTF_OFIF       : bit  absolute PWM1INTF.3;
  PWM1INTF_PHIF       : bit  absolute PWM1INTF.2;
  PWM1INTF_DCIF       : bit  absolute PWM1INTF.1;
  PWM1INTF_PRIF       : bit  absolute PWM1INTF.0;
  PWM1CLKCON          : byte absolute $0D9E;
  PWM1CLKCON_PWM1PS2  : bit  absolute PWM1CLKCON.5;
  PWM1CLKCON_PWM1PS1  : bit  absolute PWM1CLKCON.4;
  PWM1CLKCON_PWM1PS0  : bit  absolute PWM1CLKCON.3;
  PWM1CLKCON_PWM1CS1  : bit  absolute PWM1CLKCON.2;
  PWM1CLKCON_PWM1CS0  : bit  absolute PWM1CLKCON.1;
  PWM1LDCON           : byte absolute $0D9F;
  PWM1LDCON_PWM1LD    : bit  absolute PWM1LDCON.7;
  PWM1LDCON_PWM1LDM   : bit  absolute PWM1LDCON.6;
  PWM1LDCON_LDA       : bit  absolute PWM1LDCON.4;
  PWM1LDCON_LDT       : bit  absolute PWM1LDCON.3;
  PWM1LDCON_PWM1LDS1  : bit  absolute PWM1LDCON.2;
  PWM1LDCON_PWM1LDS0  : bit  absolute PWM1LDCON.1;
  PWM1LDCON_LDS0      : bit  absolute PWM1LDCON.0;
  PWM1OFCON           : byte absolute $0DA0;
  PWM1OFCON_OFM1      : bit  absolute PWM1OFCON.6;
  PWM1OFCON_OFM0      : bit  absolute PWM1OFCON.5;
  PWM1OFCON_PWM1OFM1  : bit  absolute PWM1OFCON.4;
  PWM1OFCON_OFO       : bit  absolute PWM1OFCON.3;
  PWM1OFCON_PWM1OFS1  : bit  absolute PWM1OFCON.2;
  PWM1OFCON_PWM1OFS0  : bit  absolute PWM1OFCON.1;
  PWM2PHL             : byte absolute $0DA1;
  PWM2PHL_PWM2PHL7    : bit  absolute PWM2PHL.7;
  PWM2PHL_PWM2PHL6    : bit  absolute PWM2PHL.6;
  PWM2PHL_PWM2PHL5    : bit  absolute PWM2PHL.5;
  PWM2PHL_PWM2PHL4    : bit  absolute PWM2PHL.4;
  PWM2PHL_PWM2PHL3    : bit  absolute PWM2PHL.3;
  PWM2PHL_PWM2PHL2    : bit  absolute PWM2PHL.2;
  PWM2PHL_PWM2PHL1    : bit  absolute PWM2PHL.1;
  PWM2PHL_PWM2PHL0    : bit  absolute PWM2PHL.0;
  PWM2PHH             : byte absolute $0DA2;
  PWM2PHH_PWM2PHH7    : bit  absolute PWM2PHH.7;
  PWM2PHH_PWM2PHH6    : bit  absolute PWM2PHH.6;
  PWM2PHH_PWM2PHH5    : bit  absolute PWM2PHH.5;
  PWM2PHH_PWM2PHH4    : bit  absolute PWM2PHH.4;
  PWM2PHH_PWM2PHH3    : bit  absolute PWM2PHH.3;
  PWM2PHH_PWM2PHH2    : bit  absolute PWM2PHH.2;
  PWM2PHH_PWM2PHH1    : bit  absolute PWM2PHH.1;
  PWM2PHH_PWM2PHH0    : bit  absolute PWM2PHH.0;
  PWM2DCL             : byte absolute $0DA3;
  PWM2DCL_PWM2DCL7    : bit  absolute PWM2DCL.7;
  PWM2DCL_PWM2DCL6    : bit  absolute PWM2DCL.6;
  PWM2DCL_PWM2DCL5    : bit  absolute PWM2DCL.5;
  PWM2DCL_PWM2DCL4    : bit  absolute PWM2DCL.4;
  PWM2DCL_PWM2DCL3    : bit  absolute PWM2DCL.3;
  PWM2DCL_PWM2DCL2    : bit  absolute PWM2DCL.2;
  PWM2DCL_PWM2DCL1    : bit  absolute PWM2DCL.1;
  PWM2DCL_PWM2DCL0    : bit  absolute PWM2DCL.0;
  PWM2DCH             : byte absolute $0DA4;
  PWM2DCH_PWM2DCH7    : bit  absolute PWM2DCH.7;
  PWM2DCH_PWM2DCH6    : bit  absolute PWM2DCH.6;
  PWM2DCH_PWM2DCH5    : bit  absolute PWM2DCH.5;
  PWM2DCH_PWM2DCH4    : bit  absolute PWM2DCH.4;
  PWM2DCH_PWM2DCH3    : bit  absolute PWM2DCH.3;
  PWM2DCH_PWM2DCH2    : bit  absolute PWM2DCH.2;
  PWM2DCH_PWM2DCH1    : bit  absolute PWM2DCH.1;
  PWM2DCH_PWM2DCH0    : bit  absolute PWM2DCH.0;
  PWM2PRL             : byte absolute $0DA5;
  PWM2PRL_PWM2PRL7    : bit  absolute PWM2PRL.7;
  PWM2PRL_PWM2PRL6    : bit  absolute PWM2PRL.6;
  PWM2PRL_PWM2PRL5    : bit  absolute PWM2PRL.5;
  PWM2PRL_PWM2PRL4    : bit  absolute PWM2PRL.4;
  PWM2PRL_PWM2PRL3    : bit  absolute PWM2PRL.3;
  PWM2PRL_PWM2PRL2    : bit  absolute PWM2PRL.2;
  PWM2PRL_PWM2PRL1    : bit  absolute PWM2PRL.1;
  PWM2PRL_PWM2PRL0    : bit  absolute PWM2PRL.0;
  PWM2PRH             : byte absolute $0DA6;
  PWM2PRH_PWM2PRH7    : bit  absolute PWM2PRH.7;
  PWM2PRH_PWM2PRH6    : bit  absolute PWM2PRH.6;
  PWM2PRH_PWM2PRH5    : bit  absolute PWM2PRH.5;
  PWM2PRH_PWM2PRH4    : bit  absolute PWM2PRH.4;
  PWM2PRH_PWM2PRH3    : bit  absolute PWM2PRH.3;
  PWM2PRH_PWM2PRH2    : bit  absolute PWM2PRH.2;
  PWM2PRH_PWM2PRH1    : bit  absolute PWM2PRH.1;
  PWM2PRH_PWM2PRH0    : bit  absolute PWM2PRH.0;
  PWM2OFL             : byte absolute $0DA7;
  PWM2OFL_PWM2OFL7    : bit  absolute PWM2OFL.7;
  PWM2OFL_PWM2OFL6    : bit  absolute PWM2OFL.6;
  PWM2OFL_PWM2OFL5    : bit  absolute PWM2OFL.5;
  PWM2OFL_PWM2OFL4    : bit  absolute PWM2OFL.4;
  PWM2OFL_PWM2OFL3    : bit  absolute PWM2OFL.3;
  PWM2OFL_PWM2OFL2    : bit  absolute PWM2OFL.2;
  PWM2OFL_PWM2OFL1    : bit  absolute PWM2OFL.1;
  PWM2OFL_PWM2OFL0    : bit  absolute PWM2OFL.0;
  PWM2OFH             : byte absolute $0DA8;
  PWM2OFH_PWM2OFH7    : bit  absolute PWM2OFH.7;
  PWM2OFH_PWM2OFH6    : bit  absolute PWM2OFH.6;
  PWM2OFH_PWM2OFH5    : bit  absolute PWM2OFH.5;
  PWM2OFH_PWM2OFH4    : bit  absolute PWM2OFH.4;
  PWM2OFH_PWM2OFH3    : bit  absolute PWM2OFH.3;
  PWM2OFH_PWM2OFH2    : bit  absolute PWM2OFH.2;
  PWM2OFH_PWM2OFH1    : bit  absolute PWM2OFH.1;
  PWM2OFH_PWM2OFH0    : bit  absolute PWM2OFH.0;
  PWM2TMRL            : byte absolute $0DA9;
  PWM2TMRL_PWM2TMRL7  : bit  absolute PWM2TMRL.7;
  PWM2TMRL_PWM2TMRL6  : bit  absolute PWM2TMRL.6;
  PWM2TMRL_PWM2TMRL5  : bit  absolute PWM2TMRL.5;
  PWM2TMRL_PWM2TMRL4  : bit  absolute PWM2TMRL.4;
  PWM2TMRL_PWM2TMRL3  : bit  absolute PWM2TMRL.3;
  PWM2TMRL_PWM2TMRL2  : bit  absolute PWM2TMRL.2;
  PWM2TMRL_PWM2TMRL1  : bit  absolute PWM2TMRL.1;
  PWM2TMRL_PWM2TMRL0  : bit  absolute PWM2TMRL.0;
  PWM2TMRH            : byte absolute $0DAA;
  PWM2TMRH_PWM2TMRH7  : bit  absolute PWM2TMRH.7;
  PWM2TMRH_PWM2TMRH6  : bit  absolute PWM2TMRH.6;
  PWM2TMRH_PWM2TMRH5  : bit  absolute PWM2TMRH.5;
  PWM2TMRH_PWM2TMRH4  : bit  absolute PWM2TMRH.4;
  PWM2TMRH_PWM2TMRH3  : bit  absolute PWM2TMRH.3;
  PWM2TMRH_PWM2TMRH2  : bit  absolute PWM2TMRH.2;
  PWM2TMRH_PWM2TMRH1  : bit  absolute PWM2TMRH.1;
  PWM2TMRH_PWM2TMRH0  : bit  absolute PWM2TMRH.0;
  PWM2CON             : byte absolute $0DAB;
  PWM2CON_PWM2MODE1   : bit  absolute PWM2CON.2;
  PWM2CON_PWM2MODE0   : bit  absolute PWM2CON.1;
  PWM2INTE            : byte absolute $0DAC;
  PWM2INTF            : byte absolute $0DAD;
  PWM2CLKCON          : byte absolute $0DAE;
  PWM2CLKCON_PWM2PS2  : bit  absolute PWM2CLKCON.5;
  PWM2CLKCON_PWM2PS1  : bit  absolute PWM2CLKCON.4;
  PWM2CLKCON_PWM2PS0  : bit  absolute PWM2CLKCON.3;
  PWM2CLKCON_PWM2CS1  : bit  absolute PWM2CLKCON.2;
  PWM2CLKCON_PWM2CS0  : bit  absolute PWM2CLKCON.1;
  PWM2LDCON           : byte absolute $0DAF;
  PWM2LDCON_PWM2LD    : bit  absolute PWM2LDCON.7;
  PWM2LDCON_PWM2LDM   : bit  absolute PWM2LDCON.6;
  PWM2LDCON_PWM2LDS1  : bit  absolute PWM2LDCON.2;
  PWM2LDCON_PWM2LDS0  : bit  absolute PWM2LDCON.1;
  PWM2OFCON           : byte absolute $0DB0;
  PWM2OFCON_PWM2OFM1  : bit  absolute PWM2OFCON.4;
  PWM2OFCON_PWM2OFS1  : bit  absolute PWM2OFCON.2;
  PWM2OFCON_PWM2OFS0  : bit  absolute PWM2OFCON.1;
  PWM3PHL             : byte absolute $0DB1;
  PWM3PHL_PWM3PHL7    : bit  absolute PWM3PHL.7;
  PWM3PHL_PWM3PHL6    : bit  absolute PWM3PHL.6;
  PWM3PHL_PWM3PHL5    : bit  absolute PWM3PHL.5;
  PWM3PHL_PWM3PHL4    : bit  absolute PWM3PHL.4;
  PWM3PHL_PWM3PHL3    : bit  absolute PWM3PHL.3;
  PWM3PHL_PWM3PHL2    : bit  absolute PWM3PHL.2;
  PWM3PHL_PWM3PHL1    : bit  absolute PWM3PHL.1;
  PWM3PHL_PWM3PHL0    : bit  absolute PWM3PHL.0;
  PWM3PHH             : byte absolute $0DB2;
  PWM3PHH_PWM3PHH7    : bit  absolute PWM3PHH.7;
  PWM3PHH_PWM3PHH6    : bit  absolute PWM3PHH.6;
  PWM3PHH_PWM3PHH5    : bit  absolute PWM3PHH.5;
  PWM3PHH_PWM3PHH4    : bit  absolute PWM3PHH.4;
  PWM3PHH_PWM3PHH3    : bit  absolute PWM3PHH.3;
  PWM3PHH_PWM3PHH2    : bit  absolute PWM3PHH.2;
  PWM3PHH_PWM3PHH1    : bit  absolute PWM3PHH.1;
  PWM3PHH_PWM3PHH0    : bit  absolute PWM3PHH.0;
  PWM3DCL             : byte absolute $0DB3;
  PWM3DCL_PWM3DCL7    : bit  absolute PWM3DCL.7;
  PWM3DCL_PWM3DCL6    : bit  absolute PWM3DCL.6;
  PWM3DCL_PWM3DCL5    : bit  absolute PWM3DCL.5;
  PWM3DCL_PWM3DCL4    : bit  absolute PWM3DCL.4;
  PWM3DCL_PWM3DCL3    : bit  absolute PWM3DCL.3;
  PWM3DCL_PWM3DCL2    : bit  absolute PWM3DCL.2;
  PWM3DCL_PWM3DCL1    : bit  absolute PWM3DCL.1;
  PWM3DCL_PWM3DCL0    : bit  absolute PWM3DCL.0;
  PWM3DCH             : byte absolute $0DB4;
  PWM3DCH_PWM3DCH7    : bit  absolute PWM3DCH.7;
  PWM3DCH_PWM3DCH6    : bit  absolute PWM3DCH.6;
  PWM3DCH_PWM3DCH5    : bit  absolute PWM3DCH.5;
  PWM3DCH_PWM3DCH4    : bit  absolute PWM3DCH.4;
  PWM3DCH_PWM3DCH3    : bit  absolute PWM3DCH.3;
  PWM3DCH_PWM3DCH2    : bit  absolute PWM3DCH.2;
  PWM3DCH_PWM3DCH1    : bit  absolute PWM3DCH.1;
  PWM3DCH_PWM3DCH0    : bit  absolute PWM3DCH.0;
  PWM3PRL             : byte absolute $0DB5;
  PWM3PRL_PWM3PRL7    : bit  absolute PWM3PRL.7;
  PWM3PRL_PWM3PRL6    : bit  absolute PWM3PRL.6;
  PWM3PRL_PWM3PRL5    : bit  absolute PWM3PRL.5;
  PWM3PRL_PWM3PRL4    : bit  absolute PWM3PRL.4;
  PWM3PRL_PWM3PRL3    : bit  absolute PWM3PRL.3;
  PWM3PRL_PWM3PRL2    : bit  absolute PWM3PRL.2;
  PWM3PRL_PWM3PRL1    : bit  absolute PWM3PRL.1;
  PWM3PRL_PWM3PRL0    : bit  absolute PWM3PRL.0;
  PWM3PRH             : byte absolute $0DB6;
  PWM3PRH_PWM3PRH7    : bit  absolute PWM3PRH.7;
  PWM3PRH_PWM3PRH6    : bit  absolute PWM3PRH.6;
  PWM3PRH_PWM3PRH5    : bit  absolute PWM3PRH.5;
  PWM3PRH_PWM3PRH4    : bit  absolute PWM3PRH.4;
  PWM3PRH_PWM3PRH3    : bit  absolute PWM3PRH.3;
  PWM3PRH_PWM3PRH2    : bit  absolute PWM3PRH.2;
  PWM3PRH_PWM3PRH1    : bit  absolute PWM3PRH.1;
  PWM3PRH_PWM3PRH0    : bit  absolute PWM3PRH.0;
  PWM3OFL             : byte absolute $0DB7;
  PWM3OFL_PWM3OFL7    : bit  absolute PWM3OFL.7;
  PWM3OFL_PWM3OFL6    : bit  absolute PWM3OFL.6;
  PWM3OFL_PWM3OFL5    : bit  absolute PWM3OFL.5;
  PWM3OFL_PWM3OFL4    : bit  absolute PWM3OFL.4;
  PWM3OFL_PWM3OFL3    : bit  absolute PWM3OFL.3;
  PWM3OFL_PWM3OFL2    : bit  absolute PWM3OFL.2;
  PWM3OFL_PWM3OFL1    : bit  absolute PWM3OFL.1;
  PWM3OFL_PWM3OFL0    : bit  absolute PWM3OFL.0;
  PWM3OFH             : byte absolute $0DB8;
  PWM3OFH_PWM3OFH7    : bit  absolute PWM3OFH.7;
  PWM3OFH_PWM3OFH6    : bit  absolute PWM3OFH.6;
  PWM3OFH_PWM3OFH5    : bit  absolute PWM3OFH.5;
  PWM3OFH_PWM3OFH4    : bit  absolute PWM3OFH.4;
  PWM3OFH_PWM3OFH3    : bit  absolute PWM3OFH.3;
  PWM3OFH_PWM3OFH2    : bit  absolute PWM3OFH.2;
  PWM3OFH_PWM3OFH1    : bit  absolute PWM3OFH.1;
  PWM3OFH_PWM3OFH0    : bit  absolute PWM3OFH.0;
  PWM3TMRL            : byte absolute $0DB9;
  PWM3TMRL_PWM3TMRL7  : bit  absolute PWM3TMRL.7;
  PWM3TMRL_PWM3TMRL6  : bit  absolute PWM3TMRL.6;
  PWM3TMRL_PWM3TMRL5  : bit  absolute PWM3TMRL.5;
  PWM3TMRL_PWM3TMRL4  : bit  absolute PWM3TMRL.4;
  PWM3TMRL_PWM3TMRL3  : bit  absolute PWM3TMRL.3;
  PWM3TMRL_PWM3TMRL2  : bit  absolute PWM3TMRL.2;
  PWM3TMRL_PWM3TMRL1  : bit  absolute PWM3TMRL.1;
  PWM3TMRL_PWM3TMRL0  : bit  absolute PWM3TMRL.0;
  PWM3TMRH            : byte absolute $0DBA;
  PWM3TMRH_PWM3TMRH7  : bit  absolute PWM3TMRH.7;
  PWM3TMRH_PWM3TMRH6  : bit  absolute PWM3TMRH.6;
  PWM3TMRH_PWM3TMRH5  : bit  absolute PWM3TMRH.5;
  PWM3TMRH_PWM3TMRH4  : bit  absolute PWM3TMRH.4;
  PWM3TMRH_PWM3TMRH3  : bit  absolute PWM3TMRH.3;
  PWM3TMRH_PWM3TMRH2  : bit  absolute PWM3TMRH.2;
  PWM3TMRH_PWM3TMRH1  : bit  absolute PWM3TMRH.1;
  PWM3TMRH_PWM3TMRH0  : bit  absolute PWM3TMRH.0;
  PWM3CON             : byte absolute $0DBB;
  PWM3CON_PWM3MODE1   : bit  absolute PWM3CON.2;
  PWM3CON_PWM3MODE0   : bit  absolute PWM3CON.1;
  PWM3INTE            : byte absolute $0DBC;
  PWM3INTF            : byte absolute $0DBD;
  PWM3CLKCON          : byte absolute $0DBE;
  PWM3CLKCON_PWM3PS2  : bit  absolute PWM3CLKCON.5;
  PWM3CLKCON_PWM3PS1  : bit  absolute PWM3CLKCON.4;
  PWM3CLKCON_PWM3PS0  : bit  absolute PWM3CLKCON.3;
  PWM3CLKCON_PWM3CS1  : bit  absolute PWM3CLKCON.2;
  PWM3CLKCON_PWM3CS0  : bit  absolute PWM3CLKCON.1;
  PWM3LDCON           : byte absolute $0DBF;
  PWM3LDCON_PWM3LD    : bit  absolute PWM3LDCON.7;
  PWM3LDCON_PWM3LDM   : bit  absolute PWM3LDCON.6;
  PWM3LDCON_PWM3LDS1  : bit  absolute PWM3LDCON.2;
  PWM3LDCON_PWM3LDS0  : bit  absolute PWM3LDCON.1;
  PWM3OFCON           : byte absolute $0DC0;
  PWM3OFCON_PWM3OFM1  : bit  absolute PWM3OFCON.4;
  PWM3OFCON_PWM3OFS1  : bit  absolute PWM3OFCON.2;
  PWM3OFCON_PWM3OFS0  : bit  absolute PWM3OFCON.1;
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
  {$SET_STATE_RAM '011-013:SFR'}  // PIR1, PIR2, PIR3
  {$SET_STATE_RAM '015-01C:SFR'}  // TMR0, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-080:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '08C-08C:SFR'}  // TRISA
  {$SET_STATE_RAM '091-093:SFR'}  // PIE1, PIE2, PIE3
  {$SET_STATE_RAM '095-09F:SFR'}  // OPTION_REG, PCON, WDTCON, OSCTUNE, OSCCON, OSCSTAT, ADRESL, ADRESH, ADCON0, ADCON1, ADCON2
  {$SET_STATE_RAM '0A0-0EF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-100:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '10C-10C:SFR'}  // LATA
  {$SET_STATE_RAM '111-112:SFR'}  // CM1CON0, CM1CON1
  {$SET_STATE_RAM '115-119:SFR'}  // CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '11D-11D:SFR'}  // APFCON
  {$SET_STATE_RAM '120-16F:GPR'} 
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-180:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '18C-18C:SFR'}  // ANSELA
  {$SET_STATE_RAM '191-197:SFR'}  // PMADRL, PMADRH, PMDATL, PMDATH, PMCON1, PMCON2, VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}  // RCREG, TXREG, SPBRGL, SPBRGH, RCSTA, TXSTA, BAUDCON
  {$SET_STATE_RAM '1F0-1FF:GPR'} 
  {$SET_STATE_RAM '200-200:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '20C-20C:SFR'}  // WPUA
  {$SET_STATE_RAM '270-27F:GPR'} 
  {$SET_STATE_RAM '280-280:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '28C-28C:SFR'}  // ODCONA
  {$SET_STATE_RAM '2F0-2FF:GPR'} 
  {$SET_STATE_RAM '300-300:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '30C-30C:SFR'}  // SLRCONA
  {$SET_STATE_RAM '370-37F:GPR'} 
  {$SET_STATE_RAM '380-380:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '38C-38C:SFR'}  // INLVLA
  {$SET_STATE_RAM '391-393:SFR'}  // IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '3F0-3FF:GPR'} 
  {$SET_STATE_RAM '400-400:SFR'}  // mapped to INDF0
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
  {$SET_STATE_RAM '691-695:SFR'}  // CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1CON2
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
  {$SET_STATE_RAM 'D8E-DC0:SFR'}  // PWMEN, PWMLD, PWMOUT, PWM1PHL, PWM1PHH, PWM1DCL, PWM1DCH, PWM1PRL, PWM1PRH, PWM1OFL, PWM1OFH, PWM1TMRL, PWM1TMRH, PWM1CON, PWM1INTE, PWM1INTF, PWM1CLKCON, PWM1LDCON, PWM1OFCON, PWM2PHL, PWM2PHH, PWM2DCL, PWM2DCH, PWM2PRL, PWM2PRH, PWM2OFL, PWM2OFH, PWM2TMRL, PWM2TMRH, PWM2CON, PWM2INTE, PWM2INTF, PWM2CLKCON, PWM2LDCON, PWM2OFCON, PWM3PHL, PWM3PHH, PWM3DCL, PWM3DCH, PWM3PRL, PWM3PRH, PWM3OFL, PWM3OFH, PWM3TMRL, PWM3TMRH, PWM3CON, PWM3INTE, PWM3INTF, PWM3CLKCON, PWM3LDCON, PWM3OFCON
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
  {$SET_UNIMP_BITS '011:F3'} // PIR1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:20'} // PIR2 bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '013:70'} // PIR3 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:F3'} // PIE1 bits 3,2 un-implemented (read as 0)
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
  {$SET_UNIMP_BITS '11D:EF'} // APFCON bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:17'} // ANSELA bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '192:7F'} // PMADRH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '194:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '195:7F'} // PMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:02'} // VREGCON bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUDCON bits 5,2 un-implemented (read as 0)
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
  // Pin  2 : RA5/T1CKI/CWG1A/PWM1/RX/DT/CLKIN
  // Pin  3 : T1G/RA4/AN3/C1IN1-/PWM2/TX/CK/CWG1B/CLKOUT
  // Pin  4 : RA3/T1G/MCLR/Vpp
  // Pin  5 : RA2/AN2/T0CKI/PWM3/C1OUT/CWG1FLT/CWG1A/INT
  // Pin  6 : RA1/AN1/VREF+/C1IN0-/PWM1/RX/DT/ICSPCLK/ICDCLK
  // Pin  7 : RA0/AN0/C1IN+/PWM2/TX/CK/DAC1OUT/CWG1B/ICSPDAT/ICDDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:1-7,2-6,3-5,4-4,5-3,6-2'} // PORTA


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

  // LPBOREN : Low Power Brown-out Reset enable bit
  {$define _LPBOREN_OFF  = $3FFF}  // LPBOR is disabled
  {$define _LPBOREN_ON   = $37FF}  // LPBOR is enabled

  // LVP : Low-Voltage Programming Enable
  {$define _LVP_ON       = $3FFF}  // Low-voltage programming enabled
  {$define _LVP_OFF      = $1FFF}  // High-voltage on MCLR/VPP must be used for programming

implementation
end.
