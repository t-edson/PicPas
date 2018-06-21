unit PIC16F707;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F707'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 40}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 8192}

interface
var
  INDF               : byte absolute $0000;
  TMR0               : byte absolute $0001;
  PCL                : byte absolute $0002;
  STATUS             : byte absolute $0003;
  STATUS_IRP         : bit  absolute STATUS.7;
  STATUS_RP1         : bit  absolute STATUS.6;
  STATUS_RP0         : bit  absolute STATUS.5;
  STATUS_nTO         : bit  absolute STATUS.4;
  STATUS_nPD         : bit  absolute STATUS.3;
  STATUS_Z           : bit  absolute STATUS.2;
  STATUS_DC          : bit  absolute STATUS.1;
  STATUS_C           : bit  absolute STATUS.0;
  FSR                : byte absolute $0004;
  PORTA              : byte absolute $0005;
  PORTA_RA7          : bit  absolute PORTA.7;
  PORTA_RA6          : bit  absolute PORTA.6;
  PORTA_RA5          : bit  absolute PORTA.5;
  PORTA_RA4          : bit  absolute PORTA.4;
  PORTA_RA3          : bit  absolute PORTA.3;
  PORTA_RA2          : bit  absolute PORTA.2;
  PORTA_RA1          : bit  absolute PORTA.1;
  PORTA_RA0          : bit  absolute PORTA.0;
  PORTB              : byte absolute $0006;
  PORTB_RB7          : bit  absolute PORTB.7;
  PORTB_RB6          : bit  absolute PORTB.6;
  PORTB_RB5          : bit  absolute PORTB.5;
  PORTB_RB4          : bit  absolute PORTB.4;
  PORTB_RB3          : bit  absolute PORTB.3;
  PORTB_RB2          : bit  absolute PORTB.2;
  PORTB_RB1          : bit  absolute PORTB.1;
  PORTB_RB0          : bit  absolute PORTB.0;
  PORTC              : byte absolute $0007;
  PORTC_RC7          : bit  absolute PORTC.7;
  PORTC_RC6          : bit  absolute PORTC.6;
  PORTC_RC5          : bit  absolute PORTC.5;
  PORTC_RC4          : bit  absolute PORTC.4;
  PORTC_RC3          : bit  absolute PORTC.3;
  PORTC_RC2          : bit  absolute PORTC.2;
  PORTC_RC1          : bit  absolute PORTC.1;
  PORTC_RC0          : bit  absolute PORTC.0;
  PORTD              : byte absolute $0008;
  PORTD_RD7          : bit  absolute PORTD.7;
  PORTD_RD6          : bit  absolute PORTD.6;
  PORTD_RD5          : bit  absolute PORTD.5;
  PORTD_RD4          : bit  absolute PORTD.4;
  PORTD_RD3          : bit  absolute PORTD.3;
  PORTD_RD2          : bit  absolute PORTD.2;
  PORTD_RD1          : bit  absolute PORTD.1;
  PORTD_RD0          : bit  absolute PORTD.0;
  PORTE              : byte absolute $0009;
  PORTE_RE3          : bit  absolute PORTE.3;
  PORTE_RE2          : bit  absolute PORTE.2;
  PORTE_RE1          : bit  absolute PORTE.1;
  PORTE_RE0          : bit  absolute PORTE.0;
  PCLATH             : byte absolute $000A;
  PCLATH_PCLATH4     : bit  absolute PCLATH.4;
  PCLATH_PCLATH3     : bit  absolute PCLATH.3;
  PCLATH_PCLATH2     : bit  absolute PCLATH.2;
  PCLATH_PCLATH1     : bit  absolute PCLATH.1;
  PCLATH_PCLATH0     : bit  absolute PCLATH.0;
  INTCON             : byte absolute $000B;
  INTCON_GIE         : bit  absolute INTCON.7;
  INTCON_PEIE        : bit  absolute INTCON.6;
  INTCON_TMR0IE      : bit  absolute INTCON.5;
  INTCON_INTE        : bit  absolute INTCON.4;
  INTCON_RBIE        : bit  absolute INTCON.3;
  INTCON_TMR0IF      : bit  absolute INTCON.2;
  INTCON_INTF        : bit  absolute INTCON.1;
  INTCON_RBIF        : bit  absolute INTCON.0;
  PIR1               : byte absolute $000C;
  PIR1_TMR1GIF       : bit  absolute PIR1.7;
  PIR1_ADIF          : bit  absolute PIR1.6;
  PIR1_RCIF          : bit  absolute PIR1.5;
  PIR1_TXIF          : bit  absolute PIR1.4;
  PIR1_SSPIF         : bit  absolute PIR1.3;
  PIR1_CCP1IF        : bit  absolute PIR1.2;
  PIR1_TMR2IF        : bit  absolute PIR1.1;
  PIR1_TMR1IF        : bit  absolute PIR1.0;
  PIR2               : byte absolute $000D;
  PIR2_TMR3GIF       : bit  absolute PIR2.7;
  PIR2_TMR3IF        : bit  absolute PIR2.6;
  PIR2_TMRBIF        : bit  absolute PIR2.5;
  PIR2_TMRAIF        : bit  absolute PIR2.4;
  PIR2_CCP2IF        : bit  absolute PIR2.0;
  TMR1L              : byte absolute $000E;
  TMR1H              : byte absolute $000F;
  T1CON              : byte absolute $0010;
  T1CON_TMR1CS1      : bit  absolute T1CON.7;
  T1CON_TMR1CS0      : bit  absolute T1CON.6;
  T1CON_T1CKPS1      : bit  absolute T1CON.5;
  T1CON_T1CKPS0      : bit  absolute T1CON.4;
  T1CON_T1OSCEN      : bit  absolute T1CON.3;
  T1CON_nT1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1ON       : bit  absolute T1CON.0;
  TMR2               : byte absolute $0011;
  T2CON              : byte absolute $0012;
  T2CON_TOUTPS3      : bit  absolute T2CON.6;
  T2CON_TOUTPS2      : bit  absolute T2CON.5;
  T2CON_TOUTPS1      : bit  absolute T2CON.4;
  T2CON_TOUTPS0      : bit  absolute T2CON.3;
  T2CON_TMR2ON       : bit  absolute T2CON.2;
  T2CON_T2CKPS1      : bit  absolute T2CON.1;
  T2CON_T2CKPS0      : bit  absolute T2CON.0;
  SSPBUF             : byte absolute $0013;
  SSPCON             : byte absolute $0014;
  SSPCON_WCOL        : bit  absolute SSPCON.7;
  SSPCON_SSPOV       : bit  absolute SSPCON.6;
  SSPCON_SSPEN       : bit  absolute SSPCON.5;
  SSPCON_CKP         : bit  absolute SSPCON.4;
  SSPCON_SSPM3       : bit  absolute SSPCON.3;
  SSPCON_SSPM2       : bit  absolute SSPCON.2;
  SSPCON_SSPM1       : bit  absolute SSPCON.1;
  SSPCON_SSPM0       : bit  absolute SSPCON.0;
  CCPR1L             : byte absolute $0015;
  CCPR1H             : byte absolute $0016;
  CCP1CON            : byte absolute $0017;
  CCP1CON_DC1B1      : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0      : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3     : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2     : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1     : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0     : bit  absolute CCP1CON.0;
  RCSTA              : byte absolute $0018;
  RCSTA_SPEN         : bit  absolute RCSTA.7;
  RCSTA_RX9          : bit  absolute RCSTA.6;
  RCSTA_SREN         : bit  absolute RCSTA.5;
  RCSTA_CREN         : bit  absolute RCSTA.4;
  RCSTA_ADDEN        : bit  absolute RCSTA.3;
  RCSTA_FERR         : bit  absolute RCSTA.2;
  RCSTA_OERR         : bit  absolute RCSTA.1;
  RCSTA_RX9D         : bit  absolute RCSTA.0;
  TXREG              : byte absolute $0019;
  RCREG              : byte absolute $001A;
  CCPR2L             : byte absolute $001B;
  CCPR2H             : byte absolute $001C;
  CCP2CON            : byte absolute $001D;
  CCP2CON_DC2B1      : bit  absolute CCP2CON.5;
  CCP2CON_DC2B0      : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3     : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2     : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1     : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0     : bit  absolute CCP2CON.0;
  ADRES              : byte absolute $001E;
  ADCON0             : byte absolute $001F;
  ADCON0_CHS3        : bit  absolute ADCON0.5;
  ADCON0_CHS2        : bit  absolute ADCON0.4;
  ADCON0_CHS1        : bit  absolute ADCON0.3;
  ADCON0_CHS0        : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE    : bit  absolute ADCON0.1;
  ADCON0_ADON        : bit  absolute ADCON0.0;
  OPTION_REG         : byte absolute $0081;
  OPTION_REG_nRBPU   : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG  : bit  absolute OPTION_REG.6;
  OPTION_REG_TMR0CS  : bit  absolute OPTION_REG.5;
  OPTION_REG_TMR0SE  : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA     : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2     : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1     : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0     : bit  absolute OPTION_REG.0;
  TRISA              : byte absolute $0085;
  TRISA_TRISA7       : bit  absolute TRISA.7;
  TRISA_TRISA6       : bit  absolute TRISA.6;
  TRISA_TRISA5       : bit  absolute TRISA.5;
  TRISA_TRISA4       : bit  absolute TRISA.4;
  TRISA_TRISA3       : bit  absolute TRISA.3;
  TRISA_TRISA2       : bit  absolute TRISA.2;
  TRISA_TRISA1       : bit  absolute TRISA.1;
  TRISA_TRISA0       : bit  absolute TRISA.0;
  TRISB              : byte absolute $0086;
  TRISB_TRISB7       : bit  absolute TRISB.7;
  TRISB_TRISB6       : bit  absolute TRISB.6;
  TRISB_TRISB5       : bit  absolute TRISB.5;
  TRISB_TRISB4       : bit  absolute TRISB.4;
  TRISB_TRISB3       : bit  absolute TRISB.3;
  TRISB_TRISB2       : bit  absolute TRISB.2;
  TRISB_TRISB1       : bit  absolute TRISB.1;
  TRISB_TRISB0       : bit  absolute TRISB.0;
  TRISC              : byte absolute $0087;
  TRISC_TRISC7       : bit  absolute TRISC.7;
  TRISC_TRISC6       : bit  absolute TRISC.6;
  TRISC_TRISC5       : bit  absolute TRISC.5;
  TRISC_TRISC4       : bit  absolute TRISC.4;
  TRISC_TRISC3       : bit  absolute TRISC.3;
  TRISC_TRISC2       : bit  absolute TRISC.2;
  TRISC_TRISC1       : bit  absolute TRISC.1;
  TRISC_TRISC0       : bit  absolute TRISC.0;
  TRISD              : byte absolute $0088;
  TRISD_TRISD7       : bit  absolute TRISD.7;
  TRISD_TRISD6       : bit  absolute TRISD.6;
  TRISD_TRISD5       : bit  absolute TRISD.5;
  TRISD_TRISD4       : bit  absolute TRISD.4;
  TRISD_TRISD3       : bit  absolute TRISD.3;
  TRISD_TRISD2       : bit  absolute TRISD.2;
  TRISD_TRISD1       : bit  absolute TRISD.1;
  TRISD_TRISD0       : bit  absolute TRISD.0;
  TRISE              : byte absolute $0089;
  TRISE_TRISE3       : bit  absolute TRISE.3;
  TRISE_TRISE2       : bit  absolute TRISE.2;
  TRISE_TRISE1       : bit  absolute TRISE.1;
  TRISE_TRISE0       : bit  absolute TRISE.0;
  PIE1               : byte absolute $008C;
  PIE1_TMR1GIE       : bit  absolute PIE1.7;
  PIE1_ADIE          : bit  absolute PIE1.6;
  PIE1_RCIE          : bit  absolute PIE1.5;
  PIE1_TXIE          : bit  absolute PIE1.4;
  PIE1_SSPIE         : bit  absolute PIE1.3;
  PIE1_CCP1IE        : bit  absolute PIE1.2;
  PIE1_TMR2IE        : bit  absolute PIE1.1;
  PIE1_TMR1IE        : bit  absolute PIE1.0;
  PIE2               : byte absolute $008D;
  PIE2_TMR3GIE       : bit  absolute PIE2.7;
  PIE2_TMR3IE        : bit  absolute PIE2.6;
  PIE2_TMRBIE        : bit  absolute PIE2.5;
  PIE2_TMRAIE        : bit  absolute PIE2.4;
  PIE2_CCP2IE        : bit  absolute PIE2.0;
  PCON               : byte absolute $008E;
  PCON_nPOR          : bit  absolute PCON.1;
  PCON_nBOR          : bit  absolute PCON.0;
  T1GCON             : byte absolute $008F;
  T1GCON_TMR1GE      : bit  absolute T1GCON.7;
  T1GCON_T1GPOL      : bit  absolute T1GCON.6;
  T1GCON_T1GTM       : bit  absolute T1GCON.5;
  T1GCON_T1GSPM      : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE : bit  absolute T1GCON.3;
  T1GCON_T1GVAL      : bit  absolute T1GCON.2;
  T1GCON_T1GSS1      : bit  absolute T1GCON.1;
  T1GCON_T1GSS0      : bit  absolute T1GCON.0;
  OSCCON             : byte absolute $0090;
  OSCCON_IRCF1       : bit  absolute OSCCON.5;
  OSCCON_IRCF0       : bit  absolute OSCCON.4;
  OSCCON_ICSL        : bit  absolute OSCCON.3;
  OSCCON_ICSS        : bit  absolute OSCCON.2;
  OSCTUNE            : byte absolute $0091;
  OSCTUNE_TUN5       : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4       : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3       : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2       : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1       : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0       : bit  absolute OSCTUNE.0;
  PR2                : byte absolute $0092;
  SSPADD             : byte absolute $0093;
  SSPSTAT            : byte absolute $0094;
  SSPSTAT_SMP        : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE        : bit  absolute SSPSTAT.6;
  SSPSTAT_D_nA       : bit  absolute SSPSTAT.5;
  SSPSTAT_P          : bit  absolute SSPSTAT.4;
  SSPSTAT_S          : bit  absolute SSPSTAT.3;
  SSPSTAT_R_nW       : bit  absolute SSPSTAT.2;
  SSPSTAT_UA         : bit  absolute SSPSTAT.1;
  SSPSTAT_BF         : bit  absolute SSPSTAT.0;
  WPUB               : byte absolute $0095;
  IOCB               : byte absolute $0096;
  T3CON              : byte absolute $0097;
  T3CON_TMR3CS1      : bit  absolute T3CON.7;
  T3CON_TMR3CS0      : bit  absolute T3CON.6;
  T3CON_T3CKPS1      : bit  absolute T3CON.5;
  T3CON_T3CKPS0      : bit  absolute T3CON.4;
  T3CON_T3SYNC       : bit  absolute T3CON.2;
  T3CON_TMR3ON       : bit  absolute T3CON.0;
  TXSTA              : byte absolute $0098;
  TXSTA_CSRC         : bit  absolute TXSTA.7;
  TXSTA_TX9          : bit  absolute TXSTA.6;
  TXSTA_TXEN         : bit  absolute TXSTA.5;
  TXSTA_SYNC         : bit  absolute TXSTA.4;
  TXSTA_BRGH         : bit  absolute TXSTA.2;
  TXSTA_TRMT         : bit  absolute TXSTA.1;
  TXSTA_TX9D         : bit  absolute TXSTA.0;
  SPBRG              : byte absolute $0099;
  TMR3L              : byte absolute $009A;
  TMR3H              : byte absolute $009B;
  APFCON             : byte absolute $009C;
  APFCON_SSSEL       : bit  absolute APFCON.1;
  APFCON_CCP2SEL     : bit  absolute APFCON.0;
  FVRCON             : byte absolute $009D;
  FVRCON_FVRRDY      : bit  absolute FVRCON.7;
  FVRCON_FVREN       : bit  absolute FVRCON.6;
  FVRCON_TSEN        : bit  absolute FVRCON.5;
  FVRCON_TSRNG       : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1     : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0     : bit  absolute FVRCON.2;
  FVRCON_ADFVR1      : bit  absolute FVRCON.1;
  FVRCON_ADFVR0      : bit  absolute FVRCON.0;
  T3GCON             : byte absolute $009E;
  T3GCON_TMR3GE      : bit  absolute T3GCON.7;
  T3GCON_T3GPOL      : bit  absolute T3GCON.6;
  T3GCON_T3GTM       : bit  absolute T3GCON.5;
  T3GCON_T3GSPM      : bit  absolute T3GCON.4;
  T3GCON_T3GGO_nDONE : bit  absolute T3GCON.3;
  T3GCON_T3GVAL      : bit  absolute T3GCON.2;
  T3GCON_T3GSS1      : bit  absolute T3GCON.1;
  T3GCON_T3GSS0      : bit  absolute T3GCON.0;
  ADCON1             : byte absolute $009F;
  ADCON1_ADCS2       : bit  absolute ADCON1.6;
  ADCON1_ADCS1       : bit  absolute ADCON1.5;
  ADCON1_ADCS0       : bit  absolute ADCON1.4;
  ADCON1_ADREF1      : bit  absolute ADCON1.1;
  ADCON1_ADREF0      : bit  absolute ADCON1.0;
  TACON              : byte absolute $0105;
  TACON_TMRAON       : bit  absolute TACON.7;
  TACON_TACS         : bit  absolute TACON.5;
  TACON_TASE         : bit  absolute TACON.4;
  TACON_TAPSA        : bit  absolute TACON.3;
  TACON_TAPS2        : bit  absolute TACON.2;
  TACON_TAPS1        : bit  absolute TACON.1;
  TACON_TAPS0        : bit  absolute TACON.0;
  CPSBCON0           : byte absolute $0106;
  CPSBCON0_CPSBON    : bit  absolute CPSBCON0.7;
  CPSBCON0_CPSBRM    : bit  absolute CPSBCON0.6;
  CPSBCON0_CPSBRNG1  : bit  absolute CPSBCON0.3;
  CPSBCON0_CPSBRNG0  : bit  absolute CPSBCON0.2;
  CPSBCON0_CPSBOUT   : bit  absolute CPSBCON0.1;
  CPSBCON0_TBXCS     : bit  absolute CPSBCON0.0;
  CPSBCON1           : byte absolute $0107;
  CPSBCON1_CPSBCH3   : bit  absolute CPSBCON1.3;
  CPSBCON1_CPSBCH2   : bit  absolute CPSBCON1.2;
  CPSBCON1_CPSBCH1   : bit  absolute CPSBCON1.1;
  CPSBCON1_CPSBCH0   : bit  absolute CPSBCON1.0;
  CPSACON0           : byte absolute $0108;
  CPSACON0_CPSAON    : bit  absolute CPSACON0.7;
  CPSACON0_CPSARM    : bit  absolute CPSACON0.6;
  CPSACON0_CPSARNG1  : bit  absolute CPSACON0.3;
  CPSACON0_CPSARNG0  : bit  absolute CPSACON0.2;
  CPSACON0_CPSAOUT   : bit  absolute CPSACON0.1;
  CPSACON0_TAXCS     : bit  absolute CPSACON0.0;
  CPSACON1           : byte absolute $0109;
  CPSACON1_CPSACH3   : bit  absolute CPSACON1.3;
  CPSACON1_CPSACH2   : bit  absolute CPSACON1.2;
  CPSACON1_CPSACH1   : bit  absolute CPSACON1.1;
  CPSACON1_CPSACH0   : bit  absolute CPSACON1.0;
  PMDATL             : byte absolute $010C;
  PMADRL             : byte absolute $010D;
  PMDATH             : byte absolute $010E;
  PMDATH_PMDATH5     : bit  absolute PMDATH.5;
  PMDATH_PMDATH4     : bit  absolute PMDATH.4;
  PMDATH_PMDATH3     : bit  absolute PMDATH.3;
  PMDATH_PMDATH2     : bit  absolute PMDATH.2;
  PMDATH_PMDATH1     : bit  absolute PMDATH.1;
  PMDATH_PMDATH0     : bit  absolute PMDATH.0;
  PMADRH             : byte absolute $010F;
  PMADRH_PMADRH4     : bit  absolute PMADRH.4;
  PMADRH_PMADRH3     : bit  absolute PMADRH.3;
  PMADRH_PMADRH2     : bit  absolute PMADRH.2;
  PMADRH_PMADRH1     : bit  absolute PMADRH.1;
  PMADRH_PMADRH0     : bit  absolute PMADRH.0;
  TMRA               : byte absolute $0110;
  TBCON              : byte absolute $0111;
  TBCON_TMRBON       : bit  absolute TBCON.7;
  TBCON_TBCS         : bit  absolute TBCON.5;
  TBCON_TBSE         : bit  absolute TBCON.4;
  TBCON_TBPSA        : bit  absolute TBCON.3;
  TBCON_TBPS2        : bit  absolute TBCON.2;
  TBCON_TBPS1        : bit  absolute TBCON.1;
  TBCON_TBPS0        : bit  absolute TBCON.0;
  TMRB               : byte absolute $0112;
  DACCON0            : byte absolute $0113;
  DACCON0_DACEN      : bit  absolute DACCON0.7;
  DACCON0_DACLPS     : bit  absolute DACCON0.6;
  DACCON0_DACOE      : bit  absolute DACCON0.5;
  DACCON0_DACPSS1    : bit  absolute DACCON0.3;
  DACCON0_DACPSS0    : bit  absolute DACCON0.2;
  DACCON1            : byte absolute $0114;
  DACCON1_DACR4      : bit  absolute DACCON1.4;
  DACCON1_DACR3      : bit  absolute DACCON1.3;
  DACCON1_DACR2      : bit  absolute DACCON1.2;
  DACCON1_DACR1      : bit  absolute DACCON1.1;
  DACCON1_DACR0      : bit  absolute DACCON1.0;
  ANSELA             : byte absolute $0185;
  ANSELA_ANSA7       : bit  absolute ANSELA.7;
  ANSELA_ANSA6       : bit  absolute ANSELA.6;
  ANSELA_ANSA5       : bit  absolute ANSELA.5;
  ANSELA_ANSA4       : bit  absolute ANSELA.4;
  ANSELA_ANSA3       : bit  absolute ANSELA.3;
  ANSELA_ANSA2       : bit  absolute ANSELA.2;
  ANSELA_ANSA1       : bit  absolute ANSELA.1;
  ANSELA_ANSA0       : bit  absolute ANSELA.0;
  ANSELB             : byte absolute $0186;
  ANSELB_ANSB7       : bit  absolute ANSELB.7;
  ANSELB_ANSB6       : bit  absolute ANSELB.6;
  ANSELB_ANSB5       : bit  absolute ANSELB.5;
  ANSELB_ANSB4       : bit  absolute ANSELB.4;
  ANSELB_ANSB3       : bit  absolute ANSELB.3;
  ANSELB_ANSB2       : bit  absolute ANSELB.2;
  ANSELB_ANSB1       : bit  absolute ANSELB.1;
  ANSELB_ANSB0       : bit  absolute ANSELB.0;
  ANSELC             : byte absolute $0187;
  ANSELC_ANSC7       : bit  absolute ANSELC.7;
  ANSELC_ANSC6       : bit  absolute ANSELC.6;
  ANSELC_ANSC5       : bit  absolute ANSELC.5;
  ANSELC_ANSC2       : bit  absolute ANSELC.2;
  ANSELC_ANSC1       : bit  absolute ANSELC.1;
  ANSELC_ANSC0       : bit  absolute ANSELC.0;
  ANSELD             : byte absolute $0188;
  ANSELD_ANSD7       : bit  absolute ANSELD.7;
  ANSELD_ANSD6       : bit  absolute ANSELD.6;
  ANSELD_ANSD5       : bit  absolute ANSELD.5;
  ANSELD_ANSD4       : bit  absolute ANSELD.4;
  ANSELD_ANSD3       : bit  absolute ANSELD.3;
  ANSELD_ANSD2       : bit  absolute ANSELD.2;
  ANSELD_ANSD1       : bit  absolute ANSELD.1;
  ANSELD_ANSD0       : bit  absolute ANSELD.0;
  ANSELE             : byte absolute $0189;
  ANSELE_ANSE2       : bit  absolute ANSELE.2;
  ANSELE_ANSE1       : bit  absolute ANSELE.1;
  ANSELE_ANSE0       : bit  absolute ANSELE.0;
  PMCON1             : byte absolute $018C;
  PMCON1_RD          : bit  absolute PMCON1.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-3 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
                                            // Bank 2 : TMR0
                                            // Bank 3 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-3 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-009:SFR:ALL'}        // Bank 0 : PORTA, PORTB, PORTC, PORTD, PORTE
                                            // Bank 1 : TRISA, TRISB, TRISC, TRISD, TRISE
                                            // Bank 2 : TACON, CPSBCON0, CPSBCON1, CPSACON0, CPSACON1
                                            // Bank 3 : ANSELA, ANSELB, ANSELC, ANSELD, ANSELE
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00C:SFR:ALL'}        // Bank 0 : PIR1
                                            // Bank 1 : PIE1
                                            // Bank 2 : PMDATL
                                            // Bank 3 : PMCON1
  {$SET_STATE_RAM '00D-01F:SFR'}            // Bank 0 : PIR2, TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG, CCPR2L, CCPR2H, CCP2CON, ADRES, ADCON0
  {$SET_STATE_RAM '020-06F:GPR:ALL'}       
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08D-09F:SFR'}            // Bank 1 : PIE2, PCON, T1GCON, OSCCON, OSCTUNE, PR2, SSPADD, SSPSTAT, WPUB, IOCB, T3CON, TXSTA, SPBRG, TMR3L, TMR3H, APFCON, FVRCON, T3GCON, ADCON1
  {$SET_STATE_RAM '10D-114:SFR'}            // Bank 2 : PMADRL, PMDATH, PMADRH, TMRA, TBCON, TMRB, DACCON0, DACCON1
  {$SET_STATE_RAM '115-11F:GPR'}           
  {$SET_STATE_RAM '190-19F:GPR'}           


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '100-104:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '009:0F'} // PORTE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:F1'} // PIR2 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01D:3F'} // CCP2CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:3F'} // ADCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '089:0F'} // TRISE bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:F1'} // PIE2 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:3C'} // OSCCON bits 7,6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:F5'} // T3CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:F7'} // TXSTA bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09C:03'} // APFCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:73'} // ADCON1 bits 7,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '105:BF'} // TACON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '106:CF'} // CPSBCON0 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '107:0F'} // CPSBCON1 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '108:CF'} // CPSACON0 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '109:0F'} // CPSACON1 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:1F'} // PMADRH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:BF'} // TBCON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:EC'} // DACCON0 bits 4,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '187:E7'} // ANSELC bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '189:07'} // ANSELE bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:81'} // PMCON1 bits 6,5,4,3,2,1 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '18C:80'} // PMCON1 bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : RE3/MCLR/Vpp
  // Pin  2 : RA0/AN0/SS/Vcap
  // Pin  3 : RA1/AN1/CPSA0
  // Pin  4 : RA2/AN2/CPSA1/DACOUT
  // Pin  5 : RA3/AN3/Vref/CPSA2
  // Pin  6 : RA4/CPSA3/T0CKI/TACKI
  // Pin  7 : RA5/AN4/CPSA4/SS/Vcap
  // Pin  8 : RE0/AN5/CPSA5
  // Pin  9 : RE1/AN6/CPSA6
  // Pin 10 : RE2/AN7/CPSA7
  // Pin 11 : Vdd
  // Pin 12 : Vss
  // Pin 13 : RA7/CPSB0/OSC1/CLKIN
  // Pin 14 : RA6/CPSB1/OSC2/CLKOUT/Vcap
  // Pin 15 : RC0/CPSB2/T1OSO/T1CKI
  // Pin 16 : RC1/CPSB3/T1OSI/CCP2
  // Pin 17 : RC2/CPSB4/CCP1/TBCKI
  // Pin 18 : RC3/SCK/SCL
  // Pin 19 : RD0/CPSB5/T3G
  // Pin 20 : RD1/CPSB6
  // Pin 21 : RD2/CPSB7
  // Pin 22 : RD3/CPSA8
  // Pin 23 : RC4/SDI/SDA
  // Pin 24 : RC5/CPSA9/SDO
  // Pin 25 : RC6/CPSA10/TX/CK
  // Pin 26 : RC7/CPSA11/RX/DT
  // Pin 27 : RD4/CPSA12
  // Pin 28 : RD5/CPSA13
  // Pin 29 : RD6/CPSA14
  // Pin 30 : RD7/CPSA15
  // Pin 31 : Vss
  // Pin 32 : Vdd
  // Pin 33 : RB0/AN12/CPSB8/INT
  // Pin 34 : RB1/AN10/CPSB9
  // Pin 35 : RB2/AN8/CPSB10
  // Pin 36 : RB3/AN9/CPSB11/CCP2
  // Pin 37 : RB4/AN11/CPSB12
  // Pin 38 : RB5/AN13/CPSB13/T1G/T3CKI
  // Pin 39 : RB6/CPSB14/ICSPCLK/ICDCLK
  // Pin 40 : RB7/CPSB15/ICSPDAT/ICDDAT


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-2,1-3,2-4,3-5,4-6,5-7,6-14,7-13'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-33,1-34,2-35,3-36,4-37,5-38,6-39,7-40'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-15,1-16,2-17,3-18,4-23,5-24,6-25,7-26'} // PORTC
  {$MAP_RAM_TO_PIN '008:0-19,1-20,2-21,3-22,4-27,5-28,6-29,7-30'} // PORTD
  {$MAP_RAM_TO_PIN '009:0-8,1-9,2-10,3-1'} // PORTE


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRC    = $3FFF}  // RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN
  {$define _FOSC_EXTRCIO  = $3FFE}  // RCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN
  {$define _FOSC_INTOSC   = $3FFD}  // INTOSC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, I/O function  on RA7/OSC1/CLKIN
  {$define _FOSC_INTOSCIO = $3FFC}  // INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN pins
  {$define _FOSC_EC_OSC   = $3FFB}  // EC oscillator: I/O function on RA6/OSC2/CLKOUT pin, CLKIN on RA7/OSC1/CLKIN
  {$define _FOSC_HS_OSC   = $3FFA}  // HS oscillator: High Speed crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN
  {$define _FOSC_XT_OSC   = $3FF9}  // XT oscillator: Crystal/resonator on RA6/OSC2/CLKIN and RA7/OSC1/CLKIN
  {$define _FOSC_LP_OSC   = $3FF8}  // LP oscillator: Low-power crystal on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON       = $3FFF}  // WDT enabled
  {$define _WDTE_OFF      = $3FF7}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF     = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON      = $3FEF}  // PWRT enabled

  // MCLRE : RE3/MCLR Pin Function Select bit
  {$define _MCLRE_ON      = $3FFF}  // RE3/MCLR pin function is MCLR
  {$define _MCLRE_OFF     = $3FDF}  // RE3/MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF        = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON         = $3FBF}  // Program memory code protection is enabled

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON      = $3FFF}  // BOR enabled
  {$define _BOREN_NSLEEP  = $3EFF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_OFF     = $3DFF}  // BOR disabled (Preconditioned State)
  {$define _BOREN_OFF     = $3CFF}  // BOR disabled (Preconditioned State)

  // BORV : Brown-out Reset Voltage Selection bit
  {$define _BORV_19       = $3FFF}  // Brown-out Reset Voltage (VBOR) set to 1.9 V nominal
  {$define _BORV_25       = $3BFF}  // Brown-out Reset Voltage (VBOR) set to 2.5 V nominal

  // PLLEN : INTOSC PLLEN Enable Bit
  {$define _PLLEN_ON      = $3FFF}  // INTOSC Frequency is 16 MHz (32x)
  {$define _PLLEN_OFF     = $2FFF}  // INTOSC Frequency is 500 kHz

  // VCAPEN : Voltage Regulator Capacitor Enable bits
  {$define _VCAPEN_OFF    = $3FFF}  // All VCAP pin functions are disabled
  {$define _VCAPEN_RA6    = $3FEF}  // VCAP functionality is enabled on RA6
  {$define _VCAPEN_RA5    = $3FDF}  // VCAP functionality is enabled on RA5
  {$define _VCAPEN_RA0    = $3FCF}  // VCAP functionality is enabled on RA0

implementation
end.
