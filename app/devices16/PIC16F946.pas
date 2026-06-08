unit PIC16F946;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F946'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 64}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 8192}

interface
var
  INDF              : byte absolute $0000;
  TMR0              : byte absolute $0001;
  PCL               : byte absolute $0002;
  STATUS            : byte absolute $0003;
  STATUS_IRP        : bit  absolute STATUS.7;
  STATUS_RP1        : bit  absolute STATUS.6;
  STATUS_RP0        : bit  absolute STATUS.5;
  STATUS_nTO        : bit  absolute STATUS.4;
  STATUS_nPD        : bit  absolute STATUS.3;
  STATUS_Z          : bit  absolute STATUS.2;
  STATUS_DC         : bit  absolute STATUS.1;
  STATUS_C          : bit  absolute STATUS.0;
  FSR               : byte absolute $0004;
  PORTA             : byte absolute $0005;
  PORTA_RA7         : bit  absolute PORTA.7;
  PORTA_RA6         : bit  absolute PORTA.6;
  PORTA_RA5         : bit  absolute PORTA.5;
  PORTA_RA4         : bit  absolute PORTA.4;
  PORTA_RA3         : bit  absolute PORTA.3;
  PORTA_RA2         : bit  absolute PORTA.2;
  PORTA_RA1         : bit  absolute PORTA.1;
  PORTA_RA0         : bit  absolute PORTA.0;
  PORTB             : byte absolute $0006;
  PORTB_RB7         : bit  absolute PORTB.7;
  PORTB_RB6         : bit  absolute PORTB.6;
  PORTB_RB5         : bit  absolute PORTB.5;
  PORTB_RB4         : bit  absolute PORTB.4;
  PORTB_RB3         : bit  absolute PORTB.3;
  PORTB_RB2         : bit  absolute PORTB.2;
  PORTB_RB1         : bit  absolute PORTB.1;
  PORTB_RB0         : bit  absolute PORTB.0;
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_RC0         : bit  absolute PORTC.0;
  PORTD             : byte absolute $0008;
  PORTD_RD7         : bit  absolute PORTD.7;
  PORTD_RD6         : bit  absolute PORTD.6;
  PORTD_RD5         : bit  absolute PORTD.5;
  PORTD_RD4         : bit  absolute PORTD.4;
  PORTD_RD3         : bit  absolute PORTD.3;
  PORTD_RD2         : bit  absolute PORTD.2;
  PORTD_RD1         : bit  absolute PORTD.1;
  PORTD_RD0         : bit  absolute PORTD.0;
  PORTE             : byte absolute $0009;
  PORTE_RE7         : bit  absolute PORTE.7;
  PORTE_RE6         : bit  absolute PORTE.6;
  PORTE_RE5         : bit  absolute PORTE.5;
  PORTE_RE4         : bit  absolute PORTE.4;
  PORTE_RE3         : bit  absolute PORTE.3;
  PORTE_RE2         : bit  absolute PORTE.2;
  PORTE_RE1         : bit  absolute PORTE.1;
  PORTE_RE0         : bit  absolute PORTE.0;
  PCLATH            : byte absolute $000A;
  PCLATH_PCLATH4    : bit  absolute PCLATH.4;
  PCLATH_PCLATH3    : bit  absolute PCLATH.3;
  PCLATH_PCLATH2    : bit  absolute PCLATH.2;
  PCLATH_PCLATH1    : bit  absolute PCLATH.1;
  PCLATH_PCLATH0    : bit  absolute PCLATH.0;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000D;
  PIR2_OSFIF        : bit  absolute PIR2.7;
  PIR2_C2IF         : bit  absolute PIR2.6;
  PIR2_C1IF         : bit  absolute PIR2.5;
  PIR2_LCDIF        : bit  absolute PIR2.4;
  PIR2_LVDIF        : bit  absolute PIR2.2;
  PIR2_CCP2IF       : bit  absolute PIR2.0;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_nT1SYNC     : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS1     : bit  absolute T2CON.1;
  T2CON_T2CKPS0     : bit  absolute T2CON.0;
  SSPBUF            : byte absolute $0013;
  SSPCON            : byte absolute $0014;
  SSPCON_WCOL       : bit  absolute SSPCON.7;
  SSPCON_SSPOV      : bit  absolute SSPCON.6;
  SSPCON_SSPEN      : bit  absolute SSPCON.5;
  SSPCON_CKP        : bit  absolute SSPCON.4;
  SSPCON_SSPM3      : bit  absolute SSPCON.3;
  SSPCON_SSPM2      : bit  absolute SSPCON.2;
  SSPCON_SSPM1      : bit  absolute SSPCON.1;
  SSPCON_SSPM0      : bit  absolute SSPCON.0;
  CCPR1L            : byte absolute $0015;
  CCPR1H            : byte absolute $0016;
  CCP1CON           : byte absolute $0017;
  CCP1CON_CCP1X     : bit  absolute CCP1CON.5;
  CCP1CON_CCP1Y     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  RCSTA             : byte absolute $0018;
  RCSTA_SPEN        : bit  absolute RCSTA.7;
  RCSTA_RX9         : bit  absolute RCSTA.6;
  RCSTA_SREN        : bit  absolute RCSTA.5;
  RCSTA_CREN        : bit  absolute RCSTA.4;
  RCSTA_ADDEN       : bit  absolute RCSTA.3;
  RCSTA_FERR        : bit  absolute RCSTA.2;
  RCSTA_OERR        : bit  absolute RCSTA.1;
  RCSTA_RX9D        : bit  absolute RCSTA.0;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001A;
  CCPR2L            : byte absolute $001B;
  CCPR2H            : byte absolute $001C;
  CCP2CON           : byte absolute $001D;
  CCP2CON_CCP2X     : bit  absolute CCP2CON.5;
  CCP2CON_CCP2Y     : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3    : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2    : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1    : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0    : bit  absolute CCP2CON.0;
  ADRESH            : byte absolute $001E;
  ADCON0            : byte absolute $001F;
  ADCON0_ADFM       : bit  absolute ADCON0.7;
  ADCON0_VCFG1      : bit  absolute ADCON0.6;
  ADCON0_VCFG0      : bit  absolute ADCON0.5;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_nRBPU  : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
  TRISA_TRISA7      : bit  absolute TRISA.7;
  TRISA_TRISA6      : bit  absolute TRISA.6;
  TRISA_TRISA5      : bit  absolute TRISA.5;
  TRISA_TRISA4      : bit  absolute TRISA.4;
  TRISA_TRISA3      : bit  absolute TRISA.3;
  TRISA_TRISA2      : bit  absolute TRISA.2;
  TRISA_TRISA1      : bit  absolute TRISA.1;
  TRISA_TRISA0      : bit  absolute TRISA.0;
  TRISB             : byte absolute $0086;
  TRISB_TRISB7      : bit  absolute TRISB.7;
  TRISB_TRISB6      : bit  absolute TRISB.6;
  TRISB_TRISB5      : bit  absolute TRISB.5;
  TRISB_TRISB4      : bit  absolute TRISB.4;
  TRISB_TRISB3      : bit  absolute TRISB.3;
  TRISB_TRISB2      : bit  absolute TRISB.2;
  TRISB_TRISB1      : bit  absolute TRISB.1;
  TRISB_TRISB0      : bit  absolute TRISB.0;
  TRISC             : byte absolute $0087;
  TRISC_TRISC7      : bit  absolute TRISC.7;
  TRISC_TRISC6      : bit  absolute TRISC.6;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  TRISD             : byte absolute $0088;
  TRISD_TRISD7      : bit  absolute TRISD.7;
  TRISD_TRISD6      : bit  absolute TRISD.6;
  TRISD_TRISD5      : bit  absolute TRISD.5;
  TRISD_TRISD4      : bit  absolute TRISD.4;
  TRISD_TRISD3      : bit  absolute TRISD.3;
  TRISD_TRISD2      : bit  absolute TRISD.2;
  TRISD_TRISD1      : bit  absolute TRISD.1;
  TRISD_TRISD0      : bit  absolute TRISD.0;
  TRISE             : byte absolute $0089;
  TRISE_TRISE7      : bit  absolute TRISE.7;
  TRISE_TRISE6      : bit  absolute TRISE.6;
  TRISE_TRISE5      : bit  absolute TRISE.5;
  TRISE_TRISE4      : bit  absolute TRISE.4;
  TRISE_TRISE3      : bit  absolute TRISE.3;
  TRISE_TRISE2      : bit  absolute TRISE.2;
  TRISE_TRISE1      : bit  absolute TRISE.1;
  TRISE_TRISE0      : bit  absolute TRISE.0;
  PIE1              : byte absolute $008C;
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008D;
  PIE2_OSFIE        : bit  absolute PIE2.7;
  PIE2_C2IE         : bit  absolute PIE2.6;
  PIE2_C1IE         : bit  absolute PIE2.5;
  PIE2_LCDIE        : bit  absolute PIE2.4;
  PIE2_LVDIE        : bit  absolute PIE2.2;
  PIE2_CCP2IE       : bit  absolute PIE2.0;
  PCON              : byte absolute $008E;
  PCON_SBOREN       : bit  absolute PCON.4;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  OSCCON            : byte absolute $008F;
  OSCCON_IRCF2      : bit  absolute OSCCON.6;
  OSCCON_IRCF1      : bit  absolute OSCCON.5;
  OSCCON_IRCF0      : bit  absolute OSCCON.4;
  OSCCON_OSTS       : bit  absolute OSCCON.3;
  OSCCON_HTS        : bit  absolute OSCCON.2;
  OSCCON_LTS        : bit  absolute OSCCON.1;
  OSCCON_SCS        : bit  absolute OSCCON.0;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  ANSEL             : byte absolute $0091;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPSTAT           : byte absolute $0094;
  SSPSTAT_SMP       : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE       : bit  absolute SSPSTAT.6;
  SSPSTAT_D_nA      : bit  absolute SSPSTAT.5;
  SSPSTAT_P         : bit  absolute SSPSTAT.4;
  SSPSTAT_S         : bit  absolute SSPSTAT.3;
  SSPSTAT_R_nW      : bit  absolute SSPSTAT.2;
  SSPSTAT_UA        : bit  absolute SSPSTAT.1;
  SSPSTAT_BF        : bit  absolute SSPSTAT.0;
  WPUB              : byte absolute $0095;
  IOCB              : byte absolute $0096;
  IOCB_IOCB3        : bit  absolute IOCB.7;
  IOCB_IOCB2        : bit  absolute IOCB.6;
  IOCB_IOCB1        : bit  absolute IOCB.5;
  IOCB_IOCB0        : bit  absolute IOCB.4;
  CMCON1            : byte absolute $0097;
  CMCON1_T1GSS      : bit  absolute CMCON1.1;
  CMCON1_C2SYNC     : bit  absolute CMCON1.0;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.2;
  TXSTA_TRMT        : bit  absolute TXSTA.1;
  TXSTA_TX9D        : bit  absolute TXSTA.0;
  SPBRG             : byte absolute $0099;
  CMCON0            : byte absolute $009C;
  CMCON0_C2OUT      : bit  absolute CMCON0.7;
  CMCON0_C1OUT      : bit  absolute CMCON0.6;
  CMCON0_C2INV      : bit  absolute CMCON0.5;
  CMCON0_C1INV      : bit  absolute CMCON0.4;
  CMCON0_CIS        : bit  absolute CMCON0.3;
  CMCON0_CM2        : bit  absolute CMCON0.2;
  CMCON0_CM1        : bit  absolute CMCON0.1;
  CMCON0_CM0        : bit  absolute CMCON0.0;
  VRCON             : byte absolute $009D;
  VRCON_VREN        : bit  absolute VRCON.7;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  ADRESL            : byte absolute $009E;
  ADCON1            : byte absolute $009F;
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
  ADCON1_ADCS1      : bit  absolute ADCON1.5;
  ADCON1_ADCS0      : bit  absolute ADCON1.4;
  WDTCON            : byte absolute $0105;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  LCDCON            : byte absolute $0107;
  LCDCON_LCDEN      : bit  absolute LCDCON.7;
  LCDCON_SLPEN      : bit  absolute LCDCON.6;
  LCDCON_WERR       : bit  absolute LCDCON.5;
  LCDCON_VLCDEN     : bit  absolute LCDCON.4;
  LCDCON_CS1        : bit  absolute LCDCON.3;
  LCDCON_CS0        : bit  absolute LCDCON.2;
  LCDCON_LMUX1      : bit  absolute LCDCON.1;
  LCDCON_LMUX0      : bit  absolute LCDCON.0;
  LCDPS             : byte absolute $0108;
  LCDPS_WFT         : bit  absolute LCDPS.7;
  LCDPS_BIASMD      : bit  absolute LCDPS.6;
  LCDPS_LCDA        : bit  absolute LCDPS.5;
  LCDPS_WA          : bit  absolute LCDPS.4;
  LCDPS_LP3         : bit  absolute LCDPS.3;
  LCDPS_LP2         : bit  absolute LCDPS.2;
  LCDPS_LP1         : bit  absolute LCDPS.1;
  LCDPS_LP0         : bit  absolute LCDPS.0;
  LVDCON            : byte absolute $0109;
  LVDCON_IRVST      : bit  absolute LVDCON.5;
  LVDCON_LVDEN      : bit  absolute LVDCON.4;
  LVDCON_LVDL2      : bit  absolute LVDCON.2;
  LVDCON_LVDL1      : bit  absolute LVDCON.1;
  LVDCON_LVDL0      : bit  absolute LVDCON.0;
  EEDATL            : byte absolute $010C;
  EEADRL            : byte absolute $010D;
  EEDATH            : byte absolute $010E;
  EEDATH_EEDATH5    : bit  absolute EEDATH.5;
  EEDATH_EEDATH4    : bit  absolute EEDATH.4;
  EEDATH_EEDATH3    : bit  absolute EEDATH.3;
  EEDATH_EEDATH2    : bit  absolute EEDATH.2;
  EEDATH_EEDATH1    : bit  absolute EEDATH.1;
  EEDATH_EEDATH0    : bit  absolute EEDATH.0;
  EEADRH            : byte absolute $010F;
  EEADRH_EEADRH4    : bit  absolute EEADRH.4;
  EEADRH_EEADRH3    : bit  absolute EEADRH.3;
  EEADRH_EEADRH2    : bit  absolute EEADRH.2;
  EEADRH_EEADRH1    : bit  absolute EEADRH.1;
  EEADRH_EEADRH0    : bit  absolute EEADRH.0;
  LCDDATA0          : byte absolute $0110;
  LCDDATA0_SEG7     : bit  absolute LCDDATA0.7;
  LCDDATA0_SEG6     : bit  absolute LCDDATA0.6;
  LCDDATA0_SEG5     : bit  absolute LCDDATA0.5;
  LCDDATA0_SEG4     : bit  absolute LCDDATA0.4;
  LCDDATA0_SEG3     : bit  absolute LCDDATA0.3;
  LCDDATA0_SEG2     : bit  absolute LCDDATA0.2;
  LCDDATA0_SEG1     : bit  absolute LCDDATA0.1;
  LCDDATA0_SEG0     : bit  absolute LCDDATA0.0;
  LCDDATA1          : byte absolute $0111;
  LCDDATA1_SEG15    : bit  absolute LCDDATA1.7;
  LCDDATA1_SEG14    : bit  absolute LCDDATA1.6;
  LCDDATA1_SEG13    : bit  absolute LCDDATA1.5;
  LCDDATA1_SEG12    : bit  absolute LCDDATA1.4;
  LCDDATA1_SEG11    : bit  absolute LCDDATA1.3;
  LCDDATA1_SEG10    : bit  absolute LCDDATA1.2;
  LCDDATA1_SEG9     : bit  absolute LCDDATA1.1;
  LCDDATA1_SEG8     : bit  absolute LCDDATA1.0;
  LCDDATA2          : byte absolute $0112;
  LCDDATA2_SEG23    : bit  absolute LCDDATA2.7;
  LCDDATA2_SEG22    : bit  absolute LCDDATA2.6;
  LCDDATA2_SEG21    : bit  absolute LCDDATA2.5;
  LCDDATA2_SEG20    : bit  absolute LCDDATA2.4;
  LCDDATA2_SEG19    : bit  absolute LCDDATA2.3;
  LCDDATA2_SEG18    : bit  absolute LCDDATA2.2;
  LCDDATA2_SEG17    : bit  absolute LCDDATA2.1;
  LCDDATA2_SEG16    : bit  absolute LCDDATA2.0;
  LCDDATA3          : byte absolute $0113;
  LCDDATA4          : byte absolute $0114;
  LCDDATA5          : byte absolute $0115;
  LCDDATA6          : byte absolute $0116;
  LCDDATA7          : byte absolute $0117;
  LCDDATA8          : byte absolute $0118;
  LCDDATA9          : byte absolute $0119;
  LCDDATA10         : byte absolute $011A;
  LCDDATA11         : byte absolute $011B;
  LCDSE0            : byte absolute $011C;
  LCDSE1            : byte absolute $011D;
  LCDSE2            : byte absolute $011E;
  TRISF             : byte absolute $0185;
  TRISF_TRISF7      : bit  absolute TRISF.7;
  TRISF_TRISF6      : bit  absolute TRISF.6;
  TRISF_TRISF5      : bit  absolute TRISF.5;
  TRISF_TRISF4      : bit  absolute TRISF.4;
  TRISF_TRISF3      : bit  absolute TRISF.3;
  TRISF_TRISF2      : bit  absolute TRISF.2;
  TRISF_TRISF1      : bit  absolute TRISF.1;
  TRISF_TRISF0      : bit  absolute TRISF.0;
  TRISG             : byte absolute $0187;
  TRISG_TRISG5      : bit  absolute TRISG.5;
  TRISG_TRISG4      : bit  absolute TRISG.4;
  TRISG_TRISG3      : bit  absolute TRISG.3;
  TRISG_TRISG2      : bit  absolute TRISG.2;
  TRISG_TRISG1      : bit  absolute TRISG.1;
  TRISG_TRISG0      : bit  absolute TRISG.0;
  PORTF             : byte absolute $0188;
  PORTF_RF7         : bit  absolute PORTF.7;
  PORTF_RF6         : bit  absolute PORTF.6;
  PORTF_RF5         : bit  absolute PORTF.5;
  PORTF_RF4         : bit  absolute PORTF.4;
  PORTF_RF3         : bit  absolute PORTF.3;
  PORTF_RF2         : bit  absolute PORTF.2;
  PORTF_RF1         : bit  absolute PORTF.1;
  PORTF_RF0         : bit  absolute PORTF.0;
  PORTG             : byte absolute $0189;
  PORTG_RG5         : bit  absolute PORTG.5;
  PORTG_RG4         : bit  absolute PORTG.4;
  PORTG_RG3         : bit  absolute PORTG.3;
  PORTG_RG2         : bit  absolute PORTG.2;
  PORTG_RG1         : bit  absolute PORTG.1;
  PORTG_RG0         : bit  absolute PORTG.0;
  EECON1            : byte absolute $018C;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $018D;
  LCDDATA12         : byte absolute $0190;
  LCDDATA12_SEG31   : bit  absolute LCDDATA12.7;
  LCDDATA12_SEG30   : bit  absolute LCDDATA12.6;
  LCDDATA12_SEG29   : bit  absolute LCDDATA12.5;
  LCDDATA12_SEG28   : bit  absolute LCDDATA12.4;
  LCDDATA12_SEG27   : bit  absolute LCDDATA12.3;
  LCDDATA12_SEG26   : bit  absolute LCDDATA12.2;
  LCDDATA12_SEG25   : bit  absolute LCDDATA12.1;
  LCDDATA12_SEG24   : bit  absolute LCDDATA12.0;
  LCDDATA13         : byte absolute $0191;
  LCDDATA13_SEG39   : bit  absolute LCDDATA13.7;
  LCDDATA13_SEG38   : bit  absolute LCDDATA13.6;
  LCDDATA13_SEG37   : bit  absolute LCDDATA13.5;
  LCDDATA13_SEG36   : bit  absolute LCDDATA13.4;
  LCDDATA13_SEG35   : bit  absolute LCDDATA13.3;
  LCDDATA13_SEG34   : bit  absolute LCDDATA13.2;
  LCDDATA13_SEG33   : bit  absolute LCDDATA13.1;
  LCDDATA13_SEG32   : bit  absolute LCDDATA13.0;
  LCDDATA14         : byte absolute $0192;
  LCDDATA14_SEG41   : bit  absolute LCDDATA14.1;
  LCDDATA14_SEG40   : bit  absolute LCDDATA14.0;
  LCDDATA15         : byte absolute $0193;
  LCDDATA16         : byte absolute $0194;
  LCDDATA17         : byte absolute $0195;
  LCDDATA18         : byte absolute $0196;
  LCDDATA19         : byte absolute $0197;
  LCDDATA20         : byte absolute $0198;
  LCDDATA21         : byte absolute $0199;
  LCDDATA22         : byte absolute $019A;
  LCDDATA23         : byte absolute $019B;
  LCDSE3            : byte absolute $019C;
  LCDSE4            : byte absolute $019D;
  LCDSE5            : byte absolute $019E;


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
                                            // Bank 2 : WDTCON, PORTB, LCDCON, LCDPS, LVDCON
                                            // Bank 3 : TRISF, TRISB, TRISG, PORTF, PORTG
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00D:SFR:ALL'}        // Bank 0 : PIR1, PIR2
                                            // Bank 1 : PIE1, PIE2
                                            // Bank 2 : EEDATL, EEADRL
                                            // Bank 3 : EECON1, EECON2
  {$SET_STATE_RAM '00E-00F:SFR'}            // Bank 0 : TMR1L, TMR1H
  {$SET_STATE_RAM '010-019:SFR:ALL'}        // Bank 0 : T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG
                                            // Bank 1 : OSCTUNE, ANSEL, PR2, SSPADD, SSPSTAT, WPUB, IOCB, CMCON1, TXSTA, SPBRG
                                            // Bank 2 : LCDDATA0, LCDDATA1, LCDDATA2, LCDDATA3, LCDDATA4, LCDDATA5, LCDDATA6, LCDDATA7, LCDDATA8, LCDDATA9
                                            // Bank 3 : LCDDATA12, LCDDATA13, LCDDATA14, LCDDATA15, LCDDATA16, LCDDATA17, LCDDATA18, LCDDATA19, LCDDATA20, LCDDATA21
  {$SET_STATE_RAM '01A-01B:SFR'}            // Bank 0 : RCREG, CCPR2L
  {$SET_STATE_RAM '01C-01E:SFR:ALL'}        // Bank 0 : CCPR2H, CCP2CON, ADRESH
                                            // Bank 1 : CMCON0, VRCON, ADRESL
                                            // Bank 2 : LCDSE0, LCDSE1, LCDSE2
                                            // Bank 3 : LCDSE3, LCDSE4, LCDSE5
  {$SET_STATE_RAM '01F-01F:SFR'}            // Bank 0 : ADCON0
  {$SET_STATE_RAM '020-06F:GPR:ALL'}       
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08E-08F:SFR'}            // Bank 1 : PCON, OSCCON
  {$SET_STATE_RAM '09F-09F:SFR'}            // Bank 1 : ADCON1
  {$SET_STATE_RAM '10E-10F:SFR'}            // Bank 2 : EEDATH, EEADRH
  {$SET_STATE_RAM '11A-11B:SFR'}            // Bank 2 : LCDDATA10, LCDDATA11
  {$SET_STATE_RAM '19A-19B:SFR'}            // Bank 3 : LCDDATA22, LCDDATA23


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '101-101:bnk0'} // maps to TMR0 (bank 0)
  {$SET_MAPPED_RAM '106-106:bnk0'} // maps to PORTB (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)
  {$SET_MAPPED_RAM '186-186:bnk1'} // maps to TRISB (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:F5'} // PIR2 bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01D:3F'} // CCP2CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:F5'} // PIE2 bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:13'} // PCON bits 7,6,5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:F0'} // IOCB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:03'} // CMCON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:F7'} // TXSTA bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:AF'} // VRCON bits 6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:70'} // ADCON1 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '105:1F'} // WDTCON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '109:37'} // LVDCON bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:1F'} // EEADRH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '187:3F'} // TRISG bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '189:3F'} // PORTG bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:8F'} // EECON1 bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19E:03'} // LCDSE5 bits 7,6,5,4,3,2 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RD6/SEG19
  // Pin  2 : RD7/SEG20
  // Pin  3 : RG0/SEG36
  // Pin  4 : RG1/SEG37
  // Pin  5 : RG2/SEG38
  // Pin  6 : RG3/SEG39
  // Pin  7 : RG4/SEG40
  // Pin  8 : RG5/SEG41
  // Pin  9 : Vss
  // Pin 10 : Vdd
  // Pin 11 : RF0/SEG32
  // Pin 12 : RF1/SEG33
  // Pin 13 : RF2/SEG34
  // Pin 14 : RF3/SEG35
  // Pin 15 : RB0/INT/SEG0
  // Pin 16 : RB1/SEG1
  // Pin 17 : RB2/SEG2
  // Pin 18 : RB3/SEG3
  // Pin 19 : Vdd
  // Pin 20 : Vss
  // Pin 21 : RB4/COM0
  // Pin 22 : RB5/COM1
  // Pin 23 : RB6/ICSPCLK/ICDCK/SEG14
  // Pin 24 : RB7/ICSPDAT/ICDDAT/SEG13
  // Pin 25 : AVss
  // Pin 26 : AVdd
  // Pin 27 : RA0/AN0/C1-/SEG12
  // Pin 28 : RA1/AN1/C2-/SEG7
  // Pin 29 : RA2/AN2/C2+/Vref-/COM2
  // Pin 30 : RA3/AN3/C1+/Vref+/SEG15
  // Pin 31 : RA4/C1OUT/T0CKI/SEG4
  // Pin 32 : RA5/AN4/C2OUT/SS/SEG5
  // Pin 33 : RE0/AN5/SEG21
  // Pin 34 : RE1/AN6/SEG22
  // Pin 35 : RE2/AN7/SEG23
  // Pin 36 : RE3/MCLR/Vpp
  // Pin 37 : RE4/SEG24
  // Pin 38 : Vdd
  // Pin 39 : RA7/OSC1/CLKIN/T1OSI
  // Pin 40 : RA6/OSC2/CLKOUT/T1OSO
  // Pin 41 : Vss
  // Pin 42 : RE5/SEG25
  // Pin 43 : RE6/SEG26
  // Pin 44 : RE7/SEG27
  // Pin 45 : RF4/SEG28
  // Pin 46 : RF5/SEG29
  // Pin 47 : RF6/SEG20
  // Pin 48 : RF7/SEG31
  // Pin 49 : RC0/VLCD1
  // Pin 50 : RC1/VLCD2
  // Pin 51 : RC2/VLCD3
  // Pin 52 : RC3/SEG6
  // Pin 53 : RD0/COM3
  // Pin 54 : RD1
  // Pin 55 : RD2/CCP2
  // Pin 56 : Vss
  // Pin 57 : Vdd
  // Pin 58 : RD3/SEG16
  // Pin 59 : RC4/T1G/SDO/SEG11
  // Pin 60 : RC5/T1CKI/CCP1/SEG10
  // Pin 61 : RC6/TX/CK/SCK/SCL/SEG9
  // Pin 62 : RC7/RX/DT/SDI/SDA/SEG8
  // Pin 63 : RD4/SEG17
  // Pin 64 : RD5/SEG18


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-27,1-28,2-29,3-30,4-31,5-32,6-40,7-39'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-15,1-16,2-17,3-18,4-21,5-22,6-23,7-24'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-49,1-50,2-51,3-52,4-59,5-60,6-61,7-62'} // PORTC
  {$MAP_RAM_TO_PIN '008:0-53,1-54,2-55,3-58,4-63,5-64,6-1,7-2'} // PORTD
  {$MAP_RAM_TO_PIN '009:0-33,1-34,2-35,3-36,4-37,5-42,6-43,7-44'} // PORTE
  {$MAP_RAM_TO_PIN '188:0-11,1-12,2-13,3-14,4-45,5-46,6-47,7-48'} // PORTF
  {$MAP_RAM_TO_PIN '189:0-3,1-4,2-5,3-6,4-7,5-8'} // PORTG


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $3FFF}  // RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT/T1OSO pin, RC on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_EXTRCIO   = $3FFE}  // RCIO oscillator: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, RC on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_INTOSCCLK = $3FFD}  // INTOSC oscillator: CLKOUT function on RA6/OSC2/CLKOUT/T1OSO pin, I/O function on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_INTOSCIO  = $3FFC}  // INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, I/O function on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_EC        = $3FFB}  // EC: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, CLKIN on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_HS        = $3FFA}  // HS oscillator: High-speed crystal/resonator on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_XT        = $3FF9}  // XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_LP        = $3FF8}  // LP oscillator: Low-power crystal on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $3FFF}  // WDT enabled
  {$define _WDTE_OFF       = $3FF7}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // PWRTE : Power Up Timer Enable bit
  {$define _PWRTE_OFF      = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $3FEF}  // PWRT enabled

  // MCLRE : RE3/MCLR pin function select bit
  {$define _MCLRE_ON       = $3FFF}  // RE3/MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $3FDF}  // RE3/MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF         = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON          = $3FBF}  // Program memory code protection is enabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF        = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON         = $3F7F}  // Data memory code protection is enabled

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON       = $3FFF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $3EFF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN   = $3DFF}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF      = $3CFF}  // BOR disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON        = $3FFF}  // Internal/External Switchover mode is enabled
  {$define _IESO_OFF       = $3BFF}  // Internal/External Switchover mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON       = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF      = $37FF}  // Fail-Safe Clock Monitor is disabled

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF      = $3FFF}  // In-Circuit Debugger disabled, RB6/ISCPCLK and RB7/ICSPDAT are general purpose I/O pins
  {$define _DEBUG_ON       = $2FFF}  // In-Circuit Debugger enabled, RB6/ICSPCLK and RB7/ICSPDAT are dedicated to the debugger

implementation
end.
