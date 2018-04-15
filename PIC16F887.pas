unit PIC16F887;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F887'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 40}
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
  STATUS_TO         : bit  absolute STATUS.4;
  STATUS_PD         : bit  absolute STATUS.3;
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
  PORTE_RE3         : bit  absolute PORTE.3;
  PORTE_RE2         : bit  absolute PORTE.2;
  PORTE_RE1         : bit  absolute PORTE.1;
  PORTE_RE0         : bit  absolute PORTE.0;
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000d;
  PIR2_OSFIF        : bit  absolute PIR2.7;
  PIR2_C2IF         : bit  absolute PIR2.6;
  PIR2_C1IF         : bit  absolute PIR2.5;
  PIR2_EEIF         : bit  absolute PIR2.4;
  PIR2_BCLIF        : bit  absolute PIR2.3;
  PIR2_ULPWUIF      : bit  absolute PIR2.2;
  PIR2_CCP2IF       : bit  absolute PIR2.1;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1GIV       : bit  absolute T1CON.5;
  T1CON_T1CKPS1     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS0     : bit  absolute T2CON.1;
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
  CCP1CON_P1M1      : bit  absolute CCP1CON.7;
  CCP1CON_P1M0      : bit  absolute CCP1CON.6;
  CCP1CON_DC1B1     : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0     : bit  absolute CCP1CON.4;
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
  RCREG             : byte absolute $001a;
  CCPR2L            : byte absolute $001b;
  CCPR2H            : byte absolute $001c;
  CCP2CON           : byte absolute $001d;
  CCP2CON_DC2B1     : bit  absolute CCP2CON.5;
  CCP2CON_DC2B0     : bit  absolute CCP2CON.4;
  CCP2CON_CCP2M3    : bit  absolute CCP2CON.3;
  CCP2CON_CCP2M2    : bit  absolute CCP2CON.2;
  CCP2CON_CCP2M1    : bit  absolute CCP2CON.1;
  CCP2CON_CCP2M0    : bit  absolute CCP2CON.0;
  ADRESH            : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADCS1      : bit  absolute ADCON0.7;
  ADCON0_ADCS0      : bit  absolute ADCON0.6;
  ADCON0_CHS3       : bit  absolute ADCON0.5;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RBPU   : bit  absolute OPTION_REG.7;
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
  TRISE_TRISE3      : bit  absolute TRISE.3;
  TRISE_TRISE2      : bit  absolute TRISE.2;
  TRISE_TRISE1      : bit  absolute TRISE.1;
  TRISE_TRISE0      : bit  absolute TRISE.0;
  PIE1              : byte absolute $008c;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008d;
  PIE2_OSFIE        : bit  absolute PIE2.7;
  PIE2_C2IE         : bit  absolute PIE2.6;
  PIE2_C1IE         : bit  absolute PIE2.5;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PIE2_BCLIE        : bit  absolute PIE2.3;
  PIE2_ULPWUIE      : bit  absolute PIE2.2;
  PIE2_CCP2IE       : bit  absolute PIE2.1;
  PCON              : byte absolute $008e;
  PCON_ULPWUE       : bit  absolute PCON.5;
  PCON_SBOREN       : bit  absolute PCON.4;
  PCON_POR          : bit  absolute PCON.3;
  PCON_BOR          : bit  absolute PCON.2;
  OSCCON            : byte absolute $008f;
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
  SSPCON2           : byte absolute $0091;
  SSPCON2_GCEN      : bit  absolute SSPCON2.7;
  SSPCON2_ACKSTAT   : bit  absolute SSPCON2.6;
  SSPCON2_ACKDT     : bit  absolute SSPCON2.5;
  SSPCON2_ACKEN     : bit  absolute SSPCON2.4;
  SSPCON2_RCEN      : bit  absolute SSPCON2.3;
  SSPCON2_PEN       : bit  absolute SSPCON2.2;
  SSPCON2_RSEN      : bit  absolute SSPCON2.1;
  SSPCON2_SEN       : bit  absolute SSPCON2.0;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPMSK            : byte absolute $0093;
  SSPMSK_MSK7       : bit  absolute SSPMSK.7;
  SSPMSK_MSK6       : bit  absolute SSPMSK.6;
  SSPMSK_MSK5       : bit  absolute SSPMSK.5;
  SSPMSK_MSK4       : bit  absolute SSPMSK.4;
  SSPMSK_MSK3       : bit  absolute SSPMSK.3;
  SSPMSK_MSK2       : bit  absolute SSPMSK.2;
  SSPMSK_MSK1       : bit  absolute SSPMSK.1;
  SSPMSK_MSK0       : bit  absolute SSPMSK.0;
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
  WPUB_WPUB7        : bit  absolute WPUB.7;
  WPUB_WPUB6        : bit  absolute WPUB.6;
  WPUB_WPUB5        : bit  absolute WPUB.5;
  WPUB_WPUB4        : bit  absolute WPUB.4;
  WPUB_WPUB3        : bit  absolute WPUB.3;
  WPUB_WPUB2        : bit  absolute WPUB.2;
  WPUB_WPUB1        : bit  absolute WPUB.1;
  WPUB_WPUB0        : bit  absolute WPUB.0;
  IOCB              : byte absolute $0096;
  IOCB_IOCB7        : bit  absolute IOCB.7;
  IOCB_IOCB6        : bit  absolute IOCB.6;
  IOCB_IOCB5        : bit  absolute IOCB.5;
  IOCB_IOCB4        : bit  absolute IOCB.4;
  IOCB_IOCB3        : bit  absolute IOCB.3;
  IOCB_IOCB2        : bit  absolute IOCB.2;
  IOCB_IOCB1        : bit  absolute IOCB.1;
  IOCB_IOCB0        : bit  absolute IOCB.0;
  VRCON             : byte absolute $0097;
  VRCON_VREN        : bit  absolute VRCON.7;
  VRCON_VROE        : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VRSS        : bit  absolute VRCON.4;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_SENDB       : bit  absolute TXSTA.3;
  TXSTA_BRGH        : bit  absolute TXSTA.2;
  TXSTA_TRMT        : bit  absolute TXSTA.1;
  TXSTA_TX9D        : bit  absolute TXSTA.0;
  SPBRG             : byte absolute $0099;
  SPBRG_BRG7        : bit  absolute SPBRG.7;
  SPBRG_BRG6        : bit  absolute SPBRG.6;
  SPBRG_BRG5        : bit  absolute SPBRG.5;
  SPBRG_BRG4        : bit  absolute SPBRG.4;
  SPBRG_BRG3        : bit  absolute SPBRG.3;
  SPBRG_BRG2        : bit  absolute SPBRG.2;
  SPBRG_BRG1        : bit  absolute SPBRG.1;
  SPBRG_BRG0        : bit  absolute SPBRG.0;
  SPBRGH            : byte absolute $009a;
  SPBRGH_BRG15      : bit  absolute SPBRGH.7;
  SPBRGH_BRG14      : bit  absolute SPBRGH.6;
  SPBRGH_BRG13      : bit  absolute SPBRGH.5;
  SPBRGH_BRG12      : bit  absolute SPBRGH.4;
  SPBRGH_BRG11      : bit  absolute SPBRGH.3;
  SPBRGH_BRG10      : bit  absolute SPBRGH.2;
  SPBRGH_BRG9       : bit  absolute SPBRGH.1;
  SPBRGH_BRG8       : bit  absolute SPBRGH.0;
  PWM1CON           : byte absolute $009b;
  PWM1CON_PRSEN     : bit  absolute PWM1CON.7;
  PWM1CON_PDC6      : bit  absolute PWM1CON.6;
  PWM1CON_PDC5      : bit  absolute PWM1CON.5;
  PWM1CON_PDC4      : bit  absolute PWM1CON.4;
  PWM1CON_PDC3      : bit  absolute PWM1CON.3;
  PWM1CON_PDC2      : bit  absolute PWM1CON.2;
  PWM1CON_PDC1      : bit  absolute PWM1CON.1;
  PWM1CON_PDC0      : bit  absolute PWM1CON.0;
  ECCPAS            : byte absolute $009c;
  ECCPAS_ECCPASE    : bit  absolute ECCPAS.7;
  ECCPAS_ECCPAS2    : bit  absolute ECCPAS.6;
  ECCPAS_ECCPAS1    : bit  absolute ECCPAS.5;
  ECCPAS_ECCPAS0    : bit  absolute ECCPAS.4;
  ECCPAS_PSSAC1     : bit  absolute ECCPAS.3;
  ECCPAS_PSSAC0     : bit  absolute ECCPAS.2;
  ECCPAS_PSSBD1     : bit  absolute ECCPAS.1;
  ECCPAS_PSSBD0     : bit  absolute ECCPAS.0;
  PSTRCON           : byte absolute $009d;
  PSTRCON_STRSYNC   : bit  absolute PSTRCON.4;
  PSTRCON_STRD      : bit  absolute PSTRCON.3;
  PSTRCON_STRC      : bit  absolute PSTRCON.2;
  PSTRCON_STRB      : bit  absolute PSTRCON.1;
  PSTRCON_STRA      : bit  absolute PSTRCON.0;
  ADRESL            : byte absolute $009e;
  ADCON1            : byte absolute $009f;
  ADCON1_ADFM       : bit  absolute ADCON1.6;
  ADCON1_VCFG1      : bit  absolute ADCON1.5;
  ADCON1_VCFG0      : bit  absolute ADCON1.4;
  WDTCON            : byte absolute $0105;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  CM1CON0           : byte absolute $0107;
  CM1CON0_C1ON      : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT     : bit  absolute CM1CON0.6;
  CM1CON0_C1OE      : bit  absolute CM1CON0.5;
  CM1CON0_C1POL     : bit  absolute CM1CON0.4;
  CM1CON0_C1R       : bit  absolute CM1CON0.3;
  CM1CON0_C1CH1     : bit  absolute CM1CON0.1;
  CM1CON0_C1CH0     : bit  absolute CM1CON0.0;
  CM2CON0           : byte absolute $0108;
  CM2CON0_C2ON      : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT     : bit  absolute CM2CON0.6;
  CM2CON0_C2OE      : bit  absolute CM2CON0.5;
  CM2CON0_C2POL     : bit  absolute CM2CON0.4;
  CM2CON0_C2R       : bit  absolute CM2CON0.3;
  CM2CON0_C2CH1     : bit  absolute CM2CON0.1;
  CM2CON0_C2CH0     : bit  absolute CM2CON0.0;
  CM2CON1           : byte absolute $0109;
  CM2CON1_MC1OUT    : bit  absolute CM2CON1.7;
  CM2CON1_MC2OUT    : bit  absolute CM2CON1.6;
  CM2CON1_C1RSEL    : bit  absolute CM2CON1.5;
  CM2CON1_C2RSEL    : bit  absolute CM2CON1.4;
  CM2CON1_T1GSS     : bit  absolute CM2CON1.3;
  CM2CON1_C2SYNC    : bit  absolute CM2CON1.2;
  EEDATA            : byte absolute $010c;
  EEADR             : byte absolute $010d;
  EEDATH            : byte absolute $010e;
  EEADRH            : byte absolute $010f;
  SRCON             : byte absolute $0185;
  SRCON_SR1         : bit  absolute SRCON.7;
  SRCON_SR0         : bit  absolute SRCON.6;
  SRCON_C1SEN       : bit  absolute SRCON.5;
  SRCON_C2REN       : bit  absolute SRCON.4;
  SRCON_PULSS       : bit  absolute SRCON.3;
  SRCON_PULSR       : bit  absolute SRCON.2;
  SRCON_FVREN       : bit  absolute SRCON.1;
  BAUDCTL           : byte absolute $0187;
  BAUDCTL_ABDOVF    : bit  absolute BAUDCTL.6;
  BAUDCTL_RCIDL     : bit  absolute BAUDCTL.5;
  BAUDCTL_SCKP      : bit  absolute BAUDCTL.4;
  BAUDCTL_BRG16     : bit  absolute BAUDCTL.3;
  BAUDCTL_WUE       : bit  absolute BAUDCTL.2;
  BAUDCTL_ABDEN     : bit  absolute BAUDCTL.1;
  ANSEL             : byte absolute $0188;
  ANSEL_ANS7        : bit  absolute ANSEL.7;
  ANSEL_ANS6        : bit  absolute ANSEL.6;
  ANSEL_ANS5        : bit  absolute ANSEL.5;
  ANSEL_ANS4        : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;
  ANSELH            : byte absolute $0189;
  ANSELH_ANS13      : bit  absolute ANSELH.5;
  ANSELH_ANS12      : bit  absolute ANSELH.4;
  ANSELH_ANS11      : bit  absolute ANSELH.3;
  ANSELH_ANS10      : bit  absolute ANSELH.2;
  ANSELH_ANS9       : bit  absolute ANSELH.1;
  ANSELH_ANS8       : bit  absolute ANSELH.0;
  EECON1            : byte absolute $018c;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_WRERR      : bit  absolute EECON1.6;
  EECON1_WREN       : bit  absolute EECON1.5;
  EECON1_WR         : bit  absolute EECON1.4;
  EECON1_RD         : bit  absolute EECON1.3;
  EECON2            : byte absolute $018d;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-01F:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE, PCLATH, INTCON, PIR1, PIR2, TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG, CCPR2L, CCPR2H, CCP2CON, ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-09F:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB, TRISC, TRISD, TRISE, PCLATH, INTCON, PIE1, PIE2, PCON, OSCCON, OSCTUNE, SSPCON2, PR2, SSPMSK, SSPSTAT, WPUB, IOCB, VRCON, TXSTA, SPBRG, SPBRGH, PWM1CON, ECCPAS, PSTRCON, ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0FF:GPR'} 
  {$SET_STATE_RAM '100-10F:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, WDTCON, PORTB, CM1CON0, CM2CON0, CM2CON1, PCLATH, INTCON, EEDATA, EEADR, EEDATH, EEADRH
  {$SET_STATE_RAM '110-17F:GPR'} 
  {$SET_STATE_RAM '180-18D:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, SRCON, TRISB, BAUDCTL, ANSEL, ANSELH, PCLATH, INTCON, EECON1, EECON2
  {$SET_STATE_RAM '190-1FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '100-104:bnk0'} // INDF, TMR0, PCL, STATUS, FSR
  {$SET_MAPPED_RAM '106-106:bnk0'} // PORTB
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '180-180:bnk0'} // INDF
  {$SET_MAPPED_RAM '181-181:bnk1'} // OPTION_REG
  {$SET_MAPPED_RAM '182-184:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '186-186:bnk1'} // TRISB
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '009:0F'} // PORTE
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:7F'} // PIR1
  {$SET_UNIMP_BITS '00D:FD'} // PIR2
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '01D:3F'} // CCP2CON
  {$SET_UNIMP_BITS '089:0F'} // TRISE
  {$SET_UNIMP_BITS '08C:7F'} // PIE1
  {$SET_UNIMP_BITS '08D:FD'} // PIE2
  {$SET_UNIMP_BITS '08E:33'} // PCON
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '093:00'} // SSPMSK
  {$SET_UNIMP_BITS '09D:1F'} // PSTRCON
  {$SET_UNIMP_BITS '09F:B0'} // ADCON1
  {$SET_UNIMP_BITS '105:1F'} // WDTCON
  {$SET_UNIMP_BITS '107:FB'} // CM1CON0
  {$SET_UNIMP_BITS '108:FB'} // CM2CON0
  {$SET_UNIMP_BITS '109:F3'} // CM2CON1
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH
  {$SET_UNIMP_BITS '10F:1F'} // EEADRH
  {$SET_UNIMP_BITS '185:FD'} // SRCON
  {$SET_UNIMP_BITS '187:DB'} // BAUDCTL
  {$SET_UNIMP_BITS '189:3F'} // ANSELH
  {$SET_UNIMP_BITS '18C:8F'} // EECON1


  // -- PIN mapping --

  // Pin  1 : RC7/RX/DT
  // Pin  2 : RD4
  // Pin  3 : RD5/P1B
  // Pin  4 : RD6/P1C
  // Pin  5 : RD7/P1D
  // Pin  6 : VSS
  // Pin  7 : VDD
  // Pin  8 : RB0/AN12/INT
  // Pin  9 : RB1/AN10/C12IN3-
  // Pin 10 : RB2/AN8
  // Pin 11 : RB3/AN9/PGM/C12IN2-
  // Pin 12 : RB4/AN11
  // Pin 13 : RB5/AN13/T1G
  // Pin 14 : RB6/ICSPCLK
  // Pin 15 : RB7/ICSPDAT
  // Pin 16 : RE3/nMCLR/VPP
  // Pin 17 : RA0/AN0/ULPWU/C12IN0-
  // Pin 18 : RA1/AN1/C12IN1-
  // Pin 19 : RA2/AN2/VREF-/CVREF/C2IN+
  // Pin 20 : RA3/AN3/VREF+/C1IN+
  // Pin 21 : RA4/T0CKI/C1OUT
  // Pin 22 : RA5/AN4/nSS/C2OUT
  // Pin 23 : RE0/AN5
  // Pin 24 : RE1/AN6
  // Pin 25 : RE2/AN7
  // Pin 26 : VDD
  // Pin 27 : VSS
  // Pin 28 : RA7/OSC1/CLKIN
  // Pin 29 : RA6/OSC2/CLKOUT
  // Pin 30 : RC0/T1OSO/T1CKI
  // Pin 31 : RC1/T1OSCI/CCP2
  // Pin 32 : RC2/P1A/CCP1
  // Pin 33 : RC3/SCK/SCL
  // Pin 34 : RD0
  // Pin 35 : RD1
  // Pin 36 : RD2
  // Pin 37 : RD3
  // Pin 38 : RC4/SDI/SDA
  // Pin 39 : RC5/SDO
  // Pin 40 : RC6/TX/CK


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-19,3-20,4-21,5-22,6-29,7-28'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-8,1-9,2-10,3-11,4-12,5-13,6-14,7-15'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-30,1-31,2-32,3-33,4-38,5-39,6-40,7-1'} // PORTC
  {$MAP_RAM_TO_PIN '008:0-34,1-35,2-36,3-37,4-2,5-3,6-4,7-5'} // PORTD
  {$MAP_RAM_TO_PIN '009:0-23,1-24,2-25,3-16'} // PORTE


  // -- Bits Configuration --

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF           = $3FFF}  // In-Circuit Debugger disabled, RB6/ICSPCLK and RB7/ICSPDAT are general purpose I/O pins
  {$define _DEBUG_ON            = $3FFE}  // In_Circuit Debugger enabled, RB6/ICSPCLK and RB7/ICSPDAT are dedicated to the debugger

  // LVP : Low Voltage Programming Enable bit
  {$define _LVP_ON              = $3FFF}  // RB3/PGM pin has PGM function, low voltage programming enabled
  {$define _LVP_OFF             = $3FFD}  // RB3 pin has digital I/O, HV on MCLR must be used for programming

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON            = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF           = $3FFB}  // Fail-Safe Clock Monitor is disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON             = $3FFF}  // Internal/External Switchover mode is enabled
  {$define _IESO_OFF            = $3FF7}  // Internal/External Switchover mode is disabled

  // BOREN : Brown Out Reset Selection bits
  {$define _BOREN_ON            = $3FFF}  // BOR enabled
  {$define _BOREN_NSLEEP        = $3FEF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN        = $3FDF}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF           = $3FCF}  // BOR disabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF             = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON              = $3FBF}  // Data memory code protection is enabled

  // CP : Code Protection bit
  {$define _CP_OFF              = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON               = $3F7F}  // Program memory code protection is enabled

  // MCLRE : RE3/MCLR pin function select bit
  {$define _MCLRE_ON            = $3FFF}  // RE3/MCLR pin function is MCLR
  {$define _MCLRE_OFF           = $3EFF}  // RE3/MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF           = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON            = $3DFF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON             = $3FFF}  // WDT enabled
  {$define _WDTE_OFF            = $3BFF}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRC_CLKOUT   = $3FFF}  // RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN
  {$define _FOSC_EXTRC_NOCLKOUT = $37FF}  // RCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN
  {$define _FOSC_INTRC_CLKOUT   = $2FFF}  // INTOSC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN
  {$define _FOSC_INTRC_NOCLKOUT = $27FF}  // INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN
  {$define _FOSC_EC             = $1FFF}  // EC: I/O function on RA6/OSC2/CLKOUT pin, CLKIN on RA7/OSC1/CLKIN
  {$define _FOSC_HS             = $17FF}  // HS oscillator: High-speed crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN
  {$define _FOSC_XT             = $0FFF}  // XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN
  {$define _FOSC_LP             = $07FF}  // LP oscillator: Low-power crystal on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN

  // WRT : Flash Program Memory Self Write Enable bits
  {$define _WRT_HALF            = $0700}  // 0000h to 0FFFh write protected, 1000h to 1FFFh may be modified by EECON control
  {$define _WRT_1FOURTH         = $0701}  // 0000h to 07FFh write protected, 0800h to 1FFFh may be modified by EECON control
  {$define _WRT_256             = $0702}  // 0000h to 00FFh write protected, 0100h to 1FFFh may be modified by EECON control
  {$define _WRT_OFF             = $0703}  // Write protection off

  // BOR4V : Brown-out Reset Selection bit
  {$define _BOR4V_BOR21V        = $0700}  // Brown-out Reset set to 2.1V
  {$define _BOR4V_BOR40V        = $0704}  // Brown-out Reset set to 4.0V

implementation
end.
