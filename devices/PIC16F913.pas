unit PIC16F913;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F913'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 4096}

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
  PORTE             : byte absolute $0009;
  PORTE_RE3         : bit  absolute PORTE.3;
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000d;
  PIR2_OSFIF        : bit  absolute PIR2.6;
  PIR2_C2IF         : bit  absolute PIR2.5;
  PIR2_C1IF         : bit  absolute PIR2.4;
  PIR2_LCDIF        : bit  absolute PIR2.3;
  PIR2_LVDIF        : bit  absolute PIR2.2;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1GE        : bit  absolute T1CON.5;
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
  RCREG             : byte absolute $001a;
  ADRESH            : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADFM       : bit  absolute ADCON0.7;
  ADCON0_VCFG1      : bit  absolute ADCON0.6;
  ADCON0_VCFG0      : bit  absolute ADCON0.5;
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
  TRISE             : byte absolute $0089;
  TRISE_TRISE3      : bit  absolute TRISE.3;
  PIE1              : byte absolute $008c;
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008d;
  PIE2_OSFIE        : bit  absolute PIE2.6;
  PIE2_C2IE         : bit  absolute PIE2.5;
  PIE2_C1IE         : bit  absolute PIE2.4;
  PIE2_LCDIE        : bit  absolute PIE2.3;
  PIE2_LVDIE        : bit  absolute PIE2.2;
  PCON              : byte absolute $008e;
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
  ANSEL             : byte absolute $0091;
  ANSEL_ANS4        : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;
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
  CMCON1            : byte absolute $0097;
  CMCON1_T1GSS      : bit  absolute CMCON1.1;
  CMCON1_C2SYNC     : bit  absolute CMCON1.0;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.3;
  TXSTA_TRMT        : bit  absolute TXSTA.2;
  TXSTA_TX9D        : bit  absolute TXSTA.1;
  SPBRG             : byte absolute $0099;
  CMCON0            : byte absolute $009c;
  CMCON0_C2OUT      : bit  absolute CMCON0.7;
  CMCON0_C1OUT      : bit  absolute CMCON0.6;
  CMCON0_C2INV      : bit  absolute CMCON0.5;
  CMCON0_C1INV      : bit  absolute CMCON0.4;
  CMCON0_CIS        : bit  absolute CMCON0.3;
  CMCON0_CM2        : bit  absolute CMCON0.2;
  CMCON0_CM1        : bit  absolute CMCON0.1;
  CMCON0_CM0        : bit  absolute CMCON0.0;
  VRCON             : byte absolute $009d;
  VRCON_VREN        : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  ADRESL            : byte absolute $009e;
  ADCON1            : byte absolute $009f;
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
  EEDATL            : byte absolute $010c;
  EEDATL_EEDATL7    : bit  absolute EEDATL.7;
  EEDATL_EEDATL6    : bit  absolute EEDATL.6;
  EEDATL_EEDATL5    : bit  absolute EEDATL.5;
  EEDATL_EEDATL4    : bit  absolute EEDATL.4;
  EEDATL_EEDATL3    : bit  absolute EEDATL.3;
  EEDATL_EEDATL2    : bit  absolute EEDATL.2;
  EEDATL_EEDATL1    : bit  absolute EEDATL.1;
  EEDATL_EEDATL0    : bit  absolute EEDATL.0;
  EEADRL            : byte absolute $010d;
  EEADRL_EEADRL7    : bit  absolute EEADRL.7;
  EEADRL_EEADRL6    : bit  absolute EEADRL.6;
  EEADRL_EEADRL5    : bit  absolute EEADRL.5;
  EEADRL_EEADRL4    : bit  absolute EEADRL.4;
  EEADRL_EEADRL3    : bit  absolute EEADRL.3;
  EEADRL_EEADRL2    : bit  absolute EEADRL.2;
  EEADRL_EEADRL1    : bit  absolute EEADRL.1;
  EEADRL_EEADRL0    : bit  absolute EEADRL.0;
  EEDATH            : byte absolute $010e;
  EEDATH_EEDATH5    : bit  absolute EEDATH.5;
  EEDATH_EEDATH4    : bit  absolute EEDATH.4;
  EEDATH_EEDATH3    : bit  absolute EEDATH.3;
  EEDATH_EEDATH2    : bit  absolute EEDATH.2;
  EEDATH_EEDATH1    : bit  absolute EEDATH.1;
  EEDATH_EEDATH0    : bit  absolute EEDATH.0;
  EEADRH            : byte absolute $010f;
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
  LCDDATA3          : byte absolute $0113;
  LCDDATA4          : byte absolute $0114;
  LCDDATA6          : byte absolute $0116;
  LCDDATA7          : byte absolute $0117;
  LCDDATA9          : byte absolute $0119;
  LCDDATA10         : byte absolute $011a;
  LCDSE0            : byte absolute $011c;
  LCDSE1            : byte absolute $011d;
  EECON1            : byte absolute $018c;
  EECON1_EEPGD      : bit  absolute EECON1.7;
  EECON1_WRERR      : bit  absolute EECON1.6;
  EECON1_WREN       : bit  absolute EECON1.5;
  EECON1_WR         : bit  absolute EECON1.4;
  EECON1_RD         : bit  absolute EECON1.3;
  EECON2            : byte absolute $018d;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '009-01A:SFR'}  // PORTE, PCLATH, INTCON, PIR1, PIR2, TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-087:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB, TRISC
  {$SET_STATE_RAM '089-099:SFR'}  // TRISE, PCLATH, INTCON, PIE1, PIE2, PCON, OSCCON, OSCTUNE, ANSEL, PR2, SSPADD, SSPSTAT, WPUB, IOCB, CMCON1, TXSTA, SPBRG
  {$SET_STATE_RAM '09C-09F:SFR'}  // CMCON0, VRCON, ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0FF:GPR'} 
  {$SET_STATE_RAM '100-111:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, WDTCON, PORTB, LCDCON, LCDPS, LVDCON, PCLATH, INTCON, EEDATL, EEADRL, EEDATH, EEADRH, LCDDATA0, LCDDATA1
  {$SET_STATE_RAM '113-114:SFR'}  // LCDDATA3, LCDDATA4
  {$SET_STATE_RAM '116-117:SFR'}  // LCDDATA6, LCDDATA7
  {$SET_STATE_RAM '119-11A:SFR'}  // LCDDATA9, LCDDATA10
  {$SET_STATE_RAM '11C-11D:SFR'}  // LCDSE0, LCDSE1
  {$SET_STATE_RAM '120-17F:GPR'} 
  {$SET_STATE_RAM '180-184:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR
  {$SET_STATE_RAM '186-186:SFR'}  // TRISB
  {$SET_STATE_RAM '18A-18D:SFR'}  // PCLATH, INTCON, EECON1, EECON2
  {$SET_STATE_RAM '1F0-1FF:GPR'} 


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
  {$SET_UNIMP_BITS '009:08'} // PORTE
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00D:F4'} // PIR2
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON
  {$SET_UNIMP_BITS '089:08'} // TRISE
  {$SET_UNIMP_BITS '08D:F4'} // PIE2
  {$SET_UNIMP_BITS '08E:13'} // PCON
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '091:1F'} // ANSEL
  {$SET_UNIMP_BITS '096:F0'} // IOCB
  {$SET_UNIMP_BITS '097:03'} // CMCON1
  {$SET_UNIMP_BITS '098:F7'} // TXSTA
  {$SET_UNIMP_BITS '09D:AF'} // VRCON
  {$SET_UNIMP_BITS '09F:70'} // ADCON1
  {$SET_UNIMP_BITS '105:1F'} // WDTCON
  {$SET_UNIMP_BITS '109:37'} // LVDCON
  {$SET_UNIMP_BITS '10E:3F'} // EEDATH
  {$SET_UNIMP_BITS '10F:1F'} // EEADRH
  {$SET_UNIMP_BITS '18C:8F'} // EECON1


  // -- PIN mapping --

  // Pin  1 : RE3/MCLR/Vpp
  // Pin  2 : RA0/AN0/C1-/SEG12
  // Pin  3 : RA1/AN1/C2-/SEG7
  // Pin  4 : RA2/AN2/C2+/VREF-/COM2
  // Pin  5 : RA3/AN3/C1+/VREF+/COM3/SEG15
  // Pin  6 : RA4/C1OUT/T0CKI/SEG4
  // Pin  7 : RA5/AN4/C2OUT/SS/SEG5
  // Pin  8 : Vss
  // Pin  9 : RA7/OSC1/CLKIN/T1OSI
  // Pin 10 : RA6/OSC2/CLKOUT/T1OSO
  // Pin 11 : RC0/VLCD1
  // Pin 12 : RC1/VLCD2
  // Pin 13 : RC2/VLCD3
  // Pin 14 : RC3/SEG6
  // Pin 15 : RC4/T1G/SDO/SEG11
  // Pin 16 : RC5/T1CKI/CCP1/SEG10
  // Pin 17 : RC6/TX/CK/SCK/SCL/SEG9
  // Pin 18 : RC7/RX/DT/SDI/SDA/SEG8
  // Pin 19 : Vss
  // Pin 20 : Vdd
  // Pin 21 : RB0/INT/SEG0
  // Pin 22 : RB1/SEG1
  // Pin 23 : RB2/SEG2
  // Pin 24 : RB3/SEG3
  // Pin 25 : RB4/COM0
  // Pin 26 : RB5/COM1
  // Pin 27 : RB6/ICSPCK/ICDCK/SEG14
  // Pin 28 : RB7/ICSPDAT/ICDDAT/SEG13


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-2,1-3,2-4,3-5,4-6,5-7,6-10,7-9'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-21,1-22,2-23,3-24,4-25,5-26,6-27,7-28'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-11,1-12,2-13,3-14,4-15,5-16,6-17,7-18'} // PORTC
  {$MAP_RAM_TO_PIN '009:3-1'} // PORTE


  // -- Bits Configuration --

  // DEBUG : In-Circuit Debugger Mode bit
  {$define _DEBUG_OFF      = $1FFF}  // In-Circuit Debugger disabled, RB6/ISCPCLK and RB7/ICSPDAT are general purpose I/O pins
  {$define _DEBUG_ON       = $1FFE}  // In-Circuit Debugger enabled, RB6/ICSPCLK and RB7/ICSPDAT are dedicated to the debugger

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON       = $1FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF      = $1FFD}  // Fail-Safe Clock Monitor is disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON        = $1FFF}  // Internal/External Switchover mode is enabled
  {$define _IESO_OFF       = $1FFB}  // Internal/External Switchover mode is disabled

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON       = $1FFF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $1FF7}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN   = $1FEF}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF      = $1FE7}  // BOR disabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF        = $1FFF}  // Data memory code protection is disabled
  {$define _CPD_ON         = $1FDF}  // Data memory code protection is enabled

  // CP : Code Protection bit
  {$define _CP_OFF         = $1FFF}  // Program memory code protection is disabled
  {$define _CP_ON          = $1FBF}  // Program memory code protection is enabled

  // MCLRE : RE3/MCLR pin function select bit
  {$define _MCLRE_ON       = $1FFF}  // RE3/MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $1F7F}  // RE3/MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power Up Timer Enable bit
  {$define _PWRTE_OFF      = $1FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $1EFF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $1FFF}  // WDT enabled
  {$define _WDTE_OFF       = $1DFF}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $1FFF}  // RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT/T1OSO pin, RC on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_EXTRCIO   = $1BFF}  // RCIO oscillator: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, RC on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_INTOSCCLK = $17FF}  // INTOSC oscillator: CLKOUT function on RA6/OSC2/CLKOUT/T1OSO pin, I/O function on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_INTOSCIO  = $13FF}  // INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, I/O function on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_EC        = $0FFF}  // EC: I/O function on RA6/OSC2/CLKOUT/T1OSO pin, CLKIN on RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_HS        = $0BFF}  // HS oscillator: High-speed crystal/resonator on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_XT        = $07FF}  // XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI
  {$define _FOSC_LP        = $03FF}  // LP oscillator: Low-power crystal on RA6/OSC2/CLKOUT/T1OSO and RA7/OSC1/CLKIN/T1OSI

implementation
end.
