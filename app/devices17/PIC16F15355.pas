unit PIC16F15355;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F15355'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 64}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 8192}

interface
var
  INDF0                  : byte absolute $0000;
  INDF1                  : byte absolute $0001;
  PCL                    : byte absolute $0002;
  STATUS                 : byte absolute $0003;
  STATUS_nTO             : bit  absolute STATUS.4;
  STATUS_nPD             : bit  absolute STATUS.3;
  STATUS_Z               : bit  absolute STATUS.2;
  STATUS_DC              : bit  absolute STATUS.1;
  STATUS_C               : bit  absolute STATUS.0;
  FSR0L                  : byte absolute $0004;
  FSR0H                  : byte absolute $0005;
  FSR1L                  : byte absolute $0006;
  FSR1H                  : byte absolute $0007;
  BSR                    : byte absolute $0008;
  BSR_BSR5               : bit  absolute BSR.5;
  BSR_BSR4               : bit  absolute BSR.4;
  BSR_BSR3               : bit  absolute BSR.3;
  BSR_BSR2               : bit  absolute BSR.2;
  BSR_BSR1               : bit  absolute BSR.1;
  BSR_BSR0               : bit  absolute BSR.0;
  WREG                   : byte absolute $0009;
  PCLATH                 : byte absolute $000A;
  PCLATH_PCLATH6         : bit  absolute PCLATH.6;
  PCLATH_PCLATH5         : bit  absolute PCLATH.5;
  PCLATH_PCLATH4         : bit  absolute PCLATH.4;
  PCLATH_PCLATH3         : bit  absolute PCLATH.3;
  PCLATH_PCLATH2         : bit  absolute PCLATH.2;
  PCLATH_PCLATH1         : bit  absolute PCLATH.1;
  PCLATH_PCLATH0         : bit  absolute PCLATH.0;
  INTCON                 : byte absolute $000B;
  INTCON_GIE             : bit  absolute INTCON.7;
  INTCON_PEIE            : bit  absolute INTCON.6;
  INTCON_INTEDG          : bit  absolute INTCON.0;
  PORTA                  : byte absolute $000C;
  PORTA_RA7              : bit  absolute PORTA.7;
  PORTA_RA6              : bit  absolute PORTA.6;
  PORTA_RA5              : bit  absolute PORTA.5;
  PORTA_RA4              : bit  absolute PORTA.4;
  PORTA_RA3              : bit  absolute PORTA.3;
  PORTA_RA2              : bit  absolute PORTA.2;
  PORTA_RA1              : bit  absolute PORTA.1;
  PORTA_RA0              : bit  absolute PORTA.0;
  PORTB                  : byte absolute $000D;
  PORTB_RB7              : bit  absolute PORTB.7;
  PORTB_RB6              : bit  absolute PORTB.6;
  PORTB_RB5              : bit  absolute PORTB.5;
  PORTB_RB4              : bit  absolute PORTB.4;
  PORTB_RB3              : bit  absolute PORTB.3;
  PORTB_RB2              : bit  absolute PORTB.2;
  PORTB_RB1              : bit  absolute PORTB.1;
  PORTB_RB0              : bit  absolute PORTB.0;
  PORTC                  : byte absolute $000E;
  PORTC_RC7              : bit  absolute PORTC.7;
  PORTC_RC6              : bit  absolute PORTC.6;
  PORTC_RC5              : bit  absolute PORTC.5;
  PORTC_RC4              : bit  absolute PORTC.4;
  PORTC_RC3              : bit  absolute PORTC.3;
  PORTC_RC2              : bit  absolute PORTC.2;
  PORTC_RC1              : bit  absolute PORTC.1;
  PORTC_RC0              : bit  absolute PORTC.0;
  PORTE                  : byte absolute $0010;
  PORTE_RE3              : bit  absolute PORTE.3;
  TRISA                  : byte absolute $0012;
  TRISA_TRISA7           : bit  absolute TRISA.7;
  TRISA_TRISA6           : bit  absolute TRISA.6;
  TRISA_TRISA5           : bit  absolute TRISA.5;
  TRISA_TRISA4           : bit  absolute TRISA.4;
  TRISA_TRISA3           : bit  absolute TRISA.3;
  TRISA_TRISA2           : bit  absolute TRISA.2;
  TRISA_TRISA1           : bit  absolute TRISA.1;
  TRISA_TRISA0           : bit  absolute TRISA.0;
  TRISB                  : byte absolute $0013;
  TRISB_TRISB7           : bit  absolute TRISB.7;
  TRISB_TRISB6           : bit  absolute TRISB.6;
  TRISB_TRISB5           : bit  absolute TRISB.5;
  TRISB_TRISB4           : bit  absolute TRISB.4;
  TRISB_TRISB3           : bit  absolute TRISB.3;
  TRISB_TRISB2           : bit  absolute TRISB.2;
  TRISB_TRISB1           : bit  absolute TRISB.1;
  TRISB_TRISB0           : bit  absolute TRISB.0;
  TRISC                  : byte absolute $0014;
  TRISC_TRISC7           : bit  absolute TRISC.7;
  TRISC_TRISC6           : bit  absolute TRISC.6;
  TRISC_TRISC5           : bit  absolute TRISC.5;
  TRISC_TRISC4           : bit  absolute TRISC.4;
  TRISC_TRISC3           : bit  absolute TRISC.3;
  TRISC_TRISC2           : bit  absolute TRISC.2;
  TRISC_TRISC1           : bit  absolute TRISC.1;
  TRISC_TRISC0           : bit  absolute TRISC.0;
  TRISE                  : byte absolute $0016;
  TRISE_TRISE3           : bit  absolute TRISE.3;
  LATA                   : byte absolute $0018;
  LATA_LATA7             : bit  absolute LATA.7;
  LATA_LATA6             : bit  absolute LATA.6;
  LATA_LATA5             : bit  absolute LATA.5;
  LATA_LATA4             : bit  absolute LATA.4;
  LATA_LATA3             : bit  absolute LATA.3;
  LATA_LATA2             : bit  absolute LATA.2;
  LATA_LATA1             : bit  absolute LATA.1;
  LATA_LATA0             : bit  absolute LATA.0;
  LATB                   : byte absolute $0019;
  LATB_LATB7             : bit  absolute LATB.7;
  LATB_LATB6             : bit  absolute LATB.6;
  LATB_LATB5             : bit  absolute LATB.5;
  LATB_LATB4             : bit  absolute LATB.4;
  LATB_LATB3             : bit  absolute LATB.3;
  LATB_LATB2             : bit  absolute LATB.2;
  LATB_LATB1             : bit  absolute LATB.1;
  LATB_LATB0             : bit  absolute LATB.0;
  LATC                   : byte absolute $001A;
  LATC_LATC7             : bit  absolute LATC.7;
  LATC_LATC6             : bit  absolute LATC.6;
  LATC_LATC5             : bit  absolute LATC.5;
  LATC_LATC4             : bit  absolute LATC.4;
  LATC_LATC3             : bit  absolute LATC.3;
  LATC_LATC2             : bit  absolute LATC.2;
  LATC_LATC1             : bit  absolute LATC.1;
  LATC_LATC0             : bit  absolute LATC.0;
  ADRESL                 : byte absolute $009B;
  ADRESH                 : byte absolute $009C;
  ADCON0                 : byte absolute $009D;
  ADCON0_CHS5            : bit  absolute ADCON0.7;
  ADCON0_CHS4            : bit  absolute ADCON0.6;
  ADCON0_CHS3            : bit  absolute ADCON0.5;
  ADCON0_CHS2            : bit  absolute ADCON0.4;
  ADCON0_CHS1            : bit  absolute ADCON0.3;
  ADCON0_CHS0            : bit  absolute ADCON0.2;
  ADCON0_GOnDONE         : bit  absolute ADCON0.1;
  ADCON0_ADON            : bit  absolute ADCON0.0;
  ADCON1                 : byte absolute $009E;
  ADCON1_ADFM            : bit  absolute ADCON1.7;
  ADCON1_ADCS2           : bit  absolute ADCON1.6;
  ADCON1_ADCS1           : bit  absolute ADCON1.5;
  ADCON1_ADCS0           : bit  absolute ADCON1.4;
  ADCON1_ADPREF1         : bit  absolute ADCON1.1;
  ADCON1_ADPREF0         : bit  absolute ADCON1.0;
  ADACT                  : byte absolute $009F;
  ADACT_ADACT4           : bit  absolute ADACT.4;
  ADACT_ADACT3           : bit  absolute ADACT.3;
  ADACT_ADACT2           : bit  absolute ADACT.2;
  ADACT_ADACT1           : bit  absolute ADACT.1;
  ADACT_ADACT0           : bit  absolute ADACT.0;
  RC1REG                 : byte absolute $0119;
  TX1REG                 : byte absolute $011A;
  SP1BRGL                : byte absolute $011B;
  SP1BRGH                : byte absolute $011C;
  RC1STA                 : byte absolute $011D;
  RC1STA_SPEN            : bit  absolute RC1STA.7;
  RC1STA_RX9             : bit  absolute RC1STA.6;
  RC1STA_SREN            : bit  absolute RC1STA.5;
  RC1STA_CREN            : bit  absolute RC1STA.4;
  RC1STA_ADDEN           : bit  absolute RC1STA.3;
  RC1STA_FERR            : bit  absolute RC1STA.2;
  RC1STA_OERR            : bit  absolute RC1STA.1;
  RC1STA_RX9D            : bit  absolute RC1STA.0;
  TX1STA                 : byte absolute $011E;
  TX1STA_CSRC            : bit  absolute TX1STA.7;
  TX1STA_TX9             : bit  absolute TX1STA.6;
  TX1STA_TXEN            : bit  absolute TX1STA.5;
  TX1STA_SYNC            : bit  absolute TX1STA.4;
  TX1STA_SENDB           : bit  absolute TX1STA.3;
  TX1STA_BRGH            : bit  absolute TX1STA.2;
  TX1STA_TRMT            : bit  absolute TX1STA.1;
  TX1STA_TX9D            : bit  absolute TX1STA.0;
  BAUD1CON               : byte absolute $011F;
  BAUD1CON_ABDOVF        : bit  absolute BAUD1CON.7;
  BAUD1CON_RCIDL         : bit  absolute BAUD1CON.6;
  BAUD1CON_SCKP          : bit  absolute BAUD1CON.4;
  BAUD1CON_BRG16         : bit  absolute BAUD1CON.3;
  BAUD1CON_WUE           : bit  absolute BAUD1CON.1;
  BAUD1CON_ABDEN         : bit  absolute BAUD1CON.0;
  SSP1BUF                : byte absolute $018C;
  SSP1ADD                : byte absolute $018D;
  SSP1MSK                : byte absolute $018E;
  SSP1STAT               : byte absolute $018F;
  SSP1STAT_SMP           : bit  absolute SSP1STAT.7;
  SSP1STAT_CKE           : bit  absolute SSP1STAT.6;
  SSP1STAT_D_nA          : bit  absolute SSP1STAT.5;
  SSP1STAT_P             : bit  absolute SSP1STAT.4;
  SSP1STAT_S             : bit  absolute SSP1STAT.3;
  SSP1STAT_R_nW          : bit  absolute SSP1STAT.2;
  SSP1STAT_UA            : bit  absolute SSP1STAT.1;
  SSP1STAT_BF            : bit  absolute SSP1STAT.0;
  SSP1CON1               : byte absolute $0190;
  SSP1CON1_WCOL          : bit  absolute SSP1CON1.7;
  SSP1CON1_SSPOV         : bit  absolute SSP1CON1.6;
  SSP1CON1_SSPEN         : bit  absolute SSP1CON1.5;
  SSP1CON1_CKP           : bit  absolute SSP1CON1.4;
  SSP1CON1_SSPM3         : bit  absolute SSP1CON1.3;
  SSP1CON1_SSPM2         : bit  absolute SSP1CON1.2;
  SSP1CON1_SSPM1         : bit  absolute SSP1CON1.1;
  SSP1CON1_SSPM0         : bit  absolute SSP1CON1.0;
  SSP1CON2               : byte absolute $0191;
  SSP1CON2_GCEN          : bit  absolute SSP1CON2.7;
  SSP1CON2_ACKSTAT       : bit  absolute SSP1CON2.6;
  SSP1CON2_ACKDT         : bit  absolute SSP1CON2.5;
  SSP1CON2_ACKEN         : bit  absolute SSP1CON2.4;
  SSP1CON2_RCEN          : bit  absolute SSP1CON2.3;
  SSP1CON2_PEN           : bit  absolute SSP1CON2.2;
  SSP1CON2_RSEN          : bit  absolute SSP1CON2.1;
  SSP1CON2_SEN           : bit  absolute SSP1CON2.0;
  SSP1CON3               : byte absolute $0192;
  SSP1CON3_ACKTIM        : bit  absolute SSP1CON3.7;
  SSP1CON3_PCIE          : bit  absolute SSP1CON3.6;
  SSP1CON3_SCIE          : bit  absolute SSP1CON3.5;
  SSP1CON3_BOEN          : bit  absolute SSP1CON3.4;
  SSP1CON3_SDAHT         : bit  absolute SSP1CON3.3;
  SSP1CON3_SBCDE         : bit  absolute SSP1CON3.2;
  SSP1CON3_AHEN          : bit  absolute SSP1CON3.1;
  SSP1CON3_DHEN          : bit  absolute SSP1CON3.0;
  SSP2BUF                : byte absolute $0196;
  SSP2ADD                : byte absolute $0197;
  SSP2MSK                : byte absolute $0198;
  SSP2STAT               : byte absolute $0199;
  SSP2CON1               : byte absolute $019A;
  SSP2CON2               : byte absolute $019B;
  SSP2CON3               : byte absolute $019C;
  TMR1L                  : byte absolute $020C;
  TMR1L_TMR1L7           : bit  absolute TMR1L.7;
  TMR1L_TMR1L6           : bit  absolute TMR1L.6;
  TMR1L_TMR1L5           : bit  absolute TMR1L.5;
  TMR1L_TMR1L4           : bit  absolute TMR1L.4;
  TMR1L_TMR1L3           : bit  absolute TMR1L.3;
  TMR1L_TMR1L2           : bit  absolute TMR1L.2;
  TMR1L_TMR1L1           : bit  absolute TMR1L.1;
  TMR1L_TMR1L0           : bit  absolute TMR1L.0;
  TMR1H                  : byte absolute $020D;
  TMR1H_TMR1H7           : bit  absolute TMR1H.7;
  TMR1H_TMR1H6           : bit  absolute TMR1H.6;
  TMR1H_TMR1H5           : bit  absolute TMR1H.5;
  TMR1H_TMR1H4           : bit  absolute TMR1H.4;
  TMR1H_TMR1H3           : bit  absolute TMR1H.3;
  TMR1H_TMR1H2           : bit  absolute TMR1H.2;
  TMR1H_TMR1H1           : bit  absolute TMR1H.1;
  TMR1H_TMR1H0           : bit  absolute TMR1H.0;
  T1CON                  : byte absolute $020E;
  T1CON_CKPS1            : bit  absolute T1CON.5;
  T1CON_CKPS0            : bit  absolute T1CON.4;
  T1CON_nSYNC            : bit  absolute T1CON.2;
  T1CON_RD16             : bit  absolute T1CON.1;
  T1CON_ON               : bit  absolute T1CON.0;
  T1GCON                 : byte absolute $020F;
  T1GCON_GE              : bit  absolute T1GCON.7;
  T1GCON_GPOL            : bit  absolute T1GCON.6;
  T1GCON_GTM             : bit  absolute T1GCON.5;
  T1GCON_GSPM            : bit  absolute T1GCON.4;
  T1GCON_GGO_nDONE       : bit  absolute T1GCON.3;
  T1GCON_GVAL            : bit  absolute T1GCON.2;
  T1GATE                 : byte absolute $0210;
  T1GATE_GSS4            : bit  absolute T1GATE.4;
  T1GATE_GSS3            : bit  absolute T1GATE.3;
  T1GATE_GSS2            : bit  absolute T1GATE.2;
  T1GATE_GSS1            : bit  absolute T1GATE.1;
  T1GATE_GSS0            : bit  absolute T1GATE.0;
  T1CLK                  : byte absolute $0211;
  T1CLK_CS3              : bit  absolute T1CLK.3;
  T1CLK_CS2              : bit  absolute T1CLK.2;
  T1CLK_CS1              : bit  absolute T1CLK.1;
  T1CLK_CS0              : bit  absolute T1CLK.0;
  T2TMR                  : byte absolute $028C;
  T2PR                   : byte absolute $028D;
  T2CON                  : byte absolute $028E;
  T2CON_CKPS2            : bit  absolute T2CON.6;
  T2CON_OUTPS3           : bit  absolute T2CON.3;
  T2CON_OUTPS2           : bit  absolute T2CON.2;
  T2CON_OUTPS1           : bit  absolute T2CON.1;
  T2CON_OUTPS0           : bit  absolute T2CON.0;
  T2HLT                  : byte absolute $028F;
  T2HLT_PSYNC            : bit  absolute T2HLT.7;
  T2HLT_CKPOL            : bit  absolute T2HLT.6;
  T2HLT_CKSYNC           : bit  absolute T2HLT.5;
  T2HLT_MODE4            : bit  absolute T2HLT.4;
  T2HLT_MODE3            : bit  absolute T2HLT.3;
  T2HLT_MODE2            : bit  absolute T2HLT.2;
  T2HLT_MODE1            : bit  absolute T2HLT.1;
  T2HLT_MODE0            : bit  absolute T2HLT.0;
  T2CLKCON               : byte absolute $0290;
  T2RST                  : byte absolute $0291;
  T2RST_RSEL3            : bit  absolute T2RST.3;
  T2RST_RSEL2            : bit  absolute T2RST.2;
  T2RST_RSEL1            : bit  absolute T2RST.1;
  T2RST_RSEL0            : bit  absolute T2RST.0;
  CCPR1L                 : byte absolute $030C;
  CCPR1H                 : byte absolute $030D;
  CCP1CON                : byte absolute $030E;
  CCP1CON_EN             : bit  absolute CCP1CON.7;
  CCP1CON_OUT            : bit  absolute CCP1CON.5;
  CCP1CON_FMT            : bit  absolute CCP1CON.4;
  CCP1CAP                : byte absolute $030F;
  CCPR2L                 : byte absolute $0310;
  CCPR2H                 : byte absolute $0311;
  CCP2CON                : byte absolute $0312;
  CCP2CAP                : byte absolute $0313;
  PWM3DCL                : byte absolute $0314;
  PWM3DCL_DC1            : bit  absolute PWM3DCL.7;
  PWM3DCL_DC0            : bit  absolute PWM3DCL.6;
  PWM3DCH                : byte absolute $0315;
  PWM3CON                : byte absolute $0316;
  PWM3CON_POL            : bit  absolute PWM3CON.4;
  PWM4DCL                : byte absolute $0318;
  PWM4DCH                : byte absolute $0319;
  PWM4CON                : byte absolute $031A;
  PWM5DCL                : byte absolute $031C;
  PWM5DCH                : byte absolute $031D;
  PWM5CON                : byte absolute $031E;
  PWM6DCL                : byte absolute $038C;
  PWM6DCH                : byte absolute $038D;
  PWM6CON                : byte absolute $038E;
  NCO1ACCL               : byte absolute $058C;
  NCO1ACCL_NCO1ACC7      : bit  absolute NCO1ACCL.7;
  NCO1ACCL_NCO1ACC6      : bit  absolute NCO1ACCL.6;
  NCO1ACCL_NCO1ACC5      : bit  absolute NCO1ACCL.5;
  NCO1ACCL_NCO1ACC4      : bit  absolute NCO1ACCL.4;
  NCO1ACCL_NCO1ACC3      : bit  absolute NCO1ACCL.3;
  NCO1ACCL_NCO1ACC2      : bit  absolute NCO1ACCL.2;
  NCO1ACCL_NCO1ACC1      : bit  absolute NCO1ACCL.1;
  NCO1ACCL_NCO1ACC0      : bit  absolute NCO1ACCL.0;
  NCO1ACCH               : byte absolute $058D;
  NCO1ACCH_NCO1ACC15     : bit  absolute NCO1ACCH.7;
  NCO1ACCH_NCO1ACC14     : bit  absolute NCO1ACCH.6;
  NCO1ACCH_NCO1ACC13     : bit  absolute NCO1ACCH.5;
  NCO1ACCH_NCO1ACC12     : bit  absolute NCO1ACCH.4;
  NCO1ACCH_NCO1ACC11     : bit  absolute NCO1ACCH.3;
  NCO1ACCH_NCO1ACC10     : bit  absolute NCO1ACCH.2;
  NCO1ACCH_NCO1ACC9      : bit  absolute NCO1ACCH.1;
  NCO1ACCH_NCO1ACC8      : bit  absolute NCO1ACCH.0;
  NCO1ACCU               : byte absolute $058E;
  NCO1ACCU_NCO1ACC19     : bit  absolute NCO1ACCU.3;
  NCO1ACCU_NCO1ACC18     : bit  absolute NCO1ACCU.2;
  NCO1ACCU_NCO1ACC17     : bit  absolute NCO1ACCU.1;
  NCO1ACCU_NCO1ACC16     : bit  absolute NCO1ACCU.0;
  NCO1INCL               : byte absolute $058F;
  NCO1INCL_NCO1INC7      : bit  absolute NCO1INCL.7;
  NCO1INCL_NCO1INC6      : bit  absolute NCO1INCL.6;
  NCO1INCL_NCO1INC5      : bit  absolute NCO1INCL.5;
  NCO1INCL_NCO1INC4      : bit  absolute NCO1INCL.4;
  NCO1INCL_NCO1INC3      : bit  absolute NCO1INCL.3;
  NCO1INCL_NCO1INC2      : bit  absolute NCO1INCL.2;
  NCO1INCL_NCO1INC1      : bit  absolute NCO1INCL.1;
  NCO1INCL_NCO1INC0      : bit  absolute NCO1INCL.0;
  NCO1INCH               : byte absolute $0590;
  NCO1INCH_NCO1INC15     : bit  absolute NCO1INCH.7;
  NCO1INCH_NCO1INC14     : bit  absolute NCO1INCH.6;
  NCO1INCH_NCO1INC13     : bit  absolute NCO1INCH.5;
  NCO1INCH_NCO1INC12     : bit  absolute NCO1INCH.4;
  NCO1INCH_NCO1INC11     : bit  absolute NCO1INCH.3;
  NCO1INCH_NCO1INC10     : bit  absolute NCO1INCH.2;
  NCO1INCH_NCO1INC9      : bit  absolute NCO1INCH.1;
  NCO1INCH_NCO1INC8      : bit  absolute NCO1INCH.0;
  NCO1INCU               : byte absolute $0591;
  NCO1INCU_NCO1INC19     : bit  absolute NCO1INCU.3;
  NCO1INCU_NCO1INC18     : bit  absolute NCO1INCU.2;
  NCO1INCU_NCO1INC17     : bit  absolute NCO1INCU.1;
  NCO1INCU_NCO1INC16     : bit  absolute NCO1INCU.0;
  NCO1CON                : byte absolute $0592;
  NCO1CON_N1EN           : bit  absolute NCO1CON.7;
  NCO1CON_N1OUT          : bit  absolute NCO1CON.5;
  NCO1CON_N1POL          : bit  absolute NCO1CON.4;
  NCO1CON_N1PFM          : bit  absolute NCO1CON.0;
  NCO1CLK                : byte absolute $0593;
  NCO1CLK_N1PWS2         : bit  absolute NCO1CLK.7;
  NCO1CLK_N1PWS1         : bit  absolute NCO1CLK.6;
  NCO1CLK_N1PWS0         : bit  absolute NCO1CLK.5;
  NCO1CLK_N1CKS2         : bit  absolute NCO1CLK.2;
  NCO1CLK_N1CKS1         : bit  absolute NCO1CLK.1;
  NCO1CLK_N1CKS0         : bit  absolute NCO1CLK.0;
  TMR0L                  : byte absolute $059C;
  TMR0H                  : byte absolute $059D;
  T0CON0                 : byte absolute $059E;
  T0CON0_T0EN            : bit  absolute T0CON0.7;
  T0CON0_T0OUT           : bit  absolute T0CON0.5;
  T0CON0_T016BIT         : bit  absolute T0CON0.4;
  T0CON0_T0OUTPS3        : bit  absolute T0CON0.3;
  T0CON0_T0OUTPS2        : bit  absolute T0CON0.2;
  T0CON0_T0OUTPS1        : bit  absolute T0CON0.1;
  T0CON0_T0OUTPS0        : bit  absolute T0CON0.0;
  T0CON1                 : byte absolute $059F;
  T0CON1_T0CS2           : bit  absolute T0CON1.7;
  T0CON1_T0CS1           : bit  absolute T0CON1.6;
  T0CON1_T0CS0           : bit  absolute T0CON1.5;
  T0CON1_T0ASYNC         : bit  absolute T0CON1.4;
  T0CON1_T0CKPS3         : bit  absolute T0CON1.3;
  T0CON1_T0CKPS2         : bit  absolute T0CON1.2;
  T0CON1_T0CKPS1         : bit  absolute T0CON1.1;
  T0CON1_T0CKPS0         : bit  absolute T0CON1.0;
  CWG1CLKCON             : byte absolute $060C;
  CWG1CLKCON_CS          : bit  absolute CWG1CLKCON.0;
  CWG1DAT                : byte absolute $060D;
  CWG1DAT_DAT3           : bit  absolute CWG1DAT.3;
  CWG1DAT_DAT2           : bit  absolute CWG1DAT.2;
  CWG1DAT_DAT1           : bit  absolute CWG1DAT.1;
  CWG1DAT_DAT0           : bit  absolute CWG1DAT.0;
  CWG1DBR                : byte absolute $060E;
  CWG1DBR_DBR5           : bit  absolute CWG1DBR.5;
  CWG1DBR_DBR4           : bit  absolute CWG1DBR.4;
  CWG1DBR_DBR3           : bit  absolute CWG1DBR.3;
  CWG1DBR_DBR2           : bit  absolute CWG1DBR.2;
  CWG1DBR_DBR1           : bit  absolute CWG1DBR.1;
  CWG1DBR_DBR0           : bit  absolute CWG1DBR.0;
  CWG1DBF                : byte absolute $060F;
  CWG1DBF_DBF5           : bit  absolute CWG1DBF.5;
  CWG1DBF_DBF4           : bit  absolute CWG1DBF.4;
  CWG1DBF_DBF3           : bit  absolute CWG1DBF.3;
  CWG1DBF_DBF2           : bit  absolute CWG1DBF.2;
  CWG1DBF_DBF1           : bit  absolute CWG1DBF.1;
  CWG1DBF_DBF0           : bit  absolute CWG1DBF.0;
  CWG1CON0               : byte absolute $0610;
  CWG1CON0_LD            : bit  absolute CWG1CON0.6;
  CWG1CON1               : byte absolute $0611;
  CWG1CON1_IN            : bit  absolute CWG1CON1.5;
  CWG1CON1_POLD          : bit  absolute CWG1CON1.3;
  CWG1CON1_POLC          : bit  absolute CWG1CON1.2;
  CWG1CON1_POLB          : bit  absolute CWG1CON1.1;
  CWG1CON1_POLA          : bit  absolute CWG1CON1.0;
  CWG1AS0                : byte absolute $0612;
  CWG1AS0_SHUTDOWN       : bit  absolute CWG1AS0.7;
  CWG1AS0_REN            : bit  absolute CWG1AS0.6;
  CWG1AS0_LSBD1          : bit  absolute CWG1AS0.5;
  CWG1AS0_LSBD0          : bit  absolute CWG1AS0.4;
  CWG1AS0_LSAC1          : bit  absolute CWG1AS0.3;
  CWG1AS0_LSAC0          : bit  absolute CWG1AS0.2;
  CWG1AS1                : byte absolute $0613;
  CWG1AS1_AS4E           : bit  absolute CWG1AS1.4;
  CWG1AS1_AS3E           : bit  absolute CWG1AS1.3;
  CWG1AS1_AS2E           : bit  absolute CWG1AS1.2;
  CWG1AS1_AS1E           : bit  absolute CWG1AS1.1;
  CWG1AS1_AS0E           : bit  absolute CWG1AS1.0;
  CWG1STR                : byte absolute $0614;
  CWG1STR_OVRD           : bit  absolute CWG1STR.7;
  CWG1STR_OVRC           : bit  absolute CWG1STR.6;
  CWG1STR_OVRB           : bit  absolute CWG1STR.5;
  CWG1STR_OVRA           : bit  absolute CWG1STR.4;
  CWG1STR_STRD           : bit  absolute CWG1STR.3;
  CWG1STR_STRC           : bit  absolute CWG1STR.2;
  CWG1STR_STRB           : bit  absolute CWG1STR.1;
  CWG1STR_STRA           : bit  absolute CWG1STR.0;
  PIR0                   : byte absolute $070C;
  PIR0_TMR0IF            : bit  absolute PIR0.5;
  PIR0_IOCIF             : bit  absolute PIR0.4;
  PIR0_INTF              : bit  absolute PIR0.0;
  PIR1                   : byte absolute $070D;
  PIR1_OSFIF             : bit  absolute PIR1.7;
  PIR1_CSWIF             : bit  absolute PIR1.6;
  PIR1_ADTIF             : bit  absolute PIR1.1;
  PIR1_ADIF              : bit  absolute PIR1.0;
  PIR2                   : byte absolute $070E;
  PIR2_ZCDIF             : bit  absolute PIR2.6;
  PIR2_C2IF              : bit  absolute PIR2.1;
  PIR2_C1IF              : bit  absolute PIR2.0;
  PIR3                   : byte absolute $070F;
  PIR3_RC2IF             : bit  absolute PIR3.7;
  PIR3_TX2IF             : bit  absolute PIR3.6;
  PIR3_RC1IF             : bit  absolute PIR3.5;
  PIR3_TX1IF             : bit  absolute PIR3.4;
  PIR3_BCL2IF            : bit  absolute PIR3.3;
  PIR3_SSP2IF            : bit  absolute PIR3.2;
  PIR3_BCL1IF            : bit  absolute PIR3.1;
  PIR3_SSP1IF            : bit  absolute PIR3.0;
  PIR4                   : byte absolute $0710;
  PIR4_TMR2IF            : bit  absolute PIR4.1;
  PIR4_TMR1IF            : bit  absolute PIR4.0;
  PIR5                   : byte absolute $0711;
  PIR5_CLC4IF            : bit  absolute PIR5.7;
  PIR5_CLC3IF            : bit  absolute PIR5.6;
  PIR5_CLC2IF            : bit  absolute PIR5.5;
  PIR5_CLC1IF            : bit  absolute PIR5.4;
  PIR5_TMR1GIF           : bit  absolute PIR5.0;
  PIR6                   : byte absolute $0712;
  PIR6_CCP2IF            : bit  absolute PIR6.1;
  PIR6_CCP1IF            : bit  absolute PIR6.0;
  PIR7                   : byte absolute $0713;
  PIR7_NVMIF             : bit  absolute PIR7.5;
  PIR7_NCO1IF            : bit  absolute PIR7.4;
  PIR7_CWG1IF            : bit  absolute PIR7.0;
  PIE0                   : byte absolute $0716;
  PIE0_TMR0IE            : bit  absolute PIE0.5;
  PIE0_IOCIE             : bit  absolute PIE0.4;
  PIE0_INTE              : bit  absolute PIE0.0;
  PIE1                   : byte absolute $0717;
  PIE1_OSFIE             : bit  absolute PIE1.7;
  PIE1_CSWIE             : bit  absolute PIE1.6;
  PIE1_ADTIE             : bit  absolute PIE1.1;
  PIE1_ADIE              : bit  absolute PIE1.0;
  PIE2                   : byte absolute $0718;
  PIE2_ZCDIE             : bit  absolute PIE2.6;
  PIE2_C2IE              : bit  absolute PIE2.1;
  PIE2_C1IE              : bit  absolute PIE2.0;
  PIE3                   : byte absolute $0719;
  PIE3_RC2IE             : bit  absolute PIE3.7;
  PIE3_TX2IE             : bit  absolute PIE3.6;
  PIE3_RC1IE             : bit  absolute PIE3.5;
  PIE3_TX1IE             : bit  absolute PIE3.4;
  PIE3_BCL2IE            : bit  absolute PIE3.3;
  PIE3_SSP2IE            : bit  absolute PIE3.2;
  PIE3_BCL1IE            : bit  absolute PIE3.1;
  PIE3_SSP1IE            : bit  absolute PIE3.0;
  PIE4                   : byte absolute $071A;
  PIE4_TMR2IE            : bit  absolute PIE4.1;
  PIE4_TMR1IE            : bit  absolute PIE4.0;
  PIE5                   : byte absolute $071B;
  PIE5_CLC4IE            : bit  absolute PIE5.7;
  PIE5_CLC3IE            : bit  absolute PIE5.6;
  PIE5_CLC2IE            : bit  absolute PIE5.5;
  PIE5_CLC1IE            : bit  absolute PIE5.4;
  PIE5_TMR1GIE           : bit  absolute PIE5.0;
  PIE6                   : byte absolute $071C;
  PIE6_CCP2IE            : bit  absolute PIE6.1;
  PIE6_CCP1IE            : bit  absolute PIE6.0;
  PIE7                   : byte absolute $071D;
  PIE7_NVMIE             : bit  absolute PIE7.5;
  PIE7_NCO1IE            : bit  absolute PIE7.4;
  PIE7_CWG1IE            : bit  absolute PIE7.0;
  PMD0                   : byte absolute $0796;
  PMD0_SYSCMD            : bit  absolute PMD0.7;
  PMD0_FVRMD             : bit  absolute PMD0.6;
  PMD0_NVMMD             : bit  absolute PMD0.2;
  PMD0_CLKRMD            : bit  absolute PMD0.1;
  PMD0_IOCMD             : bit  absolute PMD0.0;
  PMD1                   : byte absolute $0797;
  PMD1_NCO1MD            : bit  absolute PMD1.7;
  PMD1_TMR2MD            : bit  absolute PMD1.2;
  PMD1_TMR1MD            : bit  absolute PMD1.1;
  PMD1_TMR0MD            : bit  absolute PMD1.0;
  PMD2                   : byte absolute $0798;
  PMD2_DAC1MD            : bit  absolute PMD2.6;
  PMD2_ADCMD             : bit  absolute PMD2.5;
  PMD2_CMP2MD            : bit  absolute PMD2.2;
  PMD2_CMP1MD            : bit  absolute PMD2.1;
  PMD2_ZCDMD             : bit  absolute PMD2.0;
  PMD3                   : byte absolute $0799;
  PMD3_PWM6MD            : bit  absolute PMD3.5;
  PMD3_PWM5MD            : bit  absolute PMD3.4;
  PMD3_PWM4MD            : bit  absolute PMD3.3;
  PMD3_PWM3MD            : bit  absolute PMD3.2;
  PMD3_CCP2MD            : bit  absolute PMD3.1;
  PMD3_CCP1MD            : bit  absolute PMD3.0;
  PMD4                   : byte absolute $079A;
  PMD4_UART2MD           : bit  absolute PMD4.7;
  PMD4_UART1MD           : bit  absolute PMD4.6;
  PMD4_MSSP2MD           : bit  absolute PMD4.5;
  PMD4_MSSP1MD           : bit  absolute PMD4.4;
  PMD4_CWG1MD            : bit  absolute PMD4.0;
  PMD5                   : byte absolute $079B;
  PMD5_CLC4MD            : bit  absolute PMD5.4;
  PMD5_CLC3MD            : bit  absolute PMD5.3;
  PMD5_CLC2MD            : bit  absolute PMD5.2;
  PMD5_CLC1MD            : bit  absolute PMD5.1;
  WDTCON0                : byte absolute $080C;
  WDTCON0_WDTPS4         : bit  absolute WDTCON0.5;
  WDTCON0_WDTPS3         : bit  absolute WDTCON0.4;
  WDTCON0_WDTPS2         : bit  absolute WDTCON0.3;
  WDTCON0_WDTPS1         : bit  absolute WDTCON0.2;
  WDTCON0_WDTPS0         : bit  absolute WDTCON0.1;
  WDTCON1                : byte absolute $080D;
  WDTCON1_WDTCS2         : bit  absolute WDTCON1.6;
  WDTCON1_WDTCS1         : bit  absolute WDTCON1.5;
  WDTCON1_WDTCS0         : bit  absolute WDTCON1.4;
  WDTCON1_WINDOW2        : bit  absolute WDTCON1.2;
  WDTCON1_WINDOW1        : bit  absolute WDTCON1.1;
  WDTCON1_WINDOW0        : bit  absolute WDTCON1.0;
  WDTPSL                 : byte absolute $080E;
  WDTPSH                 : byte absolute $080F;
  WDTTMR                 : byte absolute $0810;
  WDTTMR_WDTTMR4         : bit  absolute WDTTMR.7;
  WDTTMR_WDTTMR3         : bit  absolute WDTTMR.6;
  WDTTMR_WDTTMR2         : bit  absolute WDTTMR.5;
  WDTTMR_WDTTMR1         : bit  absolute WDTTMR.4;
  WDTTMR_WDTTMR0         : bit  absolute WDTTMR.3;
  WDTTMR_STATE           : bit  absolute WDTTMR.2;
  WDTTMR_PSCNT17         : bit  absolute WDTTMR.1;
  WDTTMR_PSCNT16         : bit  absolute WDTTMR.0;
  BORCON                 : byte absolute $0811;
  BORCON_SBOREN          : bit  absolute BORCON.7;
  BORCON_BORRDY          : bit  absolute BORCON.0;
  VREGCON                : byte absolute $0812;
  VREGCON_VREGPM         : bit  absolute VREGCON.1;
  PCON0                  : byte absolute $0813;
  PCON0_STKOVF           : bit  absolute PCON0.7;
  PCON0_STKUNF           : bit  absolute PCON0.6;
  PCON0_nWDTWV           : bit  absolute PCON0.5;
  PCON0_nRWDT            : bit  absolute PCON0.4;
  PCON0_nRMCLR           : bit  absolute PCON0.3;
  PCON0_nRI              : bit  absolute PCON0.2;
  PCON0_nPOR             : bit  absolute PCON0.1;
  PCON0_nBOR             : bit  absolute PCON0.0;
  PCON1                  : byte absolute $0814;
  PCON1_nMEMV            : bit  absolute PCON1.1;
  NVMADRL                : byte absolute $081A;
  NVMADRL_NVMADRL7       : bit  absolute NVMADRL.7;
  NVMADRL_NVMADRL6       : bit  absolute NVMADRL.6;
  NVMADRL_NVMADRL5       : bit  absolute NVMADRL.5;
  NVMADRL_NVMADRL4       : bit  absolute NVMADRL.4;
  NVMADRL_NVMADRL3       : bit  absolute NVMADRL.3;
  NVMADRL_NVMADRL2       : bit  absolute NVMADRL.2;
  NVMADRL_NVMADRL1       : bit  absolute NVMADRL.1;
  NVMADRL_NVMADRL0       : bit  absolute NVMADRL.0;
  NVMADRH                : byte absolute $081B;
  NVMADRH_NVMADRH6       : bit  absolute NVMADRH.6;
  NVMADRH_NVMADRH5       : bit  absolute NVMADRH.5;
  NVMADRH_NVMADRH4       : bit  absolute NVMADRH.4;
  NVMADRH_NVMADRH3       : bit  absolute NVMADRH.3;
  NVMADRH_NVMADRH2       : bit  absolute NVMADRH.2;
  NVMADRH_NVMADRH1       : bit  absolute NVMADRH.1;
  NVMADRH_NVMADRH0       : bit  absolute NVMADRH.0;
  NVMDATL                : byte absolute $081C;
  NVMDATL_NVMDATL7       : bit  absolute NVMDATL.7;
  NVMDATL_NVMDATL6       : bit  absolute NVMDATL.6;
  NVMDATL_NVMDATL5       : bit  absolute NVMDATL.5;
  NVMDATL_NVMDATL4       : bit  absolute NVMDATL.4;
  NVMDATL_NVMDATL3       : bit  absolute NVMDATL.3;
  NVMDATL_NVMDATL2       : bit  absolute NVMDATL.2;
  NVMDATL_NVMDATL1       : bit  absolute NVMDATL.1;
  NVMDATL_NVMDATL0       : bit  absolute NVMDATL.0;
  NVMDATH                : byte absolute $081D;
  NVMDATH_NVMDATH5       : bit  absolute NVMDATH.5;
  NVMDATH_NVMDATH4       : bit  absolute NVMDATH.4;
  NVMDATH_NVMDATH3       : bit  absolute NVMDATH.3;
  NVMDATH_NVMDATH2       : bit  absolute NVMDATH.2;
  NVMDATH_NVMDATH1       : bit  absolute NVMDATH.1;
  NVMDATH_NVMDATH0       : bit  absolute NVMDATH.0;
  NVMCON1                : byte absolute $081E;
  NVMCON1_NVMREGS        : bit  absolute NVMCON1.6;
  NVMCON1_LWLO           : bit  absolute NVMCON1.5;
  NVMCON1_FREE           : bit  absolute NVMCON1.4;
  NVMCON1_WRERR          : bit  absolute NVMCON1.3;
  NVMCON1_WREN           : bit  absolute NVMCON1.2;
  NVMCON1_WR             : bit  absolute NVMCON1.1;
  NVMCON1_RD             : bit  absolute NVMCON1.0;
  NVMCON2                : byte absolute $081F;
  CPUDOZE                : byte absolute $088C;
  CPUDOZE_IDLEN          : bit  absolute CPUDOZE.7;
  CPUDOZE_DOZEN          : bit  absolute CPUDOZE.6;
  CPUDOZE_ROI            : bit  absolute CPUDOZE.5;
  CPUDOZE_DOE            : bit  absolute CPUDOZE.4;
  CPUDOZE_DOZE2          : bit  absolute CPUDOZE.2;
  CPUDOZE_DOZE1          : bit  absolute CPUDOZE.1;
  CPUDOZE_DOZE0          : bit  absolute CPUDOZE.0;
  OSCCON1                : byte absolute $088D;
  OSCCON1_NOSC2          : bit  absolute OSCCON1.6;
  OSCCON1_NOSC1          : bit  absolute OSCCON1.5;
  OSCCON1_NOSC0          : bit  absolute OSCCON1.4;
  OSCCON1_NDIV3          : bit  absolute OSCCON1.3;
  OSCCON1_NDIV2          : bit  absolute OSCCON1.2;
  OSCCON1_NDIV1          : bit  absolute OSCCON1.1;
  OSCCON1_NDIV0          : bit  absolute OSCCON1.0;
  OSCCON2                : byte absolute $088E;
  OSCCON2_COSC2          : bit  absolute OSCCON2.6;
  OSCCON2_COSC1          : bit  absolute OSCCON2.5;
  OSCCON2_COSC0          : bit  absolute OSCCON2.4;
  OSCCON2_CDIV3          : bit  absolute OSCCON2.3;
  OSCCON2_CDIV2          : bit  absolute OSCCON2.2;
  OSCCON2_CDIV1          : bit  absolute OSCCON2.1;
  OSCCON2_CDIV0          : bit  absolute OSCCON2.0;
  OSCCON3                : byte absolute $088F;
  OSCCON3_CSWHOLD        : bit  absolute OSCCON3.7;
  OSCCON3_SOSCPWR        : bit  absolute OSCCON3.6;
  OSCCON3_ORDY           : bit  absolute OSCCON3.4;
  OSCCON3_NOSCR          : bit  absolute OSCCON3.3;
  OSCSTAT                : byte absolute $0890;
  OSCSTAT_EXTOR          : bit  absolute OSCSTAT.7;
  OSCSTAT_HFOR           : bit  absolute OSCSTAT.6;
  OSCSTAT_MFOR           : bit  absolute OSCSTAT.5;
  OSCSTAT_LFOR           : bit  absolute OSCSTAT.4;
  OSCSTAT_SOR            : bit  absolute OSCSTAT.3;
  OSCSTAT_ADOR           : bit  absolute OSCSTAT.2;
  OSCSTAT_PLLR           : bit  absolute OSCSTAT.0;
  OSCEN                  : byte absolute $0891;
  OSCEN_EXTOEN           : bit  absolute OSCEN.7;
  OSCEN_HFOEN            : bit  absolute OSCEN.6;
  OSCEN_MFOEN            : bit  absolute OSCEN.5;
  OSCEN_LFOEN            : bit  absolute OSCEN.4;
  OSCEN_SOSCEN           : bit  absolute OSCEN.3;
  OSCEN_ADOEN            : bit  absolute OSCEN.2;
  OSCTUNE                : byte absolute $0892;
  OSCTUNE_HFTUN5         : bit  absolute OSCTUNE.5;
  OSCTUNE_HFTUN4         : bit  absolute OSCTUNE.4;
  OSCTUNE_HFTUN3         : bit  absolute OSCTUNE.3;
  OSCTUNE_HFTUN2         : bit  absolute OSCTUNE.2;
  OSCTUNE_HFTUN1         : bit  absolute OSCTUNE.1;
  OSCTUNE_HFTUN0         : bit  absolute OSCTUNE.0;
  OSCFRQ                 : byte absolute $0893;
  OSCFRQ_HFFRQ2          : bit  absolute OSCFRQ.2;
  OSCFRQ_HFFRQ1          : bit  absolute OSCFRQ.1;
  OSCFRQ_HFFRQ0          : bit  absolute OSCFRQ.0;
  CLKRCON                : byte absolute $0895;
  CLKRCON_CLKREN         : bit  absolute CLKRCON.7;
  CLKRCON_CLKRDC1        : bit  absolute CLKRCON.4;
  CLKRCON_CLKRDC0        : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2       : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1       : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0       : bit  absolute CLKRCON.0;
  CLKRCLK                : byte absolute $0896;
  CLKRCLK_CLKRCLK3       : bit  absolute CLKRCLK.3;
  CLKRCLK_CLKRCLK2       : bit  absolute CLKRCLK.2;
  CLKRCLK_CLKRCLK1       : bit  absolute CLKRCLK.1;
  CLKRCLK_CLKRCLK0       : bit  absolute CLKRCLK.0;
  FVRCON                 : byte absolute $090C;
  FVRCON_FVREN           : bit  absolute FVRCON.7;
  FVRCON_FVRRDY          : bit  absolute FVRCON.6;
  FVRCON_TSEN            : bit  absolute FVRCON.5;
  FVRCON_TSRNG           : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1         : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0         : bit  absolute FVRCON.2;
  FVRCON_ADFVR1          : bit  absolute FVRCON.1;
  FVRCON_ADFVR0          : bit  absolute FVRCON.0;
  DAC1CON0               : byte absolute $090E;
  DAC1CON0_OE1           : bit  absolute DAC1CON0.5;
  DAC1CON0_OE2           : bit  absolute DAC1CON0.4;
  DAC1CON0_PSS1          : bit  absolute DAC1CON0.3;
  DAC1CON0_PSS0          : bit  absolute DAC1CON0.2;
  DAC1CON0_NSS           : bit  absolute DAC1CON0.0;
  DAC1CON1               : byte absolute $090F;
  DAC1CON1_DAC1R4        : bit  absolute DAC1CON1.4;
  DAC1CON1_DAC1R3        : bit  absolute DAC1CON1.3;
  DAC1CON1_DAC1R2        : bit  absolute DAC1CON1.2;
  DAC1CON1_DAC1R1        : bit  absolute DAC1CON1.1;
  DAC1CON1_DAC1R0        : bit  absolute DAC1CON1.0;
  ZCDCON                 : byte absolute $091F;
  ZCDCON_ZCDSEN          : bit  absolute ZCDCON.7;
  ZCDCON_ZCDOUT          : bit  absolute ZCDCON.5;
  ZCDCON_ZCDPOL          : bit  absolute ZCDCON.4;
  ZCDCON_ZCDINTP         : bit  absolute ZCDCON.1;
  ZCDCON_ZCDINTN         : bit  absolute ZCDCON.0;
  CMOUT                  : byte absolute $098F;
  CMOUT_MC2OUT           : bit  absolute CMOUT.1;
  CMOUT_MC1OUT           : bit  absolute CMOUT.0;
  CM1CON0                : byte absolute $0990;
  CM1CON0_HYS            : bit  absolute CM1CON0.1;
  CM1CON1                : byte absolute $0991;
  CM1CON1_INTP           : bit  absolute CM1CON1.1;
  CM1CON1_INTN           : bit  absolute CM1CON1.0;
  CM1NCH                 : byte absolute $0992;
  CM1NCH_NCH2            : bit  absolute CM1NCH.2;
  CM1NCH_NCH1            : bit  absolute CM1NCH.1;
  CM1NCH_NCH0            : bit  absolute CM1NCH.0;
  CM1PCH                 : byte absolute $0993;
  CM1PCH_PCH2            : bit  absolute CM1PCH.2;
  CM1PCH_PCH1            : bit  absolute CM1PCH.1;
  CM1PCH_PCH0            : bit  absolute CM1PCH.0;
  CM2CON0                : byte absolute $0994;
  CM2CON1                : byte absolute $0995;
  CM2NCH                 : byte absolute $0996;
  CM2PCH                 : byte absolute $0997;
  RC2REG                 : byte absolute $0A19;
  TX2REG                 : byte absolute $0A1A;
  SP2BRGL                : byte absolute $0A1B;
  SP2BRGH                : byte absolute $0A1C;
  RC2STA                 : byte absolute $0A1D;
  TX2STA                 : byte absolute $0A1E;
  BAUD2CON               : byte absolute $0A1F;
  CLCDATA                : byte absolute $1E0F;
  CLCDATA_MLC4OUT        : bit  absolute CLCDATA.3;
  CLCDATA_MLC3OUT        : bit  absolute CLCDATA.2;
  CLCDATA_MLC2OUT        : bit  absolute CLCDATA.1;
  CLCDATA_MLC1OUT        : bit  absolute CLCDATA.0;
  CLC1CON                : byte absolute $1E10;
  CLC1CON_LC1EN          : bit  absolute CLC1CON.7;
  CLC1CON_LC1OUT         : bit  absolute CLC1CON.5;
  CLC1CON_LC1INTP        : bit  absolute CLC1CON.4;
  CLC1CON_LC1INTN        : bit  absolute CLC1CON.3;
  CLC1CON_LC1MODE2       : bit  absolute CLC1CON.2;
  CLC1CON_LC1MODE1       : bit  absolute CLC1CON.1;
  CLC1CON_LC1MODE0       : bit  absolute CLC1CON.0;
  CLC1POL                : byte absolute $1E11;
  CLC1POL_LC1POL         : bit  absolute CLC1POL.7;
  CLC1POL_LC1G4POL       : bit  absolute CLC1POL.3;
  CLC1POL_LC1G3POL       : bit  absolute CLC1POL.2;
  CLC1POL_LC1G2POL       : bit  absolute CLC1POL.1;
  CLC1POL_LC1G1POL       : bit  absolute CLC1POL.0;
  CLC1SEL0               : byte absolute $1E12;
  CLC1SEL0_LC1D1S5       : bit  absolute CLC1SEL0.5;
  CLC1SEL0_LC1D1S4       : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S3       : bit  absolute CLC1SEL0.3;
  CLC1SEL0_LC1D1S2       : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1       : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0       : bit  absolute CLC1SEL0.0;
  CLC1SEL1               : byte absolute $1E13;
  CLC1SEL1_LC1D2S5       : bit  absolute CLC1SEL1.5;
  CLC1SEL1_LC1D2S4       : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D2S3       : bit  absolute CLC1SEL1.3;
  CLC1SEL1_LC1D2S2       : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D2S1       : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D2S0       : bit  absolute CLC1SEL1.0;
  CLC1SEL2               : byte absolute $1E14;
  CLC1SEL2_LC1D3S5       : bit  absolute CLC1SEL2.5;
  CLC1SEL2_LC1D3S4       : bit  absolute CLC1SEL2.4;
  CLC1SEL2_LC1D3S3       : bit  absolute CLC1SEL2.3;
  CLC1SEL2_LC1D3S2       : bit  absolute CLC1SEL2.2;
  CLC1SEL2_LC1D3S1       : bit  absolute CLC1SEL2.1;
  CLC1SEL2_LC1D3S0       : bit  absolute CLC1SEL2.0;
  CLC1SEL3               : byte absolute $1E15;
  CLC1SEL3_LC1D4S5       : bit  absolute CLC1SEL3.5;
  CLC1SEL3_LC1D4S4       : bit  absolute CLC1SEL3.4;
  CLC1SEL3_LC1D4S3       : bit  absolute CLC1SEL3.3;
  CLC1SEL3_LC1D4S2       : bit  absolute CLC1SEL3.2;
  CLC1SEL3_LC1D4S1       : bit  absolute CLC1SEL3.1;
  CLC1SEL3_LC1D4S0       : bit  absolute CLC1SEL3.0;
  CLC1GLS0               : byte absolute $1E16;
  CLC1GLS0_LC1G1D4T      : bit  absolute CLC1GLS0.7;
  CLC1GLS0_LC1G1D4N      : bit  absolute CLC1GLS0.6;
  CLC1GLS0_LC1G1D3T      : bit  absolute CLC1GLS0.5;
  CLC1GLS0_LC1G1D3N      : bit  absolute CLC1GLS0.4;
  CLC1GLS0_LC1G1D2T      : bit  absolute CLC1GLS0.3;
  CLC1GLS0_LC1G1D2N      : bit  absolute CLC1GLS0.2;
  CLC1GLS0_LC1G1D1T      : bit  absolute CLC1GLS0.1;
  CLC1GLS0_LC1G1D1N      : bit  absolute CLC1GLS0.0;
  CLC1GLS1               : byte absolute $1E17;
  CLC1GLS1_LC1G2D4T      : bit  absolute CLC1GLS1.7;
  CLC1GLS1_LC1G2D4N      : bit  absolute CLC1GLS1.6;
  CLC1GLS1_LC1G2D3T      : bit  absolute CLC1GLS1.5;
  CLC1GLS1_LC1G2D3N      : bit  absolute CLC1GLS1.4;
  CLC1GLS1_LC1G2D2T      : bit  absolute CLC1GLS1.3;
  CLC1GLS1_LC1G2D2N      : bit  absolute CLC1GLS1.2;
  CLC1GLS1_LC1G2D1T      : bit  absolute CLC1GLS1.1;
  CLC1GLS1_LC1G2D1N      : bit  absolute CLC1GLS1.0;
  CLC1GLS2               : byte absolute $1E18;
  CLC1GLS2_LC1G3D4T      : bit  absolute CLC1GLS2.7;
  CLC1GLS2_LC1G3D4N      : bit  absolute CLC1GLS2.6;
  CLC1GLS2_LC1G3D3T      : bit  absolute CLC1GLS2.5;
  CLC1GLS2_LC1G3D3N      : bit  absolute CLC1GLS2.4;
  CLC1GLS2_LC1G3D2T      : bit  absolute CLC1GLS2.3;
  CLC1GLS2_LC1G3D2N      : bit  absolute CLC1GLS2.2;
  CLC1GLS2_LC1G3D1T      : bit  absolute CLC1GLS2.1;
  CLC1GLS2_LC1G3D1N      : bit  absolute CLC1GLS2.0;
  CLC1GLS3               : byte absolute $1E19;
  CLC1GLS3_LC1G4D4T      : bit  absolute CLC1GLS3.7;
  CLC1GLS3_LC1G4D4N      : bit  absolute CLC1GLS3.6;
  CLC1GLS3_LC1G4D3T      : bit  absolute CLC1GLS3.5;
  CLC1GLS3_LC1G4D3N      : bit  absolute CLC1GLS3.4;
  CLC1GLS3_LC1G4D2T      : bit  absolute CLC1GLS3.3;
  CLC1GLS3_LC1G4D2N      : bit  absolute CLC1GLS3.2;
  CLC1GLS3_LC1G4D1T      : bit  absolute CLC1GLS3.1;
  CLC1GLS3_LC1G4D1N      : bit  absolute CLC1GLS3.0;
  CLC2CON                : byte absolute $1E1A;
  CLC2CON_LC2EN          : bit  absolute CLC2CON.7;
  CLC2CON_LC2OUT         : bit  absolute CLC2CON.5;
  CLC2CON_LC2INTP        : bit  absolute CLC2CON.4;
  CLC2CON_LC2INTN        : bit  absolute CLC2CON.3;
  CLC2CON_LC2MODE2       : bit  absolute CLC2CON.2;
  CLC2CON_LC2MODE1       : bit  absolute CLC2CON.1;
  CLC2CON_LC2MODE0       : bit  absolute CLC2CON.0;
  CLC2POL                : byte absolute $1E1B;
  CLC2POL_LC2POL         : bit  absolute CLC2POL.7;
  CLC2POL_LC2G4POL       : bit  absolute CLC2POL.3;
  CLC2POL_LC2G3POL       : bit  absolute CLC2POL.2;
  CLC2POL_LC2G2POL       : bit  absolute CLC2POL.1;
  CLC2POL_LC2G1POL       : bit  absolute CLC2POL.0;
  CLC2SEL0               : byte absolute $1E1C;
  CLC2SEL0_LC2D1S5       : bit  absolute CLC2SEL0.5;
  CLC2SEL0_LC2D1S4       : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S3       : bit  absolute CLC2SEL0.3;
  CLC2SEL0_LC2D1S2       : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1       : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0       : bit  absolute CLC2SEL0.0;
  CLC2SEL1               : byte absolute $1E1D;
  CLC2SEL1_LC2D2S5       : bit  absolute CLC2SEL1.5;
  CLC2SEL1_LC2D2S4       : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D2S3       : bit  absolute CLC2SEL1.3;
  CLC2SEL1_LC2D2S2       : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D2S1       : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D2S0       : bit  absolute CLC2SEL1.0;
  CLC2SEL2               : byte absolute $1E1E;
  CLC2SEL2_LC2D3S5       : bit  absolute CLC2SEL2.5;
  CLC2SEL2_LC2D3S4       : bit  absolute CLC2SEL2.4;
  CLC2SEL2_LC2D3S3       : bit  absolute CLC2SEL2.3;
  CLC2SEL2_LC2D3S2       : bit  absolute CLC2SEL2.2;
  CLC2SEL2_LC2D3S1       : bit  absolute CLC2SEL2.1;
  CLC2SEL2_LC2D3S0       : bit  absolute CLC2SEL2.0;
  CLC2SEL3               : byte absolute $1E1F;
  CLC2SEL3_LC2D4S5       : bit  absolute CLC2SEL3.5;
  CLC2SEL3_LC2D4S4       : bit  absolute CLC2SEL3.4;
  CLC2SEL3_LC2D4S3       : bit  absolute CLC2SEL3.3;
  CLC2SEL3_LC2D4S2       : bit  absolute CLC2SEL3.2;
  CLC2SEL3_LC2D4S1       : bit  absolute CLC2SEL3.1;
  CLC2SEL3_LC2D4S0       : bit  absolute CLC2SEL3.0;
  CLC2GLS0               : byte absolute $1E20;
  CLC2GLS0_LC2G1D4T      : bit  absolute CLC2GLS0.7;
  CLC2GLS0_LC2G1D4N      : bit  absolute CLC2GLS0.6;
  CLC2GLS0_LC2G1D3T      : bit  absolute CLC2GLS0.5;
  CLC2GLS0_LC2G1D3N      : bit  absolute CLC2GLS0.4;
  CLC2GLS0_LC2G1D2T      : bit  absolute CLC2GLS0.3;
  CLC2GLS0_LC2G1D2N      : bit  absolute CLC2GLS0.2;
  CLC2GLS0_LC2G1D1T      : bit  absolute CLC2GLS0.1;
  CLC2GLS0_LC2G1D1N      : bit  absolute CLC2GLS0.0;
  CLC2GLS1               : byte absolute $1E21;
  CLC2GLS1_LC2G2D4T      : bit  absolute CLC2GLS1.7;
  CLC2GLS1_LC2G2D4N      : bit  absolute CLC2GLS1.6;
  CLC2GLS1_LC2G2D3T      : bit  absolute CLC2GLS1.5;
  CLC2GLS1_LC2G2D3N      : bit  absolute CLC2GLS1.4;
  CLC2GLS1_LC2G2D2T      : bit  absolute CLC2GLS1.3;
  CLC2GLS1_LC2G2D2N      : bit  absolute CLC2GLS1.2;
  CLC2GLS1_LC2G2D1T      : bit  absolute CLC2GLS1.1;
  CLC2GLS1_LC2G2D1N      : bit  absolute CLC2GLS1.0;
  CLC2GLS2               : byte absolute $1E22;
  CLC2GLS2_LC2G3D4T      : bit  absolute CLC2GLS2.7;
  CLC2GLS2_LC2G3D4N      : bit  absolute CLC2GLS2.6;
  CLC2GLS2_LC2G3D3T      : bit  absolute CLC2GLS2.5;
  CLC2GLS2_LC2G3D3N      : bit  absolute CLC2GLS2.4;
  CLC2GLS2_LC2G3D2T      : bit  absolute CLC2GLS2.3;
  CLC2GLS2_LC2G3D2N      : bit  absolute CLC2GLS2.2;
  CLC2GLS2_LC2G3D1T      : bit  absolute CLC2GLS2.1;
  CLC2GLS2_LC2G3D1N      : bit  absolute CLC2GLS2.0;
  CLC2GLS3               : byte absolute $1E23;
  CLC2GLS3_LC2G4D4T      : bit  absolute CLC2GLS3.7;
  CLC2GLS3_LC2G4D4N      : bit  absolute CLC2GLS3.6;
  CLC2GLS3_LC2G4D3T      : bit  absolute CLC2GLS3.5;
  CLC2GLS3_LC2G4D3N      : bit  absolute CLC2GLS3.4;
  CLC2GLS3_LC2G4D2T      : bit  absolute CLC2GLS3.3;
  CLC2GLS3_LC2G4D2N      : bit  absolute CLC2GLS3.2;
  CLC2GLS3_LC2G4D1T      : bit  absolute CLC2GLS3.1;
  CLC2GLS3_LC2G4D1N      : bit  absolute CLC2GLS3.0;
  CLC3CON                : byte absolute $1E24;
  CLC3CON_LC3EN          : bit  absolute CLC3CON.7;
  CLC3CON_LC3OUT         : bit  absolute CLC3CON.5;
  CLC3CON_LC3INTP        : bit  absolute CLC3CON.4;
  CLC3CON_LC3INTN        : bit  absolute CLC3CON.3;
  CLC3CON_LC3MODE2       : bit  absolute CLC3CON.2;
  CLC3CON_LC3MODE1       : bit  absolute CLC3CON.1;
  CLC3CON_LC3MODE0       : bit  absolute CLC3CON.0;
  CLC3POL                : byte absolute $1E25;
  CLC3POL_LC3POL         : bit  absolute CLC3POL.7;
  CLC3POL_LC3G4POL       : bit  absolute CLC3POL.3;
  CLC3POL_LC3G3POL       : bit  absolute CLC3POL.2;
  CLC3POL_LC3G2POL       : bit  absolute CLC3POL.1;
  CLC3POL_LC3G1POL       : bit  absolute CLC3POL.0;
  CLC3SEL0               : byte absolute $1E26;
  CLC3SEL0_LC3D1S5       : bit  absolute CLC3SEL0.5;
  CLC3SEL0_LC3D1S4       : bit  absolute CLC3SEL0.4;
  CLC3SEL0_LC3D1S3       : bit  absolute CLC3SEL0.3;
  CLC3SEL0_LC3D1S2       : bit  absolute CLC3SEL0.2;
  CLC3SEL0_LC3D1S1       : bit  absolute CLC3SEL0.1;
  CLC3SEL0_LC3D1S0       : bit  absolute CLC3SEL0.0;
  CLC3SEL1               : byte absolute $1E27;
  CLC3SEL1_LC3D2S5       : bit  absolute CLC3SEL1.5;
  CLC3SEL1_LC3D2S4       : bit  absolute CLC3SEL1.4;
  CLC3SEL1_LC3D2S3       : bit  absolute CLC3SEL1.3;
  CLC3SEL1_LC3D2S2       : bit  absolute CLC3SEL1.2;
  CLC3SEL1_LC3D2S1       : bit  absolute CLC3SEL1.1;
  CLC3SEL1_LC3D2S0       : bit  absolute CLC3SEL1.0;
  CLC3SEL2               : byte absolute $1E28;
  CLC3SEL2_LC3D3S5       : bit  absolute CLC3SEL2.5;
  CLC3SEL2_LC3D3S4       : bit  absolute CLC3SEL2.4;
  CLC3SEL2_LC3D3S3       : bit  absolute CLC3SEL2.3;
  CLC3SEL2_LC3D3S2       : bit  absolute CLC3SEL2.2;
  CLC3SEL2_LC3D3S1       : bit  absolute CLC3SEL2.1;
  CLC3SEL2_LC3D3S0       : bit  absolute CLC3SEL2.0;
  CLC3SEL3               : byte absolute $1E29;
  CLC3SEL3_LC3D4S5       : bit  absolute CLC3SEL3.5;
  CLC3SEL3_LC3D4S4       : bit  absolute CLC3SEL3.4;
  CLC3SEL3_LC3D4S3       : bit  absolute CLC3SEL3.3;
  CLC3SEL3_LC3D4S2       : bit  absolute CLC3SEL3.2;
  CLC3SEL3_LC3D4S1       : bit  absolute CLC3SEL3.1;
  CLC3SEL3_LC3D4S0       : bit  absolute CLC3SEL3.0;
  CLC3GLS0               : byte absolute $1E2A;
  CLC3GLS0_LC3G1D4T      : bit  absolute CLC3GLS0.7;
  CLC3GLS0_LC3G1D4N      : bit  absolute CLC3GLS0.6;
  CLC3GLS0_LC3G1D3T      : bit  absolute CLC3GLS0.5;
  CLC3GLS0_LC3G1D3N      : bit  absolute CLC3GLS0.4;
  CLC3GLS0_LC3G1D2T      : bit  absolute CLC3GLS0.3;
  CLC3GLS0_LC3G1D2N      : bit  absolute CLC3GLS0.2;
  CLC3GLS0_LC3G1D1T      : bit  absolute CLC3GLS0.1;
  CLC3GLS0_LC3G1D1N      : bit  absolute CLC3GLS0.0;
  CLC3GLS1               : byte absolute $1E2B;
  CLC3GLS1_LC3G2D4T      : bit  absolute CLC3GLS1.7;
  CLC3GLS1_LC3G2D4N      : bit  absolute CLC3GLS1.6;
  CLC3GLS1_LC3G2D3T      : bit  absolute CLC3GLS1.5;
  CLC3GLS1_LC3G2D3N      : bit  absolute CLC3GLS1.4;
  CLC3GLS1_LC3G2D2T      : bit  absolute CLC3GLS1.3;
  CLC3GLS1_LC3G2D2N      : bit  absolute CLC3GLS1.2;
  CLC3GLS1_LC3G2D1T      : bit  absolute CLC3GLS1.1;
  CLC3GLS1_LC3G2D1N      : bit  absolute CLC3GLS1.0;
  CLC3GLS2               : byte absolute $1E2C;
  CLC3GLS2_LC3G3D4T      : bit  absolute CLC3GLS2.7;
  CLC3GLS2_LC3G3D4N      : bit  absolute CLC3GLS2.6;
  CLC3GLS2_LC3G3D3T      : bit  absolute CLC3GLS2.5;
  CLC3GLS2_LC3G3D3N      : bit  absolute CLC3GLS2.4;
  CLC3GLS2_LC3G3D2T      : bit  absolute CLC3GLS2.3;
  CLC3GLS2_LC3G3D2N      : bit  absolute CLC3GLS2.2;
  CLC3GLS2_LC3G3D1T      : bit  absolute CLC3GLS2.1;
  CLC3GLS2_LC3G3D1N      : bit  absolute CLC3GLS2.0;
  CLC3GLS3               : byte absolute $1E2D;
  CLC3GLS3_LC3G4D4T      : bit  absolute CLC3GLS3.7;
  CLC3GLS3_LC3G4D4N      : bit  absolute CLC3GLS3.6;
  CLC3GLS3_LC3G4D3T      : bit  absolute CLC3GLS3.5;
  CLC3GLS3_LC3G4D3N      : bit  absolute CLC3GLS3.4;
  CLC3GLS3_LC3G4D2T      : bit  absolute CLC3GLS3.3;
  CLC3GLS3_LC3G4D2N      : bit  absolute CLC3GLS3.2;
  CLC3GLS3_LC3G4D1T      : bit  absolute CLC3GLS3.1;
  CLC3GLS3_LC3G4D1N      : bit  absolute CLC3GLS3.0;
  CLC4CON                : byte absolute $1E2E;
  CLC4CON_LC4EN          : bit  absolute CLC4CON.7;
  CLC4CON_LC4OUT         : bit  absolute CLC4CON.5;
  CLC4CON_LC4INTP        : bit  absolute CLC4CON.4;
  CLC4CON_LC4INTN        : bit  absolute CLC4CON.3;
  CLC4CON_LC4MODE2       : bit  absolute CLC4CON.2;
  CLC4CON_LC4MODE1       : bit  absolute CLC4CON.1;
  CLC4CON_LC4MODE0       : bit  absolute CLC4CON.0;
  CLC4POL                : byte absolute $1E2F;
  CLC4POL_LC4POL         : bit  absolute CLC4POL.7;
  CLC4POL_LC4G4POL       : bit  absolute CLC4POL.3;
  CLC4POL_LC4G3POL       : bit  absolute CLC4POL.2;
  CLC4POL_LC4G2POL       : bit  absolute CLC4POL.1;
  CLC4POL_LC4G1POL       : bit  absolute CLC4POL.0;
  CLC4SEL0               : byte absolute $1E30;
  CLC4SEL0_LC4D1S5       : bit  absolute CLC4SEL0.5;
  CLC4SEL0_LC4D1S4       : bit  absolute CLC4SEL0.4;
  CLC4SEL0_LC4D1S3       : bit  absolute CLC4SEL0.3;
  CLC4SEL0_LC4D1S2       : bit  absolute CLC4SEL0.2;
  CLC4SEL0_LC4D1S1       : bit  absolute CLC4SEL0.1;
  CLC4SEL0_LC4D1S0       : bit  absolute CLC4SEL0.0;
  CLC4SEL1               : byte absolute $1E31;
  CLC4SEL1_LC4D2S5       : bit  absolute CLC4SEL1.5;
  CLC4SEL1_LC4D2S4       : bit  absolute CLC4SEL1.4;
  CLC4SEL1_LC4D2S3       : bit  absolute CLC4SEL1.3;
  CLC4SEL1_LC4D2S2       : bit  absolute CLC4SEL1.2;
  CLC4SEL1_LC4D2S1       : bit  absolute CLC4SEL1.1;
  CLC4SEL1_LC4D2S0       : bit  absolute CLC4SEL1.0;
  CLC4SEL2               : byte absolute $1E32;
  CLC4SEL2_LC4D3S5       : bit  absolute CLC4SEL2.5;
  CLC4SEL2_LC4D3S4       : bit  absolute CLC4SEL2.4;
  CLC4SEL2_LC4D3S3       : bit  absolute CLC4SEL2.3;
  CLC4SEL2_LC4D3S2       : bit  absolute CLC4SEL2.2;
  CLC4SEL2_LC4D3S1       : bit  absolute CLC4SEL2.1;
  CLC4SEL2_LC4D3S0       : bit  absolute CLC4SEL2.0;
  CLC4SEL3               : byte absolute $1E33;
  CLC4SEL3_LC4D4S5       : bit  absolute CLC4SEL3.5;
  CLC4SEL3_LC4D4S4       : bit  absolute CLC4SEL3.4;
  CLC4SEL3_LC4D4S3       : bit  absolute CLC4SEL3.3;
  CLC4SEL3_LC4D4S2       : bit  absolute CLC4SEL3.2;
  CLC4SEL3_LC4D4S1       : bit  absolute CLC4SEL3.1;
  CLC4SEL3_LC4D4S0       : bit  absolute CLC4SEL3.0;
  CLC4GLS0               : byte absolute $1E34;
  CLC4GLS0_LC4G1D4T      : bit  absolute CLC4GLS0.7;
  CLC4GLS0_LC4G1D4N      : bit  absolute CLC4GLS0.6;
  CLC4GLS0_LC4G1D3T      : bit  absolute CLC4GLS0.5;
  CLC4GLS0_LC4G1D3N      : bit  absolute CLC4GLS0.4;
  CLC4GLS0_LC4G1D2T      : bit  absolute CLC4GLS0.3;
  CLC4GLS0_LC4G1D2N      : bit  absolute CLC4GLS0.2;
  CLC4GLS0_LC4G1D1T      : bit  absolute CLC4GLS0.1;
  CLC4GLS0_LC4G1D1N      : bit  absolute CLC4GLS0.0;
  CLC4GLS1               : byte absolute $1E35;
  CLC4GLS1_LC4G2D4T      : bit  absolute CLC4GLS1.7;
  CLC4GLS1_LC4G2D4N      : bit  absolute CLC4GLS1.6;
  CLC4GLS1_LC4G2D3T      : bit  absolute CLC4GLS1.5;
  CLC4GLS1_LC4G2D3N      : bit  absolute CLC4GLS1.4;
  CLC4GLS1_LC4G2D2T      : bit  absolute CLC4GLS1.3;
  CLC4GLS1_LC4G2D2N      : bit  absolute CLC4GLS1.2;
  CLC4GLS1_LC4G2D1T      : bit  absolute CLC4GLS1.1;
  CLC4GLS1_LC4G2D1N      : bit  absolute CLC4GLS1.0;
  CLC4GLS2               : byte absolute $1E36;
  CLC4GLS2_LC4G3D4T      : bit  absolute CLC4GLS2.7;
  CLC4GLS2_LC4G3D4N      : bit  absolute CLC4GLS2.6;
  CLC4GLS2_LC4G3D3T      : bit  absolute CLC4GLS2.5;
  CLC4GLS2_LC4G3D3N      : bit  absolute CLC4GLS2.4;
  CLC4GLS2_LC4G3D2T      : bit  absolute CLC4GLS2.3;
  CLC4GLS2_LC4G3D2N      : bit  absolute CLC4GLS2.2;
  CLC4GLS2_LC4G3D1T      : bit  absolute CLC4GLS2.1;
  CLC4GLS2_LC4G3D1N      : bit  absolute CLC4GLS2.0;
  CLC4GLS3               : byte absolute $1E37;
  CLC4GLS3_LC4G4D4T      : bit  absolute CLC4GLS3.7;
  CLC4GLS3_LC4G4D4N      : bit  absolute CLC4GLS3.6;
  CLC4GLS3_LC4G4D3T      : bit  absolute CLC4GLS3.5;
  CLC4GLS3_LC4G4D3N      : bit  absolute CLC4GLS3.4;
  CLC4GLS3_LC4G4D2T      : bit  absolute CLC4GLS3.3;
  CLC4GLS3_LC4G4D2N      : bit  absolute CLC4GLS3.2;
  CLC4GLS3_LC4G4D1T      : bit  absolute CLC4GLS3.1;
  CLC4GLS3_LC4G4D1N      : bit  absolute CLC4GLS3.0;
  PPSLOCK                : byte absolute $1E8F;
  PPSLOCK_PPSLOCKED      : bit  absolute PPSLOCK.0;
  INTPPS                 : byte absolute $1E90;
  INTPPS_INTPPS5         : bit  absolute INTPPS.5;
  INTPPS_INTPPS4         : bit  absolute INTPPS.4;
  INTPPS_INTPPS3         : bit  absolute INTPPS.3;
  INTPPS_INTPPS2         : bit  absolute INTPPS.2;
  INTPPS_INTPPS1         : bit  absolute INTPPS.1;
  INTPPS_INTPPS0         : bit  absolute INTPPS.0;
  T0CKIPPS               : byte absolute $1E91;
  T0CKIPPS_T0CKIPPS5     : bit  absolute T0CKIPPS.5;
  T0CKIPPS_T0CKIPPS4     : bit  absolute T0CKIPPS.4;
  T0CKIPPS_T0CKIPPS3     : bit  absolute T0CKIPPS.3;
  T0CKIPPS_T0CKIPPS2     : bit  absolute T0CKIPPS.2;
  T0CKIPPS_T0CKIPPS1     : bit  absolute T0CKIPPS.1;
  T0CKIPPS_T0CKIPPS0     : bit  absolute T0CKIPPS.0;
  T1CKIPPS               : byte absolute $1E92;
  T1CKIPPS_T1CKIPPS5     : bit  absolute T1CKIPPS.5;
  T1CKIPPS_T1CKIPPS4     : bit  absolute T1CKIPPS.4;
  T1CKIPPS_T1CKIPPS3     : bit  absolute T1CKIPPS.3;
  T1CKIPPS_T1CKIPPS2     : bit  absolute T1CKIPPS.2;
  T1CKIPPS_T1CKIPPS1     : bit  absolute T1CKIPPS.1;
  T1CKIPPS_T1CKIPPS0     : bit  absolute T1CKIPPS.0;
  T1GPPS                 : byte absolute $1E93;
  T1GPPS_T1GPPS5         : bit  absolute T1GPPS.5;
  T1GPPS_T1GPPS4         : bit  absolute T1GPPS.4;
  T1GPPS_T1GPPS3         : bit  absolute T1GPPS.3;
  T1GPPS_T1GPPS2         : bit  absolute T1GPPS.2;
  T1GPPS_T1GPPS1         : bit  absolute T1GPPS.1;
  T1GPPS_T1GPPS0         : bit  absolute T1GPPS.0;
  T2INPPS                : byte absolute $1E9C;
  T2INPPS_T2INPPS5       : bit  absolute T2INPPS.5;
  T2INPPS_T2INPPS4       : bit  absolute T2INPPS.4;
  T2INPPS_T2INPPS3       : bit  absolute T2INPPS.3;
  T2INPPS_T2INPPS2       : bit  absolute T2INPPS.2;
  T2INPPS_T2INPPS1       : bit  absolute T2INPPS.1;
  T2INPPS_T2INPPS0       : bit  absolute T2INPPS.0;
  CCP1PPS                : byte absolute $1EA1;
  CCP1PPS_CCP1PPS5       : bit  absolute CCP1PPS.5;
  CCP1PPS_CCP1PPS4       : bit  absolute CCP1PPS.4;
  CCP1PPS_CCP1PPS3       : bit  absolute CCP1PPS.3;
  CCP1PPS_CCP1PPS2       : bit  absolute CCP1PPS.2;
  CCP1PPS_CCP1PPS1       : bit  absolute CCP1PPS.1;
  CCP1PPS_CCP1PPS0       : bit  absolute CCP1PPS.0;
  CCP2PPS                : byte absolute $1EA2;
  CCP2PPS_CCP2PPS5       : bit  absolute CCP2PPS.5;
  CCP2PPS_CCP2PPS4       : bit  absolute CCP2PPS.4;
  CCP2PPS_CCP2PPS3       : bit  absolute CCP2PPS.3;
  CCP2PPS_CCP2PPS2       : bit  absolute CCP2PPS.2;
  CCP2PPS_CCP2PPS1       : bit  absolute CCP2PPS.1;
  CCP2PPS_CCP2PPS0       : bit  absolute CCP2PPS.0;
  CWG1PPS                : byte absolute $1EB1;
  CWG1PPS_CWG1PPS5       : bit  absolute CWG1PPS.5;
  CWG1PPS_CWG1PPS4       : bit  absolute CWG1PPS.4;
  CWG1PPS_CWG1PPS3       : bit  absolute CWG1PPS.3;
  CWG1PPS_CWG1PPS2       : bit  absolute CWG1PPS.2;
  CWG1PPS_CWG1PPS1       : bit  absolute CWG1PPS.1;
  CWG1PPS_CWG1PPS0       : bit  absolute CWG1PPS.0;
  CLCIN0PPS              : byte absolute $1EBB;
  CLCIN0PPS_CLCIN0PPS5   : bit  absolute CLCIN0PPS.5;
  CLCIN0PPS_CLCIN0PPS4   : bit  absolute CLCIN0PPS.4;
  CLCIN0PPS_CLCIN0PPS3   : bit  absolute CLCIN0PPS.3;
  CLCIN0PPS_CLCIN0PPS2   : bit  absolute CLCIN0PPS.2;
  CLCIN0PPS_CLCIN0PPS1   : bit  absolute CLCIN0PPS.1;
  CLCIN0PPS_CLCIN0PPS0   : bit  absolute CLCIN0PPS.0;
  CLCIN1PPS              : byte absolute $1EBC;
  CLCIN1PPS_CLCIN1PPS5   : bit  absolute CLCIN1PPS.5;
  CLCIN1PPS_CLCIN1PPS4   : bit  absolute CLCIN1PPS.4;
  CLCIN1PPS_CLCIN1PPS3   : bit  absolute CLCIN1PPS.3;
  CLCIN1PPS_CLCIN1PPS2   : bit  absolute CLCIN1PPS.2;
  CLCIN1PPS_CLCIN1PPS1   : bit  absolute CLCIN1PPS.1;
  CLCIN1PPS_CLCIN1PPS0   : bit  absolute CLCIN1PPS.0;
  CLCIN2PPS              : byte absolute $1EBD;
  CLCIN2PPS_CLCIN2PPS5   : bit  absolute CLCIN2PPS.5;
  CLCIN2PPS_CLCIN2PPS4   : bit  absolute CLCIN2PPS.4;
  CLCIN2PPS_CLCIN2PPS3   : bit  absolute CLCIN2PPS.3;
  CLCIN2PPS_CLCIN2PPS2   : bit  absolute CLCIN2PPS.2;
  CLCIN2PPS_CLCIN2PPS1   : bit  absolute CLCIN2PPS.1;
  CLCIN2PPS_CLCIN2PPS0   : bit  absolute CLCIN2PPS.0;
  CLCIN3PPS              : byte absolute $1EBE;
  CLCIN3PPS_CLCIN3PPS5   : bit  absolute CLCIN3PPS.5;
  CLCIN3PPS_CLCIN3PPS4   : bit  absolute CLCIN3PPS.4;
  CLCIN3PPS_CLCIN3PPS3   : bit  absolute CLCIN3PPS.3;
  CLCIN3PPS_CLCIN3PPS2   : bit  absolute CLCIN3PPS.2;
  CLCIN3PPS_CLCIN3PPS1   : bit  absolute CLCIN3PPS.1;
  CLCIN3PPS_CLCIN3PPS0   : bit  absolute CLCIN3PPS.0;
  ADACTPPS               : byte absolute $1EC3;
  ADACTPPS_ADACTPPS5     : bit  absolute ADACTPPS.5;
  ADACTPPS_ADACTPPS4     : bit  absolute ADACTPPS.4;
  ADACTPPS_ADACTPPS3     : bit  absolute ADACTPPS.3;
  ADACTPPS_ADACTPPS2     : bit  absolute ADACTPPS.2;
  ADACTPPS_ADACTPPS1     : bit  absolute ADACTPPS.1;
  ADACTPPS_ADACTPPS0     : bit  absolute ADACTPPS.0;
  SSP1CLKPPS             : byte absolute $1EC5;
  SSP1CLKPPS_SSP1CLKPPS5 : bit  absolute SSP1CLKPPS.5;
  SSP1CLKPPS_SSP1CLKPPS4 : bit  absolute SSP1CLKPPS.4;
  SSP1CLKPPS_SSP1CLKPPS3 : bit  absolute SSP1CLKPPS.3;
  SSP1CLKPPS_SSP1CLKPPS2 : bit  absolute SSP1CLKPPS.2;
  SSP1CLKPPS_SSP1CLKPPS1 : bit  absolute SSP1CLKPPS.1;
  SSP1CLKPPS_SSP1CLKPPS0 : bit  absolute SSP1CLKPPS.0;
  SSP1DATPPS             : byte absolute $1EC6;
  SSP1DATPPS_SSP1DATPPS5 : bit  absolute SSP1DATPPS.5;
  SSP1DATPPS_SSP1DATPPS4 : bit  absolute SSP1DATPPS.4;
  SSP1DATPPS_SSP1DATPPS3 : bit  absolute SSP1DATPPS.3;
  SSP1DATPPS_SSP1DATPPS2 : bit  absolute SSP1DATPPS.2;
  SSP1DATPPS_SSP1DATPPS1 : bit  absolute SSP1DATPPS.1;
  SSP1DATPPS_SSP1DATPPS0 : bit  absolute SSP1DATPPS.0;
  SSP1SSPPS              : byte absolute $1EC7;
  SSP1SSPPS_SSP1SSPPS5   : bit  absolute SSP1SSPPS.5;
  SSP1SSPPS_SSP1SSPPS4   : bit  absolute SSP1SSPPS.4;
  SSP1SSPPS_SSP1SSPPS3   : bit  absolute SSP1SSPPS.3;
  SSP1SSPPS_SSP1SSPPS2   : bit  absolute SSP1SSPPS.2;
  SSP1SSPPS_SSP1SSPPS1   : bit  absolute SSP1SSPPS.1;
  SSP1SSPPS_SSP1SSPPS0   : bit  absolute SSP1SSPPS.0;
  SSP2CLKPPS             : byte absolute $1EC8;
  SSP2CLKPPS_SSP2CLKPPS5 : bit  absolute SSP2CLKPPS.5;
  SSP2CLKPPS_SSP2CLKPPS4 : bit  absolute SSP2CLKPPS.4;
  SSP2CLKPPS_SSP2CLKPPS3 : bit  absolute SSP2CLKPPS.3;
  SSP2CLKPPS_SSP2CLKPPS2 : bit  absolute SSP2CLKPPS.2;
  SSP2CLKPPS_SSP2CLKPPS1 : bit  absolute SSP2CLKPPS.1;
  SSP2CLKPPS_SSP2CLKPPS0 : bit  absolute SSP2CLKPPS.0;
  SSP2DATPPS             : byte absolute $1EC9;
  SSP2DATPPS_SSP2DATPPS5 : bit  absolute SSP2DATPPS.5;
  SSP2DATPPS_SSP2DATPPS4 : bit  absolute SSP2DATPPS.4;
  SSP2DATPPS_SSP2DATPPS3 : bit  absolute SSP2DATPPS.3;
  SSP2DATPPS_SSP2DATPPS2 : bit  absolute SSP2DATPPS.2;
  SSP2DATPPS_SSP2DATPPS1 : bit  absolute SSP2DATPPS.1;
  SSP2DATPPS_SSP2DATPPS0 : bit  absolute SSP2DATPPS.0;
  SSP2SSPPS              : byte absolute $1ECA;
  SSP2SSPPS_SSP2SSPPS5   : bit  absolute SSP2SSPPS.5;
  SSP2SSPPS_SSP2SSPPS4   : bit  absolute SSP2SSPPS.4;
  SSP2SSPPS_SSP2SSPPS3   : bit  absolute SSP2SSPPS.3;
  SSP2SSPPS_SSP2SSPPS2   : bit  absolute SSP2SSPPS.2;
  SSP2SSPPS_SSP2SSPPS1   : bit  absolute SSP2SSPPS.1;
  SSP2SSPPS_SSP2SSPPS0   : bit  absolute SSP2SSPPS.0;
  RX1DTPPS               : byte absolute $1ECB;
  RX1DTPPS_RX1DTPPS5     : bit  absolute RX1DTPPS.5;
  RX1DTPPS_RX1DTPPS4     : bit  absolute RX1DTPPS.4;
  RX1DTPPS_RX1DTPPS3     : bit  absolute RX1DTPPS.3;
  RX1DTPPS_RX1DTPPS2     : bit  absolute RX1DTPPS.2;
  RX1DTPPS_RX1DTPPS1     : bit  absolute RX1DTPPS.1;
  RX1DTPPS_RX1DTPPS0     : bit  absolute RX1DTPPS.0;
  TX1CKPPS               : byte absolute $1ECC;
  TX1CKPPS_TX1CKPPS5     : bit  absolute TX1CKPPS.5;
  TX1CKPPS_TX1CKPPS4     : bit  absolute TX1CKPPS.4;
  TX1CKPPS_TX1CKPPS3     : bit  absolute TX1CKPPS.3;
  TX1CKPPS_TX1CKPPS2     : bit  absolute TX1CKPPS.2;
  TX1CKPPS_TX1CKPPS1     : bit  absolute TX1CKPPS.1;
  TX1CKPPS_TX1CKPPS0     : bit  absolute TX1CKPPS.0;
  RX2DTPPS               : byte absolute $1ECD;
  RX2DTPPS_RX2DTPPS5     : bit  absolute RX2DTPPS.5;
  RX2DTPPS_RX2DTPPS4     : bit  absolute RX2DTPPS.4;
  RX2DTPPS_RX2DTPPS3     : bit  absolute RX2DTPPS.3;
  RX2DTPPS_RX2DTPPS2     : bit  absolute RX2DTPPS.2;
  RX2DTPPS_RX2DTPPS1     : bit  absolute RX2DTPPS.1;
  RX2DTPPS_RX2DTPPS0     : bit  absolute RX2DTPPS.0;
  TX2CKPPS               : byte absolute $1ECE;
  TX2CKPPS_TX2CKPPS5     : bit  absolute TX2CKPPS.5;
  TX2CKPPS_TX2CKPPS4     : bit  absolute TX2CKPPS.4;
  TX2CKPPS_TX2CKPPS3     : bit  absolute TX2CKPPS.3;
  TX2CKPPS_TX2CKPPS2     : bit  absolute TX2CKPPS.2;
  TX2CKPPS_TX2CKPPS1     : bit  absolute TX2CKPPS.1;
  TX2CKPPS_TX2CKPPS0     : bit  absolute TX2CKPPS.0;
  RA0PPS                 : byte absolute $1F10;
  RA0PPS_RA0PPS4         : bit  absolute RA0PPS.4;
  RA0PPS_RA0PPS3         : bit  absolute RA0PPS.3;
  RA0PPS_RA0PPS2         : bit  absolute RA0PPS.2;
  RA0PPS_RA0PPS1         : bit  absolute RA0PPS.1;
  RA0PPS_RA0PPS0         : bit  absolute RA0PPS.0;
  RA1PPS                 : byte absolute $1F11;
  RA1PPS_RA1PPS4         : bit  absolute RA1PPS.4;
  RA1PPS_RA1PPS3         : bit  absolute RA1PPS.3;
  RA1PPS_RA1PPS2         : bit  absolute RA1PPS.2;
  RA1PPS_RA1PPS1         : bit  absolute RA1PPS.1;
  RA1PPS_RA1PPS0         : bit  absolute RA1PPS.0;
  RA2PPS                 : byte absolute $1F12;
  RA2PPS_RA2PPS4         : bit  absolute RA2PPS.4;
  RA2PPS_RA2PPS3         : bit  absolute RA2PPS.3;
  RA2PPS_RA2PPS2         : bit  absolute RA2PPS.2;
  RA2PPS_RA2PPS1         : bit  absolute RA2PPS.1;
  RA2PPS_RA2PPS0         : bit  absolute RA2PPS.0;
  RA3PPS                 : byte absolute $1F13;
  RA3PPS_RA3PPS4         : bit  absolute RA3PPS.4;
  RA3PPS_RA3PPS3         : bit  absolute RA3PPS.3;
  RA3PPS_RA3PPS2         : bit  absolute RA3PPS.2;
  RA3PPS_RA3PPS1         : bit  absolute RA3PPS.1;
  RA3PPS_RA3PPS0         : bit  absolute RA3PPS.0;
  RA4PPS                 : byte absolute $1F14;
  RA4PPS_RA4PPS4         : bit  absolute RA4PPS.4;
  RA4PPS_RA4PPS3         : bit  absolute RA4PPS.3;
  RA4PPS_RA4PPS2         : bit  absolute RA4PPS.2;
  RA4PPS_RA4PPS1         : bit  absolute RA4PPS.1;
  RA4PPS_RA4PPS0         : bit  absolute RA4PPS.0;
  RA5PPS                 : byte absolute $1F15;
  RA5PPS_RA5PPS4         : bit  absolute RA5PPS.4;
  RA5PPS_RA5PPS3         : bit  absolute RA5PPS.3;
  RA5PPS_RA5PPS2         : bit  absolute RA5PPS.2;
  RA5PPS_RA5PPS1         : bit  absolute RA5PPS.1;
  RA5PPS_RA5PPS0         : bit  absolute RA5PPS.0;
  RA6PPS                 : byte absolute $1F16;
  RA6PPS_RA6PPS4         : bit  absolute RA6PPS.4;
  RA6PPS_RA6PPS3         : bit  absolute RA6PPS.3;
  RA6PPS_RA6PPS2         : bit  absolute RA6PPS.2;
  RA6PPS_RA6PPS1         : bit  absolute RA6PPS.1;
  RA6PPS_RA6PPS0         : bit  absolute RA6PPS.0;
  RA7PPS                 : byte absolute $1F17;
  RA7PPS_RA7PPS4         : bit  absolute RA7PPS.4;
  RA7PPS_RA7PPS3         : bit  absolute RA7PPS.3;
  RA7PPS_RA7PPS2         : bit  absolute RA7PPS.2;
  RA7PPS_RA7PPS1         : bit  absolute RA7PPS.1;
  RA7PPS_RA7PPS0         : bit  absolute RA7PPS.0;
  RB0PPS                 : byte absolute $1F18;
  RB0PPS_RB0PPS4         : bit  absolute RB0PPS.4;
  RB0PPS_RB0PPS3         : bit  absolute RB0PPS.3;
  RB0PPS_RB0PPS2         : bit  absolute RB0PPS.2;
  RB0PPS_RB0PPS1         : bit  absolute RB0PPS.1;
  RB0PPS_RB0PPS0         : bit  absolute RB0PPS.0;
  RB1PPS                 : byte absolute $1F19;
  RB1PPS_RB1PPS4         : bit  absolute RB1PPS.4;
  RB1PPS_RB1PPS3         : bit  absolute RB1PPS.3;
  RB1PPS_RB1PPS2         : bit  absolute RB1PPS.2;
  RB1PPS_RB1PPS1         : bit  absolute RB1PPS.1;
  RB1PPS_RB1PPS0         : bit  absolute RB1PPS.0;
  RB2PPS                 : byte absolute $1F1A;
  RB2PPS_RB2PPS4         : bit  absolute RB2PPS.4;
  RB2PPS_RB2PPS3         : bit  absolute RB2PPS.3;
  RB2PPS_RB2PPS2         : bit  absolute RB2PPS.2;
  RB2PPS_RB2PPS1         : bit  absolute RB2PPS.1;
  RB2PPS_RB2PPS0         : bit  absolute RB2PPS.0;
  RB3PPS                 : byte absolute $1F1B;
  RB3PPS_RB3PPS4         : bit  absolute RB3PPS.4;
  RB3PPS_RB3PPS3         : bit  absolute RB3PPS.3;
  RB3PPS_RB3PPS2         : bit  absolute RB3PPS.2;
  RB3PPS_RB3PPS1         : bit  absolute RB3PPS.1;
  RB3PPS_RB3PPS0         : bit  absolute RB3PPS.0;
  RB4PPS                 : byte absolute $1F1C;
  RB4PPS_RB4PPS4         : bit  absolute RB4PPS.4;
  RB4PPS_RB4PPS3         : bit  absolute RB4PPS.3;
  RB4PPS_RB4PPS2         : bit  absolute RB4PPS.2;
  RB4PPS_RB4PPS1         : bit  absolute RB4PPS.1;
  RB4PPS_RB4PPS0         : bit  absolute RB4PPS.0;
  RB5PPS                 : byte absolute $1F1D;
  RB5PPS_RB5PPS4         : bit  absolute RB5PPS.4;
  RB5PPS_RB5PPS3         : bit  absolute RB5PPS.3;
  RB5PPS_RB5PPS2         : bit  absolute RB5PPS.2;
  RB5PPS_RB5PPS1         : bit  absolute RB5PPS.1;
  RB5PPS_RB5PPS0         : bit  absolute RB5PPS.0;
  RB6PPS                 : byte absolute $1F1E;
  RB6PPS_RB6PPS4         : bit  absolute RB6PPS.4;
  RB6PPS_RB6PPS3         : bit  absolute RB6PPS.3;
  RB6PPS_RB6PPS2         : bit  absolute RB6PPS.2;
  RB6PPS_RB6PPS1         : bit  absolute RB6PPS.1;
  RB6PPS_RB6PPS0         : bit  absolute RB6PPS.0;
  RB7PPS                 : byte absolute $1F1F;
  RB7PPS_RB7PPS4         : bit  absolute RB7PPS.4;
  RB7PPS_RB7PPS3         : bit  absolute RB7PPS.3;
  RB7PPS_RB7PPS2         : bit  absolute RB7PPS.2;
  RB7PPS_RB7PPS1         : bit  absolute RB7PPS.1;
  RB7PPS_RB7PPS0         : bit  absolute RB7PPS.0;
  RC0PPS                 : byte absolute $1F20;
  RC0PPS_RC0PPS4         : bit  absolute RC0PPS.4;
  RC0PPS_RC0PPS3         : bit  absolute RC0PPS.3;
  RC0PPS_RC0PPS2         : bit  absolute RC0PPS.2;
  RC0PPS_RC0PPS1         : bit  absolute RC0PPS.1;
  RC0PPS_RC0PPS0         : bit  absolute RC0PPS.0;
  RC1PPS                 : byte absolute $1F21;
  RC1PPS_RC1PPS4         : bit  absolute RC1PPS.4;
  RC1PPS_RC1PPS3         : bit  absolute RC1PPS.3;
  RC1PPS_RC1PPS2         : bit  absolute RC1PPS.2;
  RC1PPS_RC1PPS1         : bit  absolute RC1PPS.1;
  RC1PPS_RC1PPS0         : bit  absolute RC1PPS.0;
  RC2PPS                 : byte absolute $1F22;
  RC2PPS_RC2PPS4         : bit  absolute RC2PPS.4;
  RC2PPS_RC2PPS3         : bit  absolute RC2PPS.3;
  RC2PPS_RC2PPS2         : bit  absolute RC2PPS.2;
  RC2PPS_RC2PPS1         : bit  absolute RC2PPS.1;
  RC2PPS_RC2PPS0         : bit  absolute RC2PPS.0;
  RC3PPS                 : byte absolute $1F23;
  RC3PPS_RC3PPS4         : bit  absolute RC3PPS.4;
  RC3PPS_RC3PPS3         : bit  absolute RC3PPS.3;
  RC3PPS_RC3PPS2         : bit  absolute RC3PPS.2;
  RC3PPS_RC3PPS1         : bit  absolute RC3PPS.1;
  RC3PPS_RC3PPS0         : bit  absolute RC3PPS.0;
  RC4PPS                 : byte absolute $1F24;
  RC4PPS_RC4PPS4         : bit  absolute RC4PPS.4;
  RC4PPS_RC4PPS3         : bit  absolute RC4PPS.3;
  RC4PPS_RC4PPS2         : bit  absolute RC4PPS.2;
  RC4PPS_RC4PPS1         : bit  absolute RC4PPS.1;
  RC4PPS_RC4PPS0         : bit  absolute RC4PPS.0;
  RC5PPS                 : byte absolute $1F25;
  RC5PPS_RC5PPS4         : bit  absolute RC5PPS.4;
  RC5PPS_RC5PPS3         : bit  absolute RC5PPS.3;
  RC5PPS_RC5PPS2         : bit  absolute RC5PPS.2;
  RC5PPS_RC5PPS1         : bit  absolute RC5PPS.1;
  RC5PPS_RC5PPS0         : bit  absolute RC5PPS.0;
  RC6PPS                 : byte absolute $1F26;
  RC6PPS_RC6PPS4         : bit  absolute RC6PPS.4;
  RC6PPS_RC6PPS3         : bit  absolute RC6PPS.3;
  RC6PPS_RC6PPS2         : bit  absolute RC6PPS.2;
  RC6PPS_RC6PPS1         : bit  absolute RC6PPS.1;
  RC6PPS_RC6PPS0         : bit  absolute RC6PPS.0;
  RC7PPS                 : byte absolute $1F27;
  RC7PPS_RC7PPS4         : bit  absolute RC7PPS.4;
  RC7PPS_RC7PPS3         : bit  absolute RC7PPS.3;
  RC7PPS_RC7PPS2         : bit  absolute RC7PPS.2;
  RC7PPS_RC7PPS1         : bit  absolute RC7PPS.1;
  RC7PPS_RC7PPS0         : bit  absolute RC7PPS.0;
  ANSELA                 : byte absolute $1F38;
  ANSELA_ANSA7           : bit  absolute ANSELA.7;
  ANSELA_ANSA6           : bit  absolute ANSELA.6;
  ANSELA_ANSA5           : bit  absolute ANSELA.5;
  ANSELA_ANSA4           : bit  absolute ANSELA.4;
  ANSELA_ANSA3           : bit  absolute ANSELA.3;
  ANSELA_ANSA2           : bit  absolute ANSELA.2;
  ANSELA_ANSA1           : bit  absolute ANSELA.1;
  ANSELA_ANSA0           : bit  absolute ANSELA.0;
  WPUA                   : byte absolute $1F39;
  WPUA_WPUA7             : bit  absolute WPUA.7;
  WPUA_WPUA6             : bit  absolute WPUA.6;
  WPUA_WPUA5             : bit  absolute WPUA.5;
  WPUA_WPUA4             : bit  absolute WPUA.4;
  WPUA_WPUA3             : bit  absolute WPUA.3;
  WPUA_WPUA2             : bit  absolute WPUA.2;
  WPUA_WPUA1             : bit  absolute WPUA.1;
  WPUA_WPUA0             : bit  absolute WPUA.0;
  ODCONA                 : byte absolute $1F3A;
  ODCONA_ODCA7           : bit  absolute ODCONA.7;
  ODCONA_ODCA6           : bit  absolute ODCONA.6;
  ODCONA_ODCA5           : bit  absolute ODCONA.5;
  ODCONA_ODCA4           : bit  absolute ODCONA.4;
  ODCONA_ODCA3           : bit  absolute ODCONA.3;
  ODCONA_ODCA2           : bit  absolute ODCONA.2;
  ODCONA_ODCA1           : bit  absolute ODCONA.1;
  ODCONA_ODCA0           : bit  absolute ODCONA.0;
  SLRCONA                : byte absolute $1F3B;
  SLRCONA_SLRA7          : bit  absolute SLRCONA.7;
  SLRCONA_SLRA6          : bit  absolute SLRCONA.6;
  SLRCONA_SLRA5          : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4          : bit  absolute SLRCONA.4;
  SLRCONA_SLRA3          : bit  absolute SLRCONA.3;
  SLRCONA_SLRA2          : bit  absolute SLRCONA.2;
  SLRCONA_SLRA1          : bit  absolute SLRCONA.1;
  SLRCONA_SLRA0          : bit  absolute SLRCONA.0;
  INLVLA                 : byte absolute $1F3C;
  INLVLA_INLVLA7         : bit  absolute INLVLA.7;
  INLVLA_INLVLA6         : bit  absolute INLVLA.6;
  INLVLA_INLVLA5         : bit  absolute INLVLA.5;
  INLVLA_INLVLA4         : bit  absolute INLVLA.4;
  INLVLA_INLVLA3         : bit  absolute INLVLA.3;
  INLVLA_INLVLA2         : bit  absolute INLVLA.2;
  INLVLA_INLVLA1         : bit  absolute INLVLA.1;
  INLVLA_INLVLA0         : bit  absolute INLVLA.0;
  IOCAP                  : byte absolute $1F3D;
  IOCAP_IOCAP7           : bit  absolute IOCAP.7;
  IOCAP_IOCAP6           : bit  absolute IOCAP.6;
  IOCAP_IOCAP5           : bit  absolute IOCAP.5;
  IOCAP_IOCAP4           : bit  absolute IOCAP.4;
  IOCAP_IOCAP3           : bit  absolute IOCAP.3;
  IOCAP_IOCAP2           : bit  absolute IOCAP.2;
  IOCAP_IOCAP1           : bit  absolute IOCAP.1;
  IOCAP_IOCAP0           : bit  absolute IOCAP.0;
  IOCAN                  : byte absolute $1F3E;
  IOCAN_IOCAN7           : bit  absolute IOCAN.7;
  IOCAN_IOCAN6           : bit  absolute IOCAN.6;
  IOCAN_IOCAN5           : bit  absolute IOCAN.5;
  IOCAN_IOCAN4           : bit  absolute IOCAN.4;
  IOCAN_IOCAN3           : bit  absolute IOCAN.3;
  IOCAN_IOCAN2           : bit  absolute IOCAN.2;
  IOCAN_IOCAN1           : bit  absolute IOCAN.1;
  IOCAN_IOCAN0           : bit  absolute IOCAN.0;
  IOCAF                  : byte absolute $1F3F;
  IOCAF_IOCAF7           : bit  absolute IOCAF.7;
  IOCAF_IOCAF6           : bit  absolute IOCAF.6;
  IOCAF_IOCAF5           : bit  absolute IOCAF.5;
  IOCAF_IOCAF4           : bit  absolute IOCAF.4;
  IOCAF_IOCAF3           : bit  absolute IOCAF.3;
  IOCAF_IOCAF2           : bit  absolute IOCAF.2;
  IOCAF_IOCAF1           : bit  absolute IOCAF.1;
  IOCAF_IOCAF0           : bit  absolute IOCAF.0;
  ANSELB                 : byte absolute $1F43;
  ANSELB_ANSB7           : bit  absolute ANSELB.7;
  ANSELB_ANSB6           : bit  absolute ANSELB.6;
  ANSELB_ANSB5           : bit  absolute ANSELB.5;
  ANSELB_ANSB4           : bit  absolute ANSELB.4;
  ANSELB_ANSB3           : bit  absolute ANSELB.3;
  ANSELB_ANSB2           : bit  absolute ANSELB.2;
  ANSELB_ANSB1           : bit  absolute ANSELB.1;
  ANSELB_ANSB0           : bit  absolute ANSELB.0;
  WPUB                   : byte absolute $1F44;
  WPUB_WPUB7             : bit  absolute WPUB.7;
  WPUB_WPUB6             : bit  absolute WPUB.6;
  WPUB_WPUB5             : bit  absolute WPUB.5;
  WPUB_WPUB4             : bit  absolute WPUB.4;
  WPUB_WPUB3             : bit  absolute WPUB.3;
  WPUB_WPUB2             : bit  absolute WPUB.2;
  WPUB_WPUB1             : bit  absolute WPUB.1;
  WPUB_WPUB0             : bit  absolute WPUB.0;
  ODCONB                 : byte absolute $1F45;
  ODCONB_ODCB7           : bit  absolute ODCONB.7;
  ODCONB_ODCB6           : bit  absolute ODCONB.6;
  ODCONB_ODCB5           : bit  absolute ODCONB.5;
  ODCONB_ODCB4           : bit  absolute ODCONB.4;
  ODCONB_ODCB3           : bit  absolute ODCONB.3;
  ODCONB_ODCB2           : bit  absolute ODCONB.2;
  ODCONB_ODCB1           : bit  absolute ODCONB.1;
  ODCONB_ODCB0           : bit  absolute ODCONB.0;
  SLRCONB                : byte absolute $1F46;
  SLRCONB_SLRB7          : bit  absolute SLRCONB.7;
  SLRCONB_SLRB6          : bit  absolute SLRCONB.6;
  SLRCONB_SLRB5          : bit  absolute SLRCONB.5;
  SLRCONB_SLRB4          : bit  absolute SLRCONB.4;
  SLRCONB_SLRB3          : bit  absolute SLRCONB.3;
  SLRCONB_SLRB2          : bit  absolute SLRCONB.2;
  SLRCONB_SLRB1          : bit  absolute SLRCONB.1;
  SLRCONB_SLRB0          : bit  absolute SLRCONB.0;
  INLVLB                 : byte absolute $1F47;
  INLVLB_INLVLB7         : bit  absolute INLVLB.7;
  INLVLB_INLVLB6         : bit  absolute INLVLB.6;
  INLVLB_INLVLB5         : bit  absolute INLVLB.5;
  INLVLB_INLVLB4         : bit  absolute INLVLB.4;
  INLVLB_INLVLB3         : bit  absolute INLVLB.3;
  INLVLB_INLVLB2         : bit  absolute INLVLB.2;
  INLVLB_INLVLB1         : bit  absolute INLVLB.1;
  INLVLB_INLVLB0         : bit  absolute INLVLB.0;
  IOCBP                  : byte absolute $1F48;
  IOCBP_IOCBP7           : bit  absolute IOCBP.7;
  IOCBP_IOCBP6           : bit  absolute IOCBP.6;
  IOCBP_IOCBP5           : bit  absolute IOCBP.5;
  IOCBP_IOCBP4           : bit  absolute IOCBP.4;
  IOCBP_IOCBP3           : bit  absolute IOCBP.3;
  IOCBP_IOCBP2           : bit  absolute IOCBP.2;
  IOCBP_IOCBP1           : bit  absolute IOCBP.1;
  IOCBP_IOCBP0           : bit  absolute IOCBP.0;
  IOCBN                  : byte absolute $1F49;
  IOCBN_IOCBN7           : bit  absolute IOCBN.7;
  IOCBN_IOCBN6           : bit  absolute IOCBN.6;
  IOCBN_IOCBN5           : bit  absolute IOCBN.5;
  IOCBN_IOCBN4           : bit  absolute IOCBN.4;
  IOCBN_IOCBN3           : bit  absolute IOCBN.3;
  IOCBN_IOCBN2           : bit  absolute IOCBN.2;
  IOCBN_IOCBN1           : bit  absolute IOCBN.1;
  IOCBN_IOCBN0           : bit  absolute IOCBN.0;
  IOCBF                  : byte absolute $1F4A;
  IOCBF_IOCBF7           : bit  absolute IOCBF.7;
  IOCBF_IOCBF6           : bit  absolute IOCBF.6;
  IOCBF_IOCBF5           : bit  absolute IOCBF.5;
  IOCBF_IOCBF4           : bit  absolute IOCBF.4;
  IOCBF_IOCBF3           : bit  absolute IOCBF.3;
  IOCBF_IOCBF2           : bit  absolute IOCBF.2;
  IOCBF_IOCBF1           : bit  absolute IOCBF.1;
  IOCBF_IOCBF0           : bit  absolute IOCBF.0;
  ANSELC                 : byte absolute $1F4E;
  ANSELC_ANSC7           : bit  absolute ANSELC.7;
  ANSELC_ANSC6           : bit  absolute ANSELC.6;
  ANSELC_ANSC5           : bit  absolute ANSELC.5;
  ANSELC_ANSC4           : bit  absolute ANSELC.4;
  ANSELC_ANSC3           : bit  absolute ANSELC.3;
  ANSELC_ANSC2           : bit  absolute ANSELC.2;
  ANSELC_ANSC1           : bit  absolute ANSELC.1;
  ANSELC_ANSC0           : bit  absolute ANSELC.0;
  WPUC                   : byte absolute $1F4F;
  WPUC_WPUC7             : bit  absolute WPUC.7;
  WPUC_WPUC6             : bit  absolute WPUC.6;
  WPUC_WPUC5             : bit  absolute WPUC.5;
  WPUC_WPUC4             : bit  absolute WPUC.4;
  WPUC_WPUC3             : bit  absolute WPUC.3;
  WPUC_WPUC2             : bit  absolute WPUC.2;
  WPUC_WPUC1             : bit  absolute WPUC.1;
  WPUC_WPUC0             : bit  absolute WPUC.0;
  ODCONC                 : byte absolute $1F50;
  ODCONC_ODCC7           : bit  absolute ODCONC.7;
  ODCONC_ODCC6           : bit  absolute ODCONC.6;
  ODCONC_ODCC5           : bit  absolute ODCONC.5;
  ODCONC_ODCC4           : bit  absolute ODCONC.4;
  ODCONC_ODCC3           : bit  absolute ODCONC.3;
  ODCONC_ODCC2           : bit  absolute ODCONC.2;
  ODCONC_ODCC1           : bit  absolute ODCONC.1;
  ODCONC_ODCC0           : bit  absolute ODCONC.0;
  SLRCONC                : byte absolute $1F51;
  SLRCONC_SLRC7          : bit  absolute SLRCONC.7;
  SLRCONC_SLRC6          : bit  absolute SLRCONC.6;
  SLRCONC_SLRC5          : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4          : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3          : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2          : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1          : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0          : bit  absolute SLRCONC.0;
  INLVLC                 : byte absolute $1F52;
  INLVLC_INLVLC7         : bit  absolute INLVLC.7;
  INLVLC_INLVLC6         : bit  absolute INLVLC.6;
  INLVLC_INLVLC5         : bit  absolute INLVLC.5;
  INLVLC_INLVLC4         : bit  absolute INLVLC.4;
  INLVLC_INLVLC3         : bit  absolute INLVLC.3;
  INLVLC_INLVLC2         : bit  absolute INLVLC.2;
  INLVLC_INLVLC1         : bit  absolute INLVLC.1;
  INLVLC_INLVLC0         : bit  absolute INLVLC.0;
  IOCCP                  : byte absolute $1F53;
  IOCCP_IOCCP7           : bit  absolute IOCCP.7;
  IOCCP_IOCCP6           : bit  absolute IOCCP.6;
  IOCCP_IOCCP5           : bit  absolute IOCCP.5;
  IOCCP_IOCCP4           : bit  absolute IOCCP.4;
  IOCCP_IOCCP3           : bit  absolute IOCCP.3;
  IOCCP_IOCCP2           : bit  absolute IOCCP.2;
  IOCCP_IOCCP1           : bit  absolute IOCCP.1;
  IOCCP_IOCCP0           : bit  absolute IOCCP.0;
  IOCCN                  : byte absolute $1F54;
  IOCCN_IOCCN7           : bit  absolute IOCCN.7;
  IOCCN_IOCCN6           : bit  absolute IOCCN.6;
  IOCCN_IOCCN5           : bit  absolute IOCCN.5;
  IOCCN_IOCCN4           : bit  absolute IOCCN.4;
  IOCCN_IOCCN3           : bit  absolute IOCCN.3;
  IOCCN_IOCCN2           : bit  absolute IOCCN.2;
  IOCCN_IOCCN1           : bit  absolute IOCCN.1;
  IOCCN_IOCCN0           : bit  absolute IOCCN.0;
  IOCCF                  : byte absolute $1F55;
  IOCCF_IOCCF7           : bit  absolute IOCCF.7;
  IOCCF_IOCCF6           : bit  absolute IOCCF.6;
  IOCCF_IOCCF5           : bit  absolute IOCCF.5;
  IOCCF_IOCCF4           : bit  absolute IOCCF.4;
  IOCCF_IOCCF3           : bit  absolute IOCCF.3;
  IOCCF_IOCCF2           : bit  absolute IOCCF.2;
  IOCCF_IOCCF1           : bit  absolute IOCCF.1;
  IOCCF_IOCCF0           : bit  absolute IOCCF.0;
  WPUE                   : byte absolute $1F65;
  WPUE_WPUE3             : bit  absolute WPUE.3;
  INLVLE                 : byte absolute $1F68;
  INLVLE_INLVLE3         : bit  absolute INLVLE.3;
  IOCEP                  : byte absolute $1F69;
  IOCEP_IOCEP3           : bit  absolute IOCEP.3;
  IOCEN                  : byte absolute $1F6A;
  IOCEN_IOCEN3           : bit  absolute IOCEN.3;
  IOCEF                  : byte absolute $1F6B;
  IOCEF_IOCEF3           : bit  absolute IOCEF.3;
  STATUS_SHAD            : byte absolute $1FE4;
  WREG_SHAD              : byte absolute $1FE5;
  BSR_SHAD               : byte absolute $1FE6;
  PCLATH_SHAD            : byte absolute $1FE7;
  FSR0L_SHAD             : byte absolute $1FE8;
  FSR0H_SHAD             : byte absolute $1FE9;
  FSR1L_SHAD             : byte absolute $1FEA;
  FSR1H_SHAD             : byte absolute $1FEB;
  STKPTR                 : byte absolute $1FED;
  STKPTR_STKPTR4         : bit  absolute STKPTR.4;
  STKPTR_STKPTR3         : bit  absolute STKPTR.3;
  STKPTR_STKPTR2         : bit  absolute STKPTR.2;
  STKPTR_STKPTR1         : bit  absolute STKPTR.1;
  STKPTR_STKPTR0         : bit  absolute STKPTR.0;
  TOSL                   : byte absolute $1FEE;
  TOSH                   : byte absolute $1FEF;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-00B:SFR:ALLMAPPED'}  // Banks 0-63 : INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON
  {$SET_STATE_RAM '00C-00E:SFR'}            // Bank 0 : PORTA, PORTB, PORTC
  {$SET_STATE_RAM '010-010:SFR'}            // Bank 0 : PORTE
  {$SET_STATE_RAM '012-014:SFR'}            // Bank 0 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '016-016:SFR'}            // Bank 0 : TRISE
  {$SET_STATE_RAM '018-01A:SFR'}            // Bank 0 : LATA, LATB, LATC
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '09B-09F:SFR'}            // Bank 1 : ADRESL, ADRESH, ADCON0, ADCON1, ADACT
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '119-11F:SFR'}            // Bank 2 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-192:SFR'}            // Bank 3 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '196-19C:SFR'}            // Bank 3 : SSP2BUF, SSP2ADD, SSP2MSK, SSP2STAT, SSP2CON1, SSP2CON2, SSP2CON3
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-211:SFR'}            // Bank 4 : TMR1L, TMR1H, T1CON, T1GCON, T1GATE, T1CLK
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-291:SFR'}            // Bank 5 : T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-316:SFR'}            // Bank 6 : CCPR1L, CCPR1H, CCP1CON, CCP1CAP, CCPR2L, CCPR2H, CCP2CON, CCP2CAP, PWM3DCL, PWM3DCH, PWM3CON
  {$SET_STATE_RAM '318-31A:SFR'}            // Bank 6 : PWM4DCL, PWM4DCH, PWM4CON
  {$SET_STATE_RAM '31C-31E:SFR'}            // Bank 6 : PWM5DCL, PWM5DCH, PWM5CON
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-38E:SFR'}            // Bank 7 : PWM6DCL, PWM6DCH, PWM6CON
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '58C-593:SFR'}            // Bank 11 : NCO1ACCL, NCO1ACCH, NCO1ACCU, NCO1INCL, NCO1INCH, NCO1INCU, NCO1CON, NCO1CLK
  {$SET_STATE_RAM '59C-59F:SFR'}            // Bank 11 : TMR0L, TMR0H, T0CON0, T0CON1
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '60C-614:SFR'}            // Bank 12 : CWG1CLKCON, CWG1DAT, CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1AS0, CWG1AS1, CWG1STR
  {$SET_STATE_RAM '620-64F:GPR'}           
  {$SET_STATE_RAM '70C-713:SFR'}            // Bank 14 : PIR0, PIR1, PIR2, PIR3, PIR4, PIR5, PIR6, PIR7
  {$SET_STATE_RAM '716-71D:SFR'}            // Bank 14 : PIE0, PIE1, PIE2, PIE3, PIE4, PIE5, PIE6, PIE7
  {$SET_STATE_RAM '796-79B:SFR'}            // Bank 15 : PMD0, PMD1, PMD2, PMD3, PMD4, PMD5
  {$SET_STATE_RAM '80C-814:SFR'}            // Bank 16 : WDTCON0, WDTCON1, WDTPSL, WDTPSH, WDTTMR, BORCON, VREGCON, PCON0, PCON1
  {$SET_STATE_RAM '81A-81F:SFR'}            // Bank 16 : NVMADRL, NVMADRH, NVMDATL, NVMDATH, NVMCON1, NVMCON2
  {$SET_STATE_RAM '88C-893:SFR'}            // Bank 17 : CPUDOZE, OSCCON1, OSCCON2, OSCCON3, OSCSTAT, OSCEN, OSCTUNE, OSCFRQ
  {$SET_STATE_RAM '895-896:SFR'}            // Bank 17 : CLKRCON, CLKRCLK
  {$SET_STATE_RAM '90C-90C:SFR'}            // Bank 18 : FVRCON
  {$SET_STATE_RAM '90E-90F:SFR'}            // Bank 18 : DAC1CON0, DAC1CON1
  {$SET_STATE_RAM '91F-91F:SFR'}            // Bank 18 : ZCDCON
  {$SET_STATE_RAM '98F-997:SFR'}            // Bank 19 : CMOUT, CM1CON0, CM1CON1, CM1NCH, CM1PCH, CM2CON0, CM2CON1, CM2NCH, CM2PCH
  {$SET_STATE_RAM 'A19-A1F:SFR'}            // Bank 20 : RC2REG, TX2REG, SP2BRGL, SP2BRGH, RC2STA, TX2STA, BAUD2CON
  {$SET_STATE_RAM '1E0F-1E37:SFR'}            // Bank 60 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3, CLC4CON, CLC4POL, CLC4SEL0, CLC4SEL1, CLC4SEL2, CLC4SEL3, CLC4GLS0, CLC4GLS1, CLC4GLS2, CLC4GLS3
  {$SET_STATE_RAM '1E8F-1E93:SFR'}            // Bank 61 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS
  {$SET_STATE_RAM '1E9C-1E9C:SFR'}            // Bank 61 : T2INPPS
  {$SET_STATE_RAM '1EA1-1EA2:SFR'}            // Bank 61 : CCP1PPS, CCP2PPS
  {$SET_STATE_RAM '1EB1-1EB1:SFR'}            // Bank 61 : CWG1PPS
  {$SET_STATE_RAM '1EBB-1EBE:SFR'}            // Bank 61 : CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS
  {$SET_STATE_RAM '1EC3-1EC3:SFR'}            // Bank 61 : ADACTPPS
  {$SET_STATE_RAM '1EC5-1ECE:SFR'}            // Bank 61 : SSP1CLKPPS, SSP1DATPPS, SSP1SSPPS, SSP2CLKPPS, SSP2DATPPS, SSP2SSPPS, RX1DTPPS, TX1CKPPS, RX2DTPPS, TX2CKPPS
  {$SET_STATE_RAM '1F10-1F27:SFR'}            // Bank 62 : RA0PPS, RA1PPS, RA2PPS, RA3PPS, RA4PPS, RA5PPS, RA6PPS, RA7PPS, RB0PPS, RB1PPS, RB2PPS, RB3PPS, RB4PPS, RB5PPS, RB6PPS, RB7PPS, RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS, RC6PPS, RC7PPS
  {$SET_STATE_RAM '1F38-1F3F:SFR'}            // Bank 62 : ANSELA, WPUA, ODCONA, SLRCONA, INLVLA, IOCAP, IOCAN, IOCAF
  {$SET_STATE_RAM '1F43-1F4A:SFR'}            // Bank 62 : ANSELB, WPUB, ODCONB, SLRCONB, INLVLB, IOCBP, IOCBN, IOCBF
  {$SET_STATE_RAM '1F4E-1F55:SFR'}            // Bank 62 : ANSELC, WPUC, ODCONC, SLRCONC, INLVLC, IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '1F65-1F65:SFR'}            // Bank 62 : WPUE
  {$SET_STATE_RAM '1F68-1F6B:SFR'}            // Bank 62 : INLVLE, IOCEP, IOCEN, IOCEF
  {$SET_STATE_RAM '1FE4-1FEB:SFR'}            // Bank 63 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM '1FED-1FEF:SFR'}            // Bank 63 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:3F'} // BSR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00B:C1'} // INTCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:08'} // PORTE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '016:08'} // TRISE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F3'} // ADCON1 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:1F'} // ADACT bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:37'} // T1CON bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20F:FC'} // T1GCON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:1F'} // T1GATE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '211:0F'} // T1CLK bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '290:0F'} // T2CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '291:0F'} // T2RST bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30E:BF'} // CCP1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30F:07'} // CCP1CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '312:BF'} // CCP2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:07'} // CCP2CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '314:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '316:B0'} // PWM3CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '318:C0'} // PWM4DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31A:B0'} // PWM4CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31C:C0'} // PWM5DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31E:B0'} // PWM5CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:C0'} // PWM6DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38E:B0'} // PWM6CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '58E:0F'} // NCO1ACCU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '591:0F'} // NCO1INCU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '592:B1'} // NCO1CON bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '593:E7'} // NCO1CLK bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59E:BF'} // T0CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60C:01'} // CWG1CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60D:0F'} // CWG1DAT bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60E:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60F:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '610:C7'} // CWG1CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '611:2F'} // CWG1CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '612:FC'} // CWG1AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:1F'} // CWG1AS1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70C:31'} // PIR0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70D:C3'} // PIR1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70E:43'} // PIR2 bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '710:03'} // PIR4 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:F1'} // PIR5 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:03'} // PIR6 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '713:31'} // PIR7 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '716:31'} // PIE0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '717:C3'} // PIE1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '718:43'} // PIE2 bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71A:03'} // PIE4 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71B:F1'} // PIE5 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71C:03'} // PIE6 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71D:31'} // PIE7 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '796:C7'} // PMD0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:87'} // PMD1 bits 6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '798:67'} // PMD2 bits 7,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:3F'} // PMD3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79A:F1'} // PMD4 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79B:1E'} // PMD5 bits 7,6,5,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80C:3F'} // WDTCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80D:77'} // WDTCON1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '810:7F'} // WDTTMR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '811:81'} // BORCON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '812:02'} // VREGCON bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '814:02'} // PCON1 bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81B:7F'} // NVMADRH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81D:3F'} // NVMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81E:7F'} // NVMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88C:F7'} // CPUDOZE bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88D:7F'} // OSCCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88E:7F'} // OSCCON2 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88F:D8'} // OSCCON3 bits 5,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '890:FD'} // OSCSTAT bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '891:FC'} // OSCEN bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '892:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '893:07'} // OSCFRQ bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '895:9F'} // CLKRCON bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '896:0F'} // CLKRCLK bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90E:BD'} // DAC1CON0 bits 6,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90F:1F'} // DAC1CON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91F:B3'} // ZCDCON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '98F:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '990:D3'} // CM1CON0 bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '991:03'} // CM1CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '992:07'} // CM1NCH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '993:07'} // CM1PCH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '994:D3'} // CM2CON0 bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '995:03'} // CM2CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '996:07'} // CM2NCH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '997:07'} // CM2PCH bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'A1F:DB'} // BAUD2CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E0F:0F'} // CLCDATA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E10:BF'} // CLC1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E12:3F'} // CLC1SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E13:3F'} // CLC1SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E14:3F'} // CLC1SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E15:3F'} // CLC1SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1A:BF'} // CLC2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1B:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1C:3F'} // CLC2SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1D:3F'} // CLC2SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1E:3F'} // CLC2SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1F:3F'} // CLC2SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E24:BF'} // CLC3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E25:8F'} // CLC3POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E26:3F'} // CLC3SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E27:3F'} // CLC3SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E28:3F'} // CLC3SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E29:3F'} // CLC3SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E2E:BF'} // CLC4CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E2F:8F'} // CLC4POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E30:3F'} // CLC4SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E31:3F'} // CLC4SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E32:3F'} // CLC4SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E33:3F'} // CLC4SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E8F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E90:3F'} // INTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E91:3F'} // T0CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E92:3F'} // T1CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E93:3F'} // T1GPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E9C:3F'} // T2INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EA1:3F'} // CCP1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EA2:3F'} // CCP2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EB1:3F'} // CWG1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBB:3F'} // CLCIN0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBC:3F'} // CLCIN1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBD:3F'} // CLCIN2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBE:3F'} // CLCIN3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC3:3F'} // ADACTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC5:3F'} // SSP1CLKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC6:3F'} // SSP1DATPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC7:3F'} // SSP1SSPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC8:3F'} // SSP2CLKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC9:3F'} // SSP2DATPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECA:3F'} // SSP2SSPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECB:3F'} // RX1DTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECC:3F'} // TX1CKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECD:3F'} // RX2DTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECE:3F'} // TX2CKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F10:1F'} // RA0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F11:1F'} // RA1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F12:1F'} // RA2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F13:1F'} // RA3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F14:1F'} // RA4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F15:1F'} // RA5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F16:1F'} // RA6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F17:1F'} // RA7PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F18:1F'} // RB0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F19:1F'} // RB1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1A:1F'} // RB2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1B:1F'} // RB3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1C:1F'} // RB4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1D:1F'} // RB5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1E:1F'} // RB6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1F:1F'} // RB7PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F20:1F'} // RC0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F21:1F'} // RC1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F22:1F'} // RC2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F23:1F'} // RC3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F24:1F'} // RC4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F25:1F'} // RC5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F26:1F'} // RC6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F27:1F'} // RC7PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F65:08'} // WPUE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F68:08'} // INLVLE bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F69:08'} // IOCEP bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F6A:08'} // IOCEN bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F6B:08'} // IOCEF bits 7,6,5,4,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : ANA0/C1IN0-/C2IN0-/IOCA0/RA0
  // Pin  2 : ANA1/C1IN1-/C2IN1-/IOCA1/RA1
  // Pin  3 : DAC1OUT1/ANA2/C1IN0+/C2IN0+/IOCA2/RA2
  // Pin  4 : DAC1REF+/VREF+/ANA3/C1IN1+/IOCA3/RA3
  // Pin  5 : ANA4/IOCA4/RA4
  // Pin  6 : ANA5/IOCA5/RA5
  // Pin  7 : CLKOUT/ANA6/IOCA6/RA6
  // Pin  8 : CLKIN/ANA7/IOCA7/RA7
  // Pin  9 : ZCD1/ANB0/C2IN1+/IOCB0/RB0
  // Pin 10 : ANB1/C1IN3-/C2IN3-/SMB_I2C/IOCB1/RB1
  // Pin 11 : ANB2/SMB_I2C/IOCB2/RB2
  // Pin 12 : ANB3/C1IN2-/C2IN2-/IOCB3/RB3
  // Pin 13 : ANB4/IOCB4/RB4
  // Pin 14 : ANB5/IOCB5/RB5
  // Pin 15 : ICSPCLK/ICDCLK/ANB6/IOCB6/RB6
  // Pin 16 : ICSPDAT/ICDDAT/DAC1OUT2/ANB7/IOCB7/RB7
  // Pin 17 : SOSCO/ANC0/IOCC0/RC0
  // Pin 18 : SOSCI/ANC1/IOCC1/RC1
  // Pin 19 : ANC2/IOCC2/RC2
  // Pin 20 : ANC3/SMB_I2C/IOCC3/RC3
  // Pin 21 : ANC4/SMB_I2C/IOCC4/RC4
  // Pin 22 : ANC5/IOCC5/RC5
  // Pin 23 : ANC6/IOCC6/RC6
  // Pin 24 : ANC7/IOCC7/RC7
  // Pin 25 : VPP/MCLR_n/ICDMCLR_n/IOCE3/RE3
  // Pin 26 : AVDD/VDD
  // Pin 27 : AVSS/VSS
  // Pin 28 : VSS/VSEL0


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-1,1-2,2-3,3-4,4-5,5-6,6-7,7-8'} // PORTA
  {$MAP_RAM_TO_PIN '00D:0-9,1-10,2-11,3-12,4-13,5-14,6-15,7-16'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-17,1-18,2-19,3-20,4-21,5-22,6-23,7-24'} // PORTC
  {$MAP_RAM_TO_PIN '010:3-25'} // PORTE


  // -- Bits Configuration --

  // FEXTOSC : External Oscillator mode selection bits
  {$define _FEXTOSC_ECH      = $3FFF}  // EC above 8MHz; PFM set to high power
  {$define _FEXTOSC_ECM      = $3FFE}  // EC for 500kHz to 8MHz; PFM set to medium power
  {$define _FEXTOSC_ECL      = $3FFD}  // EC below 500kHz; PFM set to low power
  {$define _FEXTOSC_OFF      = $3FFC}  // Oscillator not enabled
  {$define _FEXTOSC_Reserved = $3FFB}  // Reserved
  {$define _FEXTOSC_HS       = $3FFA}  // HS (crystal oscillator) above 4MHz; PFM set to high power
  {$define _FEXTOSC_XT       = $3FF9}  // XT (crystal oscillator) above 500kHz, below 4MHz; PFM set to medium power
  {$define _FEXTOSC_LP       = $3FF8}  // LP (crystal oscillator) optimized for 32.768kHz; PFM set to low power

  // RSTOSC : Power-up default value for COSC bits
  {$define _RSTOSC_EXT1X     = $3FFF}  // EXTOSC operating per FEXTOSC bits
  {$define _RSTOSC_HFINT1    = $3FEF}  // HFINTOSC (1MHz)
  {$define _RSTOSC_LFINT     = $3FDF}  // LFINTOSC
  {$define _RSTOSC_SOSC      = $3FCF}  // SOSC
  {$define _RSTOSC_Reserved  = $3FBF}  // Reserved
  {$define _RSTOSC_EXT4X     = $3FAF}  // EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
  {$define _RSTOSC_HFINTPLL  = $3F9F}  // HFINTOSC with 2x PLL, with OSCFRQ = 16 MHz and CDIV = 1:1 (FOSC = 32 MHz)
  {$define _RSTOSC_HFINT32   = $3F8F}  // HFINTOSC with OSCFRQ= 32 MHz and CDIV = 1:1

  // CLKOUTEN : Clock Out Enable bit
  {$define _CLKOUTEN_ON      = $3EFF}  // CLKOUT function is enabled; FOSC/4 clock appears at OSC2
  {$define _CLKOUTEN_OFF     = $3FFF}  // CLKOUT function is disabled; i/o or oscillator function on OSC2

  // CSWEN : Clock Switch Enable bit
  {$define _CSWEN_ON         = $3FFF}  // Writing to NOSC and NDIV is allowed
  {$define _CSWEN_OFF        = $37FF}  // The NOSC and NDIV bits cannot be changed by user software

  // FCMEN : Fail-Safe Clock Monitor Enable bit
  {$define _FCMEN_ON         = $3FFF}  // FSCM timer enabled
  {$define _FCMEN_OFF        = $1FFF}  // FSCM timer disabled

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON         = $3FFF}  // MCLR pin is Master Clear function
  {$define _MCLRE_OFF        = $3FFE}  // MCLR pin function is port defined function

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF        = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON         = $3FFD}  // PWRT enabled

  // LPBOREN : Low-Power BOR enable bit
  {$define _LPBOREN_OFF      = $3FFF}  // ULPBOR disabled
  {$define _LPBOREN_ON       = $3FDF}  // ULPBOR enabled

  // BOREN : Brown-out reset enable bits
  {$define _BOREN_ON         = $3FFF}  // Brown-out Reset Enabled, SBOREN bit is ignored
  {$define _BOREN_NSLEEP     = $3FBF}  // Brown-out Reset enabled while running, disabled in sleep; SBOREN is ignored
  {$define _BOREN_SBOREN     = $3F7F}  // Brown-out reset enabled according to SBOREN bit
  {$define _BOREN_OFF        = $3F3F}  // Brown-out reset disabled

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO          = $3FFF}  // Brown-out Reset Voltage (VBOR) set to 1.9V on LF, and 2.45V on F Devices
  {$define _BORV_HI          = $3DFF}  // Brown-out Reset Voltage (VBOR) is set to 2.7V

  // ZCD : Zero-cross detect disable
  {$define _ZCD_OFF          = $3FFF}  // Zero-cross detect circuit is disabled at POR.
  {$define _ZCD_ON           = $3BFF}  // Zero-cross detect circuit is always enabled

  // PPS1WAY : Peripheral Pin Select one-way control
  {$define _PPS1WAY_ON       = $3FFF}  // The PPSLOCK bit can be cleared and set only once in software
  {$define _PPS1WAY_OFF      = $37FF}  // The PPSLOCK bit can be set and cleared repeatedly by software

  // STVREN : Stack Overflow/Underflow Reset Enable bit
  {$define _STVREN_ON        = $3FFF}  // Stack Overflow or Underflow will cause a reset
  {$define _STVREN_OFF       = $2FFF}  // Stack Overflow or Underflow will not cause a reset

  // WDTCPS : WDT Period Select bits
  {$define _WDTCPS_WDTCPS_0  = $3FE0}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_1  = $3FE1}  // Divider ratio 1:64
  {$define _WDTCPS_WDTCPS_2  = $3FE2}  // Divider ratio 1:128
  {$define _WDTCPS_WDTCPS_3  = $3FE3}  // Divider ratio 1:256
  {$define _WDTCPS_WDTCPS_4  = $3FE4}  // Divider ratio 1:512
  {$define _WDTCPS_WDTCPS_5  = $3FE5}  // Divider ratio 1:1024
  {$define _WDTCPS_WDTCPS_6  = $3FE6}  // Divider ratio 1:2048
  {$define _WDTCPS_WDTCPS_7  = $3FE7}  // Divider ratio 1:4096
  {$define _WDTCPS_WDTCPS_8  = $3FE8}  // Divider ratio 1:8192
  {$define _WDTCPS_WDTCPS_9  = $3FE9}  // Divider ratio 1:16384
  {$define _WDTCPS_WDTCPS_10 = $3FEA}  // Divider ratio 1:32768
  {$define _WDTCPS_WDTCPS_11 = $3FEB}  // Divider ratio 1:65536
  {$define _WDTCPS_WDTCPS_12 = $3FEC}  // Divider ratio 1:131072
  {$define _WDTCPS_WDTCPS_13 = $3FED}  // Divider ratio 1:262144
  {$define _WDTCPS_WDTCPS_14 = $3FEE}  // Divider ratio 1:524299
  {$define _WDTCPS_WDTCPS_15 = $3FEF}  // Divider ratio 1:1048576
  {$define _WDTCPS_WDTCPS_16 = $3FF0}  // Divider ratio 1:2097152
  {$define _WDTCPS_WDTCPS_17 = $3FF1}  // Divider ratio 1:4194304
  {$define _WDTCPS_WDTCPS_18 = $3FF2}  // Divider ratio 1:8388608
  {$define _WDTCPS_WDTCPS_19 = $3FF3}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_20 = $3FF4}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_21 = $3FF5}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_22 = $3FF6}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_23 = $3FF7}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_24 = $3FF8}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_25 = $3FF9}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_26 = $3FFA}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_27 = $3FFB}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_28 = $3FFC}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_29 = $3FFD}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_30 = $3FFE}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_31 = $3FFF}  // Divider ratio 1:65536; software control of WDTPS

  // WDTE : WDT operating mode
  {$define _WDTE_OFF         = $3F9F}  // WDT Disabled, SWDTEN is ignored
  {$define _WDTE_SWDTEN      = $3FBF}  // WDT enabled/disabled by SWDTEN bit in WDTCON0
  {$define _WDTE_NSLEEP      = $3FDF}  // WDT enabled while sleep=0, suspended when sleep=1; SWDTEN ignored
  {$define _WDTE_ON          = $3FFF}  // WDT enabled regardless of sleep; SWDTEN ignored

  // WDTCWS : WDT Window Select bits
  {$define _WDTCWS_WDTCWS_0  = $38FF}  // window delay = 87.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_1  = $39FF}  // window delay = 75 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_2  = $3AFF}  // window delay = 62.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_3  = $3BFF}  // window delay = 50 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_4  = $3CFF}  // window delay = 37.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_5  = $3DFF}  // window delay = 25 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_6  = $3EFF}  // window always open (100%); no software control; keyed access required
  {$define _WDTCWS_WDTCWS_7  = $3FFF}  // window always open (100%); software control; keyed access not required

  // WDTCCS : WDT input clock selector
  {$define _WDTCCS_LFINTOSC  = $07FF}  // WDT reference clock is the 31.0kHz LFINTOSC output
  {$define _WDTCCS_HFINTOSC  = $0FFF}  // WDT reference clock is the 31.25 kHz HFINTOSC
  {$define _WDTCCS_SOSC      = $17FF}  // WDT reference clock is the 32kHz secondary oscillator
  {$define _WDTCCS_SC        = $3FFF}  // Software Control

  // BBSIZE : Boot Block Size Selection bits
  {$define _BBSIZE_BB512     = $3FFF}  // 512 words boot block size
  {$define _BBSIZE_BB1K      = $3FFE}  // 1024 words boot block size
  {$define _BBSIZE_BB2K      = $3FFD}  // 2048 words boot block size
  {$define _BBSIZE_BB4K      = $3FFC}  // 4096 words boot block size
  {$define _BBSIZE_BB8K      = $3FFB}  // * half of user program memory
  {$define _BBSIZE_BB16K     = $3FFA}  // * half of user program memory
  {$define _BBSIZE_BB32K     = $3FF9}  // * half of user program memory
  {$define _BBSIZE_BB64K     = $3FF8}  // * half of user program memory

  // BBEN : Boot Block Enable bit
  {$define _BBEN_OFF         = $3FFF}  // Boot Block disabled
  {$define _BBEN_ON          = $3FF7}  // Boot Block enabled

  // SAFEN : SAF Enable bit
  {$define _SAFEN_OFF        = $3FFF}  // SAF disabled
  {$define _SAFEN_ON         = $3FEF}  // SAF enabled

  // WRTAPP : Application Block Write Protection bit
  {$define _WRTAPP_OFF       = $3FFF}  // Application Block not write protected
  {$define _WRTAPP_ON        = $3F7F}  // Application Block write protected

  // WRTB : Boot Block Write Protection bit
  {$define _WRTB_OFF         = $3FFF}  // Boot Block not write protected
  {$define _WRTB_ON          = $3EFF}  // Boot Block write protected

  // WRTC : Configuration Register Write Protection bit
  {$define _WRTC_OFF         = $3FFF}  // Configuration Register not write protected
  {$define _WRTC_ON          = $3DFF}  // Configuration Register write protected

  // WRTSAF : Storage Area Flash Write Protection bit
  {$define _WRTSAF_OFF       = $3FFF}  // SAF not write protected
  {$define _WRTSAF_ON        = $37FF}  // SAF write protected

  // LVP : Low Voltage Programming Enable bit
  {$define _LVP_ON           = $3FFF}  // Low Voltage programming enabled. MCLR/Vpp pin function is MCLR.
  {$define _LVP_OFF          = $1FFF}  // High Voltage on MCLR/Vpp must be used for programming

  // CP : UserNVM Program memory code protection bit
  {$define _CP_OFF           = $3FFF}  // UserNVM code protection disabled
  {$define _CP_ON            = $3FFE}  // UserNVM code protection enabled

implementation
end.
