unit PIC16F18345;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F18345'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 32}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 8192}

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
  INTCON_INTEDG            : bit  absolute INTCON.0;
  PORTA                    : byte absolute $000C;
  PORTA_RA5                : bit  absolute PORTA.5;
  PORTA_RA4                : bit  absolute PORTA.4;
  PORTA_RA3                : bit  absolute PORTA.3;
  PORTA_RA2                : bit  absolute PORTA.2;
  PORTA_RA1                : bit  absolute PORTA.1;
  PORTA_RA0                : bit  absolute PORTA.0;
  PORTB                    : byte absolute $000D;
  PORTB_RB7                : bit  absolute PORTB.7;
  PORTB_RB6                : bit  absolute PORTB.6;
  PORTB_RB5                : bit  absolute PORTB.5;
  PORTB_RB4                : bit  absolute PORTB.4;
  PORTC                    : byte absolute $000E;
  PORTC_RC7                : bit  absolute PORTC.7;
  PORTC_RC6                : bit  absolute PORTC.6;
  PORTC_RC5                : bit  absolute PORTC.5;
  PORTC_RC4                : bit  absolute PORTC.4;
  PORTC_RC3                : bit  absolute PORTC.3;
  PORTC_RC2                : bit  absolute PORTC.2;
  PORTC_RC1                : bit  absolute PORTC.1;
  PORTC_RC0                : bit  absolute PORTC.0;
  PIR0                     : byte absolute $0010;
  PIR0_TMR0IF              : bit  absolute PIR0.5;
  PIR0_IOCIF               : bit  absolute PIR0.4;
  PIR0_INTF                : bit  absolute PIR0.0;
  PIR1                     : byte absolute $0011;
  PIR1_TMR1GIF             : bit  absolute PIR1.7;
  PIR1_ADIF                : bit  absolute PIR1.6;
  PIR1_RCIF                : bit  absolute PIR1.5;
  PIR1_TXIF                : bit  absolute PIR1.4;
  PIR1_SSP1IF              : bit  absolute PIR1.3;
  PIR1_BCL1IF              : bit  absolute PIR1.2;
  PIR1_TMR2IF              : bit  absolute PIR1.1;
  PIR1_TMR1IF              : bit  absolute PIR1.0;
  PIR2                     : byte absolute $0012;
  PIR2_TMR6IF              : bit  absolute PIR2.7;
  PIR2_C2IF                : bit  absolute PIR2.6;
  PIR2_C1IF                : bit  absolute PIR2.5;
  PIR2_NVMIF               : bit  absolute PIR2.4;
  PIR2_SSP2IF              : bit  absolute PIR2.3;
  PIR2_BCL2IF              : bit  absolute PIR2.2;
  PIR2_TMR4IF              : bit  absolute PIR2.1;
  PIR2_NCO1IF              : bit  absolute PIR2.0;
  PIR3                     : byte absolute $0013;
  PIR3_OSFIF               : bit  absolute PIR3.7;
  PIR3_CSWIF               : bit  absolute PIR3.6;
  PIR3_TMR3GIF             : bit  absolute PIR3.5;
  PIR3_TMR3IF              : bit  absolute PIR3.4;
  PIR3_CLC4IF              : bit  absolute PIR3.3;
  PIR3_CLC3IF              : bit  absolute PIR3.2;
  PIR3_CLC2IF              : bit  absolute PIR3.1;
  PIR3_CLC1IF              : bit  absolute PIR3.0;
  PIR4                     : byte absolute $0014;
  PIR4_CWG2IF              : bit  absolute PIR4.7;
  PIR4_CWG1IF              : bit  absolute PIR4.6;
  PIR4_TMR5GIF             : bit  absolute PIR4.5;
  PIR4_TMR5IF              : bit  absolute PIR4.4;
  PIR4_CCP4IF              : bit  absolute PIR4.3;
  PIR4_CCP3IF              : bit  absolute PIR4.2;
  PIR4_CCP2IF              : bit  absolute PIR4.1;
  PIR4_CCP1IF              : bit  absolute PIR4.0;
  TMR0L                    : byte absolute $0015;
  TMR0H                    : byte absolute $0016;
  T0CON0                   : byte absolute $0017;
  T0CON0_T0EN              : bit  absolute T0CON0.7;
  T0CON0_T0OUT             : bit  absolute T0CON0.5;
  T0CON0_T016BIT           : bit  absolute T0CON0.4;
  T0CON0_T0OUTPS3          : bit  absolute T0CON0.3;
  T0CON0_T0OUTPS2          : bit  absolute T0CON0.2;
  T0CON0_T0OUTPS1          : bit  absolute T0CON0.1;
  T0CON0_T0OUTPS0          : bit  absolute T0CON0.0;
  T0CON1                   : byte absolute $0018;
  T0CON1_T0CS2             : bit  absolute T0CON1.7;
  T0CON1_T0CS1             : bit  absolute T0CON1.6;
  T0CON1_T0CS0             : bit  absolute T0CON1.5;
  T0CON1_T0ASYNC           : bit  absolute T0CON1.4;
  T0CON1_T0CKPS3           : bit  absolute T0CON1.3;
  T0CON1_T0CKPS2           : bit  absolute T0CON1.2;
  T0CON1_T0CKPS1           : bit  absolute T0CON1.1;
  T0CON1_T0CKPS0           : bit  absolute T0CON1.0;
  TMR1L                    : byte absolute $0019;
  TMR1H                    : byte absolute $001A;
  T1CON                    : byte absolute $001B;
  T1CON_TMR1CS1            : bit  absolute T1CON.7;
  T1CON_TMR1CS0            : bit  absolute T1CON.6;
  T1CON_T1CKPS1            : bit  absolute T1CON.5;
  T1CON_T1CKPS0            : bit  absolute T1CON.4;
  T1CON_T1SOSC             : bit  absolute T1CON.3;
  T1CON_T1SYNC             : bit  absolute T1CON.2;
  T1CON_TMR1ON             : bit  absolute T1CON.0;
  T1GCON                   : byte absolute $001C;
  T1GCON_TMR1GE            : bit  absolute T1GCON.7;
  T1GCON_T1GPOL            : bit  absolute T1GCON.6;
  T1GCON_T1GTM             : bit  absolute T1GCON.5;
  T1GCON_T1GSPM            : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE       : bit  absolute T1GCON.3;
  T1GCON_T1GVAL            : bit  absolute T1GCON.2;
  T1GCON_T1GSS1            : bit  absolute T1GCON.1;
  T1GCON_T1GSS0            : bit  absolute T1GCON.0;
  TMR2                     : byte absolute $001D;
  PR2                      : byte absolute $001E;
  T2CON                    : byte absolute $001F;
  T2CON_T2OUTPS3           : bit  absolute T2CON.6;
  T2CON_T2OUTPS2           : bit  absolute T2CON.5;
  T2CON_T2OUTPS1           : bit  absolute T2CON.4;
  T2CON_T2OUTPS0           : bit  absolute T2CON.3;
  T2CON_TMR2ON             : bit  absolute T2CON.2;
  T2CON_T2CKPS1            : bit  absolute T2CON.1;
  T2CON_T2CKPS0            : bit  absolute T2CON.0;
  TRISA                    : byte absolute $008C;
  TRISA_TRISA5             : bit  absolute TRISA.5;
  TRISA_TRISA4             : bit  absolute TRISA.4;
  TRISA_TRISA2             : bit  absolute TRISA.2;
  TRISA_TRISA1             : bit  absolute TRISA.1;
  TRISA_TRISA0             : bit  absolute TRISA.0;
  TRISB                    : byte absolute $008D;
  TRISB_TRISB7             : bit  absolute TRISB.7;
  TRISB_TRISB6             : bit  absolute TRISB.6;
  TRISB_TRISB5             : bit  absolute TRISB.5;
  TRISB_TRISB4             : bit  absolute TRISB.4;
  TRISC                    : byte absolute $008E;
  TRISC_TRISC7             : bit  absolute TRISC.7;
  TRISC_TRISC6             : bit  absolute TRISC.6;
  TRISC_TRISC5             : bit  absolute TRISC.5;
  TRISC_TRISC4             : bit  absolute TRISC.4;
  TRISC_TRISC3             : bit  absolute TRISC.3;
  TRISC_TRISC2             : bit  absolute TRISC.2;
  TRISC_TRISC1             : bit  absolute TRISC.1;
  TRISC_TRISC0             : bit  absolute TRISC.0;
  PIE0                     : byte absolute $0090;
  PIE0_TMR0IE              : bit  absolute PIE0.5;
  PIE0_IOCIE               : bit  absolute PIE0.4;
  PIE0_INTE                : bit  absolute PIE0.0;
  PIE1                     : byte absolute $0091;
  PIE1_TMR1GIE             : bit  absolute PIE1.7;
  PIE1_ADIE                : bit  absolute PIE1.6;
  PIE1_RCIE                : bit  absolute PIE1.5;
  PIE1_TXIE                : bit  absolute PIE1.4;
  PIE1_SSP1IE              : bit  absolute PIE1.3;
  PIE1_BCL1IE              : bit  absolute PIE1.2;
  PIE1_TMR2IE              : bit  absolute PIE1.1;
  PIE1_TMR1IE              : bit  absolute PIE1.0;
  PIE2                     : byte absolute $0092;
  PIE2_TMR6IE              : bit  absolute PIE2.7;
  PIE2_C2IE                : bit  absolute PIE2.6;
  PIE2_C1IE                : bit  absolute PIE2.5;
  PIE2_NVMIE               : bit  absolute PIE2.4;
  PIE2_SSP2IE              : bit  absolute PIE2.3;
  PIE2_BCL2IE              : bit  absolute PIE2.2;
  PIE2_TMR4IE              : bit  absolute PIE2.1;
  PIE2_NCO1IE              : bit  absolute PIE2.0;
  PIE3                     : byte absolute $0093;
  PIE3_OSFIE               : bit  absolute PIE3.7;
  PIE3_CSWIE               : bit  absolute PIE3.6;
  PIE3_TMR3GIE             : bit  absolute PIE3.5;
  PIE3_TMR3IE              : bit  absolute PIE3.4;
  PIE3_CLC4IE              : bit  absolute PIE3.3;
  PIE3_CLC3IE              : bit  absolute PIE3.2;
  PIE3_CLC2IE              : bit  absolute PIE3.1;
  PIE3_CLC1IE              : bit  absolute PIE3.0;
  PIE4                     : byte absolute $0094;
  PIE4_CWG2IE              : bit  absolute PIE4.7;
  PIE4_CWG1IE              : bit  absolute PIE4.6;
  PIE4_TMR5GIE             : bit  absolute PIE4.5;
  PIE4_TMR5IE              : bit  absolute PIE4.4;
  PIE4_CCP4IE              : bit  absolute PIE4.3;
  PIE4_CCP3IE              : bit  absolute PIE4.2;
  PIE4_CCP2IE              : bit  absolute PIE4.1;
  PIE4_CCP1IE              : bit  absolute PIE4.0;
  WDTCON                   : byte absolute $0097;
  WDTCON_WDTPS4            : bit  absolute WDTCON.5;
  WDTCON_WDTPS3            : bit  absolute WDTCON.4;
  WDTCON_WDTPS2            : bit  absolute WDTCON.3;
  WDTCON_WDTPS1            : bit  absolute WDTCON.2;
  WDTCON_WDTPS0            : bit  absolute WDTCON.1;
  WDTCON_SWDTEN            : bit  absolute WDTCON.0;
  ADRESL                   : byte absolute $009B;
  ADRESH                   : byte absolute $009C;
  ADCON0                   : byte absolute $009D;
  ADCON0_CHS5              : bit  absolute ADCON0.7;
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
  ADACT                    : byte absolute $009F;
  ADACT_ADACT4             : bit  absolute ADACT.4;
  ADACT_ADACT3             : bit  absolute ADACT.3;
  ADACT_ADACT2             : bit  absolute ADACT.2;
  ADACT_ADACT1             : bit  absolute ADACT.1;
  ADACT_ADACT0             : bit  absolute ADACT.0;
  LATA                     : byte absolute $010C;
  LATA_LATA5               : bit  absolute LATA.5;
  LATA_LATA4               : bit  absolute LATA.4;
  LATA_LATA2               : bit  absolute LATA.2;
  LATA_LATA1               : bit  absolute LATA.1;
  LATA_LATA0               : bit  absolute LATA.0;
  LATB                     : byte absolute $010D;
  LATB_LATB7               : bit  absolute LATB.7;
  LATB_LATB6               : bit  absolute LATB.6;
  LATB_LATB5               : bit  absolute LATB.5;
  LATB_LATB4               : bit  absolute LATB.4;
  LATC                     : byte absolute $010E;
  LATC_LATC7               : bit  absolute LATC.7;
  LATC_LATC6               : bit  absolute LATC.6;
  LATC_LATC5               : bit  absolute LATC.5;
  LATC_LATC4               : bit  absolute LATC.4;
  LATC_LATC3               : bit  absolute LATC.3;
  LATC_LATC2               : bit  absolute LATC.2;
  LATC_LATC1               : bit  absolute LATC.1;
  LATC_LATC0               : bit  absolute LATC.0;
  CM1CON0                  : byte absolute $0111;
  CM1CON0_C1ON             : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT            : bit  absolute CM1CON0.6;
  CM1CON0_C1POL            : bit  absolute CM1CON0.4;
  CM1CON0_C1SP             : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS            : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC           : bit  absolute CM1CON0.0;
  CM1CON1                  : byte absolute $0112;
  CM1CON1_C1INTP           : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN           : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH2           : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH1           : bit  absolute CM1CON1.4;
  CM1CON1_C1PCH0           : bit  absolute CM1CON1.3;
  CM1CON1_C1NCH2           : bit  absolute CM1CON1.2;
  CM1CON1_C1NCH1           : bit  absolute CM1CON1.1;
  CM1CON1_C1NCH0           : bit  absolute CM1CON1.0;
  CM2CON0                  : byte absolute $0113;
  CM2CON0_C2ON             : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT            : bit  absolute CM2CON0.6;
  CM2CON0_C2POL            : bit  absolute CM2CON0.4;
  CM2CON0_C2SP             : bit  absolute CM2CON0.2;
  CM2CON0_C2HYS            : bit  absolute CM2CON0.1;
  CM2CON0_C2SYNC           : bit  absolute CM2CON0.0;
  CM2CON1                  : byte absolute $0114;
  CM2CON1_C2INTP           : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN           : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH2           : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH1           : bit  absolute CM2CON1.4;
  CM2CON1_C2PCH0           : bit  absolute CM2CON1.3;
  CM2CON1_C2NCH2           : bit  absolute CM2CON1.2;
  CM2CON1_C2NCH1           : bit  absolute CM2CON1.1;
  CM2CON1_C2NCH0           : bit  absolute CM2CON1.0;
  CMOUT                    : byte absolute $0115;
  CMOUT_MC2OUT             : bit  absolute CMOUT.1;
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
  DACCON0_DAC1EN           : bit  absolute DACCON0.7;
  DACCON0_DAC1OE           : bit  absolute DACCON0.5;
  DACCON0_DAC1PSS1         : bit  absolute DACCON0.3;
  DACCON0_DAC1PSS0         : bit  absolute DACCON0.2;
  DACCON0_DAC1NSS          : bit  absolute DACCON0.0;
  DACCON1                  : byte absolute $0119;
  DACCON1_DAC1R4           : bit  absolute DACCON1.4;
  DACCON1_DAC1R3           : bit  absolute DACCON1.3;
  DACCON1_DAC1R2           : bit  absolute DACCON1.2;
  DACCON1_DAC1R1           : bit  absolute DACCON1.1;
  DACCON1_DAC1R0           : bit  absolute DACCON1.0;
  ANSELA                   : byte absolute $018C;
  ANSELA_ANSA5             : bit  absolute ANSELA.5;
  ANSELA_ANSA4             : bit  absolute ANSELA.4;
  ANSELA_ANSA2             : bit  absolute ANSELA.2;
  ANSELA_ANSA1             : bit  absolute ANSELA.1;
  ANSELA_ANSA0             : bit  absolute ANSELA.0;
  ANSELB                   : byte absolute $018D;
  ANSELB_ANSB7             : bit  absolute ANSELB.7;
  ANSELB_ANSB6             : bit  absolute ANSELB.6;
  ANSELB_ANSB5             : bit  absolute ANSELB.5;
  ANSELB_ANSB4             : bit  absolute ANSELB.4;
  ANSELC                   : byte absolute $018E;
  ANSELC_ANSC7             : bit  absolute ANSELC.7;
  ANSELC_ANSC6             : bit  absolute ANSELC.6;
  ANSELC_ANSC5             : bit  absolute ANSELC.5;
  ANSELC_ANSC4             : bit  absolute ANSELC.4;
  ANSELC_ANSC3             : bit  absolute ANSELC.3;
  ANSELC_ANSC2             : bit  absolute ANSELC.2;
  ANSELC_ANSC1             : bit  absolute ANSELC.1;
  ANSELC_ANSC0             : bit  absolute ANSELC.0;
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
  TX1STA_SYNC              : bit  absolute TX1STA.4;
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
  WPUB                     : byte absolute $020D;
  WPUB_WPUB7               : bit  absolute WPUB.7;
  WPUB_WPUB6               : bit  absolute WPUB.6;
  WPUB_WPUB5               : bit  absolute WPUB.5;
  WPUB_WPUB4               : bit  absolute WPUB.4;
  WPUC                     : byte absolute $020E;
  WPUC_WPUC7               : bit  absolute WPUC.7;
  WPUC_WPUC6               : bit  absolute WPUC.6;
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
  SSP2BUF                  : byte absolute $0219;
  SSP2BUF_SSP2BUF7         : bit  absolute SSP2BUF.7;
  SSP2BUF_SSP2BUF6         : bit  absolute SSP2BUF.6;
  SSP2BUF_SSP2BUF5         : bit  absolute SSP2BUF.5;
  SSP2BUF_SSP2BUF4         : bit  absolute SSP2BUF.4;
  SSP2BUF_SSP2BUF3         : bit  absolute SSP2BUF.3;
  SSP2BUF_SSP2BUF2         : bit  absolute SSP2BUF.2;
  SSP2BUF_SSP2BUF1         : bit  absolute SSP2BUF.1;
  SSP2BUF_SSP2BUF0         : bit  absolute SSP2BUF.0;
  SSP2ADD                  : byte absolute $021A;
  SSP2ADD_SSP2ADD7         : bit  absolute SSP2ADD.7;
  SSP2ADD_SSP2ADD6         : bit  absolute SSP2ADD.6;
  SSP2ADD_SSP2ADD5         : bit  absolute SSP2ADD.5;
  SSP2ADD_SSP2ADD4         : bit  absolute SSP2ADD.4;
  SSP2ADD_SSP2ADD3         : bit  absolute SSP2ADD.3;
  SSP2ADD_SSP2ADD2         : bit  absolute SSP2ADD.2;
  SSP2ADD_SSP2ADD1         : bit  absolute SSP2ADD.1;
  SSP2ADD_SSP2ADD0         : bit  absolute SSP2ADD.0;
  SSP2MSK                  : byte absolute $021B;
  SSP2MSK_SSP2MSK7         : bit  absolute SSP2MSK.7;
  SSP2MSK_SSP2MSK6         : bit  absolute SSP2MSK.6;
  SSP2MSK_SSP2MSK5         : bit  absolute SSP2MSK.5;
  SSP2MSK_SSP2MSK4         : bit  absolute SSP2MSK.4;
  SSP2MSK_SSP2MSK3         : bit  absolute SSP2MSK.3;
  SSP2MSK_SSP2MSK2         : bit  absolute SSP2MSK.2;
  SSP2MSK_SSP2MSK1         : bit  absolute SSP2MSK.1;
  SSP2MSK_SSP2MSK0         : bit  absolute SSP2MSK.0;
  SSP2STAT                 : byte absolute $021C;
  SSP2CON1                 : byte absolute $021D;
  SSP2CON2                 : byte absolute $021E;
  SSP2CON3                 : byte absolute $021F;
  ODCONA                   : byte absolute $028C;
  ODCONA_ODCA5             : bit  absolute ODCONA.5;
  ODCONA_ODCA4             : bit  absolute ODCONA.4;
  ODCONA_ODCA2             : bit  absolute ODCONA.2;
  ODCONA_ODCA1             : bit  absolute ODCONA.1;
  ODCONA_ODCA0             : bit  absolute ODCONA.0;
  ODCONB                   : byte absolute $028D;
  ODCONB_ODCB7             : bit  absolute ODCONB.7;
  ODCONB_ODCB6             : bit  absolute ODCONB.6;
  ODCONB_ODCB5             : bit  absolute ODCONB.5;
  ODCONB_ODCB4             : bit  absolute ODCONB.4;
  ODCONC                   : byte absolute $028E;
  ODCONC_ODCC7             : bit  absolute ODCONC.7;
  ODCONC_ODCC6             : bit  absolute ODCONC.6;
  ODCONC_ODCC5             : bit  absolute ODCONC.5;
  ODCONC_ODCC4             : bit  absolute ODCONC.4;
  ODCONC_ODCC3             : bit  absolute ODCONC.3;
  ODCONC_ODCC2             : bit  absolute ODCONC.2;
  ODCONC_ODCC1             : bit  absolute ODCONC.1;
  ODCONC_ODCC0             : bit  absolute ODCONC.0;
  CCPR1L                   : byte absolute $0291;
  CCPR1H                   : byte absolute $0292;
  CCP1CON                  : byte absolute $0293;
  CCP1CON_CCP1EN           : bit  absolute CCP1CON.7;
  CCP1CON_CCP1OUT          : bit  absolute CCP1CON.5;
  CCP1CON_CCP1FMT          : bit  absolute CCP1CON.4;
  CCP1CON_CCP1MODE3        : bit  absolute CCP1CON.3;
  CCP1CON_CCP1MODE2        : bit  absolute CCP1CON.2;
  CCP1CON_CCP1MODE1        : bit  absolute CCP1CON.1;
  CCP1CON_CCP1MODE0        : bit  absolute CCP1CON.0;
  CCP1CAP                  : byte absolute $0294;
  CCP1CAP_CCP1CTS3         : bit  absolute CCP1CAP.3;
  CCP1CAP_CCP1CTS2         : bit  absolute CCP1CAP.2;
  CCP1CAP_CCP1CTS1         : bit  absolute CCP1CAP.1;
  CCP1CAP_CCP1CTS0         : bit  absolute CCP1CAP.0;
  CCPR2L                   : byte absolute $0295;
  CCPR2H                   : byte absolute $0296;
  CCP2CON                  : byte absolute $0297;
  CCP2CON_CCP2EN           : bit  absolute CCP2CON.7;
  CCP2CON_CCP2OUT          : bit  absolute CCP2CON.5;
  CCP2CON_CCP2FMT          : bit  absolute CCP2CON.4;
  CCP2CON_CCP2MODE3        : bit  absolute CCP2CON.3;
  CCP2CON_CCP2MODE2        : bit  absolute CCP2CON.2;
  CCP2CON_CCP2MODE1        : bit  absolute CCP2CON.1;
  CCP2CON_CCP2MODE0        : bit  absolute CCP2CON.0;
  CCP2CAP                  : byte absolute $0298;
  CCP2CAP_CCP2CTS3         : bit  absolute CCP2CAP.3;
  CCP2CAP_CCP2CTS2         : bit  absolute CCP2CAP.2;
  CCP2CAP_CCP2CTS1         : bit  absolute CCP2CAP.1;
  CCP2CAP_CCP2CTS0         : bit  absolute CCP2CAP.0;
  CCPTMRS                  : byte absolute $029F;
  CCPTMRS_C4TSEL1          : bit  absolute CCPTMRS.7;
  CCPTMRS_C4TSEL0          : bit  absolute CCPTMRS.6;
  CCPTMRS_C3TSEL1          : bit  absolute CCPTMRS.5;
  CCPTMRS_C3TSEL0          : bit  absolute CCPTMRS.4;
  CCPTMRS_C2TSEL1          : bit  absolute CCPTMRS.3;
  CCPTMRS_C2TSEL0          : bit  absolute CCPTMRS.2;
  CCPTMRS_C1TSEL1          : bit  absolute CCPTMRS.1;
  CCPTMRS_C1TSEL0          : bit  absolute CCPTMRS.0;
  SLRCONA                  : byte absolute $030C;
  SLRCONA_SLRA5            : bit  absolute SLRCONA.5;
  SLRCONA_SLRA4            : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2            : bit  absolute SLRCONA.2;
  SLRCONA_SLRA1            : bit  absolute SLRCONA.1;
  SLRCONA_SLRA0            : bit  absolute SLRCONA.0;
  SLRCONB                  : byte absolute $030D;
  SLRCONB_SLRB7            : bit  absolute SLRCONB.7;
  SLRCONB_SLRB6            : bit  absolute SLRCONB.6;
  SLRCONB_SLRB5            : bit  absolute SLRCONB.5;
  SLRCONB_SLRB4            : bit  absolute SLRCONB.4;
  SLRCONC                  : byte absolute $030E;
  SLRCONC_SLRC7            : bit  absolute SLRCONC.7;
  SLRCONC_SLRC6            : bit  absolute SLRCONC.6;
  SLRCONC_SLRC5            : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4            : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3            : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2            : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1            : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0            : bit  absolute SLRCONC.0;
  CCPR3L                   : byte absolute $0311;
  CCPR3H                   : byte absolute $0312;
  CCP3CON                  : byte absolute $0313;
  CCP3CON_CCP3EN           : bit  absolute CCP3CON.7;
  CCP3CON_CCP3OUT          : bit  absolute CCP3CON.5;
  CCP3CON_CCP3FMT          : bit  absolute CCP3CON.4;
  CCP3CON_CCP3MODE3        : bit  absolute CCP3CON.3;
  CCP3CON_CCP3MODE2        : bit  absolute CCP3CON.2;
  CCP3CON_CCP3MODE1        : bit  absolute CCP3CON.1;
  CCP3CON_CCP3MODE0        : bit  absolute CCP3CON.0;
  CCP3CAP                  : byte absolute $0314;
  CCP3CAP_CCP3CTS3         : bit  absolute CCP3CAP.3;
  CCP3CAP_CCP3CTS2         : bit  absolute CCP3CAP.2;
  CCP3CAP_CCP3CTS1         : bit  absolute CCP3CAP.1;
  CCP3CAP_CCP3CTS0         : bit  absolute CCP3CAP.0;
  CCPR4L                   : byte absolute $0315;
  CCPR4H                   : byte absolute $0316;
  CCP4CON                  : byte absolute $0317;
  CCP4CON_CCP4EN           : bit  absolute CCP4CON.7;
  CCP4CON_CCP4OUT          : bit  absolute CCP4CON.5;
  CCP4CON_CCP4FMT          : bit  absolute CCP4CON.4;
  CCP4CON_CCP4MODE3        : bit  absolute CCP4CON.3;
  CCP4CON_CCP4MODE2        : bit  absolute CCP4CON.2;
  CCP4CON_CCP4MODE1        : bit  absolute CCP4CON.1;
  CCP4CON_CCP4MODE0        : bit  absolute CCP4CON.0;
  CCP4CAP                  : byte absolute $0318;
  CCP4CAP_CCP4CTS3         : bit  absolute CCP4CAP.3;
  CCP4CAP_CCP4CTS2         : bit  absolute CCP4CAP.2;
  CCP4CAP_CCP4CTS1         : bit  absolute CCP4CAP.1;
  CCP4CAP_CCP4CTS0         : bit  absolute CCP4CAP.0;
  INLVLA                   : byte absolute $038C;
  INLVLA_INLVLA5           : bit  absolute INLVLA.5;
  INLVLA_INLVLA4           : bit  absolute INLVLA.4;
  INLVLA_INLVLA3           : bit  absolute INLVLA.3;
  INLVLA_INLVLA2           : bit  absolute INLVLA.2;
  INLVLA_INLVLA1           : bit  absolute INLVLA.1;
  INLVLA_INLVLA0           : bit  absolute INLVLA.0;
  INLVLB                   : byte absolute $038D;
  INLVLB_INLVLB7           : bit  absolute INLVLB.7;
  INLVLB_INLVLB6           : bit  absolute INLVLB.6;
  INLVLB_INLVLB5           : bit  absolute INLVLB.5;
  INLVLB_INLVLB4           : bit  absolute INLVLB.4;
  INLVLC                   : byte absolute $038E;
  INLVLC_INLVLC7           : bit  absolute INLVLC.7;
  INLVLC_INLVLC6           : bit  absolute INLVLC.6;
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
  IOCBP                    : byte absolute $0394;
  IOCBP_IOCBP7             : bit  absolute IOCBP.7;
  IOCBP_IOCBP6             : bit  absolute IOCBP.6;
  IOCBP_IOCBP5             : bit  absolute IOCBP.5;
  IOCBP_IOCBP4             : bit  absolute IOCBP.4;
  IOCBN                    : byte absolute $0395;
  IOCBN_IOCBN7             : bit  absolute IOCBN.7;
  IOCBN_IOCBN6             : bit  absolute IOCBN.6;
  IOCBN_IOCBN5             : bit  absolute IOCBN.5;
  IOCBN_IOCBN4             : bit  absolute IOCBN.4;
  IOCBF                    : byte absolute $0396;
  IOCBF_IOCBF7             : bit  absolute IOCBF.7;
  IOCBF_IOCBF6             : bit  absolute IOCBF.6;
  IOCBF_IOCBF5             : bit  absolute IOCBF.5;
  IOCBF_IOCBF4             : bit  absolute IOCBF.4;
  IOCCP                    : byte absolute $0397;
  IOCCP_IOCCP7             : bit  absolute IOCCP.7;
  IOCCP_IOCCP6             : bit  absolute IOCCP.6;
  IOCCP_IOCCP5             : bit  absolute IOCCP.5;
  IOCCP_IOCCP4             : bit  absolute IOCCP.4;
  IOCCP_IOCCP3             : bit  absolute IOCCP.3;
  IOCCP_IOCCP2             : bit  absolute IOCCP.2;
  IOCCP_IOCCP1             : bit  absolute IOCCP.1;
  IOCCP_IOCCP0             : bit  absolute IOCCP.0;
  IOCCN                    : byte absolute $0398;
  IOCCN_IOCCN7             : bit  absolute IOCCN.7;
  IOCCN_IOCCN6             : bit  absolute IOCCN.6;
  IOCCN_IOCCN5             : bit  absolute IOCCN.5;
  IOCCN_IOCCN4             : bit  absolute IOCCN.4;
  IOCCN_IOCCN3             : bit  absolute IOCCN.3;
  IOCCN_IOCCN2             : bit  absolute IOCCN.2;
  IOCCN_IOCCN1             : bit  absolute IOCCN.1;
  IOCCN_IOCCN0             : bit  absolute IOCCN.0;
  IOCCF                    : byte absolute $0399;
  IOCCF_IOCCF7             : bit  absolute IOCCF.7;
  IOCCF_IOCCF6             : bit  absolute IOCCF.6;
  IOCCF_IOCCF5             : bit  absolute IOCCF.5;
  IOCCF_IOCCF4             : bit  absolute IOCCF.4;
  IOCCF_IOCCF3             : bit  absolute IOCCF.3;
  IOCCF_IOCCF2             : bit  absolute IOCCF.2;
  IOCCF_IOCCF1             : bit  absolute IOCCF.1;
  IOCCF_IOCCF0             : bit  absolute IOCCF.0;
  CLKRCON                  : byte absolute $039A;
  CLKRCON_CLKREN           : bit  absolute CLKRCON.7;
  CLKRCON_CLKRDC1          : bit  absolute CLKRCON.4;
  CLKRCON_CLKRDC0          : bit  absolute CLKRCON.3;
  CLKRCON_CLKRDIV2         : bit  absolute CLKRCON.2;
  CLKRCON_CLKRDIV1         : bit  absolute CLKRCON.1;
  CLKRCON_CLKRDIV0         : bit  absolute CLKRCON.0;
  MDCON                    : byte absolute $039C;
  MDCON_MDEN               : bit  absolute MDCON.7;
  MDCON_MDOPOL             : bit  absolute MDCON.4;
  MDCON_MDOUT              : bit  absolute MDCON.3;
  MDCON_MDBIT              : bit  absolute MDCON.0;
  MDSRC                    : byte absolute $039D;
  MDSRC_MDMS3              : bit  absolute MDSRC.3;
  MDSRC_MDMS2              : bit  absolute MDSRC.2;
  MDSRC_MDMS1              : bit  absolute MDSRC.1;
  MDSRC_MDMS0              : bit  absolute MDSRC.0;
  MDCARH                   : byte absolute $039E;
  MDCARH_MDCHPOL           : bit  absolute MDCARH.6;
  MDCARH_MDCHSYNC          : bit  absolute MDCARH.5;
  MDCARH_MDCH3             : bit  absolute MDCARH.3;
  MDCARH_MDCH2             : bit  absolute MDCARH.2;
  MDCARH_MDCH1             : bit  absolute MDCARH.1;
  MDCARH_MDCH0             : bit  absolute MDCARH.0;
  MDCARL                   : byte absolute $039F;
  MDCARL_MDCLPOL           : bit  absolute MDCARL.6;
  MDCARL_MDCLSYNC          : bit  absolute MDCARL.5;
  MDCARL_MDCL3             : bit  absolute MDCARL.3;
  MDCARL_MDCL2             : bit  absolute MDCARL.2;
  MDCARL_MDCL1             : bit  absolute MDCARL.1;
  MDCARL_MDCL0             : bit  absolute MDCARL.0;
  TMR3L                    : byte absolute $0411;
  TMR3H                    : byte absolute $0412;
  T3CON                    : byte absolute $0413;
  T3CON_TMR3CS1            : bit  absolute T3CON.7;
  T3CON_TMR3CS0            : bit  absolute T3CON.6;
  T3CON_T3CKPS1            : bit  absolute T3CON.5;
  T3CON_T3CKPS0            : bit  absolute T3CON.4;
  T3CON_T3SOSC             : bit  absolute T3CON.3;
  T3CON_T3SYNC             : bit  absolute T3CON.2;
  T3CON_TMR3ON             : bit  absolute T3CON.0;
  T3GCON                   : byte absolute $0414;
  T3GCON_TMR3GE            : bit  absolute T3GCON.7;
  T3GCON_T3GPOL            : bit  absolute T3GCON.6;
  T3GCON_T3GTM             : bit  absolute T3GCON.5;
  T3GCON_T3GSPM            : bit  absolute T3GCON.4;
  T3GCON_T3GGO_nDONE       : bit  absolute T3GCON.3;
  T3GCON_T3GVAL            : bit  absolute T3GCON.2;
  T3GCON_T3GSS1            : bit  absolute T3GCON.1;
  T3GCON_T3GSS0            : bit  absolute T3GCON.0;
  TMR4                     : byte absolute $0415;
  PR4                      : byte absolute $0416;
  T4CON                    : byte absolute $0417;
  T4CON_T4OUTPS3           : bit  absolute T4CON.6;
  T4CON_T4OUTPS2           : bit  absolute T4CON.5;
  T4CON_T4OUTPS1           : bit  absolute T4CON.4;
  T4CON_T4OUTPS0           : bit  absolute T4CON.3;
  T4CON_TMR4ON             : bit  absolute T4CON.2;
  T4CON_T4CKPS1            : bit  absolute T4CON.1;
  T4CON_T4CKPS0            : bit  absolute T4CON.0;
  TMR5L                    : byte absolute $0418;
  TMR5H                    : byte absolute $0419;
  T5CON                    : byte absolute $041A;
  T5CON_TMR5CS1            : bit  absolute T5CON.7;
  T5CON_TMR5CS0            : bit  absolute T5CON.6;
  T5CON_T5CKPS1            : bit  absolute T5CON.5;
  T5CON_T5CKPS0            : bit  absolute T5CON.4;
  T5CON_T5SOSC             : bit  absolute T5CON.3;
  T5CON_T5SYNC             : bit  absolute T5CON.2;
  T5CON_TMR5ON             : bit  absolute T5CON.0;
  T5GCON                   : byte absolute $041B;
  T5GCON_TMR5GE            : bit  absolute T5GCON.7;
  T5GCON_T5GPOL            : bit  absolute T5GCON.6;
  T5GCON_T5GTM             : bit  absolute T5GCON.5;
  T5GCON_T5GSPM            : bit  absolute T5GCON.4;
  T5GCON_T5GGO_nDONE       : bit  absolute T5GCON.3;
  T5GCON_T5GVAL            : bit  absolute T5GCON.2;
  T5GCON_T5GSS1            : bit  absolute T5GCON.1;
  T5GCON_T5GSS0            : bit  absolute T5GCON.0;
  TMR6                     : byte absolute $041C;
  PR6                      : byte absolute $041D;
  T6CON                    : byte absolute $041E;
  T6CON_T6OUTPS3           : bit  absolute T6CON.6;
  T6CON_T6OUTPS2           : bit  absolute T6CON.5;
  T6CON_T6OUTPS1           : bit  absolute T6CON.4;
  T6CON_T6OUTPS0           : bit  absolute T6CON.3;
  T6CON_TMR6ON             : bit  absolute T6CON.2;
  T6CON_T6CKPS1            : bit  absolute T6CON.1;
  T6CON_T6CKPS0            : bit  absolute T6CON.0;
  CCDCON                   : byte absolute $041F;
  CCDCON_CCDEN             : bit  absolute CCDCON.7;
  CCDCON_CCDS1             : bit  absolute CCDCON.1;
  CCDCON_CCDS0             : bit  absolute CCDCON.0;
  NCO1ACCL                 : byte absolute $0498;
  NCO1ACCH                 : byte absolute $0499;
  NCO1ACCU                 : byte absolute $049A;
  NCO1ACCU_NCO1ACC3        : bit  absolute NCO1ACCU.3;
  NCO1ACCU_NCO1ACC2        : bit  absolute NCO1ACCU.2;
  NCO1ACCU_NCO1ACC1        : bit  absolute NCO1ACCU.1;
  NCO1ACCU_NCO1ACC0        : bit  absolute NCO1ACCU.0;
  NCO1INCL                 : byte absolute $049B;
  NCO1INCH                 : byte absolute $049C;
  NCO1INCU                 : byte absolute $049D;
  NCO1INCU_NCO1INC3        : bit  absolute NCO1INCU.3;
  NCO1INCU_NCO1INC2        : bit  absolute NCO1INCU.2;
  NCO1INCU_NCO1INC1        : bit  absolute NCO1INCU.1;
  NCO1INCU_NCO1INC0        : bit  absolute NCO1INCU.0;
  NCO1CON                  : byte absolute $049E;
  NCO1CON_N1EN             : bit  absolute NCO1CON.7;
  NCO1CON_N1OUT            : bit  absolute NCO1CON.5;
  NCO1CON_N1POL            : bit  absolute NCO1CON.4;
  NCO1CON_N1PFM            : bit  absolute NCO1CON.0;
  NCO1CLK                  : byte absolute $049F;
  NCO1CLK_N1PWS2           : bit  absolute NCO1CLK.7;
  NCO1CLK_N1PWS1           : bit  absolute NCO1CLK.6;
  NCO1CLK_N1PWS0           : bit  absolute NCO1CLK.5;
  NCO1CLK_N1CKS1           : bit  absolute NCO1CLK.1;
  NCO1CLK_N1CKS0           : bit  absolute NCO1CLK.0;
  PWM5DCL                  : byte absolute $0617;
  PWM5DCL_PWM5DCL1         : bit  absolute PWM5DCL.7;
  PWM5DCL_PWM5DCL0         : bit  absolute PWM5DCL.6;
  PWM5DCH                  : byte absolute $0618;
  PWM5CON                  : byte absolute $0619;
  PWM5CON_PWM5EN           : bit  absolute PWM5CON.7;
  PWM5CON_PWM5OUT          : bit  absolute PWM5CON.5;
  PWM5CON_PWM5POL          : bit  absolute PWM5CON.4;
  PWM6DCL                  : byte absolute $061A;
  PWM6DCL_PWM6DCL1         : bit  absolute PWM6DCL.7;
  PWM6DCL_PWM6DCL0         : bit  absolute PWM6DCL.6;
  PWM6DCH                  : byte absolute $061B;
  PWM6CON                  : byte absolute $061C;
  PWM6CON_PWM6EN           : bit  absolute PWM6CON.7;
  PWM6CON_PWM6OUT          : bit  absolute PWM6CON.5;
  PWM6CON_PWM6POL          : bit  absolute PWM6CON.4;
  PWMTMRS                  : byte absolute $061F;
  PWMTMRS_P6TSEL1          : bit  absolute PWMTMRS.3;
  PWMTMRS_P6TSEL0          : bit  absolute PWMTMRS.2;
  PWMTMRS_P5TSEL1          : bit  absolute PWMTMRS.1;
  PWMTMRS_P5TSEL0          : bit  absolute PWMTMRS.0;
  CWG1CLKCON               : byte absolute $0691;
  CWG1CLKCON_CS            : bit  absolute CWG1CLKCON.0;
  CWG1DAT                  : byte absolute $0692;
  CWG1DAT_DAT3             : bit  absolute CWG1DAT.3;
  CWG1DAT_DAT2             : bit  absolute CWG1DAT.2;
  CWG1DAT_DAT1             : bit  absolute CWG1DAT.1;
  CWG1DAT_DAT0             : bit  absolute CWG1DAT.0;
  CWG1DBR                  : byte absolute $0693;
  CWG1DBR_DBR5             : bit  absolute CWG1DBR.5;
  CWG1DBR_DBR4             : bit  absolute CWG1DBR.4;
  CWG1DBR_DBR3             : bit  absolute CWG1DBR.3;
  CWG1DBR_DBR2             : bit  absolute CWG1DBR.2;
  CWG1DBR_DBR1             : bit  absolute CWG1DBR.1;
  CWG1DBR_DBR0             : bit  absolute CWG1DBR.0;
  CWG1DBF                  : byte absolute $0694;
  CWG1DBF_DBF5             : bit  absolute CWG1DBF.5;
  CWG1DBF_DBF4             : bit  absolute CWG1DBF.4;
  CWG1DBF_DBF3             : bit  absolute CWG1DBF.3;
  CWG1DBF_DBF2             : bit  absolute CWG1DBF.2;
  CWG1DBF_DBF1             : bit  absolute CWG1DBF.1;
  CWG1DBF_DBF0             : bit  absolute CWG1DBF.0;
  CWG1CON0                 : byte absolute $0695;
  CWG1CON0_EN              : bit  absolute CWG1CON0.7;
  CWG1CON0_LD              : bit  absolute CWG1CON0.6;
  CWG1CON0_MODE2           : bit  absolute CWG1CON0.2;
  CWG1CON0_MODE1           : bit  absolute CWG1CON0.1;
  CWG1CON0_MODE0           : bit  absolute CWG1CON0.0;
  CWG1CON1                 : byte absolute $0696;
  CWG1CON1_IN              : bit  absolute CWG1CON1.5;
  CWG1CON1_POLD            : bit  absolute CWG1CON1.3;
  CWG1CON1_POLC            : bit  absolute CWG1CON1.2;
  CWG1CON1_POLB            : bit  absolute CWG1CON1.1;
  CWG1CON1_POLA            : bit  absolute CWG1CON1.0;
  CWG1AS0                  : byte absolute $0697;
  CWG1AS0_SHUTDOWN         : bit  absolute CWG1AS0.7;
  CWG1AS0_REN              : bit  absolute CWG1AS0.6;
  CWG1AS0_LSBD1            : bit  absolute CWG1AS0.5;
  CWG1AS0_LSBD0            : bit  absolute CWG1AS0.4;
  CWG1AS0_LSAC1            : bit  absolute CWG1AS0.3;
  CWG1AS0_LSAC0            : bit  absolute CWG1AS0.2;
  CWG1AS1                  : byte absolute $0698;
  CWG1AS1_AS4E             : bit  absolute CWG1AS1.4;
  CWG1AS1_AS3E             : bit  absolute CWG1AS1.3;
  CWG1AS1_AS2E             : bit  absolute CWG1AS1.2;
  CWG1AS1_AS1E             : bit  absolute CWG1AS1.1;
  CWG1AS1_AS0E             : bit  absolute CWG1AS1.0;
  CWG1STR                  : byte absolute $0699;
  CWG1STR_OVRD             : bit  absolute CWG1STR.7;
  CWG1STR_OVRC             : bit  absolute CWG1STR.6;
  CWG1STR_OVRB             : bit  absolute CWG1STR.5;
  CWG1STR_OVRA             : bit  absolute CWG1STR.4;
  CWG1STR_STRD             : bit  absolute CWG1STR.3;
  CWG1STR_STRC             : bit  absolute CWG1STR.2;
  CWG1STR_STRB             : bit  absolute CWG1STR.1;
  CWG1STR_STRA             : bit  absolute CWG1STR.0;
  CWG2CLKCON               : byte absolute $0711;
  CWG2DAT                  : byte absolute $0712;
  CWG2DBR                  : byte absolute $0713;
  CWG2DBF                  : byte absolute $0714;
  CWG2CON0                 : byte absolute $0715;
  CWG2CON1                 : byte absolute $0716;
  CWG2AS0                  : byte absolute $0717;
  CWG2AS1                  : byte absolute $0718;
  CWG2STR                  : byte absolute $0719;
  NVMADRL                  : byte absolute $0891;
  NVMADRH                  : byte absolute $0892;
  NVMADRH_NVMADRH6         : bit  absolute NVMADRH.6;
  NVMADRH_NVMADRH5         : bit  absolute NVMADRH.5;
  NVMADRH_NVMADRH4         : bit  absolute NVMADRH.4;
  NVMADRH_NVMADRH3         : bit  absolute NVMADRH.3;
  NVMADRH_NVMADRH2         : bit  absolute NVMADRH.2;
  NVMADRH_NVMADRH1         : bit  absolute NVMADRH.1;
  NVMADRH_NVMADRH0         : bit  absolute NVMADRH.0;
  NVMDATL                  : byte absolute $0893;
  NVMDATH                  : byte absolute $0894;
  NVMDATH_NVMDATH5         : bit  absolute NVMDATH.5;
  NVMDATH_NVMDATH4         : bit  absolute NVMDATH.4;
  NVMDATH_NVMDATH3         : bit  absolute NVMDATH.3;
  NVMDATH_NVMDATH2         : bit  absolute NVMDATH.2;
  NVMDATH_NVMDATH1         : bit  absolute NVMDATH.1;
  NVMDATH_NVMDATH0         : bit  absolute NVMDATH.0;
  NVMCON1                  : byte absolute $0895;
  NVMCON1_NVMREGS          : bit  absolute NVMCON1.6;
  NVMCON1_LWLO             : bit  absolute NVMCON1.5;
  NVMCON1_FREE             : bit  absolute NVMCON1.4;
  NVMCON1_WRERR            : bit  absolute NVMCON1.3;
  NVMCON1_WREN             : bit  absolute NVMCON1.2;
  NVMCON1_WR               : bit  absolute NVMCON1.1;
  NVMCON1_RD               : bit  absolute NVMCON1.0;
  NVMCON2                  : byte absolute $0896;
  PCON0                    : byte absolute $089B;
  PCON0_STKOVF             : bit  absolute PCON0.7;
  PCON0_STKUNF             : bit  absolute PCON0.6;
  PCON0_nRWDT              : bit  absolute PCON0.4;
  PCON0_nRMCLR             : bit  absolute PCON0.3;
  PCON0_nRI                : bit  absolute PCON0.2;
  PCON0_nPOR               : bit  absolute PCON0.1;
  PCON0_nBOR               : bit  absolute PCON0.0;
  PMD0                     : byte absolute $0911;
  PMD0_SYSCMD              : bit  absolute PMD0.7;
  PMD0_FVRMD               : bit  absolute PMD0.6;
  PMD0_NVMMD               : bit  absolute PMD0.2;
  PMD0_CLKRMD              : bit  absolute PMD0.1;
  PMD0_IOCMD               : bit  absolute PMD0.0;
  PMD1                     : byte absolute $0912;
  PMD1_NCOMD               : bit  absolute PMD1.7;
  PMD1_TMR6MD              : bit  absolute PMD1.6;
  PMD1_TMR5MD              : bit  absolute PMD1.5;
  PMD1_TMR4MD              : bit  absolute PMD1.4;
  PMD1_TMR3MD              : bit  absolute PMD1.3;
  PMD1_TMR2MD              : bit  absolute PMD1.2;
  PMD1_TMR1MD              : bit  absolute PMD1.1;
  PMD1_TMR0MD              : bit  absolute PMD1.0;
  PMD2                     : byte absolute $0913;
  PMD2_DACMD               : bit  absolute PMD2.6;
  PMD2_ADCMD               : bit  absolute PMD2.5;
  PMD2_CMP2MD              : bit  absolute PMD2.2;
  PMD2_CMP1MD              : bit  absolute PMD2.1;
  PMD3                     : byte absolute $0914;
  PMD3_CWG2MD              : bit  absolute PMD3.7;
  PMD3_CWG1MD              : bit  absolute PMD3.6;
  PMD3_PWM6MD              : bit  absolute PMD3.5;
  PMD3_PWM5MD              : bit  absolute PMD3.4;
  PMD3_CCP4MD              : bit  absolute PMD3.3;
  PMD3_CCP3MD              : bit  absolute PMD3.2;
  PMD3_CCP2MD              : bit  absolute PMD3.1;
  PMD3_CCP1MD              : bit  absolute PMD3.0;
  PMD4                     : byte absolute $0915;
  PMD4_UART1MD             : bit  absolute PMD4.5;
  PMD4_MSSP2MD             : bit  absolute PMD4.2;
  PMD4_MSSP1MD             : bit  absolute PMD4.1;
  PMD5                     : byte absolute $0916;
  PMD5_CLC4MD              : bit  absolute PMD5.4;
  PMD5_CLC3MD              : bit  absolute PMD5.3;
  PMD5_CLC2MD              : bit  absolute PMD5.2;
  PMD5_CLC1MD              : bit  absolute PMD5.1;
  PMD5_DSMMD               : bit  absolute PMD5.0;
  CPUDOZE                  : byte absolute $0918;
  CPUDOZE_IDLEN            : bit  absolute CPUDOZE.7;
  CPUDOZE_DOZEN            : bit  absolute CPUDOZE.6;
  CPUDOZE_ROI              : bit  absolute CPUDOZE.5;
  CPUDOZE_DOE              : bit  absolute CPUDOZE.4;
  CPUDOZE_DOZE2            : bit  absolute CPUDOZE.2;
  CPUDOZE_DOZE1            : bit  absolute CPUDOZE.1;
  CPUDOZE_DOZE0            : bit  absolute CPUDOZE.0;
  OSCCON1                  : byte absolute $0919;
  OSCCON1_NOSC2            : bit  absolute OSCCON1.6;
  OSCCON1_NOSC1            : bit  absolute OSCCON1.5;
  OSCCON1_NOSC0            : bit  absolute OSCCON1.4;
  OSCCON1_NDIV3            : bit  absolute OSCCON1.3;
  OSCCON1_NDIV2            : bit  absolute OSCCON1.2;
  OSCCON1_NDIV1            : bit  absolute OSCCON1.1;
  OSCCON1_NDIV0            : bit  absolute OSCCON1.0;
  OSCCON2                  : byte absolute $091A;
  OSCCON2_COSC2            : bit  absolute OSCCON2.6;
  OSCCON2_COSC1            : bit  absolute OSCCON2.5;
  OSCCON2_COSC0            : bit  absolute OSCCON2.4;
  OSCCON2_CDIV3            : bit  absolute OSCCON2.3;
  OSCCON2_CDIV2            : bit  absolute OSCCON2.2;
  OSCCON2_CDIV1            : bit  absolute OSCCON2.1;
  OSCCON2_CDIV0            : bit  absolute OSCCON2.0;
  OSCCON3                  : byte absolute $091B;
  OSCCON3_CSWHOLD          : bit  absolute OSCCON3.7;
  OSCCON3_SOSCPWR          : bit  absolute OSCCON3.6;
  OSCCON3_SOSCBE           : bit  absolute OSCCON3.5;
  OSCCON3_ORDY             : bit  absolute OSCCON3.4;
  OSCCON3_NOSCR            : bit  absolute OSCCON3.3;
  OSCSTAT1                 : byte absolute $091C;
  OSCSTAT1_EXTOR           : bit  absolute OSCSTAT1.7;
  OSCSTAT1_HFOR            : bit  absolute OSCSTAT1.6;
  OSCSTAT1_LFOR            : bit  absolute OSCSTAT1.4;
  OSCSTAT1_SOR             : bit  absolute OSCSTAT1.3;
  OSCSTAT1_ADOR            : bit  absolute OSCSTAT1.2;
  OSCSTAT1_PLLR            : bit  absolute OSCSTAT1.0;
  OSCEN                    : byte absolute $091D;
  OSCEN_EXTOEN             : bit  absolute OSCEN.7;
  OSCEN_HFOEN              : bit  absolute OSCEN.6;
  OSCEN_LFOEN              : bit  absolute OSCEN.4;
  OSCEN_SOSCEN             : bit  absolute OSCEN.3;
  OSCEN_ADOEN              : bit  absolute OSCEN.2;
  OSCTUNE                  : byte absolute $091E;
  OSCTUNE_HFTUN5           : bit  absolute OSCTUNE.5;
  OSCTUNE_HFTUN4           : bit  absolute OSCTUNE.4;
  OSCTUNE_HFTUN3           : bit  absolute OSCTUNE.3;
  OSCTUNE_HFTUN2           : bit  absolute OSCTUNE.2;
  OSCTUNE_HFTUN1           : bit  absolute OSCTUNE.1;
  OSCTUNE_HFTUN0           : bit  absolute OSCTUNE.0;
  OSCFRQ                   : byte absolute $091F;
  OSCFRQ_HFFRQ3            : bit  absolute OSCFRQ.3;
  OSCFRQ_HFFRQ2            : bit  absolute OSCFRQ.2;
  OSCFRQ_HFFRQ1            : bit  absolute OSCFRQ.1;
  OSCFRQ_HFFRQ0            : bit  absolute OSCFRQ.0;
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
  CCP2PPS                  : byte absolute $0E15;
  CCP2PPS_CCP2PPS4         : bit  absolute CCP2PPS.4;
  CCP2PPS_CCP2PPS3         : bit  absolute CCP2PPS.3;
  CCP2PPS_CCP2PPS2         : bit  absolute CCP2PPS.2;
  CCP2PPS_CCP2PPS1         : bit  absolute CCP2PPS.1;
  CCP2PPS_CCP2PPS0         : bit  absolute CCP2PPS.0;
  CCP3PPS                  : byte absolute $0E16;
  CCP3PPS_CCP3PPS4         : bit  absolute CCP3PPS.4;
  CCP3PPS_CCP3PPS3         : bit  absolute CCP3PPS.3;
  CCP3PPS_CCP3PPS2         : bit  absolute CCP3PPS.2;
  CCP3PPS_CCP3PPS1         : bit  absolute CCP3PPS.1;
  CCP3PPS_CCP3PPS0         : bit  absolute CCP3PPS.0;
  CCP4PPS                  : byte absolute $0E17;
  CCP4PPS_CCP4PPS4         : bit  absolute CCP4PPS.4;
  CCP4PPS_CCP4PPS3         : bit  absolute CCP4PPS.3;
  CCP4PPS_CCP4PPS2         : bit  absolute CCP4PPS.2;
  CCP4PPS_CCP4PPS1         : bit  absolute CCP4PPS.1;
  CCP4PPS_CCP4PPS0         : bit  absolute CCP4PPS.0;
  CWG1PPS                  : byte absolute $0E18;
  CWG1PPS_CWG1PPS4         : bit  absolute CWG1PPS.4;
  CWG1PPS_CWG1PPS3         : bit  absolute CWG1PPS.3;
  CWG1PPS_CWG1PPS2         : bit  absolute CWG1PPS.2;
  CWG1PPS_CWG1PPS1         : bit  absolute CWG1PPS.1;
  CWG1PPS_CWG1PPS0         : bit  absolute CWG1PPS.0;
  CWG2PPS                  : byte absolute $0E19;
  CWG2PPS_CWG2PPS4         : bit  absolute CWG2PPS.4;
  CWG2PPS_CWG2PPS3         : bit  absolute CWG2PPS.3;
  CWG2PPS_CWG2PPS2         : bit  absolute CWG2PPS.2;
  CWG2PPS_CWG2PPS1         : bit  absolute CWG2PPS.1;
  CWG2PPS_CWG2PPS0         : bit  absolute CWG2PPS.0;
  MDCIN1PPS                : byte absolute $0E1A;
  MDCIN1PPS_MDCIN1PPS4     : bit  absolute MDCIN1PPS.4;
  MDCIN1PPS_MDCIN1PPS3     : bit  absolute MDCIN1PPS.3;
  MDCIN1PPS_MDCIN1PPS2     : bit  absolute MDCIN1PPS.2;
  MDCIN1PPS_MDCIN1PPS1     : bit  absolute MDCIN1PPS.1;
  MDCIN1PPS_MDCIN1PPS0     : bit  absolute MDCIN1PPS.0;
  MDCIN2PPS                : byte absolute $0E1B;
  MDCIN2PPS_MDCIN2PPS4     : bit  absolute MDCIN2PPS.4;
  MDCIN2PPS_MDCIN2PPS3     : bit  absolute MDCIN2PPS.3;
  MDCIN2PPS_MDCIN2PPS2     : bit  absolute MDCIN2PPS.2;
  MDCIN2PPS_MDCIN2PPS1     : bit  absolute MDCIN2PPS.1;
  MDCIN2PPS_MDCIN2PPS0     : bit  absolute MDCIN2PPS.0;
  MDMINPPS                 : byte absolute $0E1C;
  MDMINPPS_MDMINPPS4       : bit  absolute MDMINPPS.4;
  MDMINPPS_MDMINPPS3       : bit  absolute MDMINPPS.3;
  MDMINPPS_MDMINPPS2       : bit  absolute MDMINPPS.2;
  MDMINPPS_MDMINPPS1       : bit  absolute MDMINPPS.1;
  MDMINPPS_MDMINPPS0       : bit  absolute MDMINPPS.0;
  SSP2CLKPPS               : byte absolute $0E1D;
  SSP2CLKPPS_SSP2CLKPPS4   : bit  absolute SSP2CLKPPS.4;
  SSP2CLKPPS_SSP2CLKPPS3   : bit  absolute SSP2CLKPPS.3;
  SSP2CLKPPS_SSP2CLKPPS2   : bit  absolute SSP2CLKPPS.2;
  SSP2CLKPPS_SSP2CLKPPS1   : bit  absolute SSP2CLKPPS.1;
  SSP2CLKPPS_SSP2CLKPPS0   : bit  absolute SSP2CLKPPS.0;
  SSP2DATPPS               : byte absolute $0E1E;
  SSP2DATPPS_SSP2DATPPS4   : bit  absolute SSP2DATPPS.4;
  SSP2DATPPS_SSP2DATPPS3   : bit  absolute SSP2DATPPS.3;
  SSP2DATPPS_SSP2DATPPS2   : bit  absolute SSP2DATPPS.2;
  SSP2DATPPS_SSP2DATPPS1   : bit  absolute SSP2DATPPS.1;
  SSP2DATPPS_SSP2DATPPS0   : bit  absolute SSP2DATPPS.0;
  SSP2SSPPS                : byte absolute $0E1F;
  SSP2SSPPS_SSP2SSPPS4     : bit  absolute SSP2SSPPS.4;
  SSP2SSPPS_SSP2SSPPS3     : bit  absolute SSP2SSPPS.3;
  SSP2SSPPS_SSP2SSPPS2     : bit  absolute SSP2SSPPS.2;
  SSP2SSPPS_SSP2SSPPS1     : bit  absolute SSP2SSPPS.1;
  SSP2SSPPS_SSP2SSPPS0     : bit  absolute SSP2SSPPS.0;
  SSP1CLKPPS               : byte absolute $0E20;
  SSP1CLKPPS_SSP1CLKPPS4   : bit  absolute SSP1CLKPPS.4;
  SSP1CLKPPS_SSP1CLKPPS3   : bit  absolute SSP1CLKPPS.3;
  SSP1CLKPPS_SSP1CLKPPS2   : bit  absolute SSP1CLKPPS.2;
  SSP1CLKPPS_SSP1CLKPPS1   : bit  absolute SSP1CLKPPS.1;
  SSP1CLKPPS_SSP1CLKPPS0   : bit  absolute SSP1CLKPPS.0;
  SSP1DATPPS               : byte absolute $0E21;
  SSP1DATPPS_SSP1DATPPS4   : bit  absolute SSP1DATPPS.4;
  SSP1DATPPS_SSP1DATPPS3   : bit  absolute SSP1DATPPS.3;
  SSP1DATPPS_SSP1DATPPS2   : bit  absolute SSP1DATPPS.2;
  SSP1DATPPS_SSP1DATPPS1   : bit  absolute SSP1DATPPS.1;
  SSP1DATPPS_SSP1DATPPS0   : bit  absolute SSP1DATPPS.0;
  SSP1SSPPS                : byte absolute $0E22;
  SSP1SSPPS_SSP1SSPPS4     : bit  absolute SSP1SSPPS.4;
  SSP1SSPPS_SSP1SSPPS3     : bit  absolute SSP1SSPPS.3;
  SSP1SSPPS_SSP1SSPPS2     : bit  absolute SSP1SSPPS.2;
  SSP1SSPPS_SSP1SSPPS1     : bit  absolute SSP1SSPPS.1;
  SSP1SSPPS_SSP1SSPPS0     : bit  absolute SSP1SSPPS.0;
  RXPPS                    : byte absolute $0E24;
  RXPPS_RXPPS4             : bit  absolute RXPPS.4;
  RXPPS_RXPPS3             : bit  absolute RXPPS.3;
  RXPPS_RXPPS2             : bit  absolute RXPPS.2;
  RXPPS_RXPPS1             : bit  absolute RXPPS.1;
  RXPPS_RXPPS0             : bit  absolute RXPPS.0;
  TXPPS                    : byte absolute $0E25;
  TXPPS_TXPPS4             : bit  absolute TXPPS.4;
  TXPPS_TXPPS3             : bit  absolute TXPPS.3;
  TXPPS_TXPPS2             : bit  absolute TXPPS.2;
  TXPPS_TXPPS1             : bit  absolute TXPPS.1;
  TXPPS_TXPPS0             : bit  absolute TXPPS.0;
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
  T3CKIPPS                 : byte absolute $0E2C;
  T3CKIPPS_T3CKIPPS4       : bit  absolute T3CKIPPS.4;
  T3CKIPPS_T3CKIPPS3       : bit  absolute T3CKIPPS.3;
  T3CKIPPS_T3CKIPPS2       : bit  absolute T3CKIPPS.2;
  T3CKIPPS_T3CKIPPS1       : bit  absolute T3CKIPPS.1;
  T3CKIPPS_T3CKIPPS0       : bit  absolute T3CKIPPS.0;
  T3GPPS                   : byte absolute $0E2D;
  T3GPPS_T3GPPS4           : bit  absolute T3GPPS.4;
  T3GPPS_T3GPPS3           : bit  absolute T3GPPS.3;
  T3GPPS_T3GPPS2           : bit  absolute T3GPPS.2;
  T3GPPS_T3GPPS1           : bit  absolute T3GPPS.1;
  T3GPPS_T3GPPS0           : bit  absolute T3GPPS.0;
  T5CKIPPS                 : byte absolute $0E2E;
  T5CKIPPS_T5CKIPPS4       : bit  absolute T5CKIPPS.4;
  T5CKIPPS_T5CKIPPS3       : bit  absolute T5CKIPPS.3;
  T5CKIPPS_T5CKIPPS2       : bit  absolute T5CKIPPS.2;
  T5CKIPPS_T5CKIPPS1       : bit  absolute T5CKIPPS.1;
  T5CKIPPS_T5CKIPPS0       : bit  absolute T5CKIPPS.0;
  T5GPPS                   : byte absolute $0E2F;
  T5GPPS_T5GPPS4           : bit  absolute T5GPPS.4;
  T5GPPS_T5GPPS3           : bit  absolute T5GPPS.3;
  T5GPPS_T5GPPS2           : bit  absolute T5GPPS.2;
  T5GPPS_T5GPPS1           : bit  absolute T5GPPS.1;
  T5GPPS_T5GPPS0           : bit  absolute T5GPPS.0;
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
  RB4PPS                   : byte absolute $0E9C;
  RB4PPS_RB4PPS4           : bit  absolute RB4PPS.4;
  RB4PPS_RB4PPS3           : bit  absolute RB4PPS.3;
  RB4PPS_RB4PPS2           : bit  absolute RB4PPS.2;
  RB4PPS_RB4PPS1           : bit  absolute RB4PPS.1;
  RB4PPS_RB4PPS0           : bit  absolute RB4PPS.0;
  RB5PPS                   : byte absolute $0E9D;
  RB5PPS_RB5PPS4           : bit  absolute RB5PPS.4;
  RB5PPS_RB5PPS3           : bit  absolute RB5PPS.3;
  RB5PPS_RB5PPS2           : bit  absolute RB5PPS.2;
  RB5PPS_RB5PPS1           : bit  absolute RB5PPS.1;
  RB5PPS_RB5PPS0           : bit  absolute RB5PPS.0;
  RB6PPS                   : byte absolute $0E9E;
  RB6PPS_RB6PPS4           : bit  absolute RB6PPS.4;
  RB6PPS_RB6PPS3           : bit  absolute RB6PPS.3;
  RB6PPS_RB6PPS2           : bit  absolute RB6PPS.2;
  RB6PPS_RB6PPS1           : bit  absolute RB6PPS.1;
  RB6PPS_RB6PPS0           : bit  absolute RB6PPS.0;
  RB7PPS                   : byte absolute $0E9F;
  RB7PPS_RB7PPS4           : bit  absolute RB7PPS.4;
  RB7PPS_RB7PPS3           : bit  absolute RB7PPS.3;
  RB7PPS_RB7PPS2           : bit  absolute RB7PPS.2;
  RB7PPS_RB7PPS1           : bit  absolute RB7PPS.1;
  RB7PPS_RB7PPS0           : bit  absolute RB7PPS.0;
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
  RC6PPS                   : byte absolute $0EA6;
  RC6PPS_RC6PPS4           : bit  absolute RC6PPS.4;
  RC6PPS_RC6PPS3           : bit  absolute RC6PPS.3;
  RC6PPS_RC6PPS2           : bit  absolute RC6PPS.2;
  RC6PPS_RC6PPS1           : bit  absolute RC6PPS.1;
  RC6PPS_RC6PPS0           : bit  absolute RC6PPS.0;
  RC7PPS                   : byte absolute $0EA7;
  RC7PPS_RC7PPS4           : bit  absolute RC7PPS.4;
  RC7PPS_RC7PPS3           : bit  absolute RC7PPS.3;
  RC7PPS_RC7PPS2           : bit  absolute RC7PPS.2;
  RC7PPS_RC7PPS1           : bit  absolute RC7PPS.1;
  RC7PPS_RC7PPS0           : bit  absolute RC7PPS.0;
  CLCDATA                  : byte absolute $0F0F;
  CLCDATA_MLC4OUT          : bit  absolute CLCDATA.3;
  CLCDATA_MLC3OUT          : bit  absolute CLCDATA.2;
  CLCDATA_MLC2OUT          : bit  absolute CLCDATA.1;
  CLCDATA_MLC1OUT          : bit  absolute CLCDATA.0;
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
  CLC1SEL0_LC1D1S5         : bit  absolute CLC1SEL0.5;
  CLC1SEL0_LC1D1S4         : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S3         : bit  absolute CLC1SEL0.3;
  CLC1SEL0_LC1D1S2         : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1         : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0         : bit  absolute CLC1SEL0.0;
  CLC1SEL1                 : byte absolute $0F13;
  CLC1SEL1_LC1D2S5         : bit  absolute CLC1SEL1.5;
  CLC1SEL1_LC1D2S4         : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D2S3         : bit  absolute CLC1SEL1.3;
  CLC1SEL1_LC1D2S2         : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D2S1         : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D2S0         : bit  absolute CLC1SEL1.0;
  CLC1SEL2                 : byte absolute $0F14;
  CLC1SEL2_LC1D3S5         : bit  absolute CLC1SEL2.5;
  CLC1SEL2_LC1D3S4         : bit  absolute CLC1SEL2.4;
  CLC1SEL2_LC1D3S3         : bit  absolute CLC1SEL2.3;
  CLC1SEL2_LC1D3S2         : bit  absolute CLC1SEL2.2;
  CLC1SEL2_LC1D3S1         : bit  absolute CLC1SEL2.1;
  CLC1SEL2_LC1D3S0         : bit  absolute CLC1SEL2.0;
  CLC1SEL3                 : byte absolute $0F15;
  CLC1SEL3_LC1D4S5         : bit  absolute CLC1SEL3.5;
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
  CLC2SEL0_LC2D1S5         : bit  absolute CLC2SEL0.5;
  CLC2SEL0_LC2D1S4         : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S3         : bit  absolute CLC2SEL0.3;
  CLC2SEL0_LC2D1S2         : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1         : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0         : bit  absolute CLC2SEL0.0;
  CLC2SEL1                 : byte absolute $0F1D;
  CLC2SEL1_LC2D2S5         : bit  absolute CLC2SEL1.5;
  CLC2SEL1_LC2D2S4         : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D2S3         : bit  absolute CLC2SEL1.3;
  CLC2SEL1_LC2D2S2         : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D2S1         : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D2S0         : bit  absolute CLC2SEL1.0;
  CLC2SEL2                 : byte absolute $0F1E;
  CLC2SEL2_LC2D3S5         : bit  absolute CLC2SEL2.5;
  CLC2SEL2_LC2D3S4         : bit  absolute CLC2SEL2.4;
  CLC2SEL2_LC2D3S3         : bit  absolute CLC2SEL2.3;
  CLC2SEL2_LC2D3S2         : bit  absolute CLC2SEL2.2;
  CLC2SEL2_LC2D3S1         : bit  absolute CLC2SEL2.1;
  CLC2SEL2_LC2D3S0         : bit  absolute CLC2SEL2.0;
  CLC2SEL3                 : byte absolute $0F1F;
  CLC2SEL3_LC2D4S5         : bit  absolute CLC2SEL3.5;
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
  CLC3SEL0_LC3D1S5         : bit  absolute CLC3SEL0.5;
  CLC3SEL0_LC3D1S4         : bit  absolute CLC3SEL0.4;
  CLC3SEL0_LC3D1S3         : bit  absolute CLC3SEL0.3;
  CLC3SEL0_LC3D1S2         : bit  absolute CLC3SEL0.2;
  CLC3SEL0_LC3D1S1         : bit  absolute CLC3SEL0.1;
  CLC3SEL0_LC3D1S0         : bit  absolute CLC3SEL0.0;
  CLC3SEL1                 : byte absolute $0F27;
  CLC3SEL1_LC3D2S5         : bit  absolute CLC3SEL1.5;
  CLC3SEL1_LC3D2S4         : bit  absolute CLC3SEL1.4;
  CLC3SEL1_LC3D2S3         : bit  absolute CLC3SEL1.3;
  CLC3SEL1_LC3D2S2         : bit  absolute CLC3SEL1.2;
  CLC3SEL1_LC3D2S1         : bit  absolute CLC3SEL1.1;
  CLC3SEL1_LC3D2S0         : bit  absolute CLC3SEL1.0;
  CLC3SEL2                 : byte absolute $0F28;
  CLC3SEL2_LC3D3S5         : bit  absolute CLC3SEL2.5;
  CLC3SEL2_LC3D3S4         : bit  absolute CLC3SEL2.4;
  CLC3SEL2_LC3D3S3         : bit  absolute CLC3SEL2.3;
  CLC3SEL2_LC3D3S2         : bit  absolute CLC3SEL2.2;
  CLC3SEL2_LC3D3S1         : bit  absolute CLC3SEL2.1;
  CLC3SEL2_LC3D3S0         : bit  absolute CLC3SEL2.0;
  CLC3SEL3                 : byte absolute $0F29;
  CLC3SEL3_LC3D4S5         : bit  absolute CLC3SEL3.5;
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
  CLC4CON                  : byte absolute $0F2E;
  CLC4CON_LC4EN            : bit  absolute CLC4CON.7;
  CLC4CON_LC4OUT           : bit  absolute CLC4CON.5;
  CLC4CON_LC4INTP          : bit  absolute CLC4CON.4;
  CLC4CON_LC4INTN          : bit  absolute CLC4CON.3;
  CLC4CON_LC4MODE2         : bit  absolute CLC4CON.2;
  CLC4CON_LC4MODE1         : bit  absolute CLC4CON.1;
  CLC4CON_LC4MODE0         : bit  absolute CLC4CON.0;
  CLC4POL                  : byte absolute $0F2F;
  CLC4POL_LC4POL           : bit  absolute CLC4POL.7;
  CLC4POL_LC4G4POL         : bit  absolute CLC4POL.3;
  CLC4POL_LC4G3POL         : bit  absolute CLC4POL.2;
  CLC4POL_LC4G2POL         : bit  absolute CLC4POL.1;
  CLC4POL_LC4G1POL         : bit  absolute CLC4POL.0;
  CLC4SEL0                 : byte absolute $0F30;
  CLC4SEL0_LC4D1S5         : bit  absolute CLC4SEL0.5;
  CLC4SEL0_LC4D1S4         : bit  absolute CLC4SEL0.4;
  CLC4SEL0_LC4D1S3         : bit  absolute CLC4SEL0.3;
  CLC4SEL0_LC4D1S2         : bit  absolute CLC4SEL0.2;
  CLC4SEL0_LC4D1S1         : bit  absolute CLC4SEL0.1;
  CLC4SEL0_LC4D1S0         : bit  absolute CLC4SEL0.0;
  CLC4SEL1                 : byte absolute $0F31;
  CLC4SEL1_LC4D2S5         : bit  absolute CLC4SEL1.5;
  CLC4SEL1_LC4D2S4         : bit  absolute CLC4SEL1.4;
  CLC4SEL1_LC4D2S3         : bit  absolute CLC4SEL1.3;
  CLC4SEL1_LC4D2S2         : bit  absolute CLC4SEL1.2;
  CLC4SEL1_LC4D2S1         : bit  absolute CLC4SEL1.1;
  CLC4SEL1_LC4D2S0         : bit  absolute CLC4SEL1.0;
  CLC4SEL2                 : byte absolute $0F32;
  CLC4SEL2_LC4D3S5         : bit  absolute CLC4SEL2.5;
  CLC4SEL2_LC4D3S4         : bit  absolute CLC4SEL2.4;
  CLC4SEL2_LC4D3S3         : bit  absolute CLC4SEL2.3;
  CLC4SEL2_LC4D3S2         : bit  absolute CLC4SEL2.2;
  CLC4SEL2_LC4D3S1         : bit  absolute CLC4SEL2.1;
  CLC4SEL2_LC4D3S0         : bit  absolute CLC4SEL2.0;
  CLC4SEL3                 : byte absolute $0F33;
  CLC4SEL3_LC4D4S5         : bit  absolute CLC4SEL3.5;
  CLC4SEL3_LC4D4S4         : bit  absolute CLC4SEL3.4;
  CLC4SEL3_LC4D4S3         : bit  absolute CLC4SEL3.3;
  CLC4SEL3_LC4D4S2         : bit  absolute CLC4SEL3.2;
  CLC4SEL3_LC4D4S1         : bit  absolute CLC4SEL3.1;
  CLC4SEL3_LC4D4S0         : bit  absolute CLC4SEL3.0;
  CLC4GLS0                 : byte absolute $0F34;
  CLC4GLS0_LC4G1D4T        : bit  absolute CLC4GLS0.7;
  CLC4GLS0_LC4G1D4N        : bit  absolute CLC4GLS0.6;
  CLC4GLS0_LC4G1D3T        : bit  absolute CLC4GLS0.5;
  CLC4GLS0_LC4G1D3N        : bit  absolute CLC4GLS0.4;
  CLC4GLS0_LC4G1D2T        : bit  absolute CLC4GLS0.3;
  CLC4GLS0_LC4G1D2N        : bit  absolute CLC4GLS0.2;
  CLC4GLS0_LC4G1D1T        : bit  absolute CLC4GLS0.1;
  CLC4GLS0_LC4G1D1N        : bit  absolute CLC4GLS0.0;
  CLC4GLS1                 : byte absolute $0F35;
  CLC4GLS1_LC4G2D4T        : bit  absolute CLC4GLS1.7;
  CLC4GLS1_LC4G2D4N        : bit  absolute CLC4GLS1.6;
  CLC4GLS1_LC4G2D3T        : bit  absolute CLC4GLS1.5;
  CLC4GLS1_LC4G2D3N        : bit  absolute CLC4GLS1.4;
  CLC4GLS1_LC4G2D2T        : bit  absolute CLC4GLS1.3;
  CLC4GLS1_LC4G2D2N        : bit  absolute CLC4GLS1.2;
  CLC4GLS1_LC4G2D1T        : bit  absolute CLC4GLS1.1;
  CLC4GLS1_LC4G2D1N        : bit  absolute CLC4GLS1.0;
  CLC4GLS2                 : byte absolute $0F36;
  CLC4GLS2_LC4G3D4T        : bit  absolute CLC4GLS2.7;
  CLC4GLS2_LC4G3D4N        : bit  absolute CLC4GLS2.6;
  CLC4GLS2_LC4G3D3T        : bit  absolute CLC4GLS2.5;
  CLC4GLS2_LC4G3D3N        : bit  absolute CLC4GLS2.4;
  CLC4GLS2_LC4G3D2T        : bit  absolute CLC4GLS2.3;
  CLC4GLS2_LC4G3D2N        : bit  absolute CLC4GLS2.2;
  CLC4GLS2_LC4G3D1T        : bit  absolute CLC4GLS2.1;
  CLC4GLS2_LC4G3D1N        : bit  absolute CLC4GLS2.0;
  CLC4GLS3                 : byte absolute $0F37;
  CLC4GLS3_LC4G4D4T        : bit  absolute CLC4GLS3.7;
  CLC4GLS3_LC4G4D4N        : bit  absolute CLC4GLS3.6;
  CLC4GLS3_LC4G4D3T        : bit  absolute CLC4GLS3.5;
  CLC4GLS3_LC4G4D3N        : bit  absolute CLC4GLS3.4;
  CLC4GLS3_LC4G4D2T        : bit  absolute CLC4GLS3.3;
  CLC4GLS3_LC4G4D2N        : bit  absolute CLC4GLS3.2;
  CLC4GLS3_LC4G4D1T        : bit  absolute CLC4GLS3.1;
  CLC4GLS3_LC4G4D1N        : bit  absolute CLC4GLS3.0;
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
  {$SET_STATE_RAM '00C-00E:SFR'}            // Bank 0 : PORTA, PORTB, PORTC
  {$SET_STATE_RAM '010-01F:SFR'}            // Bank 0 : PIR0, PIR1, PIR2, PIR3, PIR4, TMR0L, TMR0H, T0CON0, T0CON1, TMR1L, TMR1H, T1CON, T1GCON, TMR2, PR2, T2CON
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08C-08E:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '090-094:SFR'}            // Bank 1 : PIE0, PIE1, PIE2, PIE3, PIE4
  {$SET_STATE_RAM '097-097:SFR'}            // Bank 1 : WDTCON
  {$SET_STATE_RAM '09B-09F:SFR'}            // Bank 1 : ADRESL, ADRESH, ADCON0, ADCON1, ADACT
  {$SET_STATE_RAM '0A0-0EF:GPR'}           
  {$SET_STATE_RAM '10C-10E:SFR'}            // Bank 2 : LATA, LATB, LATC
  {$SET_STATE_RAM '111-119:SFR'}            // Bank 2 : CM1CON0, CM1CON1, CM2CON0, CM2CON1, CMOUT, BORCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '120-16F:GPR'}           
  {$SET_STATE_RAM '18C-18E:SFR'}            // Bank 3 : ANSELA, ANSELB, ANSELC
  {$SET_STATE_RAM '197-197:SFR'}            // Bank 3 : VREGCON
  {$SET_STATE_RAM '199-19F:SFR'}            // Bank 3 : RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '1A0-1EF:GPR'}           
  {$SET_STATE_RAM '20C-20E:SFR'}            // Bank 4 : WPUA, WPUB, WPUC
  {$SET_STATE_RAM '211-217:SFR'}            // Bank 4 : SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '219-21F:SFR'}            // Bank 4 : SSP2BUF, SSP2ADD, SSP2MSK, SSP2STAT, SSP2CON1, SSP2CON2, SSP2CON3
  {$SET_STATE_RAM '220-26F:GPR'}           
  {$SET_STATE_RAM '28C-28E:SFR'}            // Bank 5 : ODCONA, ODCONB, ODCONC
  {$SET_STATE_RAM '291-298:SFR'}            // Bank 5 : CCPR1L, CCPR1H, CCP1CON, CCP1CAP, CCPR2L, CCPR2H, CCP2CON, CCP2CAP
  {$SET_STATE_RAM '29F-29F:SFR'}            // Bank 5 : CCPTMRS
  {$SET_STATE_RAM '2A0-2EF:GPR'}           
  {$SET_STATE_RAM '30C-30E:SFR'}            // Bank 6 : SLRCONA, SLRCONB, SLRCONC
  {$SET_STATE_RAM '311-318:SFR'}            // Bank 6 : CCPR3L, CCPR3H, CCP3CON, CCP3CAP, CCPR4L, CCPR4H, CCP4CON, CCP4CAP
  {$SET_STATE_RAM '320-36F:GPR'}           
  {$SET_STATE_RAM '38C-38E:SFR'}            // Bank 7 : INLVLA, INLVLB, INLVLC
  {$SET_STATE_RAM '391-39A:SFR'}            // Bank 7 : IOCAP, IOCAN, IOCAF, IOCBP, IOCBN, IOCBF, IOCCP, IOCCN, IOCCF, CLKRCON
  {$SET_STATE_RAM '39C-39F:SFR'}            // Bank 7 : MDCON, MDSRC, MDCARH, MDCARL
  {$SET_STATE_RAM '3A0-3EF:GPR'}           
  {$SET_STATE_RAM '411-41F:SFR'}            // Bank 8 : TMR3L, TMR3H, T3CON, T3GCON, TMR4, PR4, T4CON, TMR5L, TMR5H, T5CON, T5GCON, TMR6, PR6, T6CON, CCDCON
  {$SET_STATE_RAM '420-46F:GPR'}           
  {$SET_STATE_RAM '498-49F:SFR'}            // Bank 9 : NCO1ACCL, NCO1ACCH, NCO1ACCU, NCO1INCL, NCO1INCH, NCO1INCU, NCO1CON, NCO1CLK
  {$SET_STATE_RAM '4A0-4EF:GPR'}           
  {$SET_STATE_RAM '520-56F:GPR'}           
  {$SET_STATE_RAM '5A0-5EF:GPR'}           
  {$SET_STATE_RAM '617-61C:SFR'}            // Bank 12 : PWM5DCL, PWM5DCH, PWM5CON, PWM6DCL, PWM6DCH, PWM6CON
  {$SET_STATE_RAM '61F-61F:SFR'}            // Bank 12 : PWMTMRS
  {$SET_STATE_RAM '620-64F:GPR'}           
  {$SET_STATE_RAM '691-699:SFR'}            // Bank 13 : CWG1CLKCON, CWG1DAT, CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1AS0, CWG1AS1, CWG1STR
  {$SET_STATE_RAM '711-719:SFR'}            // Bank 14 : CWG2CLKCON, CWG2DAT, CWG2DBR, CWG2DBF, CWG2CON0, CWG2CON1, CWG2AS0, CWG2AS1, CWG2STR
  {$SET_STATE_RAM '891-896:SFR'}            // Bank 17 : NVMADRL, NVMADRH, NVMDATL, NVMDATH, NVMCON1, NVMCON2
  {$SET_STATE_RAM '89B-89B:SFR'}            // Bank 17 : PCON0
  {$SET_STATE_RAM '911-916:SFR'}            // Bank 18 : PMD0, PMD1, PMD2, PMD3, PMD4, PMD5
  {$SET_STATE_RAM '918-91F:SFR'}            // Bank 18 : CPUDOZE, OSCCON1, OSCCON2, OSCCON3, OSCSTAT1, OSCEN, OSCTUNE, OSCFRQ
  {$SET_STATE_RAM 'E0F-E22:SFR'}            // Bank 28 : PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS, CCP1PPS, CCP2PPS, CCP3PPS, CCP4PPS, CWG1PPS, CWG2PPS, MDCIN1PPS, MDCIN2PPS, MDMINPPS, SSP2CLKPPS, SSP2DATPPS, SSP2SSPPS, SSP1CLKPPS, SSP1DATPPS, SSP1SSPPS
  {$SET_STATE_RAM 'E24-E25:SFR'}            // Bank 28 : RXPPS, TXPPS
  {$SET_STATE_RAM 'E28-E2F:SFR'}            // Bank 28 : CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS, T3CKIPPS, T3GPPS, T5CKIPPS, T5GPPS
  {$SET_STATE_RAM 'E90-E92:SFR'}            // Bank 29 : RA0PPS, RA1PPS, RA2PPS
  {$SET_STATE_RAM 'E94-E95:SFR'}            // Bank 29 : RA4PPS, RA5PPS
  {$SET_STATE_RAM 'E9C-EA7:SFR'}            // Bank 29 : RB4PPS, RB5PPS, RB6PPS, RB7PPS, RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS, RC6PPS, RC7PPS
  {$SET_STATE_RAM 'F0F-F37:SFR'}            // Bank 30 : CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3, CLC4CON, CLC4POL, CLC4SEL0, CLC4SEL1, CLC4SEL2, CLC4SEL3, CLC4GLS0, CLC4GLS1, CLC4GLS2, CLC4GLS3
  {$SET_STATE_RAM 'FE4-FEB:SFR'}            // Bank 31 : STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM 'FED-FEF:SFR'}            // Bank 31 : STKPTR, TOSL, TOSH


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:1F'} // BSR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00B:C1'} // INTCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:F0'} // PORTB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:31'} // PIR0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '017:BF'} // T0CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01B:FD'} // T1CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:F0'} // TRISB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:31'} // PIE0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:3F'} // WDTCON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09E:F7'} // ADCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:1F'} // ADACT bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10C:37'} // LATA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10D:F0'} // LATB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:D7'} // CM1CON0 bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '113:D7'} // CM2CON0 bits 5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:81'} // BORCON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:AD'} // DACCON0 bits 6,4,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:1F'} // DACCON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:37'} // ANSELA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:F0'} // ANSELB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '197:03'} // VREGCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20C:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20D:F0'} // WPUB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28C:37'} // ODCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '28D:F0'} // ODCONB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '293:BF'} // CCP1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '294:0F'} // CCP1CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '297:BF'} // CCP2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '298:0F'} // CCP2CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30C:37'} // SLRCONA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30D:F0'} // SLRCONB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:BF'} // CCP3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '314:0F'} // CCP3CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '317:BF'} // CCP4CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '318:0F'} // CCP4CAP bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38C:3F'} // INLVLA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '38D:F0'} // INLVLB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '391:3F'} // IOCAP bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '392:3F'} // IOCAN bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '393:3F'} // IOCAF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '394:F0'} // IOCBP bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '395:F0'} // IOCBN bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '396:F0'} // IOCBF bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39A:9F'} // CLKRCON bits 6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39C:99'} // MDCON bits 6,5,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39D:0F'} // MDSRC bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39E:6F'} // MDCARH bits 7,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '39F:6F'} // MDCARL bits 7,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '413:FD'} // T3CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '417:7F'} // T4CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41A:FD'} // T5CON bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41E:7F'} // T6CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '41F:83'} // CCDCON bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49A:0F'} // NCO1ACCU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49D:0F'} // NCO1INCU bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49E:B1'} // NCO1CON bits 6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49F:E3'} // NCO1CLK bits 4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '617:C0'} // PWM5DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '619:B0'} // PWM5CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61A:C0'} // PWM6DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61C:B0'} // PWM6CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '61F:0F'} // PWMTMRS bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '691:01'} // CWG1CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '692:0F'} // CWG1DAT bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '693:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '694:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '695:C7'} // CWG1CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '696:2F'} // CWG1CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '697:FC'} // CWG1AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '698:1F'} // CWG1AS1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:01'} // CWG2CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:0F'} // CWG2DAT bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '713:3F'} // CWG2DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '714:3F'} // CWG2DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '715:C7'} // CWG2CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '716:2F'} // CWG2CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '717:FC'} // CWG2AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '718:1F'} // CWG2AS1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '894:3F'} // NVMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '895:7F'} // NVMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '89B:DF'} // PCON0 bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '911:C7'} // PMD0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '913:66'} // PMD2 bits 7,4,3,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '915:26'} // PMD4 bits 7,6,4,3,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '916:1F'} // PMD5 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '918:F7'} // CPUDOZE bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '919:7F'} // OSCCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91A:7F'} // OSCCON2 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91B:F8'} // OSCCON3 bits 2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91C:DD'} // OSCSTAT1 bits 5,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91D:DC'} // OSCEN bits 5,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91E:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91F:0F'} // OSCFRQ bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E0F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E10:1F'} // INTPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E11:1F'} // T0CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E12:1F'} // T1CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E13:1F'} // T1GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E14:1F'} // CCP1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E15:1F'} // CCP2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E16:1F'} // CCP3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E17:1F'} // CCP4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E18:1F'} // CWG1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E19:1F'} // CWG2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1A:1F'} // MDCIN1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1B:1F'} // MDCIN2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1C:1F'} // MDMINPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1D:1F'} // SSP2CLKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1E:1F'} // SSP2DATPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E1F:1F'} // SSP2SSPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E20:1F'} // SSP1CLKPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E21:1F'} // SSP1DATPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E22:1F'} // SSP1SSPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E24:1F'} // RXPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E25:1F'} // TXPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E28:1F'} // CLCIN0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E29:1F'} // CLCIN1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2A:1F'} // CLCIN2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2B:1F'} // CLCIN3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2C:1F'} // T3CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2D:1F'} // T3GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2E:1F'} // T5CKIPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E2F:1F'} // T5GPPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E90:1F'} // RA0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E91:1F'} // RA1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E92:1F'} // RA2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E94:1F'} // RA4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E95:1F'} // RA5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9C:1F'} // RB4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9D:1F'} // RB5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9E:1F'} // RB6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'E9F:1F'} // RB7PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA0:1F'} // RC0PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA1:1F'} // RC1PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA2:1F'} // RC2PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA3:1F'} // RC3PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA4:1F'} // RC4PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA5:1F'} // RC5PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA6:1F'} // RC6PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'EA7:1F'} // RC7PPS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F0F:0F'} // CLCDATA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F10:BF'} // CLC1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F12:3F'} // CLC1SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F13:3F'} // CLC1SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F14:3F'} // CLC1SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F15:3F'} // CLC1SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1A:BF'} // CLC2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1B:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1C:3F'} // CLC2SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1D:3F'} // CLC2SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1E:3F'} // CLC2SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F1F:3F'} // CLC2SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F24:BF'} // CLC3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F25:8F'} // CLC3POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F26:3F'} // CLC3SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F27:3F'} // CLC3SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F28:3F'} // CLC3SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F29:3F'} // CLC3SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2E:BF'} // CLC4CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F2F:8F'} // CLC4POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F30:3F'} // CLC4SEL0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F31:3F'} // CLC4SEL1 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F32:3F'} // CLC4SEL2 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'F33:3F'} // CLC4SEL3 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'FEF:7F'} // TOSH bit 7 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '08C:08'} // TRISA bit 3 un-implemented (read as 1)
  {$SET_UNIMP_BITS1 '197:01'} // VREGCON bit 0 un-implemented (read as 1)
  {$SET_UNIMP_BITS1 '892:80'} // NVMADRH bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : ECIN/OSC1/SOSCI/SOSCIN/CLKIN/ANA5/IOCA5/RA5
  // Pin  3 : OSC2/SOSCO/CLKOUT/ANA4/IOCA4/RA4
  // Pin  4 : MCLR/VPP/IOCA3/RA3
  // Pin  5 : ANC5/SMB_I2C/IOCC5/RC5
  // Pin  6 : ANC4/SMB_I2C/IOCC4/RC4
  // Pin  7 : ANC3/C1IN3-/C2IN3-/IOCC3/RC3
  // Pin  8 : ANC6/IOCC6/RC6
  // Pin  9 : ANC7/IOCC7/RC7
  // Pin 10 : ANB7/SMB_I2C/IOCB7/RB7
  // Pin 11 : ANB6/SMB_I2C/IOCB6/RB6
  // Pin 12 : ANB5/SMB_I2C/IOCB5/RB5
  // Pin 13 : ANB4/SMB_I2C/IOCB4/RB4
  // Pin 14 : ANC2/C1IN2-/C2IN2-/IOCC2/RC2
  // Pin 15 : ANC1/C1IN1-/C2IN1-/SMB_I2C/IOCC1/RC1
  // Pin 16 : ANC0/C2IN0+/SMB_I2C/IOCC0/RC0
  // Pin 17 : DAC1REF-/VREF-/ANA2/IOCA2/RA2
  // Pin 18 : ICSPCLK/ICDCLK/DAC1REF+/VREF+/ANA1/C1IN0-/C2IN0-/IOCA1/RA1
  // Pin 19 : ICSPDAT/ICDDAT/DAC1OUT/ANA0/C1IN0+/IOCA0/RA0
  // Pin 20 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-19,1-18,2-17,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '00D:4-13,5-12,6-11,7-10'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FEXTOSC : FEXTOSC External Oscillator mode Selection bits
  {$define _FEXTOSC_ECH           = $3FFF}  // EC (external clock) above 8 MHz
  {$define _FEXTOSC_ECM           = $3FFE}  // EC (external clock) for 100 kHz to 8 MHz
  {$define _FEXTOSC_ECL           = $3FFD}  // EC (external clock) below 100 kHz
  {$define _FEXTOSC_OFF           = $3FFC}  // Oscillator not enabled
  {$define _FEXTOSC_Unimplemented = $3FFB}  // Unimplemented
  {$define _FEXTOSC_HS            = $3FFA}  // HS (crystal oscillator) above 4 MHz
  {$define _FEXTOSC_XT            = $3FF9}  // XT (crystal oscillator) from 100 kHz to 4 MHz
  {$define _FEXTOSC_LP            = $3FF8}  // LP (crystal oscillator) optimized for 32.768 kHz

  // RSTOSC : Power-up default value for COSC bits
  {$define _RSTOSC_EXT1X          = $3FFF}  // EXTOSC operating per FEXTOSC bits 
  {$define _RSTOSC_HFINT1         = $3FEF}  // HFINTOSC (1MHz)
  {$define _RSTOSC_Unimplemented  = $3FDF}  // Unimplemented
  {$define _RSTOSC_LFINT          = $3FCF}  // LFINTOSC (31kHz)
  {$define _RSTOSC_SOSC           = $3FBF}  // SOSC (31kHz)
  {$define _RSTOSC_Reserved       = $3FAF}  // Reserved
  {$define _RSTOSC_EXT4X          = $3F9F}  // EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
  {$define _RSTOSC_HFINT32        = $3F8F}  // HFINTOSC with 2x PLL (32MHz)

  // CLKOUTEN : Clock Out Enable bit
  {$define _CLKOUTEN_OFF          = $3FFF}  // CLKOUT function is disabled; I/O or oscillator function on OSC2
  {$define _CLKOUTEN_ON           = $3EFF}  // CLKOUT function is enabled; FOSC/4 clock appears at OSC2

  // CSWEN : Clock Switch Enable bit
  {$define _CSWEN_ON              = $3FFF}  // Writing to NOSC and NDIV is allowed
  {$define _CSWEN_OFF             = $37FF}  // The NOSC and NDIV bits cannot be changed by user software

  // FCMEN : Fail-Safe Clock Monitor Enable
  {$define _FCMEN_ON              = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF             = $1FFF}  // Fail-Safe Clock Monitor is disabled

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON              = $3FFF}  // MCLR/VPP pin function is MCLR; Weak pull-up enabled
  {$define _MCLRE_OFF             = $3FFE}  // MCLR/VPP pin function is digital input; MCLR internally disabled; Weak pull-up under control of port pin's WPU control bit.

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF             = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON              = $3FFD}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bits
  {$define _WDTE_ON               = $3FFF}  // WDT enabled, SWDTEN is ignored
  {$define _WDTE_SLEEP            = $3FFB}  // WDT enabled while running and disabled in SLEEP/IDLE; SWDTEN is ignored
  {$define _WDTE_SWDTEN           = $3FF7}  // WDT controlled by the SWDTEN bit in the WDTCON register
  {$define _WDTE_OFF              = $3FF3}  // WDT disabled; SWDTEN is ignored

  // LPBOREN : Low-power BOR enable bit
  {$define _LPBOREN_OFF           = $3FFF}  // ULPBOR disabled
  {$define _LPBOREN_ON            = $3FDF}  // ULPBOR enabled

  // BOREN : Brown-out Reset Enable bits
  {$define _BOREN_ON              = $3FFF}  // Brown-out Reset enabled, SBOREN bit ignored
  {$define _BOREN_SLEEP           = $3FBF}  // Brown-out Reset enabled while running, disabled in Sleep; SBOREN is ignored
  {$define _BOREN_SBOREN          = $3F7F}  // Brown-out Reset enabled according to SBOREN
  {$define _BOREN_OFF             = $3F3F}  // Brown-out Reset disabled

  // BORV : Brown-out Reset Voltage selection bit
  {$define _BORV_LOW              = $3FFF}  // Brown-out voltage (Vbor) set to 2.45V
  {$define _BORV_HIGH             = $3DFF}  // Brown-out voltage (Vbor) set to 2.7V

  // PPS1WAY : PPSLOCK bit One-Way Set Enable bit
  {$define _PPS1WAY_ON            = $3FFF}  // The PPSLOCK bit can be cleared and set only once; PPS registers remain locked after one clear/set cycle
  {$define _PPS1WAY_OFF           = $37FF}  // The PPSLOCK bit can be set and cleared repeatedly (subject to the unlock sequence)

  // STVREN : Stack Overflow/Underflow Reset Enable bit
  {$define _STVREN_ON             = $3FFF}  // Stack Overflow or Underflow will cause a Reset
  {$define _STVREN_OFF            = $2FFF}  // Stack Overflow or Underflow will not cause a Reset

  // DEBUG : Debugger enable bit
  {$define _DEBUG_OFF             = $3FFF}  // Background debugger disabled
  {$define _DEBUG_ON              = $1FFF}  // Background debugger enabled

  // WRT : User NVM self-write protection bits
  {$define _WRT_OFF               = $3FFF}  // Write protection off
  {$define _WRT_BOOT              = $3FFE}  // 0000h to 01FFh write-protected, 0200h to 1FFFh may be modified
  {$define _WRT_HALF              = $3FFD}  // 0000h to 0FFFh write-protected, 1000h to 1FFFh may be modified
  {$define _WRT_ALL               = $3FFC}  // 0000h to 1FFFh write protected, no addresses may be modified

  // LVP : Low Voltage Programming Enable bit
  {$define _LVP_ON                = $3FFF}  // Low Voltage programming enabled. MCLR/VPP pin function is MCLR. MCLRE configuration bit is ignored.
  {$define _LVP_OFF               = $1FFF}  // High Voltage on MCLR/VPP must be used for programming.

  // CP : User NVM Program Memory Code Protection bit
  {$define _CP_OFF                = $3FFF}  // User NVM code protection disabled
  {$define _CP_ON                 = $3FFE}  // User NVM code protection enabled

  // CPD : Data NVM Memory Code Protection bit
  {$define _CPD_OFF               = $3FFF}  // Data NVM code protection disabled
  {$define _CPD_ON                = $3FFD}  // Data NVM code protection enabled

implementation
end.
