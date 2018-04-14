unit PIC16F785;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F785'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

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
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_RC0         : bit  absolute PORTC.0;
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RAIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RAIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_CCP1IF       : bit  absolute PIR1.5;
  PIR1_C2IF         : bit  absolute PIR1.4;
  PIR1_C1IF         : bit  absolute PIR1.3;
  PIR1_OSFIF        : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
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
  CCPR1L            : byte absolute $0013;
  CCPR1H            : byte absolute $0014;
  CCP1CON           : byte absolute $0015;
  CCP1CON_DC1B1     : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  WDTCON            : byte absolute $0018;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  ADRESH            : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADFM       : bit  absolute ADCON0.7;
  ADCON0_VCFG       : bit  absolute ADCON0.6;
  ADCON0_CHS3       : bit  absolute ADCON0.5;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RAPU   : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
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
  TRISC             : byte absolute $0087;
  TRISC_TRISC7      : bit  absolute TRISC.7;
  TRISC_TRISC6      : bit  absolute TRISC.6;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  PIE1              : byte absolute $008c;
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_CCP1IE       : bit  absolute PIE1.5;
  PIE1_C2IE         : bit  absolute PIE1.4;
  PIE1_C1IE         : bit  absolute PIE1.3;
  PIE1_OSFIE        : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
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
  ANSEL0            : byte absolute $0091;
  ANSEL0_ANS7       : bit  absolute ANSEL0.7;
  ANSEL0_ANS6       : bit  absolute ANSEL0.6;
  ANSEL0_ANS5       : bit  absolute ANSEL0.5;
  ANSEL0_ANS4       : bit  absolute ANSEL0.4;
  ANSEL0_ANS3       : bit  absolute ANSEL0.3;
  ANSEL0_ANS2       : bit  absolute ANSEL0.2;
  ANSEL0_ANS1       : bit  absolute ANSEL0.1;
  ANSEL0_ANS0       : bit  absolute ANSEL0.0;
  PR2               : byte absolute $0092;
  ANSEL1            : byte absolute $0093;
  ANSEL1_ANS11      : bit  absolute ANSEL1.3;
  ANSEL1_ANS10      : bit  absolute ANSEL1.2;
  ANSEL1_ANS9       : bit  absolute ANSEL1.1;
  ANSEL1_ANS8       : bit  absolute ANSEL1.0;
  WPUA              : byte absolute $0095;
  WPUA_WPUA5        : bit  absolute WPUA.5;
  WPUA_WPUA4        : bit  absolute WPUA.4;
  WPUA_WPUA3        : bit  absolute WPUA.3;
  WPUA_WPUA2        : bit  absolute WPUA.2;
  WPUA_WPUA1        : bit  absolute WPUA.1;
  WPUA_WPUA0        : bit  absolute WPUA.0;
  IOCA              : byte absolute $0096;
  IOCA_IOCA5        : bit  absolute IOCA.5;
  IOCA_IOCA4        : bit  absolute IOCA.4;
  IOCA_IOCA3        : bit  absolute IOCA.3;
  IOCA_IOCA2        : bit  absolute IOCA.2;
  IOCA_IOCA1        : bit  absolute IOCA.1;
  IOCA_IOCA0        : bit  absolute IOCA.0;
  REFCON            : byte absolute $0098;
  REFCON_BGST       : bit  absolute REFCON.5;
  REFCON_VRBB       : bit  absolute REFCON.4;
  REFCON_VREN       : bit  absolute REFCON.3;
  REFCON_VROE       : bit  absolute REFCON.2;
  REFCON_CVROE      : bit  absolute REFCON.1;
  VRCON             : byte absolute $0099;
  VRCON_C1VREN      : bit  absolute VRCON.7;
  VRCON_C2VREN      : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.4;
  VRCON_VR2         : bit  absolute VRCON.3;
  VRCON_VR1         : bit  absolute VRCON.2;
  VRCON_VR0         : bit  absolute VRCON.1;
  EEDATA            : byte absolute $009a;
  EEADR             : byte absolute $009b;
  EECON1            : byte absolute $009c;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $009d;
  ADRESL            : byte absolute $009e;
  ADCON1            : byte absolute $009f;
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
  ADCON1_ADCS1      : bit  absolute ADCON1.5;
  ADCON1_ADCS0      : bit  absolute ADCON1.4;
  PWMCON1           : byte absolute $0110;
  PWMCON1_OVRLP     : bit  absolute PWMCON1.7;
  PWMCON1_COMOD1    : bit  absolute PWMCON1.6;
  PWMCON1_COMOD0    : bit  absolute PWMCON1.5;
  PWMCON1_CMDLY4    : bit  absolute PWMCON1.4;
  PWMCON1_CMDLY3    : bit  absolute PWMCON1.3;
  PWMCON1_CMDLY2    : bit  absolute PWMCON1.2;
  PWMCON1_CMDLY1    : bit  absolute PWMCON1.1;
  PWMCON1_CMDLY0    : bit  absolute PWMCON1.0;
  PWMCON0           : byte absolute $0111;
  PWMCON0_PRSEN     : bit  absolute PWMCON0.7;
  PWMCON0_PASEN     : bit  absolute PWMCON0.6;
  PWMCON0_BLANK2    : bit  absolute PWMCON0.5;
  PWMCON0_BLANK1    : bit  absolute PWMCON0.4;
  PWMCON0_SYNC1     : bit  absolute PWMCON0.3;
  PWMCON0_SYNC0     : bit  absolute PWMCON0.2;
  PWMCON0_PH2EN     : bit  absolute PWMCON0.1;
  PWMCON0_PH1EN     : bit  absolute PWMCON0.0;
  PWMCLK            : byte absolute $0112;
  PWMCLK_PWMASE     : bit  absolute PWMCLK.7;
  PWMCLK_PWMP1      : bit  absolute PWMCLK.6;
  PWMCLK_PWMP0      : bit  absolute PWMCLK.5;
  PWMCLK_PER4       : bit  absolute PWMCLK.4;
  PWMCLK_PER3       : bit  absolute PWMCLK.3;
  PWMCLK_PER2       : bit  absolute PWMCLK.2;
  PWMCLK_PER1       : bit  absolute PWMCLK.1;
  PWMCLK_PER0       : bit  absolute PWMCLK.0;
  PWMPH1            : byte absolute $0113;
  PWMPH1_POL        : bit  absolute PWMPH1.7;
  PWMPH1_C2EN       : bit  absolute PWMPH1.6;
  PWMPH1_C1EN       : bit  absolute PWMPH1.5;
  PWMPH1_PH4        : bit  absolute PWMPH1.4;
  PWMPH1_PH3        : bit  absolute PWMPH1.3;
  PWMPH1_PH2        : bit  absolute PWMPH1.2;
  PWMPH1_PH1        : bit  absolute PWMPH1.1;
  PWMPH1_PH0        : bit  absolute PWMPH1.0;
  PWMPH2            : byte absolute $0114;
  CM1CON0           : byte absolute $0119;
  CM1CON0_C1ON      : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT     : bit  absolute CM1CON0.6;
  CM1CON0_C1OE      : bit  absolute CM1CON0.5;
  CM1CON0_C1POL     : bit  absolute CM1CON0.4;
  CM1CON0_C1SP      : bit  absolute CM1CON0.3;
  CM1CON0_C1R       : bit  absolute CM1CON0.2;
  CM1CON0_C1CH1     : bit  absolute CM1CON0.1;
  CM1CON0_C1CH0     : bit  absolute CM1CON0.0;
  CM2CON0           : byte absolute $011a;
  CM2CON0_C2ON      : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT     : bit  absolute CM2CON0.6;
  CM2CON0_C2OE      : bit  absolute CM2CON0.5;
  CM2CON0_C2POL     : bit  absolute CM2CON0.4;
  CM2CON0_C2SP      : bit  absolute CM2CON0.3;
  CM2CON0_C2R       : bit  absolute CM2CON0.2;
  CM2CON0_C2CH1     : bit  absolute CM2CON0.1;
  CM2CON0_C2CH0     : bit  absolute CM2CON0.0;
  CM2CON1           : byte absolute $011b;
  CM2CON1_MC1OUT    : bit  absolute CM2CON1.7;
  CM2CON1_MC2OUT    : bit  absolute CM2CON1.6;
  CM2CON1_T1GSS     : bit  absolute CM2CON1.5;
  CM2CON1_C2SYNC    : bit  absolute CM2CON1.4;
  OPA1CON           : byte absolute $011c;
  OPA1CON_OPAON     : bit  absolute OPA1CON.7;
  OPA2CON           : byte absolute $011d;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-015:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON, CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '018-018:SFR'}  // WDTCON
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-087:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB, TRISC
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-093:SFR'}  // PCON, OSCCON, OSCTUNE, ANSEL0, PR2, ANSEL1
  {$SET_STATE_RAM '095-096:SFR'}  // WPUA, IOCA
  {$SET_STATE_RAM '098-09F:SFR'}  // REFCON, VRCON, EEDATA, EEADR, EECON1, EECON2, ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-107:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '10A-10B:SFR'}  // PCLATH, INTCON
  {$SET_STATE_RAM '110-114:SFR'}  // PWMCON1, PWMCON0, PWMCLK, PWMPH1, PWMPH2
  {$SET_STATE_RAM '119-11D:SFR'}  // CM1CON0, CM2CON0, CM2CON1, OPA1CON, OPA2CON
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-187:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB, TRISC
  {$SET_STATE_RAM '18A-18B:SFR'}  // PCLATH, INTCON
  {$SET_STATE_RAM '1F0-1FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '100-107:bnk0'} // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '180-180:bnk0'} // INDF
  {$SET_MAPPED_RAM '181-181:bnk1'} // OPTION_REG
  {$SET_MAPPED_RAM '182-184:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '185-187:bnk1'} // TRISA, TRISB, TRISC
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:3F'} // PORTA
  {$SET_UNIMP_BITS '006:F0'} // PORTB
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '015:3F'} // CCP1CON
  {$SET_UNIMP_BITS '018:1F'} // WDTCON
  {$SET_UNIMP_BITS '085:3F'} // TRISA
  {$SET_UNIMP_BITS '086:F0'} // TRISB
  {$SET_UNIMP_BITS '08E:13'} // PCON
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '093:0F'} // ANSEL1
  {$SET_UNIMP_BITS '095:3F'} // WPUA
  {$SET_UNIMP_BITS '096:3F'} // IOCA
  {$SET_UNIMP_BITS '098:3E'} // REFCON
  {$SET_UNIMP_BITS '099:EF'} // VRCON
  {$SET_UNIMP_BITS '09C:0F'} // EECON1
  {$SET_UNIMP_BITS '09F:70'} // ADCON1
  {$SET_UNIMP_BITS '11B:C3'} // CM2CON1
  {$SET_UNIMP_BITS '11C:80'} // OPA1CON
  {$SET_UNIMP_BITS '11D:80'} // OPA2CON


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/OSC1/CLKIN
  // Pin  3 : RA4/AN3/T1G/OSC2/CLKOUT
  // Pin  4 : RA3/MCLR/Vpp
  // Pin  5 : RC5/CCP1
  // Pin  6 : RC4/C2OUT/PH2
  // Pin  7 : RC3/AN7/C12IN3-/OP1
  // Pin  8 : RC6/AN8/OP1-
  // Pin  9 : RC7/AN9/OP1+
  // Pin 10 : RB7/SYNC
  // Pin 11 : RB6
  // Pin 12 : RB5/AN11/OP2+
  // Pin 13 : RB4/AN10/OP2-
  // Pin 14 : RC2/AN6/C12IN2-/OP2
  // Pin 15 : RC1/AN5/C12IN1-/PH1
  // Pin 16 : RC0/AN4/C2IN+
  // Pin 17 : RA2/AN2/T0CKI/INT/C1OUT
  // Pin 18 : RA1/AN1/C12IN0-/Vref/ICSPCLK
  // Pin 19 : RA0/AN0/C1IN+/ICSPDAT
  // Pin 20 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-19,1-18,2-17,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '006:4-13,5-12,6-11,7-10'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON       = $0FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF      = $0FFE}  // Fail-Safe Clock Monitor is disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON        = $0FFF}  // Internal External Switchover mode is enabled
  {$define _IESO_OFF       = $0FFD}  // Internal External Switchover mode is disabled

  // BOREN : Brown Out Detect
  {$define _BOREN_ON       = $0FFF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $0FFB}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN   = $0FF7}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF      = $0FF3}  // BOR disabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF        = $0FFF}  // Data memory code protection is disabled
  {$define _CPD_ON         = $0FEF}  // Data memory code protection is enabled

  // CP : Code Protection bit
  {$define _CP_OFF         = $0FFF}  // Program memory code protection is disabled
  {$define _CP_ON          = $0FDF}  // Program memory code protection is enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON       = $0FFF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $0FBF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $0FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $0F7F}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $0FFF}  // WDT enabled
  {$define _WDTE_OFF       = $0EFF}  // WDT disabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $0FFF}  // EXTRC oscillator: External RC on RA5/OSC1/CLKIN, CLKOUT function on RA4/OSC2/CLKOUT pin
  {$define _FOSC_EXTRCIO   = $0DFF}  // EXTRCIO oscillator: External RC on RA5/OSC1/CLKIN, I/O function on RA4/OSC2/CLKOUT pin
  {$define _FOSC_INTOSCCLK = $0BFF}  // INTOSC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $09FF}  // INTOSCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_EC        = $07FF}  // EC: I/O function on RA4/OSC2/CLKOUT pin, CLKIN on RA5/OSC1/CLKIN
  {$define _FOSC_HS        = $05FF}  // HS oscillator: High-speed crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_XT        = $03FF}  // XT oscillator: Crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKINT
  {$define _FOSC_LP        = $01FF}  // LP oscillator: Low-power crystal on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN

implementation
end.
