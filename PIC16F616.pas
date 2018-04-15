unit PIC16F616;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F616'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 2}
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
  PORTC             : byte absolute $0007;
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
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RAIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RAIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_ECCPIF       : bit  absolute PIR1.5;
  PIR1_C2IF         : bit  absolute PIR1.4;
  PIR1_C1IF         : bit  absolute PIR1.3;
  PIR1_TMR2IF       : bit  absolute PIR1.2;
  PIR1_TMR1IF       : bit  absolute PIR1.1;
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
  CCP1CON_P1M1      : bit  absolute CCP1CON.7;
  CCP1CON_P1M0      : bit  absolute CCP1CON.6;
  CCP1CON_DC1B1     : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  PWM1CON           : byte absolute $0016;
  PWM1CON_PRSEN     : bit  absolute PWM1CON.7;
  PWM1CON_PDC6      : bit  absolute PWM1CON.6;
  PWM1CON_PDC5      : bit  absolute PWM1CON.5;
  PWM1CON_PDC4      : bit  absolute PWM1CON.4;
  PWM1CON_PDC3      : bit  absolute PWM1CON.3;
  PWM1CON_PDC2      : bit  absolute PWM1CON.2;
  PWM1CON_PDC1      : bit  absolute PWM1CON.1;
  PWM1CON_PDC0      : bit  absolute PWM1CON.0;
  ECCPAS            : byte absolute $0017;
  ECCPAS_ECCPASE    : bit  absolute ECCPAS.7;
  ECCPAS_ECCPAS2    : bit  absolute ECCPAS.6;
  ECCPAS_ECCPAS1    : bit  absolute ECCPAS.5;
  ECCPAS_ECCPAS0    : bit  absolute ECCPAS.4;
  ECCPAS_PSSAC1     : bit  absolute ECCPAS.3;
  ECCPAS_PSSAC0     : bit  absolute ECCPAS.2;
  ECCPAS_PSSBD1     : bit  absolute ECCPAS.1;
  ECCPAS_PSSBD0     : bit  absolute ECCPAS.0;
  VRCON             : byte absolute $0019;
  VRCON_C1VREN      : bit  absolute VRCON.7;
  VRCON_C2VREN      : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VP6EN       : bit  absolute VRCON.4;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  CM1CON0           : byte absolute $001a;
  CM1CON0_C1ON      : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT     : bit  absolute CM1CON0.6;
  CM1CON0_C1OE      : bit  absolute CM1CON0.5;
  CM1CON0_C1POL     : bit  absolute CM1CON0.4;
  CM1CON0_C1R       : bit  absolute CM1CON0.3;
  CM1CON0_C1CH1     : bit  absolute CM1CON0.1;
  CM1CON0_C1CH0     : bit  absolute CM1CON0.0;
  CM2CON0           : byte absolute $001b;
  CM2CON0_C2ON      : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT     : bit  absolute CM2CON0.6;
  CM2CON0_C2OE      : bit  absolute CM2CON0.5;
  CM2CON0_C2POL     : bit  absolute CM2CON0.4;
  CM2CON0_C2R       : bit  absolute CM2CON0.3;
  CM2CON0_C2CH1     : bit  absolute CM2CON0.1;
  CM2CON0_C2CH0     : bit  absolute CM2CON0.0;
  CM2CON1           : byte absolute $001c;
  CM2CON1_MC1OUT    : bit  absolute CM2CON1.7;
  CM2CON1_MC2OUT    : bit  absolute CM2CON1.6;
  CM2CON1_T1ACS     : bit  absolute CM2CON1.5;
  CM2CON1_C1HYS     : bit  absolute CM2CON1.4;
  CM2CON1_C2HYS     : bit  absolute CM2CON1.3;
  CM2CON1_T1GSS     : bit  absolute CM2CON1.2;
  CM2CON1_C2SYNC    : bit  absolute CM2CON1.1;
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
  TRISC             : byte absolute $0087;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  PIE1              : byte absolute $008c;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_ECCPIE       : bit  absolute PIE1.5;
  PIE1_C2IE         : bit  absolute PIE1.4;
  PIE1_C1IE         : bit  absolute PIE1.3;
  PIE1_TMR2IE       : bit  absolute PIE1.2;
  PIE1_TMR1IE       : bit  absolute PIE1.1;
  PCON              : byte absolute $008e;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  ANSEL             : byte absolute $0091;
  ANSEL_ANS7        : bit  absolute ANSEL.7;
  ANSEL_ANS6        : bit  absolute ANSEL.6;
  ANSEL_ANS5        : bit  absolute ANSEL.5;
  ANSEL_ANS4        : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;
  PR2               : byte absolute $0092;
  WPUA              : byte absolute $0095;
  WPUA_WPUA5        : bit  absolute WPUA.5;
  WPUA_WPUA4        : bit  absolute WPUA.4;
  WPUA_WPUA2        : bit  absolute WPUA.3;
  WPUA_WPUA1        : bit  absolute WPUA.2;
  WPUA_WPUA0        : bit  absolute WPUA.1;
  IOCA              : byte absolute $0096;
  IOCA_IOCA5        : bit  absolute IOCA.5;
  IOCA_IOCA4        : bit  absolute IOCA.4;
  IOCA_IOCA3        : bit  absolute IOCA.3;
  IOCA_IOCA2        : bit  absolute IOCA.2;
  IOCA_IOCA1        : bit  absolute IOCA.1;
  IOCA_IOCA0        : bit  absolute IOCA.0;
  SRCON0            : byte absolute $0099;
  SRCON0_SR1        : bit  absolute SRCON0.7;
  SRCON0_SR0        : bit  absolute SRCON0.6;
  SRCON0_C1SEN      : bit  absolute SRCON0.5;
  SRCON0_C2REN      : bit  absolute SRCON0.4;
  SRCON0_PULSS      : bit  absolute SRCON0.3;
  SRCON0_PULSR      : bit  absolute SRCON0.2;
  SRCON0_SRCLKEN    : bit  absolute SRCON0.1;
  SRCON1            : byte absolute $009a;
  SRCON1_SRCS1      : bit  absolute SRCON1.7;
  SRCON1_SRCS0      : bit  absolute SRCON1.6;
  ADRESL            : byte absolute $009e;
  ADCON1            : byte absolute $009f;
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
  ADCON1_ADCS1      : bit  absolute ADCON1.5;
  ADCON1_ADCS0      : bit  absolute ADCON1.4;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-005:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA
  {$SET_STATE_RAM '007-007:SFR'}  // PORTC
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-017:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON, CCPR1L, CCPR1H, CCP1CON, PWM1CON, ECCPAS
  {$SET_STATE_RAM '019-01C:SFR'}  // VRCON, CM1CON0, CM2CON0, CM2CON1
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-085:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA
  {$SET_STATE_RAM '087-087:SFR'}  // TRISC
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-08E:SFR'}  // PCON
  {$SET_STATE_RAM '090-092:SFR'}  // OSCTUNE, ANSEL, PR2
  {$SET_STATE_RAM '095-096:SFR'}  // WPUA, IOCA
  {$SET_STATE_RAM '099-09A:SFR'}  // SRCON0, SRCON1
  {$SET_STATE_RAM '09E-09F:SFR'}  // ADRESL, ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:3F'} // PORTA
  {$SET_UNIMP_BITS '007:3F'} // PORTC
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:7B'} // PIR1
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '01A:F7'} // CM1CON0
  {$SET_UNIMP_BITS '01B:F7'} // CM2CON0
  {$SET_UNIMP_BITS '01C:DF'} // CM2CON1
  {$SET_UNIMP_BITS '085:3F'} // TRISA
  {$SET_UNIMP_BITS '087:3F'} // TRISC
  {$SET_UNIMP_BITS '08C:7B'} // PIE1
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '095:37'} // WPUA
  {$SET_UNIMP_BITS '096:3F'} // IOCA
  {$SET_UNIMP_BITS '099:FD'} // SRCON0
  {$SET_UNIMP_BITS '09A:C0'} // SRCON1
  {$SET_UNIMP_BITS '09F:70'} // ADCON1


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/OSC1/CLKIN
  // Pin  3 : RA4/AN3/T1G/OSC2/CLKOUT
  // Pin  4 : RA3/MCLR/Vpp
  // Pin  5 : RC5/CCP1/P1A
  // Pin  6 : RC4/C2OUT/P1B
  // Pin  7 : RC3/AN7/C12IN3-/P1C
  // Pin  8 : RC2/AN6/C12IN2-/P1D
  // Pin  9 : RC1/AN5/C12IN1-
  // Pin 10 : RC0/AN4/C2IN+
  // Pin 11 : RA2/AN2/T0CKI/INT/C1OUT
  // Pin 12 : RA1/AN1/C12IN0-/Vref/ICSPCK
  // Pin 13 : RA0/AN0/C1IN+/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '007:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON       = $03FF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $03FE}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_OFF      = $03FD}  // BOR Disabled
  {$define _BOREN_OFF      = $03FC}  // BOR Disabled

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_8MHZ    = $03FF}  // 8 MHz
  {$define _IOSCFS_4MHZ    = $03FB}  // 4 MHz

  // CP : Code Protection bit
  {$define _CP_OFF         = $03FF}  // Program memory code protection is disabled
  {$define _CP_ON          = $03F7}  // Program memory code protection is enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON       = $03FF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $03EF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $03FF}  // PWRT disabled
  {$define _PWRTE_ON       = $03DF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $03FF}  // WDT enabled
  {$define _WDTE_OFF       = $03BF}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $03FF}  // RC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, RC on RA5/OSC1/CLKIN
  {$define _FOSC_EXTRCIO   = $037F}  // RCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, RC on RA5/OSC1/CLKIN
  {$define _FOSC_INTOSCCLK = $02FF}  // INTOSC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $027F}  // INTOSCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_EC        = $01FF}  // EC: I/O function on RA4/OSC2/CLKOUT pin, CLKIN on RA5/OSC1/CLKIN
  {$define _FOSC_HS        = $017F}  // HS oscillator: High-speed crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_XT        = $00FF}  // XT oscillator: Crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_LP        = $007F}  // LP oscillator: Low-power crystal on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN

implementation
end.
