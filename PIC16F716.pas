unit PIC16F716;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F716'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 18}
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
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_CCP1IF       : bit  absolute PIR1.5;
  PIR1_TMR2IF       : bit  absolute PIR1.4;
  PIR1_TMR1IF       : bit  absolute PIR1.3;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
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
  PWM1CON           : byte absolute $0018;
  PWM1CON_PRSEN     : bit  absolute PWM1CON.7;
  PWM1CON_PDC6      : bit  absolute PWM1CON.6;
  PWM1CON_PDC5      : bit  absolute PWM1CON.5;
  PWM1CON_PDC4      : bit  absolute PWM1CON.4;
  PWM1CON_PDC3      : bit  absolute PWM1CON.3;
  PWM1CON_PDC2      : bit  absolute PWM1CON.2;
  PWM1CON_PDC1      : bit  absolute PWM1CON.1;
  PWM1CON_PDC0      : bit  absolute PWM1CON.0;
  ECCPAS            : byte absolute $0019;
  ECCPAS_ECCPASE    : bit  absolute ECCPAS.7;
  ECCPAS_ECCPAS2    : bit  absolute ECCPAS.6;
  ECCPAS_ECCPAS0    : bit  absolute ECCPAS.5;
  ECCPAS_PSSAC1     : bit  absolute ECCPAS.4;
  ECCPAS_PSSAC0     : bit  absolute ECCPAS.3;
  ECCPAS_PSSBD1     : bit  absolute ECCPAS.2;
  ECCPAS_PSSBD0     : bit  absolute ECCPAS.1;
  ADRES             : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADCS1      : bit  absolute ADCON0.7;
  ADCON0_ADCS0      : bit  absolute ADCON0.6;
  ADCON0_CHS2       : bit  absolute ADCON0.5;
  ADCON0_CHS1       : bit  absolute ADCON0.4;
  ADCON0_CHS0       : bit  absolute ADCON0.3;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.2;
  ADCON0_ADON       : bit  absolute ADCON0.1;
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
  PIE1              : byte absolute $008c;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_CCP1IE       : bit  absolute PIE1.5;
  PIE1_TMR2IE       : bit  absolute PIE1.4;
  PIE1_TMR1IE       : bit  absolute PIE1.3;
  PCON              : byte absolute $008e;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  PR2               : byte absolute $0092;
  ADCON1            : byte absolute $009f;
  ADCON1_PCFG2      : bit  absolute ADCON1.2;
  ADCON1_PCFG1      : bit  absolute ADCON1.1;
  ADCON1_PCFG0      : bit  absolute ADCON1.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-006:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-012:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON
  {$SET_STATE_RAM '015-019:SFR'}  // CCPR1L, CCPR1H, CCP1CON, PWM1CON, ECCPAS
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRES, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-086:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-08E:SFR'}  // PCON
  {$SET_STATE_RAM '092-092:SFR'}  // PR2
  {$SET_STATE_RAM '09F-09F:SFR'}  // ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:1F'} // PORTA
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:47'} // PIR1
  {$SET_UNIMP_BITS '010:3F'} // T1CON
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '019:DF'} // ECCPAS
  {$SET_UNIMP_BITS '01F:FD'} // ADCON0
  {$SET_UNIMP_BITS '085:1F'} // TRISA
  {$SET_UNIMP_BITS '08C:47'} // PIE1
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '09F:07'} // ADCON1


  // -- PIN mapping --

  // Pin  1 : RA2/AN2
  // Pin  2 : RA3/AN3/Vref
  // Pin  3 : RA4/T0CKI
  // Pin  4 : MCLR/Vpp
  // Pin  5 : Vss
  // Pin  6 : RB0/INT/ECCPAS2
  // Pin  7 : RB1/T1OSO/T1CKI
  // Pin  8 : RB2/T1OSI
  // Pin  9 : RB3/CCP1/P1A
  // Pin 10 : RB4/ECCPAS0
  // Pin 11 : RB5/P1B
  // Pin 12 : RB6/P1C
  // Pin 13 : RB7/P1D
  // Pin 14 : Vdd
  // Pin 15 : OSC2/CLKOUT
  // Pin 16 : OSC1/CLKIN
  // Pin 17 : RA0/AN0
  // Pin 18 : RA1/AN1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // CP : Code Protect
  {$define _CP_OFF    = $20CF}  // Program memory code protection is disabled
  {$define _CP_ON     = $20CE}  // Program memory code protection is enabled

  // BODENV : Brown-out Reset Voltage bit
  {$define _BODENV_40 = $20CF}  // VBOR set to 4.0V
  {$define _BODENV_25 = $20CD}  // VBOR set to 2.5V

  // BOREN : Brown-out Reset Enable bit
  {$define _BOREN_ON  = $20CF}  // BOR enabled
  {$define _BOREN_OFF = $20CB}  // BOR disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF = $20CF}  // PWRT disabled
  {$define _PWRTE_ON  = $20C7}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON   = $20DF}  // WDT enabled
  {$define _WDTE_OFF  = $20CF}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // FOSC : Oscillator Selection bits
  {$define _FOSC_RC   = $20EF}  // RC oscillator
  {$define _FOSC_HS   = $20CF}  // HS oscillator
  {$define _FOSC_XT   = $20AF}  // XT oscillator
  {$define _FOSC_LP   = $208F}  // LP oscillator

implementation
end.
