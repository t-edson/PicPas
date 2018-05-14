unit PIC16F570;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F570'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 8}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 2048}

interface
var
  INDF            : byte absolute $0000;
  TMR0            : byte absolute $0001;
  PCL             : byte absolute $0002;
  STATUS          : byte absolute $0003;
  STATUS_PA2      : bit  absolute STATUS.7;
  STATUS_PA1      : bit  absolute STATUS.6;
  STATUS_PA0      : bit  absolute STATUS.5;
  STATUS_TO       : bit  absolute STATUS.4;
  STATUS_PD       : bit  absolute STATUS.3;
  STATUS_Z        : bit  absolute STATUS.2;
  STATUS_DC       : bit  absolute STATUS.1;
  STATUS_C        : bit  absolute STATUS.0;
  FSR             : byte absolute $0004;
  OSCCAL          : byte absolute $0005;
  OSCCAL_CAL6     : bit  absolute OSCCAL.7;
  OSCCAL_CAL5     : bit  absolute OSCCAL.6;
  OSCCAL_CAL4     : bit  absolute OSCCAL.5;
  OSCCAL_CAL3     : bit  absolute OSCCAL.4;
  OSCCAL_CAL2     : bit  absolute OSCCAL.3;
  OSCCAL_CAL1     : bit  absolute OSCCAL.2;
  OSCCAL_CAL0     : bit  absolute OSCCAL.1;
  PORTA           : byte absolute $0006;
  PORTA_RA7       : bit  absolute PORTA.7;
  PORTA_RA6       : bit  absolute PORTA.6;
  PORTA_RA5       : bit  absolute PORTA.5;
  PORTA_RA4       : bit  absolute PORTA.4;
  PORTA_RA3       : bit  absolute PORTA.3;
  PORTA_RA2       : bit  absolute PORTA.2;
  PORTA_RA1       : bit  absolute PORTA.1;
  PORTA_RA0       : bit  absolute PORTA.0;
  PORTB           : byte absolute $0007;
  PORTB_RB7       : bit  absolute PORTB.7;
  PORTB_RB6       : bit  absolute PORTB.6;
  PORTB_RB5       : bit  absolute PORTB.5;
  PORTB_RB4       : bit  absolute PORTB.4;
  PORTB_RB3       : bit  absolute PORTB.3;
  PORTB_RB2       : bit  absolute PORTB.2;
  PORTB_RB1       : bit  absolute PORTB.1;
  PORTB_RB0       : bit  absolute PORTB.0;
  PORTC           : byte absolute $0008;
  PORTC_RC7       : bit  absolute PORTC.7;
  PORTC_RC6       : bit  absolute PORTC.6;
  PORTC_RC5       : bit  absolute PORTC.5;
  PORTC_RC4       : bit  absolute PORTC.4;
  PORTC_RC3       : bit  absolute PORTC.3;
  PORTC_RC2       : bit  absolute PORTC.2;
  PORTC_RC1       : bit  absolute PORTC.1;
  PORTC_RC0       : bit  absolute PORTC.0;
  ADCON0          : byte absolute $0009;
  ADCON0_ADCS1    : bit  absolute ADCON0.7;
  ADCON0_ADCS0    : bit  absolute ADCON0.6;
  ADCON0_CHS3     : bit  absolute ADCON0.5;
  ADCON0_CHS2     : bit  absolute ADCON0.4;
  ADCON0_CHS1     : bit  absolute ADCON0.3;
  ADCON0_CHS0     : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE : bit  absolute ADCON0.1;
  ADCON0_ADON     : bit  absolute ADCON0.0;
  ADRES           : byte absolute $000A;
  ADRES_ADRES7    : bit  absolute ADRES.7;
  ADRES_ADRES6    : bit  absolute ADRES.6;
  ADRES_ADRES5    : bit  absolute ADRES.5;
  ADRES_ADRES4    : bit  absolute ADRES.4;
  ADRES_ADRES3    : bit  absolute ADRES.3;
  ADRES_ADRES2    : bit  absolute ADRES.2;
  ADRES_ADRES1    : bit  absolute ADRES.1;
  ADRES_ADRES0    : bit  absolute ADRES.0;
  INTCON0         : byte absolute $000B;
  INTCON0_ADIF    : bit  absolute INTCON0.7;
  INTCON0_CWIF    : bit  absolute INTCON0.6;
  INTCON0_T0IF    : bit  absolute INTCON0.5;
  INTCON0_RBIF    : bit  absolute INTCON0.4;
  INTCON0_GIE     : bit  absolute INTCON0.3;
  EECON           : byte absolute $0021;
  EECON_FREE      : bit  absolute EECON.4;
  EECON_WRERR     : bit  absolute EECON.3;
  EECON_WREN      : bit  absolute EECON.2;
  EECON_WR        : bit  absolute EECON.1;
  EECON_RD        : bit  absolute EECON.0;
  EEDATA          : byte absolute $0025;
  EEADR           : byte absolute $0026;
  CM1CON0         : byte absolute $0027;
  CM1CON0_C1OUT   : bit  absolute CM1CON0.7;
  CM1CON0_C1OUTEN : bit  absolute CM1CON0.6;
  CM1CON0_C1POL   : bit  absolute CM1CON0.5;
  CM1CON0_C1T0CS  : bit  absolute CM1CON0.4;
  CM1CON0_C1ON    : bit  absolute CM1CON0.3;
  CM1CON0_C1NREF  : bit  absolute CM1CON0.2;
  CM1CON0_C1PREF  : bit  absolute CM1CON0.1;
  CM1CON0_C1WU    : bit  absolute CM1CON0.0;
  CM2CON0         : byte absolute $0028;
  CM2CON0_C2OUT   : bit  absolute CM2CON0.7;
  CM2CON0_C2OUTEN : bit  absolute CM2CON0.6;
  CM2CON0_C2POL   : bit  absolute CM2CON0.5;
  CM2CON0_C2PREF2 : bit  absolute CM2CON0.4;
  CM2CON0_C2ON    : bit  absolute CM2CON0.3;
  CM2CON0_C2NREF  : bit  absolute CM2CON0.2;
  CM2CON0_C2PREF1 : bit  absolute CM2CON0.1;
  CM2CON0_C2WU    : bit  absolute CM2CON0.0;
  VRCON           : byte absolute $0029;
  VRCON_VREN      : bit  absolute VRCON.7;
  VRCON_VROE1     : bit  absolute VRCON.6;
  VRCON_VROE2     : bit  absolute VRCON.5;
  VRCON_VRR       : bit  absolute VRCON.4;
  VRCON_VR3       : bit  absolute VRCON.3;
  VRCON_VR2       : bit  absolute VRCON.2;
  VRCON_VR1       : bit  absolute VRCON.1;
  VRCON_VR0       : bit  absolute VRCON.0;
  ANSEL           : byte absolute $002A;
  ANSEL_ANS7      : bit  absolute ANSEL.7;
  ANSEL_ANS6      : bit  absolute ANSEL.6;
  ANSEL_ANS5      : bit  absolute ANSEL.5;
  ANSEL_ANS4      : bit  absolute ANSEL.4;
  ANSEL_ANS3      : bit  absolute ANSEL.3;
  ANSEL_ANS2      : bit  absolute ANSEL.2;
  ANSEL_ANS1      : bit  absolute ANSEL.1;
  ANSEL_ANS0      : bit  absolute ANSEL.0;
  IW              : byte absolute $0061;
  INTCON1         : byte absolute $0065;
  INTCON1_ADIE    : bit  absolute INTCON1.7;
  INTCON1_CWIE    : bit  absolute INTCON1.6;
  INTCON1_T0IE    : bit  absolute INTCON1.5;
  INTCON1_RBIE    : bit  absolute INTCON1.4;
  INTCON1_WUR     : bit  absolute INTCON1.3;
  ISTATUS         : byte absolute $0066;
  IFSR            : byte absolute $0067;
  IBSR            : byte absolute $0068;
  OPACON          : byte absolute $0069;
  OPACON_OPA2ON   : bit  absolute OPACON.1;
  OPACON_OPA1ON   : bit  absolute OPACON.0;


// -- Define RAM state values --
  {$CLEAR_STATE_RAM} 

  {$SET_STATE_RAM '000-00B:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_STATE_RAM '00C-01F:GPR'} 
  {$SET_STATE_RAM '020-02B:SFR'}  // INDF, EECON, PCL, STATUS, FSR, EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL, INTCON0
  {$SET_STATE_RAM '02C-03F:GPR'} 
  {$SET_STATE_RAM '040-04B:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_STATE_RAM '04C-05F:GPR'} 
  {$SET_STATE_RAM '060-06B:SFR'}  // INDF, IW, PCL, STATUS, FSR, INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL, INTCON0
  {$SET_STATE_RAM '06C-07F:GPR'} 
  {$SET_STATE_RAM '080-08B:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_STATE_RAM '08C-09F:GPR'} 
  {$SET_STATE_RAM '0A0-0AB:SFR'}  // INDF, EECON, PCL, STATUS, FSR, EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL, INTCON0
  {$SET_STATE_RAM '0AC-0BF:GPR'} 
  {$SET_STATE_RAM '0C0-0CB:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_STATE_RAM '0CC-0DF:GPR'} 
  {$SET_STATE_RAM '0E0-0EB:SFR'}  // INDF, IW, PCL, STATUS, FSR, INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL, INTCON0
  {$SET_STATE_RAM '0EC-0FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '020-020:bnk0'} // INDF
  {$SET_MAPPED_RAM '022-024:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '02B-02B:bnk0'} // INTCON0
  {$SET_MAPPED_RAM '040-04B:bnk0'} // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_MAPPED_RAM '060-060:bnk0'} // INDF
  {$SET_MAPPED_RAM '062-064:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '06A-06B:bnk1'} // ANSEL, INTCON0
  {$SET_MAPPED_RAM '080-08B:bnk0'} // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_MAPPED_RAM '0A0-0AB:bnk1'} // INDF, EECON, PCL, STATUS, FSR, EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL, INTCON0
  {$SET_MAPPED_RAM '0C0-0CB:bnk0'} // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_MAPPED_RAM '0E0-0EB:bnk3'} // INDF, IW, PCL, STATUS, FSR, INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL, INTCON0


  // -- Initial values --

  {$SET_UNIMP_BITS '00B:F1'} // INTCON0
  {$SET_UNIMP_BITS '026:3F'} // EEADR
  {$SET_UNIMP_BITS '065:F1'} // INTCON1
  {$SET_UNIMP_BITS '068:07'} // IBSR
  {$SET_UNIMP_BITS '069:03'} // OPACON


  // -- PIN mapping --

  // Pin  1 : VPP/MCLR
  // Pin  2 : RA0/AN0
  // Pin  3 : RA1/AN1/C1IN+
  // Pin  4 : RA2/AN2/CVREF1
  // Pin  5 : RA3/AN3/C2IN+
  // Pin  6 : RA4/AN4/T0CKI
  // Pin  7 : RA5/AN5
  // Pin  8 : VSS
  // Pin  9 : RA7/CLKIN/OSC1
  // Pin 10 : RA6/CLKOUT/OSC2
  // Pin 11 : RC0
  // Pin 12 : RC1/AN6/OPA1OUT
  // Pin 13 : RC2/OPA1IN-
  // Pin 14 : RC3/OPA1IN+
  // Pin 15 : RC4/OPA2IN+
  // Pin 16 : RC5/OPA2IN-
  // Pin 17 : RC6/AN7/OPA2OUT
  // Pin 18 : RC7/C2IN-
  // Pin 19 : VSS
  // Pin 20 : VDD
  // Pin 21 : RB0
  // Pin 22 : RB1
  // Pin 23 : RB2
  // Pin 24 : RB3/C1OUT
  // Pin 25 : RB4/C2OUT
  // Pin 26 : RB5
  // Pin 27 : RB6/ICSPCLK
  // Pin 28 : RB7/ICSPDAT/C1IN-/CVREF2


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-2,1-3,2-4,3-5,4-6,5-7,6-10,7-9'} // PORTA
  {$MAP_RAM_TO_PIN '007:0-21,1-22,2-23,3-24,4-25,5-26,6-27,7-28'} // PORTB
  {$MAP_RAM_TO_PIN '008:0-11,1-12,2-13,3-14,4-15,5-16,6-17,7-18'} // PORTC


  // -- Bits Configuration --

  // DRTEN : 
  {$define _DRTEN_ON          = $03DF}  // DRT Enabled
  {$define _DRTEN_OFF         = $03DE}  // DRT Disabled

  // BOREN : 
  {$define _BOREN_ON          = $03DF}  // BOR Enabled
  {$define _BOREN_OFF         = $03DD}  // BOR Disabled

  // CPSW : Code Protection bit - Flash Data Memory
  {$define _CPSW_OFF          = $03DF}  // Code protection off
  {$define _CPSW_ON           = $03DB}  // Code protection on

  // IOSCFS : Internal Oscillator Frequency Select
  {$define _IOSCFS_8MHz       = $03DF}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_4MHz       = $03D7}  // 4 MHz INTOSC Speed

  // CP : Code Protection bit
  {$define _CP_OFF            = $03DF}  // Code protection off
  {$define _CP_ON             = $03CF}  // Code protection on

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON           = $03FF}  // Enabled
  {$define _WDTE_OFF          = $03DF}  // Disabled

  // FOSC : Oscillator
  {$define _FOSC_LP           = $021F}  // LP oscillator and 18 ms DRT
  {$define _FOSC_XT           = $025F}  // XT oscillator and 18 ms DRT
  {$define _FOSC_HS           = $029F}  // HS oscillator and 18 ms DRT
  {$define _FOSC_EC           = $02DF}  // EC oscillator with I/O function on OSC2/CLKOUT
  {$define _FOSC_INTRC_IO     = $031F}  // INTRC with I/O function on OSC2/CLKOUT
  {$define _FOSC_INTRC_CLKOUT = $035F}  // INTRC with CLKOUT function on OSC2/CLKOUT
  {$define _FOSC_EXTRC_IO     = $039F}  // EXTRC with I/O function on OSC2/CLKOUT
  {$define _FOSC_EXTRC_CLKOUT = $03DF}  // EXTRC with CLKOUT function on OSC2/CLKOUT

implementation
end.
