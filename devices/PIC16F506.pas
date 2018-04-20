unit PIC16F506;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F506'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF            : byte absolute $0000;
  TMR0            : byte absolute $0001;
  PCL             : byte absolute $0002;
  STATUS          : byte absolute $0003;
  STATUS_RBWUF    : bit  absolute STATUS.7;
  STATUS_CWUF     : bit  absolute STATUS.6;
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
  PORTB           : byte absolute $0006;
  PORTB_RB5       : bit  absolute PORTB.5;
  PORTB_RB4       : bit  absolute PORTB.4;
  PORTB_RB3       : bit  absolute PORTB.3;
  PORTB_RB2       : bit  absolute PORTB.2;
  PORTB_RB1       : bit  absolute PORTB.1;
  PORTB_RB0       : bit  absolute PORTB.0;
  PORTC           : byte absolute $0007;
  PORTC_RC5       : bit  absolute PORTC.5;
  PORTC_RC4       : bit  absolute PORTC.4;
  PORTC_RC3       : bit  absolute PORTC.3;
  PORTC_RC2       : bit  absolute PORTC.2;
  PORTC_RC1       : bit  absolute PORTC.1;
  PORTC_RC0       : bit  absolute PORTC.0;
  CM1CON0         : byte absolute $0008;
  CM1CON0_C1OUT   : bit  absolute CM1CON0.7;
  CM1CON0_C1OUTEN : bit  absolute CM1CON0.6;
  CM1CON0_C1POL   : bit  absolute CM1CON0.5;
  CM1CON0_C1T0CS  : bit  absolute CM1CON0.4;
  CM1CON0_C1ON    : bit  absolute CM1CON0.3;
  CM1CON0_C1NREF  : bit  absolute CM1CON0.2;
  CM1CON0_C1PREF  : bit  absolute CM1CON0.1;
  CM1CON0_C1WU    : bit  absolute CM1CON0.0;
  ADCON0          : byte absolute $0009;
  ADCON0_ANS1     : bit  absolute ADCON0.7;
  ADCON0_ANS0     : bit  absolute ADCON0.6;
  ADCON0_ADCS1    : bit  absolute ADCON0.5;
  ADCON0_ADCS0    : bit  absolute ADCON0.4;
  ADCON0_CHS1     : bit  absolute ADCON0.3;
  ADCON0_CHS0     : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE : bit  absolute ADCON0.1;
  ADCON0_ADON     : bit  absolute ADCON0.0;
  ADRES           : byte absolute $000A;
  CM2CON0         : byte absolute $000B;
  CM2CON0_C2OUT   : bit  absolute CM2CON0.7;
  CM2CON0_C2OUTEN : bit  absolute CM2CON0.6;
  CM2CON0_C2POL   : bit  absolute CM2CON0.5;
  CM2CON0_C2PREF2 : bit  absolute CM2CON0.4;
  CM2CON0_C2ON    : bit  absolute CM2CON0.3;
  CM2CON0_C2NREF  : bit  absolute CM2CON0.2;
  CM2CON0_C2PREF1 : bit  absolute CM2CON0.1;
  CM2CON0_C2WU    : bit  absolute CM2CON0.0;
  VRCON           : byte absolute $000C;
  VRCON_VREN      : bit  absolute VRCON.7;
  VRCON_VROE      : bit  absolute VRCON.6;
  VRCON_VRR       : bit  absolute VRCON.5;
  VRCON_VR3       : bit  absolute VRCON.3;
  VRCON_VR2       : bit  absolute VRCON.2;
  VRCON_VR1       : bit  absolute VRCON.1;
  VRCON_VR0       : bit  absolute VRCON.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-00C:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTB, PORTC, CM1CON0, ADCON0, ADRES, CM2CON0, VRCON
  {$SET_STATE_RAM '00D-01F:GPR'} 
  {$SET_STATE_RAM '02D-03F:GPR'} 
  {$SET_STATE_RAM '04D-05F:GPR'} 
  {$SET_STATE_RAM '06D-07F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL
  {$SET_UNIMP_BITS '006:3F'} // PORTB
  {$SET_UNIMP_BITS '007:3F'} // PORTC
  {$SET_UNIMP_BITS '00C:EF'} // VRCON


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RB5/OSC1/CLKIN
  // Pin  3 : RB4/OSC2/CLKOUT
  // Pin  4 : RB3/MCLR/Vpp
  // Pin  5 : RC5/T0CKI
  // Pin  6 : RC4/C2OUT
  // Pin  7 : RC3
  // Pin  8 : RC2/CVref
  // Pin  9 : RC1/C2IN-
  // Pin 10 : RC0/C2IN+
  // Pin 11 : RB2/AN2/C1OUT
  // Pin 12 : RB1/AN1/C1IN-/ICSPCLK
  // Pin 13 : RB0/AN0/C1IN+/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_ON          = $007F}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_OFF         = $007E}  // 4 MHz INTOSC Speed

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON           = $007F}  // RB3/MCLR pin functions as MCLR
  {$define _MCLRE_OFF          = $007D}  // RB3/MCLR pin functions as RB3, MCLR tied internally to VDD

  // CP : Code Protect
  {$define _CP_OFF             = $007F}  // Code protection off
  {$define _CP_ON              = $007B}  // Code protection on

  // WDT : Watchdog Timer Enable bit
  {$define _WDT_ON             = $007F}  // WDT enabled
  {$define _WDT_OFF            = $0077}  // WDT disabled

  // OSC : Oscillator Selection bits
  {$define _OSC_LP             = $000F}  // LP oscillator and 18 ms DRT
  {$define _OSC_XT             = $001F}  // XT oscillator and 18 ms DRT
  {$define _OSC_HS             = $002F}  // HS oscillator and 18 ms DRT
  {$define _OSC_EC             = $003F}  // EC Osc With RB4 and 1.125 ms DRT
  {$define _OSC_IntRC_RB4EN    = $004F}  // INTRC With RB4 and 1.125 ms DRT
  {$define _OSC_IntRC_CLKOUTEN = $005F}  // INTRC With CLKOUT and 1.125 ms DRT
  {$define _OSC_ExtRC_RB4EN    = $006F}  // EXTRC With RB4 and 1.125 ms DRT
  {$define _OSC_ExtRC_CLKOUTEN = $007F}  // EXTRC With CLKOUT and 1.125 ms DRT

implementation
end.
