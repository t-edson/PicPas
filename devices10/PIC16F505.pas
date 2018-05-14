unit PIC16F505;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F505'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 14}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF         : byte absolute $0000;
  TMR0         : byte absolute $0001;
  PCL          : byte absolute $0002;
  STATUS       : byte absolute $0003;
  STATUS_RBWUF : bit  absolute STATUS.7;
  STATUS_PA0   : bit  absolute STATUS.6;
  STATUS_TO    : bit  absolute STATUS.5;
  STATUS_PD    : bit  absolute STATUS.4;
  STATUS_Z     : bit  absolute STATUS.3;
  STATUS_DC    : bit  absolute STATUS.2;
  STATUS_C     : bit  absolute STATUS.1;
  FSR          : byte absolute $0004;
  OSCCAL       : byte absolute $0005;
  OSCCAL_CAL6  : bit  absolute OSCCAL.7;
  OSCCAL_CAL5  : bit  absolute OSCCAL.6;
  OSCCAL_CAL4  : bit  absolute OSCCAL.5;
  OSCCAL_CAL3  : bit  absolute OSCCAL.4;
  OSCCAL_CAL2  : bit  absolute OSCCAL.3;
  OSCCAL_CAL1  : bit  absolute OSCCAL.2;
  OSCCAL_CAL0  : bit  absolute OSCCAL.1;
  PORTB        : byte absolute $0006;
  PORTB_RB5    : bit  absolute PORTB.5;
  PORTB_RB4    : bit  absolute PORTB.4;
  PORTB_RB3    : bit  absolute PORTB.3;
  PORTB_RB2    : bit  absolute PORTB.2;
  PORTB_RB1    : bit  absolute PORTB.1;
  PORTB_RB0    : bit  absolute PORTB.0;
  PORTC        : byte absolute $0007;
  PORTC_RC5    : bit  absolute PORTC.5;
  PORTC_RC4    : bit  absolute PORTC.4;
  PORTC_RC3    : bit  absolute PORTC.3;
  PORTC_RC2    : bit  absolute PORTC.2;
  PORTC_RC1    : bit  absolute PORTC.1;
  PORTC_RC0    : bit  absolute PORTC.0;


// -- Define RAM state values --
  {$CLEAR_STATE_RAM} 

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTB, PORTC
  {$SET_STATE_RAM '008-01F:GPR'} 
  {$SET_STATE_RAM '028-03F:GPR'} 
  {$SET_STATE_RAM '048-05F:GPR'} 
  {$SET_STATE_RAM '068-07F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL
  {$SET_UNIMP_BITS '006:3F'} // PORTB
  {$SET_UNIMP_BITS '007:3F'} // PORTC


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RB5/OSC1/CLKIN
  // Pin  3 : RB4/OSC2/CLKOUT
  // Pin  4 : RB3/MCLR/Vpp
  // Pin  5 : RC5/T0CKI
  // Pin  6 : RC4
  // Pin  7 : RC3
  // Pin  8 : RC2
  // Pin  9 : RC1
  // Pin 10 : RC0
  // Pin 11 : RB2
  // Pin 12 : RB1/ICSPCLK
  // Pin 13 : RB0/ICSPDAT
  // Pin 14 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-13,1-12,2-11,3-4,4-3,5-2'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-10,1-9,2-8,3-7,4-6,5-5'} // PORTC


  // -- Bits Configuration --

  // MCLRE : RB3/MCLR Pin Function Select bit
  {$define _MCLRE_ON           = $003F}  // RB3/MCLR pin function is MCLR
  {$define _MCLRE_OFF          = $003E}  // GP3/MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF             = $003F}  // Code protection off
  {$define _CP_ON              = $003D}  // Code protection on

  // WDT : Watchdog Timer Enable bit
  {$define _WDT_ON             = $003F}  // WDT enabled
  {$define _WDT_OFF            = $003B}  // WDT disabled

  // OSC : Oscillator Selection bits
  {$define _OSC_ExtRC_CLKOUTEN = $003F}  // External RC oscillator/CLKOUT function on RB4/OSC2/CLKOUT pin
  {$define _OSC_ExtRC_RB4EN    = $0037}  // External RC oscillator/RB4 function on RB4/OSC2/CLKOUT pin
  {$define _OSC_IntRC_CLKOUTEN = $002F}  // Internal RC oscillator/CLKOUT function on RB4/OSC2/CLKOUT pin
  {$define _OSC_IntRC_RB4EN    = $0027}  // Internal RC oscillator/RB4 function on RB4/OSC2/CLKOUT pin
  {$define _OSC_EC             = $001F}  // EC oscillator/RB4 function on RB4/OSC2/CLKOUT pin
  {$define _OSC_HS             = $0017}  // HS oscillator
  {$define _OSC_XT             = $000F}  // XT oscillator
  {$define _OSC_LP             = $0007}  // LP oscillator

implementation
end.
