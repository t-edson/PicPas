unit PIC16F54;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F54'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 18}
{$SET PIC_NUMBANKS = 1}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 512}

interface
var
  INDF        : byte absolute $0000;
  TMR0        : byte absolute $0001;
  PCL         : byte absolute $0002;
  STATUS      : byte absolute $0003;
  STATUS_PA2  : bit  absolute STATUS.7;
  STATUS_PA1  : bit  absolute STATUS.6;
  STATUS_PA0  : bit  absolute STATUS.5;
  STATUS_nTO  : bit  absolute STATUS.4;
  STATUS_nPD  : bit  absolute STATUS.3;
  STATUS_Z    : bit  absolute STATUS.2;
  STATUS_DC   : bit  absolute STATUS.1;
  STATUS_C    : bit  absolute STATUS.0;
  FSR         : byte absolute $0004;
  PORTA       : byte absolute $0005;
  PORTA_T0CKI : bit  absolute PORTA.4;
  PORTA_RA3   : bit  absolute PORTA.3;
  PORTA_RA2   : bit  absolute PORTA.2;
  PORTA_RA1   : bit  absolute PORTA.1;
  PORTA_RA0   : bit  absolute PORTA.0;
  PORTB       : byte absolute $0006;
  PORTB_RB7   : bit  absolute PORTB.7;
  PORTB_RB6   : bit  absolute PORTB.6;
  PORTB_RB5   : bit  absolute PORTB.5;
  PORTB_RB4   : bit  absolute PORTB.4;
  PORTB_RB3   : bit  absolute PORTB.3;
  PORTB_RB2   : bit  absolute PORTB.2;
  PORTB_RB1   : bit  absolute PORTB.1;
  PORTB_RB0   : bit  absolute PORTB.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-006:SFR'}            // Bank 0 : INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB
  {$SET_STATE_RAM '007-01F:GPR'}           


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:1F'} // PORTA bits 7,6,5 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '004:E0'} // FSR bits 7,6,5 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : RA2
  // Pin  2 : RA3
  // Pin  3 : T0CKI
  // Pin  4 : MCLR/Vpp
  // Pin  5 : VSS
  // Pin  6 : RB0
  // Pin  7 : RB1
  // Pin  8 : RB2
  // Pin  9 : RB3
  // Pin 10 : RB4
  // Pin 11 : RB5
  // Pin 12 : RB6/ICSPCLK
  // Pin 13 : RB7/ICSPDAT
  // Pin 14 : VDD
  // Pin 15 : OSC2/CLKOUT
  // Pin 16 : OSC1/CLKIN
  // Pin 17 : RA0
  // Pin 18 : RA1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // OSC : Oscillator selection bits
  {$define _OSC_RC  = $0FFF}  // RC oscillator
  {$define _OSC_HS  = $0FFE}  // HS oscillator
  {$define _OSC_XT  = $0FFD}  // XT oscillator
  {$define _OSC_LP  = $0FFC}  // LP oscillator

  // WDT : Watchdog timer enable bit
  {$define _WDT_ON  = $0FFF}  // WDT enabled
  {$define _WDT_OFF = $0FFB}  // WDT disabled

  // CP : Code protection bit
  {$define _CP_OFF  = $0FFF}  // Code protection off
  {$define _CP_ON   = $0FF7}  // Code protection on

implementation
end.
