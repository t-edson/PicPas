unit PIC16F57;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F57'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 2048}

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
  PORTC       : byte absolute $0007;
  PORTC_RC7   : bit  absolute PORTC.7;
  PORTC_RC6   : bit  absolute PORTC.6;
  PORTC_RC5   : bit  absolute PORTC.5;
  PORTC_RC4   : bit  absolute PORTC.4;
  PORTC_RC3   : bit  absolute PORTC.3;
  PORTC_RC2   : bit  absolute PORTC.2;
  PORTC_RC1   : bit  absolute PORTC.1;
  PORTC_RC0   : bit  absolute PORTC.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-007:SFR:ALLMAPPED'}  // Banks 0-3 : INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '008-00F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '010-01F:GPR:ALL'}       


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:1F'} // PORTA bits 7,6,5 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '004:80'} // FSR bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : T0CKI
  // Pin  2 : Vdd
  // Pin  3 : N/C
  // Pin  4 : Vss
  // Pin  5 : N/C
  // Pin  6 : RA0
  // Pin  7 : RA1
  // Pin  8 : RA2
  // Pin  9 : RA3
  // Pin 10 : RB0
  // Pin 11 : RB1
  // Pin 12 : RB2
  // Pin 13 : RB3
  // Pin 14 : RB4
  // Pin 15 : RB5
  // Pin 16 : RB6/ICSPCLK
  // Pin 17 : RB7/ICSPDAT
  // Pin 18 : RC0
  // Pin 19 : RC1
  // Pin 20 : RC2
  // Pin 21 : RC3
  // Pin 22 : RC4
  // Pin 23 : RC5
  // Pin 24 : RC6
  // Pin 25 : RC7
  // Pin 26 : OSC2/CLKOUT
  // Pin 27 : OSC1/CLKIN
  // Pin 28 : MCLR/Vpp


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-6,1-7,2-8,3-9,4-1'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-10,1-11,2-12,3-13,4-14,5-15,6-16,7-17'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-18,1-19,2-20,3-21,4-22,5-23,6-24,7-25'} // PORTC


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
