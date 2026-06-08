unit PIC10F202;

// Define hardware
{$SET PIC_MODEL    = 'PIC10F202'}
{$SET PIC_MAXFREQ  = 4000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 1}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 512}

interface
var
  INDF         : byte absolute $0000;
  TMR0         : byte absolute $0001;
  PCL          : byte absolute $0002;
  STATUS       : byte absolute $0003;
  STATUS_GPWUF : bit  absolute STATUS.7;
  STATUS_nTO   : bit  absolute STATUS.4;
  STATUS_nPD   : bit  absolute STATUS.3;
  STATUS_Z     : bit  absolute STATUS.2;
  STATUS_DC    : bit  absolute STATUS.1;
  STATUS_C     : bit  absolute STATUS.0;
  FSR          : byte absolute $0004;
  OSCCAL       : byte absolute $0005;
  OSCCAL_CAL6  : bit  absolute OSCCAL.7;
  OSCCAL_CAL5  : bit  absolute OSCCAL.6;
  OSCCAL_CAL4  : bit  absolute OSCCAL.5;
  OSCCAL_CAL3  : bit  absolute OSCCAL.4;
  OSCCAL_CAL2  : bit  absolute OSCCAL.3;
  OSCCAL_CAL1  : bit  absolute OSCCAL.2;
  OSCCAL_CAL0  : bit  absolute OSCCAL.1;
  OSCCAL_FOSC4 : bit  absolute OSCCAL.0;
  GPIO         : byte absolute $0006;
  GPIO_GP3     : bit  absolute GPIO.3;
  GPIO_GP2     : bit  absolute GPIO.2;
  GPIO_GP1     : bit  absolute GPIO.1;
  GPIO_GP0     : bit  absolute GPIO.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-006:SFR'}            // Bank 0 : INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO
  {$SET_STATE_RAM '008-01F:GPR'}           


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:DF'} // STATUS bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:0F'} // GPIO bits 7,6,5,4 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '004:E0'} // FSR bits 7,6,5 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : NC
  // Pin  2 : Vdd
  // Pin  3 : GP2/T0CKI/FOSC4
  // Pin  4 : GP1/ICSPCLK
  // Pin  5 : GP0/ICSPDAT
  // Pin  6 : NC
  // Pin  7 : Vss
  // Pin  8 : GP3/MCLR/Vpp


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-5,1-4,2-3,3-8'} // GPIO


  // -- Bits Configuration --

  // OSC : Oscillator
  {$define _OSC_IntRC = $0FFF}  // This is the only option. It is here for backward compatibility

  // WDTE : Watchdog Timer
  {$define _WDTE_ON   = $0FFF}  // WDT enabled
  {$define _WDTE_OFF  = $0FFB}  // WDT disabled

  // CP : Code Protect
  {$define _CP_OFF    = $0FFF}  // Code protection off
  {$define _CP_ON     = $0FF7}  // Code protection on

  // MCLRE : Master Clear Enable
  {$define _MCLRE_ON  = $0FFF}  // GP3/MCLR pin function  is MCLR
  {$define _MCLRE_OFF = $0FEF}  // GP3/MCLR pin fuction is digital I/O, MCLR internally tied to VDD

implementation
end.
