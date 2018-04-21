unit PIC10F204;

// Define hardware
{$SET PIC_MODEL    = 'PIC10F204'}
{$SET PIC_MAXFREQ  = 4000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 1}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 256}

interface
var
  INDF           : byte absolute $0000;
  TMR0           : byte absolute $0001;
  PCL            : byte absolute $0002;
  STATUS         : byte absolute $0003;
  STATUS_GPWUF   : bit  absolute STATUS.7;
  STATUS_CWUF    : bit  absolute STATUS.6;
  STATUS_TO      : bit  absolute STATUS.5;
  STATUS_PD      : bit  absolute STATUS.4;
  STATUS_Z       : bit  absolute STATUS.3;
  STATUS_DC      : bit  absolute STATUS.2;
  STATUS_C       : bit  absolute STATUS.1;
  FSR            : byte absolute $0004;
  OSCCAL         : byte absolute $0005;
  OSCCAL_CAL6    : bit  absolute OSCCAL.7;
  OSCCAL_CAL5    : bit  absolute OSCCAL.6;
  OSCCAL_CAL4    : bit  absolute OSCCAL.5;
  OSCCAL_CAL3    : bit  absolute OSCCAL.4;
  OSCCAL_CAL2    : bit  absolute OSCCAL.3;
  OSCCAL_CAL1    : bit  absolute OSCCAL.2;
  OSCCAL_CAL0    : bit  absolute OSCCAL.1;
  OSCCAL_FOSC4   : bit  absolute OSCCAL.0;
  GPIO           : byte absolute $0006;
  GPIO_GP3       : bit  absolute GPIO.3;
  GPIO_GP2       : bit  absolute GPIO.2;
  GPIO_GP1       : bit  absolute GPIO.1;
  GPIO_GP0       : bit  absolute GPIO.0;
  CMCON0         : byte absolute $0007;
  CMCON0_CMPOUT  : bit  absolute CMCON0.7;
  CMCON0_COUTEN  : bit  absolute CMCON0.6;
  CMCON0_POL     : bit  absolute CMCON0.5;
  CMCON0_CMPT0CS : bit  absolute CMCON0.4;
  CMCON0_CMPON   : bit  absolute CMCON0.3;
  CMCON0_CNREF   : bit  absolute CMCON0.2;
  CMCON0_CPREF   : bit  absolute CMCON0.1;
  CMCON0_CWU     : bit  absolute CMCON0.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO, CMCON0
  {$SET_STATE_RAM '010-01F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '003:DF'} // STATUS
  {$SET_UNIMP_BITS '006:0F'} // GPIO


  // -- PIN mapping --

  // Pin  1 : NC
  // Pin  2 : Vdd
  // Pin  3 : GP2/T0CKI/COUT/FOSC4
  // Pin  4 : GP1/ICSPCLK/CIN-
  // Pin  5 : GP0/ICSPDAT/CIN+
  // Pin  6 : NC
  // Pin  7 : Vss
  // Pin  8 : GP3/MCLR/Vpp


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-5,1-4,2-3,3-8'} // GPIO


  // -- Bits Configuration --

  // MCLRE : Master Clear Enable
  {$define _MCLRE_ON  = $001D}  // GP3/MCLR pin function  is MCLR
  {$define _MCLRE_OFF = $001C}  // GP3/MCLR pin fuction is digital I/O, MCLR internally tied to VDD

  // CP : Code Protect
  {$define _CP_OFF    = $001E}  // Code protection off
  {$define _CP_ON     = $001C}  // Code protection on

  // WDTE : Watchdog Timer
  {$define _WDTE_ON   = $001C}  // WDT enabled
  {$define _WDTE_OFF  = $0018}  // WDT disabled

  // OSC : Oscillator
  {$define _OSC_IntRC = $001C}  // This is the only option. It is here for backward compatibility

implementation
end.
