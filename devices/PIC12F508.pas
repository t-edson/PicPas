unit PIC12F508;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F508'}
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
  STATUS_TO    : bit  absolute STATUS.6;
  STATUS_PD    : bit  absolute STATUS.5;
  STATUS_Z     : bit  absolute STATUS.4;
  STATUS_DC    : bit  absolute STATUS.3;
  STATUS_C     : bit  absolute STATUS.2;
  FSR          : byte absolute $0004;
  OSCCAL       : byte absolute $0005;
  OSCCAL_CAL6  : bit  absolute OSCCAL.7;
  OSCCAL_CAL5  : bit  absolute OSCCAL.6;
  OSCCAL_CAL4  : bit  absolute OSCCAL.5;
  OSCCAL_CAL3  : bit  absolute OSCCAL.4;
  OSCCAL_CAL2  : bit  absolute OSCCAL.3;
  OSCCAL_CAL1  : bit  absolute OSCCAL.2;
  OSCCAL_CAL0  : bit  absolute OSCCAL.1;
  GPIO         : byte absolute $0006;
  GPIO_GP5     : bit  absolute GPIO.5;
  GPIO_GP4     : bit  absolute GPIO.4;
  GPIO_GP3     : bit  absolute GPIO.3;
  GPIO_GP2     : bit  absolute GPIO.2;
  GPIO_GP1     : bit  absolute GPIO.1;
  GPIO_GP0     : bit  absolute GPIO.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-006:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO
  {$SET_STATE_RAM '007-01F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '003:9F'} // STATUS
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL
  {$SET_UNIMP_BITS '006:3F'} // GPIO


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : GP5/OSC1/CLKIN
  // Pin  3 : GP4/OSC2
  // Pin  4 : GP3/MCLR/Vpp
  // Pin  5 : GP2/T0CKI
  // Pin  6 : GP1/ICSPCLK
  // Pin  7 : GP0/ICSPDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --



  // -- Bits Configuration --

  // MCLRE : GP3/MCLR Pin Function Select bit
  {$define _MCLRE_ON  = $001F}  // GP3/MCLR pin function is MCLR
  {$define _MCLRE_OFF = $001E}  // GP3/MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF    = $001F}  // Code protection off
  {$define _CP_ON     = $001D}  // Code protection on

  // WDT : Watchdog Timer Enable bit
  {$define _WDT_ON    = $001F}  // WDT enabled
  {$define _WDT_OFF   = $001B}  // WDT disabled

  // OSC : Oscillator Selection bits
  {$define _OSC_ExtRC = $001F}  // external RC oscillator
  {$define _OSC_LP    = $0007}  // LP oscillator
  {$define _OSC_XT    = $000F}  // XT oscillator
  {$define _OSC_IntRC = $0017}  // internal RC oscillator

implementation
end.
