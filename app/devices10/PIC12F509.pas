unit PIC12F509;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F509'}
{$SET PIC_MAXFREQ  = 4000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 2}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF         : byte absolute $0000;
  TMR0         : byte absolute $0001;
  PCL          : byte absolute $0002;
  STATUS       : byte absolute $0003;
  STATUS_GPWUF : bit  absolute STATUS.7;
  STATUS_PA0   : bit  absolute STATUS.5;
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
  GPIO         : byte absolute $0006;
  GPIO_GP5     : bit  absolute GPIO.5;
  GPIO_GP4     : bit  absolute GPIO.4;
  GPIO_GP3     : bit  absolute GPIO.3;
  GPIO_GP2     : bit  absolute GPIO.2;
  GPIO_GP1     : bit  absolute GPIO.1;
  GPIO_GP0     : bit  absolute GPIO.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-006:SFR:ALLMAPPED'}  // Banks 0-1 : INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO
  {$SET_STATE_RAM '007-00F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '010-01F:GPR:ALL'}       


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:BF'} // STATUS bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '004:DF'} // FSR bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL bit 0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:3F'} // GPIO bits 7,6 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '004:C0'} // FSR bits 7,6 un-implemented (read as 1)


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

  {$MAP_RAM_TO_PIN '006:0-7,1-6,2-5,3-4,4-3,5-2'} // GPIO


  // -- Bits Configuration --

  // OSC : Oscillator Selection bits
  {$define _OSC_ExtRC = $0FFF}  // external RC oscillator
  {$define _OSC_LP    = $0FFC}  // LP oscillator
  {$define _OSC_XT    = $0FFD}  // XT oscillator
  {$define _OSC_IntRC = $0FFE}  // internal RC oscillator

  // WDT : Watchdog Timer Enable bit
  {$define _WDT_ON    = $0FFF}  // WDT enabled
  {$define _WDT_OFF   = $0FFB}  // WDT disabled

  // CP : Code Protection bit
  {$define _CP_OFF    = $0FFF}  // Code protection off
  {$define _CP_ON     = $0FF7}  // Code protection on

  // MCLRE : GP3/MCLR Pin Function Select bit
  {$define _MCLRE_ON  = $0FFF}  // GP3/MCLR pin function is MCLR
  {$define _MCLRE_OFF = $0FEF}  // GP3/MCLR pin function is digital input, MCLR internally tied to VDD

implementation
end.
