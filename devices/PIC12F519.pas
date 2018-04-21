unit PIC12F519;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F519'}
{$SET PIC_MAXFREQ  = 8000000}
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
  GPIO         : byte absolute $0006;
  GPIO_GP5     : bit  absolute GPIO.5;
  GPIO_GP4     : bit  absolute GPIO.4;
  GPIO_GP3     : bit  absolute GPIO.3;
  GPIO_GP2     : bit  absolute GPIO.2;
  GPIO_GP1     : bit  absolute GPIO.1;
  GPIO_GP0     : bit  absolute GPIO.0;
  EECON        : byte absolute $0021;
  EECON_FREE   : bit  absolute EECON.4;
  EECON_WRERR  : bit  absolute EECON.3;
  EECON_WREN   : bit  absolute EECON.2;
  EECON_WR     : bit  absolute EECON.1;
  EECON_RD     : bit  absolute EECON.0;
  EEDATA       : byte absolute $0025;
  EEADR        : byte absolute $0026;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-006:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO
  {$SET_STATE_RAM '007-01F:GPR'} 
  {$SET_STATE_RAM '020-026:SFR'}  // INDF, EECON, PCL, STATUS, FSR, EEDATA, EEADR
  {$SET_STATE_RAM '027-03F:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '020-020:bnk0'} // INDF
  {$SET_MAPPED_RAM '022-024:bnk0'} // PCL, STATUS, FSR


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '003:BF'} // STATUS
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL
  {$SET_UNIMP_BITS '006:3F'} // GPIO
  {$SET_UNIMP_BITS '021:1F'} // EECON
  {$SET_UNIMP_BITS '026:3F'} // EEADR


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

  // CPDF : Code Protection bit - Flash Data Memory
  {$define _CPDF_OFF    = $007F}  // Code protection off
  {$define _CPDF_ON     = $007E}  // Code protection on

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_8MHz = $007F}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_4MHz = $007D}  // 4 MHz INTOSC Speed

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON    = $007F}  // RB3/MCLR Functions as MCLR
  {$define _MCLRE_OFF   = $007B}  // RB3/MCLR Functions as RB3

  // CP : Code Protection bit
  {$define _CP_OFF      = $007F}  // Code protection off
  {$define _CP_ON       = $0077}  // Code protection on

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON     = $007F}  // Enabled
  {$define _WDTE_OFF    = $006F}  // Disabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_LP     = $001F}  // LP Osc With 18 ms DRT
  {$define _FOSC_XT     = $003F}  // XT Osc With 18 ms DRT
  {$define _FOSC_INTRC  = $005F}  // INTRC With 1 ms DRT
  {$define _FOSC_EXTRC  = $007F}  // EXTRC With 1 ms DRT

implementation
end.
