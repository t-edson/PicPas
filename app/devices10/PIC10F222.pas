unit PIC10F222;

// Define hardware
{$SET PIC_MODEL    = 'PIC10F222'}
{$SET PIC_MAXFREQ  = 8000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 1}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 512}

interface
var
  INDF            : byte absolute $0000;
  TMR0            : byte absolute $0001;
  PCL             : byte absolute $0002;
  STATUS          : byte absolute $0003;
  STATUS_GPWUF    : bit  absolute STATUS.7;
  STATUS_nTO      : bit  absolute STATUS.4;
  STATUS_nPD      : bit  absolute STATUS.3;
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
  OSCCAL_FOSC4    : bit  absolute OSCCAL.0;
  GPIO            : byte absolute $0006;
  GPIO_GP3        : bit  absolute GPIO.3;
  GPIO_GP2        : bit  absolute GPIO.2;
  GPIO_GP1        : bit  absolute GPIO.1;
  GPIO_GP0        : bit  absolute GPIO.0;
  ADCON0          : byte absolute $0007;
  ADCON0_ANS1     : bit  absolute ADCON0.7;
  ADCON0_ANS0     : bit  absolute ADCON0.6;
  ADCON0_CHS1     : bit  absolute ADCON0.3;
  ADCON0_CHS0     : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE : bit  absolute ADCON0.1;
  ADCON0_ADON     : bit  absolute ADCON0.0;
  ADRES           : byte absolute $0008;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-008:SFR'}            // Bank 0 : INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO, ADCON0, ADRES
  {$SET_STATE_RAM '009-01F:GPR'}           


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:DF'} // STATUS bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:0F'} // GPIO bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '007:CF'} // ADCON0 bits 5,4 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '004:E0'} // FSR bits 7,6,5 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : NC
  // Pin  2 : Vdd
  // Pin  3 : GP2/T0CKI/FOSC4
  // Pin  4 : GP1/AN1/ICSPCLK
  // Pin  5 : GP0/AN0/ICSPDAT
  // Pin  6 : NC
  // Pin  7 : Vss
  // Pin  8 : GP3/MCLR/Vpp


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-5,1-4,2-3,3-8'} // GPIO


  // -- Bits Configuration --

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_8MHZ = $0FFF}  // 8 MHz
  {$define _IOSCFS_4MHZ = $0FFE}  // 4 MHz

  // MCPU : Master Clear Pull-up Enable bit
  {$define _MCPU_OFF    = $0FFF}  // Pull-up disabled
  {$define _MCPU_ON     = $0FFD}  // Pull-up enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON     = $0FFF}  // WDT enabled
  {$define _WDTE_OFF    = $0FFB}  // WDT disabled

  // CP : Code protection bit
  {$define _CP_OFF      = $0FFF}  // Code protection off
  {$define _CP_ON       = $0FF7}  // Code protection on

  // MCLRE : GP3/MCLR Pin Function Select bit
  {$define _MCLRE_ON    = $0FFF}  // GP3/MCLR pin function is MCLR
  {$define _MCLRE_OFF   = $0FEF}  // GP3/MCLR pin function is digital I/O, MCLR internally tied to VDD

implementation
end.
