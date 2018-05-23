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
  STATUS_TO       : bit  absolute STATUS.6;
  STATUS_PD       : bit  absolute STATUS.5;
  STATUS_Z        : bit  absolute STATUS.4;
  STATUS_DC       : bit  absolute STATUS.3;
  STATUS_C        : bit  absolute STATUS.2;
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
  ADCON0_ANS1     : bit  absolute ADCON0.5;
  ADCON0_ANS0     : bit  absolute ADCON0.4;
  ADCON0_GO_nDONE : bit  absolute ADCON0.3;
  ADCON0_ADON     : bit  absolute ADCON0.2;
  ADCON0_GO       : bit  absolute ADCON0.1;
  ADRES           : byte absolute $0008;
  ADRES_ADRES7    : bit  absolute ADRES.7;
  ADRES_ADRES6    : bit  absolute ADRES.6;
  ADRES_ADRES5    : bit  absolute ADRES.5;
  ADRES_ADRES4    : bit  absolute ADRES.4;
  ADRES_ADRES3    : bit  absolute ADRES.3;
  ADRES_ADRES2    : bit  absolute ADRES.2;
  ADRES_ADRES1    : bit  absolute ADRES.1;
  ADRES_ADRES0    : bit  absolute ADRES.0;


  // -- Define RAM state values --
  {$CLEAR_STATE_RAM} 

  {$SET_STATE_RAM '000-008:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO, ADCON0, ADRES
  {$SET_STATE_RAM '009-01F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '003:DF'} // STATUS
  {$SET_UNIMP_BITS '006:0F'} // GPIO
  {$SET_UNIMP_BITS '007:CF'} // ADCON0
  {$SET_UNIMP_BITS1 '004:E0'} // FSR Bits 7,6,5 always read as 1

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

  // MCLRE : GP3/MCLR Pin Function Select bit
  {$define _MCLRE_ON    = $001F}  // GP3/MCLR pin function is MCLR
  {$define _MCLRE_OFF   = $001E}  // GP3/MCLR pin function is digital I/O, MCLR internally tied to VDD

  // CP : Code protection bit
  {$define _CP_OFF      = $001F}  // Code protection off
  {$define _CP_ON       = $001D}  // Code protection on

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON     = $001F}  // WDT enabled
  {$define _WDTE_OFF    = $001B}  // WDT disabled

  // MCPU : Master Clear Pull-up Enable bit
  {$define _MCPU_OFF    = $001F}  // Pull-up disabled
  {$define _MCPU_ON     = $0017}  // Pull-up enabled

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_8MHZ = $001F}  // 8 MHz
  {$define _IOSCFS_4MHZ = $000F}  // 4 MHz

implementation
end.
