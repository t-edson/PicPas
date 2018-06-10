unit PIC12F510;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F510'}
{$SET PIC_MAXFREQ  = 8000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 2}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF            : byte absolute $0000;
  TMR0            : byte absolute $0001;
  PCL             : byte absolute $0002;
  STATUS          : byte absolute $0003;
  STATUS_GPWUF    : bit  absolute STATUS.7;
  STATUS_CWUF     : bit  absolute STATUS.6;
  STATUS_PA0      : bit  absolute STATUS.5;
  STATUS_TO       : bit  absolute STATUS.4;
  STATUS_PD       : bit  absolute STATUS.3;
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
  GPIO            : byte absolute $0006;
  GPIO_GP5        : bit  absolute GPIO.5;
  GPIO_GP4        : bit  absolute GPIO.4;
  GPIO_GP3        : bit  absolute GPIO.3;
  GPIO_GP2        : bit  absolute GPIO.2;
  GPIO_GP1        : bit  absolute GPIO.1;
  GPIO_GP0        : bit  absolute GPIO.0;
  CM1CON0         : byte absolute $0007;
  CM1CON0_C1OUT   : bit  absolute CM1CON0.7;
  CM1CON0_C1OUTEN : bit  absolute CM1CON0.6;
  CM1CON0_C1POL   : bit  absolute CM1CON0.5;
  CM1CON0_C1T0CS  : bit  absolute CM1CON0.4;
  CM1CON0_C1ON    : bit  absolute CM1CON0.3;
  CM1CON0_C1NREF  : bit  absolute CM1CON0.2;
  CM1CON0_C1PREF  : bit  absolute CM1CON0.1;
  CM1CON0_C1WU    : bit  absolute CM1CON0.0;
  ADCON0          : byte absolute $0008;
  ADCON0_ANS1     : bit  absolute ADCON0.7;
  ADCON0_ANS0     : bit  absolute ADCON0.6;
  ADCON0_ADCS1    : bit  absolute ADCON0.5;
  ADCON0_ADCS0    : bit  absolute ADCON0.4;
  ADCON0_CHS1     : bit  absolute ADCON0.3;
  ADCON0_CHS0     : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE : bit  absolute ADCON0.1;
  ADCON0_ADON     : bit  absolute ADCON0.0;
  ADRES           : byte absolute $0009;


  // -- Define RAM state values --
  {$CLEAR_STATE_RAM} 

  {$SET_STATE_RAM '000-009:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, GPIO, CM1CON0, ADCON0, ADRES
  {$SET_STATE_RAM '00A-01F:GPR'} 
  {$SET_STATE_RAM '02A-03F:GPR'} 


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL
  {$SET_UNIMP_BITS '006:3F'} // GPIO


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : GP5/OSC1/CLKIN
  // Pin  3 : GP4/OSC2
  // Pin  4 : GP3/MCLR/Vpp
  // Pin  5 : GP2/AN2/T0CKI/C1OUT
  // Pin  6 : GP1/AN1/C1IN-/ICSPCLK
  // Pin  7 : GP0/AN0/C1IN+/ICSPDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-7,1-6,2-5,3-4,4-3,5-2'} // GPIO


  // -- Bits Configuration --

  // IOSCFS : Internal Oscillator Frequency Select bit
  {$define _IOSCFS_ON  = $003F}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_OFF = $003E}  // 4 MHz INTOSC Speed

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON   = $003F}  // GP3/MCLR Functions as MCLR
  {$define _MCLRE_OFF  = $003D}  // GP3/MCLR pin functions as GP3, MCLR internally tied to VDD

  // CP : Code Protect
  {$define _CP_OFF     = $003F}  // Code protection off
  {$define _CP_ON      = $003B}  // Code protection on

  // WDT : Watchdog Timer Enable bit
  {$define _WDT_ON     = $003F}  // WDT enabled
  {$define _WDT_OFF    = $0037}  // WDT disabled

  // OSC : Oscillator Select
  {$define _OSC_ExtRC  = $003F}  // EXTRC with 1.125 ms DRT
  {$define _OSC_IntRC  = $002F}  // INTOSC with 1.125 ms DRT
  {$define _OSC_XT     = $001F}  // XT oscillator with 18 ms DRT
  {$define _OSC_LP     = $000F}  // LP oscillator with 18 ms DRT

implementation
end.
