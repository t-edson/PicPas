unit PIC16F84A;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F84A'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 18}
{$SET PIC_NUMBANKS = 2}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF              : byte absolute $0000;
  TMR0              : byte absolute $0001;
  PCL               : byte absolute $0002;
  STATUS            : byte absolute $0003;
  STATUS_IRP        : bit  absolute STATUS.7;
  STATUS_RP1        : bit  absolute STATUS.6;
  STATUS_RP0        : bit  absolute STATUS.5;
  STATUS_nTO        : bit  absolute STATUS.4;
  STATUS_nPD        : bit  absolute STATUS.3;
  STATUS_Z          : bit  absolute STATUS.2;
  STATUS_DC         : bit  absolute STATUS.1;
  STATUS_C          : bit  absolute STATUS.0;
  FSR               : byte absolute $0004;
  PORTA             : byte absolute $0005;
  PORTA_RA4         : bit  absolute PORTA.4;
  PORTA_RA3         : bit  absolute PORTA.3;
  PORTA_RA2         : bit  absolute PORTA.2;
  PORTA_RA1         : bit  absolute PORTA.1;
  PORTA_RA0         : bit  absolute PORTA.0;
  PORTB             : byte absolute $0006;
  PORTB_RB7         : bit  absolute PORTB.7;
  PORTB_RB6         : bit  absolute PORTB.6;
  PORTB_RB5         : bit  absolute PORTB.5;
  PORTB_RB4         : bit  absolute PORTB.4;
  PORTB_RB3         : bit  absolute PORTB.3;
  PORTB_RB2         : bit  absolute PORTB.2;
  PORTB_RB1         : bit  absolute PORTB.1;
  PORTB_RB0         : bit  absolute PORTB.0;
  EEDATA            : byte absolute $0008;
  EEADR             : byte absolute $0009;
  PCLATH            : byte absolute $000A;
  PCLATH_PCLATH4    : bit  absolute PCLATH.4;
  PCLATH_PCLATH3    : bit  absolute PCLATH.3;
  PCLATH_PCLATH2    : bit  absolute PCLATH.2;
  PCLATH_PCLATH1    : bit  absolute PCLATH.1;
  PCLATH_PCLATH0    : bit  absolute PCLATH.0;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_EEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_nRBPU  : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
  TRISA_TRISA4      : bit  absolute TRISA.4;
  TRISA_TRISA3      : bit  absolute TRISA.3;
  TRISA_TRISA2      : bit  absolute TRISA.2;
  TRISA_TRISA1      : bit  absolute TRISA.1;
  TRISA_TRISA0      : bit  absolute TRISA.0;
  TRISB             : byte absolute $0086;
  TRISB_TRISB7      : bit  absolute TRISB.7;
  TRISB_TRISB6      : bit  absolute TRISB.6;
  TRISB_TRISB5      : bit  absolute TRISB.5;
  TRISB_TRISB4      : bit  absolute TRISB.4;
  TRISB_TRISB3      : bit  absolute TRISB.3;
  TRISB_TRISB2      : bit  absolute TRISB.2;
  TRISB_TRISB1      : bit  absolute TRISB.1;
  TRISB_TRISB0      : bit  absolute TRISB.0;
  EECON1            : byte absolute $0088;
  EECON1_EEIF       : bit  absolute EECON1.4;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $0089;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-1 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-1 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-006:SFR:ALL'}        // Bank 0 : PORTA, PORTB
                                            // Bank 1 : TRISA, TRISB
  {$SET_STATE_RAM '008-009:SFR:ALL'}        // Bank 0 : EEDATA, EEADR
                                            // Bank 1 : EECON1, EECON2
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-1 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-04F:GPR:ALLMAPPED'} 


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:1F'} // PORTA bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:1F'} // TRISA bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '088:1F'} // EECON1 bits 7,6,5 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RA2
  // Pin  2 : RA3
  // Pin  3 : RA4/T0CKI
  // Pin  4 : MCLR
  // Pin  5 : Vss
  // Pin  6 : RB0/INT
  // Pin  7 : RB1
  // Pin  8 : RB2
  // Pin  9 : RB3
  // Pin 10 : RB4
  // Pin 11 : RB5
  // Pin 12 : RB6
  // Pin 13 : RB7
  // Pin 14 : Vdd
  // Pin 15 : OSC2/CLKOUT
  // Pin 16 : OSC1/CLKIN
  // Pin 17 : RA0
  // Pin 18 : RA1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRC = $3FFF}  // RC oscillator
  {$define _FOSC_HS    = $3FFE}  // HS oscillator
  {$define _FOSC_XT    = $3FFD}  // XT oscillator
  {$define _FOSC_LP    = $3FFC}  // LP oscillator

  // WDTE : Watchdog Timer
  {$define _WDTE_ON    = $3FFF}  // WDT enabled
  {$define _WDTE_OFF   = $3FFB}  // WDT disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF  = $3FFF}  // Power-up Timer is disabled
  {$define _PWRTE_ON   = $3FF7}  // Power-up Timer is enabled

  // CP : Code Protection bit
  {$define _CP_OFF     = $3FFF}  // Code protection disabled
  {$define _CP_ON      = $000F}  // All program memory is code protected

implementation
end.
