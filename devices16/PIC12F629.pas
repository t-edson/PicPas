unit PIC12F629;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F629'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 8}
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
  GPIO              : byte absolute $0005;
  GPIO_GP5          : bit  absolute GPIO.5;
  GPIO_GP4          : bit  absolute GPIO.4;
  GPIO_GP3          : bit  absolute GPIO.3;
  GPIO_GP2          : bit  absolute GPIO.2;
  GPIO_GP1          : bit  absolute GPIO.1;
  GPIO_GP0          : bit  absolute GPIO.0;
  PCLATH            : byte absolute $000A;
  PCLATH_PCLATH4    : bit  absolute PCLATH.4;
  PCLATH_PCLATH3    : bit  absolute PCLATH.3;
  PCLATH_PCLATH2    : bit  absolute PCLATH.2;
  PCLATH_PCLATH1    : bit  absolute PCLATH.1;
  PCLATH_PCLATH0    : bit  absolute PCLATH.0;
  INTCON            : byte absolute $000B;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_GPIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_GPIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_CMIF         : bit  absolute PIR1.3;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_nT1SYNC     : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  CMCON             : byte absolute $0019;
  CMCON_COUT        : bit  absolute CMCON.6;
  CMCON_CINV        : bit  absolute CMCON.4;
  CMCON_CIS         : bit  absolute CMCON.3;
  CMCON_CM2         : bit  absolute CMCON.2;
  CMCON_CM1         : bit  absolute CMCON.1;
  CMCON_CM0         : bit  absolute CMCON.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_nGPPU  : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISIO            : byte absolute $0085;
  TRISIO_TRISIO5    : bit  absolute TRISIO.5;
  TRISIO_TRISIO4    : bit  absolute TRISIO.4;
  TRISIO_TRISIO3    : bit  absolute TRISIO.3;
  TRISIO_TRISIO2    : bit  absolute TRISIO.2;
  TRISIO_TRISIO1    : bit  absolute TRISIO.1;
  TRISIO_TRISIO0    : bit  absolute TRISIO.0;
  PIE1              : byte absolute $008C;
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_CMIE         : bit  absolute PIE1.3;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PCON              : byte absolute $008E;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  OSCCAL            : byte absolute $0090;
  OSCCAL_CAL5       : bit  absolute OSCCAL.7;
  OSCCAL_CAL4       : bit  absolute OSCCAL.6;
  OSCCAL_CAL3       : bit  absolute OSCCAL.5;
  OSCCAL_CAL2       : bit  absolute OSCCAL.4;
  OSCCAL_CAL1       : bit  absolute OSCCAL.3;
  OSCCAL_CAL0       : bit  absolute OSCCAL.2;
  WPU               : byte absolute $0095;
  WPU_WPU5          : bit  absolute WPU.5;
  WPU_WPU4          : bit  absolute WPU.4;
  WPU_WPU2          : bit  absolute WPU.2;
  WPU_WPU1          : bit  absolute WPU.1;
  WPU_WPU0          : bit  absolute WPU.0;
  IOC               : byte absolute $0096;
  IOC_IOC5          : bit  absolute IOC.5;
  IOC_IOC4          : bit  absolute IOC.4;
  IOC_IOC3          : bit  absolute IOC.3;
  IOC_IOC2          : bit  absolute IOC.2;
  IOC_IOC1          : bit  absolute IOC.1;
  IOC_IOC0          : bit  absolute IOC.0;
  VRCON             : byte absolute $0099;
  VRCON_VREN        : bit  absolute VRCON.7;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  EEDATA            : byte absolute $009A;
  EEADR             : byte absolute $009B;
  EEADR_EEADR6      : bit  absolute EEADR.6;
  EEADR_EEADR5      : bit  absolute EEADR.5;
  EEADR_EEADR4      : bit  absolute EEADR.4;
  EEADR_EEADR3      : bit  absolute EEADR.3;
  EEADR_EEADR2      : bit  absolute EEADR.2;
  EEADR_EEADR1      : bit  absolute EEADR.1;
  EEADR_EEADR0      : bit  absolute EEADR.0;
  EECON1            : byte absolute $009C;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $009D;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-1 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-1 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-005:SFR:ALL'}        // Bank 0 : GPIO
                                            // Bank 1 : TRISIO
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-1 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00C:SFR:ALL'}        // Bank 0 : PIR1
                                            // Bank 1 : PIE1
  {$SET_STATE_RAM '00E-00E:SFR:ALL'}        // Bank 0 : TMR1L
                                            // Bank 1 : PCON
  {$SET_STATE_RAM '00F-00F:SFR'}            // Bank 0 : TMR1H
  {$SET_STATE_RAM '010-010:SFR:ALL'}        // Bank 0 : T1CON
                                            // Bank 1 : OSCCAL
  {$SET_STATE_RAM '019-019:SFR:ALL'}        // Bank 0 : CMCON
                                            // Bank 1 : VRCON
  {$SET_STATE_RAM '020-05F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '095-096:SFR'}            // Bank 1 : WPU, IOC
  {$SET_STATE_RAM '09A-09D:SFR'}            // Bank 1 : EEDATA, EEADR, EECON1, EECON2


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:3F'} // GPIO bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:C9'} // PIR1 bits 5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:7F'} // T1CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '019:5F'} // CMCON bits 7,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:3F'} // TRISIO bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:C9'} // PIE1 bits 5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:FC'} // OSCCAL bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '095:37'} // WPU bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:3F'} // IOC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '099:AF'} // VRCON bits 6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09B:7F'} // EEADR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09C:0F'} // EECON1 bits 7,6,5,4 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : GP5/T1CKI/OSC1/CLKIN
  // Pin  3 : GP4/T1G/OSC2/CLKOUT
  // Pin  4 : GP3/MCLR/Vpp
  // Pin  5 : GP2/T0CKI/INT/COUT
  // Pin  6 : GP1/CIN-/ICSPCLK
  // Pin  7 : GP0/CIN+/ICSPDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-7,1-6,2-5,3-4,4-3,5-2'} // GPIO


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK = $3FFF}  // RC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_EXTRCIO  = $3FFE}  // RC oscillator: I/O function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_INTRCCLK = $3FFD}  // INTOSC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_INTRCIO  = $3FFC}  // INTOSC oscillator: I/O function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_EC       = $3FFB}  // EC: I/O function on GP4/OSC2/CLKOUT pin, CLKIN on GP5/OSC1/CLKIN
  {$define _FOSC_HS       = $3FFA}  // HS oscillator: High speed crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_XT       = $3FF9}  // XT oscillator: Crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_LP       = $3FF8}  // LP oscillator: Low power crystal on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON       = $3FFF}  // WDT enabled
  {$define _WDTE_OFF      = $3FF7}  // WDT disabled

  // PWRTE : Power-Up Timer Enable bit
  {$define _PWRTE_OFF     = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON      = $3FEF}  // PWRT enabled

  // MCLRE : GP3/MCLR pin function select
  {$define _MCLRE_ON      = $3FFF}  // GP3/MCLR pin function is MCLR
  {$define _MCLRE_OFF     = $3FDF}  // GP3/MCLR pin function is digital I/O, MCLR internally tied to VDD

  // BOREN : Brown-out Detect Enable bit
  {$define _BOREN_ON      = $3FFF}  // BOD enabled
  {$define _BOREN_OFF     = $3FBF}  // BOD disabled

  // CP : Code Protection bit
  {$define _CP_OFF        = $3FFF}  // Program Memory code protection is disabled
  {$define _CP_ON         = $3F7F}  // Program Memory code protection is enabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF       = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON        = $3EFF}  // Data memory code protection is enabled

implementation
end.
