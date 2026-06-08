unit PIC12F609;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F609'}
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
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_GPIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_GPIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_C1IF         : bit  absolute PIR1.3;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_nT1SYNC     : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  VRCON             : byte absolute $0019;
  VRCON_C1VREN      : bit  absolute VRCON.7;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_FBREN       : bit  absolute VRCON.4;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  CMCON0            : byte absolute $001A;
  CMCON0_C1ON       : bit  absolute CMCON0.7;
  CMCON0_C1OUT      : bit  absolute CMCON0.6;
  CMCON0_C1OE       : bit  absolute CMCON0.5;
  CMCON0_C1POL      : bit  absolute CMCON0.4;
  CMCON0_C1R        : bit  absolute CMCON0.2;
  CMCON0_C1CH       : bit  absolute CMCON0.0;
  CMCON1            : byte absolute $001C;
  CMCON1_T1ACS      : bit  absolute CMCON1.4;
  CMCON1_C1HYS      : bit  absolute CMCON1.3;
  CMCON1_T1GSS      : bit  absolute CMCON1.1;
  CMCON1_C1SYNC     : bit  absolute CMCON1.0;
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
  PIE1_C1IE         : bit  absolute PIE1.3;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PCON              : byte absolute $008E;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  WPU               : byte absolute $0095;
  WPU_WPUA5         : bit  absolute WPU.5;
  WPU_WPUA4         : bit  absolute WPU.4;
  WPU_WPUA2         : bit  absolute WPU.2;
  WPU_WPUA1         : bit  absolute WPU.1;
  WPU_WPUA0         : bit  absolute WPU.0;
  IOC               : byte absolute $0096;
  IOC_IOC5          : bit  absolute IOC.5;
  IOC_IOC4          : bit  absolute IOC.4;
  IOC_IOC3          : bit  absolute IOC.3;
  IOC_IOC2          : bit  absolute IOC.2;
  IOC_IOC1          : bit  absolute IOC.1;
  IOC_IOC0          : bit  absolute IOC.0;
  ANSEL             : byte absolute $009F;
  ANSEL_ADCS2       : bit  absolute ANSEL.6;
  ANSEL_ADCS1       : bit  absolute ANSEL.5;
  ANSEL_ADCS0       : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;


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
                                            // Bank 1 : OSCTUNE
  {$SET_STATE_RAM '019-01A:SFR'}            // Bank 0 : VRCON, CMCON0
  {$SET_STATE_RAM '01C-01C:SFR'}            // Bank 0 : CMCON1
  {$SET_STATE_RAM '040-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '095-096:SFR'}            // Bank 1 : WPU, IOC
  {$SET_STATE_RAM '09F-09F:SFR'}            // Bank 1 : ANSEL


  // -- Define mapped RAM --




  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:3F'} // GPIO bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:09'} // PIR1 bits 7,6,5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '019:BF'} // VRCON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01A:F5'} // CMCON0 bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:1B'} // CMCON1 bits 7,6,5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:3F'} // TRISIO bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:09'} // PIE1 bits 7,6,5,4,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '095:37'} // WPU bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:3F'} // IOC bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:7F'} // ANSEL bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : GP5/T1CKI/OSC1/CLKIN
  // Pin  3 : GP4/C1IN-/T1G/OSC2/CLKOUT
  // Pin  4 : GP3/MCLR/Vpp
  // Pin  5 : GP2/T0CKI/INT/C1OUT
  // Pin  6 : GP1/C1IN0-/ICSPCLK
  // Pin  7 : GP0/C1IN+/ICSPDAT
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-7,1-6,2-5,3-4,4-3,5-2'} // GPIO


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $3FFF}  // RC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_EXTRCIO   = $3FFE}  // RCIO oscillator: I/O function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_INTOSCCLK = $3FFD}  // INTOSC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $3FFC}  // INTOSCIO oscillator: I/O function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_EC        = $3FFB}  // EC: I/O function on GP4/OSC2/CLKOUT pin, CLKIN on GP5/OSC1/CLKIN
  {$define _FOSC_HS        = $3FFA}  // HS oscillator: High-speed crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_XT        = $3FF9}  // XT oscillator: Crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_LP        = $3FF8}  // LP oscillator: Low-power crystal on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $3FFF}  // WDT enabled
  {$define _WDTE_OFF       = $3FF7}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $3FEF}  // PWRT enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON       = $3FFF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $3FDF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF         = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON          = $3FBF}  // Program memory code protection is enabled

  // IOSCFS : Internal Oscillator Frequency Select
  {$define _IOSCFS_8MHZ    = $3FFF}  // 8 MHz
  {$define _IOSCFS_4MHZ    = $3F7F}  // 4 MHz

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON       = $3FFF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $3EFF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_OFF      = $3CFF}  // BOR disabled

implementation
end.
