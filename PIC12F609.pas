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
  STATUS_TO         : bit  absolute STATUS.4;
  STATUS_PD         : bit  absolute STATUS.3;
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
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_GPIE       : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_GPIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_C1IF         : bit  absolute PIR1.3;
  PIR1_TMR1IF       : bit  absolute PIR1.2;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_T1GINV      : bit  absolute T1CON.7;
  T1CON_TMR1GE      : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1OSCEN     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1CS      : bit  absolute T1CON.1;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  VRCON             : byte absolute $0019;
  VRCON_C1VREN      : bit  absolute VRCON.7;
  VRCON_VRR         : bit  absolute VRCON.6;
  VRCON_FBREN       : bit  absolute VRCON.5;
  VRCON_VR2         : bit  absolute VRCON.4;
  VRCON_VR1         : bit  absolute VRCON.3;
  VRCON_VR0         : bit  absolute VRCON.2;
  CMCON0            : byte absolute $001a;
  CMCON0_C1ON       : bit  absolute CMCON0.6;
  CMCON0_C1OUT      : bit  absolute CMCON0.5;
  CMCON0_C1OE       : bit  absolute CMCON0.4;
  CMCON0_C1POL      : bit  absolute CMCON0.3;
  CMCON0_C1R        : bit  absolute CMCON0.2;
  CMCON0_C1CH       : bit  absolute CMCON0.1;
  CMCON1            : byte absolute $001c;
  CMCON1_T1ACS      : bit  absolute CMCON1.4;
  CMCON1_C1HYS      : bit  absolute CMCON1.3;
  CMCON1_T1GSS      : bit  absolute CMCON1.2;
  CMCON1_C1SYNC     : bit  absolute CMCON1.1;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_GPPU   : bit  absolute OPTION_REG.7;
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
  PIE1              : byte absolute $008c;
  PIE1_C1IE         : bit  absolute PIE1.3;
  PIE1_TMR1IE       : bit  absolute PIE1.2;
  PCON              : byte absolute $008e;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  WPU               : byte absolute $0095;
  WPU_WPUA5         : bit  absolute WPU.5;
  WPU_WPUA4         : bit  absolute WPU.4;
  WPU_WPUA2         : bit  absolute WPU.3;
  WPU_WPUA1         : bit  absolute WPU.2;
  WPU_WPUA0         : bit  absolute WPU.1;
  IOC               : byte absolute $0096;
  IOC_IOC5          : bit  absolute IOC.5;
  IOC_IOC4          : bit  absolute IOC.4;
  IOC_IOC3          : bit  absolute IOC.3;
  IOC_IOC2          : bit  absolute IOC.2;
  IOC_IOC1          : bit  absolute IOC.1;
  IOC_IOC0          : bit  absolute IOC.0;
  ANSEL             : byte absolute $009f;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS1        : bit  absolute ANSEL.2;
  ANSEL_ANS0        : bit  absolute ANSEL.1;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-005:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, GPIO
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-010:SFR'}  // TMR1L, TMR1H, T1CON
  {$SET_STATE_RAM '019-01A:SFR'}  // VRCON, CMCON0
  {$SET_STATE_RAM '01C-01C:SFR'}  // CMCON1
  {$SET_STATE_RAM '040-07F:GPR'} 
  {$SET_STATE_RAM '080-085:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISIO
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-08E:SFR'}  // PCON
  {$SET_STATE_RAM '090-090:SFR'}  // OSCTUNE
  {$SET_STATE_RAM '095-096:SFR'}  // WPU, IOC
  {$SET_STATE_RAM '09F-09F:SFR'}  // ANSEL
  {$SET_STATE_RAM '0F0-0FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:3F'} // GPIO
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:09'} // PIR1
  {$SET_UNIMP_BITS '019:BF'} // VRCON
  {$SET_UNIMP_BITS '01A:F5'} // CMCON0
  {$SET_UNIMP_BITS '01C:1B'} // CMCON1
  {$SET_UNIMP_BITS '085:3F'} // TRISIO
  {$SET_UNIMP_BITS '08C:09'} // PIE1
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '095:37'} // WPU
  {$SET_UNIMP_BITS '096:3F'} // IOC
  {$SET_UNIMP_BITS '09F:7F'} // ANSEL


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



  // -- Bits Configuration --

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON       = $03FF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $03FE}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_OFF      = $03FC}  // BOR disabled

  // IOSCFS : Internal Oscillator Frequency Select
  {$define _IOSCFS_8MHZ    = $03FF}  // 8 MHz
  {$define _IOSCFS_4MHZ    = $03FB}  // 4 MHz

  // CP : Code Protection bit
  {$define _CP_OFF         = $03FF}  // Program memory code protection is disabled
  {$define _CP_ON          = $03F7}  // Program memory code protection is enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON       = $03FF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $03EF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $03FF}  // PWRT disabled
  {$define _PWRTE_ON       = $03DF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $03FF}  // WDT enabled
  {$define _WDTE_OFF       = $03BF}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $03FF}  // RC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_EXTRCIO   = $037F}  // RCIO oscillator: I/O function on GP4/OSC2/CLKOUT pin, RC on GP5/OSC1/CLKIN
  {$define _FOSC_INTOSCCLK = $02FF}  // INTOSC oscillator: CLKOUT function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $027F}  // INTOSCIO oscillator: I/O function on GP4/OSC2/CLKOUT pin, I/O function on GP5/OSC1/CLKIN
  {$define _FOSC_EC        = $01FF}  // EC: I/O function on GP4/OSC2/CLKOUT pin, CLKIN on GP5/OSC1/CLKIN
  {$define _FOSC_HS        = $017F}  // HS oscillator: High-speed crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_XT        = $00FF}  // XT oscillator: Crystal/resonator on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN
  {$define _FOSC_LP        = $007F}  // LP oscillator: Low-power crystal on GP4/OSC2/CLKOUT and GP5/OSC1/CLKIN

implementation
end.
