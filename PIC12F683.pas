unit PIC12F683;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F683'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 2}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 2048}

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
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_GPIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_GPIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_CCP1IF       : bit  absolute PIR1.5;
  PIR1_CMIF         : bit  absolute PIR1.4;
  PIR1_OSFIF        : bit  absolute PIR1.3;
  PIR1_TMR2IF       : bit  absolute PIR1.2;
  PIR1_TMR1IF       : bit  absolute PIR1.1;
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
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS0     : bit  absolute T2CON.1;
  CCPR1L            : byte absolute $0013;
  CCPR1H            : byte absolute $0014;
  CCP1CON           : byte absolute $0015;
  CCP1CON_DC1B1     : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  WDTCON            : byte absolute $0018;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  CMCON0            : byte absolute $0019;
  CMCON0_COUT       : bit  absolute CMCON0.6;
  CMCON0_CINV       : bit  absolute CMCON0.5;
  CMCON0_CIS        : bit  absolute CMCON0.4;
  CMCON0_CM2        : bit  absolute CMCON0.2;
  CMCON0_CM1        : bit  absolute CMCON0.1;
  CMCON0_CM0        : bit  absolute CMCON0.0;
  CMCON1            : byte absolute $001a;
  CMCON1_T1GSS      : bit  absolute CMCON1.1;
  CMCON1_CMSYNC     : bit  absolute CMCON1.0;
  ADRESH            : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_ADFM       : bit  absolute ADCON0.7;
  ADCON0_VCFG       : bit  absolute ADCON0.6;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.3;
  ADCON0_ADON       : bit  absolute ADCON0.2;
  ADCON0_GO         : bit  absolute ADCON0.1;
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
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_CCP1IE       : bit  absolute PIE1.5;
  PIE1_CMIE         : bit  absolute PIE1.4;
  PIE1_OSFIE        : bit  absolute PIE1.3;
  PIE1_TMR2IE       : bit  absolute PIE1.2;
  PIE1_TMR1IE       : bit  absolute PIE1.1;
  PCON              : byte absolute $008e;
  PCON_ULPWUE       : bit  absolute PCON.5;
  PCON_SBODEN       : bit  absolute PCON.4;
  PCON_POR          : bit  absolute PCON.3;
  PCON_BOD          : bit  absolute PCON.2;
  OSCCON            : byte absolute $008f;
  OSCCON_IRCF2      : bit  absolute OSCCON.6;
  OSCCON_IRCF1      : bit  absolute OSCCON.5;
  OSCCON_IRCF0      : bit  absolute OSCCON.4;
  OSCCON_OSTS       : bit  absolute OSCCON.3;
  OSCCON_HTS        : bit  absolute OSCCON.2;
  OSCCON_LTS        : bit  absolute OSCCON.1;
  OSCCON_SCS        : bit  absolute OSCCON.0;
  OSCTUNE           : byte absolute $0090;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  PR2               : byte absolute $0092;
  WPU               : byte absolute $0095;
  WPU_WPU5          : bit  absolute WPU.5;
  WPU_WPU4          : bit  absolute WPU.4;
  WPU_WPU2          : bit  absolute WPU.3;
  WPU_WPU1          : bit  absolute WPU.2;
  WPU_WPU0          : bit  absolute WPU.1;
  IOC               : byte absolute $0096;
  IOC_IOC5          : bit  absolute IOC.5;
  IOC_IOC4          : bit  absolute IOC.4;
  IOC_IOC3          : bit  absolute IOC.3;
  IOC_IOC2          : bit  absolute IOC.2;
  IOC_IOC1          : bit  absolute IOC.1;
  IOC_IOC0          : bit  absolute IOC.0;
  VRCON             : byte absolute $0099;
  VRCON_VREN        : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  EEDAT             : byte absolute $009a;
  EEADR             : byte absolute $009b;
  EECON1            : byte absolute $009c;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $009d;
  ADRESL            : byte absolute $009e;
  ANSEL             : byte absolute $009f;
  ANSEL_ADCS2       : bit  absolute ANSEL.6;
  ANSEL_ADCS1       : bit  absolute ANSEL.5;
  ANSEL_ADCS0       : bit  absolute ANSEL.4;
  ANSEL_ANS3        : bit  absolute ANSEL.3;
  ANSEL_ANS2        : bit  absolute ANSEL.2;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-005:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, GPIO
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-015:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON, CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '018-01A:SFR'}  // WDTCON, CMCON0, CMCON1
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRESH, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-085:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISIO
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-090:SFR'}  // PCON, OSCCON, OSCTUNE
  {$SET_STATE_RAM '092-092:SFR'}  // PR2
  {$SET_STATE_RAM '095-096:SFR'}  // WPU, IOC
  {$SET_STATE_RAM '099-09F:SFR'}  // VRCON, EEDAT, EEADR, EECON1, EECON2, ADRESL, ANSEL
  {$SET_STATE_RAM '0A0-0BF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '005:3F'} // GPIO
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:EF'} // PIR1
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '015:3F'} // CCP1CON
  {$SET_UNIMP_BITS '018:1F'} // WDTCON
  {$SET_UNIMP_BITS '019:5F'} // CMCON0
  {$SET_UNIMP_BITS '01A:03'} // CMCON1
  {$SET_UNIMP_BITS '01F:CF'} // ADCON0
  {$SET_UNIMP_BITS '085:3F'} // TRISIO
  {$SET_UNIMP_BITS '08C:EF'} // PIE1
  {$SET_UNIMP_BITS '08E:33'} // PCON
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '095:37'} // WPU
  {$SET_UNIMP_BITS '096:3F'} // IOC
  {$SET_UNIMP_BITS '099:AF'} // VRCON
  {$SET_UNIMP_BITS '09C:0F'} // EECON1
  {$SET_UNIMP_BITS '09F:7F'} // ANSEL


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : GP5/T1CKI/OSC1/CLKIN
  // Pin  3 : GP4/AN3/T1G/OSC2/CLKOUT
  // Pin  4 : GP3/MCLR/Vpp
  // Pin  5 : GP2/AN2/T0CKI/INT/COUT/CCP1
  // Pin  6 : GP1/AN1/CIN-/Vref/ICSPCLK
  // Pin  7 : GP0/AN0/CIN+/ICSPDAT/ULPWU
  // Pin  8 : Vss


  // -- RAM to PIN mapping --



  // -- Bits Configuration --

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON       = $0FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF      = $0FFE}  // Fail-Safe Clock Monitor is disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON        = $0FFF}  // Internal External Switchover mode is enabled
  {$define _IESO_OFF       = $0FFD}  // Internal External Switchover mode is disabled

  // BOREN : Brown Out Detect
  {$define _BOREN_ON       = $0FFF}  // BOR enabled
  {$define _BOREN_NSLEEP   = $0FFB}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN   = $0FF7}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF      = $0FF3}  // BOR disabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF        = $0FFF}  // Data memory code protection is disabled
  {$define _CPD_ON         = $0FEF}  // Data memory code protection is enabled

  // CP : Code Protection bit
  {$define _CP_OFF         = $0FFF}  // Program memory code protection is disabled
  {$define _CP_ON          = $0FDF}  // Program memory code protection is enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON       = $0FFF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $0FBF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $0FFF}  // PWRT disabled
  {$define _PWRTE_ON       = $0F7F}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $0FFF}  // WDT enabled
  {$define _WDTE_OFF       = $0EFF}  // WDT disabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK  = $0FFF}  // EXTRC oscillator: External RC on RA5/OSC1/CLKIN, CLKOUT function on RA4/OSC2/CLKOUT pin
  {$define _FOSC_EXTRCIO   = $0DFF}  // EXTRCIO oscillator: External RC on RA5/OSC1/CLKIN, I/O function on RA4/OSC2/CLKOUT pin
  {$define _FOSC_INTOSCCLK = $0BFF}  // INTOSC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $09FF}  // INTOSCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_EC        = $07FF}  // EC: I/O function on RA4/OSC2/CLKOUT pin, CLKIN on RA5/OSC1/CLKIN
  {$define _FOSC_HS        = $05FF}  // HS oscillator: High-speed crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_XT        = $03FF}  // XT oscillator: Crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKINT
  {$define _FOSC_LP        = $01FF}  // LP oscillator: Low-power crystal on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN

implementation
end.
