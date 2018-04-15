unit PIC16F627;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F627'}
{$SET PIC_MAXFREQ  = 4000000}
{$SET PIC_NPINS    = 18}
{$SET PIC_NUMBANKS = 4}
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
  PORTA             : byte absolute $0005;
  PORTA_RA7         : bit  absolute PORTA.7;
  PORTA_RA6         : bit  absolute PORTA.6;
  PORTA_RA5         : bit  absolute PORTA.5;
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
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_T0IE       : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RBIE       : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RBIF       : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_EEIF         : bit  absolute PIR1.7;
  PIR1_CMIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_CCP1IF       : bit  absolute PIR1.3;
  PIR1_TMR2IF       : bit  absolute PIR1.2;
  PIR1_TMR1IF       : bit  absolute PIR1.1;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
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
  CCPR1L            : byte absolute $0015;
  CCPR1H            : byte absolute $0016;
  CCP1CON           : byte absolute $0017;
  CCP1CON_CCP1X     : bit  absolute CCP1CON.5;
  CCP1CON_CCP1Y     : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  RCSTA             : byte absolute $0018;
  RCSTA_SPEN        : bit  absolute RCSTA.7;
  RCSTA_RX9         : bit  absolute RCSTA.6;
  RCSTA_SREN        : bit  absolute RCSTA.5;
  RCSTA_CREN        : bit  absolute RCSTA.4;
  RCSTA_ADEN        : bit  absolute RCSTA.3;
  RCSTA_FERR        : bit  absolute RCSTA.2;
  RCSTA_OERR        : bit  absolute RCSTA.1;
  RCSTA_RX9D        : bit  absolute RCSTA.0;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001a;
  CMCON             : byte absolute $001f;
  CMCON_C2OUT       : bit  absolute CMCON.7;
  CMCON_C1OUT       : bit  absolute CMCON.6;
  CMCON_C2INV       : bit  absolute CMCON.5;
  CMCON_C1INV       : bit  absolute CMCON.4;
  CMCON_CIS         : bit  absolute CMCON.3;
  CMCON_CM2         : bit  absolute CMCON.2;
  CMCON_CM1         : bit  absolute CMCON.1;
  CMCON_CM0         : bit  absolute CMCON.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RBPU   : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
  TRISA_TRISA7      : bit  absolute TRISA.7;
  TRISA_TRISA6      : bit  absolute TRISA.6;
  TRISA_TRISA5      : bit  absolute TRISA.5;
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
  PIE1              : byte absolute $008c;
  PIE1_EEIE         : bit  absolute PIE1.7;
  PIE1_CMIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_CCP1IE       : bit  absolute PIE1.3;
  PIE1_TMR2IE       : bit  absolute PIE1.2;
  PIE1_TMR1IE       : bit  absolute PIE1.1;
  PCON              : byte absolute $008e;
  PCON_OSCF         : bit  absolute PCON.3;
  PCON_POR          : bit  absolute PCON.2;
  PCON_BOR          : bit  absolute PCON.1;
  PR2               : byte absolute $0092;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.3;
  TXSTA_TRMT        : bit  absolute TXSTA.2;
  TXSTA_TX9D        : bit  absolute TXSTA.1;
  SPBRG             : byte absolute $0099;
  EEDATA            : byte absolute $009a;
  EEADR             : byte absolute $009b;
  EECON1            : byte absolute $009c;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $009d;
  VRCON             : byte absolute $009f;
  VRCON_VREN        : bit  absolute VRCON.7;
  VRCON_VROE        : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-006:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-012:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON
  {$SET_STATE_RAM '015-01A:SFR'}  // CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG
  {$SET_STATE_RAM '01F-01F:SFR'}  // CMCON
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-086:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-08E:SFR'}  // PCON
  {$SET_STATE_RAM '092-092:SFR'}  // PR2
  {$SET_STATE_RAM '098-09D:SFR'}  // TXSTA, SPBRG, EEDATA, EEADR, EECON1, EECON2
  {$SET_STATE_RAM '09F-09F:SFR'}  // VRCON
  {$SET_STATE_RAM '0A0-0FF:GPR'} 
  {$SET_STATE_RAM '100-104:SFR'}  // INDF, TMR0, PCL, STATUS, FSR
  {$SET_STATE_RAM '106-106:SFR'}  // PORTB
  {$SET_STATE_RAM '10A-10B:SFR'}  // PCLATH, INTCON
  {$SET_STATE_RAM '120-14F:GPR'} 
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-184:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR
  {$SET_STATE_RAM '186-186:SFR'}  // TRISB
  {$SET_STATE_RAM '18A-18B:SFR'}  // PCLATH, INTCON
  {$SET_STATE_RAM '1F0-1FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '100-104:bnk0'} // INDF, TMR0, PCL, STATUS, FSR
  {$SET_MAPPED_RAM '106-106:bnk0'} // PORTB
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '180-180:bnk0'} // INDF
  {$SET_MAPPED_RAM '181-181:bnk1'} // OPTION_REG
  {$SET_MAPPED_RAM '182-184:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '186-186:bnk1'} // TRISB
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:F7'} // PIR1
  {$SET_UNIMP_BITS '010:3F'} // T1CON
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON
  {$SET_UNIMP_BITS '08C:F7'} // PIE1
  {$SET_UNIMP_BITS '08E:0B'} // PCON
  {$SET_UNIMP_BITS '098:F7'} // TXSTA
  {$SET_UNIMP_BITS '09C:0F'} // EECON1
  {$SET_UNIMP_BITS '09F:EF'} // VRCON


  // -- PIN mapping --

  // Pin  1 : RA2/AN2/Vref
  // Pin  2 : RA3/AN3/CMP1
  // Pin  3 : RA4/T0CKI/CMP2
  // Pin  4 : RA5/MCLR/Vpp
  // Pin  5 : Vss
  // Pin  6 : RB0/INT
  // Pin  7 : RB1/RX/DT
  // Pin  8 : RB2/TX/CK
  // Pin  9 : RB3/CCP1
  // Pin 10 : RB4/PGM
  // Pin 11 : RB5
  // Pin 12 : RB6/T1OSO/T1CKI/PGC
  // Pin 13 : RB7/T1OSI/PGD
  // Pin 14 : Vdd
  // Pin 15 : RA6/OSC2/CLKOUT
  // Pin 16 : RA7/OSC1/CLKIN
  // Pin 17 : RA0/AN0
  // Pin 18 : RA1/AN1


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-17,1-18,2-1,3-2,4-3,5-4,6-15,7-16'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB


  // -- Bits Configuration --

  // CP : Code Protection bits
  {$define _CP_OFF         = $3DFF}  // Program memory code protection off
  {$define _CP_50          = $3DFA}  // Program memory code protection off
  {$define _CP_75          = $3DF5}  // 0200h-03FFh code protected
  {$define _CP_ALL         = $3DF0}  // 0000h-03FFh code protected

  // CPD : Data Code Protection bit
  {$define _CPD_OFF        = $3DFF}  // Data memory code protection off
  {$define _CPD_ON         = $3DEF}  // Data memory code protected

  // LVP : Low-Voltage Programming Enable bit
  {$define _LVP_ON         = $3DFF}  // RB4/PGM pin has PGM function, low-voltage programming enabled
  {$define _LVP_OFF        = $3DDF}  // RB4/PGM pin has digital I/O function, HV on MCLR must be used for programming

  // BOREN : Brown-out Reset Enable bit
  {$define _BOREN_ON       = $3DFF}  // BOD Reset enabled
  {$define _BOREN_OFF      = $3DBF}  // BOD Reset disabled

  // MCLRE : RA5/MCLR pin function select
  {$define _MCLRE_ON       = $3DFF}  // RA5/MCLR pin function is MCLR
  {$define _MCLRE_OFF      = $3D7F}  // RA5/MCLR pin function is digital input, MCLR internally tied to VDD

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF      = $3DFF}  // PWRT disabled
  {$define _PWRTE_ON       = $3CFF}  // PWRT enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON        = $3FFF}  // WDT enabled
  {$define _WDTE_OFF       = $3DFF}  // WDT disabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_ERCLK     = $4DFF}  // ER oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, Resistor on RA7/OSC1/CLKIN
  {$define _FOSC_ERIO      = $49FF}  // ER oscillator: I/O function on RA6/OSC2/CLKOUT pin, Resistor on RA7/OSC1/CLKIN
  {$define _FOSC_INTOSCCLK = $45FF}  // INTRC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN
  {$define _FOSC_INTOSCIO  = $41FF}  // INTRC oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN
  {$define _FOSC_ECIO      = $0DFF}  // EC: I/O function on RA6/OSC2/CLKOUT pin, CLKIN on RA7/OSC1/CLKIN
  {$define _FOSC_HS        = $09FF}  // HS oscillator: High-speed crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN
  {$define _FOSC_XT        = $05FF}  // XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN
  {$define _FOSC_LP        = $01FF}  // LP oscillator: Low-power crystal on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN

implementation
end.
