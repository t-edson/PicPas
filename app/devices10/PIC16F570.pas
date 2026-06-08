unit PIC16F570;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F570'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 28}
{$SET PIC_NUMBANKS = 8}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 2048}

interface
var
  INDF             : byte absolute $0000;
  TMR0             : byte absolute $0001;
  PCL              : byte absolute $0002;
  STATUS           : byte absolute $0003;
  STATUS_PA2       : bit  absolute STATUS.7;
  STATUS_PA1       : bit  absolute STATUS.6;
  STATUS_PA0       : bit  absolute STATUS.5;
  STATUS_nTO       : bit  absolute STATUS.4;
  STATUS_nPD       : bit  absolute STATUS.3;
  STATUS_Z         : bit  absolute STATUS.2;
  STATUS_DC        : bit  absolute STATUS.1;
  STATUS_C         : bit  absolute STATUS.0;
  FSR              : byte absolute $0004;
  OSCCAL           : byte absolute $0005;
  OSCCAL_CAL6      : bit  absolute OSCCAL.7;
  OSCCAL_CAL5      : bit  absolute OSCCAL.6;
  OSCCAL_CAL4      : bit  absolute OSCCAL.5;
  OSCCAL_CAL3      : bit  absolute OSCCAL.4;
  OSCCAL_CAL2      : bit  absolute OSCCAL.3;
  OSCCAL_CAL1      : bit  absolute OSCCAL.2;
  OSCCAL_CAL0      : bit  absolute OSCCAL.1;
  PORTA            : byte absolute $0006;
  PORTB            : byte absolute $0007;
  PORTC            : byte absolute $0008;
  ADCON0           : byte absolute $0009;
  ADCON0_ADCS1     : bit  absolute ADCON0.7;
  ADCON0_ADCS0     : bit  absolute ADCON0.6;
  ADCON0_CHS3      : bit  absolute ADCON0.5;
  ADCON0_CHS2      : bit  absolute ADCON0.4;
  ADCON0_CHS1      : bit  absolute ADCON0.3;
  ADCON0_CHS0      : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE  : bit  absolute ADCON0.1;
  ADCON0_ADON      : bit  absolute ADCON0.0;
  ADRES            : byte absolute $000A;
  INTCON0          : byte absolute $000B;
  INTCON0_ADIF     : bit  absolute INTCON0.7;
  INTCON0_CWIF     : bit  absolute INTCON0.6;
  INTCON0_T0IF     : bit  absolute INTCON0.5;
  INTCON0_RBIF     : bit  absolute INTCON0.4;
  INTCON0_GIE      : bit  absolute INTCON0.0;
  EECON            : byte absolute $0021;
  EECON_FREE       : bit  absolute EECON.4;
  EECON_WRERR      : bit  absolute EECON.3;
  EECON_WREN       : bit  absolute EECON.2;
  EECON_WR         : bit  absolute EECON.1;
  EECON_RD         : bit  absolute EECON.0;
  EEDATA           : byte absolute $0025;
  EEADR            : byte absolute $0026;
  EEADR_EEADR5     : bit  absolute EEADR.5;
  EEADR_EEADR4     : bit  absolute EEADR.4;
  EEADR_EEADR3     : bit  absolute EEADR.3;
  EEADR_EEADR2     : bit  absolute EEADR.2;
  EEADR_EEADR1     : bit  absolute EEADR.1;
  EEADR_EEADR0     : bit  absolute EEADR.0;
  CM1CON0          : byte absolute $0027;
  CM1CON0_C1OUT    : bit  absolute CM1CON0.7;
  CM1CON0_nC1OUTEN : bit  absolute CM1CON0.6;
  CM1CON0_C1POL    : bit  absolute CM1CON0.5;
  CM1CON0_nC1T0CS  : bit  absolute CM1CON0.4;
  CM1CON0_C1ON     : bit  absolute CM1CON0.3;
  CM1CON0_C1NREF   : bit  absolute CM1CON0.2;
  CM1CON0_C1PREF   : bit  absolute CM1CON0.1;
  CM1CON0_nC1WU    : bit  absolute CM1CON0.0;
  CM2CON0          : byte absolute $0028;
  CM2CON0_C2OUT    : bit  absolute CM2CON0.7;
  CM2CON0_nC2OUTEN : bit  absolute CM2CON0.6;
  CM2CON0_C2POL    : bit  absolute CM2CON0.5;
  CM2CON0_C2PREF2  : bit  absolute CM2CON0.4;
  CM2CON0_C2ON     : bit  absolute CM2CON0.3;
  CM2CON0_C2NREF   : bit  absolute CM2CON0.2;
  CM2CON0_C2PREF1  : bit  absolute CM2CON0.1;
  CM2CON0_nC2WU    : bit  absolute CM2CON0.0;
  VRCON            : byte absolute $0029;
  VRCON_VREN       : bit  absolute VRCON.7;
  VRCON_VROE1      : bit  absolute VRCON.6;
  VRCON_VROE2      : bit  absolute VRCON.5;
  VRCON_VRR        : bit  absolute VRCON.4;
  VRCON_VR3        : bit  absolute VRCON.3;
  VRCON_VR2        : bit  absolute VRCON.2;
  VRCON_VR1        : bit  absolute VRCON.1;
  VRCON_VR0        : bit  absolute VRCON.0;
  ANSEL            : byte absolute $002A;
  ANSEL_ANS7       : bit  absolute ANSEL.7;
  ANSEL_ANS6       : bit  absolute ANSEL.6;
  ANSEL_ANS5       : bit  absolute ANSEL.5;
  ANSEL_ANS4       : bit  absolute ANSEL.4;
  ANSEL_ANS3       : bit  absolute ANSEL.3;
  ANSEL_ANS2       : bit  absolute ANSEL.2;
  ANSEL_ANS1       : bit  absolute ANSEL.1;
  ANSEL_ANS0       : bit  absolute ANSEL.0;
  IW               : byte absolute $0061;
  INTCON1          : byte absolute $0065;
  INTCON1_ADIE     : bit  absolute INTCON1.7;
  INTCON1_CWIE     : bit  absolute INTCON1.6;
  INTCON1_T0IE     : bit  absolute INTCON1.5;
  INTCON1_RBIE     : bit  absolute INTCON1.4;
  INTCON1_WUR      : bit  absolute INTCON1.0;
  ISTATUS          : byte absolute $0066;
  IFSR             : byte absolute $0067;
  IBSR             : byte absolute $0068;
  IBSR_IBSR2       : bit  absolute IBSR.2;
  IBSR_IBSR1       : bit  absolute IBSR.1;
  IBSR_IBSR0       : bit  absolute IBSR.0;
  OPACON           : byte absolute $0069;
  OPACON_OPA2ON    : bit  absolute OPACON.1;
  OPACON_OPA1ON    : bit  absolute OPACON.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-7 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : EECON
                                            // Bank 2 : TMR0
                                            // Bank 3 : IW
                                            // Bank 4 : TMR0
                                            // Bank 5 : EECON
                                            // Bank 6 : TMR0
                                            // Bank 7 : IW
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-7 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-00A:SFR:ALL'}        // Bank 0 : OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES
                                            // Bank 1 : EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL
                                            // Bank 2 : OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES
                                            // Bank 3 : INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL
                                            // Bank 4 : OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES
                                            // Bank 5 : EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL
                                            // Bank 6 : OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES
                                            // Bank 7 : INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL
  {$SET_STATE_RAM '00B-00B:SFR:ALLMAPPED'}  // Banks 0-7 : INTCON0
  {$SET_STATE_RAM '00C-00F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '010-01F:GPR:ALL'}       


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '040-04B:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0 (bank 0)
  {$SET_MAPPED_RAM '06A-06B:bnk1'} // maps to ANSEL, INTCON0 (bank 1)
  {$SET_MAPPED_RAM '080-08B:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0 (bank 0)
  {$SET_MAPPED_RAM '0A0-0AB:bnk1'} // maps to INDF, EECON, PCL, STATUS, FSR, EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL, INTCON0 (bank 1)
  {$SET_MAPPED_RAM '0C0-0CB:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0 (bank 0)
  {$SET_MAPPED_RAM '0E0-0EB:bnk3'} // maps to INDF, IW, PCL, STATUS, FSR, INTCON1, ISTATUS, IFSR, IBSR, OPACON, ANSEL, INTCON0 (bank 3)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:FE'} // OSCCAL bit 0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00B:F1'} // INTCON0 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '021:1F'} // EECON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '026:3F'} // EEADR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '065:F1'} // INTCON1 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '068:07'} // IBSR bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '069:03'} // OPACON bits 7,6,5,4,3,2 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : VPP/MCLR
  // Pin  2 : RA0/AN0
  // Pin  3 : RA1/AN1/C1IN+
  // Pin  4 : RA2/AN2/CVREF1
  // Pin  5 : RA3/AN3/C2IN+
  // Pin  6 : RA4/AN4/T0CKI
  // Pin  7 : RA5/AN5
  // Pin  8 : VSS
  // Pin  9 : RA7/CLKIN/OSC1
  // Pin 10 : RA6/CLKOUT/OSC2
  // Pin 11 : RC0
  // Pin 12 : RC1/AN6/OPA1OUT
  // Pin 13 : RC2/OPA1IN-
  // Pin 14 : RC3/OPA1IN+
  // Pin 15 : RC4/OPA2IN+
  // Pin 16 : RC5/OPA2IN-
  // Pin 17 : RC6/AN7/OPA2OUT
  // Pin 18 : RC7/C2IN-
  // Pin 19 : VSS
  // Pin 20 : VDD
  // Pin 21 : RB0
  // Pin 22 : RB1
  // Pin 23 : RB2
  // Pin 24 : RB3/C1OUT
  // Pin 25 : RB4/C2OUT
  // Pin 26 : RB5
  // Pin 27 : RB6/ICSPCLK
  // Pin 28 : RB7/ICSPDAT/C1IN-/CVREF2


  // -- RAM to PIN mapping --



  // -- Bits Configuration --

  // FOSC : Oscillator
  {$define _FOSC_LP           = $0FF8}  // LP oscillator and 18 ms DRT
  {$define _FOSC_XT           = $0FF9}  // XT oscillator and 18 ms DRT
  {$define _FOSC_HS           = $0FFA}  // HS oscillator and 18 ms DRT
  {$define _FOSC_EC           = $0FFB}  // EC oscillator with I/O function on OSC2/CLKOUT
  {$define _FOSC_INTRC_IO     = $0FFC}  // INTRC with I/O function on OSC2/CLKOUT
  {$define _FOSC_INTRC_CLKOUT = $0FFD}  // INTRC with CLKOUT function on OSC2/CLKOUT
  {$define _FOSC_EXTRC_IO     = $0FFE}  // EXTRC with I/O function on OSC2/CLKOUT
  {$define _FOSC_EXTRC_CLKOUT = $0FFF}  // EXTRC with CLKOUT function on OSC2/CLKOUT

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON           = $0FFF}  // Enabled
  {$define _WDTE_OFF          = $0FF7}  // Disabled

  // CP : Code Protection bit
  {$define _CP_OFF            = $0FFF}  // Code protection off
  {$define _CP_ON             = $0FEF}  // Code protection on

  // IOSCFS : Internal Oscillator Frequency Select
  {$define _IOSCFS_8MHz       = $0FFF}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_4MHz       = $0FBF}  // 4 MHz INTOSC Speed

  // CPSW : Code Protection bit - Flash Data Memory
  {$define _CPSW_OFF          = $0FFF}  // Code protection off
  {$define _CPSW_ON           = $0F7F}  // Code protection on

  // BOREN : 
  {$define _BOREN_ON          = $0FFF}  // BOR Enabled
  {$define _BOREN_OFF         = $0EFF}  // BOR Disabled

  // DRTEN : 
  {$define _DRTEN_ON          = $0FFF}  // DRT Enabled
  {$define _DRTEN_OFF         = $0DFF}  // DRT Disabled

implementation
end.
