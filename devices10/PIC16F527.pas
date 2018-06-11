unit PIC16F527;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F527'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF            : byte absolute $0000;
  TMR0            : byte absolute $0001;
  PCL             : byte absolute $0002;
  STATUS          : byte absolute $0003;
  STATUS_PA1      : bit  absolute STATUS.7;
  STATUS_PA0      : bit  absolute STATUS.6;
  STATUS_TO       : bit  absolute STATUS.5;
  STATUS_PD       : bit  absolute STATUS.4;
  STATUS_Z        : bit  absolute STATUS.3;
  STATUS_DC       : bit  absolute STATUS.2;
  STATUS_C        : bit  absolute STATUS.1;
  FSR             : byte absolute $0004;
  OSCCAL          : byte absolute $0005;
  OSCCAL_CAL6     : bit  absolute OSCCAL.7;
  OSCCAL_CAL5     : bit  absolute OSCCAL.6;
  OSCCAL_CAL4     : bit  absolute OSCCAL.5;
  OSCCAL_CAL3     : bit  absolute OSCCAL.4;
  OSCCAL_CAL2     : bit  absolute OSCCAL.3;
  OSCCAL_CAL1     : bit  absolute OSCCAL.2;
  OSCCAL_CAL0     : bit  absolute OSCCAL.1;
  PORTA           : byte absolute $0006;
  PORTA_RA5       : bit  absolute PORTA.5;
  PORTA_RA4       : bit  absolute PORTA.4;
  PORTA_RA3       : bit  absolute PORTA.3;
  PORTA_RA2       : bit  absolute PORTA.2;
  PORTA_RA1       : bit  absolute PORTA.1;
  PORTA_RA0       : bit  absolute PORTA.0;
  PORTB           : byte absolute $0007;
  PORTB_RB7       : bit  absolute PORTB.4;
  PORTB_RB6       : bit  absolute PORTB.3;
  PORTB_RB5       : bit  absolute PORTB.2;
  PORTB_RB4       : bit  absolute PORTB.1;
  PORTC           : byte absolute $0008;
  PORTC_RC7       : bit  absolute PORTC.7;
  PORTC_RC6       : bit  absolute PORTC.6;
  PORTC_RC5       : bit  absolute PORTC.5;
  PORTC_RC4       : bit  absolute PORTC.4;
  PORTC_RC3       : bit  absolute PORTC.3;
  PORTC_RC2       : bit  absolute PORTC.2;
  PORTC_RC1       : bit  absolute PORTC.1;
  PORTC_RC0       : bit  absolute PORTC.0;
  ADCON0          : byte absolute $0009;
  ADCON0_ADCS1    : bit  absolute ADCON0.7;
  ADCON0_ADCS0    : bit  absolute ADCON0.6;
  ADCON0_CHS3     : bit  absolute ADCON0.5;
  ADCON0_CHS2     : bit  absolute ADCON0.4;
  ADCON0_CHS1     : bit  absolute ADCON0.3;
  ADCON0_CHS0     : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE : bit  absolute ADCON0.1;
  ADCON0_ADON     : bit  absolute ADCON0.0;
  ADRES           : byte absolute $000A;
  ADRES_ADRES7    : bit  absolute ADRES.7;
  ADRES_ADRES6    : bit  absolute ADRES.6;
  ADRES_ADRES5    : bit  absolute ADRES.5;
  ADRES_ADRES4    : bit  absolute ADRES.4;
  ADRES_ADRES3    : bit  absolute ADRES.3;
  ADRES_ADRES2    : bit  absolute ADRES.2;
  ADRES_ADRES1    : bit  absolute ADRES.1;
  ADRES_ADRES0    : bit  absolute ADRES.0;
  INTCON0         : byte absolute $000B;
  INTCON0_ADIF    : bit  absolute INTCON0.7;
  INTCON0_CWIF    : bit  absolute INTCON0.6;
  INTCON0_T0IF    : bit  absolute INTCON0.5;
  INTCON0_RAIF    : bit  absolute INTCON0.4;
  INTCON0_GIE     : bit  absolute INTCON0.3;
  EECON           : byte absolute $0021;
  EECON_FREE      : bit  absolute EECON.4;
  EECON_WRERR     : bit  absolute EECON.3;
  EECON_WREN      : bit  absolute EECON.2;
  EECON_WR        : bit  absolute EECON.1;
  EECON_RD        : bit  absolute EECON.0;
  EEDATA          : byte absolute $0025;
  EEADR           : byte absolute $0026;
  CM1CON0         : byte absolute $0027;
  CM1CON0_C1OUT   : bit  absolute CM1CON0.7;
  CM1CON0_C1OUTEN : bit  absolute CM1CON0.6;
  CM1CON0_C1POL   : bit  absolute CM1CON0.5;
  CM1CON0_C1T0CS  : bit  absolute CM1CON0.4;
  CM1CON0_C1ON    : bit  absolute CM1CON0.3;
  CM1CON0_C1NREF  : bit  absolute CM1CON0.2;
  CM1CON0_C1PREF  : bit  absolute CM1CON0.1;
  CM1CON0_C1WU    : bit  absolute CM1CON0.0;
  CM2CON0         : byte absolute $0028;
  CM2CON0_C2OUT   : bit  absolute CM2CON0.7;
  CM2CON0_C2OUTEN : bit  absolute CM2CON0.6;
  CM2CON0_C2POL   : bit  absolute CM2CON0.5;
  CM2CON0_C2PREF2 : bit  absolute CM2CON0.4;
  CM2CON0_C2ON    : bit  absolute CM2CON0.3;
  CM2CON0_C2NREF  : bit  absolute CM2CON0.2;
  CM2CON0_C2PREF1 : bit  absolute CM2CON0.1;
  CM2CON0_C2WU    : bit  absolute CM2CON0.0;
  VRCON           : byte absolute $0029;
  VRCON_VREN      : bit  absolute VRCON.7;
  VRCON_VROE      : bit  absolute VRCON.6;
  VRCON_VRR       : bit  absolute VRCON.5;
  VRCON_VR3       : bit  absolute VRCON.3;
  VRCON_VR2       : bit  absolute VRCON.2;
  VRCON_VR1       : bit  absolute VRCON.1;
  VRCON_VR0       : bit  absolute VRCON.0;
  ANSEL           : byte absolute $002A;
  ANSEL_ANS7      : bit  absolute ANSEL.7;
  ANSEL_ANS6      : bit  absolute ANSEL.6;
  ANSEL_ANS5      : bit  absolute ANSEL.5;
  ANSEL_ANS4      : bit  absolute ANSEL.4;
  ANSEL_ANS3      : bit  absolute ANSEL.3;
  ANSEL_ANS2      : bit  absolute ANSEL.2;
  ANSEL_ANS1      : bit  absolute ANSEL.1;
  ANSEL_ANS0      : bit  absolute ANSEL.0;
  IW              : byte absolute $0061;
  INTCON1         : byte absolute $0065;
  INTCON1_ADIE    : bit  absolute INTCON1.7;
  INTCON1_CWIE    : bit  absolute INTCON1.6;
  INTCON1_T0IE    : bit  absolute INTCON1.5;
  INTCON1_RAIE    : bit  absolute INTCON1.4;
  INTCON1_WUR     : bit  absolute INTCON1.3;
  ISTATUS         : byte absolute $0066;
  IFSR            : byte absolute $0067;
  IBSR            : byte absolute $0068;
  OPACON          : byte absolute $0069;
  OPACON_OPA2ON   : bit  absolute OPACON.1;
  OPACON_OPA1ON   : bit  absolute OPACON.0;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-00B:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0
  {$SET_STATE_RAM '00C-00F:GPR'} 
  {$SET_STATE_RAM '010-01F:GPR'} 
  {$SET_STATE_RAM '020-020:SFR'}  // mapped to INDF
  {$SET_STATE_RAM '021-021:SFR'}  // EECON
  {$SET_STATE_RAM '022-022:SFR'}  // mapped to PCL
  {$SET_STATE_RAM '025-02A:SFR'}  // EEDATA, EEADR, CM1CON0, CM2CON0, VRCON, ANSEL
  {$SET_STATE_RAM '02B-02B:SFR'}  // mapped to INTCON0
  {$SET_STATE_RAM '02C-02F:GPR'} 
  {$SET_STATE_RAM '030-03F:GPR'} 
  {$SET_STATE_RAM '040-040:SFR'}  // mapped to INDF
  {$SET_STATE_RAM '04C-04F:GPR'} 
  {$SET_STATE_RAM '050-05F:GPR'} 
  {$SET_STATE_RAM '060-060:SFR'}  // mapped to INDF
  {$SET_STATE_RAM '061-061:SFR'}  // IW
  {$SET_STATE_RAM '062-062:SFR'}  // mapped to PCL
  {$SET_STATE_RAM '065-069:SFR'}  // INTCON1, ISTATUS, IFSR, IBSR, OPACON
  {$SET_STATE_RAM '06A-06B:SFR'}  // mapped to ANSEL, INTCON0
  {$SET_STATE_RAM '06C-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '020-020:bnk0'} // maps to INDF (bank 0)
  {$SET_MAPPED_RAM '022-024:bnk0'} // maps to PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '02B-02B:bnk0'} // maps to INTCON0 (bank 0)
  {$SET_MAPPED_RAM '02C-02F:bnk0'} // maps to area 00C-00F (bank 0)
  {$SET_MAPPED_RAM '040-04B:bnk0'} // maps to INDF, TMR0, PCL, STATUS, FSR, OSCCAL, PORTA, PORTB, PORTC, ADCON0, ADRES, INTCON0 (bank 0)
  {$SET_MAPPED_RAM '04C-04F:bnk0'} // maps to area 00C-00F (bank 0)
  {$SET_MAPPED_RAM '060-060:bnk0'} // maps to INDF (bank 0)
  {$SET_MAPPED_RAM '062-064:bnk0'} // maps to PCL, STATUS, FSR (bank 0)
  {$SET_MAPPED_RAM '06A-06A:bnk1'} // maps to ANSEL (bank 1)
  {$SET_MAPPED_RAM '06B-06B:bnk0'} // maps to INTCON0 (bank 0)
  {$SET_MAPPED_RAM '06C-06F:bnk0'} // maps to area 00C-00F (bank 0)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:7F'} // STATUS bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '004:7F'} // FSR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '005:FE'} // OSCCAL bit 0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '007:F0'} // PORTB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00B:F1'} // INTCON0 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '021:1F'} // EECON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '026:3F'} // EEADR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '029:EF'} // VRCON bit 4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '065:F1'} // INTCON1 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '066:7F'} // ISTATUS bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '067:7F'} // IFSR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '068:03'} // IBSR bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '069:03'} // OPACON bits 7,6,5,4,3,2 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : VDD
  // Pin  2 : RA5/OSC1/CLKIN
  // Pin  3 : RA4/AN3/OSC2/CLKOUT
  // Pin  4 : VPP/MCLR/RA3
  // Pin  5 : RC5
  // Pin  6 : RC4/C2OUT
  // Pin  7 : RC3/AN7/OP1
  // Pin  8 : RC6/OP1-
  // Pin  9 : RC7/OP1+
  // Pin 10 : RB7
  // Pin 11 : RB6
  // Pin 12 : RB5/OP2+
  // Pin 13 : RB4/OP2-
  // Pin 14 : RC2/AN6/OP2
  // Pin 15 : RC1/AN5/C2IN-
  // Pin 16 : RC0/AN4/C2IN+
  // Pin 17 : RA2/AN2/C1OUT/T0CKI
  // Pin 18 : RA1/AN1/C1IN-/CVREF/ICSPCLK
  // Pin 19 : RA0/AN0/C1IN+/ICSPDAT
  // Pin 20 : VSS


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '006:0-19,1-18,2-17,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '007:1-13,2-12,3-11,4-10'} // PORTB
  {$MAP_RAM_TO_PIN '008:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection
  {$define _FOSC_LP           = $0FF8}  // LP oscillator and automatic 18 ms DRT (DRTEN ignored)
  {$define _FOSC_XT           = $0FF9}  // XT oscillator and automatic 18 ms DRT (DRTEN ignored)
  {$define _FOSC_HS           = $0FFA}  // HS oscillator and automatic 18 ms DRT (DRTEN ignored)
  {$define _FOSC_EC           = $0FFB}  // EC oscillator with I/O function on OSC2/CLKOUT and 10 us startup time
  {$define _FOSC_INTRC_IO     = $0FFC}  // INTRC with I/O function on OSC2/CLKOUT and 10 us startup time
  {$define _FOSC_INTRC_CLKOUT = $0FFD}  // INTRC with CLKOUT function on OSC2/CLKOUT and 10 us startup time
  {$define _FOSC_EXTRC_IO     = $0FFE}  // EXTRC with I/O function on OSC2/CLKOUT and 10 us startup time
  {$define _FOSC_EXTRC_CLKOUT = $0FFF}  // EXTRC with CLKOUT function on OSC2/CLKOUT and 10 us startup time

  // WDTE : Watchdog Timer Enable
  {$define _WDTE_ON           = $0FFF}  // WDT Enabled
  {$define _WDTE_OFF          = $0FF7}  // WDT Disabled

  // CP : Code Protection - User Program Memory
  {$define _CP_OFF            = $0FFF}  // Code protection off
  {$define _CP_ON             = $0FEF}  // Code protection on

  // MCLRE : Master Clear Enable
  {$define _MCLRE_ON          = $0FFF}  // MCLR pin functions as MCLR
  {$define _MCLRE_OFF         = $0FDF}  // MCLR pin functions as I/O, MCLR internally tied to Vdd

  // IOSCFS : Internal Oscillator Frequency Select
  {$define _IOSCFS_8MHz       = $0FFF}  // 8 MHz INTOSC Speed
  {$define _IOSCFS_4MHz       = $0FBF}  // 4 MHz INTOSC Speed

  // CPSW : Code Protection - Self Writable Memory
  {$define _CPSW_OFF          = $0FFF}  // Code protection off
  {$define _CPSW_ON           = $0F7F}  // Code protection on

  // BOREN : Brown-out Reset Enable
  {$define _BOREN_ON          = $0FFF}  // BOR Enabled
  {$define _BOREN_OFF         = $0EFF}  // BOR Disabled

  // DRTEN : Device Reset Timer Enable
  {$define _DRTEN_ON          = $0FFF}  // DRT Enabled (18 ms)
  {$define _DRTEN_OFF         = $0DFF}  // DRT Disabled

implementation
end.
