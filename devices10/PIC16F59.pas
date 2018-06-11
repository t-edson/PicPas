unit PIC16F59;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F59'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 40}
{$SET PIC_NUMBANKS = 8}
{$SET PIC_NUMPAGES = 4}
{$SET PIC_MAXFLASH = 2048}

interface
var
  INDF        : byte absolute $0000;
  TMR0        : byte absolute $0001;
  PCL         : byte absolute $0002;
  STATUS      : byte absolute $0003;
  STATUS_PA2  : bit  absolute STATUS.7;
  STATUS_PA1  : bit  absolute STATUS.6;
  STATUS_PA0  : bit  absolute STATUS.5;
  STATUS_TO   : bit  absolute STATUS.4;
  STATUS_PD   : bit  absolute STATUS.3;
  STATUS_Z    : bit  absolute STATUS.2;
  STATUS_DC   : bit  absolute STATUS.1;
  STATUS_C    : bit  absolute STATUS.0;
  FSR         : byte absolute $0004;
  PORTA       : byte absolute $0005;
  PORTA_T0CKI : bit  absolute PORTA.4;
  PORTA_RA3   : bit  absolute PORTA.3;
  PORTA_RA2   : bit  absolute PORTA.2;
  PORTA_RA1   : bit  absolute PORTA.1;
  PORTA_RA0   : bit  absolute PORTA.0;
  PORTB       : byte absolute $0006;
  PORTB_RB7   : bit  absolute PORTB.7;
  PORTB_RB6   : bit  absolute PORTB.6;
  PORTB_RB5   : bit  absolute PORTB.5;
  PORTB_RB4   : bit  absolute PORTB.4;
  PORTB_RB3   : bit  absolute PORTB.3;
  PORTB_RB2   : bit  absolute PORTB.2;
  PORTB_RB1   : bit  absolute PORTB.1;
  PORTB_RB0   : bit  absolute PORTB.0;
  PORTC       : byte absolute $0007;
  PORTC_RC7   : bit  absolute PORTC.7;
  PORTC_RC6   : bit  absolute PORTC.6;
  PORTC_RC5   : bit  absolute PORTC.5;
  PORTC_RC4   : bit  absolute PORTC.4;
  PORTC_RC3   : bit  absolute PORTC.3;
  PORTC_RC2   : bit  absolute PORTC.2;
  PORTC_RC1   : bit  absolute PORTC.1;
  PORTC_RC0   : bit  absolute PORTC.0;
  PORTD       : byte absolute $0008;
  PORTD_RD7   : bit  absolute PORTD.7;
  PORTD_RD6   : bit  absolute PORTD.6;
  PORTD_RD5   : bit  absolute PORTD.5;
  PORTD_RD4   : bit  absolute PORTD.4;
  PORTD_RD3   : bit  absolute PORTD.3;
  PORTD_RD2   : bit  absolute PORTD.2;
  PORTD_RD1   : bit  absolute PORTD.1;
  PORTD_RD0   : bit  absolute PORTD.0;
  PORTE       : byte absolute $0009;
  PORTE_RE7   : bit  absolute PORTE.7;
  PORTE_RE6   : bit  absolute PORTE.6;
  PORTE_RE5   : bit  absolute PORTE.5;
  PORTE_RE4   : bit  absolute PORTE.4;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-009:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '00A-00F:GPR'} 
  {$SET_STATE_RAM '010-01F:GPR'} 
  {$SET_STATE_RAM '020-029:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '02A-02F:GPR'} 
  {$SET_STATE_RAM '030-03F:GPR'} 
  {$SET_STATE_RAM '040-049:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '04A-04F:GPR'} 
  {$SET_STATE_RAM '050-05F:GPR'} 
  {$SET_STATE_RAM '060-069:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '06A-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-089:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '08A-08F:GPR'} 
  {$SET_STATE_RAM '090-09F:GPR'} 
  {$SET_STATE_RAM '0A0-0A9:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '0AA-0AF:GPR'} 
  {$SET_STATE_RAM '0B0-0BF:GPR'} 
  {$SET_STATE_RAM '0C0-0C9:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '0CA-0CF:GPR'} 
  {$SET_STATE_RAM '0D0-0DF:GPR'} 
  {$SET_STATE_RAM '0E0-0E9:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC, PORTD, PORTE
  {$SET_STATE_RAM '0EA-0EF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '020-029:bnk1'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '02A-02F:bnk1'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '040-049:bnk2'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '04A-04F:bnk2'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '060-069:bnk3'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '06A-06F:bnk3'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '080-089:bnk4'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '08A-08F:bnk4'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '0A0-0A9:bnk5'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '0AA-0AF:bnk5'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '0C0-0C9:bnk6'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '0CA-0CF:bnk6'} // maps to area 00A-00F (bank 0)
  {$SET_MAPPED_RAM '0E0-0E9:bnk7'} // maps to area 000-009 (bank 0)
  {$SET_MAPPED_RAM '0EA-0EF:bnk7'} // maps to area 00A-00F (bank 0)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:1F'} // PORTA bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '009:F0'} // PORTE bits 3,2,1,0 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : RA0
  // Pin  2 : RA1
  // Pin  3 : RA2
  // Pin  4 : RA3
  // Pin  5 : Vss
  // Pin  6 : RB0
  // Pin  7 : RB1
  // Pin  8 : RB2
  // Pin  9 : RB3
  // Pin 10 : RB4
  // Pin 11 : RB5
  // Pin 12 : RB6/ICSPCLK
  // Pin 13 : RB7/ICSPDAT
  // Pin 14 : MCLR/Vpp
  // Pin 15 : Vdd
  // Pin 16 : RC0
  // Pin 17 : RC1
  // Pin 18 : RC2
  // Pin 19 : RC3
  // Pin 20 : RC4
  // Pin 21 : RC5
  // Pin 22 : RC6
  // Pin 23 : RC7
  // Pin 24 : RD0
  // Pin 25 : Vss
  // Pin 26 : RD1
  // Pin 27 : RD2
  // Pin 28 : RD3
  // Pin 29 : RD4
  // Pin 30 : RD5
  // Pin 31 : RD6
  // Pin 32 : RD7
  // Pin 33 : OSC2/CLKOUT
  // Pin 34 : OSC1/CLKIN
  // Pin 35 : Vdd
  // Pin 36 : RE4
  // Pin 37 : RE5
  // Pin 38 : RE6
  // Pin 39 : RE7
  // Pin 40 : T0CKI


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-1,1-2,2-3,3-4,4-40'} // PORTA
  {$MAP_RAM_TO_PIN '006:0-6,1-7,2-8,3-9,4-10,5-11,6-12,7-13'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-16,1-17,2-18,3-19,4-20,5-21,6-22,7-23'} // PORTC
  {$MAP_RAM_TO_PIN '008:0-24,1-26,2-27,3-28,4-29,5-30,6-31,7-32'} // PORTD
  {$MAP_RAM_TO_PIN '009:4-36,5-37,6-38,7-39'} // PORTE


  // -- Bits Configuration --

  // OSC : Oscillator selection bits
  {$define _OSC_RC  = $0FFF}  // RC oscillator
  {$define _OSC_HS  = $0FFE}  // HS oscillator
  {$define _OSC_XT  = $0FFD}  // XT oscillator
  {$define _OSC_LP  = $0FFC}  // LP oscillator

  // WDT : Watchdog timer enable bit
  {$define _WDT_ON  = $0FFF}  // WDT enabled
  {$define _WDT_OFF = $0FFB}  // WDT disabled

  // CP : Code protection bit
  {$define _CP_OFF  = $0FFF}  // Code protection off
  {$define _CP_ON   = $0FF7}  // Code protection on

implementation
end.
