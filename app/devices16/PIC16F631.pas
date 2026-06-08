unit PIC16F631;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F631'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 20}
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
  STATUS_nTO        : bit  absolute STATUS.4;
  STATUS_nPD        : bit  absolute STATUS.3;
  STATUS_Z          : bit  absolute STATUS.2;
  STATUS_DC         : bit  absolute STATUS.1;
  STATUS_C          : bit  absolute STATUS.0;
  FSR               : byte absolute $0004;
  PORTA             : byte absolute $0005;
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
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_RC0         : bit  absolute PORTC.0;
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
  INTCON_RABIE      : bit  absolute INTCON.3;
  INTCON_T0IF       : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RABIF      : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  PIR2              : byte absolute $000D;
  PIR2_OSFIF        : bit  absolute PIR2.7;
  PIR2_C2IF         : bit  absolute PIR2.6;
  PIR2_C1IF         : bit  absolute PIR2.5;
  PIR2_EEIF         : bit  absolute PIR2.4;
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
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_nRABPU : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS   : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE   : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA    : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2    : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1    : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0    : bit  absolute OPTION_REG.0;
  TRISA             : byte absolute $0085;
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
  TRISC             : byte absolute $0087;
  TRISC_TRISC7      : bit  absolute TRISC.7;
  TRISC_TRISC6      : bit  absolute TRISC.6;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  PIE1              : byte absolute $008C;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PIE2              : byte absolute $008D;
  PIE2_OSFIE        : bit  absolute PIE2.7;
  PIE2_C2IE         : bit  absolute PIE2.6;
  PIE2_C1IE         : bit  absolute PIE2.5;
  PIE2_EEIE         : bit  absolute PIE2.4;
  PCON              : byte absolute $008E;
  PCON_ULPWUE       : bit  absolute PCON.5;
  PCON_SBOREN       : bit  absolute PCON.4;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  OSCCON            : byte absolute $008F;
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
  WPUA              : byte absolute $0095;
  WPUA_WPUA5        : bit  absolute WPUA.5;
  WPUA_WPUA4        : bit  absolute WPUA.4;
  WPUA_WPUA2        : bit  absolute WPUA.2;
  WPUA_WPUA1        : bit  absolute WPUA.1;
  WPUA_WPUA0        : bit  absolute WPUA.0;
  IOCA              : byte absolute $0096;
  IOCA_IOCA5        : bit  absolute IOCA.5;
  IOCA_IOCA4        : bit  absolute IOCA.4;
  IOCA_IOCA3        : bit  absolute IOCA.3;
  IOCA_IOCA2        : bit  absolute IOCA.2;
  IOCA_IOCA1        : bit  absolute IOCA.1;
  IOCA_IOCA0        : bit  absolute IOCA.0;
  WDTCON            : byte absolute $0097;
  WDTCON_WDTPS3     : bit  absolute WDTCON.4;
  WDTCON_WDTPS2     : bit  absolute WDTCON.3;
  WDTCON_WDTPS1     : bit  absolute WDTCON.2;
  WDTCON_WDTPS0     : bit  absolute WDTCON.1;
  WDTCON_SWDTEN     : bit  absolute WDTCON.0;
  EEDAT             : byte absolute $010C;
  EEADR             : byte absolute $010D;
  WPUB              : byte absolute $0115;
  WPUB_WPUB3        : bit  absolute WPUB.7;
  WPUB_WPUB2        : bit  absolute WPUB.6;
  WPUB_WPUB1        : bit  absolute WPUB.5;
  WPUB_WPUB0        : bit  absolute WPUB.4;
  IOCB              : byte absolute $0116;
  IOCB_IOCB7        : bit  absolute IOCB.7;
  IOCB_IOCB6        : bit  absolute IOCB.6;
  IOCB_IOCB5        : bit  absolute IOCB.5;
  IOCB_IOCB4        : bit  absolute IOCB.4;
  VRCON             : byte absolute $0118;
  VRCON_C1VREN      : bit  absolute VRCON.7;
  VRCON_C2VREN      : bit  absolute VRCON.6;
  VRCON_VRR         : bit  absolute VRCON.5;
  VRCON_VP6EN       : bit  absolute VRCON.4;
  VRCON_VR3         : bit  absolute VRCON.3;
  VRCON_VR2         : bit  absolute VRCON.2;
  VRCON_VR1         : bit  absolute VRCON.1;
  VRCON_VR0         : bit  absolute VRCON.0;
  CM1CON0           : byte absolute $0119;
  CM1CON0_C1ON      : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT     : bit  absolute CM1CON0.6;
  CM1CON0_C1OE      : bit  absolute CM1CON0.5;
  CM1CON0_C1POL     : bit  absolute CM1CON0.4;
  CM1CON0_C1R       : bit  absolute CM1CON0.2;
  CM1CON0_C1CH1     : bit  absolute CM1CON0.1;
  CM1CON0_C1CH0     : bit  absolute CM1CON0.0;
  CM2CON0           : byte absolute $011A;
  CM2CON0_C2ON      : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT     : bit  absolute CM2CON0.6;
  CM2CON0_C2OE      : bit  absolute CM2CON0.5;
  CM2CON0_C2POL     : bit  absolute CM2CON0.4;
  CM2CON0_C2R       : bit  absolute CM2CON0.2;
  CM2CON0_C2CH1     : bit  absolute CM2CON0.1;
  CM2CON0_C2CH0     : bit  absolute CM2CON0.0;
  CM2CON1           : byte absolute $011B;
  CM2CON1_MC1OUT    : bit  absolute CM2CON1.7;
  CM2CON1_MC2OUT    : bit  absolute CM2CON1.6;
  CM2CON1_T1GSS     : bit  absolute CM2CON1.1;
  CM2CON1_C2SYNC    : bit  absolute CM2CON1.0;
  ANSEL             : byte absolute $011E;
  ANSEL_ANS7        : bit  absolute ANSEL.7;
  ANSEL_ANS6        : bit  absolute ANSEL.6;
  ANSEL_ANS5        : bit  absolute ANSEL.5;
  ANSEL_ANS4        : bit  absolute ANSEL.4;
  ANSEL_ANS1        : bit  absolute ANSEL.1;
  ANSEL_ANS0        : bit  absolute ANSEL.0;
  EECON1            : byte absolute $018C;
  EECON1_WRERR      : bit  absolute EECON1.3;
  EECON1_WREN       : bit  absolute EECON1.2;
  EECON1_WR         : bit  absolute EECON1.1;
  EECON1_RD         : bit  absolute EECON1.0;
  EECON2            : byte absolute $018D;
  SRCON             : byte absolute $019E;
  SRCON_SR1         : bit  absolute SRCON.7;
  SRCON_SR0         : bit  absolute SRCON.6;
  SRCON_C1SEN       : bit  absolute SRCON.5;
  SRCON_C2REN       : bit  absolute SRCON.4;
  SRCON_PULSS       : bit  absolute SRCON.3;
  SRCON_PULSR       : bit  absolute SRCON.2;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-3 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
                                            // Bank 2 : TMR0
                                            // Bank 3 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-3 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-007:SFR:ALL'}        // Bank 0 : PORTA, PORTB, PORTC
                                            // Bank 1 : TRISA, TRISB, TRISC
                                            // Bank 2 : PORTA, PORTB, PORTC
                                            // Bank 3 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00D:SFR:ALL'}        // Bank 0 : PIR1, PIR2
                                            // Bank 1 : PIE1, PIE2
                                            // Bank 2 : EEDAT, EEADR
                                            // Bank 3 : EECON1, EECON2
  {$SET_STATE_RAM '00E-010:SFR'}            // Bank 0 : TMR1L, TMR1H, T1CON
  {$SET_STATE_RAM '040-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '08E-090:SFR'}            // Bank 1 : PCON, OSCCON, OSCTUNE
  {$SET_STATE_RAM '095-097:SFR'}            // Bank 1 : WPUA, IOCA, WDTCON
  {$SET_STATE_RAM '115-116:SFR'}            // Bank 2 : WPUB, IOCB
  {$SET_STATE_RAM '118-11B:SFR'}            // Bank 2 : VRCON, CM1CON0, CM2CON0, CM2CON1
  {$SET_STATE_RAM '11E-11E:SFR'}            // Bank 2 : ANSEL
  {$SET_STATE_RAM '19E-19E:SFR'}            // Bank 3 : SRCON


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '101-101:bnk0'} // maps to TMR0 (bank 0)
  {$SET_MAPPED_RAM '105-107:bnk0'} // maps to PORTA, PORTB, PORTC (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)
  {$SET_MAPPED_RAM '185-187:bnk1'} // maps to TRISA, TRISB, TRISC (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:F0'} // PORTB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00C:01'} // PIR1 bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00D:F0'} // PIR2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:3F'} // TRISA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '086:F0'} // TRISB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08C:01'} // PIE1 bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08D:F0'} // PIE2 bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:33'} // PCON bits 7,6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08F:7F'} // OSCCON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:1F'} // OSCTUNE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '095:37'} // WPUA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:3F'} // IOCA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '097:1F'} // WDTCON bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:F0'} // WPUB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:F0'} // IOCB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '119:F7'} // CM1CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11A:F7'} // CM2CON0 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11B:C3'} // CM2CON1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11E:F3'} // ANSEL bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:0F'} // EECON1 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '19E:FC'} // SRCON bits 1,0 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/OSC1/CLKIN
  // Pin  3 : RA4/T1G/OSC2/CLKOUT
  // Pin  4 : RA3/MCLR/Vpp
  // Pin  5 : RC5
  // Pin  6 : RC4/C2OUT
  // Pin  7 : RC3/AN7/C12IN3-
  // Pin  8 : RC6
  // Pin  9 : RC7
  // Pin 10 : RB7
  // Pin 11 : RB6
  // Pin 12 : RB5
  // Pin 13 : RB4
  // Pin 14 : RC2/AN6/C12IN2-
  // Pin 15 : RC1/AN5/C12IN1-
  // Pin 16 : RC0/AN4/C2IN+
  // Pin 17 : RA2/T0CKI/INT/C1OUT
  // Pin 18 : RA1/AN1/C12IN0-/ICSPCK
  // Pin 19 : RA0/AN0/C1IN+/ICSPDAT/ULPWU
  // Pin 20 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-19,1-18,2-17,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '006:4-13,5-12,6-11,7-10'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_EXTRCCLK = $3FFF}  // RC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, RC on RA5/OSC1/CLKIN
  {$define _FOSC_EXTRCIO  = $3FFE}  // RCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, RC on RA5/OSC1/CLKIN
  {$define _FOSC_INTRCCLK = $3FFD}  // INTOSC oscillator: CLKOUT function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_INTRCIO  = $3FFC}  // INTOSCIO oscillator: I/O function on RA4/OSC2/CLKOUT pin, I/O function on RA5/OSC1/CLKIN
  {$define _FOSC_EC       = $3FFB}  // EC: I/O function on RA4/OSC2/CLKOUT pin, CLKIN on RA5/OSC1/CLKIN
  {$define _FOSC_HS       = $3FFA}  // HS oscillator: High-speed crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_XT       = $3FF9}  // XT oscillator: Crystal/resonator on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN
  {$define _FOSC_LP       = $3FF8}  // LP oscillator: Low-power crystal on RA4/OSC2/CLKOUT and RA5/OSC1/CLKIN

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON       = $3FFF}  // WDT enabled
  {$define _WDTE_OFF      = $3FF7}  // WDT disabled and can be enabled by SWDTEN bit of the WDTCON register

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF     = $3FFF}  // PWRT disabled
  {$define _PWRTE_ON      = $3FEF}  // PWRT enabled

  // MCLRE : MCLR Pin Function Select bit
  {$define _MCLRE_ON      = $3FFF}  // MCLR pin function is MCLR
  {$define _MCLRE_OFF     = $3FDF}  // MCLR pin function is digital input, MCLR internally tied to VDD

  // CP : Code Protection bit
  {$define _CP_OFF        = $3FFF}  // Program memory code protection is disabled
  {$define _CP_ON         = $3FBF}  // Program memory code protection is enabled

  // CPD : Data Code Protection bit
  {$define _CPD_OFF       = $3FFF}  // Data memory code protection is disabled
  {$define _CPD_ON        = $3F7F}  // Data memory code protection is enabled

  // BOREN : Brown-out Reset Selection bits
  {$define _BOREN_ON      = $3FFF}  // BOR enabled
  {$define _BOREN_NSLEEP  = $3EFF}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_SBODEN  = $3DFF}  // BOR controlled by SBOREN bit of the PCON register
  {$define _BOREN_OFF     = $3CFF}  // BOR disabled

  // IESO : Internal External Switchover bit
  {$define _IESO_ON       = $3FFF}  // Internal External Switchover mode is enabled
  {$define _IESO_OFF      = $3BFF}  // Internal External Switchover mode is disabled

  // FCMEN : Fail-Safe Clock Monitor Enabled bit
  {$define _FCMEN_ON      = $3FFF}  // Fail-Safe Clock Monitor is enabled
  {$define _FCMEN_OFF     = $37FF}  // Fail-Safe Clock Monitor is disabled

implementation
end.
