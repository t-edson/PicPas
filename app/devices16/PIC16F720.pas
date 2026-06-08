unit PIC16F720;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F720'}
{$SET PIC_MAXFREQ  = 16000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 4}
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
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RABIE      : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RABIF      : bit  absolute INTCON.0;
  PIR1              : byte absolute $000C;
  PIR1_TMR1GIF      : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  TMR1L             : byte absolute $000E;
  TMR1H             : byte absolute $000F;
  T1CON             : byte absolute $0010;
  T1CON_TMR1CS1     : bit  absolute T1CON.7;
  T1CON_TMR1CS0     : bit  absolute T1CON.6;
  T1CON_T1CKPS1     : bit  absolute T1CON.5;
  T1CON_T1CKPS0     : bit  absolute T1CON.4;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1ON      : bit  absolute T1CON.0;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS1     : bit  absolute T2CON.1;
  T2CON_T2CKPS0     : bit  absolute T2CON.0;
  SSPBUF            : byte absolute $0013;
  SSPCON            : byte absolute $0014;
  SSPCON_WCOL       : bit  absolute SSPCON.7;
  SSPCON_SSPOV      : bit  absolute SSPCON.6;
  SSPCON_SSPEN      : bit  absolute SSPCON.5;
  SSPCON_CKP        : bit  absolute SSPCON.4;
  SSPCON_SSPM3      : bit  absolute SSPCON.3;
  SSPCON_SSPM2      : bit  absolute SSPCON.2;
  SSPCON_SSPM1      : bit  absolute SSPCON.1;
  SSPCON_SSPM0      : bit  absolute SSPCON.0;
  CCPR1L            : byte absolute $0015;
  CCPR1H            : byte absolute $0016;
  CCP1CON           : byte absolute $0017;
  CCP1CON_DC1       : bit  absolute CCP1CON.5;
  CCP1CON_B1        : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3    : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2    : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1    : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0    : bit  absolute CCP1CON.0;
  RCSTA             : byte absolute $0018;
  RCSTA_SPEN        : bit  absolute RCSTA.7;
  RCSTA_RX9         : bit  absolute RCSTA.6;
  RCSTA_SREN        : bit  absolute RCSTA.5;
  RCSTA_CREN        : bit  absolute RCSTA.4;
  RCSTA_ADDEN       : bit  absolute RCSTA.3;
  RCSTA_FERR        : bit  absolute RCSTA.2;
  RCSTA_OERR        : bit  absolute RCSTA.1;
  RCSTA_RX9D        : bit  absolute RCSTA.0;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001A;
  ADRES             : byte absolute $001E;
  ADCON0            : byte absolute $001F;
  ADCON0_CHS3       : bit  absolute ADCON0.5;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
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
  PIE1_TMR1GIE      : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PCON              : byte absolute $008E;
  PCON_nPOR         : bit  absolute PCON.1;
  PCON_nBOR         : bit  absolute PCON.0;
  T1GCON            : byte absolute $008F;
  T1GCON_TMR1GE     : bit  absolute T1GCON.7;
  T1GCON_T1GPOL     : bit  absolute T1GCON.6;
  T1GCON_T1GTM      : bit  absolute T1GCON.5;
  T1GCON_T1GSPM     : bit  absolute T1GCON.4;
  T1GCON_T1GGO_DONE : bit  absolute T1GCON.3;
  T1GCON_T1GVAL     : bit  absolute T1GCON.2;
  T1GCON_T1GSS1     : bit  absolute T1GCON.1;
  T1GCON_T1GSS0     : bit  absolute T1GCON.0;
  OSCCON            : byte absolute $0090;
  OSCCON_IRCF1      : bit  absolute OSCCON.5;
  OSCCON_IRCF0      : bit  absolute OSCCON.4;
  OSCCON_ICSL       : bit  absolute OSCCON.3;
  OSCCON_ICSS       : bit  absolute OSCCON.2;
  OSCTUNE           : byte absolute $0091;
  OSCTUNE_TUN5      : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4      : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3      : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2      : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1      : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0      : bit  absolute OSCTUNE.0;
  PR2               : byte absolute $0092;
  SSPADD            : byte absolute $0093;
  SSPSTAT           : byte absolute $0094;
  SSPSTAT_SMP       : bit  absolute SSPSTAT.7;
  SSPSTAT_CKE       : bit  absolute SSPSTAT.6;
  SSPSTAT_D_nA      : bit  absolute SSPSTAT.5;
  SSPSTAT_P         : bit  absolute SSPSTAT.4;
  SSPSTAT_S         : bit  absolute SSPSTAT.3;
  SSPSTAT_R_nW      : bit  absolute SSPSTAT.2;
  SSPSTAT_UA        : bit  absolute SSPSTAT.1;
  SSPSTAT_BF        : bit  absolute SSPSTAT.0;
  WPUA              : byte absolute $0095;
  WPUA_WPUA5        : bit  absolute WPUA.5;
  WPUA_WPUA4        : bit  absolute WPUA.4;
  WPUA_WPUA3        : bit  absolute WPUA.3;
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
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.2;
  TXSTA_TRMT        : bit  absolute TXSTA.1;
  TXSTA_TX9D        : bit  absolute TXSTA.0;
  SPBRG             : byte absolute $0099;
  FVRCON            : byte absolute $009D;
  FVRCON_FVRRDY     : bit  absolute FVRCON.7;
  FVRCON_FVREN      : bit  absolute FVRCON.6;
  FVRCON_TSEN       : bit  absolute FVRCON.5;
  FVRCON_TSRNG      : bit  absolute FVRCON.4;
  FVRCON_ADFVR1     : bit  absolute FVRCON.1;
  FVRCON_ADFVR0     : bit  absolute FVRCON.0;
  ADCON1            : byte absolute $009F;
  ADCON1_ADCS2      : bit  absolute ADCON1.6;
  ADCON1_ADCS1      : bit  absolute ADCON1.5;
  ADCON1_ADCS0      : bit  absolute ADCON1.4;
  PMDATL            : byte absolute $010C;
  PMADRL            : byte absolute $010D;
  PMDATH            : byte absolute $010E;
  PMDATH_PMDATH5    : bit  absolute PMDATH.5;
  PMDATH_PMDATH4    : bit  absolute PMDATH.4;
  PMDATH_PMDATH3    : bit  absolute PMDATH.3;
  PMDATH_PMDATH2    : bit  absolute PMDATH.2;
  PMDATH_PMDATH1    : bit  absolute PMDATH.1;
  PMDATH_PMDATH0    : bit  absolute PMDATH.0;
  PMADRH            : byte absolute $010F;
  PMADRH_PMADRH4    : bit  absolute PMADRH.4;
  PMADRH_PMADRH3    : bit  absolute PMADRH.3;
  PMADRH_PMADRH2    : bit  absolute PMADRH.2;
  PMADRH_PMADRH1    : bit  absolute PMADRH.1;
  PMADRH_PMADRH0    : bit  absolute PMADRH.0;
  WPUB              : byte absolute $0115;
  WPUB_WPUB7        : bit  absolute WPUB.7;
  WPUB_WPUB6        : bit  absolute WPUB.6;
  WPUB_WPUB5        : bit  absolute WPUB.5;
  WPUB_WPUB4        : bit  absolute WPUB.4;
  IOCB              : byte absolute $0116;
  IOCB_IOCB7        : bit  absolute IOCB.7;
  IOCB_IOCB6        : bit  absolute IOCB.6;
  IOCB_IOCB5        : bit  absolute IOCB.5;
  IOCB_IOCB4        : bit  absolute IOCB.4;
  ANSELA            : byte absolute $0185;
  ANSELA_ANSA5      : bit  absolute ANSELA.5;
  ANSELA_ANSA4      : bit  absolute ANSELA.4;
  ANSELA_ANSA2      : bit  absolute ANSELA.2;
  ANSELA_ANSA1      : bit  absolute ANSELA.1;
  ANSELA_ANSA0      : bit  absolute ANSELA.0;
  ANSELB            : byte absolute $0186;
  ANSELB_ANSB5      : bit  absolute ANSELB.5;
  ANSELB_ANSB4      : bit  absolute ANSELB.4;
  ANSELC            : byte absolute $0187;
  ANSELC_ANSC7      : bit  absolute ANSELC.7;
  ANSELC_ANSC6      : bit  absolute ANSELC.6;
  ANSELC_ANSC3      : bit  absolute ANSELC.3;
  ANSELC_ANSC2      : bit  absolute ANSELC.2;
  ANSELC_ANSC1      : bit  absolute ANSELC.1;
  ANSELC_ANSC0      : bit  absolute ANSELC.0;
  PMCON1            : byte absolute $018C;
  PMCON1_CFGS       : bit  absolute PMCON1.6;
  PMCON1_LWLO       : bit  absolute PMCON1.5;
  PMCON1_FREE       : bit  absolute PMCON1.4;
  PMCON1_WREN       : bit  absolute PMCON1.2;
  PMCON1_WR         : bit  absolute PMCON1.1;
  PMCON1_RD         : bit  absolute PMCON1.0;
  PMCON2            : byte absolute $018D;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-000:SFR:ALLMAPPED'}  // Banks 0-3 : INDF
  {$SET_STATE_RAM '001-001:SFR:ALL'}        // Bank 0 : TMR0
                                            // Bank 1 : OPTION_REG
                                            // Bank 2 : TMR0
                                            // Bank 3 : OPTION_REG
  {$SET_STATE_RAM '002-004:SFR:ALLMAPPED'}  // Banks 0-3 : PCL, STATUS, FSR
  {$SET_STATE_RAM '005-007:SFR'}            // Bank 0 : PORTA, PORTB, PORTC
  {$SET_STATE_RAM '00A-00B:SFR:ALLMAPPED'}  // Banks 0-3 : PCLATH, INTCON
  {$SET_STATE_RAM '00C-00C:SFR:ALL'}        // Bank 0 : PIR1
                                            // Bank 1 : PIE1
                                            // Bank 2 : PMDATL
                                            // Bank 3 : PMCON1
  {$SET_STATE_RAM '00E-01A:SFR'}            // Bank 0 : TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG
  {$SET_STATE_RAM '01E-01F:SFR'}            // Bank 0 : ADRES, ADCON0
  {$SET_STATE_RAM '020-06F:GPR'}           
  {$SET_STATE_RAM '070-07F:GPR:ALLMAPPED'} 
  {$SET_STATE_RAM '085-087:SFR'}            // Bank 1 : TRISA, TRISB, TRISC
  {$SET_STATE_RAM '08E-096:SFR'}            // Bank 1 : PCON, T1GCON, OSCCON, OSCTUNE, PR2, SSPADD, SSPSTAT, WPUA, IOCA
  {$SET_STATE_RAM '098-099:SFR'}            // Bank 1 : TXSTA, SPBRG
  {$SET_STATE_RAM '09D-09D:SFR'}            // Bank 1 : FVRCON
  {$SET_STATE_RAM '09F-09F:SFR'}            // Bank 1 : ADCON1
  {$SET_STATE_RAM '0A0-0BF:GPR'}           
  {$SET_STATE_RAM '10D-10F:SFR'}            // Bank 2 : PMADRL, PMDATH, PMADRH
  {$SET_STATE_RAM '115-116:SFR'}            // Bank 2 : WPUB, IOCB
  {$SET_STATE_RAM '185-187:SFR'}            // Bank 3 : ANSELA, ANSELB, ANSELC
  {$SET_STATE_RAM '18D-18D:SFR'}            // Bank 3 : PMCON2


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '101-101:bnk0'} // maps to TMR0 (bank 0)
  {$SET_MAPPED_RAM '181-181:bnk1'} // maps to OPTION_REG (bank 1)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '005:3F'} // PORTA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '006:F0'} // PORTB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:F5'} // T1CON bits 3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:7F'} // T2CON bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '017:3F'} // CCP1CON bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:3F'} // ADCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '085:37'} // TRISA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '086:F0'} // TRISB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '08E:03'} // PCON bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '090:3C'} // OSCCON bits 7,6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '091:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '095:3F'} // WPUA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '096:3F'} // IOCA bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '098:F7'} // TXSTA bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09D:F3'} // FVRCON bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:70'} // ADCON1 bits 7,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:3F'} // PMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10F:1F'} // PMADRH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:F0'} // WPUB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:F0'} // IOCB bits 3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '185:37'} // ANSELA bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '186:30'} // ANSELB bits 7,6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '187:CF'} // ANSELC bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18C:F7'} // PMCON1 bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '18D:00'} // PMCON2 bits 7,6,5,4,3,2,1,0 un-implemented (read as 0)

  {$SET_UNIMP_BITS1 '18C:80'} // PMCON1 bit 7 un-implemented (read as 1)


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : RA5/T1CKI/CLKIN
  // Pin  3 : RA4/AN3/T1G/CLKOUT
  // Pin  4 : RA3/MCLR/Vpp
  // Pin  5 : RC5/CCP1
  // Pin  6 : RC4
  // Pin  7 : RC3/AN7
  // Pin  8 : RC6/AN8/SS
  // Pin  9 : RC7/AN9/SDO
  // Pin 10 : RB7/TX/CK
  // Pin 11 : RB6/SCK/SCL
  // Pin 12 : RB5/AN11/RX/DT
  // Pin 13 : RB4/AN10/SDI/SDA
  // Pin 14 : RC2/AN6
  // Pin 15 : RC1/AN5
  // Pin 16 : RC0/AN4
  // Pin 17 : RA2/AN2/T0CKI/INT
  // Pin 18 : RA1/AN1/ICSPCLK/ICDCLK
  // Pin 19 : RA0/AN0/ICSPDAT/ICDDAT
  // Pin 20 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-19,1-18,2-17,3-4,4-3,5-2'} // PORTA
  {$MAP_RAM_TO_PIN '006:4-13,5-12,6-11,7-10'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // FOSC : Oscillator Selection bits
  {$define _FOSC_ECCLK     = $3FFF}  // EC oscillator: CLKO function on RA4/CLKO pin, CLKI on RA5/CLKI
  {$define _FOSC_ECIO      = $3FFE}  // EC oscillator: I/O function on RA4/CLKO pin, CLKI on RA5/CLKI
  {$define _FOSC_INTOSCCLK = $3FFD}  // INTOSC oscillator: CLKO function on RA4/CLKO pin, I/O function on RA5/CLKI
  {$define _FOSC_INTOSCIO  = $3FFC}  // INTOSCIO oscillator: I/O function on RA4/CLKO pin, I/O function on RA5/CLKI

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_OFF       = $3FF7}  // WDT disabled
  {$define _WDTE_ON        = $3FFF}  // WDT enabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_ON       = $3FEF}  // PWRT enabled
  {$define _PWRTE_OFF      = $3FFF}  // PWRT disabled

  // MCLRE : RA3/MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON       = $3FFF}  // RA3/MCLR/VPP pin function is MCLR; Weak pull-up enabled.
  {$define _MCLRE_OFF      = $3FDF}  // RA3/MCLR/VPP pin function is digital input; MCLR internally disabled; Weak pull-up disabled

  // CP : Flash Program Memory Code Protection bit
  {$define _CP_ON          = $3FBF}  // 0000h to 07FFh code protection on
  {$define _CP_OFF         = $3FFF}  // Code protection off

  // BOREN : Brown-out Reset Enable bits
  {$define _BOREN_OFF      = $3CFF}  // Brown-out Reset disabled (Preconditioned State)
  {$define _BOREN_NSLEEP   = $3EFF}  // Brown-out Reset enabled during operation and disabled in Sleep
  {$define _BOREN_ON       = $3FFF}  // Brown-out Reset enabled

  // PLLEN : INTOSC PLLEN Enable Bit
  {$define _PLLEN_OFF      = $2FFF}  // INTOSC Frequency is 500 kHz
  {$define _PLLEN_ON       = $3FFF}  // INTOSC Frequency is 16 MHz (32x)

  // WRTEN : Flash memory self-write protection bits
  {$define _WRTEN_OFF      = $3FFF}  // Write protection off
  {$define _WRTEN_BOOT     = $3FFE}  // 0h to 1FFh of flash memory write protected, 200h to FFFh may be modified
  {$define _WRTEN_HALF     = $3FFD}  // 0h to 7FFh of flash memory write protected, 800h to FFFh may be modified
  {$define _WRTEN_FULL     = $3FFC}  // 0h to FFFh of flash memory write protected, no address may be modified

implementation
end.
