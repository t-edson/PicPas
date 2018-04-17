unit PIC16F721;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F721'}
{$SET PIC_MAXFREQ  = 16000000}
{$SET PIC_NPINS    = 20}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 2}
{$SET PIC_MAXFLASH = 4096}

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
  PORTA_RA5         : bit  absolute PORTA.6;
  PORTA_RA4         : bit  absolute PORTA.5;
  PORTA_RA3         : bit  absolute PORTA.4;
  PORTA_RA2         : bit  absolute PORTA.3;
  PORTA_RA1         : bit  absolute PORTA.2;
  PORTA_RA0         : bit  absolute PORTA.1;
  PORTB             : byte absolute $0006;
  PORTB_RB7         : bit  absolute PORTB.4;
  PORTB_RB6         : bit  absolute PORTB.3;
  PORTB_RB5         : bit  absolute PORTB.2;
  PORTB_RB4         : bit  absolute PORTB.1;
  PORTC             : byte absolute $0007;
  PORTC_RC7         : bit  absolute PORTC.7;
  PORTC_RC6         : bit  absolute PORTC.6;
  PORTC_RC5         : bit  absolute PORTC.5;
  PORTC_RC4         : bit  absolute PORTC.4;
  PORTC_RC3         : bit  absolute PORTC.3;
  PORTC_RC2         : bit  absolute PORTC.2;
  PORTC_RC1         : bit  absolute PORTC.1;
  PORTC_RC0         : bit  absolute PORTC.0;
  PCLATH            : byte absolute $000a;
  INTCON            : byte absolute $000b;
  INTCON_GIE        : bit  absolute INTCON.7;
  INTCON_PEIE       : bit  absolute INTCON.6;
  INTCON_TMR0IE     : bit  absolute INTCON.5;
  INTCON_INTE       : bit  absolute INTCON.4;
  INTCON_RABIE      : bit  absolute INTCON.3;
  INTCON_TMR0IF     : bit  absolute INTCON.2;
  INTCON_INTF       : bit  absolute INTCON.1;
  INTCON_RABIF      : bit  absolute INTCON.0;
  PIR1              : byte absolute $000c;
  PIR1_TMR1GIF      : bit  absolute PIR1.7;
  PIR1_ADIF         : bit  absolute PIR1.6;
  PIR1_RCIF         : bit  absolute PIR1.5;
  PIR1_TXIF         : bit  absolute PIR1.4;
  PIR1_SSPIF        : bit  absolute PIR1.3;
  PIR1_CCP1IF       : bit  absolute PIR1.2;
  PIR1_TMR2IF       : bit  absolute PIR1.1;
  PIR1_TMR1IF       : bit  absolute PIR1.0;
  TMR1L             : byte absolute $000e;
  TMR1H             : byte absolute $000f;
  T1CON             : byte absolute $0010;
  T1CON_TMR1CS1     : bit  absolute T1CON.6;
  T1CON_TMR1CS0     : bit  absolute T1CON.5;
  T1CON_T1CKPS1     : bit  absolute T1CON.4;
  T1CON_T1CKPS0     : bit  absolute T1CON.3;
  T1CON_T1SYNC      : bit  absolute T1CON.2;
  T1CON_TMR1ON      : bit  absolute T1CON.1;
  TMR2              : byte absolute $0011;
  T2CON             : byte absolute $0012;
  T2CON_TOUTPS3     : bit  absolute T2CON.6;
  T2CON_TOUTPS2     : bit  absolute T2CON.5;
  T2CON_TOUTPS1     : bit  absolute T2CON.4;
  T2CON_TOUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON      : bit  absolute T2CON.2;
  T2CON_T2CKPS0     : bit  absolute T2CON.1;
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
  CCP1CON_DC1       : bit  absolute CCP1CON.7;
  CCP1CON_B1        : bit  absolute CCP1CON.6;
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
  RCSTA_ADDEN       : bit  absolute RCSTA.3;
  RCSTA_FERR        : bit  absolute RCSTA.2;
  RCSTA_OERR        : bit  absolute RCSTA.1;
  RCSTA_RX9D        : bit  absolute RCSTA.0;
  TXREG             : byte absolute $0019;
  RCREG             : byte absolute $001a;
  ADRES             : byte absolute $001e;
  ADCON0            : byte absolute $001f;
  ADCON0_CHS3       : bit  absolute ADCON0.5;
  ADCON0_CHS2       : bit  absolute ADCON0.4;
  ADCON0_CHS1       : bit  absolute ADCON0.3;
  ADCON0_CHS0       : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE   : bit  absolute ADCON0.1;
  ADCON0_ADON       : bit  absolute ADCON0.0;
  OPTION_REG        : byte absolute $0081;
  OPTION_REG_RABPU  : bit  absolute OPTION_REG.7;
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
  TRISA_TRISA2      : bit  absolute TRISA.3;
  TRISA_TRISA1      : bit  absolute TRISA.2;
  TRISA_TRISA0      : bit  absolute TRISA.1;
  TRISB             : byte absolute $0086;
  TRISB_TRISB7      : bit  absolute TRISB.4;
  TRISB_TRISB6      : bit  absolute TRISB.3;
  TRISB_TRISB5      : bit  absolute TRISB.2;
  TRISB_TRISB4      : bit  absolute TRISB.1;
  TRISC             : byte absolute $0087;
  TRISC_TRISC7      : bit  absolute TRISC.7;
  TRISC_TRISC6      : bit  absolute TRISC.6;
  TRISC_TRISC5      : bit  absolute TRISC.5;
  TRISC_TRISC4      : bit  absolute TRISC.4;
  TRISC_TRISC3      : bit  absolute TRISC.3;
  TRISC_TRISC2      : bit  absolute TRISC.2;
  TRISC_TRISC1      : bit  absolute TRISC.1;
  TRISC_TRISC0      : bit  absolute TRISC.0;
  PIE1              : byte absolute $008c;
  PIE1_TMR1GIE      : bit  absolute PIE1.7;
  PIE1_ADIE         : bit  absolute PIE1.6;
  PIE1_RCIE         : bit  absolute PIE1.5;
  PIE1_TXIE         : bit  absolute PIE1.4;
  PIE1_SSPIE        : bit  absolute PIE1.3;
  PIE1_CCP1IE       : bit  absolute PIE1.2;
  PIE1_TMR2IE       : bit  absolute PIE1.1;
  PIE1_TMR1IE       : bit  absolute PIE1.0;
  PCON              : byte absolute $008e;
  PCON_POR          : bit  absolute PCON.1;
  PCON_BOR          : bit  absolute PCON.0;
  T1GCON            : byte absolute $008f;
  T1GCON_TMR1GE     : bit  absolute T1GCON.7;
  T1GCON_T1GPOL     : bit  absolute T1GCON.6;
  T1GCON_T1GTM      : bit  absolute T1GCON.5;
  T1GCON_T1GSPM     : bit  absolute T1GCON.4;
  T1GCON_T1GGO_DONE : bit  absolute T1GCON.3;
  T1GCON_T1GVAL     : bit  absolute T1GCON.2;
  T1GCON_T1GSS0     : bit  absolute T1GCON.1;
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
  SSPADD_ADD7       : bit  absolute SSPADD.7;
  SSPADD_ADD6       : bit  absolute SSPADD.6;
  SSPADD_ADD5       : bit  absolute SSPADD.5;
  SSPADD_ADD4       : bit  absolute SSPADD.4;
  SSPADD_ADD3       : bit  absolute SSPADD.3;
  SSPADD_ADD2       : bit  absolute SSPADD.2;
  SSPADD_ADD1       : bit  absolute SSPADD.1;
  SSPADD_ADD0       : bit  absolute SSPADD.0;
  SSPMSK            : byte absolute $0093;
  SSPMSK_MSK7       : bit  absolute SSPMSK.7;
  SSPMSK_MSK6       : bit  absolute SSPMSK.6;
  SSPMSK_MSK5       : bit  absolute SSPMSK.5;
  SSPMSK_MSK4       : bit  absolute SSPMSK.4;
  SSPMSK_MSK3       : bit  absolute SSPMSK.3;
  SSPMSK_MSK2       : bit  absolute SSPMSK.2;
  SSPMSK_MSK1       : bit  absolute SSPMSK.1;
  SSPMSK_MSK0       : bit  absolute SSPMSK.0;
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
  WPUA_WPUA5        : bit  absolute WPUA.7;
  WPUA_WPUA4        : bit  absolute WPUA.6;
  WPUA_WPUA3        : bit  absolute WPUA.5;
  WPUA_WPUA2        : bit  absolute WPUA.4;
  WPUA_WPUA1        : bit  absolute WPUA.3;
  WPUA_WPUA0        : bit  absolute WPUA.2;
  IOCA              : byte absolute $0096;
  IOCA_IOCA5        : bit  absolute IOCA.7;
  IOCA_IOCA4        : bit  absolute IOCA.6;
  IOCA_IOCA3        : bit  absolute IOCA.5;
  IOCA_IOCA2        : bit  absolute IOCA.4;
  IOCA_IOCA1        : bit  absolute IOCA.3;
  IOCA_IOCA0        : bit  absolute IOCA.2;
  TXSTA             : byte absolute $0098;
  TXSTA_CSRC        : bit  absolute TXSTA.7;
  TXSTA_TX9         : bit  absolute TXSTA.6;
  TXSTA_TXEN        : bit  absolute TXSTA.5;
  TXSTA_SYNC        : bit  absolute TXSTA.4;
  TXSTA_BRGH        : bit  absolute TXSTA.3;
  TXSTA_TRMT        : bit  absolute TXSTA.2;
  TXSTA_TX9D        : bit  absolute TXSTA.1;
  SPBRG             : byte absolute $0099;
  FVRCON            : byte absolute $009d;
  FVRCON_FVRRDY     : bit  absolute FVRCON.7;
  FVRCON_FVREN      : bit  absolute FVRCON.6;
  FVRCON_TSEN       : bit  absolute FVRCON.5;
  FVRCON_TSRNG      : bit  absolute FVRCON.4;
  FVRCON_ADFVR1     : bit  absolute FVRCON.3;
  FVRCON_ADFVR0     : bit  absolute FVRCON.2;
  ADCON1            : byte absolute $009f;
  ADCON1_ADCS2      : bit  absolute ADCON1.4;
  ADCON1_ADCS1      : bit  absolute ADCON1.3;
  ADCON1_ADCS0      : bit  absolute ADCON1.2;
  PMDATL            : byte absolute $010c;
  PMADRL            : byte absolute $010d;
  PMDATH            : byte absolute $010e;
  PMADRH            : byte absolute $010f;
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
  ANSELA_ANSA2      : bit  absolute ANSELA.3;
  ANSELA_ANSA1      : bit  absolute ANSELA.2;
  ANSELA_ANSA0      : bit  absolute ANSELA.1;
  ANSELB            : byte absolute $0186;
  ANSELB_ANSB5      : bit  absolute ANSELB.5;
  ANSELB_ANSB4      : bit  absolute ANSELB.4;
  ANSELC            : byte absolute $0187;
  ANSELC_ANSC7      : bit  absolute ANSELC.7;
  ANSELC_ANSC6      : bit  absolute ANSELC.6;
  ANSELC_ANSC3      : bit  absolute ANSELC.5;
  ANSELC_ANSC2      : bit  absolute ANSELC.4;
  ANSELC_ANSC1      : bit  absolute ANSELC.3;
  ANSELC_ANSC0      : bit  absolute ANSELC.2;
  PMCON1            : byte absolute $018c;
  PMCON1_CFGS       : bit  absolute PMCON1.6;
  PMCON1_LWLO       : bit  absolute PMCON1.5;
  PMCON1_FREE       : bit  absolute PMCON1.4;
  PMCON1_WREN       : bit  absolute PMCON1.3;
  PMCON1_WR         : bit  absolute PMCON1.2;
  PMCON1_RD         : bit  absolute PMCON1.1;
  PMCON2            : byte absolute $018d;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-007:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA, PORTB, PORTC
  {$SET_STATE_RAM '00A-00C:SFR'}  // PCLATH, INTCON, PIR1
  {$SET_STATE_RAM '00E-01A:SFR'}  // TMR1L, TMR1H, T1CON, TMR2, T2CON, SSPBUF, SSPCON, CCPR1L, CCPR1H, CCP1CON, RCSTA, TXREG, RCREG
  {$SET_STATE_RAM '01E-01F:SFR'}  // ADRES, ADCON0
  {$SET_STATE_RAM '020-07F:GPR'} 
  {$SET_STATE_RAM '080-087:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA, TRISB, TRISC
  {$SET_STATE_RAM '08A-08C:SFR'}  // PCLATH, INTCON, PIE1
  {$SET_STATE_RAM '08E-096:SFR'}  // PCON, T1GCON, OSCCON, OSCTUNE, PR2, SSPMSK, SSPSTAT, WPUA, IOCA
  {$SET_STATE_RAM '098-099:SFR'}  // TXSTA, SPBRG
  {$SET_STATE_RAM '09D-09D:SFR'}  // FVRCON
  {$SET_STATE_RAM '09F-09F:SFR'}  // ADCON1
  {$SET_STATE_RAM '0A0-0FF:GPR'} 
  {$SET_STATE_RAM '100-104:SFR'}  // INDF, TMR0, PCL, STATUS, FSR
  {$SET_STATE_RAM '10A-10F:SFR'}  // PCLATH, INTCON, PMDATL, PMADRL, PMDATH, PMADRH
  {$SET_STATE_RAM '115-116:SFR'}  // WPUB, IOCB
  {$SET_STATE_RAM '120-17F:GPR'} 
  {$SET_STATE_RAM '180-187:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, ANSELA, ANSELB, ANSELC
  {$SET_STATE_RAM '18A-18D:SFR'}  // PCLATH, INTCON, PMCON1, PMCON2
  {$SET_STATE_RAM '1F0-1FF:GPR'} 


  // -- Define mirrored registers --

  {$SET_MAPPED_RAM '080-080:bnk0'} // INDF
  {$SET_MAPPED_RAM '082-084:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '08A-08B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '100-104:bnk0'} // INDF, TMR0, PCL, STATUS, FSR
  {$SET_MAPPED_RAM '10A-10B:bnk0'} // PCLATH, INTCON
  {$SET_MAPPED_RAM '180-180:bnk0'} // INDF
  {$SET_MAPPED_RAM '181-181:bnk1'} // OPTION_REG
  {$SET_MAPPED_RAM '182-184:bnk0'} // PCL, STATUS, FSR
  {$SET_MAPPED_RAM '18A-18B:bnk0'} // PCLATH, INTCON


  // -- Initial values --

  {$SET_UNIMP_BITS '000:00'} // INDF
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '010:FD'} // T1CON
  {$SET_UNIMP_BITS '012:7F'} // T2CON
  {$SET_UNIMP_BITS '01F:3F'} // ADCON0
  {$SET_UNIMP_BITS '08E:03'} // PCON
  {$SET_UNIMP_BITS '090:3C'} // OSCCON
  {$SET_UNIMP_BITS '091:3F'} // OSCTUNE
  {$SET_UNIMP_BITS '098:F7'} // TXSTA
  {$SET_UNIMP_BITS '09D:F3'} // FVRCON
  {$SET_UNIMP_BITS '09F:70'} // ADCON1
  {$SET_UNIMP_BITS '10E:3F'} // PMDATH
  {$SET_UNIMP_BITS '10F:07'} // PMADRH
  {$SET_UNIMP_BITS '115:F0'} // WPUB
  {$SET_UNIMP_BITS '116:F0'} // IOCB
  {$SET_UNIMP_BITS '185:3F'} // ANSELA
  {$SET_UNIMP_BITS '186:3F'} // ANSELB
  {$SET_UNIMP_BITS '187:00'} // ANSELC
  {$SET_UNIMP_BITS '18C:01'} // PMCON1
  {$SET_UNIMP_BITS '18D:00'} // PMCON2


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

  {$MAP_RAM_TO_PIN '005:1-19,2-18,3-17,4-4,5-3,6-2'} // PORTA
  {$MAP_RAM_TO_PIN '006:1-13,2-12,3-11,4-10'} // PORTB
  {$MAP_RAM_TO_PIN '007:0-16,1-15,2-14,3-7,4-6,5-5,6-8,7-9'} // PORTC


  // -- Bits Configuration --

  // DEBUG : Debugger Mode
  {$define _DEBUG_ON       = $337A}  // Background debugger is enabled
  {$define _DEBUG_OFF      = $337B}  // Background debugger is disabled

  // PLLEN : INTOSC PLLEN Enable Bit
  {$define _PLLEN_OFF      = $3379}  // INTOSC Frequency is 500 kHz
  {$define _PLLEN_ON       = $337B}  // INTOSC Frequency is 16 MHz (32x)

  // BOREN : Brown-out Reset Enable bits
  {$define _BOREN_OFF      = $3373}  // Brown-out Reset disabled (Preconditioned State)
  {$define _BOREN_NSLEEP   = $337B}  // Brown-out Reset enabled during operation and disabled in Sleep
  {$define _BOREN_ON       = $337F}  // Brown-out Reset enabled

  // CP : Flash Program Memory Code Protection bit
  {$define _CP_ON          = $336B}  // 0000h to 0FFFh code protection on
  {$define _CP_OFF         = $337B}  // Code protection off

  // MCLRE : RA3/MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON       = $337B}  // RA3/MCLR/VPP pin function is MCLR; Weak pull-up enabled.
  {$define _MCLRE_OFF      = $335B}  // RA3/MCLR/VPP pin function is digital input; MCLR internally disabled; Weak pull-up disabled

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_ON       = $333B}  // PWRT enabled
  {$define _PWRTE_OFF      = $337B}  // PWRT disabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_OFF       = $337B}  // WDT disabled
  {$define _WDTE_ON        = $33FB}  // WDT enabled

  // FOSC : Oscillator Selection bits
  {$define _FOSC_ECCLK     = $337B}  // EC oscillator: CLKO function on RA4/CLKO pin, CLKI on RA5/CLKI
  {$define _FOSC_ECIO      = $327B}  // EC oscillator: I/O function on RA4/CLKO pin, CLKI on RA5/CLKI
  {$define _FOSC_INTOSCCLK = $317B}  // INTOSC oscillator: CLKO function on RA4/CLKO pin, I/O function on RA5/CLKI
  {$define _FOSC_INTOSCIO  = $307B}  // INTOSCIO oscillator: I/O function on RA4/CLKO pin, I/O function on RA5/CLKI

  // VCAPEN : 
  {$define _VCAPEN_OFF     = $0013}  // All VCAP pin functions are disabled.

  // WRTEN : Flash memory self-write protection bits
  {$define _WRTEN_OFF      = $0017}  // Write protection off
  {$define _WRTEN_BOOT     = $0015}  // 0h to 1FFh of flash memory write protected, 200h to FFFh may be modified
  {$define _WRTEN_HALF     = $0013}  // 0h to 7FFh of flash memory write protected, 800h to FFFh may be modified
  {$define _WRTEN_FULL     = $0011}  // 0h to FFFh of flash memory write protected, no address may be modified

implementation
end.
