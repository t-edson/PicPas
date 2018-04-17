unit PIC12F752;

// Define hardware
{$SET PIC_MODEL    = 'PIC12F752'}
{$SET PIC_MAXFREQ  = 20000000}
{$SET PIC_NPINS    = 8}
{$SET PIC_NUMBANKS = 4}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}

interface
var
  INDF               : byte absolute $0000;
  TMR0               : byte absolute $0001;
  PCL                : byte absolute $0002;
  STATUS             : byte absolute $0003;
  STATUS_IRP         : bit  absolute STATUS.7;
  STATUS_RP1         : bit  absolute STATUS.6;
  STATUS_RP0         : bit  absolute STATUS.5;
  STATUS_TO          : bit  absolute STATUS.4;
  STATUS_PD          : bit  absolute STATUS.3;
  STATUS_Z           : bit  absolute STATUS.2;
  STATUS_DC          : bit  absolute STATUS.1;
  STATUS_C           : bit  absolute STATUS.0;
  FSR                : byte absolute $0004;
  PORTA              : byte absolute $0005;
  PORTA_RA5          : bit  absolute PORTA.5;
  PORTA_RA4          : bit  absolute PORTA.4;
  PORTA_RA3          : bit  absolute PORTA.3;
  PORTA_RA2          : bit  absolute PORTA.2;
  PORTA_RA1          : bit  absolute PORTA.1;
  PORTA_RA0          : bit  absolute PORTA.0;
  IOCAF              : byte absolute $0008;
  IOCAF_IOCAF5       : bit  absolute IOCAF.5;
  IOCAF_IOCAF4       : bit  absolute IOCAF.4;
  IOCAF_IOCAF3       : bit  absolute IOCAF.3;
  IOCAF_IOCAF2       : bit  absolute IOCAF.2;
  IOCAF_IOCAF1       : bit  absolute IOCAF.1;
  IOCAF_IOCAF0       : bit  absolute IOCAF.0;
  PCLATH             : byte absolute $000a;
  INTCON             : byte absolute $000b;
  INTCON_GIE         : bit  absolute INTCON.7;
  INTCON_PEIE        : bit  absolute INTCON.6;
  INTCON_T0IE        : bit  absolute INTCON.5;
  INTCON_INTE        : bit  absolute INTCON.4;
  INTCON_IOCIE       : bit  absolute INTCON.3;
  INTCON_T0IF        : bit  absolute INTCON.2;
  INTCON_INTF        : bit  absolute INTCON.1;
  INTCON_IOCIF       : bit  absolute INTCON.0;
  PIR1               : byte absolute $000c;
  PIR1_TMR1GIF       : bit  absolute PIR1.5;
  PIR1_ADIF          : bit  absolute PIR1.4;
  PIR1_HLTMR1IF      : bit  absolute PIR1.3;
  PIR1_TMR2IF        : bit  absolute PIR1.2;
  PIR1_TMR1IF        : bit  absolute PIR1.1;
  PIR2               : byte absolute $000d;
  PIR2_C2IF          : bit  absolute PIR2.4;
  PIR2_C1IF          : bit  absolute PIR2.3;
  PIR2_COG1IF        : bit  absolute PIR2.2;
  PIR2_CCP1IF        : bit  absolute PIR2.1;
  TMR1L              : byte absolute $000f;
  TMR1H              : byte absolute $0010;
  T1CON              : byte absolute $0011;
  T1CON_TMR1CS1      : bit  absolute T1CON.7;
  T1CON_TMR1CS0      : bit  absolute T1CON.6;
  T1CON_T1CKPS1      : bit  absolute T1CON.5;
  T1CON_T1CKPS0      : bit  absolute T1CON.4;
  T1CON_reserved     : bit  absolute T1CON.3;
  T1CON_T1SYNC       : bit  absolute T1CON.2;
  T1CON_TMR1ON       : bit  absolute T1CON.1;
  T1GCON             : byte absolute $0012;
  T1GCON_TMR1GE      : bit  absolute T1GCON.7;
  T1GCON_T1GPOL      : bit  absolute T1GCON.6;
  T1GCON_T1GTM       : bit  absolute T1GCON.5;
  T1GCON_T1GSPM      : bit  absolute T1GCON.4;
  T1GCON_T1GGO_nDONE : bit  absolute T1GCON.3;
  T1GCON_T1GVAL      : bit  absolute T1GCON.2;
  T1GCON_T1GSS0      : bit  absolute T1GCON.1;
  CCPR1L             : byte absolute $0013;
  CCPR1H             : byte absolute $0014;
  CCP1CON            : byte absolute $0015;
  CCP1CON_DC1B1      : bit  absolute CCP1CON.5;
  CCP1CON_DC1B0      : bit  absolute CCP1CON.4;
  CCP1CON_CCP1M3     : bit  absolute CCP1CON.3;
  CCP1CON_CCP1M2     : bit  absolute CCP1CON.2;
  CCP1CON_CCP1M1     : bit  absolute CCP1CON.1;
  CCP1CON_CCP1M0     : bit  absolute CCP1CON.0;
  ADRESL             : byte absolute $001c;
  ADRESH             : byte absolute $001d;
  ADCON0             : byte absolute $001e;
  ADCON0_ADFM        : bit  absolute ADCON0.7;
  ADCON0_VCFG        : bit  absolute ADCON0.6;
  ADCON0_CHS3        : bit  absolute ADCON0.4;
  ADCON0_CHS2        : bit  absolute ADCON0.3;
  ADCON0_CHS1        : bit  absolute ADCON0.2;
  ADCON0_GO_nDONE    : bit  absolute ADCON0.1;
  ADCON0_ADON        : bit  absolute ADCON0.0;
  ADCON1             : byte absolute $001f;
  ADCON1_ADCS2       : bit  absolute ADCON1.6;
  ADCON1_ADCS1       : bit  absolute ADCON1.5;
  ADCON1_ADCS0       : bit  absolute ADCON1.4;
  OPTION_REG         : byte absolute $0081;
  OPTION_REG_RAPU    : bit  absolute OPTION_REG.7;
  OPTION_REG_INTEDG  : bit  absolute OPTION_REG.6;
  OPTION_REG_T0CS    : bit  absolute OPTION_REG.5;
  OPTION_REG_T0SE    : bit  absolute OPTION_REG.4;
  OPTION_REG_PSA     : bit  absolute OPTION_REG.3;
  OPTION_REG_PS2     : bit  absolute OPTION_REG.2;
  OPTION_REG_PS1     : bit  absolute OPTION_REG.1;
  OPTION_REG_PS0     : bit  absolute OPTION_REG.0;
  TRISA              : byte absolute $0085;
  TRISA_TRISA5       : bit  absolute TRISA.5;
  TRISA_TRISA4       : bit  absolute TRISA.4;
  TRISA_TRISA3       : bit  absolute TRISA.3;
  TRISA_TRISA2       : bit  absolute TRISA.2;
  TRISA_TRISA1       : bit  absolute TRISA.1;
  TRISA_TRISA0       : bit  absolute TRISA.0;
  IOCAP              : byte absolute $0088;
  IOCAP_IOCAP5       : bit  absolute IOCAP.5;
  IOCAP_IOCAP4       : bit  absolute IOCAP.4;
  IOCAP_IOCAP3       : bit  absolute IOCAP.3;
  IOCAP_IOCAP2       : bit  absolute IOCAP.2;
  IOCAP_IOCAP1       : bit  absolute IOCAP.1;
  IOCAP_IOCAP0       : bit  absolute IOCAP.0;
  PIE1               : byte absolute $008c;
  PIE1_TMR1GIE       : bit  absolute PIE1.5;
  PIE1_ADIE          : bit  absolute PIE1.4;
  PIE1_HLTMR1IE      : bit  absolute PIE1.3;
  PIE1_TMR2IE        : bit  absolute PIE1.2;
  PIE1_TMR1IE        : bit  absolute PIE1.1;
  PIE2               : byte absolute $008d;
  PIE2_C2IE          : bit  absolute PIE2.4;
  PIE2_C1IE          : bit  absolute PIE2.3;
  PIE2_COG1IE        : bit  absolute PIE2.2;
  PIE2_CCP1IE        : bit  absolute PIE2.1;
  OSCCON             : byte absolute $008f;
  OSCCON_IRCF1       : bit  absolute OSCCON.5;
  OSCCON_IRCF0       : bit  absolute OSCCON.4;
  OSCCON_HTS         : bit  absolute OSCCON.2;
  OSCCON_LTS         : bit  absolute OSCCON.1;
  FVRCON             : byte absolute $0090;
  FVRCON_FVREN       : bit  absolute FVRCON.7;
  FVRCON_FVRRDY      : bit  absolute FVRCON.6;
  FVRCON_FVRBUFEN    : bit  absolute FVRCON.5;
  FVRCON_FVRBUFSS    : bit  absolute FVRCON.4;
  DACCON0            : byte absolute $0091;
  DACCON0_DACEN      : bit  absolute DACCON0.5;
  DACCON0_DACRNG     : bit  absolute DACCON0.4;
  DACCON0_DACOE      : bit  absolute DACCON0.3;
  DACCON0_DACPSS0    : bit  absolute DACCON0.2;
  DACCON1            : byte absolute $0092;
  DACCON1_DACR4      : bit  absolute DACCON1.4;
  DACCON1_DACR3      : bit  absolute DACCON1.3;
  DACCON1_DACR2      : bit  absolute DACCON1.2;
  DACCON1_DACR1      : bit  absolute DACCON1.1;
  DACCON1_DACR0      : bit  absolute DACCON1.0;
  CM2CON0            : byte absolute $009b;
  CM2CON0_C2ON       : bit  absolute CM2CON0.7;
  CM2CON0_C2OUT      : bit  absolute CM2CON0.6;
  CM2CON0_C2OE       : bit  absolute CM2CON0.5;
  CM2CON0_C2POL      : bit  absolute CM2CON0.4;
  CM2CON0_C2ZLF      : bit  absolute CM2CON0.3;
  CM2CON0_C2SP       : bit  absolute CM2CON0.2;
  CM2CON0_C2HYS      : bit  absolute CM2CON0.1;
  CM2CON0_C2SYNC     : bit  absolute CM2CON0.0;
  CM2CON1            : byte absolute $009c;
  CM2CON1_C2INTP     : bit  absolute CM2CON1.7;
  CM2CON1_C2INTN     : bit  absolute CM2CON1.6;
  CM2CON1_C2PCH1     : bit  absolute CM2CON1.5;
  CM2CON1_C2PCH0     : bit  absolute CM2CON1.4;
  CM2CON1_C2NCH0     : bit  absolute CM2CON1.3;
  CM1CON0            : byte absolute $009d;
  CM1CON0_C1ON       : bit  absolute CM1CON0.7;
  CM1CON0_C1OUT      : bit  absolute CM1CON0.6;
  CM1CON0_C1OE       : bit  absolute CM1CON0.5;
  CM1CON0_C1POL      : bit  absolute CM1CON0.4;
  CM1CON0_C1ZLF      : bit  absolute CM1CON0.3;
  CM1CON0_C1SP       : bit  absolute CM1CON0.2;
  CM1CON0_C1HYS      : bit  absolute CM1CON0.1;
  CM1CON0_C1SYNC     : bit  absolute CM1CON0.0;
  CM1CON1            : byte absolute $009e;
  CM1CON1_C1INTP     : bit  absolute CM1CON1.7;
  CM1CON1_C1INTN     : bit  absolute CM1CON1.6;
  CM1CON1_C1PCH1     : bit  absolute CM1CON1.5;
  CM1CON1_C1PCH0     : bit  absolute CM1CON1.4;
  CM1CON1_C1NCH0     : bit  absolute CM1CON1.3;
  CMOUT              : byte absolute $009f;
  CMOUT_MCOUT2       : bit  absolute CMOUT.1;
  CMOUT_MCOUT1       : bit  absolute CMOUT.0;
  LATA               : byte absolute $0105;
  LATA_LATA5         : bit  absolute LATA.5;
  LATA_LATA4         : bit  absolute LATA.4;
  LATA_LATA2         : bit  absolute LATA.3;
  LATA_LATA1         : bit  absolute LATA.2;
  LATA_LATA0         : bit  absolute LATA.1;
  IOCAN              : byte absolute $0108;
  IOCAN_IOCAN5       : bit  absolute IOCAN.5;
  IOCAN_IOCAN4       : bit  absolute IOCAN.4;
  IOCAN_IOCAN3       : bit  absolute IOCAN.3;
  IOCAN_IOCAN2       : bit  absolute IOCAN.2;
  IOCAN_IOCAN1       : bit  absolute IOCAN.1;
  IOCAN_IOCAN0       : bit  absolute IOCAN.0;
  WPUA               : byte absolute $010c;
  WPUA_WPUA5         : bit  absolute WPUA.5;
  WPUA_WPUA4         : bit  absolute WPUA.4;
  WPUA_WPUA3         : bit  absolute WPUA.3;
  WPUA_WPUA2         : bit  absolute WPUA.2;
  WPUA_WPUA1         : bit  absolute WPUA.1;
  WPUA_WPUA0         : bit  absolute WPUA.0;
  SLRCONA            : byte absolute $010d;
  SLRCONA_SLRA2      : bit  absolute SLRCONA.2;
  SLRCONA_SLRA0      : bit  absolute SLRCONA.1;
  PCON               : byte absolute $010f;
  PCON_POR           : bit  absolute PCON.1;
  PCON_BOR           : bit  absolute PCON.0;
  TMR2               : byte absolute $0110;
  PR2                : byte absolute $0111;
  T2CON              : byte absolute $0112;
  T2CON_T2OUTPS3     : bit  absolute T2CON.6;
  T2CON_T2OUTPS2     : bit  absolute T2CON.5;
  T2CON_T2OUTPS1     : bit  absolute T2CON.4;
  T2CON_T2OUTPS0     : bit  absolute T2CON.3;
  T2CON_TMR2ON       : bit  absolute T2CON.2;
  T2CON_T2CKPS0      : bit  absolute T2CON.1;
  HLTMR1             : byte absolute $0113;
  HLTPR1             : byte absolute $0114;
  HLT1CON0           : byte absolute $0115;
  HLT1CON0_H1OUTPS3  : bit  absolute HLT1CON0.6;
  HLT1CON0_H1OUTPS2  : bit  absolute HLT1CON0.5;
  HLT1CON0_H1OUTPS1  : bit  absolute HLT1CON0.4;
  HLT1CON0_H1OUTPS0  : bit  absolute HLT1CON0.3;
  HLT1CON0_H1ON      : bit  absolute HLT1CON0.2;
  HLT1CON0_H1CKPS0   : bit  absolute HLT1CON0.1;
  HLT1CON1           : byte absolute $0116;
  HLT1CON1_H1ERS2    : bit  absolute HLT1CON1.4;
  HLT1CON1_H1ERS1    : bit  absolute HLT1CON1.3;
  HLT1CON1_H1ERS0    : bit  absolute HLT1CON1.2;
  HLT1CON1_H1FEREN   : bit  absolute HLT1CON1.1;
  HLT1CON1_H1REREN   : bit  absolute HLT1CON1.0;
  ANSELA             : byte absolute $0185;
  ANSELA_ANSA5       : bit  absolute ANSELA.5;
  ANSELA_ANSA4       : bit  absolute ANSELA.4;
  ANSELA_ANSA2       : bit  absolute ANSELA.3;
  ANSELA_ANSA1       : bit  absolute ANSELA.2;
  ANSELA_ANSA0       : bit  absolute ANSELA.1;
  APFCON             : byte absolute $0188;
  APFCON_T1GSEL      : bit  absolute APFCON.4;
  APFCON_COG1FSEL    : bit  absolute APFCON.3;
  APFCON_COG1O1SEL   : bit  absolute APFCON.2;
  APFCON_COG1O0SEL   : bit  absolute APFCON.1;
  OSCTUNE            : byte absolute $0189;
  OSCTUNE_TUN4       : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3       : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2       : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1       : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0       : bit  absolute OSCTUNE.0;
  PMCON1             : byte absolute $018c;
  PMCON1_WREN        : bit  absolute PMCON1.3;
  PMCON1_WR          : bit  absolute PMCON1.2;
  PMCON1_RD          : bit  absolute PMCON1.1;
  PMCON2             : byte absolute $018d;
  PMADRL             : byte absolute $018e;
  PMADRH             : byte absolute $018f;
  PMDATL             : byte absolute $0190;
  PMDATH             : byte absolute $0191;
  COG1PH             : byte absolute $0192;
  COG1PH_G1PH3       : bit  absolute COG1PH.3;
  COG1PH_G1PH2       : bit  absolute COG1PH.2;
  COG1PH_G1PH1       : bit  absolute COG1PH.1;
  COG1PH_G1PH0       : bit  absolute COG1PH.0;
  COG1BLK            : byte absolute $0193;
  COG1BLK_G1BLKR3    : bit  absolute COG1BLK.7;
  COG1BLK_G1BLKR2    : bit  absolute COG1BLK.6;
  COG1BLK_G1BLKR1    : bit  absolute COG1BLK.5;
  COG1BLK_G1BLKR0    : bit  absolute COG1BLK.4;
  COG1BLK_G1BLKF3    : bit  absolute COG1BLK.3;
  COG1BLK_G1BLKF2    : bit  absolute COG1BLK.2;
  COG1BLK_G1BLKF1    : bit  absolute COG1BLK.1;
  COG1BLK_G1BLKF0    : bit  absolute COG1BLK.0;
  COG1DB             : byte absolute $0194;
  COG1DB_G1BDR3      : bit  absolute COG1DB.7;
  COG1DB_G1BDR2      : bit  absolute COG1DB.6;
  COG1DB_G1BDR1      : bit  absolute COG1DB.5;
  COG1DB_G1BDR0      : bit  absolute COG1DB.4;
  COG1DB_G1DBF3      : bit  absolute COG1DB.3;
  COG1DB_G1DBF2      : bit  absolute COG1DB.2;
  COG1DB_G1DBF1      : bit  absolute COG1DB.1;
  COG1DB_G1DBF0      : bit  absolute COG1DB.0;
  COG1CON0           : byte absolute $0195;
  COG1CON0_G1EN      : bit  absolute COG1CON0.7;
  COG1CON0_G1OE1     : bit  absolute COG1CON0.6;
  COG1CON0_G1OE0     : bit  absolute COG1CON0.5;
  COG1CON0_G1POL1    : bit  absolute COG1CON0.4;
  COG1CON0_G1POL0    : bit  absolute COG1CON0.3;
  COG1CON0_G1LD      : bit  absolute COG1CON0.2;
  COG1CON0_G1CS0     : bit  absolute COG1CON0.1;
  COG1CON1           : byte absolute $0196;
  COG1CON1_G1FSIM    : bit  absolute COG1CON1.7;
  COG1CON1_G1RSIM    : bit  absolute COG1CON1.6;
  COG1CON1_G1FS2     : bit  absolute COG1CON1.5;
  COG1CON1_G1FS1     : bit  absolute COG1CON1.4;
  COG1CON1_G1FS0     : bit  absolute COG1CON1.3;
  COG1CON1_G1RS2     : bit  absolute COG1CON1.2;
  COG1CON1_G1RS1     : bit  absolute COG1CON1.1;
  COG1CON1_G1RS0     : bit  absolute COG1CON1.0;
  COG1ASD            : byte absolute $0197;
  COG1ASD_G1ASDE     : bit  absolute COG1ASD.7;
  COG1ASD_G1ARSEN    : bit  absolute COG1ASD.6;
  COG1ASD_G1ASDL1    : bit  absolute COG1ASD.5;
  COG1ASD_G1ASDL0    : bit  absolute COG1ASD.4;
  COG1ASD_G1ASDSHLT  : bit  absolute COG1ASD.3;
  COG1ASD_G1ASDSC2   : bit  absolute COG1ASD.2;
  COG1ASD_G1ASDSC1   : bit  absolute COG1ASD.1;
  COG1ASD_G1ASDSFLT  : bit  absolute COG1ASD.0;


  // -- Define RAM state values --

  {$SET_STATE_RAM '000-005:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, PORTA
  {$SET_STATE_RAM '008-008:SFR'}  // IOCAF
  {$SET_STATE_RAM '00A-00D:SFR'}  // PCLATH, INTCON, PIR1, PIR2
  {$SET_STATE_RAM '00F-015:SFR'}  // TMR1L, TMR1H, T1CON, T1GCON, CCPR1L, CCPR1H, CCP1CON
  {$SET_STATE_RAM '01C-01F:SFR'}  // ADRESL, ADRESH, ADCON0, ADCON1
  {$SET_STATE_RAM '040-07F:GPR'} 
  {$SET_STATE_RAM '080-085:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, TRISA
  {$SET_STATE_RAM '088-088:SFR'}  // IOCAP
  {$SET_STATE_RAM '08A-08D:SFR'}  // PCLATH, INTCON, PIE1, PIE2
  {$SET_STATE_RAM '08F-092:SFR'}  // OSCCON, FVRCON, DACCON0, DACCON1
  {$SET_STATE_RAM '09B-09F:SFR'}  // CM2CON0, CM2CON1, CM1CON0, CM1CON1, CMOUT
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-105:SFR'}  // INDF, TMR0, PCL, STATUS, FSR, LATA
  {$SET_STATE_RAM '108-108:SFR'}  // IOCAN
  {$SET_STATE_RAM '10A-10D:SFR'}  // PCLATH, INTCON, WPUA, SLRCONA
  {$SET_STATE_RAM '10F-116:SFR'}  // PCON, TMR2, PR2, T2CON, HLTMR1, HLTPR1, HLT1CON0, HLT1CON1
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-185:SFR'}  // INDF, OPTION_REG, PCL, STATUS, FSR, ANSELA
  {$SET_STATE_RAM '188-197:SFR'}  // APFCON, OSCTUNE, PCLATH, INTCON, PMCON1, PMCON2, PMADRL, PMADRH, PMDATL, PMDATH, COG1PH, COG1BLK, COG1DB, COG1CON0, COG1CON1, COG1ASD
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
  {$SET_UNIMP_BITS '005:3F'} // PORTA
  {$SET_UNIMP_BITS '008:3F'} // IOCAF
  {$SET_UNIMP_BITS '00A:1F'} // PCLATH
  {$SET_UNIMP_BITS '00C:C7'} // PIR1
  {$SET_UNIMP_BITS '00D:35'} // PIR2
  {$SET_UNIMP_BITS '011:F0'} // T1CON
  {$SET_UNIMP_BITS '012:03'} // T1GCON
  {$SET_UNIMP_BITS '015:3F'} // CCP1CON
  {$SET_UNIMP_BITS '01F:70'} // ADCON1
  {$SET_UNIMP_BITS '085:3F'} // TRISA
  {$SET_UNIMP_BITS '088:3F'} // IOCAP
  {$SET_UNIMP_BITS '08C:C7'} // PIE1
  {$SET_UNIMP_BITS '08D:35'} // PIE2
  {$SET_UNIMP_BITS '08F:36'} // OSCCON
  {$SET_UNIMP_BITS '090:F0'} // FVRCON
  {$SET_UNIMP_BITS '091:E4'} // DACCON0
  {$SET_UNIMP_BITS '092:1F'} // DACCON1
  {$SET_UNIMP_BITS '09C:F1'} // CM2CON1
  {$SET_UNIMP_BITS '09E:F1'} // CM1CON1
  {$SET_UNIMP_BITS '09F:03'} // CMOUT
  {$SET_UNIMP_BITS '105:37'} // LATA
  {$SET_UNIMP_BITS '108:3F'} // IOCAN
  {$SET_UNIMP_BITS '10C:3F'} // WPUA
  {$SET_UNIMP_BITS '10D:05'} // SLRCONA
  {$SET_UNIMP_BITS '10F:03'} // PCON
  {$SET_UNIMP_BITS '111:00'} // PR2
  {$SET_UNIMP_BITS '112:7B'} // T2CON
  {$SET_UNIMP_BITS '114:00'} // HLTPR1
  {$SET_UNIMP_BITS '115:7B'} // HLT1CON0
  {$SET_UNIMP_BITS '116:1F'} // HLT1CON1
  {$SET_UNIMP_BITS '185:37'} // ANSELA
  {$SET_UNIMP_BITS '188:17'} // APFCON
  {$SET_UNIMP_BITS '189:1F'} // OSCTUNE
  {$SET_UNIMP_BITS '18C:07'} // PMCON1
  {$SET_UNIMP_BITS '18F:03'} // PMADRH
  {$SET_UNIMP_BITS '191:3F'} // PMDATH
  {$SET_UNIMP_BITS '192:0F'} // COG1PH


  // -- PIN mapping --

  // Pin  1 : Vdd
  // Pin  2 : COG1OUT0/IOC/CLKIN/T1CKI/C2IN1-/RA5
  // Pin  3 : C1IN1-/CLKOUT/COG1OUT1/COG1FLT/IOC/T1G/AN3/RA4
  // Pin  4 : MCLR/Vpp/COG1FLT/T1G/IOC/RA3
  // Pin  5 : COG1OUT0/INT/IOC/CCP1/T0CKI/C2OUT/C1OUT/AN2/RA2
  // Pin  6 : ICSPCLK/VREF+/IOC/C2IN0-/C1IN0-/AN1/RA1
  // Pin  7 : ICSPDAT/REFOUT/DACOUT/COG1OUT1/IOC/C2IN0+/C1IN0+/AN0/RA0
  // Pin  8 : Vss


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '005:0-7,1-6,2-5,3-4,4-3,5-2'} // PORTA


  // -- Bits Configuration --

  // DEBUG : Debug Mode Enable bit
  {$define _DEBUG_OFF       = $3F79}  // Debug mode disabled
  {$define _DEBUG_ON        = $3F78}  // Debug mode enabled

  // CLKOUTEN : Clock Out Enable bit
  {$define _CLKOUTEN_OFF    = $3F7B}  // CLKOUT function disabled.  CLKOUT pin acts as I/O
  {$define _CLKOUTEN_ON     = $3F79}  // CLKOUT function enabled.  CLKOUT pin is CLKOUT

  // WRT : Flash Program Memory Self Write Enable bit
  {$define _WRT_OFF         = $3F7D}  // Flash self-write protection off
  {$define _WRT_FOURTH      = $3F79}  // 000h to 0FFh self-write protected
  {$define _WRT_HALF        = $3F75}  // 000h to 1FFh self-write protected
  {$define _WRT_ALL         = $3F71}  // 000h to 3FFh self-write protected

  // BOREN : Brown-out Reset Enable bits
  {$define _BOREN_EN        = $3F79}  // BOR enabled
  {$define _BOREN_SLEEP_DIS = $3F69}  // BOR enabled during operation and disabled in Sleep
  {$define _BOREN_DIS       = $3F49}  // BOR disabled

  // CP : Code Protection bit
  {$define _CP_OFF          = $3F79}  // Program memory code protection is disabled
  {$define _CP_ON           = $3F39}  // Program memory code protection is enabled

  // MCLRE : MCLR/VPP Pin Function Select bit
  {$define _MCLRE_ON        = $3FF9}  // MCLR pin is MCLR function with internal weak pullup
  {$define _MCLRE_OFF       = $3F79}  // MCLR pin is alternate function

  // PWRTE : Power-up Timer Enable bit
  {$define _PWRTE_OFF       = $3F79}  // Power-up Timer disabled
  {$define _PWRTE_ON        = $3E79}  // Power-up Timer enabled

  // WDTE : Watchdog Timer Enable bit
  {$define _WDTE_ON         = $3F79}  // Watchdog Timer enabled
  {$define _WDTE_OFF        = $3D79}  // Watchdog Timer disabled

  // FOSC0 : FOSC: Oscillator Selection bit
  {$define _FOSC0_EC        = $3F79}  // EC oscillator mode.  CLKIN function on RA5/CLKIN
  {$define _FOSC0_INT       = $3B79}  // Internal oscillator mode.  I/O function on RA5/CLKIN

implementation
end.
