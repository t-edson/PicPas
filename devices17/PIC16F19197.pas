unit PIC16F19197;

// Define hardware
{$SET PIC_MODEL    = 'PIC16F19197'}
{$SET PIC_MAXFREQ  = 32000000}
{$SET PIC_NPINS    = 64}
{$SET PIC_NUMBANKS = 64}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 32768}

interface
var
  INDF0                  : byte absolute $0000;
  INDF1                  : byte absolute $0001;
  PCL                    : byte absolute $0002;
  STATUS                 : byte absolute $0003;
  STATUS_TO              : bit  absolute STATUS.6;
  STATUS_PD              : bit  absolute STATUS.5;
  STATUS_Z               : bit  absolute STATUS.4;
  STATUS_DC              : bit  absolute STATUS.3;
  STATUS_C               : bit  absolute STATUS.2;
  FSR0L                  : byte absolute $0004;
  FSR0H                  : byte absolute $0005;
  FSR1L                  : byte absolute $0006;
  FSR1H                  : byte absolute $0007;
  BSR                    : byte absolute $0008;
  BSR_BSR5               : bit  absolute BSR.5;
  BSR_BSR4               : bit  absolute BSR.4;
  BSR_BSR3               : bit  absolute BSR.3;
  BSR_BSR2               : bit  absolute BSR.2;
  BSR_BSR1               : bit  absolute BSR.1;
  BSR_BSR0               : bit  absolute BSR.0;
  WREG                   : byte absolute $0009;
  PCLATH                 : byte absolute $000A;
  INTCON                 : byte absolute $000B;
  INTCON_GIE             : bit  absolute INTCON.7;
  INTCON_PEIE            : bit  absolute INTCON.6;
  INTCON_INTEDG          : bit  absolute INTCON.5;
  PORTA                  : byte absolute $000C;
  PORTA_RA7              : bit  absolute PORTA.7;
  PORTA_RA6              : bit  absolute PORTA.6;
  PORTA_RA5              : bit  absolute PORTA.5;
  PORTA_RA4              : bit  absolute PORTA.4;
  PORTA_RA3              : bit  absolute PORTA.3;
  PORTA_RA2              : bit  absolute PORTA.2;
  PORTA_RA1              : bit  absolute PORTA.1;
  PORTA_RA0              : bit  absolute PORTA.0;
  PORTB                  : byte absolute $000D;
  PORTB_RB7              : bit  absolute PORTB.7;
  PORTB_RB6              : bit  absolute PORTB.6;
  PORTB_RB5              : bit  absolute PORTB.5;
  PORTB_RB4              : bit  absolute PORTB.4;
  PORTB_RB3              : bit  absolute PORTB.3;
  PORTB_RB2              : bit  absolute PORTB.2;
  PORTB_RB1              : bit  absolute PORTB.1;
  PORTB_RB0              : bit  absolute PORTB.0;
  PORTC                  : byte absolute $000E;
  PORTC_RC7              : bit  absolute PORTC.7;
  PORTC_RC6              : bit  absolute PORTC.6;
  PORTC_RC5              : bit  absolute PORTC.5;
  PORTC_RC4              : bit  absolute PORTC.4;
  PORTC_RC3              : bit  absolute PORTC.3;
  PORTC_RC2              : bit  absolute PORTC.2;
  PORTC_RC1              : bit  absolute PORTC.1;
  PORTC_RC0              : bit  absolute PORTC.0;
  PORTD                  : byte absolute $000F;
  PORTD_RD7              : bit  absolute PORTD.7;
  PORTD_RD6              : bit  absolute PORTD.6;
  PORTD_RD5              : bit  absolute PORTD.5;
  PORTD_RD4              : bit  absolute PORTD.4;
  PORTD_RD3              : bit  absolute PORTD.3;
  PORTD_RD2              : bit  absolute PORTD.2;
  PORTD_RD1              : bit  absolute PORTD.1;
  PORTD_RD0              : bit  absolute PORTD.0;
  PORTE                  : byte absolute $0010;
  PORTE_RE7              : bit  absolute PORTE.7;
  PORTE_RE6              : bit  absolute PORTE.6;
  PORTE_RE5              : bit  absolute PORTE.5;
  PORTE_RE4              : bit  absolute PORTE.4;
  PORTE_RE3              : bit  absolute PORTE.3;
  PORTE_RE1              : bit  absolute PORTE.2;
  PORTE_RE0              : bit  absolute PORTE.1;
  PORTF                  : byte absolute $0011;
  PORTF_RF7              : bit  absolute PORTF.7;
  PORTF_RF6              : bit  absolute PORTF.6;
  PORTF_RF5              : bit  absolute PORTF.5;
  PORTF_RF4              : bit  absolute PORTF.4;
  PORTF_RF3              : bit  absolute PORTF.3;
  PORTF_RF2              : bit  absolute PORTF.2;
  PORTF_RF1              : bit  absolute PORTF.1;
  PORTF_RF0              : bit  absolute PORTF.0;
  TRISA                  : byte absolute $0012;
  TRISA_TRISA7           : bit  absolute TRISA.7;
  TRISA_TRISA6           : bit  absolute TRISA.6;
  TRISA_TRISA4           : bit  absolute TRISA.5;
  TRISA_TRISA3           : bit  absolute TRISA.4;
  TRISA_TRISA2           : bit  absolute TRISA.3;
  TRISA_TRISA1           : bit  absolute TRISA.2;
  TRISA_TRISA0           : bit  absolute TRISA.1;
  TRISB                  : byte absolute $0013;
  TRISB_TRISB7           : bit  absolute TRISB.7;
  TRISB_TRISB6           : bit  absolute TRISB.6;
  TRISB_TRISB5           : bit  absolute TRISB.5;
  TRISB_TRISB4           : bit  absolute TRISB.4;
  TRISB_TRISB3           : bit  absolute TRISB.3;
  TRISB_TRISB2           : bit  absolute TRISB.2;
  TRISB_TRISB1           : bit  absolute TRISB.1;
  TRISB_TRISB0           : bit  absolute TRISB.0;
  TRISC                  : byte absolute $0014;
  TRISC_TRISC7           : bit  absolute TRISC.7;
  TRISC_TRISC6           : bit  absolute TRISC.6;
  TRISC_TRISC5           : bit  absolute TRISC.5;
  TRISC_TRISC4           : bit  absolute TRISC.4;
  TRISC_TRISC3           : bit  absolute TRISC.3;
  TRISC_TRISC2           : bit  absolute TRISC.2;
  TRISC_TRISC1           : bit  absolute TRISC.1;
  TRISC_TRISC0           : bit  absolute TRISC.0;
  TRISD                  : byte absolute $0015;
  TRISD_TRISD7           : bit  absolute TRISD.7;
  TRISD_TRISD6           : bit  absolute TRISD.6;
  TRISD_TRISD5           : bit  absolute TRISD.5;
  TRISD_TRISD4           : bit  absolute TRISD.4;
  TRISD_TRISD3           : bit  absolute TRISD.3;
  TRISD_TRISD2           : bit  absolute TRISD.2;
  TRISD_TRISD1           : bit  absolute TRISD.1;
  TRISD_TRISD0           : bit  absolute TRISD.0;
  TRISE                  : byte absolute $0016;
  TRISE_TRISE7           : bit  absolute TRISE.7;
  TRISE_TRISE6           : bit  absolute TRISE.6;
  TRISE_TRISE5           : bit  absolute TRISE.5;
  TRISE_TRISE4           : bit  absolute TRISE.4;
  TRISE_TRISE3           : bit  absolute TRISE.3;
  TRISE_TRISE1           : bit  absolute TRISE.2;
  TRISE_TRISE0           : bit  absolute TRISE.1;
  TRISF                  : byte absolute $0017;
  TRISF_TRISF7           : bit  absolute TRISF.7;
  TRISF_TRISF6           : bit  absolute TRISF.6;
  TRISF_TRISF5           : bit  absolute TRISF.5;
  TRISF_TRISF4           : bit  absolute TRISF.4;
  TRISF_TRISF3           : bit  absolute TRISF.3;
  TRISF_TRISF2           : bit  absolute TRISF.2;
  TRISF_TRISF1           : bit  absolute TRISF.1;
  TRISF_TRISF0           : bit  absolute TRISF.0;
  LATA                   : byte absolute $0018;
  LATA_LATA7             : bit  absolute LATA.7;
  LATA_LATA6             : bit  absolute LATA.6;
  LATA_LATA4             : bit  absolute LATA.5;
  LATA_LATA3             : bit  absolute LATA.4;
  LATA_LATA2             : bit  absolute LATA.3;
  LATA_LATA1             : bit  absolute LATA.2;
  LATA_LATA0             : bit  absolute LATA.1;
  LATB                   : byte absolute $0019;
  LATB_LATB7             : bit  absolute LATB.7;
  LATB_LATB6             : bit  absolute LATB.6;
  LATB_LATB5             : bit  absolute LATB.5;
  LATB_LATB4             : bit  absolute LATB.4;
  LATB_LATB3             : bit  absolute LATB.3;
  LATB_LATB2             : bit  absolute LATB.2;
  LATB_LATB1             : bit  absolute LATB.1;
  LATB_LATB0             : bit  absolute LATB.0;
  LATC                   : byte absolute $001A;
  LATC_LATC7             : bit  absolute LATC.7;
  LATC_LATC6             : bit  absolute LATC.6;
  LATC_LATC5             : bit  absolute LATC.5;
  LATC_LATC4             : bit  absolute LATC.4;
  LATC_LATC3             : bit  absolute LATC.3;
  LATC_LATC2             : bit  absolute LATC.2;
  LATC_LATC1             : bit  absolute LATC.1;
  LATC_LATC0             : bit  absolute LATC.0;
  LATD                   : byte absolute $001B;
  LATD_LATD7             : bit  absolute LATD.7;
  LATD_LATD6             : bit  absolute LATD.6;
  LATD_LATD5             : bit  absolute LATD.5;
  LATD_LATD4             : bit  absolute LATD.4;
  LATD_LATD3             : bit  absolute LATD.3;
  LATD_LATD2             : bit  absolute LATD.2;
  LATD_LATD1             : bit  absolute LATD.1;
  LATD_LATD0             : bit  absolute LATD.0;
  LATE                   : byte absolute $001C;
  LATE_LATE7             : bit  absolute LATE.7;
  LATE_LATE6             : bit  absolute LATE.6;
  LATE_LATE5             : bit  absolute LATE.5;
  LATE_LATE4             : bit  absolute LATE.4;
  LATE_LATE3             : bit  absolute LATE.3;
  LATE_LATE1             : bit  absolute LATE.2;
  LATE_LATE0             : bit  absolute LATE.1;
  LATF                   : byte absolute $001D;
  LATF_LATF7             : bit  absolute LATF.7;
  LATF_LATF6             : bit  absolute LATF.6;
  LATF_LATF5             : bit  absolute LATF.5;
  LATF_LATF4             : bit  absolute LATF.4;
  LATF_LATF3             : bit  absolute LATF.3;
  LATF_LATF2             : bit  absolute LATF.2;
  LATF_LATF1             : bit  absolute LATF.1;
  LATF_LATF0             : bit  absolute LATF.0;
  ADCPCON0               : byte absolute $001F;
  ADCPCON0_ADCPON        : bit  absolute ADCPCON0.7;
  ADCPCON0_ADCPRDY       : bit  absolute ADCPCON0.6;
  ADLTHL                 : byte absolute $008C;
  ADLTHL_ADLTH7          : bit  absolute ADLTHL.7;
  ADLTHL_ADLTH6          : bit  absolute ADLTHL.6;
  ADLTHL_ADLTH5          : bit  absolute ADLTHL.5;
  ADLTHL_ADLTH4          : bit  absolute ADLTHL.4;
  ADLTHL_ADLTH3          : bit  absolute ADLTHL.3;
  ADLTHL_ADLTH2          : bit  absolute ADLTHL.2;
  ADLTHL_ADLTH1          : bit  absolute ADLTHL.1;
  ADLTHL_ADLTH0          : bit  absolute ADLTHL.0;
  ADLTHH                 : byte absolute $008D;
  ADLTHH_ADLTH15         : bit  absolute ADLTHH.7;
  ADLTHH_ADLTH14         : bit  absolute ADLTHH.6;
  ADLTHH_ADLTH13         : bit  absolute ADLTHH.5;
  ADLTHH_ADLTH12         : bit  absolute ADLTHH.4;
  ADLTHH_ADLTH11         : bit  absolute ADLTHH.3;
  ADLTHH_ADLTH10         : bit  absolute ADLTHH.2;
  ADLTHH_ADLTH9          : bit  absolute ADLTHH.1;
  ADLTHH_ADLTH8          : bit  absolute ADLTHH.0;
  ADUTHL                 : byte absolute $008E;
  ADUTHL_ADUTH7          : bit  absolute ADUTHL.7;
  ADUTHL_ADUTH6          : bit  absolute ADUTHL.6;
  ADUTHL_ADUTH5          : bit  absolute ADUTHL.5;
  ADUTHL_ADUTH4          : bit  absolute ADUTHL.4;
  ADUTHL_ADUTH3          : bit  absolute ADUTHL.3;
  ADUTHL_ADUTH2          : bit  absolute ADUTHL.2;
  ADUTHL_ADUTH1          : bit  absolute ADUTHL.1;
  ADUTHL_ADUTH0          : bit  absolute ADUTHL.0;
  ADUTHH                 : byte absolute $008F;
  ADUTHH_ADUTH15         : bit  absolute ADUTHH.7;
  ADUTHH_ADUTH14         : bit  absolute ADUTHH.6;
  ADUTHH_ADUTH13         : bit  absolute ADUTHH.5;
  ADUTHH_ADUTH12         : bit  absolute ADUTHH.4;
  ADUTHH_ADUTH11         : bit  absolute ADUTHH.3;
  ADUTHH_ADUTH10         : bit  absolute ADUTHH.2;
  ADUTHH_ADUTH9          : bit  absolute ADUTHH.1;
  ADUTHH_ADUTH8          : bit  absolute ADUTHH.0;
  ADERRL                 : byte absolute $0090;
  ADERRL_ADERR7          : bit  absolute ADERRL.7;
  ADERRL_ADERR6          : bit  absolute ADERRL.6;
  ADERRL_ADERR5          : bit  absolute ADERRL.5;
  ADERRL_ADERR4          : bit  absolute ADERRL.4;
  ADERRL_ADERR3          : bit  absolute ADERRL.3;
  ADERRL_ADERR2          : bit  absolute ADERRL.2;
  ADERRL_ADERR1          : bit  absolute ADERRL.1;
  ADERRL_ADERR0          : bit  absolute ADERRL.0;
  ADERRH                 : byte absolute $0091;
  ADERRH_ADERR15         : bit  absolute ADERRH.7;
  ADERRH_ADERR14         : bit  absolute ADERRH.6;
  ADERRH_ADERR13         : bit  absolute ADERRH.5;
  ADERRH_ADERR12         : bit  absolute ADERRH.4;
  ADERRH_ADERR11         : bit  absolute ADERRH.3;
  ADERRH_ADERR10         : bit  absolute ADERRH.2;
  ADERRH_ADERR9          : bit  absolute ADERRH.1;
  ADERRH_ADERR8          : bit  absolute ADERRH.0;
  ADSTPTL                : byte absolute $0092;
  ADSTPTL_ADSTPT7        : bit  absolute ADSTPTL.7;
  ADSTPTL_ADSTPT6        : bit  absolute ADSTPTL.6;
  ADSTPTL_ADSTPT5        : bit  absolute ADSTPTL.5;
  ADSTPTL_ADSTPT4        : bit  absolute ADSTPTL.4;
  ADSTPTL_ADSTPT3        : bit  absolute ADSTPTL.3;
  ADSTPTL_ADSTPT2        : bit  absolute ADSTPTL.2;
  ADSTPTL_ADSTPT1        : bit  absolute ADSTPTL.1;
  ADSTPTL_ADSTPT0        : bit  absolute ADSTPTL.0;
  ADSTPTH                : byte absolute $0093;
  ADSTPTH_ADSTPT15       : bit  absolute ADSTPTH.7;
  ADSTPTH_ADSTPT14       : bit  absolute ADSTPTH.6;
  ADSTPTH_ADSTPT13       : bit  absolute ADSTPTH.5;
  ADSTPTH_ADSTPT12       : bit  absolute ADSTPTH.4;
  ADSTPTH_ADSTPT11       : bit  absolute ADSTPTH.3;
  ADSTPTH_ADSTPT10       : bit  absolute ADSTPTH.2;
  ADSTPTH_ADSTPT9        : bit  absolute ADSTPTH.1;
  ADSTPTH_ADSTPT8        : bit  absolute ADSTPTH.0;
  ADFLTRL                : byte absolute $0094;
  ADFLTRL_ADFLTR7        : bit  absolute ADFLTRL.7;
  ADFLTRL_ADFLTR6        : bit  absolute ADFLTRL.6;
  ADFLTRL_ADFLTR5        : bit  absolute ADFLTRL.5;
  ADFLTRL_ADFLTR4        : bit  absolute ADFLTRL.4;
  ADFLTRL_ADFLTR3        : bit  absolute ADFLTRL.3;
  ADFLTRL_ADFLTR2        : bit  absolute ADFLTRL.2;
  ADFLTRL_ADFLTR1        : bit  absolute ADFLTRL.1;
  ADFLTRL_ADFLTR0        : bit  absolute ADFLTRL.0;
  ADFLTRH                : byte absolute $0095;
  ADFLTRH_ADFLTR15       : bit  absolute ADFLTRH.7;
  ADFLTRH_ADFLTR14       : bit  absolute ADFLTRH.6;
  ADFLTRH_ADFLTR13       : bit  absolute ADFLTRH.5;
  ADFLTRH_ADFLTR12       : bit  absolute ADFLTRH.4;
  ADFLTRH_ADFLTR11       : bit  absolute ADFLTRH.3;
  ADFLTRH_ADFLTR10       : bit  absolute ADFLTRH.2;
  ADFLTRH_ADFLTR9        : bit  absolute ADFLTRH.1;
  ADFLTRH_ADFLTR8        : bit  absolute ADFLTRH.0;
  ADACCL                 : byte absolute $0096;
  ADACCL_ADACC7          : bit  absolute ADACCL.7;
  ADACCL_ADACC6          : bit  absolute ADACCL.6;
  ADACCL_ADACC5          : bit  absolute ADACCL.5;
  ADACCL_ADACC4          : bit  absolute ADACCL.4;
  ADACCL_ADACC3          : bit  absolute ADACCL.3;
  ADACCL_ADACC2          : bit  absolute ADACCL.2;
  ADACCL_ADACC1          : bit  absolute ADACCL.1;
  ADACCL_ADACC0          : bit  absolute ADACCL.0;
  ADACCH                 : byte absolute $0097;
  ADACCH_ADACC15         : bit  absolute ADACCH.7;
  ADACCH_ADACC14         : bit  absolute ADACCH.6;
  ADACCH_ADACC13         : bit  absolute ADACCH.5;
  ADACCH_ADACC12         : bit  absolute ADACCH.4;
  ADACCH_ADACC11         : bit  absolute ADACCH.3;
  ADACCH_ADACC10         : bit  absolute ADACCH.2;
  ADACCH_ADACC9          : bit  absolute ADACCH.1;
  ADACCH_ADACC8          : bit  absolute ADACCH.0;
  ADACCU                 : byte absolute $0098;
  ADACCU_ADACC23         : bit  absolute ADACCU.7;
  ADACCU_ADACC22         : bit  absolute ADACCU.6;
  ADACCU_ADACC21         : bit  absolute ADACCU.5;
  ADACCU_ADACC20         : bit  absolute ADACCU.4;
  ADACCU_ADACC19         : bit  absolute ADACCU.3;
  ADACCU_ADACC18         : bit  absolute ADACCU.2;
  ADACCU_ADACC17         : bit  absolute ADACCU.1;
  ADACCU_ADACC16         : bit  absolute ADACCU.0;
  ADCNT                  : byte absolute $0099;
  ADCNT_ADCNT7           : bit  absolute ADCNT.7;
  ADCNT_ADCNT6           : bit  absolute ADCNT.6;
  ADCNT_ADCNT5           : bit  absolute ADCNT.5;
  ADCNT_ADCNT4           : bit  absolute ADCNT.4;
  ADCNT_ADCNT3           : bit  absolute ADCNT.3;
  ADCNT_ADCNT2           : bit  absolute ADCNT.2;
  ADCNT_ADCNT1           : bit  absolute ADCNT.1;
  ADCNT_ADCNT0           : bit  absolute ADCNT.0;
  ADRPT                  : byte absolute $009A;
  ADRPT_ADRPT7           : bit  absolute ADRPT.7;
  ADRPT_ADRPT6           : bit  absolute ADRPT.6;
  ADRPT_ADRPT5           : bit  absolute ADRPT.5;
  ADRPT_ADRPT4           : bit  absolute ADRPT.4;
  ADRPT_ADRPT3           : bit  absolute ADRPT.3;
  ADRPT_ADRPT2           : bit  absolute ADRPT.2;
  ADRPT_ADRPT1           : bit  absolute ADRPT.1;
  ADRPT_ADRPT0           : bit  absolute ADRPT.0;
  ADPREVL                : byte absolute $009B;
  ADPREVL_ADPREV7        : bit  absolute ADPREVL.7;
  ADPREVL_ADPREV6        : bit  absolute ADPREVL.6;
  ADPREVL_ADPREV5        : bit  absolute ADPREVL.5;
  ADPREVL_ADPREV4        : bit  absolute ADPREVL.4;
  ADPREVL_ADPREV3        : bit  absolute ADPREVL.3;
  ADPREVL_ADPREV2        : bit  absolute ADPREVL.2;
  ADPREVL_ADPREV1        : bit  absolute ADPREVL.1;
  ADPREVL_ADPREV0        : bit  absolute ADPREVL.0;
  ADPREVH                : byte absolute $009C;
  ADPREVH_ADPREV15       : bit  absolute ADPREVH.7;
  ADPREVH_ADPREV14       : bit  absolute ADPREVH.6;
  ADPREVH_ADPREV13       : bit  absolute ADPREVH.5;
  ADPREVH_ADPREV12       : bit  absolute ADPREVH.4;
  ADPREVH_ADPREV11       : bit  absolute ADPREVH.3;
  ADPREVH_ADPREV10       : bit  absolute ADPREVH.2;
  ADPREVH_ADPREV9        : bit  absolute ADPREVH.1;
  ADPREVH_ADPREV8        : bit  absolute ADPREVH.0;
  ADRESL                 : byte absolute $009D;
  ADRESL_ADRES7          : bit  absolute ADRESL.7;
  ADRESL_ADRES6          : bit  absolute ADRESL.6;
  ADRESL_ADRES5          : bit  absolute ADRESL.5;
  ADRESL_ADRES4          : bit  absolute ADRESL.4;
  ADRESL_ADRES3          : bit  absolute ADRESL.3;
  ADRESL_ADRES2          : bit  absolute ADRESL.2;
  ADRESL_ADRES1          : bit  absolute ADRESL.1;
  ADRESL_ADRES0          : bit  absolute ADRESL.0;
  ADRESH                 : byte absolute $009E;
  ADRESH_ADRES15         : bit  absolute ADRESH.7;
  ADRESH_ADRES14         : bit  absolute ADRESH.6;
  ADRESH_ADRES13         : bit  absolute ADRESH.5;
  ADRESH_ADRES12         : bit  absolute ADRESH.4;
  ADRESH_ADRES11         : bit  absolute ADRESH.3;
  ADRESH_ADRES10         : bit  absolute ADRESH.2;
  ADRESH_ADRES9          : bit  absolute ADRESH.1;
  ADRESH_ADRES8          : bit  absolute ADRESH.0;
  ADPCH                  : byte absolute $009F;
  ADPCH_ADPCH5           : bit  absolute ADPCH.5;
  ADPCH_ADPCH4           : bit  absolute ADPCH.4;
  ADPCH_ADPCH3           : bit  absolute ADPCH.3;
  ADPCH_ADPCH2           : bit  absolute ADPCH.2;
  ADPCH_ADPCH1           : bit  absolute ADPCH.1;
  ADPCH_ADPCH0           : bit  absolute ADPCH.0;
  ADACQL                 : byte absolute $010C;
  ADACQL_ADACQ7          : bit  absolute ADACQL.7;
  ADACQL_ADACQ6          : bit  absolute ADACQL.6;
  ADACQL_ADACQ5          : bit  absolute ADACQL.5;
  ADACQL_ADACQ4          : bit  absolute ADACQL.4;
  ADACQL_ADACQ3          : bit  absolute ADACQL.3;
  ADACQL_ADACQ2          : bit  absolute ADACQL.2;
  ADACQL_ADACQ1          : bit  absolute ADACQL.1;
  ADACQL_ADACQ0          : bit  absolute ADACQL.0;
  ADACQH                 : byte absolute $010D;
  ADACQH_ADACQ12         : bit  absolute ADACQH.4;
  ADACQH_ADACQ11         : bit  absolute ADACQH.3;
  ADACQH_ADACQ10         : bit  absolute ADACQH.2;
  ADACQH_ADACQ9          : bit  absolute ADACQH.1;
  ADACQH_ADACQ8          : bit  absolute ADACQH.0;
  ADCAP                  : byte absolute $010E;
  ADCAP_ADCAP4           : bit  absolute ADCAP.4;
  ADCAP_ADCAP3           : bit  absolute ADCAP.3;
  ADCAP_ADCAP2           : bit  absolute ADCAP.2;
  ADCAP_ADCAP1           : bit  absolute ADCAP.1;
  ADCAP_ADCAP0           : bit  absolute ADCAP.0;
  ADPREL                 : byte absolute $010F;
  ADPREL_PRE7            : bit  absolute ADPREL.7;
  ADPREL_PRE6            : bit  absolute ADPREL.6;
  ADPREL_PRE5            : bit  absolute ADPREL.5;
  ADPREL_PRE4            : bit  absolute ADPREL.4;
  ADPREL_PRE3            : bit  absolute ADPREL.3;
  ADPREL_PRE2            : bit  absolute ADPREL.2;
  ADPREL_PRE1            : bit  absolute ADPREL.1;
  ADPREL_PRE0            : bit  absolute ADPREL.0;
  ADPREH                 : byte absolute $0110;
  ADPREH_PRE12           : bit  absolute ADPREH.4;
  ADPREH_PRE11           : bit  absolute ADPREH.3;
  ADPREH_PRE10           : bit  absolute ADPREH.2;
  ADPREH_PRE9            : bit  absolute ADPREH.1;
  ADPREH_PRE8            : bit  absolute ADPREH.0;
  ADCON0                 : byte absolute $0111;
  ADCON0_ON              : bit  absolute ADCON0.5;
  ADCON0_CONT            : bit  absolute ADCON0.4;
  ADCON0_CS              : bit  absolute ADCON0.3;
  ADCON0_FM              : bit  absolute ADCON0.2;
  ADCON0_GO              : bit  absolute ADCON0.1;
  ADCON1                 : byte absolute $0112;
  ADCON1_PPOL            : bit  absolute ADCON1.4;
  ADCON1_IPEN            : bit  absolute ADCON1.3;
  ADCON1_GPOL            : bit  absolute ADCON1.2;
  ADCON1_DSEN            : bit  absolute ADCON1.1;
  ADCON2                 : byte absolute $0113;
  ADCON2_PSIS            : bit  absolute ADCON2.7;
  ADCON2_ADCRS2          : bit  absolute ADCON2.6;
  ADCON2_ADCRS1          : bit  absolute ADCON2.5;
  ADCON2_ADCRS0          : bit  absolute ADCON2.4;
  ADCON2_ACLR            : bit  absolute ADCON2.3;
  ADCON2_ADMD2           : bit  absolute ADCON2.2;
  ADCON2_ADMD1           : bit  absolute ADCON2.1;
  ADCON2_ADMD0           : bit  absolute ADCON2.0;
  ADCON3                 : byte absolute $0114;
  ADCON3_ADCALC2         : bit  absolute ADCON3.6;
  ADCON3_ADCALC1         : bit  absolute ADCON3.5;
  ADCON3_ADCALC0         : bit  absolute ADCON3.4;
  ADCON3_SOI             : bit  absolute ADCON3.3;
  ADCON3_ADTMD2          : bit  absolute ADCON3.2;
  ADCON3_ADTMD1          : bit  absolute ADCON3.1;
  ADCON3_ADTMD0          : bit  absolute ADCON3.0;
  ADSTAT                 : byte absolute $0115;
  ADSTAT_OV              : bit  absolute ADSTAT.7;
  ADSTAT_UTHR            : bit  absolute ADSTAT.6;
  ADSTAT_LTHR            : bit  absolute ADSTAT.5;
  ADSTAT_MATH            : bit  absolute ADSTAT.4;
  ADSTAT_ADSTAT2         : bit  absolute ADSTAT.3;
  ADSTAT_ADSTAT1         : bit  absolute ADSTAT.2;
  ADSTAT_ADSTAT0         : bit  absolute ADSTAT.1;
  ADSTAT_STAT0           : bit  absolute ADSTAT.0;
  ADREF                  : byte absolute $0116;
  ADREF_PREF1            : bit  absolute ADREF.1;
  ADREF_PREF0            : bit  absolute ADREF.0;
  ADACT                  : byte absolute $0117;
  ADACT_ADACT4           : bit  absolute ADACT.4;
  ADACT_ADACT3           : bit  absolute ADACT.3;
  ADACT_ADACT2           : bit  absolute ADACT.2;
  ADACT_ADACT1           : bit  absolute ADACT.1;
  ADACT_ADACT0           : bit  absolute ADACT.0;
  ADCLK                  : byte absolute $0118;
  ADCLK_ADCS5            : bit  absolute ADCLK.5;
  ADCLK_ADCS4            : bit  absolute ADCLK.4;
  ADCLK_ADCS3            : bit  absolute ADCLK.3;
  ADCLK_ADCS2            : bit  absolute ADCLK.2;
  ADCLK_ADCS1            : bit  absolute ADCLK.1;
  ADCLK_ADCS0            : bit  absolute ADCLK.0;
  RC1REG                 : byte absolute $0119;
  TX1REG                 : byte absolute $011A;
  SP1BRGL                : byte absolute $011B;
  SP1BRGH                : byte absolute $011C;
  RC1STA                 : byte absolute $011D;
  RC1STA_SPEN            : bit  absolute RC1STA.7;
  RC1STA_RX9             : bit  absolute RC1STA.6;
  RC1STA_SREN            : bit  absolute RC1STA.5;
  RC1STA_CREN            : bit  absolute RC1STA.4;
  RC1STA_ADDEN           : bit  absolute RC1STA.3;
  RC1STA_FERR            : bit  absolute RC1STA.2;
  RC1STA_OERR            : bit  absolute RC1STA.1;
  RC1STA_RX9D            : bit  absolute RC1STA.0;
  TX1STA                 : byte absolute $011E;
  TX1STA_CSRC            : bit  absolute TX1STA.7;
  TX1STA_TX9             : bit  absolute TX1STA.6;
  TX1STA_TXEN            : bit  absolute TX1STA.5;
  TX1STA_SYNC            : bit  absolute TX1STA.4;
  TX1STA_SENDB           : bit  absolute TX1STA.3;
  TX1STA_BRGH            : bit  absolute TX1STA.2;
  TX1STA_TRMT            : bit  absolute TX1STA.1;
  TX1STA_TX9D            : bit  absolute TX1STA.0;
  BAUD1CON               : byte absolute $011F;
  BAUD1CON_ABDOVF        : bit  absolute BAUD1CON.6;
  BAUD1CON_RCIDL         : bit  absolute BAUD1CON.5;
  BAUD1CON_SCKP          : bit  absolute BAUD1CON.4;
  BAUD1CON_BRG16         : bit  absolute BAUD1CON.3;
  BAUD1CON_WUE           : bit  absolute BAUD1CON.2;
  BAUD1CON_ABDEN         : bit  absolute BAUD1CON.1;
  SSP1BUF                : byte absolute $018C;
  SSP1ADD                : byte absolute $018D;
  SSP1ADD_MSK7           : bit  absolute SSP1ADD.7;
  SSP1ADD_MSK6           : bit  absolute SSP1ADD.6;
  SSP1ADD_MSK5           : bit  absolute SSP1ADD.5;
  SSP1ADD_MSK4           : bit  absolute SSP1ADD.4;
  SSP1ADD_MSK3           : bit  absolute SSP1ADD.3;
  SSP1ADD_MSK2           : bit  absolute SSP1ADD.2;
  SSP1ADD_MSK1           : bit  absolute SSP1ADD.1;
  SSP1ADD_MSK0           : bit  absolute SSP1ADD.0;
  SSP1MSK                : byte absolute $018E;
  SSP1STAT               : byte absolute $018F;
  SSP1STAT_SMP           : bit  absolute SSP1STAT.7;
  SSP1STAT_CKE           : bit  absolute SSP1STAT.6;
  SSP1STAT_D_nA          : bit  absolute SSP1STAT.5;
  SSP1STAT_P             : bit  absolute SSP1STAT.4;
  SSP1STAT_S             : bit  absolute SSP1STAT.3;
  SSP1STAT_R_nW          : bit  absolute SSP1STAT.2;
  SSP1STAT_UA            : bit  absolute SSP1STAT.1;
  SSP1STAT_BF            : bit  absolute SSP1STAT.0;
  SSP1CON1               : byte absolute $0190;
  SSP1CON1_WCOL          : bit  absolute SSP1CON1.7;
  SSP1CON1_SSPOV         : bit  absolute SSP1CON1.6;
  SSP1CON1_SSPEN         : bit  absolute SSP1CON1.5;
  SSP1CON1_CKP           : bit  absolute SSP1CON1.4;
  SSP1CON1_SSPM3         : bit  absolute SSP1CON1.3;
  SSP1CON1_SSPM2         : bit  absolute SSP1CON1.2;
  SSP1CON1_SSPM1         : bit  absolute SSP1CON1.1;
  SSP1CON1_SSPM0         : bit  absolute SSP1CON1.0;
  SSP1CON2               : byte absolute $0191;
  SSP1CON2_GCEN          : bit  absolute SSP1CON2.7;
  SSP1CON2_ACKSTAT       : bit  absolute SSP1CON2.6;
  SSP1CON2_ACKDT         : bit  absolute SSP1CON2.5;
  SSP1CON2_ACKEN         : bit  absolute SSP1CON2.4;
  SSP1CON2_RCEN          : bit  absolute SSP1CON2.3;
  SSP1CON2_PEN           : bit  absolute SSP1CON2.2;
  SSP1CON2_RSEN          : bit  absolute SSP1CON2.1;
  SSP1CON2_SEN           : bit  absolute SSP1CON2.0;
  SSP1CON3               : byte absolute $0192;
  SSP1CON3_ACKTIM        : bit  absolute SSP1CON3.7;
  SSP1CON3_PCIE          : bit  absolute SSP1CON3.6;
  SSP1CON3_SCIE          : bit  absolute SSP1CON3.5;
  SSP1CON3_BOEN          : bit  absolute SSP1CON3.4;
  SSP1CON3_SDAHT         : bit  absolute SSP1CON3.3;
  SSP1CON3_SBCDE         : bit  absolute SSP1CON3.2;
  SSP1CON3_AHEN          : bit  absolute SSP1CON3.1;
  SSP1CON3_DHEN          : bit  absolute SSP1CON3.0;
  TMR1L                  : byte absolute $020C;
  TMR1L_TMR1L7           : bit  absolute TMR1L.7;
  TMR1L_TMR1L6           : bit  absolute TMR1L.6;
  TMR1L_TMR1L5           : bit  absolute TMR1L.5;
  TMR1L_TMR1L4           : bit  absolute TMR1L.4;
  TMR1L_TMR1L3           : bit  absolute TMR1L.3;
  TMR1L_TMR1L2           : bit  absolute TMR1L.2;
  TMR1L_TMR1L1           : bit  absolute TMR1L.1;
  TMR1L_TMR1L0           : bit  absolute TMR1L.0;
  TMR1H                  : byte absolute $020D;
  TMR1H_TMR1H7           : bit  absolute TMR1H.7;
  TMR1H_TMR1H6           : bit  absolute TMR1H.6;
  TMR1H_TMR1H5           : bit  absolute TMR1H.5;
  TMR1H_TMR1H4           : bit  absolute TMR1H.4;
  TMR1H_TMR1H3           : bit  absolute TMR1H.3;
  TMR1H_TMR1H2           : bit  absolute TMR1H.2;
  TMR1H_TMR1H1           : bit  absolute TMR1H.1;
  TMR1H_TMR1H0           : bit  absolute TMR1H.0;
  T1CON                  : byte absolute $020E;
  T1CON_T1CKPS1          : bit  absolute T1CON.5;
  T1CON_T1CKPS0          : bit  absolute T1CON.4;
  T1CON_RD16             : bit  absolute T1CON.2;
  T1GCON                 : byte absolute $020F;
  T1GCON_GE              : bit  absolute T1GCON.6;
  T1GCON_GTM             : bit  absolute T1GCON.4;
  T1GCON_GSPM            : bit  absolute T1GCON.3;
  T1GCON_GGO_nDONE       : bit  absolute T1GCON.2;
  T1GCON_GVAL            : bit  absolute T1GCON.1;
  T1GATE                 : byte absolute $0210;
  T1GATE_GSS4            : bit  absolute T1GATE.4;
  T1GATE_GSS3            : bit  absolute T1GATE.3;
  T1GATE_GSS2            : bit  absolute T1GATE.2;
  T1GATE_GSS1            : bit  absolute T1GATE.1;
  T1GATE_GSS0            : bit  absolute T1GATE.0;
  T1CLK                  : byte absolute $0211;
  T1CLK_T1CS3            : bit  absolute T1CLK.3;
  T1CLK_T1CS2            : bit  absolute T1CLK.2;
  T1CLK_T1CS1            : bit  absolute T1CLK.1;
  T1CLK_T1CS0            : bit  absolute T1CLK.0;
  CCPTMRS0               : byte absolute $021E;
  CCPTMRS0_P4TSEL1       : bit  absolute CCPTMRS0.7;
  CCPTMRS0_P4TSEL0       : bit  absolute CCPTMRS0.6;
  CCPTMRS0_P3TSEL1       : bit  absolute CCPTMRS0.5;
  CCPTMRS0_P3TSEL0       : bit  absolute CCPTMRS0.4;
  CCPTMRS0_C2TSEL1       : bit  absolute CCPTMRS0.3;
  CCPTMRS0_C2TSEL0       : bit  absolute CCPTMRS0.2;
  CCPTMRS0_C1TSEL1       : bit  absolute CCPTMRS0.1;
  CCPTMRS0_C1TSEL0       : bit  absolute CCPTMRS0.0;
  T2TMR                  : byte absolute $028C;
  T2PR                   : byte absolute $028D;
  T2CON                  : byte absolute $028E;
  T2CON_T2CKPS2          : bit  absolute T2CON.6;
  T2CON_T2CKPS1          : bit  absolute T2CON.5;
  T2CON_T2CKPS0          : bit  absolute T2CON.4;
  T2CON_T2OUTPS3         : bit  absolute T2CON.3;
  T2CON_T2OUTPS2         : bit  absolute T2CON.2;
  T2CON_T2OUTPS1         : bit  absolute T2CON.1;
  T2CON_T2OUTPS0         : bit  absolute T2CON.0;
  T2HLT                  : byte absolute $028F;
  T2HLT_PSYNC            : bit  absolute T2HLT.7;
  T2HLT_CKPOL            : bit  absolute T2HLT.6;
  T2HLT_CKSYNC           : bit  absolute T2HLT.5;
  T2HLT_MODE4            : bit  absolute T2HLT.4;
  T2HLT_MODE3            : bit  absolute T2HLT.3;
  T2HLT_MODE2            : bit  absolute T2HLT.2;
  T2HLT_MODE1            : bit  absolute T2HLT.1;
  T2HLT_MODE0            : bit  absolute T2HLT.0;
  T2CLKCON               : byte absolute $0290;
  T2CLKCON_CS3           : bit  absolute T2CLKCON.3;
  T2CLKCON_CS2           : bit  absolute T2CLKCON.2;
  T2CLKCON_CS1           : bit  absolute T2CLKCON.1;
  T2CLKCON_CS0           : bit  absolute T2CLKCON.0;
  T2RST                  : byte absolute $0291;
  T2RST_RSEL4            : bit  absolute T2RST.4;
  T2RST_RSEL3            : bit  absolute T2RST.3;
  T2RST_RSEL2            : bit  absolute T2RST.2;
  T2RST_RSEL1            : bit  absolute T2RST.1;
  T2RST_RSEL0            : bit  absolute T2RST.0;
  T4TMR                  : byte absolute $0292;
  T4PR                   : byte absolute $0293;
  T4CON                  : byte absolute $0294;
  T4CON_T4CKPS2          : bit  absolute T4CON.6;
  T4CON_T4CKPS1          : bit  absolute T4CON.5;
  T4CON_T4CKPS0          : bit  absolute T4CON.4;
  T4CON_T4OUTPS3         : bit  absolute T4CON.3;
  T4CON_T4OUTPS2         : bit  absolute T4CON.2;
  T4CON_T4OUTPS1         : bit  absolute T4CON.1;
  T4CON_T4OUTPS0         : bit  absolute T4CON.0;
  T4HLT                  : byte absolute $0295;
  T4CLKCON               : byte absolute $0296;
  T4RST                  : byte absolute $0297;
  CCPR1L                 : byte absolute $030C;
  CCPR1H                 : byte absolute $030D;
  CCP1CON                : byte absolute $030E;
  CCP1CON_EN             : bit  absolute CCP1CON.7;
  CCP1CON_OE             : bit  absolute CCP1CON.6;
  CCP1CON_OUT            : bit  absolute CCP1CON.5;
  CCP1CON_FMT            : bit  absolute CCP1CON.4;
  CCP1CAP                : byte absolute $030F;
  CCP1CAP_CTS2           : bit  absolute CCP1CAP.3;
  CCP1CAP_CTS1           : bit  absolute CCP1CAP.2;
  CCP1CAP_CTS0           : bit  absolute CCP1CAP.1;
  CCPR2L                 : byte absolute $0310;
  CCPR2H                 : byte absolute $0311;
  CCP2CON                : byte absolute $0312;
  CCP2CAP                : byte absolute $0313;
  PWM3DCL                : byte absolute $0314;
  PWM3DCL_DC1            : bit  absolute PWM3DCL.7;
  PWM3DCL_DC0            : bit  absolute PWM3DCL.6;
  PWM3DCH                : byte absolute $0315;
  PWM3DCH_DC9            : bit  absolute PWM3DCH.7;
  PWM3DCH_DC8            : bit  absolute PWM3DCH.6;
  PWM3DCH_DC7            : bit  absolute PWM3DCH.5;
  PWM3DCH_DC6            : bit  absolute PWM3DCH.4;
  PWM3DCH_DC5            : bit  absolute PWM3DCH.3;
  PWM3DCH_DC4            : bit  absolute PWM3DCH.2;
  PWM3DCH_DC3            : bit  absolute PWM3DCH.1;
  PWM3DCH_DC2            : bit  absolute PWM3DCH.0;
  PWM3CON                : byte absolute $0316;
  PWM3CON_POL            : bit  absolute PWM3CON.4;
  PWM4DCL                : byte absolute $0318;
  PWM4DCH                : byte absolute $0319;
  PWM4CON                : byte absolute $031A;
  SMT1TMRL               : byte absolute $048C;
  SMT1TMRL_SMT1TMR7      : bit  absolute SMT1TMRL.7;
  SMT1TMRL_SMT1TMR6      : bit  absolute SMT1TMRL.6;
  SMT1TMRL_SMT1TMR5      : bit  absolute SMT1TMRL.5;
  SMT1TMRL_SMT1TMR4      : bit  absolute SMT1TMRL.4;
  SMT1TMRL_SMT1TMR3      : bit  absolute SMT1TMRL.3;
  SMT1TMRL_SMT1TMR2      : bit  absolute SMT1TMRL.2;
  SMT1TMRL_SMT1TMR1      : bit  absolute SMT1TMRL.1;
  SMT1TMRL_SMT1TMR0      : bit  absolute SMT1TMRL.0;
  SMT1TMRH               : byte absolute $048D;
  SMT1TMRH_SMT1TMR15     : bit  absolute SMT1TMRH.7;
  SMT1TMRH_SMT1TMR14     : bit  absolute SMT1TMRH.6;
  SMT1TMRH_SMT1TMR13     : bit  absolute SMT1TMRH.5;
  SMT1TMRH_SMT1TMR12     : bit  absolute SMT1TMRH.4;
  SMT1TMRH_SMT1TMR11     : bit  absolute SMT1TMRH.3;
  SMT1TMRH_SMT1TMR10     : bit  absolute SMT1TMRH.2;
  SMT1TMRH_SMT1TMR9      : bit  absolute SMT1TMRH.1;
  SMT1TMRH_SMT1TMR8      : bit  absolute SMT1TMRH.0;
  SMT1TMRU               : byte absolute $048E;
  SMT1TMRU_SMT1TMR23     : bit  absolute SMT1TMRU.7;
  SMT1TMRU_SMT1TMR22     : bit  absolute SMT1TMRU.6;
  SMT1TMRU_SMT1TMR21     : bit  absolute SMT1TMRU.5;
  SMT1TMRU_SMT1TMR20     : bit  absolute SMT1TMRU.4;
  SMT1TMRU_SMT1TMR19     : bit  absolute SMT1TMRU.3;
  SMT1TMRU_SMT1TMR18     : bit  absolute SMT1TMRU.2;
  SMT1TMRU_SMT1TMR17     : bit  absolute SMT1TMRU.1;
  SMT1TMRU_SMT1TMR16     : bit  absolute SMT1TMRU.0;
  SMT1CPRL               : byte absolute $048F;
  SMT1CPRL_CPR7          : bit  absolute SMT1CPRL.7;
  SMT1CPRL_CPR6          : bit  absolute SMT1CPRL.6;
  SMT1CPRL_CPR5          : bit  absolute SMT1CPRL.5;
  SMT1CPRL_CPR4          : bit  absolute SMT1CPRL.4;
  SMT1CPRL_CPR3          : bit  absolute SMT1CPRL.3;
  SMT1CPRL_CPR2          : bit  absolute SMT1CPRL.2;
  SMT1CPRL_CPR1          : bit  absolute SMT1CPRL.1;
  SMT1CPRL_CPR0          : bit  absolute SMT1CPRL.0;
  SMT1CPRH               : byte absolute $0490;
  SMT1CPRH_CPR15         : bit  absolute SMT1CPRH.7;
  SMT1CPRH_CPR14         : bit  absolute SMT1CPRH.6;
  SMT1CPRH_CPR13         : bit  absolute SMT1CPRH.5;
  SMT1CPRH_CPR12         : bit  absolute SMT1CPRH.4;
  SMT1CPRH_CPR11         : bit  absolute SMT1CPRH.3;
  SMT1CPRH_CPR10         : bit  absolute SMT1CPRH.2;
  SMT1CPRH_CPR9          : bit  absolute SMT1CPRH.1;
  SMT1CPRH_CPR8          : bit  absolute SMT1CPRH.0;
  SMT1CPRU               : byte absolute $0491;
  SMT1CPRU_CPR23         : bit  absolute SMT1CPRU.7;
  SMT1CPRU_CPR22         : bit  absolute SMT1CPRU.6;
  SMT1CPRU_CPR21         : bit  absolute SMT1CPRU.5;
  SMT1CPRU_CPR20         : bit  absolute SMT1CPRU.4;
  SMT1CPRU_CPR19         : bit  absolute SMT1CPRU.3;
  SMT1CPRU_CPR18         : bit  absolute SMT1CPRU.2;
  SMT1CPRU_CPR17         : bit  absolute SMT1CPRU.1;
  SMT1CPRU_CPR16         : bit  absolute SMT1CPRU.0;
  SMT1CPWL               : byte absolute $0492;
  SMT1CPWL_CPW7          : bit  absolute SMT1CPWL.7;
  SMT1CPWL_CPW6          : bit  absolute SMT1CPWL.6;
  SMT1CPWL_CPW5          : bit  absolute SMT1CPWL.5;
  SMT1CPWL_CPW4          : bit  absolute SMT1CPWL.4;
  SMT1CPWL_CPW3          : bit  absolute SMT1CPWL.3;
  SMT1CPWL_CPW2          : bit  absolute SMT1CPWL.2;
  SMT1CPWL_CPW1          : bit  absolute SMT1CPWL.1;
  SMT1CPWL_CPW0          : bit  absolute SMT1CPWL.0;
  SMT1CPWH               : byte absolute $0493;
  SMT1CPWH_CPW15         : bit  absolute SMT1CPWH.7;
  SMT1CPWH_CPW14         : bit  absolute SMT1CPWH.6;
  SMT1CPWH_CPW13         : bit  absolute SMT1CPWH.5;
  SMT1CPWH_CPW12         : bit  absolute SMT1CPWH.4;
  SMT1CPWH_CPW11         : bit  absolute SMT1CPWH.3;
  SMT1CPWH_CPW10         : bit  absolute SMT1CPWH.2;
  SMT1CPWH_CPW9          : bit  absolute SMT1CPWH.1;
  SMT1CPWH_CPW8          : bit  absolute SMT1CPWH.0;
  SMT1CPWU               : byte absolute $0494;
  SMT1CPWU_CPW23         : bit  absolute SMT1CPWU.7;
  SMT1CPWU_CPW22         : bit  absolute SMT1CPWU.6;
  SMT1CPWU_CPW21         : bit  absolute SMT1CPWU.5;
  SMT1CPWU_CPW20         : bit  absolute SMT1CPWU.4;
  SMT1CPWU_CPW19         : bit  absolute SMT1CPWU.3;
  SMT1CPWU_CPW18         : bit  absolute SMT1CPWU.2;
  SMT1CPWU_CPW17         : bit  absolute SMT1CPWU.1;
  SMT1CPWU_CPW16         : bit  absolute SMT1CPWU.0;
  SMT1PRL                : byte absolute $0495;
  SMT1PRL_SMT1PR7        : bit  absolute SMT1PRL.7;
  SMT1PRL_SMT1PR6        : bit  absolute SMT1PRL.6;
  SMT1PRL_SMT1PR5        : bit  absolute SMT1PRL.5;
  SMT1PRL_SMT1PR4        : bit  absolute SMT1PRL.4;
  SMT1PRL_SMT1PR3        : bit  absolute SMT1PRL.3;
  SMT1PRL_SMT1PR2        : bit  absolute SMT1PRL.2;
  SMT1PRL_SMT1PR1        : bit  absolute SMT1PRL.1;
  SMT1PRL_SMT1PR0        : bit  absolute SMT1PRL.0;
  SMT1PRH                : byte absolute $0496;
  SMT1PRH_SMT1PR15       : bit  absolute SMT1PRH.7;
  SMT1PRH_SMT1PR14       : bit  absolute SMT1PRH.6;
  SMT1PRH_SMT1PR13       : bit  absolute SMT1PRH.5;
  SMT1PRH_SMT1PR12       : bit  absolute SMT1PRH.4;
  SMT1PRH_SMT1PR11       : bit  absolute SMT1PRH.3;
  SMT1PRH_SMT1PR10       : bit  absolute SMT1PRH.2;
  SMT1PRH_SMT1PR9        : bit  absolute SMT1PRH.1;
  SMT1PRH_SMT1PR8        : bit  absolute SMT1PRH.0;
  SMT1PRU                : byte absolute $0497;
  SMT1PRU_SMT1PR23       : bit  absolute SMT1PRU.7;
  SMT1PRU_SMT1PR22       : bit  absolute SMT1PRU.6;
  SMT1PRU_SMT1PR21       : bit  absolute SMT1PRU.5;
  SMT1PRU_SMT1PR20       : bit  absolute SMT1PRU.4;
  SMT1PRU_SMT1PR19       : bit  absolute SMT1PRU.3;
  SMT1PRU_SMT1PR18       : bit  absolute SMT1PRU.2;
  SMT1PRU_SMT1PR17       : bit  absolute SMT1PRU.1;
  SMT1PRU_SMT1PR16       : bit  absolute SMT1PRU.0;
  SMT1CON0               : byte absolute $0498;
  SMT1CON0_STP           : bit  absolute SMT1CON0.6;
  SMT1CON0_WPOL          : bit  absolute SMT1CON0.5;
  SMT1CON0_SPOL          : bit  absolute SMT1CON0.4;
  SMT1CON0_CPOL          : bit  absolute SMT1CON0.3;
  SMT1CON1               : byte absolute $0499;
  SMT1CON1_SMT1GO        : bit  absolute SMT1CON1.7;
  SMT1CON1_REPEAT        : bit  absolute SMT1CON1.6;
  SMT1STAT               : byte absolute $049A;
  SMT1STAT_CPRUP         : bit  absolute SMT1STAT.7;
  SMT1STAT_CPWUP         : bit  absolute SMT1STAT.6;
  SMT1STAT_RST           : bit  absolute SMT1STAT.5;
  SMT1STAT_TS            : bit  absolute SMT1STAT.4;
  SMT1STAT_WS            : bit  absolute SMT1STAT.3;
  SMT1STAT_AS            : bit  absolute SMT1STAT.2;
  SMT1CLK                : byte absolute $049B;
  SMT1CLK_CSEL2          : bit  absolute SMT1CLK.3;
  SMT1CLK_CSEL1          : bit  absolute SMT1CLK.2;
  SMT1CLK_CSEL0          : bit  absolute SMT1CLK.1;
  SMT1CLK_SMT1CSEL0      : bit  absolute SMT1CLK.0;
  SMT1SIG                : byte absolute $049C;
  SMT1SIG_SSEL4          : bit  absolute SMT1SIG.4;
  SMT1SIG_SSEL3          : bit  absolute SMT1SIG.3;
  SMT1SIG_SSEL2          : bit  absolute SMT1SIG.2;
  SMT1SIG_SSEL1          : bit  absolute SMT1SIG.1;
  SMT1SIG_SSEL0          : bit  absolute SMT1SIG.0;
  SMT1WIN                : byte absolute $049D;
  SMT1WIN_WSEL4          : bit  absolute SMT1WIN.4;
  SMT1WIN_WSEL3          : bit  absolute SMT1WIN.3;
  SMT1WIN_WSEL2          : bit  absolute SMT1WIN.2;
  SMT1WIN_WSEL1          : bit  absolute SMT1WIN.1;
  SMT1WIN_WSEL0          : bit  absolute SMT1WIN.0;
  TMR0L                  : byte absolute $059C;
  TMR0L_TMR0L7           : bit  absolute TMR0L.7;
  TMR0L_TMR0L6           : bit  absolute TMR0L.6;
  TMR0L_TMR0L5           : bit  absolute TMR0L.5;
  TMR0L_TMR0L4           : bit  absolute TMR0L.4;
  TMR0L_TMR0L3           : bit  absolute TMR0L.3;
  TMR0L_TMR0L2           : bit  absolute TMR0L.2;
  TMR0L_TMR0L1           : bit  absolute TMR0L.1;
  TMR0L_TMR0L0           : bit  absolute TMR0L.0;
  TMR0H                  : byte absolute $059D;
  TMR0H_TMR0H7           : bit  absolute TMR0H.7;
  TMR0H_TMR0H6           : bit  absolute TMR0H.6;
  TMR0H_TMR0H5           : bit  absolute TMR0H.5;
  TMR0H_TMR0H4           : bit  absolute TMR0H.4;
  TMR0H_TMR0H3           : bit  absolute TMR0H.3;
  TMR0H_TMR0H2           : bit  absolute TMR0H.2;
  TMR0H_TMR0H1           : bit  absolute TMR0H.1;
  TMR0H_TMR0H0           : bit  absolute TMR0H.0;
  T0CON0                 : byte absolute $059E;
  T0CON0_T0EN            : bit  absolute T0CON0.7;
  T0CON0_T0OE            : bit  absolute T0CON0.6;
  T0CON0_T0OUT           : bit  absolute T0CON0.5;
  T0CON0_T016BIT         : bit  absolute T0CON0.4;
  T0CON0_T0OUTPS3        : bit  absolute T0CON0.3;
  T0CON0_T0OUTPS2        : bit  absolute T0CON0.2;
  T0CON0_T0OUTPS1        : bit  absolute T0CON0.1;
  T0CON0_T0OUTPS0        : bit  absolute T0CON0.0;
  T0CON1                 : byte absolute $059F;
  T0CON1_T0CS2           : bit  absolute T0CON1.7;
  T0CON1_T0CS1           : bit  absolute T0CON1.6;
  T0CON1_T0CS0           : bit  absolute T0CON1.5;
  T0CON1_T0ASYNC         : bit  absolute T0CON1.4;
  T0CON1_T0CKPS2         : bit  absolute T0CON1.3;
  T0CON1_T0CKPS1         : bit  absolute T0CON1.2;
  T0CON1_T0CKPS0         : bit  absolute T0CON1.1;
  T0CON1_T0PS0           : bit  absolute T0CON1.0;
  CWG1CLKCON             : byte absolute $060C;
  CWG1ISM                : byte absolute $060D;
  CWG1ISM_CWG1ISM3       : bit  absolute CWG1ISM.3;
  CWG1ISM_CWG1ISM2       : bit  absolute CWG1ISM.2;
  CWG1ISM_CWG1ISM1       : bit  absolute CWG1ISM.1;
  CWG1ISM_CWG1ISM0       : bit  absolute CWG1ISM.0;
  CWG1DBR                : byte absolute $060E;
  CWG1DBR_DBR5           : bit  absolute CWG1DBR.5;
  CWG1DBR_DBR4           : bit  absolute CWG1DBR.4;
  CWG1DBR_DBR3           : bit  absolute CWG1DBR.3;
  CWG1DBR_DBR2           : bit  absolute CWG1DBR.2;
  CWG1DBR_DBR1           : bit  absolute CWG1DBR.1;
  CWG1DBR_DBR0           : bit  absolute CWG1DBR.0;
  CWG1DBF                : byte absolute $060F;
  CWG1DBF_DBF5           : bit  absolute CWG1DBF.5;
  CWG1DBF_DBF4           : bit  absolute CWG1DBF.4;
  CWG1DBF_DBF3           : bit  absolute CWG1DBF.3;
  CWG1DBF_DBF2           : bit  absolute CWG1DBF.2;
  CWG1DBF_DBF1           : bit  absolute CWG1DBF.1;
  CWG1DBF_DBF0           : bit  absolute CWG1DBF.0;
  CWG1CON0               : byte absolute $0610;
  CWG1CON0_LD            : bit  absolute CWG1CON0.6;
  CWG1CON0_CWG1MODE2     : bit  absolute CWG1CON0.2;
  CWG1CON0_CWG1MODE1     : bit  absolute CWG1CON0.1;
  CWG1CON0_CWG1MODE0     : bit  absolute CWG1CON0.0;
  CWG1CON1               : byte absolute $0611;
  CWG1CON1_IN            : bit  absolute CWG1CON1.5;
  CWG1CON1_POLD          : bit  absolute CWG1CON1.4;
  CWG1CON1_POLC          : bit  absolute CWG1CON1.3;
  CWG1CON1_POLB          : bit  absolute CWG1CON1.2;
  CWG1CON1_POLA          : bit  absolute CWG1CON1.1;
  CWG1AS0                : byte absolute $0612;
  CWG1AS0_SHUTDOWN       : bit  absolute CWG1AS0.7;
  CWG1AS0_REN            : bit  absolute CWG1AS0.6;
  CWG1AS0_LSBD1          : bit  absolute CWG1AS0.4;
  CWG1AS0_LSBD0          : bit  absolute CWG1AS0.3;
  CWG1AS0_LSAC1          : bit  absolute CWG1AS0.2;
  CWG1AS0_LSAC0          : bit  absolute CWG1AS0.1;
  CWG1AS1                : byte absolute $0613;
  CWG1AS1_AS6E           : bit  absolute CWG1AS1.6;
  CWG1AS1_AS5E           : bit  absolute CWG1AS1.5;
  CWG1AS1_AS4E           : bit  absolute CWG1AS1.4;
  CWG1AS1_AS3E           : bit  absolute CWG1AS1.3;
  CWG1AS1_AS2E           : bit  absolute CWG1AS1.2;
  CWG1AS1_AS1E           : bit  absolute CWG1AS1.1;
  CWG1AS1_AS0E           : bit  absolute CWG1AS1.0;
  CWG1STR                : byte absolute $0614;
  CWG1STR_OVRD           : bit  absolute CWG1STR.7;
  CWG1STR_OVRC           : bit  absolute CWG1STR.6;
  CWG1STR_OVRB           : bit  absolute CWG1STR.5;
  CWG1STR_OVRA           : bit  absolute CWG1STR.4;
  CWG1STR_STRD           : bit  absolute CWG1STR.3;
  CWG1STR_STRC           : bit  absolute CWG1STR.2;
  CWG1STR_STRB           : bit  absolute CWG1STR.1;
  CWG1STR_STRA           : bit  absolute CWG1STR.0;
  PIR0                   : byte absolute $070C;
  PIR0_TMR0IF            : bit  absolute PIR0.5;
  PIR0_IOCIF             : bit  absolute PIR0.4;
  PIR0_INTF              : bit  absolute PIR0.3;
  PIR1                   : byte absolute $070D;
  PIR1_OSFIF             : bit  absolute PIR1.5;
  PIR1_CSWIF             : bit  absolute PIR1.4;
  PIR1_ADTIF             : bit  absolute PIR1.3;
  PIR1_ADIF              : bit  absolute PIR1.2;
  PIR2                   : byte absolute $070E;
  PIR2_ZCDIF             : bit  absolute PIR2.4;
  PIR2_C2IF              : bit  absolute PIR2.3;
  PIR2_C1IF              : bit  absolute PIR2.2;
  PIR3                   : byte absolute $070F;
  PIR3_RC2IF             : bit  absolute PIR3.7;
  PIR3_TX2IF             : bit  absolute PIR3.6;
  PIR3_RC1IF             : bit  absolute PIR3.5;
  PIR3_TX1IF             : bit  absolute PIR3.4;
  PIR3_BCL1IF            : bit  absolute PIR3.3;
  PIR3_SSP1IF            : bit  absolute PIR3.2;
  PIR4                   : byte absolute $0710;
  PIR4_TMR4IF            : bit  absolute PIR4.3;
  PIR4_TMR2IF            : bit  absolute PIR4.2;
  PIR4_TMR1IF            : bit  absolute PIR4.1;
  PIR5                   : byte absolute $0711;
  PIR5_CLC4IF            : bit  absolute PIR5.7;
  PIR5_CLC3IF            : bit  absolute PIR5.6;
  PIR5_CLC2IF            : bit  absolute PIR5.5;
  PIR5_CLC1IF            : bit  absolute PIR5.4;
  PIR5_TMR1GIF           : bit  absolute PIR5.3;
  PIR6                   : byte absolute $0712;
  PIR6_CRIF              : bit  absolute PIR6.3;
  PIR6_CCP2IF            : bit  absolute PIR6.2;
  PIR6_CCP1IF            : bit  absolute PIR6.1;
  PIR7                   : byte absolute $0713;
  PIR7_NVMIF             : bit  absolute PIR7.5;
  PIR7_CWG1IF            : bit  absolute PIR7.4;
  PIR8                   : byte absolute $0714;
  PIR8_LCDIF             : bit  absolute PIR8.7;
  PIR8_RTCCIF            : bit  absolute PIR8.6;
  PIR8_SMT1PWAIF         : bit  absolute PIR8.5;
  PIR8_SMT1PRAIF         : bit  absolute PIR8.4;
  PIR8_SMT1IF            : bit  absolute PIR8.3;
  PIE0                   : byte absolute $0716;
  PIE0_TMR0IE            : bit  absolute PIE0.5;
  PIE0_IOCIE             : bit  absolute PIE0.4;
  PIE0_INTE              : bit  absolute PIE0.3;
  PIE1                   : byte absolute $0717;
  PIE1_OSFIE             : bit  absolute PIE1.5;
  PIE1_CSWIE             : bit  absolute PIE1.4;
  PIE1_ADTIE             : bit  absolute PIE1.3;
  PIE1_ADIE              : bit  absolute PIE1.2;
  PIE2                   : byte absolute $0718;
  PIE2_ZCDIE             : bit  absolute PIE2.4;
  PIE2_C2IE              : bit  absolute PIE2.3;
  PIE2_C1IE              : bit  absolute PIE2.2;
  PIE3                   : byte absolute $0719;
  PIE3_RC2IE             : bit  absolute PIE3.7;
  PIE3_TX2IE             : bit  absolute PIE3.6;
  PIE3_RC1IE             : bit  absolute PIE3.5;
  PIE3_TX1IE             : bit  absolute PIE3.4;
  PIE3_BCL1IE            : bit  absolute PIE3.3;
  PIE3_SSP1IE            : bit  absolute PIE3.2;
  PIE4                   : byte absolute $071A;
  PIE4_TMR4IE            : bit  absolute PIE4.3;
  PIE4_TMR2IE            : bit  absolute PIE4.2;
  PIE4_TMR1IE            : bit  absolute PIE4.1;
  PIE5                   : byte absolute $071B;
  PIE5_CLC4IE            : bit  absolute PIE5.7;
  PIE5_CLC3IE            : bit  absolute PIE5.6;
  PIE5_CLC2IE            : bit  absolute PIE5.5;
  PIE5_CLC1IE            : bit  absolute PIE5.4;
  PIE5_TMR1GIE           : bit  absolute PIE5.3;
  PIE6                   : byte absolute $071C;
  PIE6_CRIE              : bit  absolute PIE6.6;
  PIE6_CCP2IE            : bit  absolute PIE6.5;
  PIE6_CCP1IE            : bit  absolute PIE6.4;
  PIE7                   : byte absolute $071D;
  PIE7_NVMIE             : bit  absolute PIE7.5;
  PIE7_CWG1IE            : bit  absolute PIE7.4;
  PIE8                   : byte absolute $071E;
  PIE8_LCDIE             : bit  absolute PIE8.7;
  PIE8_RTCCIE            : bit  absolute PIE8.6;
  PIE8_SMT1PWAIE         : bit  absolute PIE8.5;
  PIE8_SMT1PRAIE         : bit  absolute PIE8.4;
  PIE8_SMT1IE            : bit  absolute PIE8.3;
  PMD0                   : byte absolute $0796;
  PMD0_SYSCMD            : bit  absolute PMD0.5;
  PMD0_FVRMD             : bit  absolute PMD0.4;
  PMD0_ACTMD             : bit  absolute PMD0.3;
  PMD0_NVMMD             : bit  absolute PMD0.2;
  PMD0_IOCMD             : bit  absolute PMD0.1;
  PMD1                   : byte absolute $0797;
  PMD1_TMR4MD            : bit  absolute PMD1.4;
  PMD1_TMR2MD            : bit  absolute PMD1.3;
  PMD1_TMR1MD            : bit  absolute PMD1.2;
  PMD1_TMR0MD            : bit  absolute PMD1.1;
  PMD2                   : byte absolute $0798;
  PMD2_RTCCMD            : bit  absolute PMD2.6;
  PMD2_DACMD             : bit  absolute PMD2.5;
  PMD2_ADCMD             : bit  absolute PMD2.4;
  PMD2_CMP2MD            : bit  absolute PMD2.3;
  PMD2_CMP1MD            : bit  absolute PMD2.2;
  PMD2_ZCDMD             : bit  absolute PMD2.1;
  PMD3                   : byte absolute $0799;
  PMD3_CCP4MD            : bit  absolute PMD3.4;
  PMD3_CCP3MD            : bit  absolute PMD3.3;
  PMD3_CCP2MD            : bit  absolute PMD3.2;
  PMD3_CCP1MD            : bit  absolute PMD3.1;
  PMD4                   : byte absolute $079A;
  PMD4_UART2MD           : bit  absolute PMD4.6;
  PMD4_UART1MD           : bit  absolute PMD4.5;
  PMD4_MSSP1MD           : bit  absolute PMD4.4;
  PMD4_CWG1MD            : bit  absolute PMD4.3;
  PMD5                   : byte absolute $079B;
  PMD5_SMT1MD            : bit  absolute PMD5.6;
  PMD5_LCDMD             : bit  absolute PMD5.5;
  PMD5_CLC4MD            : bit  absolute PMD5.4;
  PMD5_CLC3MD            : bit  absolute PMD5.3;
  PMD5_CLC2MD            : bit  absolute PMD5.2;
  PMD5_CLC1MD            : bit  absolute PMD5.1;
  WDTCON0                : byte absolute $080C;
  WDTCON0_WDTSEN         : bit  absolute WDTCON0.5;
  WDTCON0_WDTPS3         : bit  absolute WDTCON0.4;
  WDTCON0_WDTPS2         : bit  absolute WDTCON0.3;
  WDTCON0_SWDTEN         : bit  absolute WDTCON0.1;
  WDTCON1                : byte absolute $080D;
  WDTCON1_WDTCS2         : bit  absolute WDTCON1.6;
  WDTCON1_WDTCS1         : bit  absolute WDTCON1.5;
  WDTCON1_WDTCS0         : bit  absolute WDTCON1.4;
  WDTCON1_WINDOW2        : bit  absolute WDTCON1.3;
  WDTCON1_WINDOW1        : bit  absolute WDTCON1.2;
  WDTCON1_WINDOW0        : bit  absolute WDTCON1.1;
  WDTPSL                 : byte absolute $080E;
  WDTPSL_PSCNT7          : bit  absolute WDTPSL.7;
  WDTPSL_PSCNT6          : bit  absolute WDTPSL.6;
  WDTPSL_PSCNT5          : bit  absolute WDTPSL.5;
  WDTPSL_PSCNT4          : bit  absolute WDTPSL.4;
  WDTPSL_PSCNT3          : bit  absolute WDTPSL.3;
  WDTPSL_PSCNT2          : bit  absolute WDTPSL.2;
  WDTPSL_PSCNT1          : bit  absolute WDTPSL.1;
  WDTPSL_PSCNT0          : bit  absolute WDTPSL.0;
  WDTPSH                 : byte absolute $080F;
  WDTPSH_PSCNT15         : bit  absolute WDTPSH.7;
  WDTPSH_PSCNT14         : bit  absolute WDTPSH.6;
  WDTPSH_PSCNT13         : bit  absolute WDTPSH.5;
  WDTPSH_PSCNT12         : bit  absolute WDTPSH.4;
  WDTPSH_PSCNT11         : bit  absolute WDTPSH.3;
  WDTPSH_PSCNT10         : bit  absolute WDTPSH.2;
  WDTPSH_PSCNT9          : bit  absolute WDTPSH.1;
  WDTPSH_PSCNT8          : bit  absolute WDTPSH.0;
  WDTTMR                 : byte absolute $0810;
  WDTTMR_WDTTMR3         : bit  absolute WDTTMR.7;
  WDTTMR_WDTTMR2         : bit  absolute WDTTMR.6;
  WDTTMR_WDTTMR1         : bit  absolute WDTTMR.5;
  WDTTMR_WDTTMR0         : bit  absolute WDTTMR.4;
  WDTTMR_WDTSTATE        : bit  absolute WDTTMR.3;
  WDTTMR_STATE           : bit  absolute WDTTMR.2;
  WDTTMR_PSCNT17         : bit  absolute WDTTMR.1;
  WDTTMR_PSCNT16         : bit  absolute WDTTMR.0;
  BORCON                 : byte absolute $0811;
  BORCON_SBOREN          : bit  absolute BORCON.7;
  BORCON_BORRDY          : bit  absolute BORCON.6;
  VREGCON                : byte absolute $0812;
  VREGCON_VREGPM         : bit  absolute VREGCON.1;
  PCON0                  : byte absolute $0813;
  PCON0_STKOVF           : bit  absolute PCON0.7;
  PCON0_STKUNF           : bit  absolute PCON0.6;
  PCON0_WDTWV            : bit  absolute PCON0.5;
  PCON0_RWDT             : bit  absolute PCON0.4;
  PCON0_RMCLR            : bit  absolute PCON0.3;
  PCON0_RI               : bit  absolute PCON0.2;
  PCON0_POR              : bit  absolute PCON0.1;
  PCON0_BOR              : bit  absolute PCON0.0;
  PCON1                  : byte absolute $0814;
  PCON1_MEMV             : bit  absolute PCON1.6;
  PCON1_VBATBOR          : bit  absolute PCON1.5;
  NVMADRL                : byte absolute $081A;
  NVMADRL_NVMADR7        : bit  absolute NVMADRL.7;
  NVMADRL_NVMADR6        : bit  absolute NVMADRL.6;
  NVMADRL_NVMADR5        : bit  absolute NVMADRL.5;
  NVMADRL_NVMADR4        : bit  absolute NVMADRL.4;
  NVMADRL_NVMADR3        : bit  absolute NVMADRL.3;
  NVMADRL_NVMADR2        : bit  absolute NVMADRL.2;
  NVMADRL_NVMADR1        : bit  absolute NVMADRL.1;
  NVMADRL_NVMADR0        : bit  absolute NVMADRL.0;
  NVMADRH                : byte absolute $081B;
  NVMADRH_NVMADR14       : bit  absolute NVMADRH.6;
  NVMADRH_NVMADR13       : bit  absolute NVMADRH.5;
  NVMADRH_NVMADR12       : bit  absolute NVMADRH.4;
  NVMADRH_NVMADR11       : bit  absolute NVMADRH.3;
  NVMADRH_NVMADR10       : bit  absolute NVMADRH.2;
  NVMADRH_NVMADR9        : bit  absolute NVMADRH.1;
  NVMADRH_NVMADR8        : bit  absolute NVMADRH.0;
  NVMDATL                : byte absolute $081C;
  NVMDATL_NVMDAT7        : bit  absolute NVMDATL.7;
  NVMDATL_NVMDAT6        : bit  absolute NVMDATL.6;
  NVMDATL_NVMDAT5        : bit  absolute NVMDATL.5;
  NVMDATL_NVMDAT4        : bit  absolute NVMDATL.4;
  NVMDATL_NVMDAT3        : bit  absolute NVMDATL.3;
  NVMDATL_NVMDAT2        : bit  absolute NVMDATL.2;
  NVMDATL_NVMDAT1        : bit  absolute NVMDATL.1;
  NVMDATL_NVMDAT0        : bit  absolute NVMDATL.0;
  NVMDATH                : byte absolute $081D;
  NVMDATH_NVMDAT13       : bit  absolute NVMDATH.5;
  NVMDATH_NVMDAT12       : bit  absolute NVMDATH.4;
  NVMDATH_NVMDAT11       : bit  absolute NVMDATH.3;
  NVMDATH_NVMDAT10       : bit  absolute NVMDATH.2;
  NVMDATH_NVMDAT9        : bit  absolute NVMDATH.1;
  NVMDATH_NVMDAT8        : bit  absolute NVMDATH.0;
  NVMCON1                : byte absolute $081E;
  NVMCON1_NVMREGS        : bit  absolute NVMCON1.6;
  NVMCON1_LWLO           : bit  absolute NVMCON1.5;
  NVMCON1_FREE           : bit  absolute NVMCON1.4;
  NVMCON1_WRERR          : bit  absolute NVMCON1.3;
  NVMCON1_WREN           : bit  absolute NVMCON1.2;
  NVMCON1_WR             : bit  absolute NVMCON1.1;
  NVMCON1_RD             : bit  absolute NVMCON1.0;
  NVMCON2                : byte absolute $081F;
  CPUDOZE                : byte absolute $088C;
  CPUDOZE_IDLEN          : bit  absolute CPUDOZE.7;
  CPUDOZE_DOZEN          : bit  absolute CPUDOZE.6;
  CPUDOZE_ROI            : bit  absolute CPUDOZE.5;
  CPUDOZE_DOE            : bit  absolute CPUDOZE.4;
  CPUDOZE_DOZE2          : bit  absolute CPUDOZE.3;
  CPUDOZE_DOZE1          : bit  absolute CPUDOZE.2;
  CPUDOZE_DOZE0          : bit  absolute CPUDOZE.1;
  OSCCON1                : byte absolute $088D;
  OSCCON1_NOSC2          : bit  absolute OSCCON1.6;
  OSCCON1_NOSC1          : bit  absolute OSCCON1.5;
  OSCCON1_NOSC0          : bit  absolute OSCCON1.4;
  OSCCON1_NDIV3          : bit  absolute OSCCON1.3;
  OSCCON1_NDIV2          : bit  absolute OSCCON1.2;
  OSCCON1_NDIV1          : bit  absolute OSCCON1.1;
  OSCCON1_NDIV0          : bit  absolute OSCCON1.0;
  OSCCON2                : byte absolute $088E;
  OSCCON2_COSC2          : bit  absolute OSCCON2.6;
  OSCCON2_COSC1          : bit  absolute OSCCON2.5;
  OSCCON2_COSC0          : bit  absolute OSCCON2.4;
  OSCCON2_CDIV3          : bit  absolute OSCCON2.3;
  OSCCON2_CDIV2          : bit  absolute OSCCON2.2;
  OSCCON2_CDIV1          : bit  absolute OSCCON2.1;
  OSCCON2_CDIV0          : bit  absolute OSCCON2.0;
  OSCCON3                : byte absolute $088F;
  OSCCON3_CSWHOLD        : bit  absolute OSCCON3.6;
  OSCCON3_SOSCPWR        : bit  absolute OSCCON3.5;
  OSCCON3_ORDY           : bit  absolute OSCCON3.4;
  OSCCON3_NOSCR          : bit  absolute OSCCON3.3;
  OSCSTAT                : byte absolute $0890;
  OSCSTAT_EXTOR          : bit  absolute OSCSTAT.7;
  OSCSTAT_HFOR           : bit  absolute OSCSTAT.6;
  OSCSTAT_MFOR           : bit  absolute OSCSTAT.5;
  OSCSTAT_LFOR           : bit  absolute OSCSTAT.4;
  OSCSTAT_SOR            : bit  absolute OSCSTAT.3;
  OSCSTAT_ADOR           : bit  absolute OSCSTAT.2;
  OSCSTAT_PLLR           : bit  absolute OSCSTAT.1;
  OSCEN                  : byte absolute $0891;
  OSCEN_EXTOEN           : bit  absolute OSCEN.7;
  OSCEN_HFOEN            : bit  absolute OSCEN.6;
  OSCEN_MFOEN            : bit  absolute OSCEN.5;
  OSCEN_LFOEN            : bit  absolute OSCEN.4;
  OSCEN_SOSCEN           : bit  absolute OSCEN.3;
  OSCEN_ADOEN            : bit  absolute OSCEN.2;
  OSCTUNE                : byte absolute $0892;
  OSCTUNE_TUN5           : bit  absolute OSCTUNE.5;
  OSCTUNE_TUN4           : bit  absolute OSCTUNE.4;
  OSCTUNE_TUN3           : bit  absolute OSCTUNE.3;
  OSCTUNE_TUN2           : bit  absolute OSCTUNE.2;
  OSCTUNE_TUN1           : bit  absolute OSCTUNE.1;
  OSCTUNE_TUN0           : bit  absolute OSCTUNE.0;
  OSCFRQ                 : byte absolute $0893;
  OSCFRQ_HFFRQ2          : bit  absolute OSCFRQ.2;
  OSCFRQ_HFFRQ1          : bit  absolute OSCFRQ.1;
  OSCFRQ_HFFRQ0          : bit  absolute OSCFRQ.0;
  ACTCON                 : byte absolute $0894;
  ACTCON_ACTEN           : bit  absolute ACTCON.4;
  ACTCON_ACTUD           : bit  absolute ACTCON.3;
  ACTCON_ACTLOCK         : bit  absolute ACTCON.2;
  ACTCON_ACTORS          : bit  absolute ACTCON.1;
  FVRCON                 : byte absolute $090C;
  FVRCON_FVREN           : bit  absolute FVRCON.7;
  FVRCON_FVRRDY          : bit  absolute FVRCON.6;
  FVRCON_TSEN            : bit  absolute FVRCON.5;
  FVRCON_TSRNG           : bit  absolute FVRCON.4;
  FVRCON_CDAFVR1         : bit  absolute FVRCON.3;
  FVRCON_CDAFVR0         : bit  absolute FVRCON.2;
  FVRCON_ADFVR1          : bit  absolute FVRCON.1;
  FVRCON_ADFVR0          : bit  absolute FVRCON.0;
  DAC1CON0               : byte absolute $090E;
  DAC1CON0_OE1           : bit  absolute DAC1CON0.5;
  DAC1CON0_OE2           : bit  absolute DAC1CON0.4;
  DAC1CON0_DAC1PSS1      : bit  absolute DAC1CON0.3;
  DAC1CON0_DAC1PSS0      : bit  absolute DAC1CON0.2;
  DAC1CON1               : byte absolute $090F;
  DAC1CON1_DAC1R4        : bit  absolute DAC1CON1.4;
  DAC1CON1_DAC1R3        : bit  absolute DAC1CON1.3;
  DAC1CON1_DAC1R2        : bit  absolute DAC1CON1.2;
  DAC1CON1_DAC1R1        : bit  absolute DAC1CON1.1;
  DAC1CON1_DAC1R0        : bit  absolute DAC1CON1.0;
  ZCDCON                 : byte absolute $091F;
  ZCDCON_ZCDSEN          : bit  absolute ZCDCON.6;
  ZCDCON_ZCDOUT          : bit  absolute ZCDCON.5;
  ZCDCON_ZCDPOL          : bit  absolute ZCDCON.4;
  ZCDCON_ZCDINTP         : bit  absolute ZCDCON.3;
  ZCDCON_ZCDINTN         : bit  absolute ZCDCON.2;
  CMOUT                  : byte absolute $098F;
  CMOUT_MC2OUT           : bit  absolute CMOUT.1;
  CMOUT_MC1OUT           : bit  absolute CMOUT.0;
  CM1CON0                : byte absolute $0990;
  CM1CON0_HYS            : bit  absolute CM1CON0.2;
  CM1CON1                : byte absolute $0991;
  CM1CON1_INTP           : bit  absolute CM1CON1.2;
  CM1CON1_INTN           : bit  absolute CM1CON1.1;
  CM1NSEL                : byte absolute $0992;
  CM1NSEL_NCH2           : bit  absolute CM1NSEL.2;
  CM1NSEL_NCH1           : bit  absolute CM1NSEL.1;
  CM1NSEL_NCH0           : bit  absolute CM1NSEL.0;
  CM1PSEL                : byte absolute $0993;
  CM1PSEL_PCH2           : bit  absolute CM1PSEL.3;
  CM1PSEL_PCH1           : bit  absolute CM1PSEL.2;
  CM1PSEL_PCH0           : bit  absolute CM1PSEL.1;
  CM2CON0                : byte absolute $0994;
  CM2CON1                : byte absolute $0995;
  CM2NSEL                : byte absolute $0996;
  CM2PSEL                : byte absolute $0997;
  RC2REG                 : byte absolute $0A19;
  TX2REG                 : byte absolute $0A1A;
  SP2BRGL                : byte absolute $0A1B;
  SP2BRGH                : byte absolute $0A1C;
  RC2STA                 : byte absolute $0A1D;
  TX2STA                 : byte absolute $0A1E;
  BAUD2CON               : byte absolute $0A1F;
  RTCCON                 : byte absolute $0C0C;
  RTCCON_RTCEN           : bit  absolute RTCCON.6;
  RTCCON_RTCWREN         : bit  absolute RTCCON.5;
  RTCCON_RTCSYNC         : bit  absolute RTCCON.4;
  RTCCON_HALFSEC         : bit  absolute RTCCON.3;
  RTCCON_RTCCLKSEL1      : bit  absolute RTCCON.1;
  RTCCON_RTCCLKSEL0      : bit  absolute RTCCON.0;
  RTCCAL                 : byte absolute $0C0D;
  RTCCAL_CAL7            : bit  absolute RTCCAL.7;
  RTCCAL_CAL6            : bit  absolute RTCCAL.6;
  RTCCAL_CAL5            : bit  absolute RTCCAL.5;
  RTCCAL_CAL4            : bit  absolute RTCCAL.4;
  RTCCAL_CAL3            : bit  absolute RTCCAL.3;
  RTCCAL_CAL2            : bit  absolute RTCCAL.2;
  RTCCAL_CAL1            : bit  absolute RTCCAL.1;
  RTCCAL_CAL0            : bit  absolute RTCCAL.0;
  ALRMCON                : byte absolute $0C0E;
  ALRMCON_ALRMEN         : bit  absolute ALRMCON.7;
  ALRMCON_CHIME          : bit  absolute ALRMCON.6;
  ALRMCON_AMASK3         : bit  absolute ALRMCON.5;
  ALRMCON_AMASK2         : bit  absolute ALRMCON.4;
  ALRMCON_AMASK1         : bit  absolute ALRMCON.3;
  ALRMCON_AMASK0         : bit  absolute ALRMCON.2;
  ALRMRPT                : byte absolute $0C0F;
  ALRMRPT_ARPT7          : bit  absolute ALRMRPT.7;
  ALRMRPT_ARPT6          : bit  absolute ALRMRPT.6;
  ALRMRPT_ARPT5          : bit  absolute ALRMRPT.5;
  ALRMRPT_ARPT4          : bit  absolute ALRMRPT.4;
  ALRMRPT_ARPT3          : bit  absolute ALRMRPT.3;
  ALRMRPT_ARPT2          : bit  absolute ALRMRPT.2;
  ALRMRPT_ARPT1          : bit  absolute ALRMRPT.1;
  ALRMRPT_ARPT0          : bit  absolute ALRMRPT.0;
  YEAR                   : byte absolute $0C10;
  YEAR_YEARH3            : bit  absolute YEAR.7;
  YEAR_YEARH2            : bit  absolute YEAR.6;
  YEAR_YEARH1            : bit  absolute YEAR.5;
  YEAR_YEARH0            : bit  absolute YEAR.4;
  YEAR_YEARL3            : bit  absolute YEAR.3;
  YEAR_YEARL2            : bit  absolute YEAR.2;
  YEAR_YEARL1            : bit  absolute YEAR.1;
  YEAR_YEARL0            : bit  absolute YEAR.0;
  MONTH                  : byte absolute $0C11;
  MONTH_MONTHH           : bit  absolute MONTH.4;
  MONTH_MONTHL3          : bit  absolute MONTH.3;
  MONTH_MONTHL2          : bit  absolute MONTH.2;
  MONTH_MONTHL1          : bit  absolute MONTH.1;
  MONTH_MONTHL0          : bit  absolute MONTH.0;
  WEEKDAY                : byte absolute $0C12;
  WEEKDAY_WDAY2          : bit  absolute WEEKDAY.2;
  WEEKDAY_WDAY1          : bit  absolute WEEKDAY.1;
  WEEKDAY_WDAY0          : bit  absolute WEEKDAY.0;
  DAY                    : byte absolute $0C13;
  DAY_DAYH1              : bit  absolute DAY.5;
  DAY_DAYH0              : bit  absolute DAY.4;
  DAY_DAYL3              : bit  absolute DAY.3;
  DAY_DAYL2              : bit  absolute DAY.2;
  DAY_DAYL1              : bit  absolute DAY.1;
  DAY_DAYL0              : bit  absolute DAY.0;
  HOURS                  : byte absolute $0C14;
  HOURS_HRH1             : bit  absolute HOURS.5;
  HOURS_HRH0             : bit  absolute HOURS.4;
  HOURS_HRL3             : bit  absolute HOURS.3;
  HOURS_HRL2             : bit  absolute HOURS.2;
  HOURS_HRL1             : bit  absolute HOURS.1;
  HOURS_HRL0             : bit  absolute HOURS.0;
  MINUTES                : byte absolute $0C15;
  MINUTES_MINH2          : bit  absolute MINUTES.6;
  MINUTES_MINH1          : bit  absolute MINUTES.5;
  MINUTES_MINH0          : bit  absolute MINUTES.4;
  MINUTES_MINL3          : bit  absolute MINUTES.3;
  MINUTES_MINL2          : bit  absolute MINUTES.2;
  MINUTES_MINL1          : bit  absolute MINUTES.1;
  MINUTES_MINL0          : bit  absolute MINUTES.0;
  SECONDS                : byte absolute $0C16;
  SECONDS_SECH2          : bit  absolute SECONDS.6;
  SECONDS_SECH1          : bit  absolute SECONDS.5;
  SECONDS_SECH0          : bit  absolute SECONDS.4;
  SECONDS_SECL3          : bit  absolute SECONDS.3;
  SECONDS_SECL2          : bit  absolute SECONDS.2;
  SECONDS_SECL1          : bit  absolute SECONDS.1;
  SECONDS_SECL0          : bit  absolute SECONDS.0;
  ALRMMTH                : byte absolute $0C17;
  ALRMMTH_MTHALRMH0      : bit  absolute ALRMMTH.4;
  ALRMMTH_MTHALRML3      : bit  absolute ALRMMTH.3;
  ALRMMTH_MTHALRML2      : bit  absolute ALRMMTH.2;
  ALRMMTH_MTHALRML1      : bit  absolute ALRMMTH.1;
  ALRMMTH_MTHALRML0      : bit  absolute ALRMMTH.0;
  ALRMWD                 : byte absolute $0C18;
  ALRMWD_WDALRM2         : bit  absolute ALRMWD.2;
  ALRMWD_WDALRM1         : bit  absolute ALRMWD.1;
  ALRMWD_WDALRM0         : bit  absolute ALRMWD.0;
  ALRMDAY                : byte absolute $0C19;
  ALRMDAY_DAYALRMH1      : bit  absolute ALRMDAY.5;
  ALRMDAY_DAYALRMH0      : bit  absolute ALRMDAY.4;
  ALRMDAY_DAYALRML3      : bit  absolute ALRMDAY.3;
  ALRMDAY_DAYALRML2      : bit  absolute ALRMDAY.2;
  ALRMDAY_DAYALRML1      : bit  absolute ALRMDAY.1;
  ALRMDAY_DAYALRML0      : bit  absolute ALRMDAY.0;
  ALRMHR                 : byte absolute $0C1A;
  ALRMHR_HRALRMH1        : bit  absolute ALRMHR.5;
  ALRMHR_HRALRMH0        : bit  absolute ALRMHR.4;
  ALRMHR_HRALRML3        : bit  absolute ALRMHR.3;
  ALRMHR_HRALRML2        : bit  absolute ALRMHR.2;
  ALRMHR_HRALRML1        : bit  absolute ALRMHR.1;
  ALRMHR_HRALRML0        : bit  absolute ALRMHR.0;
  ALRMMIN                : byte absolute $0C1B;
  ALRMMIN_MINALRMH2      : bit  absolute ALRMMIN.6;
  ALRMMIN_MINALRMH1      : bit  absolute ALRMMIN.5;
  ALRMMIN_MINALRMH0      : bit  absolute ALRMMIN.4;
  ALRMMIN_MINALRML3      : bit  absolute ALRMMIN.3;
  ALRMMIN_MINALRML2      : bit  absolute ALRMMIN.2;
  ALRMMIN_MINALRML1      : bit  absolute ALRMMIN.1;
  ALRMMIN_MINALRML0      : bit  absolute ALRMMIN.0;
  ALRMSEC                : byte absolute $0C1C;
  ALRMSEC_SECALRMH2      : bit  absolute ALRMSEC.6;
  ALRMSEC_SECALRMH1      : bit  absolute ALRMSEC.5;
  ALRMSEC_SECALRMH0      : bit  absolute ALRMSEC.4;
  ALRMSEC_SECALRML3      : bit  absolute ALRMSEC.3;
  ALRMSEC_SECALRML2      : bit  absolute ALRMSEC.2;
  ALRMSEC_SECALRML1      : bit  absolute ALRMSEC.1;
  ALRMSEC_SECALRML0      : bit  absolute ALRMSEC.0;
  PORTG                  : byte absolute $0C8C;
  PORTG_RG7              : bit  absolute PORTG.7;
  PORTG_RG6              : bit  absolute PORTG.6;
  PORTG_RG5              : bit  absolute PORTG.5;
  PORTG_RG4              : bit  absolute PORTG.4;
  PORTG_RG3              : bit  absolute PORTG.3;
  PORTG_RG2              : bit  absolute PORTG.2;
  PORTG_RG1              : bit  absolute PORTG.1;
  PORTG_RG0              : bit  absolute PORTG.0;
  PORTH                  : byte absolute $0C8D;
  PORTH_RH3              : bit  absolute PORTH.3;
  PORTH_RH2              : bit  absolute PORTH.2;
  PORTH_RH1              : bit  absolute PORTH.1;
  PORTH_RH0              : bit  absolute PORTH.0;
  TRISG                  : byte absolute $0C8E;
  TRISG_TRISG7           : bit  absolute TRISG.7;
  TRISG_TRISG6           : bit  absolute TRISG.6;
  TRISG_TRISG4           : bit  absolute TRISG.5;
  TRISG_TRISG3           : bit  absolute TRISG.4;
  TRISG_TRISG2           : bit  absolute TRISG.3;
  TRISG_TRISG1           : bit  absolute TRISG.2;
  TRISG_TRISG0           : bit  absolute TRISG.1;
  TRISH                  : byte absolute $0C8F;
  TRISH_TRISH3           : bit  absolute TRISH.3;
  TRISH_TRISH2           : bit  absolute TRISH.2;
  TRISH_TRISH1           : bit  absolute TRISH.1;
  TRISH_TRISH0           : bit  absolute TRISH.0;
  LATG                   : byte absolute $0C90;
  LATG_LATG7             : bit  absolute LATG.7;
  LATG_LATG6             : bit  absolute LATG.6;
  LATG_LATG4             : bit  absolute LATG.5;
  LATG_LATG3             : bit  absolute LATG.4;
  LATG_LATG2             : bit  absolute LATG.3;
  LATG_LATG1             : bit  absolute LATG.2;
  LATG_LATG0             : bit  absolute LATG.1;
  LATH                   : byte absolute $0C91;
  LATH_LATH3             : bit  absolute LATH.3;
  LATH_LATH2             : bit  absolute LATH.2;
  LATH_LATH1             : bit  absolute LATH.1;
  LATH_LATH0             : bit  absolute LATH.0;
  VB0GPR                 : byte absolute $0E8C;
  VB0GPR_VB0GPR7         : bit  absolute VB0GPR.7;
  VB0GPR_VB0GPR6         : bit  absolute VB0GPR.6;
  VB0GPR_VB0GPR5         : bit  absolute VB0GPR.5;
  VB0GPR_VB0GPR4         : bit  absolute VB0GPR.4;
  VB0GPR_VB0GPR3         : bit  absolute VB0GPR.3;
  VB0GPR_VB0GPR2         : bit  absolute VB0GPR.2;
  VB0GPR_VB0GPR1         : bit  absolute VB0GPR.1;
  VB0GPR_VB0GPR0         : bit  absolute VB0GPR.0;
  VB1GPR                 : byte absolute $0E8D;
  VB1GPR_VB1GPR7         : bit  absolute VB1GPR.7;
  VB1GPR_VB1GPR6         : bit  absolute VB1GPR.6;
  VB1GPR_VB1GPR5         : bit  absolute VB1GPR.5;
  VB1GPR_VB1GPR4         : bit  absolute VB1GPR.4;
  VB1GPR_VB1GPR3         : bit  absolute VB1GPR.3;
  VB1GPR_VB1GPR2         : bit  absolute VB1GPR.2;
  VB1GPR_VB1GPR1         : bit  absolute VB1GPR.1;
  VB1GPR_VB1GPR0         : bit  absolute VB1GPR.0;
  VB2GPR                 : byte absolute $0E8E;
  VB2GPR_VB2GPR7         : bit  absolute VB2GPR.7;
  VB2GPR_VB2GPR6         : bit  absolute VB2GPR.6;
  VB2GPR_VB2GPR5         : bit  absolute VB2GPR.5;
  VB2GPR_VB2GPR4         : bit  absolute VB2GPR.4;
  VB2GPR_VB2GPR3         : bit  absolute VB2GPR.3;
  VB2GPR_VB2GPR2         : bit  absolute VB2GPR.2;
  VB2GPR_VB2GPR1         : bit  absolute VB2GPR.1;
  VB2GPR_VB2GPR0         : bit  absolute VB2GPR.0;
  VB3GPR                 : byte absolute $0E8F;
  VB3GPR_VB3GPR7         : bit  absolute VB3GPR.7;
  VB3GPR_VB3GPR6         : bit  absolute VB3GPR.6;
  VB3GPR_VB3GPR5         : bit  absolute VB3GPR.5;
  VB3GPR_VB3GPR4         : bit  absolute VB3GPR.4;
  VB3GPR_VB3GPR3         : bit  absolute VB3GPR.3;
  VB3GPR_VB3GPR2         : bit  absolute VB3GPR.2;
  VB3GPR_VB3GPR1         : bit  absolute VB3GPR.1;
  VB3GPR_VB3GPR0         : bit  absolute VB3GPR.0;
  LCDCON                 : byte absolute $1D0C;
  LCDCON_LCDEN           : bit  absolute LCDCON.7;
  LCDCON_SLPEN           : bit  absolute LCDCON.6;
  LCDCON_WERR            : bit  absolute LCDCON.5;
  LCDCON_LMUX3           : bit  absolute LCDCON.3;
  LCDCON_LMUX2           : bit  absolute LCDCON.2;
  LCDCON_LMUX1           : bit  absolute LCDCON.1;
  LCDCON_LMUX0           : bit  absolute LCDCON.0;
  LCDPS                  : byte absolute $1D0D;
  LCDPS_WFT              : bit  absolute LCDPS.7;
  LCDPS_LCDA             : bit  absolute LCDPS.6;
  LCDPS_WA               : bit  absolute LCDPS.5;
  LCDPS_LP3              : bit  absolute LCDPS.3;
  LCDPS_LP2              : bit  absolute LCDPS.2;
  LCDPS_LP1              : bit  absolute LCDPS.1;
  LCDPS_LP0              : bit  absolute LCDPS.0;
  LCDSE0                 : byte absolute $1D0E;
  LCDSE0_SE07            : bit  absolute LCDSE0.7;
  LCDSE0_SE06            : bit  absolute LCDSE0.6;
  LCDSE0_SE05            : bit  absolute LCDSE0.5;
  LCDSE0_SE04            : bit  absolute LCDSE0.4;
  LCDSE0_SE03            : bit  absolute LCDSE0.3;
  LCDSE0_SE02            : bit  absolute LCDSE0.2;
  LCDSE0_SE01            : bit  absolute LCDSE0.1;
  LCDSE0_SE00            : bit  absolute LCDSE0.0;
  LCDSE1                 : byte absolute $1D0F;
  LCDSE1_SE15            : bit  absolute LCDSE1.7;
  LCDSE1_SE14            : bit  absolute LCDSE1.6;
  LCDSE1_SE13            : bit  absolute LCDSE1.5;
  LCDSE1_SE12            : bit  absolute LCDSE1.4;
  LCDSE1_SE11            : bit  absolute LCDSE1.3;
  LCDSE1_SE10            : bit  absolute LCDSE1.2;
  LCDSE1_SE09            : bit  absolute LCDSE1.1;
  LCDSE1_SE08            : bit  absolute LCDSE1.0;
  LCDSE2                 : byte absolute $1D10;
  LCDSE2_SE23            : bit  absolute LCDSE2.7;
  LCDSE2_SE22            : bit  absolute LCDSE2.6;
  LCDSE2_SE21            : bit  absolute LCDSE2.5;
  LCDSE2_SE20            : bit  absolute LCDSE2.4;
  LCDSE2_SE19            : bit  absolute LCDSE2.3;
  LCDSE2_SE18            : bit  absolute LCDSE2.2;
  LCDSE2_SE17            : bit  absolute LCDSE2.1;
  LCDSE2_SE16            : bit  absolute LCDSE2.0;
  LCDSE3                 : byte absolute $1D11;
  LCDSE3_SE31            : bit  absolute LCDSE3.7;
  LCDSE3_SE30            : bit  absolute LCDSE3.6;
  LCDSE3_SE29            : bit  absolute LCDSE3.5;
  LCDSE3_SE28            : bit  absolute LCDSE3.4;
  LCDSE3_SE27            : bit  absolute LCDSE3.3;
  LCDSE3_SE26            : bit  absolute LCDSE3.2;
  LCDSE3_SE25            : bit  absolute LCDSE3.1;
  LCDSE3_SE24            : bit  absolute LCDSE3.0;
  LCDSE4                 : byte absolute $1D12;
  LCDSE4_SE39            : bit  absolute LCDSE4.7;
  LCDSE4_SE38            : bit  absolute LCDSE4.6;
  LCDSE4_SE37            : bit  absolute LCDSE4.5;
  LCDSE4_SE36            : bit  absolute LCDSE4.4;
  LCDSE4_SE35            : bit  absolute LCDSE4.3;
  LCDSE4_SE34            : bit  absolute LCDSE4.2;
  LCDSE4_SE33            : bit  absolute LCDSE4.1;
  LCDSE4_SE32            : bit  absolute LCDSE4.0;
  LCDSE5                 : byte absolute $1D13;
  LCDSE5_SE47            : bit  absolute LCDSE5.7;
  LCDSE5_SE46            : bit  absolute LCDSE5.6;
  LCDSE5_SE45            : bit  absolute LCDSE5.5;
  LCDSE5_SE44            : bit  absolute LCDSE5.4;
  LCDSE5_SE43            : bit  absolute LCDSE5.3;
  LCDSE5_SE42            : bit  absolute LCDSE5.2;
  LCDSE5_SE41            : bit  absolute LCDSE5.1;
  LCDSE5_SE40            : bit  absolute LCDSE5.0;
  LCDVCON1               : byte absolute $1D14;
  LCDVCON1_LPEN          : bit  absolute LCDVCON1.7;
  LCDVCON1_EN5V          : bit  absolute LCDVCON1.6;
  LCDVCON1_BIAS2         : bit  absolute LCDVCON1.2;
  LCDVCON1_BIAS1         : bit  absolute LCDVCON1.1;
  LCDVCON1_BIAS0         : bit  absolute LCDVCON1.0;
  LCDVCON2               : byte absolute $1D15;
  LCDVCON2_CPWDT         : bit  absolute LCDVCON2.7;
  LCDVCON2_LCDVSRC3      : bit  absolute LCDVCON2.3;
  LCDVCON2_LCDVSRC2      : bit  absolute LCDVCON2.2;
  LCDVCON2_LCDVSRC1      : bit  absolute LCDVCON2.1;
  LCDVCON2_LCDVSRC0      : bit  absolute LCDVCON2.0;
  LCDREF                 : byte absolute $1D16;
  LCDREF_LCDCST2         : bit  absolute LCDREF.2;
  LCDREF_LCDCST1         : bit  absolute LCDREF.1;
  LCDREF_LCDCST0         : bit  absolute LCDREF.0;
  LCDRL                  : byte absolute $1D17;
  LCDRL_LRLAP1           : bit  absolute LCDRL.7;
  LCDRL_LRLAP0           : bit  absolute LCDRL.6;
  LCDRL_LRLBP1           : bit  absolute LCDRL.5;
  LCDRL_LRLBP0           : bit  absolute LCDRL.4;
  LCDRL_LCDIRI           : bit  absolute LCDRL.3;
  LCDRL_LRLAT1           : bit  absolute LCDRL.2;
  LCDRL_LRLAT0           : bit  absolute LCDRL.1;
  LCDDATA0               : byte absolute $1D18;
  LCDDATA0_S07C0         : bit  absolute LCDDATA0.7;
  LCDDATA0_S06C0         : bit  absolute LCDDATA0.6;
  LCDDATA0_S05C0         : bit  absolute LCDDATA0.5;
  LCDDATA0_S04C0         : bit  absolute LCDDATA0.4;
  LCDDATA0_S03C0         : bit  absolute LCDDATA0.3;
  LCDDATA0_S02C0         : bit  absolute LCDDATA0.2;
  LCDDATA0_S01C0         : bit  absolute LCDDATA0.1;
  LCDDATA0_S00C0         : bit  absolute LCDDATA0.0;
  LCDDATA1               : byte absolute $1D19;
  LCDDATA1_S15C0         : bit  absolute LCDDATA1.7;
  LCDDATA1_S14C0         : bit  absolute LCDDATA1.6;
  LCDDATA1_S13C0         : bit  absolute LCDDATA1.5;
  LCDDATA1_S12C0         : bit  absolute LCDDATA1.4;
  LCDDATA1_S11C0         : bit  absolute LCDDATA1.3;
  LCDDATA1_S10C0         : bit  absolute LCDDATA1.2;
  LCDDATA1_S09C0         : bit  absolute LCDDATA1.1;
  LCDDATA1_S08C0         : bit  absolute LCDDATA1.0;
  LCDDATA2               : byte absolute $1D1A;
  LCDDATA2_S23C0         : bit  absolute LCDDATA2.7;
  LCDDATA2_S22C0         : bit  absolute LCDDATA2.6;
  LCDDATA2_S21C0         : bit  absolute LCDDATA2.5;
  LCDDATA2_S20C0         : bit  absolute LCDDATA2.4;
  LCDDATA2_S19C0         : bit  absolute LCDDATA2.3;
  LCDDATA2_S18C0         : bit  absolute LCDDATA2.2;
  LCDDATA2_S17C0         : bit  absolute LCDDATA2.1;
  LCDDATA2_S16C0         : bit  absolute LCDDATA2.0;
  LCDDATA3               : byte absolute $1D1B;
  LCDDATA3_S31C0         : bit  absolute LCDDATA3.7;
  LCDDATA3_S30C0         : bit  absolute LCDDATA3.6;
  LCDDATA3_S29C0         : bit  absolute LCDDATA3.5;
  LCDDATA3_S28C0         : bit  absolute LCDDATA3.4;
  LCDDATA3_S27C0         : bit  absolute LCDDATA3.3;
  LCDDATA3_S26C0         : bit  absolute LCDDATA3.2;
  LCDDATA3_S25C0         : bit  absolute LCDDATA3.1;
  LCDDATA3_S24C0         : bit  absolute LCDDATA3.0;
  LCDDATA4               : byte absolute $1D1C;
  LCDDATA4_S39C0         : bit  absolute LCDDATA4.7;
  LCDDATA4_S38C0         : bit  absolute LCDDATA4.6;
  LCDDATA4_S37C0         : bit  absolute LCDDATA4.5;
  LCDDATA4_S36C0         : bit  absolute LCDDATA4.4;
  LCDDATA4_S35C0         : bit  absolute LCDDATA4.3;
  LCDDATA4_S34C0         : bit  absolute LCDDATA4.2;
  LCDDATA4_S33C0         : bit  absolute LCDDATA4.1;
  LCDDATA4_S32C0         : bit  absolute LCDDATA4.0;
  LCDDATA5               : byte absolute $1D1D;
  LCDDATA5_S45C0         : bit  absolute LCDDATA5.6;
  LCDDATA5_S44C0         : bit  absolute LCDDATA5.5;
  LCDDATA5_S43C0         : bit  absolute LCDDATA5.4;
  LCDDATA5_S42C0         : bit  absolute LCDDATA5.3;
  LCDDATA5_S41C0         : bit  absolute LCDDATA5.2;
  LCDDATA5_S40C0         : bit  absolute LCDDATA5.1;
  LCDDATA6               : byte absolute $1D1E;
  LCDDATA6_S07C1         : bit  absolute LCDDATA6.7;
  LCDDATA6_S06C1         : bit  absolute LCDDATA6.6;
  LCDDATA6_S05C1         : bit  absolute LCDDATA6.5;
  LCDDATA6_S04C1         : bit  absolute LCDDATA6.4;
  LCDDATA6_S03C1         : bit  absolute LCDDATA6.3;
  LCDDATA6_S02C1         : bit  absolute LCDDATA6.2;
  LCDDATA6_S01C1         : bit  absolute LCDDATA6.1;
  LCDDATA6_S00C1         : bit  absolute LCDDATA6.0;
  LCDDATA7               : byte absolute $1D1F;
  LCDDATA7_S15C1         : bit  absolute LCDDATA7.7;
  LCDDATA7_S14C1         : bit  absolute LCDDATA7.6;
  LCDDATA7_S13C1         : bit  absolute LCDDATA7.5;
  LCDDATA7_S12C1         : bit  absolute LCDDATA7.4;
  LCDDATA7_S11C1         : bit  absolute LCDDATA7.3;
  LCDDATA7_S10C1         : bit  absolute LCDDATA7.2;
  LCDDATA7_S09C1         : bit  absolute LCDDATA7.1;
  LCDDATA7_S08C1         : bit  absolute LCDDATA7.0;
  LCDDATA8               : byte absolute $1D20;
  LCDDATA8_S23C1         : bit  absolute LCDDATA8.7;
  LCDDATA8_S22C1         : bit  absolute LCDDATA8.6;
  LCDDATA8_S21C1         : bit  absolute LCDDATA8.5;
  LCDDATA8_S20C1         : bit  absolute LCDDATA8.4;
  LCDDATA8_S19C1         : bit  absolute LCDDATA8.3;
  LCDDATA8_S18C1         : bit  absolute LCDDATA8.2;
  LCDDATA8_S17C1         : bit  absolute LCDDATA8.1;
  LCDDATA8_S16C1         : bit  absolute LCDDATA8.0;
  LCDDATA9               : byte absolute $1D21;
  LCDDATA9_S31C1         : bit  absolute LCDDATA9.7;
  LCDDATA9_S30C1         : bit  absolute LCDDATA9.6;
  LCDDATA9_S29C1         : bit  absolute LCDDATA9.5;
  LCDDATA9_S28C1         : bit  absolute LCDDATA9.4;
  LCDDATA9_S27C1         : bit  absolute LCDDATA9.3;
  LCDDATA9_S26C1         : bit  absolute LCDDATA9.2;
  LCDDATA9_S25C1         : bit  absolute LCDDATA9.1;
  LCDDATA9_S24C1         : bit  absolute LCDDATA9.0;
  LCDDATA10              : byte absolute $1D22;
  LCDDATA10_S39C1        : bit  absolute LCDDATA10.7;
  LCDDATA10_S38C1        : bit  absolute LCDDATA10.6;
  LCDDATA10_S37C1        : bit  absolute LCDDATA10.5;
  LCDDATA10_S36C1        : bit  absolute LCDDATA10.4;
  LCDDATA10_S35C1        : bit  absolute LCDDATA10.3;
  LCDDATA10_S34C1        : bit  absolute LCDDATA10.2;
  LCDDATA10_S33C1        : bit  absolute LCDDATA10.1;
  LCDDATA10_S32C1        : bit  absolute LCDDATA10.0;
  LCDDATA11              : byte absolute $1D23;
  LCDDATA11_S45C1        : bit  absolute LCDDATA11.6;
  LCDDATA11_S44C1        : bit  absolute LCDDATA11.5;
  LCDDATA11_S43C1        : bit  absolute LCDDATA11.4;
  LCDDATA11_S42C1        : bit  absolute LCDDATA11.3;
  LCDDATA11_S41C1        : bit  absolute LCDDATA11.2;
  LCDDATA11_S40C1        : bit  absolute LCDDATA11.1;
  LCDDATA12              : byte absolute $1D24;
  LCDDATA12_S07C2        : bit  absolute LCDDATA12.7;
  LCDDATA12_S06C2        : bit  absolute LCDDATA12.6;
  LCDDATA12_S05C2        : bit  absolute LCDDATA12.5;
  LCDDATA12_S04C2        : bit  absolute LCDDATA12.4;
  LCDDATA12_S03C2        : bit  absolute LCDDATA12.3;
  LCDDATA12_S02C2        : bit  absolute LCDDATA12.2;
  LCDDATA12_S01C2        : bit  absolute LCDDATA12.1;
  LCDDATA12_S00C2        : bit  absolute LCDDATA12.0;
  LCDDATA13              : byte absolute $1D25;
  LCDDATA13_S15C2        : bit  absolute LCDDATA13.7;
  LCDDATA13_S14C2        : bit  absolute LCDDATA13.6;
  LCDDATA13_S13C2        : bit  absolute LCDDATA13.5;
  LCDDATA13_S12C2        : bit  absolute LCDDATA13.4;
  LCDDATA13_S11C2        : bit  absolute LCDDATA13.3;
  LCDDATA13_S10C2        : bit  absolute LCDDATA13.2;
  LCDDATA13_S09C2        : bit  absolute LCDDATA13.1;
  LCDDATA13_S08C2        : bit  absolute LCDDATA13.0;
  LCDDATA14              : byte absolute $1D26;
  LCDDATA14_S23C2        : bit  absolute LCDDATA14.7;
  LCDDATA14_S22C2        : bit  absolute LCDDATA14.6;
  LCDDATA14_S21C2        : bit  absolute LCDDATA14.5;
  LCDDATA14_S20C2        : bit  absolute LCDDATA14.4;
  LCDDATA14_S19C2        : bit  absolute LCDDATA14.3;
  LCDDATA14_S18C2        : bit  absolute LCDDATA14.2;
  LCDDATA14_S17C2        : bit  absolute LCDDATA14.1;
  LCDDATA14_S16C2        : bit  absolute LCDDATA14.0;
  LCDDATA15              : byte absolute $1D27;
  LCDDATA15_S31C2        : bit  absolute LCDDATA15.7;
  LCDDATA15_S30C2        : bit  absolute LCDDATA15.6;
  LCDDATA15_S29C2        : bit  absolute LCDDATA15.5;
  LCDDATA15_S28C2        : bit  absolute LCDDATA15.4;
  LCDDATA15_S27C2        : bit  absolute LCDDATA15.3;
  LCDDATA15_S26C2        : bit  absolute LCDDATA15.2;
  LCDDATA15_S25C2        : bit  absolute LCDDATA15.1;
  LCDDATA15_S24C2        : bit  absolute LCDDATA15.0;
  LCDDATA16              : byte absolute $1D28;
  LCDDATA16_S39C2        : bit  absolute LCDDATA16.7;
  LCDDATA16_S38C2        : bit  absolute LCDDATA16.6;
  LCDDATA16_S37C2        : bit  absolute LCDDATA16.5;
  LCDDATA16_S36C2        : bit  absolute LCDDATA16.4;
  LCDDATA16_S35C2        : bit  absolute LCDDATA16.3;
  LCDDATA16_S34C2        : bit  absolute LCDDATA16.2;
  LCDDATA16_S33C2        : bit  absolute LCDDATA16.1;
  LCDDATA16_S32C2        : bit  absolute LCDDATA16.0;
  LCDDATA17              : byte absolute $1D29;
  LCDDATA17_S45C2        : bit  absolute LCDDATA17.6;
  LCDDATA17_S44C2        : bit  absolute LCDDATA17.5;
  LCDDATA17_S43C2        : bit  absolute LCDDATA17.4;
  LCDDATA17_S42C2        : bit  absolute LCDDATA17.3;
  LCDDATA17_S41C2        : bit  absolute LCDDATA17.2;
  LCDDATA17_S40C2        : bit  absolute LCDDATA17.1;
  LCDDATA18              : byte absolute $1D2A;
  LCDDATA18_S07C3        : bit  absolute LCDDATA18.7;
  LCDDATA18_S06C3        : bit  absolute LCDDATA18.6;
  LCDDATA18_S05C3        : bit  absolute LCDDATA18.5;
  LCDDATA18_S04C3        : bit  absolute LCDDATA18.4;
  LCDDATA18_S03C3        : bit  absolute LCDDATA18.3;
  LCDDATA18_S02C3        : bit  absolute LCDDATA18.2;
  LCDDATA18_S01C3        : bit  absolute LCDDATA18.1;
  LCDDATA18_S00C3        : bit  absolute LCDDATA18.0;
  LCDDATA19              : byte absolute $1D2B;
  LCDDATA19_S15C3        : bit  absolute LCDDATA19.7;
  LCDDATA19_S14C3        : bit  absolute LCDDATA19.6;
  LCDDATA19_S13C3        : bit  absolute LCDDATA19.5;
  LCDDATA19_S12C3        : bit  absolute LCDDATA19.4;
  LCDDATA19_S11C3        : bit  absolute LCDDATA19.3;
  LCDDATA19_S10C3        : bit  absolute LCDDATA19.2;
  LCDDATA19_S09C3        : bit  absolute LCDDATA19.1;
  LCDDATA19_S08C3        : bit  absolute LCDDATA19.0;
  LCDDATA20              : byte absolute $1D2C;
  LCDDATA20_S23C3        : bit  absolute LCDDATA20.7;
  LCDDATA20_S22C3        : bit  absolute LCDDATA20.6;
  LCDDATA20_S21C3        : bit  absolute LCDDATA20.5;
  LCDDATA20_S20C3        : bit  absolute LCDDATA20.4;
  LCDDATA20_S19C3        : bit  absolute LCDDATA20.3;
  LCDDATA20_S18C3        : bit  absolute LCDDATA20.2;
  LCDDATA20_S17C3        : bit  absolute LCDDATA20.1;
  LCDDATA20_S16C3        : bit  absolute LCDDATA20.0;
  LCDDATA21              : byte absolute $1D2D;
  LCDDATA21_S31C3        : bit  absolute LCDDATA21.7;
  LCDDATA21_S30C3        : bit  absolute LCDDATA21.6;
  LCDDATA21_S29C3        : bit  absolute LCDDATA21.5;
  LCDDATA21_S28C3        : bit  absolute LCDDATA21.4;
  LCDDATA21_S27C3        : bit  absolute LCDDATA21.3;
  LCDDATA21_S26C3        : bit  absolute LCDDATA21.2;
  LCDDATA21_S25C3        : bit  absolute LCDDATA21.1;
  LCDDATA21_S24C3        : bit  absolute LCDDATA21.0;
  LCDDATA22              : byte absolute $1D2E;
  LCDDATA22_S39C3        : bit  absolute LCDDATA22.7;
  LCDDATA22_S38C3        : bit  absolute LCDDATA22.6;
  LCDDATA22_S37C3        : bit  absolute LCDDATA22.5;
  LCDDATA22_S36C3        : bit  absolute LCDDATA22.4;
  LCDDATA22_S35C3        : bit  absolute LCDDATA22.3;
  LCDDATA22_S34C3        : bit  absolute LCDDATA22.2;
  LCDDATA22_S33C3        : bit  absolute LCDDATA22.1;
  LCDDATA22_S32C3        : bit  absolute LCDDATA22.0;
  LCDDATA23              : byte absolute $1D2F;
  LCDDATA23_S45C3        : bit  absolute LCDDATA23.6;
  LCDDATA23_S44C3        : bit  absolute LCDDATA23.5;
  LCDDATA23_S43C3        : bit  absolute LCDDATA23.4;
  LCDDATA23_S42C3        : bit  absolute LCDDATA23.3;
  LCDDATA23_S41C3        : bit  absolute LCDDATA23.2;
  LCDDATA23_S40C3        : bit  absolute LCDDATA23.1;
  LCDDATA24              : byte absolute $1D30;
  LCDDATA24_S07C4        : bit  absolute LCDDATA24.7;
  LCDDATA24_S06C4        : bit  absolute LCDDATA24.6;
  LCDDATA24_S05C4        : bit  absolute LCDDATA24.5;
  LCDDATA24_S04C4        : bit  absolute LCDDATA24.4;
  LCDDATA24_S03C4        : bit  absolute LCDDATA24.3;
  LCDDATA24_S02C4        : bit  absolute LCDDATA24.2;
  LCDDATA24_S01C4        : bit  absolute LCDDATA24.1;
  LCDDATA24_S00C4        : bit  absolute LCDDATA24.0;
  LCDDATA25              : byte absolute $1D31;
  LCDDATA25_S15C4        : bit  absolute LCDDATA25.7;
  LCDDATA25_S14C4        : bit  absolute LCDDATA25.6;
  LCDDATA25_S13C4        : bit  absolute LCDDATA25.5;
  LCDDATA25_S12C4        : bit  absolute LCDDATA25.4;
  LCDDATA25_S11C4        : bit  absolute LCDDATA25.3;
  LCDDATA25_S10C4        : bit  absolute LCDDATA25.2;
  LCDDATA25_S09C4        : bit  absolute LCDDATA25.1;
  LCDDATA25_S08C4        : bit  absolute LCDDATA25.0;
  LCDDATA26              : byte absolute $1D32;
  LCDDATA26_S23C4        : bit  absolute LCDDATA26.7;
  LCDDATA26_S22C4        : bit  absolute LCDDATA26.6;
  LCDDATA26_S21C4        : bit  absolute LCDDATA26.5;
  LCDDATA26_S20C4        : bit  absolute LCDDATA26.4;
  LCDDATA26_S19C4        : bit  absolute LCDDATA26.3;
  LCDDATA26_S18C4        : bit  absolute LCDDATA26.2;
  LCDDATA26_S17C4        : bit  absolute LCDDATA26.1;
  LCDDATA26_S16C4        : bit  absolute LCDDATA26.0;
  LCDDATA27              : byte absolute $1D33;
  LCDDATA27_S31C4        : bit  absolute LCDDATA27.7;
  LCDDATA27_S30C4        : bit  absolute LCDDATA27.6;
  LCDDATA27_S29C4        : bit  absolute LCDDATA27.5;
  LCDDATA27_S28C4        : bit  absolute LCDDATA27.4;
  LCDDATA27_S27C4        : bit  absolute LCDDATA27.3;
  LCDDATA27_S26C4        : bit  absolute LCDDATA27.2;
  LCDDATA27_S25C4        : bit  absolute LCDDATA27.1;
  LCDDATA27_S24C4        : bit  absolute LCDDATA27.0;
  LCDDATA28              : byte absolute $1D34;
  LCDDATA28_S39C4        : bit  absolute LCDDATA28.7;
  LCDDATA28_S38C4        : bit  absolute LCDDATA28.6;
  LCDDATA28_S37C4        : bit  absolute LCDDATA28.5;
  LCDDATA28_S36C4        : bit  absolute LCDDATA28.4;
  LCDDATA28_S35C4        : bit  absolute LCDDATA28.3;
  LCDDATA28_S34C4        : bit  absolute LCDDATA28.2;
  LCDDATA28_S33C4        : bit  absolute LCDDATA28.1;
  LCDDATA28_S32C4        : bit  absolute LCDDATA28.0;
  LCDDATA29              : byte absolute $1D35;
  LCDDATA29_S45C4        : bit  absolute LCDDATA29.6;
  LCDDATA29_S44C4        : bit  absolute LCDDATA29.5;
  LCDDATA29_S43C4        : bit  absolute LCDDATA29.4;
  LCDDATA29_S42C4        : bit  absolute LCDDATA29.3;
  LCDDATA29_S41C4        : bit  absolute LCDDATA29.2;
  LCDDATA29_S40C4        : bit  absolute LCDDATA29.1;
  LCDDATA30              : byte absolute $1D36;
  LCDDATA30_S07C5        : bit  absolute LCDDATA30.7;
  LCDDATA30_S06C5        : bit  absolute LCDDATA30.6;
  LCDDATA30_S05C5        : bit  absolute LCDDATA30.5;
  LCDDATA30_S04C5        : bit  absolute LCDDATA30.4;
  LCDDATA30_S03C5        : bit  absolute LCDDATA30.3;
  LCDDATA30_S02C5        : bit  absolute LCDDATA30.2;
  LCDDATA30_S01C5        : bit  absolute LCDDATA30.1;
  LCDDATA30_S00C5        : bit  absolute LCDDATA30.0;
  LCDDATA31              : byte absolute $1D37;
  LCDDATA31_S15C5        : bit  absolute LCDDATA31.7;
  LCDDATA31_S14C5        : bit  absolute LCDDATA31.6;
  LCDDATA31_S13C5        : bit  absolute LCDDATA31.5;
  LCDDATA31_S12C5        : bit  absolute LCDDATA31.4;
  LCDDATA31_S11C5        : bit  absolute LCDDATA31.3;
  LCDDATA31_S10C5        : bit  absolute LCDDATA31.2;
  LCDDATA31_S09C5        : bit  absolute LCDDATA31.1;
  LCDDATA31_S08C5        : bit  absolute LCDDATA31.0;
  LCDDATA32              : byte absolute $1D38;
  LCDDATA32_S23C5        : bit  absolute LCDDATA32.7;
  LCDDATA32_S22C5        : bit  absolute LCDDATA32.6;
  LCDDATA32_S21C5        : bit  absolute LCDDATA32.5;
  LCDDATA32_S20C5        : bit  absolute LCDDATA32.4;
  LCDDATA32_S19C5        : bit  absolute LCDDATA32.3;
  LCDDATA32_S18C5        : bit  absolute LCDDATA32.2;
  LCDDATA32_S17C5        : bit  absolute LCDDATA32.1;
  LCDDATA32_S16C5        : bit  absolute LCDDATA32.0;
  LCDDATA33              : byte absolute $1D39;
  LCDDATA33_S31C5        : bit  absolute LCDDATA33.7;
  LCDDATA33_S30C5        : bit  absolute LCDDATA33.6;
  LCDDATA33_S29C5        : bit  absolute LCDDATA33.5;
  LCDDATA33_S28C5        : bit  absolute LCDDATA33.4;
  LCDDATA33_S27C5        : bit  absolute LCDDATA33.3;
  LCDDATA33_S26C5        : bit  absolute LCDDATA33.2;
  LCDDATA33_S25C5        : bit  absolute LCDDATA33.1;
  LCDDATA33_S24C5        : bit  absolute LCDDATA33.0;
  LCDDATA34              : byte absolute $1D3A;
  LCDDATA34_S39C5        : bit  absolute LCDDATA34.7;
  LCDDATA34_S38C5        : bit  absolute LCDDATA34.6;
  LCDDATA34_S37C5        : bit  absolute LCDDATA34.5;
  LCDDATA34_S36C5        : bit  absolute LCDDATA34.4;
  LCDDATA34_S35C5        : bit  absolute LCDDATA34.3;
  LCDDATA34_S34C5        : bit  absolute LCDDATA34.2;
  LCDDATA34_S33C5        : bit  absolute LCDDATA34.1;
  LCDDATA34_S32C5        : bit  absolute LCDDATA34.0;
  LCDDATA35              : byte absolute $1D3B;
  LCDDATA35_S45C5        : bit  absolute LCDDATA35.6;
  LCDDATA35_S44C5        : bit  absolute LCDDATA35.5;
  LCDDATA35_S43C5        : bit  absolute LCDDATA35.4;
  LCDDATA35_S42C5        : bit  absolute LCDDATA35.3;
  LCDDATA35_S41C5        : bit  absolute LCDDATA35.2;
  LCDDATA35_S40C5        : bit  absolute LCDDATA35.1;
  LCDDATA36              : byte absolute $1D3C;
  LCDDATA36_S07C6        : bit  absolute LCDDATA36.7;
  LCDDATA36_S06C6        : bit  absolute LCDDATA36.6;
  LCDDATA36_S05C6        : bit  absolute LCDDATA36.5;
  LCDDATA36_S04C6        : bit  absolute LCDDATA36.4;
  LCDDATA36_S03C6        : bit  absolute LCDDATA36.3;
  LCDDATA36_S02C6        : bit  absolute LCDDATA36.2;
  LCDDATA36_S01C6        : bit  absolute LCDDATA36.1;
  LCDDATA36_S00C6        : bit  absolute LCDDATA36.0;
  LCDDATA37              : byte absolute $1D3D;
  LCDDATA37_S15C6        : bit  absolute LCDDATA37.7;
  LCDDATA37_S14C6        : bit  absolute LCDDATA37.6;
  LCDDATA37_S13C6        : bit  absolute LCDDATA37.5;
  LCDDATA37_S12C6        : bit  absolute LCDDATA37.4;
  LCDDATA37_S11C6        : bit  absolute LCDDATA37.3;
  LCDDATA37_S10C6        : bit  absolute LCDDATA37.2;
  LCDDATA37_S09C6        : bit  absolute LCDDATA37.1;
  LCDDATA37_S08C6        : bit  absolute LCDDATA37.0;
  LCDDATA38              : byte absolute $1D3E;
  LCDDATA38_S23C6        : bit  absolute LCDDATA38.7;
  LCDDATA38_S22C6        : bit  absolute LCDDATA38.6;
  LCDDATA38_S21C6        : bit  absolute LCDDATA38.5;
  LCDDATA38_S20C6        : bit  absolute LCDDATA38.4;
  LCDDATA38_S19C6        : bit  absolute LCDDATA38.3;
  LCDDATA38_S18C6        : bit  absolute LCDDATA38.2;
  LCDDATA38_S17C6        : bit  absolute LCDDATA38.1;
  LCDDATA38_S16C6        : bit  absolute LCDDATA38.0;
  LCDDATA39              : byte absolute $1D3F;
  LCDDATA39_S31C6        : bit  absolute LCDDATA39.7;
  LCDDATA39_S30C6        : bit  absolute LCDDATA39.6;
  LCDDATA39_S29C6        : bit  absolute LCDDATA39.5;
  LCDDATA39_S28C6        : bit  absolute LCDDATA39.4;
  LCDDATA39_S27C6        : bit  absolute LCDDATA39.3;
  LCDDATA39_S26C6        : bit  absolute LCDDATA39.2;
  LCDDATA39_S25C6        : bit  absolute LCDDATA39.1;
  LCDDATA39_S24C6        : bit  absolute LCDDATA39.0;
  LCDDATA40              : byte absolute $1D40;
  LCDDATA40_S39C6        : bit  absolute LCDDATA40.7;
  LCDDATA40_S38C6        : bit  absolute LCDDATA40.6;
  LCDDATA40_S37C6        : bit  absolute LCDDATA40.5;
  LCDDATA40_S36C6        : bit  absolute LCDDATA40.4;
  LCDDATA40_S35C6        : bit  absolute LCDDATA40.3;
  LCDDATA40_S34C6        : bit  absolute LCDDATA40.2;
  LCDDATA40_S33C6        : bit  absolute LCDDATA40.1;
  LCDDATA40_S32C6        : bit  absolute LCDDATA40.0;
  LCDDATA41              : byte absolute $1D41;
  LCDDATA41_S45C6        : bit  absolute LCDDATA41.6;
  LCDDATA41_S44C6        : bit  absolute LCDDATA41.5;
  LCDDATA41_S43C6        : bit  absolute LCDDATA41.4;
  LCDDATA41_S42C6        : bit  absolute LCDDATA41.3;
  LCDDATA41_S41C6        : bit  absolute LCDDATA41.2;
  LCDDATA41_S40C6        : bit  absolute LCDDATA41.1;
  LCDDATA42              : byte absolute $1D42;
  LCDDATA42_S07C7        : bit  absolute LCDDATA42.7;
  LCDDATA42_S06C7        : bit  absolute LCDDATA42.6;
  LCDDATA42_S05C7        : bit  absolute LCDDATA42.5;
  LCDDATA42_S04C7        : bit  absolute LCDDATA42.4;
  LCDDATA42_S03C7        : bit  absolute LCDDATA42.3;
  LCDDATA42_S02C7        : bit  absolute LCDDATA42.2;
  LCDDATA42_S01C7        : bit  absolute LCDDATA42.1;
  LCDDATA42_S00C7        : bit  absolute LCDDATA42.0;
  LCDDATA43              : byte absolute $1D43;
  LCDDATA43_S15C7        : bit  absolute LCDDATA43.7;
  LCDDATA43_S14C7        : bit  absolute LCDDATA43.6;
  LCDDATA43_S13C7        : bit  absolute LCDDATA43.5;
  LCDDATA43_S12C7        : bit  absolute LCDDATA43.4;
  LCDDATA43_S11C7        : bit  absolute LCDDATA43.3;
  LCDDATA43_S10C7        : bit  absolute LCDDATA43.2;
  LCDDATA43_S09C7        : bit  absolute LCDDATA43.1;
  LCDDATA43_S08C7        : bit  absolute LCDDATA43.0;
  LCDDATA44              : byte absolute $1D44;
  LCDDATA44_S23C7        : bit  absolute LCDDATA44.7;
  LCDDATA44_S22C7        : bit  absolute LCDDATA44.6;
  LCDDATA44_S21C7        : bit  absolute LCDDATA44.5;
  LCDDATA44_S20C7        : bit  absolute LCDDATA44.4;
  LCDDATA44_S19C7        : bit  absolute LCDDATA44.3;
  LCDDATA44_S18C7        : bit  absolute LCDDATA44.2;
  LCDDATA44_S17C7        : bit  absolute LCDDATA44.1;
  LCDDATA44_S16C7        : bit  absolute LCDDATA44.0;
  LCDDATA45              : byte absolute $1D45;
  LCDDATA45_S31C7        : bit  absolute LCDDATA45.7;
  LCDDATA45_S30C7        : bit  absolute LCDDATA45.6;
  LCDDATA45_S29C7        : bit  absolute LCDDATA45.5;
  LCDDATA45_S28C7        : bit  absolute LCDDATA45.4;
  LCDDATA45_S27C7        : bit  absolute LCDDATA45.3;
  LCDDATA45_S26C7        : bit  absolute LCDDATA45.2;
  LCDDATA45_S25C7        : bit  absolute LCDDATA45.1;
  LCDDATA45_S24C7        : bit  absolute LCDDATA45.0;
  LCDDATA46              : byte absolute $1D46;
  LCDDATA46_S39C7        : bit  absolute LCDDATA46.7;
  LCDDATA46_S38C7        : bit  absolute LCDDATA46.6;
  LCDDATA46_S37C7        : bit  absolute LCDDATA46.5;
  LCDDATA46_S36C7        : bit  absolute LCDDATA46.4;
  LCDDATA46_S35C7        : bit  absolute LCDDATA46.3;
  LCDDATA46_S34C7        : bit  absolute LCDDATA46.2;
  LCDDATA46_S33C7        : bit  absolute LCDDATA46.1;
  LCDDATA46_S32C7        : bit  absolute LCDDATA46.0;
  LCDDATA47              : byte absolute $1D47;
  LCDDATA47_S45C7        : bit  absolute LCDDATA47.6;
  LCDDATA47_S44C7        : bit  absolute LCDDATA47.5;
  LCDDATA47_S43C7        : bit  absolute LCDDATA47.4;
  LCDDATA47_S42C7        : bit  absolute LCDDATA47.3;
  LCDDATA47_S41C7        : bit  absolute LCDDATA47.2;
  LCDDATA47_S40C7        : bit  absolute LCDDATA47.1;
  CLCDATA                : byte absolute $1E0F;
  CLCDATA_MLC4OUT        : bit  absolute CLCDATA.3;
  CLCDATA_MLC3OUT        : bit  absolute CLCDATA.2;
  CLCDATA_MLC2OUT        : bit  absolute CLCDATA.1;
  CLCDATA_MLC1OUT        : bit  absolute CLCDATA.0;
  CLC1CON                : byte absolute $1E10;
  CLC1CON_LC1EN          : bit  absolute CLC1CON.7;
  CLC1CON_LC1OUT         : bit  absolute CLC1CON.6;
  CLC1CON_LC1INTP        : bit  absolute CLC1CON.5;
  CLC1CON_LC1INTN        : bit  absolute CLC1CON.4;
  CLC1CON_LC1MODE2       : bit  absolute CLC1CON.2;
  CLC1CON_LC1MODE1       : bit  absolute CLC1CON.1;
  CLC1CON_LC1MODE0       : bit  absolute CLC1CON.0;
  CLC1POL                : byte absolute $1E11;
  CLC1POL_LC1POL         : bit  absolute CLC1POL.7;
  CLC1POL_LC1G4POL       : bit  absolute CLC1POL.6;
  CLC1POL_LC1G3POL       : bit  absolute CLC1POL.5;
  CLC1POL_LC1G2POL       : bit  absolute CLC1POL.4;
  CLC1POL_LC1G1POL       : bit  absolute CLC1POL.3;
  CLC1SEL0               : byte absolute $1E12;
  CLC1SEL0_LC1D1S7       : bit  absolute CLC1SEL0.7;
  CLC1SEL0_LC1D1S6       : bit  absolute CLC1SEL0.6;
  CLC1SEL0_LC1D1S5       : bit  absolute CLC1SEL0.5;
  CLC1SEL0_LC1D1S4       : bit  absolute CLC1SEL0.4;
  CLC1SEL0_LC1D1S3       : bit  absolute CLC1SEL0.3;
  CLC1SEL0_LC1D1S2       : bit  absolute CLC1SEL0.2;
  CLC1SEL0_LC1D1S1       : bit  absolute CLC1SEL0.1;
  CLC1SEL0_LC1D1S0       : bit  absolute CLC1SEL0.0;
  CLC1SEL1               : byte absolute $1E13;
  CLC1SEL1_LC1D2S7       : bit  absolute CLC1SEL1.7;
  CLC1SEL1_LC1D2S6       : bit  absolute CLC1SEL1.6;
  CLC1SEL1_LC1D2S5       : bit  absolute CLC1SEL1.5;
  CLC1SEL1_LC1D2S4       : bit  absolute CLC1SEL1.4;
  CLC1SEL1_LC1D2S3       : bit  absolute CLC1SEL1.3;
  CLC1SEL1_LC1D2S2       : bit  absolute CLC1SEL1.2;
  CLC1SEL1_LC1D2S1       : bit  absolute CLC1SEL1.1;
  CLC1SEL1_LC1D2S0       : bit  absolute CLC1SEL1.0;
  CLC1SEL2               : byte absolute $1E14;
  CLC1SEL2_LC1D3S7       : bit  absolute CLC1SEL2.7;
  CLC1SEL2_LC1D3S6       : bit  absolute CLC1SEL2.6;
  CLC1SEL2_LC1D3S5       : bit  absolute CLC1SEL2.5;
  CLC1SEL2_LC1D3S4       : bit  absolute CLC1SEL2.4;
  CLC1SEL2_LC1D3S3       : bit  absolute CLC1SEL2.3;
  CLC1SEL2_LC1D3S2       : bit  absolute CLC1SEL2.2;
  CLC1SEL2_LC1D3S1       : bit  absolute CLC1SEL2.1;
  CLC1SEL2_LC1D3S0       : bit  absolute CLC1SEL2.0;
  CLC1SEL3               : byte absolute $1E15;
  CLC1SEL3_LC1D4S7       : bit  absolute CLC1SEL3.7;
  CLC1SEL3_LC1D4S6       : bit  absolute CLC1SEL3.6;
  CLC1SEL3_LC1D4S5       : bit  absolute CLC1SEL3.5;
  CLC1SEL3_LC1D4S4       : bit  absolute CLC1SEL3.4;
  CLC1SEL3_LC1D4S3       : bit  absolute CLC1SEL3.3;
  CLC1SEL3_LC1D4S2       : bit  absolute CLC1SEL3.2;
  CLC1SEL3_LC1D4S1       : bit  absolute CLC1SEL3.1;
  CLC1SEL3_LC1D4S0       : bit  absolute CLC1SEL3.0;
  CLC1GLS0               : byte absolute $1E16;
  CLC1GLS0_LC1G1D4T      : bit  absolute CLC1GLS0.7;
  CLC1GLS0_LC1G1D4N      : bit  absolute CLC1GLS0.6;
  CLC1GLS0_LC1G1D3T      : bit  absolute CLC1GLS0.5;
  CLC1GLS0_LC1G1D3N      : bit  absolute CLC1GLS0.4;
  CLC1GLS0_LC1G1D2T      : bit  absolute CLC1GLS0.3;
  CLC1GLS0_LC1G1D2N      : bit  absolute CLC1GLS0.2;
  CLC1GLS0_LC1G1D1T      : bit  absolute CLC1GLS0.1;
  CLC1GLS0_LC1G1D1N      : bit  absolute CLC1GLS0.0;
  CLC1GLS1               : byte absolute $1E17;
  CLC1GLS1_LC1G2D4T      : bit  absolute CLC1GLS1.7;
  CLC1GLS1_LC1G2D4N      : bit  absolute CLC1GLS1.6;
  CLC1GLS1_LC1G2D3T      : bit  absolute CLC1GLS1.5;
  CLC1GLS1_LC1G2D3N      : bit  absolute CLC1GLS1.4;
  CLC1GLS1_LC1G2D2T      : bit  absolute CLC1GLS1.3;
  CLC1GLS1_LC1G2D2N      : bit  absolute CLC1GLS1.2;
  CLC1GLS1_LC1G2D1T      : bit  absolute CLC1GLS1.1;
  CLC1GLS1_LC1G2D1N      : bit  absolute CLC1GLS1.0;
  CLC1GLS2               : byte absolute $1E18;
  CLC1GLS2_LC1G3D4T      : bit  absolute CLC1GLS2.7;
  CLC1GLS2_LC1G3D4N      : bit  absolute CLC1GLS2.6;
  CLC1GLS2_LC1G3D3T      : bit  absolute CLC1GLS2.5;
  CLC1GLS2_LC1G3D3N      : bit  absolute CLC1GLS2.4;
  CLC1GLS2_LC1G3D2T      : bit  absolute CLC1GLS2.3;
  CLC1GLS2_LC1G3D2N      : bit  absolute CLC1GLS2.2;
  CLC1GLS2_LC1G3D1T      : bit  absolute CLC1GLS2.1;
  CLC1GLS2_LC1G3D1N      : bit  absolute CLC1GLS2.0;
  CLC1GLS3               : byte absolute $1E19;
  CLC1GLS3_LC1G4D4T      : bit  absolute CLC1GLS3.7;
  CLC1GLS3_LC1G4D4N      : bit  absolute CLC1GLS3.6;
  CLC1GLS3_LC1G4D3T      : bit  absolute CLC1GLS3.5;
  CLC1GLS3_LC1G4D3N      : bit  absolute CLC1GLS3.4;
  CLC1GLS3_LC1G4D2T      : bit  absolute CLC1GLS3.3;
  CLC1GLS3_LC1G4D2N      : bit  absolute CLC1GLS3.2;
  CLC1GLS3_LC1G4D1T      : bit  absolute CLC1GLS3.1;
  CLC1GLS3_LC1G4D1N      : bit  absolute CLC1GLS3.0;
  CLC2CON                : byte absolute $1E1A;
  CLC2CON_LC2EN          : bit  absolute CLC2CON.7;
  CLC2CON_LC2OUT         : bit  absolute CLC2CON.6;
  CLC2CON_LC2INTP        : bit  absolute CLC2CON.5;
  CLC2CON_LC2INTN        : bit  absolute CLC2CON.4;
  CLC2CON_LC2MODE2       : bit  absolute CLC2CON.2;
  CLC2CON_LC2MODE1       : bit  absolute CLC2CON.1;
  CLC2CON_LC2MODE0       : bit  absolute CLC2CON.0;
  CLC2POL                : byte absolute $1E1B;
  CLC2POL_LC2POL         : bit  absolute CLC2POL.7;
  CLC2POL_LC2G4POL       : bit  absolute CLC2POL.6;
  CLC2POL_LC2G3POL       : bit  absolute CLC2POL.5;
  CLC2POL_LC2G2POL       : bit  absolute CLC2POL.4;
  CLC2POL_LC2G1POL       : bit  absolute CLC2POL.3;
  CLC2SEL0               : byte absolute $1E1C;
  CLC2SEL0_LC2D1S7       : bit  absolute CLC2SEL0.7;
  CLC2SEL0_LC2D1S6       : bit  absolute CLC2SEL0.6;
  CLC2SEL0_LC2D1S5       : bit  absolute CLC2SEL0.5;
  CLC2SEL0_LC2D1S4       : bit  absolute CLC2SEL0.4;
  CLC2SEL0_LC2D1S3       : bit  absolute CLC2SEL0.3;
  CLC2SEL0_LC2D1S2       : bit  absolute CLC2SEL0.2;
  CLC2SEL0_LC2D1S1       : bit  absolute CLC2SEL0.1;
  CLC2SEL0_LC2D1S0       : bit  absolute CLC2SEL0.0;
  CLC2SEL1               : byte absolute $1E1D;
  CLC2SEL1_LC2D2S7       : bit  absolute CLC2SEL1.7;
  CLC2SEL1_LC2D2S6       : bit  absolute CLC2SEL1.6;
  CLC2SEL1_LC2D2S5       : bit  absolute CLC2SEL1.5;
  CLC2SEL1_LC2D2S4       : bit  absolute CLC2SEL1.4;
  CLC2SEL1_LC2D2S3       : bit  absolute CLC2SEL1.3;
  CLC2SEL1_LC2D2S2       : bit  absolute CLC2SEL1.2;
  CLC2SEL1_LC2D2S1       : bit  absolute CLC2SEL1.1;
  CLC2SEL1_LC2D2S0       : bit  absolute CLC2SEL1.0;
  CLC2SEL2               : byte absolute $1E1E;
  CLC2SEL2_LC2D3S7       : bit  absolute CLC2SEL2.7;
  CLC2SEL2_LC2D3S6       : bit  absolute CLC2SEL2.6;
  CLC2SEL2_LC2D3S5       : bit  absolute CLC2SEL2.5;
  CLC2SEL2_LC2D3S4       : bit  absolute CLC2SEL2.4;
  CLC2SEL2_LC2D3S3       : bit  absolute CLC2SEL2.3;
  CLC2SEL2_LC2D3S2       : bit  absolute CLC2SEL2.2;
  CLC2SEL2_LC2D3S1       : bit  absolute CLC2SEL2.1;
  CLC2SEL2_LC2D3S0       : bit  absolute CLC2SEL2.0;
  CLC2SEL3               : byte absolute $1E1F;
  CLC2SEL3_LC2D4S7       : bit  absolute CLC2SEL3.7;
  CLC2SEL3_LC2D4S6       : bit  absolute CLC2SEL3.6;
  CLC2SEL3_LC2D4S5       : bit  absolute CLC2SEL3.5;
  CLC2SEL3_LC2D4S4       : bit  absolute CLC2SEL3.4;
  CLC2SEL3_LC2D4S3       : bit  absolute CLC2SEL3.3;
  CLC2SEL3_LC2D4S2       : bit  absolute CLC2SEL3.2;
  CLC2SEL3_LC2D4S1       : bit  absolute CLC2SEL3.1;
  CLC2SEL3_LC2D4S0       : bit  absolute CLC2SEL3.0;
  CLC2GLS0               : byte absolute $1E20;
  CLC2GLS0_LC2G1D4T      : bit  absolute CLC2GLS0.7;
  CLC2GLS0_LC2G1D4N      : bit  absolute CLC2GLS0.6;
  CLC2GLS0_LC2G1D3T      : bit  absolute CLC2GLS0.5;
  CLC2GLS0_LC2G1D3N      : bit  absolute CLC2GLS0.4;
  CLC2GLS0_LC2G1D2T      : bit  absolute CLC2GLS0.3;
  CLC2GLS0_LC2G1D2N      : bit  absolute CLC2GLS0.2;
  CLC2GLS0_LC2G1D1T      : bit  absolute CLC2GLS0.1;
  CLC2GLS0_LC2G1D1N      : bit  absolute CLC2GLS0.0;
  CLC2GLS1               : byte absolute $1E21;
  CLC2GLS1_LC2G2D4T      : bit  absolute CLC2GLS1.7;
  CLC2GLS1_LC2G2D4N      : bit  absolute CLC2GLS1.6;
  CLC2GLS1_LC2G2D3T      : bit  absolute CLC2GLS1.5;
  CLC2GLS1_LC2G2D3N      : bit  absolute CLC2GLS1.4;
  CLC2GLS1_LC2G2D2T      : bit  absolute CLC2GLS1.3;
  CLC2GLS1_LC2G2D2N      : bit  absolute CLC2GLS1.2;
  CLC2GLS1_LC2G2D1T      : bit  absolute CLC2GLS1.1;
  CLC2GLS1_LC2G2D1N      : bit  absolute CLC2GLS1.0;
  CLC2GLS2               : byte absolute $1E22;
  CLC2GLS2_LC2G3D4T      : bit  absolute CLC2GLS2.7;
  CLC2GLS2_LC2G3D4N      : bit  absolute CLC2GLS2.6;
  CLC2GLS2_LC2G3D3T      : bit  absolute CLC2GLS2.5;
  CLC2GLS2_LC2G3D3N      : bit  absolute CLC2GLS2.4;
  CLC2GLS2_LC2G3D2T      : bit  absolute CLC2GLS2.3;
  CLC2GLS2_LC2G3D2N      : bit  absolute CLC2GLS2.2;
  CLC2GLS2_LC2G3D1T      : bit  absolute CLC2GLS2.1;
  CLC2GLS2_LC2G3D1N      : bit  absolute CLC2GLS2.0;
  CLC2GLS3               : byte absolute $1E23;
  CLC2GLS3_LC2G4D4T      : bit  absolute CLC2GLS3.7;
  CLC2GLS3_LC2G4D4N      : bit  absolute CLC2GLS3.6;
  CLC2GLS3_LC2G4D3T      : bit  absolute CLC2GLS3.5;
  CLC2GLS3_LC2G4D3N      : bit  absolute CLC2GLS3.4;
  CLC2GLS3_LC2G4D2T      : bit  absolute CLC2GLS3.3;
  CLC2GLS3_LC2G4D2N      : bit  absolute CLC2GLS3.2;
  CLC2GLS3_LC2G4D1T      : bit  absolute CLC2GLS3.1;
  CLC2GLS3_LC2G4D1N      : bit  absolute CLC2GLS3.0;
  CLC3CON                : byte absolute $1E24;
  CLC3CON_LC3EN          : bit  absolute CLC3CON.7;
  CLC3CON_LC3OUT         : bit  absolute CLC3CON.6;
  CLC3CON_LC3INTP        : bit  absolute CLC3CON.5;
  CLC3CON_LC3INTN        : bit  absolute CLC3CON.4;
  CLC3CON_LC3MODE2       : bit  absolute CLC3CON.2;
  CLC3CON_LC3MODE1       : bit  absolute CLC3CON.1;
  CLC3CON_LC3MODE0       : bit  absolute CLC3CON.0;
  CLC3POL                : byte absolute $1E25;
  CLC3POL_LC3POL         : bit  absolute CLC3POL.7;
  CLC3POL_LC3G4POL       : bit  absolute CLC3POL.6;
  CLC3POL_LC3G3POL       : bit  absolute CLC3POL.5;
  CLC3POL_LC3G2POL       : bit  absolute CLC3POL.4;
  CLC3POL_LC3G1POL       : bit  absolute CLC3POL.3;
  CLC3SEL0               : byte absolute $1E26;
  CLC3SEL0_LC3D1S7       : bit  absolute CLC3SEL0.7;
  CLC3SEL0_LC3D1S6       : bit  absolute CLC3SEL0.6;
  CLC3SEL0_LC3D1S5       : bit  absolute CLC3SEL0.5;
  CLC3SEL0_LC3D1S4       : bit  absolute CLC3SEL0.4;
  CLC3SEL0_LC3D1S3       : bit  absolute CLC3SEL0.3;
  CLC3SEL0_LC3D1S2       : bit  absolute CLC3SEL0.2;
  CLC3SEL0_LC3D1S1       : bit  absolute CLC3SEL0.1;
  CLC3SEL0_LC3D1S0       : bit  absolute CLC3SEL0.0;
  CLC3SEL1               : byte absolute $1E27;
  CLC3SEL1_LC3D2S7       : bit  absolute CLC3SEL1.7;
  CLC3SEL1_LC3D2S6       : bit  absolute CLC3SEL1.6;
  CLC3SEL1_LC3D2S5       : bit  absolute CLC3SEL1.5;
  CLC3SEL1_LC3D2S4       : bit  absolute CLC3SEL1.4;
  CLC3SEL1_LC3D2S3       : bit  absolute CLC3SEL1.3;
  CLC3SEL1_LC3D2S2       : bit  absolute CLC3SEL1.2;
  CLC3SEL1_LC3D2S1       : bit  absolute CLC3SEL1.1;
  CLC3SEL1_LC3D2S0       : bit  absolute CLC3SEL1.0;
  CLC3SEL2               : byte absolute $1E28;
  CLC3SEL2_LC3D3S7       : bit  absolute CLC3SEL2.7;
  CLC3SEL2_LC3D3S6       : bit  absolute CLC3SEL2.6;
  CLC3SEL2_LC3D3S5       : bit  absolute CLC3SEL2.5;
  CLC3SEL2_LC3D3S4       : bit  absolute CLC3SEL2.4;
  CLC3SEL2_LC3D3S3       : bit  absolute CLC3SEL2.3;
  CLC3SEL2_LC3D3S2       : bit  absolute CLC3SEL2.2;
  CLC3SEL2_LC3D3S1       : bit  absolute CLC3SEL2.1;
  CLC3SEL2_LC3D3S0       : bit  absolute CLC3SEL2.0;
  CLC3SEL3               : byte absolute $1E29;
  CLC3SEL3_LC3D4S7       : bit  absolute CLC3SEL3.7;
  CLC3SEL3_LC3D4S6       : bit  absolute CLC3SEL3.6;
  CLC3SEL3_LC3D4S5       : bit  absolute CLC3SEL3.5;
  CLC3SEL3_LC3D4S4       : bit  absolute CLC3SEL3.4;
  CLC3SEL3_LC3D4S3       : bit  absolute CLC3SEL3.3;
  CLC3SEL3_LC3D4S2       : bit  absolute CLC3SEL3.2;
  CLC3SEL3_LC3D4S1       : bit  absolute CLC3SEL3.1;
  CLC3SEL3_LC3D4S0       : bit  absolute CLC3SEL3.0;
  CLC3GLS0               : byte absolute $1E2A;
  CLC3GLS0_LC3G1D4T      : bit  absolute CLC3GLS0.7;
  CLC3GLS0_LC3G1D4N      : bit  absolute CLC3GLS0.6;
  CLC3GLS0_LC3G1D3T      : bit  absolute CLC3GLS0.5;
  CLC3GLS0_LC3G1D3N      : bit  absolute CLC3GLS0.4;
  CLC3GLS0_LC3G1D2T      : bit  absolute CLC3GLS0.3;
  CLC3GLS0_LC3G1D2N      : bit  absolute CLC3GLS0.2;
  CLC3GLS0_LC3G1D1T      : bit  absolute CLC3GLS0.1;
  CLC3GLS0_LC3G1D1N      : bit  absolute CLC3GLS0.0;
  CLC3GLS1               : byte absolute $1E2B;
  CLC3GLS1_LC3G2D4T      : bit  absolute CLC3GLS1.7;
  CLC3GLS1_LC3G2D4N      : bit  absolute CLC3GLS1.6;
  CLC3GLS1_LC3G2D3T      : bit  absolute CLC3GLS1.5;
  CLC3GLS1_LC3G2D3N      : bit  absolute CLC3GLS1.4;
  CLC3GLS1_LC3G2D2T      : bit  absolute CLC3GLS1.3;
  CLC3GLS1_LC3G2D2N      : bit  absolute CLC3GLS1.2;
  CLC3GLS1_LC3G2D1T      : bit  absolute CLC3GLS1.1;
  CLC3GLS1_LC3G2D1N      : bit  absolute CLC3GLS1.0;
  CLC3GLS2               : byte absolute $1E2C;
  CLC3GLS2_LC3G3D4T      : bit  absolute CLC3GLS2.7;
  CLC3GLS2_LC3G3D4N      : bit  absolute CLC3GLS2.6;
  CLC3GLS2_LC3G3D3T      : bit  absolute CLC3GLS2.5;
  CLC3GLS2_LC3G3D3N      : bit  absolute CLC3GLS2.4;
  CLC3GLS2_LC3G3D2T      : bit  absolute CLC3GLS2.3;
  CLC3GLS2_LC3G3D2N      : bit  absolute CLC3GLS2.2;
  CLC3GLS2_LC3G3D1T      : bit  absolute CLC3GLS2.1;
  CLC3GLS2_LC3G3D1N      : bit  absolute CLC3GLS2.0;
  CLC3GLS3               : byte absolute $1E2D;
  CLC3GLS3_LC3G4D4T      : bit  absolute CLC3GLS3.7;
  CLC3GLS3_LC3G4D4N      : bit  absolute CLC3GLS3.6;
  CLC3GLS3_LC3G4D3T      : bit  absolute CLC3GLS3.5;
  CLC3GLS3_LC3G4D3N      : bit  absolute CLC3GLS3.4;
  CLC3GLS3_LC3G4D2T      : bit  absolute CLC3GLS3.3;
  CLC3GLS3_LC3G4D2N      : bit  absolute CLC3GLS3.2;
  CLC3GLS3_LC3G4D1T      : bit  absolute CLC3GLS3.1;
  CLC3GLS3_LC3G4D1N      : bit  absolute CLC3GLS3.0;
  CLC4CON                : byte absolute $1E2E;
  CLC4CON_LC4EN          : bit  absolute CLC4CON.7;
  CLC4CON_LC4OUT         : bit  absolute CLC4CON.6;
  CLC4CON_LC4INTP        : bit  absolute CLC4CON.5;
  CLC4CON_LC4INTN        : bit  absolute CLC4CON.4;
  CLC4CON_LC4MODE2       : bit  absolute CLC4CON.2;
  CLC4CON_LC4MODE1       : bit  absolute CLC4CON.1;
  CLC4CON_LC4MODE0       : bit  absolute CLC4CON.0;
  CLC4POL                : byte absolute $1E2F;
  CLC4POL_LC4POL         : bit  absolute CLC4POL.7;
  CLC4POL_LC4G4POL       : bit  absolute CLC4POL.6;
  CLC4POL_LC4G3POL       : bit  absolute CLC4POL.5;
  CLC4POL_LC4G2POL       : bit  absolute CLC4POL.4;
  CLC4POL_LC4G1POL       : bit  absolute CLC4POL.3;
  CLC4SEL0               : byte absolute $1E30;
  CLC4SEL0_LC4D1S7       : bit  absolute CLC4SEL0.7;
  CLC4SEL0_LC4D1S6       : bit  absolute CLC4SEL0.6;
  CLC4SEL0_LC4D1S5       : bit  absolute CLC4SEL0.5;
  CLC4SEL0_LC4D1S4       : bit  absolute CLC4SEL0.4;
  CLC4SEL0_LC4D1S3       : bit  absolute CLC4SEL0.3;
  CLC4SEL0_LC4D1S2       : bit  absolute CLC4SEL0.2;
  CLC4SEL0_LC4D1S1       : bit  absolute CLC4SEL0.1;
  CLC4SEL0_LC4D1S0       : bit  absolute CLC4SEL0.0;
  CLC4SEL1               : byte absolute $1E31;
  CLC4SEL1_LC4D2S7       : bit  absolute CLC4SEL1.7;
  CLC4SEL1_LC4D2S6       : bit  absolute CLC4SEL1.6;
  CLC4SEL1_LC4D2S5       : bit  absolute CLC4SEL1.5;
  CLC4SEL1_LC4D2S4       : bit  absolute CLC4SEL1.4;
  CLC4SEL1_LC4D2S3       : bit  absolute CLC4SEL1.3;
  CLC4SEL1_LC4D2S2       : bit  absolute CLC4SEL1.2;
  CLC4SEL1_LC4D2S1       : bit  absolute CLC4SEL1.1;
  CLC4SEL1_LC4D2S0       : bit  absolute CLC4SEL1.0;
  CLC4SEL2               : byte absolute $1E32;
  CLC4SEL2_LC4D3S7       : bit  absolute CLC4SEL2.7;
  CLC4SEL2_LC4D3S6       : bit  absolute CLC4SEL2.6;
  CLC4SEL2_LC4D3S5       : bit  absolute CLC4SEL2.5;
  CLC4SEL2_LC4D3S4       : bit  absolute CLC4SEL2.4;
  CLC4SEL2_LC4D3S3       : bit  absolute CLC4SEL2.3;
  CLC4SEL2_LC4D3S2       : bit  absolute CLC4SEL2.2;
  CLC4SEL2_LC4D3S1       : bit  absolute CLC4SEL2.1;
  CLC4SEL2_LC4D3S0       : bit  absolute CLC4SEL2.0;
  CLC4SEL3               : byte absolute $1E33;
  CLC4SEL3_LC4D4S7       : bit  absolute CLC4SEL3.7;
  CLC4SEL3_LC4D4S6       : bit  absolute CLC4SEL3.6;
  CLC4SEL3_LC4D4S5       : bit  absolute CLC4SEL3.5;
  CLC4SEL3_LC4D4S4       : bit  absolute CLC4SEL3.4;
  CLC4SEL3_LC4D4S3       : bit  absolute CLC4SEL3.3;
  CLC4SEL3_LC4D4S2       : bit  absolute CLC4SEL3.2;
  CLC4SEL3_LC4D4S1       : bit  absolute CLC4SEL3.1;
  CLC4SEL3_LC4D4S0       : bit  absolute CLC4SEL3.0;
  CLC4GLS0               : byte absolute $1E34;
  CLC4GLS0_LC4G1D4T      : bit  absolute CLC4GLS0.7;
  CLC4GLS0_LC4G1D4N      : bit  absolute CLC4GLS0.6;
  CLC4GLS0_LC4G1D3T      : bit  absolute CLC4GLS0.5;
  CLC4GLS0_LC4G1D3N      : bit  absolute CLC4GLS0.4;
  CLC4GLS0_LC4G1D2T      : bit  absolute CLC4GLS0.3;
  CLC4GLS0_LC4G1D2N      : bit  absolute CLC4GLS0.2;
  CLC4GLS0_LC4G1D1T      : bit  absolute CLC4GLS0.1;
  CLC4GLS0_LC4G1D1N      : bit  absolute CLC4GLS0.0;
  CLC4GLS1               : byte absolute $1E35;
  CLC4GLS1_LC4G2D4T      : bit  absolute CLC4GLS1.7;
  CLC4GLS1_LC4G2D4N      : bit  absolute CLC4GLS1.6;
  CLC4GLS1_LC4G2D3T      : bit  absolute CLC4GLS1.5;
  CLC4GLS1_LC4G2D3N      : bit  absolute CLC4GLS1.4;
  CLC4GLS1_LC4G2D2T      : bit  absolute CLC4GLS1.3;
  CLC4GLS1_LC4G2D2N      : bit  absolute CLC4GLS1.2;
  CLC4GLS1_LC4G2D1T      : bit  absolute CLC4GLS1.1;
  CLC4GLS1_LC4G2D1N      : bit  absolute CLC4GLS1.0;
  CLC4GLS2               : byte absolute $1E36;
  CLC4GLS2_LC4G3D4T      : bit  absolute CLC4GLS2.7;
  CLC4GLS2_LC4G3D4N      : bit  absolute CLC4GLS2.6;
  CLC4GLS2_LC4G3D3T      : bit  absolute CLC4GLS2.5;
  CLC4GLS2_LC4G3D3N      : bit  absolute CLC4GLS2.4;
  CLC4GLS2_LC4G3D2T      : bit  absolute CLC4GLS2.3;
  CLC4GLS2_LC4G3D2N      : bit  absolute CLC4GLS2.2;
  CLC4GLS2_LC4G3D1T      : bit  absolute CLC4GLS2.1;
  CLC4GLS2_LC4G3D1N      : bit  absolute CLC4GLS2.0;
  CLC4GLS3               : byte absolute $1E37;
  CLC4GLS3_LC4G4D4T      : bit  absolute CLC4GLS3.7;
  CLC4GLS3_LC4G4D4N      : bit  absolute CLC4GLS3.6;
  CLC4GLS3_LC4G4D3T      : bit  absolute CLC4GLS3.5;
  CLC4GLS3_LC4G4D3N      : bit  absolute CLC4GLS3.4;
  CLC4GLS3_LC4G4D2T      : bit  absolute CLC4GLS3.3;
  CLC4GLS3_LC4G4D2N      : bit  absolute CLC4GLS3.2;
  CLC4GLS3_LC4G4D1T      : bit  absolute CLC4GLS3.1;
  CLC4GLS3_LC4G4D1N      : bit  absolute CLC4GLS3.0;
  RF0PPS                 : byte absolute $1E38;
  RF0PPS_RF0PPS5         : bit  absolute RF0PPS.5;
  RF0PPS_RF0PPS4         : bit  absolute RF0PPS.4;
  RF0PPS_RF0PPS3         : bit  absolute RF0PPS.3;
  RF0PPS_RF0PPS2         : bit  absolute RF0PPS.2;
  RF0PPS_RF0PPS1         : bit  absolute RF0PPS.1;
  RF0PPS_RF0PPS0         : bit  absolute RF0PPS.0;
  RF1PPS                 : byte absolute $1E39;
  RF1PPS_RF1PPS5         : bit  absolute RF1PPS.5;
  RF1PPS_RF1PPS4         : bit  absolute RF1PPS.4;
  RF1PPS_RF1PPS3         : bit  absolute RF1PPS.3;
  RF1PPS_RF1PPS2         : bit  absolute RF1PPS.2;
  RF1PPS_RF1PPS1         : bit  absolute RF1PPS.1;
  RF1PPS_RF1PPS0         : bit  absolute RF1PPS.0;
  RF2PPS                 : byte absolute $1E3A;
  RF2PPS_RF2PPS5         : bit  absolute RF2PPS.5;
  RF2PPS_RF2PPS4         : bit  absolute RF2PPS.4;
  RF2PPS_RF2PPS3         : bit  absolute RF2PPS.3;
  RF2PPS_RF2PPS2         : bit  absolute RF2PPS.2;
  RF2PPS_RF2PPS1         : bit  absolute RF2PPS.1;
  RF2PPS_RF2PPS0         : bit  absolute RF2PPS.0;
  RF3PPS                 : byte absolute $1E3B;
  RF3PPS_RF3PPS5         : bit  absolute RF3PPS.5;
  RF3PPS_RF3PPS4         : bit  absolute RF3PPS.4;
  RF3PPS_RF3PPS3         : bit  absolute RF3PPS.3;
  RF3PPS_RF3PPS2         : bit  absolute RF3PPS.2;
  RF3PPS_RF3PPS1         : bit  absolute RF3PPS.1;
  RF3PPS_RF3PPS0         : bit  absolute RF3PPS.0;
  RF4PPS                 : byte absolute $1E3C;
  RF4PPS_RF4PPS5         : bit  absolute RF4PPS.5;
  RF4PPS_RF4PPS4         : bit  absolute RF4PPS.4;
  RF4PPS_RF4PPS3         : bit  absolute RF4PPS.3;
  RF4PPS_RF4PPS2         : bit  absolute RF4PPS.2;
  RF4PPS_RF4PPS1         : bit  absolute RF4PPS.1;
  RF4PPS_RF4PPS0         : bit  absolute RF4PPS.0;
  RF5PPS                 : byte absolute $1E3D;
  RF5PPS_RF5PPS5         : bit  absolute RF5PPS.5;
  RF5PPS_RF5PPS4         : bit  absolute RF5PPS.4;
  RF5PPS_RF5PPS3         : bit  absolute RF5PPS.3;
  RF5PPS_RF5PPS2         : bit  absolute RF5PPS.2;
  RF5PPS_RF5PPS1         : bit  absolute RF5PPS.1;
  RF5PPS_RF5PPS0         : bit  absolute RF5PPS.0;
  RF6PPS                 : byte absolute $1E3E;
  RF6PPS_RF6PPS5         : bit  absolute RF6PPS.5;
  RF6PPS_RF6PPS4         : bit  absolute RF6PPS.4;
  RF6PPS_RF6PPS3         : bit  absolute RF6PPS.3;
  RF6PPS_RF6PPS2         : bit  absolute RF6PPS.2;
  RF6PPS_RF6PPS1         : bit  absolute RF6PPS.1;
  RF6PPS_RF6PPS0         : bit  absolute RF6PPS.0;
  RF7PPS                 : byte absolute $1E3F;
  RF7PPS_RF7PPS5         : bit  absolute RF7PPS.5;
  RF7PPS_RF7PPS4         : bit  absolute RF7PPS.4;
  RF7PPS_RF7PPS3         : bit  absolute RF7PPS.3;
  RF7PPS_RF7PPS2         : bit  absolute RF7PPS.2;
  RF7PPS_RF7PPS1         : bit  absolute RF7PPS.1;
  RF7PPS_RF7PPS0         : bit  absolute RF7PPS.0;
  RG0PPS                 : byte absolute $1E40;
  RG0PPS_RG0PPS5         : bit  absolute RG0PPS.5;
  RG0PPS_RG0PPS4         : bit  absolute RG0PPS.4;
  RG0PPS_RG0PPS3         : bit  absolute RG0PPS.3;
  RG0PPS_RG0PPS2         : bit  absolute RG0PPS.2;
  RG0PPS_RG0PPS1         : bit  absolute RG0PPS.1;
  RG0PPS_RG0PPS0         : bit  absolute RG0PPS.0;
  RG1PPS                 : byte absolute $1E41;
  RG1PPS_RG1PPS5         : bit  absolute RG1PPS.5;
  RG1PPS_RG1PPS4         : bit  absolute RG1PPS.4;
  RG1PPS_RG1PPS3         : bit  absolute RG1PPS.3;
  RG1PPS_RG1PPS2         : bit  absolute RG1PPS.2;
  RG1PPS_RG1PPS1         : bit  absolute RG1PPS.1;
  RG1PPS_RG1PPS0         : bit  absolute RG1PPS.0;
  RG2PPS                 : byte absolute $1E42;
  RG2PPS_RG2PPS5         : bit  absolute RG2PPS.5;
  RG2PPS_RG2PPS4         : bit  absolute RG2PPS.4;
  RG2PPS_RG2PPS3         : bit  absolute RG2PPS.3;
  RG2PPS_RG2PPS2         : bit  absolute RG2PPS.2;
  RG2PPS_RG2PPS1         : bit  absolute RG2PPS.1;
  RG2PPS_RG2PPS0         : bit  absolute RG2PPS.0;
  RG3PPS                 : byte absolute $1E43;
  RG3PPS_RG3PPS5         : bit  absolute RG3PPS.5;
  RG3PPS_RG3PPS4         : bit  absolute RG3PPS.4;
  RG3PPS_RG3PPS3         : bit  absolute RG3PPS.3;
  RG3PPS_RG3PPS2         : bit  absolute RG3PPS.2;
  RG3PPS_RG3PPS1         : bit  absolute RG3PPS.1;
  RG3PPS_RG3PPS0         : bit  absolute RG3PPS.0;
  RG4PPS                 : byte absolute $1E44;
  RG4PPS_RG4PPS5         : bit  absolute RG4PPS.5;
  RG4PPS_RG4PPS4         : bit  absolute RG4PPS.4;
  RG4PPS_RG4PPS3         : bit  absolute RG4PPS.3;
  RG4PPS_RG4PPS2         : bit  absolute RG4PPS.2;
  RG4PPS_RG4PPS1         : bit  absolute RG4PPS.1;
  RG4PPS_RG4PPS0         : bit  absolute RG4PPS.0;
  RG6PPS                 : byte absolute $1E46;
  RG6PPS_RG6PPS5         : bit  absolute RG6PPS.5;
  RG6PPS_RG6PPS4         : bit  absolute RG6PPS.4;
  RG6PPS_RG6PPS3         : bit  absolute RG6PPS.3;
  RG6PPS_RG6PPS2         : bit  absolute RG6PPS.2;
  RG6PPS_RG6PPS1         : bit  absolute RG6PPS.1;
  RG6PPS_RG6PPS0         : bit  absolute RG6PPS.0;
  RG7PPS                 : byte absolute $1E47;
  RG7PPS_RG7PPS5         : bit  absolute RG7PPS.5;
  RG7PPS_RG7PPS4         : bit  absolute RG7PPS.4;
  RG7PPS_RG7PPS3         : bit  absolute RG7PPS.3;
  RG7PPS_RG7PPS2         : bit  absolute RG7PPS.2;
  RG7PPS_RG7PPS1         : bit  absolute RG7PPS.1;
  RG7PPS_RG7PPS0         : bit  absolute RG7PPS.0;
  RH0PPS                 : byte absolute $1E48;
  RH0PPS_RH0PPS5         : bit  absolute RH0PPS.5;
  RH0PPS_RH0PPS4         : bit  absolute RH0PPS.4;
  RH0PPS_RH0PPS3         : bit  absolute RH0PPS.3;
  RH0PPS_RH0PPS2         : bit  absolute RH0PPS.2;
  RH0PPS_RH0PPS1         : bit  absolute RH0PPS.1;
  RH0PPS_RH0PPS0         : bit  absolute RH0PPS.0;
  RH1PPS                 : byte absolute $1E49;
  RH1PPS_RH1PPS5         : bit  absolute RH1PPS.5;
  RH1PPS_RH1PPS4         : bit  absolute RH1PPS.4;
  RH1PPS_RH1PPS3         : bit  absolute RH1PPS.3;
  RH1PPS_RH1PPS2         : bit  absolute RH1PPS.2;
  RH1PPS_RH1PPS1         : bit  absolute RH1PPS.1;
  RH1PPS_RH1PPS0         : bit  absolute RH1PPS.0;
  RH2PPS                 : byte absolute $1E4A;
  RH2PPS_RH2PPS5         : bit  absolute RH2PPS.5;
  RH2PPS_RH2PPS4         : bit  absolute RH2PPS.4;
  RH2PPS_RH2PPS3         : bit  absolute RH2PPS.3;
  RH2PPS_RH2PPS2         : bit  absolute RH2PPS.2;
  RH2PPS_RH2PPS1         : bit  absolute RH2PPS.1;
  RH2PPS_RH2PPS0         : bit  absolute RH2PPS.0;
  RH3PPS                 : byte absolute $1E4B;
  RH3PPS_RH3PPS5         : bit  absolute RH3PPS.5;
  RH3PPS_RH3PPS4         : bit  absolute RH3PPS.4;
  RH3PPS_RH3PPS3         : bit  absolute RH3PPS.3;
  RH3PPS_RH3PPS2         : bit  absolute RH3PPS.2;
  RH3PPS_RH3PPS1         : bit  absolute RH3PPS.1;
  RH3PPS_RH3PPS0         : bit  absolute RH3PPS.0;
  ANSELF                 : byte absolute $1E50;
  ANSELF_ANSF7           : bit  absolute ANSELF.7;
  ANSELF_ANSF6           : bit  absolute ANSELF.6;
  ANSELF_ANSF5           : bit  absolute ANSELF.5;
  ANSELF_ANSF4           : bit  absolute ANSELF.4;
  ANSELF_ANSF3           : bit  absolute ANSELF.3;
  ANSELF_ANSF2           : bit  absolute ANSELF.2;
  ANSELF_ANSF1           : bit  absolute ANSELF.1;
  ANSELF_ANSF0           : bit  absolute ANSELF.0;
  WPUF                   : byte absolute $1E51;
  WPUF_WPUF7             : bit  absolute WPUF.7;
  WPUF_WPUF6             : bit  absolute WPUF.6;
  WPUF_WPUF5             : bit  absolute WPUF.5;
  WPUF_WPUF4             : bit  absolute WPUF.4;
  WPUF_WPUF3             : bit  absolute WPUF.3;
  WPUF_WPUF2             : bit  absolute WPUF.2;
  WPUF_WPUF1             : bit  absolute WPUF.1;
  WPUF_WPUF0             : bit  absolute WPUF.0;
  ODCONF                 : byte absolute $1E52;
  ODCONF_ODCF7           : bit  absolute ODCONF.7;
  ODCONF_ODCF6           : bit  absolute ODCONF.6;
  ODCONF_ODCF5           : bit  absolute ODCONF.5;
  ODCONF_ODCF4           : bit  absolute ODCONF.4;
  ODCONF_ODCF3           : bit  absolute ODCONF.3;
  ODCONF_ODCF2           : bit  absolute ODCONF.2;
  ODCONF_ODCF1           : bit  absolute ODCONF.1;
  ODCONF_ODCF0           : bit  absolute ODCONF.0;
  SLRCONF                : byte absolute $1E53;
  SLRCONF_SLRF7          : bit  absolute SLRCONF.7;
  SLRCONF_SLRF6          : bit  absolute SLRCONF.6;
  SLRCONF_SLRF5          : bit  absolute SLRCONF.5;
  SLRCONF_SLRF4          : bit  absolute SLRCONF.4;
  SLRCONF_SLRF3          : bit  absolute SLRCONF.3;
  SLRCONF_SLRF2          : bit  absolute SLRCONF.2;
  SLRCONF_SLRF1          : bit  absolute SLRCONF.1;
  SLRCONF_SLRF0          : bit  absolute SLRCONF.0;
  INLVLF                 : byte absolute $1E54;
  INLVLF_INLVLF7         : bit  absolute INLVLF.7;
  INLVLF_INLVLF6         : bit  absolute INLVLF.6;
  INLVLF_INLVLF5         : bit  absolute INLVLF.5;
  INLVLF_INLVLF4         : bit  absolute INLVLF.4;
  INLVLF_INLVLF3         : bit  absolute INLVLF.3;
  INLVLF_INLVLF2         : bit  absolute INLVLF.2;
  INLVLF_INLVLF1         : bit  absolute INLVLF.1;
  INLVLF_INLVLF0         : bit  absolute INLVLF.0;
  HIDRVF                 : byte absolute $1E55;
  HIDRVF_HIDF7           : bit  absolute HIDRVF.7;
  ANSELG                 : byte absolute $1E5B;
  ANSELG_ANSG7           : bit  absolute ANSELG.7;
  ANSELG_ANSG6           : bit  absolute ANSELG.6;
  ANSELG_ANSG4           : bit  absolute ANSELG.5;
  ANSELG_ANSG3           : bit  absolute ANSELG.4;
  ANSELG_ANSG2           : bit  absolute ANSELG.3;
  ANSELG_ANSG1           : bit  absolute ANSELG.2;
  ANSELG_ANSG0           : bit  absolute ANSELG.1;
  WPUG                   : byte absolute $1E5C;
  WPUG_WPUG7             : bit  absolute WPUG.7;
  WPUG_WPUG6             : bit  absolute WPUG.6;
  WPUG_WPUG5             : bit  absolute WPUG.5;
  WPUG_WPUG4             : bit  absolute WPUG.4;
  WPUG_WPUG3             : bit  absolute WPUG.3;
  WPUG_WPUG2             : bit  absolute WPUG.2;
  WPUG_WPUG1             : bit  absolute WPUG.1;
  WPUG_WPUG0             : bit  absolute WPUG.0;
  ODCONG                 : byte absolute $1E5D;
  ODCONG_ODCG7           : bit  absolute ODCONG.7;
  ODCONG_ODCG6           : bit  absolute ODCONG.6;
  ODCONG_ODCG4           : bit  absolute ODCONG.5;
  ODCONG_ODCG3           : bit  absolute ODCONG.4;
  ODCONG_ODCG2           : bit  absolute ODCONG.3;
  ODCONG_ODCG1           : bit  absolute ODCONG.2;
  ODCONG_ODCG0           : bit  absolute ODCONG.1;
  SLRCONG                : byte absolute $1E5E;
  SLRCONG_SLRG7          : bit  absolute SLRCONG.7;
  SLRCONG_SLRG6          : bit  absolute SLRCONG.6;
  SLRCONG_SLRG4          : bit  absolute SLRCONG.5;
  SLRCONG_SLRG3          : bit  absolute SLRCONG.4;
  SLRCONG_SLRG2          : bit  absolute SLRCONG.3;
  SLRCONG_SLRG1          : bit  absolute SLRCONG.2;
  SLRCONG_SLRG0          : bit  absolute SLRCONG.1;
  INLVLG                 : byte absolute $1E5F;
  INLVLG_INLVLG7         : bit  absolute INLVLG.7;
  INLVLG_INLVLG6         : bit  absolute INLVLG.6;
  INLVLG_INLVLG5         : bit  absolute INLVLG.5;
  INLVLG_INLVLG4         : bit  absolute INLVLG.4;
  INLVLG_INLVLG3         : bit  absolute INLVLG.3;
  INLVLG_INLVLG2         : bit  absolute INLVLG.2;
  INLVLG_INLVLG1         : bit  absolute INLVLG.1;
  INLVLG_INLVLG0         : bit  absolute INLVLG.0;
  IOCGP                  : byte absolute $1E60;
  IOCGP_IOCGP5           : bit  absolute IOCGP.5;
  IOCGN                  : byte absolute $1E61;
  IOCGN_IOCGN5           : bit  absolute IOCGN.5;
  IOCGF                  : byte absolute $1E62;
  IOCGF_IOCGF5           : bit  absolute IOCGF.5;
  WPUH                   : byte absolute $1E67;
  WPUH_WPUH3             : bit  absolute WPUH.3;
  WPUH_WPUH2             : bit  absolute WPUH.2;
  WPUH_WPUH1             : bit  absolute WPUH.1;
  WPUH_WPUH0             : bit  absolute WPUH.0;
  ODCONH                 : byte absolute $1E68;
  ODCONH_ODCH3           : bit  absolute ODCONH.4;
  ODCONH_ODCH2           : bit  absolute ODCONH.3;
  ODCONH_ODCH1           : bit  absolute ODCONH.2;
  ODCONH_ODCH0           : bit  absolute ODCONH.1;
  SLRCONH                : byte absolute $1E69;
  SLRCONH_SLRH3          : bit  absolute SLRCONH.4;
  SLRCONH_SLRH2          : bit  absolute SLRCONH.3;
  SLRCONH_SLRH1          : bit  absolute SLRCONH.2;
  SLRCONH_SLRH0          : bit  absolute SLRCONH.1;
  INLVLH                 : byte absolute $1E6A;
  INLVLH_INLVLH3         : bit  absolute INLVLH.3;
  INLVLH_INLVLH2         : bit  absolute INLVLH.2;
  INLVLH_INLVLH1         : bit  absolute INLVLH.1;
  INLVLH_INLVLH0         : bit  absolute INLVLH.0;
  PPSLOCK                : byte absolute $1E8F;
  PPSLOCK_PPSLOCKED      : bit  absolute PPSLOCK.0;
  INTPPS                 : byte absolute $1E90;
  INTPPS_INTPPS5         : bit  absolute INTPPS.5;
  INTPPS_INTPPS4         : bit  absolute INTPPS.4;
  INTPPS_INTPPS3         : bit  absolute INTPPS.3;
  INTPPS_INTPPS2         : bit  absolute INTPPS.2;
  INTPPS_INTPPS1         : bit  absolute INTPPS.1;
  INTPPS_INTPPS0         : bit  absolute INTPPS.0;
  T0CKIPPS               : byte absolute $1E91;
  T0CKIPPS_T0CKIPPS5     : bit  absolute T0CKIPPS.5;
  T0CKIPPS_T0CKIPPS4     : bit  absolute T0CKIPPS.4;
  T0CKIPPS_T0CKIPPS3     : bit  absolute T0CKIPPS.3;
  T0CKIPPS_T0CKIPPS2     : bit  absolute T0CKIPPS.2;
  T0CKIPPS_T0CKIPPS1     : bit  absolute T0CKIPPS.1;
  T0CKIPPS_T0CKIPPS0     : bit  absolute T0CKIPPS.0;
  T1CKIPPS               : byte absolute $1E92;
  T1CKIPPS_T1CKIPPS5     : bit  absolute T1CKIPPS.5;
  T1CKIPPS_T1CKIPPS4     : bit  absolute T1CKIPPS.4;
  T1CKIPPS_T1CKIPPS3     : bit  absolute T1CKIPPS.3;
  T1CKIPPS_T1CKIPPS2     : bit  absolute T1CKIPPS.2;
  T1CKIPPS_T1CKIPPS1     : bit  absolute T1CKIPPS.1;
  T1CKIPPS_T1CKIPPS0     : bit  absolute T1CKIPPS.0;
  T1GPPS                 : byte absolute $1E93;
  T1GPPS_T1GPPS5         : bit  absolute T1GPPS.5;
  T1GPPS_T1GPPS4         : bit  absolute T1GPPS.4;
  T1GPPS_T1GPPS3         : bit  absolute T1GPPS.3;
  T1GPPS_T1GPPS2         : bit  absolute T1GPPS.2;
  T1GPPS_T1GPPS1         : bit  absolute T1GPPS.1;
  T1GPPS_T1GPPS0         : bit  absolute T1GPPS.0;
  T2INPPS                : byte absolute $1E9C;
  T2INPPS_T2INPPS5       : bit  absolute T2INPPS.5;
  T2INPPS_T2INPPS4       : bit  absolute T2INPPS.4;
  T2INPPS_T2INPPS3       : bit  absolute T2INPPS.3;
  T2INPPS_T2INPPS2       : bit  absolute T2INPPS.2;
  T2INPPS_T2INPPS1       : bit  absolute T2INPPS.1;
  T2INPPS_T2INPPS0       : bit  absolute T2INPPS.0;
  T4INPPS                : byte absolute $1E9D;
  T4INPPS_T4INPPS5       : bit  absolute T4INPPS.5;
  T4INPPS_T4INPPS4       : bit  absolute T4INPPS.4;
  T4INPPS_T4INPPS3       : bit  absolute T4INPPS.3;
  T4INPPS_T4INPPS2       : bit  absolute T4INPPS.2;
  T4INPPS_T4INPPS1       : bit  absolute T4INPPS.1;
  T4INPPS_T4INPPS0       : bit  absolute T4INPPS.0;
  CCP1PPS                : byte absolute $1EA1;
  CCP1PPS_CCP1PPS5       : bit  absolute CCP1PPS.5;
  CCP1PPS_CCP1PPS4       : bit  absolute CCP1PPS.4;
  CCP1PPS_CCP1PPS3       : bit  absolute CCP1PPS.3;
  CCP1PPS_CCP1PPS2       : bit  absolute CCP1PPS.2;
  CCP1PPS_CCP1PPS1       : bit  absolute CCP1PPS.1;
  CCP1PPS_CCP1PPS0       : bit  absolute CCP1PPS.0;
  CCP2PPS                : byte absolute $1EA2;
  CCP2PPS_CCP2PPS5       : bit  absolute CCP2PPS.5;
  CCP2PPS_CCP2PPS4       : bit  absolute CCP2PPS.4;
  CCP2PPS_CCP2PPS3       : bit  absolute CCP2PPS.3;
  CCP2PPS_CCP2PPS2       : bit  absolute CCP2PPS.2;
  CCP2PPS_CCP2PPS1       : bit  absolute CCP2PPS.1;
  CCP2PPS_CCP2PPS0       : bit  absolute CCP2PPS.0;
  SMT1WINPPS             : byte absolute $1EA9;
  SMT1WINPPS_SMT1WINPPS5 : bit  absolute SMT1WINPPS.5;
  SMT1WINPPS_SMT1WINPPS4 : bit  absolute SMT1WINPPS.4;
  SMT1WINPPS_SMT1WINPPS3 : bit  absolute SMT1WINPPS.3;
  SMT1WINPPS_SMT1WINPPS2 : bit  absolute SMT1WINPPS.2;
  SMT1WINPPS_SMT1WINPPS1 : bit  absolute SMT1WINPPS.1;
  SMT1WINPPS_SMT1WINPPS0 : bit  absolute SMT1WINPPS.0;
  SMT1SIGPPS             : byte absolute $1EAA;
  SMT1SIGPPS_SMT1SIGPPS5 : bit  absolute SMT1SIGPPS.5;
  SMT1SIGPPS_SMT1SIGPPS4 : bit  absolute SMT1SIGPPS.4;
  SMT1SIGPPS_SMT1SIGPPS3 : bit  absolute SMT1SIGPPS.3;
  SMT1SIGPPS_SMT1SIGPPS2 : bit  absolute SMT1SIGPPS.2;
  SMT1SIGPPS_SMT1SIGPPS1 : bit  absolute SMT1SIGPPS.1;
  SMT1SIGPPS_SMT1SIGPPS0 : bit  absolute SMT1SIGPPS.0;
  CWG1PPS                : byte absolute $1EB1;
  CWG1PPS_CWG1PPS5       : bit  absolute CWG1PPS.5;
  CWG1PPS_CWG1PPS4       : bit  absolute CWG1PPS.4;
  CWG1PPS_CWG1PPS3       : bit  absolute CWG1PPS.3;
  CWG1PPS_CWG1PPS2       : bit  absolute CWG1PPS.2;
  CWG1PPS_CWG1PPS1       : bit  absolute CWG1PPS.1;
  CWG1PPS_CWG1PPS0       : bit  absolute CWG1PPS.0;
  CLCIN0PPS              : byte absolute $1EBB;
  CLCIN0PPS_CLCIN0PPS5   : bit  absolute CLCIN0PPS.5;
  CLCIN0PPS_CLCIN0PPS4   : bit  absolute CLCIN0PPS.4;
  CLCIN0PPS_CLCIN0PPS3   : bit  absolute CLCIN0PPS.3;
  CLCIN0PPS_CLCIN0PPS2   : bit  absolute CLCIN0PPS.2;
  CLCIN0PPS_CLCIN0PPS1   : bit  absolute CLCIN0PPS.1;
  CLCIN0PPS_CLCIN0PPS0   : bit  absolute CLCIN0PPS.0;
  CLCIN1PPS              : byte absolute $1EBC;
  CLCIN1PPS_CLCIN1PPS5   : bit  absolute CLCIN1PPS.5;
  CLCIN1PPS_CLCIN1PPS4   : bit  absolute CLCIN1PPS.4;
  CLCIN1PPS_CLCIN1PPS3   : bit  absolute CLCIN1PPS.3;
  CLCIN1PPS_CLCIN1PPS2   : bit  absolute CLCIN1PPS.2;
  CLCIN1PPS_CLCIN1PPS1   : bit  absolute CLCIN1PPS.1;
  CLCIN1PPS_CLCIN1PPS0   : bit  absolute CLCIN1PPS.0;
  CLCIN2PPS              : byte absolute $1EBD;
  CLCIN2PPS_CLCIN2PPS5   : bit  absolute CLCIN2PPS.5;
  CLCIN2PPS_CLCIN2PPS4   : bit  absolute CLCIN2PPS.4;
  CLCIN2PPS_CLCIN2PPS3   : bit  absolute CLCIN2PPS.3;
  CLCIN2PPS_CLCIN2PPS2   : bit  absolute CLCIN2PPS.2;
  CLCIN2PPS_CLCIN2PPS1   : bit  absolute CLCIN2PPS.1;
  CLCIN2PPS_CLCIN2PPS0   : bit  absolute CLCIN2PPS.0;
  CLCIN3PPS              : byte absolute $1EBE;
  CLCIN3PPS_CLCIN3PPS5   : bit  absolute CLCIN3PPS.5;
  CLCIN3PPS_CLCIN3PPS4   : bit  absolute CLCIN3PPS.4;
  CLCIN3PPS_CLCIN3PPS3   : bit  absolute CLCIN3PPS.3;
  CLCIN3PPS_CLCIN3PPS2   : bit  absolute CLCIN3PPS.2;
  CLCIN3PPS_CLCIN3PPS1   : bit  absolute CLCIN3PPS.1;
  CLCIN3PPS_CLCIN3PPS0   : bit  absolute CLCIN3PPS.0;
  ADACTPPS               : byte absolute $1EC3;
  ADACTPPS_ADACTPPS5     : bit  absolute ADACTPPS.5;
  ADACTPPS_ADACTPPS4     : bit  absolute ADACTPPS.4;
  ADACTPPS_ADACTPPS3     : bit  absolute ADACTPPS.3;
  ADACTPPS_ADACTPPS2     : bit  absolute ADACTPPS.2;
  ADACTPPS_ADACTPPS1     : bit  absolute ADACTPPS.1;
  ADACTPPS_ADACTPPS0     : bit  absolute ADACTPPS.0;
  SSP1CLKPPS             : byte absolute $1EC5;
  SSP1CLKPPS_SSP1CLKPPS5 : bit  absolute SSP1CLKPPS.5;
  SSP1CLKPPS_SSP1CLKPPS4 : bit  absolute SSP1CLKPPS.4;
  SSP1CLKPPS_SSP1CLKPPS3 : bit  absolute SSP1CLKPPS.3;
  SSP1CLKPPS_SSP1CLKPPS2 : bit  absolute SSP1CLKPPS.2;
  SSP1CLKPPS_SSP1CLKPPS1 : bit  absolute SSP1CLKPPS.1;
  SSP1CLKPPS_SSP1CLKPPS0 : bit  absolute SSP1CLKPPS.0;
  SSP1DATPPS             : byte absolute $1EC6;
  SSP1DATPPS_SSP1DATPPS5 : bit  absolute SSP1DATPPS.5;
  SSP1DATPPS_SSP1DATPPS4 : bit  absolute SSP1DATPPS.4;
  SSP1DATPPS_SSP1DATPPS3 : bit  absolute SSP1DATPPS.3;
  SSP1DATPPS_SSP1DATPPS2 : bit  absolute SSP1DATPPS.2;
  SSP1DATPPS_SSP1DATPPS1 : bit  absolute SSP1DATPPS.1;
  SSP1DATPPS_SSP1DATPPS0 : bit  absolute SSP1DATPPS.0;
  SSP1SSPPS              : byte absolute $1EC7;
  SSP1SSPPS_SSP1SSPPS5   : bit  absolute SSP1SSPPS.5;
  SSP1SSPPS_SSP1SSPPS4   : bit  absolute SSP1SSPPS.4;
  SSP1SSPPS_SSP1SSPPS3   : bit  absolute SSP1SSPPS.3;
  SSP1SSPPS_SSP1SSPPS2   : bit  absolute SSP1SSPPS.2;
  SSP1SSPPS_SSP1SSPPS1   : bit  absolute SSP1SSPPS.1;
  SSP1SSPPS_SSP1SSPPS0   : bit  absolute SSP1SSPPS.0;
  RX1PPS                 : byte absolute $1ECB;
  RX1PPS_RX1PPS5         : bit  absolute RX1PPS.5;
  RX1PPS_RX1PPS4         : bit  absolute RX1PPS.4;
  RX1PPS_RX1PPS3         : bit  absolute RX1PPS.3;
  RX1PPS_RX1PPS2         : bit  absolute RX1PPS.2;
  RX1PPS_RX1PPS1         : bit  absolute RX1PPS.1;
  RX1PPS_RX1PPS0         : bit  absolute RX1PPS.0;
  TX1PPS                 : byte absolute $1ECC;
  TX1PPS_TX1PPS5         : bit  absolute TX1PPS.5;
  TX1PPS_TX1PPS4         : bit  absolute TX1PPS.4;
  TX1PPS_TX1PPS3         : bit  absolute TX1PPS.3;
  TX1PPS_TX1PPS2         : bit  absolute TX1PPS.2;
  TX1PPS_TX1PPS1         : bit  absolute TX1PPS.1;
  TX1PPS_TX1PPS0         : bit  absolute TX1PPS.0;
  RX2PPS                 : byte absolute $1ECD;
  RX2PPS_RX2PPS5         : bit  absolute RX2PPS.5;
  RX2PPS_RX2PPS4         : bit  absolute RX2PPS.4;
  RX2PPS_RX2PPS3         : bit  absolute RX2PPS.3;
  RX2PPS_RX2PPS2         : bit  absolute RX2PPS.2;
  RX2PPS_RX2PPS1         : bit  absolute RX2PPS.1;
  RX2PPS_RX2PPS0         : bit  absolute RX2PPS.0;
  TX2PPS                 : byte absolute $1ECE;
  TX2PPS_TX2PPS5         : bit  absolute TX2PPS.5;
  TX2PPS_TX2PPS4         : bit  absolute TX2PPS.4;
  TX2PPS_TX2PPS3         : bit  absolute TX2PPS.3;
  TX2PPS_TX2PPS2         : bit  absolute TX2PPS.2;
  TX2PPS_TX2PPS1         : bit  absolute TX2PPS.1;
  TX2PPS_TX2PPS0         : bit  absolute TX2PPS.0;
  RA0PPS                 : byte absolute $1F10;
  RA0PPS_RA0PPS5         : bit  absolute RA0PPS.5;
  RA0PPS_RA0PPS4         : bit  absolute RA0PPS.4;
  RA0PPS_RA0PPS3         : bit  absolute RA0PPS.3;
  RA0PPS_RA0PPS2         : bit  absolute RA0PPS.2;
  RA0PPS_RA0PPS1         : bit  absolute RA0PPS.1;
  RA0PPS_RA0PPS0         : bit  absolute RA0PPS.0;
  RA1PPS                 : byte absolute $1F11;
  RA1PPS_RA1PPS5         : bit  absolute RA1PPS.5;
  RA1PPS_RA1PPS4         : bit  absolute RA1PPS.4;
  RA1PPS_RA1PPS3         : bit  absolute RA1PPS.3;
  RA1PPS_RA1PPS2         : bit  absolute RA1PPS.2;
  RA1PPS_RA1PPS1         : bit  absolute RA1PPS.1;
  RA1PPS_RA1PPS0         : bit  absolute RA1PPS.0;
  RA2PPS                 : byte absolute $1F12;
  RA2PPS_RA2PPS5         : bit  absolute RA2PPS.5;
  RA2PPS_RA2PPS4         : bit  absolute RA2PPS.4;
  RA2PPS_RA2PPS3         : bit  absolute RA2PPS.3;
  RA2PPS_RA2PPS2         : bit  absolute RA2PPS.2;
  RA2PPS_RA2PPS1         : bit  absolute RA2PPS.1;
  RA2PPS_RA2PPS0         : bit  absolute RA2PPS.0;
  RA3PPS                 : byte absolute $1F13;
  RA3PPS_RA3PPS5         : bit  absolute RA3PPS.5;
  RA3PPS_RA3PPS4         : bit  absolute RA3PPS.4;
  RA3PPS_RA3PPS3         : bit  absolute RA3PPS.3;
  RA3PPS_RA3PPS2         : bit  absolute RA3PPS.2;
  RA3PPS_RA3PPS1         : bit  absolute RA3PPS.1;
  RA3PPS_RA3PPS0         : bit  absolute RA3PPS.0;
  RA4PPS                 : byte absolute $1F14;
  RA4PPS_RA4PPS5         : bit  absolute RA4PPS.5;
  RA4PPS_RA4PPS4         : bit  absolute RA4PPS.4;
  RA4PPS_RA4PPS3         : bit  absolute RA4PPS.3;
  RA4PPS_RA4PPS2         : bit  absolute RA4PPS.2;
  RA4PPS_RA4PPS1         : bit  absolute RA4PPS.1;
  RA4PPS_RA4PPS0         : bit  absolute RA4PPS.0;
  RA5PPS                 : byte absolute $1F15;
  RA5PPS_RA5PPS5         : bit  absolute RA5PPS.5;
  RA5PPS_RA5PPS4         : bit  absolute RA5PPS.4;
  RA5PPS_RA5PPS3         : bit  absolute RA5PPS.3;
  RA5PPS_RA5PPS2         : bit  absolute RA5PPS.2;
  RA5PPS_RA5PPS1         : bit  absolute RA5PPS.1;
  RA5PPS_RA5PPS0         : bit  absolute RA5PPS.0;
  RA6PPS                 : byte absolute $1F16;
  RA6PPS_RA6PPS5         : bit  absolute RA6PPS.5;
  RA6PPS_RA6PPS4         : bit  absolute RA6PPS.4;
  RA6PPS_RA6PPS3         : bit  absolute RA6PPS.3;
  RA6PPS_RA6PPS2         : bit  absolute RA6PPS.2;
  RA6PPS_RA6PPS1         : bit  absolute RA6PPS.1;
  RA6PPS_RA6PPS0         : bit  absolute RA6PPS.0;
  RA7PPS                 : byte absolute $1F17;
  RA7PPS_RA7PPS5         : bit  absolute RA7PPS.5;
  RA7PPS_RA7PPS4         : bit  absolute RA7PPS.4;
  RA7PPS_RA7PPS3         : bit  absolute RA7PPS.3;
  RA7PPS_RA7PPS2         : bit  absolute RA7PPS.2;
  RA7PPS_RA7PPS1         : bit  absolute RA7PPS.1;
  RA7PPS_RA7PPS0         : bit  absolute RA7PPS.0;
  RB0PPS                 : byte absolute $1F18;
  RB0PPS_RB0PPS5         : bit  absolute RB0PPS.5;
  RB0PPS_RB0PPS4         : bit  absolute RB0PPS.4;
  RB0PPS_RB0PPS3         : bit  absolute RB0PPS.3;
  RB0PPS_RB0PPS2         : bit  absolute RB0PPS.2;
  RB0PPS_RB0PPS1         : bit  absolute RB0PPS.1;
  RB0PPS_RB0PPS0         : bit  absolute RB0PPS.0;
  RB1PPS                 : byte absolute $1F19;
  RB1PPS_RB1PPS5         : bit  absolute RB1PPS.5;
  RB1PPS_RB1PPS4         : bit  absolute RB1PPS.4;
  RB1PPS_RB1PPS3         : bit  absolute RB1PPS.3;
  RB1PPS_RB1PPS2         : bit  absolute RB1PPS.2;
  RB1PPS_RB1PPS1         : bit  absolute RB1PPS.1;
  RB1PPS_RB1PPS0         : bit  absolute RB1PPS.0;
  RB2PPS                 : byte absolute $1F1A;
  RB2PPS_RB2PPS5         : bit  absolute RB2PPS.5;
  RB2PPS_RB2PPS4         : bit  absolute RB2PPS.4;
  RB2PPS_RB2PPS3         : bit  absolute RB2PPS.3;
  RB2PPS_RB2PPS2         : bit  absolute RB2PPS.2;
  RB2PPS_RB2PPS1         : bit  absolute RB2PPS.1;
  RB2PPS_RB2PPS0         : bit  absolute RB2PPS.0;
  RB3PPS                 : byte absolute $1F1B;
  RB3PPS_RB3PPS5         : bit  absolute RB3PPS.5;
  RB3PPS_RB3PPS4         : bit  absolute RB3PPS.4;
  RB3PPS_RB3PPS3         : bit  absolute RB3PPS.3;
  RB3PPS_RB3PPS2         : bit  absolute RB3PPS.2;
  RB3PPS_RB3PPS1         : bit  absolute RB3PPS.1;
  RB3PPS_RB3PPS0         : bit  absolute RB3PPS.0;
  RB4PPS                 : byte absolute $1F1C;
  RB4PPS_RB4PPS5         : bit  absolute RB4PPS.5;
  RB4PPS_RB4PPS4         : bit  absolute RB4PPS.4;
  RB4PPS_RB4PPS3         : bit  absolute RB4PPS.3;
  RB4PPS_RB4PPS2         : bit  absolute RB4PPS.2;
  RB4PPS_RB4PPS1         : bit  absolute RB4PPS.1;
  RB4PPS_RB4PPS0         : bit  absolute RB4PPS.0;
  RB5PPS                 : byte absolute $1F1D;
  RB5PPS_RB5PPS5         : bit  absolute RB5PPS.5;
  RB5PPS_RB5PPS4         : bit  absolute RB5PPS.4;
  RB5PPS_RB5PPS3         : bit  absolute RB5PPS.3;
  RB5PPS_RB5PPS2         : bit  absolute RB5PPS.2;
  RB5PPS_RB5PPS1         : bit  absolute RB5PPS.1;
  RB5PPS_RB5PPS0         : bit  absolute RB5PPS.0;
  RB6PPS                 : byte absolute $1F1E;
  RB6PPS_RB6PPS5         : bit  absolute RB6PPS.5;
  RB6PPS_RB6PPS4         : bit  absolute RB6PPS.4;
  RB6PPS_RB6PPS3         : bit  absolute RB6PPS.3;
  RB6PPS_RB6PPS2         : bit  absolute RB6PPS.2;
  RB6PPS_RB6PPS1         : bit  absolute RB6PPS.1;
  RB6PPS_RB6PPS0         : bit  absolute RB6PPS.0;
  RB7PPS                 : byte absolute $1F1F;
  RB7PPS_RB7PPS5         : bit  absolute RB7PPS.5;
  RB7PPS_RB7PPS4         : bit  absolute RB7PPS.4;
  RB7PPS_RB7PPS3         : bit  absolute RB7PPS.3;
  RB7PPS_RB7PPS2         : bit  absolute RB7PPS.2;
  RB7PPS_RB7PPS1         : bit  absolute RB7PPS.1;
  RB7PPS_RB7PPS0         : bit  absolute RB7PPS.0;
  RC0PPS                 : byte absolute $1F20;
  RC0PPS_RC0PPS5         : bit  absolute RC0PPS.5;
  RC0PPS_RC0PPS4         : bit  absolute RC0PPS.4;
  RC0PPS_RC0PPS3         : bit  absolute RC0PPS.3;
  RC0PPS_RC0PPS2         : bit  absolute RC0PPS.2;
  RC0PPS_RC0PPS1         : bit  absolute RC0PPS.1;
  RC0PPS_RC0PPS0         : bit  absolute RC0PPS.0;
  RC1PPS                 : byte absolute $1F21;
  RC1PPS_RC1PPS5         : bit  absolute RC1PPS.5;
  RC1PPS_RC1PPS4         : bit  absolute RC1PPS.4;
  RC1PPS_RC1PPS3         : bit  absolute RC1PPS.3;
  RC1PPS_RC1PPS2         : bit  absolute RC1PPS.2;
  RC1PPS_RC1PPS1         : bit  absolute RC1PPS.1;
  RC1PPS_RC1PPS0         : bit  absolute RC1PPS.0;
  RC2PPS                 : byte absolute $1F22;
  RC2PPS_RC2PPS5         : bit  absolute RC2PPS.5;
  RC2PPS_RC2PPS4         : bit  absolute RC2PPS.4;
  RC2PPS_RC2PPS3         : bit  absolute RC2PPS.3;
  RC2PPS_RC2PPS2         : bit  absolute RC2PPS.2;
  RC2PPS_RC2PPS1         : bit  absolute RC2PPS.1;
  RC2PPS_RC2PPS0         : bit  absolute RC2PPS.0;
  RC3PPS                 : byte absolute $1F23;
  RC3PPS_RC3PPS5         : bit  absolute RC3PPS.5;
  RC3PPS_RC3PPS4         : bit  absolute RC3PPS.4;
  RC3PPS_RC3PPS3         : bit  absolute RC3PPS.3;
  RC3PPS_RC3PPS2         : bit  absolute RC3PPS.2;
  RC3PPS_RC3PPS1         : bit  absolute RC3PPS.1;
  RC3PPS_RC3PPS0         : bit  absolute RC3PPS.0;
  RC4PPS                 : byte absolute $1F24;
  RC4PPS_RC4PPS5         : bit  absolute RC4PPS.5;
  RC4PPS_RC4PPS4         : bit  absolute RC4PPS.4;
  RC4PPS_RC4PPS3         : bit  absolute RC4PPS.3;
  RC4PPS_RC4PPS2         : bit  absolute RC4PPS.2;
  RC4PPS_RC4PPS1         : bit  absolute RC4PPS.1;
  RC4PPS_RC4PPS0         : bit  absolute RC4PPS.0;
  RC5PPS                 : byte absolute $1F25;
  RC5PPS_RC5PPS5         : bit  absolute RC5PPS.5;
  RC5PPS_RC5PPS4         : bit  absolute RC5PPS.4;
  RC5PPS_RC5PPS3         : bit  absolute RC5PPS.3;
  RC5PPS_RC5PPS2         : bit  absolute RC5PPS.2;
  RC5PPS_RC5PPS1         : bit  absolute RC5PPS.1;
  RC5PPS_RC5PPS0         : bit  absolute RC5PPS.0;
  RC6PPS                 : byte absolute $1F26;
  RC6PPS_RC6PPS5         : bit  absolute RC6PPS.5;
  RC6PPS_RC6PPS4         : bit  absolute RC6PPS.4;
  RC6PPS_RC6PPS3         : bit  absolute RC6PPS.3;
  RC6PPS_RC6PPS2         : bit  absolute RC6PPS.2;
  RC6PPS_RC6PPS1         : bit  absolute RC6PPS.1;
  RC6PPS_RC6PPS0         : bit  absolute RC6PPS.0;
  RC7PPS                 : byte absolute $1F27;
  RC7PPS_RC7PPS5         : bit  absolute RC7PPS.5;
  RC7PPS_RC7PPS4         : bit  absolute RC7PPS.4;
  RC7PPS_RC7PPS3         : bit  absolute RC7PPS.3;
  RC7PPS_RC7PPS2         : bit  absolute RC7PPS.2;
  RC7PPS_RC7PPS1         : bit  absolute RC7PPS.1;
  RC7PPS_RC7PPS0         : bit  absolute RC7PPS.0;
  RD0PPS                 : byte absolute $1F28;
  RD0PPS_RD0PPS5         : bit  absolute RD0PPS.5;
  RD0PPS_RD0PPS4         : bit  absolute RD0PPS.4;
  RD0PPS_RD0PPS3         : bit  absolute RD0PPS.3;
  RD0PPS_RD0PPS2         : bit  absolute RD0PPS.2;
  RD0PPS_RD0PPS1         : bit  absolute RD0PPS.1;
  RD0PPS_RD0PPS0         : bit  absolute RD0PPS.0;
  RD1PPS                 : byte absolute $1F29;
  RD1PPS_RD1PPS5         : bit  absolute RD1PPS.5;
  RD1PPS_RD1PPS4         : bit  absolute RD1PPS.4;
  RD1PPS_RD1PPS3         : bit  absolute RD1PPS.3;
  RD1PPS_RD1PPS2         : bit  absolute RD1PPS.2;
  RD1PPS_RD1PPS1         : bit  absolute RD1PPS.1;
  RD1PPS_RD1PPS0         : bit  absolute RD1PPS.0;
  RD2PPS                 : byte absolute $1F2A;
  RD2PPS_RD2PPS5         : bit  absolute RD2PPS.5;
  RD2PPS_RD2PPS4         : bit  absolute RD2PPS.4;
  RD2PPS_RD2PPS3         : bit  absolute RD2PPS.3;
  RD2PPS_RD2PPS2         : bit  absolute RD2PPS.2;
  RD2PPS_RD2PPS1         : bit  absolute RD2PPS.1;
  RD2PPS_RD2PPS0         : bit  absolute RD2PPS.0;
  RD3PPS                 : byte absolute $1F2B;
  RD3PPS_RD3PPS5         : bit  absolute RD3PPS.5;
  RD3PPS_RD3PPS4         : bit  absolute RD3PPS.4;
  RD3PPS_RD3PPS3         : bit  absolute RD3PPS.3;
  RD3PPS_RD3PPS2         : bit  absolute RD3PPS.2;
  RD3PPS_RD3PPS1         : bit  absolute RD3PPS.1;
  RD3PPS_RD3PPS0         : bit  absolute RD3PPS.0;
  RD4PPS                 : byte absolute $1F2C;
  RD4PPS_RD4PPS5         : bit  absolute RD4PPS.5;
  RD4PPS_RD4PPS4         : bit  absolute RD4PPS.4;
  RD4PPS_RD4PPS3         : bit  absolute RD4PPS.3;
  RD4PPS_RD4PPS2         : bit  absolute RD4PPS.2;
  RD4PPS_RD4PPS1         : bit  absolute RD4PPS.1;
  RD4PPS_RD4PPS0         : bit  absolute RD4PPS.0;
  RD5PPS                 : byte absolute $1F2D;
  RD5PPS_RD5PPS5         : bit  absolute RD5PPS.5;
  RD5PPS_RD5PPS4         : bit  absolute RD5PPS.4;
  RD5PPS_RD5PPS3         : bit  absolute RD5PPS.3;
  RD5PPS_RD5PPS2         : bit  absolute RD5PPS.2;
  RD5PPS_RD5PPS1         : bit  absolute RD5PPS.1;
  RD5PPS_RD5PPS0         : bit  absolute RD5PPS.0;
  RD6PPS                 : byte absolute $1F2E;
  RD6PPS_RD6PPS5         : bit  absolute RD6PPS.5;
  RD6PPS_RD6PPS4         : bit  absolute RD6PPS.4;
  RD6PPS_RD6PPS3         : bit  absolute RD6PPS.3;
  RD6PPS_RD6PPS2         : bit  absolute RD6PPS.2;
  RD6PPS_RD6PPS1         : bit  absolute RD6PPS.1;
  RD6PPS_RD6PPS0         : bit  absolute RD6PPS.0;
  RD7PPS                 : byte absolute $1F2F;
  RD7PPS_RD7PPS5         : bit  absolute RD7PPS.5;
  RD7PPS_RD7PPS4         : bit  absolute RD7PPS.4;
  RD7PPS_RD7PPS3         : bit  absolute RD7PPS.3;
  RD7PPS_RD7PPS2         : bit  absolute RD7PPS.2;
  RD7PPS_RD7PPS1         : bit  absolute RD7PPS.1;
  RD7PPS_RD7PPS0         : bit  absolute RD7PPS.0;
  RE0PPS                 : byte absolute $1F30;
  RE0PPS_RE0PPS5         : bit  absolute RE0PPS.5;
  RE0PPS_RE0PPS4         : bit  absolute RE0PPS.4;
  RE0PPS_RE0PPS3         : bit  absolute RE0PPS.3;
  RE0PPS_RE0PPS2         : bit  absolute RE0PPS.2;
  RE0PPS_RE0PPS1         : bit  absolute RE0PPS.1;
  RE0PPS_RE0PPS0         : bit  absolute RE0PPS.0;
  RE1PPS                 : byte absolute $1F31;
  RE1PPS_RE1PPS5         : bit  absolute RE1PPS.5;
  RE1PPS_RE1PPS4         : bit  absolute RE1PPS.4;
  RE1PPS_RE1PPS3         : bit  absolute RE1PPS.3;
  RE1PPS_RE1PPS2         : bit  absolute RE1PPS.2;
  RE1PPS_RE1PPS1         : bit  absolute RE1PPS.1;
  RE1PPS_RE1PPS0         : bit  absolute RE1PPS.0;
  RE3PPS                 : byte absolute $1F33;
  RE3PPS_RE3PPS5         : bit  absolute RE3PPS.5;
  RE3PPS_RE3PPS4         : bit  absolute RE3PPS.4;
  RE3PPS_RE3PPS3         : bit  absolute RE3PPS.3;
  RE3PPS_RE3PPS2         : bit  absolute RE3PPS.2;
  RE3PPS_RE3PPS1         : bit  absolute RE3PPS.1;
  RE3PPS_RE3PPS0         : bit  absolute RE3PPS.0;
  RE4PPS                 : byte absolute $1F34;
  RE4PPS_RE4PPS5         : bit  absolute RE4PPS.5;
  RE4PPS_RE4PPS4         : bit  absolute RE4PPS.4;
  RE4PPS_RE4PPS3         : bit  absolute RE4PPS.3;
  RE4PPS_RE4PPS2         : bit  absolute RE4PPS.2;
  RE4PPS_RE4PPS1         : bit  absolute RE4PPS.1;
  RE4PPS_RE4PPS0         : bit  absolute RE4PPS.0;
  RE5PPS                 : byte absolute $1F35;
  RE5PPS_RE5PPS5         : bit  absolute RE5PPS.5;
  RE5PPS_RE5PPS4         : bit  absolute RE5PPS.4;
  RE5PPS_RE5PPS3         : bit  absolute RE5PPS.3;
  RE5PPS_RE5PPS2         : bit  absolute RE5PPS.2;
  RE5PPS_RE5PPS1         : bit  absolute RE5PPS.1;
  RE5PPS_RE5PPS0         : bit  absolute RE5PPS.0;
  RE6PPS                 : byte absolute $1F36;
  RE6PPS_RE6PPS5         : bit  absolute RE6PPS.5;
  RE6PPS_RE6PPS4         : bit  absolute RE6PPS.4;
  RE6PPS_RE6PPS3         : bit  absolute RE6PPS.3;
  RE6PPS_RE6PPS2         : bit  absolute RE6PPS.2;
  RE6PPS_RE6PPS1         : bit  absolute RE6PPS.1;
  RE6PPS_RE6PPS0         : bit  absolute RE6PPS.0;
  RE7PPS                 : byte absolute $1F37;
  RE7PPS_RE7PPS5         : bit  absolute RE7PPS.5;
  RE7PPS_RE7PPS4         : bit  absolute RE7PPS.4;
  RE7PPS_RE7PPS3         : bit  absolute RE7PPS.3;
  RE7PPS_RE7PPS2         : bit  absolute RE7PPS.2;
  RE7PPS_RE7PPS1         : bit  absolute RE7PPS.1;
  RE7PPS_RE7PPS0         : bit  absolute RE7PPS.0;
  ANSELA                 : byte absolute $1F38;
  ANSELA_ANSA7           : bit  absolute ANSELA.7;
  ANSELA_ANSA6           : bit  absolute ANSELA.6;
  ANSELA_ANSA4           : bit  absolute ANSELA.5;
  ANSELA_ANSA3           : bit  absolute ANSELA.4;
  ANSELA_ANSA2           : bit  absolute ANSELA.3;
  ANSELA_ANSA1           : bit  absolute ANSELA.2;
  ANSELA_ANSA0           : bit  absolute ANSELA.1;
  WPUA                   : byte absolute $1F39;
  WPUA_WPUA7             : bit  absolute WPUA.7;
  WPUA_WPUA6             : bit  absolute WPUA.6;
  WPUA_WPUA5             : bit  absolute WPUA.5;
  WPUA_WPUA4             : bit  absolute WPUA.4;
  WPUA_WPUA3             : bit  absolute WPUA.3;
  WPUA_WPUA2             : bit  absolute WPUA.2;
  WPUA_WPUA1             : bit  absolute WPUA.1;
  WPUA_WPUA0             : bit  absolute WPUA.0;
  ODCONA                 : byte absolute $1F3A;
  ODCONA_ODCA7           : bit  absolute ODCONA.7;
  ODCONA_ODCA6           : bit  absolute ODCONA.6;
  ODCONA_ODCA4           : bit  absolute ODCONA.5;
  ODCONA_ODCA3           : bit  absolute ODCONA.4;
  ODCONA_ODCA2           : bit  absolute ODCONA.3;
  ODCONA_ODCA1           : bit  absolute ODCONA.2;
  ODCONA_ODCA0           : bit  absolute ODCONA.1;
  SLRCONA                : byte absolute $1F3B;
  SLRCONA_SLRA7          : bit  absolute SLRCONA.7;
  SLRCONA_SLRA6          : bit  absolute SLRCONA.6;
  SLRCONA_SLRA4          : bit  absolute SLRCONA.5;
  SLRCONA_SLRA3          : bit  absolute SLRCONA.4;
  SLRCONA_SLRA2          : bit  absolute SLRCONA.3;
  SLRCONA_SLRA1          : bit  absolute SLRCONA.2;
  SLRCONA_SLRA0          : bit  absolute SLRCONA.1;
  INLVLA                 : byte absolute $1F3C;
  INLVLA_INLVLA7         : bit  absolute INLVLA.7;
  INLVLA_INLVLA6         : bit  absolute INLVLA.6;
  INLVLA_INLVLA5         : bit  absolute INLVLA.5;
  INLVLA_INLVLA4         : bit  absolute INLVLA.4;
  INLVLA_INLVLA3         : bit  absolute INLVLA.3;
  INLVLA_INLVLA2         : bit  absolute INLVLA.2;
  INLVLA_INLVLA1         : bit  absolute INLVLA.1;
  INLVLA_INLVLA0         : bit  absolute INLVLA.0;
  ANSELB                 : byte absolute $1F43;
  ANSELB_ANSB7           : bit  absolute ANSELB.7;
  ANSELB_ANSB6           : bit  absolute ANSELB.6;
  ANSELB_ANSB5           : bit  absolute ANSELB.5;
  ANSELB_ANSB4           : bit  absolute ANSELB.4;
  ANSELB_ANSB3           : bit  absolute ANSELB.3;
  ANSELB_ANSB2           : bit  absolute ANSELB.2;
  ANSELB_ANSB1           : bit  absolute ANSELB.1;
  ANSELB_ANSB0           : bit  absolute ANSELB.0;
  WPUB                   : byte absolute $1F44;
  WPUB_WPUB7             : bit  absolute WPUB.7;
  WPUB_WPUB6             : bit  absolute WPUB.6;
  WPUB_WPUB5             : bit  absolute WPUB.5;
  WPUB_WPUB4             : bit  absolute WPUB.4;
  WPUB_WPUB3             : bit  absolute WPUB.3;
  WPUB_WPUB2             : bit  absolute WPUB.2;
  WPUB_WPUB1             : bit  absolute WPUB.1;
  WPUB_WPUB0             : bit  absolute WPUB.0;
  ODCONB                 : byte absolute $1F45;
  ODCONB_ODCB7           : bit  absolute ODCONB.7;
  ODCONB_ODCB6           : bit  absolute ODCONB.6;
  ODCONB_ODCB5           : bit  absolute ODCONB.5;
  ODCONB_ODCB4           : bit  absolute ODCONB.4;
  ODCONB_ODCB3           : bit  absolute ODCONB.3;
  ODCONB_ODCB2           : bit  absolute ODCONB.2;
  ODCONB_ODCB1           : bit  absolute ODCONB.1;
  ODCONB_ODCB0           : bit  absolute ODCONB.0;
  SLRCONB                : byte absolute $1F46;
  SLRCONB_SLRB7          : bit  absolute SLRCONB.7;
  SLRCONB_SLRB6          : bit  absolute SLRCONB.6;
  SLRCONB_SLRB5          : bit  absolute SLRCONB.5;
  SLRCONB_SLRB4          : bit  absolute SLRCONB.4;
  SLRCONB_SLRB3          : bit  absolute SLRCONB.3;
  SLRCONB_SLRB2          : bit  absolute SLRCONB.2;
  SLRCONB_SLRB1          : bit  absolute SLRCONB.1;
  SLRCONB_SLRB0          : bit  absolute SLRCONB.0;
  INLVLB                 : byte absolute $1F47;
  INLVLB_INLVLB7         : bit  absolute INLVLB.7;
  INLVLB_INLVLB6         : bit  absolute INLVLB.6;
  INLVLB_INLVLB5         : bit  absolute INLVLB.5;
  INLVLB_INLVLB4         : bit  absolute INLVLB.4;
  INLVLB_INLVLB3         : bit  absolute INLVLB.3;
  INLVLB_INLVLB2         : bit  absolute INLVLB.2;
  INLVLB_INLVLB1         : bit  absolute INLVLB.1;
  INLVLB_INLVLB0         : bit  absolute INLVLB.0;
  IOCBP                  : byte absolute $1F48;
  IOCBP_IOCBP7           : bit  absolute IOCBP.7;
  IOCBP_IOCBP6           : bit  absolute IOCBP.6;
  IOCBP_IOCBP5           : bit  absolute IOCBP.5;
  IOCBP_IOCBP4           : bit  absolute IOCBP.4;
  IOCBP_IOCBP3           : bit  absolute IOCBP.3;
  IOCBP_IOCBP2           : bit  absolute IOCBP.2;
  IOCBP_IOCBP1           : bit  absolute IOCBP.1;
  IOCBP_IOCBP0           : bit  absolute IOCBP.0;
  IOCBN                  : byte absolute $1F49;
  IOCBN_IOCBN7           : bit  absolute IOCBN.7;
  IOCBN_IOCBN6           : bit  absolute IOCBN.6;
  IOCBN_IOCBN5           : bit  absolute IOCBN.5;
  IOCBN_IOCBN4           : bit  absolute IOCBN.4;
  IOCBN_IOCBN3           : bit  absolute IOCBN.3;
  IOCBN_IOCBN2           : bit  absolute IOCBN.2;
  IOCBN_IOCBN1           : bit  absolute IOCBN.1;
  IOCBN_IOCBN0           : bit  absolute IOCBN.0;
  IOCBF                  : byte absolute $1F4A;
  IOCBF_IOCBF7           : bit  absolute IOCBF.7;
  IOCBF_IOCBF6           : bit  absolute IOCBF.6;
  IOCBF_IOCBF5           : bit  absolute IOCBF.5;
  IOCBF_IOCBF4           : bit  absolute IOCBF.4;
  IOCBF_IOCBF3           : bit  absolute IOCBF.3;
  IOCBF_IOCBF2           : bit  absolute IOCBF.2;
  IOCBF_IOCBF1           : bit  absolute IOCBF.1;
  IOCBF_IOCBF0           : bit  absolute IOCBF.0;
  ANSELC                 : byte absolute $1F4E;
  ANSELC_ANSC7           : bit  absolute ANSELC.7;
  ANSELC_ANSC6           : bit  absolute ANSELC.6;
  ANSELC_ANSC5           : bit  absolute ANSELC.5;
  ANSELC_ANSC4           : bit  absolute ANSELC.4;
  ANSELC_ANSC3           : bit  absolute ANSELC.3;
  ANSELC_ANSC2           : bit  absolute ANSELC.2;
  ANSELC_ANSC1           : bit  absolute ANSELC.1;
  ANSELC_ANSC0           : bit  absolute ANSELC.0;
  WPUC                   : byte absolute $1F4F;
  WPUC_WPUC7             : bit  absolute WPUC.7;
  WPUC_WPUC6             : bit  absolute WPUC.6;
  WPUC_WPUC5             : bit  absolute WPUC.5;
  WPUC_WPUC4             : bit  absolute WPUC.4;
  WPUC_WPUC3             : bit  absolute WPUC.3;
  WPUC_WPUC2             : bit  absolute WPUC.2;
  WPUC_WPUC1             : bit  absolute WPUC.1;
  WPUC_WPUC0             : bit  absolute WPUC.0;
  ODCONC                 : byte absolute $1F50;
  ODCONC_ODCC7           : bit  absolute ODCONC.7;
  ODCONC_ODCC6           : bit  absolute ODCONC.6;
  ODCONC_ODCC5           : bit  absolute ODCONC.5;
  ODCONC_ODCC4           : bit  absolute ODCONC.4;
  ODCONC_ODCC3           : bit  absolute ODCONC.3;
  ODCONC_ODCC2           : bit  absolute ODCONC.2;
  ODCONC_ODCC1           : bit  absolute ODCONC.1;
  ODCONC_ODCC0           : bit  absolute ODCONC.0;
  SLRCONC                : byte absolute $1F51;
  SLRCONC_SLRC7          : bit  absolute SLRCONC.7;
  SLRCONC_SLRC6          : bit  absolute SLRCONC.6;
  SLRCONC_SLRC5          : bit  absolute SLRCONC.5;
  SLRCONC_SLRC4          : bit  absolute SLRCONC.4;
  SLRCONC_SLRC3          : bit  absolute SLRCONC.3;
  SLRCONC_SLRC2          : bit  absolute SLRCONC.2;
  SLRCONC_SLRC1          : bit  absolute SLRCONC.1;
  SLRCONC_SLRC0          : bit  absolute SLRCONC.0;
  INLVLC                 : byte absolute $1F52;
  INLVLC_INLVLC7         : bit  absolute INLVLC.7;
  INLVLC_INLVLC6         : bit  absolute INLVLC.6;
  INLVLC_INLVLC5         : bit  absolute INLVLC.5;
  INLVLC_INLVLC4         : bit  absolute INLVLC.4;
  INLVLC_INLVLC3         : bit  absolute INLVLC.3;
  INLVLC_INLVLC2         : bit  absolute INLVLC.2;
  INLVLC_INLVLC1         : bit  absolute INLVLC.1;
  INLVLC_INLVLC0         : bit  absolute INLVLC.0;
  IOCCP                  : byte absolute $1F53;
  IOCCP_IOCCP7           : bit  absolute IOCCP.7;
  IOCCP_IOCCP6           : bit  absolute IOCCP.6;
  IOCCP_IOCCP5           : bit  absolute IOCCP.5;
  IOCCP_IOCCP4           : bit  absolute IOCCP.4;
  IOCCP_IOCCP3           : bit  absolute IOCCP.3;
  IOCCP_IOCCP2           : bit  absolute IOCCP.2;
  IOCCP_IOCCP1           : bit  absolute IOCCP.1;
  IOCCP_IOCCP0           : bit  absolute IOCCP.0;
  IOCCN                  : byte absolute $1F54;
  IOCCN_IOCCN7           : bit  absolute IOCCN.7;
  IOCCN_IOCCN6           : bit  absolute IOCCN.6;
  IOCCN_IOCCN5           : bit  absolute IOCCN.5;
  IOCCN_IOCCN4           : bit  absolute IOCCN.4;
  IOCCN_IOCCN3           : bit  absolute IOCCN.3;
  IOCCN_IOCCN2           : bit  absolute IOCCN.2;
  IOCCN_IOCCN1           : bit  absolute IOCCN.1;
  IOCCN_IOCCN0           : bit  absolute IOCCN.0;
  IOCCF                  : byte absolute $1F55;
  IOCCF_IOCCF7           : bit  absolute IOCCF.7;
  IOCCF_IOCCF6           : bit  absolute IOCCF.6;
  IOCCF_IOCCF5           : bit  absolute IOCCF.5;
  IOCCF_IOCCF4           : bit  absolute IOCCF.4;
  IOCCF_IOCCF3           : bit  absolute IOCCF.3;
  IOCCF_IOCCF2           : bit  absolute IOCCF.2;
  IOCCF_IOCCF1           : bit  absolute IOCCF.1;
  IOCCF_IOCCF0           : bit  absolute IOCCF.0;
  ANSELD                 : byte absolute $1F59;
  ANSELD_ANSD7           : bit  absolute ANSELD.7;
  ANSELD_ANSD6           : bit  absolute ANSELD.6;
  ANSELD_ANSD5           : bit  absolute ANSELD.5;
  ANSELD_ANSD4           : bit  absolute ANSELD.4;
  ANSELD_ANSD3           : bit  absolute ANSELD.3;
  ANSELD_ANSD2           : bit  absolute ANSELD.2;
  ANSELD_ANSD1           : bit  absolute ANSELD.1;
  ANSELD_ANSD0           : bit  absolute ANSELD.0;
  WPUD                   : byte absolute $1F5A;
  WPUD_WPUD7             : bit  absolute WPUD.7;
  WPUD_WPUD6             : bit  absolute WPUD.6;
  WPUD_WPUD5             : bit  absolute WPUD.5;
  WPUD_WPUD4             : bit  absolute WPUD.4;
  WPUD_WPUD3             : bit  absolute WPUD.3;
  WPUD_WPUD2             : bit  absolute WPUD.2;
  WPUD_WPUD1             : bit  absolute WPUD.1;
  WPUD_WPUD0             : bit  absolute WPUD.0;
  ODCOND                 : byte absolute $1F5B;
  ODCOND_ODCD7           : bit  absolute ODCOND.7;
  ODCOND_ODCD6           : bit  absolute ODCOND.6;
  ODCOND_ODCD5           : bit  absolute ODCOND.5;
  ODCOND_ODCD4           : bit  absolute ODCOND.4;
  ODCOND_ODCD3           : bit  absolute ODCOND.3;
  ODCOND_ODCD2           : bit  absolute ODCOND.2;
  ODCOND_ODCD1           : bit  absolute ODCOND.1;
  ODCOND_ODCD0           : bit  absolute ODCOND.0;
  SLRCOND                : byte absolute $1F5C;
  SLRCOND_SLRD7          : bit  absolute SLRCOND.7;
  SLRCOND_SLRD6          : bit  absolute SLRCOND.6;
  SLRCOND_SLRD5          : bit  absolute SLRCOND.5;
  SLRCOND_SLRD4          : bit  absolute SLRCOND.4;
  SLRCOND_SLRD3          : bit  absolute SLRCOND.3;
  SLRCOND_SLRD2          : bit  absolute SLRCOND.2;
  SLRCOND_SLRD1          : bit  absolute SLRCOND.1;
  SLRCOND_SLRD0          : bit  absolute SLRCOND.0;
  INLVLD                 : byte absolute $1F5D;
  INLVLD_INLVLD7         : bit  absolute INLVLD.7;
  INLVLD_INLVLD6         : bit  absolute INLVLD.6;
  INLVLD_INLVLD5         : bit  absolute INLVLD.5;
  INLVLD_INLVLD4         : bit  absolute INLVLD.4;
  INLVLD_INLVLD3         : bit  absolute INLVLD.3;
  INLVLD_INLVLD2         : bit  absolute INLVLD.2;
  INLVLD_INLVLD1         : bit  absolute INLVLD.1;
  INLVLD_INLVLD0         : bit  absolute INLVLD.0;
  ANSELE                 : byte absolute $1F64;
  ANSELE_ANSE7           : bit  absolute ANSELE.7;
  ANSELE_ANSE6           : bit  absolute ANSELE.6;
  ANSELE_ANSE5           : bit  absolute ANSELE.5;
  ANSELE_ANSE4           : bit  absolute ANSELE.4;
  ANSELE_ANSE3           : bit  absolute ANSELE.3;
  ANSELE_ANSE1           : bit  absolute ANSELE.2;
  ANSELE_ANSE0           : bit  absolute ANSELE.1;
  WPUE                   : byte absolute $1F65;
  WPUE_WPUE7             : bit  absolute WPUE.7;
  WPUE_WPUE6             : bit  absolute WPUE.6;
  WPUE_WPUE5             : bit  absolute WPUE.5;
  WPUE_WPUE4             : bit  absolute WPUE.4;
  WPUE_WPUE3             : bit  absolute WPUE.3;
  WPUE_WPUE1             : bit  absolute WPUE.2;
  WPUE_WPUE0             : bit  absolute WPUE.1;
  ODCONE                 : byte absolute $1F66;
  ODCONE_ODCE7           : bit  absolute ODCONE.7;
  ODCONE_ODCE6           : bit  absolute ODCONE.6;
  ODCONE_ODCE5           : bit  absolute ODCONE.5;
  ODCONE_ODCE4           : bit  absolute ODCONE.4;
  ODCONE_ODCE3           : bit  absolute ODCONE.3;
  ODCONE_ODCE1           : bit  absolute ODCONE.2;
  ODCONE_ODCE0           : bit  absolute ODCONE.1;
  SLRCONE                : byte absolute $1F67;
  SLRCONE_SLRE7          : bit  absolute SLRCONE.7;
  SLRCONE_SLRE6          : bit  absolute SLRCONE.6;
  SLRCONE_SLRE5          : bit  absolute SLRCONE.5;
  SLRCONE_SLRE4          : bit  absolute SLRCONE.4;
  SLRCONE_SLRE3          : bit  absolute SLRCONE.3;
  SLRCONE_SLRE1          : bit  absolute SLRCONE.2;
  SLRCONE_SLRE0          : bit  absolute SLRCONE.1;
  INLVLE                 : byte absolute $1F68;
  INLVLE_INLVLE7         : bit  absolute INLVLE.7;
  INLVLE_INLVLE6         : bit  absolute INLVLE.6;
  INLVLE_INLVLE5         : bit  absolute INLVLE.5;
  INLVLE_INLVLE4         : bit  absolute INLVLE.4;
  INLVLE_INLVLE3         : bit  absolute INLVLE.3;
  INLVLE_INLVLE1         : bit  absolute INLVLE.2;
  INLVLE_INLVLE0         : bit  absolute INLVLE.1;
  IOCEP                  : byte absolute $1F69;
  IOCEP_IOCEP7           : bit  absolute IOCEP.7;
  IOCEP_IOCEP6           : bit  absolute IOCEP.6;
  IOCEP_IOCEP5           : bit  absolute IOCEP.5;
  IOCEP_IOCEP4           : bit  absolute IOCEP.4;
  IOCEP_IOCEP3           : bit  absolute IOCEP.3;
  IOCEP_IOCEP1           : bit  absolute IOCEP.2;
  IOCEP_IOCEP0           : bit  absolute IOCEP.1;
  IOCEN                  : byte absolute $1F6A;
  IOCEN_IOCEN7           : bit  absolute IOCEN.7;
  IOCEN_IOCEN6           : bit  absolute IOCEN.6;
  IOCEN_IOCEN5           : bit  absolute IOCEN.5;
  IOCEN_IOCEN4           : bit  absolute IOCEN.4;
  IOCEN_IOCEN3           : bit  absolute IOCEN.3;
  IOCEN_IOCEN1           : bit  absolute IOCEN.2;
  IOCEN_IOCEN0           : bit  absolute IOCEN.1;
  IOCEF                  : byte absolute $1F6B;
  IOCEF_IOCEF7           : bit  absolute IOCEF.7;
  IOCEF_IOCEF6           : bit  absolute IOCEF.6;
  IOCEF_IOCEF5           : bit  absolute IOCEF.5;
  IOCEF_IOCEF4           : bit  absolute IOCEF.4;
  IOCEF_IOCEF3           : bit  absolute IOCEF.3;
  IOCEF_IOCEF1           : bit  absolute IOCEF.2;
  IOCEF_IOCEF0           : bit  absolute IOCEF.1;
  STATUS_SHAD            : byte absolute $1FE4;
  STATUS_SHAD_Z_SHAD     : bit  absolute STATUS_SHAD.2;
  STATUS_SHAD_DC_SHAD    : bit  absolute STATUS_SHAD.1;
  STATUS_SHAD_C_SHAD     : bit  absolute STATUS_SHAD.0;
  WREG_SHAD              : byte absolute $1FE5;
  BSR_SHAD               : byte absolute $1FE6;
  PCLATH_SHAD            : byte absolute $1FE7;
  FSR0L_SHAD             : byte absolute $1FE8;
  FSR0H_SHAD             : byte absolute $1FE9;
  FSR1L_SHAD             : byte absolute $1FEA;
  FSR1H_SHAD             : byte absolute $1FEB;
  STKPTR                 : byte absolute $1FED;
  TOSL                   : byte absolute $1FEE;
  TOSH                   : byte absolute $1FEF;


  // -- Define RAM state values --

  {$CLEAR_STATE_RAM}

  {$SET_STATE_RAM '000-01D:SFR'}  // INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON, PORTA, PORTB, PORTC, PORTD, PORTE, PORTF, TRISA, TRISB, TRISC, TRISD, TRISE, TRISF, LATA, LATB, LATC, LATD, LATE, LATF
  {$SET_STATE_RAM '01F-01F:SFR'}  // ADCPCON0
  {$SET_STATE_RAM '020-06F:GPR'} 
  {$SET_STATE_RAM '070-07F:GPR'} 
  {$SET_STATE_RAM '080-080:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '08C-09F:SFR'}  // ADLTHL, ADLTHH, ADUTHL, ADUTHH, ADERRL, ADERRH, ADSTPTL, ADSTPTH, ADFLTRL, ADFLTRH, ADACCL, ADACCH, ADACCU, ADCNT, ADRPT, ADPREVL, ADPREVH, ADRESL, ADRESH, ADPCH
  {$SET_STATE_RAM '0A0-0EF:GPR'} 
  {$SET_STATE_RAM '0F0-0FF:GPR'} 
  {$SET_STATE_RAM '100-100:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '10C-11F:SFR'}  // ADACQL, ADACQH, ADCAP, ADPREL, ADPREH, ADCON0, ADCON1, ADCON2, ADCON3, ADSTAT, ADREF, ADACT, ADCLK, RC1REG, TX1REG, SP1BRGL, SP1BRGH, RC1STA, TX1STA, BAUD1CON
  {$SET_STATE_RAM '120-16F:GPR'} 
  {$SET_STATE_RAM '170-17F:GPR'} 
  {$SET_STATE_RAM '180-180:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '18C-192:SFR'}  // SSP1BUF, SSP1ADD, SSP1MSK, SSP1STAT, SSP1CON1, SSP1CON2, SSP1CON3
  {$SET_STATE_RAM '1A0-1EF:GPR'} 
  {$SET_STATE_RAM '1F0-1FF:GPR'} 
  {$SET_STATE_RAM '200-200:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '20C-211:SFR'}  // TMR1L, TMR1H, T1CON, T1GCON, T1GATE, T1CLK
  {$SET_STATE_RAM '21E-21E:SFR'}  // CCPTMRS0
  {$SET_STATE_RAM '220-26F:GPR'} 
  {$SET_STATE_RAM '270-27F:GPR'} 
  {$SET_STATE_RAM '280-280:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '28C-297:SFR'}  // T2TMR, T2PR, T2CON, T2HLT, T2CLKCON, T2RST, T4TMR, T4PR, T4CON, T4HLT, T4CLKCON, T4RST
  {$SET_STATE_RAM '2A0-2EF:GPR'} 
  {$SET_STATE_RAM '2F0-2FF:GPR'} 
  {$SET_STATE_RAM '300-300:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '30C-316:SFR'}  // CCPR1L, CCPR1H, CCP1CON, CCP1CAP, CCPR2L, CCPR2H, CCP2CON, CCP2CAP, PWM3DCL, PWM3DCH, PWM3CON
  {$SET_STATE_RAM '318-31A:SFR'}  // PWM4DCL, PWM4DCH, PWM4CON
  {$SET_STATE_RAM '320-36F:GPR'} 
  {$SET_STATE_RAM '370-37F:GPR'} 
  {$SET_STATE_RAM '380-380:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '3A0-3EF:GPR'} 
  {$SET_STATE_RAM '3F0-3FF:GPR'} 
  {$SET_STATE_RAM '400-400:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '420-46F:GPR'} 
  {$SET_STATE_RAM '470-47F:GPR'} 
  {$SET_STATE_RAM '480-480:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '48C-49D:SFR'}  // SMT1TMRL, SMT1TMRH, SMT1TMRU, SMT1CPRL, SMT1CPRH, SMT1CPRU, SMT1CPWL, SMT1CPWH, SMT1CPWU, SMT1PRL, SMT1PRH, SMT1PRU, SMT1CON0, SMT1CON1, SMT1STAT, SMT1CLK, SMT1SIG, SMT1WIN
  {$SET_STATE_RAM '4A0-4EF:GPR'} 
  {$SET_STATE_RAM '4F0-4FF:GPR'} 
  {$SET_STATE_RAM '500-500:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '520-56F:GPR'} 
  {$SET_STATE_RAM '570-57F:GPR'} 
  {$SET_STATE_RAM '580-580:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '59C-59F:SFR'}  // TMR0L, TMR0H, T0CON0, T0CON1
  {$SET_STATE_RAM '5A0-5EF:GPR'} 
  {$SET_STATE_RAM '5F0-5FF:GPR'} 
  {$SET_STATE_RAM '600-600:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '60C-614:SFR'}  // CWG1CLKCON, CWG1ISM, CWG1DBR, CWG1DBF, CWG1CON0, CWG1CON1, CWG1AS0, CWG1AS1, CWG1STR
  {$SET_STATE_RAM '620-66F:GPR'} 
  {$SET_STATE_RAM '670-67F:GPR'} 
  {$SET_STATE_RAM '680-680:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '6A0-6EF:GPR'} 
  {$SET_STATE_RAM '6F0-6FF:GPR'} 
  {$SET_STATE_RAM '700-700:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '70C-714:SFR'}  // PIR0, PIR1, PIR2, PIR3, PIR4, PIR5, PIR6, PIR7, PIR8
  {$SET_STATE_RAM '716-71E:SFR'}  // PIE0, PIE1, PIE2, PIE3, PIE4, PIE5, PIE6, PIE7, PIE8
  {$SET_STATE_RAM '720-76F:GPR'} 
  {$SET_STATE_RAM '770-77F:GPR'} 
  {$SET_STATE_RAM '780-780:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '796-79B:SFR'}  // PMD0, PMD1, PMD2, PMD3, PMD4, PMD5
  {$SET_STATE_RAM '7A0-7EF:GPR'} 
  {$SET_STATE_RAM '7F0-7FF:GPR'} 
  {$SET_STATE_RAM '800-800:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '80C-814:SFR'}  // WDTCON0, WDTCON1, WDTPSL, WDTPSH, WDTTMR, BORCON, VREGCON, PCON0, PCON1
  {$SET_STATE_RAM '81A-81F:SFR'}  // NVMADRL, NVMADRH, NVMDATL, NVMDATH, NVMCON1, NVMCON2
  {$SET_STATE_RAM '820-86F:GPR'} 
  {$SET_STATE_RAM '870-87F:GPR'} 
  {$SET_STATE_RAM '880-880:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '88C-894:SFR'}  // CPUDOZE, OSCCON1, OSCCON2, OSCCON3, OSCSTAT, OSCEN, OSCTUNE, OSCFRQ, ACTCON
  {$SET_STATE_RAM '8A0-8EF:GPR'} 
  {$SET_STATE_RAM '8F0-8FF:GPR'} 
  {$SET_STATE_RAM '900-900:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '90C-90C:SFR'}  // FVRCON
  {$SET_STATE_RAM '90E-90F:SFR'}  // DAC1CON0, DAC1CON1
  {$SET_STATE_RAM '91F-91F:SFR'}  // ZCDCON
  {$SET_STATE_RAM '920-96F:GPR'} 
  {$SET_STATE_RAM '970-97F:GPR'} 
  {$SET_STATE_RAM '980-980:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '98F-997:SFR'}  // CMOUT, CM1CON0, CM1CON1, CM1NSEL, CM1PSEL, CM2CON0, CM2CON1, CM2NSEL, CM2PSEL
  {$SET_STATE_RAM '9A0-9EF:GPR'} 
  {$SET_STATE_RAM '9F0-9FF:GPR'} 
  {$SET_STATE_RAM 'A00-A00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'A19-A1F:SFR'}  // RC2REG, TX2REG, SP2BRGL, SP2BRGH, RC2STA, TX2STA, BAUD2CON
  {$SET_STATE_RAM 'A20-A6F:GPR'} 
  {$SET_STATE_RAM 'A70-A7F:GPR'} 
  {$SET_STATE_RAM 'A80-A80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'AA0-AEF:GPR'} 
  {$SET_STATE_RAM 'AF0-AFF:GPR'} 
  {$SET_STATE_RAM 'B00-B00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'B20-B6F:GPR'} 
  {$SET_STATE_RAM 'B70-B7F:GPR'} 
  {$SET_STATE_RAM 'B80-B80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'BA0-BEF:GPR'} 
  {$SET_STATE_RAM 'BF0-BFF:GPR'} 
  {$SET_STATE_RAM 'C00-C00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'C0C-C1C:SFR'}  // RTCCON, RTCCAL, ALRMCON, ALRMRPT, YEAR, MONTH, WEEKDAY, DAY, HOURS, MINUTES, SECONDS, ALRMMTH, ALRMWD, ALRMDAY, ALRMHR, ALRMMIN, ALRMSEC
  {$SET_STATE_RAM 'C20-C6F:GPR'} 
  {$SET_STATE_RAM 'C70-C7F:GPR'} 
  {$SET_STATE_RAM 'C80-C80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'C8C-C91:SFR'}  // PORTG, PORTH, TRISG, TRISH, LATG, LATH
  {$SET_STATE_RAM 'CA0-CEF:GPR'} 
  {$SET_STATE_RAM 'CF0-CFF:GPR'} 
  {$SET_STATE_RAM 'D00-D00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'D20-D6F:GPR'} 
  {$SET_STATE_RAM 'D70-D7F:GPR'} 
  {$SET_STATE_RAM 'D80-D80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'DA0-DEF:GPR'} 
  {$SET_STATE_RAM 'DF0-DFF:GPR'} 
  {$SET_STATE_RAM 'E00-E00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'E20-E6F:GPR'} 
  {$SET_STATE_RAM 'E70-E7F:GPR'} 
  {$SET_STATE_RAM 'E80-E80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'E8C-E8F:SFR'}  // VB0GPR, VB1GPR, VB2GPR, VB3GPR
  {$SET_STATE_RAM 'EA0-EEF:GPR'} 
  {$SET_STATE_RAM 'EF0-EFF:GPR'} 
  {$SET_STATE_RAM 'F00-F00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'F20-F6F:GPR'} 
  {$SET_STATE_RAM 'F70-F7F:GPR'} 
  {$SET_STATE_RAM 'F80-F80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM 'FA0-FEF:GPR'} 
  {$SET_STATE_RAM 'FF0-FFF:GPR'} 
  {$SET_STATE_RAM '1000-1000:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1020-106F:GPR'} 
  {$SET_STATE_RAM '1070-107F:GPR'} 
  {$SET_STATE_RAM '1080-1080:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '10A0-10EF:GPR'} 
  {$SET_STATE_RAM '10F0-10FF:GPR'} 
  {$SET_STATE_RAM '1100-1100:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1120-116F:GPR'} 
  {$SET_STATE_RAM '1170-117F:GPR'} 
  {$SET_STATE_RAM '1180-1180:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '11A0-11EF:GPR'} 
  {$SET_STATE_RAM '11F0-11FF:GPR'} 
  {$SET_STATE_RAM '1200-1200:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1220-126F:GPR'} 
  {$SET_STATE_RAM '1270-127F:GPR'} 
  {$SET_STATE_RAM '1280-1280:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '12A0-12EF:GPR'} 
  {$SET_STATE_RAM '12F0-12FF:GPR'} 
  {$SET_STATE_RAM '1300-1300:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1320-136F:GPR'} 
  {$SET_STATE_RAM '1370-137F:GPR'} 
  {$SET_STATE_RAM '1380-1380:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '13A0-13EF:GPR'} 
  {$SET_STATE_RAM '13F0-13FF:GPR'} 
  {$SET_STATE_RAM '1400-1400:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1420-146F:GPR'} 
  {$SET_STATE_RAM '1470-147F:GPR'} 
  {$SET_STATE_RAM '1480-1480:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '14A0-14EF:GPR'} 
  {$SET_STATE_RAM '14F0-14FF:GPR'} 
  {$SET_STATE_RAM '1500-1500:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1520-156F:GPR'} 
  {$SET_STATE_RAM '1570-157F:GPR'} 
  {$SET_STATE_RAM '1580-1580:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '15A0-15EF:GPR'} 
  {$SET_STATE_RAM '15F0-15FF:GPR'} 
  {$SET_STATE_RAM '1600-1600:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1620-166F:GPR'} 
  {$SET_STATE_RAM '1670-167F:GPR'} 
  {$SET_STATE_RAM '1680-1680:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '16A0-16EF:GPR'} 
  {$SET_STATE_RAM '16F0-16FF:GPR'} 
  {$SET_STATE_RAM '1700-1700:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1720-176F:GPR'} 
  {$SET_STATE_RAM '1770-177F:GPR'} 
  {$SET_STATE_RAM '1780-1780:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '17A0-17EF:GPR'} 
  {$SET_STATE_RAM '17F0-17FF:GPR'} 
  {$SET_STATE_RAM '1800-1800:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1820-186F:GPR'} 
  {$SET_STATE_RAM '1870-187F:GPR'} 
  {$SET_STATE_RAM '1880-1880:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '18A0-18EF:GPR'} 
  {$SET_STATE_RAM '18F0-18FF:GPR'} 
  {$SET_STATE_RAM '1900-1900:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1920-196F:GPR'} 
  {$SET_STATE_RAM '1970-197F:GPR'} 
  {$SET_STATE_RAM '1980-1980:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '19F0-19FF:GPR'} 
  {$SET_STATE_RAM '1A00-1A00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1A70-1A7F:GPR'} 
  {$SET_STATE_RAM '1A80-1A80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1AF0-1AFF:GPR'} 
  {$SET_STATE_RAM '1B00-1B00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1B70-1B7F:GPR'} 
  {$SET_STATE_RAM '1B80-1B80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1BF0-1BFF:GPR'} 
  {$SET_STATE_RAM '1C00-1C00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1C70-1C7F:GPR'} 
  {$SET_STATE_RAM '1C80-1C80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1CF0-1CFF:GPR'} 
  {$SET_STATE_RAM '1D00-1D00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1D0C-1D47:SFR'}  // LCDCON, LCDPS, LCDSE0, LCDSE1, LCDSE2, LCDSE3, LCDSE4, LCDSE5, LCDVCON1, LCDVCON2, LCDREF, LCDRL, LCDDATA0, LCDDATA1, LCDDATA2, LCDDATA3, LCDDATA4, LCDDATA5, LCDDATA6, LCDDATA7, LCDDATA8, LCDDATA9, LCDDATA10, LCDDATA11, LCDDATA12, LCDDATA13, LCDDATA14, LCDDATA15, LCDDATA16, LCDDATA17, LCDDATA18, LCDDATA19, LCDDATA20, LCDDATA21, LCDDATA22, LCDDATA23, LCDDATA24, LCDDATA25, LCDDATA26, LCDDATA27, LCDDATA28, LCDDATA29, LCDDATA30, LCDDATA31, LCDDATA32, LCDDATA33, LCDDATA34, LCDDATA35, LCDDATA36, LCDDATA37, LCDDATA38, LCDDATA39, LCDDATA40, LCDDATA41, LCDDATA42, LCDDATA43, LCDDATA44, LCDDATA45, LCDDATA46, LCDDATA47
  {$SET_STATE_RAM '1D70-1D7F:GPR'} 
  {$SET_STATE_RAM '1D80-1D80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1DF0-1DFF:GPR'} 
  {$SET_STATE_RAM '1E00-1E00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1E0F-1E44:SFR'}  // CLCDATA, CLC1CON, CLC1POL, CLC1SEL0, CLC1SEL1, CLC1SEL2, CLC1SEL3, CLC1GLS0, CLC1GLS1, CLC1GLS2, CLC1GLS3, CLC2CON, CLC2POL, CLC2SEL0, CLC2SEL1, CLC2SEL2, CLC2SEL3, CLC2GLS0, CLC2GLS1, CLC2GLS2, CLC2GLS3, CLC3CON, CLC3POL, CLC3SEL0, CLC3SEL1, CLC3SEL2, CLC3SEL3, CLC3GLS0, CLC3GLS1, CLC3GLS2, CLC3GLS3, CLC4CON, CLC4POL, CLC4SEL0, CLC4SEL1, CLC4SEL2, CLC4SEL3, CLC4GLS0, CLC4GLS1, CLC4GLS2, CLC4GLS3, RF0PPS, RF1PPS, RF2PPS, RF3PPS, RF4PPS, RF5PPS, RF6PPS, RF7PPS, RG0PPS, RG1PPS, RG2PPS, RG3PPS, RG4PPS
  {$SET_STATE_RAM '1E46-1E4B:SFR'}  // RG6PPS, RG7PPS, RH0PPS, RH1PPS, RH2PPS, RH3PPS
  {$SET_STATE_RAM '1E50-1E55:SFR'}  // ANSELF, WPUF, ODCONF, SLRCONF, INLVLF, HIDRVF
  {$SET_STATE_RAM '1E5B-1E62:SFR'}  // ANSELG, WPUG, ODCONG, SLRCONG, INLVLG, IOCGP, IOCGN, IOCGF
  {$SET_STATE_RAM '1E67-1E6A:SFR'}  // WPUH, ODCONH, SLRCONH, INLVLH
  {$SET_STATE_RAM '1E70-1E7F:GPR'} 
  {$SET_STATE_RAM '1E80-1E80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1E8F-1E93:SFR'}  // PPSLOCK, INTPPS, T0CKIPPS, T1CKIPPS, T1GPPS
  {$SET_STATE_RAM '1E9C-1E9D:SFR'}  // T2INPPS, T4INPPS
  {$SET_STATE_RAM '1EA1-1EA2:SFR'}  // CCP1PPS, CCP2PPS
  {$SET_STATE_RAM '1EA9-1EAA:SFR'}  // SMT1WINPPS, SMT1SIGPPS
  {$SET_STATE_RAM '1EB1-1EB1:SFR'}  // CWG1PPS
  {$SET_STATE_RAM '1EBB-1EBE:SFR'}  // CLCIN0PPS, CLCIN1PPS, CLCIN2PPS, CLCIN3PPS
  {$SET_STATE_RAM '1EC3-1EC3:SFR'}  // ADACTPPS
  {$SET_STATE_RAM '1EC5-1EC7:SFR'}  // SSP1CLKPPS, SSP1DATPPS, SSP1SSPPS
  {$SET_STATE_RAM '1ECB-1ECE:SFR'}  // RX1PPS, TX1PPS, RX2PPS, TX2PPS
  {$SET_STATE_RAM '1EF0-1EFF:GPR'} 
  {$SET_STATE_RAM '1F00-1F00:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1F10-1F31:SFR'}  // RA0PPS, RA1PPS, RA2PPS, RA3PPS, RA4PPS, RA5PPS, RA6PPS, RA7PPS, RB0PPS, RB1PPS, RB2PPS, RB3PPS, RB4PPS, RB5PPS, RB6PPS, RB7PPS, RC0PPS, RC1PPS, RC2PPS, RC3PPS, RC4PPS, RC5PPS, RC6PPS, RC7PPS, RD0PPS, RD1PPS, RD2PPS, RD3PPS, RD4PPS, RD5PPS, RD6PPS, RD7PPS, RE0PPS, RE1PPS
  {$SET_STATE_RAM '1F33-1F3C:SFR'}  // RE3PPS, RE4PPS, RE5PPS, RE6PPS, RE7PPS, ANSELA, WPUA, ODCONA, SLRCONA, INLVLA
  {$SET_STATE_RAM '1F43-1F4A:SFR'}  // ANSELB, WPUB, ODCONB, SLRCONB, INLVLB, IOCBP, IOCBN, IOCBF
  {$SET_STATE_RAM '1F4E-1F55:SFR'}  // ANSELC, WPUC, ODCONC, SLRCONC, INLVLC, IOCCP, IOCCN, IOCCF
  {$SET_STATE_RAM '1F59-1F5D:SFR'}  // ANSELD, WPUD, ODCOND, SLRCOND, INLVLD
  {$SET_STATE_RAM '1F64-1F6B:SFR'}  // ANSELE, WPUE, ODCONE, SLRCONE, INLVLE, IOCEP, IOCEN, IOCEF
  {$SET_STATE_RAM '1F70-1F7F:GPR'} 
  {$SET_STATE_RAM '1F80-1F80:SFR'}  // mapped to INDF0
  {$SET_STATE_RAM '1FE4-1FEB:SFR'}  // STATUS_SHAD, WREG_SHAD, BSR_SHAD, PCLATH_SHAD, FSR0L_SHAD, FSR0H_SHAD, FSR1L_SHAD, FSR1H_SHAD
  {$SET_STATE_RAM '1FED-1FEF:SFR'}  // STKPTR, TOSL, TOSH
  {$SET_STATE_RAM '1FF0-1FFF:GPR'} 


  // -- Define mapped RAM --

  {$SET_MAPPED_RAM '080-08B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '0F0-0FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '100-10B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1000-100B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1070-107F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1080-108B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '10F0-10FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1100-110B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1170-117F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1180-118B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '11F0-11FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1200-120B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1270-127F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1280-128B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '12F0-12FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1300-130B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1370-137F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1380-138B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '13F0-13FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1400-140B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1470-147F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1480-148B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '14F0-14FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1500-150B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1570-157F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1580-158B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '15F0-15FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1600-160B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1670-167F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1680-168B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '16F0-16FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '170-17F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1700-170B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1770-177F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1780-178B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '17F0-17FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '180-18B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1800-180B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1870-187F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1880-188B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '18F0-18FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1900-190B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1970-197F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1980-198B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '19F0-19FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1A00-1A0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1A70-1A7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1A80-1A8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1AF0-1AFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1B00-1B0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1B70-1B7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1B80-1B8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1BF0-1BFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1C00-1C0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1C70-1C7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1C80-1C8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1CF0-1CFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1D00-1D0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1D70-1D7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1D80-1D8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1DF0-1DFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1E00-1E0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1E70-1E7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1E80-1E8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1EF0-1EFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1F0-1FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1F00-1F0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1F70-1F7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '1F80-1F8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '1FF0-1FFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '200-20B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '270-27F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '280-28B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '2F0-2FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '300-30B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '370-37F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '380-38B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '3F0-3FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '400-40B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '470-47F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '480-48B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '4F0-4FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '500-50B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '570-57F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '580-58B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '5F0-5FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '600-60B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '670-67F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '680-68B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '6F0-6FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '700-70B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '770-77F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '780-78B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '7F0-7FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '800-80B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '870-87F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '880-88B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '8F0-8FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '900-90B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '970-97F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM '980-98B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM '9F0-9FF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'A00-A0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'A70-A7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'A80-A8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'AF0-AFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'B00-B0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'B70-B7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'B80-B8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'BF0-BFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'C00-C0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'C70-C7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'C80-C8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'CF0-CFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'D00-D0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'D70-D7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'D80-D8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'DF0-DFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'E00-E0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'E70-E7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'E80-E8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'EF0-EFF:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'F00-F0B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'F70-F7F:bnk0'} // maps to area 070-07F (bank 0)
  {$SET_MAPPED_RAM 'F80-F8B:bnk0'} // maps to INDF0, INDF1, PCL, STATUS, FSR0L, FSR0H, FSR1L, FSR1H, BSR, WREG, PCLATH, INTCON (bank 0)
  {$SET_MAPPED_RAM 'FF0-FFF:bnk0'} // maps to area 070-07F (bank 0)


  // -- Un-implemented fields --

  {$SET_UNIMP_BITS '003:1F'} // STATUS bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '008:3F'} // BSR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00A:7F'} // PCLATH bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '00B:C1'} // INTCON bits 5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '010:FB'} // PORTE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '012:DF'} // TRISA bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '016:FB'} // TRISE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '018:DF'} // LATA bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01C:FB'} // LATE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '01F:81'} // ADCPCON0 bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '09F:3F'} // ADPCH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10D:1F'} // ADACQH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '10E:1F'} // ADCAP bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '110:1F'} // ADPREH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '111:D5'} // ADCON0 bits 5,3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '112:E1'} // ADCON1 bits 4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '114:7F'} // ADCON3 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '115:F7'} // ADSTAT bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '116:03'} // ADREF bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '117:1F'} // ADACT bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '118:3F'} // ADCLK bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '11F:DB'} // BAUD1CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20E:37'} // T1CON bits 7,6,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '20F:FC'} // T1GCON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '210:1F'} // T1GATE bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '211:0F'} // T1CLK bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '290:0F'} // T2CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '291:1F'} // T2RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '296:0F'} // T4CLKCON bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '297:1F'} // T4RST bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '30F:07'} // CCP1CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '313:07'} // CCP2CAP bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '314:C0'} // PWM3DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '316:B0'} // PWM3CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '318:C0'} // PWM4DCL bits 5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '31A:B0'} // PWM4CON bits 6,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '498:BF'} // SMT1CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '499:CF'} // SMT1CON1 bits 5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49A:E7'} // SMT1STAT bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49B:07'} // SMT1CLK bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49C:1F'} // SMT1SIG bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '49D:1F'} // SMT1WIN bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '59E:BF'} // T0CON0 bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60C:01'} // CWG1CLKCON bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60D:0F'} // CWG1ISM bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60E:3F'} // CWG1DBR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '60F:3F'} // CWG1DBF bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '610:C7'} // CWG1CON0 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '611:2F'} // CWG1CON1 bits 7,6,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '612:FC'} // CWG1AS0 bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '613:7F'} // CWG1AS1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70C:31'} // PIR0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70D:C3'} // PIR1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70E:43'} // PIR2 bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '70F:F3'} // PIR3 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '710:0B'} // PIR4 bits 7,6,5,4,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '711:F1'} // PIR5 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '712:83'} // PIR6 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '713:21'} // PIR7 bits 7,6,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '714:C7'} // PIR8 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '716:31'} // PIE0 bits 7,6,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '717:C3'} // PIE1 bits 5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '718:43'} // PIE2 bits 7,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '719:F3'} // PIE3 bits 3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71A:0B'} // PIE4 bits 7,6,5,4,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71B:F1'} // PIE5 bits 3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71C:83'} // PIE6 bits 6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71D:21'} // PIE7 bits 7,6,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '71E:C7'} // PIE8 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '796:E5'} // PMD0 bits 4,3,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '797:17'} // PMD1 bits 7,6,5,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '798:E7'} // PMD2 bits 4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '799:0F'} // PMD3 bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79A:D1'} // PMD4 bits 5,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '79B:7E'} // PMD5 bits 7,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80C:3F'} // WDTCON0 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '80D:77'} // WDTCON1 bits 7,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '810:7F'} // WDTTMR bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '811:81'} // BORCON bits 6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '812:02'} // VREGCON bits 7,6,5,4,3,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '814:03'} // PCON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81D:3F'} // NVMDATH bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '81E:7F'} // NVMCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88C:F7'} // CPUDOZE bit 3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88D:7F'} // OSCCON1 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88E:7F'} // OSCCON2 bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '88F:D8'} // OSCCON3 bits 5,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '890:FD'} // OSCSTAT bit 1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '891:FC'} // OSCEN bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '892:3F'} // OSCTUNE bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '893:07'} // OSCFRQ bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '894:CA'} // ACTCON bits 5,4,2,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90E:BC'} // DAC1CON0 bits 6,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '90F:1F'} // DAC1CON1 bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '91F:B3'} // ZCDCON bits 6,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '98F:03'} // CMOUT bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '990:D3'} // CM1CON0 bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '991:03'} // CM1CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '992:07'} // CM1NSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '993:07'} // CM1PSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '994:D3'} // CM2CON0 bits 5,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '995:03'} // CM2CON1 bits 7,6,5,4,3,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '996:07'} // CM2NSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '997:07'} // CM2PSEL bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'A1F:DB'} // BAUD2CON bits 5,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C0C:BB'} // RTCCON bits 6,2 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C0E:FC'} // ALRMCON bits 1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C11:0F'} // MONTH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C12:07'} // WEEKDAY bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C13:3F'} // DAY bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C14:3F'} // HOURS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C15:7F'} // MINUTES bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C16:7F'} // SECONDS bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C17:1F'} // ALRMMTH bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C18:07'} // ALRMWD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C19:3F'} // ALRMDAY bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C1A:3F'} // ALRMHR bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C1B:7F'} // ALRMMIN bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C1C:7F'} // ALRMSEC bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C8D:0F'} // PORTH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C8E:DF'} // TRISG bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C8F:0F'} // TRISH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C90:DF'} // LATG bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS 'C91:0F'} // LATH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D0D:BF'} // LCDPS bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D14:C7'} // LCDVCON1 bits 5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D15:8F'} // LCDVCON2 bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D16:07'} // LCDREF bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D1D:3F'} // LCDDATA5 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D23:3F'} // LCDDATA11 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D29:3F'} // LCDDATA17 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D2F:3F'} // LCDDATA23 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D35:3F'} // LCDDATA29 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D3B:3F'} // LCDDATA35 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D41:3F'} // LCDDATA41 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1D47:3F'} // LCDDATA47 bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E0F:0F'} // CLCDATA bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E10:BF'} // CLC1CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E11:8F'} // CLC1POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1A:BF'} // CLC2CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E1B:8F'} // CLC2POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E24:BF'} // CLC3CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E25:8F'} // CLC3POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E2E:BF'} // CLC4CON bit 6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E2F:8F'} // CLC4POL bits 6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E38:3F'} // RF0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E39:3F'} // RF1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3A:3F'} // RF2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3B:3F'} // RF3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3C:3F'} // RF4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3D:3F'} // RF5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3E:3F'} // RF6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E3F:3F'} // RF7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E40:3F'} // RG0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E41:3F'} // RG1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E42:3F'} // RG2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E43:3F'} // RG3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E44:3F'} // RG4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E46:3F'} // RG6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E47:3F'} // RG7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E48:3F'} // RH0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E49:3F'} // RH1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E4A:3F'} // RH2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E4B:3F'} // RH3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E55:80'} // HIDRVF bits 6,5,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E5B:DF'} // ANSELG bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E5D:DF'} // ODCONG bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E5E:DF'} // SLRCONG bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E60:20'} // IOCGP bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E61:20'} // IOCGN bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E62:20'} // IOCGF bits 7,6,4,3,2,1,0 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E67:0F'} // WPUH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E68:0F'} // ODCONH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E69:0F'} // SLRCONH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E6A:0F'} // INLVLH bits 7,6,5,4 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E8F:01'} // PPSLOCK bits 7,6,5,4,3,2,1 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E90:3F'} // INTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E91:3F'} // T0CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E92:3F'} // T1CKIPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E93:3F'} // T1GPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E9C:3F'} // T2INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1E9D:3F'} // T4INPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EA1:3F'} // CCP1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EA2:3F'} // CCP2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EA9:3F'} // SMT1WINPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EAA:3F'} // SMT1SIGPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EB1:3F'} // CWG1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBB:3F'} // CLCIN0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBC:3F'} // CLCIN1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBD:3F'} // CLCIN2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EBE:3F'} // CLCIN3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC3:3F'} // ADACTPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC5:3F'} // SSP1CLKPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC6:3F'} // SSP1DATPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1EC7:3F'} // SSP1SSPPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECB:3F'} // RX1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECC:3F'} // TX1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECD:3F'} // RX2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1ECE:3F'} // TX2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F10:3F'} // RA0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F11:3F'} // RA1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F12:3F'} // RA2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F13:3F'} // RA3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F14:3F'} // RA4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F15:3F'} // RA5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F16:3F'} // RA6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F17:3F'} // RA7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F18:3F'} // RB0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F19:3F'} // RB1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1A:3F'} // RB2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1B:3F'} // RB3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1C:3F'} // RB4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1D:3F'} // RB5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1E:3F'} // RB6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F1F:3F'} // RB7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F20:3F'} // RC0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F21:3F'} // RC1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F22:3F'} // RC2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F23:3F'} // RC3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F24:3F'} // RC4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F25:3F'} // RC5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F26:3F'} // RC6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F27:3F'} // RC7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F28:3F'} // RD0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F29:3F'} // RD1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2A:3F'} // RD2PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2B:3F'} // RD3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2C:3F'} // RD4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2D:3F'} // RD5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2E:3F'} // RD6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F2F:3F'} // RD7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F30:3F'} // RE0PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F31:3F'} // RE1PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F33:3F'} // RE3PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F34:3F'} // RE4PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F35:3F'} // RE5PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F36:3F'} // RE6PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F37:3F'} // RE7PPS bits 7,6 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F38:DF'} // ANSELA bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F3A:DF'} // ODCONA bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F3B:DF'} // SLRCONA bit 5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F64:FB'} // ANSELE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F65:FB'} // WPUE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F66:FB'} // ODCONE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F67:FB'} // SLRCONE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F68:FB'} // INLVLE bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F69:FB'} // IOCEP bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F6A:FB'} // IOCEN bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1F6B:FB'} // IOCEF bit 2 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FE4:07'} // STATUS_SHAD bits 7,6,5,4,3 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FE6:1F'} // BSR_SHAD bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FE7:7F'} // PCLATH_SHAD bit 7 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FED:1F'} // STKPTR bits 7,6,5 un-implemented (read as 0)
  {$SET_UNIMP_BITS '1FEF:7F'} // TOSH bit 7 un-implemented (read as 0)


  // -- PIN mapping --

  // Pin  1 : VLCD2/ANE1/RE1
  // Pin  2 : VLCD1/ANE0/RE0
  // Pin  3 : SEG42/ANG0/RG0
  // Pin  4 : SEG43/ANG1/RG1
  // Pin  5 : SEG44/ANG2/RG2
  // Pin  6 : SEG45/ANG3/RG3
  // Pin  7 : VPP/ICDMCLR_n/MCLR_n/IOCG5/RG5
  // Pin  8 : SEG26/ANG4/RG4
  // Pin  9 : AVSS/VSS
  // Pin 10 : AVDD/VDD
  // Pin 11 : SEG25/ANF7/C1IN3-/C2IN3-/HIF7/RF7
  // Pin 12 : SEG24/ANF6/C1IN0+/RF6
  // Pin 13 : DAC1OUT1/SEG23/ANF5/C1IN1-/C2IN1-/RF5
  // Pin 14 : SEG22/ANF4/C2IN0+/RF4
  // Pin 15 : SEG21/ANF3/C1IN2-/C2IN2-/RF3
  // Pin 16 : SEG20/ANF1/C1OUT/RF2
  // Pin 17 : SEG19/ANF1/C2OUT/RF1
  // Pin 18 : SEG41/ANF0/C1IN0-/C2IN0-/RF0
  // Pin 19 : COM7/SEG15/ANG7/RG7
  // Pin 20 : COM6/ANG6/RG6
  // Pin 21 : VREF+/SEG35/ANA3/RA3
  // Pin 22 : SEG34/ANA2/C1IN1+/C2IN1+/RA2
  // Pin 23 : SEG18/ANA1/RA1
  // Pin 24 : SEG33/ANA0/C1IN4-/C2IN4-/RA0
  // Pin 25 : COM5/RH1
  // Pin 26 : COM4/RH0
  // Pin 27 : VBAT/RA5
  // Pin 28 : SEG14/ANA4/C1IN1+/C2IN1+/RA4
  // Pin 29 : SOSCI/SEG32/IOCC1/RC1
  // Pin 30 : SOSCO/SEG40/IOCC0/RC0
  // Pin 31 : SEG27/IOCC6/RC6
  // Pin 32 : SEG28/IOCC7/RC7
  // Pin 33 : SEG13/IOCC2/RC2
  // Pin 34 : SEG17/IOCC3/RC3
  // Pin 35 : SEG16/IOCC4/RC4
  // Pin 36 : SEG12/IOCC5/RC5
  // Pin 37 : ICSPDAT/ICDDAT/DAC1OUT2/SEG39/ANB7/IOCB7/RB7
  // Pin 38 : VDD
  // Pin 39 : CLKIN/SEG37/ANA7/RA7
  // Pin 40 : CLKOUT/SEG36/ANA6/RA6
  // Pin 41 : VSS
  // Pin 42 : ICSPCLK/ICDCLK/SEG38/ANB6/IOCB6/RB6
  // Pin 43 : SEG29/ANB5/IOCB5/RB5
  // Pin 44 : SEG11/ANB4/IOCB4/RB4
  // Pin 45 : SEG10/ANB3/IOCB3/RB3
  // Pin 46 : SEG9/ANB2/IOCB2/RB2
  // Pin 47 : SEG8/ANB1/IOCB1/RB1
  // Pin 48 : ZCD/SEG30/ANB0/IOCB0/RB0
  // Pin 49 : SEG7/AND7/RD7
  // Pin 50 : SEG6/AND6/RD6
  // Pin 51 : SEG5/AND5/RD5
  // Pin 52 : SEG4/AND4/RD4
  // Pin 53 : SEG3/AND3/RD3
  // Pin 54 : SEG2/AND2/RD2
  // Pin 55 : SEG1/AND1/RD1
  // Pin 56 : SEG47/CFLY2/RH3
  // Pin 57 : SEG46/CFLY1/RH2
  // Pin 58 : SEG0/AND0/RD0
  // Pin 59 : SEG31/ANE7/IOCE7/RE7
  // Pin 60 : COM3/ANE6/IOCE6/RE6
  // Pin 61 : COM2/ANE5/IOCE5/RE5
  // Pin 62 : COM1/ANE4/IOCE4/RE4
  // Pin 63 : COM0/ANE3/IOCE3/RE3
  // Pin 64 : VLCD3/RE2


  // -- RAM to PIN mapping --

  {$MAP_RAM_TO_PIN '00C:0-24,1-23,2-22,3-21,4-28,5-27,6-40,7-39'} // PORTA
  {$MAP_RAM_TO_PIN '00D:0-48,1-47,2-46,3-45,4-44,5-43,6-42,7-37'} // PORTB
  {$MAP_RAM_TO_PIN '00E:0-30,1-29,2-33,3-34,4-35,5-36,6-31,7-32'} // PORTC
  {$MAP_RAM_TO_PIN '00F:0-58,1-55,2-54,3-53,4-52,5-51,6-50,7-49'} // PORTD
  {$MAP_RAM_TO_PIN '010:1-2,2-1,3-63,4-62,5-61,6-60,7-59'} // PORTE
  {$MAP_RAM_TO_PIN '011:0-18,1-17,2-16,3-15,4-14,5-13,6-12,7-11'} // PORTF
  {$MAP_RAM_TO_PIN 'C8C:0-3,1-4,2-5,3-6,4-8,5-7,6-20,7-19'} // PORTG
  {$MAP_RAM_TO_PIN 'C8D:0-26,1-25,2-57,3-56'} // PORTH


  // -- Bits Configuration --

  // FEXTOSC : External Oscillator mode selection bits
  {$define _FEXTOSC_ECH      = $3FFF}  // EC above 8MHz; PFM set to high power
  {$define _FEXTOSC_ECM      = $3FFE}  // EC for 500kHz to 8MHz; PFM set to medium power
  {$define _FEXTOSC_ECL      = $3FFD}  // EC below 500kHz; PFM set to low power
  {$define _FEXTOSC_OFF      = $3FF8}  // Oscillator not enabled

  // RSTOSC : Power-up default value for COSC bits
  {$define _RSTOSC_EXT1X     = $3FFF}  // EXTOSC operating per FEXTOSC bits
  {$define _RSTOSC_HFINT1    = $3FEF}  // HFINTOSC (1MHz)
  {$define _RSTOSC_LFINT     = $3FDF}  // LFINTOSC
  {$define _RSTOSC_SOSC      = $3FCF}  // SOSC
  {$define _RSTOSC_Reserved  = $3FBF}  // Reserved
  {$define _RSTOSC_EXT4X     = $3FAF}  // EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
  {$define _RSTOSC_HFINTPLL  = $3F9F}  // HFINTOSC with 2x PLL, with OSCFRQ = 16 MHz and CDIV = 1:1 (FOSC = 32 MHz)
  {$define _RSTOSC_HFINT32   = $3F8F}  // HFINTOSC with OSCFRQ= 32 MHz and CDIV = 1:1

  // CLKOUTEN : Clock Out Enable bit
  {$define _CLKOUTEN_OFF     = $3FFF}  // CLKOUT function is disabled; i/o or oscillator function on OSC2
  {$define _CLKOUTEN_ON      = $3EFF}  // CLKOUT function is enabled; FOSC/4 clock appears at OSC2

  // VBATEN : VBAT Pin Enable bit
  {$define _VBATEN_ON        = $3DFF}  // VBAT functionality is enabled
  {$define _VBATEN_OFF       = $3FFF}  // VBAT functionality is disabled

  // LCDPEN : LCD Charge Pump Mode bit
  {$define _LCDPEN_OFF       = $3BFF}  // LCD Charge Pump is disabled.
  {$define _LCDPEN_ON        = $3FFF}  // LCD Charge Pump is enabled

  // CSWEN : Clock Switch Enable bit
  {$define _CSWEN_ON         = $3FFF}  // Writing to NOSC and NDIV is allowed
  {$define _CSWEN_OFF        = $37FF}  // The NOSC and NDIV bits cannot be changed by user software

  // FCMEN : Fail-Safe Clock Monitor Enable bit
  {$define _FCMEN_ON         = $3FFF}  // FSCM timer enabled
  {$define _FCMEN_OFF        = $1FFF}  // FSCM timer disabled

  // MCLRE : Master Clear Enable bit
  {$define _MCLRE_ON         = $3FFF}  // MCLR pin is Master Clear function
  {$define _MCLRE_OFF        = $3FFE}  // MCLR pin function is port defined function

  // PWRTE : Power-up Timer selection bits
  {$define _PWRTE_OFF        = $3FFF}  // PWRT disable
  {$define _PWRTE_PWRT_64    = $3FFD}  // PWRT set at 64 ms
  {$define _PWRTE_PWRT_16    = $3FFB}  // PWRT set at 16 ms
  {$define _PWRTE_PWRT_1     = $3FF9}  // PWRT set at 1 ms

  // LPBOREN : Low-Power BOR enable bit
  {$define _LPBOREN_OFF      = $3FFF}  // ULPBOR disabled
  {$define _LPBOREN_ON       = $3FDF}  // ULPBOR enabled

  // BOREN : Brown-out reset enable bits
  {$define _BOREN_ON         = $3FFF}  // Brown-out Reset Enabled, SBOREN bit is ignored
  {$define _BOREN_NSLEEP     = $3FBF}  // Brown-out Reset enabled while running, disabled in sleep; SBOREN is ignored
  {$define _BOREN_SBOREN     = $3F7F}  // Brown-out reset enabled according to SBOREN bit
  {$define _BOREN_OFF        = $3F3F}  // Brown-out reset disabled

  // BORV : Brown-out Reset Voltage Selection
  {$define _BORV_LO          = $3FFF}  // Brown-out Reset Voltage (VBOR) set to 1.9V on LF, and 2.45V on F Devices
  {$define _BORV_HI          = $3DFF}  // Brown-out Reset Voltage (VBOR) is set to 2.7V

  // ZCD : Zero-cross detect disable
  {$define _ZCD_OFF          = $3FFF}  // Zero-cross detect circuit is disabled at POR.
  {$define _ZCD_ON           = $3BFF}  // Zero-cross detect circuit is always enabled

  // PPS1WAY : Peripheral Pin Select one-way control
  {$define _PPS1WAY_ON       = $3FFF}  // The PPSLOCK bit can be cleared and set only once in software
  {$define _PPS1WAY_OFF      = $37FF}  // The PPSLOCK bit can be set and cleared repeatedly by software

  // STVREN : Stack Overflow/Underflow Reset Enable bit
  {$define _STVREN_ON        = $3FFF}  // Stack Overflow or Underflow will cause a reset
  {$define _STVREN_OFF       = $2FFF}  // Stack Overflow or Underflow will not cause a reset

  // WDTCPS : WDT Period Select bits
  {$define _WDTCPS_WDTCPS_0  = $3FE0}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_1  = $3FE1}  // Divider ratio 1:64
  {$define _WDTCPS_WDTCPS_2  = $3FE2}  // Divider ratio 1:128
  {$define _WDTCPS_WDTCPS_3  = $3FE3}  // Divider ratio 1:256
  {$define _WDTCPS_WDTCPS_4  = $3FE4}  // Divider ratio 1:512
  {$define _WDTCPS_WDTCPS_5  = $3FE5}  // Divider ratio 1:1024
  {$define _WDTCPS_WDTCPS_6  = $3FE6}  // Divider ratio 1:2048
  {$define _WDTCPS_WDTCPS_7  = $3FE7}  // Divider ratio 1:4096
  {$define _WDTCPS_WDTCPS_8  = $3FE8}  // Divider ratio 1:8192
  {$define _WDTCPS_WDTCPS_9  = $3FE9}  // Divider ratio 1:16384
  {$define _WDTCPS_WDTCPS_10 = $3FEA}  // Divider ratio 1:32768
  {$define _WDTCPS_WDTCPS_11 = $3FEB}  // Divider ratio 1:65536
  {$define _WDTCPS_WDTCPS_12 = $3FEC}  // Divider ratio 1:131072
  {$define _WDTCPS_WDTCPS_13 = $3FED}  // Divider ratio 1:262144
  {$define _WDTCPS_WDTCPS_14 = $3FEE}  // Divider ratio 1:524299
  {$define _WDTCPS_WDTCPS_15 = $3FEF}  // Divider ratio 1:1048576
  {$define _WDTCPS_WDTCPS_16 = $3FF0}  // Divider ratio 1:2097152
  {$define _WDTCPS_WDTCPS_17 = $3FF1}  // Divider ratio 1:4194304
  {$define _WDTCPS_WDTCPS_18 = $3FF2}  // Divider ratio 1:8388608
  {$define _WDTCPS_WDTCPS_19 = $3FF3}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_20 = $3FF4}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_21 = $3FF5}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_22 = $3FF6}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_23 = $3FF7}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_24 = $3FF8}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_25 = $3FF9}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_26 = $3FFA}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_27 = $3FFB}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_28 = $3FFC}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_29 = $3FFD}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_30 = $3FFE}  // Divider ratio 1:32
  {$define _WDTCPS_WDTCPS_31 = $3FFF}  // Divider ratio 1:65536; software control of WDTPS

  // WDTE : WDT operating mode
  {$define _WDTE_OFF         = $3F9F}  // WDT Disabled, SWDTEN is ignored
  {$define _WDTE_SWDTEN      = $3FBF}  // WDT enabled/disabled by SWDTEN bit in WDTCON0
  {$define _WDTE_NSLEEP      = $3FDF}  // WDT enabled while sleep=0, suspended when sleep=1; SWDTEN ignored
  {$define _WDTE_ON          = $3FFF}  // WDT enabled regardless of sleep; SWDTEN ignored

  // WDTCWS : WDT Window Select bits
  {$define _WDTCWS_WDTCWS_0  = $38FF}  // window delay = 87.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_1  = $39FF}  // window delay = 75 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_2  = $3AFF}  // window delay = 62.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_3  = $3BFF}  // window delay = 50 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_4  = $3CFF}  // window delay = 37.5 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_5  = $3DFF}  // window delay = 25 percent of time; no software control; keyed access required
  {$define _WDTCWS_WDTCWS_6  = $3EFF}  // window always open (100%); no software control; keyed access required
  {$define _WDTCWS_WDTCWS_7  = $3FFF}  // window always open (100%); software control; keyed access not required

  // WDTCCS : WDT input clock selector
  {$define _WDTCCS_LFINTOSC  = $07FF}  // WDT reference clock is the 31.0kHz LFINTOSC output
  {$define _WDTCCS_HFINTOSC  = $0FFF}  // WDT reference clock is the 31.25 kHz HFINTOSC
  {$define _WDTCCS_SOSC      = $17FF}  // WDT reference clock is the SOSC
  {$define _WDTCCS_SC        = $3FFF}  // Software Control

  // BBSIZE : Boot Block Size Selection bits
  {$define _BBSIZE_65536     = $3FF8}  // Boot Block Size (Words) 65536
  {$define _BBSIZE_32768     = $3FF9}  // Boot Block Size (Words) 32768
  {$define _BBSIZE_16384     = $3FFA}  // Boot Block Size (Words) 16384
  {$define _BBSIZE_8192      = $3FFB}  // Boot Block Size (Words) 8192
  {$define _BBSIZE_4096      = $3FFC}  // Boot Block Size (Words) 4069
  {$define _BBSIZE_2048      = $3FFD}  // Boot Block Size (Words) 2048
  {$define _BBSIZE_1024      = $3FFE}  // Boot Block Size (Words) 1024
  {$define _BBSIZE_512       = $3FFF}  // Boot Block Size (Words) 512

  // BBEN : Boot Block Enable bit
  {$define _BBEN_OFF         = $3FFF}  // Boot Block disabled
  {$define _BBEN_ON          = $3FF7}  // Boot Block enabled

  // SAFEN : SAF Enable bit
  {$define _SAFEN_OFF        = $3FFF}  // SAF disabled
  {$define _SAFEN_ON         = $3FEF}  // SAF enabled

  // WRTAPP : Application Block Write Protection bit
  {$define _WRTAPP_OFF       = $3FFF}  // Application Block NOT write-protected
  {$define _WRTAPP_ON        = $3F7F}  // Application Block write-protected

  // WRTB : Boot Block Write Protection bit
  {$define _WRTB_OFF         = $3FFF}  // Boot Block NOT write-protected
  {$define _WRTB_ON          = $3EFF}  // Boot Block write-protected

  // WRTC : Configuration Register Write Protection bit
  {$define _WRTC_OFF         = $3FFF}  // Configuration Words NOT write-protected
  {$define _WRTC_ON          = $3DFF}  // Configuration Word write-protected

  // WRTD : Data EEPROM Write Protection bit
  {$define _WRTD_OFF         = $3FFF}  // Data EEPROM NOT write-protected
  {$define _WRTD_ON          = $3BFF}  // Data EEPROM write-protected

  // WRTSAF : Storage Area Flash Write Protection bit
  {$define _WRTSAF_OFF       = $3FFF}  // SAF NOT write-protected
  {$define _WRTSAF_ON        = $37FF}  // SAF write-protected

  // LVP : Low Voltage Programming Enable bit
  {$define _LVP_ON           = $3FFF}  // Low Voltage programming enabled. MCLR/Vpp pin function is MCLR.
  {$define _LVP_OFF          = $1FFF}  // High Voltage on MCLR/Vpp must be used for programming

  // CP : UserNVM Program memory code protection bit
  {$define _CP_OFF           = $3FFF}  // UserNVM code protection disabled
  {$define _CP_ON            = $3FFE}  // UserNVM code protection enabled

implementation
end.
