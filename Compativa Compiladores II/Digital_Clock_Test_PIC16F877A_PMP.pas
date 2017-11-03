{
*  Código adaptado por Tito Hinostroza, para compilar con Pic Micro Pascal. 
*
*  (C) AguHDz 20-OCT-2017
*  Ultima Actualizacion: 01-NOV-2017
*
*  Compilador Pic Micro Pascal v.2.1.4 (http://www.pmpcomp.fr/)
*
*  Microcontrolador: PIC16F877A
*
*  RELOJ DIGITAL (TEST COMPARACION COMPILADORES)
*  =============================================
*  Este proyecto es una demostración del uso del compilador PicPas con
*  el microcontrolador PIC16F877A para hacer un reloj de tiempo real con
*  el integrado DS1307.
*
*  Se trata de un reloj totalmente funcional con tres botones de ajuste
*  SET, INC y DEC.
*
*  Ajuste:
*
*  1. Pulsar SET durante 1 segundo.
*  2. Aparece el cursor bajo los dígitos de año. Pulsar INC para
*     incremetar el año o DEC para decrementarlo. Cada pulsación
*     produce el avance o retroceso de una unidad del digito
*     editado. La pulsación larga permite un avance o retroceso
*     repetitivo haciendo más cómodo el ajuste.
*  3. Pulsar SET para pasar a la edición del mes y proceder del
*     mismo modo que en el ajuste del año pulsando INC o DEC.
*  4. Pulsar SET para ajustar del mismo modo el día del mes, hora,
*     y minutos. (los segundos siempre se inIcian a cero después
*     de cada ajuste)
*  5. Tras ajustar minutos y pulsar SET se pasa a la edición del
*     día de la semana (LUN...DOM). Proceder de igual manera
*     pulsando INC o DEC.
*  6. Pulsar SET para finalizar ajuste. El reloj comienza a funcionar
*     con la nueva hora y día.
*
*  NOTAS:
*  - Durante la edición, llegado al límete inferior o superior del
*    dígito editado se pasa automáticamente al valor inferior o
*    superior. Límites:
*        - Año: 00..99
*        - Mes: 01..12
*        - Día: 01..31 (28, 29 o 30 dependiendo del mes)
*        - Hora: 00..23
*        - Minuto: 00..59
*        - Día de Semana: LUN..DOM
*  - El límite superior del mes de febrero para años bisiestos
*    y los meses de 30 y 31 días los ajusta el programa de manera
*    automática. En caso de error en la edición, corrige el valor
*    de manera automática al límite superior válido para el mes.
*  - El integrado DS1307 es un reloj de tiempo real que funciona
*    de manera autónoma, y que sigue funcionando gracias a su batería
*    sin necesidad de suministro eléctrico exterior, por lo que no es
*    necesario ajustar el reloj cada vez que se desconecta la
*    alimentación. Gracias a su bajo consumo, con una batería
*    tipo botón estándar de 48mAh puede seguir funcionando durante
*    más de 10 años sin necesidad de suministro eléctrico exterior.
}

{$PROCESSOR PIC16F877A}
{$FREQUENCY 4Mhz}
program Dig_Clk;

const
// __I/O pin & estados_________________________________________________________
  HIGH_ST = 1;             // Estado digital alto (HIGH)
  LOW_ST  = 0;             // Estado digital bajo (LOW)

// __RTC DS1307________________________________________________________________
  DS1307_CONF  = $90;      // 1 Hz en salida SOUT del DS1307.

// __Pulsadores________________________________________________________________
  TIEMPO_ANTIREBOTE = 10;  // Milisegundos espera evitar rebote mecánico de pulsador.
  TIEMPO_REPETICION = 500; // Milisegundos pulsación continua que equivale a otra pulsación.

//__Menú de edición de fecha y hora____________________________________________
  SET_ANO        = 1;
  SET_MES        = 2;
  SET_DIA        = 3;
  SET_HORA       = 4;
  SET_MINUTO     = 5;
  SET_DIA_SEM    = 6;
  SALIR_SET_TIME = 7;

// __Display LCD_______________________________________________________________
// Valores de RS del LCD.
  LCD_CmdMode  = 0;        // Indicador envío de Comando (instrucción de configuración)
  LCD_CharMode = 1;        // Indicador envío de Dato (carácter)
// ---------------------------------------------------------------------------
// COMANDOS PARA DISPLAY LCD COMPATIBLE CON ESTANDAR HITACHI HD44780
// ---------------------------------------------------------------------------
  LCD_CMD_CLEAR_DISPLAY              = $01;
  LCD_CMD_DISP_CURS_HOME    = $02;
  LCD_CMD_CHAR_ENTRY_MODE       = $04;
      LCD_CMD_INCREMENT              = $02;
      LCD_CMD_DECREMENT              = $00;
      LCD_CMD_DISP_SHIFT_ON       = $01;
      LCD_CMD_DISP_SHIFT_OFF      = $00;
  LCD_CMD_DISP_ON_OFF_CUR  = $08;
      LCD_CMD_DISPLAY_ON             = $04;
      LCD_CMD_DISPLAY_OFF            = $00;
      LCD_CMD_CURSOR_UND_ON    = $02;
      LCD_CMD_CURSOR_UND_OFF   = $00;
      LCD_CMD_CURSOR_BLINK_ON        = $01;
      LCD_CMD_CURS_BLINK_OFF       = $00;
  LCD_CMD_DISP_CURS_SHIFT   = $10;
      LCD_CMD_DISPLAY_SHIFT          = $08;
      LCD_CMD_CURSOR_MOVE            = $00;
      LCD_CMD_RIGHT                  = $04;
      LCD_CMD_LEFT                   = $00;
  LCD_CMD_FUNCTION_SET               = $20;
      LCD_CMD_8BIT_INTERFACE         = $10;
      LCD_CMD_4BIT_INTERFACE         = $00;
      LCD_CMD_2LINES                 = $08;
      LCD_CMD_1LINE                  = $00;
      LCD_CMD_F_FONT_5_10            = $02;
      LCD_CMD_F_FONT_5_8             = $00;
  LCD_CMD_SET_DISPLAY_ADD        = $80;
      LCD_CMD_ROW_0                  = $00;
      LCD_CMD_ROW_1                  = $40;
      LCD_CMD_ROW_2                  = $14;
      LCD_CMD_ROW_3                  = $54;
  LCD_CMD_SET_CGRAM_ADD          = $40;

  LCD_CURSOR_HOME            = LCD_CMD_DISP_CURS_HOME;
  LCD_CLEAR                  = LCD_CMD_CLEAR_DISPLAY;
  LCD_CURSOR_UNDELINE        = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_ON + LCD_CMD_CURSOR_UND_ON;
  LCD_CURSOR_BLINK           = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_ON + LCD_CMD_CURSOR_BLINK_ON;
  LCD_CURSOR_UND_BLINK = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_ON + LCD_CMD_CURSOR_UND_ON + LCD_CMD_CURSOR_BLINK_ON;
  LCD_CURSOR_OFF             = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_ON + LCD_CMD_CURSOR_UND_OFF + LCD_CMD_CURS_BLINK_OFF;
  LCD_ON                     = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_ON;
  LCD_OFF                    = LCD_CMD_DISP_ON_OFF_CUR + LCD_CMD_DISPLAY_OFF;
  LCD_DISPLAY_SHIFT_RIGHT    = LCD_CMD_DISP_CURS_SHIFT + LCD_CMD_DISPLAY_SHIFT + LCD_CMD_RIGHT;
  LCD_DISPLAY_SHIFT_LEFT     = LCD_CMD_DISP_CURS_SHIFT + LCD_CMD_DISPLAY_SHIFT + LCD_CMD_LEFT;
  LCD_DISPLAY_CUR_RIGHT   = LCD_CMD_DISP_CURS_SHIFT + LCD_CMD_CURSOR_MOVE + LCD_CMD_RIGHT;
  LCD_DISPLAY_CURSOR_LEFT    = LCD_CMD_DISP_CURS_SHIFT + LCD_CMD_CURSOR_MOVE + LCD_CMD_LEFT;
// ---------------------------------------------------------------------------


var
// __Comunicación I2C__________________________________________________________
  SDA        : boolean absolute PORTB.0;   // Pin SDA del bus I2C
  SCL        : boolean absolute PORTB.1;   // Pin SCL del bus I2C

// __Display LCD_______________________________________________________________
  LCD_DATA_4 : boolean absolute PORTB.4;   // Pines de datos
  LCD_DATA_5 : boolean absolute PORTB.5;
  LCD_DATA_6 : boolean absolute PORTB.6;
  LCD_DATA_7 : boolean absolute PORTB.7;
  LCD_RS     : boolean absolute PORTB.2;   // Pin RS
  LCD_EN     : boolean absolute PORTB.3;   // Pin Enable

// __Pulsadores________________________________________________________________
  P_INC      : boolean absolute PORTA.0;   // Pulsador INC
  P_DEC      : boolean absolute PORTA.1;   // Pulsador DEC
  P_SET      : boolean absolute PORTA.2;   // Pulsador SET

// __RTC DS1307________________________________________________________________
  DS1307_DiaSemana : byte;      // Día de la semana (formato numérico 1..7)
  DS1307_Dia       : byte;      // Día del mes.
  DS1307_Mes       : byte;      // Mes del año.
  DS1307_Ano       : byte;      // Año (solo dos dígitos)
  DS1307_Hora      : byte;      // Hora del día.
  DS1307_Minuto    : byte;      // Minuto de la hora.
  DS1307_Segundo   : byte;      // Segundo del minuto.
  SOUT             : boolean absolute PORTA.3;         // Pin que lee la salida SOUT

// __ Menú edición de fecha y hora_____________________________________________
  editMenuState    : byte;      // Posición o estado dentro del menú de edición.

// CONFIGURATION WORD PIC16F87XA
// PIC16F873A
// PIC16F874A
// PIC16F876A
// PIC16F877A
// =======================================
// CP : FLASH Program Memory Code Protection bit.
{$DEFINE _CP_ON         =    $1FFF}      // All program memory code-protected
{$DEFINE _CP_ALL        =    $1FFF}      // All program memory code-protected
{$DEFINE _CP_OFF        =    $3FFF}      // Code protection off
// DEBUG : In-Circuit Debugger Mode bit
// RB6-RB7 are dedicaded to the debugger.
{$DEFINE _DEBUG_ON      =    $37FF}      // In-Circuit Debugger enabled, RB6 and RB7 are dedicated to the debugger
{$DEFINE _DEBUG_OFF     =    $3FFF}      // In-Circuit Debugger disabled, RB6 and RB7 are general purpose I/O pins
// WRT1:WRT0 : Flash Program Memory Write Enable bits.
{$DEFINE _WRT_HALF      =    $39FF}      // 0000h to 0FFFh write-protected; 1000h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_1FOURTH   =    $3BFF}      // 0000h to 07FFh write-protected; 0800h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_256       =    $3DFF}      // 0000h to 00FFh write-protected; 0100h to 1FFFh may be written to by EECON control
{$DEFINE _WRT_OFF       =    $3FFF}      // Write protection off; all program memory may be written to by EECON control
// CPD : Data EEPROM Memory Code Protection bit.
{$DEFINE _CPD_ON        =    $3EFF}      // Data EEPROM code-protected
{$DEFINE _CPD_OFF       =    $3FFF}      // Data EEPROM code protection off
// LVP : Low-Voltage (Single-Supply) In-Circuit Serial Programming Enable bit.
{$DEFINE _LVP_OFF       =    $3F7F}      // RB3 is digital I/O, HV on MCLR must be used for programming
{$DEFINE _LVP_ON        =    $3FFF}      // RB3/PGM pin has PGM function; low-voltage programming enabled
// BOREN : Brown-out Reset Enable bit.
// Enable BOREN automatically enable PWRTEN, regardless of the
// value of bit PWRTEN. Ensure the PWRTEN is enable any time
// BOREN is enable.
{$DEFINE _BOREN_OFF     =    $3FBF}      // BOR disabled
{$DEFINE _BOREN_ON      =    $3FFF}      // BOR enabled
// /PWRTEN : Power-up Timer Enable bit.
{$DEFINE _PWRTEN_ON     =    $3FF7}      // PWRT enabled
{$DEFINE _PWRTEN_OFF    =    $3FFF}      // PWRT disabled
// WDTEN : Watchdog Timer Eneble bit.
{$DEFINE _WDT_OFF       =    $3FFB}      // WDT disabled
{$DEFINE _WDT_ON        =    $3FFF}      // WDT enabled
// FOSC1:FOSC2 : Oscilator Seleccion bits.
{$DEFINE _LP_OSC        =    $3FFC}      // LP oscillator
{$DEFINE _XT_OSC        =    $3FFD}      // XT oscillator
{$DEFINE _HS_OSC        =    $3FFE}      // HS oscillator
{$DEFINE _RC_OSC        =    $3FFF}      // RC oscillator
// =======================================
// The erased (unprogrammed) value of the configuration word is 3FFFFh.
// Configuration Word Address : 2007h.
//{$CONFIG _CP_OFF, _DEBUG_OFF, _WRT_OFF, _CPD_OFF, _LVP_ON, _BOREN_ON, _PWRTEN_ON, _WDT_OFF, _HS_OSC }

//************************************************************************************************//
//********************************** F U N C I O N E S *******************************************//
//************************************************************************************************//

//****************************************************************************
//  Funciones de manejo de display LCD 16x4
//*****************************************************************************
procedure LCD_send4Bits(date : byte);
begin
    LCD_DATA_4 := date.4;
    LCD_DATA_5 := date.5;
    LCD_DATA_6 := date.6;
    LCD_DATA_7 := date.7;
    LCD_EN     := HIGH_ST;
    delay_ms(2);
    LCD_EN     := LOW_ST;
    delay_ms(2);
end;

procedure LCD_send(date : byte);
begin
    LCD_send4Bits(date);
    LCD_send4Bits(date SHL 4);
end;

procedure LCD_command(comm : byte);
begin
    LCD_RS := LCD_CmdMode;
    LCD_send(comm);
end;

procedure LCD_putChar(date : byte);
begin
    LCD_RS := LCD_CharMode;
    LCD_send(date);
end;

procedure LCD_gotoXY(columna, fila : byte);
begin
  if(fila = 0) then
    LCD_command(LCD_CMD_SET_DISPLAY_ADD + columna + LCD_CMD_ROW_0)
  else
    LCD_command(LCD_CMD_SET_DISPLAY_ADD + columna + LCD_CMD_ROW_1);
end;

procedure LCD_init;
begin
    // Pines iniciados como Salidas.
    TRISB.RB4 := 0;  //LCD_DATA_4
    TRISB.RB5 := 0;  //LCD_DATA_5
    TRISB.RB6 := 0;  //LCD_DATA_6
    TRISB.RB7 := 0;  //LCD_DATA_7
    TRISB.RB2 := 0;  //LCD_RS
    TRISB.RB3 := 0;  //LCD_EN

    delay_ms(200);   // Espera para asegurar tensión estable tras inicio.
    LCD_RS := LOW_ST;
    LCD_EN := LOW_ST;

    // INICIALIZACION POR SOFTWARE DE DISPLAY LCD.
    // Los tiempos de espera y secuencia de datos son los indicados en todos los
    // datasheets de los displays compatibles con el estándar Hitachi HD44780.
    delay_ms(30);    // Espera >= 15 ms
    // INICIACION DE DISPLAY MODO 4 BITS DE DATOS.
    LCD_send4Bits(%00110000);
    delay_ms(5);    // Espera >= 4.1 ms
    LCD_send4Bits(%00110000);
    delay_ms(1);    // Espera >= 100 us
    LCD_send4Bits(%00110000);
    LCD_send4Bits(%00100000);
    LCD_command(LCD_CMD_FUNCTION_SET + LCD_CMD_4BIT_INTERFACE + LCD_CMD_2LINES + LCD_CMD_F_FONT_5_8);
    // FIN DE INICIALIZACION POR SOFTWARE DEL DISPLAY LCD.

    // CONFIGURACION DE DISPLAY LCD.
    LCD_command(LCD_OFF);
    LCD_command(LCD_CMD_CHAR_ENTRY_MODE + LCD_CMD_INCREMENT + LCD_CMD_DISP_SHIFT_OFF);
    LCD_command(LCD_ON);
    LCD_command(LCD_CLEAR);
end;

//****************************************************************************
//  Funciones de comunicación I2C mediante software.
//*****************************************************************************
procedure I2C_start;      // START function for communicate I2C
begin
    SDA := HIGH_ST;
    SCL := HIGH_ST;
   // Configura pines I2C como Salidas.
    TRISB.RB1 := 0;  //SCL
    TRISB.RB0 := 0;  //SDA
    SDA := LOW_ST;
    SCL := LOW_ST;
end;

procedure I2C_stop;       // STOP function for communicate I2C
begin
    SDA := LOW_ST;
    SCL := HIGH_ST;
    SDA := HIGH_ST;
end;

FUNCTION I2C_writeByte(dato : byte) : bOOLEAN;     // Send data to I2C
var
    LoopCounter : byte;
    ACKbit      : BOOLEAN;
begin
    LoopCounter := 0;
    while(LoopCounter<8) do BEGIN
        SDA  := dato.7;    // SDA = bit de más peso del valor dato.
        SCL  := HIGH_ST;
        dato := dato shL 1;
        SCL  := LOW_ST;
        inc(LoopCounter);
    end;

    TRISB0 := 1;  //SetAsInput SDA
    SCL      := HIGH_ST;
    ACKbit   := SDA;
    SCL      := LOW_ST;
    TRISB0 := 0;  //SetAsOutput SDA;
    exit(ACKbit);
end;

FUNCTION I2C_readByte(ACKBit : boolean) : byte;   // Receive data from I2C
var
    LoopCounter : byte;
    dato        : byte;
begin
    dato := 0;
    TRISB0 := 1;  //SetAsInput SDA

    LoopCounter := 0;
    while(LoopCounter<8) do begin
        SCL    := HIGH_ST;
        dato   := dato SHL 1;
        dato.0 := SDA;
        SCL    := LOW_ST;
        inc(LoopCounter);
    end;

    TRISB0 := 0;  //SetAsOutput SDA;
    SDA := not ACKBit;
    SCL := HIGH_ST;
    SCL := LOW_ST;
    exit(dato);
end;

//****************************************************************************
//  - Función: BCDToDecimal
//  - Descripción: Transforma un número en formato BCD a Decimal.
//  - Entrada:
//      > bcdByte: Número en formato BCD
//  - Salida: Número en formato Decimal.
//*****************************************************************************
FUNCTION BCDToDecimal(bcdByte : byte) : byte;
var
  decimal : byte;
begin
  decimal := 0;
  while(bcdByte>$09) do BEGIN
    bcdByte := bcdByte - $10;
    decimal := decimal + 10;
  end;
  decimal := decimal + bcdByte;    // Suma el resto <= $09.
  exit(decimal);                   // Devuelve valor en formato decimal.
end;

//****************************************************************************
//  - Función: decimalToBCD
//  - Descripción: Transforma un número en formato Decimal a BCD.
//  - Entrada:
//      > decimalByte: Número en formato Decimal
//  - Salida: Número en formato BCD.
//*****************************************************************************
FUNCTION decimalToBCD (decimalByte : byte) : byte;
var
  BCDByte : byte;
begin
  bcdByte := 0;
  while(decimalByte>9) do BEGIN
    BCDByte     := bcdByte     + $10;
    decimalByte := decimalByte - 10;
  end;
  bcdByte := bcdByte + decimalByte;    // Suma el resto <= 9.
  exit(bcdByte);                       // Devuelve valor en formato BCD.
end;

//****************************************************************************
//  - Función: DS1307_timeRead
//  - Descripción: Lee fecha y hora del DS1307 a través del bus I2C.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************
procedure DS1307_timeRead;
begin
  I2C_start;           // Inicia comunicación I2C.
  I2C_writeByte($D0);  // Dirección I2C del DS1307.
  I2C_writeByte($00);  // Primera dirección a leer/escribir.
  I2C_start;           // Reinicia comunicación I2C.
  I2C_writeByte($D1);  // DS1307 en Modo Escritura.
  DS1307_Segundo   := BCDToDecimal(I2C_readByte(true));  // ASK = 1
  DS1307_Minuto    := BCDToDecimal(I2C_readByte(true));
  DS1307_Hora      := BCDToDecimal(I2C_readByte(true));
  //DS1307_DiaSemana := BCDToDecimal(DS1307_DiaSemana);  // Valor 1...7 (igual en decimal que en BCD)
  DS1307_Dia       := BCDToDecimal(I2C_readByte(true));
  DS1307_Mes       := BCDToDecimal(I2C_readByte(true));
  DS1307_Ano       := BCDToDecimal(I2C_readByte(false)); // ASK = 0
          // El último ASK antes del Stop debe ser sea cero (/ASK).
  I2C_stop;
end;

//****************************************************************************
//  - Función: DS1307_timeWrite
//  - Descripción: Escribe fecha y hora en el DS1307 a través del bus I2C.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************
procedure DS1307_timeWrite;
begin
  I2C_start;           // Inicia comunicación I2C
  I2C_writeByte($D0);  // Dirección I2C del DS1307.
  I2C_writeByte($00);  // Primera dirección a leer/escribir.
  I2C_writeByte(0);    // Siempre que se ajusta la fecha y hora los Segundos=0
  I2C_writeByte(decimalToBCD(DS1307_Minuto));
  I2C_writeByte(decimalToBCD(DS1307_Hora));
  I2C_writeByte(DS1307_DiaSemana);  // Valor 1...7 (igual en decimal que en BCD)
  I2C_writeByte(decimalToBCD(DS1307_Dia));
  I2C_writeByte(decimalToBCD(DS1307_Mes));
  I2C_writeByte(decimalToBCD(DS1307_Ano));
  I2C_stop;
end;

//****************************************************************************
//  - Función: LCDPrintDiaSemana
//  - Descripción: Muesta en display LCD el día de la semana actual en
//    formato texto.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************
procedure LCDPrintDiaSemana;
begin
    if(DS1307_DiaSemana = 1) then BEGIN
      //LCD_print("DOM");
      LCD_putChar(byte('D'));
      LCD_putChar(byte('O'));
      LCD_putChar(byte('M'));
    END elsE if(DS1307_DiaSemana = 2) then BEGIN
      //LCD_print("LUN");
      LCD_putChar(byte('L'));
      LCD_putChar(byte('U'));
      LCD_putChar(byte('N'));
    END elsE if(DS1307_DiaSemana = 3) then BEGIN
      //LCD_print("MAR");
      LCD_putChar(byte('M'));
      LCD_putChar(byte('A'));
      LCD_putChar(byte('R'));
    END elsE if(DS1307_DiaSemana = 4) then BEGIN
      //LCD_print("MIE");
      LCD_putChar(byte('M'));
      LCD_putChar(byte('I'));
      LCD_putChar(byte('E'));
    END elsE if(DS1307_DiaSemana = 5) then BEGIN
      //LCD_print("JUE");
      LCD_putChar(byte('J'));
      LCD_putChar(byte('U'));
      LCD_putChar(byte('E'));
    END elsE if(DS1307_DiaSemana = 6) then BEGIN
      //LCD_print("VIE");
      LCD_putChar(byte('V'));
      LCD_putChar(byte('I'));
      LCD_putChar(byte('E'));
    END elsE if(DS1307_DiaSemana = 7) then BEGIN
      //LCD_print("SAB");
      LCD_putChar(byte('S'));
      LCD_putChar(byte('A'));
      LCD_putChar(byte('B'));
    end;
end;

//****************************************************************************
//  - Función: bisiesto
//  - Descripción: Comprueba si el año actual es bisiesto [margen de 2000 a 2099].
//      Para otros márgenes de años, habría que aplicar el algoritmo genérico
//      teniendo en cuenta los años múltiplos de 100 o 400.
//  - NOTAS: Detalle curioso. Para siglos anteriores al XX, habría que tener en
//      cuenta que en España y otros países catolicos el mes de octubre de 1582
//      sólo tuvo 20 días. Ese mes, el día siguiente al jueves 4 fue viernes 15.
//      En el resto del mundo, el cambio fue produciendose en los siguientes
//      siglos (hasta el XX). Por ejemplo, en Inglaterra y colonias fue en 1752
//      (el día siguiente al 03/09/1752 fue 14/091782). Este cambio introdujo
//      las reglas actuales para los años multiplos de 100 y 400.
//  - Entrada: Ninguna.
//  - Salida:
//      > Devuelve 1 si el año es bisiesto, y 0 si no lo es.
//*****************************************************************************
FUNCTION bisiesto : boolean;
var
  dato : byte;
begin
  // Devuelve 0 si (DS1307_timeAno%4)!=0, y 1 si (DS1307_timeAno%4)==0
  dato := DS1307_Ano;
  while(dato >= 4) do BEGIN
    dato := dato - 4;
  end;
  if(dato = 0) then
    exit(true)
  else
    exit(false);
  //return !(DS1307_Ano%4);
end;

//****************************************************************************
//  - Función: diasDelMes
//  - Descripción: Devuelve el número de días de cualquier mes del año actual.
//  - Entrada: Ninguna.
//  - Salida:
//      > Número en días del mes.
//*****************************************************************************
FUNCTION diasDelMes : byte;
begin
    if(DS1307_Mes = 2) then  BEGIN      // Mes = febrero
        if(bisiesto) then
          exit(29)
        else
          exit(28);
         // Bisiesto: 29 días / No bisiesto: 28 días.
    END else BEGIN
        if((DS1307_Mes = 4) OR (DS1307_Mes = 6) OR (DS1307_Mes = 9) OR (DS1307_Mes = 11)) then
          exit(30)                // Meses de 30 días.
        else
          exit(31);                // Meses de 31 días.
    end;
end;

//****************************************************************************
//  - Función: LCDPrintNumero
//  - Descripción: Imprime en la pantalla LCD un número de 2 dígitos.
//  - Entrada:
//      > numero: Número entre 0 y 99 a imprimir.
//  - Salida: Ninguna.
//*****************************************************************************
procedure LCDPrintNumero(numero : byte);
begin
    numero := decimalToBCD(numero);
    LCD_putChar((numero SHR 4)+48);      // Imprime dígito decena.
    LCD_putChar((numero AND $0F)+48);   // Imprime dígito unidad.
end;

//****************************************************************************
//  - Función: timeShow
//  - Descripción: Muestra en el display LCD la fecha y hora.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************/
procedure timeShow;
begin
    LCD_gotoXY(1,0);
    LCDPrintNumero(DS1307_Dia);
    LCD_putChar(byte('/'));
    LCDPrintNumero(DS1307_Mes);
    LCD_putChar(byte('/'));
    LCDPrintNumero(DS1307_Ano);
    LCD_putChar(byte(' '));
    LCD_putChar(byte(' '));
    LCD_putChar(byte(' '));
    LCDPrintDiaSemana;
    LCD_gotoXY(1,1);
    LCDPrintNumero(DS1307_Hora);
    LCD_putChar(byte(':'));
    LCDPrintNumero(DS1307_Minuto);
    LCD_putChar(byte(':'));
    LCDPrintNumero(DS1307_Segundo);
end;

//****************************************************************************
//  - Función: cicloTimeSet
//  - Descripción: Subfunción de la función timeRead() que edita las variables
//    del día y hora del reloj.
//  - Entrada:
//      > limInf : Límite Inferior de la variable editada.
//      > limSup : Límite Superior de la variable editada.
//      > lcdX   : Posición X del display en la que se muestra la variable.
//      > lcdY   : Posición Y (fila) del display en la que se muestra la variable.
//      > dato   : Dato editado.
//  - Salida: El valor editado.
//*****************************************************************************/
FUNCTION cicloTimeSet(limInf, limSup, lcdX, lcdY, editDato : byte) : byte;
begin
    while((P_INC AND P_DEC) = LOW_ST) do BEGIN  // Si se pulsa INC o DEC.
        LCD_Command(LCD_CURSOR_OFF);      // Cursor Off
        if(P_INC=LOW_ST) then BEGIN            // Se ha pulsado INC.
            inc(editDato);
            if(editDato>limSup) then editDato:=limInf;  // Controla que no se supere el límite superior.
        END else BEGIN                             // Se ha pulsado DEC.
            dec(editDato);
            if((editDato<limInf) OR (editDato=$FF)) then editDato:=limSup;  // Si limInf=0 dec(dato) puede ser 0xFF.
        end;

        LCD_gotoXY(lcdX, lcdY);           // Coloca el cursor en la posición de inicio de impresión del dato editado.

        if(editMenuState=SET_DIA_SEM) then  BEGIN // Si se está editando del día de la semana, se imprime el texto.
           DS1307_DiaSemana := editDato;
           LCDPrintDiaSemana;
        END else BEGIN                           // El resto son variables numéricas de 2 dígitos.
           LCDPrintNumero(editDato);
        end;
        delay_ms(TIEMPO_REPETICION);      // Espera el tiempo de autorepetición de la tecla pulsada.
    end;

    if(P_SET=LOW_ST) then BEGIN                // Si se pulsa SET.
        inc(editMenuState);
        while(P_SET=LOW_ST) do BEGIN           // Espera antirebote mecánico del pulsador.
          delay_ms(TIEMPO_ANTIREBOTE);
        end;
        if(editDato>limSup) then editDato:=limSup;  // Evita posible bug al modificar el año o el mes, si
                                                        // no se modifica el día y en ese año o mes ya no es válido.
    end;

    if(editMenuState=SET_DIA_SEM) then inc(lcdX);   // Si se está editando el día de la semana, se desplaza el cursor
                                                        // una posición más, ya que el texto ocupa 3 posiciones, en lugar
                                                        // de dos como el resto de variables.
    inc(lcdX);
    LCD_gotoXY(lcdX, lcdY);           // Coloca el cursor en la parte izquierda de la variable editada.
    LCD_Command(LCD_CURSOR_UNDELINE); // Cursor On
    exit(editDato);
end;

//****************************************************************************
//  - Función: timeRead
//  - Descripción: Set fecha y hora mediante pulsadores y cursor en display LCD.
//    Programado según la lógica de una "máquina de estado". La variable global
//    editMenuState indica la posición del cursor dentro del bucle de fijación de fecha y
//    hora.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************
procedure timeSet;
var
  aux : byte;
begin
    LCD_gotoXY(7,1);           // Goto posición de Segundos en display.
    LCDPrintNumero(0);         // 00 en posición de Segundos del display.
    LCD_Command(LCD_CURSOR_UNDELINE);       // Cursor On
    while(editMenuState<SALIR_SET_TIME) do BEGIN
        while(editMenuState=SET_ANO) do BEGIN
          DS1307_Ano := cicloTimeSet(0,99,7,0,DS1307_Ano);              // Set año.
        end;
        while(editMenuState=SET_MES) do BEGIN
          DS1307_Mes := cicloTimeSet(1,12,4,0,DS1307_Mes);              // Set mes.
        end;
        while(editMenuState=SET_DIA) do BEGIN
          aux := diasDelMes;
          DS1307_Dia := cicloTimeSet(1,aux,1,0,DS1307_Dia);             // Set día.
        end;
        while(editMenuState=SET_HORA) do BEGIN
          DS1307_Hora := cicloTimeSet(0,23,1,1,DS1307_Hora);            // Set hora.
        end;
        while(editMenuState=SET_MINUTO) do BEGIN
          DS1307_Minuto := cicloTimeSet(0,59,4,1,DS1307_Minuto);        // Set minutos.
        end;
        while(editMenuState=SET_DIA_SEM) do BEGIN
          DS1307_DiaSemana := cicloTimeSet(1,7,12,0,DS1307_DiaSemana);  // Set día de la semana.
        end;
    end;
    LCD_Command(LCD_CURSOR_OFF);       // Cursor Off
end;

//****************************************************************************
//  - Función: Setup
//  - Descripción: Inicializa Microcontrolador y Hardware externo conectado.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//*****************************************************************************
procedure setup;
begin
    CMCON  := $07;         // Deshabilita comparadores.
    ADCON1 := $06;         // Todos los pines configurados como digitales.
    ADCON0 := $00;         // Desactiva conversor A/D.
    INTCON.GIE := 0;       // Todas las interrupciones desactivadas.

    TRISA.0 := 1; // SetAsInput P_INC;     // Configura Pulsadores como Entradas.
    TRISA.1 := 1; // SetAsInput P_DEC
    TRISA.2 := 1; // SetAsInput P_SET
    TRISA.3 := 0;  //SetAsInput SOUT;

    I2C_start;                  // Inicia comunicación I2C
    I2C_writeByte($D0);         // Dirección I2C del DS1307.
    I2C_writeByte($07);         // Escribe en la dirección 07h.
    I2C_writeByte(DS1307_CONF); // Configura 1 Hz en salida SOUT del DS1307.
    I2C_stop;

    LCD_init;                   // Inicializa display LCD.
end;

//****************************************************************************
//  - Descripción: Programa Principal.
//  - Entrada: Ninguna.
//  - Salida: Ninguna.
//****************************************************************************
begin
    setup;

    while(true) do BEGIN
        if(P_SET=LOW_ST) then BEGIN  // Comprueba si se ha pulsado SET
            editMenuState := SET_ANO;
            // Espera fin pulsación y antirebote mecánico.
            while(P_SET=LOW_ST) do delay_ms(TIEMPO_ANTIREBOTE);
            timeSet;            // Ajuste de reloj.
            DS1307_timeWrite;   // Envía datos editados.
        end;

        DS1307_timeRead;  // Lee la fecha y hora en el DS1307.
        timeShow;         // Actualiza display LCD con fecha y hora.

        // Espera 1 segundo usando salida SOUT del DS1307 (1 Hz)
        repeat until(SOUT=LOW_ST);        // Espera durante pulso alto.
        repeat until(SOUT=HIGH_ST);       // Espera durante pulso bajo.
    end;
end.
