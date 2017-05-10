// (C) AguHDz 06-05-2017
// Ultima Actualizacion: 07-05-2017
// Prueba para compilador PicPas v.0.5.7
// Manejo de display LCD 16x2.
// Imprime de manera repatitiva mensaje en cualquier LCD 16x2 acompatible con
// el estandar HITACHI HD44780.
 
   {
   Puertos de PIC 16F84 usados:
       - RB0..RB7 a LCD Data (8 PIC pins)
       - RA0..RA1 a LCD Control (2 PIC pins)
   }
 
   { -----  Conexiones entre LCD y PIC ----
 
    ---- Alimentacion y Contraste ----
    Vss  (LCD pin 1) a negativo alimentacion
    Vdd  (LCD pin 2) a + 5V, (si no queremos controlar el apagado el LCD)
    Contraste (LCD pin 3) a tierra a traves de una resistencia fija (p.e. 2K2,
    menos valor = mayor contraste) o de un potenciometro ajustable de 5K.
 
    ---- Control ----
    RS (LCD pin 4) a PIC RA0
    RW (LCD pin 5) a tierra (= LCD pin 1)
    E  (LCD pin 6) a PIC RA.1
 
    ---- Data ----
    D0 (LCD pin  7) a PIC RB.0
    D1 (LCD pin  8) a PIC RB.1
    D2 (LCD pin  9) a PIC RB.2
    D3 (LCD pin 10) a PIC RB.3
    D4 (LCD pin 11) a PIC RB.4
    D5 (LCD pin 12) a PIC RB.5
    D6 (LCD pin 13) a PIC RB.6
    D7 (LCD pin 14) a PIC RB.7

10/05/2017: Adaptado por Tito Hinostroza, para usar las nuevas 
funciones de PicPas 0.6.0
    }
 
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program LCD8bit;
uses PIC16F84A; 
const
  // LCD commands
  // Indentation means: values have to be summed up with the non indented value.
  //   e.g. use "LCDCommand(LCD_DISPLAY_ON_OFF_AND_CURSOR + LCD_DISPLAY_ON)" to switch the
  //   display on, with no underline cursor and cursor not blinking.
 
      LCD_CLEAR_DISPLAY                = $01;
 
      LCD_DISPLAY_AND_CURSOR_HOME      = $02;
 
      LCD_CHARACTER_ENTRY_MODE         = $04;
             LCD_INCREMENT             = $02;
             LCD_DECREMENT             = $00;
             LCD_DISPLAY_SHIFT_ON      = $01;
             LCD_DISPLAY_SHIFT_OFF     = $00;
 
      LCD_DISPLAY_ON_OFF_AND_CURSOR    = $08;
             LCD_DISPLAY_ON            = $04;
             LCD_DISPLAY_OFF           = $00;
             LCD_CURSOR_UNDERLINE_ON   = $02;
             LCD_CURSOR_UNDERLINE_OFF  = $00;
             LCD_CURSOR_BLINK_ON       = $01;
             LCD_CURSOR_BLINK_OFF      = $00;
 
      LCD_DISPLAY_AND_CURSOR_SHIFT     = $10;
             LCD_DISPLAY_SHIFT         = $08;
             LCD_CURSOR_MOVE           = $00;
             LCD_RIGHT                 = $04;
             LCD_LEFT                  = $00;
 
      LCD_FUNCTION_SET                 = $20;
             LCD_8BIT_INTERFACE        = $10;
             LCD_4BIT_INTERFACE        = $00;
             LCD_2LINES                = $08;
             LCD_1LINE                 = $00;
 
      LCD_SET_DISPLAY_ADDRESS          = $80;
             LCD_ROW_0                 = $00;
             LCD_ROW_1                 = $40;
 
      LCD_SET_CGRAM_ADDRESS            = $40;
 
 
const
  CmdMode  = false;    // valores de pin RS
  CharMode = true;
 
var
  LCDData : byte absolute PORTB;       // 8 BIT DATA BUS.
  RS      : boolean absolute PORTA.0;  // SELECT REGISTER.
  EN      : boolean absolute PORTA.1;  // ENABLE STARTS DATA READ/WRITE.
  Counter : byte;
 
//***************************************************************************//
// Envia un datos en Display.
//***************************************************************************//
procedure LcdSend(dat: byte);
begin
  LCDData := dat; // Coloca dato en puerto de datos del LCD.
  EN := true;       // pulso de 2 ms.
  delay_ms(2);
  EN := false;
end;
 
//***************************************************************************//
// El dato enviado es un Comando.
//***************************************************************************//
procedure LCDCommand(comm: byte);
begin
  RS := CmdMode;
  LcdSend(comm);
end;
 
//***************************************************************************//
// El dato enviado es un caracter a mostrar en el display LCD.
//***************************************************************************//
procedure LCDWriteChar(c: byte);
begin
  RS := CharMode;
  LcdSend(c);
end;
 
//***************************************************************************//
// Rutina de inicializacion del display LCD 16x2.
// Resetea el LCD y lo inicializa en modo 8 bit mode, 2 columnas y cursor off.
//***************************************************************************//
procedure LCDInit;
begin
  TRISA := %11111100;
  TRISB := %00000000;
 
  RS := false;
  EN := false;
  delay_ms(500); // Espera 500ms
 
  // INICIACION DE DISPLAY MODO 8 BITS DE DATOS
  // Los tiempos de espera son los indicados en todos los
  // datasheets de los displays compatibles con el estandar Hitachi HD44780.
  LcdCommand(LCD_FUNCTION_SET + LCD_8BIT_INTERFACE);
  delay_ms(20); // Espera > 15ms
  LcdCommand(LCD_FUNCTION_SET + LCD_8BIT_INTERFACE);
  delay_ms(5);  // Espera > 4100us
  LcdCommand(LCD_FUNCTION_SET + LCD_8BIT_INTERFACE);
  delay_ms(1);  // Espera > 100us
 
  LcdCommand(LCD_FUNCTION_SET + LCD_8BIT_INTERFACE + LCD_2LINES);
  LcdCommand(LCD_DISPLAY_ON_OFF_AND_CURSOR + LCD_DISPLAY_OFF);
  LcdCommand(LCD_CLEAR_DISPLAY);
  LcdCommand(LCD_CHARACTER_ENTRY_MODE + LCD_INCREMENT + LCD_DISPLAY_SHIFT_OFF);
 
  LcdCommand(LCD_DISPLAY_ON_OFF_AND_CURSOR + LCD_DISPLAY_ON);
  LcdCommand(LCD_DISPLAY_AND_CURSOR_HOME);
end;
 
//***************************************************************************//
// PROGRAMA PRINCIPAL
//***************************************************************************//
begin
  LCDInit;
  while true do
  begin
    LCDWriteChar($48);  //H
    LCDWriteChar($65); // e;
    LCDWriteChar($6C); // l;
    LCDWriteChar($6C); // l
    LCDWriteChar($6F); // o;
    LCDWriteChar($A0); // Espacio Blanco;
    LCDWriteChar($57); // W;
    LCDWriteChar($6F); // o;
    LCDWriteChar($72); // r;
    LCDWriteChar($6C); // l;
    LCDWriteChar($64); // d;
 
    LcdCommand(LCD_SET_DISPLAY_ADDRESS + LCD_ROW_1);
 
    LCDWriteChar($50); // P;
    LCDWriteChar($69); // i;
    LCDWriteChar($63); // c;
    LCDWriteChar($50); // P;
    LCDWriteChar($61); // a;
    LCDWriteChar($73); // s;
    LCDWriteChar($A0); // Espacio Blanco;
    LCDWriteChar($30); // 0;
    LCDWriteChar($2E); // .;
    LCDWriteChar($35); // 5;
    LCDWriteChar($2E); // .;
    LCDWriteChar($37); // 7;
    LCDWriteChar($A0); // Espacio Blanco;
   
    Counter := 3;
    repeat
    begin
      LCDWriteChar($3E); // >);
      delay_ms(1000);
      dec(Counter);
    end;
    until Counter = 0;
 
    LcdCommand(LCD_CLEAR_DISPLAY);
  end;
end.
///***************************************************************************//
