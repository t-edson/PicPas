{Description of the unit.}
{$IFNDEF LCD_EN}
{$MSGBOX 'Debe definir LCD_EN'}
{$ENDIF}
unit LCDlib;
interface
uses PIC16F84A, LCDconst;

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
//***************************************************************************//
// El dato enviado es un Comando.
//***************************************************************************//
procedure LCDCommand(comm: byte);
//***************************************************************************//
// El dato enviado es un caracter a mostrar en el display LCD.
//***************************************************************************//
procedure LCDWriteChar(register c: char);
//***************************************************************************//
// Rutina de inicializacion del display LCD 16x2.
// Resetea el LCD y lo inicializa en modo 8 bit mode, 2 columnas y cursor off.
//***************************************************************************//
procedure LCDInit;
  
implementation

procedure LcdSend(dat: byte);
begin
  LCDData := dat; // Coloca dato en puerto de datos del LCD.
  EN := true;       // pulso de 2 ms.
  delay_ms(2);
  EN := false;
end;
 
procedure LCDCommand(comm: byte);
begin
  RS := CmdMode;
  LcdSend(comm);
end;
procedure LCDWriteChar(register c: char);
begin
  RS := CharMode;   //No usa W
  LcdSend(ord(c));  //Cuidado con "c", porque es de tipo Register
end;
procedure LCDClear;
begin
  RS := CmdMode;
  LcdSend(LCD_CLEAR_DISPLAY);
end;

procedure LCDhexa(n: byte);
var 
  n1, n2: byte; //nibles
begin
  {La idea del algoritmo, no es que sea eficiente, sino seguro, porque lo vamos a 
  usar para probar al compialdor}
  n1 := n and $f0;
	n1 := n1 >> 2;
	n1 := n1 >> 2;
	n2 := n and $0f;
  LcdSend(48 + n1);
  LcdSend(48 + n2);
end;
 
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

end.
