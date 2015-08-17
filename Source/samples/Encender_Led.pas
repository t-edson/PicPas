{programa de demostración de uso}
{$PROCESSOR PIC16F690 }
{$FREQUENCY 8 MHZ }
{$OPTIMIZE SPEED }
{$POINTERS LARGE }
{$CONFIG FCMEN = OFF, IESO = OFF, OSC = INTOSCIO, WDT = OFF }
{$DEFINE PK2} // Generate code for testing on PicKit2 evaluation board, else it is for the target board.
{$DEFINE USE_SERIALPORT_OUTPUT} // Generate code for Serial Output only

program aaa;
var
  INDF  : BYTE @00;
  STATUS: BYTE absolute 03;
  FSR   : BYTE absolute 04;
  PORTA : BYTE absolute 05;
  TRISA : BYTE absolute $85;
  PORTB : BYTE absolute 06;
  TRISB : BYTE absolute 134;
  a,b: byte;
begin
STATUS := 32;
PORTB := 0;   //pone como salida
STATUS := 0;
PORTB := 0;   //apaga salidas
//parpadeo
delay_ms(1000);
a := 0;
while a<>10 do begin
  delay_ms(300);
  PORTB := 255;
  delay_ms(300);
  PORTB := 0;
  a := a + 1;
end;
end;

