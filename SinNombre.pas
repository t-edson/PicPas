 {Prueba Puertos de Entrada y Salida.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
program LedBlink;
uses a, b;
var
PORTB: BYTE absolute $06;
PinPausa: boolean absolute PORTB.1;
x: byte;
procedure proc1;
begin
  delay_ms(100);
end;
procedure DetectaEntrada;
begin
  PinPausa := true;
  proc1;
end;
var y: byte;
begin
  PORTB := 5;
//  delay_ms(500);
  DetectaEntrada;
end.
