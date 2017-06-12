// Prueba de esctritura en la EEPROM interna del PIC16F84A

{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F877A}
program EEPROMInterna;

uses
  PIC16F84A;  
 
var
	EECON1   :byte absolute $88;  // FALLA ENSAMBLADOR SI SE DEJA $88.
	EECON2   :byte absolute $89;  // FALLA ENSAMBLADOR SI SE DEJA $89.

  Contador :byte;
procedure WriteEEPROM(direccion , valor: byte);
begin
  EEADR  := direccion;
  EEDATA := valor;
  EECON1.2 := 1;
  EECON2   := $55;
  EECON2   := $AA;
  EECON1.1 := 1;
  EECON1.2 := 0;
  repeat until (EECON1.1 = 0);
  setbank(0);
  SetBank(10);
end;

begin
  WriteEEPROM(contador,$FF);
end.
