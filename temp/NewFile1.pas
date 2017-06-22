{
*  (C) AguHDz 05-JUN-2017
*  Ultima Actualizacion: 14-JUN-2017
*
*  Compilador PicPas v.0.6.9 (https://github.com/t-edson/PicPas)
*
*  ESCRITURA DE DATOS EN EEPROM INTERNA DEL MICROCONTROLADOR
*  Ejemplo de uso de nombrea de variables y constantes definindas en nueva
*  UNIT 16F84A.pas, ahora mas coherente en el uso de los nombre de bytes y
*  bits de la zona SFR de memoria y sus registros.
}
 
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program EEPROMInterna;
 
uses
  PIC16F84A;  
 
var
  Contador :byte;
 
procedure WriteEEPROMASM(direccion , valor: byte);
begin
ASM
ESCRITURA:               ; Establecer EEADR y EEDATA
  MOVF  direccion,w      ; Direccion de esctritura
  MOVWF EEADR                ; Escribe la dirección en EEADR
  MOVF  valor,w          ; Dato a escribir en EEPROM
  MOVWF EEDATA             ; Se escribe el dato en EEDATA
  BSF   STATUS,bit_RP0     ; Selecciona el banco 1
  BSF   EECON1,bit_WREN    ; Permiso de escritura activado
;Comienzo de la secuencia de escritura
  MOVLW $55
  MOVWF EECON2             ; Se escribe el dato 55 h en EECON2
  MOVLW $AA
  MOVWF EECON2             ; Se escribe AA h en EECON2
  BSF   EECON1,bit_WR        ; Comienza la escritura
  BCF   EECON1,bit_WREN    ; Permiso de escritura desactivado
ESPERA:
  BTFSC EECON1,bit_WR    ; Espera a que termine la escritura
  GOTO  ESPERA
  BCF   STATUS,bit_RP0     ; Selecciona el banco 0
END
end;
 
procedure WriteEEPROM(direccion , valor: byte);
begin
  EEADR       := direccion;
  EEDATA      := valor;
  EECON1_WREN := 1;
  EECON2      := $55;
  EECON2      := $AA;
  EECON1_WR   := 1;
  EECON1_WREN := 0;
  repeat until (EECON1_WR = 0);
end;
 
begin
  repeat
  for contador:=$00 to $10 do  
    WriteEEPROMASM(contador,$00);
  end;
 
  for contador:=$00 to $10 do  
    WriteEEPROM(contador,$FF);
  end;
  until false;
end.
