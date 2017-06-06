uses PIC16f84A;
var
  resultado : word;  // Variable global necesaria para devolver el valor de multiplicacion.

//***********************************************************************
//  PROCEDIMIENTO: Multiplicacion
//  Multiplica dos valores de 8 bits.
//  El resultado queda almacenado en una variable global llamada resultado.
//  Por ahora PicPas (v.0.6.7) no permite el uso de funciones.
//***********************************************************************
procedure Multiplicar (multiplicando, multiplicador : byte);
begin
ASM
;Inicializacion de Registros
  BCF STATUS,5              ; RP0=0 / Trabajamos en el Banco de memoria 0.
  CLRF resultado.LOW        ; Limpia el byte bajo de la variable global resultado.
  CLRF resultado.HIGH       ; Limpia el byte alto de la variable global resultado.
;Comprueba multiplicacion por cero.
  MOVLW $00
  SUBWF multiplicador,W
  BTFSC STATUS,2
  GOTO MULT_FIN             ; Si multiplicador = 0 entonces acabar.
;LOOP de multiplicacion
MULT_LOOP:
  MOVF multiplicando,W      ; Carga el multiplicador en el registro W.
  ADDWF resultado.LOW,F     ; Suma el valor de multiplicando al byte bajo de la variable global resultado
  BTFSC STATUS,0            ; Comprueba el bit CARRY del registro STATUS.
  INCF resultado.HIGH,F     ; Si CARRY es 0 resultado.LOW se ha desbordado se incrementa resultado.HIGH
  DECFSZ multiplicador,F    ; Decrementa multiplicador y comprueba si ha llegado a cero.
  GOTO MULT_LOOP            ; nuevo paso del bucle de multiplicacion.
MULT_FIN:
END
end;

begin
  Multiplicar(1,2);
end. 
