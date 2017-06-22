{
*  (C) AguHDz 05-JUN-2017
*  Ultima Actualizacion: 14-JUN-2017
*
*  Compilador PicPas v.0.6.9 (https://github.com/t-edson/PicPas)
*
*  MULTIPLICACION DOS VALORES DE UN BYTE CON RESULTADO EN 2 BYTES
*  PicPas aun no ha implementado la operacion de multiplicacion,
*  el siguiente procedimiento en escrito en ensamblador permite
*  realizar operaciones de multiplicacion.
*
*  El ejemplo de aplicacion corre sobre un PIC16F877A, y multiplica
*  el valor introducido por los puertos RA0..RA5 y RE0..RE1 = variable auxiliar
*  por el valor leido en el puerto B (RB0..RB7). El resultado es de 2 bytes y
*  se muestra como salida digital por los puertos C y D.
}
 
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$MODE PICPAS}
program Multipl_HEX_ASM;
 
uses PIC16F877A;
 
var
  resultado : word; // Variable global para mostrar el resultado en display.
  auxiliar  : byte; // Variable auxiliar para operar con los valores leidos de entrada
                    // en los puertos A y E.
 
//***********************************************************************
//  FUNCION: Multiplicar
//  Multiplica dos valores de 8 bits.
//  Devuelve el resultado en variable tipo word de 16 bits.
//***********************************************************************
procedure Multiplicar (multiplicando, multiplicador : byte) : word;
var
  multiplicacion : word;
begin
ASM
;Inicializacion de Registros
  BCF STATUS_RP0                 ; RP0=0 / Trabajamos en el Banco de memoria 0.
  CLRF multiplicacion.LOW        ; Limpia el byte bajo de la variable global resultado.
  CLRF multiplicacion.HIGH       ; Limpia el byte alto de la variable global resultado.
;Comprueba multiplicacion por cero.
  MOVLW $00
  SUBWF multiplicador,W
  BTFSC STATUS_Z
  GOTO MULT_FIN                  ; Si multiplicador = 0 entonces acabar.
;LOOP de multiplicacion
MULT_LOOP:
  MOVF multiplicando,W           ; Carga el multiplicador en el registro W.
  ADDWF multiplicacion.LOW,F     ; Suma el valor de multiplicando al byte bajo de la variable global resultado
  BTFSC STATUS_C                 ; Comprueba el bit CARRY del registro STATUS.
  INCF multiplicacion.HIGH,F     ; Si CARRY es 0 resultado.LOW se ha desbordado se incrementa resultado.HIGH
  DECFSZ multiplicador,F         ; Decrementa multiplicador y comprueba si ha llegado a cero.
  GOTO MULT_LOOP                 ; nuevo paso del bucle de multiplicacion.
MULT_FIN:
END
exit(multiplicacion);
end;
 
//***********************************************************************
// PROGRAMA PRINCIPAL ***************************************************
//***********************************************************************
begin
  ADCON1 := $07;           // Todos los pines configurados como digitales.
  ADCON0 := $00;           // Desactiva conversor A/D.
  SetAsInput(PORTA);
  SetAsInput(PORTB);
  SetAsInput(PORTE.0);
  SetAsInput(PORTE.1);
  SetAsOutput(PORTC);
  SetAsOutput(PORTD);
  repeat  
    auxiliar   := PORTA;
    auxiliar.6 := PORTE.0;
    auxiliar.7 := PORTE.1;
    resultado  := Multiplicar(auxiliar,PORTB);
    PORTC      := resultado.LOW;
    PORTD      := resultado.HIGH;
  until false;
end.
