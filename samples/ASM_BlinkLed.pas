{Sample program to blink a Led on PORTB.0
using ASM and a Pascal variable.}
uses PIC16F84A;
{$FREQUENCY 4 MHZ }
var counter: byte;
begin
asm
;------------- SETUP ---------------
        BSF   STATUS, STATUS_RP0
        CLRF  PORTB
        BCF   STATUS, STATUS_RP0
;----------  MAIN LOOP -------------
INICIO: 
        BSF   PORTB, 0 ;led ON
        CALL  ESPERA   ;delay
        BCF   PORTB, 0 ;led OFF
        CALL  ESPERA   ;delay
        GOTO  INICIO
;-------- SUBROUTINES --------------
ESPERA:
        CLRF COUNTER
LOOP1:  DECFSZ COUNTER, F
        GOTO LOOP1
        RETURN
end
end.
