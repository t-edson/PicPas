{Rutina de verificación de las rutinas en ensamblador.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
{$Mode Pascal}
uses PIC16F84A;
var
  pinLed: bit absolute PORTB.0;
  vbyte: byte;
	vbool: boolean;
  vbit: bit;
var
  abyte: byte absolute $20;
const
	CBYTE = 3; 

  procedure bien;
  begin
    pinLed := 1;
    delay_ms(30);
    pinLed := 0;
    delay_ms(70);
  end;
  procedure Mal;
  begin
    pinLed := 1;
    delay_ms(1500);
    pinLed := 0;
    asm SLEEP end
  end;
begin
  SetAsOutput(pinLed);
  pinLed := 0;

asm
  ;Opcode test
  ADDWF vbyte, w
  ANDWF vbyte, w
  CLRF vbyte
  CLRW 
  COMF vbyte, f
  DECF vbyte, f
  INCF vbyte, w

  ;test jumps
  DECFSZ vbyte, w
	NOP
  INCFSZ vbyte, w
	NOP
 
  IORWF vbyte, w
  MOVF vbyte, w
  MOVWF vbyte
  RLF vbyte, w
  RRF vbyte, w
  SUBWF vbyte, w
  SWAPF vbyte, w
  XORWF vbyte, w
  BCF vbyte, 0
  BSF vbyte, 0
  BTFSC vbyte, 0
	NOP
  BTFSS vbyte, 0
  NOP
  ADDLW 0
  ANDLW 0
  ;CALL 
  CLRWDT 
  GOTO $+1
  IORLW 0
  MOVLW 0
  ;RETFIE
  ;RETLW 
	;RETURN 
  ;SLEEP 
	SUBLW 0
	XORLW 0
end
  //variable asigment
	abyte := 66;
	asm
    MOVLW 65
    MOVWF $20  ;absolute address
  end
  if abyte = 65 then bien else mal; 

  vbyte := 0;
  asm
    MOVLW 5
	  MOVWF vbyte
	end
  if vbyte = 5 then bien else mal; 

  vbool := false;
  asm BSF vbool end
  if vbool then bien else mal; 

  vbit := 1;
  asm BCF vbit end
  if vbit=0 then bien else mal; 

  vbit := 1;
  asm 
    BTFSC vbit
    BCF vbit 
  end
  if vbit=0 then bien else mal; 
	
	//constant access
  vbyte := 0;
  asm 
    MOVLW CBYTE 
    MOVWF vbyte
  end
  if vbyte = CBYTE then bien else mal; 

  vbyte := 0;
  asm 
    BSF vbyte, CBYTE
  end
  if vbyte = 8 then bien else mal; 

	//jumps
  asm 
    GOTO $+2
    SLEEP  ;stop if not jump
  end

	vbyte := 10;
  asm 
    DECFSZ vbyte, f
    GOTO $-1
  end
  if vbit=0 then bien else mal; 

	vbyte := 10;
  asm 
  label1:
    DECFSZ vbyte, f
    GOTO label1
  end
  if vbit=0 then bien else mal; 
  asm org $ end
  vbit := 1;
end.
