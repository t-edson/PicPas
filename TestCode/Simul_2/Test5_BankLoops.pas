{Programa para la verifiación del manejo de bancos RAM, dentro de lazos.}

{$PROCESSOR PIC16C63}
{$FREQUENCY 8Mhz}
uses PIC16C63;  //Se requiere un MCU con más de un banco 
var
  a0: byte;               //at bank0
  a1: byte absolute $A0;  //at bank 1
begin
  // NOTE: PicPas must be configured with optimization activated
  // Validation must be done, verifying the messages.
  // Correct execution of this code no necessary means the code generation is OK.

  ///////////////////////////////////////////////////
  ///// WHILE defined in Compilation Time
  ///////////////////////////////////////////////////

  //WHILE false
  a0 := 0;  //Bank 0
  while false do 
    a1 := 1;  //Bank 1. Must not generate code.
  end;
  {$MSGBOX 'Is this 0? -> ' + CURRBANK} 

  //WHILE true
  a0 := 0;  //Bank 0
  while true do 
    a1 := 1;  //Bank 1. Must not generate code.
  end;
  //Verify if _BANSEL(0) is included at the endof the WHILE block
  {$MSGBOX 'Is this 0? -> ' + CURRBANK} 

  //Normal WHILE 
  a0 := 0;  //Bank 0
  while PORTB = 0 do //condition in bank 0
    a1 := 1;  //Block in bank 1. 
  end;
  //Verify if CurrBank = 0 at the end of the WHILE (because condition end in bank 0)
  {$MSGBOX 'Is this 0? -> ' + CURRBANK} 

  //Normal WHILE 
  a0 := 0;  //Bank 0
  while TRISB = 0 do //condition in bank 1
    a0 := 1;  //Block in bank 0. 
  end;
  //Verify if CurrBank = 1 at the end of the WHILE (because condition end in bank 1)
  {$MSGBOX 'Is this 1? -> ' + CURRBANK} 
  
  
end.
