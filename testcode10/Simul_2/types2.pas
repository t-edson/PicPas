uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
type
  byteptr = ^byte;
  wordptr = ^word;
type  //Tipos arreglos
  Tachar = array[3] of char;
  Tabyte = array[3] of byte;
  Taword = array[3] of word;
var  //Variables arreglos
  achar: Tachar;
  abyte: Tabyte;
  aword: Taword;
var  //Variables para punteros
  ptrByte1, ptrByte2: byteptr;
  ptrWord1, ptrWord2: wordptr; 
  m, n : byte;
  x, y : word;
begin
  SetAsOutput(pinLed);
  pinLed := 0;
  
  //////////////////////////////////////////////////////
  //////////////// Punteros a byte /////////////////////
  //////////////////////////////////////////////////////
  ptrByte1 := 0;    //asignación constante
  if ptrByte1 = 0 then good else bad end;
  ptrByte2 := 255;  //asignación constante
  if ptrByte2 = 255 then good else bad end;
  m := 5;
  ptrByte1 := m;    //asignación byte 
  if ptrByte1 = 5 then good else bad end;
  
  ptrByte1 := $85;
  ptrByte2 := ptrByte1;    //asignación punteros
  if ptrByte2 = $85 then good else bad end;

  //Aritmética de punteros
  ptrByte2 := ptrByte1+$10;    //asignación expresión de punteros
  if ptrByte2 = $95 then good else bad end;
  
  ptrByte2 := ptrByte1-$10;    //asignación expresión de punteros
  if ptrByte2 = $75 then good else bad end;

  inc(ptrByte2); 
  dec(ptrByte2); 
  if ptrByte2 = $75 then good else bad end;
  
  //Acceso a variables
  m := $23;
  ptrByte1 := @m;

  n := $12;
  ptrByte2 := @n;

  if ptrByte1^ = $23 then good else bad end;
  ptrByte1^ := $FF;
  if m = $ff then good else bad end;

  m := $23;  
  ptrByte2^ := ptrByte1^;
  if n = $23 then good else bad end;

  ptrByte1 := @m;
  n := ptrByte1^;
  if m = n then good else bad end;

  //Operaciones con desreferencia stVarRefVar
  m := $12;
  if ptrByte1^ = $12 then good else bad end;
  if ptrByte1^ + 1 = $13 then good else bad end;
  if ptrByte1^ - 1 = $11 then good else bad end;
  if ptrByte1^ + ptrByte1^ = $24 then good else bad end;
  if $0f and ptrByte1^  = $02 then good else bad end;
  
  //Pendientes
//  delay_ms(ptrByte1^);
//  Inc(ptrByte1^);
//  Dec(ptrByte1^);
//  ptrByte1^.bit7 := 0;
//  chr(ptrByte1^);
//  bit(ptrByte1^);
//  word(ptrByte1^);
//  dword(ptrByte1^);

  //Operaciones con desreferencia stVarRefExp
  //Se asume para esta prueba que "n", está ubicado después de "m"
  //De otar forma no funcionará, porque ptrByte1+1, fallaría
  n := $12;  
  if (ptrByte1+1)^ = $12 then good else bad end;
  if (ptrByte1+1)^ + 1 = $13 then good else bad end;
  if (ptrByte1+1)^ - 1 = $11 then good else bad end;
  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
 //  if (ptrByte1+1)^ + (ptrByte1+1)^ = $24 then good else bad end;  
  }
  if $0f and (ptrByte1+1)^  = $02 then good else bad end;
  
  //Pendientes
//  delay_ms((ptrByte1+1)^);
//  Inc((ptrByte1+1)^);
//  Dec((ptrByte1+1)^);
//  (ptrByte1+1)^.bit7 := 0;
//  chr((ptrByte1+1)^);
//  bit((ptrByte1+1)^);
//  word((ptrByte1+1)^);
//  dword((ptrByte1+1)^);


  //////////////////////////////////////////////////////
  //////////////// Punteros a word /////////////////////
  //////////////////////////////////////////////////////
  ptrWord1 := 0;    //asignación constante
  if ptrWord1 = 0 then good else bad end;
  ptrWord2 := 255;  //asignación constante
  if ptrWord2 = 255 then good else bad end;
  m := 5;
  ptrWord1 := m;    //asignación byte 
  if ptrWord1 = 5 then good else bad end;
  
  ptrWord1 := $85;
  ptrWord2 := ptrWord1;    //asignación punteros
  if ptrWord2 = $85 then good else bad end;

  //Aritmética de punteros
  ptrWord2 := ptrWord1+$10;    //asignación expresión de punteros
  if ptrWord2 = $95 then good else bad end;
  
  ptrWord2 := ptrWord1-$10;    //asignación expresión de punteros
  if ptrWord2 = $75 then good else bad end;

  inc(ptrWord2); 
  dec(ptrWord2); 
  if ptrWord2 = $75 then good else bad end;
  
  //Acceso a variables
  x := $23;
  ptrWord1 := @x;

  y := $12;
  ptrWord2 := @y;

  if ptrWord1^ = word($23) then good else bad end;
  ptrWord1^ := word($FF);
  if x = word($ff) then good else bad end;

  x := $23;  
  ptrWord2^ := ptrWord1^;
  if y = word($23) then good else bad end;

  ptrWord1 := @x;
  y := ptrWord1^;
  if x = y then good else bad end;

  //Operaciones con desreferencia stVarRefVar
  x := $12;
  if ptrWord1^ = word($12) then good else bad end;
  if ptrWord1^ + word(1) = word($13) then good else bad end;
  if ptrWord1^ - word(1) = word($11) then good else bad end;
//  if ptrWord1^ + ptrWord1^ = word($24) then good else bad end;
//  if word($0f) and ptrWord1^  = word($02) then good else bad end;
//  
//  //Pendientes
////  delay_ms(ptrWord1^);
////  Inc(ptrWord1^);
////  Dec(ptrWord1^);
////  ptrWord1^.bit7 := 0;
////  chr(ptrWord1^);
////  bit(ptrWord1^);
////  word(ptrWord1^);
////  dword(ptrWord1^);
//
//  //Operaciones con desreferencia stVarRefExp
//  //Se asume para esta prueba que "y", está ubicado después de "x"
//  //De otar forma no funcionará, porque ptrWord1+1, fallaría
//  y := $12;  
//  if (ptrWord1+1)^ = $12 then good else bad end;
//  if (ptrWord1+1)^ + 1 = $13 then good else bad end;
//  if (ptrWord1+1)^ - 1 = $11 then good else bad end;
//  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
// //  if (ptrWord1+1)^ + (ptrWord1+1)^ = $24 then good else bad end;  
//  }
//  if $0f and (ptrWord1+1)^  = $02 then good else bad end;
//  
//  //Pendientes
////  delay_ms((ptrWord1+1)^);
////  Inc((ptrWord1+1)^);
////  Dec((ptrWord1+1)^);
////  (ptrWord1+1)^.bit7 := 0;
////  chr((ptrWord1+1)^);
////  bit((ptrWord1+1)^);
////  word((ptrWord1+1)^);
////  dword((ptrWord1+1)^);
//
//
  //////////////////////////////////////////////////////
  ////////////////////// Arreglos  /////////////////////
  //////////////////////////////////////////////////////
//  achar[0] := 'a';
//  if 'a' = achar[0] then good else bad end;
//  vbyte := 1;
//  achar[vbyte] := 'a';
//  if achar[vbyte] = 'a' then good else bad end;
//
//  abyte[0] := 1;
//  vbyte := 1;
//  abyte[vbyte] := 1;
//  
//  vbyte := 1;
//  aword[0] := word(5000);
//  aword[vbyte] := word(5000);
end.
