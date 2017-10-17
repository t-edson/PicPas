{Rutina de verificación para el manejo de tipos.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
uses UnitTest, PIC16F877A;
var  //tipos básicos
  vbit  : bit;
  vbool : boolean;
	vbyte : byte;
  vchar : char;
	vword : word;
  vdword: dword;
type  //tipos equivalentes
  bit1   = bit;
  bool1  = boolean;
	byte1  = byte;
  char1  = char;
	word1  = word;
  dword1 = dword;
var  //variables de tipos equivalentes
  vbit1  : bit1;
  vbool1 : bool1;
	vbyte1 : byte1;
  vchar1 : char1;
	vword1 : word1;
  vdword1: dword1;
type  //Tipos arreglos
  Tachar = array[3] of char;
  Tabyte = array[3] of byte;
  Taword = array[3] of word;
var
  achar: Tachar;
  abyte: Tabyte;
  aword: Taword;
   
//  procedure proc1(x, y: byte1): word1;
//  begin
//    exit(x+y);
//  end; 
  
begin
  SetAsOutput(pinLed);
  pinLed := 0;

	//////////////  Existencia básica de tipos ///////////////////
  vbit := 1;
  if vbit = 1 then good else bad end;
  vbool := true;
  if vbool = true then good else bad end;
	vbyte := 1;
  if vbyte = 1 then good else bad end;
  vchar:= ' ';
  if vchar = ' ' then good else bad end;
	vword:= 1;
  if vword = word(1) then good else bad end;
	vdword:= 1;
  if vdword = dword(1) then good else bad end;

  //Operaciones con tipos equivalentes
  vbit1 := 1;
  if vbit1 = 1 then good else bad end;
  vbool1 := true;
  if vbool1 = true then good else bad end;
	vbyte1 := 1;
  if vbyte1 = 1 then good else bad end;
	vbyte1 := vbyte1 + 1;
  if vbyte1 = 2 then good else bad end;
  
  vchar1:= ' ';
  if vchar1 = ' ' then good else bad end;
	vword1:= 1;
  if vword1 = word(1) then good else bad end;
  
	vdword1:= 1;
  if vdword1 = dword(1) then good else bad end;
	vdword1:= 1000;
  if vdword1 = dword(1000) then good else bad end;

  //Uso de variables de tipos equivalentes
  vword1 := 0;
  for vbyte1:=1 to 5 do 
    vword1 := vword1 + 1;
  end;
  if vword1 = word(5) then good else bad end;

  //Paso de parámetros a procedimientos  
  //  if proc1(1,2) = 3 then good else bad end;
  // NO IMPLEMENTADO
  
  //////////////////////////////////////////////////////
  ////////// Pruebas básicas a tipos  //////////////////
  //////////////////////////////////////////////////////
  achar[0] := 'a';
//  if 'a' = achar[0] then good else bad end;
  vbyte := 1;
  achar[vbyte] := 'a';
//  if achar[vbyte] = 'a' then good else bad end;

  abyte[0] := 1;
  vbyte := 1;
  abyte[vbyte] := 1;
  
  vbyte := 1;
  aword[0] := word(5000);
  aword[vbyte] := word(5000);
end.
