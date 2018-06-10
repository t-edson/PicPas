{Rutina de verificación para el manejo de tipos.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
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
type   //Tipos punteros
  byteptr = ^byte;
  wordptr = ^word;
var
  ptrByte1, ptrByte2: byteptr;
  ptrWord1, ptrWord2: wordptr; 
//  m, n : byte;
//  x, y : word;
   
//  procedure proc1(x, y: byte1): word1;
//  begin
//    exit( word(x+y));
//  end; 
  
begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////
	//////////////  Existencia básica de tipos ///////////////////
  //////////////////////////////////////////////////////
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
  


end.
