{Test code for validation of Variable declarations and Function Systems.}
uses PIC10F202, UnitTest;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var  //simple type declarations
  vbit: bit;
  vbool: boolean;
  vchar: char;
	vbyte: byte;
  vword: word;
  vdword: dword;
begin
  SetAsOutput(pinLed);

  ///////////////////////////////////////////////////
  ///// System Function
  ///////////////////////////////////////////////////

  /////// Byte() function
  vword := $0102;
  if byte(vword) = 2  then good else bad end;
  if byte(vword+1) = 3  then good else bad end;

  vdword := $01020304;
  if byte(vdword) = 4 then good else bad end;
  if byte(vdword+dword(1)) = 5 then good else bad end;

  /////// Word() function
  vword := word(65);  //word of byte
  if vword = word(65) then good else bad end;
  vword := word('A');  //word of char
  if vword = word(65)  then good else bad end;
  vword := word($FFFF);  //word of word
  if vword = $FFFF  then good else bad end;
  vword := word($01020304);  //word of dword
  if vword = $0304  then good else bad end;

  vbyte := 65;
  if word(vbyte) = word(65) then good else bad end;
  if word(vbyte+1) = word(66) then good else bad end;
  vchar := 'A';  //word of char
  if word(vchar) = word(65)  then good else bad end;
  vword := $FFFF;  //word of word
  if word(vword) = $FFFF  then good else bad end;
  if word(vword+1) = word(0) then good else bad end;
  vdword := $01020304;  //word of dword
  if word(vdword) = $0304  then good else bad end;
  if word(vdword+dword(1)) = $0305  then good else bad end;

  /////// DWord() function
  vdword := dword(65);  //dword of byte
  if vdword = dword(65) then good else bad end;
  vdword := dword('A');  //dword of char
  if vdword = dword(65)  then good else bad end;
  vdword := dword($FFFF);  //dword of word
  if vdword = dword($FFFF) then good else bad end;
  vdword := dword($01020304);  //dword of dword
  if vdword = $01020304  then good else bad end;

  vbyte := 65;
  if dword(vbyte) = dword(65) then good else bad end;
  if dword(vbyte+1) = dword(66) then good else bad end;
  vchar := 'A';  //dword of char
  if dword(vchar) = dword(65)  then good else bad end;
  vword := $FFFF;  //dword of word
  if dword(vword) = dword($FFFF)  then good else bad end;
  if dword(vword+1) = dword(0) then good else bad end;

//Funciones comentadas por falta de memoria Flash

//  vdword := $01020304;  //dword of dword
//  if dword(vdword) = $01020304  then good else bad end;
//  if dword(vdword+dword(1)) = $01020305  then good else bad end;
//  
//  /////// SetAsInput() function
//  {Cuidado que esto puede fallar en Proteus, porque parece que no maneja
//  bien los bits no implementados, y puede leer $FF u otro valor en TRISA}
////  SetAsInput(PORTA);
////  if TRISA = $3F then good else bad end;
//
//  SetAsInput(PORTB);
//  if TRISB = $FF then good else bad end;
//  
//  /////// SetAsOutput() function
//  SetAsOutput(PORTA);
//  if TRISA = 0 then good else bad end;
//  SetAsOutput(PORTB);
//  if TRISB = 0 then good else bad end;
  
end.

