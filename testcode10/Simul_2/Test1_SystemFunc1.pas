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

  /////// Inc() procedure
  vchar  := #0;
  vbyte  := 0;
  vword  := 0;
  vdword := 0;
  Inc(vchar ); if vchar  = #1       then good else bad end;
  Inc(vbyte ); if vbyte  = 1        then good else bad end;
  Inc(vword ); if vword  = word(1)  then good else bad end;
  Inc(vdword); if vdword = dword(1) then good else bad end;

  vword  := $FF;
  Inc(vword ); if vword  = $100  then good else bad end;
  vdword := $FFFF;
  Inc(vdword); if vdword = $10000 then good else bad end;

  /////// Dec() procedure
  vchar  := #0;
  vbyte  := 0;
  vword  := 0;
  vdword := 0;
  Dec(vchar ); if vchar  = #255      then good else bad end;
  Dec(vbyte ); if vbyte  = $FF       then good else bad end;
  Dec(vword ); if vword  = $FFFF     then good else bad end;
  Dec(vdword); if vdword = $FFFFFFFF then good else bad end;

  vword  := $100;
  Dec(vword ); if vword  = word($FF) then good else bad end;
  vdword := $10000;
  Dec(vdword); if vdword = dword($FFFF) then good else bad end;
  
  /////// Ord() function
  vchar := 'A';
  vbyte := Ord(vchar);
  if vbyte  = 65  then good else bad end;
  vbyte := Ord('B');
  if vbyte  = 66  then good else bad end;
  vbyte := 34+Ord('B');
  if vbyte  = 100  then good else bad end;
  vbyte := (vbyte + 1)+Ord('A');
  if vbyte  = 166  then good else bad end;
  

  /////// Chr() function
  vbyte := 65;
  vchar := Chr(vbyte);
  if vchar = 'A'  then good else bad end;
  vchar := Chr(66);
  if vchar  = 'B'  then good else bad end;
  vbyte := 1;
  vchar := Chr(65+vbyte);
  if vchar  = 'B'  then good else bad end;

  /////// Bit() function
  vbit := bit(65);
  if vbit = 1  then good else bad end;
  vbit := bit(0);
  if vbit = 0  then good else bad end;

  vbyte := 65;
  vbit := bit(vbyte);
  if vbit = 1  then good else bad end;
  
  vbyte := 0;
  vbit := bit(vbyte);
  if vbit  = 0  then good else bad end;
  vbit := bit(vbyte+1);
  if vbit  = 1  then good else bad end;
  if bit(2) and 1 = 1 then good else bad end;
  
  vbool := true;
  if bit(vbool) = 1 then good else bad end;
  if bit(not vbool) = 0 then good else bad end;
  if bit(not vbool) and bit(vbool) = 0 then good else bad end;
  if bit(not vbool) or bit(vbool) = 1 then good else bad end;

  /////// Boolean() function
  vbool := boolean(65);
  if vbool  then good else bad end;
  vbool := boolean(0);
  if not vbool then good else bad end;

  vbyte := 65;
  vbool := boolean(vbyte);
  if vbool then good else bad end;
  
  vbyte := 0;
  vbool := boolean(vbyte);
  if not vbool then good else bad end;
  vbool := boolean(vbyte+1);
  if vbool then good else bad end;
  if boolean(2) and true then good else bad end;
  
  vbit := 1;
  if boolean(vbit) then good else bad end;
  if not boolean(not vbit) then good else bad end;
  if not (boolean(not vbit) and boolean(vbit) ) then good else bad end;
  if boolean(not vbit) or boolean(vbit) then good else bad end;
  
  /////// Byte() function
  vbyte := byte(65);  //byte of byte
  if vbyte = 65  then good else bad end;
  vbyte := byte('A');  //byte of char
  if vbyte = 65  then good else bad end;
  vbyte := byte($FFFF);  //byte of word
  if vbyte = $FF  then good else bad end;

  vbit := 0;
  if byte(vbit) = 0  then good else bad end;
  vbit := 1;
  if byte(vbit)+1 = 2  then good else bad end;

  vbit := 1;
  if byte(vbit and 0) = 0  then good else bad end;

  vbyte := 10;
  if byte(vbyte) = 10  then good else bad end;
  vbyte := 10;
  if byte(vbyte+5) = 15  then good else bad end;

  vchar := '0';
  if byte(vchar) = 48  then good else bad end;
  
 
end.

