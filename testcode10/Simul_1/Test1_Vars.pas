{Test code for validation of Variable declarations and Function Systems.}
uses PIC10F202;
{$FREQUENCY 4Mhz}
{$OUTPUTHEX 'output.hex'}
var  //test pin
  pinLed: bit absolute GPIO.0;
procedure bien;
begin
  pinLed := 1;
  delay_ms(30);
  pinLed := 0;
  delay_ms(20);
end;
procedure Mal;
begin
  pinLed := 1;
  delay_ms(2000);
  pinLed := 0;
  asm SLEEP end
end;
var  //simple type declarations
  vbit: bit;
  vbool: boolean;
  vchar: char;
	vbyte: byte;
  vword: word;
  vdword: dword;
var  //absolute type declarations
  abit: bit absolute $20.0;
  abool: boolean absolute vbool;
  achar: char absolute vchar;
	abyte: byte absolute vbyte;
  aword: word absolute vword;
  byteL: byte absolute aword.low;
  byteH: byte absolute aword.high;
const
  consWord = word($0A00); 
begin
  SetAsOutput(pinLed);

  ///////////////////////////////////////////////////
  ///// Var declration
  ///////////////////////////////////////////////////
  //Basic assigment
  vbit := 0;
	if vbit=0 then bien else mal end;
  vbit := 1;
	if vbit=1 then bien else mal end;

  vbool := false;
	if vbool then mal else bien end;
  vbool := true;
	if vbool then bien else mal end;

	vchar := ' ';
	if vchar = ' ' then bien else mal end;
  vchar := #65;
	if vchar = chr(65) then bien else mal end;
  vchar := #65;
	if vchar = 'A' then bien else mal end;
	
	//Absolute position
  abool := false;
	if vbool then mal else bien end;
  abool := true;
	if vbool then bien else mal end;

  vbyte := 5;
	if abyte = 0 then mal else bien end;
  vbyte := 255;
	if abyte = 255 then bien else mal end;

  //Access to bit
  vbyte := 0;
  vbyte.bit0 := 1;
  vbyte.bit7 := 1;
	if vbyte = $81 then bien else mal end;
	if ($10).bit0 = 0 then bien else mal end;
	if ($10).bit4 = 1 then bien else mal end;
	if ($FFFF).high.bit7 = 1 then bien else mal end;

  //Access to bytes of word
  vword:=$FF01;
  vbyte := vword.low;
	if vbyte = 1 then bien else mal end;
  vbyte := vword.high;
	if vbyte = 255 then bien else mal end;
  vbyte := consWord.high;
	if vbyte = 10 then bien else mal end;

  byteL := 0;
  aword := $2010;
	if byteL = $10 then bien else mal end;
	if byteH = $20 then bien else mal end;

  aword := $2010;
  vword.low.bit0 := 1;
	if byteL = $11 then bien else mal end;

	aword := 0;
	byteL := 5;
	aword.high := aword.low;
	if aword = $0505 then bien else mal end;

	aword := 0;
  aword.low.bit0 := 1;
	aword.high.bit0 := aword.low.bit0;
	if aword = $0101 then bien else mal end;
	
  aword := $2010;
	if vword = $2010 then bien else mal end;
  
  //Access to bytes of dword
  vdword := $01020304;
  if vdword.low = 4 then bien else mal end;
  if vdword.high = 3 then bien else mal end;
//  if vdword.extra= 2 then bien else mal end;
//  if vdword.ultra = 1 then bien else mal end;
//  if vdword.lowword = $0304 then bien else mal end;
//  if vdword.highword = $0102 then bien else mal end;
// 
end.

