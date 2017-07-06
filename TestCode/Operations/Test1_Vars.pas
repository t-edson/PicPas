{Test code for validation of variable declarations.}
{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
uses PIC16F84A;
var  //test pin
  pinLed: bit absolute PORTB.0;
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
	
  //Word doesn't overlap right
//  aword := $2010;
//	if vword = $2010 then bien else mal end;

end.

