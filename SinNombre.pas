{$PROCESSOR PIC16F84}
{$FREQUENCY 8Mhz}
uses LCDlib, Lcdconst;
begin
  LCDInit;
  while true do
  begin
    LCDWriteChar('H'); 
    LCDWriteChar('e'); 
    LCDWriteChar('l'); 
    LCDWriteChar('l'); 
    LCDWriteChar('o'); 
    LCDWriteChar(' '); 
    LCDWriteChar('W'); 
    LCDWriteChar('o'); 
    LCDWriteChar('r'); 
    LCDWriteChar('l'); 
    LCDWriteChar('d'); 
 
    LcdCommand(LCD_SET_DISPLAY_ADDRESS + LCD_ROW_1);
 
    LCDWriteChar('P'); 
    LCDWriteChar('i'); 
    LCDWriteChar('c'); 
    LCDWriteChar('P'); 
    LCDWriteChar('a'); 
    LCDWriteChar('s'); 
    LCDWriteChar(' '); 
    LCDWriteChar('0'); 
    LCDWriteChar('.'); 
    LCDWriteChar('6'); 
    LCDWriteChar('.'); 
    LCDWriteChar('3'); 
    LCDWriteChar(' '); 
   
    Counter := 3;
    repeat
      LCDWriteChar('>'); 
      delay_ms(1000);
      dec(Counter);
    until Counter = 0;
 
    LcdCommand(LCD_CLEAR_DISPLAY);
  end;
end.

