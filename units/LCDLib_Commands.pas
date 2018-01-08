{
*
*  (C) AguHDz 06-JUL-2017
*  Ultima Actualizacion: 01-SEP-2017
*
*  Compilador PicPas v.0.7.6 (https://github.com/t-edson/PicPas)
*
*  LIBRERIA DE COMANDOS PARA DISPLAY LCD COMPATIBLE CON ESTANDAR HITACHI HD44780
*
}
unit LCDLib_Commands;

interface

const
  // LCD commands
  // Indentation means: values have to be summed up with the non indented value.
  // e.g. use "LCDCommand(LCD_DISPLAY_ON_OFF_AND_CURSOR + LCD_DISPLAY_ON)" to switch the
  // display on, with no underline cursor and cursor not blinking.
 
    LCD_CLEAR_DISPLAY                = $01;

    LCD_DISPLAY_AND_CURSOR_HOME      = $02;

    LCD_CHARACTER_ENTRY_MODE         = $04;
           LCD_INCREMENT             = $02;
           LCD_DECREMENT             = $00;
           LCD_DISPLAY_SHIFT_ON      = $01;
           LCD_DISPLAY_SHIFT_OFF     = $00;

    LCD_DISPLAY_ON_OFF_AND_CURSOR    = $08;
           LCD_DISPLAY_ON            = $04;
           LCD_DISPLAY_OFF           = $00;
           LCD_CURSOR_UNDERLINE_ON   = $02;
           LCD_CURSOR_UNDERLINE_OFF  = $00;
           LCD_CURSOR_BLINK_ON       = $01;
           LCD_CURSOR_BLINK_OFF      = $00;

    LCD_DISPLAY_AND_CURSOR_SHIFT     = $10;
           LCD_DISPLAY_SHIFT         = $08;
           LCD_CURSOR_MOVE           = $00;
           LCD_RIGHT                 = $04;
           LCD_LEFT                  = $00;

    LCD_FUNCTION_SET                 = $20;
           LCD_8BIT_INTERFACE        = $10;
           LCD_4BIT_INTERFACE        = $00;
           LCD_2LINES                = $08;
           LCD_1LINE                 = $00;
           LCD_F_FONT_5_10           = $02;
           LCD_F_FONT_5_8            = $00;

    LCD_SET_DISPLAY_ADDRESS          = $80;
           LCD_ROW_0                 = $00;
           LCD_ROW_1                 = $40;
           LCD_ROW_2                 = $14;
           LCD_ROW_3                 = $54;

    LCD_SET_CGRAM_ADDRESS            = $40;
 
implementation

end.
