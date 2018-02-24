{
* (C) AguHDz 27-JUN-2017
* Last update: 01-JUL-2017
*
* Compiler PicPas v.0.7.1 (https://github.com/t-edson/PicPas)
*
* SERIAL COMMUNICATION RS232 (UART) THROUGH SOFTWARE
* =================================================================================================== ====================================================================================================
*
Sending and receiving ASCII characters through RS232 serial port creating a
* UART through software. Any pin of the I / O ports of the microcontroller
* is valid to be configured as a Transmission (TX) or Reception (RX) line
* data
*
* This application example uses the RS-232 protocol in its configuration more
* common: 8 data bits, 1 stop bit, no parity or flow control
* (only 2 communication threads), at a speed of 1200 bits per second.
*
* To modify the transmission speed or adapt it to the speed of the
* microcontroller alone it would be necessary to modify the delay time
* with the constants CYCLES_DELAY_1 and CYCLES_DELAY_2 in the procedure
* MEDIOBITDELAY.
*
* The five procedures necessary to create this software UART occupy
* less than 80 bytes of program memory.
*
}
 
{$PROCESSOR PIC16F84A}
{$FREQUENCY 8Mhz}
{$MODE PICPAS}
 
uses PIC16F84A;
 
const
  DataBitCount = 8;        // 8 bits of data, without parity or flow control.

  LF           = Chr(10);  // LF / NL (Line Feed / New Line) - Line Jump.
  CR           = Chr(13);  // CR (Carriage Return) - Return of Car.

  HIGH_LEVEL   = 1;        // High level (one logical)
  LOW_LEVEL    = 0;        // Low level (logical zero)
 
var
  UART_RX : bit absolute PORTB_RB7;
  UART_TX : bit absolute PORTB_RB6;

// ------------------------------------------------ -----------------
// Procedure MediumBitDelay
// Baudrate = 1200 bits per second (BPS)
// Delay = 0.000417 seconds (1e6 / Baudrate / 2).
// ------------------------------------------------ -----------------

procedure MediumBitDelay;
const
// -----------------------------------------------------------------
// Clock frequency = 20 MHz    
// Actual delay = 0.000417 seconds = 2085 cycles
// Error = 0 %
//   CYCLES_DELAY_1 = $9F;
//   CYCLES_DELAY_2 = $02;
// -----------------------------------------------------------------
// Clock frequency = 12 MHz    
// Actual delay = 0.000417 seconds = 1251 cycles
// Error = 0 %
//   CYCLES_DELAY_1 = $F8;
//   CYCLES_DELAY_2 = $01;
// -----------------------------------------------------------------
// Clock frequency = 10 MHz    
// Actual delay = 0.0004172 seconds = 1043 cycles
// Error = -0.0479616306954 %
//   CYCLES_DELAY_1 = $CF;
//   CYCLES_DELAY_2 = $01;
// -----------------------------------------------------------------
// Clock frequency = 8 MHz    
// Actual delay = 0.000417 seconds = 834 cycles
// Error = 0 %
   CYCLES_DELAY_1 = $A5;
   CYCLES_DELAY_2 = $01;
// -----------------------------------------------------------------
// Delay Code Generator: http://www.golovchenko.org/cgi-bin/delay
// -----------------------------------------------------------------
var
  d1, d2 : byte;
begin
  ASM  
              ;2078 cycles -> 20 MHz
              ;1243 cycles -> 12 MHz
              ;1038 cycles -> 10 MHz
              ; 828 cycles ->  8 MHz
              movlw        CYCLES_DELAY_1
              movwf        d1
              movlw        CYCLES_DELAY_2
              movwf        d2
  Delay_0:              
              decfsz       d1, f
              goto         $+2
              decfsz       d2, f
              goto         Delay_0
                         
              ;2 cycles    
              goto         $+1               ; -> For 8, 12 and 20 MHz
              ;2 cycles    
              ;goto         $+1               ; -> For 12 MHz
              ;1 cycle
              ;nop                            ; -> For 10 and 20 Mhz
              ;4 cycles (call & return)
  END
end;
 
  
// -----------------------------------------------------------------
// Procedure BITDELAY
// Delay = 0.000833 seconds (1e6/Baudrate).
// -----------------------------------------------------------------
procedure BitDelay;
begin
  MediumBitDelay;  // 0.000417 seconds
  MediumBitDelay;  // 0.000417 seconds
                  // 0.000001 seconds cycles call & return.
           // TOTAL: 0.000835 seconds (Error < 0,2%)
end;

// ------------------------------------------------ -----------------
// Procedure UARTSOFT_INIT
// Initialize the serial communication pins.
// ------------------------------------------------ -----------------

procedure UARTSoft_Init;
begin
  SetAsOutput(UART_TX);    // Departure.
  SetAsInput(UART_RX);     // Entry.
  UART_Tx := HIGH_LEVEL;   // Set the TX line to 1.
end;
 
// ------------------------------------------------ -----------------
// Procedure UARTSOFT_SENTCHAR
// Send a character sent by the serial port (UART).
// ------------------------------------------------ -----------------

procedure UARTSoft_SendChar(dato : char);
var
  contador, dataValue : byte;
begin
  dataValue := Ord(dato);            // Conversion of input character to variable type byte.
  contador  := 0;                    // Initialize data bit counter.
  UART_TX   := LOW_LEVEL;            // The transmission begins.

  BitDelay;                          // Time in low logic level of the transmission line (TX).
 
  repeat                             // Send the 8 bits of data.
    UART_TX   := dataValue.0;        // The transmission line takes the status of the corresponding data bit.
    BitDelay;                        // Wait with data bit status in the transmission line (TX).
    dataValue := dataValue>>1;       // Move the data byte to the right to send the bit in the next row.
    Inc(contador);                   // Increase data bit counter.

  until (contador = DataBitCount);   // It ends when the 8 bits of data have been transmitted.
 
  UART_TX  := HIGH_LEVEL;            // Send the Stop bit.
  BitDelay;                          // Wait with stop bit status in transmission line (TX).

end;
 
// ------------------------------------------------ -----------------
// Procedure UARTSOFT_GETCHAR
// Wait and read a character sent by the serial port (UART).
// ------------------------------------------------ -----------------

procedure UARTSoft_GetChar : char;
var
  contador, dataValue : byte;
begin
  contador  := 0;                    // Initialize data bit counter.
  dataValue := 0;                    // Initializes to zero the variable that will contain the received byte.
  repeat until(UART_RX = LOW_LEVEL); // Wait until the transmission starts.
  BitDelay;                          // Wait for the transmission start bit time.
  MediumBitDelay;                     // Wait 1/2 transmission time to read at a central point of the pulse.

 
  repeat                             // Receive the 8 bits of data.

    dataValue   := dataValue>>1;     // Shifts the partially received data to the right before adding a new bit.
    dataValue.7 := UART_RX;          // Add bit of data received.
    BitDelay;                        // Timeout before detecting status of the next data bit.

    Inc(contador);                   // Increase data bit counter.
  until (contador = DataBitCount);   // It ends when the 8 bits of data have been received.
 
                  // Check correct reception by means of a Stop bit.
                  // Here the detection of the parity bits could be added.

    if (UART_RX = HIGH_LEVEL) then   // Stop bit must be a logical one.
        MediumBitDelay;               // Final wait to complete the time of the complete bit frame.

        exit(Chr(DataValue));        // Returns the data read.

    else                             // An error has occurred!
        MediumBitDelay;               // Final wait to complete the time of the complete bit frame.
        exit(Chr(0));                // If it detects error, it returns the value zero.

    end;  
end;
 
 
// *****************************************************************
// MAIN PROGRAM  *********************************************
// *****************************************************************
begin
  UARTSoft_Init;             // Initializes TX and RX communication ports.
  
 
  UARTSoft_SendChar('H');    // Message HELLO WORLD
  UARTSoft_SendChar('E');
  UARTSoft_SendChar('L');
  UARTSoft_SendChar('L');
  UARTSoft_SendChar('O');
  UARTSoft_SendChar(' ');
  UARTSoft_SendChar('W');
  UARTSoft_SendChar('O');
  UARTSoft_SendChar('R');
  UARTSoft_SendChar('L');
  UARTSoft_SendChar('D');
  UARTSoft_SendChar(LF);     // Line Jump
  UARTSoft_SendChar(CR);     // Carriage return
 
  repeat
    UARTSoft_SendChar(UARTSoft_GetChar);  // Write in the terminal each character received (ECHO)
  until false;
 
end.