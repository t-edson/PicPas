{
*  (C) AguHDz 27-JUN-2017
*  Ultima Actualizacion: 01-JUL-2017
*
*  Compilador PicPas v.0.7.1 (https://github.com/t-edson/PicPas)
*
*  COMUNICACION SERIE RS232 (UART) MEDIANTE SOFTWARE
*  =================================================
*  Envio y recepcion de caracteres ASCII mediante puerto serie RS232 creando una
*  UART mediante software. Cualquier pin de los puertos I/O del microcontrolador
*  es valido para configurarse como linea de Transmision (TX) o Recepcion (RX) de
*  datos.
*
*  Este ejemplo de aplicacion utiliza el protocolo RS-232 en su configuracion mas
*  comun: 8 bits de datos, 1 bit de Stop, sin paridad ni control de flujo
*  (solo 2 hilos de comunicacion), a una velocidad de 1200 bits por segundo.
*
*  Para modificar la velocidad de transmision o adaptarla a la velocidad del
*  microcontrolador solo seria necesario modificar el tiempo de retardo
*  con las constantes CICLOS_DELAY_1 y CICLOS_DELAY_2 en el procedimiento
*  MEDIOBITDELAY.
*
*  Los cinco procedimientos necesarios para crear esta UART software ocupan
*  menos de 80 bytes de memoria de programa.
*
}
 
{$MODE PICPAS}
uses PIC16F84A;
{$FREQUENCY 8Mhz}
 
const
  DataBitCount = 8;        // 8 bits de datos, sin paridad ni control de flujo.
  LF           = Chr(10);  // LF/NL (Line Feed/New Line) - Salto de Linea.
  CR           = Chr(13);  // CR (Carriage Return) - Retorno de Carro.
  HIGH_LEVEL   = 1;        // Nivel alto (uno logico)
  LOW_LEVEL    = 0;        // Nivel bajo (cero logico)
 
var
  UART_RX : bit absolute PORTB_RB7;
  UART_TX : bit absolute PORTB_RB6;
 
// -----------------------------------------------------------------
// Procedure MEDIOBITDELAY
// Baudrate = 1200 bits per second (BPS)
// Delay = 0.000417 seconds (1e6/Baudrate/2).
// -----------------------------------------------------------------
procedure MedioBitDelay;
const
// -----------------------------------------------------------------
// Clock frequency = 20 MHz    
// Actual delay = 0.000417 seconds = 2085 cycles
// Error = 0 %
//   CICLOS_DELAY_1 = $9F;
//   CICLOS_DELAY_2 = $02;
// -----------------------------------------------------------------
// Clock frequency = 12 MHz    
// Actual delay = 0.000417 seconds = 1251 cycles
// Error = 0 %
//   CICLOS_DELAY_1 = $F8;
//   CICLOS_DELAY_2 = $01;
// -----------------------------------------------------------------
// Clock frequency = 10 MHz    
// Actual delay = 0.0004172 seconds = 1043 cycles
// Error = -0.0479616306954 %
//   CICLOS_DELAY_1 = $CF;
//   CICLOS_DELAY_2 = $01;
// -----------------------------------------------------------------
// Clock frequency = 8 MHz    
// Actual delay = 0.000417 seconds = 834 cycles
// Error = 0 %
   CICLOS_DELAY_1 = $A5;
   CICLOS_DELAY_2 = $01;
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
              movlw        CICLOS_DELAY_1
              movwf        d1
              movlw        CICLOS_DELAY_2
              movwf        d2
  Delay_0:              
              decfsz       d1, f
              goto         $+2
              decfsz       d2, f
              goto         Delay_0
                         
              ;2 cycles    
              goto         $+1               ; -> Para 8, 12 y 20 MHz
              ;2 cycles    
              ;goto         $+1               ; -> Para 12 MHz
              ;1 cycle
              ;nop                            ; -> Para 10 y 20 Mhz
              ;4 cycles (call & return)
  END
end;
 
 
// -----------------------------------------------------------------
// Procedure BITDELAY
// Delay = 0.000833 seconds (1e6/Baudrate).
// -----------------------------------------------------------------
procedure BitDelay;
begin
  MedioBitDelay;  // 0.000417 seconds
  MedioBitDelay;  // 0.000417 seconds
                  // 0.000001 seconds cycles call & return.
           // TOTAL: 0.000835 seconds (Error < 0,2%)
end;
 
 
// -----------------------------------------------------------------
// Procedure UARTSOFT_INIT
// Inicializa los pines de comunicacion serie.
// -----------------------------------------------------------------
procedure UARTSoft_Init;
begin
  SetAsOutput(UART_TX);    // Salida.
  SetAsInput(UART_RX);     // Entrada.
  UART_Tx := HIGH_LEVEL;   // Pone a 1 la linea TX.
end;
 
 
// -----------------------------------------------------------------
// Procedure UARTSOFT_SENTCHAR
// Envia un caracter enviado por el puerto serie (UART).
// -----------------------------------------------------------------
procedure UARTSoft_SendChar(dato : char);
var
  contador, dataValue : byte;
begin
  dataValue := Ord(dato);            // Conversion de caracter de entrada a variable tipo byte.
  contador  := 0;                    // Inicializa contador de bits de datos.
  UART_TX   := LOW_LEVEL;            // Comienza la transmision.
  BitDelay;                          // Tiempo en nivel logico bajo de la linea de transmision (TX).
 
  repeat                             // Envia los 8 bits de datos.
    UART_TX   := dataValue.0;        // La linea de transmision toma el estado del bit de datos correspondiente.
    BitDelay;                        // Espera con estado de bit de datos en el linea de transmision (TX).
    dataValue := dataValue>>1;       // Desplaza a la derecha el byte de datos para en siguiente vuelta enviar el bit.
    Inc(contador);                   // Incrementa contador de bits de datos.
  until (contador = DataBitCount);   // Acaba cuando se han transmitido los 8 bits de datos.
 
  UART_TX  := HIGH_LEVEL;            // Envia el bit de Stop.
  BitDelay;                          // Espera con estado de bits de Stop en linea de transmision (TX).
end;
 
 
// -----------------------------------------------------------------
// Procedure UARTSOFT_GETCHAR
// Espera y lee un caracter enviado por el puerto serie (UART).
// -----------------------------------------------------------------
procedure UARTSoft_GetChar : char;
var
  contador, dataValue : byte;
begin
  contador  := 0;                    // Inicializa contador de bits de datos.
  dataValue := 0;                    // Inicializa a cero la variable que va a contener el byte recibido.
  repeat until(UART_RX = LOW_LEVEL); // Espera hasta deteccion de inicio la transmision.
  BitDelay;                          // Espera el tiempo del bit de inicio de transmision.
  MedioBitDelay;                     // Espera 1/2 tiempo de transmision para hacer la lectura en un punto central del pulso.
 
  repeat                             // Recibe los 8 bits de datos.
    dataValue   := dataValue>>1;     // Desplaza a la derecha el dato parcialmente recibido antes de añadir un nuevo bit.
    dataValue.7 := UART_RX;          // Añade bit de datos recibido.
    BitDelay;                        // Tiempo de espera antes de detectar estado del siguiente bit de datos.
    Inc(contador);                   // Incrementa contador de bits de datos.
  until (contador = DataBitCount);   // Acaba cuando se han recibido los 8 bits de datos.
 
    // Comprueba correcta recepcion mediante bit de Stop.
    // Aquí se podría añadir en su caso la deteccion de los bits de paridad.
    if (UART_RX = HIGH_LEVEL) then   // Bit de Stop debe ser un uno logico.
        MedioBitDelay;               // Espera final para completar el tiempo de la trama de bits completa.
        exit(Chr(DataValue));        // Devuelve el dato leido.
    else                             // Ha ocurrido algun error !
        MedioBitDelay;               // Espera final para completar el tiempo de la trama de bits completa.
        exit(Chr(0));                // Si detecta error devuelve el valor cero.
    end;  
end;
 
 
// *****************************************************************
// PROGRAMA PRINCIPAL  *********************************************
// *****************************************************************
begin
  UARTSoft_Init;             // Inicializa puertos de comunicacion TX y RX.  
 
  UARTSoft_SendChar('H');    // Mensaje HOLA MUNDO
  UARTSoft_SendChar('O');
  UARTSoft_SendChar('L');
  UARTSoft_SendChar('A');
  UARTSoft_SendChar(' ');
  UARTSoft_SendChar('M');
  UARTSoft_SendChar('U');
  UARTSoft_SendChar('N');
  UARTSoft_SendChar('D');
  UARTSoft_SendChar('O');
  UARTSoft_SendChar(LF);     // Salto de Linea.
  UARTSoft_SendChar(CR);     // Retorno de Carro.
 
  repeat
    UARTSoft_SendChar(UARTSoft_GetChar);  // Escribe en el terminal cada caracter recibido (ECHO)
  until false;
 
end.
