{
*  (C) AguHDz 20-JUN-2017
*  Ultima Actualizacion: 25-JUN-2017
*
*  Compilador PicPas v.0.7.0 (https://github.com/t-edson/PicPas)
*
*  INTERRUPCIONES HARDWARE
*  Aunque Picpas aun no implemente un tipo de procedimiento especifico para
*  tratar las interrupciones del microcontrolador, usando codigo ensamblador
*  es sencillo implementar todo tipo de interrupciones.
*
*  El este ejemplo de aplicacion, la interrupcion la produce el desbordamiento
*  del timer TMR0 del microcontrolador. Para ello, configuramos los registros del
*  microcontrolador para producir una interrupcion cada aproximadamente 0,25 segundos,
*  y usando un contador de interrupciones hacemos que los LED_1 y LED_2 cambien de
*  estado cada segundo, independientemente de lo que este haciendo en ese momento
*  el microcontrolador.
*
*  Para comprobar que se conservan los valores de los registros previos a la interrupcion
*  y que por lo tanto no se produce deterioros en el programa principal que ejecuta el
*  microcontrolador, se ha programado un LOOP que simplemente replica los valores de LED_1
*  y LED_2 usando procedimientos que incluye ciclos FOR simplemente para verificar que la
*  interrupcion no corrompe los registros en uso, y una vez finalizo el codigo de la interrupcion
*  (procedimiento _ISR_) el programa principal continua por donde estaba antes de producirse la
*  interrupcion.
}
 
{$PROCESSOR PIC16F84A}
{$FREQUENCY 1Mhz}
{$MODE PICPAS}
 
program Interrupciones;
 
uses PIC16F84A;
 
const
  IniTMR0 = 12;   // Valor inicial y de recarga de TRM0. Tiempo desbordamento =
                  // (256-IniTMR0)/(XTAL/4/Divisor Prescaler) =
                  // (256-12)/(1e6/4/256) = 0,249856 segundos.
 
var
  LED_1          : bit absolute PORTB_RB0;  // LED_1 conectado al pin RB0.
  LED_2          : bit absolute PORTB_RB1;  // LED_2 conectado al pin RB1.
  ContadorInTMR0 : byte;                    // Contador de interrupciones de TMR0.
  LED_1_replica  : bit absolute PORTB_RB7;  // LED_1_replica conectado al pin RB7 (hacer algo entre interrupciones)
  LED_2_replica  : bit absolute PORTB_RB6;  // LED_2_replica conectado al pin RB6 (hacer algo entre interrupciones)
 
 
//***********************************************************************
//  PROCEDIMIENTO: _ISR_
//  I.S.R. : Interrupt Service Routine (Rutina de Servicio a la Interrupcion)
//  Cualquier interrupcion producida en el uC 16F84A salta a la
//  direccion $0004, a partir de la que código de programa debe decidir
//  que hacer.
//***********************************************************************
procedure _ISR_;
var
  Reg_W, Reg_STATUS : byte;  // Para guardar valores previos a interrupcion y restablecerlos antes de salir.
begin
ASM
;------------------------------------------------------------------------------------
; Posiciona la primera instruccion de interrupcion en la direccion $0004.
;------------------------------------------------------------------------------------
  org $0004
;------------------------------------------------------------------------------------
; Inicio de instrucciones que se ejecutan cuando se produce una interrupcion.
;------------------------------------------------------------------------------------
  MOVWF Reg_W        ; Guarda en registro W en Reg_W.
  SWAPF STATUS,w     ; Invierte los nibbles del registro de estado (STATUS) y lo guarda en W.
                     ; Se usa SWAPF en lugar de MOVF porque no afecta al flag Z del registro STATUS.
                     ; Es la tecnica recomendada por Microchip para salvaguardar los valores previos
                     ; a la interrupcion.
  MOVWF Reg_STATUS   ; Guarda el contenido de W en Reg_STATUS.
;------------------------------------------------------------------------------------
END
 
  // INTERRUPCION TIMER TMR0 --------------------------------------------------------
  if (INTCON_T0IF = 1) then         // Comprueba si la interrupcion la ha producido el desbordamiento del TMR0.
    TMR0             := IniTMR0;    // Valor inicial de TMR0 para nueva cuenta.
    INTCON_T0IF      := 0;          // Restablece el valor del flag de deteccion de interrupcion TMR0.
    Inc(ContadorInTMR0);            // Incrementa contador de interrupciones
    if (ContadorInTMR0 = 4) then    // 4*(256-12)/(1e6/4/256) = 0,999424 segundos.      
      LED_1          := NOT LED_1;  // LED_1 invierte su valor.
      LED_2          := NOT LED_2;  // LED_2 invierte su valor.
      ContadorInTMR0 := 0;          // Se inicializa el contador de interrupciones.
    end;
  end;
  // --------------------------------------------------------------------------------
 
ASM
;------------------------------------------------------------------------------------
; Fin de interrupcion y reposicion de los valores previos de W y STATUS.
;------------------------------------------------------------------------------------
  SWAPF Reg_STATUS,w ; Invertimos los nibbles para dejar en su posicion correcta el registro STATUS.
  MOVWF STATUS       ; Restauramos el valor de STATUS previo a la interrupcion.
  SWAPF Reg_W,f      ; Invertimos los nibbles de Reg_W y lo guardamos en la misma posicion de memoria.
  SWAPF Reg_W,w      ; Volvemos a invertor los nibbles de Reg_W y lo guardamos en el registro W, con
                     ; lo que queda con el valor que tenia antes de la interrupcion.
                     ; Se usa SWAPF en lugar de MOVF porque no afecta al flag Z del registro STATUS.
                     ; Es la tecnica recomendada por Microchip para salvaguardar los valores previos
                     ; a la interrupcion.
  RETFIE             ; Retorna de la interrupcion.
;------------------------------------------------------------------------------------
END
end;
 
//***********************************************************************
// Procedimientos de prueba para hacer algo entre interrupciones
// y comprobar que las interrupciones no corrompen el funcionamiento del programa principal.
//***********************************************************************
Procedure EncenderReplicaLED_1;
var i : byte;
begin
  for i:=0 to 250 do
    LED_1_replica := 1;
  end;
end;
 
Procedure ApagarReplicaLED_1;
var i : byte;
begin
  for i:=0 to 250 do
    LED_1_replica := 0;
  end;
end;
 
Procedure EncenderReplicaLED_2;
var i : byte;
begin
  for i:=0 to 250 do
    LED_2_replica := 1;
  end;
end;  
 
Procedure ApagarReplicaLED_2;
var i : byte;
begin
  for i:=0 to 250 do
    LED_2_replica := 0;
  end;
end;  
 
 
//***********************************************************************
// PROGRAMA PRINCIPAL ***************************************************
//***********************************************************************
begin
  // Configura puertos con entradas o salidas ---------------------------------------
  SetAsOutput(LED_1);         // Puerto como salida para encender/apagar el LED_1.
  SetAsOutput(LED_2);         // Puerto como salida para encender/apagar el LED_2.
  SetAsOutput(LED_1_replica); // Puerto como salida para hacer algo entre interrupciones.
  SetAsOutput(LED_2_replica); // Puerto como salida para hacer algo entre interrupciones.
  // --------------------------------------------------------------------------------
 
  // Configuracion de Timer TMR0 ----------------------------------------------------
  OPTION_REG_T0CS := 0;       // Origen de pulsos de incremento de TMR0 es cada ciclo de instruccion (Xtal/4).
  OPTION_REG_T0SE := 0;       // Incrementea contador de TMR0 en los pulsos de bajada.
  OPTION_REG_PSA  := 0;       // El divisor de frecuenta usado es el de TMR0
  OPTION_REG_PS2  := 1;       // Configura divisor (Preescaler) de TMR0 con valor 111 = 1:256.
  OPTION_REG_PS1  := 1;
  OPTION_REG_PS0  := 1;
  // --------------------------------------------------------------------------------
 
  // Inicializa variables -----------------------------------------------------------
  ContadorInTMR0  := 0;       // Inicializa contador de interrupciones producidad por TMR0.
  LED_1           := 1;       // Inicializa el LED_1 encendido.
  LED_2           := 0;       // Inicializa el LED_2 apagado.
  TMR0            := IniTMR0; // Carga valor de inicio de cuenta en TRM0.
  // --------------------------------------------------------------------------------
 
  // Habilita interrupciones --------------------------------------------------------
  INTCON_GIE      := 1;       // Habilita interruptiones de manera general.
  INTCON_T0IE     := 1;       // Habilita interrupciones por desbordamiento del Timer TMR0.
  // --------------------------------------------------------------------------------
 
  // LOOP de programa principal -----------------------------------------------------
  repeat  
    // Hacer algo. Por ejemplo, encender otros dos leds llamando a procedimiento para
    // comprobar que se conservan los valores de los registros previos a la interrupcion.
    if (LED_1 = 1) then
      EncenderReplicaLED_1;
      ApagarReplicaLED_2;
    else
      ApagarReplicaLED_1;
      EncenderReplicaLED_2;
    end;    
  until false;
  // --------------------------------------------------------------------------------
 
  // --------------------------------------------------------------------------------
  // Llamada a la funcion de interrupcion para que PicPas la compile.
  // En realidad nunca se va a ejecutar por estar despues del LOOP
  // infinito repeat.. until anterior.
  // Pero eso el compilador no lo sabe y "le engañamos" para que compile
  // la funcion que interrupciones.
  _ISR_;
  // --------------------------------------------------------------------------------
end.
