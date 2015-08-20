PicPas 0.2b
===========
Compilador en Pascal para microcontroladores PIC 

NOTA: Este compilador está aún en fase Alfa y tiene funcionalidades limitadas. 


PicPas es un compilador sencillo, escrito en Lazarus,  que genera código ejecutable, para los microcontroladores PIC de rango medio (la serie 16F).

Por el momento solo soporta los tipos byte, word y boolean. 

No se requiere librerias ni programas adicionales para la compilación. PicPas genera el archivo *.hex directamente.

Incluye una sencilla IDE para facilitar el desarrollo de programas.

Las opciones de optimización de código no están muy desarrolladas pero el código obtenido es bastante compacto, como el que podría generar cualquier compilador similar.

Como ejemplo el siguiente código, es para  parpadear un led en el puerto B:

```
program aaa;
var
  INDF  : BYTE @00;
  STATUS: BYTE absolute 03;
  FSR   : BYTE absolute 04;
  PORTA : BYTE absolute 05;
  TRISA : BYTE absolute $85;
  PORTB : BYTE absolute 06;
  TRISB : BYTE absolute 134;
  a,b: byte;
  pin: boolean absolute PORTB.4;
begin
  STATUS := 32;
  PORTB := 0;   //pone como salida
  STATUS := 0;
  PORTB := 0;   //apaga salidas
  //parpadeo
  delay_ms(1000);
  while true do begin
    delay_ms(300);
    pin := true;
    delay_ms(300);
    PIN := false;
  end;
end;

```

##Código Fuente

El código fuente se encuentra en la carpeta /Source.

Para compilar se debe disponer de las librerías indicadas en las propiedades del proyecto. Estas son:

* SynFacilUtils
* MisUtils
* ConfigFrame 
* PicUtils 
* Xpres 

Todas ellas deberían estar disponibles. Verificar las versiones requeridas.