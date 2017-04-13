PicPas
======
Compilador en Pascal para microcontroladores PIC. 

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/04/picpas-768x573.jpg "Título de la imagen")

NOTA: Este compilador está aún en fase Alfa y tiene funcionalidades limitadas. 

PicPas es un compilador sencillo, escrito en Lazarus, que genera código ejecutable, para los microcontroladores PIC de rango medio (la serie 16F).

Por el momento solo soporta los tipos bit, byte, boolean y word (soporte limitado). 

No se requiere librerias ni programas adicionales para la compilación. PicPas genera el archivo *.hex directamente.

Incluye una pequeña IDE para facilitar el desarrollo de programas.

Las opciones de optimización de código no están muy desarrolladas pero el código obtenido es bastante compacto, como el que podría generar cualquier compilador comercial.

Como ejemplo el siguiente código, es para hacer parpadear un led en el puerto B:

```
{Sample program to blink a Led on PORTB.4}
{$FREQUENCY 4 MHZ }
{$PROCESSOR PIC16F84}
program BlinkLed;
const
  HIGH = true;
  LOW = false;
var
  STATUS: BYTE absolute $03;
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  RP0 : boolean @STATUS.5;
  RP1 : boolean @STATUS.6;
  pin: boolean absolute PORTB.4;
begin                          
  RP0 := HIGH;
  TRISB := 0;   //all outputs
  RP0 := LOW;
  PORTB := 0;   //init
  delay_ms(1000);  //wait
  while true do begin
    delay_ms(300);
    pin := HIGH;
    delay_ms(300);
    pin := LOW;
  end;
end.
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

Para más información sobre el compilador, leer la documentación técnica.

## Desarrollo

PicPas es un proyecto de software libre (licencia GPL), y está abierto al aporte de cualquiera. 

Hay aún mucho por desarrollar y documentar, así que cualquier aporte al proyecto, será bienvenido.