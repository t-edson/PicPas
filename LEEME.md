PicPas
======
Compilador multiplataforma en Pascal para microcontroladores PIC de la serie 16F. 

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/04/PicPas.png "Título de la imagen")

PicPas es un compilador sencillo, escrito en Lazarus, que genera código ejecutable, para los microcontroladores PIC de rango medio (la serie 16F).

No se requiere librerias ni programas adicionales para la compilación. PicPas genera el archivo *.hex directamente.

PicPas trabaja con una versión simplificada de Pascal, que ha sido adaptada para trabajar con dispositivos de recursos limitados. 

Por el momento solo soporta los tipos bit, byte, char boolean, word y dword (soporte limitado). 

Incluye una IDE completa, para facilitar el desarrollo de programas.

El compilador incluye opciones de optimización de código, de modo que genera un código bastante compacto, como el que podría generar cualquier compilador comercial.

Como ejemplo el siguiente código, es para hacer parpadear un led en el puerto B:

```
{Sample program to blink a Led on PORTB.7}
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC16F84A}
program BlinkLed;
var
  PORTB : BYTE absolute $06;
  TRISB : BYTE absolute $86;
  pin: bit absolute PORTB.7;
begin                          
  TRISB := 0;   //all outputs
  while true do
    delay_ms(1000);
    pin := not pin;
  end;
end.```
```

## Instalación

PicPas no necesita instalación, y no tiene mayores dependencias que las propias del sistema operativo, donde se ejecuta.

Para ejecutarlo, basta con descargar la carpeta de GitHub. Ahi se incluye una versión compilada para Windows-32bits (PicPas-win32.exe) y Ubuntu (PicPas-linux).

Si se requiere versiones para otra plataforma, se necesitará compilar  el proyecto para esa plataforma en particular.

Al iniciar, puede que PicPas, genere mensajes de advertencia, si no encuentra las carpetas requeridas.

## Código Fuente

El código fuente se encuentra en la carpeta /Source.

Para compilar PicPas se debe disponer de las librerías indicadas en las propiedades del proyecto. Estas son:

* SynFacilUtils
* MisUtils
* MiConfig
* PicUtils 
* Xpres 

Todas ellas deberían estar disponibles en la Web. Verificar las versiones requeridas.

PicPas ha sido compilado, usando la versión 1.6.2 de Lazarus. Se ha probado en Windows y Ubuntu.

Para más información sobre el compilador, leer la documentación técnica.

## Desarrollo

PicPas es un proyecto de software libre (licencia GPL), y está abierto al aporte de cualquiera. 

Hay aún mucho por desarrollar y documentar, así que cualquier aporte al proyecto, será bienvenido.