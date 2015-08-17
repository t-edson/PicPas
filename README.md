PicPas 0.2b
===========
Compilador en Pascal para microcontroaldores PIC 

NOTA: Este compilador está aún en fase Alfa y tiene funcionalidades limitadas. 


PicPas es un compilador sencillo, escrito en Lazarus,  que genera código ejecutable, para los microcontroladores PIC de rango medio (la serie 16F).

Por el momento solo soporta los tipos byte, word y boolean. 

No se requiere librerias ni programas adicionales para la compilación. PicPas genera el archivo *.hex directamente.

Inclcuye una sencilla IDE para facilitar el desarrollo de programas.

Las opciones de optimización de código no están muy desarrolladas pero el código obtenido es bastante compacto, como el que podría generar cualquier compilador similar.

##Código Fuente

El código fuente se encuentra en la carpeta /Source.

Para compilar se debe disponer de las librerías indicadas en las propiedades del proyecto. Estas son:

* SynFacilUtils
* MisUtils
* ConfigFrame 
* PicUtils 
* Xpres 

Todas ellas estándeberían estar disponibles. Verificar las versiones requeridas.