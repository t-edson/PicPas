# PicUtils 1.6
==============

Librería en Lazarus, con utilidades para la programación de microcontroladores PIC.

## Descripción

Unidad con utilidades para la programación de microcontroladores PIC de rango medio con instrucciones de 14 bits. Incluye a la mayoría de la serie PIC16FXXXX.

Se define un objeto que representa a un PIC de esta serie, que está dimensionado para poder representar al dispositivo más complejo.

El objetivo de esta unidad es poder servir como base para la implementación de ensambladores, compiladores o simuladores.

## Funcionalidades

* Modela la arquitectura de hardware de un PIC.
* Permite codificar y decodificar las instrucciones del microcontrolador.
* Incluye rutinas para el reconocimiento de las instrucciones en ensamblador del PIC.
* Permite generar el archivo de salida HEX, para la grabación del PIC.

## Modo de trabajo

Se puede trabajar de diversas formas con la libreria, dependiendo de la función que se desee implementar. Pero en general, casi siempre se requerirá primero crear un objeto de la clase TPIC16:

```
uses  ... , pic16utils;
var
    pic: TPIC16;
...
	pic := TPIC16.Create;
	
	//hacer algo
	
	pic.Destroy;

```

La mayoría de las funcionalidades de la librería se acceden mediante métodos del objeto TPIC16. Así si por ejemplo se desea limpiar la memoria flash, se debe llamar al método:

```
	pic.ClearMemFlash;
```

## Generación del archivo *.hex

Para obtener el archivo de salida, primero se deben introducir las instrucciones en la memoria flash del objeto TPIC16.

Una forma es usar los métodos codAsm(), que trabaja introduciendo las instrucciones de forma secuencial:

```
  pic.codAsm(CLRWDT);
  pic.codAsmK(MOVLW,$01);
  pic.codAsmF(MOVWF,$21);  
```

Luego se debe llamar al método: GenHex(): 

```
  pic.GenHex('salida.hex');
```
## Modelado del hardware

El objeto TPIC16, representa a un PIC, incluyendo su arquitectura interna. En la presente versión, solo se representa a la memoria flash y los registros básicos, como el contador de programa y el acumulador W.

La memoria RAM se modela como una tabla lineal de tipo:

```
  TPIC16Ram = array[0..PIC_MAX_RAM-1] of TPIC16RamCell;
```

Pero para facilitar el trabajo con las páginas, se usa el objeto TRAMBank que representa a una página de memoria. Este objeto incluye métodos para manejar la arquitectura especial que puede tener la memoria del PIC.

Para administrar eficiéntemente la memoria del PIC, se maneja el registro TPIC16RamCell, que guarda información detallada de cada posición de la RAM:
```
  TPIC16RamCell = record
    value  : byte;     
    used   : byte;     
    name   : string;   
    state  : TPIC16CellState;  //estado de la celda
  end;
```
La propiedad "used", es en realidad un mapa de bits, en donde se marcan los bits que estén ocupados, ya que en los PIC, es común manejar variables de 1 bit de longitud.

De la misma forma, la memoria flash se representa con una tabla lineal de tipo:

```
  TPIC16Flash = array[0..PIC_MAX_FLASH-1] of TPIC16FlashCell;
```

Pero para facilitar el manejo de las múltiples páginas que puede tener un dispositivo, se usa el objeto TFlashPage, que representa a una página de la memoria flash.

Para adaptar el objeto TPIC16, a un dispositivo en especial, se debe configurar los parámetros de hardware. Para facilitar esta tarea, se dispone de la unidad PIC16devices, que contiene la configuración para diferentes modelos de PIC.

## Codificando instrucciones

Sea para generar un acrhivo *.hex o para realizar una simulación (no implementado en la versión actual), se requerirá ingresar las instrucciones en la memoria flash del PIC. Para ello existen diversos métodos de acuerdo a la sintaxis de la instrucción.

Todas las instrucciones se han dividido en 5 categoría distintas de acuerdo a su sintaxis, estas son:

Sintaxis:  

* De la forma: NEMÓNICO f,d -> Se codifican con el método codAsmFD()
* De la forma: NEMÓNICO f   -> Se codifican con el método codAsmF()
* De la forma: NEMÓNICO f,b -> Se codifican con el método codAsmFB()
* De la forma: NEMÓNICO k   -> Se codifican con el método codAsmK()
* De la forma: NEMÓNICO a   -> Se codifican con el método codAsmA()
* De la forma: NEMÓNICO     -> Se codifican con el método codAsm()

Donde:

  f->dirección de un registro en RAM (0..127)
  
  d->destino (W o F)
  
  b->número de bit (0..7)
  
  a->dirección destino (0..$7FF)
  
  k->literal byte (0..255)
  
 
## Notas

Actualmente solo se incluyen las rutinas para el manejo de microcontroladores PIC de rango medio, con instrucciones de 14 bits.

