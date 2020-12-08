ogEditGraf 2.5
==============

Librería en Lazarus, para la creación de editores simples de objetos gráficos.

![SynFacilCompletion](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2018/04/Sin-título-12.png "Título de la imagen")

Esta librería permite implementar fácilmente un editor de objetos gráficos en dos dimensiones. Los objetos gráficos se crean a partir de una clase base, que incluye las funciones básicas para poder ser manipulados por un editor, con opciones de seleccionar, mover, y redimensionar los objetos. 

Se compone de las unidades:

* ogMotGraf2d.pas -> Es el motor gráfico, en donde se encuentran las rutinas de dibujo. Usa métodos comunes del lienzo (Canvas), pero puede ser cambiado para usar alguna otra librería gráfica.
* ogDefObjGraf.pas -> Es donde se define la clase TObjGraf, que es la clase que se usa para crear a todos los objetos gráficos de nuestra aplicación. También se definen algunos objetos accesorios.
* ogEditionMot.pas -> Es el motor de edición de objetos gráficos. Esta diseñado para trabajar con los objetos TObjGraf o descendientes. Incluye las rutinas para seleccionar, mover y redimensionar objetos con el ratón.
* ogControls.pas -> Unidad que define objetos gráficos que funcionan al estilo de los controles de una GUI típica. También define a la clase TObjGrafCtrls, descendiente de TObjGraf que puede incluir controles.

Para implementar un sencillo editor de objetos gráficos, se puede incluir el siguiente código en el formulario principal:

```
unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  Classes, Forms, Controls, Graphics, ExtCtrls, ogEditionMot, ogDefObjGraf;

type
  //define el tipo de objeto a dibujar
  TMiObjeto = class(TObjGraf)
    procedure Draw; override;
  end;

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;   //donde se dibujará
    procedure FormCreate(Sender: TObject);
  private
    motEdi: TEditionMot;  //motor de edición
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TMiObjeto.Draw();
begin
  v2d.SetPen(psSolid, 1, clBlack);
  v2d.RectangR(x, y, x+width, y+height);
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var og: TMiObjeto;
begin
  //Crea motor de edición
  motEdi := TEditionMot.Create(PaintBox1);
  //Agrega objeto
  og := TMiObjeto.Create(motEdi.v2d);
  motEdi.AddGraphObject(og);
end;

end.
```

Este ejemplo mostrará un objeto rectangular en pantalla, con posibilidad de desplazarlo y dimensionarlo.

Las rutinas de dibujo, se dirigen siempre, a un control PaintBox, que debe ser indicado al momento de crear el motor de edición.

Este sencillo ejemplo solo requiere incluir un control TPaintBox en el formulario principal. Sin embargo, para modularizar mejor la aplicación, se sugiere usar una unidad especial para definir los objetos gráficos de nuestra aplicación, y un frame para incluir el PaintBox y las rutinas de trabajo del motor de edición.

== Arquitectura de los proyectos ===

Las unidades de la librería siguen una organización particular, que determina también la arquitectura de la aplicación.


Para más información, revisar los ejemplos.
