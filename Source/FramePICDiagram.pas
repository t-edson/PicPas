unit FramePICDiagram;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Graphics,
  ogMotEdicion, ogMotGraf2D, ogDefObjGraf, Pic16Utils;

type
  //define el tipo de objeto a dibujar

  { TPicObject }

  TPicObject = class(TObjGraf)
  private
    pic: TPIC16;   //referencia al PIC
    xpin: Single;  //Posición X del Pin
    nPinsDiag: Integer;  //Número de pines a dibujar
  public
    procedure DibCuerpo;
    procedure Dibujar; override;
    constructor Create(mGraf: TMotGraf); override;
  end;

  { TfraPICDiagram }
  TfraPICDiagram = class(TFrame)
    PaintBox1: TPaintBox;
  private
    ogPic: TPicObject;
    motEdi: TModEdicion;
  public
    pic: TPIC16;
    procedure Refrescar;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
  SEP_PIN = 20;   //Separación entre pines
  LON_PIN = 15;   //Longitud de pin

procedure TPicObject.DibCuerpo;
var
  ypic: Single;
  i: Integer;
begin
  //Calcula altura
  height := (nPinsDiag div 2) * SEP_PIN;
  //Dibuja borde
  v2d.FijaLapiz(psSolid, 1, clBlack);
  v2d.RectangR(x, y, x+Width, y+Height);
  //Dibuja pines de la izquierda
  ypic := y+SEP_PIN/2;   //posición inicial
  xpin:=x-LON_PIN;
  for i:=1 to nPinsDiag div 2 do begin
    v2d.Linea(xpin, ypic, xpin+LON_PIN, ypic);
    ypic := ypic + SEP_PIN;
  end;
  //Dibuja pines de la derecha
  ypic := y+SEP_PIN/2;   //posición inicial
  xpin:=x+width;
  for i:=1 to nPinsDiag div 2 do begin
    v2d.Linea(xpin, ypic, xpin+LON_PIN, ypic);
    ypic := ypic + SEP_PIN;
  end;
end;

{ TPicObject }
procedure TPicObject.Dibujar;
begin
  if pic= nil then begin
    //Cuando no se ha iniciado el PIC
    v2d.FijaLapiz(psSolid, 1, clBlack);
    v2d.RectangR(x, y, x+Width, y+Height);
  end else begin
    //Caso normal
    v2d.Texto(x,y-18,pic.Model);
    if pic.Npins <= 8 then begin
      nPinsDiag := 8;
      DibCuerpo;
    end else if pic.Npins <=18 then begin
      nPinsDiag := 18;
      DibCuerpo;
    end else if pic.Npins <=28 then begin
      nPinsDiag := 28;
      DibCuerpo;
    end else if pic.Npins <=40 then begin
      nPinsDiag := 40;
      DibCuerpo;
    end else begin
      //Caso de muchos pines
      v2d.FijaLapiz(psSolid, 1, clBlack);
      v2d.RectangR(x, y, x+Width, y+Height);
    end;
  end;
  inherited;
end;

constructor TPicObject.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  Width := 100;
  Height := 100;
end;

{ TfraPICDiagram }
procedure TfraPICDiagram.Refrescar;
begin
  motEdi.Refrescar;
  ogPic.pic := pic;  //Actualiza referencia
end;
constructor TfraPICDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //crea motor de edición
  motEdi := TModEdicion.Create(PaintBox1);
  //agrega objeto
  ogPic := TPicObject.Create(motEdi.v2d);
  ogPic.Highlight := false;
  ogPic.pic := pic;
  motEdi.AgregarObjGrafico(ogPic);
end;

destructor TfraPICDiagram.Destroy;
begin
  motEdi.Destroy;
  inherited Destroy;
end;

end.

