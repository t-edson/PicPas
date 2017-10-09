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
    procedure DibState(const xc, yc: Single; const pin: TPICpin);
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

procedure TPicObject.DibState(const xc, yc: Single; const pin: TPICpin);
{Dibuja un indicador del estado lógico del PIN}
begin
  if pin.typ = pptPort then begin
    //Ve estado
    if pic.ram[pin.add].value and (1<<pin.bit) = 0 then begin
      //Estado lógico bajo
    end else begin
      //Estado lógico alto
      v2d.Barra(xc-5, yc-5, xc+5, yc+5, clRed);
    end;
  end;
end;
procedure TPicObject.DibCuerpo;
var
  ypin, lblWidth: Single;
  i, nPinsSide: Integer;
  lbl: String;
begin
  //Calcula altura
  nPinsSide := nPinsDiag div 2;
  height := nPinsSide * SEP_PIN;
  //Dibuja borde
  v2d.FijaLapiz(psSolid, 1, clBlack);
  v2d.RectangR(x, y, x+Width, y+Height);
  //Dibuja pines de la izquierda
  ypin := y+SEP_PIN/2;   //posición inicial
  xpin:=x-LON_PIN;
  for i:=1 to nPinsDiag div 2 do begin
    //Pin
    v2d.Linea(xpin, ypin, xpin+LON_PIN, ypin);
    //Etiqueta
    lbl := pic.pines[i].GetLabel;
    v2d.Texto(x+2, ypin-8, lbl);
    //Estado de pin
    DibState(xpin, ypin,  pic.pines[i]);
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
  //Dibuja pines de la derecha
  ypin := y+SEP_PIN/2;   //posición inicial
  xpin:=x+width;
  for i:=1 to nPinsDiag div 2 do begin
    //Pin
    v2d.Linea(xpin, ypin, xpin+LON_PIN, ypin);
    //Etiqueta
    lbl := pic.pines[nPinsDiag-i+1].GetLabel;
    lblWidth := v2d.TextWidth(lbl);
    v2d.Texto(xpin - lblWidth - 2, ypin-8, lbl);
    //Estado de pin
    DibState(xpin+LON_PIN, ypin,  pic.pines[nPinsDiag-i+1]);
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
end;

{ TPicObject }
procedure TPicObject.Dibujar;
var
  ancho: Single;
begin
  if pic= nil then begin
    //Cuando no se ha iniciado el PIC
    v2d.FijaLapiz(psSolid, 1, clBlack);
    v2d.FijaRelleno(clGray);
    v2d.RectangR(x, y, x+Width, y+Height);
  end else begin
    //Caso normal
    ancho := v2d.TextWidth(pic.Model);
    v2d.SetText(True, False, False);
    v2d.Texto(x + width/2 - ancho/2 , y - 18, pic.Model);
    v2d.SetText(False, False, False);
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
  Width := 140;
  Height := 180;
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

