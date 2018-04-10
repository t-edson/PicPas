unit FramePICDiagram;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Graphics, Menus,
  ActnList, ogMotEdicion, ogMotGraf2D, ogDefObjGraf, Pic16Utils,
  MisUtils;

type

  { TPicObject }
  //Define el objeto gráfico PIC
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

  //Define el objeto gráfico LogicState
  TOgLogicState = class(TObjGraf)
  private
    pic: TPIC16;   //referencia al PIC
    FState: boolean;
    ptos: array of TFPoint;
    procedure DibState(const xc, yc: Single; const pin: TPICpin);
  public
    //procedure SetState(Value: boolean);
    procedure Dibujar; override;
    constructor Create(mGraf: TMotGraf); override;
  end;

  { TfraPICDiagram }
  TfraPICDiagram = class(TFrame)
    acGenAddLogTog: TAction;
    acGenDelObject: TAction;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    procedure acGenAddLogTogExecute(Sender: TObject);
    procedure acGenDelObjectExecute(Sender: TObject);
  private
    Fpic: TPIC16;
    ogPic: TPicObject;
    motEdi: TModEdicion;
    procedure fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
    procedure motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetPic(AValue: TPIC16);
  public
    property pic: TPIC16 read Fpic write SetPic;
    procedure Refrescar;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
  SEP_PIN = 20;   //Separación entre pines
  LON_PIN = 15;   //Longitud de pin

{ TPicObject }
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
  //Dibuja borde y fondo
  v2d.FijaLapiz(psSolid, 1, clGray);
  v2d.FijaRelleno(clGray);
  v2d.RectangR(x, y, x+Width, y+Height);
  //Dibuja pines de la izquierda
  ypin := y+SEP_PIN/2;   //posición inicial
  xpin:=x-LON_PIN;
  for i:=1 to nPinsDiag div 2 do begin
    //Pin
    v2d.rectang(xpin, ypin-2, xpin+LON_PIN+1, ypin+2);
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
    v2d.rectang(xpin, ypin-2, xpin+LON_PIN-1, ypin+2);
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
{ TOgLogicState }
procedure TOgLogicState.DibState(const xc, yc: Single; const pin: TPICpin);
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
procedure TOgLogicState.Dibujar;
begin
  //Cuando no se ha iniciado el PIC
  v2d.FijaLapiz(psSolid, 1, clBlack);
  if FState then v2d.FijaRelleno(clRed)
  else v2d.FijaRelleno(clGray);
  //v2d.RectangR(x, y, x+Width, y+Height);
  ptos[0].x := x;
  ptos[0].y := y;
  ptos[1].x := x+20;
  ptos[1].y := y;
  ptos[2].x := x+30;
  ptos[2].y := y+10;
  ptos[3].x := x+20;
  ptos[3].y := y+20;
  ptos[4].x := x;
  ptos[4].y := y+20;
  v2d.Polygon(ptos);
  inherited;
end;
constructor TOgLogicState.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  setlength(ptos, 5);
  Width  := 30;
  Height := 20;
  pc_SUP_CEN.Visible := false;
  pc_INF_CEN.Visible := false;
  pc_CEN_IZQ.Visible := false;
  pc_CEN_DER.Visible := false;
  SizeLocked := true;
end;
{ TfraPICDiagram }
procedure TfraPICDiagram.Refrescar;
begin
  motEdi.Refrescar;
end;
procedure TfraPICDiagram.fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MsgBox('fraPICDiagramKeyDown');
end;
procedure TfraPICDiagram.motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
begin
  if motEdi.seleccion.Count <> 1 then exit;  //Hay más de uno
  if motEdi.Seleccionado.LoSelecciona(X,Y) then begin
    //Click sobre un objeto seleccionado
    if motEdi.Seleccionado is TOgLogicState then begin
      LogInp := TOgLogicState(motEdi.Seleccionado);
      LogInp.FState := true;
      Refrescar;
      //MsgBox('TOggle');
    end;
  end;
end;
procedure TfraPICDiagram.motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
begin
  if motEdi.seleccion.Count <> 1 then exit;  //Hay más de uno
  if motEdi.Seleccionado.LoSelecciona(X,Y) then begin
    if motEdi.Seleccionado is TOgLogicState then begin
      LogInp := TOgLogicState(motEdi.Seleccionado);
      LogInp.FState := false;
      //MsgBox('Up');
    end;
  end;
end;
procedure TfraPICDiagram.motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
begin
  //Verifica el estado para activar acciones
  acGenDelObject.Visible := motEdi.seleccion.Count>0;
  PopupMenu1.PopUp;
end;
constructor TfraPICDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //crea motor de edición
  motEdi := TModEdicion.Create(PaintBox1);
  //agrega objeto
  ogPic := TPicObject.Create(motEdi.v2d);
  ogPic.Highlight := false;
  motEdi.AgregarObjGrafico(ogPic);
  OnKeyDown := @fraPICDiagramKeyDown;
  motEdi.OnMouseDownLeft := @motEdi_MouseDown;
  motEdi.OnMouseUp       := @motEdi_MouseUp;
  motEdi.OnMouseUpRight  := @motEdi_MouseUpRight;
end;
destructor TfraPICDiagram.Destroy;
begin
  motEdi.Destroy;
  inherited Destroy;
end;
procedure TfraPICDiagram.SetPic(AValue: TPIC16);
begin
  Fpic := AValue;
  ogPic.pic := pic;  //Actualiza referencia
end;
/////////////////////// Acciones /////////////////////////
procedure TfraPICDiagram.acGenAddLogTogExecute(Sender: TObject);
{Agrega un Objeto Gráfico LogicToggle}
var
  logTog: TOgLogicState;
begin
  logTog := TOgLogicState.Create(motEdi.v2d);
  logTog.Highlight := false;
  motEdi.AgregarObjGrafico(logTog);
  logTog.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenDelObjectExecute(Sender: TObject);
{Elimina un Objeto Gráfico.}
begin
  if ogPic.Selected then begin
    MsgExc('Cannot delete PIC device.');
    ogPic.Deselec;
  end;
  motEdi.ElimSeleccion;
end;

end.

