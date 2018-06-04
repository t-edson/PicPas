unit FramePICDiagram;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, fgl, Forms, Controls, ExtCtrls, Graphics, Menus,
  ActnList, LCLProc, ogMotEdicion, ogMotGraf2D, ogDefObjGraf, PicCore, Parser,
  MisUtils;
type
  {Objeto que modela las propiedades gráficas de un pin del PIC.
  Se podría usar el mismo tipo TpicCore.pines[] y usar ese arreglo como lista enlugar
  de crear TPicPin, pero la idea de TPicPin es que modela objetos gráficos con geomatría
  y visibles, mientras que TpicCore.pines[] almacena propiedades eléctricas y lógicas
  del dispositivo.}
  TPinGraph = class
    x1, y1, x2, y2: Single; //Posición
    lbl : string;     //Etiqueta
    xLbl, yLbl: Single; //Posición de la etiqueta
    pCnx: TPtoConx;   //Punto de conexión asociado
    picPin : ^TPICPin;  //Guarda referencia
  end;
  TPicPinList = specialize TFPGObjectList<TPinGraph>;  //Lista para gestionar los puntos de control

  {Incluye propiedades de los componentes para este editor gráfico.}

  { TPicComponent }

  TPicComponent = class(TObjGraf)
  public
    Ref: string;  //Nomenclatura única del componente: R1, R2, CI1
    ShowRef: boolean;
    xRef, yRef: Single;  //Ubicación relativa de la etiqueta Ref
    pins: TPicPinList;  //Lista de pines
    function AddPin(x1, y1, x2, y2: Single): TPinGraph;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TPicObject }
  //Define el objeto gráfico PIC
  TPicObject = class(TPicComponent)
  private
    pic: TPicCore;   //referencia al PIC
    xpin: Single;  //Posición X del Pin
    nPinsDiag: Integer;  //Número de pines a dibujar
    nPinsSide: Integer;
    procedure DibState(const xc, yc: Single; const pin: TPICPin);
  public
    procedure SetPic(pic0: TPicCore);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgLogicState }
  //Define el objeto gráfico LogicState
  TOgLogicState = class(TPicComponent)
  private
    pic: TPicCore;   //referencia al PIC
    FState: boolean;
    ptos: array of TFPoint;
    procedure DibState(const xc, yc: Single; const pin: TPICPin);
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TfraPICDiagram }
  TfraPICDiagram = class(TFrame)
    acGenAddLogTog: TAction;
    acGenDelObject: TAction;
    acGenConnect: TAction;
    ActionList1: TActionList;
    mnConnect: TMenuItem;
    mnReset: TMenuItem;
    mnRun: TMenuItem;
    MenuItem3: TMenuItem;
    mnAddLogicTog: TMenuItem;
    mnStepOver: TMenuItem;
    mnDelete: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    procedure acGenAddLogTogExecute(Sender: TObject);
    procedure acGenConnectExecute(Sender: TObject);
    procedure acGenDelObjectExecute(Sender: TObject);
  private
    Fpic: TPicCore;
    ogPic: TPicObject;
    motEdi: TModEdicion;
    procedure ConnectAction(Sender: TObject);
    function ExistsRef(ARef: string): boolean;
    function UniqueRef(RefBase: string): string;
    procedure fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
    procedure motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    procedure Refrescar;
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
  SEP_PIN = 20;   //Separación entre pines
  LON_PIN = 15;   //Longitud de pin

{ TPicComponent }
function TPicComponent.AddPin(x1, y1, x2, y2: Single): TPinGraph;
var
  pin: TPinGraph;
begin
  pin := TPinGraph.Create;
  pin.x1 := x1;
  pin.y1 := y1;
  pin.x2 := x2;
  pin.y2 := y2;
  pins.Add(pin);
  Result := pin;
end;
constructor TPicComponent.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  pins:= TPicPinList.Create(true);  //Lista de pines
end;
destructor TPicComponent.Destroy;
begin
  pins.Destroy;
  inherited Destroy;
end;

{ TPicObject }
procedure TPicObject.DibState(const xc, yc: Single; const pin: TPICPin);
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
procedure TPicObject.SetPic(pic0: TPicCore);
{Fija el dispositivo de trabajo y prepara las estructuras que
definen la geometría del componente, de modo que el dibujo sea rápido.}
var
  newHeight, i: Integer;
  ypin, lblWidth: Single;
  pin: TPinGraph;
begin
  pic := pic0;  //Actualiza referencia
  Name := pic0.Model;
  //Define geometría del cuerpo del PIC
  if pic.Npins <= 6 then begin
    nPinsDiag := 6;
  end else if pic.Npins <=8 then begin
    nPinsDiag := 8;
  end else if pic.Npins <=18 then begin
    nPinsDiag := 18;
  end else if pic.Npins <=28 then begin
    nPinsDiag := 28;
  end else if pic.Npins <=40 then begin
    nPinsDiag := 40;
  end else begin
    //Caso de muchos pines
    nPinsDiag := 40;
  end;
  nPinsSide := nPinsDiag div 2;  //Pines pro lado
  newHeight := nPinsSide * SEP_PIN; //Altura del chip
  ReSize(Width, newHeight);      //Actualiza forma
  {Calcula posiciones relativas de los pines asumiendo un formato de encapsulado DIL.
  Se crearán también los puntos de conexión en cada uno de los pines}
  pins.Clear;
  PtosConex.Clear;  //Se aprovechará para crear puntos de conexión
  //Pines de la izquierda
  ypin := SEP_PIN/2;   //posición inicial
  xpin := -LON_PIN;
  for i:=1 to nPinsSide do begin
    //Pin
    pin := AddPin(xpin, ypin-2, xpin+LON_PIN+1, ypin+2);
    pin.lbl := pic.pines[i].GetLabel;
    pin.xLbl := 2;
    pin.yLbl := ypin-8;
    pin.pCnx := AddPtoConex(xpin, ypin-1);
    pin.pCnx.data := pin;  //Guarda referencia al Pin
//    pin.picPin := @pic.pines[i];
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
  //Pines de la derecha
  ypin := SEP_PIN/2;   //posición inicial
  xpin := width;
  for i:=1 to nPinsSide do begin
    //Pin
    pin := AddPin(xpin, ypin-2, xpin+LON_PIN-1, ypin+2);
    pin.lbl := pic.pines[nPinsDiag-i+1].GetLabel;
    lblWidth := v2d.TextWidth(pin.lbl);
    pin.xLbl := xpin - lblWidth - 2;
    pin.yLbl := ypin-8;
    pin.pCnx := AddPtoConex(xpin+LON_PIN-1, ypin);
    pin.pCnx.data := pin;  //Guarda referencia al Pin
//    pin.picPin := @pic.pines[nPinsDiag-i+1];
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
  ShowPtosConex:=true;  //Muestra los puntos de conexión
end;
procedure TPicObject.Draw;
var
  ancho: Single;
  pin : TPinGraph;
begin
  if pic= nil then begin
    v2d.SetPen(psSolid, 1, clBlack);
    v2d.SetBrush(clGray);
    v2d.RectangR(x, y, x+Width, y+Height);
  end else begin //Caso normal
    //Dibuja título
    ancho := v2d.TextWidth(Name);
    v2d.SetText(True, False, False);
    v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
    //Dibuja cuerpo
    v2d.SetText(False, False, False);
    v2d.SetPen(psSolid, 1, clGray);
    v2d.SetBrush(clGray);
    v2d.RectangR(x, y, x+Width, y+Height);  //fondo
    for pin in pins do begin
      //Dibuja los pines
      v2d.rectang(x+pin.x1, y+pin.y1, x+pin.x2, y+pin.y2);
      v2d.Texto(x+pin.xLbl, y+pin.yLbl, pin.lbl);
    end;
  end;
  inherited;
end;
constructor TPicObject.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  Width := 140;
  Height := 180;
//  pcTOP_CEN.Visible := false;
//  pcBOT_CEN.Visible := false;
//  pcCEN_LEF.Visible := false;
//  pcCEN_RIG.Visible := false;
  SizeLocked := true;
end;
destructor TPicObject.Destroy;
begin
  inherited Destroy;
end;
{ TOgLogicState }
procedure TOgLogicState.DibState(const xc, yc: Single; const pin: TPICPin);
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
procedure TOgLogicState.Draw;
var
  ancho: Single;
begin
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Dibuja cuerpo
  v2d.SetPen(psSolid, 1, clBlack);
  if FState then v2d.SetBrush(clRed)
  else v2d.SetBrush(clGray);
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
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  AddPtoConex(30, 10);
  ShowPtosConex:=true;
end;
destructor TOgLogicState.Destroy;
begin
  inherited Destroy;
end;

{ TfraPICDiagram }
procedure TfraPICDiagram.Refrescar;
begin
  motEdi.Refrescar;
end;
procedure TfraPICDiagram.SetCompiler(cxp0: TCompilerBase);
begin
  Fpic := cxp0.picCore;
  //Inicia dispositivo
  ogPic.SetPic(cxp0.picCore);
end;
procedure TfraPICDiagram.fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MsgBox('fraPICDiagramKeyDown');
end;
procedure TfraPICDiagram.ConnectAction(Sender: TObject);
var
  mnItem: TMenuItem;
begin
  if Sender is TMenuItem then begin
    mnItem := TMenuItem(Sender);
    msgbox('Eureka:' + mnItem.Hint);
  end;
end;
function TfraPICDiagram.ExistsRef(ARef: string): boolean;
{Indica si existe algún compoente con la referencia Aref}
var
  og: TObjGraf;
begin
  for og in motEdi.objetos do begin
    if TPicComponent(og).Ref = ARef then exit(true);
  end;
  exit(false);
end;
function TfraPICDiagram.UniqueRef(RefBase: string): string;
{Obtiene una referencia única tomando como base la cadena "RefBase", de modo que si
en "RefBase" se indica "R", se generará los nombres R1, R2, R3, ... }
var
  n: Integer;
begin
  n := 1;   //Empieza con este valor
  Result := RefBase + IntToStr(n);  //Nombre tentativo
  While ExistsRef(Result) do begin
    Inc(n);
    Result := RefBase + IntToStr(n);
  end;
end;
procedure TfraPICDiagram.motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
begin
  if motEdi.seleccion.Count <> 1 then exit;  //Hay más de uno
  if motEdi.Selected.LoSelecciona(X,Y) then begin
    //Click sobre un objeto seleccionado
    if motEdi.Selected is TOgLogicState then begin
      LogInp := TOgLogicState(motEdi.Selected);
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
  if motEdi.Selected.LoSelecciona(X,Y) then begin
    if motEdi.Selected is TOgLogicState then begin
      LogInp := TOgLogicState(motEdi.Selected);
      LogInp.FState := false;
      //MsgBox('Up');
    end;
  end;
end;
procedure TfraPICDiagram.motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
var
  og: TObjGraf;
  it, it2: TMenuItem;
  pin: TPinGraph;
  comp1, comp2: TPicComponent;
begin
  //Verifica el estado para activar acciones
  acGenDelObject.Visible := motEdi.seleccion.Count>0;
  if motEdi.seleccion.Count = 1 then begin
    comp1 := TPicComponent(motEdi.Selected);  //Componente fuente
    //Hay uno seleccionado
    mnReset.Visible   := true;
    mnRun.Visible     := true;
    mnStepOver.Visible:= true;
    mnConnect.Visible := true;
    //Actualiza menú de Conexión, con objetos gráficos
    mnConnect.Clear;
    for og in motEdi.objetos do begin
      it := AddItemToMenu(mnConnect, og.Name, nil);
      comp2 := TPicComponent(og);
      for pin in comp2.pins do begin
        if pin = nil then continue;
        it2 := AddItemToMenu(it, pin.lbl, @ConnectAction);
        it2.Hint := comp1.Ref +'-'+ comp2.Ref + '.'+ pin.lbl;
      end;
    end;
  end else begin
    //Hay varios seleccionados
    mnReset.Visible   := false;
    mnRun.Visible     := false;
    mnStepOver.Visible:= false;
    mnConnect.Visible := false;
  end;
  //Muestra
  PopupMenu1.PopUp;
end;
constructor TfraPICDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //crea motor de edición
  motEdi := TModEdicion.Create(PaintBox1);
  //agrega objeto
  ogPic := TPicObject.Create(motEdi.v2d);
  ogPic.Ref := 'CI1';
  ogPic.Highlight := false;

  motEdi.AddGraphObject(ogPic);
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
/////////////////////// Acciones /////////////////////////
procedure TfraPICDiagram.acGenAddLogTogExecute(Sender: TObject);
{Agrega un Objeto Gráfico LogicToggle}
var
  logTog: TOgLogicState;
begin
  logTog := TOgLogicState.Create(motEdi.v2d);
  logTog.Highlight := false;
  logTog.Name := 'Logic';
  logTog.Ref := UniqueRef('LG');  //Genera nombe único
  motEdi.AddGraphObject(logTog);
  logTog.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenConnectExecute(Sender: TObject);
{Connecta el objeto a otro}
begin

end;
procedure TfraPICDiagram.acGenDelObjectExecute(Sender: TObject);
{Elimina un Objeto Gráfico.}
begin
  if ogPic.Selected then begin
    MsgExc('Cannot delete PIC device.');
    ogPic.Deselec;
  end;
  motEdi.DeleteSelected;
end;

end.


