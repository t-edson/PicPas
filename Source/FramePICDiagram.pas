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

  { TPinGraph }

  TPinGraph = class
  private
    v2d   : TMotGraf;   //motor gráfico
    x1, y1, x2, y2: Single; //Posición
    lbl   : string;     //Etiqueta
    xLbl, yLbl: Single; //Posición de la etiqueta
    pCnx  : TPtoConx;   //Punto de conexión asociado
    {Se necesita la referenica al PIC para poder leer el valor del voltaje del pin de
    salida, ucndo este pin es parte de un PIC.}
    pic   : TPicCore;
    //picPin : ^TPICPin;  //Guarda referencia
  public
    nPin: integer;    //Número de pin (del encapsulado)
    procedure GetThev(out vThev, rThev: Single);
    procedure SetLabel(xl, yl: Single; txt: string; align: TAlignment =
      taLeftJustify);
  end;
  TPicPinList = specialize TFPGObjectList<TPinGraph>;  //Lista para gestionar los puntos de control

  { TOgComponent }
  {Incluye propiedades de los componentes para este editor gráfico.}
  TOgComponent = class(TObjGraf)
  public
    Ref: string;  //Nomenclatura única del componente: R1, R2, CI1
    ShowRef: boolean;
    xRef, yRef: Single;  //Ubicación relativa de la etiqueta Ref
    pins: TPicPinList;  //Lista de pines
    function AddPin(x1, y1, x2, y2: Single): TPinGraph;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgConector }
  TOgConector = class(TOgComponent)
  private
    procedure PCtlConnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
    procedure PCtlDisconnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
  public
    pinsNod: TPicPinList;  //Lista de pines conectadas al nodo
    function LoSelecciona(xr, yr: Integer): Boolean; override;
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgPic }
  //Define el objeto gráfico PIC
  TOgPic = class(TOgComponent)
  private
    pic: TPicCore;   //referencia al PIC
    xpin: Single;  //Posición X del Pin
    nPinsDiag: Integer;  //Número de pines a dibujar
    nPinsSide: Integer;
    procedure DrawState(const xc, yc: Single; pin: TPinGraph);
  public
    procedure SetPic(pic0: TPicCore);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgLogicState }
  //Define el objeto gráfico LogicState
  TOgLogicState = class(TOgComponent)
  private
    FState: boolean;
    ptos: array of TFPoint;
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgLedRed }
  //Define el objeto Diodo Led
  TOgLedRed = class(TOgComponent)
  private
    FState: boolean;
    ptos: array of TFPoint;
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
    acGenAddConn: TAction;
    acGenAddLed: TAction;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnConnect: TMenuItem;
    mnReset: TMenuItem;
    mnRun: TMenuItem;
    MenuItem3: TMenuItem;
    mnAddLogicTog: TMenuItem;
    mnStepOver: TMenuItem;
    mnDelete: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    procedure acGenAddConnExecute(Sender: TObject);
    procedure acGenAddLedExecute(Sender: TObject);
    procedure acGenAddLogTogExecute(Sender: TObject);
    procedure acGenConnectExecute(Sender: TObject);
    procedure acGenDelObjectExecute(Sender: TObject);
  private  //Nombres y referencias
    function ExistsName(AName: string): boolean;
    function UniqueName(NameBase: string): string;
    function ExistsRef(ARef: string): boolean;
    function UniqueRef(RefBase: string): string;
  private
    Fpic: TPicCore;
    ogPic: TOgPic;
    motEdi: TModEdicion;
    procedure ConnectAction(Sender: TObject);
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

{ TPinGraph }
procedure TPinGraph.GetThev(out vThev, rThev: Single);
{Devuelve el equivalente de Thevening del pin cuando es una salida.}
begin
  if pic = nil then begin
    //No pertenece a un PIC
    vThev := 0;
    rThev := 1e+6;
  end else begin
    //Es pin de un pic
    pic.GetPinThev(nPin, vThev, rThev);
  end;
end;
procedure TPinGraph.SetLabel(xl, yl: Single; txt: string;
                             align: TAlignment = taLeftJustify);
begin
  lbl := txt;
  yLbl := yl;
  case align of
  taLeftJustify: xLbl := xl;  //Justificado a la
  taRightJustify: xLbl := xl - v2d.TextWidth(txt);
  end
end;
{ TOgComponent }
function TOgComponent.AddPin(x1, y1, x2, y2: Single): TPinGraph;
var
  pin: TPinGraph;
begin
  pin := TPinGraph.Create;
  pin.v2d := self.v2d;
  pin.x1 := x1;
  pin.y1 := y1;
  pin.x2 := x2;
  pin.y2 := y2;
  pin.nPin := pins.Count+1;
  pins.Add(pin);
  Result := pin;
end;
constructor TOgComponent.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  pins:= TPicPinList.Create(true);  //Lista de pines
end;
destructor TOgComponent.Destroy;
  begin
    pins.Destroy;
    inherited Destroy;
  end;
procedure TOgConector.PCtlConnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
{Un punto de control se conecta a un punto de conexión }
begin
  //Ubica componente que contiene el punto de conexión
//  og1 := pCnx.Parent;
//  if not (og1 is TOgComponent) then exit;
//  comp1 := TOgComponent(og1);
  if not(pCnx.data is TPinGraph) then exit;
  pinsNod.Add( TPinGraph(pCnx.data) );
//  DebugLn('PCtlConnect');
end;
procedure TOgConector.PCtlDisconnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
{Un punto de control se desconecta a un punto de conexión }
begin
  if not(pCnx.data is TPinGraph) then exit;
  pinsNod.Remove( TPinGraph(pCnx.data) );
//  DebugLn('PCtlDisconnect');
end;
{ TOgConector }
function TOgConector.LoSelecciona(xr, yr: Integer): Boolean;
var
  x0, y0, x1, y1: Integer;
begin
  v2d.XYpant(pcBEGIN.x, pcBEGIN.y,  x0, y0);
  v2d.XYpant(pcEND.x, pcEND.y, x1, y1);
  Result := PointSelectSegment(xr, yr, x0, y0, x1, y1 );
end;
procedure TOgConector.Draw;
begin
  //Calcula el voltaje el conector, que es tratado como un nodo.

  //Descripción
  //v2d.SetText(clBlack, 11,'', true);
  //v2d.Texto(X + 2, Y -20, 'Conector');
  v2d.SetPen(psSolid, 1, clBlack);
  v2d.Linea(pcBEGIN.x, pcBEGIN.y, pcEND.x, pcEND.y);
  inherited Draw;
end;
constructor TOgConector.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  pinsNod := TPicPinList.Create(false);
  pcBEGIN.OnConnect := @PCtlConnect;
  pcEND.OnConnect := @PCtlConnect;
  pcBEGIN.OnDisconnect := @PCtlDisconnect;
  pcEND.OnDisconnect := @PCtlDisconnect;
end;
destructor TOgConector.Destroy;
begin
  pinsNod.Destroy;
  inherited Destroy;
end;
{ TOgPic }
procedure TOgPic.DrawState(const xc, yc: Single; pin: TPinGraph);
{Dibuja un indicador del estado lógico del PIN}
var
  vThev, rThev: Single;
begin
  pin.GetThev(vThev, rThev);
  if vThev > 2.5 then begin
    //Se asuem en alta a partir de 2.5V
    v2d.Barra(xc-5, yc-5, xc+5, yc+5, clRed);
  end;
end;
procedure TOgPic.SetPic(pic0: TPicCore);
{Fija el dispositivo de trabajo y prepara las estructuras que
definen la geometría del componente, de modo que el dibujo sea rápido.}
var
  newHeight, i: Integer;
  ypin: Single;
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
  //Actualiza tamaño. Se debe hacer antes de calcular las posiciones de los Ptos. de Conexión.
  ReSize(Width, newHeight);
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
    pin.SetLabel(2, ypin-8, pic.pines[i].GetLabel);
    pin.pCnx := AddPtoConex(xpin, ypin-1);
    pin.pCnx.data := pin;  //pin.pCnx también guarda referencia al Pin
    pin.pic := pic0;  //guarda referencia el PIC
    //pin.lValue := @;
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
  //Pines de la derecha
  ypin := SEP_PIN/2 + (nPinsSide-1) * SEP_PIN;   //posición inicial
  xpin := width;
  for i:=nPinsSide+1 to nPinsDiag do begin
    //Pin
    pin := AddPin(xpin, ypin-2, xpin+LON_PIN-1, ypin+2);
    pin.SetLabel(xpin-2, ypin-8, pic.pines[i].GetLabel, taRightJustify);
    pin.pCnx := AddPtoConex(xpin+LON_PIN-1, ypin);
    pin.pCnx.data := pin;  //pin.pCnx también guarda referencia al Pin
    pin.pic := pic0;  //guarda referencia el PIC
    //Calcula siguiente posición
    ypin := ypin - SEP_PIN;
  end;
  //Actualiza posición.
  Relocate(x, y);  //Se mantiene la posición, pero se hace para actualizar a los puntos de conexión
end;
procedure TOgPic.Draw;
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
      //Visualiza el estado lógico de pin
      DrawState(x+pin.x1, y+pin.y1, pin);
    end;
  end;
  inherited;
end;
constructor TOgPic.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  Width := 140;
  Height := 180;
//  pcTOP_CEN.Visible := false;
//  pcBOT_CEN.Visible := false;
//  pcCEN_LEF.Visible := false;
//  pcCEN_RIG.Visible := false;
  SizeLocked := true;
//  ShowPtosConex:=true;  //Muestra los puntos de conexión
end;
destructor TOgPic.Destroy;
begin
  inherited Destroy;
end;
{ TOgLogicState }
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
var
  pin: TPinGraph;
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
  pin := AddPin(30, 10, 30, 10);
  pin.pCnx := AddPtoConex(30, 10);
  pin.pCnx.data := pin;  //pin.pCnx también guarda referencia al Pin
  //ShowPtosConex:=true;
end;
destructor TOgLogicState.Destroy;
begin
  inherited Destroy;
end;
{ TOgLedRed }
procedure TOgLedRed.Draw;
var
  ancho: Single;
begin
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Verifica valor lógico

  //FState
  //Dibuja cuerpo
  v2d.SetPen(psSolid, 1, clBlack);
  if FState then v2d.SetBrush(clRed)
  else v2d.SetBrush(clGray);
  //v2d.RectangR(x, y, x+Width, y+Height);
  v2d.Ellipse(x, y, x+width, y+height);
  v2d.Ellipse(x+3, y+3, x+width-3, y+height-3);
  inherited;
end;
constructor TOgLedRed.Create(mGraf: TMotGraf);
var
  pin: TPinGraph;
begin
  inherited Create(mGraf);
  setlength(ptos, 5);
  Width  := 24;
  Height := 24;
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  pin := AddPin(30, 10, 30, 10);
  pin.pCnx := AddPtoConex(12, 0);
  pin.pCnx.data := pin;  //pin.pCnx también guarda referencia al Pin
  //ShowPtosConex:=true;
end;
destructor TOgLedRed.Destroy;
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
function TfraPICDiagram.ExistsName(AName: string): boolean;
{Indica si existe algún componente con el nombre AName}
var
  og: TObjGraf;
begin
  for og in motEdi.objetos do begin
    if og.Name = AName then exit(true);
  end;
  exit(false);
end;
function TfraPICDiagram.UniqueName(NameBase: string): string;
{Obtiene un nombre único tomando como base la cadena "NameBase", de modo que si
en "NameBase" se indica "Nombre", se generará los nombres Nombre1, Nombre2, ... }
var
  n: Integer;
begin
  n := 1;   //Empieza con este valor
  Result := NameBase + IntToStr(n);  //Nombre tentativo
  While ExistsName(Result) do begin
    Inc(n);
    Result := NameBase + IntToStr(n);
  end;
end;
function TfraPICDiagram.ExistsRef(ARef: string): boolean;
{Indica si existe algún componente con la referencia Aref}
var
  og: TObjGraf;
begin
  for og in motEdi.objetos do begin
    if not(og is TOgComponent) then continue;
    if TOgComponent(og).Ref = ARef then exit(true);
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
  //pCnx: TPtoConx;
  //oc: TOgConector;
  //xv, yv: Single;
begin
  if motEdi.seleccion.Count = 1 then begin
    //Hay uno seleccionado
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
  //Verifica si se inicia la conexión de un pin
//  if Button = mbLeft then begin
//    pCnx := motEdi.ConnectionPointMarked;
//    if pCnx <> nil then begin
//      //Se soltó en con un punto de conexión marcado
//      oc := TOgConector.Create(motEdi.v2d);  //Crea objeto
//      oc.behav := behav1D;    //De tipo conector
//      motEdi.AddGraphObject(oc);  //Lo agrega al editor
//      //Ahora se conecta un nodo (Punto de control) al Pto. de Conexión
//      pCnx.ConnectTo(oc.pcBEGIN);
//      pCnx.Locate(pCnx.x, pCnx.y); //Actualiza el "enganche"
//      oc.Selec;          //Selecciona el conector
//      motEdi.v2d.XYvirt(X, Y, xv, yv);  //Obtiene coordenadas del mouse
//      //oc.pcEND.Locate(xv+50, yv+50);   //Posiciona Punto final del conector
//      oc.pcEND.OnChangePosition(oc.pcEND, 0, 0, xv+20, yv+20);
//      //oc.ReSize(oc.Width, oc.Height);
//      motEdi.CapturoEvento := oc;   //Indica al motor de edición que el conector se está dimensionando
//      motEdi.EstPuntero := EP_DIMEN_OBJ;  //Pone editor en modo "Dimensionando"
//      motEdi.Refrescar;            //Actualiza pantalla
//    end;
//  end;
end;
procedure TfraPICDiagram.motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
begin
  if motEdi.seleccion.Count = 1 then begin
    //Hay un compoente seleccionado
    if motEdi.Selected.LoSelecciona(X,Y) then begin
      if motEdi.Selected is TOgLogicState then begin
        LogInp := TOgLogicState(motEdi.Selected);
        LogInp.FState := false;
      end;
    end;
  end;
end;
procedure TfraPICDiagram.motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
var
  og: TObjGraf;
  it, it2: TMenuItem;
  pin2, pin1: TPinGraph;
  comp1, comp2: TOgComponent;
  pCnx: TPtoConx;
begin
  //Verifica el estado para activar acciones
  acGenDelObject.Visible := motEdi.seleccion.Count>0;
  if motEdi.seleccion.Count = 0 then begin
    //Ninguno seleccionado
    mnReset.Visible   := true;
    mnRun.Visible     := true;
    mnStepOver.Visible:= false;
    //mnAddLogicTog.Visible := true;
    acGenAddLogTog.Visible := true;
    acGenAddLed.Visible    := true;
    acGenAddConn.Visible   := true;
  end else if (motEdi.seleccion.Count = 1) and (motEdi.Selected is TOgComponent) then begin
    //Hay un componente seleccionado
    comp1 := TOgComponent(motEdi.Selected);  //Componente fuente
    mnReset.Visible   := true;
    mnRun.Visible     := true;
    mnStepOver.Visible:= true;
    //mnAddLogicTog.Visible := false;
    acGenAddLogTog.Visible := false;
    acGenAddLed.Visible    := false;
    acGenAddConn.Visible   := false;
  end else begin
    //Se ha seleccionado otra cosa o hay varios seleccionados
    mnReset.Visible   := false;
    mnRun.Visible     := false;
    mnStepOver.Visible:= false;
    //mnAddLogicTog.Visible := false;
    acGenAddLogTog.Visible := false;
    acGenAddLed.Visible    := false;
    acGenAddConn.Visible   := false;
  end;
  //Verifica la funcionalidad del menú de "Conectar a"
  //Verify if Connection point marked
  pCnx := motEdi.ConnectionPointMarked;
  if pCnx = nil then begin
    mnConnect.Visible := false;
  end else begin
    mnConnect.Visible := true;
    //mnAddLogicTog.Visible := false;  //Para que no confunda
    acGenAddLogTog.Visible := false;
    acGenAddLed.Visible    := false;
    acGenAddConn.Visible   := false;
    //Ubica componente de origen
    if not(pCnx.Parent is TOgComponent) then exit;
    comp1 := TOgComponent(pCnx.Parent);
    pin1 := TPinGraph(pCnx.data); //Aquí se guarda la referencia al pin2
    mnConnect.Caption := Format('Connect %s to', [pin1.lbl]);
    if (comp1 = nil) or (pin1=nil) then exit;  //Protección
    //Actualiza menú de Conexión, con objetos gráficos
    mnConnect.Clear;
    for og in motEdi.objetos do begin
      if not(og is TOgComponent) then continue;
      it := AddItemToMenu(mnConnect, og.Name, nil);
      comp2 := TOgComponent(og);
      for pin2 in comp2.pins do begin
        if pin2 = nil then continue;
        it2 := AddItemToMenu(it, pin2.lbl, @ConnectAction);
        it2.Hint := comp1.Ref + '.' + IntToStr(pin1.nPin)+'-'+
                    comp2.Ref + '.' + IntToStr(pin2.nPin);
      end;
    end;
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
  ogPic := TOgPic.Create(motEdi.v2d);
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
procedure TfraPICDiagram.acGenConnectExecute(Sender: TObject);
{Connecta el objeto a otro}
begin

end;
procedure TfraPICDiagram.acGenAddLogTogExecute(Sender: TObject);
{Agrega un Objeto Gráfico LogicToggle}
var
  logTog: TOgLogicState;
begin
  logTog := TOgLogicState.Create(motEdi.v2d);
  logTog.Highlight := false;
  logTog.Name := UniqueName('Logic');
  logTog.Ref := UniqueRef('LG');  //Genera nombe único
  motEdi.AddGraphObject(logTog);
  logTog.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenAddLedExecute(Sender: TObject);
{Agrega un Objeto Gráfico LogicToggle}
var
  led: TOgLedRed;
begin
  led := TOgLedRed.Create(motEdi.v2d);
  led.Highlight := false;
  led.Name := UniqueName('Led');
  led.Ref := UniqueRef('D');  //Genera nombe único
  motEdi.AddGraphObject(led);
  led.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenAddConnExecute(Sender: TObject);
var
  conn: TOgConector;
begin
  conn :=  TOgConector.Create(motEdi.v2d);
  conn.behav := behav1D;    //De tipo conector
  //conn.Highlight := false;
  conn.Name := UniqueName('Connector');
  conn.Ref := UniqueRef('CN');  //Genera nombe único
  motEdi.AddGraphObject(conn);
  conn.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenDelObjectExecute(Sender: TObject);
{Elimina un Objeto Gráfico.}
begin
  if ogPic.Selected then begin
    MsgExc('Cannot delete PIC device.');
    ogPic.Deselec;
  end;
  //Elimina elementos seleccionados
  motEdi.DeleteSelected;
end;

end.


