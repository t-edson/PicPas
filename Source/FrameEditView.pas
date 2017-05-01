{Frame que contiene a una vista para mostrar lso editores.}
unit FrameEditView;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazUTF8, LazFileUtils, SynEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, LCLProc, Menus, SynFacilUtils,
  MisUtils, fgl, SynEditMiscClasses;
type
  {Clase derivada de "TSynFacilEditor". Se usa para asociar un SynEdit a un
  TSynFacilEditor}

  { TSynEditor }

  TSynEditor = class(TSynFacilEditor)
  private
    FCaption: string;
    procedure edSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure SetCaption(AValue: string);
  public  //Inicialización
    SynEdit: TSynEdit;  //Editor SynEdit
    tabWidth: integer;  //ancho de l engueta
    panTabs : TPanel;   //referencia al Panel de las lenguetas.
    property Caption: string read FCaption write SetCaption;  //etiqueta de la pestaña
    function SaveAsDialog(SaveDialog1: TSaveDialog): boolean; override;
    function SaveQuery(SaveDialog1: TSaveDialog): boolean;
    constructor Create(AOwner: TComponent; nomDef0, extDef0: string; panTabs0: TPanel);
    destructor Destroy; override;
  end;

  TEditorList = specialize TFPGObjectList<TSynEditor>;
  TSynEditorEvent = procedure(ed: TSynEditor) of object;
  { TfraEditView }
  TfraEditView = class(TFrame)
    Image1: TImage;
    ImgCompletion: TImageList;
    mnNewTab: TMenuItem;
    mnCloseTab: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PopUpTabs: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure mnCloseTabClick(Sender: TObject);
    procedure mnNewTabClick(Sender: TObject);
  private  //Métodos para dibujo de las lenguetas
    xIniTabs : integer;  //Coordenada inicial desde donde se dibujan las lenguetas
    procedure SetTabIndex(AValue: integer);
    procedure DibLeng(x1, x2: integer; coltex: TColor; Activo: boolean; txt: string
      );   //dibuja una lengueta
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1Paint(Sender: TObject);
    procedure InitTabs;
  private
    FTabIndex: integer;
    editors  : TEditorList;
    lang     : string;
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetModified: boolean;
    function LastIndex: integer;
    function AddEdit: TSynEditor;
    procedure DeleteEdit;
  public  //Manejo de pestañas
    property TabIndex: integer read FTabIndex write SetTabIndex;   //panel actualmente activo
    function Count: integer;
    function ActiveEditor: TSynEditor;
    function SearchEditorIdx(filname: string): integer;
    function SelectEditor(filname: string): boolean;
    procedure SelectNextEditor;
    procedure SelectPrevEditor;
    function HasFocus: boolean;
    procedure SetFocus; override;
  public  //Administración de archivos
    OnChangeEditorState: TSynEditorEvent;
    OnChangeFileInform: procedure of object;
    OnSelectEditor: procedure of object;  //Cuando cambia la selección de editor
    property Modified: boolean read GetModified;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    procedure NewFile;
    procedure LoadFile(fileName: string);
    procedure SaveFile;
    function OpenDialog(filter: string): boolean;
    function SaveAsDialog: boolean;
    procedure CloseEditor;
    function CloseAll: boolean;
  public //Inicialización
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetLanguage(lang0: string);
  end;

implementation
{$R *.lfm}
const
  MIN_WIDTH_TAB = 50;  //Ancho por defecto de la lengueta
  FONT_TAB_SIZE = 9;
  SEPAR_TABS = 2;  //Separación adicional, entre pestañas

{ TSynEditor }
procedure TSynEditor.edSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
  if Line = self.linErr then begin
      Special := True ;  //marca como línea especial
      Markup.Background := TColor($3030A0); //color de fondo
  end;
end;
procedure TSynEditor.SetCaption(AValue: string);
{Cambiar el título, cambia el ancho de la lengueta}
var
  w: Integer;
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  panTabs.Canvas.Font.Size := FONT_TAB_SIZE;  {Fija atrubutos de texto, para que el
                                        cálculo con "TextWidth", de ancho sea correcto}
  w := panTabs.Canvas.TextWidth(AValue) + 30;
  if w < MIN_WIDTH_TAB then w := MIN_WIDTH_TAB;
  tabWidth := w;
  panTabs.Invalidate;   //Para refrescar el dibujo
end;
function TSynEditor.SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
begin
  Result := inherited SaveAsDialog(SaveDialog1);
  if Result then exit;
  //Se ha cambiado el nombre del archivo. Actualiza.
  Caption := ExtractFileNameOnly(NomArc);
end;
function TSynEditor.SaveQuery(SaveDialog1: TSaveDialog): boolean;
{Versión de SaveQuery(), que verifica si el editor tiene nombre.}
//Verifica si es necesario guardar el archivo antes de ejecutar alguna operación.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  resp: integer;
begin
  Result := false;
  if SynEdit.Modified then begin
    resp := MessageDlg('', dic('El archivo %s ha sido modificado.' +  LineEnding +
                     '¿Deseas guardar los cambios?',[ExtractFileName(NomArc)]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if resp = mrCancel then begin
      Result := true;   //Sale con "true"
      Exit;
    end;
    if resp = mrYes then begin  //guardar
      if NomArc='' then begin
        //Es un archivo nuevo
        SaveAsDialog(SaveDialog1);
      end else begin
        SaveFile;  //ACtualiz "Error"
      end;
    end;
  end;
end;
constructor TSynEditor.Create(AOwner: TComponent; nomDef0, extDef0: string;
  panTabs0: TPanel);
begin
  SynEdit:= TSynEdit.Create(AOwner);// Crea un editor
  inherited Create(SynEdit, nomDef0, extDef0);
  tabWidth := 30;  //valor por defecto
  panTabs := panTabs0;

  //configuración del editor
  SynEdit.Options:=[eoBracketHighlight];  //quita la línea vertical
  SynEdit.Options := SynEdit.Options - [eoSmartTabs];
  SynEdit.Options := SynEdit.Options - [eoTrimTrailingSpaces];
  SynEdit.Options := SynEdit.Options + [eoKeepCaretX];
  SynEdit.Options := SynEdit.Options + [eoTabIndent];  //permite indentar con <Tab>
  SynEdit.Options2:= SynEdit.Options2 + [eoCaretSkipTab];
  SynEdit.TabWidth:= 2;
  SynEdit.OnSpecialLineMarkup:=@edSpecialLineMarkup;
//  InicEditorC1(SynEdit);
  //define paneles
//  self.PanFileSaved := StatusBar1.Panels[0]; //panel para mensaje "Guardado"
//  self.PanCursorPos := StatusBar1.Panels[1];  //panel para la posición del cursor
//
//  self.PanForEndLin := StatusBar1.Panels[2];  //panel para el tipo de delimitador de línea
//  self.PanCodifFile := StatusBar1.Panels[3];  //panel para la codificación del archivo
//  self.PanLangName  := StatusBar1.Panels[4];  //panel para el lenguaje
//  self.PanFileName  := StatusBar1.Panels[5];  //panel para el nombre del archivo

  NewFile;        //para actualizar estado
end;
destructor TSynEditor.Destroy;
begin
  inherited Destroy;
  FreeAndNil(SynEdit);  //El "Owner", intentará destruirlo, por eso lo ponemos en NIL
end;
{ TfraEditView }
procedure TfraEditView.SetTabIndex(AValue: integer);
begin
  if AValue>editors.Count-1 then AValue := editors.Count-1;
  if FTabIndex = AValue then Exit;
  if FTabIndex<>-1 then begin  //que no sea la primera vez
    editors[FTabIndex].SynEdit.Visible := false;  //oculta el anterior
  end;
  FTabIndex := AValue;   //cambia valor
  editors[FTabIndex].SynEdit.Visible := true;  //muestra el nuevo
  if OnSelectEditor<>nil then OnSelectEditor;  //dispara evento
  Panel1.Invalidate;   //para refrescar
end;
//Métodos pàra el dibujo de lenguetas
procedure TfraEditView.DibLeng(x1, x2: integer; coltex: TColor; Activo: boolean;
  txt: string);
  procedure GetX1X2(const xrmin: integer; y: integer; out xr1, xr2: integer);
  {devuelve las coordenadas x1 y x2 de la línea "y" de la lengueta}
  begin
    case y of
    0: begin  //priemra fila
        xr1 := x1+4;
        xr2 := xrmin -4;
      end;
    1: begin
        xr1 := x1+2;
        xr2 := xrmin -2;
      end;
    2: begin
        xr1 := x1+1;
        xr2 := xrmin ;
      end;
    3: begin
        xr1 := x1+1;
        xr2 := xrmin + 1;
      end;
    else  //otras filas
      xr1 := x1;
      xr2 := xrmin + (y div 2);
    end;
  end;
var
  cv: TCanvas;
  y1, y2, alto, xr1, xr2, xrmin, xrmin2, i: Integer;
  r: TRect;
  colBorde: TColor;
begin
  alto := panel1.Height;
  cv := Panel1.canvas;
  cv.Font.Size:= FONT_TAB_SIZE;
  cv.Font.Color := clBlack;
  cv.Font.Color := coltex;   //Color de texto
  y1 := 0;
  y2 := y1 + alto;
  //Fija Línea y color de fondo
  cv.Pen.Style := psSolid;
  cv.Pen.Width := 1;
  if Activo then cv.Pen.Color := clWhite else cv.Pen.Color := clMenu;
  //Dibuja fondo de lengueta. El dibujo es línea por línea
  xrmin := x2 - (alto div 4);  //corrige inicio, para que el punto medio de la pendiente,  caiga en x2
  xrmin2 := x2 + (alto div 4)+1;  //corrige inicio, para que el punto medio de la pendiente,  caiga en x2
  for i:=0 to alto-1 do begin
    GetX1X2(xrmin, i, xr1, xr2);
    cv.Line(xr1, i, xr2, i);
  end;
  //Dibuja borde de lengueta
  colBorde := clGray;
  cv.Pen.Color := colBorde;
  cv.Line(x1,y1+4,x1,y2);  //lateral izquierdo
  cv.Line(x1+4,y1, xrmin-4, y1);  //superior
  cv.Line(xrmin+2, y1+4, xrmin2, y2);  //lateral derecho
  //Bordes
  GetX1X2(xrmin, 0, xr1, xr2);
  cv.Pixels[xr1,0] := colBorde;
  cv.Pixels[xr2,0] := colBorde;
  GetX1X2(xrmin, 1, xr1, xr2);
  cv.Pixels[xr1,1] := colBorde;
  cv.Pixels[xr1+1,1] := colBorde;
  cv.Pixels[xr2,1] := colBorde;
  cv.Pixels[xr2-1,1] := colBorde;
  GetX1X2(xrmin, 2, xr1, xr2);
  cv.Pixels[xr1,2] := colBorde;
  cv.Pixels[xr2,2] := colBorde;
  cv.Pixels[xr2-1,2] := colBorde;
  GetX1X2(xrmin, 3, xr1, xr2);
  cv.Pixels[xr1,3] := colBorde;
  cv.Pixels[xr2,3] := colBorde;
  //Dibuja ícono
  ImgCompletion.Draw(cv, x1+4, 4, 7);
  //Elimina objetos y pone texto
  r.Top := y1;
  r.Bottom := y2;
  r.Left := x1+20;  //Deja espacio para el ícono
  r.Right := x2-7;  //deja espacio para el botón de cierre
  cv.TextRect(r, x1+22, 4 ,txt);
end;
procedure TfraEditView.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  x1, x2, i: Integer;
  edi: TSynEditor;
begin
  x1 := xIniTabs;
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    x2 := x1 + edi.tabWidth;
    if (X>x1) and (X<x2) then begin
      TabIndex := i;
      if Shift = [ssRight] then begin
        PopUpTabs.PopUp;
      end;
      exit;
    end;
    x1 := x2 + SEPAR_TABS;
  end;
end;
procedure TfraEditView.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;
procedure TfraEditView.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Pasa el enfoque al editor que se ha seleccionado
  if TabIndex<>-1 then begin
    try
      editors[TabIndex].SynEdit.SetFocus;
    except

    end;
  end;
end;
procedure TfraEditView.Panel1Paint(Sender: TObject);
var
  x1, i, x2, xa: Integer;
  edi: TSynEditor;
begin
  //Dibuja las pestañas
  x1 := xIniTabs;
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    x2 := x1 + edi.tabWidth;
    if i = TabIndex then begin
      //No dibuja ahora al activo, lo deja para después.
      xa := x1;   //guarda coordenada
    end else begin
      DibLeng(x1, x2, clBlack, false, edi.Caption);
    end;
    x1 := x2 + SEPAR_TABS;
  end;
  //Dibuja al final al activo, para que aparezca enciama
  if TabIndex<>-1 then begin
    edi := editors[TabIndex];
    x1 := xa;
    x2 := x1 + edi.tabWidth;
    DibLeng(x1, x2, clBlack, true, edi.Caption);
  end;
end;
procedure TfraEditView.InitTabs;
{Inicia el dibujo de las lenguetas}
begin
  xIniTabs := 20;
  Panel1.OnMouseMove := @Panel1MouseMove;
  panel1.OnMouseDown := @Panel1MouseDown;
  panel1.OnMouseUp := @Panel1MouseUp;
end;
//////////////////////////////////////////////////////////////
procedure TfraEditView.ChangeEditorState;
var
  ed: TSynEditor;
begin
  ed := ActiveEditor;
  if OnChangeEditorState<>nil then OnChangeEditorState(ed);
end;
procedure TfraEditView.editChangeFileInform;
begin
  if OnChangeFileInform<>nil then OnChangeFileInform;
end;
function TfraEditView.GetCanRedo: boolean;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit(false);
  //Busca editor actual
  ed := ActiveEditor;
  Result := ed.CanRedo;
end;
function TfraEditView.GetCanUndo: boolean;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit(false);
  //Busca editor actual
  ed := ActiveEditor;
  Result := ed.CanUndo;
end;
function TfraEditView.GetModified: boolean;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit(false);
  //Busca editor actual
  ed := ActiveEditor;
  Result := ed.Modified;
end;
function TfraEditView.LastIndex: integer;
{Devuelve el índice de la última pestaña.}
begin
  Result :=editors.Count - 1;
end;
function TfraEditView.AddEdit: TSynEditor;
{Agrega una nueva ventana de eición a la vista, y devuelve la referencia.}
var
  ed: TSynEditor;
begin
  //Crea Editor.
  ed := TSynEditor.Create(self, 'SinNombre', 'pas', Panel1);
  ed.OnChangeEditorState := @ChangeEditorState;
  ed.OnChangeFileInform := @editChangeFileInform;
  ed.hl.IconList := ImgCompletion;
  ed.SetLanguage(lang);
  //Configura PageControl
  ed.SynEdit.Parent := self;
  ed.SynEdit.Align := alClient;
  ed.Caption := 'NewFile' + IntToStr(editors.Count+1);
  ed.NomArc := '';  //Pone sin nombre para saber que no se ha guardado
  editors.Add(ed);   //agrega a la lista
  TabIndex := LastIndex;
  //Actualzia referencias
  Result := ed;
end;
procedure TfraEditView.DeleteEdit;
{Elimina al editor activo.}
begin
  if TabIndex=-1 then exit;
  editors.Delete(TabIndex);
  //Hay que actualiza TabIndex
  if editors.Count = 0 then begin
    //Era el único
    FTabIndex := -1;
  end else begin
    //había al menos 2
    if TabIndex > editors.Count - 1 then begin
      //Quedó apuntando fuera
      FTabIndex := editors.Count - 1;   //limita
      //No es necesario ocultar el anterior, porque se eliminó
      editors[FTabIndex].SynEdit.Visible := true;  //muestra el nuevo
    end else begin
      //Queda apuntando al siguiente. No es necesario modificar.
      //No es necesario ocultar el anterior, porque se eliminó
      editors[FTabIndex].SynEdit.Visible := true;  //muestra el nuevo
    end;
  end;
  if OnSelectEditor<>nil then OnSelectEditor;
  Panel1.Invalidate;   //para refrescar
end;
///Manejo de pestañas
function TfraEditView.Count: integer;
begin
  Result := editors.Count;
end;
function TfraEditView.ActiveEditor: TSynEditor;
{Devuelve el editor SynEditor, activo, es decir el que se encuentra en la lengueta
activa. }
var
  i: Integer;
begin
  if editors.Count = 0 then exit(nil);
  i := TabIndex;
  Result := editors[i];   //Solo funcionará si no se desordenan las enguetas
end;
function TfraEditView.SearchEditorIdx(filname: string): integer;
{Busca entre las ventanas abiertas al archivo indicado, y devuelve el índice del
editor. Si no lo encuentra devuelve -1}
var
  ed: TSynEditor;
  i: integer;
begin
  for i:=0 to editors.Count-1 do begin
    ed := editors[i];
    if Upcase(ed.NomArc) = UpCase(filname) then exit(i);
  end;
  exit(-1);
end;
function TfraEditView.SelectEditor(filname: string): boolean;
{Activa el editor que corresponde al archivo indicado. Si no encuentra el archivo,
devuelve FALSE.}
var
  edIdx: integer;
begin
  if editors.Count=0 then exit(false);  //no hay ventanas
  if filname = '@' then begin
    //"@", indica el editor actual
    exit(true);
  end;
  edIdx := SearchEditorIdx(filname);
  if edIdx = -1 then begin
    exit(false);
  end else begin
    TabIndex := edIdx;
    exit(true);
  end;
end;
procedure TfraEditView.SelectNextEditor;
{Selecciona al siguiente editor.}
begin
  if Count = 0 then exit;
  if TabIndex=-1 then exit;
  if TabIndex = LastIndex then TabIndex := 0 else TabIndex := TabIndex + 1;
  SetFocus;
end;
procedure TfraEditView.SelectPrevEditor;
{Selecciona al editor anterior.}
begin
  if Count = 0 then exit;
  if TabIndex=-1 then exit;
  if TabIndex = 0 then TabIndex := LastIndex else TabIndex := TabIndex -1;
  SetFocus;
end;
function TfraEditView.HasFocus: boolean;
{Indica si alguno de los editores, tiene el enfoque.}
var
  i: Integer;
begin
  for i:=0 to editors.Count-1 do begin
    if editors[i].SynEdit.Focused then exit(true);
  end;
  exit(false);
end;
procedure TfraEditView.SetFocus;
begin
//  inherited SetFocus;
  if TabIndex = -1 then exit;
  if editors[TabIndex].SynEdit.Visible then begin
    editors[TabIndex].SynEdit.SetFocus;
  end;
end;
//Administración de archivos
procedure TfraEditView.NewFile;
{Abre una nueva ventana de edición.}
var
  ed: TSynEditor;
begin
  ed := AddEdit;
  ed.LoadSyntaxFromFile('PicPas_PIC16.xml');
end;
procedure TfraEditView.LoadFile(fileName: string);
//Carga un archivo en el editor
var
  ed: TSynEditor;
  ext: string;
begin
  ed := AddEdit;
  ed.LoadFile(fileName);
  //Carga la sintaxis apropiada
  ext := ExtractFileExt(fileName);
  case Upcase(ext) of
  '.PAS': ed.LoadSyntaxFromFile('PicPas_PIC16.xml');
  end;
  //ed.LoadSyntaxFromPath;  //para que busque el archivo apropiado
  ed.Caption := ExtractFileNameOnly(fileName);
end;
function TfraEditView.OpenDialog(filter: string): boolean;
//Muestra el cuadro de diálogo para abrir un archivo. Si hay error devuelve FALSE.
var arc0: string;
begin
  OpenDialog1.Filter:=filter;
  if not OpenDialog1.Execute then exit(true);    //se canceló
  arc0 := OpenDialog1.FileName;
  LoadFile(arc0);  //legalmente debería darle en UTF-8
  Result := true;   //sale sin incidencias
end;
procedure TfraEditView.SaveFile;
begin
  if ActiveEditor=nil then exit;
  if ActiveEditor.NomArc='' then begin
    //Es un archivo nuevo
    ActiveEditor.SaveAsDialog(SaveDialog1);
  end else begin
    ActiveEditor.SaveFile;
  end;
end;
function TfraEditView.SaveAsDialog: boolean;
begin
  if ActiveEditor=nil then exit;
  Result := ActiveEditor.SaveAsDialog(SaveDialog1);
  if Result then exit;   //se canceló
  //ACtualiza etiqueta
end;
procedure TfraEditView.CloseEditor;
{Cierra el editor actual.}
begin
  if ActiveEditor=nil then exit;
  if ActiveEditor.SaveQuery(SaveDialog1) then exit;  //cancelado
  DeleteEdit;
end;
function TfraEditView.CloseAll: boolean;
{Cierra todas las ventanas, pidiendo confirmación. Si se cancela, devuelve TRUE}
begin
  while editors.Count>0 do begin
    if ActiveEditor=nil then exit;
    if ActiveEditor.SaveQuery(SaveDialog1) then exit(true);  //cancelado
    DeleteEdit;
  end;
  exit(false);
end;
//Inicialización
constructor TfraEditView.Create(AOwner: TComponent);
begin
  inherited;
  editors:= TEditorList.Create(true);
  panel1.OnPaint := @Panel1Paint;
  FTabIndex := -1;
  InitTabs;
end;
destructor TfraEditView.Destroy;
begin
  editors.Destroy;
  inherited Destroy;
end;
procedure TfraEditView.SetLanguage(lang0: string);
begin
  lang := lang0;
  case lowerCase(lang) of
  'es': begin
    end;
  'en': begin
    end;
  end;
end;
//Menú
procedure TfraEditView.mnNewTabClick(Sender: TObject);
begin
  NewFile;
  SetFocus;
end;
procedure TfraEditView.mnCloseTabClick(Sender: TObject);
begin
  CloseEditor;
  SetFocus;
end;

end.
//92
