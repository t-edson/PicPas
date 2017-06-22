{Frame que contiene a una vista para mostrar lso editores.}
unit FrameEditView;
{$mode objfpc}{$H+}
interface
uses
  Classes, windows, SysUtils, FileUtil, LazUTF8, LazFileUtils, SynEdit, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, LCLProc, Menus, LCLType, Globales,
  SynFacilUtils, SynFacilCompletion, SynFacilHighlighter, MisUtils, XpresBas,
  strutils, fgl, SynEditMiscClasses, SynEditKeyCmds, SynPluginMultiCaret;
type
  {Clase derivada de "TSynFacilEditor". Se usa para asociar un SynEdit a un
  TSynFacilEditor}

  //Versión personalizada de  TSynFacilComplet

  { TSynFacilComplet2 }

  TSynFacilComplet2 = class(TSynFacilComplet)
    function IsKeyword(const AKeyword: string): boolean; override;
  end;

  { TSynEditor }
  {VErsión personalizada de TSynFacilEditor, que usa TSynFacilComplet2, como resaltador}
  TSynEditor = class(TSynFacilEditor)
  private  //Manejo de edición síncrona
    cursorPos: array of TPOINT;  //guarda posiciones de cursor
    firstTok : boolean;  //bandera para identificar al primer token
    tokFind  : String;   //token a buscar
    procedure AddCursorPos(x,y: integer);
    procedure ExploreLine(lin: string; x1, x2, y: integer);
    procedure ExploreForSyncro;
    procedure SetCursors;
  private
    FCaption : string;
    procedure edSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure SetCaption(AValue: string);
  protected
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public  //Inicialización
    SynEdit: TSynEdit;  //Editor SynEdit
    tabWidth: integer;  //ancho de l engueta
    panTabs : TPanel;   //referencia al Panel de las lenguetas.
    property Caption: string read FCaption write SetCaption;  //etiqueta de la pestaña
    function SaveAsDialog(SaveDialog1: TSaveDialog): boolean; override;
    function SaveQuery(SaveDialog1: TSaveDialog): boolean; reintroduce;
    constructor Create(AOwner: TComponent; nomDef0, extDef0: string; panTabs0: TPanel); reintroduce;
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
    procedure RefreshTabs;
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
    fMultiCaret: TSynPluginMultiCaret;
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetModified: boolean;
    function LastIndex: integer;
    function NewName: string;
    function AddEdit(ext: string): TSynEditor;
    procedure DeleteEdit;
  public  //Manejo de pestañas
    property TabIndex: integer read FTabIndex write SetTabIndex;   //panel actualmente activo
    function Count: integer;
    function ActiveEditor: TSynEditor;
    function SearchEditorIdx(filname: string): integer;
    function SearchEditorIdxByTab(tabName: string): integer;
    function SelectEditor(filname: string): boolean;
    procedure SelectNextEditor;
    procedure SelectPrevEditor;
    function HasFocus: boolean;
    procedure SetFocus; override;
  public  //Administración de archivos
    tmpPath: string;  //ruta usada para crear archivos temporales para los editores
    OnChangeEditorState: TSynEditorEvent;
    OnChangeFileInform: procedure of object;
    OnSelectEditor: procedure of object;  //Cuando cambia la selección de editor
    property Modified: boolean read GetModified;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    procedure NewFile;
    function LoadFile(fileName: string): boolean;
    function SelectOrLoad(fileName: string): boolean;
    function SelectOrLoad(const srcPos: TSrcPos; highlightLine: boolean): boolean;
    procedure SaveFile;
    procedure SaveAll;
    function OpenDialog: boolean;
    function SaveAsDialog: boolean;
    procedure CloseEditor;
    function CloseAll: boolean;
    procedure LoadLastFileEdited;
  private  //Manejo de menús recientes
    mnRecents   : TMenuItem;  //Menú de archivos recientes
    RecentFiles : TStringList;  //Lista de archivos recientes
    MaxRecents  : integer;    //Máxima cantidad de archivos recientes
    procedure RecentClick(Sender: TObject);
    procedure ActualMenusReciente(Sender: TObject);
    procedure AgregArcReciente(arch: string);
  public //Inicialización
    procedure InitMenuRecents(menRecents0: TMenuItem; RecentList: TStringList;
      MaxRecents0: integer = 5);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetLanguage(idLang: string);
  end;

implementation
{$R *.lfm}
const
  MIN_WIDTH_TAB = 50;  //Ancho por defecto de la lengueta
  FONT_TAB_SIZE = 9;
  SEPAR_TABS = 2;  //Separación adicional, entre pestañas

{ TSynFacilComplet2 }
var
  MSG_NOFIL: string;
  MSG_MODIF: string;
  MSG_PASFI: string;
  MSG_ALLFI: string;

function TSynFacilComplet2.IsKeyword(const AKeyword: string): boolean;
{Esta rutina es llamada por el Markup, que resalta palabras iguales. Se implementa
para evitar que se resalten palabras muy conocidas}
begin
  //Para el lenguaje Pascal, esta rutina funciona bien
  case UpCase(AKeyword) of
  'CONS','VAR','TYPE','BEGIN','END','IF','THEN','ELSE','WHILE',
  'DO','REPEAT','UNTIL','FOR','TO','AND','OR','XOR','NOT','DIV','MOD','IN':
    exit(true)
  else
    exit(false);
  end;
end;
{ TSynEditor }
procedure TSynEditor.AddCursorPos(x, y: integer);
{Agrega una posición de cursor al areglo CursorPos[]}
var
  n: Integer;
begin
  n := high(CursorPos) + 1;
  setlength(CursorPos, n + 1);
  CursorPos[n].x := x;
  CursorPos[n].y := y;
end;
procedure TSynEditor.ExploreLine(lin: string; x1, x2, y: integer);
var
  xtok: Integer;
  tok: String;
begin
  if x2<x1 then exit;  //no es válido
  //Hay una línea que debe ser explorada
//  linAfec := system.Copy(lin, x1, x2-x1+1);  //La parte afectada
//  DebugLn('--Lin=%s x1=%d,x2=%d', [lin, x1, x2]);
  hl.SetLine(lin, 0);
  while not hl.GetEol do begin
    tok := hl.GetToken; //lee el token
    //TokenAtt := hl.GetTokenAttribute;  //lee atributo
    xtok := hl.GetX;
    if (xtok>=x1) and (xtok<x2) then begin
      if firstTok then begin
        //El primer token es la palabra a buscar
        tokFind := UpCase(tok);   //guarda palabra
        firstTok := false;
        AddCursorPos(xtok, y);   //guarda coordenadas
      end else begin
        //Es una búsqueda normal
        if UpCase(tok) = tokFind then begin
          //Guarda solo si coincide con el primer token
         AddCursorPos(xtok, y);   //guarda coordenadas
        end;
      end;
    end;
    //pasa al siguiente
    hl.Next;
  end;
//    MsgBox(lin);
end;
procedure TSynEditor.ExploreForSyncro;
{Explora el texto seleccionado, para habilitar la edición sincronizada, con múltiples
cursores.}
var
  row: LONG;
  x1, x2: integer;
  lin: String;
begin
  setlength(cursorPos,0);   //limpia para guardar posiciones
  firstTok := true;   //Para la búsqeuda correcta
  for row := ed.BlockBegin.y to ed.BlockEnd.y do begin
    lin := ed.Lines[row-1];  //linea en la selección
    //Calcula coordenadas de la línea afectada
    if row=ed.BlockBegin.y then begin
      //Línea inicial
      if row = ed.BlockEnd.y then begin
        //Es también la línea final
        x1 := ed.BlockBegin.x;
        x2 := ed.BlockEnd.x-1;
      end else begin
        //Es solo línea inicial
        x1 := ed.BlockBegin.x;
        x2 := length(lin);
      end;
    end else if row=ed.BlockEnd.y then begin
      //Línea final
      x1 := 1;
      x2 := ed.BlockEnd.x-1;
    end else begin
      //Línea interior
      x1 := 1;
      x2 := length(lin);
    end;
    //Explora
    ExploreLine(lin, x1, x2, row);
  end;
end;
procedure TSynEditor.SetCursors;
var
  i: Integer;
begin
  if high(cursorPos)<0 then exit;
//  ed.CommandProcessor(ecPluginMultiCaretClearAll, '', nil);
  for i:= high(cursorPos) downto 0 do begin
    //Explora la revés para dejar el último cursor al inicio del texto
    if i = 0 then begin
      //El último
      ed.CaretY := cursorPos[i].y;   //primero la fila
      ed.CaretX := cursorPos[i].x;
      ed.ExecuteCommand(ecPluginMultiCaretSetCaret, '', nil);
//    ed.CommandProcessor(ecPluginMultiCaretSetCaret, '', nil);
    end else begin
      ed.CaretY := cursorPos[i].y;   //primero la fila
      ed.CaretX := cursorPos[i].x;
//    ed.ExecuteCommand(ecPluginMultiCaretSetCaret, '', nil);
      ed.CommandProcessor(ecPluginMultiCaretSetCaret, '', nil);
    end;
  end;
end;
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
procedure TSynEditor.edKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lexState: TFaLexerState;
begin
  if (Shift = [ssCtrl]) and (Key = VK_J) then begin
    //Exploramos el texto usando el resaltador
    //Utilizaremos el mismo resaltador
    lexState := hl.State; //Guarda, por si acaso, el estado del elxer
    ExploreForSyncro;     //Explora selección
    hl.State := lexState; //recupera estado
    SetCursors;           //Coloca los cursores
//    ed.CommandProcessor(ecSelWordRight, '', nil);
  end;
  if Key = VK_ESCAPE then begin
    //Cancela una posible edición de múltiples cursores
    ed.CommandProcessor(ecPluginMultiCaretClearAll, '', nil);
  end;
  //Pasa el evento
  if OnKeyDown <> nil then OnKeyDown(Sender, Key, Shift);
end;
function TSynEditor.SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
begin
  SaveDialog1.Filter := MSG_PASFI + '|*.pas|' + MSG_ALLFI + '|*.*';
  SaveDialog1.DefaultExt := '.pas';
  Result := inherited SaveAsDialog(SaveDialog1);
  if Result then exit;
  //Se ha cambiado el nombre del archivo. Actualiza.
  Caption := ExtractFileName(NomArc);
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
    resp := MessageDlg('', Format(MSG_MODIF, [ExtractFileName(NomArc)]),
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
//  inherited Create(SynEdit, nomDef0, extDef0);
  ////////////// Código modificado del constructor /////////////
  ed := SynEdit;
  hl := TSynFacilComplet2.Create(ed.Owner);  //crea resaltador
  hl.SelectEditor(ed);  //inicia
  //intercepta eventos
  ed.OnChange:=@edChange;   //necesita interceptar los cambios
  ed.OnStatusChange:=@edStatusChange;
  ed.OnMouseDown:=@edMouseDown;
  ed.OnKeyUp:=@edKeyUp;     //para funcionamiento del completado
  ed.OnKeyDown:=@edKeyDown;
  ed.OnKeyPress:=@edKeyPress;
  ed.OnUTF8KeyPress:=@edUTF8KeyPress;
  ed.OnCommandProcessed:=@edCommandProcessed;  //necesita para actualizar el cursor
//  RecentFiles := TStringList.Create;
  MaxRecents := 1;   //Inicia con 1
  //guarda parámetros
  nomDef := nomDef0;
  extDef := extDef0;
  NewFile;   //Inicia editor con archivo vacío
  ///////////////////////////////////////////////////////////////
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
  InicEditorC1(SynEdit);
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
procedure TfraEditView.SetLanguage(idLang: string);
begin
  lang := idLang;
  curLang := idLang;
  {$I ..\language\tra_FrameEditView.pas}
end;
procedure TfraEditView.RefreshTabs;
begin
  if editors.Count = 1 then begin
    //No vale la pena
    Panel1.Visible := false;
  end else begin
    Panel1.Visible := true;
  end;
  Panel1.Invalidate;   //para refrescar
end;
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
  RefreshTabs;
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
      TabIndex := i;  //Selecciona
      if Shift = [ssRight] then begin
        PopUpTabs.PopUp;
      end else if Shift = [ssMiddle] then begin
        //Cerrar el archivo
        CloseEditor;
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
function TfraEditView.NewName: string;
{Genera un nombre de archivo que no se repita enter las pestañas.}
var
  n: Integer;
begin
  n := 0;
  repeat
    inc(n);
    Result := 'NewFile' + IntToStr(n);
  until SearchEditorIdxByTab(Result)=-1;
end;
function TfraEditView.AddEdit(ext: string): TSynEditor;
{Agrega una nueva ventana de eición a la vista, y devuelve la referencia.}
var
  ed: TSynEditor;
  n: Integer;
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
  //Configura el borrado de la palabra actual
  n := 46;
  ed.SynEdit.Keystrokes.BeginUpdate;
  ed.SynEdit.Keystrokes[n].Key := VK_DELETE;
  ed.SynEdit.Keystrokes[n].Shift := [ssCtrl];
  ed.SynEdit.Keystrokes[n].ShiftMask := [];
  ed.SynEdit.Keystrokes.EndUpdate;
  //Deshabilita Ctrl+N
  n := ed.SynEdit.Keystrokes.FindCommand(ecInsertLine);
  ed.SynEdit.Keystrokes[n].ShortCut := 0;   //Esto debe dehabilitarlo
  n := 84;
  ed.SynEdit.Keystrokes.BeginUpdate;
  ed.SynEdit.Keystrokes[n].Key := VK_SUBTRACT;
  ed.SynEdit.Keystrokes[n].Shift := [ssShift,ssAlt];
  ed.SynEdit.Keystrokes[n].ShiftMask := [];
  ed.SynEdit.Keystrokes.EndUpdate;
  //Configura el desplegado con Alt+Shift+"+"
  n := 85;
  ed.SynEdit.Keystrokes.BeginUpdate;
  ed.SynEdit.Keystrokes[n].Key := VK_ADD;
  ed.SynEdit.Keystrokes[n].Shift := [ssShift,ssAlt];
  ed.SynEdit.Keystrokes[n].ShiftMask := [];
  ed.SynEdit.Keystrokes.EndUpdate;

  //Configura múltiples cursores
  fMultiCaret := TSynPluginMultiCaret.Create(self);
  with fMultiCaret do begin
    Editor := ed.SynEdit;
    with KeyStrokes do begin
      with Add do begin
        Command    := ecPluginMultiCaretSetCaret;
        Key        := VK_INSERT;
        Shift      := [ssShift, ssCtrl];
        ShiftMask  := [ssShift,ssCtrl,ssAlt];
      end;
      with Add do begin
        Command    := ecPluginMultiCaretUnsetCaret;
        Key        := VK_DELETE;
        Shift      := [ssShift, ssCtrl];
        ShiftMask  := [ssShift,ssCtrl,ssAlt];
      end;
    end;
  end;

  ed.Caption := NewName + ext;   //Pone nombre diferente
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
    //Había al menos 2
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
  RefreshTabs;
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
function TfraEditView.SearchEditorIdxByTab(tabName: string): integer;
var
  ed: TSynEditor;
  i: integer;
begin
  for i:=0 to editors.Count-1 do begin
    ed := editors[i];
    if Upcase(ed.Caption) = UpCase(tabName) then exit(i);
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
  ed := AddEdit('.pas');
  ed.NomArc := tmpPath + DirectorySeparator + ed.Caption;
  ed.LoadSyntaxFromFile(rutApp + 'PicPas_PIC16.xml');
end;
function TfraEditView.LoadFile(fileName: string): boolean;
//Carga un archivo en el editor. Si encuentra algún error. Devuelve FALSE.
var
  ed: TSynEditor;
  ext: string;
begin
  Result := true;   //por defecto
  if SelectEditor(filename) then exit; //Ya estaba abierto
  ed := AddEdit('');   //Dispara OnSelecEditor
  if Pos(DirectorySeparator, fileName) = 0 then begin
    //Es ruta relativa, la vuelve abosulta
    fileName := rutApp + fileName;
  end;
  ed.LoadFile(fileName);
  if ed.Error='' then begin
    AgregArcReciente(fileName);
  end else begin
    Result := false;  //Hubo error
  end;
  //Carga la sintaxis apropiada
  ext := ExtractFileExt(fileName);
  case Upcase(ext) of
  '.PAS': ed.LoadSyntaxFromFile('PicPas_PIC16.xml');
  end;
  //ed.LoadSyntaxFromPath;  //para que busque el archivo apropiado
  ed.Caption := ExtractFileName(fileName);
  {Dispara otra vez, para actualizar bien el nombre del archivo, en el Caption de la
  ventana principal.}
  if OnSelectEditor<>nil then OnSelectEditor;
end;
function TfraEditView.SelectOrLoad(fileName: string): boolean;
{Selecciona la ventana del editor que contiene al archivo solicitado. Si no lo tiene
abierto, lo intenta abrir. Si falla, devuelve FALSE}
begin
  //Se ha identificado el archivo con el error
  if SelectEditor(filename) then begin
    //Lo tenía abierto.
    exit(true);
  end else begin
    //No está abierto el archivo, lo abrimos
    Result := LoadFile(filename);
  end;
end;
function TfraEditView.SelectOrLoad(const srcPos: TSrcPos; highlightLine: boolean): boolean;
//Versión de SelectOrLoad(), que además posiciona el cursor en la coordenada indicada
begin
  Result := SelectOrLoad(srcPos.fil);
  if Result then begin
    if (srcpos.row>=0) and (srcpos.col>=0)  then begin
      //posiciona curosr
      ActiveEditor.SynEdit.CaretX := srcPos.col;
      ActiveEditor.SynEdit.CaretY := srcPos.row;
      //Define línea con error
      if highlightLine then ActiveEditor.linErr := srcPos.row;
      ActiveEditor.SynEdit.Invalidate;  //refresca
      SetFocus;
    end;
  end;
end;
function TfraEditView.OpenDialog: boolean;
//Muestra el cuadro de diálogo para abrir un archivo. Si hay error devuelve FALSE.
var arc0: string;
begin
  OpenDialog1.Filter:= MSG_PASFI + '|*.pas|' + MSG_ALLFI + '|*.*';
  if not OpenDialog1.Execute then exit(true);    //se canceló
  arc0 := OpenDialog1.FileName;
  LoadFile(arc0);  //legalmente debería darle en UTF-8
  AgregArcReciente(arc0);
  Result := true;   //sale sin incidencias
end;
procedure TfraEditView.SaveFile;
//Guarda el editor actual
begin
  if ActiveEditor=nil then exit;
  if ActiveEditor.NomArc='' then begin
    //Es un archivo nuevo
    ActiveEditor.SaveAsDialog(SaveDialog1);
  end else begin
    ActiveEditor.SaveFile;
  end;
  //Actualiza por si acaso, era un archivo nuevo
  AgregArcReciente(ActiveEditor.NomArc);
end;
procedure TfraEditView.SaveAll;
{Guarda todas las ventanas abiertas en el editor.}
var
  i: Integer;
begin
  for i:=0 to editors.Count-1 do begin
    if editors[i].Modified then begin
      //Actualiza por si acaso, era un archivo nuevo
      AgregArcReciente(editors[i].NomArc);
    end;
    if editors[i].NomArc<>'' then begin
      //No deberái pasar que el archivo esté sin nombre.
      editors[i].SaveFile;
    end;
  end;
end;
function TfraEditView.SaveAsDialog: boolean;
{Muestra la ventana para grabar un archivo. Si se cancela, devuelve TRUE.}
begin
  if ActiveEditor=nil then exit;
  Result := ActiveEditor.SaveAsDialog(SaveDialog1);
  if Result then exit;   //se canceló
  if OnSelectEditor<>nil then OnSelectEditor;
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
procedure TfraEditView.LoadLastFileEdited;
{Carga el último archivo de la lista de recientes}
begin
  if mnRecents.Count = 0 then exit;
  ActualMenusReciente(self);
  mnRecents.Items[0].Click;
end;
procedure TfraEditView.RecentClick(Sender: TObject);
//Se selecciona un archivo de la lista de recientes
var
  cap: string;
begin
  cap := TMenuItem(Sender).Caption;
  LoadFile(MidStr(cap, 4,150));
end;
procedure TfraEditView.ActualMenusReciente(Sender: TObject);
{Actualiza el menú de archivos recientes con la lista de los archivos abiertos
recientemente. }
var
  i: Integer;
begin
  if mnRecents = nil then exit;
  if RecentFiles = nil then exit;
  //proteciión
  if RecentFiles.Count = 0 then begin
    mnRecents[0].Caption := MSG_NOFIL;
    mnRecents[0].Enabled:=false;
    for i:= 1 to mnRecents.Count-1 do begin
      mnRecents[i].Visible:=false;
    end;
    exit;
  end;
  //hace visible los ítems
  mnRecents[0].Enabled:=true;
  for i:= 0 to mnRecents.Count-1 do begin
    if i<RecentFiles.Count then
      mnRecents[i].Visible:=true
    else
      mnRecents[i].Visible:=false;
  end;
  //pone etiquetas a los menús, incluyendo un atajo numérico
  for i:=0 to RecentFiles.Count-1 do begin
    mnRecents[i].Caption := '&'+IntToStr(i+1)+' '+RecentFiles[i];
  end;
end;
procedure TfraEditView.AgregArcReciente(arch: string);
//Agrega el nombre de un archivo reciente
var hay: integer; //bandera-índice
    i: integer;
begin
  if RecentFiles = nil then exit;
  //verifica si ya existe
  hay := -1;   //valor inicial
  for i:= 0 to RecentFiles.Count-1 do
    if RecentFiles[i] = arch then hay := i;
  if hay = -1 then  //no existe
    RecentFiles.Insert(0,arch)  //agrega al inicio
  else begin //ya existe
    RecentFiles.Delete(hay);     //lo elimina
    RecentFiles.Insert(0,arch);  //lo agrega al inicio
  end;
  while RecentFiles.Count>MaxRecents do  //mantiene tamaño máximo
    RecentFiles.Delete(MaxRecents);
end;
//Inicialización
procedure TfraEditView.InitMenuRecents(menRecents0: TMenuItem; RecentList: TStringList;
      MaxRecents0: integer=5);
//Configura un menú, con el historial de los archivos abiertos recientemente
//"nRecents", es el número de archivos recientes que se guardará
var
  i: Integer;
begin
  mnRecents := menRecents0;
  RecentFiles := RecentList;  //gaurda referencia a lista
  MaxRecents := MaxRecents0;
  //configura menú
  mnRecents.OnClick:=@ActualMenusReciente;
  for i:= 1 to MaxRecents do begin
    AddItemToMenu(mnRecents, '&'+IntToStr(i), @RecentClick);
  end;
end;
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
