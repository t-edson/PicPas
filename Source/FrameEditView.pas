{Frame que contiene a una vista para mostrar lso editores.}
unit FrameEditView;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, Dialogs, ComCtrls, ExtCtrls,
  SynEdit, Graphics, LCLProc, Menus, LCLType, StdCtrls, strutils, fgl,
  SynEditMiscClasses, SynEditKeyCmds, SynPluginMultiCaret,
  Globales, SynFacilUtils, SynFacilCompletion, SynFacilHighlighter,
  MisUtils, XpresBas;
type

  { TSynFacilComplet2 }
  //Versión personalizada de  TSynFacilComplet
  TSynFacilComplet2 = class(TSynFacilComplet)
    function IsKeyword(const AKeyword: string): boolean; override;
  end;

  { TSynEditor }
  {Versión personalizada de TSynFacilEditor, que usa TSynFacilComplet2, como resaltador}
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
    x1      : integer;  //Coordenada inicial de dibujo
    tabWidth: integer;  //ancho de lengueta
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
    ImgCompletion: TImageList;
    lblBackground: TLabel;
    mnCloseAll: TMenuItem;
    mnNewTab: TMenuItem;
    mnCloseTab: TMenuItem;
    mnNewTab1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopUpTabs: TPopupMenu;
    SaveDialog1: TSaveDialog;
    UpDown1: TUpDown;
    procedure FrameResize(Sender: TObject);
    procedure mnCloseAllClick(Sender: TObject);
    procedure mnCloseTabClick(Sender: TObject);
    procedure mnNewTabClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private  //Métodos para dibujo de las lenguetas
    xIniTabs : integer;  //Coordenada inicial desde donde se dibujan las lenguetas
    tabDrag  : integer;
    tabSelec : integer;
    procedure AddListUnits(OpEve: TFaOpenEvent);
    procedure ConfigureSyntax(ed: TSynEditor; Complete: boolean = true);
    procedure evoLoadItems(opEve: TFaOpenEvent; curEnv: TFaCursorEnviron;
                           out Cancel: boolean);
    procedure MakeActiveTabVisible;
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure RefreshTabs;
    procedure SetTabIndex(AValue: integer);
    procedure DibLeng(edi: TSynEditor; coltex: TColor; Activo: boolean; txt: string
      );   //dibuja una lengueta
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdateX1CoordTabs;
    procedure Panel1Paint(Sender: TObject);
    procedure InitTabs;
  private
    FTabIndex  : integer;
    editors    : TEditorList;
    FTabViewMode: integer;
    lang       : string;
    fMultiCaret: TSynPluginMultiCaret;
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetModified: boolean;
    function LastIndex: integer;
    function NewName(ext: string): string;
    function AddEdit(ext: string): TSynEditor;
    procedure DeleteEdit;
    procedure SetTabViewMode(AValue: integer);
  public  //Manejo de pestañas
    property TabViewMode: integer read FTabViewMode write SetTabViewMode;  //Modo de visualización
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
    OnRequireSynEditConfig: procedure(ed: TsynEdit) of object;
    property Modified: boolean read GetModified;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    procedure Undo;
    procedure Redo;
    procedure SelectAll;
    procedure NewFile;
    function LoadFile(fileName: string): boolean;
    function SelectOrLoad(fileName: string): boolean;
    function SelectOrLoad(const srcPos: TSrcPos; highlightLine: boolean): boolean;
    procedure SaveFile;
    procedure SaveAll;
    function OpenDialog: boolean;
    function SaveAsDialog: boolean;
    function CloseEditor: boolean;
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
    procedure UpdateSynEditConfig;
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
  MSG_NOFILES: string;
  MSG_MODIFSAV: string;
  MSG_PASFILES: string;
  MSG_ALLFILES: string;
  MSG_NOSYNFIL: string;

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
  row: integer;
  xl1, xl2: integer;
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
        xl1 := ed.BlockBegin.x;
        xl2 := ed.BlockEnd.x-1;
      end else begin
        //Es solo línea inicial
        xl1 := ed.BlockBegin.x;
        xl2 := length(lin);
      end;
    end else if row=ed.BlockEnd.y then begin
      //Línea final
      xl1 := 1;
      xl2 := ed.BlockEnd.x-1;
    end else begin
      //Línea interior
      xl1 := 1;
      xl2 := length(lin);
    end;
    //Explora
    ExploreLine(lin, xl1, xl2, row);
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
  SaveDialog1.Filter := MSG_PASFILES + '|*.pas|' + MSG_ALLFILES + '|*.*';
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
    resp := MessageDlg('', Format(MSG_MODIFSAV, [ExtractFileName(NomArc)]),
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
  SynEdit.Options := SynEdit.Options + [eoTabsToSpaces];  //permite indentar con <Tab>

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
  if FTabViewMode = 0 then begin
    //Se muestran siempre
    Panel1.Visible := true;
  end else if FTabViewMode = 1 then begin
    //Se oculta, cuando hay una sola pestaña
    if editors.Count = 1 then begin
      //No vale la pena
      Panel1.Visible := false;
    end else begin
      Panel1.Visible := true;
    end;
  end else begin
    //Se oculta siempre
    Panel1.Visible := false;
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
procedure TfraEditView.DibLeng(edi: TSynEditor; coltex: TColor; Activo: boolean;
  txt: string);
var
  x1, x2: integer;

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
  //Lee coordenadas horizontales
  x1 := edi.x1;
  x2 := edi.x1 + edi.tabWidth;
  alto := panel1.Height;
  y1 := 0;
  y2 := y1 + alto;
  //Inicia dibujo
  cv := Panel1.canvas;
  cv.Font.Size:= FONT_TAB_SIZE;
  cv.Font.Color := clBlack;
  cv.Font.Color := coltex;   //Color de texto
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
  x2, i: Integer;
  edi: TSynEditor;
begin
  {Se asuma que las lenguetas ya tienen su coordenada x1, actualizada, porque ya
  han sido dibujadas, así que no llamaremos a UpdateX1CoordTabs.}
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    x2 := edi.x1 + edi.tabWidth;
    if (X>edi.x1) and (X<x2) then begin
      TabIndex := i;  //Selecciona
      if Shift = [ssRight] then begin
        PopUpTabs.PopUp;
      end else if Shift = [ssMiddle] then begin
        //Cerrar el archivo
        CloseEditor;
      end else if Shift = [ssLeft] then begin
        //Solo selección
        MakeActiveTabVisible;
        //Inicia el arrastre
        Panel1.BeginDrag(false, 10);
        tabDrag := i;  //gaurda el índice del arrastrado
      end;
      exit;
    end;
  end;
end;
procedure TfraEditView.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
//  debugln('Move');
end;
procedure TfraEditView.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  edi: TSynEditor;
  x2, i, x2Mid: Integer;
begin
  Accept := true;
  //Ve a cual lengüeta selecciona
  tabSelec := -1;
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    x2Mid := edi.x1 + edi.tabWidth div 2;
    x2 := edi.x1 + edi.tabWidth;
    if (X>edi.x1) and (X<x2) then begin
      if X<x2Mid then begin
        //Está en la primera mitad.
        tabSelec := i;  //Selecciona
      end else begin
        //En la mitad final, selecciona el siguiente
        tabSelec := i+1;  //Selecciona
      end;
    end;
  end;
  //Genera marca en la lengüeta
  if tabSelec<>-1 then begin
//    debugln('leng selec: %d', [tabselec]);
    Panel1.Invalidate;
  end;
end;
procedure TfraEditView.Panel1EndDrag(Sender, Target: TObject; X, Y: Integer);
{Se termina el arrastre, sea que se soltó en alguna parte, o se canceló.}
begin
  tabSelec := -1;
  Panel1.Invalidate;
end;
procedure TfraEditView.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
{Se soltó la lengueta en el panel.}
begin
  if TabIndex<0 then exit;
  if tabSelec<0 then exit;
  //Corrección
  if tabSelec>TabIndex then tabSelec := tabSelec-1;
  if tabSelec>editors.Count-1 then exit;
//  debugln('Panel1DragDrop: %d a %d', [TabIndex, tabSelec]);
  editors.Move(TabIndex, tabSelec);
  TabIndex := tabSelec;
  Panel1.Invalidate;
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
procedure TfraEditView.UpdateX1CoordTabs;
{Actualiza la coordenada x1, de las lenguetas, considerando el valor actual de
"xIniTabs". El valor x1, representa la coordenada en que se dibuajaría la lengueta.}
var
  i, x1: integer;
  edi: TSynEditor;
begin
  {Este algoritmo debe ser igual a Panel1Paint(), para que no haya inconsistencias.}
  x1 := xIniTabs;
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    edi.x1 := x1;   //Actualiza coordenada
    //Calcula siguiente coordenada
    x1 := x1 + edi.tabWidth + SEPAR_TABS;
  end;
end;
procedure TfraEditView.MakeActiveTabVisible;
{Configura "xIniTabs", de modo que haga visible la pestaña del editor activo.}
var
  x1, x2: integer;
begin
  if Count=0 then exit;
  UpdateX1CoordTabs;
  x1 := ActiveEditor.x1;
  x2 := ActiveEditor.x1 + ActiveEditor.tabWidth;
  if x2 > self.Width then begin
    //Pestaña sale de página, por la derecha
    xIniTabs := xIniTabs - (x2-self.Width);
  end else if x1 < Panel2.Width then begin
    //Pestaña sale de página, por la izquierda
    xIniTabs := xIniTabs + (Panel2.Width - x1);
  end else begin
//    debugln('Pestaña se dibuja adentro');
  end;
end;
procedure TfraEditView.Panel1Paint(Sender: TObject);
var
  i, x1: Integer;
  edi: TSynEditor;
  cv: TCanvas;
begin
  //Actualzia coordenadas
  UpdateX1CoordTabs;
  //Dibuja las pestañas
  for i := 0 to editors.Count-1 do begin
    edi := editors[i];
    if i <> TabIndex then begin
      //Dibuja todo menos al activo, lo deja para después.
      DibLeng(edi, clBlack, false, edi.Caption);
    end;
  end;
  //Dibuja al final al activo, para que aparezca encima
  if TabIndex<>-1 then begin
    edi := editors[TabIndex];
    DibLeng(edi, clBlack, true, edi.Caption);
  end;
  //Dibuja la marca de movimiento de lengüeta
  if (tabSelec>=0) and (tabSelec<editors.Count) then begin
    edi := editors[tabSelec];
    x1 := edi.x1+2;
    cv := Panel1.canvas;
    cv.Pen.Width := 5;
    cv.Pen.Color := clGray;
    cv.Line(x1 ,0, x1, Panel1.Height);
  end else if tabSelec = editors.Count then begin
    //Se marac al final de la última pestaña
    edi := editors[editors.Count-1];  //el útlimo
    x1 := edi.x1 + edi.tabWidth +2;
    cv := Panel1.canvas;
    cv.Pen.Width := 5;
    cv.Pen.Color := clGray;
    cv.Line(x1 ,0, x1, Panel1.Height);
  end;
end;
procedure TfraEditView.InitTabs;
{Inicia el dibujo de las lenguetas}
begin
  xIniTabs := panel2.Width;  //Empeiza dibujando al lado de las flechas
  Panel1.OnMouseMove := @Panel1MouseMove;
  panel1.OnMouseDown := @Panel1MouseDown;
  panel1.OnMouseUp   := @Panel1MouseUp;
  panel1.OnDragOver := @Panel1DragOver;
  panel1.OnDragDrop := @Panel1DragDrop;
  panel1.OnEndDrag := @Panel1EndDrag;
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
function TfraEditView.NewName(ext: string): string;
{Genera un nombre de archivo que no se repita enter las pestañas abiertas.}
var
  n: Integer;
begin
  n := 0;
  repeat
    inc(n);
    Result := 'NewFile' + IntToStr(n) + ext;
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

  ed.Caption := NewName(ext);   //Pone nombre diferente
  ed.NomArc := '';  //Pone sin nombre para saber que no se ha guardado
  if OnRequireSynEditConfig<>nil then  //Configura
    OnRequireSynEditConfig(ed.SynEdit);
  editors.Add(ed);   //agrega a la lista
  TabIndex := LastIndex;
  //Configura desplazamiento para asegurarse que la pestaña se mostrará visible
  MakeActiveTabVisible;
  //Actualiza referencias
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
    FrameResize(self);  //para ubciar mensaje de fondo
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
  MakeActiveTabVisible;
  if OnSelectEditor<>nil then OnSelectEditor;
  RefreshTabs;
end;
procedure TfraEditView.SetTabViewMode(AValue: integer);
begin
  if FTabViewMode = AValue then Exit;
  FTabViewMode := AValue;
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
  MakeActiveTabVisible;
end;
procedure TfraEditView.SelectPrevEditor;
{Selecciona al editor anterior.}
begin
  if Count = 0 then exit;
  if TabIndex=-1 then exit;
  if TabIndex = 0 then TabIndex := LastIndex else TabIndex := TabIndex -1;
  SetFocus;
  MakeActiveTabVisible;
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
procedure TfraEditView.Undo;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit;
  //Busca editor actual
  ed := ActiveEditor;
  ed.Undo;
end;
procedure TfraEditView.Redo;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit;
  //Busca editor actual
  ed := ActiveEditor;
  ed.Redo;
end;
procedure TfraEditView.SelectAll;
var
  ed: TSynEditor;
begin
  if editors.Count = 0 then exit;
  //Busca editor actual
  ed := ActiveEditor;
  ed.SelectAll;
end;

procedure TfraEditView.evoLoadItems(opEve: TFaOpenEvent;
  curEnv: TFaCursorEnviron; out Cancel: boolean);
begin
  opEve.ClearAvails;
debugln('evoLoadItems');
  opEve.AddAvail('alfa');
  opEve.AddAvail('unicorn');
  opEve.AddAvail('usame');
  opEve.AddAvail('beta');

  Cancel := true;
end;
procedure TfraEditView.AddListUnits(OpEve: TFaOpenEvent);
{Agrega la lista de unidades disponibles, a la lista Items[] de un Evento de apertura.}
var
  directorio, nomArc: String;
  SearchRec: TSearchRec;
begin
  if OpEve=nil then exit;
  directorio := rutUnits;
  if FindFirst(directorio + '\*.pas', faDirectory, SearchRec) = 0 then begin
    repeat
      nomArc := SysToUTF8(SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then begin
        //directorio
      end else begin //archivo
        //Argega nombre de archivo
        nomArc := copy(nomArc, 1, length(nomArc)-4);
        opEve.AddItem(nomArc, -1);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;
procedure TfraEditView.ConfigureSyntax(ed: TSynEditor; Complete: boolean = true);
var
  opEve: TFaOpenEvent;
  synFile: String;
  ext: string;
begin
  ext := ExtractFileExt(ed.NomArc);
  case Upcase(ext) of
  '.PAS': begin
      //Es Pascal
      synFile := rutSyntax + DirectorySeparator + 'PicPas_PIC16.xml';
      if not FileExists(synFile) then begin
        MsgErr(MSG_NOSYNFIL, [synFile]);
        exit;
      end;
      ed.LoadSyntaxFromFile(synFile);
      if Complete then begin
        //Configura eventos de apertura.
        opEve := ed.hl.FindOpenEvent('unit1');
        opEve.ClearItems;
        AddListUnits(opEve);  //Configura unidades disponibles
      //    opEve.OnLoadItems := @evoLoadItems;

        opEve := ed.hl.FindOpenEvent('unit2');
        opEve.ClearItems;
        AddListUnits(opEve);  //Configura unidades disponibles
      //    opEve.OnLoadItems := @evoLoadItems;

        opEve := ed.hl.FindOpenEvent('unit3');
        opEve.ClearItems;
        AddListUnits(opEve);  //Configura unidades disponibles
      //    opEve.OnLoadItems := @evoLoadItems;
      end;
    end;
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
  ConfigureSyntax(ed);
end;
function TfraEditView.LoadFile(fileName: string): boolean;
//Carga un archivo en el editor. Si encuentra algún error. Devuelve FALSE.
var
  ed: TSynEditor;
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
  ConfigureSyntax(ed);
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
      ActiveEditor.SynEdit.CaretY := srcPos.row;
//      ActiveEditor.SynEdit.CaretX := srcPos.col;
      ActiveEditor.SynEdit.LogicalCaretXY := Point(srcPos.col, srcPos.row);
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
  OpenDialog1.Filter:= MSG_PASFILES + '|*.pas|' + MSG_ALLFILES + '|*.*';
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
function TfraEditView.CloseEditor: boolean;
{Cierra el editor actual. Si se cancela el mensaje de "Grabar", devuelve FALSE}
begin
  if ActiveEditor=nil then exit(true);
  if ActiveEditor.SaveQuery(SaveDialog1) then
    exit(false);  //cancelado
  DeleteEdit;
  exit(true);
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
    mnRecents[0].Caption := MSG_NOFILES;
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
procedure TfraEditView.UpdateSynEditConfig;
{India que se desea cambiar la configuración de los SynEdit.}
var
  i: Integer;
begin
  //Pide configuración para todos los editores abiertos
  for i:=0 to editors.Count-1 do begin
    if OnRequireSynEditConfig<>nil then begin
      OnRequireSynEditConfig(editors[i].SynEdit);
    end;
    //Actualiza resaltador
    ConfigureSyntax(editors[i], false);
  end;
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
  tabSelec := -1;
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
procedure TfraEditView.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
  btNext: SelectNextEditor;
  btPrev: SelectPrevEditor;
  end;
end;
procedure TfraEditView.mnCloseTabClick(Sender: TObject);
begin
  CloseEditor;
  SetFocus;
end;
procedure TfraEditView.mnCloseAllClick(Sender: TObject);
begin
  while self.Count>0 do begin
    if not CloseEditor then
      break;  //Se canceló
  end;
  SetFocus;
end;
procedure TfraEditView.FrameResize(Sender: TObject);
begin
  //Configura ubciacio de etiquetas
  if Count>0 then exit;   //Está oculto
  lblBackground.Left := self.Width div 2 - lblBackground.Width div 2;
  lblBackground.Top := self.Height div 2;
end;

end.
//92
