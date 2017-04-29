{Frame que contiene a una vista para mostrar lso editores.}
unit FrameEditView;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, ComCtrls, Graphics,
  Dialogs, ExtCtrls, SynFacilUtils, MisUtils, fgl, SynEditMiscClasses;
type
  {Clase derivada de "TSynFacilEditor". Se usa para asociar un SynEdit a un
  TSynFacilEditor}

  { TSynEditor }

  TSynEditor = class(TSynFacilEditor)
  private
    procedure edSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
  public  //Inicialización
    tab: TTabSheet;   //referencia a la pestaña del PageControl
    SynEdit: TSynEdit;  //Editor SynEdit
    constructor Create(AOwner: TComponent; nomDef0, extDef0: string);
    destructor Destroy; override;
  end;

  TEditorList = specialize TFPGObjectList<TSynEditor>;
  TSynEditorEvent = procedure(ed: TSynEditor) of object;
  { TfraEditView }
  TfraEditView = class(TFrame)
    Image1: TImage;
    ImgCompletion: TImageList;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
  private
    curEdit: TSynEditor;
    editors: TEditorList;
    lang: string;
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetModified: boolean;
  public
    OnChangeEditorState: TSynEditorEvent;
    OnChangeFileInform: procedure of object;
    property Modified: boolean read GetModified;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;

    function Count: integer;
    function ActiveEditor: TSynEditor;
    function SearchEditor(filname: string): TSynEditor;
    function SelectEditor(filname: string): boolean;
    function AddEdit: TSynEditor;
    procedure LoadFile(fileName: string);
    function OpenDialog(filter: string): boolean;
    function SaveAsDialog: boolean;
    procedure Refresh;
  public //Inicialización
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetLanguage(lang0: string);
  end;

implementation

{$R *.lfm}

{ TSynEditor }

procedure TSynEditor.edSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
  if Line = self.linErr then begin
      Special := True ;  //marca como línea especial
      Markup.Background := TColor($3030A0); //color de fondo
  end;
end;
constructor TSynEditor.Create(AOwner: TComponent; nomDef0, extDef0: string);
begin
  SynEdit:= TSynEdit.Create(AOwner);// Crea un editor
  inherited Create(SynEdit, nomDef0, extDef0);

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
  LoadSyntaxFromFile('PicPas_PIC16.xml');

end;
destructor TSynEditor.Destroy;
begin
  inherited Destroy;
//  SynEdit.Destroy;   //Se debe destruir al final
end;

{ TfraEditView }

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
function TfraEditView.ActiveEditor: TSynEditor;
{Devuelve el editor SynEditor, activo, es decir el que se encuentra en la lengueta
activa. }
var
//  con: TControl;
//  syned: TSynEdit;
  i: Integer;
begin
  if editors.Count = 0 then exit(nil);
  i := PageControl1.ActivePage.TabIndex;
  Result := editors[i];   //Solo funcionará si no se desordenan las enguetas
//  con := PageControl1.ActivePage.Controls[0];
//  MsgBox(con.ClassName);
//  if (con is TSynEdit) then begin
//    syned := TSynEdit(con);
//
//    MsgErr('SynEdit');
//  end else begin
//    MsgErr('Internal error.');
//    exit(nil);
//  end;
end;
function TfraEditView.SearchEditor(filname: string): TSynEditor;
{Busca entre las ventanas abiertas al archivo indicado. Si no lo encuentra devuelve NIL}
var
  ed: TSynEditor;
begin
  for ed in editors do begin
    if Upcase(ed.NomArc) = UpCase(filname) then exit(ed);
  end;
  exit(nil);
end;
function TfraEditView.SelectEditor(filname: string): boolean;
{Activa el editor que corresponde al archivo indicado. Si no encuentra el archivo,
devuelve FALSE.}
var
  ed: TSynEditor;
  tab: TTabSheet;
begin
  ed := SearchEditor(filname);
  if ed= nil then begin
    exit(false);
  end else begin
    tab := ed.tab;  //ubica lengueta
    PageControl1.ActivePage := tab;  //la activa
    exit(true);
  end;
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

function TfraEditView.Count: integer;
begin
  Result := editors.Count;
end;

function TfraEditView.AddEdit: TSynEditor;
{Agrega una nueva ventana de eición a la vista, y devuelve la referencia.}
var
  ed: TSynEditor;
  tab: TTabSheet;
begin
  tab := PageControl1.AddTabSheet;
  {Crea Editor. Notar que se crea teniendo como Owner a "tab", para que se libere }
  ed := TSynEditor.Create(tab, 'SinNombre', 'pas');
  ed.OnChangeEditorState := @ChangeEditorState;
  ed.OnChangeFileInform := @editChangeFileInform;
  ed.hl.IconList := ImgCompletion;
  ed.SetLanguage(lang);
  //Configura PageControl
  ed.tab := tab;   //actualiza la pestaña
  ed.SynEdit.Parent := tab;  //lo ubica en la pestaña
  ed.SynEdit.Align := alClient;
  tab.Caption := 'XXX';
  //Actualzia referencias
  editors.Add(ed);   //agrega a la lista
  curEdit := ed;     //actualiza referencia  ¿Es necesario? ¿No basta con ver la página activa?
  Result := ed;
  Refresh;
end;
procedure TfraEditView.LoadFile(fileName: string);
//Carga un archivo en el editor
var
  ed: TSynEditor;
begin
  ed := AddEdit;
  ed.LoadFile(fileName);
  ed.LoadSyntaxFromPath;  //para que busque el archivo apropiado
  ed.tab.Caption := fileName;
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
function TfraEditView.SaveAsDialog: boolean;
//Guarda el contenido del editor, permitiendo cambiar el nombre con un diálogo.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0, NomArc: String;
  resp: TModalResult;
begin
  Result := false;
  if not SaveDialog1.Execute then begin  //se canceló
    Result := true;   //Sale con "true"
    exit;    //se canceló
  end;
  arc0 := SaveDialog1.FileName;
  if FileExists(arc0) then begin
    resp := MessageDlg('', dic('El archivo %s ya existe.' + LineEnding +
                  '¿Deseas sobreescribirlo?',[arc0]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if (resp = mrCancel) or (resp = mrNo) then Exit;
  end;
  NomArc := UTF8ToSys(arc0);   //asigna nuevo nombre
  if ExtractFileExt(NomArc) = '' then NomArc += '.' + ActiveEditor.extDef;  //completa extensión
  ActiveEditor.NomArc := NomArc;
  ActiveEditor.SaveFile;   //lo guarda
end;

procedure TfraEditView.Refresh;
{Refresca el frame, para mostrar correctamente a lso editores.}
begin
  if editors.Count=0 then begin
    //No hay editores abiertos
    PageControl1.Visible := false;
  end else if editors.Count = 1 then begin
    //Hay un solo editor
    PageControl1.Visible := true;

  end else begin
    //Hay varios editores
    PageControl1.Visible := true;

  end;
end;

constructor TfraEditView.Create(AOwner: TComponent);
begin
  inherited;
  editors:= TEditorList.Create(true);
  PageControl1.Align := alClient;
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

end.
//92
