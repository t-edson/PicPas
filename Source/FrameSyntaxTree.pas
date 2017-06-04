unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, StdCtrls,
  ComCtrls, Menus, ActnList, ExtCtrls, ComboEx, XpresElementsPIC, Globales,
  FormElemProperty, XpresParserPIC, FormConfig, FrameArcExplor, MisUtils;
type
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    acGenRefres: TAction;
    acGenGoTo: TAction;
    acGenProp: TAction;
    acGenViewGr: TAction;
    acGenViewDec: TAction;
    acGenExpAll: TAction;
    ActionList1: TActionList;
    ComboBoxEx1: TComboBoxEx;
    frmArcExplor1: TfrmArcExplor;
    ImageList1: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnGoTo: TMenuItem;
    mnRefresh: TMenuItem;
    mnProper: TMenuItem;
    mnRefresh1: TMenuItem;
    mnRefresh2: TMenuItem;
    Panel1: TPanel;
    PopupElem: TPopupMenu;
    PopupGroup: TPopupMenu;
    PopupFrame: TPopupMenu;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
    procedure acGenExpAllExecute(Sender: TObject);
    procedure acGenGoToExecute(Sender: TObject);
    procedure acGenRefresExecute(Sender: TObject);
    procedure acGenPropExecute(Sender: TObject);
    procedure acGenViewDecExecute(Sender: TObject);
    procedure acGenViewGrExecute(Sender: TObject);
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    syntaxTree: TXpTreeElements;
    function AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
    procedure frmArcExplor1DoubleClickFile(nod: TExplorNode);
    procedure frmArcExplor1MenuOpenFile(nod: TExplorNode);
    procedure frmElemPropertyExplore(elem: TxpElement);
    procedure RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
    procedure RefreshByGroups(nodMain: TTreeNode; curEle: TxpElement);
    function SelectedIsMain: boolean;
    function SelectedIsGroup: boolean;
    function SelectedIsElement: boolean;
  public
    OnSelectElemen: procedure(var elem: TxpElement) of object;
    OnOpenFile: procedure(filname: string) of object;
    //Se requeire información del archivo actual
    OnReqCurFile: procedure(var filname: string) of object;
    procedure LocateFile(filname: string);
    procedure Init(syntaxTree0: TXpTreeElements);
    procedure Refresh;
    procedure SetLanguage(idLang: string);
  end;

implementation
{$R *.lfm}
var
  TIT_MAIN, TIT_UNIT : string;
  TIT_CONS: String;
  TIT_VARS: String;
  TIT_FUNC: String;
  TIT_OTHER: String;

{ TfraSyntaxTree }
procedure TfraSyntaxTree.SetLanguage(idLang: string);
{Fija el lenguaje de acuerdo al valor de Globales.curLang}
begin
  curLang := idLang;
  {$I ..\language\tra_FrameSyntaxTree.pas}
  Refresh;
end;
procedure TfraSyntaxTree.frmArcExplor1DoubleClickFile(nod: TExplorNode);
begin
  if OnOpenFile<>nil then OnOpenFile(nod.Path);
end;
procedure TfraSyntaxTree.frmArcExplor1MenuOpenFile(nod: TExplorNode);
begin
  if OnOpenFile<>nil then OnOpenFile(nod.Path);
end;
procedure TfraSyntaxTree.Init(syntaxTree0: TXpTreeElements);
begin
  syntaxTree := syntaxTree0;
  TreeView1.ReadOnly := true;
  frmElemProperty.OnExplore := @frmElemPropertyExplore;
  SetLanguage('en');   //Inicia idioma
  //Configura filtros del explorador de archivos
  frmArcExplor1.Filter.Items.Add('*.pas,*.pp');  //los filtros se separan por comas
  frmArcExplor1.Filter.Items.Add('*');  //para seleccionar todos
  frmArcExplor1.Filter.ItemIndex:=0;    //selecciona la primera opción por defecto
  frmArcExplor1.InternalPopupFile := true;
  frmArcExplor1.InternalPopupFolder := true;
  frmArcExplor1.OnDoubleClickFile := @frmArcExplor1DoubleClickFile;
  frmArcExplor1.OnMenuOpenFile := @frmArcExplor1MenuOpenFile;
end;
function TfraSyntaxTree.AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
{Agrega un elemento a un noco.}
var
  nod: TTreeNode;
begin
  nod := TreeView1.Items.AddChild(nodParent, elem.name);
  if elem is TxpEleCon then begin
    nod.ImageIndex := 4;
    nod.SelectedIndex := 4;
  end else if elem is TxpEleVar then begin
    nod.ImageIndex := 2;
    nod.SelectedIndex := 2;
  end else if elem is TxpEleFun then begin
    nod.ImageIndex := 3;
    nod.SelectedIndex := 3;
  end else if elem is TxpEleUnit then begin
    nod.ImageIndex := 6;
    nod.SelectedIndex := 6;
  end else if elem is TxpEleBody then begin
    nod.ImageIndex := 12;
    nod.SelectedIndex := 12;
  end else begin
    nod.ImageIndex := 0;
    nod.SelectedIndex := 0;
  end;
  nod.Data := elem;
  Result := nod;
end;
procedure TfraSyntaxTree.frmElemPropertyExplore(elem: TxpElement);
begin
  acGenGoToExecute(self);
end;
procedure TfraSyntaxTree.RefreshByGroups(nodMain: TTreeNode; curEle: TxpElement);
var
  elem, elFun: TxpElement;
  nodVar, nodOtr, nodFun, nodCte, nodUni, nodEleUni, nodEleFun: TTreeNode;
begin
  //Agrega grupos
  nodUni := nil;
  nodVar := nil;
  nodCte := nil;
  nodFun := nil;
  nodOtr := nil;  //por defecto
  //Agrega elementos
  for elem in curEle.elements do begin
    if elem is TxpEleUnit then begin
      if noduni = nil then begin
        nodUni := TreeView1.Items.AddChild(nodMain, TIT_UNIT);
        nodUni.ImageIndex := 0;
        nodUni.SelectedIndex := 0;
      end;
      nodEleUni := AddNodeTo(nodUni, elem);
      //Agrega los elementos de la unidad
      RefreshByDeclar(nodEleUni, elem);  //No agrupa
      nodEleUni.Expanded := false;
    end else if elem is TxpEleCon then begin  //constante
      if nodCte= nil then begin
        nodCte := TreeView1.Items.AddChild(nodMain, TIT_CONS);
        nodCte.ImageIndex := 0;
        nodCte.SelectedIndex := 0;
      end;
      AddNodeTo(nodCte, elem);
    end else if elem is TxpEleVar then begin  //variable
      if nodVar = nil then begin
        nodVar := TreeView1.Items.AddChild(nodMain, TIT_VARS);
        nodVar.ImageIndex := 0;
        nodVar.SelectedIndex := 0;
      end;
      AddNodeTo(nodVar, elem);
    end else if elem is TxpEleFun then begin  //función
      if nodFun = nil then begin  //Si no se ha creado, lo crea
        nodFun := TreeView1.Items.AddChild(nodMain, TIT_FUNC);
        nodFun.ImageIndex := 0;
        nodFun.SelectedIndex := 0;
      end;
      nodEleFun := AddNodeTo(nodFun, elem);
      if elem.elements<>nil then begin
        //Tiene sus propios elementos
        for elFun in elem.elements do begin
          AddNodeTo(nodEleFun, elFun);
        end;
      end;
    end else if elem is TxpEleBody then begin  //cuerpo
      AddNodeTo(nodMain, elem);
    end else begin
      if nodOtr = nil then begin  //Si no se ha creado, lo crea
        nodOtr := TreeView1.Items.AddChild(nodMain, TIT_OTHER);
        nodOtr.ImageIndex := 0;
        nodOtr.SelectedIndex := 0;
      end;
      AddNodeTo(nodOtr, nil);
    end;
  end;
  nodMain.Expanded := true;
//  if nodUni<>nil then nodUni.Expanded := true;
  if nodUni<>nil then nodUni.Expanded := true;
  if nodCte<>nil then nodCte.Expanded := true;
  if nodVar<>nil then nodVar.Expanded := true;
  if nodFun<>nil then nodFun.Expanded := true;
  if nodOtr<>nil then nodOtr.Expanded := true;
end;
procedure TfraSyntaxTree.RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
var
  elem, elem2: TxpElement;
  nodElem: TTreeNode;
begin
  //Agrega elementos
  for elem in curEle.elements do begin
      nodElem := AddNodeTo(nodMain, elem);
      if elem is TxpEleUnit then begin
        //Es una unidad
        RefreshByDeclar(nodElem, elem);  //Llamada recursiva
        nodElem.Expanded := false;
      end else if elem.elements<>nil then begin
        //Tiene sus propios elementos
        for elem2 in elem.elements do begin
          AddNodeTo(nodElem, elem2);
        end;
      end;
  end;
  nodMain.Expanded := true;
end;
procedure TfraSyntaxTree.Refresh;
var
  nodMain: TTreeNode;
begin
  case Config.viewMode of
  vmGroups: begin
    TreeView1.Visible := true;
    frmArcExplor1.Visible := false;

    TreeView1.Items.BeginUpdate;
    TreeView1.Items.Clear;
    nodMain := TreeView1.Items.AddChild(nil, TIT_MAIN);
    nodMain.ImageIndex := 1;
    nodMain.SelectedIndex := 1;
    RefreshByGroups(nodMain, syntaxTree.main);
    TreeView1.Items.EndUpdate;
  end;
  vmDeclar: begin
    TreeView1.Visible := true;
    frmArcExplor1.Visible := false;

    TreeView1.Items.BeginUpdate;
    TreeView1.Items.Clear;
    nodMain := TreeView1.Items.AddChild(nil, TIT_MAIN);
    nodMain.ImageIndex := 1;
    nodMain.SelectedIndex := 1;
    RefreshByDeclar(nodMain, syntaxTree.main);
    TreeView1.Items.EndUpdate;
  end;
  vmFileExp: begin  //Modo de explorador de archivos
    TreeView1.Visible := false;
    frmArcExplor1.Visible := true;
    frmArcExplor1.Align := alClient;

  end;
  end;
end;
function TfraSyntaxTree.SelectedIsMain: boolean;
//Indica si el nodo seleccionado es el nodo raiz
begin
  if TreeView1.Selected = nil then exit(false);
  if TreeView1.Selected.Level = 0 then exit(true);
  exit(false);
end;
function TfraSyntaxTree.SelectedIsGroup: boolean;
begin
  if TreeView1.Selected = nil then exit(false);
  if (Config.viewMode = vmGroups) and (TreeView1.Selected.Level = 1) then begin
    exit(true);
  end;
  exit(false);
end;
function TfraSyntaxTree.SelectedIsElement: boolean;
//Indica si el nodo seleccionado es un nodo que representa a un elemeno.
var
  nod: TTreeNode;
begin
  if TreeView1.Selected = nil then exit(false);
  nod := TreeView1.Selected;
  if Config.viewMode = vmGroups  then begin
    if nod.Level = 2 then begin
      //Es esta vista, todos los del segucdo nivel deben ser elementos.
      exit(true);
    end;
    if (nod.Level = 1) and (nod.Text = TIT_BODY_ELE) then exit(true);
    if nod.Level = 3 then begin
      //Los de tercer nivel, deben ser los elementos locales de procedimientos
      //o de las unidades.
      exit(true);
    end;
  end;
  if Config.viewMode = vmDeclar then begin
    //En modo de declaraciones, es más fácil. Todos son elementos.
    if nod.Level >= 1 then exit(true);
  end;
  exit(false);
end;
procedure TfraSyntaxTree.LocateFile(filname: string);
begin
  //Ubica el archvio actual en el explorador de archivo
  if not self.Visible then exit;
  if frmArcExplor1.Visible then begin
    frmArcExplor1.LocateFileOnTree(filname);
  end;
end;
procedure TfraSyntaxTree.TreeView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  nod: TTreeNode;
begin
  //Quita la selección, si se pulsa en una zona vacía
  nod := TreeView1.GetNodeAt(X,Y);
  if nod=nil then TreeView1.Selected := nil;
  //Abre el menú que corresponda
  if button = mbRight then begin
    if SelectedIsElement then begin
      PopupElem.PopUp;
    end else if SelectedIsGroup then begin
      PopupGroup.PopUp;
    end else begin
      PopupFrame.PopUp;
    end;
  end;
end;
procedure TfraSyntaxTree.TreeView1SelectionChanged(Sender: TObject);
var
  elem: TxpElement;
begin
  if not frmElemProperty.Visible then exit;
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then begin
    frmElemProperty.Clear;
    exit;
  end;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProperty.Exec(elem);
end;
procedure TfraSyntaxTree.TreeView1DblClick(Sender: TObject);
begin
  acGenGoToExecute(self);
end;
procedure TfraSyntaxTree.ComboBoxEx1Change(Sender: TObject);
begin
  if Config = nil then exit;
  case ComboBoxEx1.ItemIndex of
  0: Config.viewMode := vmGroups;
  1: Config.viewMode := vmDeclar;
  2: Config.viewMode := vmFileExp;
  end;
  Refresh;
end;
//////////////////////// Acciones /////////////////////
procedure TfraSyntaxTree.acGenRefresExecute(Sender: TObject);
begin
  Refresh;
end;
procedure TfraSyntaxTree.acGenGoToExecute(Sender: TObject);
var
  elem: TxpElement;
begin
  if SelectedIsElement then begin
    elem := TxpElement(TreeView1.Selected.Data);
    if OnSelectElemen <> nil  then OnSelectElemen(elem);
  end;
end;
procedure TfraSyntaxTree.acGenExpAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do begin
    TreeView1.Items[i].Expanded := true;
  end;
end;
procedure TfraSyntaxTree.acGenPropExecute(Sender: TObject);
var
  elem: TxpElement;
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProperty.Exec(elem);
  frmElemProperty.Show;
end;
procedure TfraSyntaxTree.acGenViewGrExecute(Sender: TObject);
{Muestra elementos por grupos}
begin
  Config.viewMode := vmGroups;
  Refresh;
end;
procedure TfraSyntaxTree.acGenViewDecExecute(Sender: TObject);
{Muestra elementos por declaración}
begin
  Config.viewMode := vmDeclar;
  Refresh;
end;

end.


