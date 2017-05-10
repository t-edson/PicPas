unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, StdCtrls,
  ComCtrls, Menus, ActnList, ExtCtrls, ComboEx, XpresElementsPIC, Globales,
  FormElemProperty, XpresParserPIC, MisUtils, XpresBas;
type
  TTreeViewMode = (vmGroups,   //Muestra por grupos
                   vmDeclar    //Muestra en el orden de declaración
                   );
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    acGenRefres: TAction;
    acGenGoTo: TAction;
    acGenProp: TAction;
    acGenViewGr: TAction;
    acGenViewDec: TAction;
    ActionList1: TActionList;
    ComboBoxEx1: TComboBoxEx;
    ImageList1: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    procedure acGenGoToExecute(Sender: TObject);
    procedure acGenRefresExecute(Sender: TObject);
    procedure acGenPropExecute(Sender: TObject);
    procedure acGenViewDecExecute(Sender: TObject);
    procedure acGenViewGrExecute(Sender: TObject);
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    syntaxTree: TXpTreeElements;
    viewMode: TTreeViewMode;
    function AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
    procedure RefreshByDeclar;
    procedure RefreshByGroups;
    function SelectedIsMain: boolean;
    function SelectedIsGroup: boolean;
    function SelectedIsElement: boolean;
  public
    OnSelectElemen: procedure(var elem: TxpElement) of object;
    procedure Init(syntaxTree0: TXpTreeElements);
    procedure Refresh;
    procedure SetLanguage;
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
procedure TfraSyntaxTree.SetLanguage;
{Fija el lenguaje de acuerdo al valor de Globales.curLang}
begin
  TIT_MAIN := trans('Program','Programa');
  TIT_UNIT := trans('Units','Unidades');
  TIT_CONS := trans('Constants','Constantes');
  TIT_VARS := trans('Variables','Variables');
  TIT_FUNC := trans('Functions','Funciones');
  TIT_OTHER:= trans('Others','Otros');
  Label1.Caption := Trans('Code Explorer', 'Explorador de Código');
end;
procedure TfraSyntaxTree.Init(syntaxTree0: TXpTreeElements);
begin
  syntaxTree := syntaxTree0;
  TreeView1.ReadOnly := true;
  viewMode := vmGroups;
  SetLanguage;
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
procedure TfraSyntaxTree.RefreshByGroups;
var
  elem, elFun, elUni: TxpElement;
  nodMain, nodVar, nodOtr, nodFun, nodCte, nodUni, nodEleUni, nodEleFun: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  nodMain := TreeView1.Items.AddChild(nil, TIT_MAIN);
  nodMain.ImageIndex := 1;
  nodMain.SelectedIndex := 1;
  //Agrega grupos
  nodUni := nil;
  nodVar := nil;
  nodCte := nil;
  nodFun := nil;
  nodOtr := nil;  //por defecto
  //Agrega elementos
  for elem in syntaxTree.main.elements do begin
    if elem is TxpEleUnit then begin
      if noduni = nil then begin
        nodUni := TreeView1.Items.AddChild(nodMain, TIT_UNIT);
        nodUni.ImageIndex := 0;
        nodUni.SelectedIndex := 0;
      end;
      nodEleUni := AddNodeTo(nodUni, elem);
      //Agrega los elementos de la unidad
      for elUni in elem.elements do begin
        AddNodeTo(nodEleUni, elUni);
      end;
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
  TreeView1.Items.EndUpdate;
end;
procedure TfraSyntaxTree.RefreshByDeclar;
var
  elem, elem2: TxpElement;
  nodMain, nodElem: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  nodMain := TreeView1.Items.AddChild(nil, TIT_MAIN);
  nodMain.ImageIndex := 1;
  nodMain.SelectedIndex := 1;
  //Agrega elementos
  for elem in syntaxTree.main.elements do begin
      nodElem := AddNodeTo(nodMain, elem);
      if elem.elements<>nil then begin
        //Tiene sus propios elementos
        for elem2 in elem.elements do begin
          AddNodeTo(nodElem, elem2);
        end;
      end;
  end;
  nodMain.Expanded := true;
  TreeView1.Items.EndUpdate;
end;
procedure TfraSyntaxTree.Refresh;
begin
  if viewMode = vmGroups then RefreshByGroups;
  if viewMode = vmDeclar then RefreshByDeclar;
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
  if (viewMode = vmGroups) and (TreeView1.Selected.Level = 1) then begin
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
  if viewMode = vmGroups  then begin
    if nod.Level = 2 then exit(true);
    if (nod.Level = 1) and (nod.Text = TIT_BODY_ELE) then exit(true);
  end;
  if viewMode = vmDeclar then begin
    //En modo de declaraciones, es más fácil. Todos son elementos.
    if nod.Level >= 1 then exit(true);
  end;
  exit(false);
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
procedure TfraSyntaxTree.TreeView1DblClick(Sender: TObject);
begin
  acGenGoToExecute(self);
end;
procedure TfraSyntaxTree.ComboBoxEx1Change(Sender: TObject);
begin
  case ComboBoxEx1.ItemIndex of
  0: viewMode := vmGroups;
  1: viewMode := vmDeclar;
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
procedure TfraSyntaxTree.acGenPropExecute(Sender: TObject);
var
  elem: TxpElement;
  fun: TxpEleFun;
  xvar: TxpEleVar;
  cons: TxpEleCon;
  tmp: String;
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  tmp := '';
  if elem is TxpEleCon then begin
    cons := TxpEleCon(elem);
    tmp := 'Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + elem.srcDec.Fil + ':(' + IntToStr(elem.srcDec.Row) + ',' +
                                             IntToStr(elem.srcDec.Col)+')' + LineEnding +
           'Num.Llamadas: ' + IntToStr(cons.nCalled);
  end;
  if elem is TxpEleVar then begin
    xvar := TxpEleVar(elem);
    tmp := 'Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + elem.srcDec.Fil + ':(' + IntToStr(elem.srcDec.Row) + ',' +
                                             IntToStr(elem.srcDec.Col)+')' + LineEnding +
           'Direcc. Solicitada: ' + IntToStr(xvar.solAdr) + ':' + IntToStr(xvar.solBit) + LineEnding +
           'Direcc. Asignada: ' + xvar.AddrString + LineEnding +
           'Num.Llamadas: ' + IntToStr(xvar.nCalled) ;
  end;
  if elem is TxpEleFun then begin
    fun := TxpEleFun(elem);
    tmp := 'Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + fun.srcDec.Fil + ':(' + IntToStr(fun.srcDec.Row) + ',' +
                                             IntToStr(fun.srcDec.Col)+')' + LineEnding +
           'Dirección: $' + IntToHex(fun.adrr, 3) + LineEnding +
           'Num.Llamadas: ' + IntToStr(fun.nCalled) + lineending +
           'Tamaño: ' + IntToStr(fun.srcSize);
  end;
  if tmp<>'' then begin
    frmElemProperty.Memo1.Text := tmp;
    frmElemProperty.ShowModal;
  end;
end;
procedure TfraSyntaxTree.acGenViewGrExecute(Sender: TObject);
{Muestra elementos por grupos}
begin
  viewMode := vmGroups;
  Refresh;
end;
procedure TfraSyntaxTree.acGenViewDecExecute(Sender: TObject);
{Muestra elementos por declaración}
begin
  viewMode := vmDeclar;
  Refresh;
end;

end.

