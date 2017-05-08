unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Menus,
  XpresElementsPIC, Globales, MisUtils, XpresBas;
type
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    ImageList1: TImageList;
    Label1: TLabel;
    mnGoTo: TMenuItem;
    mnGoTo1: TMenuItem;
    mnProper1: TMenuItem;
    mnRefresh: TMenuItem;
    mnProper: TMenuItem;
    mnRefresh1: TMenuItem;
    PopupElem: TPopupMenu;
    PopupGroup: TPopupMenu;
    TreeView1: TTreeView;
    procedure mnProperClick(Sender: TObject);
    procedure mnRefreshClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    syntaxTree: TXpTreeElements;
    function AddNodeTo(nodParent: TTreeNode; nodName: string; elem: TxpElement
      ): TTreeNode;
    function SelectedIsMain: boolean;
    function SelectedIsGroup: boolean;
    function SelectedIsElement: boolean;
  public
    OnSelectElemen: procedure(out srcPos: TSrcPos) of object;
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
  Label1.Caption := Trans('Syntax Tree', 'Árbol de Sintaxis');
end;
procedure TfraSyntaxTree.Init(syntaxTree0: TXpTreeElements);
begin
  syntaxTree := syntaxTree0;
  SetLanguage;
end;
function TfraSyntaxTree.AddNodeTo(nodParent: TTreeNode; nodName: string; elem: TxpElement): TTreeNode;
{Agrega un elemento a un noco.}
var
  nod: TTreeNode;
begin
  nod := TreeView1.Items.AddChild(nodParent, nodName);
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
  end else begin
    nod.ImageIndex := 0;
    nod.SelectedIndex := 0;
  end;
  nod.Data := elem;
  Result := nod;
end;
procedure TfraSyntaxTree.Refresh;
var
  elem, elUni: TxpElement;
  nodMain, nodVar, nodOtr, nodFun, nodCte, nodUni, uni: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  TreeView1.ReadOnly := true;
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
      uni := AddNodeTo(nodUni, elem.name, elem);
      //Agrega los elementos de la unidad
      for elUni in elem.elements do begin
        AddNodeTo(uni, elUni.name, elUni);
      end;
    end else if elem is TxpEleCon then begin  //constante
      if nodCte= nil then begin
        nodCte := TreeView1.Items.AddChild(nodMain, TIT_CONS);
        nodCte.ImageIndex := 0;
        nodCte.SelectedIndex := 0;
      end;
      AddNodeTo(nodCte, elem.name, elem);
    end else if elem is TxpEleVar then begin  //variable
      if nodVar = nil then begin
        nodVar := TreeView1.Items.AddChild(nodMain, TIT_VARS);
        nodVar.ImageIndex := 0;
        nodVar.SelectedIndex := 0;
      end;
      AddNodeTo(nodVar, elem.name, elem);
    end else if elem is TxpEleFun then begin  //función
      if nodFun = nil then begin  //Si no se ha creado, lo crea
        nodFun := TreeView1.Items.AddChild(nodMain, TIT_FUNC);
        nodFun.ImageIndex := 0;
        nodFun.SelectedIndex := 0;
      end;
      AddNodeTo(nodFun, elem.name, elem);
    end else begin
      if nodOtr = nil then begin  //Si no se ha creado, lo crea
        nodOtr := TreeView1.Items.AddChild(nodMain, TIT_OTHER);
        nodOtr.ImageIndex := 0;
        nodOtr.SelectedIndex := 0;
      end;
      AddNodeTo(nodOtr, elem.name, nil);
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
  if TreeView1.Selected.Level = 1 then exit(true);
  exit(false);
end;
function TfraSyntaxTree.SelectedIsElement: boolean;
//Indica si el nodo seleccionado es un nodo que representa a un elemeno.
begin
  if TreeView1.Selected = nil then exit(false);
  if TreeView1.Selected.Level = 2 then exit(true);
  exit(false);
end;

procedure TfraSyntaxTree.TreeView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbRight then begin
    if SelectedIsElement then PopupElem.PopUp;
    if SelectedIsGroup then PopupGroup.PopUp;
  end;
end;
procedure TfraSyntaxTree.TreeView1DblClick(Sender: TObject);
var
  elem: TxpElement;
begin
  if SelectedIsElement then begin
    elem := TxpElement(TreeView1.Selected.Data);
    if OnSelectElemen <> nil  then OnSelectElemen(elem.srcDec);
  end;
end;
//Opciones del menú
procedure TfraSyntaxTree.mnRefreshClick(Sender: TObject);  //Refresh
begin
  Refresh;
end;
procedure TfraSyntaxTree.mnProperClick(Sender: TObject);
var
  elem: TxpElement;
  fun: TxpEleFun;
  xvar: TxpEleVar;
  cons: TxpEleCon;
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  if elem is TxpEleCon then begin
    cons := TxpEleCon(elem);
    MsgBox('Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + elem.srcDec.Fil + ':(' + IntToStr(elem.srcDec.Row) + ',' +
                                             IntToStr(elem.srcDec.Col)+')' + LineEnding +
           'Num.Llamadas: ' + IntToStr(cons.nCalled) );
  end;
  if elem is TxpEleVar then begin
    xvar := TxpEleVar(elem);
    MsgBox('Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + elem.srcDec.Fil + ':(' + IntToStr(elem.srcDec.Row) + ',' +
                                             IntToStr(elem.srcDec.Col)+')' + LineEnding +
           'Direcc. Solicitada: ' + IntToStr(xvar.solAdr) + ':' + IntToStr(xvar.solBit) + LineEnding +
           'Direcc. Asignada: ' + xvar.AddrString + LineEnding +
           'Num.Llamadas: ' + IntToStr(xvar.nCalled) );
  end;
  if elem is TxpEleFun then begin
    fun := TxpEleFun(elem);
    MsgBox('Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + fun.srcDec.Fil + ':(' + IntToStr(fun.srcDec.Row) + ',' +
                                             IntToStr(fun.srcDec.Col)+')' + LineEnding +
           'Dirección: $' + IntToHex(fun.adrr, 3) + LineEnding +
           'Num.Llamadas: ' + IntToStr(fun.nCalled) + lineending +
           'Tamaño: ' + IntToStr(fun.srcSize)
           );
  end;
end;

end.

