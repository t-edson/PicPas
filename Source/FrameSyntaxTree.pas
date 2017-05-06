unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Menus,
  MisUtils, XpresElementsPIC;
type
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    ImageList1: TImageList;
    Label1: TLabel;
    mnGoTo: TMenuItem;
    mnRefresh: TMenuItem;
    mnProper: TMenuItem;
    PopupMenu1: TPopupMenu;
    TreeView1: TTreeView;
    procedure mnProperClick(Sender: TObject);
    procedure mnRefreshClick(Sender: TObject);
  private
    syntaxTree: TXpTreeElements;
    procedure AddNodeTo(nodParent: TTreeNode; nodName: string; imgIndex: integer;
      Data: Pointer);
  public
    procedure Init(syntaxTree0: TXpTreeElements);
    procedure Refresh;
  end;

implementation

{$R *.lfm}

{ TfraSyntaxTree }
procedure TfraSyntaxTree.Init(syntaxTree0: TXpTreeElements);
begin
  syntaxTree := syntaxTree0;
end;
procedure TfraSyntaxTree.AddNodeTo(nodParent: TTreeNode; nodName: string; imgIndex: integer;
                                   Data: Pointer);
{Agrega un elemento a un noco.}
var
  nod: TTreeNode;
begin
  nod := TreeView1.Items.AddChild(nodParent, nodName);
  nod.ImageIndex := imgIndex;
  nod.SelectedIndex := imgIndex;
  nod.Data := Data;
end;
procedure TfraSyntaxTree.Refresh;
var
  elem : TxpElement;
  nodMain, nodVar, nodOtr, nodFun, nodCte, nodUni: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  TreeView1.ReadOnly := true;
  nodMain := TreeView1.Items.AddChild(nil,'program');
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
        nodUni := TreeView1.Items.AddChild(nodMain, 'Units');
        nodUni.ImageIndex := 0;
        nodUni.SelectedIndex := 0;
      end;
      AddNodeTo(nodUni, elem.name, 6, elem);
    end else if elem is TxpEleCon then begin  //constante
      if nodCte= nil then begin
        nodCte := TreeView1.Items.AddChild(nodMain, 'Constants');
        nodCte.ImageIndex := 0;
        nodCte.SelectedIndex := 0;
      end;
      AddNodeTo(nodCte, elem.name, 4, elem);
    end else if elem is TxpEleVar then begin  //variable
      if nodVar = nil then begin
        nodVar := TreeView1.Items.AddChild(nodMain, 'Variables');
        nodVar.ImageIndex := 0;
        nodVar.SelectedIndex := 0;
      end;
      AddNodeTo(nodVar, elem.name, 2, elem);
    end else if elem is TxpEleFun then begin  //función
      if nodFun = nil then begin  //Si no se ha creado, lo crea
        nodFun := TreeView1.Items.AddChild(nodMain, 'Functions');
        nodFun.ImageIndex := 0;
        nodFun.SelectedIndex := 0;
      end;
      AddNodeTo(nodFun, elem.name, 3, elem);
    end else begin
      if nodOtr = nil then begin  //Si no se ha creado, lo crea
        nodOtr := TreeView1.Items.AddChild(nodMain, 'Others');
        nodOtr.ImageIndex := 0;
        nodOtr.SelectedIndex := 0;
      end;
      AddNodeTo(nodOtr, elem.name, -1, nil);
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
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  if elem is TxpEleVar then begin
    xvar := TxpEleVar(elem);
    MsgBox('Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + elem.src.Fil + ':(' + IntToStr(elem.src.Row) + ',' +
                                             IntToStr(elem.src.Col)+')' + LineEnding +
           'Direcc. Solicitada: ' + IntToStr(xvar.solAdr) + ':' + IntToStr(xvar.solBit) + LineEnding +
           'Direcc. Asignada: ' + xvar.AddrString + LineEnding +
           'Num.Llamadas: ' + IntToStr(xvar.nCalled) );
  end;
  if elem is TxpEleFun then begin
    fun := TxpEleFun(elem);
    MsgBox('Nombre: ' + elem.name + LineEnding +
           'Tipo: ' + elem.typ.name + LineEnding +
           'Ubicación: ' + fun.src.Fil + ':(' + IntToStr(fun.src.Row) + ',' +
                                             IntToStr(fun.src.Col)+')' + LineEnding +
           'Dirección: $' + IntToHex(fun.adrr, 3) + LineEnding +
           'Num.Llamadas: ' + IntToStr(fun.nCalled) + lineending +
           'Tamaño: ' + IntToStr(fun.srcSize)
           );
  end;
end;

end.

