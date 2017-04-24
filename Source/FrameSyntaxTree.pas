unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Menus,
  XpresElementsPIC;
type
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    ImageList1: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    TreeView1: TTreeView;
    procedure MenuItem1Click(Sender: TObject);
  private
    syntaxTree: TXpTreeElements;
  public
    procedure Init(syntaxTree0: TXpTreeElements);
    procedure Refresh;
  end;

implementation

{$R *.lfm}

{ TfraSyntaxTree }
procedure TfraSyntaxTree.MenuItem1Click(Sender: TObject);  //Refresh
begin
  Refresh;
end;

procedure TfraSyntaxTree.Init(syntaxTree0: TXpTreeElements);
begin
  syntaxTree := syntaxTree0;
end;

procedure TfraSyntaxTree.Refresh;
var
  elem : TxpElement;
  nodMain, nod, nodVar, nodOtr, nodFun, nodCte, nodUni: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  TreeView1.ReadOnly := true;
  nodMain := TreeView1.Items.AddChild(nil,'program');
  nodMain.ImageIndex := 1;
  nodMain.SelectedIndex := 1;
  //Agrega grupos
  nodUni := TreeView1.Items.AddChild(nodMain, 'Units');
  nodUni.ImageIndex := 0;
  nodUni.SelectedIndex := 0;
  nodVar := TreeView1.Items.AddChild(nodMain, 'Variables');
  nodVar.ImageIndex := 0;
  nodVar.SelectedIndex := 0;
  nodCte:= nil;
  nodFun := nil;
  nodOtr := nil;  //por defecto
  //Agrega elementos
  for elem in syntaxTree.main.elements do begin
//    if elem is TxpEleu then begin
//      nod := TreeView1.Items.AddChild(nodUni, elem.name);
//      nod.ImageIndex := 5;
//      nod.SelectedIndex := 5;
//    end;
    if elem is TxpEleCon then begin  //constante
      if nodCte= nil then begin
        nodCte := TreeView1.Items.AddChild(nodMain, 'Constants');
        nodCte.ImageIndex := 0;
        nodCte.SelectedIndex := 0;
      end;
      nod := TreeView1.Items.AddChild(nodCte, elem.name);
      nod.ImageIndex := 4;
      nod.SelectedIndex := 4;
    end else if elem is TxpEleVar then begin  //varible
      nod := TreeView1.Items.AddChild(nodVar, elem.name);
      nod.ImageIndex := 2;
      nod.SelectedIndex := 2;
    end else if elem is TxpEleFun then begin  //función
      if nodFun = nil then begin  //Si no se ha creado, lo crea
        nodFun := TreeView1.Items.AddChild(nodMain, 'Functions');
        nodFun.ImageIndex := 0;
        nodFun.SelectedIndex := 0;
      end;
      nod := TreeView1.Items.AddChild(nodFun, elem.name);
      nod.ImageIndex := 3;
      nod.SelectedIndex := 3;
    end else begin
      if nodOtr = nil then begin  //Si no se ha creado, lo crea
        nodOtr := TreeView1.Items.AddChild(nodMain, 'Others');
        nodOtr.ImageIndex := 0;
        nodOtr.SelectedIndex := 0;
      end;
      nod := TreeView1.Items.AddChild(nodOtr, elem.name);
//      nod.ImageIndex := 1;
//      nod.SelectedIndex := 1;
    end;
  end;
  nodMain.Expanded := true;
//  if nodUni<>nil then nodUni.Expanded := true;
  if nodCte<>nil then nodCte.Expanded := true;
  if nodVar<>nil then nodVar.Expanded := true;
  if nodFun<>nil then nodFun.Expanded := true;
  if nodOtr<>nil then nodOtr.Expanded := true;
  TreeView1.Items.EndUpdate;
end;

end.

