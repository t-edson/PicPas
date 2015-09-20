unit FormCodeExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, XpresElements;

type

  { TfrmCodeExplorer }

  TfrmCodeExplorer = class(TForm)
    ImageList1: TImageList;
    mnRefresh: TMenuItem;
    PopupMenu1: TPopupMenu;
    TabControl1: TTabControl;
    Timer1: TTimer;
    tree: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure mnRefreshClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    treeCode: TXpTreeElements;
  public
    procedure Init(treeCode0: TXpTreeElements);
    procedure Refresh;
    procedure SetLanguage(lang: string);
  end;

var
  frmCodeExplorer: TfrmCodeExplorer;

implementation
{$R *.lfm}

procedure TfrmCodeExplorer.FormCreate(Sender: TObject);
begin
{  Item := tree.Items.AddChild(nil,'nodo1');
  tree.Items.AddChild(item,'subnodo');
  tree.Items.AddChild(item,'subnodo');
  tree.Items.AddChild(item,'subnodo');
  Item := tree.Items.AddChild(nil,'nodo2');
  Item := tree.Items.AddChild(nil,'nodo3');}
end;

procedure TfrmCodeExplorer.mnRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TfrmCodeExplorer.Timer1Timer(Sender: TObject);
begin
  if self.Visible then
    Refresh;
end;

procedure TfrmCodeExplorer.Refresh;
  procedure AddElements(elements: TxpElements; Tree: TTreeView; Item: TTreeNode);
  var
    elem : TxpElement;
    ItemTemp: TTreeNode;
    item0: TTreeNode;
  begin
     for elem in elements do begin
       if (elem.elements<>nil) and (elem.elements.Count>0) then begin
         ItemTemp := Item;
         Item := Tree.Items.AddChild(Item, elem.name);
         Item.ImageIndex:=0;
         Item.SelectedIndex:=0;
         AddElements(elem.elements, Tree, Item);
         Item.Expanded:=true;
         Item := ItemTemp;
       end else begin
         item0 := Tree.Items.AddChild(Item, elem.name);
         item0.ImageIndex:=0;
         item0.SelectedIndex:=0;
       end;
     end;
  end;
begin
  if treeCode = nil then exit;
  tree.Items.Clear;
  tree.Items.BeginUpdate;
  AddElements(treeCode.main.elements, tree, nil);
  tree.Items.EndUpdate;
end;

procedure TfrmCodeExplorer.Init(treeCode0: TXpTreeElements);
{Inicia el explorador}
begin
 treeCode := treeCode0;
end;
procedure TfrmCodeExplorer.SetLanguage(lang: string);
begin
  case lowerCase(lang) of
  'es': begin
      Caption:='Explorador de Código';
      mnRefresh.Caption:='&Refrescar';
    end;
  'en': begin
      Caption:='Code Explorer';
      mnRefresh.Caption:='&Refresh';
    end;
  end;
end;

end.

