unit FrameCfgExtTool;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, StdCtrls, LCLProc,
  Graphics, MisUtils, Types, LCLIntf, Dialogs, Buttons, EditBtn, ActnList,
  Globales, SynFacilBasic, MiConfigXML, process;
type
  //Representa una herramienta exterrna

  { TExternTool }
  TExternTool = object
  public
    name    : string;  //Nombre de la herramienta
    path    : string;  //Ruta del ejecutable
    ComLine : string;  //Línea de comandos
    WaitOnExit: boolean;  //Esperar hasta que termine
    ShowInTbar: boolean;  //Mostrar en barra de herramientas
    IconFile16: string;  //Archivo que contiene el ícono 16*16
    IconFile32: string;  //Archivo que contiene el ícono 32*32
    iconIdx16   : integer; //Índice del ícono
    iconIdx32   : integer; //Índice del ícono
    procedure ReadFromString(const str: string);
    function ToString: string;
    procedure SetAction16(action: TAction);
  end;

  { TfraCfgExtTool }
  TfraCfgExtTool = class(TFrame)
    butAdd: TBitBtn;
    butRemove: TBitBtn;
    butTest: TButton;
    chkWaitExit: TCheckBox;
    chkShowTBar: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    txtIconPath32: TFileNameEdit;
    txtName: TEdit;
    txtComLine: TEdit;
    txtPath: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    ListBox1: TListBox;
    txtIconPath16: TFileNameEdit;
    procedure butAddClick(Sender: TObject);
    procedure butRemoveClick(Sender: TObject);
    procedure butTestClick(Sender: TObject);
    procedure chkShowTBarChange(Sender: TObject);
    procedure chkWaitExitChange(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure txtComLineChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtPathChange(Sender: TObject);
  private
    curTool: TExternTool;  //Atributo actual
    NoEvents: boolean;   //bandera
    procedure ControlsToListBox;
    procedure EstadoCampos(estado: boolean);
    procedure fraCfgExtToolPaint(Sender: TObject);
    procedure ListBoxToControls;
  public
    ExternTools: TStringList;  //Lista de archivos recientes
    TopImages: integer;  //Númro de íconos máximo en las listas de íconos
    defIconIdx: integer;
    OnReplaceParams: procedure(var comLine: string) of object;
    procedure ExecTool(idx: integer);
    procedure Execute(const tool: TExternTool);
  public  //Inicialización
    imgList16, imgList32: TImageList;
    procedure Init(section: string; cfgFile: TMiConfigXML);
    procedure SetImageList(imList16, imList32: TImageList; defIcon: integer);
    procedure ReloadIcons;
    procedure SetLanguage;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
var
  ER_FAIL_EXEC_, PRE_TOOL_NAME, ER_ICO_FIL_UNEXIS_, ER_BOTH_ICO_SPEC
  : String;
{$I ..\language\tra_CfgExtTool.pas}

{ TExternTool }
procedure TExternTool.ReadFromString(const str: string);
var
  a: TStringDynArray;
begin
  a := Explode(#9, str);
  name := a[0];
  path := a[2];
  ComLine := a[3];
  WaitOnExit := f2B(a[4]);
  ShowInTbar := f2B(a[5]);
  IconFile16 := a[6];
  IconFile32 := a[7];
  if a[8]='' then iconIdx16 := -1 else iconIdx16 := F2i(a[8]);
  if a[9]='' then iconIdx32 := -1 else iconIdx32 := F2i(a[9]);
end;
function TExternTool.ToString: string;
begin
  //La cadena a almacenar en la lista, incluye todos los campos, pero se formatea
  //para que solo aparezca el nombre.
  Result := name + #9 +
            space(24) + #9 +  //Con esto se ocultan los otros campos
            path + #9 +
            ComLine + #9 +
            B2f(WaitOnExit) + #9 +
            B2f(ShowInTbar) + #9 +
            IconFile16 + #9 +
            IconFile32 + #9 +
            i2F(iconIdx16) + #9 +
            i2F(iconIdx16) + #9 +
            #9 +   //Campo de ampliación
            #9 +   //Campo de ampliación
            '';         //Campo de ampliación
end;
procedure TExternTool.SetAction16(action: TAction);
{Configura un elemento TAction con los datos de la herramienta externa.}
begin
  action.Visible := ShowInTbar;
  action.Caption:= name;
  action.Hint := name;
  action.ImageIndex := iconIdx16;
end;
{TfraCfgExtTool}
procedure TfraCfgExtTool.Execute(const tool: TExternTool);
{Ejecutar la herramienta externa indicada. }
var
  p: TProcess;
  ComLine, Path: string;
begin
  p := TProcess.Create(nil); //Crea proceso
  if tool.WaitOnExit then p.Options:= p.Options + [poWaitOnExit];
  Path := tool.path;
  ComLine := tool.ComLine;
  if OnReplaceParams <> nil then begin
    OnReplaceParams(Path);  //Reemplaza parámetros
    OnReplaceParams(ComLine);  //Reemplaza parámetros
  end;
  p.CommandLine := Path + ' ' + ComLine;
//  p.Executable:=path;
//  p.Parameters.Clear;
//  p.Parameters.Add(ComLine);
  try
    p.Execute;
  except
    MsgBox(ER_FAIL_EXEC_, [p.Executable]);
  end;
  p.Free;
end;
procedure TfraCfgExtTool.ExecTool(idx: integer);
{Ejecuta la herramienta, de índice "idx"}
var
  cad: String;
  tool: TExternTool;
begin
  if (idx<0) or (idx>ExternTools.Count-1) then exit;
  cad := ExternTools[idx];
  tool.ReadFromString(cad);
  Execute(tool);
end;
procedure TfraCfgExtTool.butTestClick(Sender: TObject);
{Prueba la herramienta externa, que se tiene actualmente en ListBox1.}
var
  cad: String;
  tool: TExternTool;
begin
  if ListBox1.ItemIndex = -1 then exit;
  //Verifica si existen los archivos de íconos
  if (txtIconPath16.Text<>'') and not FileExists(txtIconPath16.Text) then begin
    MsgExc(ER_ICO_FIL_UNEXIS_, [txtIconPath16.Text]);
    if txtIconPath16.Visible then txtIconPath16.SetFocus;
    exit;
  end;
  if (txtIconPath32.Text<>'') and not FileExists(txtIconPath32.Text) then begin
    MsgExc(ER_ICO_FIL_UNEXIS_, [txtIconPath32.Text]);
    if txtIconPath32.Visible then txtIconPath32.SetFocus;
    exit;
  end;
  if (txtIconPath16.Text='') and (txtIconPath32.Text<>'') then begin
    MsgExc(ER_BOTH_ICO_SPEC);
    if txtIconPath16.Visible then txtIconPath16.SetFocus;
    exit;
  end;
  if (txtIconPath16.Text<>'') and (txtIconPath32.Text='') then begin
    MsgExc(ER_BOTH_ICO_SPEC);
    if txtIconPath32.Visible then txtIconPath32.SetFocus;
    exit;
  end;
  //Prueba ejecutando la herramienta seleccionada
  cad := ListBox1.Items[ListBox1.ItemIndex];
  tool.ReadFromString(cad);
  Execute(tool);
end;
procedure TfraCfgExtTool.butAddClick(Sender: TObject);
var
  r: TExternTool;
begin
  r.name := PRE_TOOL_NAME + IntToStr(ListBox1.Count+1);
  r.path := patApp;
  r.ComLine := '';
  r.ShowInTbar := false;
  r.WaitOnExit := false;
  ListBox1.AddItem(r.ToString, nil);
end;
procedure TfraCfgExtTool.butRemoveClick(Sender: TObject);
begin
  if ListBox1.ItemIndex=-1 then exit;
  ListBox1.Items.Delete(ListBox1.ItemIndex);
end;
procedure TfraCfgExtTool.EstadoCampos(estado: boolean);
begin
  label1.Enabled  := estado;
  label2.Enabled  := estado;
  label3.Enabled  := estado;
  label5.Enabled  := estado;
  label6.Enabled  := estado;
  Label4.Enabled  := estado;
  Label7.Enabled  := estado;
  txtIconPath16.Enabled := estado;
  txtIconPath32.Enabled := estado;

  txtName.Enabled := estado;
  txtPath.Enabled := estado;
  txtComLine.Enabled := estado;
  chkWaitExit.Enabled := estado;
  chkShowTBar.Enabled := estado;
  butTest.Enabled := estado;
end;
procedure TfraCfgExtTool.fraCfgExtToolPaint(Sender: TObject);
begin
  ListBox1Click(self);  //Inicia
end;
procedure TfraCfgExtTool.ListBoxToControls;
{Mueve el contendio de curTool, a los controles}
var
  cad: TCaption;
begin
  cad := ListBox1.Items[ListBox1.ItemIndex];
  curTool.ReadFromString(cad);
  NoEvents := true;  //protege
  txtName.Text        := curTool.name;
  txtPath.Text        := curTool.path;
  txtComLine.Text     := curTool.ComLine;
  chkWaitExit.Checked := curTool.WaitOnExit;
  chkShowTBar.Checked := curTool.ShowInTbar;
  txtIconPath16.Text := curTool.IconFile16;
  txtIconPath32.Text := curTool.IconFile32;
  debugln('ListBoxToControls');
  NoEvents := false;
end;
procedure TfraCfgExtTool.ControlsToListBox;
{Mueve el contenido de los controles a la lista ListBox1.}
begin
  if NoEvents then exit;  //Se deshabilitaron eventos
  if ListBox1.ItemIndex = -1 then begin
    exit;
  end;
  curTool.name       := txtName.Text;
  curTool.path       := txtPath.Text;
  curTool.ComLine    := txtComLine.Text;
  curTool.WaitOnExit := chkWaitExit.Checked;
  curTool.ShowInTbar := chkShowTBar.Checked;
  curTool.IconFile16 := txtIconPath16.Text;
  curTool.IconFile32 := txtIconPath32.Text;
  ListBox1.Items[ListBox1.ItemIndex] := curTool.ToString;
end;
procedure TfraCfgExtTool.ListBox1Click(Sender: TObject);
{Se selecciona una Herramienta de la lista de herramientas.}
begin
  if ListBox1.ItemIndex = -1 then begin
    txtName.Text := '';
    txtPath.Text := '';;
    txtComLine.Text := '';
    EstadoCampos(false);
    exit;
  end;
  ListBoxToControls;
  EstadoCampos(true);
end;
procedure TfraCfgExtTool.txtNameChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.txtPathChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.txtComLineChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.chkWaitExitChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.chkShowTBarChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.Init(section: string; cfgFile: TMiConfigXML);
begin
//  cfgFile.Asoc_StrList(section+ '/extern_tools', @ExternTools);
  cfgFile.Asoc_StrList_TListBox(section+ '/extern_tools', @ExternTools, ListBox1);
  self.OnPaint := @fraCfgExtToolPaint;
end;
procedure TfraCfgExtTool.SetImageList(imList16, imList32: TImageList; defIcon: integer);
{Pasa las referencias a los ImageList, para que se puedan configurar los íconos}
begin
  imgList16:= imList16;
  imgList32:= imList32;
  TopImages := imList32.Count;
  defIconIdx := defIcon;  //Ícono por defecto
end;
procedure TfraCfgExtTool.ReloadIcons;
{Explora las herramientas externas para actualizar los íconos y los índices a los
íconos para que se puedan mostrar correctamente. }
var
  lin: String;
  extTool: TExternTool;
  Picture: TPicture;
  SrcBmp: TBitMap;
  i: integer;
begin
  //Limpia los íconos adicionales
  while imgList16.Count>TopImages do begin
    imgList16.Delete(imgList16.Count-1);
  end;
  while imgList32.Count>TopImages do begin
    imgList32.Delete(imgList32.Count-1);
  end;
  for i:=0 to ExternTools.Count-1 do begin
    lin := ExternTools[i];
    extTool.ReadFromString(lin);
    //Solo carga si existe. No se tratan errores aquí proque esta rutina se hace al inicio.
    extTool.iconIdx16 := defIconIdx;
    extTool.iconIdx32 := defIconIdx;
    if (extTool.IconFile16<>'') and FileExists(extTool.IconFile16) then begin
      Picture := TPicture.Create;
      SrcBmp := TBitmap.Create;
      try
        Picture.LoadFromFile(extTool.IconFile16);
        SrcBmp.Assign(Picture.Graphic);
        imgList16.Add(SrcBmp, nil);
      finally
        Picture.Free;
        SrcBmp.Free;
      end;
      extTool.iconIdx16 := imgList16.Count-1;
    end;
    if (extTool.IconFile32<>'') and FileExists(extTool.IconFile32) then begin
      Picture := TPicture.Create;
      SrcBmp := TBitmap.Create;
      try
        Picture.LoadFromFile(extTool.IconFile32);
        SrcBmp.Assign(Picture.Graphic);
        imgList32.Add(SrcBmp, nil);
      finally
        Picture.Free;
        SrcBmp.Free;
      end;
      extTool.iconIdx32 := imgList32.Count-1;
    end;
    ExternTools[i] := extTool.ToString;  //Actualiza el ítem
  end;
end;
constructor TfraCfgExtTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExternTools := TStringList.Create;  //crea lista
  NoEvents := false;
end;
destructor TfraCfgExtTool.Destroy;
begin
  FreeAndNil(ExternTools);
  inherited Destroy;
end;

end.

