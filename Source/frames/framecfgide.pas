unit FrameCfgIDE;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ConfigFrame;

type
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);

  { TfraCfgIDE }
  TfraCfgIDE = class(TCfgFrame)
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
  private
    f: TForm;
    FViewPanMsg: boolean;
    fViewStatusbar: Boolean;
    FViewToolbar: boolean;
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewToolbar(AValue: boolean);
  public
    StateToolbar: TStyleToolbar;
    property ViewStatusbar: Boolean read FViewStatusbar write SetViewStatusbar;
    property ViewToolbar: boolean read FViewToolbar write SetViewToolbar;
    property ViewPanMsg: boolean read FViewPanMsg write SetViewPanMsg;
    procedure Iniciar(secINI0: string; form: TForm);
  end;

implementation
{$R *.lfm}

{ TfraCfgIDE }
procedure TfraCfgIDE.Iniciar(secINI0: string; form: TForm);
begin
  secINI := secINI0;  //sección INI
  f := form;  //needed reference
  //asociaciones
  Asoc_Enum_TRadGroup(@StateToolbar, SizeOf(TStyleToolbar),
                      RadioGroup1,'StateStatusbar',2);
  Asoc_Bol(@FViewPanMsg,'VerPanMensaj',true);
  Asoc_Bol(@ViewStatusbar,'VerStatusbar',true);
  Asoc_Bol(@FViewToolbar,'VerBarHerram',true);
end;

procedure TfraCfgIDE.SetViewPanMsg(AValue: boolean);
begin
  if FViewPanMsg=AValue then Exit;
  FViewPanMsg:=AValue;
  OnUpdateChanges;   //llama a evento para actualizar
end;
procedure TfraCfgIDE.SetViewStatusbar(AValue: Boolean);
begin
  if FViewStatusbar=AValue then Exit;
  FViewStatusbar:=AValue;
  OnUpdateChanges;   //llama a evento para actualizar
end;
procedure TfraCfgIDE.SetViewToolbar(AValue: boolean);
begin
  if FViewToolbar=AValue then Exit;
  FViewToolbar:=AValue;
  OnUpdateChanges;   //llama a evento para actualizar
end;


end.

