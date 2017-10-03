unit FormPICExplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Pic16Devices, Pic16Utils, MisUtils;

type

  { TfrmPICExplorer }

  TfrmPICExplorer = class(TForm)
    cmbModel: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    txtDescr: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure cmbModelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    pic : TPIC16;
  end;

var
  frmPICExplorer: TfrmPICExplorer;

implementation

{$R *.lfm}

{ TfrmPICExplorer }

procedure TfrmPICExplorer.FormCreate(Sender: TObject);
begin
  pic := TPIC16.Create;
  cmbModel.Items.Clear;
  GetSupportedDevices(cmbModel.Items);
  cmbModel.ItemIndex:=0;
end;
procedure TfrmPICExplorer.FormDestroy(Sender: TObject);
begin
  pic.Destroy;
end;

procedure TfrmPICExplorer.cmbModelChange(Sender: TObject);
begin
  if not GetHardwareInfo(pic, cmbModel.Text) then begin
    txtDescr.Clear;
    exit;
  end;
  txtDescr.Lines.BeginUpdate;
  txtDescr.Clear;
  txtDescr.Lines.Add('Model: ' + cmbModel.Text);
  txtDescr.Lines.Add('Max Clock Frequency: ' + IntToStr(pic.MaxFreq));
  txtDescr.Lines.Add('Number of RAM banks: ' + IntToStr(pic.NumBanks));
  txtDescr.Lines.Add('Number of Flash pages: ' + IntToStr(pic.NumPages));
  txtDescr.Lines.Add('Total RAM Memory: ' + IntToStr(pic.TotalMemRAM));
  txtDescr.Lines.Add('Total Flash Memory: ' + IntToStr(pic.MaxFlash));
  txtDescr.Lines.EndUpdate;
end;

end.

