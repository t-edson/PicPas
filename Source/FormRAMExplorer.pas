unit FormRAMExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  FrameRamExplorer, Parser;
type

  { TfrmRAMExplorer }

  TfrmRAMExplorer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fra: TfraRamExplorer;
  public
    procedure Exec(cxp0: TCompilerBase);
  end;

var
  frmRAMExplorer: TfrmRAMExplorer;

implementation

{$R *.lfm}

{ TfrmRAMExplorer }

procedure TfrmRAMExplorer.FormCreate(Sender: TObject);
begin
  fra:= TfraRamExplorer.Create(self);
  fra.Parent := self;
  fra.panTitle.Visible := false;
end;

procedure TfrmRAMExplorer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F12 then begin
    Key := 0;
    self.Hide;
  end;
  if Key = VK_ESCAPE then begin
    Key := 0;
    self.Hide;
  end;
end;

procedure TfrmRAMExplorer.Exec(cxp0: TCompilerBase);
begin
  fra.SetCompiler(cxp0);
  Caption := 'RAM Explorer. PICModel=' + cxp0.PICName;
  Show;
  self.Width := 600;
  self.Height := 480;
  fra.Align := alClient;
//  fra.Invalidate;
end;

end.

