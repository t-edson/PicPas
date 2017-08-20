unit FormDebugger;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Parser, FrameRamExplorer, Pic16Utils;

type

  { TfrmDebugger }

  TfrmDebugger = class(TForm)
    Button1: TButton;
    CoolBar1: TCoolBar;
    Label1: TLabel;
    lblROM: TLabel;
    lblSTACK: TLabel;
    Panel1: TPanel;
    panRAM: TPanel;
    panStatis: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fraRamExp: TfraRamExplorer;
    FBackColor: TColor;
    FTextColor: TColor;
    procedure panStatisPaint(Sender: TObject);
    { private declarations }
  public
    pic: TPIC16;
    procedure Exec(cxp: TCompiler);
  end;

var
  frmDebugger: TfrmDebugger;

implementation

{$R *.lfm}

{ TfrmDebugger }
procedure TfrmDebugger.FormCreate(Sender: TObject);
begin
  fraRamExp:= TfraRamExplorer.Create(self);
  fraRamExp.Parent := panRAM;
  fraRamExp.Align := alClient;
end;
procedure TfrmDebugger.Button1Click(Sender: TObject);
begin
  panStatisPaint(self);
end;

procedure TfrmDebugger.panStatisPaint(Sender: TObject);
var
  cv: TCanvas;
  procedure Barra(x0, y0: integer; alt: integer; porc: Single);
  var
    alt2, dif: Integer;
    n: Int64;
  begin
    if alt<15 then exit;
    if alt>120 then alt := 100;
    //Dibuja fondo
    {$ifdef UNIX}
//    cv.Brush.Color := clForm;
    cv.Brush.Color := FBackColor;
    {$else}
//    cv.Brush.Color := clMenu;
    cv.Brush.Color := FBackColor;
    {$endif}
    cv.FillRect(x0, y0, x0 + 20, y0 +alt);
    //Dibuja barra
    n := round(porc*100);
    cv.Pen.Color := clGreen;
    if n < 40 then begin
      cv.Brush.Color := clGreen;
    end else if n < 80 then begin
      cv.Brush.Color := clYellow;
    end else begin
      cv.Brush.Color := clRed;
    end;
    alt2 := Round(alt*porc);
    dif := alt-alt2;
    cv.FillRect(x0, y0 + dif , x0 + 20, y0 + alt2 + dif);
    //Borde
    cv.Pen.Color := clGray;
    cv.Frame(x0, y0, x0 + 20, y0 +alt);
    cv.Frame(x0-2, y0-2, x0 + 22, y0 +alt+2);
    //Texto
    cv.Brush.Style := bsClear;
    cv.Font.Bold := true;
    cv.Font.Color := FTextColor;
    if n<10 then begin
      cv.TextOut(x0+2, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else if n < 100 then begin
      cv.TextOut(x0-1, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else begin
      cv.TextOut(x0-3, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end;
  end;
begin
  cv := panStatis.Canvas;
//  Barra(lblRAM.Left + 5, lblRAM.Top + 20, panStatis.Height-35, 0.5);
  Barra(lblROM.Left + 5, lblROM.Top + 20, panStatis.Height-35, 0.5);
  Barra(lblSTACK.Left+ 5, lblSTACK.Top + 20, panStatis.Height-35, 0.5);
end;

procedure TfrmDebugger.Exec(cxp: TCompiler);
begin
  pic := cxp.pic;
  fraRamExp.pic := pic;
  fraRamExp.Invalidate;
  self.Show;
end;

end.

